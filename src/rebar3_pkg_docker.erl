%% rebar3_pkg_docker.erl
%% Provides: rebar3 pkg docker <subcmd>
%% Currently supports: `compile`
-module(rebar3_pkg_docker).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(NAMESPACE, pkg).
-define(NAME, docker).
-define(DEPS, [{default, compile}, {default, app_discovery}, {default, release}]).
-define(REBAR3_URL, "https://s3.amazonaws.com/rebar3/rebar3").
%% Public init used by rebar3_pkg_prv.erl to register this provider
init(State) ->
    Provider = providers:create([
        {name, ?NAME},
        {namespace, ?NAMESPACE},
        {module, ?MODULE},
        {bare, true},
        %% ensure code is compiled before docker step
        {deps, ?DEPS},
        {opts, opts()},
        {short_desc, "Run pkg docker subcommands (e.g., compile) inside a container"},
        {desc, "Spin up a Linux Mint (or chosen) container, mount the repo, and run subcommands"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

opts() ->
    [
        {image, $i, "image", string, "Docker image (default: linuxmintd/mint22-amd64)"},
        {platform, $p, "platform", string, "Docker --platform (e.g. linux/amd64)"},
        {hostdir, $d, "hostdir", string, "Host project dir to mount (default: cwd)"},
        {workdir, 0, "workdir", string, "Container workdir (default: /opt/damagebdd)"},
        {use_local, 0, "use-local-rebar3", boolean, "Use ./rebar3 from repo instead of apt rebar3"},
        {use_tty, 0, "use-tty", boolean, "Force -t (TTY). Default: auto-detect"},
        {rebar3_url, 0, ?REBAR3_URL, string,
            "HTTPS URL to rebar3 binary in S3 (required to install rebar3 from S3)"}
    ].

do(State) ->
    {Args, Positionals} = rebar_state:command_parsed_args(State),
    SubCmd =
        case Positionals of
            [] -> "compile";
            [S | _] -> S
        end,
    case SubCmd of
        "compile" ->
            run_compile_in_container(State, Args);
        Other ->
            rebar_api:error(
                "Unknown subcommand for 'pkg docker': ~s~nTry: rebar3 pkg docker compile", [Other]
            ),
            {error, {unknown_subcommand, Other}}
    end.

%% Install local deb, fetch rebar3 from S3, then run `rebar3 compile` in /opt/damagebdd
run_compile_in_container(State, Args) ->
    Image = proplists:get_value(image, Args, "linuxmintd/mint22-amd64"),
    Platform = proplists:get_value(platform, Args, undefined),
    WorkdirC = proplists:get_value(workdir, Args, "/opt/damagebdd"),
    UseTTYOpt = proplists:get_value(use_tty, Args, undefined),

    %% REQUIRED: where to fetch rebar3 from (public HTTPS S3 object)
    Rebar3URL = proplists:get_value(rebar3_url, Args, ?REBAR3_URL),
    {ok, Cwd} = file:get_cwd(),
    HostDir = filename:absname(proplists:get_value(hostdir, Args, Cwd)),

    %% Path to the local deb *inside* the container once mounted
    DebPathInContainer =
        filename:join(WorkdirC, "_build/pkg/deb/damage_0.1.0_amd64.deb"),

    %% Inside-container script:
    %%  - base deps for packaging and curl
    %%  - download rebar3 from S3 to /usr/local/bin
    %%  - install the local deb
    %%  - compile the project with the downloaded rebar3
    Script = lists:flatten(
        io_lib:format(
            "set -e; "
            "apt-get update -y; "
            "DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends "
            "erlang ca-certificates curl dpkg xz-utils; "
            "install -d -m 0755 /usr/local/bin; "
            "curl -fsSL ~s -o /usr/local/bin/rebar3; "
            "chmod +x /usr/local/bin/rebar3; "
            "cd ~s; "
            "apt-get install -y ./_build/pkg/deb/damage_0.1.0_amd64.deb || "
            "(dpkg -i ~s && apt-get -f install -y); "
            "rebar3 version; "
            "rebar3 compile",
            [
                shell_quote(Rebar3URL),
                shell_quote(WorkdirC),
                shell_quote(DebPathInContainer)
            ]
        )
    ),

    VolFlag = "-v " ++ shell_quote(HostDir) ++ ":" ++ shell_quote(WorkdirC),
    WorkFlag = "-w " ++ shell_quote(WorkdirC),
    PlatFlag =
        case Platform of
            undefined -> "";
            P -> " --platform " ++ shell_quote(P)
        end,
    TTYFlag = docker_tty_flag(UseTTYOpt),

    Cmd = string:join(
        [
            "docker",
            "run",
            "--rm",
            TTYFlag,
            VolFlag,
            WorkFlag,
            PlatFlag,
            shell_quote(Image),
            "bash",
            "-lc",
            shell_quote(Script)
        ],
        " "
    ),

    rebar_api:info("docker (install deb + compile): ~s", [Cmd]),
    {Exit, Output} = run_cmd(Cmd),
    case Exit of
        0 ->
            rebar_api:info("docker output:~n~s", [Output]),
            {ok, State};
        Code ->
            rebar_api:error(
                "docker FAILED (exit ~p).~nCommand:~n~s~n---- output begin ----~n~s~n---- output end ----",
                [Code, Cmd, Output]
            ),
            {error, {docker_failed, Code}}
    end.

%% Choose -it only if we have a TTY (or user forces it)
docker_tty_flag(true) ->
    "-it";
docker_tty_flag(false) ->
    "-i";
docker_tty_flag(undefined) ->
    case os:cmd("[ -t 1 ] && echo yes || echo no") of
        "yes\n" -> "-it";
        "yes\r\n" -> "-it";
        _ -> "-i"
    end.

%% Run a command, capturing stdout+stderr and the real exit status.
run_cmd(Cmd) when is_list(Cmd) ->
    Sh = "/bin/sh",
    Args = ["-lc", Cmd],
    Port = open_port(
        {spawn_executable, Sh},
        [exit_status, use_stdio, stderr_to_stdout, binary, {args, Args}]
    ),
    collect_port(Port, <<>>).

collect_port(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_port(Port, <<Acc/binary, Data/binary>>);
        {Port, {exit_status, Code}} ->
            {Code, binary_to_list(Acc)}
        %% 10 min safety timeout; adjust as needed
    after 600000 ->
        port_close(Port),
        {999, binary_to_list(Acc)}
    end.

format_error(E) ->
    io_lib:format("pkg docker error: ~p", [E]).

%% ---------- helpers (POSIX-safe quoting) ----------

shell_quote(S) when is_list(S) ->
    Needs = lists:any(fun(C) -> (C =< 32) orelse (C =:= $") orelse (C =:= $') end, S),
    case Needs of
        true -> [$' | escape_squotes(S)] ++ [$'];
        false -> S
    end;
shell_quote(B) when is_binary(B) -> shell_quote(binary_to_list(B));
shell_quote(A) when is_atom(A) -> shell_quote(atom_to_list(A)).

escape_squotes([]) -> [];
escape_squotes([$' | T]) -> [$', $\\, $', $' | escape_squotes(T)];
escape_squotes([H | T]) -> [H | escape_squotes(T)].
