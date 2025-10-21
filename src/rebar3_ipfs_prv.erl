%% rebar3_ipfs_prv.erl
%% A rebar3 provider: `rebar3 ipfs lock`
%% - Adds rebar.lock and each project dep dir to IPFS
%% - Emits a manifest JSON with all CIDs
-module(rebar3_ipfs_prv).
-behaviour(provider).

-include_lib("kernel/include/logger.hrl").

-export([init/1, do/1, format_error/1]).

-define(NAMESPACE, ipfs).
-define(NAME, lock).
-define(DEPS, [{default, compile}, {default, app_discovery}]).

%% ---------- Provider registration ----------

init(State) ->
    Provider = providers:create([
        {name, ?NAME},
        {namespace, ?NAMESPACE},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 ipfs lock --out _build/ipfs --pin --profile prod"},
        {opts, opts()},
        {short_desc, "Hash lockfile + deps to IPFS and write a manifest"},
        {desc,
            "Adds rebar.lock and each dependency directory to IPFS (recursive), "
            "optionally pins, and writes a manifest JSON with CIDs."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

opts() ->
    [
        {out, $o, "out", string, "Output dir (default: _build/ipfs)"},
        {profile, $p, "profile", string, "Build profile (default: current)"},
        {pin, undefined, "pin", boolean, "Pin CIDs after add (default: false)"},
        {ipfs_bin, $b, "ipfs-bin", string, "ipfs binary (default: \"ipfs\")"}
    ].

%% ---------- Entry point ----------

do(State) ->
    try
        Meta = project_meta(State),
        OutDir = proplists:get_value(out_dir, Meta, "_build/ipfs"),
        ok = ensure_dir(OutDir),

        %% 1) rebar.lock
        {LockCID, LockPath} = add_lock(State, Meta),

        %% 2) deps (from project apps on disk)
        Deps = deps_on_disk(State),
        DepResults = [add_dep(Dep) || Dep <- Deps],

        %% 3) write manifest
        Manifest = manifest(Meta, LockCID, LockPath, DepResults),
        ManifestPath = filename:join(OutDir, "manifest.json"),
        ok = write_file(ManifestPath, jsx:encode(Manifest)),

        rebar_api:info("ipfs: wrote manifest ~s", [ManifestPath]),
        {ok, State}
    catch
        Class:Reason:Stack ->
            rebar_api:error("ipfs: fatal ~p: ~p~n~s", [Class, Reason, format_stack(Stack)]),
            {error, {ipfs_failed, Reason}}
    end.

format_error(R) -> io_lib:format("ipfs error: ~p", [R]).

%% ---------- Helpers (project/profile), kept similar to your pkg prv ----------

current_profile(State) ->
    case rebar_state:current_profiles(State) of
        [P | _] -> atom_to_list(P);
        [] -> "default"
        %% mirrors your approach of reading the current profile. :contentReference[oaicite:1]{index=1}
    end.

project_meta(State) ->
    %% Keep this aligned with your metadata flow (app, version, profile, base_dir)
    {Args, _} = rebar_state:command_parsed_args(State),
    Profile0 = current_profile(State),
    Profile = proplists:get_value(profile, Args, Profile0),

    Apps = rebar_state:project_apps(State),
    AppName =
        case Apps of
            [AI | _] -> binary_to_list(rebar_app_info:name(AI));
            [] -> "unknown_app"
        end,
    BaseDir = rebar_dir:base_dir(State),

    %% Prefer your RELEASES version logic when available.
    Vsn =
        case maybe_release_version(Profile, AppName) of
            {ok, S} -> S;
            _ -> "0.0.0"
        end,

    OutDir =
        case proplists:get_value(out, Args) of
            undefined -> "_build/ipfs";
            O -> O
        end,

    [
        {app, AppName},
        {version, Vsn},
        {profile, Profile},
        {base_dir, BaseDir},
        {out_dir, OutDir},
        {ipfs_bin, proplists:get_value(ipfs_bin, Args, "ipfs")},
        {pin, proplists:get_value(pin, Args, false)}
    ].

maybe_release_version(Profile, App) ->
    File = filename:join(["_build", Profile, "rel", App, "releases", "RELEASES"]),
    case file:consult(File) of
        {ok, [Terms]} ->
            case find_release_vsn(Terms, App) of
                undefined -> {error, not_found};
                V -> {ok, V}
            end;
        _ ->
            {error, no_releases}
        %% mirrors your RELEASES reader pattern. :contentReference[oaicite:2]{index=2}
    end.

find_release_vsn([H | T], App) ->
    case H of
        {release, Name, Vsn, _Erts, _Apps, permanent} ->
            case same_name(Name, App) of
                true -> to_list(Vsn);
                false -> find_release_vsn(T, App)
            end;
        _ ->
            find_release_vsn(T, App)
    end;
%% same shape as yours. :contentReference[oaicite:3]{index=3}
find_release_vsn([], _) ->
    undefined.

same_name(Name, App) when is_atom(Name) -> atom_to_list(Name) =:= App;
same_name(Name, App) when is_binary(Name) -> binary_to_list(Name) =:= App;
same_name(Name, App) when is_list(Name) -> Name =:= App.

to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(L) when is_list(L) -> L.

%% ---------- Filesystem + IPFS ----------

ensure_dir(Out) ->
    ok = filelib:ensure_dir(filename:join(Out, "placeholder")).

write_file(Path, Bin) ->
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, Bin).

shell_escape(S) when is_list(S) ->
    Needs = lists:any(fun(C) -> (C =< 32) orelse (C =:= $") orelse (C =:= $') end, S),
    case Needs of
        true -> [$' | esc_squotes(S)] ++ [$'];
        false -> S
    end;
shell_escape(B) when is_binary(B) -> shell_escape(binary_to_list(B)).
esc_squotes([]) -> [];
esc_squotes([$' | T]) -> [$', $\\, $', $' | esc_squotes(T)];
esc_squotes([H | T]) -> [H | esc_squotes(T)].
%% (escape is in the same spirit as your fpm command build) :contentReference[oaicite:4]{index=4}

ipfs_bin(Meta) -> proplists:get_value(ipfs_bin, Meta, "ipfs").
pin_flag(Meta) ->
    case proplists:get_value(pin, Meta, false) of
        true -> true;
        _ -> false
    end.

ipfs_add_dir(Bin, Dir, Pin) ->
    %% -Q quiet CIDs only, -r recursive, -n hash only? We want real add:
    Cmd = string:join(
        [
            shell_escape(Bin),
            "add",
            "-Qr"
            | (case Pin of
                true -> ["--pin"];
                _ -> []
            end)
        ] ++
            [shell_escape(Dir)],
        " "
    ),
    Out = os:cmd("sh -c " ++ shell_escape(Cmd)),
    string:trim(Out).

ipfs_add_file(Bin, Path, Pin) ->
    Cmd = string:join(
        [
            shell_escape(Bin),
            "add",
            "-Q"
            | (case Pin of
                true -> ["--pin"];
                _ -> []
            end)
        ] ++
            [shell_escape(Path)],
        " "
    ),
    Out = os:cmd("sh -c " ++ shell_escape(Cmd)),
    string:trim(Out).

%% ---------- Collect deps ----------

deps_on_disk(State) ->
    %% Reuse project_apps to locate on-disk dep roots
    [dep_info(AI) || AI <- rebar_state:project_apps(State)].

dep_info(AI) ->
    #{
        name => rebar_app_info:name(AI),
        vsn => rebar_app_info:original_vsn(AI),
        dir => rebar_app_info:dir(AI)
    }.

%% ---------- Actions ----------

add_lock(_State, Meta) ->
    BaseDir = proplists:get_value(base_dir, Meta),
    LockPath = filename:join(BaseDir, "rebar.lock"),
    case file:read_file_info(LockPath) of
        {ok, _} ->
            CID = ipfs_add_file(ipfs_bin(Meta), LockPath, pin_flag(Meta)),
            rebar_api:info("ipfs: rebar.lock -> ~s", [CID]),
            {CID, LockPath};
        _ ->
            rebar_api:info("ipfs: no rebar.lock found", []),
            {"", LockPath}
    end.

add_dep(#{name := Name, vsn := Vsn0, dir := Dir}) ->
    Vsn =
        case Vsn0 of
            undefined -> "";
            X -> X
        end,
    CID = ipfs_add_dir("ipfs", Dir, false),
    #{name => Name, vsn => Vsn, dir => Dir, cid => CID}.

manifest(Meta, LockCID, LockPath, DepResults) ->
    App = proplists:get_value(app, Meta),
    Vsn = proplists:get_value(version, Meta),
    Profile = proplists:get_value(profile, Meta),
    BaseDir = proplists:get_value(base_dir, Meta),
    #{
        app => App,
        version => Vsn,
        profile => Profile,
        base_dir => BaseDir,
        generated_at => erlang:system_time(second),
        lock => #{path => LockPath, cid => LockCID},
        deps => [dep_json(D) || D <- DepResults]
    }.

dep_json(#{name := N, vsn := V, dir := D, cid := C}) ->
    #{name => to_bin(N), vsn => to_bin(V), dir => to_bin(D), cid => to_bin(C)}.

to_bin(A) when is_atom(A) -> list_to_binary(atom_to_list(A));
to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> list_to_binary(L);
to_bin(Else) -> list_to_binary(io_lib:format("~p", [Else])).

format_stack(Stack) ->
    lists:flatten([io_lib:format("  ~p~n", [S]) || S <- Stack]).
