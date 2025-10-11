-module(rebar3_pkg_prv).
-behaviour(provider).
-include_lib("kernel/include/logger.hrl").

-export([init/1, do/1, format_error/1]).

-define(NAMESPACE, pkg).
-define(NAME, gen).
-define(DEPS, [{default, compile}, {default, app_discovery}, {default, release}]).

init(State) ->
    %% Register {pkg, gen} as before
    GenProvider = providers:create([
        {name, ?NAME},
        {namespace, ?NAMESPACE},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 pkg gen --target arch --version 1.2.3"},
        {opts, opts()},
        {short_desc, "Generate packaging files / packages (arch|rpm|deb). 1"},
        {desc,
            "Create distro packaging scaffolds from project metadata; optionally build with fpm."}
    ]),
    State1 = rebar_state:add_provider(State, GenProvider),

    %% NEW: also register {pkg, docker} from the standalone module
    {ok, State2} = rebar3_pkg_docker:init(State1),

    {ok, State2}.

opts() ->
    [
        {target, $t, "target", string, "arch|rpm|deb (default: all from rebar.config)"},
        {version, $v, "version", string, "override version (default: app vsn)"},
        {arch, $a, "arch", string, "package arch (e.g., x86_64, aarch64)"},
        {out, $o, "out", string, "output directory (default: _build/pkg)"},
        {fpm, undefined, "fpm", boolean, "use fpm to build final package"}
    ].

do(State) ->
    try
        Cfg = cfg(State),
        rebar_api:info("pkg: starting (targets from config/cli)", []),
        Targets = targets(State, Cfg),

        Results = [run_target(State, Cfg, T) || T <- Targets],

        {OkCnt, Errs} = summarize(Results),
        case Errs of
            [] ->
                rebar_api:info("pkg: done (~p ok)", [OkCnt]),
                {ok, State};
            _ ->
                %% Log a compact summary once more
                rebar_api:error("pkg: completed with ~p ok / ~p failed", [OkCnt, length(Errs)]),
                {error, {pkg_failed_targets, Errs}}
        end
    catch
        Class:Reason:Stack ->
            rebar_api:error(
                "pkg: fatal ~p: ~p~nStacktrace:~n~s",
                [Class, Reason, format_stack(Stack)]
            ),
            {error, {pkg_fatal, Class, Reason}}
    end.

%% Run one target with its own try/catch so one failure doesn’t kill the rest.
run_target(State, Cfg, Target) ->
    try
        rebar_api:info("pkg: generating ~p", [Target]),
        gen(State, Cfg, Target),
        rebar_api:info("pkg: ok ~p", [Target]),
        {ok, Target}
    catch
        Class:Reason:Stack ->
            rebar_api:error(
                "pkg: FAILED ~p~n  Class: ~p~n  Reason: ~p~n  Stack:~n~s",
                [Target, Class, Reason, format_stack(Stack)]
            ),
            {error, Target, Class, Reason, Stack}
    end.

summarize(Results) ->
    OkCnt = length([ok || {ok, _} <- Results]),
    Errs = [{T, C, R, S} || {error, T, C, R, S} <- Results],
    {OkCnt, Errs}.

format_stack(Stack) ->
    lists:flatten([io_lib:format("  ~p~n", [S]) || S <- Stack]).

format_error(Reason) ->
    io_lib:format("pkg error: ~p", [Reason]).

%% ---------- helpers ----------

cfg(State) ->
    %% {pkg, [...] } from rebar.config (dict-backed)
    rebar_state:get(State, pkg, []).

%% Read CLI flag --target from parsed args; fall back to config list {targets, [...] }.
targets(State, Cfg) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(target, Args) of
        undefined ->
            case lists:keyfind(targets, 1, Cfg) of
                {targets, Ts} -> Ts;
                false -> [arch, rpm, deb]
            end;
        TStr when is_list(TStr) ->
            [list_to_atom(TStr)];
        T when is_atom(T) ->
            [T]
    end.

gen(State, Cfg, arch) -> do_arch(State, Cfg);
gen(State, Cfg, rpm) -> do_rpm(State, Cfg);
gen(State, Cfg, deb) -> do_deb(State, Cfg).

project_meta(State, Cfg) ->
    %% Active profile & build base dir
    Profiles = rebar_state:current_profiles(State),
    Profile =
        case Profiles of
            [P | _] -> atom_to_list(P);
            [] -> "default"
        end,
    %% e.g. "_build/prod"
    BaseDir = rebar_dir:base_dir(State),

    %% CLI args (parsed)
    {Args, _} = rebar_state:command_parsed_args(State),

    %% Discover the main app & version (prefer project app; fallback to relx; then defaults)
    %Apps = rebar_state:project_apps(State),
    Opts = rebar_state:opts(State),
    Keys = [K || {K, _V} <- dict:to_list(Opts)],
    rebar_api:info("Pkg Opts: ~p", [Keys]),
    {release, {AppName0, _}, _Deps} = lists:keyfind(release, 1, dict:fetch(relx, Opts)),
    Apps = rebar_state:project_apps(State),
    [AppInfo | _] = Apps,
    rebar_api:info("appinfo: ~p", [AppInfo]),
    Vsn0 = rebar_app_info:original_vsn(AppInfo),

    %% Allow overrides from CLI
    Profile1 =
        case proplists:get_value(profile, Args) of
            undefined -> Profile;
            P0 -> P0
        end,
    Version =
        case proplists:get_value(version, Args) of
            undefined -> Vsn0;
            V -> V
        end,
    Arch =
        case proplists:get_value(arch, Args) of
            undefined -> default_arch();
            A0 -> A0
        end,
    OutDir =
        case proplists:get_value(out, Args) of
            undefined -> "_build/pkg";
            O -> O
        end,

    %% Log (optional)
    rebar_api:info("pkg: app=~s vsn=~s profile=~s basedir=~s", [
        AppName0, Version, Profile1, BaseDir
    ]),

    %% Meta used by generators/templates
    [
        {app, AppName0},
        {version, Version},
        {arch, Arch},
        {out_dir, OutDir},
        {profile, Profile1},
        {base_dir, BaseDir}
    ] ++ Cfg.

default_arch() ->
    case os:type() of
        {unix, linux} ->
            case os:cmd("uname -m") of
                "x86_64\n" -> "x86_64";
                "aarch64\n" -> "aarch64";
                M -> string:trim(M)
            end;
        _ ->
            "x86_64"
    end.

ensure_out_dir(Out) ->
    ok = filelib:ensure_dir(filename:join(Out, "placeholder")).

write_file(Path, Bin) ->
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, Bin).

tmpl_path(RelPath) ->
    Priv = application:get_env(rebar3_pkg, priv_dir, code:priv_dir(rebar3_pkg)),
    filename:join([Priv, "templates", RelPath]).

render_file(RelPath, Vars) ->
    Path = tmpl_path(RelPath),
    case file:read_file(Path) of
        {ok, Bin} -> 
                            rebar_api:debug("template context ~p", [Vars]),

bbmustache:render(Bin, normalize_context(Vars));
        _Error     -> error(Path)
    end.

bin_path(Meta) ->
    App = safe_get(app, Meta, "unknown_app"),
    BaseDir = safe_get(
        base_dir,
        Meta,
        filename:join("_build", safe_get(profile, Meta, "default"))
    ),
    join_all([BaseDir, "rel", App, "bin", App]).

safe_get(Key, Meta, Default) ->
    case proplists:get_value(Key, Meta) of
        undefined -> Default;
        V when is_list(V), V =/= [] -> V;
        V0 when is_atom(V0) -> atom_to_list(V0);
        _ -> Default
    end.

join_all(Parts0) ->
    %% normalize all parts to non-empty strings
    Parts = [P || P <- [normalize(Pt) || Pt <- Parts0], P =/= ""],
    filename:join(Parts).

normalize_context(Context) when is_map(Context) ->
    maps:from_list([{key_to_string(K), V} || {K, V} <- maps:to_list(Context)]);
normalize_context(Context) when is_list(Context) ->
    maps:from_list([{key_to_string(K), V} || {K, V} <- Context]);
normalize_context({K, V}) ->
    maps:from_list([{key_to_string(K), V}]);
normalize_context(_) ->
    #{}.
normalize(undefined) -> "";
normalize([]) -> "";
normalize(A) when is_atom(A) -> atom_to_list(A);
normalize(B) when is_binary(B) -> binary_to_list(B);
normalize(S) when is_list(S) -> S;
%% last resort, never empty
normalize(Other) -> io_lib:format("~p", [Other]).
key_to_string(K) when is_atom(K) ->
    atom_to_list(K);
key_to_string(K) when is_binary(K) ->
    binary_to_list(K);
key_to_string(K) when is_list(K) ->
    %% already a string
    K;
key_to_string(K) ->
    %% fallback to string
    io_lib:format("~p", [K]).
to_map(M) when is_map(M) -> M;
to_map(L) when is_list(L) -> maps:from_list(L).

merge_meta(Base, Adds) ->
    maps:merge(to_map(Base), to_map(Adds)).


%% ---------- Target generators ----------

do_deb(State, Cfg) ->
    Meta   = project_meta(State, Cfg),
    OutDir = safe_get(out_dir, Meta, "_build/pkg"),
    App    = safe_get(app, Meta, "unknown_app"),
    Base   = join_all([OutDir, "deb", App]),
    ensure_out_dir(Base),
    Vars   = meta_to_vars(Meta),

    ok = write_file(join_all([Base, "DEBIAN", "control"]),
                    render_file("deb/control.mustache", Vars)),
    Postinst = join_all([Base, "DEBIAN", "postinst"]),
    ok = write_file(Postinst, render_file("deb/postinst.mustache", Vars)),
    ok = file:change_mode(Postinst, 8#755),

    %% collect CLI/config fpm flag and ensure proper install_prefix for runtime
    {Args, _} = rebar_state:command_parsed_args(State),
    FpmFlag   = case proplists:get_value(fpm, Args) of
                    true  -> true;
                    false -> false;
                    undefined ->
                        %% fallback to config `{pkg, [{fpm, true}|...]}` or default true
                        proplists:get_value(fpm, Cfg, true)
                end,

    %% prefer /opt/<app> when bundling a relx release
    InstallPrefix = proplists:get_value(install_prefix, Meta, filename:join("/opt", App)),

    FpmMeta = merge_meta(Vars, #{
        fpm => FpmFlag,
        after_install => Postinst,
        install_prefix => InstallPrefix
    }),

    maybe_fpm(FpmMeta, deb),
    rebar_api:info("deb: wrote control & postinst in ~s", [Base]),
    ok.


do_arch(State, Cfg) ->
    Meta  = project_meta(State, Cfg),
    OutDir= safe_get(out_dir, Meta, "_build/pkg"),
    App   = safe_get(app, Meta, "unknown_app"),
    Out   = join_all([OutDir, "arch", App]),
    ensure_out_dir(Out),
    Vars  = meta_to_vars(Meta),

    ok = write_file(join_all([Out, "PKGBUILD"]),
                    render_file("arch/PKGBUILD.mustache", Vars)),
    maybe_fpm(Vars, arch),
    rebar_api:info("arch: wrote ~s", [join_all([Out, "PKGBUILD"])]),
    ok.

do_rpm(State, Cfg) ->
    Meta  = project_meta(State, Cfg),
    OutDir= safe_get(out_dir, Meta, "_build/pkg"),
    App   = safe_get(app, Meta, "unknown_app"),
    Out   = join_all([OutDir, "rpm", App]),
    ensure_out_dir(Out),
    Vars  = meta_to_vars(Meta),

    ok = write_file(join_all([Out, App ++ ".spec"]),
                    render_file("rpm/spec.mustache", Vars)),
    maybe_fpm(Vars, rpm),
    rebar_api:info("rpm: wrote spec to ~s", [Out]),
    ok.

meta_to_vars(Meta) ->
    App = safe_get(app, Meta, "app"),
    #{
      app => App,
      version => proplists:get_value(version, Meta),
      arch => proplists:get_value(arch, Meta),
      maintainer => proplists:get_value(maintainer, Meta, "Unknown <noreply@example.org>"),
      license => proplists:get_value(license, Meta, "MIT"),
      homepage => proplists:get_value(homepage, Meta, ""),
      description => proplists:get_value(description, Meta, App),
      install_prefix => proplists:get_value(install_prefix, Meta, "/usr"),
      service_name => proplists:get_value(service_name, Meta, App),
      create_user => proplists:get_value(create_user, Meta, true),
      user => proplists:get_value(user, Meta, App),
      group => proplists:get_value(group, Meta, App),
      bin_path => bin_path(Meta),
      etc_dir => proplists:get_value(etc_dir, Meta, "/etc/" ++ App),
      var_dir => proplists:get_value(var_dir, Meta, "/var/lib/" ++ App),
      log_dir => proplists:get_value(log_dir, Meta, "/var/log/" ++ App),
      unit_wants => proplists:get_value(unit_wants, Meta, "network-online.target"),
      out_dir => proplists:get_value(out_dir, Meta, "_build/pkg"),
      auto_start => proplists:get_value(auto_start, Meta, true),

      %% NEW: kerl bootstrap knobs
      otp_version => proplists:get_value(otp_version, Meta, "28.0.2"),
      rebar3_url  => proplists:get_value(rebar3_url,  Meta, "https://s3.amazonaws.com/rebar3/rebar3")
    }.


%% ---- fpm integration (fpm >= 1.17.0) -------------------------------

maybe_fpm(Meta, Target) ->
    UseFpm = (maps:get(fpm, Meta, true) =:= true),
    case UseFpm of
        false ->
            rebar_api:info("Not using fpm.", []),
            ok;
        true ->
            App      = maps:get(app, Meta),
            Version  = maps:get(version, Meta),
            Arch     = maps:get(arch, Meta, "native"),
            Prefix   = maps:get(install_prefix, Meta, filename:join("/opt", App)),
            Bin      = maps:get(bin_path, Meta),

            BinDir   = filename:dirname(Bin),
            RelDir   = filename:dirname(BinDir),

            OutBase  = maps:get(out_dir, Meta, "_build/pkg"),
            Maint    = maps:get(maintainer,  Meta, undefined),
            Lic      = maps:get(license,     Meta, undefined),
            Url      = maps:get(homepage,    Meta, undefined),
            Desc     = maps:get(description, Meta, undefined),

            AI = maps:get(after_install,  Meta, undefined),
            BI = maps:get(before_install, Meta, undefined),
            AR = maps:get(after_remove,   Meta, undefined),
            BR = maps:get(before_remove,  Meta, undefined),

            TypeStr   = target_to_type(Target),
            TargetDir = filename:join(OutBase, type_to_dir(Target)),
            ok = filelib:ensure_dir(filename:join(TargetDir, "placeholder")),

            Pattern = filename:join([TargetDir, App ++ "*"]),
            rebar_api:info("fpm: cleaning old packages: ~s", [Pattern]),
            lists:foreach(fun file:delete/1, filelib:wildcard(Pattern)),

            OutArg = filename:join(TargetDir, "") ++ "/",

            BaseArgs = [
                "fpm","-s","dir",
                "-t", TypeStr,
                "-n", App,
                "-v", Version,
                "-a", Arch,
                "--prefix", Prefix,
                "-p", OutArg,
                "--force"
            ],

            OptMeta =
                add_opt("--maintainer", Maint,
                add_opt("--license",    Lic,
                add_opt("--url",        Url,
                add_opt("--description",Desc, [])))),

            ScriptMeta =
                add_opt("--after-install",  AI,
                add_opt("--before-install", BI,
                add_opt("--after-remove",   AR,
                add_opt("--before-remove",  BR, [])))),

            Argv = BaseArgs ++ OptMeta ++ ScriptMeta ++ ["-C", RelDir, "."],
            Cmd  = string:join([shell_escape(A) || A <- Argv], " "),

            rebar_api:info("fpm cmd: ~s", [Cmd]),

            Full = "sh -c " ++ shell_escape(Cmd ++ " ; printf '\\nEXIT:%s' $?"),
            Out  = os:cmd(Full),

            %% --- no guards calling functions ---
            Lines    = string:tokens(Out, "\n"),
            ExitLine = case Lines of [] -> "EXIT:255"; _ -> lists:last(Lines) end,
            IsExit   = lists:prefix("EXIT:", ExitLine),

            case IsExit of
                true ->
                    CodeStr = string:substr(ExitLine, 6),
                    case (catch list_to_integer(CodeStr)) of
                        0 ->
                            rebar_api:info("fpm: wrote package(s) under ~s", [TargetDir]),
                            ok;
                        N when is_integer(N) ->
                            rebar_api:error("fpm failed (exit ~p). Output:~n~s", [N, Out]),
                            {error, {fpm_failed, N}};
                        _ ->
                            rebar_api:error("fpm: could not parse exit code. Output:~n~s", [Out]),
                            {error, fpm_exit_unknown}
                    end;
                false ->
                    rebar_api:error("fpm: unexpected output. Output:~n~s", [Out]),
                    {error, fpm_output_unexpected}
            end
    end.




target_to_type(arch) -> "pacman";
target_to_type(rpm) -> "rpm";
target_to_type(deb) -> "deb".

type_to_dir(arch) -> "arch";
type_to_dir(rpm) -> "rpm";
type_to_dir(deb) -> "deb".

add_opt(_Flag, undefined, Acc) -> Acc;
add_opt(_Flag, [], Acc) -> Acc;
add_opt(Flag, Val, Acc) -> Acc ++ [Flag, Val].

shell_escape(S) when is_list(S) ->
    %% If S has whitespace or quotes, wrap it in single quotes and
    %% escape any embedded single-quotes as: '\''  (end, backslash-quote, start)
    NeedsQuoting =
        lists:any(
            fun(C) ->
                (C =< 32) orelse (C =:= $") orelse (C =:= $\')
            end,
            S
        ),
    case NeedsQuoting of
        true -> [$' | escape_squotes(S)] ++ [$'];
        false -> S
    end;
shell_escape(B) when is_binary(B) -> shell_escape(binary_to_list(B));
shell_escape(A) when is_atom(A) -> shell_escape(atom_to_list(A)).

escape_squotes([]) -> [];
escape_squotes([$' | T]) -> [$', $\\, $', $' | escape_squotes(T)];
escape_squotes([H | T]) -> [H | escape_squotes(T)].
