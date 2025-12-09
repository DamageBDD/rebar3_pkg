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
    {ok, State3} = rebar3_ipfs_prv:init(State2),

    {ok, State3}.

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

current_profile(State) ->
    case rebar_state:current_profiles(State) of
        % Currently only support one profile at a time
        [default, P] -> atom_to_list(P);
        [default] -> "default";
        [] -> "default"
    end.

maybe_release_app_from_config(State) ->
    %% Try to find {release, {App,_},_} or {release, App,_} in config
    try
        Opts = rebar_state:opts(State),
        Relx =
            case dict:find(relx, Opts) of
                {ok, R} ->
                    R;
                error ->
                    case rebar_state:project_config(State) of
                        PC when is_list(PC) -> proplists:get_value(relx, PC, []);
                        _ -> []
                    end
            end,
        case lists:keyfind(release, 1, Relx) of
            {release, {App, _}, _} when is_atom(App) -> {ok, App};
            {release, App, _} when is_atom(App) -> {ok, App};
            _ -> error
        end
    catch
        _:_ -> error
    end.

maybe_release_app_from_build(Profile) ->
    RelDir = filename:join(["_build", Profile, "rel"]),
    case file:list_dir(RelDir) of
        {ok, Entries} ->
            Cands = [E || E <- Entries, E =/= "lib", not lists:prefix("erts-", E)],
            case Cands of
                [Name | _] -> {ok, list_to_atom(Name)};
                [] -> error
            end;
        _ ->
            error
    end.

find_appinfo(App, Apps) ->
    %% Apps :: [rebar_app_info()]
    Apps0 = [{rebar_app_info:name(AI), AI} || AI <- Apps],
    case lists:keyfind(atom_to_binary(App), 1, Apps0) of
        false -> undefined;
        {_Name, AI} -> AI
    end.

%% ---------- main ----------
%% @doc Get the release version from _build/<Profile>/rel/<App>/releases/RELEASES
-spec release_version(
    Profile :: string() | atom(),
    App :: atom() | string()
) ->
    {ok, string()} | {error, term()}.
release_version(Profile0, App0) ->
    Profile = to_list(Profile0),
    AppName = to_list(App0),
    File = filename:join(["_build", Profile, "rel", AppName, "releases", "RELEASES"]),
    rebar_api:info("RELEASE file ~p", [File]),
    case file:consult(File) of
        {ok, [[{release, AppName, Version, _ErtsVsn, _Apps, permanent}]]} ->
            {ok, Version};
        Error ->
            Error
    end.

%% ---- helpers ----

to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(L) when is_list(L) -> L.

project_meta(State, Cfg) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    rebar_api:info("Args ~p", [Args]),

    Profile0 = current_profile(State),
    Profile =
        case proplists:get_value(profile, Args) of
            undefined -> Profile0;
            P -> P
        end,

    %% Figure out the release app deterministically.
    AppAtom0 =
        case maybe_release_app_from_config(State) of
            {ok, A} ->
                A;
            error ->
                case maybe_release_app_from_build(Profile) of
                    {ok, A} ->
                        A;
                    error ->
                        case rebar_state:current_app(State) of
                            undefined ->
                                case rebar_state:project_apps(State) of
                                    [AI | _] -> rebar_app_info:name(AI);
                                    [] -> undefined
                                end;
                            AI ->
                                rebar_app_info:name(AI)
                        end
                end
        end,

    AppAtom = AppAtom0,
    Apps = rebar_state:project_apps(State),
    AppInfo =
        case AppAtom of
            undefined -> undefined;
            A0 -> find_appinfo(A0, Apps)
        end,

    Version =
        case AppAtom of
            undefined ->
                rebar_api:info("Arg Version ~p", [Args]),
                proplists:get_value(version, Args, "0.0.0");
            A1 ->
                {ok, Vsn} = release_version(Profile, A1),
                rebar_api:info("Release Version ~p", [Vsn]),
                Vsn
        end,

    Arch =
        case proplists:get_value(arch, Args) of
            undefined -> default_arch();
            A2 -> A2
        end,
    OutDir =
        case proplists:get_value(out, Args) of
            undefined -> "_build/pkg";
            O -> O
        end,

    AppName =
        case AppAtom of
            undefined -> "unknown_app";
            A3 -> atom_to_list(A3)
        end,
    BaseDir = rebar_dir:base_dir(State),
    AppDetails = rebar_app_info:app_details(AppInfo),
    Maintainer = proplists:get_value(maintainer, AppDetails),
    Links = proplists:get_value(links, AppDetails),
    Licenses = string:join(proplists:get_value(licenses, AppDetails), " "),
    Description = proplists:get_value(description, AppDetails),

    %rebar_api:info(
    %    "state: ~p cfg: ~p",
    %    [State, Cfg]
    %),
    rebar_api:info(
        "pkg: app=~s vsn=~s profile=~s basedir=~s maintainer=~p",
        [AppName, Version, Profile, BaseDir, Maintainer]
    ),

    [
        {app, AppName},
        {version, Version},
        {maintainer, Maintainer},
        {arch, Arch},
        {out_dir, OutDir},
        {profile, Profile},
        {description, Description},
        {base_dir, BaseDir},
        {links, Links},
        {licenses, Licenses}
    ] ++ Cfg.

meta_to_vars(Meta) ->
    App = safe_get(app, Meta, "app"),
    #{
        app => App,
        version => proplists:get_value(version, Meta),
        arch => proplists:get_value(arch, Meta),
        maintainer => proplists:get_value(maintainer, Meta, "Unknown <noreply@example.org>"),
        licenses => proplists:get_value(licenses, Meta, ["MIT"]),
        links => proplists:get_value(links, Meta, ""),
        homepage => proplists:get_value(homepage, Meta, ""),
        description => proplists:get_value(description, Meta, App),
        install_prefix => proplists:get_value(install_prefix, Meta, "/opt"),
        service_name => proplists:get_value(service_name, Meta, App),
        create_user => proplists:get_value(create_user, Meta, "true"),
        user => proplists:get_value(user, Meta, App),
        group => proplists:get_value(group, Meta, App),
        bin_path => bin_path(Meta),
        base_dir => proplists:get_value(base_dir, Meta),
        etc_dir => proplists:get_value(etc_dir, Meta, "/etc/" ++ App),
        var_dir => proplists:get_value(var_dir, Meta, "/var/lib/" ++ App),
        log_dir => proplists:get_value(log_dir, Meta, "/var/log/" ++ App),
        systemd_unit => proplists:get_value(systemd_unit, Meta, ""),
        unit_wants => proplists:get_value(unit_wants, Meta, "network-online.target"),
        out_dir => proplists:get_value(out_dir, Meta, "_build/pkg"),
        auto_start => proplists:get_value(auto_start, Meta, "true"),

        depends => proplists:get_value(depends, Meta, ""),
        recommends => proplists:get_value(recommends, Meta, ""),
        suggests => proplists:get_value(suggests, Meta, ""),
        %% NEW: kerl bootstrap knobs
        otp_version => proplists:get_value(otp_version, Meta, "28.0.2"),
        rebar3_url => proplists:get_value(
            rebar3_url, Meta, "https://s3.amazonaws.com/rebar3/rebar3"
        )
    }.

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
            bbmustache:render(Bin, normalize_context(Vars));
        _Error ->
            error(Path)
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
%% Collect platform-specific specs as a flat list of strings
plat_specs(Meta, Key, Platform) ->
    Raw = proplists:get_value(Key, Meta, []),
    Items =
        case Raw of
            undefined -> [];
            % single string
            S when is_list(S), S =/= [], is_integer(hd(S)) -> [S];
            % list [...]
            L when is_list(L) -> L;
            A when is_atom(A) -> [atom_to_list(A)];
            B when is_binary(B) -> [binary_to_list(B)];
            _ -> []
        end,
    lists:append([to_keep(I, Platform) || I <- Items]).

%% Keep only entries for the current platform (or 'all'), and
%% always return a *list of strings*.
to_keep({Plat, Spec}, Platform) when Plat =:= Platform -> normalize_spec(Spec);
to_keep({all, Spec}, _Platform) -> normalize_spec(Spec);
to_keep({_, _}, _Platform) -> [];
to_keep(Spec, _Platform) -> normalize_spec(Spec).

%% Normalize one item:
%%  - If it's a single string/atom/binary -> [String]
%%  - If it's a list of strings -> [String, ...]
normalize_spec(S) when is_list(S), S =/= [], is_integer(hd(S)) ->
    % a single string
    [S];
normalize_spec(L) when is_list(L) ->
    % list of strings
    [to_str(E) || E <- L];
normalize_spec(S) ->
    % single non-list
    [to_str(S)].

to_str(S) when is_list(S) -> S;
to_str(S) when is_atom(S) -> atom_to_list(S);
to_str(S) when is_binary(S) -> binary_to_list(S).

%% Debian control field joiner: "a, b, c" or "" if none
-spec deb_join([string()]) -> string().
deb_join([]) -> "";
deb_join(L) -> string:join(L, ", ").

%% Arch PKGBUILD helper:
%% Return **just the quoted items**: "'a' 'b'" (no parentheses).
%% Let the template add the surrounding ().
-spec arch_array_items([string()]) -> string().
arch_array_items([]) -> "";
arch_array_items(L) -> "('" ++ string:join(L, "' '") ++ "')".

%% ---------- Target generators ----------

do_deb(State, Cfg) ->
    Meta = project_meta(State, Cfg),
    OutDir = safe_get(out_dir, Meta, "_build/pkg"),
    App = safe_get(app, Meta, "unknown_app"),
    Base = join_all([OutDir, "deb", App]),
    ensure_out_dir(Base),

    %% Base Vars
    Vars0 = meta_to_vars(Meta),

    %% Resolve Debian fields
    DebDepends = deb_join(plat_specs(Meta, depends, deb)),
    DebRecommends = deb_join(plat_specs(Meta, recommends, deb)),
    DebSuggests = deb_join(plat_specs(Meta, suggests, deb)),

    %% Vars for control.mustache
    Vars = Vars0#{
        depends => DebDepends,
        recommends => DebRecommends,
        suggests => DebSuggests
    },

    %% control
    ok = write_file(
        join_all([Base, "DEBIAN", "control"]),
        render_file("deb/control.mustache", Vars)
    ),

    %% main postinst
    Postinst = join_all([Base, "DEBIAN", "postinst"]),
    ok = write_file(Postinst, render_file("deb/postinst.mustache", Vars)),
    ok = file:change_mode(Postinst, 8#755),

    %% OPTIONAL: copy postinst.d to release tree so fpm can package it
    ok = maybe_copy_postinst_d(Meta, Vars),

    %% fpm toggle
    {Args, _} = rebar_state:command_parsed_args(State),
    FpmFlag =
        case proplists:get_value(fpm, Args) of
            true -> true;
            false -> false;
            undefined -> proplists:get_value(fpm, Cfg, true)
        end,

    InstallPrefix = proplists:get_value(install_prefix, Meta, "/opt"),

    FpmMeta = merge_meta(Vars, #{
        fpm => FpmFlag,
        after_install => Postinst,
        install_prefix => InstallPrefix
    }),

    maybe_fpm(FpmMeta, deb),
    rebar_api:info("deb: wrote control, postinst~s in ~s",
        [maybe_postinst_d_suffix(Meta), Base]),
    ok.



do_arch(State, Cfg) ->
    Meta = project_meta(State, Cfg),
    OutDir = safe_get(out_dir, Meta, "_build/pkg"),
    App = safe_get(app, Meta, "unknown_app"),
    Out = join_all([OutDir, "arch", App]),
    ensure_out_dir(Out),

    %% Base Vars
    Vars0 = meta_to_vars(Meta),

    %% Resolve Arch fields
    ArchDependsL = plat_specs(Meta, depends, arch),
    ArchRecommL = plat_specs(Meta, recommends, arch),
    ArchSuggestsL = plat_specs(Meta, suggests, arch),
    rebar_api:info("do arch ~p ~p", [ArchSuggestsL, ArchDependsL]),
    ArchDepends = arch_array_items(ArchDependsL),
    ArchOptdepends = arch_array_items(ArchRecommL ++ ArchSuggestsL),
    rebar_api:info("do arch ~p ~p", [ArchDepends, ArchOptdepends]),

    %% Vars for PKGBUILD.mustache & .install
    Vars = Vars0#{
        depends => ArchDepends,
        optdepends => ArchOptdepends
    },

    Postinst = join_all([Out, App ++ ".install"]),
    ok = write_file(Postinst, render_file("arch/pkg.install.mustache", Vars)),
    ok = file:change_mode(Postinst, 8#755),

    ok = write_file(
        join_all([Out, "PKGBUILD"]),
        render_file("arch/PKGBUILD.mustache", Vars)
    ),

    %% fpm toggle
    {Args, _} = rebar_state:command_parsed_args(State),
    FpmFlag =
        case proplists:get_value(fpm, Args) of
            true -> true;
            false -> false;
            undefined -> proplists:get_value(fpm, Cfg, true)
        end,

    InstallPrefix = proplists:get_value(install_prefix, Meta, "/opt"),
    FpmMeta = merge_meta(Vars, #{
        fpm => FpmFlag,
        after_install => Postinst,
        install_prefix => InstallPrefix
    }),

    maybe_fpm(FpmMeta, arch),
    rebar_api:info("arch: wrote ~s", [join_all([Out, "PKGBUILD"])]),
    ok.

do_rpm(State, Cfg) ->
    Meta = project_meta(State, Cfg),
    OutDir = safe_get(out_dir, Meta, "_build/pkg"),
    App = safe_get(app, Meta, "unknown_app"),
    Out = join_all([OutDir, "rpm", App]),
    ensure_out_dir(Out),
    Vars = meta_to_vars(Meta),

    ok = write_file(
        join_all([Out, App ++ ".spec"]),
        render_file("rpm/spec.mustache", Vars)
    ),
    maybe_fpm(Vars, rpm),
    rebar_api:info("rpm: wrote spec to ~s", [Out]),
    ok.

%% ---- fpm integration (fpm >= 1.17.0) -------------------------------
strip_trailing_slash(P) ->
    case lists:last(P) of
        $/ -> lists:sublist(P, 1, length(P) - 1);
        _ -> P
    end.

maybe_fpm(Meta, Target) ->
    UseFpm = (maps:get(fpm, Meta, true) =:= true),
    case UseFpm of
        false ->
            rebar_api:info("Not using fpm.", []),
            ok;
        true ->
            App = maps:get(app, Meta),
            % Normalize prefix so we always get /opt/<app>
            Prefix0 = maps:get(install_prefix, Meta, "/opt"),
            Prefix1 = strip_trailing_slash(Prefix0),
            Prefix =
                case filename:basename(Prefix1) of
                    % already /opt/<app>
                    App -> Prefix1;
                    % becomes /opt/<app>
                    _ -> filename:join(Prefix1, App)
                end,
            Version = maps:get(version, Meta),
            Arch = maps:get(arch, Meta, "native"),

            Bin = maps:get(bin_path, Meta),
            BinDir = filename:dirname(Bin),
            RelDir = filename:dirname(BinDir),

            OutBase = maps:get(out_dir, Meta, "_build/pkg"),
            Maint = maps:get(maintainer, Meta, undefined),
            Lic = maps:get(license, Meta, undefined),
            Url = maps:get(homepage, Meta, undefined),
            Desc = maps:get(description, Meta, undefined),

            AI = maps:get(after_install, Meta, undefined),
            BI = maps:get(before_install, Meta, undefined),
            AR = maps:get(after_remove, Meta, undefined),
            BR = maps:get(before_remove, Meta, undefined),

            TypeStr = target_to_type(Target),
            TargetDir = filename:join(OutBase, type_to_dir(Target)),
            ok = filelib:ensure_dir(filename:join(TargetDir, "placeholder")),

            Pattern = filename:join([TargetDir, App ++ "*"]),
            rebar_api:info("fpm: cleaning old packages: ~s", [Pattern]),
            lists:foreach(fun file:delete/1, filelib:wildcard(Pattern)),

            OutArg = filename:join(TargetDir, "") ++ "/",

            BaseArgs = [
                "fpm",
                "-s",
                "dir",
                "-t",
                TypeStr,
                "-n",
                App,
                "-v",
                Version,
                "-a",
                Arch,
                "--prefix",
                Prefix,
                "-p",
                OutArg,
                "--force"
            ],

            OptMeta =
                add_opt(
                    "--maintainer",
                    Maint,
                    add_opt(
                        "--license",
                        Lic,
                        add_opt(
                            "--url",
                            Url,
                            add_opt("--description", Desc, [])
                        )
                    )
                ),

            ScriptMeta =
                add_opt(
                    "--after-install",
                    AI,
                    add_opt(
                        "--before-install",
                        BI,
                        add_opt(
                            "--after-remove",
                            AR,
                            add_opt("--before-remove", BR, [])
                        )
                    )
                ),

            Argv = BaseArgs ++ OptMeta ++ ScriptMeta ++ ["-C", RelDir, "."],
            Cmd = string:join([shell_escape(A) || A <- Argv], " "),

            rebar_api:info("fpm cmd: ~s", [Cmd]),

            Full = "sh -c " ++ shell_escape(Cmd ++ " ; printf '\\nEXIT:%s' $?"),
            Out = os:cmd(Full),

            %% --- no guards calling functions ---
            Lines = string:tokens(Out, "\n"),
            ExitLine =
                case Lines of
                    [] -> "EXIT:255";
                    _ -> lists:last(Lines)
                end,
            IsExit = lists:prefix("EXIT:", ExitLine),

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
%% If {postinst_d, Dir} is set in Meta, copy Dir -> <release>/postinst.d
%% so FPM will include it under ${install_prefix}/${app}/postinst.d
maybe_copy_postinst_d(Meta, Vars) ->
    case proplists:get_value(postinst_d, Meta) of
        undefined ->
            ok;
        [] ->
            ok;
        Src ->
            case filelib:is_dir(Src) of
                false ->
                    rebar_api:warn(
                        "deb: postinst_d path ~s is not a directory, skipping",
                        [Src]
                    ),
                    ok;
                true ->
                    %% RelDir is what maybe_fpm/2 uses as -C <RelDir> .
                    Bin = maps:get(bin_path, Vars),
                    BinDir = filename:dirname(Bin),
                    RelDir = filename:dirname(BinDir),

                    Dest = filename:join(RelDir, "postinst.d"),
                    ok = filelib:ensure_dir(filename:join(Dest, "placeholder")),
                    copy_postinst_dir(Src, Dest)
            end
    end.

copy_postinst_dir(Src, Dest) ->
    %% cp -a "$Src/." "$Dest/"
    Argv = ["cp", "-a", Src ++ "/.", Dest ++ "/"],
    Cmd = string:join([shell_escape(A) || A <- Argv], " "),
    rebar_api:info("deb: copying postinst_d via: ~s", [Cmd]),
    Out = os:cmd(Cmd),
    rebar_api:info("deb: postinst_d copy output:~n~s", [Out]),
    ok.

maybe_postinst_d_suffix(Meta) ->
    case proplists:get_value(postinst_d, Meta) of
        undefined -> "";
        [] -> "";
        _ -> " & postinst.d"
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
