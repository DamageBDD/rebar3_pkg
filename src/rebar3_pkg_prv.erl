-module(rebar3_pkg_prv).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).


-define(PROVIDER, pkg).
-define(NAMESPACE, pkg).
-define(NAME, gen).
-define(DEPS, [{default, compile},{default, app_discovery}]).

init(State) ->
    Provider = providers:create(
      [{name, ?NAME},
      {namespace, ?NAMESPACE},
       {module, ?MODULE},
       {bare, true},
       {deps, ?DEPS},
       {example, "rebar3 pkg gen --target arch --version 1.2.3"},
       {opts, opts()},
       {short_desc, "Generate packaging files / packages (arch|rpm|deb)."},
       {desc, "Create distro packaging scaffolds from project metadata; optionally build with fpm."}
      ]),
    {ok, rebar_state:add_provider(State, Provider)}.

opts() ->
  [
    {target,  $t, "target",  string, "arch|rpm|deb (default: all from rebar.config)"},
    {version, $v, "version", string, "override version (default: app vsn)"},
    {arch,    $a, "arch",    string, "package arch (e.g., x86_64, aarch64)"},
    {out,     $o, "out",     string, "output directory (default: _build/pkg)"},
    {fpm,     undefined, "fpm", boolean, "use fpm to build final package"}
  ].

do(State) ->
    Cfg = cfg(State),
    rebar_api:info("pkg: projinfo (~p)", [State]),
    Targets = targets(State, Cfg),
    lists:foreach(fun(T) -> gen(State, Cfg, T) end, Targets),
    rebar_api:info("pkg: done (~p)", [Targets]),
    {ok, State}.

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
gen(State, Cfg, rpm)  -> do_rpm(State, Cfg);
gen(State, Cfg, deb)  -> do_deb(State, Cfg).

project_meta(State, Cfg) ->
    Opts = rebar_state:opts(State),
        Keys = [K || {K, _V} <- dict:to_list(Opts)],
        rebar_api:error("STATE Opts: ~p", [Keys]),
    {release, {AppName0,_}, Deps} = lists:keyfind(release, 1, dict:fetch(relx, Opts)),
    %VsnCache =  dict:fetch(vsn_cache, Opts),
    %    rebar_api:error("STATE vsn_cache: ~p", [dict:to_list(VsnCache)]),
    %Dir = rebar_dir:base_dir(State),
     %   rebar_api:error("STATE base_dir: ~p", [Dir]),
    %AppInfo = rebar_app_info:discover(Dir,State),
    
Apps = rebar_state:project_apps(State),
%% take the first app, or handle multi-app differently
[AppInfo | _] = Apps,
App = rebar_app_info:name(AppInfo),
Vsn0 = rebar_app_info:original_vsn(AppInfo),
        rebar_api:error("STATE vsn: ~p", [Vsn0]),


    AppName = atom_to_list(AppName0),

    {Args, _} = rebar_state:command_parsed_args(State),
    Vsn0    = rebar_app_info:original_vsn(AppInfo),
    Version = case proplists:get_value(version, Args) of
                undefined -> Vsn0;
                V -> V
              end,
    Arch = case proplists:get_value(arch, Args) of
             undefined -> default_arch();
             A -> A
           end,
    OutDir = case proplists:get_value(out, Args) of
               undefined -> "_build/pkg";
               O -> O
             end,
    Base = [
      {app, AppName},
      {version, Version},
      {arch, Arch},
      {out_dir, OutDir}
    ],
    Base ++ Cfg.

default_arch() ->
    case os:type() of
      {unix, linux} ->
          case os:cmd("uname -m") of
            "x86_64\n" -> "x86_64";
            "aarch64\n" -> "aarch64";
            M -> string:trim(M)
          end;
      _ -> "x86_64"
    end.

ensure_out_dir(Out) ->
    ok = filelib:ensure_dir(filename:join(Out, "placeholder")).

write_file(Path, Bin) ->
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, Bin).

render(Template, Vars) ->
    bbmustache:render(Template, Vars).

bin_path(Meta) ->
    proplists:get_value(bin, Meta,
      "_build/default/rel/" ++ proplists:get_value(app, Meta) ++
      "/bin/" ++ proplists:get_value(app, Meta)).

%% ---------- Target generators ----------

do_arch(State, Cfg) ->
    Meta = project_meta(State, Cfg),
    Out = filename:join(proplists:get_value(out_dir, Meta), "arch/" ++ proplists:get_value(app, Meta)),
    ensure_out_dir(Out),
    Vars = meta_to_vars(Meta),
    PKGBUILD = render(rebar_pkg_tmpl:pkgbuild(), Vars),
    ok = write_file(filename:join(Out, "PKGBUILD"), PKGBUILD),
    maybe_fpm(Meta, arch),
    rebar_api:info("arch: wrote ~s", [filename:join(Out, "PKGBUILD")]),
    ok.

do_rpm(State, Cfg) ->
    Meta = project_meta(State, Cfg),
    Out = filename:join(proplists:get_value(out_dir, Meta), "rpm/" ++ proplists:get_value(app, Meta)),
    ensure_out_dir(Out),
    Vars = meta_to_vars(Meta),
    Spec = render(rebar_pkg_tmpl:spec(), Vars),
    ok = write_file(filename:join(Out, proplists:get_value(app, Meta) ++ ".spec"), Spec),
    maybe_fpm(Meta, rpm),
    rebar_api:info("rpm: wrote spec"), ok.

do_deb(State, Cfg) ->
    Meta = project_meta(State, Cfg),
    Base = filename:join(proplists:get_value(out_dir, Meta), "deb/" ++ proplists:get_value(app, Meta)),
    ensure_out_dir(Base),
    Vars = meta_to_vars(Meta),
    ok = write_file(filename:join(Base, "DEBIAN/control"), render(rebar_pkg_tmpl:deb_control(), Vars)),
    ok = write_file(filename:join(Base, "DEBIAN/postinst"), render(rebar_pkg_tmpl:deb_postinst(), Vars)),
    ok = file:change_mode(filename:join(Base, "DEBIAN/postinst"), 8#755),
    maybe_fpm(Meta, deb),
    rebar_api:info("deb: wrote control & postinst"), ok.

meta_to_vars(Meta) ->
    #{
      app => proplists:get_value(app, Meta),
      version => proplists:get_value(version, Meta),
      arch => proplists:get_value(arch, Meta),
      maintainer => proplists:get_value(maintainer, Meta, "Unknown <noreply@example.org>"),
      license => proplists:get_value(license, Meta, "MIT"),
      homepage => proplists:get_value(homepage, Meta, ""),
      description => proplists:get_value(description, Meta, proplists:get_value(app, Meta)),
      install_prefix => proplists:get_value(install_prefix, Meta, "/usr"),
      service_name => proplists:get_value(service_name, Meta, proplists:get_value(app, Meta)),
      user => proplists:get_value(user, Meta, proplists:get_value(app, Meta)),
      group => proplists:get_value(group, Meta, proplists:get_value(app, Meta)),
      bin_path => bin_path(Meta),
      etc_dir => proplists:get_value(etc_dir, Meta, "/etc/" ++ proplists:get_value(app, Meta)),
      var_dir => proplists:get_value(var_dir, Meta, "/var/lib/" ++ proplists:get_value(app, Meta)),
      log_dir => proplists:get_value(log_dir, Meta, "/var/log/" ++ proplists:get_value(app, Meta)),
      unit_wants => proplists:get_value(unit_wants, Meta, "network-online.target")
    }.

maybe_fpm(Meta, Target) ->
    UseFpm = case proplists:get_value(fpm, Meta, false) of true -> true; _ -> false end,
    case UseFpm of
      true ->
        PackageType = case Target of arch -> "pacman"; rpm -> "rpm"; deb -> "deb" end,
        App = proplists:get_value(app, Meta),
        Version = proplists:get_value(version, Meta),
        Arch = proplists:get_value(arch, Meta),
        Prefix = proplists:get_value(install_prefix, Meta),
        Bin = bin_path(Meta),
        Cmd = io_lib:format(
          "fpm -s dir -t ~s -n ~s -v ~s -a ~s --prefix ~s ~s",
          [PackageType, App, Version, Arch, Prefix, Bin]),
        rebar_api:info("fpm: ~s", [Cmd]),
        os:cmd(lists:flatten(Cmd)),
        ok;
      false -> ok
    end.
