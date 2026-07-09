-module(rebar3_pkg_version_prv).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(NAMESPACE, pkg).
-define(NAME, version).
-define(DEPS, [{default, compile}, {default, app_discovery}, {default, release}]).

init(State) ->
    Provider = providers:create([
        {name, ?NAME},
        {namespace, ?NAMESPACE},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 pkg version"},
        {opts, []},
        {short_desc, "Print the package version resolver diagnostics"},
        {desc, "Prints the expected package version, git directory, HEAD tags, and valid semver tags."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    Info = rebar3_pkg_prv:version_info(State),
    App = printable(maps:get(app, Info, undefined)),
    Version = printable(maps:get(version, Info, undefined)),
    Profile = printable(maps:get(profile, Info, undefined)),
    BaseDir = printable(maps:get(base_dir, Info, undefined)),
    GitDir = printable(maps:get(git_dir, Info, undefined)),
    Tags = maps:get(git_tags, Info, []),
    SemverTags = maps:get(semver_tags, Info, []),
    Source = printable(maps:get(source, Info, undefined)),

    rebar_api:info("pkg: app=~s profile=~s", [App, Profile]),
    rebar_api:info("pkg: base_dir=~s", [BaseDir]),
    rebar_api:info("pkg: git_dir=~s", [GitDir]),
    rebar_api:info("pkg: HEAD tags=~p", [Tags]),
    rebar_api:info("pkg: valid semver tags=~p", [SemverTags]),
    rebar_api:info("pkg: version source=~s", [Source]),
    rebar_api:info("pkg: expected version=~s", [Version]),
    {ok, State}.

format_error(Reason) ->
    io_lib:format("pkg version error: ~p", [Reason]).

printable(undefined) -> "undefined";
printable(A) when is_atom(A) -> atom_to_list(A);
printable(B) when is_binary(B) -> binary_to_list(B);
printable(L) when is_list(L) -> L;
printable(Other) -> lists:flatten(io_lib:format("~p", [Other])).
