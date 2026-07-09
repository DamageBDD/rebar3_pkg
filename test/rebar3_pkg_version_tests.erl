-module(rebar3_pkg_version_tests).

-include_lib("eunit/include/eunit.hrl").

parse_valid_semver_tags_test_() ->
    [
        ?_assertMatch({ok, "1.7.0-rc1", _}, rebar3_pkg_version:parse_semver_tag("v1.7.0-rc1")),
        ?_assertMatch({ok, "1.7.0", _}, rebar3_pkg_version:parse_semver_tag("1.7.0")),
        ?_assertMatch({ok, "1.7.0+build.1", _}, rebar3_pkg_version:parse_semver_tag("v1.7.0+build.1"))
    ].

reject_non_semver_tags_test_() ->
    [
        ?_assertEqual(error, rebar3_pkg_version:parse_semver_tag("nsecbunker-phase4-packaging-ready")),
        ?_assertEqual(error, rebar3_pkg_version:parse_semver_tag("nsecbunker-phase4-packaging-ready+build.1.ref747756e")),
        ?_assertEqual(error, rebar3_pkg_version:parse_semver_tag("v1.07.0")),
        ?_assertEqual(error, rebar3_pkg_version:parse_semver_tag("v1.7"))
    ].

pick_highest_semver_test() ->
    Semvers = lists:append([
        rebar3_pkg_version:maybe_semver_tag(T)
     || T <- ["v1.7.0-rc1", "v1.7.0", "nsecbunker-phase4-packaging-ready"]
    ]),
    ?assertMatch({"1.7.0", "v1.7.0", _}, rebar3_pkg_version:pick_highest_semver(Semvers)).

compare_prerelease_identifiers_test() ->
    Semvers = lists:append([
        rebar3_pkg_version:maybe_semver_tag(T)
     || T <- ["v1.7.0-rc.2", "v1.7.0-rc.10"]
    ]),
    ?assertMatch({"1.7.0-rc.10", "v1.7.0-rc.10", _}, rebar3_pkg_version:pick_highest_semver(Semvers)).

project_root_from_rebar_build_dir_test() ->
    RawRoot = filename:absname("."),
    Root = normalize_path(RawRoot),
    BuildDir = filename:join([RawRoot, "_build", "default"]),
    ?assertEqual(Root, rebar3_pkg_version:project_root(BuildDir)).

normalize_path(Path0) ->
    filename:join([Part || Part <- filename:split(Path0), Part =/= "."]).
