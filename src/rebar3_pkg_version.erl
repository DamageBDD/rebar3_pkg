-module(rebar3_pkg_version).

-export([
    resolve/4,
    info/4,
    log_source/2,
    find_release_vsn/2,
    git_dir/1,
    git_head_tags/1,
    git_semver_version/1,
    semver_tags/1,
    parse_semver_tag/1,
    maybe_semver_tag/1,
    pick_highest_semver/1,
    project_root/1,
    to_list/1
]).

%% Resolve precedence:
%%   --version -> semver tag on HEAD -> RELEASES -> app metadata -> 0.0.0
resolve(Args, BaseDir, AppInfo, ReleaseFun) ->
    case cli_version(Args) of
        {ok, Vsn0} ->
            {Vsn0, {cli, Vsn0}};
        error ->
            case git_semver_version(BaseDir) of
                {ok, Vsn1, Tag} ->
                    {Vsn1, {git, Tag, Vsn1}};
                error ->
                    release_or_app_version(AppInfo, ReleaseFun)
            end
    end.

info(Args, BaseDir, AppInfo, ReleaseFun) ->
    GitDir = git_dir(BaseDir),
    Tags = git_head_tags(BaseDir),
    SemverTags = semver_tags(Tags),
    {Vsn, Source} = resolve(Args, BaseDir, AppInfo, ReleaseFun),
    #{
        version => Vsn,
        source => Source,
        git_dir => GitDir,
        git_tags => Tags,
        semver_tags => SemverTags
    }.

log_source(Prefix, {cli, Vsn}) ->
    rebar_api:info("~s: version from --version ~s", [Prefix, Vsn]);
log_source(Prefix, {git, Tag, Vsn}) ->
    rebar_api:info("~s: version from semver git tag ~s -> ~s", [Prefix, Tag, Vsn]);
log_source(Prefix, {release, Vsn}) ->
    rebar_api:info("~s: version from RELEASES ~s", [Prefix, Vsn]);
log_source(Prefix, {app, Vsn}) ->
    rebar_api:info("~s: version from app metadata ~s", [Prefix, Vsn]);
log_source(Prefix, default) ->
    rebar_api:warn("~s: no version found; using 0.0.0", [Prefix]).

cli_version(Args) ->
    case proplists:get_value(version, Args) of
        undefined -> error;
        [] -> error;
        V -> {ok, to_list(V)}
    end.

release_or_app_version(AppInfo, ReleaseFun) ->
    case release_version(ReleaseFun) of
        {ok, Vsn0} ->
            Vsn = to_list(Vsn0),
            {Vsn, {release, Vsn}};
        error ->
            appinfo_version_or_default(AppInfo)
    end.

release_version(ReleaseFun) ->
    try ReleaseFun() of
        {ok, Vsn} -> {ok, Vsn};
        _ -> error
    catch
        _:_ -> error
    end.

appinfo_version_or_default(AppInfo) ->
    case appinfo_version(AppInfo) of
        {ok, Vsn} -> {Vsn, {app, Vsn}};
        error -> {"0.0.0", default}
    end.

appinfo_version(undefined) ->
    error;
appinfo_version(AppInfo) ->
    case rebar_app_info:original_vsn(AppInfo) of
        undefined -> error;
        [] -> error;
        Vsn -> {ok, to_list(Vsn)}
    end.

%% ---------- RELEASES helpers ----------

find_release_vsn([H | T], App) ->
    case H of
        {release, Name, Vsn, _ErtsVsn, _Apps, permanent} ->
            case same_name(Name, App) of
                true -> Vsn;
                false -> find_release_vsn(T, App)
            end;
        _ ->
            find_release_vsn(T, App)
    end;
find_release_vsn([], _) ->
    undefined.

same_name(Name, App) when is_atom(Name) -> atom_to_list(Name) =:= to_list(App);
same_name(Name, App) when is_binary(Name) -> binary_to_list(Name) =:= to_list(App);
same_name(Name, App) when is_list(Name) -> Name =:= to_list(App).

%% ---------- git helpers ----------

%% rebar_dir:base_dir/1 commonly points at _build/<profile>.  Move back to the
%% project root before asking git for HEAD tags, then keep cwd as a fallback for
%% direct local invocation.
git_dir(BaseDir) ->
    Candidates = unique_paths([project_root(BaseDir), normalize_dir(BaseDir), "."]),
    case first_git_toplevel(Candidates) of
        {ok, Root} -> Root;
        error -> project_root(BaseDir)
    end.

git_head_tags(BaseDir) ->
    Dirs = unique_paths([git_dir(BaseDir), project_root(BaseDir), normalize_dir(BaseDir), "."]),
    unique_tags(lists:append([git_tag_lines(Dir) || Dir <- Dirs])).

git_semver_version(BaseDir) ->
    Semvers = lists:append([maybe_semver_tag(T) || T <- git_head_tags(BaseDir)]),
    case Semvers of
        [] ->
            error;
        _ ->
            {Vsn, Tag, _Key} = pick_highest_semver(Semvers),
            {ok, Vsn, Tag}
    end.

semver_tags(Tags) ->
    [Tag || {_Vsn, Tag, _Key} <- lists:append([maybe_semver_tag(T) || T <- Tags])].

git_tag_lines(Dir) ->
    git_lines(["git", "-C", Dir, "tag", "--points-at", "HEAD"]).

first_git_toplevel([Dir | Rest]) ->
    case git_lines(["git", "-C", Dir, "rev-parse", "--show-toplevel"]) of
        [Root | _] -> {ok, Root};
        [] -> first_git_toplevel(Rest)
    end;
first_git_toplevel([]) ->
    error.

git_lines(Argv) ->
    Cmd = string:join([shell_escape(A) || A <- Argv], " "),
    Out = os:cmd(Cmd ++ " 2>/dev/null"),
    [Line || Line <- [string:trim(L) || L <- string:tokens(Out, "\n")], Line =/= ""].

project_root(undefined) ->
    ".";
project_root([]) ->
    ".";
project_root(BaseDir0) ->
    BaseDir = filename:absname(to_list(BaseDir0)),
    case split_build(filename:split(BaseDir), []) of
        {ok, RootParts} -> filename:join(RootParts);
        error -> BaseDir
    end.

normalize_dir(undefined) -> ".";
normalize_dir([]) -> ".";
normalize_dir(Dir) -> to_list(Dir).

split_build(["_build" | _], Acc) ->
    {ok, lists:reverse(Acc)};
split_build([H | T], Acc) ->
    split_build(T, [H | Acc]);
split_build([], _Acc) ->
    error.

unique_tags(Tags) ->
    lists:reverse(
        lists:foldl(
            fun(Tag, Acc) ->
                case lists:member(Tag, Acc) of
                    true -> Acc;
                    false -> [Tag | Acc]
                end
            end,
            [],
            Tags
        )
    ).

unique_paths(Paths) ->
    unique_tags([P || P <- [normalize_dir(Path) || Path <- Paths], P =/= ""]).

shell_escape(S) when is_list(S) ->
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

%% ---------- semver helpers ----------

maybe_semver_tag(Tag0) ->
    case parse_semver_tag(Tag0) of
        {ok, Vsn, Key} -> [{Vsn, tag_name(Tag0), Key}];
        error -> []
    end.

pick_highest_semver([H | T]) ->
    lists:foldl(
        fun({_, _, Key} = Candidate, {_, _, BestKey} = Best) ->
            case semver_compare(Key, BestKey) of
                gt -> Candidate;
                _ -> Best
            end
        end,
        H,
        T
    ).

parse_semver_tag(Tag0) ->
    Tag = strip_v_prefix(tag_name(Tag0)),
    {Core, Build} = split_once(Tag, $+),
    case valid_build(Build) of
        true ->
            {Base, Pre} = split_once(Core, $-),
            case valid_core_base(Base) of
                true ->
                    case string:tokens(Base, ".") of
                        [MajS, MinS, PatchS] ->
                            case
                                {
                                    parse_core_num(MajS),
                                    parse_core_num(MinS),
                                    parse_core_num(PatchS),
                                    parse_pre(Pre)
                                }
                            of
                                {{ok, Maj}, {ok, Min}, {ok, Patch}, {ok, PreKey}} ->
                                    {ok, Tag, {Maj, Min, Patch, PreKey}};
                                _ ->
                                    error
                            end;
                        _ ->
                            error
                    end;
                false ->
                    error
            end;
        false ->
            error
    end.

tag_name(Tag0) ->
    Tag = string:trim(to_list(Tag0)),
    Prefix = "refs/tags/",
    case lists:prefix(Prefix, Tag) of
        true -> string:substr(Tag, length(Prefix) + 1);
        false -> Tag
    end.

strip_v_prefix([$v | T]) -> T;
strip_v_prefix([$V | T]) -> T;
strip_v_prefix(S) -> S.

split_once(S, Char) ->
    case string:chr(S, Char) of
        0 -> {S, none};
        N -> {string:substr(S, 1, N - 1), string:substr(S, N + 1)}
    end.

parse_core_num(S) ->
    case valid_core_num(S) of
        true -> {ok, list_to_integer(S)};
        false -> error
    end.

valid_core_base(S) ->
    S =/= "" andalso
        not lists:prefix(".", S) andalso
        not lists:suffix(".", S) andalso
        string:str(S, "..") =:= 0.

valid_core_num("0") -> true;
valid_core_num([H | _] = S) -> H >= $1 andalso H =< $9 andalso all_digits(S);
valid_core_num(_) -> false.

parse_pre(none) ->
    {ok, release};
parse_pre(Pre) ->
    case valid_ident_list(Pre, true) of
        true -> {ok, [pre_ident(I) || I <- string:tokens(Pre, ".")]};
        false -> error
    end.

valid_build(none) -> true;
valid_build(Build) -> valid_ident_list(Build, false).

valid_ident_list(S, CheckNumericLeadingZero) ->
    S =/= "" andalso
        not lists:prefix(".", S) andalso
        not lists:suffix(".", S) andalso
        string:str(S, "..") =:= 0 andalso
        lists:all(fun(I) -> valid_ident(I, CheckNumericLeadingZero) end, string:tokens(S, ".")).

valid_ident(S, CheckNumericLeadingZero) ->
    valid_ident_chars(S) andalso
        case {CheckNumericLeadingZero, all_digits(S)} of
            {true, true} -> valid_core_num(S);
            _ -> true
        end.

valid_ident_chars(S) ->
    S =/= "" andalso
        lists:all(
            fun(C) ->
                (C >= $0 andalso C =< $9) orelse
                    (C >= $A andalso C =< $Z) orelse
                    (C >= $a andalso C =< $z) orelse
                    C =:= $-
            end,
            S
        ).

all_digits(S) ->
    S =/= "" andalso lists:all(fun(C) -> C >= $0 andalso C =< $9 end, S).

pre_ident(S) ->
    case all_digits(S) of
        true -> {num, list_to_integer(S)};
        false -> {str, S}
    end.

semver_compare({MajA, MinA, PatchA, PreA}, {MajB, MinB, PatchB, PreB}) ->
    case compare_nums([MajA, MinA, PatchA], [MajB, MinB, PatchB]) of
        eq -> compare_pre(PreA, PreB);
        Other -> Other
    end.

compare_nums([A | As], [B | Bs]) ->
    case compare_int(A, B) of
        eq -> compare_nums(As, Bs);
        Other -> Other
    end;
compare_nums([], []) ->
    eq.

compare_int(A, B) when A > B -> gt;
compare_int(A, B) when A < B -> lt;
compare_int(_, _) -> eq.

compare_pre(release, release) ->
    eq;
compare_pre(release, _) ->
    gt;
compare_pre(_, release) ->
    lt;
compare_pre([], []) ->
    eq;
compare_pre([], [_ | _]) ->
    lt;
compare_pre([_ | _], []) ->
    gt;
compare_pre([A | As], [B | Bs]) ->
    case compare_pre_ident(A, B) of
        eq -> compare_pre(As, Bs);
        Other -> Other
    end.

compare_pre_ident({num, A}, {num, B}) -> compare_int(A, B);
compare_pre_ident({num, _}, {str, _}) -> lt;
compare_pre_ident({str, _}, {num, _}) -> gt;
compare_pre_ident({str, A}, {str, B}) when A > B -> gt;
compare_pre_ident({str, A}, {str, B}) when A < B -> lt;
compare_pre_ident(_, _) -> eq.

%% ---------- conversion ----------

to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(L) when is_list(L) -> L.
