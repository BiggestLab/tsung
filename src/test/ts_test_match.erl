%%%-------------------------------------------------------------------
%%% File    : ts_test_search.erl
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Description : unit tests for ts_search module
%%%
%%% $Id: ts_test_search.erl 904 2008-10-08 08:16:38Z nniclausse $
%%%-------------------------------------------------------------------
-module(ts_test_match).

-compile(export_all).


-include_lib("eunit/include/eunit.hrl").
-include_lib("ts_profile.hrl").
-include_lib("ts_config.hrl").

-define(MAX_COUNT,42).
-define(COUNT,5).
-define(USER_ID,2).
-define(SESSION_ID,1).
-define(COUNTS,{5,42,2,1}).

test()->
    ok.
match_abort_ok_test() ->
    myset_env(),
    Data="C'est n'est pas une chaine de caractere",
    ?assertMatch(?COUNT, ts_search:match([#match{regexp="Erreur", do=abort, 'when'=match}],Data, ?COUNTS,[],[])).

match_abort_nok_test() ->
    myset_env(),
    Data="Ceci est une Erreur",
    ?assertMatch(0, ts_search:match([#match{regexp="Erreur", do=abort, 'when'=match}],Data, ?COUNTS,[],[])).

nomatch_abort_ok_test() ->
    myset_env(),
    Data="C'est n'est pas une chaine de caractere",
    ?assertMatch(0, ts_search:match([#match{regexp="Erreur", do=abort, 'when'=nomatch}],Data, ?COUNTS,[],[])).

nomatch_abort_nok_test() ->
    myset_env(),
    Data="Ceci est une Erreur",
    ?assertMatch(?COUNT, ts_search:match([#match{regexp="Erreur", do=abort, 'when'=nomatch}],Data, ?COUNTS,[],[])).

nomatch_continue_ok_test() ->
    myset_env(),
    Data="C'est n'est pas une chaine de caractere",
    ?assertMatch(?COUNT, ts_search:match([#match{regexp="Erreur", do=continue, 'when'=nomatch}],Data, ?COUNTS,[],[])).

nomatch_continue_nok_test() ->
    myset_env(),
    Data="Ceci est une Erreur",
    ?assertMatch(?COUNT, ts_search:match([#match{regexp="Erreur", do=continue, 'when'=nomatch}],Data, ?COUNTS,[],[])).

match_continue_ok_test() ->
    myset_env(),
    Data="C'est n'est pas une chaine de caractere",
    ?assertMatch(?COUNT, ts_search:match([#match{regexp="Erreur", do=continue, 'when'=match}],Data, ?COUNTS,[],[])).

match_continue_nok_test() ->
    myset_env(),
    Data="Ceci est une Erreur",
    ?assertMatch(?COUNT, ts_search:match([#match{regexp="Erreur", do=continue, 'when'=match}],Data, ?COUNTS,[],[])).

nomatch_loop_ok_test() ->
    myset_env(),
    Data="C'est n'est pas une chaine de caractere",
    ?assertMatch(?COUNT+1, ts_search:match([#match{regexp="Erreur", do=loop, max_loop=?COUNT, loop_back=0, sleep_loop=1,'when'=nomatch}],Data, ?COUNTS,[],[])).

nomatch_loop_nok_test() ->
    myset_env(),
    Data="Ceci est une Erreur",
    ?assertMatch(?COUNT, ts_search:match([#match{regexp="Erreur", do=loop, max_loop=?COUNT, loop_back=0, sleep_loop=1,'when'=nomatch}],Data, ?COUNTS,[],[])).

match_loop_ok_test() ->
    myset_env(),
    Data="C'est n'est pas une chaine de caractere",
    ?assertMatch(?COUNT, ts_search:match([#match{regexp="Erreur", do=loop, max_loop=?COUNT, loop_back=0, sleep_loop=1,'when'=match}],Data, ?COUNTS,[],[])).

match_loop_nok_test() ->
    myset_env(),
    Data="Ceci est une Erreur",
    ?assertMatch(?COUNT+1, ts_search:match([#match{regexp="Erreur", do=loop, max_loop=?COUNT, loop_back=0, sleep_loop=1, 'when'=match}],Data, ?COUNTS,[],[])).


nomatch_restart_ok_test() ->
    myset_env(),
    Data="C'est n'est pas une chaine de caractere",
    ?assertMatch(?MAX_COUNT, ts_search:match([#match{regexp="Erreur", do=restart, max_restart=?COUNT,'when'=nomatch}],Data, ?COUNTS,[],[])).

nomatch_restart_nok_test() ->
    myset_env(),
    Data="Ceci est une Erreur",
    ?assertMatch(?COUNT, ts_search:match([#match{regexp="Erreur", do=restart, max_restart=?COUNT,'when'=nomatch}],Data, ?COUNTS,[],[])).

match_restart_ok_test() ->
    myset_env(),
    Data="C'est n'est pas une chaine de caractere",
    ?assertMatch(?COUNT, ts_search:match([#match{regexp="Erreur", do=restart, max_restart=?COUNT,'when'=match}],Data, ?COUNTS,[],[])).

match_restart_nok_test() ->
    myset_env(),
    Data="Ceci est une Erreur",
    ?assertMatch(?MAX_COUNT, ts_search:match([#match{regexp="Erreur", do=restart, max_restart=?COUNT, 'when'=match}],Data, ?COUNTS,[],[])).

match_subst_undef_test() ->
    myset_env(),
    Data="Ceci est une Erreur",
    ?assertMatch(?COUNT, ts_search:match([#match{regexp="%%_mydynvar%%", do=restart, subst=true, 'when'=match}],Data, ?COUNTS,[],[])).

match_subst_undef2_test() ->
    myset_env(),
    Data="Ceci est une Erreur",
    ?assertMatch(?COUNT, ts_search:match([#match{regexp="ttt%%_mydynvar%%", do=restart, subst=true, 'when'=match}],Data, ?COUNTS,[],[])).

match_subst_test() ->
    myset_env(),
    Data="Ceci est une Erreur ",
    Dynvar=ts_dynvars:new(mydynvar,"Erreur"),
    ?assertMatch(?MAX_COUNT, ts_search:match([#match{regexp="%%_mydynvar%%", do=restart, subst=true, 'when'=match}],Data, ?COUNTS,Dynvar,[])).

%%% loop_sleep/2: how long a looping request waits before it is replayed.
%%% sleep_loop is the value frozen at config-parse time; sleep_var names a
%%% dynvar (a Retry-After off the response) that overrides it when usable.

-define(RETRY_MATCH, #match{do=loop, 'when'=match, sleep_loop=50,
                            sleep_var=ra, sleep_var_unit=1000,
                            sleep_max=30000}).

%% no sleep_var at all: unchanged behaviour, the static value is used as-is
loop_sleep_static_test() ->
    myset_env(),
    ?assertEqual(50, ts_search:loop_sleep(#match{do=loop, sleep_loop=50}, [])).

%% the 2xx that ends a retry loop carries no Retry-After, so "variable set but
%% empty/undefined" is the common path and must fall back silently
loop_sleep_var_missing_test() ->
    myset_env(),
    ?assertEqual(50, ts_search:loop_sleep(?RETRY_MATCH, [])),
    ?assertEqual(50, ts_search:loop_sleep(?RETRY_MATCH, ts_dynvars:new(ra,undefined))),
    ?assertEqual(50, ts_search:loop_sleep(?RETRY_MATCH, ts_dynvars:new(ra,""))),
    ?assertEqual(50, ts_search:loop_sleep(?RETRY_MATCH, ts_dynvars:new(ra,<<>>))).

%% delta-seconds, in the shapes the dynvar backends actually produce
loop_sleep_delta_seconds_test() ->
    myset_env(),
    ?assertEqual(2000, ts_search:loop_sleep(?RETRY_MATCH, ts_dynvars:new(ra,"2"))),
    ?assertEqual(2000, ts_search:loop_sleep(?RETRY_MATCH, ts_dynvars:new(ra,<<"2">>))),
    ?assertEqual(2000, ts_search:loop_sleep(?RETRY_MATCH, ts_dynvars:new(ra,2))),
    %% surrounding whitespace survives header parsing often enough to matter
    ?assertEqual(3000, ts_search:loop_sleep(?RETRY_MATCH, ts_dynvars:new(ra," 3 "))),
    %% fractions are not RFC 9110 but cost nothing and buy sub-second backoff
    ?assertEqual(500,  ts_search:loop_sleep(?RETRY_MATCH, ts_dynvars:new(ra,"0.5"))).

%% the unit multiplier is what turns the bare number into milliseconds
loop_sleep_unit_test() ->
    myset_env(),
    Millis = ?RETRY_MATCH#match{sleep_var_unit=1},
    ?assertEqual(250, ts_search:loop_sleep(Millis, ts_dynvars:new(ra,"250"))).

%% an HTTP-date is legal Retry-After but is not honoured: see ts_search
loop_sleep_http_date_test() ->
    myset_env(),
    Date = "Wed, 21 Oct 2026 07:28:00 GMT",
    ?assertEqual(50, ts_search:loop_sleep(?RETRY_MATCH, ts_dynvars:new(ra,Date))).

%% garbage must fall back, never crash the virtual user
loop_sleep_garbage_test() ->
    myset_env(),
    ?assertEqual(50, ts_search:loop_sleep(?RETRY_MATCH, ts_dynvars:new(ra,"soon"))),
    ?assertEqual(50, ts_search:loop_sleep(?RETRY_MATCH, ts_dynvars:new(ra,"12abc"))),
    ?assertEqual(50, ts_search:loop_sleep(?RETRY_MATCH, ts_dynvars:new(ra,{a,b}))),
    ?assertEqual(50, ts_search:loop_sleep(?RETRY_MATCH, ts_dynvars:new(ra,[{a,b}]))).

%% a hostile or misconfigured Retry-After must not park the user for an hour
loop_sleep_clamp_test() ->
    myset_env(),
    ?assertEqual(30000, ts_search:loop_sleep(?RETRY_MATCH, ts_dynvars:new(ra,"3600"))),
    ?assertEqual(30000, ts_search:loop_sleep(?RETRY_MATCH, ts_dynvars:new(ra,"31"))),
    %% exactly at the ceiling is not a clamp
    ?assertEqual(30000, ts_search:loop_sleep(?RETRY_MATCH, ts_dynvars:new(ra,"30"))).

%% `Retry-After: 0' taken literally is a busy spin against a server that is
%% already shedding; the static backoff is the floor
loop_sleep_floor_test() ->
    myset_env(),
    ?assertEqual(50, ts_search:loop_sleep(?RETRY_MATCH, ts_dynvars:new(ra,"0"))),
    ?assertEqual(50, ts_search:loop_sleep(?RETRY_MATCH, ts_dynvars:new(ra,"-5"))),
    %% ...and never zero even if the scenario itself asked for no backoff
    NoStatic = ?RETRY_MATCH#match{sleep_loop=0},
    ?assertEqual(1, ts_search:loop_sleep(NoStatic, ts_dynvars:new(ra,"0"))).

%% the sleep really is driven by the response: match/5 threads the dynvars of
%% the response it just matched into the loop clause
loop_sleep_end_to_end_test() ->
    myset_env(),
    Data="Please retry-after a while",
    Match=#match{regexp="retry-after", do=loop, 'when'=match, max_loop=?COUNT,
                 loop_back=0, sleep_loop=1, sleep_var=ra, sleep_var_unit=1,
                 sleep_max=30000},
    Start=erlang:monotonic_time(millisecond),
    ?assertMatch(?COUNT+1, ts_search:match([Match],Data,?COUNTS,ts_dynvars:new(ra,"120"),[])),
    ?assert(erlang:monotonic_time(millisecond) - Start >= 100).

myset_env()->
    myset_env(0).
myset_env(Level)->
    application:set_env(stdlib,debug_level,Level).

