-module(ts_test).
-behaviour(ts_plugin).

%% Required callback functions for ts_plugin behavior

%% Required exports for ts_plugin behavior
-export([new/2, destroy/1, run/4, stop/2]).
-export([add_dynparams/4, get_message/2, get_message/1, session_defaults/0, dump/2, parse/2, parse_config/2, decode_buffer/2, new_session/0, parse_bidi/2]).

%-include_lib("tsung/include/tsung.hrl").

-include("ts_test.hrl").
-record(state, {}).

add_dynparams(_Bool, _DynData, Param, _HostData) ->
    Param#test_request{}.

get_message(_Server, _SessionData) ->
    io:format("Received message/2: ~p", [_SessionData]),
    error_logger:info_msg("Received message/2: ~p", [_SessionData]),
    {ok,_SessionData}.

get_message(#test_request{type=echo, data=Data}) ->
    io:format("Received message/1: ~p", [Data]),
    error_logger:info_msg("Received message/1: ~p", [Data]),
    list_to_binary(binary_to_list(<<?ECHO:8>>) ++ Data).

session_defaults() ->
    {ok, []}.

dump(_Type, Data) ->
    Data.

parse(_Data, _State) ->
    {ok, _State}.

parse_config(Element, Conf) ->
	ts_config_test:parse_config(Element, Conf).

decode_buffer(_Data, _State) ->
    {ok, _State}.

new_session() ->
    {ok, []}.

parse_bidi(_Data, _State) ->
    {nodata, _State, think}.



%% Create a new plugin instance
new(_Server, _SessionData) ->
    io:format("Starting test module: ~p", [_SessionData]),
    error_logger:info_msg("~nStarting test module ~n~p", [_SessionData]),
    {ok, #state{}}.

%% Destroy the plugin instance
destroy(_State) ->
    ok.

%% Run the plugin: handle the request and return the same message
run(_Server, _SessionData, State, _Opts) ->
    %% Simulate receiving a message and sending it back
    io:format("Running test module: ~p", [State]),
    %% Write to debug log
    error_logger:info_msg("~nRunning test module ~n"),
    %% Return the message for assertion
    State.

%% Stop the plugin
stop(_Server, State) ->
    {ok, State}.
