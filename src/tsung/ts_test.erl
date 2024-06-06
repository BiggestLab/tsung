-module(ts_test).
-behaviour(ts_plugin).

%% Required callback functions for ts_plugin behavior

%% Required exports for ts_plugin behavior
-export([new/2, destroy/1, stop/2]).
-export([add_dynparams/4, get_message/2, session_defaults/0, dump/2, parse/2, parse_config/2, decode_buffer/2, new_session/0, parse_bidi/2]).

%-include_lib("tsung/include/tsung.hrl").

-include("ts_test.hrl").
-record(state, {}).

add_dynparams(_Bool, _DynData, Param, _HostData) ->
    Param#test_request{}.

get_message(Server, SessionData) ->
    {test_request, echo, _, Msg} = Server,
    io:format("Received message/2: ~p", [Msg]),
    error_logger:info_msg("Received message/2: ~p", [Msg]),
    {list_to_binary(Msg),SessionData}.

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

%% Stop the plugin
stop(_Server, State) ->
    {ok, State}.
