%%%
%%%  Copyright 2008 © Nicolas Niclausse
%%%
%%%  Author : Nicolas Niclausse <nicolas.nniclausse@niclux.org>
%%%  Created: 21 oct 2008 by Nicolas Niclausse <nicolas.niclausse@niclux.org>
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%
%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two; the MPL (Mozilla Public License), which EPL (Erlang
%%%  Public License) is based on, is included in this exception.

-module(ts_os_mon_munin).
-vc('$Id: ts_os_mon_snmp.erl,v 0.0 2008/10/21 12:57:49 nniclaus Exp $ ').
-author('nicolas.niclausse@niclux.org').

-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc munin plugin for ts_os_mon
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("ts_macros.hrl").
-include("ts_os_mon.hrl").

-define(READ_TIMEOUT,2500). % 2.5 sec
-define(SEND_TIMEOUT,5000).
-define(RETRY_SLEEP,30000).

%% if interval is more than this, we must send ping to avoid closed
%% connection from munin node server (default timeout is 10s in recent
%% version of munin-node):
-define(MAX_INTERVAL,8000).
-define(PING_INTERVAL,5000).

-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,{
          mon,        % pid of mon server
          interval,   % interval in msec between gathering of data
          socket,     % tcp socket
          port,       % tcp port of munin-node server
          host,       % remote munin-node hostname
          addr,       % remote munin-node IP addr
          ncpus,      % number of cpus of remote server
          plugins=[]  % extra munin plugins to fetch (e.g. ["if_eth0","diskstats"])
         }).

start(Args) ->
    ?LOGF("starting os_mon_munin with args ~p",[Args],?NOTICE),
    gen_server:start_link(?MODULE, Args, []).

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init({HostStr, {Port}, Interval, MonServer}) ->
    %% kept for backward compatibility with configs/callers that pass no plugins
    init({HostStr, {Port, []}, Interval, MonServer});
init({HostStr, {Port, Plugins}, Interval, MonServer}) ->
    ?LOGF("Starting munin mgr on ~p:~p (extra plugins: ~p)~n",
          [HostStr,Port,Plugins], ?DEB),
    {ok, IP} = inet:getaddr(HostStr, inet),
    erlang:start_timer(?INIT_WAIT, self(), connect ),
    {ok, #state{mon=MonServer, host=HostStr, interval=Interval, addr=IP,
                port=Port, plugins=Plugins}}.


%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    {stop, {unknown_message, Msg}, State}.


%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({timeout,_Ref,connect},State=#state{addr=IP,port=Port,host=HostStr}) ->
    Opts=[list,
          {active, false},
          {packet, line},
          {send_timeout, ?SEND_TIMEOUT},
          {keepalive, true}
         ],
    case gen_tcp:connect(IP, Port, Opts) of
        {ok, Socket} ->
            case gen_tcp:recv(Socket,0, ?READ_TIMEOUT) of
                {ok, "# munin node at "++ Str} ->
                    MuninHost = ts_utils:chop(Str),
                    ?LOGF("Connected to ~p~n", [MuninHost], ?INFO),
                    %% We want CPU value ranging from 0 to 100, so we need the max value :
                    gen_tcp:send(Socket,"config cpu\n"),
                    ConfigCPU=read_munin_data(Socket),
                    NCPUs = case proplists:get_value('user.max',ConfigCPU) of
                                Num when  is_number(Num) ->
                                    Num/100 ;
                                _ ->
                                    ?LOG("can't find the number of CPU, assume one~n",?NOTICE),
                                    1
                            end,
                    ?LOGF("first fetch successful to ~p~n", [MuninHost], ?INFO),
                    case (State#state.interval > ?MAX_INTERVAL) of
                        true ->
                            erlang:start_timer(?PING_INTERVAL, self(), ping );
                        _ ->
                            ok
                    end,
                    erlang:start_timer(State#state.interval, self(), send_request ),
                    {noreply, State#state{socket=Socket,host=MuninHost,ncpus=NCPUs}};
                {error, Reason} ->
                    ?LOGF("Error while connecting to munin server: ~p~n", [Reason], ?ERR),
                    {stop, Reason, State}
            end;
        {error, Reason} ->
            ?LOGF("Can't connect to munin server on ~p, reason:~p~n", [HostStr, Reason], ?ERR),
            {stop, Reason, State}
    end;

handle_info({timeout, _Ref, ping},  State=#state{socket=Socket} ) ->
    gen_tcp:send(Socket,"\n"),
    gen_tcp:recv(Socket,0,?READ_TIMEOUT),
    erlang:start_timer(?PING_INTERVAL, self(), ping ),
    {noreply, State};

handle_info({timeout, _Ref, send_request},  State=#state{socket=Socket,host=Hostname} ) ->
    Start = erlang:monotonic_time(millisecond),
    %% cpu/memory/load are always fetched; any plugins named in the config's
    %% <munin plugins="..."/> attribute are fetched too (see fetch_plugins/3).
    ?LOGF("Fetching munin for cpu on host ~p~n", [Hostname], ?DEB),
    gen_tcp:send(Socket,"fetch cpu\n"),
    AllCPU=read_munin_data(Socket),
    ?LOGF("Fetching munin for memory on host ~p~n", [Hostname], ?DEB),
    gen_tcp:send(Socket,"fetch memory\n"),
    AllMem=read_munin_data(Socket),
    ?LOGF("Fetching munin for load on host ~p~n", [Hostname], ?DEB),
    gen_tcp:send(Socket,"fetch load\n"),
    AllLoad=read_munin_data(Socket),
    %% sum all cpu types, except idle.
    NonIdle=lists:keydelete('idle.value',1,AllCPU),
    RawCpu = lists:foldl(fun({_Key,Val},Acc) when is_integer(Val)->
                                 Acc+Val
                         %% float division: `div 1000' truncated, so a 1500 ms
                         %% interval was scaled as if it were 1000 ms (and any
                         %% sub-second interval divided by zero)
                         end,0,NonIdle) / (State#state.interval / 1000),
    Cpu=check_value(RawCpu,{Hostname,"cpu"})/State#state.ncpus,
    ?LOGF(" munin cpu on host ~p is  ~p~n", [Hostname,Cpu], ?DEB),
    %% returns free + buffer + cache
    FunFree = fun({Key,Val},Acc) when ((Key=='buffers.value') or
                                       (Key=='free.value')    or
                                       (Key=='cached.value') ) ->
                      Acc+Val;
                 (_, Acc) -> Acc
              end,
    FreeMem=check_value(lists:foldl(FunFree,0,AllMem),{Hostname,"memory"})/1048576,%MBytes
    ?LOGF(" munin memory on host ~p is ~p~n", [Hostname,FreeMem], ?DEB),
    %% load only has one value at present
    Load = lists:foldl(fun({_Key,Val},Acc) -> Acc+Val end,0,AllLoad),
    ?LOGF(" munin load on host ~p is ~p~n", [Hostname,Load], ?DEB),
    Extra = fetch_plugins(Socket, Hostname, State#state.plugins),
    ts_os_mon:send(State#state.mon,[{sample_counter, {cpu, Hostname}, Cpu},
                                    {sample, {freemem, Hostname}, FreeMem},
                                    {sample, {load, Hostname}, Load} | Extra]),
    %% Schedule the NEXT poll `interval' after this one STARTED, not after it
    %% finished. Each fetch is a round trip to the monitored host plus plugin
    %% execution there; restarting the timer afterwards made the real period
    %% interval+fetch_time, so a nominal 1s poll of a remote host actually
    %% sampled at ~0.75Hz. A sampling instrument that silently samples slower
    %% than configured under-reports exactly the transients it was raised to
    %% catch. If a cycle overruns the interval we fire immediately rather than
    %% queue up backlog.
    Elapsed = erlang:monotonic_time(millisecond) - Start,
    erlang:start_timer(max(0, State#state.interval - Elapsed), self(), send_request ),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: fetch_plugins/3
%% Description: fetch each configured extra munin plugin and turn every numeric
%%   field into a sample. Reported as sample_counter because the interesting
%%   plugins (if_*, diskstats) expose monotonic counters, and sample_counter
%%   records the per-interval delta -- so network and disk I/O appear on the
%%   same timeline as throughput and latency instead of as raw totals.
%%   Metric names are "<plugin>.<field>", e.g. 'if_eth0.down'.
%% Returns: list of {sample_counter, {Name, Hostname}, Value}
%%--------------------------------------------------------------------
fetch_plugins(_Socket, _Hostname, []) ->
    [];
fetch_plugins(Socket, Hostname, Plugins) ->
    lists:flatmap(fun(Plugin) ->
                          ?LOGF("Fetching munin for ~p on host ~p~n",
                                [Plugin, Hostname], ?DEB),
                          gen_tcp:send(Socket, "fetch " ++ Plugin ++ "\n"),
                          Data = read_munin_data(Socket),
                          [ {sample_counter, {plugin_metric(Plugin, Key), Hostname}, Val}
                            || {Key, Val} <- Data, is_number(Val) ]
                  end, Plugins).

%% 'down.value' from plugin "if_eth0" becomes 'if_eth0.down'
plugin_metric(Plugin, Key) ->
    KeyStr = atom_to_list(Key),
    Base = case lists:suffix(".value", KeyStr) of
               true  -> lists:sublist(KeyStr, length(KeyStr) - 6);
               false -> KeyStr
           end,
    list_to_atom(Plugin ++ "." ++ Base).


%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, #state{socket=undefined}) ->
    ok;
terminate(_Reason, #state{socket=Socket}) ->
    gen_tcp:close(Socket).

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

read_munin_data(Socket)->
    read_munin_data(Socket,gen_tcp:recv(Socket,0,?READ_TIMEOUT),[]).

read_munin_data(_Socket,{ok,".\n"}, Acc)->
    Acc;
read_munin_data(Socket,{ok, "graph_args --base "++ Data}, Acc) when is_list(Acc)->
    %% special case for getting the number of cpus
    NewAcc = case re:run(Data,"--upper-limit (\\d+)",[{capture,all_but_first,list}]) of
                 {match,[Val]} when length(Val) > 0 ->
                     ?LOGF("the munin node has ~p CPUs ~n",[Val],?INFO),
                     [{'user.max',list_to_integer(Val)}| Acc];
                 _ ->
                     ?LOGF("upper-limit don't match ~p~n",[Data],?WARN),
                     Acc
             end,
    read_munin_data(Socket,gen_tcp:recv(Socket,0,?READ_TIMEOUT), NewAcc);
read_munin_data(Socket,{ok, Data}, Acc) when is_list(Acc)->
    ?DebugF("Parse munin data: ~p~n",[Data]),
    NewAcc = case string:tokens(Data," \n") of
                 [Key, Value] ->
                     try ts_utils:list_to_number(Value) of
                         Num when is_number(Num) ->
                             [{list_to_atom(Key), Num }|Acc]
                     catch
                         _Type:_Exp ->
                             Acc
                     end;
                 [_Key| _Rest] ->
                      Acc;
                 _ ->
                     ?LOGF("Unknown data received from munin server: ~p~n",[Data],?WARN),
                     Acc
             end,
    read_munin_data(Socket,gen_tcp:recv(Socket,0,?READ_TIMEOUT), NewAcc);
read_munin_data(Socket,{error, timeout}, Acc) when is_list(Acc)->
    %% the remote server may be overloaded, wait a bit before retrying
    ?LOG("munin: timeout error, server must be overloaded, sleep for 30 sec~n", ?WARN),
    gen_tcp:close(Socket),
    timer:sleep(?RETRY_SLEEP),
    erlang:error(server_timeout).

%% check is this a valid value (positive at least)
check_value(Val,_) when Val > 0 -> Val;
check_value(Val,{Host, Type})  ->
    ?LOGF("munin: bad ~s value on host ~p: ~p~n", [Type, Host, Val],?WARN),
    0.
