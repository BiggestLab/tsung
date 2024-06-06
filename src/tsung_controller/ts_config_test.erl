-module(ts_config_test).
-vc('$Id$ ').
-author('roman@biggestlab.io').


-include("ts_profile.hrl").
-include("ts_test.hrl").
-include("ts_config.hrl").


-export([parse_config/2]).

-include("xmerl.hrl").


%%----------------------------------------------------------------------
%% Func: parse_config/2
%% Args: Element, Config
%% Returns: List
%% Purpose: parse a request defined in the XML config file
%%----------------------------------------------------------------------
%% Parsing other elements
parse_config(Element = #xmlElement{name=dyn_variable}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
parse_config(Element = #xmlElement{name=test},
             Config=#config{curid = Id, session_tab = Tab,
                            sessions = [CurS | _], dynvar=DynVar,
			    subst    = SubstFlag, match=MatchRegExp}) ->

    Request = case ts_config:getAttr(atom, Element#xmlElement.attributes, type) of 
                  echo ->
                      ValRaw = ts_config:getText(Element#xmlElement.content),
		      %is this needed ?
		      CleanStr = ts_utils:clean_str(ValRaw),
                      #test_request{data=CleanStr, type=echo}
              end,
    Msg= #ts_request{ack     = parse,
                     endpage = true,
                     dynvar_specs  = DynVar,
                     subst   = SubstFlag,
                     match   = MatchRegExp,
                     param   = Request},

    ts_config:mark_prev_req(Id-1, Tab, CurS),
    ets:insert(Tab,{{CurS#session.id, Id}, Msg }),
    lists:foldl( fun(A,B)->ts_config:parse(A,B) end,
                 Config#config{dynvar=undefined},
                 Element#xmlElement.content);
%% Parsing other elements
parse_config(Element = #xmlElement{}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
%% Parsing non #xmlElement elements
parse_config(_, Conf = #config{}) ->
    Conf.

