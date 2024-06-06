-vc('$Id$ ').
-author('roman@biggestlab.io').

-record(test_request, {
          type,
          arith,
          data             % may be a string or two numbers
         }).

%% 
-record(test_dyndata, 
        { 
          none
         }
       ).

%% unused
-record(test, 
        { 
          fixme
         }
       ).

%%% Supported byte code instructions
-define(ECHO, 0).
-define(ADD, 1).
-define(SUB, 2).

