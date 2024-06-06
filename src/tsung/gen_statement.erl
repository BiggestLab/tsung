-module(gen_statement).
-export([generate/1, uuid/1]).
-include("statement.hrl").

generate({_Pid, DynData}) ->
    StatementMap = ?XAPI_STUDENT_ANS_STATEMENT,
    RegistrationId = uuid_cus:to_string(uuid_cus:uuid1()),

    StatementId = proplists:get_value(statement_id, DynData),

    % Update template id with the actual id
    UpdatedIdBody = maps:put(<<"id">>, list_to_binary(StatementId), StatementMap),

    % Update template registration id with the actual id
    Context = maps:get(<<"context">>, UpdatedIdBody),
    UpdatedContext = maps:put(<<"registration">>, list_to_binary(RegistrationId), Context),
    UpdatedMap = maps:put(<<"context">>, UpdatedContext, UpdatedIdBody),
    jsx:encode(UpdatedMap).

uuid({_Pid, _DynData}) ->
    UUID = uuid_cus:to_string(uuid_cus:uuid1()),
    UUID.

    





    
