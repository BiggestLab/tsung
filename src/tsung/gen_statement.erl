-module(gen_statement).
-export([generate_teacher_start_game/1, 
        generate_student_joins_game/1, 
        generate_question_start/1,
        generate_student_ans/1,
        generate_question_end/1,
        generate_game_end/1,
        uuid/1]).

-include("student_ans_statement.hrl").
-include("teacher_start_game_statement.hrl").
-include("student_joins_game_statement.hrl").
-include("question_start_statement.hrl").
-include("question_end_statement.hrl").
-include("game_end_statement.hrl").


generate_teacher_start_game({_Pid, DynData}) ->
    generate_statement(?XAPI_TEACHER_START_GAME_STATEMENT, DynData).

generate_student_joins_game({_Pid, DynData}) ->
    generate_statement(?XAPI_STUDENT_JOINS_GAME_STATEMENT, DynData).

generate_question_start({_Pid, DynData}) ->
    generate_statement(?XAPI_QUESTION_START_STATEMENT, DynData).

generate_student_ans({_Pid, DynData}) ->
    generate_statement(?XAPI_STUDENT_ANS_STATEMENT, DynData).

generate_question_end({_Pid, DynData}) ->
    generate_statement(?XAPI_QUESTION_END_STATEMENT, DynData).

generate_game_end({_Pid, DynData}) ->
    generate_statement(?XAPI_GAME_END_STATEMENT, DynData).

generate_statement(StatementMap, DynData) ->
    RegistrationId = proplists:get_value(registration_id, DynData),
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

    





    
