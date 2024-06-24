-module(gen_statement).
-export([generate_teacher_start_game/1, 
        generate_student_joins_game/1, 
        generate_teacher_joins_game/1, 
        generate_question_start/1,
        generate_student_ans/1,
        generate_question_end/1,
        generate_game_end/1,
        uuid/1]).

-include("student_ans_statement.hrl").
-include("teacher_start_game_statement.hrl").
-include("student_joins_game_statement.hrl").
-include("teacher_joins_game_statement.hrl").
-include("question_start_statement.hrl").
-include("question_end_statement.hrl").
-include("game_end_statement.hrl").


generate_teacher_start_game({_Pid, DynData}) ->
    generate_statement(?XAPI_TEACHER_START_GAME_STATEMENT, DynData).

generate_student_joins_game({_Pid, DynData}) ->
    StudentName = proplists:get_value(student_joins_counter, DynData),
    GroupId = proplists:get_value(group_id , DynData),

    Statement = ?XAPI_STUDENT_JOINS_GAME_STATEMENT,
    Object = maps:get(<<"object">>, Statement),
    UpdatedObject = maps:put(<<"name">>, list_to_binary(GroupId), Object),
    UpdatedName = maps:put(<<"object">>, UpdatedObject, Statement),

    generate_statement(UpdatedName, DynData, StudentName).

generate_teacher_joins_game({_Pid, DynData}) ->
    GroupId = proplists:get_value(group_id , DynData),

    Statement = ?XAPI_TEACHER_JOINS_GAME_STATEMENT,
    Object = maps:get(<<"object">>, Statement),
    UpdatedObject = maps:put(<<"name">>, list_to_binary(GroupId), Object),
    UpdatedName = maps:put(<<"object">>, UpdatedObject, Statement),

    generate_statement(UpdatedName, DynData, "teacher").

generate_question_start({_Pid, DynData}) ->
    GroupId = proplists:get_value(group_id , DynData),
    QuestionCounter = proplists:get_value(question_counter, DynData),

    Statement = ?XAPI_QUESTION_START_STATEMENT,

    Actor = maps:get(<<"actor">>, Statement),
    UpdatedActor = maps:put(<<"name">>, list_to_binary(GroupId), Actor),
    UpdatedActorName = maps:put(<<"actor">>, UpdatedActor, Statement),

    ObjectId1 = <<"https://ichallenge.dev.idg.aksorn.com/games/โครงสร้างและหน้าที่อวัยวะในระบบหายใจ 1/">>,
    ObjectId = <<ObjectId1/binary, QuestionCounter/binary>>,


    % Gotta update the res and name in correctResponsesPattern
    Object = maps:get(<<"object">>, Statement),
    Definition = maps:get(<<"definition">>, Object),

    CorrectResponsesPattern = maps:put(<<"correctResponsesPattern">>, [select_number_from_range(1,4)] , Definition),
    DefinitionName = maps:put(<<"name">>, #{<<"th">> => list_to_binary(integer_to_list(QuestionCounter))} , CorrectResponsesPattern),
    

    UpdatedDefinition = maps:put(<<"definition">>, DefinitionName, Object),
    UpdatedObject = maps:put(<<"id">>, ObjectId, UpdatedDefinition),
    UpdatedName = maps:put(<<"object">>, UpdatedObject, UpdatedActorName),

    generate_statement(UpdatedName, DynData).

generate_student_ans({_Pid, DynData}) ->
    StudentName = proplists:get_value(student_ans_counter, DynData),
    generate_statement(?XAPI_STUDENT_ANS_STATEMENT, DynData, StudentName).

generate_question_end({_Pid, DynData}) ->
    generate_statement(?XAPI_QUESTION_END_STATEMENT, DynData).

generate_game_end({_Pid, DynData}) ->
    generate_statement(?XAPI_GAME_END_STATEMENT, DynData).


generate_statement(StatementMap, DynData, Name) ->
    RegistrationId = proplists:get_value(registration_id, DynData),
    StatementId = proplists:get_value(statement_id, DynData),


    UpdatedTimestamp = maps:put(<<"timestamp">>, list_to_binary(create_timestamp()), StatementMap),

    % Update template id with the actual id
    UpdatedIdBody = maps:put(<<"id">>, list_to_binary(StatementId), UpdatedTimestamp),

    % Update actor name
    Actor = maps:get(<<"actor">>, UpdatedIdBody),
    UpdatedActor = maps:put(<<"name">>, list_to_binary(Name), Actor),
    UpdatedName = maps:put(<<"actor">>, UpdatedActor, UpdatedIdBody),

    % Update template registration id with the actual id
    Context = maps:get(<<"context">>, UpdatedIdBody),
    UpdatedContext = maps:put(<<"registration">>, list_to_binary(RegistrationId), Context),
    UpdatedMap = maps:put(<<"context">>, UpdatedContext, UpdatedName),

    jsx:encode(UpdatedMap).

create_timestamp() ->
    %% Get the local time
    {{YYYY, MM, DD}, {HH, MI, SS}} = erlang:localtime(),

    ISO8601 =
        io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ", [YYYY, MM, DD, HH, MI, SS]),

    %% Convert the formatted date and time to a binary
    binary_to_list(list_to_binary(ISO8601)).

generate_statement(StatementMap, DynData) ->
    Name = "null",
    generate_statement(StatementMap, DynData, Name).

uuid({_Pid, _DynData}) ->
    UUID = uuid_cus:to_string(uuid_cus:uuid1()),
    UUID.

select_number_from_range(Min, Max) ->
    Min + rand:uniform(Max - Min + 1) - 1.





    
