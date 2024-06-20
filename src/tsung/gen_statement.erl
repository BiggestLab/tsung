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
    generate_statement(?XAPI_QUESTION_START_STATEMENT, DynData).

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
    {Year, Month, Day, Hour, Minute, Second} = calendar:universal_time(),
    Millisecond = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) rem 1000,
    Format = "~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~3..0wZ",
    Timestamp = io_lib:format(Format, [Year, Month, Day, Hour, Minute, Second, Millisecond]),
    lists:flatten(Timestamp).

generate_statement(StatementMap, DynData) ->
    Name = "null",
    generate_statement(StatementMap, DynData, Name).

uuid({_Pid, _DynData}) ->
    UUID = uuid_cus:to_string(uuid_cus:uuid1()),
    UUID.

    





    
