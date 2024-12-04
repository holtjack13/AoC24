-module(day3).

-export([
    solve_part_1/0,
    solve_part_2/0
]).

% ------------------------------------ PART 1 ------------------------------- %
solve_part_1() -> 
    {ok, CorruptDump} = file:read_file('input'),
    {ok, SearchPattern} = re:compile("mul\\((\\d{1,3}),(\\d{1,3})\\)"),
    {match, Instructions} = re:run(CorruptDump, SearchPattern, [global, {capture, all, list}]),
    lists:sum(lists:map(fun(I) -> run_instruction(I) end, Instructions)).

run_instruction([_, O1, O2]) ->
    {I1, _} = string:to_integer(O1),
    {I2, _} = string:to_integer(O2),
    I1 * I2.

% ------------------------------------ PART 2 ------------------------------- %
solve_part_2() ->
    {ok, CorruptDump} = file:read_file('input'),
    {ok, SearchPattern} = re:compile("mul\\((\\d{1,3}),(\\d{1,3})\\)|do\\(\\)|don\\'t\\(\\)"),
    {match, Instructions} = re:run(CorruptDump, SearchPattern, [global, {capture, all, list}]),
    run_all_instructions(Instructions, 0, enabled).

run_all_instructions([], Acc, _) -> Acc;
run_all_instructions([I | Rest], Acc, IsEnabled) ->
    case {I, IsEnabled} of
        {["do()"], _} -> run_all_instructions(Rest, Acc, enabled);
        {["don't()"], _} -> run_all_instructions(Rest, Acc, disabled);
        {_, disabled} -> run_all_instructions(Rest, Acc, disabled);
        {Mul, enabled} -> run_all_instructions(Rest, Acc + run_instruction(Mul), enabled)
    end.

    

