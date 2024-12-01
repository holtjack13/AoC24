-module(day1).

-export([
    calculate_total_distance/0,
    calculate_similariy_score/0
]).

% Naive solution
% 1. Parse both columns into two seperate lists, sorting 
% 2. Iterate through both lists, adding up abs(left - right)

% --------------------------- PART 1 --------------------------------------%
calculate_total_distance() ->
    {ok, Fd} = file:open('input', read),
    {Left, Right} = prepare_lists(Fd, [], []),
    {SortedLeft, SortedRight} = {lists:sort(Left), lists:sort(Right)},
    calculate_total_distance_inner(SortedLeft, SortedRight, 0).

prepare_lists(Fd, Left, Right) ->
    case file:read_line(Fd) of
        eof -> {Left, Right};
        {ok, Line} -> 
            [LeftValue, RightValue] = string:tokens(string:trim(Line), " "),
            {LeftInt, _} = string:to_integer(LeftValue),
            {RightInt, _} = string:to_integer(RightValue),
            prepare_lists(
                Fd, 
                lists:append(Left, [LeftInt]),
                lists:append(Right, [RightInt])
            )
    end.

calculate_total_distance_inner([], [], Acc) -> Acc;
calculate_total_distance_inner([L | LRest], [R | RRest], Acc) ->
    calculate_total_distance_inner(LRest, RRest, Acc + abs(L - R)).

% --------------------------- PART 2 --------------------------------------%

calculate_similariy_score() ->
    {ok, Fd} = file:open('input', read),
    {Left, Right} = prepare_lists(Fd, [], []),
    FrequencyMap = build_frequency_map(Right, maps:new()),
    calculate_similarity_score_inner(Left, FrequencyMap, 0).

build_frequency_map([], Map) -> Map;
build_frequency_map([R | Rest], Map) -> 
    CurrentCount = maps:get(R, Map, 0),
    UpdatedMap = maps:put(R, CurrentCount + 1, Map),
    build_frequency_map(Rest, UpdatedMap).

calculate_similarity_score_inner([], _, Acc) -> Acc;
calculate_similarity_score_inner([L | Rest], FrequencyMap, Acc) ->
    calculate_similarity_score_inner(
      Rest, 
      FrequencyMap, 
      Acc + (L * maps:get(L, FrequencyMap, 0))
    ).
