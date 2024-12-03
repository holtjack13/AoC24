-module(day2).

-export([solve_part_1/0, solve_part_2/0]).

solve_part_1() ->
    {ok, Fd} = file:open(input, read),
    Reports = parse_reports(Fd, []),
    check_all_reports(Reports, 0, 0).

solve_part_2() ->
    {ok, Fd} = file:open('input_fail', read),
    Reports = parse_reports(Fd, []),
    check_all_reports(Reports, 0, 1).

parse_reports(Fd, Reports) ->
    case file:read_line(Fd) of
        eof ->
            Reports;
        {ok, ReportString} ->
            AtoI =
                fun(S) ->
                   {Int, _} = string:to_integer(S),
                   Int
                end,
            parse_reports(Fd,
                          Reports
                          ++ [lists:map(AtoI,
                                        string:tokens(
                                            string:trim(ReportString), " "))])
    end.

check_all_reports([], NumberOfSafeReports, _) -> NumberOfSafeReports;
check_all_reports([Report | Rest], NumberOfSafeReports, MistakeAllowance) ->
    ReportStatus =
        case check_report(Report, ascending, MistakeAllowance) =:= safe
             orelse check_report(Report, descending, MistakeAllowance) =:= safe
        of
            true -> 1;
            false -> 0
        end,
    check_all_reports(Rest, NumberOfSafeReports + ReportStatus, MistakeAllowance).


% If we've run out of comparisons to make, the report is safe
check_report([_], _, _) -> safe;

% If the ascending or descending sequence breaks at any point, the report is
% unsafe
check_report([L1, L2 | Rest], ascending, MistakeAllowance) when L1 >= L2 ->
    if MistakeAllowance =< 0 -> unsafe;
       true -> check_report([L2] ++ Rest, ascending, MistakeAllowance - 1)
    end;
check_report([L1, L2 | Rest], descending, MistakeAllowance) when L1 =< L2 ->
    if MistakeAllowance =< 0 -> unsafe;
       true -> check_report([L1] ++ Rest, descending, MistakeAllowance - 1)
    end;

% If the jump between values has a magnitude greater than 3 or less than 1
% i.e. both values are equal, the report is unsafe
check_report([L1, L2 | Rest], Direction, MistakeAllowance) when abs(L1 - L2) > 3 orelse L1 =:= L2 ->
    if MistakeAllowance =< 0 -> unsafe;
       true -> check_report([L2] ++ Rest, Direction, MistakeAllowance - 1)
    end;

% In any other scenario, we need to continue scanning the report
check_report([_ | Rest], Direction, MistakeAllowance) ->
    check_report(Rest, Direction, MistakeAllowance).
