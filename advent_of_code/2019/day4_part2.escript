#!/usr/bin/env escript
%
% day4_part2.escript
% Advent of Code 2019 - Day 4
% https://adventofcode.com/2019/day/4
%
% Chad Gibbons
% December 8, 2019
%

meets_rules(Number) ->
    % surely there's some cool mathematical way of doing this?
    Digits = [list_to_integer([Char]) || Char <- integer_to_list(Number)],

    % accumulate how much consecutive digits we encounter
    Fun = fun(CurrDigit, [{H,N}|T] = AccIn) ->
                  PrevDigit = H,
                  case CurrDigit of
                      _ when CurrDigit < PrevDigit ->
                          throw(false);
                      _ when CurrDigit == PrevDigit ->
                          [{CurrDigit, N+1} | T];
                      _ when CurrDigit /= PrevDigit ->
                          [{CurrDigit, 1}] ++ AccIn
                  end;
             (CurrDigit, [] = AccIn) ->
                  [{CurrDigit, 1}]
          end,
    case catch lists:foldl(Fun, [], Digits) of
        false -> false;
        % then determine if there were any doubles (singles and more than
        % doubles do not count towards a valid password)
        C when is_list(C) -> 
            Doubles = [Digit || {Digit, Count} <- C, Count == 2],
            case length(Doubles) of
                0 -> false;
                _ -> true
            end
    end.

main([Range]) ->
    [S, E] = string:tokens(Range, "-"),
    io:fwrite("Range start:~p end:~p~n", [S, E]),

    {Start, _} = string:to_integer(S),
    {End, _} = string:to_integer(E),

    All = lists:seq(Start, End),
    io:fwrite("All #s length:~p~n", [length(All)]),

    Numbers = [X || X <- All, meets_rules(X)],
    io:fwrite("#s meeting rules: ~p~n", [length(Numbers)]),
    ok;

main(_) ->
    io:fwrite("Usage: day4.escript [start-end]~n"),
    halt(1).

