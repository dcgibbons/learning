#!/usr/bin/env escript
%
% day4.escript
% Advent of Code 2019 - Day 4
% https://adventofcode.com/2019/day/4
%
% Chad Gibbons
% December 8, 2019
%

meets_rules(Number) ->
    % surely there's some cool mathematical way of doing this?
    Digits = [list_to_integer([Char]) || Char <- integer_to_list(Number)],
    case catch lists:foldl(
        fun(CurrDigit, {[], false}) ->
            {[CurrDigit], false};

           (CurrDigit, {AccIn, true}) ->
            PrevDigit = lists:last(AccIn),
            case CurrDigit of
                _ when CurrDigit < PrevDigit -> throw(false);
                _ -> {AccIn ++ [CurrDigit], true}
            end;

           (CurrDigit, {AccIn, false}) ->
            PrevDigit = lists:last(AccIn),
            case CurrDigit of
                _ when CurrDigit < PrevDigit -> throw(false);
                _ when CurrDigit =/= PrevDigit-> {AccIn ++ [CurrDigit], false};
                _ when CurrDigit =:= PrevDigit-> {AccIn ++ [CurrDigit], true}
            end
        end, {[], false}, Digits) of
        false -> false;
        {_, false} -> false;
        {_, true} -> true
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

