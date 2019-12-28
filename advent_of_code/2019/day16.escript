#!/usr/bin/env escript
%
% day16.escript
% Advent of Code 2019 - Day 16
% https://adventofcode.com/2019/day/16
%
% Chad Gibbons
% December 27, 2019
%

-mode(compile).

main([Filename, Steps]) ->
    {ok, Data} = prepare_data(Filename),
    BasePattern = [0, 1, 0, -1],

    FFTFun = fun(_Elem, AccIn) ->
        fft(AccIn, BasePattern)
    end,
    FinalData = lists:foldl(FFTFun, Data, lists:seq(1, to_integer(Steps))),

    io:fwrite("FinalData=~p~n", [lists:sublist(FinalData, 1, 8)]),
    ok.

prepare_data(Filename) ->
    {ok, BinData} = file:read_file(Filename),
    Fun = fun($\n, AccIn) ->
                  AccIn;
             (C, AccIn) ->
                  [C - $0 | AccIn]
          end,
    {ok, lists:reverse(lists:foldl(Fun, [], binary_to_list(BinData)))}.


repeat_pattern(MaxLength, NRepeats, BasePattern) ->
    Repeats = trunc(ceil(MaxLength / (length(BasePattern) * NRepeats))) + 1,
    RepeatFun = fun(_Elem, AccIn) ->
                    Fun = fun(Digit, X) ->
                                  lists:flatten(lists:duplicate(NRepeats, Digit), X)
                          end,
                    lists:foldl(Fun, AccIn, BasePattern)
                end,
    R = lists:reverse(lists:foldl(RepeatFun, [], lists:seq(1, Repeats))),
    lists:sublist(R, 2, MaxLength).

fft(Signal, BasePattern) ->
    SignalLength = length(Signal),

    ProcFun = fun({A, B}, AccIn) ->
                      AccIn + A * B
              end,

    Fun = fun(Index, AccIn) ->
        RepeatedPattern = repeat_pattern(SignalLength, Index, BasePattern),
        A = lists:foldl(ProcFun, 0, lists:zip(Signal, RepeatedPattern)),
        [abs(A) rem 10 | AccIn]
    end,

    lists:reverse(lists:foldl(Fun, [], lists:seq(1, SignalLength))).

%
% shortcut function to convert a binary string directly into an integer
% e.g. <<"-6">> -> -6
%
to_integer(L) when is_list(L) -> list_to_integer(L);
to_integer(B) when is_binary(B) -> list_to_integer(binary_to_list(B)).

