#!/usr/bin/env escript
%
% day8.escript
% Advent of Code 2019 - Day 8
%
% Chad Gibbons
% December 12, 2019
%

main([Filename, Width, Height]) ->
    {ok, BinData} = file:read_file(Filename),
    Data = lists:droplast(binary_to_list(BinData)),  % get rid of newline

    W = list_to_integer(Width),
    H = list_to_integer(Height),

    Layers = chunks(Data, W * H),
    DigitCount = count_digits(Layers),

    FewestZerosFun = fun({Zeros, _Ones, _Twos} = C, {Fewest, Counts}) ->
                             case Zeros of
                                 0 -> {Fewest, Counts};
                                 _ when Zeros =< Fewest -> {Zeros, C};
                                 _ -> {Fewest, Counts}
                             end
                     end,
    FewestZeros = lists:foldl(FewestZerosFun, {999999, []}, DigitCount),
    io:fwrite("fewest zeros=~p~n", [FewestZeros]),
    ok;

main(_) ->
    io:fwrite("Usage: day8.escript [input filename] [width] [height]~n"),
    ok.

chunks([], _) -> [];
chunks(List, Len) when Len > length(List) ->
    [List];
chunks(List, Len) ->
    {Head, Tail} = lists:split(Len, List),
    [Head | chunks(Tail, Len)].

count_digits(Data) ->
    LayerFun = fun(Digit, {Zeros, Ones, Twos} = AccIn) ->
                       case Digit of
                           $0 -> {Zeros+1, Ones, Twos};
                           $1 -> {Zeros, Ones+1, Twos};
                           $2 -> {Zeros, Ones, Twos+1};
                           _ -> AccIn
                       end
               end,
    Fun = fun(Layer) ->
                  lists:foldl(LayerFun, {0,0,0}, Layer)
          end,
    lists:map(Fun, Data).

