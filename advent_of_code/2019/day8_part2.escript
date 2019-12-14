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

    Image = lists:reverse(decode_image(Layers, W, H)),

    % write the resulting image bitmap into gnuplot format
    io:fwrite("plot '-' matrix with image~n", []),
    lists:foreach(
        fun(Row) ->
            lists:foreach(
                fun(Pixel) ->
                    io:fwrite("~c ", [Pixel])
                end,
                Row),
            io:fwrite("~n", [])
        end,
        Image),
    io:fwrite("e~ne~n", []),
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

decode_pixel(Layers, W, _H, X, Y) ->
    lists:foldl(
        fun(L, PixelIn) ->
            Pixel = lists:nth(Y*W + X + 1, L),
            case Pixel of
                $2 when PixelIn =:= undefined -> undefined;
                _ when PixelIn =:= undefined -> Pixel;
                _ -> PixelIn
            end
        end,
        undefined,
        Layers).

decode_image(Layers, W, H) ->
    Fun1 = fun(Y) ->
        lists:foldl(
            fun(X, LineIn) ->
                LineIn ++ [decode_pixel(Layers, W, H, X, Y)]
            end,
            [],
            lists:seq(0, W-1))
        end,
    lists:map(Fun1, lists:seq(0, H-1)).

