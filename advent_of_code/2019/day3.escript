#!/usr/bin/env escript
%! -smp enable -sname day3 debug verbose
%
% day3.escript
% Advent of Code 2019 - Day 3
%
% Chad Gibbons
% December 8, 2019
%

% from a given origin point and a motion, generate a set of X,Y coordinates
% for the wire from that origin
trace({$L, Dist}, {X, Y}) ->
    [{X - Dx, Y} || Dx <- lists:seq(1, Dist)];
trace({$R, Dist}, {X, Y}) ->
    [{X + Dx, Y} || Dx <- lists:seq(1, Dist)];
trace({$U, Dist}, {X, Y}) ->
    [{X, Y - Dy} || Dy <- lists:seq(1, Dist)];
trace({$D, Dist}, {X, Y}) ->
    [{X, Y + Dy} || Dy <- lists:seq(1, Dist)].

% calculate the manhattan distance between two points
manhattan_distance({X1, Y1}, {X2, Y2}) ->
    abs(X1 - X2) + abs(Y1 - Y2).

% convert the raw input data to a set of wire routes
wire_data_to_routes(Data, Origin) ->
    % create a list of motion tuples, e.g. "U1010" -> {$U,1010}
    Motions = lists:map(
        fun([Dir|Dist]) ->
            {Int, _Rest} = string:to_integer(Dist),
            {Dir, Int}
        end,
        string:tokens(Data, ",")),

    % trace each specified motion from the last wire point calculated, starting
    % with the origin, producing a complete set of all points the wire occupies
    lists:foldl(
        fun(NextMotion, WirePositions) ->
            LastPosition = lists:last(WirePositions),
            WirePositions ++ trace(NextMotion, LastPosition)
        end,
        [Origin],  % the wire starts at the specified origin
        Motions).

main([Filename]) ->
    {ok, File} = file:open(Filename, [read]),

    Origin = {0, 0},

    Wire1Routes = wire_data_to_routes(io:get_line(File, ""), Origin),
    Wire2Routes = wire_data_to_routes(io:get_line(File, ""), Origin),

    Intersections = sets:intersection(sets:from_list(Wire1Routes),
                                      sets:from_list(Wire2Routes)),

    % look through all the intersctions finding the closest manhattan distance
    % to the origin
    Closest = sets:fold(
        fun(Elem, AccIn) ->
            case manhattan_distance(Elem, Origin) of
                _ when Elem == Origin ->
                    AccIn;
                Dist when Dist < AccIn ->
                    io:fwrite("closer intersection: ~p [~p]~n", [Elem, Dist]),
                    Dist;
                _ ->
                    AccIn
            end
        end,
        2147483647,
        Intersections),

    io:fwrite("Closest=~p~n", [Closest]),
    ok;

main(_) ->
    io:format("Usage: day3 [input]\n"),
    halt(1).

