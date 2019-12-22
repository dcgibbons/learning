#!/usr/bin/env escript
%! -smp enable -sname day3 debug verbose
%
% day3_part2.escript
% Advent of Code 2019 - Day 3, Part 2
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
    [_|Routes] = lists:foldl(
        fun(NextMotion, WirePositions) ->
            LastPosition = lists:last(WirePositions),
            WirePositions ++ trace(NextMotion, LastPosition)
        end,
        [Origin],  % the wire starts at the specified origin
        Motions),
    Routes.


wire_steps_to([Point|_], Point) ->
    1; % all done, we found the intersection
wire_steps_to([_|T], Point) ->
    1 + wire_steps_to(T, Point).

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
        fun(Intersection, LowestSoFar) ->
            % find the intersection in each wire's path, counting how many
            % steps it took to get there
            % TODO: recalculating steps each time is super slow is an obvious
            % algorithm improvement point
            Wire1Steps = wire_steps_to(Wire1Routes, Intersection),
            Wire2Steps = wire_steps_to(Wire2Routes, Intersection),
            io:fwrite("Intersection:~p Wire1Steps:~p Wire2Steps:~p~n",
                      [Intersection, Wire1Steps, Wire2Steps]),
            case Wire1Steps + Wire2Steps of
                Dist when Dist < LowestSoFar -> Dist;
                _ -> LowestSoFar
            end
        end,
        2147483647,
        Intersections),

    io:fwrite("Closest=~p~n", [Closest]),
    ok;

main(_) ->
    io:format("Usage: day3 [input]\n"),
    halt(1).

