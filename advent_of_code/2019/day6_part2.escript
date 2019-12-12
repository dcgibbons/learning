#!/usr/bin/env escript
%
% day6_part2.escript
% Advent of Code 2019 - Day 6
% https://adventofcode.com/2019/day/6
%
% Chad Gibbons
% December 10, 2019
%

main([Filename]) ->
    {ok, File} = file:open(Filename, [read]),
    
    OrbitsMapData = read_orbits_map(File),

    G = digraph:new(),
    Fun = fun({Left, Right}) ->
                  V1 = digraph:add_vertex(G, Left),
                  V2 = digraph:add_vertex(G, Right),
                  digraph:add_edge(G, V2, V1),
                  digraph:add_edge(G, V1, V2)  % so we can backtrack
          end,
    lists:foreach(Fun, OrbitsMapData),

    % determine the total # of orbits from V -> COM
    {COM, []} = digraph:vertex(G, "COM"),
    Fun2 = fun("COM", AccIn) -> AccIn;
              (V, AccIn) ->
                   Path = digraph:get_path(G, V, COM),
                   AccIn + length(Path) - 1
           end,
    TotalOrbits = lists:foldl(Fun2, 0, digraph:vertices(G)),
    io:fwrite("TotalOrbit=~p~n", [TotalOrbits]),

    % find the first object SAN is oribiting
    {SAN, []} = digraph:vertex(G, "SAN"),
    [SANOrbiting] = digraph:out_neighbours(G, SAN),
    io:fwrite("SAN is orbiting:~p~n", [SANOrbiting]),

    % find YOU and then the path from it to the previously found object
    {YOU, []} = digraph:vertex(G, "YOU"),
    Path = digraph:get_short_path(G, YOU, SANOrbiting),
    io:fwrite("# of transfers required: ~p~n", [length(Path)-2]),  % -2 to remove start/end objects

    ok.

read_orbits_map(File) ->
    read_orbits_map(File, []).

read_orbits_map(File, MapData) ->
    case io:get_line(File, "") of
        {error, _} = Error -> Error;
        eof -> MapData;  % base case, all done
        Data -> read_orbits_map(File, MapData ++ [parse_orbit(Data)])
    end.

parse_orbit(Data) ->
    [Left, Right] = string:tokens(Data, ")\n"),
    {Left, Right}.

