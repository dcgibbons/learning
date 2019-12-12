#!/usr/bin/env escript
%
% day6.escript
% Advent of Code 2019 - Day 6
% https://adventofcode.com/2019/day/6
%
% Chad Gibbons
% December 10, 2019
%

main([Filename]) ->
    {ok, File} = file:open(Filename, [read]),
    
    OrbitsMapData = read_orbits_map(File),
    io:fwrite("OrbitsData=~p~n", [OrbitsMapData]),

    G = digraph:new(),
    Fun = fun({Left, Right}) ->
                  V1 = digraph:add_vertex(G, Left),
                  V2 = digraph:add_vertex(G, Right),
                  case digraph:add_edge(G, V2, V1) of
                      {error, _} = Error -> throw(Error);
                      _ -> ok
                  end
          end,
    lists:foreach(Fun, OrbitsMapData),

    io:format("#V:~p #E:~p~n", [digraph:no_edges(G), digraph:no_vertices(G)]), 
    io:format("is_acyclic:~p~n", [digraph_utils:is_acyclic(G)]),
    io:format("is_arborescence:~p~n", [digraph_utils:is_arborescence(G)]),
    io:format("is_tree:~p~n", [digraph_utils:is_tree(G)]),

    {COM, []} = digraph:vertex(G, "COM"),
    Fun2 = fun("COM", AccIn) -> AccIn;
              (V, AccIn) ->
                   Path = digraph:get_path(G, V, COM),
                   io:format("V=~p path=~p~n", [V, Path]),
                   AccIn + length(Path) - 1
           end,
    TotalOrbits = lists:foldl(Fun2, 0, digraph:vertices(G)),
    io:fwrite("TotalOrbit=~p~n", [TotalOrbits]),
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

