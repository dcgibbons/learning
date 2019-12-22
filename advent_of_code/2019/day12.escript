#!/usr/bin/env escript
%
% day12.escript
% Advent of Code 2019 - Day 12
% https://adventofcode.com/2019/day/12
%
% Chad Gibbons
% December 22, 2019
%

-mode(compile).

main([Filename, Steps]) ->
    Moons = prepare_data(Filename),

    TimeStepFun = fun(_Step, IncomingMoons) ->
        apply_velocity(apply_gravity(IncomingMoons))
    end,

    UpdatedMoons = lists:foldl(TimeStepFun, Moons, lists:seq(1, to_integer(Steps))),
    io:fwrite("Moons after simulation: ~p~n", [UpdatedMoons]),

    TotalEnergy = total_energy(UpdatedMoons),
    io:fwrite("Total energy in system: ~p~n", [TotalEnergy]),
    ok.

%
% calculate the total energy in the system
%
total_energy(Moons) ->
    MoonEnergyFun = fun({{X0, Y0, Z0}, {DX, DY, DZ}} = _Moon, AccIn) ->
        PotentialEnergy = abs(X0) + abs(Y0) + abs(Z0),
        KineticEnergy = abs(DX) + abs(DY) + abs(DZ),
        AccIn + (PotentialEnergy* KineticEnergy)
    end,
    lists:foldl(MoonEnergyFun, 0, Moons).

%
% apply gravity from all other moons to each moon
%
apply_gravity(Moons) ->

    CompareFun = fun({{X1,Y1,Z1},_} = _OtherMoon,
                     {{X0,Y0,Z0},{DX,DY,DZ}} = _ThisMoon) ->
        DeltaX = compare_gravity(X0, X1),
        DeltaY = compare_gravity(Y0, Y1),
        DeltaZ = compare_gravity(Z0, Z1),
        {{X0, Y0, Z0}, {DX + DeltaX, DY + DeltaY, DZ + DeltaZ}}
    end,

    GravityFun = fun(Moon, UpdatedMoons) ->
        OtherMoons = lists:delete(Moon, Moons),
        UpdatedMoon = lists:foldl(CompareFun, Moon, OtherMoons),
        [UpdatedMoon | UpdatedMoons]
    end,

    lists:foldl(GravityFun, [], Moons).

%
% compares a gravity component of one moon to another and returns the gravity
% delta of 1, -1, or 0 (unchanged)
%
compare_gravity(This, Other) ->
    case This of
        N when N < Other -> 1;
        N when N > Other -> -1;
        _ -> 0
    end.

%
% apply velocity to each moon
%
apply_velocity(Moons) ->
    VelocityFun = fun({{X0, Y0, Z0}, {DX, DY, DZ} = Deltas} = _Moon) ->
        {{X0 + DX, Y0 + DY, Z0 + DZ}, Deltas}
    end,
    lists:map(VelocityFun, Moons).

%
% reads the input data, parses it, and prepares the moon's locations and
% velocities for further processing
%
prepare_data(Filename) ->
    {ok, BinData} = file:read_file(Filename),
    Moons = part(parse(tokenize(BinData), [])),
    MoonsAndVelocities = [{Moon, {0,0,0}} || Moon <- Moons],
    MoonsAndVelocities.

%
% tokenize input data in this format:
%   <x=-1, y=0, z=2>
%   <x=2, y=-10, z=-7>
%   <x=4, y=-8, z=8>
%   <x=3, y=5, z=-1>
% into just letter, number string tokens, e.g. [<<"x">>, <<"-6">>, ...]
%
tokenize(BinData) when is_binary(BinData) -> string:lexemes(BinData, "<>=, \n").

%
% parse letter, number string tokens into just a list of integers
% e.g. [<<"x">>, <<"-6">>, ...] -> [-6, ...]
%
parse([<<"x">>, Value | T], Acc) -> parse(T, [to_integer(Value) | Acc]);
parse([<<"y">>, Value | T], Acc) -> parse(T, [to_integer(Value) | Acc]);
parse([<<"z">>, Value | T], Acc) -> parse(T, [to_integer(Value) | Acc]);
parse([], Acc) -> lists:reverse(Acc).

%
% partition the sequence of integers into x,y,z tuples
% e.g. [-6, 1, 3, ...] -> [{-6,1,3}, ...]
%
part(List) -> part(List, []).
part([], Acc) -> lists:reverse(Acc);
part([H], Acc) -> lists:reverse([[H]|Acc]);
part([H1,H2,H3|T], Acc) -> part(T, [{H1,H2,H3}|Acc]).

%
% shortcut function to convert a binary string directly into an integer
% e.g. <<"-6">> -> -6
%
to_integer(L) when is_list(L) -> list_to_integer(L);
to_integer(B) when is_binary(B) -> list_to_integer(binary_to_list(B)).

