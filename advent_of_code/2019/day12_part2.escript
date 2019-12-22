#!/usr/bin/env escript
%%! -smp enable
%
% day12_part2.escript
% Advent of Code 2019 - Day 12
% https://adventofcode.com/2019/day/12#part2
%
% Chad Gibbons
% December 22, 2019
%
% Solution Note: I totally had to give in and consult the reddit oracles for
% advice on this one. The brute force approach (checked in as a previous version
% of this source file) was easy but useless. This solution takes advantage of
% that each X,Y,Z position can be independently computed and the cycle count for
% each calculated. Then finding the least-common-multiplier for all 3 values
% give you the total cycle count.
%

-mode(compile).

main([Filename]) ->
    Moons = prepare_data(Filename),

    % Calculate total # of steps required for each X, Y, and Z axis
    ApplyFun = fun(Pos) ->
        apply_loop(Pos, 1, Moons, Moons)
    end,
    [X, Y, Z] = Steps = lists:map(ApplyFun, lists:seq(1, 3)),  % X, Y, and Z...
    io:fwrite("Steps: ~p~n", [Steps]),

    % Calculate the least common mulitple for all 3 values to get the total
    % number of steps required
    TotalSteps = lcm(X, Y, Z),
    io:fwrite("Total Steps to return to original: ~p~n", [TotalSteps]),
    ok.

%
% apply gravity and velocity to all moons, but for one axis at a time - do this
% in a loop until the moon position/velocity for that axis returns to its
% original value, and then return the number of steps it took
%
apply_loop(PosNum, Step, OriginalMoons, CurrentMoons) ->
    UpdatedMoons = apply_velocity(PosNum, apply_gravity(PosNum, CurrentMoons)),
    case UpdatedMoons of
        C when C == OriginalMoons -> Step;
        _ -> apply_loop(PosNum, Step + 1, OriginalMoons, UpdatedMoons)
    end.

%
% apply ye olde gravity, but only for the specified axis
%
apply_gravity(PosNum, Moons) ->
    CompareFun = fun({OtherPos, _} = _OtherMoon,
                     {ThisPos, ThisVelocity} = _ThisMoon) ->
        Pos0 = element(PosNum, ThisPos),
        Pos1 = element(PosNum, OtherPos),
        Delta = compare_gravity(Pos0, Pos1),
        {ThisPos, setelement(PosNum,
                             ThisVelocity,
                             element(PosNum, ThisVelocity) + Delta)}
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
% apply velocity to each moon, but only for the specified axis
%
apply_velocity(PosNum, Moons) ->
    VelocityFun = fun({MoonPos, MoonVelocity} = _Moon) ->
        Pos = element(PosNum, MoonPos),
        Delta = element(PosNum, MoonVelocity),
        {setelement(PosNum, MoonPos, Pos + Delta), MoonVelocity}
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

%
% greatest common denominator and least common multiple helpers
%
gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).
lcm(A, B) -> abs(A * B div gcd(A, B)).
lcm(A, B, C) -> lcm(A, lcm(B, C)).

