%%%----------------------------------------------------------------------
%%% Copyright: (c) 2011-2013 MoreloSoft - Hova Networks S.A.P.I. de C.V.  
%%% All rights reserved.
%%%
%%% Redistribution and use in any form, with or without modification, 
%%% is strictly prohibited.
%%%
%%% Created by : Jorge Garrido <jorge.garrido@morelosoft.com> [zgb]
%%%----------------------------------------------------------------------
-module(fingerlprint_lib).

-export([rpcoin/3, carry/1, encode_iso_19794/1]).

%%-----------------------------------------------------------------------
%% send mesage port driver
rpcoin(Port, _From, Msg) ->
    Port ! {self(), {command, Msg}},
    receive 
	{Port, {data, Data}} ->
		decode(Data)
    end.

%%-----------------------------------------------------------------------
%% encode FMD on ISO 19794
%% WARNING: This function is deprecated, since we use our impl of HOVERBOX!
encode_iso_19794(FMD) ->
    encode_iso_19794(FMD, [], 'out').
encode_iso_19794([], Acc,_)       -> Acc;
encode_iso_19794([92 | []], Acc, _) -> Acc;
encode_iso_19794([92 | T], Acc, _) ->
    [ X | Ts ] = T,
    {Accumulator, List} = case X of 92 -> {[92], Ts}; _ -> {[], T} end, 
    encode_iso_19794(List, Acc ++ Accumulator, 'in');
encode_iso_19794([ 110 | T], Acc, 'in') ->
    encode_iso_19794(T, Acc ++ [10], 'out');
encode_iso_19794([ 116 | T], Acc, 'in') ->
    encode_iso_19794(T, Acc ++ [9], 'out');
encode_iso_19794([ 114 | T], Acc, 'in') ->
    encode_iso_19794(T, Acc ++ [13], 'out');
encode_iso_19794([ 102 | T], Acc, 'in') ->
    encode_iso_19794(T, Acc ++ [12], 'out');
encode_iso_19794([ 98 | T], Acc, 'in') ->
    encode_iso_19794(T, Acc ++ [8], 'out');
encode_iso_19794([117 | T], Acc, 'in')-> 
    [A,B,C,D | Ts] = T,
    {ok, Dec, _} = io_lib:fread("~16u", [A,B,C,D]),
    encode_iso_19794(Ts, Acc ++ Dec, 'out');
encode_iso_19794([H | T], Acc, _N) ->
    encode_iso_19794(T, Acc ++ [H], _N).

%%-----------------------------------------------------------------------
%% encode size for FMD as unsigned bytes (valid on a binary)
carry(N) ->
    Carry = binary:encode_unsigned(N),
    case erlang:byte_size(Carry) of
	1 -> <<0, Carry/binary>>;
	_ -> Carry
    end.

%%-----------------------------------------------------------------------
%% decode response of the  port driver
decode([49]) ->
    {access, 'YES'};
decode([48]) ->
    {access, 'NO'};
decode("M")  ->
    {error, 'Cannot allocate memmory'};
decode(Rpc)    ->
    {error, 'UNKNOWN', Rpc}.
