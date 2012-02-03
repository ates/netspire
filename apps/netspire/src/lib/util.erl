-module(util).

-export([timestamp/0, to_hex/1, do_bxor/2, binary_to_hex_string/1]).

timestamp() ->
    {MegaSeconds, Seconds, _} = erlang:now(),
    MegaSeconds * 1000000 + Seconds.

to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

do_bxor(B1, B2) ->
    do_bxor(B1, B2, <<>>).
do_bxor(<<>>, B2, Ret) ->
    list_to_binary([Ret, B2]);
do_bxor(<<I1, Rest1/binary>>, <<I2, Rest2/binary>>, Acc) ->
    do_bxor(Rest1, Rest2, list_to_binary([Acc, I1 bxor I2])).

binary_to_hex_string(Bin) ->
    list_to_hex_string(binary_to_list(Bin)).

%%
%% Internal functions
%% 
hex(N) when N < 10 -> $0 + N;
hex(N) when N >= 10, N < 16 -> $A + (N - 10).

list_to_hex_string([]) -> [];
list_to_hex_string([H | T]) ->
    to_hex(H) ++ list_to_hex_string(T).

%% Tests
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

to_hex_test() ->
    ?assert(to_hex(10) =:= "0A"),
    ?assert(to_hex(16) =:= "10"),
    ?assert(to_hex(255) =:= "FF").

do_bxor_test() ->
    ?assert(do_bxor(<<1,2>>, <<3,4>>) =:= <<2,6>>).

binary_to_hex_string_test() ->
    ?assert(binary_to_hex_string(<<"erlang">>) =:= "65726C616E67").

-endif.
