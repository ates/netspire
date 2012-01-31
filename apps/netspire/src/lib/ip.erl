-module(ip).

%% API
-export([ip2long/1, address/1, is_macaddr/1, bin_ipv6_to_address/1,
         long2ip/1, in_range/2, range2list/1, broadcast/1, range/1]).

-include("netspire.hrl").

%% Supported formats of MACADDR
%% 08002b:010203, 08002b-010203, 0800.2b01.0203
%% 08-00-2b-01-02-03, 08:00:2b:01:02:03
-define(MAC_FMT1, "^([0-9a-f]{2}([:-]|$)){6}").
-define(MAC_FMT2, "^([0-9a-f]{6}([:-]|$)){2}").
-define(MAC_FMT3, "^([0-9a-f]{4}(\.|$)){3}$").
-define(MAC_REGEXP, string:join([?MAC_FMT1, ?MAC_FMT2, ?MAC_FMT3], "|")).

%% @doc Convert IP address to long integer.
-spec ip2long(ip_address() | tuple() | non_neg_integer()) ->
    non_neg_integer() | {error, einval}.
ip2long(IP) when is_integer(IP) -> IP;
ip2long(IP) when is_list(IP) ->
    case address(IP) of
        {ok, {_, Address}} ->
            ip2long(Address);
        _ ->
            {error, einval}
    end;
ip2long({A, B, C, D}) ->
    (A bsl 24) bor (B bsl 16) bor (C bsl 8) bor D.

%% @doc Parse IP address and returns tuple with IP and protocol type.
-spec address(ip_address()) ->
    {ok, {inet | inet6, inet:ip_address()}} | {error, einval}.
address(IP) when is_list(IP) ->
    case inet_parse:address(IP) of
        {ok, Address} ->
            address(Address);
        _ ->
            {error, einval}
    end;
address(IP) when tuple_size(IP) == 4 ->
    {ok, {inet, IP}};
address(IP) when tuple_size(IP) == 8 ->
    {ok, {inet6, IP}}.

%% @doc Verifies is MAC address is correct.
-spec is_macaddr(string()) -> true | false.
is_macaddr(Address) ->
    case re:run(Address, ?MAC_REGEXP, [{capture, none}, caseless]) of
        match -> true;
        _ -> false
    end.

%% @doc Convert binary to IPv6 address.
-spec bin_ipv6_to_address(binary()) -> ip_address().
bin_ipv6_to_address(Bin) when byte_size(Bin) =:= 16 ->
    list_to_tuple([I || <<I:16>> <= Bin]).

%% @doc Convert long integer to IPv4 address.
-spec long2ip(non_neg_integer()) -> ip_address().
long2ip(IP) when IP =< 4294967295 ->
    {(IP div 16777216) rem 256, (IP div 65536) rem 256, (IP div 256) rem 256, IP rem 256}.

%% @doc Verifies is the IP address included in the specific network.
-spec in_range(ip_address(), string()) -> true | false.
in_range(Address, Network) ->
    {N, M} = parse_address(Network),
    (ip2long(Address) band M) == (ip2long(N) band M).

%% @doc Returns list of all IPv4 addresses of specified range.
%% Supported formats are Network/Mask and FirstAddress-SecondAddress
%% Mask may be integer value or in dot notation
range2list(Address) when is_list(Address) ->
    case string:tokens(Address, "-") of
        [First, Last] ->
            try
                {ok, {inet, {I1, I2, I3, I4}}} = address(First),
                {ok, {inet, {I5, I6, I7, I8}}} = address(Last),
                {I1, I2, I3} = {I5, I6, I7},
                [{I1, I2, I3, I} || I <- lists:seq(I4, I8)]
            catch
                _:_ ->
                    throw({error, invalid_range_specified})
            end;
        _ ->
            {First, Last} = range(Address),
            [long2ip(I) || I <- lists:seq(ip2long(First), ip2long(Last))]
    end.

%% @doc Returns broadcast address of network.
-spec broadcast(string()) -> non_neg_integer().
broadcast(Address) ->
    {IP, Mask} = parse_address(Address),
    Network = ip2long(IP) band ip2long(Mask),
    long2ip(Network bor (bnot ip2long(Mask) band 16#ffffffff)).

%% @doc Returns the first and second address for specific network.
range(Address) ->
    Broadcast = ip2long(broadcast(Address)),
    {IP, Mask} = parse_address(Address),
    case Mask of
        4294967294 -> % 31
            {long2ip((ip2long(IP) band ip2long(Mask))), long2ip(Broadcast)};
        4294967295 -> % 32
            {long2ip(ip2long(IP)), long2ip(ip2long(IP))};
        _ ->
            {long2ip((ip2long(IP) band ip2long(Mask)) + 1), long2ip(Broadcast - 1)}
    end.

%%
%% Internal functions
%%
parse_address(Address) ->
    case string:tokens(Address, "/") of
        [IP, Mask] ->
            try
                M = list_to_integer(Mask),
                {IP, (16#ffffffff bsr (32 - M)) bsl (32 - M)}
            catch
                _:_ -> {IP, ip2long(Mask)}
            end;
        _ -> % assume that Address specified without / and assign 32 as mask
            {Address, ip2long("255.255.255.255")}
    end.

%% Tests
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

ip2long_test() ->
    Tests = [
        {"127.0.0.1", 2130706433},
        {{127,0,0,1}, 2130706433},
        {"0.0.0.0", 0},
        {"255.255.255.255", 4294967295}
    ],
    [?assert(R =:= ip2long(T)) || {T, R} <- Tests].

address_test() ->
    ?assert({ok, {inet, {127,0,0,1}}} =:= address("127.0.0.1")),
    ?assert({ok, {inet6, {0,0,0,0,0,0,0,1}}} =:= address("::1")).

is_macaddr_test() ->
    ?assert(is_macaddr("AB:CD:EF:00:11:22") =:= true),
    ?assert(is_macaddr("AB-CD-EF-00-11-22") =:= true),
    ?assert(is_macaddr("ab:cd:ef:00:11:22") =:= true),
    ?assert(is_macaddr("ab-cd-ef-00-11-22") =:= true),
    ?assert(is_macaddr("ab-cz-ef-00-110-22") =:= false),
    ?assert(is_macaddr("08002b:010203") =:= true),
    ?assert(is_macaddr("08002b-010203") =:= true),
    ?assert(is_macaddr("0800.2b01.0203") =:= true).

bin_ipv6_to_address_test() ->
    Addr = <<222,173,190,175,0,0,0,0,0,0,0,0,0,0,0,1>>,
    ?assert(bin_ipv6_to_address(Addr) =:= {57005,48815,0,0,0,0,0,1}).

long2ip_test() ->
    ?assert(long2ip(4294967) =:= {0,65,137,55}).

in_range_test() ->
    ?assert(in_range("1.1.1.1", "1.1.1.0/24") =:= true),
    ?assert(in_range("10.10.1.1", "10.10.0.0/24") =:= false).

range2list_test() ->
    ?assert(range2list("1.1.1.1-1.1.1.3") =:= [{1,1,1,1}, {1,1,1,2}, {1,1,1,3}]),
    ?assert(range2list("1.1.1.1/31") =:= [{1,1,1,0}, {1,1,1,1}]).

broadcast_test() ->
    ?assert(broadcast("1.1.1.0/24") =:= {1,1,1,255}).

range_test() ->
    ?assert(range("1.1.1.1/31") =:= {{1,1,1,0}, {1,1,1,1}}).

-endif.
