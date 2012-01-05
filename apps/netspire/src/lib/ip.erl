-module(ip).

%% API
-export([ip2long/1, address/1, is_macaddr/1]).

-include("netspire.hrl").

%% Supported formats of MACADDR
%% 08002b:010203, 08002b-010203, 0800.2b01.0203
%% 08-00-2b-01-02-03, 08:00:2b:01:02:03
-define(MAC_FMT1, "^([0-9a-f]{2}([:-]|$)){6}").
-define(MAC_FMT2, "^([0-9a-f]{6}([:-]|$)){2}").
-define(MAC_FMT3, "^([0-9a-f]{4}(\.|$)){3}$").
-define(MAC_REGEXP, string:join([?MAC_FMT1, ?MAC_FMT2, ?MAC_FMT3], "|")).

%% @doc Convert IP address to long integer.
-spec ip2long(ip_address()) -> non_neg_integer() | {error, einval}.
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

-endif.
