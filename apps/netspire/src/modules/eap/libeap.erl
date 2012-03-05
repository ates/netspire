-module(libeap).

-export([challenge/0, attribute_value/1, compose/1, type2string/1]).

-include("eap.hrl").
-include("radius.hrl").

%% @doc Generates challenge value
challenge() ->
    crypto:md5(crypto:rand_bytes(128)).

%% @doc Concatenates all EAP-Messages attributes
%% to get the complete EAP packet
%% @end
attribute_value(Request) ->
    Attrs = Request#radius_packet.attrs,
    case proplists:get_all_values("EAP-Message", Attrs) of
        [] ->
            undefined;
        List ->
            list_to_binary(List)
    end.

%% @doc Compose EAP packet
compose(Value) when byte_size(Value) =< ?EAP_MESSAGE_LEN ->
    [{"EAP-Message", Value}];
compose(Value) ->
    compose(Value, []).

compose(Value, Acc) when byte_size(Value) =< ?EAP_MESSAGE_LEN ->
    lists:reverse([{"EAP-Message", Value} | Acc]);
compose(<<Value:?EAP_MESSAGE_LEN/binary-unit:8, Rest/binary>>, Acc) ->
    compose(Rest, [{"EAP-Message", Value} | Acc]).

%% @doc Converts EAP authentication type to string
type2string(?EAP_TLS) -> "EAP-TLS";
type2string(?EAP_TTLS) -> "EAP-TTLS";
type2string(?EAP_LEAP) -> "EAP-LEAP";
type2string(?EAP_PEAP) -> "EAP-PEAP";
type2string(?EAP_MSCHAPV2) -> "EAP-MSCHAPv2";
type2string(?EAP_MD5_CHALLENGE) -> "EAP-MD5";
type2string(Type) -> integer_to_list(Type).
