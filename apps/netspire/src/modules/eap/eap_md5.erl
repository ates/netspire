-module(eap_md5).

%% API
-export([challenge/2, authenticate/4]).

-include("eap.hrl").

%% MD5 Packet Format in EAP Type-Data
%% --- ------ ------ -- --- ---------
%%  0                   1                   2                   3
%%  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |  Value-Size   |  Value ...
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |  Name ...
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-define(CHALLENGE_LEN, 16). % 16 octets

%% <<Code:8, Ident:8, Length:16, Type:8, Data:136>> = 176 bits = 22 octets
%% Data = <<Length:8, Challenge:128>> = 17 octets
-define(PACKET_LENGTH, 22).

%% @doc Generates challenge value
challenge(UserName, Ident) ->
    Challenge = libeap:challenge(),
    MD5Packet = <<?CHALLENGE_LEN:8, Challenge/binary>>,
    Packet = <<?EAP_REQUEST:8, (Ident + 1):8, ?PACKET_LENGTH:16, ?EAP_MD5_CHALLENGE:8, MD5Packet/binary>>,
    eap_reg:push(UserName, Challenge),
    {ok, libeap:compose(Packet)}.

authenticate(UserName, Ident, Password, Packet) ->
    case eap_reg:pull(UserName) of
        undefined ->
            false;
        Challenge ->
            Hash = crypto:md5([Ident, Password, Challenge]),
            <<_Size:8, ReqHash:16/binary-unit:8, _Rest/binary>> = Packet,
            Hash =:= ReqHash
    end.
