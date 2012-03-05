%% EAP PACKET FORMAT
%% --- ------ ------
%% 0                   1                   2                   3
%% 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |     Code      |  Identifier   |            Length             |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |    Data ...
%% +-+-+-+-+
%%
%% EAP Request and Response Packet Format
%% --- ------- --- -------- ------ ------
%%  0                   1                   2                   3
%%  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |     Code      |  Identifier   |            Length             |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |     Type      |  Type-Data ...
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
-define(EAP_PACKET, Code:8, Ident:8, Length:16, Type:8, Data/binary).
%% The size of EAP packet without data
-define(EAP_PACKET_NODATA_SIZE, 5).

%% Length of EAP-Message attribute value in octets
-define(EAP_MESSAGE_LEN, 253).

%% EAP Message codes
-define(EAP_REQUEST, 1).
-define(EAP_RESPONSE, 2).
-define(EAP_SUCCESS, 3).
-define(EAP_FAILURE, 4).

%% EAP Type codes
-define(EAP_IDENTIFY, 1).
-define(EAP_NOTIFICATION, 2).
-define(EAP_NAK, 3).
-define(EAP_MD5_CHALLENGE, 4).

%% EAP Authentication codes
-define(EAP_TLS, 13).
-define(EAP_LEAP, 17).
-define(EAP_TTLS, 21).
-define(EAP_PEAP, 25).
-define(EAP_MSCHAPV2, 26).

%% EAP Success and Failure packet format
%%
%% 0                   1                   2                   3
%% 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |     Code      |  Identifier   |            Length             |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%
%% Length is 4 octets
-define(EAP_SUCCESS(I), [{"EAP-Message", <<?EAP_SUCCESS:8, I:8, 4:16>>}]).
-define(EAP_FAILURE(I), [{"EAP-Message", <<?EAP_FAILURE:8, I:8, 4:16>>}]).
