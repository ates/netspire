%%%
%%% draft-kamath-pppext-eap-mschapv2-00.txt
%%% 
-module(eap_mschap_v2).

%% API
-export([challenge/2, authenticate/6]).

-include("eap.hrl").
-include("radius.hrl").

-import(mod_mschap_v2, [
        mschap_v2_challenge_hash/3,
        mschap_v2_challenge_response/2,
        mschap_v2_auth_response/3
]).

-define(OPCODE_CHALLENGE, 1).
-define(OPCODE_RESPONSE, 2).
-define(OPCODE_SUCCESS, 3).
-define(OPCODE_FAILURE, 4).
-define(EAP_MSCHAPV2_RESPONSE, 26).

-define(CHALLENGE_LEN, 16). % 16 octets
-define(CHALLENGE_PACKET_SIZE, 26). % size of packet without NAME length
%% The Message field contains a 42-octet authenticator response string
-define(MESSAGE_LEN, 42).

-record(emschap, {username, challenge, phase, ntresponse, pwhash}).

%% Challenge packet
%%
%%  0                   1                   2                   3
%%  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |     Code      |   Identifier  |            Length             |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |     Type      |   OpCode      |  MS-CHAPv2-ID |  MS-Length...
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |   MS-Length   |  Value-Size   |  Challenge...
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |                             Challenge...
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |                             Name...
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
challenge(UserName, Ident) ->
    Challenge = libeap:challenge(),
    Name = list_to_binary(UserName),
    Length = ?CHALLENGE_PACKET_SIZE + byte_size(Name),
    Packet = <<?EAP_REQUEST:8,
               (Ident + 1):8,
               Length:16,
               ?EAP_MSCHAPV2_RESPONSE:8,
               ?OPCODE_CHALLENGE:8,
               (Ident + 1):8,
               (Length - ?EAP_PACKET_NODATA_SIZE):16,
               16:8, % the length of the Challenge field
               Challenge/binary,
               Name/binary>>,
    E = #emschap{username = UserName, challenge = Challenge, phase = 1},
    eap_reg:push(UserName, E),
    {ok, libeap:compose(Packet)}.

%% Success Request packet
%% 0                   1                   2                   3
%% 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |     Code      |   Identifier  |            Length             |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |     Type      |   OpCode      |  MS-CHAPv2-ID |  MS-Length...
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |   MS-Length   |                    Message...
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
authenticate(UserName, _Password, _Ident, <<?OPCODE_SUCCESS>>, Request, Client) ->
    case eap_reg:pull(UserName, true) of
        undefined ->
            false;
        E when is_record(E, emschap) andalso E#emschap.phase == 2 ->
            Auth = Request#radius_packet.auth,
            Secret = Client#nas_spec.secret,
            MPPE = mschap_v2_mppe:mppe_attrs(E#emschap.ntresponse, E#emschap.pwhash, Auth, Secret),
            Policy = case gen_module:get_option(mod_eap, mschapv2_require_encryption) of
                yes ->
                    % Encryption required
                    [{"MS-MPPE-Encryption-Policy", <<2:32>>}];
                _ ->
                    % Encryption allowed
                    [{"MS-MPPE-Encryption-Policy", <<1:32>>}]
            end,
            Types = case gen_module:get_option(mod_eap, mschapv2_require_strong) of
                yes ->
                    % 128 bit keys
                    [{"MS-MPPE-Encryption-Types", <<4:32>>}];
                _ ->
                    % 40- or 128-bit keys may be used
                    [{"MS-MPPE-Encryption-Types", <<6:32>>}]
            end,
            {accept, MPPE ++ Policy ++ Types}
    end;
authenticate(UserName, Password, Ident, Packet, _Request, _Client) ->
    <<_OpCode:8, MSIdent:8, _MSLen:16, _ValueSize:8, Response:49/binary-unit:8, _Name:4/binary-unit:8>> = Packet,
    <<PeerChallenge:16/binary-unit:8, _Zero:8/binary-unit:8, NTResponse:24/binary-unit:8, _Flags:1/binary-unit:8>> = Response,
    case eap_reg:pull(UserName) of
        undefined ->
            reject;
        E when is_record(E, emschap) andalso E#emschap.phase == 1 ->
            PasswordHash = pwhash(Password),
            PrevChallenge = E#emschap.challenge,
            Challenge = mschap_v2_challenge_hash(PeerChallenge, PrevChallenge, UserName),
            ChallengeResponse = mschap_v2_challenge_response(Challenge, PasswordHash),
            Name = list_to_binary(UserName),
            Length = ?MESSAGE_LEN + ?EAP_PACKET_NODATA_SIZE + byte_size(Name),
            case ChallengeResponse == NTResponse of
                true ->
                    Message = mschap_v2_auth_response(PasswordHash, NTResponse, Challenge),
                    Pkt = <<?EAP_REQUEST:8,
                            (Ident + 1):8,
                            Length:16,
                            ?EAP_MSCHAPV2_RESPONSE:8,
                            ?OPCODE_SUCCESS:8,
                            (MSIdent + 1):8,
                            (Length - ?EAP_PACKET_NODATA_SIZE):16,
                            (list_to_binary(Message))/binary>>,
                    eap_reg:push(UserName, E#emschap{phase = 2, ntresponse = NTResponse, pwhash = PasswordHash}),
                    {challenge, libeap:compose(Pkt)};
                _ -> reject
            end
    end.

pwhash(Password) ->
    crypto:md4(util:latin1_to_unicode(Password)).
