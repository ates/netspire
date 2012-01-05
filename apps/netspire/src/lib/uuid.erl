-module(uuid).

%% API
-export([v4/0, to_string/1]).

%% @doc Generate the UUID and returns as binary.
-spec v4() -> binary().
v4() ->
    random:seed(now()),
    % round(math:pow(2, N)) where N = 48, 12, 32, 30
    R1 = random:uniform(281474976710656) - 1,
    R2 = random:uniform(4096) - 1,
    R3 = random:uniform(4294967296) - 1,
    R4 = random:uniform(1073741824) - 1,
    <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>.

%% @doc Convert binary UUID to string.
-spec to_string(binary()) -> string().
to_string(U) ->
    Parts = get_parts(U),
    lists:flatten(io_lib:format(
            "~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", Parts)).

-spec get_parts(binary()) -> list().
get_parts(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
    [TL, TM, THV, CSR, CSL, N].

%% Tests
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(V4, <<253,59,232,214,32,200,65,86,158,18,205,136,200,148,87,255>>).

to_string_test() ->
    Result = "fd3be8d6-20c8-4156-9e12-cd88c89457ff",
    ?assert(Result =:= to_string(?V4)).

get_parts_test() ->
    Result = [4248561878,8392,16726,158,18,225987364411391],
    ?assert(Result =:= get_parts(?V4)).

-endif.
