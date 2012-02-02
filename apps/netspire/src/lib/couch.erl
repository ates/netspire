-module(couch).

-export([proplist_to_doc/1, update_doc/2, get_value/2, make_path/1]).

%% @doc Prepares document for CouchDB from PropList.
proplist_to_doc(PropList) ->
    proplist_to_doc(PropList, []).

proplist_to_doc([], Acc) ->
    {lists:reverse(Acc)};
proplist_to_doc([{Name, Value}|Tail], Acc) ->
    Entry = {encode_name(Name), encode_value(Value)},
    proplist_to_doc(Tail, [Entry | Acc]).

%% @doc Replaces values in document by new ones.
%% The values will be added in document if they are not already present
%% @end
update_doc([], Doc) ->
    Doc;
update_doc([{Name, Value}|Tail], Doc) ->
    Doc1 = couchbeam_doc:set_value(encode_name(Name), encode_value(Value), Doc),
    update_doc(Tail, Doc1).

%% @doc Returns the value of the key.
get_value(Key, Doc) ->
    case couchbeam_doc:get_value(encode_name(Key), Doc) of
        undefined -> undefined;
        Value when is_binary(Value) ->
            binary_to_list(Value);
        Value -> Value
    end.

%% @doc Compose path to the view.
make_path(DesignView) when is_list(DesignView) ->
    [D, V] = string:tokens(DesignView, "/"), {D, V};
make_path(DesignView) ->
    DesignView.

%% Internal functions
encode_name(Name) when is_atom(Name) ->
    erlang:atom_to_binary(Name, latin1);
encode_name(Name) ->
    Name.

encode_value(Value) when is_list(Value) ->
    list_to_binary(Value);
encode_value(Value) when is_atom(Value), Value == true orelse Value == false ->
    Value;
encode_value(Value) when tuple_size(Value) == 4 orelse tuple_size(Value) == 8 ->
    list_to_binary(inet_parse:ntoa(Value));
encode_value(Value) ->
    Value.

%% Tests
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

proplist_to_doc_test() ->
    PropList = [
        {name, "joel"},
        {amount, 10.10},
        {active, true}
    ],
    Result = {[
        {<<"name">>, <<"joel">>},
        {<<"amount">>, 10.10},
        {<<"active">>, true}
    ]},
    ?assert(proplist_to_doc(PropList) =:= Result).

encode_name_test() ->
    ?assert(encode_name(amount) =:= <<"amount">>).

encode_value_test() ->
    ?assert(encode_value("amount") =:= <<"amount">>),
    ?assert(encode_value({1,1,1,1}) =:= <<"1.1.1.1">>),
    ?assert(encode_value({0,0,0,0,0,0,0,1}) =:= <<"::1">>),
    ?assert(encode_value(true) =:= true).

update_doc_test() ->
    Doc = proplist_to_doc([{name, "joel"}, {amount, 100}]),
    Doc1 = proplist_to_doc([{name, "joel"}, {amount, 50}, {active, true}]),
    Values = [
        {amount, 50},
        {active, true}
    ],
    ?assert(update_doc(Values, Doc) =:= Doc1).

get_value_test() ->
    PropList = [
        {name, "joel"},
        {amount, 10.10},
        {active, true}
    ],
    Doc = proplist_to_doc(PropList),
    ?assert(get_value(name, Doc) =:= "joel"),
    ?assert(get_value(amount, Doc) =:= 10.10),
    ?assert(get_value(active, Doc) =:= true).

make_path_test() ->
    ?assert(make_path(all_docs) =:= all_docs),
    ?assert(make_path("Transaction/all") =:= {"Transaction", "all"}).

-endif.
