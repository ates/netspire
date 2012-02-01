-module(netspire_couchdb).

-behaviour(gen_server).

%% API
-export([start_link/0, fetch/1, fetch/2, make_doc/1, update_doc/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

-include("netspire.hrl").

-record(state, {db = undefined}).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

fetch(DesignView) ->
    fetch(DesignView, []).

fetch(DesignView, Options) ->
    gen_server:call(?MODULE, {fetch, DesignView, Options}).

make_doc(Proplist) ->
    gen_server:call(?MODULE, {make_doc, Proplist}).

update_doc(Proplist, Doc) ->
    gen_server:call(?MODULE, {update_doc, Proplist, Doc}).

init([]) ->
    case netspire_config:get_option(couchdb_backend) of
        undefined ->
            ?ERROR_MSG("CouchDB backend options are not specified~n", []),
            {error, no_couchdb_options};
        Options when is_list(Options) ->
            Host = proplists:get_value(host, Options, "localhost"),
            Port = proplists:get_value(port, Options, 5984),
            DBName = proplists:get_value(database, Options, "netspire"),
            S = couchbeam:server_connection(Host, Port, "", []),
            {ok, Database} = couchbeam:open_db(S, DBName, []),
            ?INFO_MSG("Connection to CouchDB backend defined: "
                "~s:~p/~s~n", [Host, Port, DBName]),
            {ok, #state{db = Database}}
    end.

handle_call({fetch, DesignView, Options}, _From, State) ->
    case couchbeam_view:fetch(State#state.db, make_path(DesignView), Options) of
        {ok, Result} ->
            {reply, {ok, Result}, State};
        Error ->
            {reply, Error, State}
    end;

handle_call({make_doc, Proplist}, _From, State) ->
    Doc = proplist_to_doc(Proplist),
    Reply =  couchbeam:save_doc(State#state.db, Doc),
    {reply, Reply, State};

handle_call({update_doc, Proplist, Doc}, _From, State) ->
    Reply = update_existent_doc(Proplist, Doc),
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%
%% Internal functions
%%
make_path(all_docs) -> all_docs;
make_path(Path) when is_list(Path) ->
    [Design, View] = string:tokens(Path, "/"),
    {Design, View}.

proplist_to_doc(P) ->
    proplist_to_doc(P, []).

proplist_to_doc([], Acc) ->
    {lists:reverse(Acc)};
proplist_to_doc([{Name, Value}|T], Acc) ->
    Entry = {encode_name(Name), encode_value(Value)},
    proplist_to_doc(T, [Entry | Acc]).

encode_name(Name) when is_atom(Name) ->
    erlang:atom_to_binary(Name, latin1);
encode_name(Name) ->
    Name.

encode_value(Value) when is_list(Value) ->
    list_to_binary(Value);
encode_value(Value) when is_atom(Value), Value == true orelse Value == false ->
    Value;
encode_value(Value) ->
    Value.

update_existent_doc([], Doc) ->
    Doc;
update_existent_doc([{Name, Value}|T], Doc) ->
    Doc1 = couchbeam_doc:set_value(encode_name(Name), encode_value(Value), Doc),
    update_existent_doc(T, Doc1).
