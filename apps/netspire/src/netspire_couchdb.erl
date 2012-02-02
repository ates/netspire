-module(netspire_couchdb).

-behaviour(gen_server).

%% API
-export([start_link/0, fetch/1, fetch/2, save_doc/1, update_doc/2]).

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

save_doc(Proplist) ->
    gen_server:call(?MODULE, {save_doc, Proplist}).

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
    View = couch:make_path(DesignView),
    case couchbeam_view:fetch(State#state.db, View, Options) of
        {ok, Result} ->
            {reply, {ok, Result}, State};
        Error ->
            {reply, Error, State}
    end;

handle_call({save_doc, Proplist}, _From, State) ->
    Doc = couch:proplist_to_doc(Proplist),
    Reply =  couchbeam:save_doc(State#state.db, Doc),
    {reply, Reply, State};

handle_call({update_doc, Proplist, Doc}, _From, State) ->
    Doc1 = couch:update_doc(Proplist, Doc),
    Reply = couchbeam:save_doc(State#state.db, Doc1),
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
