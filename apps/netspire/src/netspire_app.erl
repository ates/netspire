-module(netspire_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

-include("netspire.hrl").

-type application_start_type() :: normal
    | {takeover, node()} | {failover, node()}.

-spec start(application_start_type(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    consider_profiling(),
    netspire_logger:start(),
    case netspire_sup:start_link() of
        {ok, _Pid} = Sup ->
            add_code_path(),
            init_mnesia(),
            start_services(),
            start_modules(),
            Sup;
        Error ->
            Error
    end.

-spec stop(term()) -> ok.
stop(_State) ->
    ok.

%%
%% Internal functions
%%

%% @doc Starts profiling of the application.
-spec consider_profiling() -> profiling | not_profiling.
consider_profiling() ->
    case application:get_env(profile) of
        {ok, true} ->
            eprof:start(),
            eprof:start_profiling([self()]);
        _ ->
            not_profiling
    end.

%% @doc Adds the specified directories to the end of the code path.
%% Looking for the option <em>code_path</em> with list of the directories 
%% in the configuration file and add they to the end of code path.
%% @end
-spec add_code_path() -> ok.
add_code_path() ->
    case netspire_config:get_option(code_path) of
        undefined -> ok;
        Directories ->
            code:add_pathsz(Directories)
    end.

init_mnesia() ->
    ?INFO_MSG("Checking availability of cluster environment~n", []),
    Nodes = case net_adm:host_file() of
        {error, _} -> [];
        _ ->
            [N || N <- net_adm:world(), N =/= node()]
    end,
    case Nodes of
        [] ->
            ?INFO_MSG("No additional nodes were found~n", []),
            mnesia:create_schema([node()]),
            mnesia:start();
        _ ->
            ?INFO_MSG("Connected nodes: ~p~n", [Nodes]),
            mnesia:change_config(extra_db_nodes, Nodes),
            mnesia:start(),
            ok = waiting_for_tables()
    end.

waiting_for_tables() ->
    Tables = [Tab || Tab <- mnesia:system_info(tables), mnesia:table_info(Tab, local_content) =:= false],
    case mnesia:wait_for_tables(Tables, 30000) of
        ok -> ok;
        {timeout, Tabs} ->
            throw({error, {timeout_waiting_for_tables, Tabs}});
        {error, Reason} ->
            throw({error, {failed_waiting_for_tables, Reason}})
    end.

%% @doc Starts services defined in configuration file.
-spec start_services() -> ok.
start_services() ->
    F = fun({Module, Options}) ->
            Module:start(Options)
    end,
    case netspire_config:get_option(services) of
        undefined -> ok;
        Services ->
            lists:foreach(F, Services)
    end.

%% @doc Start dynamic modules
-spec start_modules() -> ok.
start_modules() ->
    F = fun({Module, Options}) ->
            gen_module:start_module(Module, Options)
    end,
    case netspire_config:get_option(modules) of
        undefined -> ok;
        Modules ->
            lists:foreach(F, Modules)
    end.
