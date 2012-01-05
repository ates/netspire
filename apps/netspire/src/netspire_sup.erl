-module(netspire_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-define(SPEC(M), {M, {M, start_link, []},
        permanent, infinity, supervisor, [M]}).
-define(CHILDRENS, [netspire_misc_sup]).
-define(TABLES, [
    {netspire_modules, [{keypos, 2}]},
    {netspire_netflow, [{keypos, 2}]}
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    create_tables(?TABLES),
    {ok, {{one_for_one, 5, 10}, [?SPEC(M) || M <- ?CHILDRENS]}}.

%% @doc Creates permanent tables.
create_tables(Tables) when is_list(Tables) ->
    lists:foreach(fun create_tables/1, Tables);
create_tables({Table, Opts}) ->
    ets:new(Table, [named_table, public | Opts]).
