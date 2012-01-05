-module(netspire_config).

-behaviour(gen_server).

%% API
-export([start_link/0, get_option/1, reload/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

-include("netspire.hrl").

-define(NETSPIRE_CONFIG, "./netspire.conf").

-record(state, {
    config = undefined :: undefined | string(),
    dict = undefined :: undefined | dict()
}).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Looking for the value for the specific option.
%% Returns the option value or 'undefined' atom in case if the option
%% does not exists
%% @end
-spec get_option(term()) -> any() | undefined.
get_option(Option) ->
    gen_server:call(?MODULE, {get_option, Option}).

%% @doc Reload configuration file.
-spec reload() -> ok | {error, term()}.
reload() ->
    gen_server:call(?MODULE, reload).

%% @doc Loading configuration file.
%% The configuration file should be specified using
%% NETSPIRE_CONFIG shell variable or by passing 'config' option
%% to the erl emulator
%% If both variables were not defined then function
%% will attempt to open netspire.conf file in the current directory
%% @end
init([]) ->
    Config = case os:getenv("NETSPIRE_CONFIG") of
        false ->
            case application:get_env(config) of
                undefined ->
                    ?NETSPIRE_CONFIG;
                {ok, File} ->
                    File
            end;
        File -> File
    end,
    ?INFO_MSG("Loading configuration file ~s~n", [Config]),
    case load_file(Config) of
        {ok, Dict} ->
            {ok, #state{config = Config, dict = Dict}};
        {error, Reason} ->
            {error, Reason}
    end.

handle_call({get_option, Option}, _From, State) ->
    Reply = case dict:find(Option, State#state.dict) of
        {ok, Value} ->
            Value;
        _ -> undefined
    end,
    {reply, Reply, State};

handle_call(reload, _From, State) ->
    case load_file(State#state.config) of
        {ok, Dict} ->
            ?INFO_MSG("Configuration file was successful reloaded~n", []),
            {reply, ok, State#state{dict = Dict}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

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
-spec load_file(string()) -> {ok, dict()} | {error, term()}.
load_file(File) ->
    case file:consult(File) of
        {ok, Terms} ->
            {ok, dict:from_list(Terms)};
        {error, Reason} ->
            Msg = file:format_error(Reason),
            ?ERROR_MSG("Can't load configuration file ~s: ~s~n", [File, Msg]),
            {error, Reason}
    end.
