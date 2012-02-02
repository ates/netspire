-module(iptraffic_sup).

-behaviour(supervisor).

%% API
-export([start/1, start_link/1, stop/0, init_session/1, resume_all/0,
         delete_session/2]).

%% supervisor callbacks
-export([init/1]).

-include("netspire.hrl").
-include("iptraffic.hrl").

start(Options) ->
    init_mnesia(),
    ChildSpec = {?MODULE,
                 {?MODULE, start_link, [Options]},
                 transient,
                 infinity,
                 supervisor,
                 [iptraffic_sup]
                },
    supervisor:start_child(netspire_sup, ChildSpec),
    {ok, Count} = iptraffic_sup:resume_all(),
    ?INFO_MSG("~p previously started session(s) has been resumed~n", [Count]).

stop() ->
    supervisor:terminate_child(netspire_sup, iptraffic_sup),
    supervisor:delete_child(netspire_sup, iptraffic_sup).

init_mnesia() ->
    mnesia:create_table(ipt_session, [{disc_copies, [node()]}, {index, [uuid]},
        {attributes, record_info(fields, ipt_session)}]),
    mnesia:add_table_copy(ipt_session, node(), disc_copies).

start_link(Options) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Options]).

init([Options]) ->
    ChildSpec = {mod_iptraffic,
                 {mod_iptraffic, start_link, [Options]},
                 transient,
                 30000,
                 worker,
                 [mod_iptraffic]
                },
    {ok, {{one_for_one, 100, 1}, [ChildSpec]}}.

init_session(UserName) ->
    init_session(UserName, uuid:to_string(uuid:v4())).

init_session(UserName, UUID) ->
    ChildSpec = {{session, UserName},
                 {iptraffic_session, start_link, [UUID]},
                 transient,
                 3000,
                 worker,
                 [iptraffic_session]
                },
    supervisor:start_child(?MODULE, ChildSpec).

resume_all() ->
    Key = mnesia:dirty_first(ipt_session),
    resume_session(Key, 0).

resume_session('$end_of_table', Count) ->
    {ok, Count};
resume_session(Key, Count) ->
    [State] = mnesia:dirty_read(ipt_session, Key),
    case is_process_alive(State#ipt_session.pid) of
        false ->
            #ipt_session{username = UserName, uuid = UUID} = State,
            init_session(UserName, UUID),
            Next = mnesia:dirty_next(ipt_session, Key),
            resume_session(Next, Count + 1);
        true ->
            Next = mnesia:dirty_next(ipt_session, Key),
            resume_session(Next, Count)
    end.

delete_session(Session, Request) ->
    netspire_hooks:run(ippool_release_ip, [Request]),
    iptraffic_session:stop(Session#ipt_session.pid),
    supervisor:delete_child(iptraffic_sup, {session, Session#ipt_session.username}).
