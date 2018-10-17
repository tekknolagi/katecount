-module(state_handler).
-behavior(gen_server).

% API functions
-export([log_request/0,
         get_req_count/0,
         start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {count = 0}).

log_request() ->
    gen_server:cast(?MODULE, log_request).

get_req_count() ->
    gen_server:call(?MODULE, req_count).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).




init([]) ->
    {ok, #state{}}.

handle_call(req_count, _From, State) ->
    {reply, State#state.count, State};
handle_call(_Req, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(log_request, State) ->
    Count = State#state.count,
    {noreply, State#state{count = Count + 1}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
