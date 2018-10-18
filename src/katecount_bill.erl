% It's not a glamorous job, but someone has to do it. -- thebb01
-module(katecount_bill).
-behaviour(gen_server).
-export([init/1, handle_info/2, start_link/0, read_history/0]).

-define(SECONDS, 1000).
-define(MINUTES, ?SECONDS*60).
-define(HOURS, ?MINUTES*60).

start_link() ->
    gen_server:start_link(?MODULE, ?MODULE, []).

init(Args) ->
  io:format("Bill is alive!~n"),
  % TODO: determine if this causes an error because of the timer state. I can't
  % tell how to send the process BOTH timers so that there's no weird clause
  % error for pattern matching
  CheckTimer = erlang:send_after(10*?SECONDS, self(), check),
  BootTimer = erlang:send_after(1, self(), boot),
  {ok, {CheckTimer, BootTimer}}.

maps_map_kv(F, M) ->
    maps:fold(fun(K,V,AccIn) ->
                      {NK, NV} = F(K, V),
                      maps:put(NK, NV, AccIn)
              end,
              #{}, M).

atomize(B) when is_binary(B) ->
    binary_to_atom(B, utf8);
atomize(L) when is_list(L) ->
    lists:map(fun(E) -> atomize(E) end, L);
atomize(M) when is_map(M) ->
    maps_map_kv(fun(K,V) -> {atomize(K), atomize(V)} end, M);
atomize(I) -> I.

unixtime() ->
    {Mega, S, _} = os:timestamp(),
    Mega*1000 + S.

read_history() ->
    Filename = '/h/mberns01/public_html/history.json',
    {ok, ContentsJSON} = file:read_file(Filename),
    % [timestamp, data(hash)]
    lists:map(fun atomize/1, jsone:decode(ContentsJSON)).

write_history(PrevHist, Summary) ->
    Filename = '/h/mberns01/public_html/history.json',
    Hist = PrevHist ++ [maps:put(timestamp, unixtime(), Summary)],
    {ok, T} = file:open(Filename, [write]),
    ok = file:write(T, jsone:encode(Hist)),
    ok = file:close(T).

handle_info(check, {OldCheckTimer, OldBootTimer}) ->
  erlang:cancel_timer(OldCheckTimer),
  Stats = daemon:get_stats(),
  Mod = case rand:uniform() < 0.05 of
            true ->
                maps:put('magnetized needle and a steady hand', 1, Stats);
            false -> 
                Stats
        end,
  Editors = #{
    summary => Mod,
    available => daemon:tracked(),
    num_nodes => length(nodes())
   },

  History = read_history(),
  write_history(History, Editors),
  Editors1 = maps:put(history, History, Editors),

  EditorsJson = jsone:encode(Editors1),
  % io:format("Dumping json... ~w~n", [EditorsJson]),
  {ok, Target} = file:open('/h/mberns01/public_html/editors.json',
                           [write, {encoding,utf8}]),
  case file:write(Target, EditorsJson) of
      ok -> ok; %io:format("write was ok~n");
      {error, R} -> io:format("write failed: ~p~n", [R])
  end,
  ok = file:close(Target),
  CheckTimer = erlang:send_after(10*?SECONDS, self(), check),
  {noreply, {CheckTimer, OldBootTimer}};

handle_info(boot, {OldCheckTimer, OldBootTimer}) ->
  io:format("Launching again...~n"),
  erlang:cancel_timer(OldBootTimer),
  daemon:boot(),
  BootTimer = erlang:send_after(1*?HOURS, self(), boot),
  {noreply, {OldCheckTimer, BootTimer}}.
