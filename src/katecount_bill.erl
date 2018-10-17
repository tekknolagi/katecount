% It's not a glamorous job, but someone has to do it. -- thebb01
-module(katecount_bill).
-behaviour(gen_server).
-export([init/1, handle_info/2, start_link/0]).

start_link() ->
    gen_server:start_link(?MODULE, ?MODULE, []).

init(Args) ->
  io:format("bill:init got ~p~n", [Args]),
  Timer = erlang:send_after(1, self(), check),
  {ok, Timer}.

handle_info(check, OldTimer) ->
  erlang:cancel_timer(OldTimer),
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
  EditorsJson = jsone:encode(Editors),
  % io:format("Dumping json... ~w~n", [EditorsJson]),
  {ok, Target} = file:open('/h/mberns01/public_html/editors.json',
                           [write, {encoding,utf8}]),
  case file:write(Target, EditorsJson) of
      ok -> ok; %io:format("write was ok~n");
      {error, R} -> io:format("write failed: ~p~n", [R])
  end,
  ok = file:close(Target),
  Timer = erlang:send_after(10000, self(), check),
  {noreply, Timer}.
