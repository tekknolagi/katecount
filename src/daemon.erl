% This file must be run after starting a node/REPL with:
%    erl -sname daemon -rsh ssh
% and make sure the private keys are set up right.

% TODO: This should probably have a heartbeat and the daemon has some sort of
% cache that is invalidated whenever a node sends new stats. That way it's not
% a huge poll every time.

-module(daemon).
-compile(export_all).

tracked() ->
     [vi, vim, nvim, kate, gedit, atom, ed, emacs, subl, nano, ed, drracket,
      gvim, xemacs, libreoffice, joe, acme, eclipse, geany, pico].
    % TODO: fix
    % {ok, ContentsJSON} = file:read_file('/h/mberns01/katecount/tracked.json'),
    % Contents = jsone:decode(ContentsJSON),
    % lists:map(fun(B) -> binary_to_atom(B, utf8) end, Contents).


boot(Host) when is_atom(Host) ->
    HostName = list_to_atom(atom_to_list('node@') ++ atom_to_list(Host)),
    case lists:member(HostName, nodes())
    of true ->
           doing_nothing;
       false ->
           % TODO: This should have a monitor or something.
           % spawn(slave, start, [Host, 'node'])
           spawn(slave, start, [Host, 'node', [], self(), "erl"])
    end;
boot(Hosts) when is_list(Hosts) ->
    % net_adm:host_file()
    % Start an Erlang node on every host in ~/.erlang.hosts ... or in
    % machines().
    % TODO: This should be done async.
    lists:map(fun boot/1, Hosts),
    % Load the current module on all the remote hosts.
    % TODO: This should be done only when they are all brought up, or on a per-host
    % basis.
    c:nl(?MODULE).
boot() -> boot(machines()).

% send_it_back(Daemon) ->
%     Daemon ! node().

group_by(F, Coalesce, Default, L) ->
    lists:foldr(fun({K,V}, D) ->
                        Cur = maps:get(K, D, Default),
                        maps:put(K, Coalesce(V, Cur), D)
                end,
                maps:new(),
                [ {X, F(X)} || X <- L ]).

group_by_sum(L) ->
    Default = fun(_) -> 1 end,
    Coalesce = fun(A,B) -> A+B end,
    Start = 0,
    group_by(Default, Coalesce, Start, L).

list_running_editors() ->
    % 1. Get the processes, but only the command part.
    % 2. Get all the editors we care about.
    % 3. Get only the first column (ie the command name).
    % 4. Trim the trailing newline.
    TrackedStr = lists:join("|", lists:map(fun atom_to_list/1, tracked())),
    Cmd = to_string(
        "ps -eo command | grep -E '^(~s)' | awk '{print $1}' | sed 's/^[[:space:]]\+//'",
        [TrackedStr]
       ),
    PsResult = os:cmd(Cmd),
    % This is gross and hacky. I hate string:split.
    Editors = lists:filter(fun(E) -> E =/= [] end,
                           string:split(PsResult, "\n", all)),
    group_by_sum(lists:map(fun list_to_atom/1, Editors)).

% get_editors_on(Host) when is_atom(Host) ->
%     % Pid = spawn(Host, ?MODULE, list_running_editors, []).
%     rpc:call(Host, ?MODULE, list_running_editors, []);
get_editors_on(Hosts) when is_list(Hosts) ->
    % AllHosts = lists:map(fun get_editors_on/1, Hosts),
    {AllHosts, _BadNodes} = rpc:multicall(
                              Hosts,
                              ?MODULE,
                              list_running_editors,
                              [],
                              5000),
    lists:foldl(
      fun(E, AccIn) -> maps_merge(E, AccIn, fun(A,B) -> A+B end, 0) end,
      maps:new(),
      AllHosts).
get_stats() ->
    % io:format("Getting stats from ~w servers...~n", [length(nodes())]),
    c:nl(?MODULE),  % async
    get_editors_on([node()|nodes()]).

maps_merge(Map1, Map2, Coalesce, Default) ->
    Map1OntoMap2 = maps:fold(fun(K1, V1, AccIn) ->
                                     V2 = maps:get(K1, Map2, Default),
                                     maps:put(K1, Coalesce(V1, V2), AccIn)
                             end,
                             maps:new(),
                             Map1),
    maps:fold(fun(K2, V2, AccIn) ->
                      V1 = maps:get(K2, Map1, Default),
                      maps:put(K2, Coalesce(V1, V2), AccIn)
              end,
              Map1OntoMap2,
              Map2).

to_string(Pattern, Values) ->
    lists:flatten(io_lib:format(Pattern, Values)).

lab(Room, Computer) ->
    to_string("lab~s~s", [Room, Computer]).

vm(Idx) ->
    if Idx < 10 -> to_string("vm-hw0~w", [Idx]);
       Idx >= 10 -> to_string("vm-hw~w", [Idx])
    end.

machines() ->
    lists:map(fun list_to_atom/1,
              [ lab("116", [N]) || N <- lists:seq($a, $z) ] ++
              [ lab("118", [N]) || N <- lists:seq($a, $z) ] ++
              [ lab("120", [N]) || N <- lists:seq($a, $z) ] ++
              [ vm(N) || N <- lists:seq(0, 9) ] ++
              [ "white-hole", "black-hole", "linux", "red-giant" ]).

start_link() ->
    % spawn_link(daemon, boot, []),
    {ok, spawn_link(?MODULE, loop, [])}.

loop() ->
    receive
        {cat, Msg} -> Msg;
        _ -> ok
    end.
