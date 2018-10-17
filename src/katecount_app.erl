%%%-------------------------------------------------------------------
%% @doc katecount public API
%% @end
%%%-------------------------------------------------------------------

-module(katecount_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    io:format("start app called~n"),
    % TODO: why does this go in app?
    Dispatch = cowboy_router:compile([
        {'_', [
               {"/", web, []},
               {"/editors.json", editor_json, []}
              ]
        }
    ]),
    % cowboy:start_clear(my_http_listener, 100,
    %     [{port, 9000}],
    %     [{env, [{dispatch, Dispatch}]}]
    % ),
    % spawn(daemon, boot, []),
    {ok, _} = cowboy:start_clear(http,
                                 [{port, 8080}], #{
                                     env => #{dispatch => Dispatch}
                                    }),
    katecount_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
