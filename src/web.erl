-module(web).

-export([init/2]). %%3, handle/2, terminate/3]).

% init(Req, _Opts) ->
%     {ok, Req, undefined_state}.
% 
% handle(Req, State) ->
%     {ok, Req2} = cowboy_req:reply(200, [
%         {<<"content-type">>, <<"text/plain">>}
%     ], <<"Hello World!">>, Req),
%     {ok, Req2, State}.
% 
% terminate(_Reason, _Req, _State) ->
%     ok.

init(Req0, Opts) ->
    state_handler:log_request(),
    Count = state_handler:get_req_count(),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain">>
	}, [<<"Hello world! ">>, integer_to_binary(Count), <<"\n">>], Req0),
    {ok, Req, Opts}.
