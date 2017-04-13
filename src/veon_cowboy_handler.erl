%%%-------------------------------------------------------------------
%%% @author konstantin.shamko
%%% @copyright (C) 2017, Oxagile LLC
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2017 2:56 PM
%%%-------------------------------------------------------------------
-module(veon_cowboy_handler).
-author("konstantin.shamko").


% -include("../include/screen_response_types.hrl").

-export([init/3, rest_init/2]).
-export([
  content_types_provided/2,
  content_types_accepted/2,
  allowed_methods/2]).

-export([respond/2]).

%-------------------------
% tell cowboy to use rest handler
%-------------------------
init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

%-------------------------
% init rest handler
%-------------------------
rest_init(Req, Params) ->
  {ok, Req, Params}.

%-------------------------
% we accept POST & GET requests only
%-------------------------
allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

%-------------------------
% we use json as data format
%-------------------------
content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, respond}], Req, State}.

%-------------------------
% we use json as data format for POST requests also
%-------------------------
content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, respond}], Req, State}.

%-------------------------
% send server response
%-------------------------
respond(Req, [Type]) ->
  Response = process_request(Type, Req),

  ResponseHeaders = [
    {<<"content-type">>, <<"application/json">>},
    {<<"access-control-allow-origin">>, <<"*">>}
  ],

  cowboy_req:reply(http_status(Response), ResponseHeaders, to_json(Response), Req),
  {halt, Req, [Type]}.


%%%===============================================
%%% Internal Functions
%%%===============================================

% add movie
process_request(movie_add, Req) ->

  [{resp, ok}];
% get movie
process_request(movie_get, _Req) ->
  [{resp, ok}];

%
process_request(movie_book, Req) ->
  %{ok, EventJson, _Req} = cowboy_req:body(Req),
  %Nodes = nodes(),
  %[{Node, rpc:cast(Node, screen_game_event_starter, do_event, [EventJson])} || Node <-Nodes],
  %screen_game_event_starter:do_event(EventJson),
  [{status, ok}];
%-------------------------
% catch all other messages
%-------------------------
process_request(_Type, _Req) ->
  [{error, not_found}].


%-------------------------
% Calculate status to http response
%-------------------------
http_status([{error, _Reason}]) ->
  404;
http_status(_) ->
  200.

%-------------------------
% Transform response to json format
%-------------------------
to_json(Data) ->
  mochijson2:encode(Data).