%%%-------------------------------------------------------------------
%%% @author konstantin.shamko
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2017 2:56 PM
%%%-------------------------------------------------------------------
-module(veon_cowboy_handler).
-author("konstantin.shamko").


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
  Response = process_request(Type, cowboy_req:method(Req)),

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
process_request(movie_add, {<<"POST">>, Req}) ->
  {ok, Json, _Req} = cowboy_req:body(Req),

  case veon_movie:new(from_json(Json)) of
    {ok, Movie} ->
      veon_movie:movie_save(Movie),
      [{resp, ok}];
    {error, Errors} -> [{app_error, maps:to_list(Errors)}]
  end;

% get movie
process_request(movie_get, {<<"GET">>, Req}) ->
  {ImdbId, _} = cowboy_req:binding(imdbid, Req),
  {ScreenId, _} = cowboy_req:binding(screenid, Req),


  case veon_movie:movie_get(ImdbId, ScreenId) of
    [] -> [{error, movie_not_found}];
    Movie -> Movie
  end;

% book a seat
process_request(movie_book,  {<<"POST">>, Req}) ->
  {ok, JsonString, _} = cowboy_req:body(Req),
  Json = from_json(JsonString),
  ImdbId = proplists:get_value(<<"imdbId">>, Json),
  ScreenId = proplists:get_value(<<"screenId">>, Json),

  case veon_movie:movie_book(ImdbId, ScreenId) of
    {atomic, {error, Error}} -> [{app_error, Error}];
    {atomic, {ok, Message}} -> [{resp, Message}];
    _ ->
      {app_error, unknown_error}
  end;

%-------------------------
% catch all other messages
%-------------------------
process_request(_Type, _Req) ->
  [{error, not_found}].


%-------------------------
% Calculate status to http response
%-------------------------
http_status([{app_error, _}]) ->
  400;
http_status([{error, _Reason}]) ->
  404;
http_status(_) ->
  200.

%-------------------------
% Transform response to json format
%-------------------------
to_json(Data) ->
  mochijson2:encode(Data).

%-------------------------
% Transform JSON to proplist
%-------------------------
from_json(Json) ->
  {struct, JsonData} = mochijson2:decode(Json),
  JsonData.
