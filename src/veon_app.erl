%%%-------------------------------------------------------------------
%% @doc veon public API
%% @end
%%%-------------------------------------------------------------------

-module(veon_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================
start(_StartType, _StartArgs) ->

  PublicDispatch = cowboy_router:compile([
    {'_', [
      {"/movie/add", veon_cowboy_handler, [movie_add]},
      {"/movie/get/:mid/:sid", veon_cowboy_handler, [movie_get]},
      {"/movie/book", veon_cowboy_handler, [movie_book]}
    ]}
  ]),

  {ok, _} = cowboy:start_http(pub, 3000, [{port, 8889}], [{env, [{dispatch, PublicDispatch}]}]),

  %mnesia:create_schema([node()]),
  %screen_mnesia:init_db(),
  veon_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
