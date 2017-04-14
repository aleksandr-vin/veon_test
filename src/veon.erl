%%%-------------------------------------------------------------------
%%% @author konstantin.shamko
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2017 4:18 PM
%%%-------------------------------------------------------------------
-module(veon).
-author("konstantin.shamko").

%% API
-export([
  start/0,
  stop/0
]).

-define(APPS, [inets, crypto, cowlib, ranch, cowboy, mnesia, veon]).

%% ===================================================================
%% API functions
%% ===================================================================
start() ->
  ok = ensure_started(?APPS).

stop() ->
  ok = stop_apps(lists:reverse(?APPS)).

%% ===================================================================
%% Internal functions
%% ===================================================================
ensure_started([]) -> ok;
ensure_started([App | Apps]) ->
  case application:start(App) of
    ok -> ensure_started(Apps);
    {error, {already_started, App}} -> ensure_started(Apps)
  end.

stop_apps([]) -> ok;
stop_apps([App | Apps]) ->
  application:stop(App),
  stop_apps(Apps).
