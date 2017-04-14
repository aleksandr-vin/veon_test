%%%-------------------------------------------------------------------
%%% @author konstantin.shamko
%%% @copyright (C) 2017, Oxagile LLC
%%% @doc
%%%
%%% @end
%%% Created : 14. Apr 2017 10:26 AM
%%%-------------------------------------------------------------------
-module(veon_movie).
-author("konstantin.shamko").

%% API
-export([new/1, movie_save/1, movie_book/2, movie_get/2]).

-record(movie, {imdbId, screenId, movieTitle, seatsAvailable, seatsReserved }).

%===========================================
%
% Public functions
%
%===========================================

%---------------------------
% Create new movie entity
%---------------------------
new(JsonData) ->
  Errors = maps:new(),
  Movie = maps:new(),

  {_, Movie1, Errors1} = init_field(imdbId, proplists:get_value(<<"imdbId">>, JsonData), Movie, Errors),
  {_, Movie2, Errors2} = init_field(screenId, proplists:get_value(<<"screenId">>, JsonData), Movie1, Errors1),
  {_, Movie3, Errors3} = init_field(seatsAvailable, proplists:get_value(<<"availableSeats">>, JsonData), Movie2, Errors2),

  case maps:size(Errors3) of
    0 -> {ok, Movie3};
    _ -> {error, Errors3}
  end.

%-----------------------
% Save movie to mnesia
%-----------------------
movie_save(Movie) ->
  ok.

%-----------------------
% Book a tiket. Update data in mnesia
%-----------------------
movie_book(ImdbId, ScreenId) ->
  ok.

%-----------------------
% Get movie info. Read from mnesia
%-----------------------
movie_get(ImdbId, ScreenId) ->
  ok.


%===========================================
%
% Private functions
%
%===========================================

%-------------------------------------------
% Init fields for movie entity
%-------------------------------------------

% field missing in json
init_field(Field, undefined, Movie, Errors) ->
  {error, Movie, maps:put(Field, <<"Undefined field">>, Errors)};

% validate imdbId and ask Internet for movie title
init_field(imdbId, Val, Movie, Errors) ->
  {ok, {{_, _, _}, _, Body}} = httpc:request(get, {"http://www.omdbapi.com/?i=" ++ binary_to_list(Val), []}, [], []),
  {struct, JsonData} = mochijson2:decode(Body),

  case proplists:get_value(<<"Response">>, JsonData) of
    <<"True">> ->
      Movie1 = maps:put(imdbId, Val, Movie),
      Movie2 = maps:put(movieTitle,  proplists:get_value(<<"Title">>, JsonData), Movie1),
      {ok, Movie2, Errors};

    _ -> {error, Movie, maps:put(imdbId, proplists:get_value(<<"Error">>, JsonData), Errors)}
  end;

% init screenId
init_field(screenId, Val, Movie, Errors) ->
  Movie1 = maps:put(screenId, Val, Movie),
  {ok, Movie1, Errors};

% init valid seats count
init_field(seatsAvailable, Val, Movie, Errors) when is_integer(Val), Val > 0 ->
  Movie1 = maps:put(seatsAvailable, Val, Movie),
  {ok, Movie1, Errors};

% set error for incorrect  seats count
init_field(seatsAvailable, _Val, Movie, Errors) ->
  {error, Movie, maps:put(seatsAvailable, <<"Field should be positive int">>, Errors)};

% catch all other fields which are incorrect by default
init_field(Field, _Val, Movie, Errors) ->
  {error, Movie, maps:put(Field, <<"incorrect_field">>, Errors)}.