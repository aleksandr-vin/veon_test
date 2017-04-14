%%%-------------------------------------------------------------------
%%% @author konstantin.shamko
%%% @doc
%%%
%%% @end
%%% Created : 14. Apr 2017 10:26 AM
%%%-------------------------------------------------------------------
-module(veon_movie).
-author("konstantin.shamko").

%% API
-export([init_table/0, new/1, movie_save/1, movie_book/2, movie_get/2]).

%%---------------------------------------------------------------------
%% Data Type: movie
%% where:
%%    id: Unique field to identify movie: tuple {imdbId, screenId}
%%    imdbId: String with movie id from imdb
%%    screenId: String with externally managed screen id
%%    movieTitle: String with move title. Found by imdb id via 3rd party API
%%    seatsAvailable: Integer with number of available seats
%%    seatsReserved: Integer with number of reserved seats
%%----------------------------------------------------------------------
-record(movie, {id, imdbId, screenId, movieTitle, seatsAvailable, seatsReserved }).

%===========================================
%
% Public functions
%
%===========================================

%------------------
% Create mnesia table
%------------------
init_table() ->
  mnesia:create_table(movie, [
    {attributes, record_info(fields, movie)},
    {type, set}
  ]).


%%----------------------------------------------------------------------
%% Function: new/1
%% Purpose: Create new movie entity
%% Args:   JsonData - proplist (http://erlang.org/doc/man/proplists.html) generated from input JSON
%% Returns: A tuple of {ok, Movie} - Movie is a #movie record
%%     or {error, Errors} - Errors - map with fields and associated errors
%%----------------------------------------------------------------------
new(JsonData) ->

  Errors = maps:new(),
  Movie = maps:new(),

  {_, Movie1, Errors1} = init_field(imdbId, proplists:get_value(<<"imdbId">>, JsonData), Movie, Errors),
  {_, Movie2, Errors2} = init_field(screenId, proplists:get_value(<<"screenId">>, JsonData), Movie1, Errors1),
  {_, Movie3, Errors3} = init_field(seatsAvailable, proplists:get_value(<<"availableSeats">>, JsonData), Movie2, Errors2),

  case maps:size(Errors3) of
    0 ->
      M = #movie{
        id = {maps:get(imdbId, Movie3),maps:get(screenId, Movie3)},
        imdbId = maps:get(imdbId, Movie3),
        screenId = maps:get(screenId, Movie3),
        movieTitle = maps:get(movieTitle, Movie3),
        seatsAvailable = maps:get(seatsAvailable, Movie3),
        seatsReserved = 0
      },
      {ok, M};
    _ -> {error, Errors3}
  end.

%%----------------------------------------------------------------------
%% Function: movie_save/1
%% Purpose: Save movie to mnesia
%% Args:   Movie - #movie record. basically it can be initiated with new/1
%% Returns: ok | exit({aborted, Reason})
%%----------------------------------------------------------------------
movie_save(Movie) ->
  mnesia:dirty_write(Movie).


%%----------------------------------------------------------------------
%% Function: movie_book/2
%% Purpose: Book a tiket. Update data in mnesia
%% Args:   ImdbId - String with movie id from imdb
%%         ScreenId - String with externally managed screen id
%% Returns: {ok, Message} | {error, message}
%%----------------------------------------------------------------------
movie_book(ImdbId, ScreenId) ->

  F = fun() ->

      case mnesia:wread({movie, {ImdbId, ScreenId}}) of
        [] -> {error, movie_not_found};
        [Movie] ->
          ReservedSeats = Movie#movie.seatsReserved + 1,
          if
            ReservedSeats =< Movie#movie.seatsAvailable ->
              mnesia:write(Movie#movie{seatsReserved = ReservedSeats}),
              {ok, seat_reserved};
            true -> {error, no_available_seats}
          end
      end
    end,

  mnesia:transaction(F).

%%----------------------------------------------------------------------
%% Function: movie_get/2
%% Purpose: Get movie info. Read from mnesia
%% Args:   ImdbId - String with movie id from imdb
%%         ScreenId - String with externally managed screen id
%% Returns: List with movie data | Empty list
%%----------------------------------------------------------------------
movie_get(ImdbId, ScreenId) ->

  case mnesia:dirty_read(movie, {ImdbId, ScreenId}) of
    [] -> [];
    [Movie] ->
      [
        {imdbId, Movie#movie.imdbId},
        {screenId, Movie#movie.screenId},
        {movieTitle, Movie#movie.movieTitle},
        {availableSeats, Movie#movie.seatsAvailable},
        {reservedSeats, Movie#movie.seatsReserved}
      ]
  end.



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