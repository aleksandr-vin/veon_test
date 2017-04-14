veon
=====

An OTP application

Build
-----

    $ rebar3 compile
    $ erl -pa _build/default/lib/*/ebin -s veon


Endpoints
---------

    $ curl -X POST -d '{"imdbId": "tt0111161", "availableSeats": 100, "screenId": "screen_123456"}' -H "Content-Type: application/json" http://localhost:8889/movie/add
    $ curl -X GET  -H "Content-Type: application/json" http://localhost:8889/movie/get/tt0111161/screen_123456
