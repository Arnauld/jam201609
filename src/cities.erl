-module(cities).
-author("Arnauld").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([new/0, declare/3, linked_to/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
new() ->
  [].

declare(Cities, City, LinkedTo) ->
  [{City, LinkedTo} | Cities].

linked_to(Cities, City) ->
  [].

