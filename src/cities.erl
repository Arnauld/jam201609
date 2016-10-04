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

declare(Cities, _, []) ->
  Cities;
declare(Cities, City, [Link|Others]) ->
  NewCities = [{City, Link} | Cities],
  declare(NewCities, City, Others).

linked_to(Cities, City) ->
  collect_links_of(Cities, City, []).

collect_links_of([], _City, Collected) ->
  Collected;
collect_links_of([{City, Other} | Others], City, Collected) ->
  collect_links_of(Others, City, [Other | Collected]);
collect_links_of([{Other, City} | Others], City, Collected) ->
  collect_links_of(Others, City, [Other | Collected]);
collect_links_of([_ | Others], City, Collected) ->
  collect_links_of(Others, City, Collected).

