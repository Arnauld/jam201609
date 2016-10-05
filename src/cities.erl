-module(cities).
-author("Arnauld").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([new/0, declare/3, linked_to/2]).

-export([start/0, loop/1, stop/0]).
-export([declare/2, linked_to/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start() ->
  Pid = spawn(?MODULE, loop, [cities:new()]),
  register(?MODULE, Pid),
  {ok, Pid}.

stop() ->
  ?MODULE ! stop.

declare(City, Links) ->
  ?MODULE ! {declare, City, Links}.

linked_to(City) ->
  ?MODULE ! {linked_to, City, self()},
  receive
    {linked_to, City, Links} ->
      Links
  end.

loop(Cities) ->
  receive
    {declare, City, Links} ->
      NewCities = cities:declare(Cities, City, Links),
      loop(NewCities);

    {linked_to, City, From} ->
      Links = cities:linked_to(Cities, City),
      From ! {linked_to, City, Links},
      loop(Cities);

    stop ->
      ok
  end.

new() ->
  [].

declare(Cities, _, []) ->
  Cities;
declare(Cities, City, [Link | Others]) ->
  NewCities = case is_link_present(Cities, City, Link) of
                true ->
                  Cities;
                _ ->
                  [{City, Link} | Cities]
              end,
  declare(NewCities, City, Others).

linked_to(Cities, City) ->
  collect_links_of(Cities, City, []).

is_link_present([], _City1, _City2) -> false;
is_link_present([{City1, City2} | _Others], City1, City2) -> true;
is_link_present([{City2, City1} | _Others], City1, City2) -> true;
is_link_present([_Head | Others], City1, City2) -> is_link_present(Others, City1, City2).

collect_links_of([], _City, Collected) ->
  Collected;
collect_links_of([{City, Other} | Others], City, Collected) ->
  collect_links_of(Others, City, [Other | Collected]);
collect_links_of([{Other, City} | Others], City, Collected) ->
  collect_links_of(Others, City, [Other | Collected]);
collect_links_of([_ | Others], City, Collected) ->
  collect_links_of(Others, City, Collected).

