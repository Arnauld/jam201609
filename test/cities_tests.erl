-module(cities_tests).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  ?assert(true).


should_declare_a_city_and_its_links__test() ->
  Cities0 = cities:new(),
  Cities1 = cities:declare(Cities0, paris, [london, madrid, algiers, essen, milan]),

  ?assertEqual(
    [london, madrid, algiers, essen, milan],
    cities:linked_to(Cities1, paris)).

should_declare_city_and_complete_existing_links__test() ->
  Cities0 = cities:new(),
  Cities1 = cities:declare(Cities0, paris, [london, madrid]),
  Cities2 = cities:declare(Cities1, algiers, [paris, madrid]),

  ?assertEqual(
    lists:sort([london, madrid, algiers]),
    lists:sort(cities:linked_to(Cities2, paris))).

should_return_unique_links_even_on_multiple_declarations__test() ->
  Cities0 = cities:new(),
  Cities1 = cities:declare(Cities0, paris, [london, essen]),
  Cities2 = cities:declare(Cities1, london, [paris, essen, new_york]),

  ?assertEqual(
    lists:sort([london, essen]),
    lists:sort(cities:linked_to(Cities2, paris))).


should_start_cities_which_then_maintain_its_own_state__test() ->
  {ok, Pid} = cities:start(),
  Pid ! {declare, paris, [milan, essen]},
  Pid ! {declare, london, [paris, essen, new_york]},
  Pid ! {linked_to, paris, self()},
  receive
    {linked_to, paris, Links} ->
      ?assertEqual(
        lists:sort([milan, essen, london]),
        lists:sort(Links))
  end.

should_start_cities_and_interact_with_its_api__test() ->
  {ok, Pid} = cities:start(),
  cities:declare(paris, [milan, essen]),
  cities:declare(london, [paris, essen, new_york]),
  Links = cities:linked_to(paris),
  ?assertEqual(
    lists:sort([milan, essen, london]),
    lists:sort(Links)).
