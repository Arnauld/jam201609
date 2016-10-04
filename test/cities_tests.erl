-module(cities_tests).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  ?assert(true).


should_declare_a_city_and_its_links__test() ->
  Cities0 = cities:new(),
  Cities1 = cities:declare(Cities0, paris, [london, madrid, algiers, essen, milan]),

  ?assertEqual([london, madrid, algiers, essen, milan], cities:linked_to(Cities1, paris)).

should_declare_city_and_complete_existing_links__test() ->
  Cities0 = cities:new(),
  Cities1 = cities:declare(Cities0, paris, [london, madrid]),
  Cities2 = cities:declare(Cities1, algiers, [paris, madrid]),

  ?assertEqual(lists:sort([london, madrid, algiers]),
               lists:sort(cities:linked_to(Cities2, paris))).
