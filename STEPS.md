# Erlang & Rebar

1. Download and install erlang
2. Download and add rebar to `$PATH`

```bash
→ erl
Erlang/OTP 18 [erts-7.2.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V7.2.1  (abort with ^G)
1> q().
ok
2> %
```

```bash
$ rebar -V
rebar 2.6.1 R15B03 20150928_141254 git 2.6.1
```

Notes: One uses rebar 2 because it is actually the one supported by Intellij.
You should have a look to **rebar3** or **erlang.mk** instead.

# Project Setup

```bash
$ mkdir jam201609
$ cd jam201609
$ rebar create-app appid=myapp
$ touch rebar.config
$ mkdir src/
$ mkdir test/
```

`rebar.config`
```erlang
{erl_opts, [debug_info]}.
{deps, []}.
{cover_enabled, true}.
```

Compile and execute tests:

```bash
→ rebar clean compile eunit
==> jam201609 (clean)
==> jam201609 (compile)
Compiled src/myapp_app.erl
Compiled src/myapp_sup.erl
==> jam201609 (eunit)
Compiled src/myapp_app.erl
Compiled src/myapp_sup.erl
```

Setup Git:

```bash
$ git init && git status
$ echo ".eunit/" >> .gitignore
$ echo ".rebar/" >> .gitignore
$ echo "ebin/"   >> .gitignore
$ git add .
$ git commit -m "initial project setup"
[master (root-commit) 1243612] initial project setup
 ...
 create mode 100644 .gitignore
 create mode 100644 STEPS.md
 create mode 100644 rebar.config
```

Open Project in Intellij and setup sdk.

```bash
$ echo ".idea/" >> .gitignore
$ echo "*.iml" >> .gitignore
$ git status
$ git commit -a -m "ignore intellij"
```

# Step 1 - List and Pattern Matching

## Dummy Test

`test/cities_tests.erl`

```erlang
-module(cities_tests).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  ?assert(true).
```

```bash
→ rebar clean eunit
==> jam201609 (clean)
==> jam201609 (eunit)
Compiled src/myapp_app.erl
Compiled src/myapp_sup.erl
Compiled test/cities_tests.erl
  Test passed.
Cover analysis: /Users/Arnauld/Projects/erlang101/jam201609/.eunit/index.html
```

## Cities

`test/cities_tests.erl`

```erlang
-module(cities_tests).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  ?assert(true).


should_declare_a_city_and_its_links__test() ->
  Cities0 = cities:new(),
  Cities1 = cities:declare(Cities0, paris, [london, madrid, algiers, essen, milan]),

  ?assertEqual([london, madrid, algiers, essen, milan], cities:linked_to(Cities1, paris)).
```



```bash
→ rebar clean eunit
==> jam201609 (clean)
==> jam201609 (eunit)
Compiled src/myapp_app.erl
Compiled src/myapp_sup.erl
Compiled test/cities_tests.erl
cities_tests: should_declare_a_city_and_its_links__test...*failed*
in function cities:new/0
  called as new()
in call from cities_tests:should_declare_a_city_and_its_links__test/0 (test/cities_tests.erl, line 11)
in call from cities_tests:should_declare_a_city_and_its_links__test/0
**error:undef
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 1.
Cover analysis: /Users/Arnauld/Projects/erlang101/jam201609/.eunit/index.html
ERROR: One or more eunit tests failed.
ERROR: eunit failed while processing /Users/Arnauld/Projects/erlang101/jam201609: rebar_abort
```

### Basic Implementation

`src/cities.erl`

```erlang
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

```

Important points:

* `-export([...]).` to expose the function outside of the module
* `new/0` functions are identified by their name **and** their arity
* Variables are uppercased
* list is represented with: `[]` is the empty list; `[Head|Tail]` to build/decompose a list

```bash
→ rebar clean eunit
==> jam201609 (clean)
==> jam201609 (eunit)
Compiled src/myapp_app.erl
Compiled src/myapp_sup.erl
src/cities.erl:19: Warning: variable 'Cities' is unused
src/cities.erl:19: Warning: variable 'City' is unused
Compiled src/cities.erl
Compiled test/cities_tests.erl
cities_tests: should_declare_a_city_and_its_links__test...*failed*
in function cities_tests:'-should_declare_a_city_and_its_links__test/0-fun-0-'/2 (test/cities_tests.erl, line 14)
**error:{assertEqual,[{module,cities_tests},
              {line,14},
              {expression,"cities : linked_to ( Cities1 , paris )"},
              {expected,[london,madrid,algiers,essen,milan]},
              {value,[]}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 1.
Cover analysis: /Users/Arnauld/Projects/erlang101/jam201609/.eunit/index.html
ERROR: One or more eunit tests failed.
ERROR: eunit failed while processing /Users/Arnauld/Projects/erlang101/jam201609: rebar_abort
```

### Fix `linked_to`: Tail Recursion



`src/cities.erl`

```erlang

linked_to([], City) ->
  [];
linked_to([{City, Links} | Tail], City) ->
  Links;
linked_to([Head | Tail], City) ->
  linked_to(Tail, City).
```


```bash
→ rebar clean eunit
==> jam201609 (clean)
==> jam201609 (eunit)
Compiled src/myapp_app.erl
Compiled src/myapp_sup.erl
src/cities.erl:19: Warning: variable 'City' is unused
src/cities.erl:21: Warning: variable 'Tail' is unused
src/cities.erl:23: Warning: variable 'Head' is unused
Compiled src/cities.erl
Compiled test/cities_tests.erl
  2 tests passed.
Cover analysis: /Users/Arnauld/Projects/erlang101/jam201609/.eunit/index.html
```

Replace unused variable with `_`

```erlang
linked_to([], _) ->
  [];
linked_to([{City, Links} | _], City) ->
  Links;
linked_to([_ | Tail], City) ->
  linked_to(Tail, City).
```

### Rework Cities model

`test/cities_tests.erl`

```erlang
should_declare_city_and_complete_existing_links__test() ->
  Cities0 = cities:new(),
  Cities1 = cities:declare(Cities0, paris, [london, madrid]),
  Cities2 = cities:declare(Cities1, algiers, [paris, madrid]),

  ?assertEqual(lists:sort([london, madrid, algiers]),
               lists:sort(cities:linked_to(Cities2, paris))).
```


```bash
→ rebar clean eunit
==> jam201609 (clean)
==> jam201609 (eunit)
Compiled src/myapp_app.erl
Compiled src/myapp_sup.erl
Compiled src/cities.erl
Compiled test/cities_tests.erl
cities_tests: should_declare_city_and_complete_existing_links__test...*failed*
in function cities_tests:'-should_declare_city_and_complete_existing_links__test/0-fun-0-'/2 (test/cities_tests.erl, line 22)
**error:{assertEqual,[{module,cities_tests},
              {line,22},
              {expression,"lists : sort ( cities : linked_to ( Cities2 , paris ) )"},
              {expected,[algiers,london,madrid]},
              {value,[london,madrid]}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
Cover analysis: /Users/Arnauld/Projects/erlang101/jam201609/.eunit/index.html
ERROR: One or more eunit tests failed.
ERROR: eunit failed while processing /Users/Arnauld/Projects/erlang101/jam201609: rebar_abort
```

**Exercise: fix this!**

Tips, goes from:

```
[ {paris,   [london, madrid]},
  {algiers, [paris, madrid] } ]
```

to:

```
[ {paris,   london},
  {paris,   madrid},
  {algiers, paris },
  {algiers, madrid} ]
```

...

Declaration first:

```erlang
declare(Cities, _, []) ->
  Cities;
declare(Cities, City, [Link|Others]) ->
  NewCities = [{City, Link} | Cities],
  declare(NewCities, City, Others).
```

Fix `linked_to`:

```erlang

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

```

```bash
→ rebar eunit
==> jam201609 (eunit)
Compiled src/cities.erl
  All 3 tests passed.
Cover analysis: /Users/Arnauld/Projects/erlang101/jam201609/.eunit/index.html
```

### Ooops: same duplicate

Let's play with the terminal:

```bash
→ erl
Erlang/OTP 18 [erts-7.2.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V7.2.1  (abort with ^G)
1> c('src/cities.erl').
{ok,cities}
2> Cities0 = cities:
declare/3      linked_to/2    module_info/0  module_info/1  new/0

2> Cities0 = cities:new().
[]
3> Cities1 = cities:declare(Cities0, paris, [london, essen]).
[{paris,essen},{paris,london}]
4> Cities2 = cities:declare(Cities1, london, [paris, essen, new_york]).
[{london,new_york},
 {london,essen},
 {london,paris},
 {paris,essen},
 {paris,london}]
5> cities:linked_to(Cities2, paris).
[london,essen,london]
6> q().
ok
7> %
```

```bash
echo "*.beam" >> .gitignore
```

Grab this behavior in a corresponding test:

`test/cities_test.erl`

```erlang
should_return_unique_links_even_on_multiple_declarations__test() ->
  Cities0 = cities:new(),
  Cities1 = cities:declare(Cities0, paris, [london, essen]),
  Cities2 = cities:declare(Cities1, london, [paris, essen, new_york]),

  ?assertEqual(lists:sort([london, essen]),
               lists:sort(cities:linked_to(Cities2, paris))).
```

```bash
→ rebar eunit
==> jam201609 (eunit)
Compiled src/cities.erl
Compiled test/cities_tests.erl
cities_tests: should_return_unique_links_even_on_multiple_declarations__test...*failed*
in function cities_tests:'-should_return_unique_links_even_on_multiple_declarations__test/0-fun-0-'/2 (test/cities_tests.erl, line 30)
**error:{assertEqual,[{module,cities_tests},
              {line,30},
              {expression,"lists : sort ( cities : linked_to ( Cities2 , paris ) )"},
              {expected,[essen,london]},
              {value,[essen,london,london]}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
```

**Exercise: fix this!**

Tips, goes from:

```erlang
  ...
  NewCities = [{City, Link} | Cities],
  ...
```

to

```erlang
  ...
  NewCities = case is_link_present(Cities, City, Link) of
                true  -> 
                    Cities;
                _ -> 
                    [{City, Link} | Cities]
              end,
                
  
  is_link_present([], _City1, _City2) ->
    false;
  is_link_present(...) ->
    ...
```

`src/cities.erl`

```erlang
declare(Cities, _, []) ->
  Cities;
declare(Cities, City, [Link|Others]) ->
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
is_link_present([{City1, City2}|_Others], City1, City2) -> true;
is_link_present([{City2, City1}|_Others], City1, City2) -> true;
is_link_present([_Head|Others], City1, City2) -> is_link_present(Others, City1, City2).
```


# Step 2 - Process

How to maintain state across multiple invocation without providing previous state.

`test/cities_test.erl`

```erlang
should_start_cities_which_then_maintain_its_own_state__test() ->
  Pid = cities:start(),
  Pid ! {declare, paris, [milan, essen]},
  Pid ! {declare, london, [paris, essen, new_york]},
  Pid ! {linked_to, paris, self()},
  receive
    {linked_to, paris, Links} ->
      ?assertEqual(list:sort([milan, essen, london]),
                   list:sort(Links))
  end.
```

[Protocol vs API](doc/protocol_vs_api.md)

```
[self]                              [cities]    [mailbox]
   |    start                          |
   ||--------------------------------->||
   ||   pid                            ||
   || <--------------------------------||
   |                                   |
   |  M1: {declare, City, Links}       |
   |------------------------------------------> [M1]
   |                                   |
   |  M2: {declare, City, Links}       |
   |------------------------------------------> [M1,M2]
   |                                   |
   |  M3: {linked_to, City, From}      |
   |------------------------------------------> [M1,M2,M3]
   |                                   |
   ||                                  |
   ||<---------------------------------|
   ||                                  |
```

* Each **Process** (Pid) has its own Mailbox to store all received message
* **Mailbox** is a FIFO queue
* A Process is managed by a scheduler, that orchestrates parts of its execution based on 'tick': 
  a process can execute up to a certain amount of tick before, 
  scheduler trigger an other process execution. This allow all processes to be executed, 
  without any preemption from a given process which take all processing power...
* `receive` instruction traverses the entire **mailbox to pattern match** a message; 
  its a blocking instruction that pause the process until a message match the patterns or
  a timeout occurs.
* `self()` gives the **current Pid**.
  
  
```batch
→ rebar eunit
==> jam201609 (eunit)
src/cities.erl:21: Warning: variable 'Msg' is unused
Compiled src/cities.erl
cities_tests: should_start_cities_which_then_maintain_its_own_state__test...*timed out*
undefined
=======================================================
  Failed: 0.  Skipped: 0.  Passed: 4.
One or more tests were cancelled.
Cover analysis: /Users/Arnauld/Projects/erlang101/jam201609/.eunit/index.html
ERROR: One or more eunit tests failed.
ERROR: eunit failed while processing /Users/Arnauld/Projects/erlang101/jam201609: rebar_abort
```

What happened?

...

`src/cities.erl`

```erlang
loop(Cities) ->
  receive
    {declare, City, Links} ->
      NewCities = cities:declare(Cities, City, Links),
      loop(NewCities);

    {linked_to, City, From} ->
      Links = cities:linked_to(Cities, City),
      From ! {linked_to, City, Links},
      loop(Cities)
  end.
```

Problem: protocol coherence

* request `{linked_to, City, From}`
* response `{linked_to, City, Links}`

## Mask Protocol behind API

```erlang
Pid = cities:start()
```

**How to know if process is started, Pid is a non typed variable?**

use pattern matching!

```erlang
{ok, Pid} = cities:start()
```

**Also, I still need the Pid to interact with the cities process!!**

use register name!

`src/cities.erl`

```erlang
start() ->
  Pid = spawn(?MODULE, loop, [cities:new()]),
  register(?MODULE, Pid),
  {ok, Pid}.
```

`test/cities_test.erl`

```erlang
should_start_cities_and_interact_with_its_api__test() ->
  {ok, Pid} = cities:start(),
  cities:declare(paris, [milan, essen]),
  cities:declare(london, [paris, essen, new_york]),
  Links = cities:linked_to(paris),
  ?assertEqual(
    lists:sort([milan, essen, london]),
    lists:sort(Links)).
```

```bash
→ rebar eunit
==> jam201609 (eunit)
test/cities_tests.erl:50: Warning: variable 'Pid' is unused
Compiled test/cities_tests.erl
cities_tests: should_start_cities_and_interact_with_its_api__test...*failed*
in function erlang:register/2
  called as register(cities,<0.77.0>)
in call from cities:start/0 (src/cities.erl, line 17)
in call from cities_tests:should_start_cities_and_interact_with_its_api__test/0 (test/cities_tests.erl, line 50)
**error:badarg
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 5.
Cover analysis: /Users/Arnauld/Projects/erlang101/jam201609/.eunit/index.html
ERROR: One or more eunit tests failed.
ERROR: eunit failed while processing /Users/Arnauld/Projects/erlang101/jam201609: rebar_abort
```

What happened?

```bash
→ erl
Erlang/OTP 18 [erts-7.2.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V7.2.1  (abort with ^G)
1> c('src/cities.erl').
{ok,cities}
2> cities:start().
{ok,<0.40.0>}
3> cities:start().
** exception error: bad argument
     in function  register/2
        called as register(cities,<0.42.0>)
     in call from cities:start/0 (src/cities.erl, line 17)
```


Process started in previous test is still running thus register fails...

One needs to stop the process:

`src/cities.erl`
```erlang
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
```

And put a **finally** block in tests:

```erlang
should_start_cities_which_then_maintain_its_own_state__test() ->
  {ok, Pid} = cities:start(),
  try
    Pid ! {declare, paris, [milan, essen]},
    Pid ! {declare, london, [paris, essen, new_york]},
    Pid ! {linked_to, paris, self()},
    receive
      {linked_to, paris, Links} ->
        ?assertEqual(
          lists:sort([milan, essen, london]),
          lists:sort(Links))
    end
  after
    Pid ! stop
  end.

should_start_cities_and_interact_with_its_api__test() ->
  {ok, _Pid} = cities:start(),
  try
    cities:declare(paris, [milan, essen]),
    cities:declare(london, [paris, essen, new_york]),
    Links = cities:linked_to(paris),
    ?assertEqual(
      lists:sort([milan, essen, london]),
      lists:sort(Links))
  after
    cities:stop()
  end.
```

```bash
→ rebar eunit
==> jam201609 (eunit)
Compiled test/cities_tests.erl
cities_tests: should_start_cities_and_interact_with_its_api__test...*failed*
in function cities:stop/0
  called as stop()
in call from cities_tests:should_start_cities_and_interact_with_its_api__test/0 (test/cities_tests.erl, line 55)
in call from cities_tests:should_start_cities_and_interact_with_its_api__test/0
**error:undef
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 5.
Cover analysis: /Users/Arnauld/Projects/erlang101/jam201609/.eunit/index.html
ERROR: One or more eunit tests failed.
ERROR: eunit failed while processing /Users/Arnauld/Projects/erlang101/jam201609: rebar_abort

```

Implements the missing functions...

```erlang
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
```

## Let's play with distributed behavior

`Terminal 1`

```bash
→ erl -sname T1
Erlang/OTP 18 [erts-7.2.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V7.2.1  (abort with ^G)
(T1@Mentem)1> nodes().
[]
(T1@Mentem)2>
```

`Terminal 2`

```bash
→ erl -sname T2
Erlang/OTP 18 [erts-7.2.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V7.2.1  (abort with ^G)
(T2@Mentem)1> nodes().
[]
(T2@Mentem)2> net_kernel:connect('T1@Mentem').
true
(T2@Mentem)3> nodes().
['T1@Mentem']
(T2@Mentem)4>
```

`Terminal 1`

```bash
(T1@Mentem)2> nodes().
['T2@Mentem']
(T1@Mentem)3> c('src/cities.erl').
{ok,cities}
(T1@Mentem)4> cities:start().
{ok,<0.51.0>}
(T1@Mentem)5>
```

`Terminal 2`

```bash
(T2@Mentem)4> rpc:call('T1@Mentem', cities, declare, [paris, [london, madrid]]).
{declare,paris,[london,madrid]}
```

`Terminal 1`

```bash
(T1@Mentem)5> cities:linked_to(paris).
[london,madrid]
```

What happen if...

`Terminal 2`

```bash
(T2@Mentem)5> self().
<0.39.0>
(T2@Mentem)6> rpc:call('T1@Mentem', cities, declare, [paris, essen]).
{declare,paris,essen}
(T2@Mentem)7>
```

`Terminal 1`

```bash
(T1@Mentem)6> self().
<0.39.0>
(T1@Mentem)7>
=ERROR REPORT==== 6-Oct-2016::01:22:02 ===
Error in process <0.51.0> on node 'T1@Mentem' with exit value:
{function_clause,[{cities,declare,
                          [[{paris,madrid},{paris,london}],paris,essen],
                          [{file,"src/cities.erl"},{line,52}]},
                  {cities,loop,1,[{file,"src/cities.erl"},{line,37}]}]}
```

`Terminal 2`

```bash
(T2@Mentem)7> rpc:call('T1@Mentem', cities, declare, [paris, essen]).
{badrpc,{'EXIT',{badarg,[{cities,declare,2,
                                 [{file,"src/cities.erl"},{line,25}]},
                         {rpc,'-handle_call_call/6-fun-0-',5,
                              [{file,"rpc.erl"},{line,206}]}]}}}
```

Ooops process crashed and is not any more available for Terminal 2...

`Terminal 1`

```bash
(T1@Mentem)7> cities:linked_to(paris).
** exception error: bad argument
     in function  cities:linked_to/1 (src/cities.erl, line 28)
(T1@Mentem)8> self().
<0.58.0>
```

* `self()` has changed!

# Still more process: link and monitor

## Homemade supervisor

`src/cities_sup.erl`

```erlang
-module(cities_sup).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start() ->
  spawn(?MODULE, init, []).

init([]) ->
  process_flag(trap_exit, true),
  {ok, Pid} = start_cities(),
  loop(Pid).

start_cities() ->
  cities:start_link().

loop(CitiesPid) ->
    receive
      {'EXIT', Pid, normal} ->
        ok;
      
      {'EXIT', Pid, Reason} ->
        error_logger:info_msg("Ooops, cities stopped ~p ~n", [Reason]),
        {ok, NewPid} = start_cities(),
        loop(NewPid)
    end.
```

`src/cities.erl` : slightly modified to add a log on start and a `start_link`:


```erlang
start() ->
  Pid = spawn(?MODULE, init, []),
  register(?MODULE, Pid),
  {ok, Pid}.

start_link() ->
  Pid = spawn_link(?MODULE, init, [[]]),
  register(?MODULE, Pid),
  {ok, Pid}.

init([]) ->
  Cities = cities:new(),
  error_logger:info_msg("Cities started ~n", []),
  loop(Cities).

```

```bash
→ erl
Erlang/OTP 18 [erts-7.2.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V7.2.1  (abort with ^G)
1> c('src/cities.erl').
{ok,cities}
2> c('src/cities_sup.erl').
src/cities_sup.erl:23: Warning: variable 'CitiesPid' is unused
src/cities_sup.erl:25: Warning: variable 'Pid' is unused
src/cities_sup.erl:28: Warning: variable 'Pid' is unused
{ok,cities_sup}
3> cities_sup:start().
<0.45.0>
4>
=INFO REPORT==== 7-Oct-2016::19:30:10 ===
Cities started

4> cities:declare(paris, [madrid]).
{declare,paris,[madrid]}
5> cities:linked_to(paris).
[madrid]
6> cities:declare(paris, somthing_invalid).

=INFO REPORT==== 7-Oct-2016::19:30:44 ===
Ooops, cities stopped {function_clause,
                          [{cities,declare,
                               [[{paris,madrid}],paris,somthing_invalid],
                               [{file,"src/cities.erl"},{line,63}]},
                           {cities,loop,1,
                               [{file,"src/cities.erl"},{line,48}]}]}

=ERROR REPORT==== 7-Oct-2016::19:30:44 ===
Error in process <0.46.0> with exit value:
{function_clause,[{cities,declare,
                          [[{paris,madrid}],paris,somthing_invalid],
                          [{file,"src/cities.erl"},{line,63}]},
                  {cities,loop,1,[{file,"src/cities.erl"},{line,48}]}]}
{declare,paris,somthing_invalid}

=INFO REPORT==== 7-Oct-2016::19:30:44 ===
Cities started
7> cities:linked_to(paris).
[]
8>
```

