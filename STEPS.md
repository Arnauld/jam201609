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

