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
 5 files changed, 62 insertions(+)
 create mode 100644 .gitignore
 create mode 100644 rebar.config
```

Open Project in Intellij and setup sdk.

```bash
$ echo ".idea/" >> .gitignore
$ echo "*.iml" >> .gitignore
$ git status
$ git commit -a -m "ignore intellij"
```
