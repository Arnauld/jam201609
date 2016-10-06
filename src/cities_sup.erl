-module(cities_sup).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/0, init/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start() ->
  spawn(?MODULE, init, [[]]).

init([]) ->
  process_flag(trap_exit, true),
  {ok, Pid} = start_cities(),
  loop(Pid).

start_cities() ->
  cities:start().

loop(CitiesPid) ->
    receive
      {'EXIT', Pid, normal} ->
        ok;

      {'EXIT', Pid, Reason} ->
        error_logger:info_msg("Ooops, cities stopped ~p ~n", [Reason]),
        {ok, Pid} = start_cities(),
        loop(Pid)
    end.