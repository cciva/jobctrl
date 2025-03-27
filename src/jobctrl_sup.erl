-module(jobctrl_sup).
-behaviour(supervisor).

% public
-export([start_link/0]).

% callbacks
-export([init/1]).

start_link() ->
  supervisor:start_link({local, jobctrl_master}, ?MODULE, []).

init(_Args) ->
  SupFlags = #{
    strategy => one_for_one
  },

  JobQueueSup = #{
    id => job_queue,
    start => {job_queue, start_link, []},
    modules => [job_queue],
    type => supervisor
  },

  Children = [JobQueueSup],
  {ok, {SupFlags, Children}}.
