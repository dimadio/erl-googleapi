-module(googleapi_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, add_child/4, stop_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, Module, Type, Args), {Name, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

add_child(Name, Module, Type, Args)->
    supervisor:start_child(?MODULE, ?CHILD(Name, Module, Type, Args)).


stop_child(Name)->
    ok =  supervisor:terminate_child(?MODULE, Name),
    supervisor:delete_child(?MODULE, Name).
