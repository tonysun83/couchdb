% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_views_worker_server).


-behaviour(gen_server).


-export([
    start_link/0
]).


-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-define(MAX_WORKERS, 100).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_) ->
    couch_views_jobs:set_timeout(),
    State0 = #{
        workers => #{},
        acceptor_pid => undefined
    },
    State = spawn_acceptor(State0),
    {ok, State}.


terminate(_, _St) ->
    ok.


handle_call(Msg, _From, St) ->
    {stop, {bad_call, Msg}, {bad_call, Msg}, St}.


handle_cast({job, Job, JobData}, State) ->
    State1 = start_worker(State, Job, JobData),
    State2 = spawn_acceptor(State1),
    {noreply, State2};

handle_cast(Msg, St) ->
    {stop, {bad_cast, Msg}, St}.


handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    LogMsg = "~p : process ~p exited with ~p",
    couch_log:error(LogMsg, [?MODULE, Pid, Reason]),
    State1 = check_finished_process(State, Pid),
    {noreply, State1};

handle_info(Msg, St) ->
    couch_log:notice("~s ignoring info ~w", [?MODULE, Msg]),
    {noreply, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


start_worker(State, Job, JobData) ->
    #{workers := Workers} = State,
    {Pid, _Ref} = spawn_monitor(fun () -> couch_views_worker:start(Job, JobData) end),
    Workers1 = Workers#{Pid => true},
    State#{workers := Workers1}.


spawn_acceptor(State) ->
    #{
        workers := Workers,
        acceptor_pid := Pid
    } = State,
    MaxWorkers = config:get_integer("couch_views", "max_workers", ?MAX_WORKERS),
    case maps:size(Workers) >= MaxWorkers of
        false when not is_pid(Pid) ->
            Parent = self(),
            {Pid1, _Ref} = spawn_monitor(fun() -> blocking_acceptor(Parent) end),
            State#{acceptor_pid := Pid1};
        _ ->
            State
    end.


blocking_acceptor(Parent) ->
    case couch_views_jobs:accept() of
        not_found ->
            blocking_acceptor(Parent);
        {ok, Job, JobData} ->
            gen_server:cast(Parent, {job, Job, JobData})
    end.


check_finished_process(#{acceptor_pid := Pid} = State, Pid) ->
    State1 = State#{acceptor_pid := undefined},
    spawn_acceptor(State1);

check_finished_process(State, Pid) ->
    #{workers := Workers} = State,
    Workers1 = maps:remove(Pid, Workers),
    State#{workers := Workers1}.
