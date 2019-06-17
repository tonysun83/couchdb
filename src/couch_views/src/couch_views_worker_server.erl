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


-define(TYPE_CHECK_PERIOD_DEFAULT, 500).
-define(MAX_JITTER_DEFAULT, 100).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_) ->
    couch_views_jobs:set_timeout(),
    schedule_check(),
    {ok, #{}}.


terminate(_, _St) ->
    ok.


handle_call(Msg, _From, St) ->
    {stop, {bad_call, Msg}, {bad_call, Msg}, St}.


handle_cast(Msg, St) ->
    {stop, {bad_cast, Msg}, St}.


handle_info(check_for_jobs, State) ->
    accept_jobs(),
    schedule_check(),
    {noreply, State};

handle_info({'DOWN', _Ref, process, Pid, Reason}, St) ->
    LogMsg = "~p : process ~p exited with ~p",
    couch_log:error(LogMsg, [?MODULE, Pid, Reason]),
    {noreply, St};

handle_info(Msg, St) ->
    couch_log:notice("~s ignoring info ~w", [?MODULE, Msg]),
    {noreply, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


accept_jobs() ->
    case couch_views_jobs:accept() of
        not_found ->
            ok;
        {ok, Job, JobData} ->
            start_worker(Job, JobData),
            % keep accepting jobs until not_found
            accept_jobs()
    end.


start_worker(Job, JobData) ->
    % TODO Should I monitor it, or let jobs do that?
    spawn_monitor(fun () -> couch_views_worker:start(Job, JobData) end),
    ok.


schedule_check() ->
    Timeout = get_period_msec(),
    MaxJitter = max(Timeout div 2, get_max_jitter_msec()),
    Wait = Timeout + rand:uniform(max(1, MaxJitter)),
    timer:send_after(Wait, self(), check_for_jobs).


get_period_msec() ->
    config:get_integer("couch_views", "type_check_period_msec",
        ?TYPE_CHECK_PERIOD_DEFAULT).


get_max_jitter_msec() ->
    config:get_integer("couch_views", "type_check_max_jitter_msec",
        ?MAX_JITTER_DEFAULT).
