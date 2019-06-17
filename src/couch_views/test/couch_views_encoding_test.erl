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

-module(couch_views_encoding_test).

-include_lib("eunit/include/eunit.hrl").

val_encoding_test() ->
    Values = [
        null,
        true,
        1.0,
        <<"a">>,
        {[{<<"a">>, 1.0}, {<<"b">>, <<"hello">>}]}
    ],
    lists:foreach(fun (Val) ->
        EncVal = couch_views_encoding:encode(Val),
        ?assertEqual(Val, couch_views_encoding:decode(EncVal))
    end, Values).


correct_ordering_test() ->
    Ordered = [
        %  Special values sort before all other types
        null,
        false,
        true,

        %  Then numbers
        % 1,
        % 2,
        % 3.0,
        % 4,

        1.0,
        2.0,
        3.0,
        4.0,

        [<<"a">>],
        [<<"b">>],
        [<<"b">>, <<"c">>],
        [<<"b">>, <<"c">>, <<"a">>],
        [<<"b">>, <<"d">>],
        [<<"b">>, <<"d">>, <<"e">>],

        % Then objects, compared each key value in the list until different.
        % Larger objects sort after their subset objects
        {[{<<"a">>, 1.0}]},
        {[{<<"a">>, 2.0}]},
        {[{<<"b">>, 1.0}]},
        {[{<<"b">>, 2.0}]},

        % Member order does matter for collation
        {[{<<"b">>, 2.0}, {<<"a">>, 1.0}]},
        {[{<<"b">>, 2.0}, {<<"c">>, 2.0}]}

    ],

    BinList = lists:map(fun couch_views_encoding:encode/1, Ordered),
    SortedBinList = lists:sort(BinList),
    DecodedBinList = lists:map(fun couch_views_encoding:decode/1,
        SortedBinList),
    ?assertEqual(Ordered, DecodedBinList).
