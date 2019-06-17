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

-module(couch_views_encoding).


-export([
    encode/1,
    decode/1
]).


-define(NULL, 16#00).
-define(FALSE, 16#26).
-define(TRUE, 16#27).
-define(NUMBER, 16#40).
-define(STRING, 16#41).
-define(LIST, 16#42).
-define(OBJECT, 16#43).


encode(X) ->
    Encoded = encode_int(X),
    erlfdb_tuple:pack(Encoded).


decode(EncodedVal) ->
    Val = erlfdb_tuple:unpack(EncodedVal),
    decode_int(Val).


encode_int(X) when is_atom(X) -> encode_atom(X);
encode_int(X) when is_number(X) -> encode_number(X);
encode_int(X) when is_binary(X) -> encode_binary(X);
encode_int(X) when is_list(X) -> encode_list(X);
encode_int(X) when is_tuple(X) -> encode_object(X).


encode_atom(null) ->
    {?NULL};

encode_atom(false) ->
    {?FALSE};

encode_atom(true) ->
    {?TRUE}.


encode_number(Val) ->
    {?NUMBER, float(Val)}.


encode_binary(Val) ->
    % TODO add sort strings
    {?STRING, Val}.


encode_list(List) ->
    EncodedItems = lists:map(fun encode_int/1, List),
    {?LIST, list_to_tuple(EncodedItems)}.


encode_object({Props}) ->
    EncodedProps = lists:map(fun({K, V}) -> 
        EncodedK = encode_int(K),
        EncodedV = encode_int(V),
        {EncodedK, EncodedV}
    end, Props),
    {?OBJECT, list_to_tuple(EncodedProps)}.


decode_int({?NULL}) ->
    null;

decode_int({?FALSE}) ->
    false;

decode_int({?TRUE}) ->
    true;

decode_int({?STRING, String}) ->
    String;

decode_int({?NUMBER, Number}) ->
    case Number - trunc(Number) of
        0 -> trunc(Number); % convert to integer
        _ -> Number
    end;

decode_int({?LIST, List}) ->
    lists:map(fun decode_int/1, tuple_to_list(List));

decode_int({?OBJECT, Object}) ->
    Props = lists:map(fun({EncodedK, EncodedV}) ->
        K = decode_int(EncodedK),
        V = decode_int(EncodedV),
        {K, V}
    end, tuple_to_list(Object)),
    {Props}.
