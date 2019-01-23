%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, Voxter Communications Inc
%%% @doc
%%% @author Ben Bradford
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_ledgers_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("crossbar/src/crossbar.hrl").

to_csv_test_() ->
    TestDir = code:lib_dir('crossbar', 'test'),

    NoDesc = load(TestDir, "ledger_no_description.json"),
    WithDesc = load(TestDir, "ledger_description.json"),

    NoDescCSV = iolist_to_binary(api_util:csv_body([NoDesc], 'true')),

    [HeadersRow, NoDescRow | _] = binary:split(NoDescCSV, <<"\n">>, ['global']),

    WithDescRow = iolist_to_binary(api_util:csv_body([WithDesc], 'false')),

    BothCSV = iolist_to_binary(api_util:csv_body([NoDesc, WithDesc], 'true')),

    ?debugFmt("~s~n~s~n~s~n~n", [HeadersRow, NoDescRow, WithDescRow]),

    ?debugFmt("~s~n~n", [BothCSV]),

    Ls = [{h, cell_count(HeadersRow)}, {n, cell_count(NoDescRow)}, {w, cell_count(WithDescRow)}],

    ?debugFmt("lengths: ~p~n", [Ls]),

    ?_assert(all_match(Ls)).

cell_count(Row) ->
    length(binary:split(Row, <<"\",\"">>, ['global'])).

all_match([{_, A}, {D, A} | T]) ->
    all_match([{D, A} | T]);
all_match([_]) -> 'true';
all_match([]) ->  'true';
all_match(_F) ->
    ?debugFmt("not matching ~p", [_F]),
    'false'.

load(Dir, File) ->
    {'ok', JSON} = file:read_file(filename:join([Dir, File])),
    JObj = kz_json:decode(JSON),
    kz_ledger:public_json(kz_ledger:from_json(JObj)).
