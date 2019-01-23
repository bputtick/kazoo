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

    NoDescCSV = api_util:csv_body(NoDesc, 'true'),

    WithDescCSV = api_util:csv_body(WithDesc, 'false'),

    ?debugFmt("~nno: ~p~n~s~n~nwi: ~p~n~s~n~n"
             ,[NoDescCSV, NoDescCSV
              ,WithDescCSV, WithDescCSV
              ]),
    ?_assert('false').


load(Dir, File) ->
    {'ok', JSON} = file:read_file(filename:join([Dir, File])),
    kz_json:decode(JSON).
