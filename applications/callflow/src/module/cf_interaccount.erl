%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Barnaby Puttick
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_interaccount).

-behaviour(gen_cf_action).

-include_lib("callflow/src/callflow.hrl").

-export([handle/2, lookup_account/1]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successful.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
	Call1  = maybe_prepend_cid(Data, Call),
	cf_resources:handle(request_data(), Call1).


-spec request_data() -> kz_json:object().
request_data() ->
	kz_json:from_list(
      [{<<"use_local_resources">>, false}
      ,{<<"caller_id_type">>, <<"internal">>}
      ,{<<"force_interaccount">>, true}
      ]).

-spec maybe_prepend_cid(kz_json:object(), kapps_call:call()) -> kapps_call:call().
maybe_prepend_cid(Data, Call) ->
    NamePre = kz_json:get_ne_binary_value(<<"caller_id_name_prefix">>, Data, <<>>),
    NumberPre = kz_json:get_ne_binary_value(<<"caller_id_number_prefix">>, Data, <<>>),

    OrigPreName = kapps_call:kvs_fetch(<<"prepend_cid_name">>, <<>>, Call),
    OrigPreNum  = kapps_call:kvs_fetch(<<"prepend_cid_number">>, <<>>, Call),

    {Name, Number}
        = case kz_json:get_ne_binary_value(<<"apply_to">>, Data, <<"current">>) of
              <<"original">> ->
                  {NamePre, NumberPre};
              <<"current">> ->
                  {<<NamePre/binary, OrigPreName/binary>>
                  ,<<NumberPre/binary, OrigPreNum/binary>>
                  }
          end,
    lager:info("update prepend cid to <~s> ~s", [Name, Number]),
    Updates = [fun(C) -> kapps_call:kvs_store('prepend_cid_number', Number, C) end
              ,fun(C) -> kapps_call:kvs_store('prepend_cid_name', Name, C) end
              ],
    Call1 = kapps_call:exec(Updates, Call),
    cf_exe:set_call(Call1),
    Call1.

-spec lookup_account(kz_term:ne_binary()) -> any().
lookup_account(Number) ->
	case 
		kz_datamgr:get_single_result(<<"accounts">>, 
									 <<"accounts/listing_by_ia_prefix">>, 
									 [{'keys', build_keys(Number)}
									  ,'first_when_multiple'
									 ])
	of
		{'ok', JObj} ->
			Value = kz_json:get_value(<<"value">>, JObj),
			AccountId = kz_json:get_value(<<"account_id">>, Value),
			InterAccount = kz_json:get_value(<<"inter_account">>, Value),
			Matches = kz_json:get_ne_value(<<"matches">>, InterAccount, []),
			lager:debug("Found: ~p",[Matches]),
			MatchedNumber = maybe_match_number(Matches, Number, InterAccount),
			lager:debug("Processing number: ~s",[MatchedNumber]),
			{'ok', AccountId, set_number_props(AccountId, MatchedNumber)};
        {'error', _R}=E ->
            lager:warning("unable to lookup interaccount number ~s: ~p", [Number, _R]),
            E
    end.

-spec build_keys(kz_term:ne_binary()) -> [integer()].
build_keys(Number) ->
    case only_numeric(Number) of
        <<>> -> [];
        <<D:1/binary, Rest/binary>> ->
            build_keys(Rest, D, [kz_term:to_integer(D)])
    end.

-spec only_numeric(binary()) -> binary().
only_numeric(Number) ->
    << <<N>> || <<N>> <= Number, is_numeric(N)>>.

-spec is_numeric(integer()) -> boolean().
is_numeric(N) ->
    N >= $0
        andalso N =< $9.

-spec build_keys(binary(), kz_term:ne_binary(), [integer()]) -> [integer()].
build_keys(<<D:1/binary, Rest/binary>>, Prefix, Acc) ->
    build_keys(Rest, <<Prefix/binary, D/binary>>, [kz_term:to_integer(<<Prefix/binary, D/binary>>) | Acc]);
build_keys(<<>>, _, Acc) -> 
	lager:debug("Built search list: ~p", [Acc]),
	Acc.


-spec maybe_match_number(list(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
maybe_match_number([], Number, _) ->
    lager:debug("No inter-account matches found for ~s",[Number]),
	Number;
maybe_match_number([Regex|Regexes], Number, InterAccount) ->
	lager:debug("Found match regex: ~s for ~s", [Regex, Number]),
	case re:run(Number, Regex, [{'capture', 'all', 'binary'}]) of	
        'nomatch' ->
	        lager:debug("nomatch for ~s -> ~s", [Regex, Number]),
            maybe_match_number(Regexes, Number, InterAccount);
        'match' ->
	        lager:debug("match for ~s -> ~s", [Regex, Number]),
            Number;
        {'match', Captures} ->
	        lager:debug("capturematch for ~s -> ~s (~p)", [Regex, Number, Captures]),
            lists:last(Captures)
    end.



-spec set_number_props(kz_term:ne_binary(), kz_term:ne_binary()) -> any().
set_number_props(AccountId, Number) ->
	[{'pending_port', 'false'}
    ,{'local', 'false'}
    ,{'number', Number}
    ,{'account_id', AccountId}
    ,{'inbound_cnam', 'false'}
    ,{'force_outbound', 'false'}
    ].
