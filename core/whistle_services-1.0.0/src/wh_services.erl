%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_services).

-export([add_service_plan/2]).
-export([delete_service_plan/2]).
-export([service_plan_json/1]).
-export([public_json/1]).
-export([to_json/1]).

-export([empty/0]).
-export([allow_updates/1]).
-export([from_service_json/1]).
-export([reconcile/1, reconcile/2
         ,reconcile_only/1, reconcile_only/2
         ,save_as_dirty/1
        ]).
-export([fetch/1]).
-export([update/4]).
-export([save/1]).
-export([delete/1]).

-export([activation_charges/3]).
-export([commit_transactions/2]).
-export([select_bookkeeper/1]).
-export([check_bookkeeper/2]).
-export([set_billing_id/2]).
-export([get_billing_id/1]).
-export([find_reseller_id/1]).

-export([account_id/1]).
-export([is_dirty/1]).
-export([quantity/3]).
-export([update_quantity/3]).
-export([category_quantity/3]).
-export([cascade_quantity/3]).
-export([cascade_category_quantity/3]).
-export([reset_category/2]).
-export([get_service_module/1]).

-export([is_reseller/1]).
-export([get_reseller_id/1]).

-export([dry_run/1]).

-include("whistle_services.hrl").

-record(wh_services, {account_id :: api_binary()
                      ,billing_id :: api_binary()
                      ,current_billing_id :: api_binary()
                      ,new_billing_id = 'false' :: boolean()
                      ,dirty = 'false' :: boolean()
                      ,deleted = 'false' :: boolean()
                      ,status = <<"good_standing">> :: ne_binary()
                      ,jobj = wh_json:new() :: wh_json:object()
                      ,updates = wh_json:new() :: wh_json:object()
                      ,cascade_quantities = wh_json:new() :: wh_json:object()
                     }).

-define(QUANTITIES, <<"quantities">>).
-define(BASE_BACKOFF, 50).

-type services() :: #wh_services{}.
-type bookkeeper() :: 'wh_bookkeeper_braintree' | 'wh_bookkeeper_local'.
-export_type([services/0
              ,bookkeeper/0
             ]).

%%%===================================================================
%%% Operations
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec empty() -> services().
empty() ->
    #wh_services{}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec new(ne_binary()) -> services().
new(<<_/binary>> = AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    AccountJObj = get_account_definition(AccountDb, AccountId),

    Props = base_service_props(AccountId, AccountDb, AccountJObj),

    BillingId = props:get_value(<<"billing_id">>, Props),
    IsReseller = props:get_value(<<"pvt_reseller">>, Props),

    #wh_services{account_id=AccountId
                 ,jobj=wh_json:from_list(Props)
                 ,cascade_quantities=cascade_quantities(AccountId, IsReseller)
                 ,dirty='true'
                 ,billing_id=BillingId
                 ,current_billing_id=BillingId
                 ,deleted=wh_doc:is_soft_deleted(AccountJObj)
                }.

-spec base_service_props(ne_binary(), ne_binary(), wh_json:object()) ->
                                wh_proplist().
base_service_props(AccountId, AccountDb, AccountJObj) ->
    ResellerId = get_reseller_id(AccountId),
    PvtTree = kz_account:tree(AccountJObj),

    IsReseller = depreciated_is_reseller(AccountJObj),
    BillingId = depreciated_billing_id(AccountJObj),

    Now = wh_util:current_tstamp(),

    [{<<"_id">>, AccountId}
     ,{<<"pvt_created">>, Now}
     ,{<<"pvt_modified">>, Now}
     ,{<<"pvt_type">>, <<"service">>}
     ,{<<"pvt_vsn">>, <<"1">>}
     ,{<<"pvt_account_id">>, AccountId}
     ,{<<"pvt_account_db">>, AccountDb}
     ,{<<"pvt_status">>, <<"good_standing">>}
     ,{<<"pvt_reseller">>, IsReseller}
     ,{<<"pvt_reseller_id">>, ResellerId}
     ,{<<"pvt_tree">>, PvtTree}
     ,{?QUANTITIES, wh_json:new()}
     ,{<<"billing_id">>, BillingId}
     ,{<<"plans">>, populate_service_plans(AccountJObj, ResellerId)}
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec from_service_json(wh_json:object()) -> services().
from_service_json(JObj) ->
    AccountId = wh_json:get_value(<<"pvt_account_id">>, JObj),
    IsReseller = wh_json:is_true(<<"pvt_reseller">>, JObj),
    BillingId = wh_json:get_value(<<"billing_id">>, JObj, AccountId),

    #wh_services{account_id=AccountId
                 ,jobj=JObj
                 ,cascade_quantities=cascade_quantities(AccountId, IsReseller)
                 ,status=wh_json:get_ne_value(<<"pvt_status">>, JObj, <<"good_standing">>)
                 ,billing_id=BillingId
                 ,current_billing_id=BillingId
                 ,deleted=wh_doc:is_soft_deleted(JObj)
                }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch(ne_binary()) -> services().
fetch(<<_/binary>> = Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    %% TODO: if reseller populate cascade via merchant id
    case couch_mgr:open_cache_doc(?WH_SERVICES_DB, AccountId) of
        {'ok', JObj} ->
            handle_fetch_result(AccountId, JObj);
        {'error', _R} ->
            lager:debug("unable to open account ~s services doc (creating new): ~p", [Account, _R]),
            new(AccountId)
    end.

-spec handle_fetch_result(ne_binary(), wh_json:object()) -> services().
handle_fetch_result(AccountId, JObj) ->
    lager:debug("loaded account service doc ~s", [AccountId]),
    IsReseller = wh_json:is_true(<<"pvt_reseller">>, JObj),
    BillingId = wh_json:get_value(<<"billing_id">>, JObj, AccountId),
    #wh_services{account_id=AccountId
                 ,jobj=JObj
                 ,cascade_quantities=cascade_quantities(AccountId, IsReseller)
                 ,status=wh_json:get_ne_value(<<"pvt_status">>, JObj, <<"good_standing">>)
                 ,billing_id=BillingId
                 ,current_billing_id=BillingId
                 ,deleted=wh_doc:is_soft_deleted(JObj)
                 ,dirty=wh_json:is_true(<<"pvt_dirty">>, JObj)
                }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec add_service_plan(ne_binary(), services()) -> services().
add_service_plan(PlanId, #wh_services{jobj=JObj}=Services) ->
    ResellerId = wh_json:get_value(<<"pvt_reseller_id">>, JObj),
    Services#wh_services{jobj=wh_service_plans:add_service_plan(PlanId, ResellerId, JObj)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_service_plan(ne_binary(), services()) -> services().
delete_service_plan(PlanId, #wh_services{jobj=JObj}=Services) ->
    Services#wh_services{jobj=wh_service_plans:delete_service_plan(PlanId, JObj)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec save_as_dirty(ne_binary() | services()) -> services().
-spec save_as_dirty(ne_binary() | services(), pos_integer()) -> services().
save_as_dirty(<<_/binary>> = Account) ->
    save_as_dirty(fetch(Account));
save_as_dirty(#wh_services{}=Services) ->
    save_as_dirty(Services, ?BASE_BACKOFF).

save_as_dirty(#wh_services{jobj=JObj
                           ,account_id = <<_/binary>> = AccountId
                          }=Services
              ,BackOff
             ) ->
    Props = [{<<"_id">>, AccountId}
             ,{<<"pvt_dirty">>, 'true'}
             ,{<<"pvt_modified">>, wh_util:current_tstamp()}
            ],
    UpdatedJObj = wh_json:set_values(props:filter_undefined(Props), JObj),
    case couch_mgr:save_doc(?WH_SERVICES_DB, UpdatedJObj) of
        {'ok', SavedJObj} ->
            lager:debug("marked services as dirty for account ~s", [AccountId]),
            Services#wh_services{jobj=JObj
                                 ,status=wh_json:get_ne_value(<<"pvt_status">>, SavedJObj, <<"good_standing">>)
                                 ,deleted=wh_doc:is_soft_deleted(SavedJObj)
                                 ,dirty='true'
                                };
        {'error', 'not_found'} ->
            lager:debug("service database does not exist, attempting to create"),
            'true' = couch_mgr:db_create(?WH_SERVICES_DB),
            timer:sleep(BackOff),
            save_as_dirty(Services, BackOff);
        {'error', 'conflict'} ->
            save_conflicting_as_dirty(Services, BackOff)
    end.

-spec save_conflicting_as_dirty(services(), pos_integer()) -> services().
save_conflicting_as_dirty(#wh_services{account_id=AccountId}, BackOff) ->
    {'ok', Existing} = couch_mgr:open_doc(?WH_SERVICES_DB, AccountId),
    NewServices = from_service_json(Existing),

    case is_dirty(NewServices) of
        'true' ->
            lager:debug("services doc for ~s saved elsewhere", [AccountId]),
            NewServices;
        'false' ->
            lager:debug("new services doc for ~s not dirty, marking it as so", [AccountId]),
            timer:sleep(BackOff + random:uniform(?BASE_BACKOFF)),
            save_as_dirty(NewServices, BackOff*2)
    end.

-spec save(services()) -> services().
-spec save(services(), pos_integer()) -> services().
save(#wh_services{}=Services) ->
    save(Services, ?BASE_BACKOFF).

save(#wh_services{jobj=JObj
                  ,updates=UpdatedQuantities
                  ,account_id=AccountId
                  ,dirty=ForceDirty
                 }=Services
     ,BackOff
    ) ->
    CurrentQuantities = wh_json:get_value(?QUANTITIES, JObj, wh_json:new()),
    Dirty = have_quantities_changed(UpdatedQuantities, CurrentQuantities) orelse ForceDirty,
    Props = [{<<"_id">>, AccountId}
             ,{<<"pvt_dirty">>, Dirty}
             ,{<<"pvt_modified">>, wh_util:current_tstamp()}
             ,{?QUANTITIES, wh_json:merge_jobjs(UpdatedQuantities, CurrentQuantities)}
            ],
    UpdatedJObj = wh_json:set_values(props:filter_undefined(Props), JObj),
    case couch_mgr:save_doc(?WH_SERVICES_DB, UpdatedJObj) of
        {'ok', NewJObj} ->
            lager:debug("saved services for ~s", [AccountId]),
            IsReseller = wh_json:is_true(<<"pvt_reseller">>, JObj),
            _ = maybe_clean_old_billing_id(Services),
            BillingId = wh_json:get_value(<<"billing_id">>, NewJObj, AccountId),
            Services#wh_services{jobj=NewJObj
                                 ,cascade_quantities=cascade_quantities(AccountId, IsReseller)
                                 ,status=wh_json:get_ne_value(<<"pvt_status">>, NewJObj, <<"good_stainding">>)
                                 ,billing_id=BillingId
                                 ,current_billing_id=BillingId
                                 ,deleted=wh_doc:is_soft_deleted(NewJObj)
                                };
        {'error', 'not_found'} ->
            lager:debug("service database does not exist, attempting to create"),
            'true' = couch_mgr:db_create(?WH_SERVICES_DB),
            timer:sleep(BackOff),
            save(Services, BackOff);
        {'error', 'conflict'} ->
            lager:debug("services for ~s conflicted, merging changes and retrying", [AccountId]),
            timer:sleep(BackOff + random:uniform(?BASE_BACKOFF)),
            {'ok', Existing} = couch_mgr:open_doc(?WH_SERVICES_DB, AccountId),
            save(Services#wh_services{jobj=Existing}, BackOff*2)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec delete(ne_binary()) -> wh_std_return().
delete(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    %% TODO: support other bookkeepers, and just cancel subscriptions....
    _ = (catch braintree_customer:delete(AccountId)),
    case couch_mgr:open_doc(?WH_SERVICES_DB, AccountId) of
        {'ok', JObj} ->
            lager:debug("marking services for account ~s as deleted", [AccountId]),
            couch_mgr:save_doc(?WH_SERVICES_DB, wh_json:set_values([{<<"pvt_deleted">>, 'true'}
                                                                    ,{<<"pvt_dirty">>, 'true'}
                                                                   ]
                                                                   ,JObj
                                                                  ));
        {'error', 'not_found'} -> {'ok', wh_json:new()};
        {'error', _R}=E ->
            lager:debug("unable to mark service plan ~s as deleted: ~p", [AccountId, _R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_billing_id(api_binary(), ne_binary() | services()) -> 'undefined' | services().
set_billing_id('undefined', _) -> 'undefined';
set_billing_id(BillingId, #wh_services{billing_id=BillingId}) ->
    'undefined';
set_billing_id(BillingId, #wh_services{account_id=BillingId
                                       ,jobj=ServicesJObj
                                      }=Services) ->
    Services#wh_services{jobj=wh_json:set_value(<<"billing_id">>, BillingId, ServicesJObj)
                         ,billing_id=BillingId
                         ,dirty='true'
                        };
set_billing_id(BillingId, #wh_services{jobj=ServicesJObj}=Services) ->
    PvtTree = wh_json:get_value(<<"pvt_tree">>, ServicesJObj, [BillingId]),
    try lists:last(PvtTree) of
        BillingId ->
            Services#wh_services{jobj=wh_json:set_value(<<"billing_id">>, BillingId, ServicesJObj)
                                 ,billing_id=BillingId
                                 ,dirty='true'
                                };
        _Else ->
            throw({'invalid_billing_id', <<"Requested billing id is not the parent of this account">>})
    catch
        {'EXIT', _} ->
            throw({'invalid_billing_id', <<"Unable to determine if billing id is valid">>})
    end;
set_billing_id(BillingId, <<_/binary>> = AccountId) ->
    set_billing_id(BillingId, fetch(AccountId)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_billing_id(ne_binary()) -> ne_binary().
get_billing_id(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    lager:debug("determining if account ~s is able to make updates", [AccountId]),
    case couch_mgr:open_cache_doc(?WH_SERVICES_DB, AccountId) of
        {'error', _R} ->
            lager:debug("unable to open account ~s services: ~p", [AccountId, _R]),
            AccountId;
        {'ok', ServicesJObj} ->
            case wh_json:get_ne_value(<<"billing_id">>, ServicesJObj, AccountId) of
                AccountId -> AccountId;
                BillingId ->
                    lager:debug("following billing id ~s", [BillingId]),
                    get_billing_id(BillingId)
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), ne_binary(), integer(), services()) -> services().
update(Category, Item, Quantity, Services) when not is_integer(Quantity) ->
    update(Category, Item, wh_util:to_integer(Quantity), Services);
update(Category, Item, Quantity, #wh_services{updates=JObj}=Services) when is_binary(Category), is_binary(Item) ->
    Services#wh_services{updates=wh_json:set_value([Category, Item], Quantity, JObj)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec activation_charges(ne_binary(), ne_binary(), services() | ne_binary()) -> number().
activation_charges(Category, Item, #wh_services{jobj=ServicesJObj}) ->
    Plans = wh_service_plans:from_service_json(ServicesJObj),
    wh_service_plans:activation_charges(Category, Item, Plans);
activation_charges(Category, Item, <<_/binary>> = Account) ->
    activation_charges(Category, Item, fetch(Account)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec commit_transactions(services(), wh_transactions:wh_transactions()) -> atom().
commit_transactions(#wh_services{billing_id=BillingId}, Activations) ->
    Bookkeeper = select_bookkeeper(BillingId),
    Transactions = [Activation
                    || Activation <- Activations
                           ,wh_transaction:amount(Activation) > 0
                   ],
    Bookkeeper:commit_transactions(BillingId, Transactions).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec select_bookkeeper(ne_binary()) -> bookkeeper().
select_bookkeeper(BillingId) ->
    ResellerId = get_reseller_id(BillingId),
    {'ok', MasterAccountId} = whapps_util:get_master_account_id(),
    case ResellerId =/= MasterAccountId of
        'true' -> 'wh_bookkeeper_local';
        'false' ->
            whapps_config:get_atom(?WHS_CONFIG_CAT, <<"master_account_bookkeeper">>, 'wh_bookkeeper_local')
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec check_bookkeeper(ne_binary(), integer()) -> boolean().
check_bookkeeper(BillingId, Amount) ->
    Bookkeeper = select_bookkeeper(BillingId),
    case Bookkeeper of
        'wh_bookkeeper_local' ->
            Balance = wht_util:current_balance(BillingId),
            Balance - Amount =< 0;
        _Else -> Bookkeeper:is_good_standing(BillingId)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec service_plan_json(ne_binary() | services()) -> wh_json:object().
service_plan_json(#wh_services{jobj=ServicesJObj}) ->
    Plans = wh_service_plans:from_service_json(ServicesJObj),
    wh_service_plans:public_json(Plans);
service_plan_json(<<_/binary>> = Account) ->
    service_plan_json(fetch(Account)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec public_json(ne_binary() | services()) -> wh_json:object().
public_json(#wh_services{jobj=ServicesJObj
                         ,cascade_quantities=CascadeQuantities
                        }) ->
    AccountId = wh_json:get_value(<<"pvt_account_id">>, ServicesJObj),
    InGoodStanding = try maybe_follow_billling_id(AccountId, ServicesJObj) of
                         'true' -> 'true'
                     catch
                         'throw':_ -> 'false'
                     end,
    Props = [{<<"account_quantities">>, wh_json:get_value(?QUANTITIES, ServicesJObj, wh_json:new())}
             ,{<<"cascade_quantities">>, CascadeQuantities}
             ,{<<"plans">>, wh_service_plans:plan_summary(ServicesJObj)}
             ,{<<"billing_id">>, wh_json:get_value(<<"billing_id">>, ServicesJObj, AccountId)}
             ,{<<"reseller">>, wh_json:is_true(<<"pvt_reseller">>, ServicesJObj)}
             ,{<<"reseller_id">>, wh_json:get_ne_value(<<"pvt_reseller_id">>, ServicesJObj)}
             ,{<<"dirty">>, wh_json:is_true(<<"pvt_dirty">>, ServicesJObj)}
             ,{<<"in_good_standing">>, InGoodStanding}
             ,{<<"items">>, wh_service_plans:public_json_items(ServicesJObj)}
            ],
    wh_json:from_list(Props);
public_json(<<_/binary>> = Account) ->
    public_json(fetch(Account)).

-spec to_json(services()) -> wh_json:object().
to_json(#wh_services{jobj=JObj
                    ,updates=UpdatedQuantities
                    }
       ) ->
    CurrentQuantities = wh_json:get_value(?QUANTITIES, JObj, wh_json:new()),
    Props = [{?QUANTITIES, wh_json:merge_jobjs(UpdatedQuantities, CurrentQuantities)}],
    wh_json:set_values(props:filter_undefined(Props), JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_reseller_id(api_binary()) -> api_binary().
find_reseller_id('undefined') ->
    case whapps_util:get_master_account_id() of
        {'error', _} -> 'undefined';
        {'ok', MasterAccountId} -> MasterAccountId
    end;
find_reseller_id(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    case couch_mgr:open_cache_doc(?WH_SERVICES_DB, AccountId) of
        {'ok', JObj} ->
            case wh_json:get_ne_value(<<"pvt_reseller_id">>, JObj) of
                'undefined' -> get_reseller_id(Account);
                ResellerId -> ResellerId
            end;
        {'error', _} -> get_reseller_id(Account)
    end.

%%%===================================================================
%%% Services functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Throws an error if the billing account is not in "good_standing",
%% used when update requests are made to kill them if there are
%% accounting issues.
%% @end
%%--------------------------------------------------------------------
-spec allow_updates(ne_binary()) -> 'true'.
allow_updates(<<_/binary>> = Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    lager:debug("determining if account ~s is able to make updates", [AccountId]),
    case couch_mgr:open_cache_doc(?WH_SERVICES_DB, AccountId) of
        {'error', _R} ->
            lager:debug("unable to open account ~s services: ~p", [AccountId, _R]),
            default_maybe_allow_updates(AccountId);
        {'ok', ServicesJObj} ->
            maybe_follow_billling_id(AccountId, ServicesJObj)
    end.

-spec maybe_follow_billling_id(ne_binary(), wh_json:object()) -> 'true'.
maybe_follow_billling_id(AccountId, ServicesJObj) ->
    case wh_json:get_ne_value(<<"billing_id">>, ServicesJObj, AccountId) of
        AccountId -> maybe_allow_updates(AccountId, ServicesJObj);
        BillingId ->
            lager:debug("following billing id ~s", [BillingId]),
            allow_updates(BillingId)
    end.

-spec maybe_allow_updates(ne_binary(), wh_json:object()) -> 'true'.
maybe_allow_updates(AccountId, ServicesJObj) ->
    Plans = wh_service_plans:plan_summary(ServicesJObj),
    case wh_util:is_empty(Plans)
        orelse wh_json:get_value(<<"pvt_status">>, ServicesJObj)
    of
        'true' ->
            lager:debug("allowing request for account with no service plans"),
            'true';
        <<"good_standing">> ->
            lager:debug("allowing request for account in good standing"),
            'true';
        Status ->
            maybe_local_bookkeeper_allow_updates(AccountId, Status)
    end.

-spec maybe_local_bookkeeper_allow_updates(ne_binary(), ne_binary()) -> 'true'.
maybe_local_bookkeeper_allow_updates(AccountId, Status) ->
    case select_bookkeeper(AccountId) of
        'wh_bookkeeper_local' -> spawn_move_to_good_standing(AccountId);
        Bookkeeper -> maybe_bookkeeper_allow_updates(Bookkeeper, AccountId, Status)
    end.

-spec maybe_bookkeeper_allow_updates(atom(), ne_binary(), ne_binary()) -> 'true'.
maybe_bookkeeper_allow_updates(Bookkeeper, AccountId, Status) ->
    case Bookkeeper:is_good_standing(AccountId) of
        'true' -> spawn_move_to_good_standing(AccountId);
        'false' ->
            lager:debug("denying update request for services ~s due to status ~s", [AccountId, Status]),
            Error = io_lib:format("Unable to continue due to billing account ~s status", [AccountId]),
            throw({Status, wh_util:to_binary(Error)})
    end.

-spec default_maybe_allow_updates(ne_binary()) -> 'true'.
default_maybe_allow_updates(AccountId) ->
    case whapps_config:get_is_true(?WHS_CONFIG_CAT, <<"default_allow_updates">>, 'true') of
        'true' -> 'true';
        'false' ->
            lager:debug("denying update request, ~s.default_allow_updates is false", [?WHS_CONFIG_CAT]),
            Error = io_lib:format("Service updates are disallowed by default for billing account ~s", [AccountId]),
            throw({<<"updates_disallowed">>, wh_util:to_binary(Error)})
    end.

-spec spawn_move_to_good_standing(ne_binary()) -> 'true'.
spawn_move_to_good_standing(<<_/binary>> = AccountId) ->
    spawn(fun() -> move_to_good_standing(AccountId) end),
    'true'.

-spec move_to_good_standing(ne_binary()) -> services().
move_to_good_standing(<<_/binary>> = AccountId) ->
    #wh_services{jobj=JObj}=Services = fetch(AccountId),
    save(Services#wh_services{jobj=wh_json:set_value(<<"pvt_status">>, <<"good_standing">>, JObj)}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile_only(api_binary()) -> 'false' | services().
-spec reconcile(api_binary()) -> 'false' | services().

reconcile_only('undefined') -> 'false';
reconcile_only(<<_/binary>> = Account) ->
    lager:debug("reconcile all services for ~s", [Account]),
    lists:foldl(fun reconcile_module/2
                ,fetch(Account)
                ,get_service_modules()
               ).

-spec reconcile_module(atom(), services()) -> services().
reconcile_module(M, Services) ->
    M:reconcile(Services).

reconcile('undefined') -> 'false';
reconcile(<<_/binary>> = Account) ->
    save(reconcile_only(Account)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile_only(api_binary(), text()) -> 'false' | services().
reconcile_only('undefined', _Module) -> 'false';
reconcile_only(<<_/binary>> = Account, Module) ->
    lager:debug("reconcile ~s services for ~s", [Module, Account]),
    case get_service_module(Module) of
        'false' -> 'false';
        ServiceModule ->
            CurrentServices = fetch(Account),
            ServiceModule:reconcile(CurrentServices)
    end.

-spec reconcile(api_binary(), text()) -> 'false' | services().
reconcile('undefined', _Module) -> 'false';
reconcile(<<_/binary>> = Account, Module) ->
    timer:sleep(1000),
    maybe_save(reconcile_only(Account, Module)).

%%%===================================================================
%%% Access functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec account_id(services()) -> ne_binary().
account_id(#wh_services{account_id=AccountId}) ->
    AccountId.

-spec is_dirty(services()) -> boolean().
is_dirty(#wh_services{dirty=IsDirty}) ->
    wh_util:is_true(IsDirty).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec quantity(ne_binary(), ne_binary(), services()) -> integer().
quantity(_, _, #wh_services{deleted='true'}) -> 0;
quantity(Category, Item, #wh_services{updates=UpdatedQuantities
                                      ,jobj=JObj
                                     }) ->
    CurrentQuantities = wh_json:get_value(?QUANTITIES, JObj, wh_json:new()),
    Quantities = wh_json:merge_jobjs(UpdatedQuantities, CurrentQuantities),
    wh_json:get_integer_value([Category, Item], Quantities, 0).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_quantity(ne_binary(), ne_binary(), services()) -> integer().
update_quantity(_, _, #wh_services{deleted='true'}) -> 0;
update_quantity(Category, Item, #wh_services{updates=JObj}) ->
    wh_json:get_integer_value([Category, Item], JObj, 0).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec category_quantity(ne_binary(), ne_binaries(), services()) -> integer().
category_quantity(_, _, #wh_services{deleted='true'}) -> 0;
category_quantity(Category, Exceptions, #wh_services{updates=UpdatedQuantities
                                                     ,jobj=JObj
                                                    }) ->
    CurrentQuantities = wh_json:get_value(?QUANTITIES, JObj, wh_json:new()),
    Quantities = wh_json:merge_jobjs(UpdatedQuantities, CurrentQuantities),
    lists:foldl(fun(Item, Sum) ->
                        case lists:member(Item, Exceptions) of
                            'true' -> Sum;
                            'false' ->
                                wh_json:get_integer_value([Category, Item], Quantities, 0) + Sum
                        end
                end, 0, wh_json:get_keys(Category, Quantities)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cascade_quantity(ne_binary(), ne_binary(), services()) -> integer().
cascade_quantity(_, _, #wh_services{deleted='true'}) -> 0;
cascade_quantity(Category, Item, #wh_services{cascade_quantities=JObj}=Services) ->
    wh_json:get_integer_value([Category, Item], JObj, 0)
        + quantity(Category, Item, Services).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cascade_category_quantity(ne_binary(), ne_binaries(), services()) -> integer().
cascade_category_quantity(_, _, #wh_services{deleted='true'}) -> 0;
cascade_category_quantity(Category, Exceptions, #wh_services{cascade_quantities=Quantities}=Services) ->
    lists:foldl(fun(Item, Sum) ->
                        case lists:member(Item, Exceptions) of
                            'true' -> Sum;
                            'false' ->
                                wh_json:get_integer_value([Category, Item], Quantities, 0) + Sum
                        end
                end, category_quantity(Category, Exceptions, Services)
                ,wh_json:get_keys(Category, Quantities)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reset_category(ne_binary(), services()) -> services().
reset_category(Category, #wh_services{updates=JObj}=Services) ->
    Services#wh_services{updates=wh_json:set_value(Category, wh_json:new(), JObj)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% Helper function to know if an account is a reseller or not.
%% @end
%%--------------------------------------------------------------------
-spec is_reseller(ne_binary() | services()) -> boolean().
is_reseller(#wh_services{jobj=ServicesJObj}) ->
    wh_json:is_true(<<"pvt_reseller">>, ServicesJObj);
is_reseller(<<_/binary>> = Account) ->
    is_reseller(fetch(Account)).

%%--------------------------------------------------------------------
%% @public
%% @doc

%% @end
%%--------------------------------------------------------------------
-spec dry_run(services()) -> wh_json:object().
dry_run(Services) ->
    ActivationsCharges = dry_run_activation_charges(Services),
    calculate_charges(Services, ActivationsCharges).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_charges(services(), wh_json:objects()) -> wh_json:object().
calculate_charges(Services, JObjs) ->
    case calculate_services_charges(Services) of
        {'error', _} -> wh_json:new();
        {'no_plan', _} -> wh_json:new();
        {'ok', PlansCharges} ->
            calculate_transactions_charges(PlansCharges, JObjs)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec calculate_services_charges(services()) ->
                                        {'no_plan' | 'error' | 'ok', wh_json:object()}.
-spec calculate_services_charges(services(), wh_service_plans:plans()) ->
                                        {'error' | 'ok', wh_json:object()}.
calculate_services_charges(#wh_services{jobj=ServiceJObj}=Services) ->
    case wh_service_plans:from_service_json(ServiceJObj) of
        [] -> {'no_plan', wh_json:new()};
        ServicePlans ->
            calculate_services_charges(Services, ServicePlans)
    end.

calculate_services_charges(#wh_services{jobj=ServiceJObj}=Services, ServicePlans) ->
    case wh_service_plans:create_items(ServiceJObj) of
        {'error', _} -> {'error', wh_json:new()};
        {'ok', ServiceItems} ->
            PlanItems = wh_service_plans:create_items(Services, ServicePlans),
            Changed = wh_service_items:get_updated_items(PlanItems, ServiceItems),
            {'ok', wh_service_items:public_json(Changed)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec calculate_transactions_charges(wh_json:object(), wh_json:objects()) -> wh_json:object().
calculate_transactions_charges(PlansCharges, JObjs) ->
    lists:foldl(
      fun calculate_transactions_charge_fold/2
      ,PlansCharges
      ,JObjs
     ).

-spec calculate_transactions_charge_fold(wh_json:object(), wh_json:object()) -> wh_json:object().
calculate_transactions_charge_fold(JObj, Acc) ->
    Amount = wh_json:get_value(<<"amount">>, JObj, 0),
    Quantity = wh_json:get_value(<<"quantity">>, JObj, 0),
    SubTotal = wh_json:get_value(<<"activation_charges">>, Acc, 0),
    case SubTotal + (Amount*Quantity) of
        Zero when Zero == 0 ->
            %% Works for 0.0 and 0. May compare to a threshold though…
            Acc;
        Total ->
            Category = wh_json:get_value(<<"category">>, JObj),
            Item = wh_json:get_value(<<"item">>, JObj),
            Props = [{<<"activation_charges">>, Total}
                     ,{[Category, Item, <<"activation_charges">>], Amount}
                    ],
            wh_json:set_values(Props, Acc)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec dry_run_activation_charges(services()) -> wh_json:objects().
-spec dry_run_activation_charges(ne_binary(), wh_json:object()
                                 ,services(), wh_json:objects()
                                ) -> wh_json:objects().
-spec dry_run_activation_charges(ne_binary(), ne_binary()
                                 ,wh_json:object(), services()
                                 ,wh_json:objects()
                                ) -> wh_json:objects().
dry_run_activation_charges(#wh_services{updates=Updates}=Services) ->
    wh_json:foldl(
      fun(Category, CategoryJObj, Acc) ->
              dry_run_activation_charges(Category, CategoryJObj, Services, Acc)
      end
      ,[]
      ,Updates
     ).

dry_run_activation_charges(Category, CategoryJObj, Services, JObjs) ->
    wh_json:foldl(
      fun(Item, Quantity, Acc1) ->
              dry_run_activation_charges(Category, Item, Quantity, Services, Acc1)
      end
      ,JObjs
      ,CategoryJObj
     ).

dry_run_activation_charges(Category, Item, Quantity, #wh_services{jobj=JObj}=Services, JObjs) ->
    case wh_json:get_value([?QUANTITIES, Category, Item], JObj, 0) of
        Quantity -> JObjs;
        OldQuantity ->
            ServicesJObj = wh_services:to_json(Services),
            Plans = wh_service_plans:from_service_json(ServicesJObj),
            ServicePlan = wh_service_plans:public_json(Plans),
            ItemPlan = wh_json:get_first_defined([[Category, Item], [Category, <<"_all">>]], ServicePlan),
            As = wh_json:get_ne_value(<<"as">>, ItemPlan, Item),

            [wh_json:from_list(
               [{<<"category">>, Category}
                ,{<<"item">>, As}
                ,{<<"amount">>, activation_charges(Category, Item, Services)}
                ,{<<"quantity">>, Quantity-OldQuantity}
               ])
             |JObjs
            ]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_service_modules() -> atoms().
get_service_modules() ->
    case whapps_config:get(?WHS_CONFIG_CAT, <<"modules">>) of
        'undefined' ->
            get_filesystem_service_modules();
        Modules ->
            lager:debug("configured service modules: ~p", [Modules]),
            [Module || M <- Modules,
                       (Module = wh_util:try_load_module(M)) =/= 'false'
            ]
    end.

-spec get_filesystem_service_modules() -> atoms().
get_filesystem_service_modules() ->
    Mods = [Mod
            || P <- filelib:wildcard([code:lib_dir('whistle_services'), "/src/services/*.erl"]),
               begin
                   Name = wh_util:to_binary(filename:rootname(filename:basename(P))),
                   (Mod = wh_util:try_load_module(Name)) =/= 'false'
               end
           ],
    lager:debug("found filesystem service modules: ~p", [Mods]),
    Mods.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_service_module(text()) -> atom() | 'false'.
get_service_module(Module) when not is_binary(Module) ->
    get_service_module(wh_util:to_binary(Module));
get_service_module(<<"wh_service_", _/binary>> = Module) ->
    ServiceModules = get_service_modules(),
    case [M
          || M <- ServiceModules,
             wh_util:to_binary(M) =:= Module
         ]
    of
        [M] -> M;
        _Else -> 'false'
    end;
get_service_module(Module) ->
    get_service_module(<<"wh_service_", Module/binary>>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cascade_quantities(ne_binary(), boolean()) -> wh_json:object().

cascade_quantities(<<_/binary>> = Account, 'false') ->
    do_cascade_quantities(Account, <<"services/cascade_quantities">>);
cascade_quantities(<<_/binary>> = Account, 'true') ->
    do_cascade_quantities(Account, <<"services/reseller_quantities">>).

-spec do_cascade_quantities(ne_binary(), ne_binary()) -> wh_json:object().
do_cascade_quantities(<<_/binary>> = Account, <<_/binary>> = View) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    ViewOptions = ['group'
                   ,'reduce'
                   ,{'startkey', [AccountId]}
                   ,{'endkey', [AccountId, wh_json:new()]}
                  ],
    case couch_mgr:get_results(?WH_SERVICES_DB, View, ViewOptions) of
        {'error', _} -> wh_json:new();
        {'ok', JObjs} ->
            lists:foldl(fun do_cascade_quantities_fold/2, wh_json:new(), JObjs)
    end.

-spec do_cascade_quantities_fold(wh_json:object(), wh_json:object()) ->
                                        wh_json:object().
-spec do_cascade_quantities_fold(wh_json:object(), wh_json:object(), wh_json:key()) ->
                                        wh_json:object().
do_cascade_quantities_fold(JObj, J) ->
    do_cascade_quantities_fold(JObj, J, wh_json:get_value(<<"key">>, JObj)).

do_cascade_quantities_fold(JObj, J, [_|Keys]) ->
    Value = wh_json:get_integer_value(<<"value">>, JObj),
    wh_json:set_value(Keys, Value, J).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determine the billing id as it is currently set on the account
%% definition as this will be depreciated in the future.
%% @end
%%--------------------------------------------------------------------
-spec depreciated_billing_id(wh_json:object()) -> ne_binary().
depreciated_billing_id(JObj) ->
    AccountId = wh_json:get_value(<<"pvt_account_id">>, JObj),
    wh_json:get_value(<<"billing_id">>, JObj, AccountId).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determine if pvt_reseller is currently set on the account
%% definition as this will be depreciated in the future.
%% @end
%%--------------------------------------------------------------------
-spec depreciated_is_reseller(wh_json:object()) -> boolean().
depreciated_is_reseller(JObj) ->
    wh_json:is_true(<<"pvt_reseller">>, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determine what service plans are currently set on the account
%% definition as this will be depreciated in the future.
%% @end
%%--------------------------------------------------------------------
-spec populate_service_plans(wh_json:object(), ne_binary()) -> wh_json:object().
populate_service_plans(JObj, ResellerId) ->
    Plans = incorporate_default_service_plan(ResellerId, master_default_service_plan()),
    incorporate_depreciated_service_plans(Plans, JObj).

-spec default_service_plan_id(ne_binary()) -> api_binary().
default_service_plan_id(ResellerId) ->
    case couch_mgr:open_doc(?WH_SERVICES_DB, ResellerId) of
        {'ok', JObj} -> wh_json:get_value(<<"default_service_plan">>, JObj);
        {'error', _R} ->
            lager:debug("unable to open reseller ~s services: ~p", [ResellerId, _R]),
            'undefined'
    end.

-spec depreciated_default_service_plan_id(ne_binary()) -> api_binary().
depreciated_default_service_plan_id(ResellerId) ->
    ResellerDb = wh_util:format_account_id(ResellerId, 'encoded'),
    case couch_mgr:open_doc(ResellerDb, ResellerId) of
        {'ok', JObj} -> wh_json:get_value(<<"default_service_plan">>, JObj);
        {'error', _R} ->
            lager:debug("unable to open reseller ~s account definition: ~p", [ResellerId, _R]),
            'undefined'
    end.

-spec master_default_service_plan() -> wh_json:object().
master_default_service_plan() ->
    case whapps_util:get_master_account_id() of
        {'error', _} -> wh_json:new();
        {'ok', MasterAccountId} ->
            incorporate_default_service_plan(MasterAccountId, wh_json:new())
    end.

-spec incorporate_default_service_plan(ne_binary(), wh_json:object()) -> wh_json:object().
incorporate_default_service_plan(ResellerId, JObj) ->
    case depreciated_default_service_plan_id(ResellerId) of
        'undefined' ->
            incorporate_only_default_service_plan(ResellerId, JObj);
        PlanId ->
            maybe_augment_with_plan(ResellerId, JObj, PlanId)
    end.

-spec incorporate_only_default_service_plan(ne_binary(), wh_json:object()) -> wh_json:object().
incorporate_only_default_service_plan(ResellerId, JObj) ->
    maybe_augment_with_plan(ResellerId, JObj, default_service_plan_id(ResellerId)).

maybe_augment_with_plan(_ResellerId, JObj, 'undefined') -> JObj;
maybe_augment_with_plan(ResellerId, JObj, PlanId) ->
    Plan = wh_json:from_list([{<<"account_id">>, ResellerId}]),
    wh_json:set_value(PlanId, Plan, JObj).

-spec incorporate_depreciated_service_plans(wh_json:object(), wh_json:object()) -> wh_json:object().
incorporate_depreciated_service_plans(Plans, JObj) ->
    PlanIds = wh_json:get_value(<<"pvt_service_plans">>, JObj),
    ResellerId = wh_json:get_value(<<"pvt_reseller_id">>, JObj),
    case wh_util:is_empty(PlanIds)
        orelse wh_util:is_empty(ResellerId)
    of
        'true' -> Plans;
        'false' ->
            lists:foldl(fun(PlanId, Ps) ->
                                maybe_augment_with_plan(ResellerId, Ps, PlanId)
                        end
                        ,Plans
                        ,PlanIds
                       )
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_reseller_id(ne_binaries() | ne_binary()) -> ne_binary().
get_reseller_id([]) ->
    {'ok', MasterAccountId} = whapps_util:get_master_account_id(),
    MasterAccountId;
get_reseller_id([Parent|Ancestors]) ->
    case couch_mgr:open_cache_doc(?WH_SERVICES_DB, Parent) of
        {'error', _R} ->
            lager:debug("failed to open services doc ~s durning reseller search: ~p", [Parent, _R]),
            get_reseller_id(Ancestors);
        {'ok', JObj} ->
            get_reseller_id(Parent, Ancestors, JObj)
    end;
get_reseller_id(<<_/binary>> = Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'ok', AccountJObj} ->
            get_reseller_id(lists:reverse(kz_account:tree(AccountJObj)));
        {'error', _R} ->
            lager:info("unable to open account definition for ~s: ~p", [AccountId, _R]),
            get_reseller_id([])
    end.

-spec get_reseller_id(ne_binary(), ne_binaries(), wh_json:object()) -> api_binary().
get_reseller_id(Parent, Ancestors, JObj) ->
    case wh_json:is_true(<<"pvt_reseller">>, JObj) of
        'false' -> get_reseller_id(Ancestors);
        'true' -> Parent
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_save('false' | services()) -> 'false' |
                                          services() |
                                          {'error', 'no_change'}.
maybe_save('false') -> 'false';
maybe_save(#wh_services{jobj=JObj
                        ,updates=UpdatedQuantities
                       }=Services) ->
    CurrentQuantities = wh_json:get_value(?QUANTITIES, JObj, wh_json:new()),
    case have_quantities_changed(UpdatedQuantities, CurrentQuantities) of
        'true' ->
            lager:debug("quantities have changed, saving dirty services"),
            save(Services);
        'false' ->
            lager:debug("no service quantity changes"),
            {'error', 'no_change'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec have_quantities_changed(wh_json:object(), wh_json:object()) -> boolean().
have_quantities_changed(Updated, Current) ->
    KeyNotSameFun = fun(Key) ->
                            wh_json:get_value(Key, Updated) =/= wh_json:get_value(Key, Current)
                    end,

    any_changed(KeyNotSameFun, Updated)
        orelse any_changed(KeyNotSameFun, Current).

-type changed_fun() :: fun((wh_json:key()) -> boolean()).

-spec any_changed(changed_fun(), wh_json:object()) -> boolean().
any_changed(KeyNotSameFun, Quantities) ->
    lists:any(KeyNotSameFun
              ,[[Category, Item]
                || Category <- wh_json:get_keys(Quantities),
                   Item <- wh_json:get_keys(Category, Quantities)
               ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_account_definition(ne_binary(), ne_binary()) -> wh_json:object().
get_account_definition(<<_/binary>> = AccountDb, <<_/binary>> = AccountId) ->
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'error', _R} ->
            lager:debug("unable to get account defintion for ~s: ~p", [AccountId, _R]),
            wh_json:new();
        {'ok', JObj} -> JObj
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_clean_old_billing_id(services()) -> services().
maybe_clean_old_billing_id(#wh_services{billing_id=BillingId
                                        ,current_billing_id=BillingId
                                       }=Services) ->
    Services;
maybe_clean_old_billing_id(#wh_services{current_billing_id=BillingId
                                        ,account_id=BillingId
                                        ,jobj=JObj
                                       }=Services) ->
    case wh_json:is_true(<<"pvt_reseller">>, JObj) of
        'true' -> Services;
        'false' ->
            _ = wh_service_sync:clean(BillingId),
            Services
    end;
maybe_clean_old_billing_id(#wh_services{}=Services) ->
    Services.
