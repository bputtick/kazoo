%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Event subscription REST point
%%%
%%% Client requests an event subscription (assuming authed^2 (authenticated
%%% and authorized)).
%%%
%%% evtsub creates the proper queue on AMQP bound to the proper exchange with
%%% the proper binding, but does not consume messages off the queue yet. Queue
%%% identifier is placed in client's session, persisted to couch.
%%%
%%% Client requests events; evtsub creates a consumer of the queue, pulls off
%%% a limited number of messages (100 max?), and returns them in bulk to the
%%% client. evtsub also disconnects the consumer to keep messages queued in AMQP-
%%% land.
%%%
%%% Client requests event cancellation (or auth token times out, an event which
%%% evtsub has subscribed to within Crossbar); evtsub removes AMQP queue.
%%%
%%% @end
%%% Created :  9 Dec 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(evtsub_resource).

-export([init/1]).
-export([to_html/2, to_json/2, to_xml/2, to_text/2]).
-export([from_json/2, from_xml/2, from_text/2]).
-export([generate_etag/2, encodings_provided/2, finish_request/2, is_authorized/2]).
-export([content_types_accepted/2, content_types_provided/2, resource_exists/2, allowed_methods/2]).
-export([process_post/2]). %, post_is_create/2, create_path/2]).

-import(logger, [format_log/3]).

-include("crossbar.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-record(context, {
	  content_types_provided = [] :: list(tuple(atom(), list(string()))) | []
	  ,content_types_accepted = [] :: list(tuple(atom(), list(string()))) | []
	  ,amqp_host = "" :: string()
	  ,session = #session{} :: #session{}
          ,request = undefined % function to call in evtsub.erl
	 }).

init(Opts) ->
    Opts1 = lists:foldl(fun({_, OptsTmp}, Acc) -> lists:merge(OptsTmp, Acc) end, Opts
			,crossbar_bindings:run(<<"evtsub.init">>, Opts)),
    Provided = lists:foldr(fun({Fun, L}, Acc) ->
				   lists:foldr(fun(EncType, Acc1) -> [ {EncType, Fun} | Acc1 ] end, Acc, L)
			   end, [], props:get_value(content_types_provided, Opts1, [])),
    Accepted = lists:foldr(fun({Fun, L}, Acc) ->
				   lists:foldr(fun(EncType, Acc1) -> [ {EncType, Fun} | Acc1 ] end, Acc, L)
			   end, [], props:get_value(content_types_accepted, Opts1, [])),
    {ok, #context{
       content_types_provided = Provided
       ,content_types_accepted = Accepted
       ,amqp_host = props:get_value(amqp_host, Opts1)
      }}.

resource_exists(RD, Context) ->
    case wrq:path_info(request, RD) of
	undefined -> {{error, "Invalid request"}, RD, Context};
	Req when is_list(Req) ->
	    try
		Fun = list_to_existing_atom(Req),
		case erlang:function_exported(evtsub, Fun, 1) of
		    true ->
			format_log(info, "EVTSUB_R: ~p found.~n", [Fun]),
			{true, RD, Context#context{request=Fun}};
		    false ->
			format_log(info, "EVTSUB_R: ~p/1 not exported~n", [Fun]),
			{false, RD, Context}
		end
	    catch
		error:badarg ->
		    format_log(info, "EVTSUB_R: ~p doesn't exist~n", [Req]),
		    {false, RD, Context}
	    end
    end.

content_types_provided(RD, #context{content_types_provided=CTP}=Context) ->
    {CTP, RD, Context}.

content_types_accepted(RD, #context{content_types_accepted=CTA}=Context) ->
    {CTA, RD, Context}.

allowed_methods(RD, Context) ->
    {['GET', 'POST', 'PUT', 'DELETE', 'HEAD'], RD, Context}.

is_authorized(RD, Context) ->
    S = crossbar_session:start_session(RD),
    Res = crossbar_bindings:run(<<"evtsub.is_authorized">>, RD),
    %% TODO - process results of bindings and 
    {true, RD, Context#context{session=S}}.

generate_etag(RD, Context) ->
    { mochihex:to_hex(crypto:md5(wrq:resp_body(RD))), RD, Context }.

encodings_provided(RD, Context) ->
    { [{"identity", fun(X) -> X end},
       {"gzip", fun(X) -> zlib:gzip(X) end}],
      RD, Context}.

process_post(RD, #context{request=Fun}=Context) ->
    PostBody = mochiweb_util:parse_qs(wrq:req_body(RD)),
    QS = wrq:req_qs(RD),
    Params = lists:ukeymerge(1, lists:ukeysort(1, PostBody), lists:ukeysort(1, QS)),
    %% maybe assume JSON if we hit here?
    {true, wrq:append_to_response_body(whistle_util:to_binary(evtsub:Fun(Params)), RD), Context}.

%post_is_create(RD, Context) ->
%    {false, RD, Context}.

%create_path(RD, Context) ->
%    {"/cp", RD, Context}.

finish_request(RD, #context{session=S}=Context) ->
    {true, crossbar_session:finish_session(S, RD), Context}.

%% Convert the input to Erlang
from_text(RD, Context) ->
    format_log(info, "EVTSUB_R: from_text: QS: ~p ReqB: ~p~n", [wrq:req_qs(RD), wrq:req_body(RD)]),
    {true, RD, Context}.

from_json(RD, Context) ->
    format_log(info, "EVTSUB_R: from_json: QS: ~p ReqB: ~p~n", [wrq:req_qs(RD), wrq:req_body(RD)]),
    {true, RD, Context}.

from_xml(RD, Context) ->
    format_log(info, "EVTSUB_R: from_xml: QS: ~p ReqB: ~p~n", [wrq:req_qs(RD), wrq:req_body(RD)]),
    {true, RD, Context}.


%% Generate the response
to_html(RD, #context{request=Fun}=Context) ->
    format_log(info, "EVTSUB_R: to_html: ReqB: ~p~n", [wrq:resp_body(RD)]),
    {evtsub:Fun(RD), RD, Context}.

to_json(RD, #context{request=Fun}=Context) ->
    format_log(info, "EVTSUB_R: to_json: ReqB: ~p resp: ~s~n", [wrq:resp_body(RD), mochijson2:encode(evtsub:Fun(RD))]),
    {mochijson2:encode(evtsub:Fun(RD)), RD, Context}.

to_xml(RD, Context) ->
    format_log(info, "EVTSUB_R: to_xml: ReqB: ~p~n", [wrq:resp_body(RD)]),
    {"<xml><evtsub type='xml' /></xml>", RD, Context}.

to_text(RD, #context{request=Fun}=Context) ->
    format_log(info, "EVTSUB_R: to_text: ReqB: ~p~n", [wrq:resp_body(RD)]),
    {io_lib:format("txt ~p", [evtsub:Fun(RD)]), RD, Context}.
