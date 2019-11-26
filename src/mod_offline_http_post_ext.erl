%% name of module must match file name
%% Update: info@ph-f.nl
-module(mod_offline_http_post_ext).
-author("dev@codepond.org").
%% modified by @tareqassi

-behaviour(gen_mod).

-export([start/2, stop/1, mod_options/1, depends/2, mod_opt_type/1, create_message/1]).

-include("scram.hrl").
-include("xmpp.hrl").
-include("logger.hrl").

start(_Host, _Opt) ->
  ?INFO_MSG("mod_offline_http_post_ext loading", []),
  inets:start(),
  ?INFO_MSG("HTTP client started", []),
  ejabberd_hooks:add(offline_message_hook, _Host, ?MODULE, create_message, 1).

stop (_Host) ->
  ?INFO_MSG("stopping mod_offline_http_post_ext", []),
  ejabberd_hooks:delete(offline_message_hook, _Host, ?MODULE, create_message, 1).

mod_opt_type(auth_token) -> fun iolist_to_binary/1;
mod_opt_type(post_url) -> fun iolist_to_binary/1;

mod_options(_Host) ->
    [{auth_token, "tobechanged"},
     {post_url, "/tbchanged/"}].

depends(_Host, _Opts) ->
    [].

create_message({Action, Packet} = Acc) when (Packet#message.type == chat) and (Packet#message.body /= []) ->
  [{text, _, Body}] = Packet#message.body,
  ?INFO_MSG("Message Body is ~p~n ", [Body]),
  From = Packet#message.from,
  To = Packet#message.to,
  Token = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, auth_token, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
  ?INFO_MSG("Token is ~p~n ", [Token]),
  PostUrl = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, post_url, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
  ?INFO_MSG("PostUrl is ~p~n ", [PostUrl]),
  ToUser = To#jid.luser,
  ?INFO_MSG("to is ~p~n ", [ToUser]),
  FromUser = From#jid.luser,
  ?INFO_MSG("From is ~p~n ", [FromUser]),
  Vhost = To#jid.lserver,
  ?INFO_MSG("Vhost is ~p~n ", [Vhost]),
  MessageId = Packet#message.id,
  ?INFO_MSG("MessageId is ~p~n ", [MessageId]),
  MessageType = fxml:get_path_s(xmpp:encode(Packet), [{elem,list_to_binary("mtype")}, {attr, list_to_binary("value")}]),
  ?INFO_MSG("MessageType is ~p~n ", [MessageType]),
  SenderId = fxml:get_path_s(xmpp:encode(Packet), [{elem,list_to_binary("channel")}, {attr, list_to_binary("senderId")}]),
  ?INFO_MSG("SenderId is ~p~n ", [SenderId]),
  RecipientId = fxml:get_path_s(xmpp:encode(Packet), [{elem,list_to_binary("channel")}, {attr, list_to_binary("recipientId")}]),
  ?INFO_MSG("RecipientId is ~p~n ", [RecipientId]),

  case MessageType of
    <<"text">>  ->
      ?INFO_MSG("processing text message ", []),
      Data = string:join(["to=", binary_to_list(ToUser), 
                          "&from=", binary_to_list(FromUser), 
                          "&vhost=", binary_to_list(Vhost), 
                          "&messageType=", binary_to_list(MessageType),
                          "&senderId=", binary_to_list(SenderId),
                          "&recipientId=", binary_to_list(RecipientId),
                          "&body=", binary_to_list(Body), 
                          "&messageId=", binary_to_list(MessageId)], ""),
      post_offline_message(PostUrl, Token, Data);
    <<"location">> ->
      ?INFO_MSG("processing location message ", []),
      Longitude = fxml:get_path_s(xmpp:encode(Packet), [{elem, list_to_binary("location")}, {attr, list_to_binary("lng")}]),
      ?INFO_MSG("longitude is ~p ", [Longitude]),
      Latitude = fxml:get_path_s(xmpp:encode(Packet), [{elem, list_to_binary("location")}, {attr, list_to_binary("lat")}]),
      ?INFO_MSG("longitude is ~p ", [Latitude]),
      Data = string:join(["to=", binary_to_list(ToUser), 
                          "&from=", binary_to_list(FromUser), 
                          "&longitude=", binary_to_list(Longitude), 
                          "&latitude=", binary_to_list(Latitude), 
                          "&vhost=", binary_to_list(Vhost), 
                          "&messageType=", binary_to_list(MessageType),
                          "&senderId=", binary_to_list(SenderId),
                          "&recipientId=", binary_to_list(RecipientId),
                          "&body=", binary_to_list(Body), 
                          "&messageId=", binary_to_list(MessageId)], ""),
      post_offline_message(PostUrl, Token, Data);
    <<"buyrequest">> ->
      ?INFO_MSG("processing location message ", []),
      BuyRequestId = fxml:get_path_s(xmpp:encode(Packet), [{elem, list_to_binary("buyrequest")}, {attr, list_to_binary("id")}]),
      ?INFO_MSG("BuyRequestId is ~p ", [BuyRequestId]),
      Data = string:join(["to=", binary_to_list(ToUser), 
                          "&from=", binary_to_list(FromUser), 
                          "&buyRequestId=", binary_to_list(BuyRequestId),  
                          "&vhost=", binary_to_list(Vhost), 
                          "&messageType=", binary_to_list(MessageType), 
                          "&senderId=", binary_to_list(SenderId),
                          "&recipientId=", binary_to_list(RecipientId),
                          "&body=", binary_to_list(Body), 
                          "&messageId=", binary_to_list(MessageId)], ""),
      post_offline_message(PostUrl, Token, Data); 
    <<"media">> ->
      ?INFO_MSG("processing mediaa message ", []),
      MediaType = fxml:get_path_s(xmpp:encode(Packet), [{elem, list_to_binary("url")}, {attr, list_to_binary("mediaType")}]),
      ?INFO_MSG("mediatype is ~p ", [MediaType]),
      Link = fxml:get_path_s(xmpp:encode(Packet), [{elem, list_to_binary("url")}, {attr, list_to_binary("link")}]),
      ?INFO_MSG("link is ~p ", [Link]),
      Data = string:join(["to=", binary_to_list(ToUser), 
                          "&from=", binary_to_list(FromUser), 
                          "&url=", binary_to_list(Link), 
                          "&mediaType=", binary_to_list(MediaType), 
                          "&vhost=", binary_to_list(Vhost), 
                          "&messageType=", binary_to_list(MessageType),
                          "&senderId=", binary_to_list(SenderId),
                          "&recipientId=", binary_to_list(RecipientId), 
                          "&body=", binary_to_list(Body), 
                          "&messageId=", binary_to_list(MessageId)], ""),
      post_offline_message(PostUrl, Token, Data);
    <<"">> ->
      ?INFO_MSG("missing mtype or its value", [])
  end,
  Acc;

create_message(Acc) ->
  Acc.

post_offline_message(PostUrl, Token, Data) ->
  ?INFO_MSG("post ~p to ~p using ~p~n ", [Data, PostUrl, Token]),
  Request = {binary_to_list(PostUrl), [{"Authorization", binary_to_list(Token)}], "application/x-www-form-urlencoded;  charset=utf-8", Data},
  httpc:request(post, Request,[],[]),
  ?INFO_MSG("post request sent", []).
