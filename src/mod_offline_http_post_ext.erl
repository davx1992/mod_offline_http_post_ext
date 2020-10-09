%% name of module must match file name
%% Update: info@ph-f.nl
-module(mod_offline_http_post_ext).

-author("dev@codepond.org").

%% modified by @tareqassi

-behaviour(gen_mod).

-export([create_message/1,
         depends/2,
         mod_opt_type/1,
         mod_options/1,
         start/2,
         stop/1]).

-include("scram.hrl").

-include("xmpp.hrl").

-include("logger.hrl").

depends(_Host, _Opts) -> [].

mod_options(_Host) ->
    [{auth_token, <<"secret">>},
     {post_url, <<"http://example.com/test">>}].

mod_opt_type(auth_token) -> fun iolist_to_binary/1;
mod_opt_type(post_url) -> fun iolist_to_binary/1.

start(_Host, _Opt) ->
    ?INFO_MSG("mod_offline_http_post_ext loading", []),
    inets:start(),
    ?INFO_MSG("HTTP client started", []),
    ejabberd_hooks:add(offline_message_hook,
                       _Host,
                       ?MODULE,
                       create_message,
                       1).

stop(_Host) ->
    ?INFO_MSG("stopping mod_offline_http_post_ext", []),
    ejabberd_hooks:delete(offline_message_hook,
                          _Host,
                          ?MODULE,
                          create_message,
                          1).

create_message({Action, Packet} = Acc)
    when (Packet#message.type == chat) and
             (Packet#message.body /= []) ->
    [{text, _, Body}] = Packet#message.body,
    ?INFO_MSG("Message Body is ~p~n ", [Body]),
    From = Packet#message.from,
    To = Packet#message.to,
    Token = gen_mod:get_module_opt(To#jid.lserver,
                                   ?MODULE,
                                   auth_token),
    ?INFO_MSG("Token is ~p~n ", [Token]),
    PostUrl = gen_mod:get_module_opt(To#jid.lserver,
                                     ?MODULE,
                                     post_url),
    ?INFO_MSG("PostUrl is ~p~n ", [PostUrl]),
    ToUser = To#jid.luser,
    ?INFO_MSG("to is ~p~n ", [ToUser]),
    FromUser = From#jid.luser,
    ?INFO_MSG("From is ~p~n ", [FromUser]),
    Vhost = To#jid.lserver,
    ?INFO_MSG("Vhost is ~p~n ", [Vhost]),
    MessageId = Packet#message.id,
    ?INFO_MSG("MessageId is ~p~n ", [MessageId]),
    MarkerId = fxml:get_path_s(xmpp:encode(Packet),
                               [{elem, list_to_binary("meta")},
                                {attr, list_to_binary("markerId")}]),

    ?INFO_MSG("processing text message ", []),
    Data = string:join(["to=",
                        binary_to_list(ToUser),
                        "&from=",
                        binary_to_list(FromUser),
                        "&body=",
                        binary_to_list(Body),
                        "&markerId=",
                        binary_to_list(MarkerId),
                        "&messageId=",
                        binary_to_list(MessageId)],
                       ""),
    post_offline_message(PostUrl, Token, Data),
    Acc;
create_message({Action, Packet} = Acc)
    when Packet#message.type == normal ->
    case misc:unwrap_mucsub_message(Packet) of
        #message{} = Msg ->
            [{text, _, Body}] = Msg#message.body,
            From = Msg#message.from,
            To = Msg#message.to,
            Token = gen_mod:get_module_opt(To#jid.lserver,
                                           ?MODULE,
                                           auth_token),
            ?INFO_MSG("Token is ~p~n ", [Token]),
            PostUrl = gen_mod:get_module_opt(To#jid.lserver,
                                             ?MODULE,
                                             post_url),
            ?INFO_MSG("PostUrl is ~p~n ", [PostUrl]),
            ToUser = To#jid.luser,
            ?INFO_MSG("to is ~p~n ", [ToUser]),
            FromUser = From#jid.luser,
            ?INFO_MSG("From is ~p~n ", [FromUser]),
            Vhost = To#jid.lserver,
            ?INFO_MSG("Vhost is ~p~n ", [Vhost]),
            MessageId = Msg#message.id,
            ?INFO_MSG("MessageId is ~p~n ", [MessageId]),
            MarkerId = fxml:get_path_s(xmpp:encode(Msg),
                                       [{elem, list_to_binary("meta")},
                                        {attr, list_to_binary("markerId")}]),
            SenderId = fxml:get_path_s(xmpp:encode(Msg),
                                       [{elem, list_to_binary("user")},
                                        {attr, list_to_binary("id")}]),

            ?INFO_MSG("Offilen processing text message ~p~n",
                      [Msg]),
            ?INFO_MSG("processing text message ", []),
            Data = string:join(["to=",
                                binary_to_list(ToUser),
                                "&from=",
                                binary_to_list(FromUser),
                                "&body=",
                                binary_to_list(Body),
                                "&markerId=",
                                binary_to_list(MarkerId),
                                "&senderId=",
                                binary_to_list(SenderId),
                                "&messageId=",
                                binary_to_list(MessageId)],
                               ""),
            post_offline_message(PostUrl, Token, Data),
            Msg
    end,
    Acc;
create_message(Acc) -> Acc.

post_offline_message(PostUrl, Token, Data) ->
    ?INFO_MSG("post ~p to ~p using ~p~n ",
              [Data, PostUrl, Token]),
    Request = {binary_to_list(PostUrl),
               [{"Authorization", binary_to_list(Token)}],
               "application/x-www-form-urlencoded;  "
               "charset=utf-8",
               Data},
    httpc:request(post, Request, [], []),
    ?INFO_MSG("post request sent", []).
