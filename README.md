Ejabberd 18.09 module to send offline user's message via POST request to target URL.
This module can call an api to send e.g. a push message. 
The request body is in application/x-www-form-urlencoded format. See the example below.



Installation
------------

1. cd /opt/ejabberd-18.09/.ejabberd-module/sources/
2. git clone https://github.com/tareqassi/ejabberd-http-post-offline-messages.git;
3. bash /opt/ejabberd-18.09/.ejabberd-module/bin/ejabberdctl module-install mod_offline_http_post_ext
4. /etc/init.d/ejabberd restart;

That's it. The module is now installed.

Configuration
-------------

Add the following to ejabberd configuration under `modules:`

```
mod_offline_http_post_ext:
    auth_token: "secret"
    post_url: "http://example.com/notify"
```

-    auth_token - user defined, hard coded token that will be sent as part of the request's body. Use this token on the target server to validate that the request arrived from a trusted source.
-    post_url - the server's endpoint url

Example of the outgoing request:
--------------------------------

```
array(5) {
  ["to"]=>
  string(11) "testuser"
  ["from"]=>
  string(7) "patrick"
  ["vhost"]=>
  string(16) "server.myserver.nl"
  ["body"]=>
  string(7) "This is a testmessage"
  ["messageId"]=>
  string(13) "purple3838f2f"
}
```

# ejabberd-http-post-offline-messages
