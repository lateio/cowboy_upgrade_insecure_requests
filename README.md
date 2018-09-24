cowboy_upgrade_insecure_requests
=====

Purpose
-------

Upgrade insecure requests in cowboy http server

Aims to be compliant with 'Upgrade Insecure Requests' W3C doc
https://www.w3.org/TR/upgrade-insecure-requests


Behavior
--------

When this middleware executes on an http request with a header `Upgrade-Insecure-Requests: 1`, further request handling is forgone and an http redirect response to an https uri is returned. This middleware will not act on https requests. Thus the same middleware configuration can be used for both `cowboy_clear:start()` and `cowboy_tls:start()` if such a configuration is desirable.

By default the reply status code will be `307 Temporary Redirect`

By default the the middleware will merely swap the scheme of the the current request uri from http to https.
```
http://arv.io/path?query -> https://arv.io/path?query
```

However, if the request uri specifies an explicit port, the port is dropped and the default https port is used.
```
http://arv.io:8080/path?query -> https://arv.io/path?query
```


Customization
-------------

The returned redirect response can be customized by adding a map under the key `'Upgrade-Insecure-Requests'` in cowboy env.

```Erlang
RedirOpts = #{...},
Opts = #{env => #{'Upgrade-Insecure-Requests' => RedirOpts, ...}},
{_, _} = cowboy_clear:start(_, _, Opts).
```

Following keys can be defined in this map. *All keys are optional*.
* `'host'` - Override the host in redirect uri
* `'port'` - Override the port in redirect uri, use `'undefined'` for https default port
* `'path'` - Override the path in redirect uri
* `'qs'` - Override the query in redirect uri
* `'fragment'` - Add a fragment to redirect uri
* `'reply_status'` - Override the http reply status

Erlang type specifications for the options are as follows:
```Erlang
-type mfargs() :: {Module :: module(), Function :: atom(), Args :: [term()]}.
-type upgrade_insecure_requests_opts() ::
    #{
        'host'     => iodata()  | mfargs(),
        'port'     => undefined | inet:port_number() | mfargs(),
        'path'     => iodata()  | mfargs(),
        'qs'       => iodata()  | mfargs(),
        'fragment' => iodata()  | mfargs(),
        'reply_status' => 301 | 302 | 303 | 307 | 308 | mfargs()
    }.
```

In addition to static overrides, it is possible to use function calls to dynamically generate values for redirect options. By specifying as the value of a key in opts map a tuple in the form `{Module :: module(), Function :: atom(), Arguments :: [term()]}`, the request object and cowboy env will be added at the start of the Arguments list and the function will be applied. The return values must be the appropriate type as specified in the type spec. For example:

```Erlang
-module(foo).
-export([path/3,qs/2]).

path(Req = #{path := Path}, Env = #{prefix1 := Prefix1}, Prefix2) ->
    [$/, Prefix1, $/, Prefix2, Path].

qs(Req = #{qs := <<>>}, Env) ->
    <<"ssl_redirect=true">>;
qs(Req = #{qs := Qs}, Env) ->
    [Qs, $&, <<"ssl_redirect=true">>].

...

RedirOpts = #{
    path => {foo,path,["prefix2"]},
    qs   => {foo,qs,[]}
}.
```


Build
-----

    $ rebar3 compile
