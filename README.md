rebar3_pkg
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_pkg, {git, "https://host/user/rebar3_pkg.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_pkg
    ===> Fetching rebar3_pkg
    ===> Compiling rebar3_pkg
    <Plugin Output>
