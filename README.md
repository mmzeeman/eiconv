# Erlang NIF wrapper for iconv

[![Build Status](https://travis-ci.org/zotonic/eiconv.svg?branch=master)](https://travis-ci.org/zotonic/eiconv)
[![Hex pm](http://img.shields.io/hexpm/v/eiconv.svg?style=flat)](https://hex.pm/packages/eiconv)

An Erlang wrapper for the character set conversion utility [iconv](https://en.wikipedia.org/wiki/Iconv).

## Getting started

### Include as a dependency

Using [rebar3](http://www.rebar3.org/), add the dependency to your `rebar.config` file:

```erlang
{deps, [
    {eiconv, "1.0.0"},
    %% ...
]}.
```

and run `$ rebar3 compile`.

### Use `eiconv` module

To convert from `utf-8` to `ascii`:

```erlang
ToConvert = "123",
{ok, Converted} = eiconv:convert("utf-8", "ascii", ToConvert)),
io:format("Converted '~s' to '~s'~n", [ToConvert, Converted])
```

## `eiconv` API

### Using `convert/3`

```erlang
{ok, Converted} = eiconv:convert("utf-8", "ascii", "123"))
```

### Using CD (Conversion Descriptor)

```erlang
{ok, CD} = eiconv:open("utf8", "ascii"),
{ok, Converted} = eiconv:conv(CD, "123")),
ok = eiconvclose(CD)
```

## `iconv` API

### Using `convert/3`

```erlang
Converted = eiconv:convert("utf-8", "ascii", "123"))
```

(Note it return directly the converted text and not a tuple {ok, Converted})

## Authors

Wrapper provided by Maas-Maarten Zeeman and the [Zotonic](https://github.com/zotonic) team.

* [Maas-Maarten Zeeman](https://github.com/mmzeeman)
* [Arjan Scherpenisse](https://github.com/arjan)
* [Marc Worrell](https://github.com/mworrell)
