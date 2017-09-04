# Erlang NIF wrapper for iconv

[![Build Status](https://secure.travis-ci.org/zotonic/eiconv.png?branch=master)](http://travis-ci.org/zotonic/eiconv)

An Erlang wrapper for the character set conversion utility [iconv](https://en.wikipedia.org/wiki/Iconv).

## Getting started

### Include as a dependency

Using [rebar3](http://www.rebar3.org/), add the dependency to your `rebar.config` file:

```erlang
{deps, [
	{eiconv, ".*", {git, "git://github.com/zotonic/eiconv.git", {branch, "master"}}}
]}.
```

You must also enable the compilation of `eiconv_nif.so` through [port_compiler](https://github.com/blt/port_compiler). Again, add in your `rebar.config` file:

```erlang
{overrides,
 [{override, eiconv,
   [
	{plugins, [pc]},
	{port_env, [{"darwin|freebsd|openbsd", "LDFLAGS", "$LDFLAGS -liconv"},
				{"freebsd|openbsd", "CFLAGS", "$CFLAGS -I/usr/local/include"},
				{"freebsd|openbsd", "LDFLAGS", "$LDFLAGS -L/usr/local/lib"}]},

	{port_specs, [{"priv/eiconv_nif.so", ["c_src/*.c"]}]},
	{artifacts, ["priv/eiconv_nif.so"]},

	{provider_hooks, [
					  {post,
					   [
						{compile, {pc, compile}},
						{clean, {pc, clean}}
					   ]
					  }]
	}
   ]}
]}.
```

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
