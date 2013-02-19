REBAR=./rebar

all: compile

rebar:
	curl -L -O https://github.com/downloads/basho/rebar/rebar
	chmod u+x $(REBAR)

compile: rebar
	$(REBAR) compile

test: compile
	$(REBAR) eunit

clean: rebar
	$(REBAR) clean

distclean: 
	rm $(REBAR)
