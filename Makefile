REBAR=$(shell which rebar)

DEPS_EBIN = deps/*/ebin ebin

default: all

all:    $(REBAR) deps compile

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

clean:  $(REBAR)
	$(REBAR) clean

console:
	erl -sname 'erltweet_console' -pa ${DEPS_EBIN}