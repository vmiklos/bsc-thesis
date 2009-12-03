T = $(wildcard t[0-9][0-9][0-9][0-9]-*.sh)

all: $(T)
	@echo "passed $$(echo $(T)|wc -w) tests."

$(T):
	@echo "*** $@ ***"; sh $@

.PHONY: $(T)
