all: deps

deps:
	./mix compile

app:
	./mix compile

tests: deps
	./mix eunit

clean:
	./mix clean

distclean: clean

.PHONY: all deps app tests clean distclean