all:
	erlc *erl
	erl

clean:
	rm -f *beam *dump
