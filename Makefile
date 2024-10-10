CC=bin/cosmocc
CFLAGS=-Wall -Os
ifeq ($(CC), bin/cosmocc)
CFLAGS+=-mtiny -s
else
LDFLAGS=-lm
endif

betris.exe: betris.c $(CC)
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

bin/cosmocc: cosmocc.zip
	unzip $<
	touch $@

cosmocc.zip:
	wget https://cosmo.zip/pub/cosmocc/cosmocc.zip
