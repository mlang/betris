CC=bin/cosmocc
CFLAGS=-Os
ifeq ($(CC), bin/cosmocc)
CFLAGS+=-mtiny
else
LDFLAGS=-lm
endif

betris.exe: betris.c $(CC)
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

bin/cosmocc: cosmocc.zip
	unzip $<

cosmocc.zip:
	wget https://cosmo.zip/pub/cosmocc/cosmocc.zip
