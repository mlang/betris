CC=bin/cosmocc
CFLAGS=-Wall -Os
ifeq ($(CC), bin/cosmocc)
CFLAGS+=-mtiny -s
else
LDFLAGS=-lm
endif
COSMOCC_VERSION=4.0.2

betris.exe: betris.c $(CC)
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

bin/cosmocc: cosmocc-$(COSMOCC_VERSION).zip
	unzip $<
	touch $@

cosmocc-$(COSMOCC_VERSION).zip:
	wget https://cosmo.zip/pub/cosmocc/cosmocc-$(COSMOCC_VERSION).zip
