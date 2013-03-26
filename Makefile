all: ping6x whobot

CFLAGS= -DINET6 -DHAVE_GETIFADDRS -DUSER_IF_MEDIA -D__APPLE_USE_RFC_3542=1 -DUSE_RFC2292BIS -DSIGINFO
CFLAGS+= -DPING_STANDALONE

ping6x: ping6x.c
	clang $(CFLAGS) $< -o $@

run: ping6x
	./ping6x -Qwc1 ff02::1%en0

whobot:
	cabal configure
	cabal build
