CFLAGS= -DINET6 -DHAVE_GETIFADDRS -DUSER_IF_MEDIA -D__APPLE_USE_RFC_3542=1 -DUSE_RFC2292BIS

ping6x: ping6x.c
	clang $(CFLAGS) $< -o $@
