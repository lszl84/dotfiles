PREFIX  ?= $(HOME)/.local
SCANNER  = wayland-scanner

CFLAGS  ?= -Wall -O2
LDLIBS   = -lwayland-client

PROTO_HDRS = gen/wlr-foreign-toplevel-management-unstable-v1.h
PROTO_SRCS = gen/wlr-foreign-toplevel-management-unstable-v1.c

all: focused-output

focused-output: focused-output.c $(PROTO_HDRS) $(PROTO_SRCS)
	gcc $(CFLAGS) -o $@ focused-output.c $(PROTO_SRCS) -Igen $(LDLIBS)

gen/wlr-foreign-toplevel-management-unstable-v1.h: protocols/wlr-foreign-toplevel-management-unstable-v1.xml | gen
	$(SCANNER) client-header $< $@
gen/wlr-foreign-toplevel-management-unstable-v1.c: protocols/wlr-foreign-toplevel-management-unstable-v1.xml | gen
	$(SCANNER) public-code $< $@

gen:
	mkdir -p gen

install: focused-output
	install -Dm0755 focused-output $(DESTDIR)$(PREFIX)/bin/focused-output

clean:
	rm -rf focused-output gen

.PHONY: all install clean
