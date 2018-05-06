ROOT = .
BUILDDIR = ..
LD = gcc
CC = gcc
CFLAGS ?= $(shell pkg-config --cflags libzmq) -fPIC -pthread -g -O0
LDFLAGS ?= $(shell pkg-config --libs libzmq)
SRCS = socket.c context.c msg.c constants.c util.c core.c poll.c emacs-zmq.c
OBJS = $(SRCS:.c=.o)
HEADERS = $(wildcard *.h)

.PHONY: all

all: emacs-zmq.so
	cp emacs-zmq.so $(BUILDDIR)

%.so: $(OBJS)
	$(LD) -shared $(LDFLAGS) -o $@ $^

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -I$(ROOT) -c $<
