ROOT = .
LD = gcc
CC = gcc
CFLAGS ?= $(shell pkg-config --cflags libzmq) -fPIC
LDFLAGS ?= $(shell pkg-config --libs libzmq)
CPPFLAGS ?= -E -dM
SRCS = socket.c context.c msg.c
OBJS = $(SRCS:.c=.o)
HEADERS = $(wildcard *.h)

.PHONY: all

all: emacs-zmq.so

%.so: $(OBJS)
	$(LD) -shared $(LDFLAGS) -o $@ $^

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -I$(ROOT) -I$(ROOT)/../../.. -c $<
