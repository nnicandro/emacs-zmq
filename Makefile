EMACS ?= emacs
CFLAGS ?= $(shell pkg-config --cflags libzmq)
CPPFLAGS ?= -E -dM
TEST_ORDER = zmq-utility zmq-encryption zmq-contexts zmq-messages zmq-sockets zmq-send-unicode zmq-polling zmq-subprocess
FILES = zmq-constants.el zmq.el zmq-ffi.el
ELCFILES = $(FILES:.el=.elc)
LIBS = -L /Users/nathan/.emacs.d/el-get/ffi

.PHONY: all build compile test clean

all: compile

test:
	$(EMACS) -nw -Q -batch -L . $(LIBS) -l ert -l zmq-tests.el \
--eval "(ert-run-tests-batch-and-exit '(member $(TEST_ORDER)))"

clean:
	rm -f *~
	rm -f \#*\#
	rm -f *.elc
	rm -f zmq-constants.el # generate constants on next compile

compile: build $(ELCFILES)

build: zmq-constants.el

zmq-constants.el:
	echo "#include <zmq.h>" >> zmq_header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) zmq_header.h | awk -f gen-constants.awk - > zmq-constants.el
	rm zmq_header.h

$(ELCFILES): %.elc: %.el
	$(EMACS) --batch -Q -L . $(LIBS) -f batch-byte-compile $<
