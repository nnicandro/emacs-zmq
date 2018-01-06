EMACS ?= emacs
TEST_ORDER = zmq-utility zmq-encryption zmq-contexts zmq-messages zmq-sockets zmq-send-unicode zmq-polling zmq-subprocess
FILES = zmq.el zmq-ffi.el zmq-constants.el
ELCFILES = $(FILES:.el=.elc)
LIBS = -L /Users/nathan/.emacs.d/el-get/ffi

.PHONY: all compile test clean

all: compile

test:
	$(EMACS) -nw -Q -batch -L . $(LIBS) -l ert -l zmq-tests.el \
--eval "(ert-run-tests-batch-and-exit '(member $(TEST_ORDER)))"

clean:
	rm -f *~
	rm -f \#*\#
	rm -f *.elc

compile: $(ELCFILES)

$(ELCFILES): %.elc: %.el
	$(EMACS) --batch -Q -L . $(LIBS) -f batch-byte-compile $<
