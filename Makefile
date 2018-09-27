ROOT = .
EMACS ?= emacs
FILES = zmq.el
# CFLAGS = -g
ELCFILES = $(FILES:.el=.elc)

.PHONY: all compile lib clean

all: emacs-zmq.so compile

test:
	$(EMACS) -nw -Q -batch -L . -l ert -l zmq-tests.el \
--eval "(ert-run-tests-batch-and-exit)"

clean:
	$(MAKE) -C src clean
	rm emacs-zmq.so

compile: $(ELCFILES)

lib: emacs-zmq.so

emacs-zmq.so: src/Makefile src/emacs-zmq.so
	$(MAKE) -C src install
	cp src/emacs-zmq.so .

src/configure:
	cd src && autoreconf -vi

src/Makefile: src/configure
	cd src && ./configure --libdir=`pwd`

$(ELCFILES): %.elc: %.el
	$(EMACS) --batch -Q -L . $(LIBS) -f batch-byte-compile $<
