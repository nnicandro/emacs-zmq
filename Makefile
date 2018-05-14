ROOT = .
EMACS ?= emacs
FILES = zmq.el
ELCFILES = $(FILES:.el=.elc)

.PHONY: all compile

all: emacs-zmq.so compile

test:
	$(EMACS) -nw -Q -batch -L . -l ert -l zmq-tests.el \
--eval "(ert-run-tests-batch-and-exit))"

compile: $(ELCFILES)

emacs-zmq.so: src/configure src/Makefile
	cd src && make install
	cp src/emacs-zmq.so .

src/configure:
	cd src && autoreconf -vi

src/Makefile: src/configure
	cd src && ./configure --libdir=`pwd`

$(ELCFILES): %.elc: %.el
	$(EMACS) --batch -Q -L . $(LIBS) -f batch-byte-compile $<
