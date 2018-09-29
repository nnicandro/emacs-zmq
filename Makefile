ROOT = .
EMACS ?= emacs
FILES = zmq.el
# CFLAGS = -g
ELCFILES = $(FILES:.el=.elc)

.PHONY: all compile lib clean

all: lib compile

test:
	$(EMACS) -nw -Q -batch -L . -l ert -l zmq-tests.el \
--eval "(ert-run-tests-batch-and-exit)"

clean:
	$(MAKE) -C src clean
	rm emacs-zmq.so

compile: $(ELCFILES)

lib: src/Makefile
	$(MAKE) -C src install
	ln -fs src/emacs-zmq.so ./emacs-zmq.so

src/Makefile: src/configure
	cd src && ./configure --libdir=`pwd`

src/configure:
	cd src && autoreconf -vi

$(ELCFILES): %.elc: %.el
	$(EMACS) --batch -Q -L . $(LIBS) -f batch-byte-compile $<
