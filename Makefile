ROOT = .
SHELL = bash
EMACS ?= emacs
EFILES := zmq.el
# CPPFLAGS = -DEZMQ_DEBUG=0
ELCFILES = $(EFILES:.el=.elc)

export ZMQ_GIT_REPO ?= https://github.com/zeromq/libzmq
# The version of ZMQ to build
export ZMQ_VERSION ?= 4.3.1
# Directory in which the emacs-zmq module will be written
EZMQ_LIBDIR ?= $(CURDIR)
# NOTE: The ZMQ_LIBS and ZMQ_CFLAGS can be set before configuring the project
# to point to the ZMQ to build with.

MODULE_EXT := $(shell $(EMACS) -Q --batch --eval "(princ (and (boundp 'module-file-suffix) module-file-suffix))")
ifeq ($(MODULE_EXT), nil)
  $(error No module support in $(EMACS))
endif
EZMQ_MODULE := emacs-zmq$(MODULE_EXT)

.PHONY: all
all: $(EZMQ_LIBDIR)/$(EZMQ_MODULE) compile

.PHONY: configure
configure: src/configure
	cd src && ./configure CPPFLAGS="$(CPPFLAGS)" \
        --prefix=$(CURDIR) \
		--enable-shared=emacs-zmq --enable-static=zeromq \
		--without-docs --enable-drafts=yes --enable-libunwind=no \
		--disable-curve-keygen --disable-perf --disable-eventfd

$(EZMQ_LIBDIR)/$(EZMQ_MODULE): src/Makefile
	$(MAKE) -C src
	mkdir -p $(EZMQ_LIBDIR)
	cp src/.libs/$(EZMQ_MODULE) $(EZMQ_LIBDIR)/$(EZMQ_MODULE)

src/Makefile: src/configure
	$(MAKE) configure

# Needed for static Windows builds of libzmq, see libzmq/INSTALL
ifeq ($(MODULE_EXT),.dll)
CPPFLAGS += -DZMQ_STATIC
endif

src/configure: src/configure.ac src/Makefile.am
	cd src && autoreconf -i

.PHONY: test
test:
	$(EMACS) -nw -Q -batch -L . -l ert -l zmq-tests.el \
		--eval "(ert-run-tests-batch-and-exit)"

.PHONY: clean
clean:
	$(MAKE) -C src clean
	$(RM) emacs-zmq.* $(ELCFILES)

.PHONY: clean-zmq-build
clean-zmq-build:
	$(RM) -r src/libzmq-build
	$(MAKE) -C src clean-libzmq

.PHONY: compile
compile: $(ELCFILES)

$(ELCFILES): %.elc: %.el
	$(EMACS) --batch -Q -L . -f batch-byte-compile $<

ifneq (,$(filter products,$(MAKECMDGOALS)))
  ifeq (,$(shell which $(CC)))
    $(error "Compiler $(CC) not found.")
  endif
  PRODUCT := emacs-zmq-$(shell $(CC) -dumpmachine)
  ifneq ($(shell command -v shasum),)
    SHA256SUM := shasum -a 256
  else
    SHA256SUM := sha256sum
  endif
endif

.PHONY: products
products: products/$(PRODUCT).tar.gz.sha256

products/$(PRODUCT).tar.gz: $(EZMQ_LIBDIR)/$(EZMQ_MODULE)
	mkdir -p products/$(PRODUCT)
	cp $(EZMQ_LIBDIR)/*$(EZMQ_MODULE) products/$(PRODUCT)
	cd products && \
		tar -czf $(CURDIR)/products/$(PRODUCT).tar.gz $(PRODUCT)

products/$(PRODUCT).tar.gz.sha256: products/$(PRODUCT).tar.gz
	cd products && \
		$(SHA256SUM) $(PRODUCT).tar.gz > $(PRODUCT).tar.gz.sha256
