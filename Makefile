ROOT = .
EMACS ?= emacs
FILES = zmq.el
# CFLAGS = -g
CPPFLAGS = -DEZMQ_DEBUG=0
ELCFILES = $(FILES:.el=.elc)

ZMQ_GIT_REPO ?= https://github.com/zeromq/libzmq
ZMQ_BASE_BUILD_DIR = $(CURDIR)/libzmq/build

# For building zmq locally
ZMQ_VERSION ?= 4.3.1
ZMQ_BUILD_DIR = $(ZMQ_BASE_BUILD_DIR)/v$(ZMQ_VERSION)
ZMQ_PKG_CONFIG_DIR = $(ZMQ_BUILD_DIR)/lib/pkgconfig

# Build ZMQ locally if ZMQ_CFLAGS and ZMQ_LIBS are not set in the environment.
# If they are, then use those settings without building ZMQ locally.
ifeq ($(ZMQ_CFLAGS),)
	ifeq ($(ZMQ_LIBS),)
BUILD_ZMQ_LOCALLY = yes
	endif
endif

.PHONY: all
all: lib compile

.PHONY: test
test:
	$(EMACS) -nw -Q -batch -L . -l ert -l zmq-tests.el \
--eval "(ert-run-tests-batch-and-exit)"

.PHONY: clean
clean:
	$(MAKE) -C src clean
	rm -f emacs-zmq.so $(ELCFILES)

.PHONY: cleanall
clean-zmq-build:
	rm -rf $(ZMQ_BASE_BUILD_DIR)

.PHONY: compile
compile: $(ELCFILES)

.PHONY: lib
lib: src/Makefile
	$(MAKE) CPPFLAGS=$(CPPFLAGS) -C src install
	ln -fs src/emacs-zmq.so ./emacs-zmq.so

src/Makefile: src/configure libzmq
ifneq ($(BUILD_ZMQ_LOCALLY),yes)
	$(info ZMQ_LIBS = $(ZMQ_LIBS))
	$(info ZMQ_CFLAGS = $(ZMQ_CFLAGS))
	$(error One of ZMQ_LIBS or ZMQ_CFLAGS not set)
endif
	cd src && ./configure PKG_CONFIG_PATH=$(ZMQ_PKG_CONFIG_DIR):$(PKG_CONFIG_PATH) \
		--libdir=$(CURDIR)/src

src/configure: src/configure.ac
	cd src && autoreconf -i

$(ELCFILES): %.elc: %.el
	$(EMACS) --batch -Q -L . $(LIBS) -f batch-byte-compile $<

### Building zmq locally

.PHONY: libzmq
ifeq ($(BUILD_ZMQ_LOCALLY), yes)
# We use the pkg-config file as a proxy for determining if the library is built
# since the configure script to build emacs-zmq will use pkg-config anyways.
libzmq: $(ZMQ_PKG_CONFIG_DIR)/libzmq.pc
else
libzmq:
endif

libzmq/.git:
	git clone --quiet $(ZMQ_GIT_REPO) libzmq

$(ZMQ_PKG_CONFIG_DIR)/libzmq.pc: libzmq/.git
	$(info Building ZMQ locally)
	$(info ZMQ_VERSION = $(ZMQ_VERSION))
	cd libzmq && \
	git checkout master && \
	git pull --quiet origin && \
	git checkout v$(ZMQ_VERSION) && \
	./autogen.sh && \
	./configure --quiet --without-docs --prefix=$(ZMQ_BUILD_DIR) \
		--enable-drafts=yes --enable-libunwind=no && \
	make install
