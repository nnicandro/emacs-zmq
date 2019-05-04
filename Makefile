ROOT = .
SHELL = bash
EMACS ?= emacs
FILES = zmq.el
# CFLAGS = -g
CPPFLAGS = -DEZMQ_DEBUG=0
ELCFILES = $(FILES:.el=.elc)

ZMQ_GIT_REPO ?= https://github.com/zeromq/libzmq

# Set this to a host triple to enable cross compiling
ZMQ_BUILD_HOST ?=

# Build ZMQ locally if ZMQ_CFLAGS and ZMQ_LIBS are not set in the environment.
# If they are, then use those settings without building ZMQ locally.
ifeq ($(ZMQ_CFLAGS),)
ifeq ($(ZMQ_LIBS),)
BUILD_ZMQ_LOCALLY = yes
ZMQ_VERSION ?= 4.3.1
ZMQ_BASE_BUILD_DIR = $(CURDIR)/libzmq/build/$(ZMQ_BUILD_HOST)
ZMQ_BUILD_DIR = $(ZMQ_BASE_BUILD_DIR)/v$(ZMQ_VERSION)
ZMQ_PKG_CONFIG_DIR = $(ZMQ_BUILD_DIR)/pkgconfig
$(shell touch version)
endif
endif

ifneq (,$(or $(findstring mingw, $(ZMQ_BUILD_HOST)), \
			 $(findstring cygwin, $(ZMQ_BUILD_HOST)), \
			 $(findstring msys, $(ZMQ_BUILD_HOST)), \
			 $(findstring Windows_NT, $(OS))))
ZMQ_BUILD_FOR_WINDOWS = yes
endif

ifneq ($(ZMQ_BUILD_FOR_WINDOWS),)
CXXFLAGS="-static-libgcc -static-libstdc++"
SHARED_EXT := .dll
else
CXXFLAGS=""
SHARED_EXT := .so
endif

SHARED := emacs-zmq$(SHARED_EXT)

.PHONY: all
all: $(SHARED) compile

$(SHARED): src/Makefile
	$(MAKE) CPPFLAGS=$(CPPFLAGS) -C src install

ifeq ($(BUILD_ZMQ_LOCALLY),yes)
src/Makefile: libzmq src/Makefile.am src/configure
else
src/Makefile: src/Makefile.am src/configure
endif
	cd src && ./configure PKG_CONFIG_PATH=$(ZMQ_PKG_CONFIG_DIR):$(PKG_CONFIG_PATH) \
		--host=$(ZMQ_BUILD_HOST) --libdir=$(CURDIR)

src/configure: src/configure.ac
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
	$(RM) libzmq.*
	$(RM) -r $(ZMQ_BASE_BUILD_DIR)

.PHONY: compile
compile: $(ELCFILES)

$(ELCFILES): %.elc: %.el
	$(EMACS) --batch -Q -L . $(LIBS) -f batch-byte-compile $<

### Make products (mainly for Travis)
# But only if we are cross compiling. For testing purposes, be sure to call
# make clean first.

ifneq ($(ZMQ_BUILD_HOST),)
ifneq ($(shell command -v shasum),)
# OS X
SHA256SUM := shasum -a 256
else
# GNU Coreutils
SHA256SUM := sha256sum
endif

.PHONY: products
products:
	make $(SHARED)
	mkdir -p products
	tar -czf products/emacs-zmq-$(ZMQ_BUILD_HOST).tar.gz *$(SHARED_EXT)
	cd products && $(SHA256SUM) emacs-zmq-$(ZMQ_BUILD_HOST).tar.gz > \
		emacs-zmq-$(ZMQ_BUILD_HOST).tar.gz.sha256
endif

### Building ZMQ locally

.PHONY: libzmq
ifneq ($(BUILD_ZMQ_LOCALLY),)
# We use the pkg-config file as a proxy for determining if the library is built
# since the configure script to build emacs-zmq will use pkg-config anyways.
libzmq: $(ZMQ_PKG_CONFIG_DIR)/libzmq.pc
ifneq ($(shell cat version), $(ZMQ_VERSION))
# Clean out object files so that we ensure the right libzmq is linked.
	$(MAKE) -i clean
	echo $(ZMQ_VERSION) > version
ifneq ($(ZMQ_BUILD_FOR_WINDOWS),)
	cp $(ZMQ_BUILD_DIR)/bin/libzmq.dll $(CURDIR)
endif
endif
else
libzmq: ;
endif

libzmq/.git:
	git clone --quiet $(ZMQ_GIT_REPO) libzmq

# Don't depend on libzmq/.git since checking out a version of zmq that is
# already built will still cause this recipe to fire since every time a
# checkout occurs, the libzmq/.git directory timestamp is updated.
$(ZMQ_PKG_CONFIG_DIR)/libzmq.pc:
ifeq ($(wildcard libzmq/.git),)
	$(info Downloading libzmq from $(ZMQ_GIT_REPO))
	$(MAKE) libzmq/.git
endif
	$(info Building ZMQ v$(ZMQ_VERSION))
	cd libzmq && \
	git checkout master && \
	git pull --quiet origin && \
	git checkout v$(ZMQ_VERSION) && \
	./autogen.sh && \
	./configure CXXFLAGS=$(CXXFLAGS) --quiet --without-docs --prefix=$(ZMQ_BUILD_DIR) \
		--enable-drafts=yes --enable-libunwind=no --enable-static=no \
		--disable-curve-keygen --disable-perf --disable-eventfd \
		--with-pkgconfigdir=$(ZMQ_PKG_CONFIG_DIR) \
		--host=$(ZMQ_BUILD_HOST) && \
	$(MAKE) install
