ROOT = .
EMACS ?= emacs
FILES = zmq.el
# CFLAGS = -g
CPPFLAGS = -DEZMQ_DEBUG=0
ELCFILES = $(FILES:.el=.elc)

ZMQ_GIT_REPO ?= https://github.com/zeromq/libzmq

# Build ZMQ locally if ZMQ_CFLAGS and ZMQ_LIBS are not set in the environment.
# If they are, then use those settings without building ZMQ locally.
ifeq ($(ZMQ_CFLAGS),)
ifeq ($(ZMQ_LIBS),)
BUILD_ZMQ_LOCALLY = yes
ZMQ_VERSION ?= 4.3.1
ZMQ_BASE_BUILD_DIR = $(CURDIR)/libzmq/build
ZMQ_BUILD_DIR = $(ZMQ_BASE_BUILD_DIR)/v$(ZMQ_VERSION)
ZMQ_PKG_CONFIG_DIR = $(ZMQ_BUILD_DIR)/lib/pkgconfig
$(shell touch version)
endif
endif

.PHONY: all
all: emacs-zmq.so compile

emacs-zmq.so: src/Makefile
	$(MAKE) CPPFLAGS=$(CPPFLAGS) -C src install

ifeq ($(BUILD_ZMQ_LOCALLY),yes)
src/Makefile: libzmq src/Makefile.am src/configure
else
src/Makefile: src/Makefile.am src/configure
endif
	cd src && ./configure PKG_CONFIG_PATH=$(ZMQ_PKG_CONFIG_DIR):$(PKG_CONFIG_PATH) \
		--libdir=$(CURDIR)

src/configure: src/configure.ac
	cd src && autoreconf -i

.PHONY: test
test:
	$(EMACS) -nw -Q -batch -L . -l ert -l zmq-tests.el \
--eval "(ert-run-tests-batch-and-exit)"

.PHONY: clean
clean:
	$(MAKE) -C src clean
	rm -f emacs-zmq.so emacs-zmq.la $(ELCFILES)

.PHONY: clean-zmq-build
clean-zmq-build:
	rm -rf $(ZMQ_BASE_BUILD_DIR)

.PHONY: compile
compile: $(ELCFILES)

$(ELCFILES): %.elc: %.el
	$(EMACS) --batch -Q -L . $(LIBS) -f batch-byte-compile $<

### Building ZMQ locally

.PHONY: libzmq
ifeq ($(BUILD_ZMQ_LOCALLY), yes)
# We use the pkg-config file as a proxy for determining if the library is built
# since the configure script to build emacs-zmq will use pkg-config anyways.
libzmq: $(ZMQ_PKG_CONFIG_DIR)/libzmq.pc
ifneq ($(shell cat version), $(ZMQ_VERSION))
# Clean out object files so that we ensure that the right libzmq is linked in
# the case that a version of ZMQ is being linked and we had
# previously linked a different version.
	$(MAKE) -i clean
	echo $(ZMQ_VERSION) > version
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
	./configure --quiet --without-docs --prefix=$(ZMQ_BUILD_DIR) \
		--enable-drafts=yes --enable-libunwind=no --enable-static=no && \
	$(MAKE) install
