# Convert #define E... and #define ZMQ_... macros in zmq.h to a form suitable
# for Emacs. This script is meant to be called like
#
#     echo "#include <zmq.h>" > tmp.c && gcc -E -dM -DZMQ_BUILD_DRAFT_API=1 tmp.c | awk -f gen-constants.awk && rm tmp.c
#
# and will extract all the ZMQ constants for printing. The
# produced output should be placed in the file,
# src/constants.c
#
# NOTE: You will have to manually comment out a few of the
# constants.

/^#define ZMQ_/ {
    # Don't process ZMQ_VERSION macros
    if(index($2, "VERSION") == 0) {
        # Remove #define
        $1 = ""
        $0 = substr($0, 2)
        # Massage into an appropriate form
        const = $1
        gsub(/_/, "-")
        gsub(/ZMQ/, "zmq")
        print "EZMQ_DEFCONST(\"" $1 "\", " const ");"
    }
}
