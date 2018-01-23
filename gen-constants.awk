# Convert #define E... and #define ZMQ_... macros in zmq.h to a form suitable
# for Emacs. This script is meant to be passed the output of
#
#     cpp -dM <<< "#include <zmq.h>"
#
# and will attempt to extract all the relevant error codes and ZMQ constants
# for printing. Once the constants are obtained, it prints an emacs-lisp
# representation of the constants to stdout. The produced output should be
# placed in a file, zmq-constants.el

/^#define (E|ZMQ_)/ {
    # Don't process ZMQ_VERSION macros
    if(index($2, "VERSION") == 0) {
        # Remove #define
        $1 = ""
        $0 = substr($0, 2)
        # Massage into a form suitable for Emacs
        gsub(/0x/, "#x")
        gsub(/_/, "-")
        gsub(/ZMQ/, "zmq")
        if($1 ~ /^E/) {
            $0 = "zmq-" $0
            # prefix errors that are aliases of other errors
            gsub(/ E/, " zmq-E")
            # remove parentheses around expressions like
            # (-1), (zmq-HAUSNUMERO + 53), ...
            gsub(/\(|\)/, "")
            # If there are errors like "zmq-ETERM zmq-HAUSNUMERO + 53" at this
            # stage convert them into "zmq-ETERM (+ zmq-HAUSNUMERO 53)"
            if($4) {
                $0 = $1 " (" $3 " " $2 " " $4 ")"
            }
            sym = $1; $1 = ""
            errors[sym] = substr($0, 2)
        } else {
            sym = $1; $1 = ""
            val = substr($0, 2)
            # If a define statement has no value, set it to t.
            if(!val) {
                val = "t"
            }
            constants[sym] = val
        }
    }
}

END {
    print ";; Automatically generated"
    for(n = 0; n < ir; n++) {
        constants[ic++] = refconstants[n]
    }
    for(sym in constants) {
        num = constants[sym]
        if(num in constants) {
            num = constants[num]
        }
        print "(defconst " sym  " " num ")"
    }
    all_errors = "(defconst zmq-error-alist\n(list "
    for(sym in errors) {
        num = errors[sym]
        # For errors that reference other errors
        if(num in errors) {
            num = errors[num]
        }
        all_errors = all_errors "(cons " num " '" sym ")\n"
    }
    print all_errors "))"
    print ""
    print "(provide 'zmq-constants)"

}
