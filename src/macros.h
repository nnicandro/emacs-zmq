#ifndef __EZMQ_MACROS_H__
#define __EZMQ_MACROS_H__

#define INTERN(env, val) env->intern(env, val)

#define EZMQ_NONLOCAL_EXIT(env) (env->non_local_exit_check(env) != emacs_funcall_exit_return)

#define EZMQ_EXTRACT_INT(env, name, val)            \
    intmax_t name = env->extract_integer(env, val); \
    do {                                            \
        if(EZMQ_NONLOCAL_EXIT(env)) {               \
            return NULL;                            \
        }                                           \
    } while(0)

#define EZMQ_EXTRACT_OPTIONAL_INT(env, name, val) \
    intmax_t name = env->eq(env, val, Qnil) ? 0 : \
        env->extract_integer(env, val);           \
    do {                                          \
        if(EZMQ_NONLOCAL_EXIT(env)) {             \
            return NULL;                          \
        }                                         \
    } while(0)

#define EZMQ_EXTRACT_OBJ(env, name, type, val)           \
    ezmq_obj_t *name = ezmq_extract_obj(env, type, val); \
    if(!name) return NULL                                \

#define EZMQ_CHECK_ERROR(env, expr)             \
    do {                                        \
        int retcode = expr;                     \
        if(retcode == -1) {                     \
            ezmq_signal_error(env);             \
        }                                       \
    } while(0)

#define EZMQ_DOC(name, doc, args)  \
    const char *__zmq_doc_##name = doc "\n\\(fn " args ")"

#define EZMQ_CAR(env, list) env->funcall(env, Qcar, 1, (emacs_value []){ list })

#define EZMQ_CDR(env, list) env->funcall(env, Qcdr, 1, (emacs_value []){ list })

#define EZMQ_MAKE_FUN(env, argmin, argmax, name, ename) \
    bind_function(env,                                  \
                  ename,                                \
                  env->make_function(env,               \
                                     argmin, argmax,    \
                                     &F##name,          \
                                     __zmq_doc_##name,  \
                                     NULL))

#endif /* __EZMQ_MACROS_H__ */
