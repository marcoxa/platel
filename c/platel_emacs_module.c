/* -*- Mode: C -*- */

/* platel_emacs_module.c
 *
 * See file COPYING in the top directory for copyright and licensing
 * information.
 */

#ifndef _PLATEL_EMACS_MODULE_C
#define _PLATEL_EMACS_MODULE_C

#include <stdbool.h>
#include <assert.h>
#include <emacs-module.h>
#include "platel.h"

int plugin_is_GPL_compatible;


static emacs_value
platel_e_is_big_endian(emacs_env * env,
		       ptrdiff_t nargs,
		       emacs_value *args,
		       void* data
		       ) {
  bool is_be;
  emacs_value result;

  /* assert(nargs == 0); */
  is_be = platel_is_bigendian();

  if (is_be)
    result = env->intern(env, "t");
  else
    result = env->intern(env, "nil");

  return result;
}


static emacs_value
platel_e_is_little_endian(emacs_env * env,
			  ptrdiff_t nargs,
			  emacs_value *args,
			  void* data
			  ) {
  bool is_be;
  emacs_value result;

  /* assert(nargs == 0); */
  is_be = platel_is_littleendian(); /* Yeah, I could do it shorter. */

  if (is_be)
    result = env->intern(env, "t");
  else
    result = env->intern(env, "nil");

  return result;
}


int
emacs_module_init(struct emacs_runtime * er) {

  if (er->size < sizeof(*er))
    return 1;			/* Error code. */
  else {
    emacs_env*  eenv =
      er->get_environment(er);
  
    emacs_value is_be_symbol =
      eenv->intern(eenv, "platel:is-big-endian");
  
    emacs_value is_le_symbol =
      eenv->intern(eenv, "platel:is-little-endian");
    
    emacs_value is_be_func =
      eenv->make_function(eenv, 0, 0, platel_e_is_big_endian,
			  "Return `T' if the current platform is big-endian.",
			  NULL);
  
    emacs_value is_le_func =
      eenv->make_function(eenv, 0, 0, platel_e_is_little_endian,
			  "Return `T' if the current platform is little-endian.",
			  NULL);
  
    emacs_value def_is_be_args[] = {is_be_symbol, is_be_func};
    emacs_value def_is_le_args[] = {is_le_symbol, is_le_func};
  
    eenv->funcall(eenv, eenv->intern(eenv, "defalias"), 2, def_is_be_args);
    eenv->funcall(eenv, eenv->intern(eenv, "defalias"), 2, def_is_le_args);
  }
  return 0;			/* All is well. */
}

#endif /* _PLATEL_EMACS_MODULE_C */

/* end of file -- platel.c */
