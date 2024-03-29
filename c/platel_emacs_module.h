/* -*- Mode: C -*- */

/* platel_emacs_module.h
 *
 * See file COPYING in the top directory for copyright and licensing
 * information.
 */

#ifndef _PLATEL_EMACS_MODULE_H
#define _PLATEL_EMACS_MODULE_H

#include <emacs-module.h>
#include "platel.h"

extern int plugin_is_GPL_compatible;

extern int emacs_module_init(struct emacs_runtime *);

#endif /* _PLATEL_EMACS_MODULE_H */

/* end of file -- platel.h */
