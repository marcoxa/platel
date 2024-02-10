/* -*- Mode: C -*- */

/* platel.c
 *
 * See file COPYING in the top directory for copyright and licensing
 * information.
 */

#ifndef _PLATEL_C
#define _PLATEL_C

#include "platel.h"

_Bool platel_is_littleendian() {
  int one = 1;

  return (_Bool) *((char *) &one);
}


_Bool platel_is_bigendian() {
  return ! platel_is_littleendian();
}

#endif /* _PLATEL_C */

/* end of file -- platel.c */
