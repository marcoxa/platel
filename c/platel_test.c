/* -*- Mode: C -*- */

/* platel_test.c
 *
 * See file COPYING in the top directory for copyright and licensing
 * information.
 */

#ifndef _PLATEL_TEST_C
#define _PLATEL_TEST_C

#include <stdio.h>
#include "platel.h"

int main() {

  if (platel_is_bigendian())
    printf("BIG\n");
  else
    printf("little\n");
}


#endif /* _PLATEL_TEST_C */

/* end of file -- platel.c */
