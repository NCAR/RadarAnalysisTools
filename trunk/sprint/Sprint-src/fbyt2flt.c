#include <stdio.h>
#include "dorade.h"

/* The following function takes four bytes that are parts of a
 * 32-bit IEEE floating point word and builds a floating point
 * word on the current machine. Note, so far, only the Cray requires
 * special action because it's the only one that doesn't use IEEE.
 */
float four_bytes_2_flt(c1,c2,c3,c4)
     char c1,c2,c3,c4;

{
  int it1,it2,it3,it4,ival;
  int type, num, bitoff, stride, ierr;
  float cfval;
  union fltint
    {
      float fval;
      int   ival;
    } mixed;
  

    it1 = c1;
    if (it1 < 0) it1 = it1 + 256;
    it1 = it1 << 24;

    it2 = c2;
    if (it2 < 0) it2 = it2 + 256;
    it2 = it2 << 16;

    it3 = c3;
    if (it3 < 0) it3 = it3 + 256;
    it3 = it3 << 8;

    it4 = c4;
    if (it4 < 0) it4 = it4 + 256;

    mixed.ival = it1 | it2 | it3 | it4;

  if (IEEEFLOAT) {
    return mixed.fval;
  }

  else {
    mixed.ival = mixed.ival << 32;
    type = 2;
    num  = 1;
    bitoff = 0;
    stride = 0;
    ierr = 0;

    /* convert IEEE 32-bit float to Cray 64-bit float */
/*    ierr = IEG2CRAY(&type, &num, &(mixed.ival), &bitoff, &cfval, &stride); */
    return cfval;
  }

}

    

  
