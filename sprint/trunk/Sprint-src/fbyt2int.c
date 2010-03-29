#include <stdio.h>

/* the following function takes 4 bytes as arguments and constructs
 * a 4 byte integer or float out of them. 
 */
int four_bytes_2_int(c1,c2,c3,c4) 
     char c1,c2,c3,c4;
{
  int it1,it2,it3,it4;
  unsigned int ival;

  it1 = c1;
  if (it1 < 0) it1 = it1 + 256;
  it1 = it1 << 24;

  it2 = c2;
  if (it2 < 0) it2 = it2 + 256;
  it2 = it2 << 16;

  it3 = c3;
  if (it3 < 0) it3 = it3 + 256;
  it3 = it3 <<  8;

  it4 = c4;
  if (it4 < 0) it4 = it4 + 256;

  ival = it1 | it2 | it3 | it4;

/* make negative, if necessary, for machines with word sizes bigger than 32 */
/* the first big number is 2**31 and the second is 2**32 */
  if (ival >= 2147483648) ival = ival - 4294967296;

  return ival;
}
