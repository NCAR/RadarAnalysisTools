#include <stdio.h>

/* the following function takes two bytes (chars) as arguments and returns
 * an integer value by combining them.
 */
int two_bytes_2_int(c1,c2)
     char c1,c2;
{
  int it1,it2,ival;

  it1 = c1;
  if (it1 < 0) it1 = it1 + 256;
  it1 = it1 << 8;

  it2 = c2;
  if (it2 < 0) it2 = it2 + 256;

  ival = it1 | it2;
/* make negative, if necessary, for machines with word sizes > 16 bits */
  if (ival >= 32768) ival = ival - 65536; 

  return ival;
}
