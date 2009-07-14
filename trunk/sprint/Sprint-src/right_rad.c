#include <stdio.h>
#include "dorade.h"

/* This function determines if the beam just read in is from the requested
 * radar. A kludge has to be added for eldora ff since the radar names
 * on the tape aren't correct.
 */
int right_rad(radnam1, radnam2, tilt)
     char *radnam1, *radnam2;
     float tilt;
{
  if (strcmp(radnam1, "FORE") == 0) return 0;

  if (strcmp(radnam2, "") == 0) {
    return 1;
  }
  else if (strcmp(radnam2, "FORE") == 0) {
    if (tilt > 0.0) {
      return 1;
    }
    else {
      return 0;
    }
  }
  else if (strcmp(radnam2, "AFT") == 0) {
    if (tilt < 0.0) {
      return 1;
    }
    else {
      return 0;
    }
  }

  return;
}

