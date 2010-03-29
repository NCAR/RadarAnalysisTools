c
c----------------------------------------------------------------------X
c
      SUBROUTINE VERDATE(LABSTDIN)
C
C     Prints out copyright and version date and passes 
C     LABSTDIN back to main program for copyright frame.
C
      CHARACTER*60 LABSTDIN(12)

      WRITE(6,3)
    3 FORMAT(///,
     + '************************************************************',/,
     + '*  NCAR/MMM program to do radar data display and analysis  *',/,
     + '*  Copyright (C) 1993-2004       - All Rights Reserved     *',/,
     + '*  University Corporation for Atmospheric Research (UCAR)  *',/,
     + '*  Release date:  August 5, 2004                           *',/,
     + '************************************************************',/)
      WRITE(LABSTDIN,3)

      RETURN
      END
