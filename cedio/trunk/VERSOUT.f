      SUBROUTINE VERSOUT
C
C     THIS SUBROUTINE OUTPUTS THE DATE THE EXECUTABLE WAS CREATED
C
      WRITE(*,10)
 10   FORMAT(/'---CEDRIC: Custom Editing and Display of Reduced ',
     X     'Information in Cartesian space',/,
     X     '---NCAR/Mesoscale and Microscale Division ',/,
     X     '---COPYRIGHT (C) 1993 1995 1997 1999-2004',/,
     X     '---UNIVERSITY CORPORATION FOR ATMOSPHERIC RESEARCH',/,
     X     '---ALL RIGHTS RESERVED.',/,
     X     '---RELEASE DATE: July 21, 2004'/)

      RETURN

      END
