      SUBROUTINE VERSOUT
C
C     THIS SUBROUTINE OUTPUTS THE DATE THE EXECUTABLE WAS CREATED
C
      INCLUDE 'SPRINT.INC'
c      PARAMETER (MAXSKP=27,MXCNT=500)

      WRITE(*,10)
 10   FORMAT(/
     X     '---SPRINT: Sorted Position Radar Interpolation ',/,
     X     '---NCAR/Mesoscale and Microscale Division ',/,
     X     '---COPYRIGHT (C) 1993 1995 1997 1999-2004',/,
     X     '---UNIVERSITY CORPORATION FOR ATMOSPHERIC RESEARCH',/,
     X     '---ALL RIGHTS RESERVED.',/,
     X     '---RELEASE DATE: May 11, 2009'/)

      WRITE(*,11)
 11   FORMAT('---SPRINT.INC parameters:')
      WRITE(*,13)INT(WORDSZ)
 13   FORMAT(6X,'Computer word size = ',I2)
      WRITE(*,15)INTSZ
 15   FORMAT(6X,'Word size of interpolated values = ',I2)
      WRITE(*,17)INTSZ,MAXWRD
 17   FORMAT(6X,
     x'Maximum number of ',I2,'-bit words contained in 64-bit word = ',
     x     I2)
      WRITE(*,19)IDIM
 19   FORMAT(6X,
     x'First dimension used in packing/unpacking memory allocation = ',
     x     I2)
      WRITE(*,21)IDIM2
 21   FORMAT(6X,
     x'Second dimension used in packing/unpacking memory allocation = ',
     x     I2)
      WRITE(*,23)MAXFLD
 23   FORMAT(6X,
     x'Maximum number of (inp/out) fields that can be processed = ',
     x     I2)
      WRITE(*,25)MXCRT
 25   FORMAT(6X,'Maximum number of grid points along any one axis = ',
     x     I3)
      WRITE(*,27)MAXPLN
 27   FORMAT(6X,'Maximum number of grid points in a plane = ',I6)
      WRITE(*,29)MAXZ
 29   FORMAT(6X,'Maximum number of interpolated (2D) levels = ',I2)
      WRITE(*,31)MAXYZ
 31   FORMAT(6X,'Maximum number of interpolated (3D) values = ',I8)
      WRITE(*,33)MAXRNG
 33   FORMAT(6X,'Maximum number of input range gates = ',I4)
      WRITE(*,35)MAXEL
 35   FORMAT(6X,'Maximum number of input sweeps (scans) = ',I3)
      WRITE(*,37)MAXSKP
 37   FORMAT(6X,'Maximum number of fixed angle (FXTABLE) entries = ',I3)
      WRITE(*,39)MXCNT
 39   FORMAT(6X,'Maximum number of calibration file entries = ',I3)
      WRITE(*,41)MAXIN,NIOB
 41   FORMAT(6X,'Required space for internal disk file = ',2I8)
      WRITE(*,43)NRCBF,IVDIM
 43   FORMAT(6X,'Parameters for scratch array memory allocation = ',2I8)
      WRITE(*,45)NID
 45   FORMAT(6x,'Size of housekeeping array (ID) for each beam = ',I3)
      WRITE(*,47)IDPTR_INT
 47   FORMAT(6x,'Initial pointer for field name info in ID = ',I3)
      WRITE(*,49)IPTR_INT
 49   FORMAT(6x,'Initial pointer for beam angle info in ID = ',I3)

      WRITE(*,99)
 99   FORMAT(/)
      RETURN
      END



