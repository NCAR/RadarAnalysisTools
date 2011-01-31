      SUBROUTINE VERSOUT
C
C     THIS SUBROUTINE OUTPUTS THE DATE THE EXECUTABLE WAS CREATED
C
      INCLUDE 'SPRINT.INC'
c      PARAMETER (MAXSKP=27,MXCNT=500)

      WRITE(*,10)
 10   FORMAT(/
     X     '---SPRINT: Sorted Position Radar Interpolation ',/,
     X     '---UCAR/NCAR MMM and EOL Divisions ',/,
     X     '---COPYRIGHT (C) 1993 1995 1997 1999-2009',/,
     X     '---UNIVERSITY CORPORATION FOR ATMOSPHERIC RESEARCH',/,
     X     '---ALL RIGHTS RESERVED.',/,
     X     '---RELEASE DATE: November 24, 2010'/)

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
      WRITE(*,51)LFTIM,JRH6,JRH7,JRH8,IBAD
 51   FORMAT(6x,'Other variables set in UFCART: LFTIM,JRH6-7-8,IBAD = ',
     +     I2,I3,I4,I5,I7)
      WRITE(*,53)DEBUG,DEBUGIT
 53   FORMAT(6x,'Logical debugging variables set in UFCART:',
     +     ' DEBUG,DEBUGIT = ',2I2)
      WRITE(*,55)
 55   FORMAT(/,6x,
     + 'SPRINT outputs these fortran units with debugging information')
      WRITE(*,57)
 57   FORMAT(/,6x,'fort.7 = Input radar beam information')
      WRITE(*,59)
 59   FORMAT(6x,'fort.8 = Debugging information (DEBUG=.TRUE.)')
      WRITE(*,61)
 61   FORMAT(6x,'fort.9 = Debugging information for Dorade sweep files')

      WRITE(*,99)
 99   FORMAT(/)
      RETURN
      END



