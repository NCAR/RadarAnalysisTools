      SUBROUTINE MY_PLCHMQ(IX,IY,LABEL,SIZE,ANGLE,POSIT)
C
C Plot a character string at fractional locations after 
C after conversions from user-specified integer locations.
C     IX - User-specified integer location (1 --> 1024) in X direction
C     IY - User-specified integer location (1 --> 1024) in Y direction
C     LABEL  - Character string
C     LENGTH - Length of character string
C     SIZE   - Size of characters (typically 12)
C     ANGLE  - Angle relative to positive x-axis 
C              ( 0) plots string parallel to x-axis (horizontal)
C              (90) plots string normal to x-axis (vertical)
C     POSIT  - Character string is plotted, starting at
C              (-1) - Beginning of character string
C              ( 0) - Middle of character string
C              ( 1) - End of character string
C
      INTEGER IX,IY
      REAL SIZE,ANGLE,POSIT
      CHARACTER LABEL*80

c      print *,'MY_PLCHMQ: LABEL=',LABEL
c      print *,'MY_PLCHMQ: SIZE,ANGLE,POSIT=',SIZE,ANGLE,POSIT
c      print *,'MY_PLCHMQ: IX,IY=',IX,IY
      LL=1
      CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LL)
c      print *,'MY_PLCHMQ: ul,ur,ub,ut=',ul,ur,ub,ut
c      print *,'MY_PLCHMQ: fl,fr,fb,ft=',fl,fr,fb,ft
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)

c      WRITE(LABL,105)LABELJOBNUM,TODAY,TYME,NFRAME
c105   FORMAT(A8,'(',A8,'--',A8,')--FRAME=',I4)
      FX=IX/1024.
      FY=IY/1024.
c     print *,'MY_PLCHMQ: FX,FY=',FX,FY
      CALL PLCHMQ(FX,FY,LABEL,SIZE,ANGLE,POSIT)
      CALL SET(FL,FR,FB,FT,UL,UR,UB,UT,1)

      RETURN
      END
