      SUBROUTINE BLHED(IBUF,LEV,N3,ZLEV,VNYQ,NST)
C
C        BUILDS, PACKS AND WRITES 10 WORD LEVEL HEADER
C
      INCLUDE 'CEDRIC.INC'
      PARAMETER (LDIM=10)
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X                IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X                NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      CHARACTER*2 CTEMP
      DIMENSION IBUF(LDIM)

C     LJM - May need to change ZSCALE to 100.0 for some SPRINT files
C           that used GRIDPPI and elevation angle > 32.768 deg.
C
      DATA ZSCALE /1000.0/

      print *,'BLHED: lev,zlev,vnyq=',lev,zlev,vnyq
      NST=0
      CTEMP='LE'
      READ(CTEMP,10)IBUF(1)
 10   FORMAT(A2)
      CTEMP='VE'
      READ(CTEMP,10)IBUF(2)
      CTEMP='L '
      READ(CTEMP,10)IBUF(3)
C      IBUF(1)='LE'
C      IBUF(2)='VE'
C      IBUF(3)='L '
C      IBUF(4)=(CSP(1,N3)+(LEV-1)*CSP(3,N3))*ID(68)
      IBUF(4)=ZLEV*ZSCALE
      IBUF(5)=LEV
      IBUF(6)=NFL
      IBUF(7)=NPLANE
      IBUF(8)=ID(96)
      IBUF(9)=ID(97)
      IBUF(10)=VNYQ*100.0
      CALL SBYTES(IBUF,IBUF,0,16,0,LDIM)
      CALL CWRITE(IBUF,LDIM,NST)
      RETURN
      END
