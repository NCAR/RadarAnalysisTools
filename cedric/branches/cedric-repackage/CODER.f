      SUBROUTINE CODER(IOUT,RBUF,NX,NY,BAD,IWIND,PLWIND,SCLDSP,
     X                 ISYM,NSYM,WIDTH,ZREF,NCWORD,ZLEV,NAMF)
C
C        PRODUCES A CODED DISPLAY ON EITHER A TERMINAL OR THE
C                 PRINT FILE.
C
      PARAMETER (NDSYM=27)
      DIMENSION RBUF(NX,NY),IWIND(2,3),PLWIND(2,3),NSYM(2),NCWORD(3)
      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      CHARACTER*4 AXNAM
      CHARACTER*2 NAMF(4)
      CHARACTER LABI*130, LINE*130, ICEN*3
      CHARACTER*1 ISYM(NDSYM,2)
      CHARACTER*1 IBL,IDOT,ICOL,IPL,IEQ
      DATA IDOT/'.'/, ICOL/':'/, IPL/'+'/, IEQ/'='/
      DATA IBL/' '/
      DATA NTAB/5/
      DATA NDIG/1/
      DATA NI,NJ/125,1024/
      IFIXAX=NCWORD(3)
      II=NCWORD(1)
      I1=IWIND(1,II)
      I2=IWIND(2,II)
      IJ=NCWORD(2)
      J1=IWIND(1,IJ)
      J2=IWIND(2,IJ)
C
C        INITIALIZE FOR THE ENTIRE DISPLAY
C
      MAXAC=NI/NDIG
      LOOP=0
      WIDINV=1./WIDTH
      ILAST=I1-1
   10 CONTINUE
C
C        GENERATE NEXT SECTION OF THE DISPLAY
C
      IFRST=ILAST+1
      ILAST=ILAST+MAXAC
      IF(ILAST.GT.I2) ILAST=I2
      NGO=ILAST-IFRST+1
      LASCOL=NTAB+NGO*NDIG
      L=J2
      LOOP=LOOP+1
   15 CONTINUE
C
C        GENERATE NEXT PAGE OF DISPLAY
C
      WRITE(IOUT,105) I1,I2,PLWIND(1,II),PLWIND(2,II),LABAXS(II,IUNAXS),
     X                J1,J2,PLWIND(1,IJ),PLWIND(2,IJ),LABAXS(IJ,IUNAXS)
  105 FORMAT(//8X,'DISPLAY RANGES FROM (',I3,' TO ',I3,')   (',F7.2,
     X         ' TO ',F7.2,1X,A4,') ALONG I'/19X,'AND FROM (',I3,
     X         ' TO ',I3,')   (',F7.2,' TO ',F7.2,1X,A4,') ALONG J')
      WRITE(IOUT,100) NAMF,SCLDSP,AXNAM(IFIXAX),ZLEV,
     X                LABAXS(IFIXAX,IUNAXS),LOOP
  100 FORMAT(1X,4A2,' ( X ',F6.1,')',34X,A2,'=',F7.2,' ',A4,
     X         3X,'P.',I2)
      KNT=2
   20 CONTINUE
C
C        GENERATE NEXT LINE OF DISPLAY
C
      LINE=IBL
      IF(MOD(L,5).EQ.0) THEN
        WRITE (LINE,101)L
  101   FORMAT(1X,I3,'+')
      ELSE
        LINE(5:5)=ICOL
      END IF
      IPOS=IFRST
      DO 25 I=1,NGO
      LOC=NTAB + I*NDIG
      IF(RBUF(IPOS,L).EQ.BAD) THEN
        LINE(LOC:LOC)=IDOT
      ELSE
        RNUM=(RBUF(IPOS,L)-ZREF)*SCLDSP
        K=2.0 + SIGN(0.5,RNUM)
        RNUM=ABS(RNUM)
        ITEST=RNUM*WIDINV + 1.0
        IF(ITEST.GT.NSYM(K)) ITEST=27
        LINE(LOC:LOC)=ISYM(ITEST,K)
      END IF
      IPOS=IPOS+1
   25 CONTINUE
      WRITE(IOUT,102) LINE(1:LASCOL)
  102 FORMAT(A)
      L=L-1
      IF(L.LT.J1) GO TO 30
      KNT=KNT+1
      IF(KNT.LT.NJ) GO TO 20
   30 CONTINUE
C
C        GENERATE AXIS LABEL AT BOTTOM OF PAGE
C
      LABI=IBL
      LINE=IBL
      IPOS=IFRST
      DO 35 I=1,NGO
      LOC=NTAB + I*NDIG
      IF(MOD(IPOS,5).NE.0) THEN
        LINE(LOC:LOC)=IEQ
      ELSE
        LINE(LOC:LOC)=IPL
        WRITE (ICEN,103)IPOS
  103   FORMAT(I3)
        LABI(LOC-2:LOC)=ICEN(1:3)
      END IF
      IPOS=IPOS+1
   35 CONTINUE
      WRITE(IOUT,102) LINE(1:LASCOL)
      WRITE(IOUT,102) LABI(1:LASCOL)
      IF(L.GE.J1) GO TO 15
      IF(ILAST.LT.I2) GO TO 10
      RETURN
      END
