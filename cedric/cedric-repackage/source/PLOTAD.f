      SUBROUTINE PLOTAD(ID,RBUF,NX,NY,PLWIND,BAD,ZLEV,NAMF,SCL,ISPAC,
     X                  NDIG,NSYM,WIDTH,ZREF,CSP,NCX,NCWORD,ITIT,ITYP,
     X                  ISKPI,ISKPJ,LABFLG)
C
C        DIGITIZES EITHER ALPHANUMERIC OR DIGITAL INFORMATION ONTO
C                  A MAP BACKGROUND DEPENDING UPON THE VALUE OF ITYP:
C                  IF ITYP=1, ALPHANUMERIC PLOT
C                         =2, DIGITAL PLOT
C

      INCLUDE 'CEDRIC.INC'
      PARAMETER (NDSYM=27)
      DIMENSION ID(NID),RBUF(NX,NY),PLWIND(2,3),CSP(3,3),NCX(3),
     X          NCWORD(3),NSYM(2),MAXSYM(2)
      CHARACTER*2 NAMF(4)
      CHARACTER*80 LABEL
      CHARACTER*16 CTEMP
      CHARACTER*80 CITIT
      CHARACTER*16 CFMTX
      CHARACTER*16 CFMTY
      CHARACTER *8 NOWDAT,IFMTX(2),IFMTY(2),ITIT(5)
      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      CHARACTER*4 AXNAM
      COMMON /RANGEC/ XBEG,XEND,YBEG,YEND,XRANGE(2),YRANGE(2),DELRG(2)
      COMMON /SYMTAB/ ISYM(NDSYM,2)
      COMMON /OVRLAY/ IOLAY,NPOLAY(5,MXVSCT),ICOLAY(10)
      CHARACTER*1 ISYM,IAXIS(3)
      CHARACTER KHAR*8,KNOT*1,IDOT*1,IBL*1
      DATA MAXSYM/ 26, 26 /
      DATA IAXIS/'X','Y','Z'/
      DATA IDOT/'.'/
      DATA IBL/ ' ' /
      DATA SIZ/8.0/

      LOCPLT(R)=1023.*R
      CALL GSCLIP(0)
C
C     TRANSFER ITIT INFO TO CHAR ARRAY 
C
      WRITE (CITIT(1:80),530)(ITIT(I),I=1,5),BLANK
 530  FORMAT(5A8,40A1)
      print *,'PLOTAD:  itit=',itit
      print *,'PLOTAD: citit=',citit,'X'
      
C
C        INITIALIZE PLOTTING INFO AND SET UP SCALING
C
      L1=NCWORD(1)
      L2=NCWORD(2)
      L3=NCWORD(3)
      CALL RGINI(XL,XR,YB,YT,PLWIND,CSP,NCX,NCWORD)
      IF(IOLAY.EQ.2) GO TO 10
C
C        FULL PLOT LABELING
C
      CALL MAJMIN(XRANGE(1),DELRG(1),IFMTX,MAJORX,MINORX,NDIG1,ISZ1,
     X            YRANGE(1),DELRG(2),IFMTY,MAJORY,MINORY,NDIG2,ISZ2)
c      CALL MAJMIN(DELRG(1),IFMTX,MAJORX,MINORX)
c      CALL MAJMIN(DELRG(2),IFMTY,MAJORY,MINORY)
      CALL SET(XL,XR,YB,YT,XRANGE(1),XRANGE(2),YRANGE(1),YRANGE(2),1)
      WRITE (CFMTX,520)IFMTX
      WRITE (CFMTY,520)IFMTY

C     Always draw grid and grid labels
C
      CALL LABMOD(CFMTX,CFMTY,NDIG1,NDIG2,ISZ1,ISZ2,4,4,0)
      CALL PERIML(MAJORX,MINORX,MAJORY,MINORY)

      CALL DATEE(NOWDAT)
      CF=1./ID(69)
      XOR=ID(40)*CF
      SF=1./ID(68)
      XREL=ID(41)*SF
      YREL=ID(42)*SF
      WRITE (LABEL,101)(ID(I),I=116,121),(ID(I),I=125,127),
     X                     (ID(I),I=13,15),AXNAM(L3),
     X                     ZLEV,LABAXS(L3,IUNAXS),(NAMF(I),I=1,4)
  101 FORMAT(I2.2,'/',I2.2,'/',I2.2,6X,I2.2,2(':',I2.2),'-',I2.2,
     X     2(':',I2.2),7X,3A2,7X,A2,'=',F7.2,' ',A4,6X,4A2)
      IF (LABFLG.GT.5) THEN
         CALL MY_PLCHMQ(60,1010,LABEL,12.,0.,-1.)
         WRITE (LABEL,102)NOWDAT
 102     FORMAT('(AS OF ',A8,')')
         CALL MY_PLCHMQ(10,985,LABEL(1:16),12.,0.,-1.)
         WRITE (LABEL,106)XREL,YREL,XOR
 106     FORMAT('ORIGIN=(',F7.2,',',F7.2,') KM   X-AXIS=',F5.1,' DEG')
         CALL MY_PLCHMQ(430,985,LABEL(1:46),12.,0.,-1.)
         CALL MY_PLCHMQ(200,960,CITIT(1:40),12.,0.,-1.)
         WRITE (LABEL,103)AXNAM(L2), LABAXS(L2,IUNAXS)
 103     FORMAT(A1,' ',A4)
      END IF
      LOCY=LOCPLT(YB+(YT-YB)*0.15)
      IF (LABFLG.GT.5) THEN
         CALL MY_PLCHMQ(10,LOCY,LABEL(1:16),12.,90.,-1.)
      END IF
      WRITE (LABEL,103)AXNAM(L1), LABAXS(L1,IUNAXS)
      LOCY=LOCPLT(YB)-50
      IF (LABFLG.GT.5) THEN
         CALL MY_PLCHMQ(150,LOCY,LABEL(1:6),12.,0.,-1.)
      END IF
   10 CONTINUE
C
C        SUPPLY ESSENTIAL LABELING AND GENERATE PLOT
C
      INC=ISPAC
      IF(INC.GE.MAXPLN) INC=INC/MAXPLN
      WRITE (LABEL,108)INC
  108 FORMAT('EVERY ',I2,' POINT IS PLOTTED')
      IF(IOLAY.EQ.2) THEN
         IF (LABFLG.GT.5) THEN
            CALL MY_PLCHMQ(125,880,LABEL(1:25),12.,0.,-1.)
            WRITE (LABEL,105)(NAMF(I),I=1,4)
 105        FORMAT('OVERLAY FIELD IS ',4A2)
            CALL MY_PLCHMQ(500,880,LABEL(1:25),12.,0.,-1.)
         END IF
      ELSE
         IF (LABFLG.GT.5) THEN
            CALL MY_PLCHMQ(275,LOCY,LABEL(1:25),12.,0.,-1.)
         END IF
      END IF
      CALL SET(XL,XR,YB,YT,XBEG,XEND,YBEG,YEND,1)
      WRITE (LABEL,107)SCL
  107 FORMAT('( X ',F7.2,' )')
      IF (LABFLG.GT.5) THEN
         CALL MY_PLCHMQ(230,985,LABEL(1:13),12.,0.,-1.)
      END IF
C
C        PLOT BACKGROUND HAS BEEN GENERATED- PROCEED WITH DISPLAY
C
      IF(ITYP.EQ.1) THEN
C
C        ALPHA DISPLAY
C
C
C           ENCODE REFERENCE INFO- VALUES FIRST
C
         LOCX=LOCPLT(XR)+5
         IF(IOLAY.EQ.2) LOCX=LOCX+80
         LOCY=925
           X=ZREF-(WIDTH*NSYM(1))
         TOP=ZREF+(WIDTH*NSYM(2))
   15    CONTINUE
            WRITE (LABEL,104)X
  104       FORMAT(F6.1)
            CALL MY_PLCHMQ(LOCX,LOCY,LABEL(1:6),8.,0.,-1.)
            X=X+WIDTH
            LOCY=LOCY-17
         IF(X.LE.TOP) GO TO 15
C
C           PUT SYMBOLS NEXT TO THE VALUES
C
            LOCX=LOCX+60
            LOCY=934
            KHAR=ISYM(NDSYM,1)
            CALL MY_PLCHMQ(LOCX,LOCY,KHAR(1:1),8.,0.,0.)
            DO 25 K=1,2
               LAST=NSYM(K)
               IF(LAST.LE.0) GO TO 25
               DO 20 I=1,LAST
                  J=IABS((2-K)*(LAST+1)-I)
                  LOCY=LOCY-17
                  KHAR=ISYM(J,K)
                  CALL MY_PLCHMQ(LOCX,LOCY,KHAR(1:1),
     X                 8.,0.,0.)
   20          CONTINUE
   25       CONTINUE
            LOCY=LOCY-17
            KHAR=ISYM(NDSYM,2)
            CALL MY_PLCHMQ(LOCX,LOCY,KHAR(1:1),8.,0.,0.)
            WIDINV=1./WIDTH
      ELSE
C
C        DIGITAL DISPLAY
C
         IPDIV=10**NDIG
         INDIV=IPDIV/10
         WRITE (CTEMP,201)NDIG
         READ (CTEMP,500)IFMTX(1)
 500     FORMAT(A8)
         READ (CTEMP,510)IFMTX(2)
 510     FORMAT(8X,A2)
C         ENCODE(10,201,IFMTX) NDIG
  201    FORMAT(6X,'(I',I1,')')
      END IF
C
C        PUT THE APPROPRIATE SYMBOLS ON THE MAP BACKGROUND
C
      IF(IABS(ISPAC).GE.MAXPLN) THEN
         INC=ISPAC/MAXPLN
         KNOT=IBL
      ELSE
         INC=ISPAC
         KNOT=IDOT
      END IF
      ILV=-1
      IF(INC.LE.0) THEN
         ILV=IABS(INC+1)
         INC=1
      END IF
C
C        CALCULATE THE HEIGHT OF 1/2 CHARACTER AT SIZE=0
C
      HAFCHR=((YEND-YBEG)/(YT-YB)) * 0.0078125 * 0.5
      DO 60 J=1,NY,(INC*ISKPJ)
         RJ=J
         IF(RJ.LT.YBEG.OR.RJ.GT.YEND) GO TO 60
         DO 55 I=1,NX,(INC*ISKPI)
            RI=I
            IF(RI.LT.XBEG.OR.RI.GT.XEND) GO TO 55
            IF(ILV.GE.0.AND.MOD(IABS(I-J),2).NE.ILV) GO TO 55
            KHAR=KNOT
            NC=1
            X=RBUF(I,J)
            ADY=HAFCHR
            IF(X.EQ.BAD) THEN
               IF(KNOT.EQ.IBL) GO TO 55
               GO TO 40
            END IF
            ADY=0.0
            IF(ITYP.NE.1) GO TO 35
C
C              ALPHA SYMBOL- GOOD VALUE
C
               X=(X-ZREF)*SCL
               K=2.0+SIGN(0.5,X)
               X=ABS(X)
               ITEST=X*WIDINV+1.0
               IF(ITEST.GT.NSYM(K)) ITEST=NDSYM
               KHAR=ISYM(ITEST,K)
               GO TO 40
   35       CONTINUE
C
C              DIGITAL SYMBOL- GOOD VALUE
C
               NUM=X*SCL+0.5
               IF(X.LT.0.0) NUM=NUM-1
               IF(NUM.GE.0) THEN
                  ITEST=NUM/IPDIV
                  IF(ITEST.NE.0) THEN
                     KHAR=ISYM(NDSYM,2)
                  ELSE
                     NC=NDIG
                     WRITE (CTEMP,520)IFMTX
 520                 FORMAT(2A8)
                     WRITE (KHAR,CTEMP)NUM
C                     ENCODE(NDIG,IFMTX,KHAR) 
                  END IF
               ELSE
                  ITEST=NUM/INDIV
                  IF(ITEST.NE.0) THEN
                     KHAR=ISYM(NDSYM,1)
                  ELSE
                     NC=NDIG
                     WRITE (CTEMP,520)IFMTX
                     WRITE (KHAR,CTEMP)NUM
C                     ENCODE(NDIG,IFMTX,KHAR) NUM
                  END IF
               END IF
   40       CONTINUE
            CALL PLCHMQ(RI,RJ+ADY,KHAR(1:NC),SIZ,0.,0.)
   55    CONTINUE
   60 CONTINUE
      IF(IOLAY.NE.1) THEN
         IF (LABFLG.GT.5) THEN
            CALL MYFRAME
         ELSE
            CALL FRAME
         END IF
      END IF
      RETURN
      END
