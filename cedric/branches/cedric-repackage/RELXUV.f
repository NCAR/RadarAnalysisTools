      SUBROUTINE RELXUV(U,V,DERR,DWDZ,XYDELI,NX,NY,
     X                  RPAR,BAD,ITER,DMN,KST,CSP,L,ZLEV)
C
C        ITERATIVE ADJUSTMENT OF THE U,V FIELDS TO MINIMIZE THE
C        MEAN DIFFERENCE BETWEEN THE HORIZONTAL AND VERTICAL CONVERGENCE.
C        THIS ROUTINE IS CALLED FOR EACH LEVEL TO BE PROCESSED.
C
C          U- U-COMPONENT FIELD TO ADJUST           (INPUT/OUTPUT)
C          V- V-COMPONENT FIELD TO ADJUST           (INPUT/OUTPUT)
C       DERR- SCRATCH FIELD (USED FOR HOR-CONV AND DIFF FIELDS)
C       DWDZ- VERTICAL CONVERGENCE FIELD               (INPUT)
C     XYDELI- INVERSE OF THE X AND Y SPACINGS IN KM.   (INPUT)
C         NX- # VALUES ALONG X-AXIS                    (INPUT)
C         NY- # VALUES ALONG Y-AXIS                    (INPUT)
C       RPAR- CONTROLLING PARAMETERS FOR PROCEDURE...  (INPUT)
C             1- RELAXATION COEFFICIENT
C             2- MEAN DIFFERENCE TO ITERATE TO
C             3- MAX. # OF ITERATIONS
C        BAD- MISSING DATA VALUE                       (INPUT)
C       ITER- NUMBER OF ACTUAL ITERATIONS              (OUTPUT)
C        DMN- MEAN DIFFERENCE ACTUALIZED               (OUTPUT)
C        KST- STATUS FLAG...                           (OUTPUT)
C             0- CONVERGENCE CRITERIA SATISFIED
C             1- FAILED TO CONVERGE (DMN INCREASED OR MAX. ITER EXCEEDED)
C             2- NO DATA AT THIS LEVEL
C
      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      CHARACTER*4 AXNAM
      DIMENSION U(NX,NY),V(NX,NY),DERR(NX,NY),DWDZ(NX,NY),
     X          XYDELI(2),RPAR(3),CSP(3,3)
C
      IACTC=0
      ITER=0
      RELAX= RPAR(1)
      ERRMAX=RPAR(2)
      ITRMAX=RPAR(3)
      COEFX=RELAX/(2.0*XYDELI(1))
      COEFY=RELAX/(2.0*XYDELI(2))
      NXM1=NX-1
      NYM1=NY-1
      DMPREV=1.E+8
C
    1 CONTINUE
C
C        ITERATION LOOP
C
         CNT=0.0
         DMN=0.0
         DSQR=0.0
         DMAX=1.0E-35
         DMIN=1.0E+35
C
C           COMPUTE HORIZONTAL CONVERGENCE
C
         CALL PCONVG(U,V,DERR,NX,NY,3,XYDELI,BAD,CSP,IACTC)
C
C           CALCULATE THE DIFFERENCE FIELD
C
         DO 10 J=1,NY
            DO 5 I=1,NX
               IF(DERR(I,J).NE.BAD.AND.DWDZ(I,J).NE.BAD) THEN
                  DERR(I,J)=DWDZ(I,J)-DERR(I,J)
C
C                  MOD TO USE MEAN OF SIGNED DIFFERENCE FIELD AS TEST
C
C                  DMN=DMN+ABS(DERR(I,J))
                  DMN=DMN+DERR(I,J)
                  CNT=CNT+1.0
                  IF (ABS(DERR(I,J)).LT.DMIN) DMIN=ABS(DERR(I,J))
                  IF (ABS(DERR(I,J)).GT.DMAX) DMAX=ABS(DERR(I,J))
                  DSQR=DSQR+DERR(I,J)**2
               ELSE
                  DERR(I,J)=BAD
               END IF
    5       CONTINUE
   10    CONTINUE
C
C           TEST FOR CONVERGENCE
C
         IF(CNT.EQ.0.0) GO TO 92
C         MOD -ABS VALUE MUST BE TAKEN WHEN USING SIGNED DIFFERENCE FIELD
C         DMN=DMN/CNT
         DMN=ABS(DMN/CNT)
         DSQR=DSQR/CNT
         DSQR=DSQR-DMN**2.0
         DSQR=SQRT(DSQR)
         IF(ITER.GE.0) THEN
            IF((DMN.LE.ERRMAX .AND. ITRMAX.GT.0) .OR.
     X          (DMN.LE.ERRMAX .AND. ITRMAX.LT.0 .AND. 
     X           ITER.GE.IABS(ITRMAX)))  GO TO 90
            IF(ITER.GE.IABS(ITRMAX)) GO TO 91
            IF((DMN-DMPREV).GE.ERRMAX .AND. ITRMAX.GT.0)  GO TO 91
         END IF
C
         IF (ITER.EQ.0) THEN
            IF (L.EQ.1) THEN
               WRITE(6,154)AXNAM(3),LABAXS(3,1)
 154           FORMAT(/4X,A1,' LEVEL (',A4,')   ITER #     N     ',
     X            'MEAN DIFF   ','      STDV     MIN DIFF     ',
     X            'MAX DIFF','      CONVERGED')
            ELSE
               WRITE(6,129)
 129           FORMAT(/4X,1X,'         ',4X,'   ITER #     N     ',
     X           'MEAN DIFF   ','      STDV     MIN DIFF     MAX DIFF',
     X           '      CONVERGED')
            END IF
            WRITE(6,156)L,ZLEV,ITER,NINT(CNT),DMN,DSQR,DMIN,DMAX,'NO'
 156        FORMAT(1X,I3,F12.2,3X,I6,3X,I5,5X,4(E9.2,4X),A8)
         ELSE
            WRITE(6,159)ITER,NINT(CNT),DMN,DSQR,DMIN,DMAX,'NO'
 159        FORMAT(19X,I6,3X,I5,5X,4(E9.2,4X),A8)
         END IF
         ITER=ITER+1
         DMPREV=DMN
C
C           ADJUST THE U,V FIELDS
C
         DO 20 J=2,NYM1
            DO 15 I=2,NXM1
               IF(DERR(I,J).NE.BAD .AND.
     X            U(I-1,J).NE.BAD .AND.
     X            U(I+1,J).NE.BAD )THEN
                     UA=COEFX*DERR(I,J)
                     U(I+1,J)=U(I+1,J)-UA
                     U(I-1,J)=U(I-1,J)+UA
               END IF
               IF(DERR(I,J).NE.BAD .AND.
     X            V(I,J-1).NE.BAD .AND.
     X            V(I,J+1).NE.BAD )THEN
                     VA=COEFY*DERR(I,J)
                     V(I,J+1)=V(I,J+1)-VA
                     V(I,J-1)=V(I,J-1)+VA
               END IF
   15       CONTINUE
   20    CONTINUE
C
      GO TO 1
C
   90 CONTINUE
         KST=0
         IF (ITER.EQ.0) THEN
            IF (L.EQ.1) THEN
               WRITE(6,154)AXNAM(3),LABAXS(3,1)
            ELSE
               WRITE(6,129)
            END IF
            WRITE(6,156)L,ZLEV,ITER,NINT(CNT),DMN,DSQR,DMIN,DMAX,'YES'
         ELSE
            WRITE(6,159)ITER,NINT(CNT),DMN,DSQR,DMIN,DMAX,'YES'
         END IF
      RETURN
C
   91 CONTINUE
         KST=1
         IF (ITER.EQ.0) THEN
            WRITE(6,156)L,ZLEV,ITER,NINT(CNT),DMN,DSQR,DMIN,DMAX,'NO'
         ELSE
            WRITE(6,159)ITER,NINT(CNT),DMN,DSQR,DMIN,DMAX,'NO'
         END IF
      RETURN
C
   92 CONTINUE
         KST=2
         IF (ITER.EQ.0) THEN
            IF (L.EQ.1) THEN
               WRITE(6,154)AXNAM(3),LABAXS(3,1)
            ELSE
               WRITE(6,129)
            END IF
            WRITE(6,156)L,ZLEV,ITER,NINT(CNT),DMN,DSQR,DMIN,DMAX,
     X           'NO DATA'
         ELSE
            WRITE(6,159)ITER,NINT(CNT),DMN,DSQR,DMIN,DMAX,'NO DATA'
         END IF
      RETURN
      END
