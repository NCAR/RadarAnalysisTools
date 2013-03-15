      SUBROUTINE REGCAL(X,Y,NX,NY,IWIND,NCWORD,BAD,N,FMNX,STDX,
     X                  FMNY,STDY,CCF,STDERR,C0,C1,ISOP)
C
C        REGRESSION CALCULATION OF Y VERSUS X UNDER WINDOWING
C
C          ISOP- 0 NO ACCUMULATION OF STATS
C                1 ACCUMULATE THEM
C                2 RETURN INFO BASED UPON ACCUMULATED STATS
C
      DIMENSION X(NX,NY),Y(NX,NY),IWIND(2,3),NCWORD(3)
C
      DOUBLE PRECISION  SX, SY, CXY, FSQX, FSQY

      DOUBLE PRECISION SXV,SYV,CXYV,FSQXV,FSQYV
      COMMON /REGRBF/ NRV
      COMMON /REGRBF2/ SXV,SYV,CXYV,FSQXV,FSQYV
      DATA EPS /1E-03/
C
      IF(ISOP.EQ.2) THEN
C
C        VOLUME STATS TO BE USED
C
         N=NRV
         SX=SXV
         SY=SYV
         CXY=CXYV
         FSQX=FSQXV
         FSQY=FSQYV
         GO TO 25
C
      ELSE
C
C        STATS FOR THIS PLANE ONLY
C
         N=0
         SX=0.0
         SY=0.0
         CXY=0.0
         FSQX=0.0
         FSQY=0.0
C
      END IF
C
C     PROCEED WITH STATS ACCUMULATION
C
      IF(NX.EQ.0.OR.NY.EQ.0) GO TO 25
      IH=NCWORD(1)
      IV=NCWORD(2)
      IB=IWIND(1,IH)
      IE=IWIND(2,IH)
      JB=IWIND(1,IV)
      JE=IWIND(2,IV)
C
      DO 20 J=JB,JE
         DO 10 I=IB,IE
            FX=X(I,J)
            FY=Y(I,J)
            IF(FX.EQ.BAD.OR.FY.EQ.BAD) GO TO 10
C
            SX=SX+FX
            SY=SY+FY
            CXY=CXY+FX*FY
            FSQX=FSQX+FX**2
            FSQY=FSQY+FY**2
            N=N+1
C
   10    CONTINUE
   20 CONTINUE
C
   25 CONTINUE
C
C     CALCULATE THE RESULTS
C
      IF(N.LE.0) THEN
C
C        NO GOOD VALUES
C
         FMNX=0.0
         FMNY=0.0
         STDX=0.0
         STDY=0.0
         CCF=0.0
         STDERR=0.0
         C0=0.0
         C1=0.0
C
      ELSE
C
C        STATS CAN BE COMPUTED
C
         IF(ISOP.EQ.1) THEN
C
C           ACCUMULATE STATS FOR VOLUME
C
            NRV=NRV+N
            SXV=SXV+SX
            SYV=SYV+SY
            CXYV=CXYV+CXY
            FSQXV=FSQXV+FSQX
            FSQYV=FSQYV+FSQY
         END IF
C
         RN=N
         XN=1./RN
         FMNX=SX*XN
         FMNY=SY*XN
         STDX=FSQX*XN-FMNX**2
         STDX=SQRT(AMAX1(STDX,0.0))
         STDY=FSQY*XN-FMNY**2
         STDY=SQRT(AMAX1(STDY,0.0))
         CCF=RN*STDX*STDY
         IF(CCF.GT.0.0) CCF= (CXY-RN*FMNX*FMNY)/CCF
         STDERR=STDY*SQRT(AMAX1(1.0-CCF*CCF,0.0))
         C1=0.0
         C0=0.0
         IF((STDX-EPS).GT.0.0) THEN
            C1=CCF*(STDY/STDX)
            C0=FMNY-(C1*FMNX)
         END IF
C
      END IF
C
      RETURN
      END
