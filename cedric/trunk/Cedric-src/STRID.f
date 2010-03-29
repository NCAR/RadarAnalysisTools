      SUBROUTINE STRID(F,NX,NY,IWIND,NCWORD,FLG,FAVG,FSTD,N,
     X                 I1,I2,J1,J2,FMN,FMX,ISOP)
C
C        STATISTICS OVER A TWO-DIMENSIONAL FIELD
C           ISOP- ACCUMULATOR OPTIONS:
C                 =0 NO ACCUMULATION OF STATISTICS
C                 =1 ACCUMULATE STATISTICS
C                 =2 SEND BACK ACCUMULATED STATISTICS
C
      DOUBLE PRECISION FACC,FSQR
      DOUBLE PRECISION FACV,FSQV
      COMMON /STATBF2/ FACV,FSQV
      COMMON /STATBF/ NV,IV1,IV2,JV1,JV2,FVMN,FVMX
C
      DIMENSION F(NX,NY),IWIND(2,3),NCWORD(3)
C
      IF(ISOP.EQ.2) THEN
C
C        VOLUME SUMMARY
C
         N=NV
         I1=IV1
         I2=IV2
         J1=JV1
         J2=JV2
         FMN=FVMN
         FMX=FVMX
         FACC=FACV
         FSQR=FSQV
         GO TO 25
      ELSE
C
C        STILL WORKING ON A PLANE
C
         N=0
         I1=NX
         I2=0
         J1=0
         J2=0
         FMN=1.E8
         FMX= -FMN
         FACC=0.0
         FSQR=0.0
      END IF
      IF(NX.EQ.0.OR.NY.EQ.0) GO TO 25
      IH=NCWORD(1)
      IV=NCWORD(2)
      IB=IWIND(1,IH)
      IE=IWIND(2,IH)
      JB=IWIND(1,IV)
      JE=IWIND(2,IV)
      DO 20 J=JB,JE
       DO 10 I=IB,IE
        FIJ=F(I,J)
        IF(FIJ.EQ.FLG) THEN
           GO TO 10
        END IF
        FACC=FACC+FIJ
        FSQR=FSQR+FIJ**2
        IF(FIJ.LT.FMN) FMN=FIJ 
        IF(FIJ.GT.FMX) FMX=FIJ
        IF(J1.EQ.0) J1=J
        J2=J
        I1=MIN0(I,I1)
        I2=MAX0(I,I2)
        N=N+1
   10  CONTINUE
   20 CONTINUE
C
   25 CONTINUE
C
      IF(N.LE.0) THEN
C
C        NO GOOD VALUES THIS PLANE
C
         FAVG=0.0
         FSTD=0.0
         FMN=0.0
         FMX=0.0
         I1=0
         J1=0
C
      ELSE
C
C        COMPUTE STATISTICS
C
         IF(ISOP.EQ.1) THEN
C
C           ACCUMULATE STATS FROM THIS PLANE
C
            NV=NV+N
            IV1=MIN0(I1,IV1)
            IV2=MAX0(I2,IV2)
            JV1=MIN0(J1,JV1)
            JV2=MAX0(J2,JV2)
            FVMN=AMIN1(FMN,FVMN)
            FVMX=AMAX1(FMX,FVMX)
            FACV=FACV+FACC
            FSQV=FSQV+FSQR
         END IF
C
      XN=1./FLOAT(N)
      FAVG=FACC*XN
      FSQR=FSQR*XN
      FSTD=FSQR-FAVG**2
      IF(FSTD.LT.0.0) FSTD=0.0
      FSTD=SQRT(FSTD)
C
      END IF
C
      RETURN
      END
