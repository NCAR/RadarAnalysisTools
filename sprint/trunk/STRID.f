      SUBROUTINE STRID(IFLDID,F,NX,NY,FLG,N,LEV,ZLEV)
C
C     STATISTICS FOR LEVELS AND FIELDS - For each level output
C     LEVEL-COORD-FIELD-MEAN-MNSQ-STDV-NMPT-I1-I2-J1-J2-MIN-MAX
C       1    Zlev   #1 ...........
C                   #2 ...........
C                   #3 ...........
C       2    Zlev   #1 ...........
C                   #2 ...........
C                   #3 ...........
C      etc.
C
      DIMENSION F(NX,NY)
      CHARACTER*8 IFLDID
      DATA IPR,LASLEV/6,0/
      FAVG=0.0
      FSQR=0.0
      FSTD=0.0
      N=0
      I1=NX
      I2=0
      J1=0
      J2=0
      FMN=1.E20
      FMX=-1.E20
      IF (NX.EQ.0 .OR. NY.EQ.0) RETURN
      DO 20 J=1,NY
         DO 10 I=1,NX
            FIJ=F(I,J)
            IF (FIJ.EQ.FLG) GOTO 10
            FAVG=FAVG+FIJ
            FSQR=FSQR+FIJ*FIJ
            IF (FIJ.LT.FMN) FMN=FIJ
            IF (FIJ.GT.FMX) FMX=FIJ
            IF (J1.EQ.0) J1=J
            J2=J
            IF (I.LT.I1) I1=I
            IF (I.GT.I2) I2=I
            N=N+1
 10      CONTINUE
 20   CONTINUE
      IF (N.GT.0) GOTO 100
         FMN=0.0
         FMX=0.0
         I1=0
         GO TO 150
 100  CONTINUE
      XN=1.0/FLOAT(N)
      FAVG=FAVG*XN
      FSQR=FSQR*XN
      FSTD=FSQR-FAVG*FAVG
      IF (FSTD.LT.0) FSTD=0.0
      FSTD=SQRT(FSTD)
 150  CONTINUE
      IF (LEV.EQ.LASLEV) THEN
         WRITE (IPR,2001)IFLDID,FAVG,FSQR,FSTD,N,I1,I2,J1,J2,FMN,FMX
      ELSE
         WRITE (IPR,2002)LEV,ZLEV,IFLDID,FAVG,FSQR,FSTD,N,I1,I2,J1,J2,
     X                     FMN,FMX
         LASLEV=LEV
      ENDIF
 2001 FORMAT (20X,A8,5X,F10.4,2(2X,F10.4),4X,5I5,
     X               5X,F10.4,2X,F10.4)
 2002 FORMAT (4X,I3,F8.3,5X,A8,5X,F10.4,2(2X,F10.4),4X,5I5,
     X               5X,F10.4,2X,F10.4)
      RETURN
      END
