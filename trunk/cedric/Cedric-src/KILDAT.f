      SUBROUTINE KILDAT(A,NX,NY,XBEG,XEND,YBEG,YEND,NPX,NPY,
     X                  ISKPI,ISKPJ)
C
C        RESHUFFLES THE ARRAY WHEN THE WINDOW INTERSECTS THE DATA FIELD
C
      DIMENSION A(NX,NY)
      DATA EPS /1.0E-6/
      I1=AMAX1(XBEG+.95,1.0)
      I2=AMIN1(XEND+.05,FLOAT(NX))
      J1=AMAX1(YBEG+.95,1.0)
      J2=AMIN1(YEND+.05,FLOAT(NY))
      NPX=(I2-I1)/ISKPI + 1
      NPY=(J2-J1)/ISKPJ + 1
      IF(NPX.EQ.NX.AND.NPY.EQ.NY)    RETURN
C
C        RESHUFFLE THE ARRAY
C
      J=0
      DO 10 JJ=J1,J2,ISKPJ
      J=J+1
      I=0
      DO 10 II=I1,I2,ISKPI
      I=I+1
      A(I,J)=A(II,JJ)
   10 CONTINUE

      XBEG=(XBEG-FLOAT(I1-1))
      IF (XBEG.LT.0.0) XBEG = ((XBEG-1.0)/ISKPI) + 1.0
      XEND=(XEND-FLOAT(I1-1))
      XEND=(XEND-1.0)/ISKPI + 1.0

      YBEG=(YBEG-FLOAT(J1-1))
      IF (YBEG.LT.0.0) YBEG = ((YBEG-1.0)/ISKPJ) + 1.0
      YEND=(YEND-FLOAT(J1-1))
      YEND=(YEND-1.0)/ISKPJ + 1.0

      RETURN
      END
