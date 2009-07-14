c
c----------------------------------------------------------------------X
c
      SUBROUTINE SPQUAD (X,Y,C,N,SR,XOR)
      DIMENSION       X(1)       ,Y(1)       ,C(3)
      FN = N
      SUMX = 0.
      SUMY = 0.
      SUMX2 = 0.
      SUMX3 = 0.
      SUMX4 = 0.
      SUMXY = 0.
      SUMX2Y = 0.
      IF (SR) 101,101,103
  101 DO 102 I=1,N
         SUMX = SUMX+X(I)
         SUMY = SUMY+Y(I)
         SUMX2 = SUMX2+X(I)**2
         SUMX3 = SUMX3+X(I)**3
         SUMX4 = SUMX4+X(I)**4
         SUMXY = SUMXY+X(I)*Y(I)
         SUMX2Y = SUMX2Y+X(I)**2*Y(I)
  102 CONTINUE
      GO TO 105
  103 XS = XOR-SR
      DO 104 I=1,N
         XS = XS+SR
         SUMX = SUMX+XS
         SUMY = SUMY+Y(I)
         SUMX2 = SUMX2+XS**2
         SUMX3 = SUMX3+XS**3
         SUMX4 = SUMX4+XS**4
         SUMXY = SUMXY+XS*Y(I)
         SUMX2Y = SUMX2Y+XS**2*Y(I)
  104 CONTINUE
  105 D = FN*(SUMX2*SUMX4-SUMX3**2)+SUMX*(SUMX3*SUMX2-SUMX*SUMX4)+
     1    SUMX2*(SUMX*SUMX3-SUMX2**2)
      C(1) = (SUMX2*SUMX4-SUMX3**2)*SUMY+(SUMX3*SUMX2-SUMX*SUMX4)*SUMXY+
     1       (SUMX*SUMX3-SUMX2**2)*SUMX2Y
      C(2) = (SUMX3*SUMX2-SUMX*SUMX4)*SUMY+(FN*SUMX4-SUMX2**2)*SUMXY+
     1       (SUMX*SUMX2-FN*SUMX3)*SUMX2Y
      C(3) = (SUMX*SUMX3-SUMX2**2)*SUMY+(SUMX*SUMX2-FN*SUMX3)*SUMXY+
     1       (FN*SUMX2-SUMX**2)*SUMX2Y
      C(1) = C(1)/D
      C(2) = C(2)/D
      C(3) = C(3)/D
      RETURN
      END
