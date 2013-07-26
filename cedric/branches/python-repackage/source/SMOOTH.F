      REAL FUNCTION SMOOTH(X,Y,DY,S,V,A)
C
C     FUNCTION TO FIT A CUBIC SPLINES TO DATA POINTS.
C     THIS FUNCTION IS NOT AN INTERPOLATOR, BUT RATHER 
C     SMOOTHS THE DATA BASED ON ERROR ESTIMATES FOR THE
C     DATA VALUES AND A SO CALLED SMOOTHING PARAMETER PROVIDED
C     BY THE USER.
C     REFERENCE: A PRACTICAL GUIDE TO SPLINES by CARL DE BOOR
C                SPRINGER-VERLAG, 1978, PP. 235-243
C     

      INTEGER I, NPM1
      COMMON /FIT/ NPOINT
      DIMENSION A(NPOINT,4)
      REAL DY(NPOINT),S,V(NPOINT,7),X(NPOINT),Y(NPOINT),
     X     CHANGE,P,PREVSF,PREVP,SFP,SIXP,SIX1MP,UTRU
      
      CALL SETUPQ(X,DY,Y,V,A(1,4))
      IF (S .GT. 0.)    GOTO 20
 10   P = 1.
      CALL CHOL1D(P,V,A(1,4),1,A(1,3),A(1,1))
      SFP = 0.
                        GOTO 60
 20   P = 0.
      CALL CHOL1D(P,V,A(1,4),1,A(1,3),A(1,1))
      SFP = 0.
      DO 21 I=1,NPOINT
       SFP = SFP + (A(I,1)*DY(I))**2
 21   CONTINUE
      SFP = SFP*36.
      IF (SFP.LE.S)   GOTO 60
      PREVP = 0.
      PREVSF = SFP
      UTRU = 0.
      DO 25 I=1,NPOINT
         UTRU = UTRU + V(I-1,4)*(A(I-1,3)*(A(I-1,3)+A(I,3))+A(I,3)**2)
 25   CONTINUE
      P = (SFP-S)/(24.*UTRU)
C     
C     SECANT ITERATION FOR THE DETERMINATION OF P STARTS HERE.
C
 30   CALL CHOL1D(P,V,A(1,4),1,A(1,3),A(1,1))
      SFP = 0.
      DO 35 I=1,NPOINT
         SFP = SFP + (A(I,1)*DY(I))**2
 35   CONTINUE
      SFP = SFP*36.*(1.-P)**2
      IF (SFP .LE. 1.01*S)    GOTO 60
      IF (SFP .GE. PREVSF)    GOTO 10
      CHANGE = (P-PREVP)/(SFP-PREVSF)*(SFP-S)
      PREVP = P
      P = P - CHANGE
      PREVSF = SFP
      IF (P .LT. 1.)          GOTO 30
      P = 1. - SQRT(S/PREVSF)*(1.-PREVP)
                              GOTO 30
C     CORRECT VALUE OF P HAS BEEN FOUND.
C     COMPUTE POLY. COEFF. FROM Q*U (IN A(.,1)).
C
 60   SMOOTH = SFP
      SIX1MP = 6.*(1.-P)
      DO 61 I=1,NPOINT
         A(I,1) = Y(I) - SIX1MP*DY(I)**2*A(I,1)
 61   CONTINUE
      SIXP = 6.*P
      DO 62 I=1,NPOINT
         A(I,3) = A(I,3)*SIXP
 62   CONTINUE
      NPM1 = NPOINT - 1
      DO 63 I=1,NPM1
         A(I,4) = (A(I+1,3)-A(I,3))/V(I,4)
         A(I,2) = (A(I+1,1)-A(I,1))/V(I,4) - 
     X             (A(I,3)+A(I,4)/3.*V(I,4))/2.*V(I,4)
 63   CONTINUE
      RETURN
      END

      SUBROUTINE SETUPQ (X,DX,Y,V,QTY)
C
C     TO BE CALLED IN SMOOTH
C
      COMMON /FIT/ NPOINT
      INTEGER NPOINT, I, NPM1
      REAL DX(NPOINT),QTY(NPOINT),V(NPOINT,7),X(NPOINT),Y(NPOINT),
     X     DIFF,PREV
      NPM1 = NPOINT - 1
      V(1,4) = X(2) - X(1)
      DO 11 I=2,NPM1
         V(I,4) = X(I+1) - X(I)
         V(I,1) = DX(I-1)/V(I-1,4)
         V(I,2) = -DX(I)/V(I,4) - DX(I)/V(I-1,4)
         V(I,3) = DX(I+1)/V(I,4)
 11   CONTINUE
      V(NPOINT,1) = 0.
      DO 12 I=2,NPM1
         V(I,5) = V(I,1)**2 + V(I,2)**2 + V(I,3)**2
 12   CONTINUE
      IF (NPM1 .LT. 3)  GOTO 14
      DO 13 I=3,NPM1
         V(I-1,6) = V(I-1,2)*V(I,1) + V(I-1,3)*V(I,2)
 13   CONTINUE
 14   V(NPM1,6)=0.
      IF (NPM1 .LT. 4)  GOTO 16
      DO 15 I=4,NPM1
         V(I-2,7) = V(I-2,3)*V(I,1)
 15   CONTINUE
 16   V(NPM1-1,7) = 0.
      V(NPM1,7) = 0.
C
C     CONSTRUCR Q-TRANSP. *Y IN QTY
      PREV = (Y(2) - Y(1))/V(1,4)
      DO 21 I=2,NPM1
         DIFF = (Y(I+1)-Y(I))/V(I,4)
         QTY(I) = DIFF - PREV
         PREV = DIFF
 21   CONTINUE

      RETURN

      END

      
      SUBROUTINE CHOL1D(P,V,QTY,NCOL,U,QU)
C
C     
C
      COMMON /FIT/ NPOINT
      INTEGER NCOL,NPOINT,I,NPM1,NPM2
      REAL P,QTY(NPOINT),QU(NPOINT),U(NPOINT),V(NPOINT,7),
     X     SIX1MP,TWOP

      NPM1 = NPOINT - 1
      
      SIX1MP = 6.*(1.-P)
      TWOP = 2.*P
      DO 2 I=2,NPM1
         V(I,1) = SIX1MP*V(I,5) + TWOP*(V(I-1,4)+V(I,4))
         V(I,2) = SIX1MP*V(I,6) + P*V(I,4)
         V(I,3) = SIX1MP*V(I,7)
 2    CONTINUE
      NPM2 = NPOINT - 2
      IF (NPM2 .GE. 2)   GOTO 10
      U(1) = 0.
      U(2) = QTY(2)/V(2,1)
      U(3) = 0.
      GOTO 41

C     FACTORIZATION
 10   DO 20 I=2,NPM2
         RATIO = V(I,2)/V(I,1)
         V(I+1,1) = V(I+1,1) - RATIO*V(I,2)
         V(I+1,2) = V(I+1,2) - RATIO*V(I,3)
         V(I,2) = RATIO
         RATIO = V(I,3)/V(I,1)
         V(I+2,1) = V(I+2,1) - RATIO*V(I,3)
         V(I,3) = RATIO
 20   CONTINUE
C
C     FORWARD SUBSTITUTION
C
      U(1) = 0.
      V(1,3) = 0.
      U(2) = QTY(2)
      DO 30 I=2,NPM2
         U(I+1) = QTY(I+1) - V(I,2)*U(I) - V(I-1,3)*U(I-1)
 30   CONTINUE
C
C     BACK SUBSTITUTION
C
      U(NPOINT) = 0.
      U(NPM1) = U(NPM1)/V(NPM1,1)
      DO 40 I=NPM2,2,-1
         U(I) = U(I)/V(I,1)-U(I+1)*V(I,2)-U(I+2)*V(I,3)
 40   CONTINUE
 41   PREV = 0.
      DO 50 I=2,NPOINT
         QU(I) = (U(I) - U(I-1))/V(I-1,4)
         QU(I-1) = QU(I) - PREV
         PREV = QU(I)
 50   CONTINUE
      QU(NPOINT) = -QU(NPOINT)

      RETURN

      END
         
      
         
