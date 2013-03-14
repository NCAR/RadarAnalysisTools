      SUBROUTINE REMINT(ABLR,KIJ,NX,NY,CSP,ANGR,ATR,XOR,YOR,
     X                  LOX,LOY,KINT)
C
C        GENERATES THE INTERPOLATION TABLE FOR THE REMAPPING
C
      DIMENSION ABLR(NX,NY,2),KIJ(NX,NY,2),CSP(3,3)
C
C        TRANSFORMATION FUNCTIONS FOR THE REMAPPING
C
         FX(X,Y)=ACSF*X-ASNF*Y+XORIG
         FY(X,Y)=ASNF*X+ACSF*Y+YORIG
C
      ASNF=SIN(ANGR*ATR)
      ACSF=COS(ANGR*ATR)
      XORIG=XOR
      YORIG=YOR
      XD=CSP(3,1)
      YD=CSP(3,2)
      XDI=1./XD
      YDI=1./YD
      X1=CSP(1,1)
      X2=CSP(2,1)
      Y1=CSP(1,2)
      Y2=CSP(2,2)
      KINT=0
C
C        TRANSLATE EACH NEW POINT TO THE OLD COORDINATE SYSTEM
C
      Y=-LOY*YD
      DO 20 J=1,NY
         Y=Y+YD
         X=-LOX*XD
         DO 10 I=1,NX
            X=X+XD
            XOLD=FX(X,Y)
            YOLD=FY(X,Y)
            ABLR(I,J,1)=0.0
            ABLR(I,J,2)=0.0
            KIJ(I,J,1)=0
            KIJ(I,J,2)=0
            RM=(XOLD-X1)*XDI+1.0
            M=RM
            IF(M.LT.1.OR.M.GE.NX) GO TO 10
            RN=(YOLD-Y1)*YDI+1.0
            N=RN
            IF(N.LT.1.OR.N.GE.NY) GO TO 10
            ABLR(I,J,1)=RM-FLOAT(M)
            ABLR(I,J,2)=RN-FLOAT(N)
            KIJ(I,J,1)=M
            KIJ(I,J,2)=N
            KINT=KINT+1
   10    CONTINUE
   20 CONTINUE
      RETURN
      END
