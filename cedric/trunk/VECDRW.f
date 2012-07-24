      SUBROUTINE VECDRW(RBUF,NX,NY,CSP,PLWIND,L1,L2,INC,ILV,
     X                  OMS,HPRO,IAROW,BAD,NVEC,NVECCOL,ISKPI,ISKPJ)
C
C        DRAWS VECTORS ON A 2-DIMENSIONAL SURFACE
C        IAROW- IF=1, FIXED ARROWHEADS
C                 =2, PROPORTIONAL ARROWHEADS
C
      COMMON /AXSTRB/ AXSFAC(3)
      DIMENSION RBUF(NX,NY,2),CSP(3,3),PLWIND(2,3)
      DIMENSION ICOLMAP(10)
      DATA CL,CT,ST/320.0, 0.92388, 0.382683/
      DATA EPS/0.5/
      DATA SLOP/0.01/
      DATA CLRMN,CLREL,RMN,RMX/0.5,0.25,160.,6400./
      DATA ICOLMAP/1,61,5,8,25,38,32,2,60,0/

      CALL GSCLIP(1)
      CALL GQPLCI(IERROR,IOLDCOL)
      IF (IERROR.NE.0) THEN
         WRITE(*,*)'***ERROR CALLING GQPLCI IN VECDRW***'
         CALL FLUSH_STDOUT
      END IF
      CALL SFLUSH

C     Change line width to JLW. Default line thickness if ILW
C
      CALL GETUSV('LW',ILW)
      JLW=1200
      IF(JLW.LT.ILW)JLW=ILW
      CALL SETUSV('LW',JLW)

      IF (NVECCOL.GT.0) CALL GSPLCI(ICOLMAP(NVECCOL))
      X1=CSP(1,L1)
      XD=CSP(3,L1)
      Y1=CSP(1,L2)
      YD=CSP(3,L2)
      ADJX=AXSFAC(L1)
      ADJY=AXSFAC(L2)
C
C        PROCEED WITH PLOTTING OF VECTORS
C
      NVEC=0
      XMIN=PLWIND(1,L1)-SLOP
      XMAX=PLWIND(2,L1)+SLOP
      YMIN=PLWIND(1,L2)-SLOP
      YMAX=PLWIND(2,L2)+SLOP
      DO 50 J=1,NY,(INC*ISKPJ)
         Y=Y1+(J-1)*YD
         IF(Y.LT.YMIN.OR.Y.GT.YMAX) GO TO 50
         DO 40 I=1,NX,(INC*ISKPI)
            X=X1+(I-1)*XD
            IF(X.LT.XMIN.OR.X.GT.XMAX) GO TO 40
            IF(ILV.GE.0.AND.MOD(IABS(I-J),2).NE.ILV) GO TO 40
            U=RBUF(I,J,1)
            V=RBUF(I,J,2)
            IF(U.EQ.BAD.OR.V.EQ.BAD) GO TO 40
            NVEC=NVEC+1
            XND=X+U*OMS
            YND=Y+V*OMS
            CALL FL2INT(X,Y,N1,N2)
            CALL FL2INT(XND,YND,N3,N4)
            CALL PLOTIF(CMFX(N1),CMFY(N2),0)
            CALL PLOTIF(CMFX(N3),CMFY(N4),1)
            IF(N1.EQ.N3.AND.N2.EQ.N4) THEN
C
C              ZERO WIND SPEED
C
               CALL PLOTIF(CMFX(N1+1),CMFY(N2+1),1)
               GO TO 40
            END IF
            DX=N3-N1
            DY=N4-N2
            R=SQRT(DX*DX+DY*DY)
            IF(R.LT.EPS) GO TO 40
            C1=CL/R
            IF(IAROW.EQ.2) THEN
C
C              PROPORTIONAL ARROWHEADS
C
               IF(R.LT.CLRMN) GO TO 40
               C1=CLREL
               IF(R.LT.RMN) C1=RMN*CLREL/R
               IF(R.GT.RMX) C1=RMX*CLREL/R
            END IF
            C1=C1*HPRO
            N5=N3-C1*(CT*DX-ST*DY)
            N6=N4-C1*(CT*DY+ST*DX)
            N7=N3-C1*(CT*DX+ST*DY)
            N8=N4-C1*(CT*DY-ST*DX)
            CALL PLOTIF(CMFX(N5),CMFY(N6),0)
            CALL PLOTIF(CMFX(N3),CMFY(N4),1)
            CALL PLOTIF(CMFX(N7),CMFY(N8),1)
   40   CONTINUE
   50 CONTINUE
      CALL GSCLIP(0)
      CALL SFLUSH
      CALL GSPLCI(IOLDCOL)

C     Restore line width to ILW
C
      CALL SETUSV('LW',ILW)
      RETURN
      END
