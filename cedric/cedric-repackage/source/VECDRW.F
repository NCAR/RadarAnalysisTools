      SUBROUTINE VECDRW(RBUF,NX,NY,CSP,PLWIND,L1,L2,INC,ILV,
     X                  OMS,HPRO,IAROW,BAD,NVEC,NVECCOL,ISKPI,ISKPJ)
C
C        DRAWS VECTORS ON A 2-DIMENSIONAL SURFACE
C        IAROW- IF=1, FIXED ARROWHEADS
C                 =2, PROPORTIONAL ARROWHEADS
C
C     Note: CMFX or CMFY coordinate conversions do not work properly
C           when module is built on a 64-bit machine (shiraz).  Works
C           on 32-bit bora since the NCAR graphics there is likely
C           a version older than the one on shiraz,  bora and shiraz
C           ncargversion = 5.1.1, but tikal ncargversion = 6.0.0
C
C     Coordinate systems:
C     User       - geometric coordinates (e.g. X= -200 km --> 200 km)
C     Metacode   - Old flatbed plotter (0 --> 32767)
C     Fractional - Newer digital display (0.0 --> 1.0)
C
C     FL2INT - Converts user (X,Y) to metacode coordinate system
C              Equivalent to MX=KUMX(X) and MY=KUMY(Y)
C     CMFX(Y)- Converts metacode coordinates to fractional
C     PLOTIF - Moves to fractional coordinates with pen up
C             (0), or down (1).
C     Note: If the plot window mapping has been set with 
C           CALL SET(FL,FR,FB,FT,UL,UR,UB,UT,1), the "pen"
C           will be moved to user coordinates, given a
C           fractional coordinate as input to PLOTIF.
C     Replaced all NCAR graphics CMFX(or Y) calls with 
C     computation of fractional CMFX(MX) ==> FMX=MX/32767.
C     and still use PLOTIF since the fractional FMX will
C     plot at its equivalent user coordinate.
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
      LinLin=1
      CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LinLin)
c      print *,'VECDRW: initial graphics window mapping'
c      print *,'VECDRW: fl,fr,fb,ft=',fl,fr,fb,ft
c      print *,'VECDRW: ul,ur,ub,ut=',ul,ur,ub,ut

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
            FN1=N1/32767.
            FN2=N2/32767.
            FN3=N3/32767.
            FN4=N4/32767.
c            CALL PLOTIF(CMFX(N1),CMFY(N2),0)
c            CALL PLOTIF(CMFX(N3),CMFY(N4),1)
            CALL PLOTIF(FN1,FN2,0)
            CALL PLOTIF(FN3,FN4,1)
c            print *,'VECDRW:   x,n1,fn1=',x,n1,fn1
c            print *,'VECDRW:   y,n2,fn2=',y,n2,fn2
c            print *,'VECDRW: xnd,n3,fn3=',xnd,n3,fn3
c            print *,'VECDRW: ynd,n4,fn4=',ynd,n4,fn4
            IF(N1.EQ.N3.AND.N2.EQ.N4) THEN
C
C              ZERO WIND SPEED
C
               FN11=FLOAT(N1+1)/32767.
               FN21=FLOAT(N2+1)/32767.
c               CALL PLOTIF(CMFX(N1+1),CMFY(N2+1),1)
               CALL PLOTIF(FN11,FN21,1)
               GO TO 40
            END IF
            DX=N3-N1
            DY=N4-N2
            R=SQRT(DX*DX+DY*DY)
            IF(R.LT.EPS) GO TO 40
            C1=CL/R
c            print *,'VECDRW: dx,dy,r,cl,c1=',dx,dy,r,cl,c1
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
c            print *,'VECDRW: hpro,c1,ct,st=',hpro,c1,ct,st
c            N5=N3-C1*(CT*DX-ST*DY)
c            N6=N4-C1*(CT*DY+ST*DX)
c            N7=N3-C1*(CT*DX+ST*DY)
c            N8=N4-C1*(CT*DY-ST*DX)
            FN5=(N3-C1*(CT*DX-ST*DY))/32767.
            FN6=(N4-C1*(CT*DY+ST*DX))/32767.
            FN7=(N3-C1*(CT*DX+ST*DY))/32767.
            FN8=(N4-C1*(CT*DY-ST*DX))/32767.
c            print *,'VECDRW: fn5,fn6,fn7,fn8=',fn5,fn6,fn7,fn8
            CALL PLOTIF(FN5,FN6,0)
            CALL PLOTIF(FN3,FN4,1)
            CALL PLOTIF(FN7,FN8,1)
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
