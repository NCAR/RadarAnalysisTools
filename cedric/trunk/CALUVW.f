
      SUBROUTINE CALUVW(RBUF,NX,NY,NRADS,NRQF,NPLANE,RC,CSP,ANGXAX,
     X                  Z,VTEST,ITEQN,WEIGHT,SCLADV,KEEPW,
     X                  KEEPPE,BAD,IACTC,KOT)
C
C     CALCULATES THE U,V,W COMPONENTS FOR 2 OR MORE DOPPLER RADARS
C	 IACTC - (0) 3-D Cartesian, (1) Coplane
C        KOT - Index of Z-coordinate (Z)
C        DT2 -   two equation determinant
C        DT3 - three equation determinant
C
C     GEOMETRIC FACTORS used for quality of (U,V,W) estimates based
C        solely on the geometry associated with the radar network.
C        See Appendix F of the CEDRIC documentation.
C
C        E1  - Multiplier of W in 2eq-solution (U3 = U2 + EWU*W)
C        E2  - Multiplier of W in 2eq-solution (V3 = V2 + EWV*W)
C        UA (UASAV) - Normalized standard deviation of U component (USTD)
C        VA (VASAV) - Normalized standard deviation of V component (VSTD)
C        WA (WASAV) - Normalized standard deviation of W component (WSTD)
C
C     All output values are stored in RBUF(I,J,1-8), 
C        where (I,J) are (X,Y) indices and 1-8 are field numbers.
C        2-eqn: (1-8) = U, V, CNT, USTD, VSTD, EWU, EWV, and MPE
C        3-eqn: (1-7) = U, V, W, CNT, USTD, VSTD, WSTD
C
      INCLUDE 'CEDRIC.INC'
      PARAMETER (MXRAD=14,MXADV=MXRAD+1)
      COMMON /ARBRNE/ IABFLG(MXRAD),NFAZEL(2,MXRAD),SCAZEL(2,MXRAD)
      CHARACTER*8 NFAZEL
      COMMON /ADVECT/ ITVOLM(MXADV),NFLTIM(MXADV),TADV(MXADV),
     X                ISHAD(MXADV),JSHAD(MXADV),IADTYP(MXADV)
      CHARACTER*8 NFLTIM
      DIMENSION RBUF(NX,NY,MXCRT-2),WEIGHT(1)
      DIMENSION VRSAV(MXRAD)
      DIMENSION A(3),B(3),C(3),D(3),RC(3,NRADS),VTEST(3),CSP(3,3),
     X          XRSAV(MXRAD),YRSAV(MXRAD),ZRSAV(MXRAD),WTSAV(MXRAD)
      REAL   XR,YR,ZR,WT,VR,XRSAV,YRSAV,ZRSAV,WTSAV,R,RINV,A,B,C,D,
     X       DT2,DT2INV,DT3,DT3INV,WINV,E1,E2,DIV,U,V,W,UA,VA,WA,
     X       T1,T2,T3,P1,P2,P3,Q1,Q2,Q3,SUMW,VRSAV,VRSAM,PEV
      EQUIVALENCE (GRAB,IGRAB)
      LOGICAL IUNITY,ITWO,KEEPW,KEEPPE
      DATA I4BAD/ O'20000100000' /
      DATA EPS/1.E-8/
      DATA IBAD/-32768/
      DATA INUM/1/

C
C        INLINE FUNCTION FOR FETCHING 16-BIT VALUES (R TO L)
C
C      IGET16(IWD,IP)=SHIFT(SHIFTL(IWD,64-(16*IP)),48)
C
C        ROTATION ABOUT THE ORIGIN
C
      ROTX(X,Y)=ACSF*X-ASNF*Y
      ROTY(X,Y)=ASNF*X+ACSF*Y
C

c-----print *,'CALUVW: nrads=',nrads
c-----print *,'CALUVW: vtests=',vtest(1),vtest(2),vtest(3)
      ATR=ATAN(1.)/45.
      ANGR=AMOD(450.-ANGXAX,360.)*ATR
      ANGRCO=(ANGXAX-90.)*ATR
      ASNF=0.0
      ACSF=1.0
      IF (IACTC.EQ.1 .AND. KOT.EQ.1) THEN
C
C      GET COORD. OF RADARS IN COPLANE SPACE
C
         WRITE(*,20)
 20      FORMAT(/5X,'SYNTHESIS PERFORMED IN COPLANE SPACE')
         INUM=0
      END IF
      
      NEQN=3-ITEQN
      UNSCAD=1./SCLADV
      X1=CSP(1,1)
      XD=CSP(3,1)
      Y1=CSP(1,2)
      YD=CSP(3,2)
C
C        CALCULATE THE U,V,W COMPONENTS OF THIS COMBINATION
C
      Y=Y1-YD
      DO 80 J=1,NY
      Y=Y+YD
      X=X1-XD
      DO 70 I=1,NX
      X=X+XD
C
C        ESTABLISH NUMBER OF RADARS AT THIS LOCATION
C
      ITWO=.FALSE.
      IUNITY=.FALSE.
      M=0
      MBITS=0
      DO 30 N=1,NRADS
      IF(RBUF(I,J,N).EQ.BAD.OR.WEIGHT(N).EQ.BAD.OR.
     X                         WEIGHT(N).EQ.0.0) GO TO 30
      IF(IABFLG(N).EQ.1) THEN
C
C        AIRBORNE DOPPLER
C
         GRAB=RBUF(I,J,NRADS+N)
         IF(IGET16(IGRAB,1).EQ.IBAD.OR.
     X      IGET16(IGRAB,2).EQ.IBAD) GO TO 30
      END IF
      M=M+1
      MBITS=ICEDOR(MBITS,ICEDSHFT(1,N-1))
C      MBITS=OR(MBITS,LSHIFT(1,N-1))
   30 CONTINUE
      IF(M.LE.1 .AND. NRADS.NE.2) GO TO 41
      IF(M.LE.3) IUNITY=.TRUE.
      IF(M.EQ.2.OR.ITEQN.NE.0) ITWO=.TRUE.
C
C        INITIALIZE LEAST SQUARES EQUATION
C
      SUMW=0.0
      DO 25 N=1,3
         A(N)=0.0
         B(N)=0.0
         C(N)=0.0
         D(N)=0.0
   25 CONTINUE
C
C        LOOP ON EACH RADAR
C
      M=0
      VRBAD=0.0
      DO 40 N=1,NRADS
      VR=RBUF(I,J,N)
      WT=WEIGHT(N)

      IF (NRADS.EQ.2) THEN
C
C     FOR FORCED 2 RADAR SOLUTION, WANT TO CALCULATE GEO FACTORS EVERYWHERE.
C     THUS, EVEN IF ONE OR BOTH OF THE VRs ARE BAD, CALCULATE THEM
C
         IF (WT.EQ.BAD .OR. WT.EQ.0.0) GOTO 40
         IF (VR.EQ.BAD) VRBAD=1.0
      ELSE
C
C     FOR THREE OR MORE RADAR POSSIBILITY, DON'T CALC. GEO FACTORS EVERYWHERE
         IF(VR.EQ.BAD.OR.WT.EQ.BAD.OR.WT.EQ.0.0) GO TO 40

C     FOR THREE OR MORE RADAR POSSIBILITY, DO CALC. GEO FACTORS EVERYWHERE
         IF(WT.EQ.BAD.OR.WT.EQ.0.0) GO TO 40
      END IF
      IF(IUNITY) WT=1.0
      IF(IABFLG(N).EQ.1) THEN
C
C        AIRBORNE DOPPLER
C
         GRAB=RBUF(I,J,NRADS+N)
         IAZ=IGET16(IGRAB,1)
         IEL=IGET16(IGRAB,2)
         IF(IAZ.EQ.IBAD.OR.IEL.EQ.IBAD) GO TO 40
         AZ=IAZ*SCAZEL(1,N)*ATR
         EL=IEL*SCAZEL(2,N)*ATR
         COSEL=COS(EL)
         XR=SIN(AZ)*COSEL
         YR=COS(AZ)*COSEL
         ZR=SIN(EL)
      ELSE
C
C        X,Y,Z DISTANCE
C
         IF (IACTC.EQ.0) THEN
C
C     IF 3-D CARTESIAN COORD. SYSTEM
C
            XR=ROTX(X,Y)-RC(1,N)
            YR=ROTY(X,Y)-RC(2,N)
            ZR=Z-RC(3,N)
         ELSE IF (IACTC.EQ.1) THEN
C
C     IF COPLANE COORDINATE SYSTEM
C
            XR=X-RC(1,N)
            YR=Y-RC(2,N)
            ZR=0.0
         ELSE
            WRITE(*,*)'***INVALID COORDINATE SYSTEM IN CALUVW***'
         END IF
            
         R=SQRT(XR**2+YR**2+ZR**2)
         IF(R.LE.EPS) GO TO 40
         RINV=1./R
         IF(IADTYP(N).NE.0) THEN
C
C           ADVECTION CORRECTIONS
C
            IF(IADTYP(N).EQ.1) THEN
               XAD=X-FLOAT(ISHAD(N))*XD
               YAD=Y-FLOAT(JSHAD(N))*YD
            ELSE IF(IADTYP(N).EQ.2) THEN
               GRAB=RBUF(I,J,NRADS+N)
               IF (IGRAB.EQ.I4BAD) THEN
C
C     SET ALL OUTPUT VARS TO BAD
C
                  U=BAD
                  V=BAD
                  W=BAD
                  UASAV=BAD
                  VASAV=BAD
                  WASAV=BAD
                  E1=BAD
                  E2=BAD
                  IF (VRBAD.EQ.0.0) THEN
                     CNT=MBITS
                  ELSE
                     CNT=0.0
                  END IF
                  GOTO 65
               END IF
               XAD=X-(IGET16(IGRAB,1)*UNSCAD*XD)
               YAD=Y-(IGET16(IGRAB,2)*UNSCAD*YD)
            END IF
C
            IF (IACTC.EQ.0) THEN
C     3-D CARTESIAN
               XR=ROTX(XAD,YAD)-RC(1,N)
               YR=ROTY(XAD,YAD)-RC(2,N)
               VR=VR*SQRT(XR**2+YR**2+ZR**2)*RINV
            ELSE IF (IACTC.EQ.1) THEN
C     COPLANE 
               XR=XAD-RC(1,N)
               YR=YAD-RC(2,N)
               VR=VR*SQRT(XR**2+YR**2+ZR**2)*RINV
            ELSE
               WRITE(*,*)'***INVALID COORDINATE SYSTEM IN CALUVW***'
            END IF
C
         END IF
C
         XR=XR*RINV
         YR=YR*RINV
         ZR=ZR*RINV
C
      END IF
C
      M=M+1
      SUMW=SUMW+WT
      XRSAV(M)=XR
      YRSAV(M)=YR
      ZRSAV(M)=ZR
      WTSAV(M)=WT
      VRSAV(M)=VR


      A(1)=A(1)+WT*XR*XR
      B(1)=B(1)+WT*YR*XR
      B(2)=B(2)+WT*YR*YR
      C(1)=C(1)+WT*ZR*XR
      C(2)=C(2)+WT*ZR*YR
      C(3)=C(3)+WT*ZR*ZR
      D(1)=D(1)+WT*VR*XR
      D(2)=D(2)+WT*VR*YR
      D(3)=D(3)+WT*VR*ZR
   40 CONTINUE
C
C        END OF RADAR LOOP FOR THIS X,Y,Z LOCATION
C
   41 CONTINUE
      U=BAD
      V=BAD
      W=BAD
      UASAV=BAD
      VASAV=BAD
      WASAV=BAD
      E1=BAD
      E2=BAD
      IF (VRBAD.EQ.0.0) THEN
         CNT=MBITS
      ELSE
         CNT=0.0
      ENDIF
      IF(M.LE.1) GO TO 65
      IF(SUMW.LE.0.0) GO TO 65
      A(2)=B(1)
      A(3)=C(1)
      B(3)=C(2)
C
C        NORMALIZE THE MATRIX EQUATION
C
      WINV=1./SUMW
      DO 45 N=1,3
      A(N)=A(N)*WINV
      B(N)=B(N)*WINV
      C(N)=C(N)*WINV
      D(N)=D(N)*WINV
   45 CONTINUE
      DT2=A(1)*B(2)-A(2)*B(1)
      IF(ABS(DT2).LE.EPS) GO TO 65
      DT2INV=1./DT2
      P3=B(1)*C(2)-B(2)*C(1)
      Q3=A(2)*C(1)-A(1)*C(2)
      E1=P3*DT2INV
      E2=Q3*DT2INV
C
C        CALCULATE VARIANCE OF TWO EQUATION U,V ESTIMATES
C
      UA=0.0
      VA=0.0
      DIV=WINV*DT2INV
      DO 50 N=1,M
         UA=UA+(WTSAV(N)*DIV*(B(2)*XRSAV(N)-B(1)*YRSAV(N)))**2
         VA=VA+(WTSAV(N)*DIV*(A(1)*YRSAV(N)-A(2)*XRSAV(N)))**2
   50 CONTINUE
      UA=SQRT(UA)
      VA=SQRT(VA)
      UASAV=UA
      VASAV=VA
      IF (VRBAD.EQ.1.0) GOTO 65
C
C        2 RADAR CASE
C
      IF(ABS(E1).LT.VTEST(1).AND.ABS(E2).LT.VTEST(1).AND.
     X   UA.LT.VTEST(2).AND.VA.LT.VTEST(2)) THEN
         U=(D(1)*B(2)-D(2)*B(1))*DT2INV
         V=(D(2)*A(1)-D(1)*A(2))*DT2INV
      END IF
      IF (ITWO) GO TO 65
C
C        3 OR MORE RADAR SOLUTION
C
      T1=A(2)*B(3)-A(3)*B(2)
      T2=A(1)*B(3)-A(3)*B(1)
      T3=A(1)*B(2)-A(2)*B(1)
      DT3=C(1)*T1-C(2)*T2+C(3)*T3

C     Change this test to prevent mixing 2eq and 3eq UVstd values
C     since UASAV and VASAV already calculated for 2eq above.
C     (LJM 8/23/2011)
C
      IF(ABS(DT3).LE.EPS)THEN
         UASAV=BAD
         VASAV=BAD
         GO TO 65
      END IF
C
C        CALCULATE VARIANCE OF U,V,W ESTIMATES
C
      UA=0.0
      VA=0.0
      WA=0.0
      DT3INV=1./DT3
      DIV=WINV*DT3INV
      P1=B(2)*C(3)-B(3)*C(2)
      P2=B(1)*C(3)-B(3)*C(1)
C     P3 DEFINED ABOVE
      Q1=A(3)*C(2)-A(2)*C(3)
      Q2=A(3)*C(1)-A(1)*C(3)
C     Q3 DEFINED ABOVE
      DO 55 N=1,M
         UA=UA+(WTSAV(N)*DIV*(P1*XRSAV(N)-P2*YRSAV(N)+P3*ZRSAV(N)))**2
         VA=VA+(WTSAV(N)*DIV*(Q1*XRSAV(N)-Q2*YRSAV(N)+Q3*ZRSAV(N)))**2
         WA=WA+(WTSAV(N)*DIV*(T1*XRSAV(N)-T2*YRSAV(N)+T3*ZRSAV(N)))**2
   55 CONTINUE
      UA=SQRT(UA)
      VA=SQRT(VA)
      WA=SQRT(WA)

C     Save these (UA,VA) calculations since 
C     (UA,VA) already calculated for 2eq above.
C     Otherwise, the 2eq (UA,VA) will be used
C     when WA.GE.VTEST(3). (LJM 8/23/2011)
C
      UASAV=UA
      VASAV=VA
      WASAV=WA
      IF(UA.GE.VTEST(2).OR.VA.GE.VTEST(2).OR.WA.GE.VTEST(3))THEN
c--------print *,'CALUVW: UA,VA,WA=',UA,VA,WA
         GO TO 65
      END IF
C
C        GOOD ESTIMATE OF W
C
      UASAV=UA
      VASAV=VA
      U=(D(1)*P1-D(2)*P2+D(3)*P3) * DT3INV
      V=(D(1)*Q1-D(2)*Q2+D(3)*Q3) * DT3INV
      W=(D(1)*T1-D(2)*T2+D(3)*T3) * DT3INV
   65 CONTINUE
C
C        SAVE U,V,W,CT AND SIGMA VALUES IN BUFFERS
C
      IF(KEEPW) THEN
C
C        W FIELD IS BEING CALCULATED 
C        Three-equation solution
C
         RBUF(I,J,1)=U
         RBUF(I,J,2)=V
         RBUF(I,J,3)=W
         RBUF(I,J,4)=CNT
         RBUF(I,J,5)=UASAV
         RBUF(I,J,6)=VASAV
         RBUF(I,J,7)=WASAV
C
      ELSE
C
C        CALCULATION OF U,V ONLY FOR ALL DATA POINTS
C        Two-equation solution
C
         RBUF(I,J,1)=U
         RBUF(I,J,2)=V
         RBUF(I,J,3)=CNT
         RBUF(I,J,4)=UASAV
         RBUF(I,J,5)=VASAV
         RBUF(I,J,6)=E1
         RBUF(I,J,7)=E2
C
      END IF
C
      IF(KEEPPE) THEN
C
C        CALCULATE MOST PROBABLE ERROR  --IF REQUESTED
C
         PEV=BAD
         IDENO=M-NEQN
         IF(ITEQN.NE.0) W=0.0
         IF(IDENO.GT.0.AND.U.NE.BAD.AND.V.NE.BAD.AND.W.NE.BAD) THEN
C
C           GOOD VALUES AT THIS LOCATION
C

            SUMW=0.0
            DO 68 N=1,M
               R=SQRT(XRSAV(N)**2+YRSAV(N)**2+ZRSAV(N)**2)
c               IF ((XRSAV(N).EQ.0 .AND. YRSAV(N).EQ.0) .OR.
c     X              R.EQ.0.0) THEN
c                  PEV=BAD
c                  GOTO 67
c               END IF
c               AZ=ATAN2(XRSAV(N),YRSAV(N))
c               EL=ASIN(ZRSAV(N)/R)
c               VRSAM=(U*SIN(AZ)+V*COS(AZ))*COS(EL)+(W*SIN(EL))
               VRSAM=(U*xrsav(n)+V*yrsav(n)+W*zrsav(n))
               SUMW=SUMW+(VRSAV(N)-VRSAM)**2
c               if (sumw.gt.100.) then
c       write(*,*)'***vrsav(n),vrsam,u,v,w,xrsav(n),yrsav(n),zrsav(n)='
c     X  ,vrsav(n),vrsam,u,v,w,xrsav(n),yrsav(n),zrsav(n)
c       write(*,*)'***z=',z
c       write(*,*)'***m,xr(1),xr(2),xr(3),xr(4),xr(5),xr(6),xr(7)=',
c     X      m,xrsav(1),xrsav(2),xrsav(3),xrsav(4),xrsav(5),xrsav(6),
c     X      xrsav(7)
c       write(*,*)'***m,yr(1),yr(2),yr(3),yr(4),yr(5),yr(6),yr(7)=',
c     X      m,yrsav(1),yrsav(2),yrsav(3),yrsav(4),yrsav(5),yrsav(6),
c     X      yrsav(7)
c       end if
   68       CONTINUE
            PEV=SQRT(SUMW/FLOAT(IDENO))
 67         CONTINUE
         END IF
C
         RBUF(I,J,8)=PEV
C
      END IF
C
C
   70 CONTINUE
   80 CONTINUE
      RETURN
      END
