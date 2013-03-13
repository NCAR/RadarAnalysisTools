      SUBROUTINE CURVEF(IOPT,IDIR,NAMWGT,NAMINP,NAMOUT,SIGMA,IBUF1,
     X     RBUFD,RBUFW,VBUF,ACOEF,IWOP,N1,N2,NST)
C     
C     THIS SUBROUTINE FITS 1-D CURVES TO DATA
C     
      INCLUDE 'CEDRIC.INC'
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X     IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X     NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      COMMON /FIT/ NPOINTS
      DIMENSION TBUFD(MAXP),TBUFW(MAXP),
     X     XDATA(MAXP),RBUFW(N1,N2),IBUF1(1),VBUF(MAXP,7),
     X     ACOEF(MAXP,4),ILEV(3),IWIND(2,3),PWIND(2,3),RBUFD(N1,N2)
      CHARACTER*1 IWOP
      CHARACTER*2 NAMINP(4),NAMWGT(4),NAMOUT(4)
      DATA ILEV/3,3,2/
      
      NST=0
      NX=NCX(1)
      NY=NCX(2)
      NZ=NCX(3)
C     
C     SETUP WINDOW AS REQUESTED BY USER
C     
      CALL WINSET(IWIND,PWIND,IWOP)
C     
C     FIND NEEDED FIELDS
C     
      JFLD=LOCFLDID(NAMINP,ID(176),5,NFL,4)
      IF (JFLD.EQ.0) THEN
         WRITE(*,50)NAMINP
 50      FORMAT(/,5X,'+++CANNOT FIND INPUT FIELD:',4A2)
         CALL DIVZERO(1.0)
      END IF
      
      IF (IOPT.EQ.1) THEN
C
C     FIND WEIGHT FIELD
C
         IFLD=LOCFLDID(NAMWGT,ID(176),5,NFL,4)
         IF (IFLD.EQ.0) THEN
            WRITE(*,60)NAMWGT
 60         FORMAT(/,5X,'+++CANNOT FIND WEIGHT FIELD:',4A2)
            CALL DIVZERO(1.0)
         END IF
      END IF
      
      KFLD=LOCFLDID(NAMOUT,ID(176),5,NFL,4)
      IF (KFLD.EQ.0) THEN
C     
C     ADD A FIELD FOR OUTPUT
C     
         KFLD=IADFLD(NAMOUT,INT(SCLFLD(MAPVID(JFLD,2))),6)
         IF (KFLD.LT.0) THEN
            NST=1
            RETURN
         END IF
         KFLD=MAPVID(KFLD,1)
      END IF
      
C     
C     SETUP LOOP LIMITS
C     
      IF (IDIR.EQ.1) THEN
         N1=NZ
         N2=NY
         N3=NX
         LL1=IWIND(1,3)
         LU1=IWIND(2,3)
         LL2=IWIND(1,2)
         LU2=IWIND(2,2)
         LL3=IWIND(1,1)
         LU3=IWIND(2,1)
      ELSE IF (IDIR.EQ.2) THEN
         N1=NZ
         N2=NX
         N3=NY
         LL1=IWIND(1,3)
         LU1=IWIND(2,3)
         LL2=IWIND(1,1)
         LU2=IWIND(2,1)
         LL3=IWIND(1,2)
         LU3=IWIND(2,2)
      ELSE IF (IDIR.EQ.3) THEN
         N1=NY
         N2=NX
         N3=NZ
         LL1=IWIND(1,2)
         LU1=IWIND(2,2)
         LL2=IWIND(1,1)
         LU2=IWIND(2,1)
         LL3=IWIND(1,3)
         LU3=IWIND(2,3)
      ELSE
         WRITE(*,40)IDIR
 40      FORMAT(/,5X,'+++UNKNOWN DIRECTION FOR CURVE FIT:',I3)
         CALL DIVZERO(1.0)
      END IF
      
      IF ((LU3-LL3).GT.MAXP) THEN
         WRITE(*,110)(LU3-LL3)
 110     FORMAT(/,5X,'+++TOO MANY POINTS ALONG AXIS:',I4)
         CALL DIVZERO(1.0)
      END IF
      
      IF (IDIR.EQ.1) THEN
C     
C     SMOOTHING IN X DIRECTION
C     
         DO 100 I=LL1,LU1
C     
C     FETCH A PLANE OF THE INPUT FIELD
C     
            CALL FETCHD(IN,ID,I,JFLD,IBUF1,RBUFD,NX,NY,
     X           ILEV(IDIR),BAD,ZLEV,NST)
C     
C     FETCH A PLANE OF THE WEIGHT FIELD
C     
            IF (IOPT.EQ.1) THEN
               CALL FETCHD(IN,ID,I,IFLD,IBUF1,RBUFW,NX,NY,
     X              ILEV(IDIR),BAD,ZLEV,NST)
            END IF
            DO 150 J=LL2,LU2
               NPOINTS=0
               L=0
               WGTSUM=0.0
               DO 160 K=LL3,LU3
                  IF (RBUFD(K,J).NE.BAD) THEN
                     L=L+1
                     TBUFD(L)=RBUFD(K,J)
                     TBUFW(L)=RBUFW(K,J)
                     IF (IOPT.EQ.2) TBUFW(L)=1.0
                     WGTSUM=TBUFW(L)+WGTSUM
                     XDATA(L)=CSP(1,1)+(K-1)*CSP(3,1)
                     NPOINTS=NPOINTS+1
                  END IF
 160           CONTINUE
               IF (NPOINTS.GT.2) THEN
                  
C     
C     CALL THE CURVE FITTING ROUTINE 
C     
                  SIGMASQ = (SIGMA**2)*WGTSUM
                  CALL SMOOTH(XDATA,TBUFD,TBUFW,SIGMASQ,VBUF,ACOEF)
                  L=0
                  DO 190 K=LL3,LU3
                     IF (RBUFD(K,J).NE.BAD) THEN
                        L=L+1
                        RBUFD(K,J)=ACOEF(L,1)
                     END IF
 190              CONTINUE
               END IF
 150        CONTINUE
C     
C     NOW STORE THE RESULTS
C     
            CALL PLACED(IN,ID,I,KFLD,IBUF1,RBUFD,N2,N3,
     X           ILEV(IDIR),BAD,NST)
            
 100     CONTINUE
         
      ELSE IF (IDIR.EQ.2) THEN
C     
C     SMOOTH IN Y DIRECTION
C     
         DO 200 I=LL1,LU1
C     
C     FETCH A PLANE OF THE INPUT FIELD
C     
            CALL FETCHD(IN,ID,I,JFLD,IBUF1,RBUFD,NX,NY,
     X           ILEV(IDIR),BAD,ZLEV,NST)
C     
C     FETCH A PLANE OF THE WEIGHT FIELD
C     
            IF (IOPT.EQ.1) THEN
               CALL FETCHD(IN,ID,I,IFLD,IBUF1,RBUFW,NX,NY,
     X              ILEV(IDIR),BAD,ZLEV,NST)
            END IF
            DO 250 J=LL2,LU2
               NPOINTS=0
               L=0
               WGTSUM=0.0
               DO 260 K=LL3,LU3
                  IF (RBUFD(J,K).NE.BAD) THEN
                     L=L+1
                     TBUFD(L)=RBUFD(J,K)
                     TBUFW(L)=RBUFW(J,K)
                     IF (IOPT.EQ.2) TBUFW(L)=1.0
                     WGTSUM=TBUFW(L)+WGTSUM
                     XDATA(L)=CSP(1,2)+(K-1)*CSP(3,2)
                     NPOINTS=NPOINTS+1
                  END IF
 260           CONTINUE
               IF (NPOINTS.GT.2) THEN
                  
C     
C     CALL THE CURVE FITTING ROUTINE 
C     
                  SIGMASQ = (SIGMA**2)*WGTSUM
                  CALL SMOOTH(XDATA,TBUFD,TBUFW,SIGMASQ,VBUF,ACOEF)
                  L=0
                  DO 290 K=LL3,LU3
                     IF (RBUFD(J,K).NE.BAD) THEN
                        L=L+1
                        RBUFD(J,K)=ACOEF(L,1)
                     END IF
 290              CONTINUE
               END IF
 250        CONTINUE
C     
C     NOW STORE THE RESULTS
C     
            CALL PLACED(IN,ID,I,KFLD,IBUF1,RBUFD,N2,N3,
     X           ILEV(IDIR),BAD,NST)
            
 200     CONTINUE
         
      ELSE IF (IDIR.EQ.3) THEN
C     
C     SMOOTH IN Z DIRECTION
C     
         DO 300 I=LL1,LU1
C     
C     FETCH A PLANE OF THE INPUT FIELD
C     
            CALL FETCHD(IN,ID,I,JFLD,IBUF1,RBUFD,NX,NZ,
     X           ILEV(IDIR),BAD,ZLEV,NST)
C     
C     FETCH A PLANE OF THE WEIGHT FIELD
C     
            IF (IOPT.EQ.1) THEN
               CALL FETCHD(IN,ID,I,IFLD,IBUF1,RBUFW,NX,NZ,
     X              ILEV(IDIR),BAD,ZLEV,NST)
            END IF
            DO 350 J=LL2,LU2
               NPOINTS=0
               L=0
               WGTSUM=0.0
               DO 360 K=LL3,LU3
                  IF (RBUFD(J,K).NE.BAD) THEN
                     L=L+1
                     TBUFD(L)=RBUFD(J,K)
                     TBUFW(L)=RBUFW(J,K)
                     IF (IOPT.EQ.2) TBUFW(L)=1.0
                     WGTSUM=TBUFW(L)+WGTSUM
                     XDATA(L)=CSP(1,3)+(K-1)*CSP(3,3)
                     NPOINTS=NPOINTS+1
                  END IF
 360           CONTINUE
               IF (NPOINTS.GT.2) THEN
                  
C     
C     CALL THE CURVE FITTING ROUTINE 
C     
                  SIGMASQ = (SIGMA**2)*WGTSUM
                  CALL SMOOTH(XDATA,TBUFD,TBUFW,SIGMASQ,VBUF,ACOEF)
                  L=0
                  DO 390 K=LL3,LU3
                     IF (RBUFD(J,K).NE.BAD) THEN
                        L=L+1
                        RBUFD(J,K)=ACOEF(L,1)
                     END IF
 390              CONTINUE
               END IF
 350        CONTINUE
C     
C     NOW STORE THE RESULTS
C     
            CALL PLACED(IN,ID,I,KFLD,IBUF1,RBUFD,N2,N3,
     X           ILEV(IDIR),BAD,NST)
            
 300     CONTINUE
      END IF
      
      RETURN
      
      END
      
      
      
