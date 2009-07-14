      SUBROUTINE AREA(IIN1,DAT,AZA,C1,BDVAL,MNGATE,MXGATE,MANG,
     +  DROLD,R0,ITPOLD,FXOLD,ITIME,NIN1,MXR,MXA,MXF)

      INCLUDE 'areatime.inc'

      CHARACTER*8 NIN1

      DIMENSION DAT(MXR,MXA,MXF),AZA(MXA,2)

      DATA DTR/0.01745329/
      DATA SUMMIN,IPTIME/0.,0./


      IF(ITIME.EQ.IPTIME)RETURN
      IF(ITPOLD.EQ.3)RETURN
      IF(NA.EQ.0)THEN
         NAMAREA=NIN1
         DO 10 I=1,10
            DO 10 J=1,200
               DO 10 K=1,30
 10               STAREA(I,J,K)=0.
         IHR=ITIME/10000
         IMN=(ITIME-IHR*10000)/100
         ISEC=ITIME-IHR*10000-IMN*100
         ISEC1=IHR*3600+IMN*60+ISEC

      END IF
      NA=NA+1
      WRITE(*,*)'NA,ITIME,IPTIME= ',NA,ITIME,IPTIME
      IPTIME=ITIME


      DO 25 K=1,NAREA
      AZSUM=0.
      FXAREA(NA,K)=FXOLD
      TMAREA(NA,K)=ITIME
      IHR=ITIME/10000
      IMN=(ITIME-IHR*10000)/100
      ISEC=ITIME-IHR*10000-IMN*100
      ISEC2=IHR*3600+IMN*60+ISEC
      DTSEC=ISEC2-ISEC1
      IF(DTSEC.LT.-70000)DTSEC=DTSEC+86400
      TMIN(NA,K)=DTSEC/60.
      X1BOX(K)=X1INT(K)+DTSEC*UBOX(K)*0.001
      X2BOX(K)=X2INT(K)+DTSEC*UBOX(K)*0.001
      Y1BOX(K)=Y1INT(K)+DTSEC*VBOX(K)*0.001
      Y2BOX(K)=Y2INT(K)+DTSEC*VBOX(K)*0.001




      DELX=X2BOX(K)-X1BOX(K)
      DELY=Y2BOX(K)-Y1BOX(K)
      ANGROT=ATAN2(ABS(DELX),ABS(DELY))
      IF((DELX.LT.0..AND.DELY.GT.0.).OR.
     +   (DELX.GT.0..AND.DELY.LT.0.))ANGROT=-ANGROT
      R=SQRT(X1BOX(K)*X1BOX(K)+Y1BOX(K)*Y1BOX(K))
      ANGPT=ATAN2(X1BOX(K),Y1BOX(K))
      X1=R*SIN(ANGPT-ANGROT)
      Y1=R*COS(ANGPT-ANGROT)
c      RNGBOX(K)=X1
      RNGBOX(K)=(K-1)*WIDTH(K)

      R=SQRT(X2BOX(K)*X2BOX(K)+Y2BOX(K)*Y2BOX(K))
      ANGPT=ATAN2(X2BOX(K),Y2BOX(K))
      X2=R*SIN(ANGPT-ANGROT)
      Y2=R*COS(ANGPT-ANGROT)
      X1=X1-0.5*WIDTH(K)
      X2=X2+0.5*WIDTH(K)
      IF(Y1.GT.Y2)THEN
         TMP=Y1
         Y1=Y2
         Y2=TMP
      END IF

      IF(AREATYP(K).EQ.'AREA    ')THEN

         DO 50 J=2,MANG-1
            SINAZ=SIN(AZA(J,1)*DTR-ANGROT)
            COSAZ=COS(AZA(J,1)*DTR-ANGROT)
            DELAZ=ABS(AZA(J+1,1)-AZA(J-1,1))
            IF(DELAZ.GT.300.)DELAZ=360.-DELAZ
            AZSUM=AZSUM+0.5*DELAZ
            DELAZ=0.5*DELAZ*DTR
            DO 100 I=MNGATE+1,MXGATE
               RNG=R0+(I-1)*DROLD
               IF(RNG.LE.0.)GO TO 100
               XP=RNG*SINAZ
               YP=RNG*COSAZ
               IF(DAT(I,J,IIN1).EQ.BDVAL)GO TO 100
               IF(XP.GE.X1.AND.XP.LE.X2.AND.YP.GE.Y1.AND.
     +            YP.LE.Y2)THEN

                  DO 110 ICL=1,NCLA(K)
                     IF(CLAREA(ICL,K).GT.DAT(I,J,IIN1))GO TO 100
                        STAREA(ICL,NA,K)=STAREA(ICL,NA,K)+
     +                   DROLD*RNG*DELAZ
 110              CONTINUE
               END IF
 100        CONTINUE
 50      CONTINUE

      ELSE IF(AREATYP(K).EQ.'INTEGRAL')THEN

         DO 150 J=2,MANG-1
            SINAZ=SIN(AZA(J,1)*DTR-ANGROT)
            COSAZ=COS(AZA(J,1)*DTR-ANGROT)
            DELAZ=ABS(AZA(J+1,1)-AZA(J-1,1))
            IF(DELAZ.GT.300.)DELAZ=360.-DELAZ
            AZSUM=AZSUM+0.5*DELAZ
            DELAZ=0.5*DELAZ*DTR
            DO 200 I=MNGATE+1,MXGATE
               RNG=R0+(I-1)*DROLD
               IF(RNG.LE.0.)GO TO 200
               XP=RNG*SINAZ
               YP=RNG*COSAZ
               IF(DAT(I,J,IIN1).EQ.BDVAL)GO TO 200
               IF(XP.GE.X1.AND.XP.LE.X2.AND.YP.GE.Y1.AND.
     +            YP.LE.Y2)THEN
                  STAREA(1,NA,K)=STAREA(1,NA,K)+DAT(I,J,IIN1)*DROLD*
     +              RNG*DELAZ
               END IF
 200        CONTINUE
 150     CONTINUE

      ELSE IF(AREATYP(K).EQ.'AVERAGE ')THEN

         DO 250 J=2,MANG-1
            SINAZ=SIN(AZA(J,1)*DTR-ANGROT)
            COSAZ=COS(AZA(J,1)*DTR-ANGROT)
            SINAZA=SIN(AZA(J,1)*DTR)
            COSAZA=COS(AZA(J,1)*DTR)
            DELAZ=ABS(AZA(J+1,1)-AZA(J-1,1))
            IF(DELAZ.GT.300.)DELAZ=360.-DELAZ
            AZSUM=AZSUM+0.5*DELAZ
            DELAZ=0.5*DELAZ*DTR
            DO 300 I=MNGATE+1,MXGATE
               RNG=R0+(I-1)*DROLD
               IF(RNG.LE.0.)GO TO 300
               XP=RNG*SINAZ
               YP=RNG*COSAZ
               XA=RNG*SINAZA
               YA=RNG*COSAZA
               IF(DAT(I,J,IIN1).EQ.BDVAL)GO TO 300
               IF(XP.GE.X1.AND.XP.LE.X2.AND.YP.GE.Y1.AND.
     +            YP.LE.Y2)THEN
                  STAREA(1,NA,K)=STAREA(1,NA,K)+DAT(I,J,IIN1)*DROLD*
     +              RNG*DELAZ
                  STAREA(2,NA,K)=STAREA(2,NA,K)+DAT(I,J,IIN1)*DROLD*
     +              RNG*DELAZ*YA
                  STAREA(3,NA,K)=STAREA(3,NA,K)+DROLD*
     +              RNG*DELAZ
               END IF
 300        CONTINUE
 250     CONTINUE
      IF(STAREA(3,NA,K).GT.0.)THEN
         STAREA(1,NA,K)=STAREA(1,NA,K)/STAREA(3,NA,K)
         STAREA(2,NA,K)=STAREA(2,NA,K)/STAREA(3,NA,K)
c         X=STAREA(1,NA,K)
c         Y=STAREA(2,NA,K)
c         R=SQRT(X*X+Y*Y)
c         ANG=ATAN2(X,Y)*57.3

c         STAREA(1,NA,K)=R
c         STAREA(2,NA,K)=ANG
      ELSE 
         STAREA(1,NA,K)=-999.
         STAREA(2,NA,K)=-999.
      END IF

      END IF
      IF(AZSUM.LT.SUMMIN)THEN
         NA=NA-1
         RETURN
      END IF
 25   CONTINUE
      RETURN
      END
      
