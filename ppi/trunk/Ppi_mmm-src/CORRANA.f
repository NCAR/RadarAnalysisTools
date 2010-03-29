      SUBROUTINE CORRANA(DAT,AZA,IOUT,IIN1,NIN2,C1,C2,C3,C4,BDVAL,
     +   MNGATE,MXGATE,NANG,AVGI,DROLD,R0,TMP1,TMP2,MXR,MXA,MXF)

      DIMENSION TMP1(MXR),TMP2(MXR,MXA)
      DIMENSION DAT(MXR,MXA,MXF),AZA(MXA,2)
      DIMENSION ANGMIN(2),ANGMAX(2),ANGINC(2),NSTEP(2)
      DATA ANGMIN(1),ANGMAX(1)/-90.,90./
      DATA ISKP/1/
      CHARACTER*8 NIN2

C  CORRELATE ANALYTIC FUNCTION WITH DATA
C  NIN2= TYPE OF FUNCTION
C  C1= WAVELENGTH OF FUNCTION
C  C2= ANGLE STEP WHEN USING LINEAR COS FUNCTION
C  C3= ANGLE FOR LINEAR COS FUNCTION- IF C2 IS NON-ZERO C3 WILL BE IGNORED
C  C4= LENGTH OF LINEAR COS FUNCTION

      DATA PI/3.14159265/
      DATA DTR/0.01745329/

      AVGI=ABS(AVGI)

      ANG=INT(C3)
      IANGI=NINT(ABS(C3-ANG)*100.)
      ANGI=IANGI
      ANGI=ABS(ANGI)
      DO 50 I=MNGATE,MXGATE
         DO 50 J=1,NANG
 50         DAT(I,J,IOUT)=BDVAL

      DO 60 I=1,NANG
         TMP1(I)=COS(AZA(I,1)*DTR)
 60      TMP2(I,1)=SIN(AZA(I,1)*DTR)

      WAVE=C1
      HWAVE=0.5*C1
      IDEL=(HWAVE/DROLD)+1.0001

      IF(NIN2.EQ.'COS     ')THEN
      DO 100 I=MNGATE+IDEL,MXGATE-IDEL

         RNG=R0+(I-1)*DROLD
         DAZ=RNG*SIN(AVGI*DTR)
         JDEL=(HWAVE/DAZ)+1.0001
         IF(JDEL.GT.50)GO TO 100

         DO 200 J=1+JDEL,NANG-JDEL

            X0=RNG*SIN(AZA(J,1)*DTR)
            Y0=RNG*COS(AZA(J,1)*DTR)
            RNPTS=0.
            S1=0.
            S2=0.
            SS1=0.
            SS2=0.
            S12=0.


            DO 120 I1=I-IDEL,I+IDEL
               RNG1=R0+(I1-1)*DROLD
               DO 120 J1=J-JDEL,J+JDEL
                  X=RNG1*SIN(AZA(J1,1)*DTR)
                  Y=RNG1*COS(AZA(J1,1)*DTR)
                  DL=SQRT((X-X0)*(X-X0)+(Y-Y0)*(Y-Y0))
                  D=DAT(I1,J1,IIN1)


                  IF(DL.LE.HWAVE.AND.D.NE.BDVAL)THEN
                     AMP=20.*COS(2.*PI*DL/WAVE)
                     RNPTS=RNPTS+1.
                     S1=S1+AMP
                     SS1=SS1+AMP*AMP
                     S2=S2+D
                     SS2=SS2+D*D
                     S12=S12+AMP*D
                  END IF
 120        CONTINUE

            IF(RNPTS.GT.3.)THEN
               RNINV=1./RNPTS
               T1=SS1-RNINV*S1*S1
               T2=SS2-RNINV*S2*S2
               IF(T1.LE.0.1.OR.T2.LE.0.1)GO TO 200
               DAT(I,J,IOUT)=100.*(S12-RNINV*S1*S2)/
     +                 SQRT(T1*T2)
            END IF
 200     CONTINUE
 100  CONTINUE

      ELSE IF(NIN2.EQ.'COSLIN  '.OR.NIN2.EQ.'AVERAGE ')THEN

         ANGINC(1)=C2

         WAVY=C4
         IF(C3.EQ.0.)THEN
            ANGMIN(1)=-90.
            ANGMAX(1)=90.
         ELSE
            ANGMIN(1)=ANG-ANGI
            ANGMAX(1)=ANG+ANGI
         END IF
         NSTEP(1)=(ANGMAX(1)-ANGMIN(1))/ANGINC(1)+1.0001         
         ITER=1
         IDEL1=(0.5*WAVY)/DROLD+1.0001
         IDEL=MAX0(IDEL,IDEL1)

         WRITE(*,*)'NSTEP,ANGMN,ANGMX,WAVE,WAVY= ',NSTEP(1),
     +        ANGMIN(1),ANGMAX(1),WAVE,WAVY
         DO 300 I=MNGATE+IDEL,MXGATE-IDEL

            RNG=R0+(I-1)*DROLD
            IF(RNG.LE.0.5)GO TO 300
            DAZ=RNG*SIN(AVGI*DTR)
            
            JDEL=(HWAVE/DAZ)+1.0001
            JDEL1=(0.5*WAVY/DAZ)+1.0001
            JDEL=MAX0(JDEL,JDEL1)

            IF(JDEL.GT.50)GO TO 300

            DO 400 J=1+JDEL,NANG-JDEL
               IF(DAT(I,J,IIN1).EQ.BDVAL)GO TO 400
               DO 410 IANG=1,NSTEP(ITER)
                  X0=RNG*TMP2(J,1)
                  Y0=RNG*TMP1(J)
                  RNPTS=0.
                  S1=0.
                  S2=0.
                  SS1=0.
                  SS2=0.
                  S12=0.
                  TOTPTS=0.
                  ANG=ANGMIN(ITER)+(IANG-1)*ANGINC(ITER)

                  AX=COS(ANG*DTR)
                  AY=-SIN(ANG*DTR)
                  SINANG=SIN(-ANG*DTR)
                  COSANG=COS(-ANG*DTR)
                  IF(I.EQ.394.AND.J.EQ.77)WRITE(*,*)'X0,Y0,ANG,AX,AY=',
     +              X0,Y0,ANG,AX,AY
                  DO 420 I1=I-IDEL,I+IDEL,ISKP
                     RNG1=R0+(I1-1)*DROLD
                     DO 420 J1=J-JDEL,J+JDEL,ISKP
                        X=RNG1*TMP2(J1,1)
                        Y=RNG1*TMP1(J1)
                        DX=X-X0
                        DY=Y-Y0
                        DXP=DX*COSANG+DY*SINANG
                        IF(ABS(DXP).GT.HWAVE)GO TO 420
                        D=DAT(I1,J1,IIN1)
                        DYP=-DX*SINANG+DY*COSANG

                        IF(ABS(DYP).GT.0.5*WAVY)GO TO 420
                        IF(DAT(I1,J1,IIN1).EQ.BDVAL)GO TO 420
                        AMP=20.*COS(2.*PI*(AX*DX+AY*DY)/WAVE)
      IF(I.EQ.394.AND.J.EQ.77)WRITE(*,*)'I,J,I1,J1= ',I,J,I1,J1
      IF(I.EQ.394.AND.J.EQ.77)WRITE(*,*)'X,Y,DX,DY= ',X,Y,DX,DY
      IF(I.EQ.394.AND.J.EQ.77)WRITE(*,*)'DXP,DYP,AMP,DAT= ',
     +              DXP,DYP,AMP,DAT(I1,J1,IIN1)


                        S2=S2+D
                        SS2=SS2+D*D
                        S1=S1+AMP
                        SS1=SS1+AMP*AMP
                        S12=S12+AMP*D
                        RNPTS=RNPTS+1.
 420              CONTINUE

               IF(RNPTS.GT.20.)THEN
                  RNINV=1./RNPTS
                  T1=SS1-RNINV*S1*S1
                  T2=SS2-RNINV*S2*S2
                  IF(T1.LE.0.1.OR.T2.LE.0.1)GO TO 400
                  COR=100.*(S12-RNINV*S1*S2)/
     +                 SQRT(T1*T2)
                  IF(COR.GT.DAT(I,J,IOUT))DAT(I,J,IOUT)=COR
                  IF(NIN2.EQ.'AVERAGE ')DAT(I,J,IOUT)=S2/RNPTS
               END IF
 410        CONTINUE
            IF(COR.GT.90.)WRITE(*,*)I,J,X0,Y0,COR
 400     CONTINUE
 300  CONTINUE


                  
            

      END IF

      RETURN
      END
