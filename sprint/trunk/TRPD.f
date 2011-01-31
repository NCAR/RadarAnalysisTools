      SUBROUTINE TRPD(IDAT,IEL,IRNG,KZLV,IREF,IFLG,IVVAR,NST,MN,
     X                NUMFILT,KZA,MXFLD,VNYQUIST,IPPI,ILLE,ILLZ)
C
C     Note: The QUAL field gets messed up when more than 8 fields are
C           interpolated.  My suspicion has to do with the double-role
C           being played by IBLV in routines calling TRPD and being
C           passed into IVVAR that is a one-dimensional array. 
C           (LJM 7/27/9).
C           Changed IVVAR to IBLV since IBLV is used several places
C           and IVVAR is used only for packing/unpacking variance
C           information for local velocity unfolding.
C
C     Does the bilinear (4-pt) interpolation from radar scan to a single
C     XY-position for each of NFLI (NOF) fields.  Called from one of
C     the main interpolators: TRPVOL, TRPARVOL, or TRPPI.  Returns
C     either lower plane or upper plane results for all fields.  After
C     calls to TRPD for lower and upper planes, COMBIN is called to finish
C     the interpolation in the Z-direction.
C
C     IDAT - Output array of interpolated fields
C     IREF - Seems to serve no purpose
C
C     METH interpolation method flag [METSPC(1) set in INTERP]
C
C        METH = 1  ==>  Normal bilinear interpolations
C        METH = 2  ==>  Closest point interpolations
C
C     GRIDPPI flag set in CRTSET:
C
C        IPPI     = 0  ==>  Normal interpolations to cartesian grid
C        IPPI     = 1  ==>  XY interpolations to constant elevation surfaces
C        ILLE     = 1  ==>  LonLat interpolations to constant elevation surfaces
C        ILLZ     = 1  ==>  LonLat interpolations to constant height surfaces
C
C     Note: KZA and KAZ are the same integer array of azimuths (BEAMIN)
C           MXFLD (in routine parameter list) and MAXFLD are same variable
C           True value for field(i)= sclif(i,2) * integer value
C           True value for azimuth = sclaz * integer value in array KZA
C
C     Note: Local unfolding of velocities - want folding index (k) from the
C            V(true) = V(meas) + 2*k*Vnyq or k = (V_t - V_m)/(2*V_n)
C     Use one of the measured velocities as an estimate of the true value.
C     A scaled number (MN) is computed from IRAYV(J,K,L) which is the 
C     velocity*scale.  This number which is computed as
C         MN=NINT((FLOAT(IRAYV(J,K,L))/FLOAT(MAXVEL))*UNSNYQ).
C      With UNSNYQ = 15 and SCLNYQ=1/15, when IRAYV = MAXVEL = NYQ, MN = 15.
C      Reference velocity for unfolding becomes ISTAN = MAXVEL*MN*SCLNYQ
C             MN = (Vel/Vnyq)*Unsnyq ==> ISTAN = Vel
C
      INCLUDE 'SPRINT.INC'
c-----PARAMETER (MAXEL=150,NID=129+3*MAXEL)
c-----PARAMETER (NIOB=85000,MAXIN=8500,MAXLEN=MAXIN/4)
c-----PARAMETER (MAXRNG=1024,MAXFLD=16)
c-----PARAMETER (IDIM=64/WORDSZ)
c-----DATA IBAD/-32768/

      PARAMETER (NRHD=10)
      DIMENSION Z(2),IDAT(MXFLD),IREF(MXFLD),IT(4),IVVAR(IDIM),KZA(3)
      DIMENSION IBLV(IDIM,IDIM2,NRCBF,2)
      LOGICAL IS360
      COMMON /IO/ KPCK(NIOB),IRAY(MAXIN,3),KAZ(3),KAZC,KAZP,IDSNEW,
     X     ITMDIF,ITIME(4),IBEGT(4),NSTBM,NRDOPT,ILSTREC
      COMMON /SCNDAT/ ELB(MAXEL),DEI(MAXEL),KDIR(MAXEL),KNDREC(MAXEL),
     X     NEL,IEL1,IEL2,KEL,KPEL,ISD,ELTOL,RNOT,DRG,NG,MAXRD,
     X     CFAC(MAXFLD)
      COMMON/ADJUST/INFLD(MAXFLD,3),SCLIF(MAXFLD,2),NIF,IOFLD(MAXFLD,3),
     X     SCLOF(MAXFLD,2),NOF,SCLAZ,SCLRNG,SCLEL,UNSCAZ,UNSCRG,UNSCEL,
     X     LOWAZ,IZAD,IS360,MINAZ,MAXAZ,METSPC(MAXFLD),IWFLD(MAXFLD),
     X     NFLI,SCLNYQ,UNSNYQ

      COMMON /CFLDC/ IFIELD(MAXFLD), INTINF(MAXFLD,3)
      CHARACTER*8 INTINF,IFIELD

      COMMON /FNAMES/ CINFLD(MAXFLD),CIOFLD(MAXFLD)
      COMMON /NGATES/ NRGC,NRGP
      CHARACTER*8 CINFLD, CIOFLD
      EQUIVALENCE (METSPC(1),METH),(METSPC(2),NGAVG),(METSPC(3),MINGN),
     X            (METSPC(4),MXREST),(METSPC(5),IGSPAC),
     X            (METSPC(7),MGC1),(METSPC(8),MGC0),(METSPC(9),MINGC)
      DATA VELMAX/60.0/
      DATA IFRST/0/
      DATA ATR/ 0.017453293/
C      DATA MASKCP/ O'1777777777777777777776' /
      DATA MASKCP/0/
      DATA MASK/O'177777'/

      DIMENSION RIT(4)
      CHARACTER*1 UNFOLD
C
C     In-line functions: JPOS and IRAYV
C
      JPOS(J,K)= NRHD+J+(K-1)*NRGC
      IRAYV(J,I,K)=IRAY((NRHD+J+(K-1)*(NRGC*ABS(KAZP-I)+NRGP*
     X     ABS(KAZC-I))),I)
cqual-print *,'TRPD-IBLV: idim*idim2*nrcbf*2=',idim*idim2*nrcbf*2
cqual-print *,'TRPD: idim,ivvar=',idim,ivvar
C
C     CREATE A BIT MASK USED FOR FLAGGING TYPE OF INTERPOLATION DONE
C
      IF (IFRST.EQ.0) THEN
         MASKCP = 0
         DO I=1,(WORDSZ/16)
            MASKCP = ICEDOR(MASKCP,ICEDSHFT(MASK,(I-1)*16))
         END DO
         MASKCP = MASKCP - 1
         IFRST=1
      END IF
      DO I=1,IDIM
         IVVAR(I)=0
      END DO
      RGKM=IRNG*SCLRNG
      RGNUM=(RGKM-RNOT)/DRG + 1.0
      NST=0
      I1=RGNUM
      I2=I1+1
      J=RGNUM+0.5
      D1=1.0
      D2=1.0
      MXRGD=ABS(RGNUM-FLOAT(J)) * IGSPAC
      MXAZD=0
      IF(KZA(KAZP).EQ.KZA(KAZC)) GO TO 3
      D1=IABS(KZA(KAZP)-KZLV)
      D2=IABS(KZA(KAZC)-KZLV)
      MXAZD=IRNG*SIN(AMIN1(D1,D2)*SCLAZ*ATR)
c-----write(8,*) 'Trpd: mxrest,mxrgd,mxazd=',mxrest,mxrgd,mxazd
 3    CONTINUE
      K=KAZP
      IF(D1.GT.D2) K=KAZC
C      IVVAR=0

      IF(METH.NE.1) GO TO 60
C
C     METHOD 1- BI-LINEAR INTERPOLATION
C               NGAVG> 1: WITH    RANGE SMOOTHING
C               NGAVG<=1: WITHOUT RANGE SMOOTHING
C     METHOD 2- CLOSEST POINT INTERPOLATION 
C
      DIV=1./(D1+D2)
      DFRAC=D1*DIV
      IF (DFRAC.GE.1.0) DFRAC=D2*DIV
      IF(NGAVG.GT.1) GO TO 30
C
C     METHOD 1- WEIGHTED BI-LINEAR INTERPOLATION (4 PTS. ONLY)
C               WITHOUT RANGE SMOOTHING
C
      FPDF=4.0+DFRAC
      W1=I2-RGNUM
      W2=RGNUM-I1
c-----write(8,*)'Trpd: nfli,iwfld=',nfli,(iwfld(i),i=1,nfli)
      DO 20 I=1,NFLI
         L=IWFLD(I)
         IDAT(I)=ITMDIF
         IF(L.LT.0)GO TO 20
         N1=JPOS(I1,L)
         N2=N1+1
         NEAR=JPOS(J,L)
         IF(IRAYV(I1,1,L).EQ.IBAD.OR.IRAYV(I1+1,1,L).EQ.IBAD .OR.
     X      IRAYV(I1,2,L).EQ.IBAD.OR.IRAYV(I1+1,2,L).EQ.IBAD) GO TO 19
C     
C        PROCEED WITH INTERPOLATION- ALL POINTS ARE GOOD.  Except branch
C        to 5 if any data transformation (INFLD(L,3).NE.0) is being done.
C
         IF(INFLD(L,3).NE. 0) GO TO 5
         Z1=W1*IRAYV(I1,KAZC,L)+W2*IRAYV(I1+1,KAZC,L)
         Z2=W1*IRAYV(I1,KAZP,L)+W2*IRAYV(I1+1,KAZP,L)
         IDEST=NINT((Z1*D1+Z2*D2) * DIV)
         IDAT(I)=ICEDOR(IDEST,1)

c--------debug (ljm)
         if(debug)then
            az=sclaz*kzlv
            razp=sclaz*kza(kazp)
            razc=sclaz*kza(kazc)
            rit(1)=sclif(i,2)*irayv(i1,  kazc,l)
            rit(2)=sclif(i,2)*irayv(i1+1,kazc,l)
            rit(3)=sclif(i,2)*irayv(i1,  kazp,l)
            rit(4)=sclif(i,2)*irayv(i1+1,kazp,l)
            ridest=sclif(i,2)*idat(i)
            write(8,1770)ifield(i)(1:4),rgkm,razp,az,razc,
     +           (rit(m),m=1,4),ridest
 1770       format(' Trpdf: fld=',a4,' Rng=',f7.3,
     +           '  azp,az,azc=',3f7.2,
     +           '  Fld=',4f7.2,' Fldout=',f7.2)
         end if
c--------debug (ljm)
         
         GO TO 20
 5       CONTINUE
         IF(INFLD(L,2).GT.2) GO TO 10
C
C        CONVERT 10(LOG) FIELD TO LINEAR UNITS
C
         SFAC=0.1*SCLIF(L,2)
         Z1=W1*EXP(IRAYV(I1,KAZC,L)*SFAC)
     x     +W2*EXP(IRAYV(I1+1,KAZC,L)*SFAC)
         Z2=W1*EXP(IRAYV(I1,KAZP,L)*SFAC)
     x     +W2*EXP(IRAYV(I1+1,KAZP,L)*SFAC)
         IDEST=NINT(ALOG((Z1*D1+Z2*D2)*DIV)*SCLIF(L,1)*10.)
         IDAT(I)=ICEDOR(IDEST,1)
         GO TO 20
 10      CONTINUE
C
C        UNFOLD VELOCITIES FIRST - TRPPPI Nyquist velocity comes from level header
C
c         IF(IPPI.EQ.1)THEN
            NYQ=VNYQUIST*SCLIF(I,1)
c         ELSE
c            NYQ=INFLD(L,3)
c         END IF
         VNYQ=NYQ*SCLIF(I,2)
         IF(VNYQ .LE. 0.0)THEN
            PRINT 11
 11         FORMAT(/1X,
     X           '+++  TRPD ERROR - NYQUIST VELOCITY IS ZERO -',
     X           ' CANNOT DO LOCAL UNFOLDING +++')
            STOP
         END IF
         NYQT2=NYQ*2
         MAXVEL=NYQ
         VDIV=DIV*512.0

C        Don't stop program if Nyquist changes.
C
         IF (IFLG.EQ.0 .OR. MN.EQ.-25) THEN
            MN=NINT((FLOAT(IRAYV(J,K,L))/FLOAT(MAXVEL))*UNSNYQ)
            IF (IABS(MN).GT.15 .AND. NUMFILT.EQ.0) THEN
C     Velocity outside Nyquist co-interval
               ITYP=0
               CALL TPQERX(328,ITYP)
               VEL=FLOAT(IRAYV(J,K,L))*SCLIF(I,2)
               write(8,*) 
     +         '     Trpd: mn,vnyq,k,vel=',mn,vnyq,k,vel
               rit(1)=sclif(i,2)*IRAYV(I1,KAZC,L)
               rit(2)=sclif(i,2)*IRAYV(I1+1,KAZC,L)
               rit(3)=sclif(i,2)*IRAYV(I1,KAZP,L)
               rit(4)=sclif(i,2)*IRAYV(I1+1,KAZP,L)
               write(8,*) 
     +         '     Trpd: all four vels=',(rit(m),m=1,4)
               IF(ITYP.EQ.1)THEN
                  GO TO 70
               ELSE
                  GO TO 19
               END IF
            END IF
         END IF
         
         IF (IABS(MN).GT.15 .AND. NUMFILT.EQ.0) THEN
C     Velocity outside Nyquist co-interval
            ITYP=0
            CALL TPQERX(328,ITYP)
            VEL=FLOAT(IRAYV(J,K,L))*SCLIF(I,2)
            write(8,*) 
     +      '     Trpd: mn,vnyq,k,vel=',mn,vnyq,k,vel
            rit(1)=sclif(i,2)*IRAYV(I1,KAZC,L)
            rit(2)=sclif(i,2)*IRAYV(I1+1,KAZC,L)
            rit(3)=sclif(i,2)*IRAYV(I1,KAZP,L)
            rit(4)=sclif(i,2)*IRAYV(I1+1,KAZP,L)
            write(8,*) 
     +      '     Trpd: all four vels=',(rit(m),m=1,4)
            IF(ITYP.EQ.1)THEN
               GO TO 70
            ELSE
               GO TO 19
            END IF
         END IF
         
         ISTAN=(MAXVEL*MN)*SCLNYQ
         IT(1)=IRAYV(I1,KAZC,L)
         IT(2)=IRAYV(I1+1,KAZC,L)
         IT(3)=IRAYV(I1,KAZP,L)
         IT(4)=IRAYV(I1+1,KAZP,L)

c--------debug (ljm) - before unfolding of velocity field 
         if(debug)then
            rit(1)=sclif(i,2)*it(1)
            rit(2)=sclif(i,2)*it(2)
            rit(3)=sclif(i,2)*it(3)
            rit(4)=sclif(i,2)*it(4)
            unfold='B'
            ridest=sclif(i,2)*istan
            az=sclaz*kzlv
            razp=sclaz*kza(kazp)
            razc=sclaz*kza(kazc)
            write(8,1771)ifield(i)(1:4),rgkm,razp,az,razc,
     +           (rit(m),m=1,4),ridest,vnyq,unfold
 1771       format(' Trpdv: fld=',a4,' Rng=',f7.3,
     +           '  azp,az,azc=',3f7.2,
     +           '  Fld=',4f7.2,' Refvel=',f7.2,' vnyq=',f5.2,a2)
         end if
c--------debug (ljm)

c         IVVAR=0
         SUM=0.0
         SUMSQ=0.0

         unfold='N'
         DO 15 M=1,4
            IDIF=ISTAN-IT(M)
            IF(IABS(IDIF).GT.NYQ) THEN
               KN=(IDIF+ISIGN(NYQ-1,IDIF))/NYQT2
               IT(M)=IT(M)+KN*NYQT2
               unfold='Y'
            END IF
            VELOCU=IT(M)*SCLIF(L,2)
            SUM=SUM+VELOCU
            SUMSQ=SUMSQ+VELOCU*VELOCU
 15      CONTINUE

         CALL IVVPCK(IVVAR,IDIM,FPDF,SUM,SUMSQ)

         Z1=W1*IT(1)+W2*IT(2)
         Z2=W1*IT(3)+W2*IT(4)
C         IDAT(I)=(NINT((Z1*D1+Z2*D2)*VDIV/64.)*32+(16+MN))*2+1
         IDEST=NINT((Z1*D1 + Z2*D2) *DIV)
         IDAT(I)=ICEDOR(IDEST,1) 

c-----debug (ljm) - after unfolding of velocity field
         if(debug)then
            rit(1)=it(1)*sclif(i,2)
            rit(2)=it(2)*sclif(i,2)
            rit(3)=it(3)*sclif(i,2)
            rit(4)=it(4)*sclif(i,2)
            ridest=idat(i)*sclif(i,2)
            razp=sclaz*kza(kazp)
            razc=sclaz*kza(kazc)
            az=sclaz*kzlv
            write(8,1772)ifield(i),rgkm,razp,az,razc,
     +           (rit(m),m=1,4),ridest,vnyq,unfold
 1772       format(' Trpdv: fld=',a4,' Rng=',f7.3,
     +           '  azp,az,azp=',3f7.2,
     +           '  Fld=',4f7.2,' Fldout=',f7.2,' vnyq=',f5.2,a2)
         end if
c-----debug (ljm)

         GO TO 20

 19      CONTINUE
C
C        SELECT THE CLOSEST POINT IF WITHIN RANGE 
C           Larger of user-specified distance or gate spacing.
C           INTERP: DMIN*UNSCRG --> METSPC(6)
C           METHOD: MAX(SCLRNG*METSPC(6),DRG) --> METSPC(4)
C           TRPD:   METSPC(4) --> MXREST
C
         IDAT(I)=IBAD
         IF(MXRGD.GT.MXREST .OR. MXAZD.GT.MXREST) GO TO 20
         IDAT(I)=ICEDAND(IRAYV(J,K,L),MASKCP)
 20   CONTINUE
c-----write(8,*)'Trpd-return to trpppi after 20'
      RETURN

 30   CONTINUE
C
C     METHOD 1- AFTER RANGE SMOOTHING, WEIGHTED ANGULAR INTERPOLATION
C
      NGUSE=NINT(RGKM*(FLOAT(MGC1)*0.001) + (FLOAT(MGC0)*0.001))
      MING=MAX0(MINGN,NGUSE-MINGC)
      NGUSE=MAX0(NGUSE,NGAVG)
      N=NGUSE/2
C
C        EVEN else ODD NUMBER OF GATES TO SMOOTH
C
      IF(NGUSE-N*2.EQ.0) THEN
C        EVEN NUMBER
         I1=I2-N
         I2=I1+NGUSE-1
      ELSE
C        ODD NUMBER
         I1=J-N
         I2=J+N
      END IF

      IF(I1.LT.1) I1=1
      IF(I2.GT.MIN(NRGC,NRGP)) I2=MIN(NRGC,NRGP)

      DO 50 I=1,NFLI
         L=IWFLD(I)
         IDAT(I)=ITMDIF
         IF(L.LT.0) GO TO 50
         N1=JPOS(I1,L)
         N2=JPOS(I2,L)
         NFG=JPOS(0,L)
         NEAR=JPOS(J,L)
         IF(INFLD(L,3).NE.0) GO TO 35
C
C        NO ADJUSTMENTS TO THE DATA ARE NECESSARY
C
         DO 32 KB=1,2
            KS=0
            IDR=0
            Z(KB)=0.0
            DO 31 N=N1,N2
               IF(IRAYV(I1+(N-N1),KB,L).EQ.IBAD) GO TO 31
               KS=KS+1
               IDR=IDR+N-NFG
               Z(KB)=Z(KB)+IRAYV(I1+(N-N1),KB,L)
 31         CONTINUE
            IF(KS.LT.MING) GO TO 49
            IDR=(ABS((FLOAT(IDR)/FLOAT(KS))-RGNUM))*IGSPAC
            IF(IDR.GT.MXREST) GO TO 49
            Z(KB)=Z(KB)/FLOAT(KS)
 32      CONTINUE
         IDEST=NINT((Z(KAZC)*D1+Z(KAZP)*D2)*DIV)
         IDAT(I)=ICEDOR(IDEST,1)
         GO TO 50

 35      CONTINUE
         IF(INFLD(L,2).GT.2) GO TO 40
C
C        CONVERT 10(LOG) FIELD TO LINEAR POWER UNITS FIRST
C
         SFAC=0.1*SCLIF(L,2)
         DO 38 KB=1,2
            KS=0
            IDR=0
            Z(KB)=0.0
            DO 37 N=N1,N2
               IF(IRAYV(I1+(N-N1),KB,L).EQ.IBAD) GO TO 37
               KS=KS+1
               IDR=IDR+N-NFG
               Z(KB)=Z(KB)+EXP(IRAYV(I1+(N-N1),KB,L)*SFAC)
 37         CONTINUE
            IF(KS.LT.MING) GO TO 49
            IDR=(ABS((FLOAT(IDR)/FLOAT(KS))-RGNUM))*IGSPAC
            IF(IDR.GT.MXREST) GO TO 49
            Z(KB)=Z(KB)/FLOAT(KS)
 38      CONTINUE
         IDEST=NINT(ALOG((Z(KAZC)*D1+Z(KAZP)*D2)*DIV)*SCLIF(L,1)*10.)
         IDAT(I)=ICEDOR(IDEST,1)
         GO TO 50
         
 40      CONTINUE
C
C        UNFOLD VELOCITIES FIRST
C
         NYQ=INFLD(L,3)
         NYQT2=NYQ*2
         MAXVEL=NYQ
         VDIV=DIV*512.0
         IF (IFLG.EQ.1 .AND. MN.NE.-25) THEN
            ISTAN=(MAXVEL*MN)*SCLNYQ
         ELSE IF(IRAYV(J,K,L).NE.IBAD .AND. (IFLG.EQ.0 .OR. MN.EQ.-25))
     X           THEN
            MN=NINT((FLOAT(IRAYV(J,K,L))/FLOAT(MAXVEL))*UNSNYQ)
            IF(IABS(MN).GT.15 .AND. NUMFILT.EQ.0) THEN
C     Velocity outside Nyquist co-interval
               ITYP=0
               CALL TPQERX(328,ITYP)
               VEL=FLOAT(IRAYV(J,K,L))*SCLIF(I,2)
               write(8,*) 
     +         '     Trpd: mn,vnyq,k,vel=',mn,vnyq,k,vel
               rit(1)=sclif(i,2)*IRAYV(I1,KAZC,L)
               rit(2)=sclif(i,2)*IRAYV(I1+1,KAZC,L)
               rit(3)=sclif(i,2)*IRAYV(I1,KAZP,L)
               rit(4)=sclif(i,2)*IRAYV(I1+1,KAZP,L)
               write(8,*) 
     +         '     Trpd: all four vels=',(rit(m),m=1,4)
               IF(ITYP.EQ.1)THEN
                  GO TO 70
               ELSE
                  GO TO 49
               END IF
            END IF
            ISTAN=(MAXVEL*MN)*SCLNYQ
         ELSE
            ISTAN=IBAD
         END IF
c         IF(IABS(MN).GT.15) THEN
c     Velocity outside Nyquist co-interval
c            CALL TPQERX(328,0)
c            VEL=FLOAT(IRAYV(J,K,L))*SCLIF(I,2)
c            write(8,*) 
c     +      '     Trpd: mn,vnyq,k,vel=',mn,vnyq,k,vel
c            rit(1)=sclif(i,2)*IRAYV(I1,KAZC,L)
c            rit(2)=sclif(i,2)*IRAYV(I1+1,KAZC,L)
c            rit(3)=sclif(i,2)*IRAYV(I1,KAZP,L)
c            rit(4)=sclif(i,2)*IRAYV(I1+1,KAZP,L)
c            write(8,*) 
c     +      '     Trpd: all four vels=',(rit(m),m=1,4)
c            GO TO 49
c         END IF
C         IVVAR=0
         CNT=0.0
         SUM=0.0
         SUMSQ=0.0
         
         DO 42 KB=1,2
            KS=0
            IDR=0
            Z(KB)=0.0
            
            DO 41 N=N1,N2
               IF(IRAYV(I1+(N-N1),KB,L).EQ.IBAD) GO TO 41
               KS=KS+1
               IDR=IDR+N-NFG
               IF(ISTAN.EQ.IBAD) THEN
                  MN=NINT((FLOAT(IRAYV(I1+(N-N1),KB,L))/FLOAT(MAXVEL))*
     X                 UNSNYQ)
                  IF (IABS(MN).GT.15 .AND. NUMFILT.EQ.0) THEN
C     Velocity outside Nyquist co-interval
                     ITYP=0
                     CALL TPQERX(328,ITYP)
                     VEL=FLOAT(IRAYV(I1+(N-N1),KB,L))
                     write(8,*) 
     +               '     Trpd: mn,vnyq,k,vel=',mn,vnyq,kb,vel
                     rit(1)=sclif(i,2)*IRAYV(I1,KAZC,L)
                     rit(2)=sclif(i,2)*IRAYV(I1+1,KAZC,L)
                     rit(3)=sclif(i,2)*IRAYV(I1,KAZP,L)
                     rit(4)=sclif(i,2)*IRAYV(I1+1,KAZP,L)
                     write(8,*) 
     +               '     Trpd: all four vels=',(rit(m),m=1,4)
                     IF(ITYP.EQ.1)THEN
                        GO TO 70
                     ELSE
                        GO TO 41
                     END IF
                  END IF
                  ISTAN=(MAXVEL*MN)*SCLNYQ
               END IF
               KN=0
               IDIF=ISTAN-IRAYV(I1+(N-N1),KB,L)
               IF(IABS(IDIF).GT.NYQ) KN=(IDIF+ISIGN(NYQ-1,IDIF))/NYQT2
               VELOCU=(IRAYV(I1+(N-N1),KB,L)+KN*NYQT2)*SCLIF(L,2)
               CNT=CNT+1.0
               SUM=SUM+VELOCU
               SUMSQ=SUMSQ+VELOCU*VELOCU
               Z(KB)=Z(KB)+IRAYV(I1+(N-N1),KB,L)+KN*NYQT2
 41         CONTINUE

            IF(KS.LT.MING) GO TO 49
            IDR=(ABS((FLOAT(IDR)/FLOAT(KS))-RGNUM))*IGSPAC
            IF(IDR.GT.MXREST) GO TO 49
            Z(KB)=Z(KB)/FLOAT(KS)
 42      CONTINUE

C         IDAT(I)=(NINT((Z(KAZC)*D1+Z(KAZP)*D2)*VDIV/64.)*32+(16+MN))*2+1
         IDEST=NINT((Z(KAZC)*D1 + Z(KAZP)*D2)*DIV)
         IDAT(I)=ICEDOR(IDEST,1)
         CALL IVVPCK(IVVAR,IDIM,CNT+DFRAC,SUM,SUMSQ)
         GO TO 50

 49      CONTINUE
C
C        BAD DATA FOUND- SELECT CLOSEST POINT INSTEAD OF INTERPOLATING
C
         IDAT(I)=IBAD
         IF(MXRGD.GT.MXREST.OR.MXAZD.GT.MXREST) GO TO 50
         IDAT(I)=ICEDAND(IRAYV(J,K,L),MASKCP)
 50   CONTINUE
c-----write(8,*)'Trpd-return to trpppi after 50'
      RETURN

 60   CONTINUE
C
C     METHOD 2- SELECT THE CLOSEST POINT FOR ALL FIELDS/LOCATIONS
C
      DO 65 I=1,NFLI
         L=IWFLD(I)
         IDAT(I)=ITMDIF
         IF(L.LT.0) GO TO 65
         IDAT(I)=IBAD
         IF(MXRGD.GT.MXREST .OR. MXAZD.GT.MXREST) GO TO 65
         N=JPOS(J,IWFLD(I))
         IDAT(I)=ICEDAND(IRAYV(J,K,L),MASKCP)
 65   CONTINUE
c-----write(8,*)'Trpd-return to trpppi after 65'
      RETURN

 70   CONTINUE
C
C     An error occurred - no interpolation 
C
      NST=1
c-----write(8,*)'Trpd-return to trpppi: nst=',nst
      RETURN
      END
