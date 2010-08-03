      SUBROUTINE TRPARVOL(ICART,ICTAB,ICOB,NDX,NDY,NDZ,YRTAB,
     X     IBLV,NST,DASANG,IFLAT,NUMFILT,XRTAB,IVREF)
C
C     Handles the interpolation of a volume of airborne data from either
C     For- or Aft-looking constant TILT-ANGLE scans.  These scans are
C     interpolated as if they are constant-elevation angle scans with "z" 
C     axis in the direction the plane is flying and "horizontal planes" 
C     perpendicular to this direction.  After interpolation the "xyz"
C     coordinates are rearranged to conform to normal Cartesian coordinats,
C     but with the angle of the +X-axis 90 deg to the right of the flight
C     direction (azimuth of aircraft track over the ground = heading + drift).
C     The XYZ-origin is automatically picked as the first lat/lon position 
C     of the aircraft within the requested time, with the final +Y-direction 
C     in the track direction, +X-direction out the right wing, and the final 
C     +Z-direction upward.
C
C     Airborne (moving platform) angles:
C        ROLL - angle of wings (0) horizontal, (+) left wing is up
C        PITCH - angle of nose (0) horizontal, (+) nose is up
C        HEADING - azimuth angle of nose (0) True north, (+) Clockwise looking down
C        DRIFT - (0) drift equals heading, (+) motion vector more CW than heading
C        ROTATION - angle between radar beam and vertical axis of the aircraft
C                 (0) along vertical stabilizer, (+) clockwise looking forward
C        TILT - angle between the radar beam when it is in a plane containing
C               the longitudinal axis of the aircraft (Eldora has two beams, one
C               is for-looking and one is aft-looking, both near 15 deg.)
C               (0) perpendicular to longitudinal axis, (+) towards aircraft nose   
C        Provided that you are looking in the direction that the aircraft nose
C        is pointed and think of this as the +Z axis of a ground-based radar, 
C        ROTATION is analagous to the azimuth angle and TILT is analagous to the
C        elevation angle of the ground-based radar.
C
C     Coplane flag set in RADAR (DORADE format)
C
C     ICOPLANE = 5  ==>  AIRBORNE TILT-ANGLE SCANS, INTERPOLATING TO CART GRID
C
      INCLUDE 'SPRINT.INC'

c-----PARAMETER (MAXEL=150,NID=129+3*MAXEL)
c-----PARAMETER (NIOB=85000,MAXIN=8500,MAXLEN=MAXIN/4)
c-----PARAMETER (IDIM=64/WORDSZ,MAXWRD=IDIM*WORDSZ/INTSZ)
c-----PARAMETER (IDIM2=4,MAXFLD=IDIM2*MAXWRD)
c-----PARAMETER (NRCBF=400000,IVDIM=(NRCBF+1)/(WORDSZ/8))
c-----PARAMETER (MAXPLN=65536)

      PARAMETER (MAXBMS=1000)
      LOGICAL IS360,IFN360
      DIMENSION ICART(MAXPLN),ICOB(IDIM,IDIM2,NDX,NDY,NDZ),
     X     YRTAB(MXCRT,2),IBLV(IDIM,IDIM2,NRCBF,2),IDATL(MAXFLD),
     X     IDATH(MAXFLD),IVREF(IVDIM,2),IDATCB(MAXFLD),ICTAB(4),
     X     KZBUF(2,2),IBKNT(4,2),IVV2(IDIM),XRTAB(MXCRT,2)
      DIMENSION RDATH(MAXFLD)
      COMMON /NGATES/ NRGC,NRGP
      COMMON /IDBLK/ID(NID)
      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X     ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      COMMON /IO/ KPCK(NIOB),IRAY(MAXIN,3),KAZ(3),KAZC,KAZP,IDSNEW,
     X     ITMDIF,ITIME(4),IBEGT(4),NSTBM,NRDOPT,ILSTREC
      COMMON /SCNDAT/ ELB(MAXEL),DEI(MAXEL),KDIR(MAXEL),KNDREC(MAXEL),
     X     NEL,IEL1,IEL2,KEL,KPEL,ISD,ELTOL,RNOT,DRG,NG,MAXRD,
     X     CFAC(MAXFLD)
      COMMON/ADJUST/INFLD(MAXFLD,3),SCLIF(MAXFLD,2),NIF,IOFLD(MAXFLD,3),
     X     SCLOF(MAXFLD,2),NOF,SCLAZ,SCLRNG,SCLEL,UNSCAZ,UNSCRG,UNSCEL,
     X     LOWAZ,IZAD,IS360,MINAZ,MAXAZ,METSPC(MAXFLD),IWFLD(MAXFLD),
     X     NFLI,SCLNYQ,UNSNYQ
      COMMON /FNAMES/ CINFLD(MAXFLD),CIOFLD(MAXFLD)
      CHARACTER*8 CINFLD, CIOFLD
      COMMON /SETAZM/ USAZ1,USAZ2,USGAP,USINC,IFOR36,IFORDR
      COMMON /RHIS/ IRHICRS,LOWAZ2,MAXAZ2,MINAZ2,ICART2(MAXPLN),
     X     ICTAB2(4),KZV(3),IDSV
      COMMON /COPLAN/ ICOPLN,BASAD
      COMMON /SCAN/ ICOPLANE,IFLGBAS,IPPI,ILLE,ILLZ
      COMMON /AIRBRN/ ALATS(MAXEL),ALONS(MAXEL),IALTFLG,THE_TILT
      REAL   THE_TILT
      COMMON /CINP/IUN,ISKP,ORLAT,ORLON
      COMMON /CINPC/ NMRAD,ITAP
      CHARACTER*4 NMRAD
      CHARACTER*8 ITAP
      LOGICAL ICOPLN
      REAL TEST,YDIST

      DATA ATR /0.0174533/
      DATA IDATL/MAXFLD*0/
      DATA IDATH/MAXFLD*0/
      DATA IDATCB/MAXFLD*0/
      DATA MASK/ O'100000'/
      DATA VNYQ/0.0/

      IRHICRS=0
      ORLAT=ALATS(1)
      ORLON=ALONS(1)
C
C     CREATE A BIT MASK USED TO INTIALIZE ICOB. THE MASK IS
C     32768 (DECIMAL) REPEATED THROUGH THE LENGTH OF THE WORD
C
      INITCB = 0
      DO I=1,(WORDSZ/16)
         INITCB = ICEDOR(INITCB,ICEDSHFT(MASK,(I-1)*16))
      END DO
C     360 SCANS ONLY
      ICROSS=IZAD/2
      MAXEND= USGAP * UNSCAZ

C     (XORTR,ZORTR) is lower left corner of cartesian system rel. to aircraft

      XORTR=X1
      YORTR=Y1
      ZORTR=Z1
c     (LJM - 08/13/09)
      print *,'TRPARVOL: LL corner XYZ=',xortr,yortr,zortr
      DO 3 K=1,NDZ
         DO 2 J=1,NDY
            DO 1 I=1,NDX
               DO 19 L=1,IDIM
                  ICOB(L,1,I,J,K)=INITCB
                  ICOB(L,2,I,J,K)=INITCB
                  ICOB(L,3,I,J,K)=INITCB
                  ICOB(L,4,I,J,K)=INITCB
 19            CONTINUE
 1          CONTINUE
 2       CONTINUE
 3    CONTINUE
      DO 5 I=1,NY
         YRTAB(I,1)=Y1+(I-1)*YD
         YRTAB(I,2)=YRTAB(I,1)**2
 5    CONTINUE
      YDI=1./YD

c-----debugging statements (ljm)
c     user-specified grid for airborne 
c     (LJM - 8/13/09)     
      write(*,1765)x1,x2,xd,nx,ndx
      write(*,1766)y1,y2,yd,ny,ndy
      write(*,1767)z1,z2,zd,nz,ndz
 1765 format('trp: x=',3f8.1,2i8)
 1766 format('     y=',3f8.1,2i8)
 1767 format('     z=',3f8.1,2i8)
      print *,'TRPARVOL: orlat-lon=',orlat,-orlon
c-----debugging statements (ljm)

C
C     LOOP OVER ALL SWEEPS IN VOLUME
C
      NSWPS=ID(35)
      ATR=ATAN(1.)/45.
      RTA=1./ATR
      KL=2
      KH=1
      IDS=0
      IBKNT(2,1)=0
      IBKNT(2,2)=0
      KEL=IEL1-1

c    (LJM - 08/13/09)
      print *,'TRPARVOL: nswps=',nswps 
      DO 200 ISWP=1,NSWPS
c         iprnt_swp=mod(iswp,2000)
c        (LJM - 8/13/09)
         iprnt_swp=0
         KEL=KEL+1
         KAZP=1
         KAZC=2
         IFN360=.TRUE.
         NRDOPT=1
         CALL BEAMIN
c        (LJM - 08/13/09)
         print *,'TRPARVOL: iswp,nstbm=',iswp,nstbm
         IF (NSTBM.NE.0) GOTO 704
         IPTR=36
c         NBEAMS=ID(131+(ISWP-1)*3)
c         DRIFT=REAL(ID(130+(ISWP-1)*3))/REAL(ID(44))
         NBEAMS=ID(IPTR+(ISWP-1)*3)
         DRIFT=REAL(ID(IPTR-1+(ISWP-1)*3))/REAL(ID(44))

c        (LJM - 8/13/09)
         print *,'TRPARVOL: iswp,id(36),id(35)=',iswp,id(36),id(35)
         print *,'TRPARVOL: iswp,nbeams,drift=',iswp,nbeams,drift
         ALAT=ALATS(ISWP)
         ALON=ALONS(ISWP)
         ALON=-ALON
         ORLON2=-ORLON
c        (LJM - 08/13/09)
         print *,'TRPARVOL: alat-lon,xy=',alat,alon,x,yps
         CALL LL2XYDRV(ALAT,ALON,X,YPS,ORLAT,ORLON2,ANGXAX)
         IF (ISWP.NE.NSWPS) THEN
            ALAT=ALATS(ISWP+1)
            ALON=ALONS(ISWP+1)
            ALON=-ALON
            ORLON2=-ORLON
            CALL LL2XYDRV(ALAT,ALON,X,YPE,ORLAT,ORLON2,ANGXAX)
         END IF

c        (LJM - 8/13/09)
         print *,'TRPARVOL: ids,idsnew,yps,ype=',ids,idsnew,yps,ype
         IDS=IDSNEW
         KSAV=KL
         KL=KH
         KH=KSAV
         JL=2.+(IDS*0.5)
         KZLO=IDS*32767
         KZHI=KZLO
C
C     LOOP OVER ALL RAYS IN SWEEP
C
         DO 100 NRAY=2,NBEAMS
c            iprnt_ray=mod(nray,1000)
c           (LJM - 8/13/09)
            iprnt_ray=0
            KSAV=KAZC
            KAZC=KAZP
            KAZP=KSAV
            CALL BEAMIN
            IF (NSTBM.NE.0) GOTO 704
            NRGP=IRAY(8,KAZP)
            NRGC=IRAY(8,KAZC)
            ALAT=IRAY(9,KAZP)/10000.
            ALON=IRAY(10,KAZP)/10000.
            ALON=-ALON
            ORLON2=-ORLON
            CALL LL2XYDRV(ALAT,ALON,X,YP1,ORLAT,ORLON2,ANGXAX)
            IF(THE_TILT .EQ. 0.0) THEN 
               TILT=IRAY(6,KAZP)/64.
            ELSE
               TILT = THE_TILT
            END IF

            IF (NRAY.EQ.2 .AND. ISWP.NE.NSWPS) THEN
C
C     INITIALIZATION FOR LOCATIONS IN FRONT OF THIS SCAN
C
               CALL RSCART(ICART,MAXPLN,NRCBF,ICTAB,KAZ(KAZP),IDS,JL,IC,
     X              IHI,IBKNT(1,KH),KZHI,IS360,NST,IABOVE)
               IF (NST.NE.0) GOTO 800
            END IF
            IF (IS360) ICSAV=IC
c           (LJM - 8/13/09)
            if(iprnt_swp.eq.0.and.iprnt_ray.eq.0)then
               print *,'pre-Scnset: nray,kl,kh,ibknt=',nray,kl,kh,
     +              ibknt(2,kl)
            end if
            IF (NRAY.EQ.2 .AND. IBKNT(2,KL).NE.0) THEN
C
C     INITIALIZATION FOR LOCATIONS BEHIND THIS SCAN
C
               CALL SCNSET(KZBUF(1,KL),IBKNT(1,KL),IBLV(1,1,1,KL),
     X              KAZ(KAZP),IDS,JL,ILO,LPMN,LPMX,KZLO,IS360,IZAD,
     X              ICROSS,NST,IBELOW)
c               print *,'a-Scnset: ibelow=',kaz(kazp),ids,jl,ilo,lpmn,
c     +              lpmx,kzlo,is360,izad,icross,nst,ibelow
               IF (NST.NE.0) GOTO 800
            END IF
            IF (IS360)ILSAV=ILO
            IF (ISWP.NE.NSWPS) THEN
C
C     INTERPOLATION OF XZ-LOCATIONS FORWARD OF THIS SCAN
C
 35            CALL IGETCP2(ICART(IC),KZLV,IX,IZ)
               KZHI=KZLV
               IF(IS360) THEN
C     360 SCANS ONLY - Interpolate in XZ planes at constant Y
C              X    - horizontal  distance from lower-left corner of grid
C              Z    - vertical        "      "       "        "    "   "
C              DIST - distance in XZ-plane   "       "        "    "   "
C            where (XORTR,ZORTR)=(X1,Z1) is the      "        "    "   "
C
                  IF(IABS(KZLV-KAZ(KAZC)).GT.ICROSS)
     X                 KAZ(KAZC)=KAZ(KAZC)+ISIGN(IZAD,KZLV-KAZ(KAZC))
                  IF(IABS(KZLV-KAZ(KAZP)).GT.ICROSS)
     X                 KAZ(KAZP)=KAZ(KAZP)+ISIGN(IZAD,KZLV-KAZ(KAZP))
                  ITST=(KAZ(KAZP)-KZLV)*IDS
                  IF(ITST.GT.0) GO TO 80
               END IF
               
               ITST=(KAZ(KAZC)-KZLV)*IDS
               IF (ITST.LT.0 .AND. ISWP.GT.1) GOTO 55
               IF (ITST.LT.0 .AND. ISWP.EQ.1) THEN
                   GOTO 80
               END IF
               X=(IX-1)*XD
               Z=(IZ-1)*ZD
               RGXZSQ=(X+XORTR)**2. + (Z+ZORTR)**2.
               RGXZ=SQRT(RGXZSQ)
               DIST=RGXZ
               DELY=DIST*TAN(TILT*ATR)
               IY1=((YP1 + DELY) - YRTAB(1,1))*YDI + 2.
               IF (ISWP.NE.NSWPS) THEN
                   YDIST = YPE-YPS
                   IY2=((2*YDIST)+(YP1 + DELY) - YRTAB(1,1))*YDI + 1.
               ELSE
                  IY2=NY
               END IF
               IF (IY1.EQ.0 .AND. IY2.EQ.0) GOTO 38
               IY1=MAX0(IY1,1)
               IY2 = NY
C               IY2=MIN0(IY2,NY)
               IF (IY1.GT.IY2) Then
                   GOTO 38
               ENDIF
               TEST = NINT(YP1+DELY)*1000.
               IF(TEST .GT. 32768) THEN
                   GOTO 38
               END IF                 
C     
C     Grid y locations found between this and next scan at this (x,z) position.
C     
c------debugging statements (ljm)
c              (LJM - 8/13/09)
               if(iprnt_swp.eq.0.and.iprnt_ray.eq.0)then
               write(*,1768)nray,ic,ix,iz,x,z,yps,ype,2*ydist,yp1,dely,
     X              iy1,iy2
 1768          format('trp: nrcxz=',4i4,' xz=',2f8.3,
     X              ' ypsed=',3f8.3,' yp1,dy=',2f8.3,' iy12=',2i4)
               end if
c------debugging statements (ljm)

               DO 37 I=IY1,IY2
                  RNG=RGXZ/COS(TILT*ATR)
                  IRNG=RNG*UNSCRG+0.5
                  RNG=IRNG*SCLRNG
                  IG=(RNG-RNOT)/DRG+1.0
                  IF (IG.LE.0 .OR. IG.GE.NRGC .OR. IG.GE.NRGP) GOTO 37
                  MN=-25
                  CALL TRPD(IDATH,IEL,IRNG,KZLV,IDATL,0,
     X                 IBLV(1,4,IHI,KH),NST,MN,NUMFILT,KAZ,MAXFLD,
     X                 VNYQ,IPPI,ILLE,ILLZ)
c-----debug (ljm)
                  if(iprnt_swp.eq.0.and.iprnt_ray.eq.0)then
                  do nn=1,nof
                     rdath(nn)=sclif(nn,2)*idath(nn)
                  end do
                  azp=sclaz*kaz(kazp)
                  azc=sclaz*kaz(kazc)
                  az =sclaz*kzlv
                  write(*,1769)i,azp,az,azc,ix,iz,(rdath(nn),nn=1,nof)
 1769             format('Trp1 axz: ',i4,3f8.3,2i4,' flds=',16f8.3)
                  end if
c-----debug (ljm)
                  IF (NST.NE.0) GOTO 800
                  IF (WORDSZ.EQ.32) THEN
                     CALL IPUT16(IBLV(1,1,IHI,KH),
     X                    NINT((YP1+DELY)*1000.),I)
                     IBLV(2,1,IHI,KH)=ICART(IC)
C                     IBLV(2,1,IHI,KH)=1500
                  ELSE IF (WORDSZ.EQ.64) THEN
                     CALL IPUT16(IWRD,NINT((YP1+DELY)*1000.),I)
                     IBLV(1,1,IHI,KH)=ICEDOR(IWRD,ICEDSHFT(ICART(IC),
     X                    -32))
                  END IF
                  CALL IPKDAT(IBLV(1,2,IHI,KH),IDATH,NFLI,MAXFLD)
                  ILOC=(IHI-1)/(WORDSZ/8) + 1
                  INDX=(WORDSZ/8) - (ILOC*(WORDSZ/8) - IHI)
                  CALL IPUT8(IVREF(ILOC,KH),INDX,MN)
C                  IVREF(IHI,KH)=MN
                  IHI=IHI+IDS
                  IF (IHI.LE.0 .OR. IHI.GT.NRCBF) GOTO 707
 37            CONTINUE
 38            CONTINUE
               IC=IC+IDS
               IF (IS360) THEN
                  IF (IC.LE.0) IC=ICTAB(2)
                  IF (IC.GT.ICTAB(2)) IC=1
                  IF (IC.EQ.ICSAV) GOTO 45
               END IF
               IF (IC.GT.0 .AND. IC.LE.ICTAB(2)) THEN
                   GOTO 35
               END IF
            END IF

 45         CONTINUE
            KZHI=IDS*32767
 55         CONTINUE
C     
C     INTERPOLATE LOCATIONS BELOW THE CURRENT PPI SCAN AND WRITE 
C     THEM TO OUTPUT BUFFERS
C
c           (LJM - 8/13/09)
            print *,'Trp after 55: ibelow,is360=',ibelow,is360
            ibelow=1
            IF (IBELOW.EQ.1) THEN
               CALL IGETCP(IBLV(1,1,ILO,KL),KZLV,IX,IZ)
               KZLO=KZLV
               IF(IS360) THEN
C     360 SCANS ONLY
                  IF(IABS(KZLV-KAZ(KAZC)).GT.ICROSS)
     X                 KAZ(KAZC)=KAZ(KAZC)+ISIGN(IZAD,KZLV-KAZ(KAZC))
                  IF(IABS(KZLV-KAZ(KAZP)).GT.ICROSS)
     X                 KAZ(KAZP)=KAZ(KAZP)+ISIGN(IZAD,KZLV-KAZ(KAZP))
                  ITST=(KAZ(KAZP)-KZLV)*IDS
                  IF(ITST.GT.0) GO TO 80
               END IF
               ITST=(KAZ(KAZC)-KZLV)*IDS
               IF (ITST.LT.0) GOTO 80
               CALL IGET16(IBLV(1,1,ILO,KL),IYP2,IY)
               X=(IX-1)*XD
               Z=(IZ-1)*ZD
               RGXZ=SQRT((X+XORTR)**2 + (Z+ZORTR)**2)
               IRNG=RGXZ*UNSCRG+0.5
               IRNG=IRNG/COS(TILT*ATR)
               RNG=IRNG*SCLRNG
               IG=(RNG-RNOT)/DRG+1.0
               IF (IG.LE.0 .OR. IG.GE.NRGP .OR. IG.GE.NRGC) GOTO 48
               DELY=(IRNG*SCLRNG)*SIN(TILT*ATR)
               IDISTY=((IY-1)*YD + YORTR - IYP2/1000.)*100.
               DISTY=IDISTY/100.
               YP2=IYP2/1000.
               IF (((IY-1)*YD + YORTR).GT.(YP1+DELY)) GOTO 48
               CALL IGTDAT(IBLV(1,2,ILO,KL),IDATL,NFLI,MAXFLD)
C               MN=IVREF(ILO,KL)
               ILOC=(ILO-1)/(WORDSZ/8) + 1
               INDX=(WORDSZ/8) - (ILOC*(WORDSZ/8) - ILO)
               CALL IGET8(IVREF(ILOC,KL),INDX,MN)
               CALL TRPD(IDATH,IEL,IRNG,KZLV,IDATL,1,IVV2,NST,MN,
     X              NUMFILT,KAZ,MAXFLD,VNYQ,IPPI,ILLE,ILLZ)
c-----debug (ljm)
               if(iprnt_swp.eq.0.and.iprnt_ray.eq.0)then
               do nn=1,nof
                  rdath(nn)=sclif(nn,2)*idath(nn)
               end do
               write(*,1770)i,azp,az,azc,iz,ix,(rdath(nn),nn=1,nof)
 1770          format('Trp2 azx: ',i4,3f8.3,2i4,' flds=',16f8.3)
               end if
c-----debug (ljm)
               IF (NST.NE.0) GOTO 800
C
C     CALCULATE THE IMPORTANT WEIGHTING FACTOR BASED ON Y DISTANCE
C     BETWEEN TWO SWEEPS
               IEL=(DISTY/((YP1+DELY)-YP2))*UNSCEL
               IF (IEL.LT.0) THEN
                  WRITE(*,*)'***IEL,DISTY,YP1,YP2,IY,DELY,TILTP,IRNG=',
     X                 IEL,DISTY,YP1,YP2,IY,DELY,TILTP,IRNG
                  STOP
               END IF
               IF (IEL.GT.(1.0*UNSCEL)) THEN
                  WRITE(*,*)'***(YP1+DELY),((IY-1)*YD + YORTR)=',
     X                 (YP1+DELY),((IY-1)*YD + YORTR)
                  WRITE(*,*)'***DISTY,YP2,YP1=',DISTY,YP2,YP1
                  WRITE(*,*)'***IEL=',IEL
                  STOP
               END IF
               CALL COMBIN(IDATH,IDATL,IDATCB,IEL,IRNG,KZLV,
     X              IBLV(1,4,ILO,KL),IVV2,MAXFLD)
c-----debug (ljm)
               if(iprnt_swp.eq.0.and.iprnt_ray.eq.0)then
               do nn=1,nof
                  rdath(nn)=sclif(nn,2)*idatcb(nn)
               end do
               azp=sclaz*kaz(kazp)
               azc=sclaz*kaz(kazc)
               az =sclaz*kzlv
               write(*,1771)i,azp,az,azc,iz,ix,(rdath(nn),nn=1,nof)
 1771          format(' Cmb azx: ',i4,3f8.3,2i4,' flds=',16f8.3)
               end if
c-----debug (ljm)
               IF (IX.LE.0 .OR. IY.LE.0 .OR. IZ.LE.0) THEN
                  WRITE(*,*)'***IX,IY,IZ=',IX,IY,IZ
                  STOP
               END IF

c------debugging statements (ljm)
c              (LJM - 8/13/09)
               write(*,1772)idath(1),idatl(1),IY,IX,IZ
 1772          format('up-lw:iyxz=',5i8)
c------debugging statements (ljm)

               CALL IPKDAT(ICOB(1,1,IX,IY,IZ),IDATCB,NOF,MAXFLD)
 48            ILO=ILO+IDS
               IF(ILO.GE.LPMN.AND.ILO.LE.LPMX) GO TO 55
               IF (IS360) THEN
                  IF (ILO.NE.ILSAV) THEN
                     IF (ILO.LT.LPMN) ILO=LPMX
                     IF (ILO.GT.LPMX) ILO=LPMN
C
C     GO AND READ NEXT POINT BELOW CURRENT SCAN
C
                     GOTO 55
                  END IF
               END IF
            END IF
            KZLO=IDS*32767
C
C     NO MORE LOCATIONS BELOW THE CURRENT SCAN
C
 80         CONTINUE
 100     CONTINUE
C
C     END OF ELEVATION SCAN
C
c        (LJM - 8/13/09)
         print *,'TRPARVOL: ihi,ids,nrcbf=',ihi,ids,nrcbf
         IHI=IHI-IDS
         IF (IHI.LE.0 .OR. IHI.GT.NRCBF) THEN
            WRITE(*,*)'***IHI ERROR***.IHI,IDS=',IHI,IDS
            STOP
         END IF
         IBKNT(2+IDS,KH)=IHI
         IBKNT(2,KH)=1
         IBKNT(4,KH)=1
         CALL IGETCP(IBLV(1,1,IHI,KH),KZBUF(JL,KH),IX,IZ)
         K=1+(2-JL)*(NRCBF-1)
         CALL IGETCP(IBLV(1,1,K,KH),KZBUF(3-JL,KH),IX,IZ)
 200  CONTINUE
C
C     END OF VOLUME SCAN
C
      NST=0
      RETURN

 704  CALL TPQERX(312,1)
      GOTO 800
 707  CALL TPQERX(315,0)
      GOTO 800
      
 800  CONTINUE
      NST=1

      RETURN
      END
