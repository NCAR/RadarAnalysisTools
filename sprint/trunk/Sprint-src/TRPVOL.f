      SUBROUTINE TRPVOL(ICART,ICTAB,ICOB,NDX,NDY,NDZ,ZRTAB,
     X     IBLV,NST,DASANG,IFLAT,NUMFILT,XRTAB,IVREF,
     X     VNYQUIST,XGRID,YGRID)
C
C     PREPARES FOR ACTUAL INTERPOLATION TASK - Clever, but confusing (LJM).
C     Two beams of radar data are read in (BEAMIN) and used to interpolate
C     to all horizontal grid points which are bounded by these two beams.
C     The grid (x,y) were originally sorted (SINSRT) to be in increasing
C     azimuth order (CRTOUT and CRTTCP call SINSRT).  All this code is
C     doing is determining the affected output grid points and getting the
C     beams (angles stored in KZA) for angular-weighting in TRPD where the
C     actual interpolation is done.  Lots of code is devoted to handling
C     grid points bounded, for example, by angles like 359 and 001 deg.
C
C     Coplane and GRIDPPI flags set in CRTSET or other routines.
C
C     ICOPLANE = 0  ==>  PPI SCANS, INTERPOLATING TO CART GRID
C     ICOPLANE = 1  ==>  COPLANE SCANS, INTERPOLATING TO ANGLES IN DATA
C     ICOPLANE = 2  ==>  COPLANE SCANS, INTERPOLATING TO USER SPEC. ANGLES
C     ICOPLANE = 3  ==>  COPLANE SCANS, INTERPOLATING TO CART GRID
C                        Set in RPNCAR (FOF RP field format) or UFNCAR (UF)
C                        according to sweep mode (2) coplane or (3) RHI
C     ICOPLANE = 4  ==>  RHI SCANS, INTERPOLATING TO CART GRID
C                        Set in RPNCAR (FOF RP field format) or UFNCAR (UF)
C                        according to sweep mode (2) coplane or (3) RHI
C                        Uses COPLANE interpolation code so that azimuth
C                        in the RHIs is 90 - elevation angle.
C     IPPI     = 0  ==>  Normal interpolations to cartesian grid
C     IPPI     = 1  ==>  XY interpolations to constant elevation surfaces
C     ILLE     = 1  ==>  LonLat interpolations to constant elevation surfaces
C     ILLZ     = 1  ==>  LonLat interpolations to constant height surfaces
C
C     MAXPLN - Maximum number of grid points in a plane
C     NPREC  - Maximum number of grid points in a plane
C     NDX    - Number of grid points in x-direction
C     NDY    - Number of grid points in y-direction
C     NDZ    - Number of grid points in e-direction
C     IEL1   - First index in e-direction
C     IEL2   - Last  index in e-direction
C     MXCRT  - Maximum number of grid points in any one direction
C     NRCBF  - Maximum amount of scratch memory 
C     COLO   - Previous value of e-level 
C     COHI   - Current value of e-level 
C
C     See BEAMIN:
C        KAZ(KAZC) - Integer array of azimuths = NINT(SCLAZ * Floating pt)
C        KAZC      - Current ray indexl in arrays IRAY and KAZ
C        KAZP      - Previous ray index in arrays IRAY and KAZ
C        IRAY      - Integer array of (1-NG) field values and housekeeping
C                    IRAY[(I=1,MAXIN),KAZP] = Previous beam's worth of data
C                    IRAY[(I=1,MAXIN),KAZC] = Current  beam's worth of data
C                    IRAY[(I=1,MAXIN),   3] = Copy of current beam for 
C                                             possible 360 deg crossing
C                    First beam in sweep is stored in KAZP=1.
C        NSTBM     - Beam status from BEAMIN: 
C                    (0) New beam, (1) End sweep, (2) Something is wrong
C     
C     Note: NPREC and MAXPLN refer to same parameter
C
C     IBLV   - Contains local velocity variance information
C              (same as IVVAR in routine TRPD which is packed into 64-bit
C               words in routine IVVPCK)
C
      INCLUDE 'SPRINT.INC'

c-----PARAMETER (MAXEL=150,NID=129+3*MAXEL)
c-----PARAMETER (NIOB=85000,MAXIN=8500,MAXLEN=MAXIN/4)
c-----PARAMETER (IDIM=64/WORDSZ,MAXWRD=IDIM*WORDSZ/INTSZ)
c-----PARAMETER (IDIM2=4,MAXFLD=IDIM2*MAXWRD)

c-----PARAMETER (MAXRNG=1024)
c-----PARAMETER (MAXPLN=65536,MAXZ=10,MAXYZ=MAXPLN*MAXZ)
c-----PARAMETER (NRCBF=400000,IVDIM=(NRCBF+1)/(WORDSZ/8))

      LOGICAL IS360,IFN360
      COMMON /FORMAT/ IRP,IBLOCK
      COMMON /NGATES/ NRGC,NRGP
      DIMENSION ICART(MAXPLN),ICOB(IDIM,IDIM2,NDX,NDY,NDZ),
     X     ZRTAB(MXCRT,2),IBLV(IDIM,IDIM2,NRCBF,2),IDATL(MAXFLD),
     X     IDATH(MAXFLD),IVREF(IVDIM,2),IDATCB(MAXFLD),ICTAB(4),
     X     KZBUF(2,2),IBKNT(4,2),IVV2(IDIM),XRTAB(MXCRT,2)
      DIMENSION XGRID(MAXPLN),YGRID(MAXPLN)

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

      COMMON /CFLDC/ IFIELD(MAXFLD), INTINF(MAXFLD,3)
      CHARACTER*8 INTINF,IFIELD

      DIMENSION VNYQUIST(MAXEL)
      LOGICAL NYQ_WARN

c      common /junk/ azc1,azp1,upels(nrcbf,2)

      LOGICAL ICOPLN
      DATA ATR /0.0174533/
      DATA IDATL/MAXFLD*0/
      DATA IDATH/MAXFLD*0/
      DATA IDATCB/MAXFLD*0/
C      DATA INITCB/ 1000004000020000100000B /
      DATA MASK/ O'100000'/
      DATA VNYQ/0.0/

c-----debugging variables (ljm)
      CHARACTER TITL*15,NMB*2
      DIMENSION RAZ(3)

      DATA ICMOD/5000/
C     
C     THE FOLLOWING FUNCTION CALCULATES ELEV ANGLE FROM COPLANE AND HOR AZIM.
C     
c-----Never called
c-----CALEL(F,A)=ATAN((TAN(F*ATR)*ABS(SIN(A*ATR))))/ATR
C
C     CONVERT AZIM IN THE HOR. TO AZIM IN THE VERTICAL FOR RHI SCANS
C
c-----Never called
c-----KZH2KZV(AH,C)=ATAN2(TAN(90.*ATR - ATAN2(TAN(AH*ATR),COS(C*ATR))),
c----X     1./SIN(C*ATR))*RTA*UNSCAZ + 0.5

      FX(X,Y)=ACSF*X-ASNF*Y+XORTR
      FY(X,Y)=ASNF*X+ACSF*Y+YORTR

cqual-print *,'TRPVOL-IBLV: idim*idim2*nrcbf*2=',idim*idim2*nrcbf*2 
      NYQ_WARN=.TRUE.
      DEBUG = .FALSE.
      write(8,*)'TRPVOL: DEBUG=',DEBUG

C     360 SCANS ONLY
      ICROSS=IZAD/2
      MAXEND= USGAP * UNSCAZ

c-----debug (ljm)
      if(debug)then
         ij=ndx+(ndy-1)*ndx
         write(8,*) 'TRPVOL - begin debugging output'
         write(8,*) 'In Trp: ippi,ille,illz,icoplane=',
     +        ippi,ille,illz,icoplane
         write(8,*) 'In Trp:          xorg,yorg,zrad=',
     +        xorg,yorg,zrad
         write(8,*) 'In Trp:          ndx,ndy,ndz,ij=',
     +        ndx,ndy,ndz,ij
         write(8,*) 'In Trp:       xgrid(1),ygrid(1)=',
     +        xgrid(1),ygrid(1)
         write(8,*) 'In Trp:     xgrid(ij),ygrid(ij)=',
     +        xgrid(ij),ygrid(ij)
         write(8,*) 'In Trp:is360,izad,iadjaz,icross,maxend=',
     +        is360,izad,iadjaz,idsnew,icross,maxend
         write(8,*) 'In Trp:      sclrng,sclaz,sclel=',
     +        sclrng,sclaz,sclel
         write(8,*) 'In Trp:    unscrg,unscaz,unscel=',
     +        unscrg,unscaz,unscel
         write(8,*) 'In Trp:            nif,nof,nfli=',
     +        nif,nof,nfli
         write(8,*) 'In Trp:   cinfld - input fields=',
     +        (cinfld(n),n=1,nif)
         write(8,*) 'In Trp:    sclif - input scales=',
     +        (sclif(n,2),n=1,nif)
         write(8,*) 'In Trp:  ciofld - output fields=',
     +        (ciofld(n),n=1,nof)
         write(8,*) 'In Trp:   sclof - output scales=',
     +        (sclof(n,2),n=1,nof)
         write(8,*) 'In Trp:  ifield - output fields=',
     +        (ifield(n),n=1,nfli)
         write(8,*) 'In Trp:   Number of input scans=',
     +        iel1,iel2
         write(8,*) 'In Trp:             Input scans=',
     +        (elb(k),k=iel1,iel2)
         write(8,*) 'In Trp:      Nyquist velocities=',
     +        (vnyquist(k),k=iel1,iel2)
         azl=lowaz/unscaz
         azl2=lowaz2/unscaz
         azmn=minaz/unscaz
         azmn2=minaz2/unscaz
         azmx=maxaz/unscaz
         azmx2=maxaz2/unscaz
         write(8,*) 'In Trp: beamtsts low-min-max az=',
     +        lowaz,lowaz2,minaz,minaz2,maxaz,maxaz2
         write(8,*) 'In Trp: beamtsts low-min-max az=',
     +        azl,azl2,azmn,azmn2,azmx,azmx2
      end if
c-----debug (ljm)

C
C     XORTR,YORTR is lower left corner of cartesian system rel. to radar
C
      IF (ICOPLANE.EQ.0 .OR. ICOPLANE.EQ.3 .OR. ICOPLANE.EQ.4) THEN
C
C     Interpolate PPI, COPLANE, or RHI to regular 3-D cartesian (XYZ) grid
C
         IF(ILLZ.EQ.0)THEN
            XORTR = X1*ACSF - Y1*ASNF - XORG
            YORTR = X1*ASNF + Y1*ACSF - YORG
            ZORTR = Z1 - ZRAD
         ELSE
            XORTR = XGRID(1)
            YORTR = YGRID(1)
         END IF

      ELSE IF (ICOPLANE.EQ.1 .OR. ICOPLANE.EQ.2) THEN
C
C     Interpolate COPLANE to COPLANE coordinate system
C
         ANGRD=(ANGXAX-90.)*ATR
         ASNFKP=SIN(ANGRD)
         ACSFKP=COS(ANGRD)
C
C     Find radar coordinates in rotated coordinate system
C
         XRAD=XORG*ACSFKP - YORG*ASNFKP
         YRAD=XORG*ASNFKP + YORG*ACSFKP
C
C     Find coordinate of lower left hand part of grid, relative to radar

         XORTR = X1-XRAD
         YORTR = Y1-YRAD
      END IF

c-----debug (ljm)
      if(debug)write(8,*) 'In Trp: icoplane, xy (lower left corner)=',
     +     icoplane,xortr,yortr,zortr,z1,zrad
c-----debug (ljm)

C
C     INITIALIZE OUTPUT TABLES AND HEIGHT BUFFERS
C
      IF (ICOPLANE.EQ.1) NDZ=IEL2

C     CREATE A BIT MASK USED TO INITIALIZE ICOB. THE MASK IS
C     32768 (DECIMAL) REPEATED THROUGH THE LENGTH OF THE WORD
C        INITCB:
C        Bits 01-16 => 1000000000000000 = 32768
C        Bits 17-32 => 1000000000000000 = 32768
C        Bits 01-32 => 10000000000000001000000000000000
C           decimal => -2147450880 since sign bit is 1 => Neg
C
      INITCB = 0
      DO I=1,(WORDSZ/16)
         INITCB = ICEDOR(INITCB,ICEDSHFT(MASK,(I-1)*16))
      END DO

      NCNT=0
      DO K=1,NDZ
         DO J=1,NDY
            DO I=1,NDX
               NCNT=NCNT+1
               DO L=1,IDIM
                  ICOB(L,1,I,J,K)=INITCB
                  ICOB(L,2,I,J,K)=INITCB
                  ICOB(L,3,I,J,K)=INITCB
                  ICOB(L,4,I,J,K)=INITCB
               END DO
            END DO
         END DO
      END DO
         
      IF(NCNT.GT.MAXYZ)THEN
         PRINT *
         PRINT *,'     ',
     X   '+++ TRPVOL: INITIALIZED NCNT=',NCNT,' VALUES IN ICOB +++'
         PRINT *,'     ',
     X   '+++ MAX ALLOWED XYZ GRID POINTS (',MAXYZ,') EXCEEDED +++ '
         PRINT *
         STOP
      END IF

      IF (ICOPLANE.EQ.1) THEN
         Z1=ELB(1)
         Z2=ELB(IEL2)
         NZ=IEL2
         ZD=ELB(3)-ELB(2)
         IF (ZD.EQ.0.0 .AND. IPPI.EQ.0) THEN
            WRITE(*,*)'*** INVALID ZD IN TRPVOL: Z1,Z2,ZD,NZ=',
     +           Z1,Z2,ZD,NZ
            STOP
         ELSE
            ZD=(Z2-Z1)/(NZ+1)
            WRITE(*,*)'*** VALID ZD IN TRPVOL: Z1,Z2,ZD,NZ=',
     +           Z1,Z2,ZD,NZ
         END IF
         IF (ZD.EQ.0.0 .AND. ILLZ.EQ.1) THEN
            WRITE(*,*)'*** INVALID ZD IN TRPVOL: Z1,Z2,ZD,NZ=',
     +           Z1,Z2,ZD,NZ
            STOP
         ELSE
            ZD=(Z2-Z1)/(NZ+1)
            WRITE(*,*)'*** VALID ZD IN TRPVOL: Z1,Z2,ZD,NZ=',
     +           Z1,Z2,ZD,NZ
         END IF
      END IF

C     Compute third coordinate for interpolation
C        ZRTAB - Coordinate for vertical interpolation (usually height)
C        XRTAB - Coordinate for horizontal interpolation (horizontal range)
C
      IF (ICOPLANE.EQ.0 .OR. ICOPLANE.EQ.3 .OR. ICOPLANE.EQ.4) THEN
C
C     Interpolate PPI, COPLANE, or RHI to regular 3-D cartesian (XYZ) grid
C
         DO I=1,NZ
            ZRTAB(I,1)=Z1+(I-1)*ZD-ZRAD
            ZRTAB(I,2)=ZRTAB(I,1)**2
         END DO
         DO I=1,NX
            XRTAB(I,1)=X1+(I-1)*XD-XORG
            XRTAB(I,2)=XRTAB(I,1)**2
         END DO
         
      ELSE IF (ICOPLANE.EQ.1 .OR. ICOPLANE.EQ.2) THEN
C
C     Interpolate COPLANE to COPLANE coordinate system
C
         DO 7 I=1,NZ
            ZRTAB(I,1)=Z1+(I-1)*ZD
 7       CONTINUE
      END IF

c-----debug (ljm)
      if(debug)then
         write(8,*) "IN TRPVOL ",Z1,Z2,ZD,NZ
         write(8,*) "IN TRPVOL ",(zrtab(i,1),i=1,nz)
      end if
c-----debug (ljm)

      IF (ICOPLANE.NE.1) ZDI=1./ZD
      IF (ICOPLANE.EQ.4) XDI=1./XD
C
C     CHECK IF CURVED EARTH CORRECTIONS ARE DESIRED
C
      IF (IFLAT.EQ.1) THEN
         AKZI=0.0
      ELSE 
         AKZI=3./(4.*EDIAM)
      END IF
      ATR=ATAN(1.)/45.
      RTA=1./ATR
      KL=2
      KH=1
      IDS=0
      IBKNT(2,1)=0
      IBKNT(2,2)=0
      KEL=IEL1-1
C------------------------------------------------------------------------
C     Top of (kel) loop over all scans in the volume
C     Set flags before calling BEAMIN:
C        KAZC=2    ==> Implies store first beam of sweep in 2nd column of IRAY
C        NRDOPT=1  ==> go to next elev sweep within ELB(IEL1)-ELB(IEL2)
C        KEL  - Index for current  radar sweep (Initially KEL  = IEL1-1)
C        KPEL - Index for previous radar sweep (Initially KPEL = 0)
C        ISD  - 1 * Sign (ELB(2)-ELB(1))
C
C
 10   CONTINUE
      KEL=KEL+1
c-----debug (ljm)
      if(debug)then
         write(8,*) '  '
         write(8,*) '***********************'
         write(8,*) 'K loop - top: kel,kpel,isd,iel1,iel2=',
     +        kel,kpel,isd,iel1,iel2
         write(8,*) 'K loop - top: elv,vny=',elb(kel),vnyquist(kel)
      end if
c-----debug (ljm)

      write(6,1770)kel,elb(kel),vnyquist(kel),iel2
 1770 format('TRPVOL: K-top: kel,elb,vnyq=',i8,2f8.2,i8)
      if(debug)write(8,*)'TRPVOL: K-top: kel,iel2=',
     +     kel,iel2,' elev,vnyq=',elb(kel),vnyquist(kel)
      IF(KEL.GT.IEL2)GO TO 703

C     Set indices for storing previous (KAZP) and current (KAZC)
C     radar scan angles in array KAZ(3).  Read beams with BEAMIN.
C
      KAZP=1
      KAZC=2
C     360 SCANS ONLY
      IFN360=.TRUE.
      NRDOPT=1

C     All input scans and interpolations have IRHICRS = 0,
C     except for RHI scans interpolated to (x,y,z).
C
      IRHICRS=0

      if(debug)write(8,*)'TRPVOL: irhicrs,icoplane=',irhicrs,icoplane
      IF (ICOPLANE.EQ.4) THEN
C     RHI scan --> (x,y,z)
C        Preparations for interpolating RHI scans.  If fixed angle has 
C        crossed 90 or 270, special processing will have to be done.
C        IRHICRS - (0) No special processing required
C                  (1) Scan crosses 090 or 270
C                  (2) Scan crosses 045 or 225
C                  (3) Scan crosses 135 or 315
C
         IF (KEL.GT.1) THEN
            ANG1=ELB(KEL-ISD)
            ANG2=ELB(KEL)
            IF (ANG1.GT.360.0) ANG1=ANG1-360.0
            IF (ANG2.GT.360.0) ANG2=ANG2-360.0
            IF ((ANG1.GE. 45. .AND. ANG2.GE. 45.) .AND. 
     X          (ANG1.LT.135. .AND. ANG2.LT.135.) .OR. 
     X          (ANG1.GE.225. .AND. ANG2.GE.225.) .AND.
     X          (ANG1.LT.315. .AND. ANG2.LT.315.))THEN
               IRHICRS=1
            ELSE IF ((ANG1.LT. 45. .AND. ANG2.GE. 45.) .OR.
     X               (ANG1.LT.225. .AND. ANG2.GE.225.)) THEN
               IRHICRS=2
            ELSE IF ((ANG1.LE.135. .AND. ANG2.GT.135.) .OR.
     X               (ANG1.LE.315. .AND. ANG2.GT.315.)) THEN
               IRHICRS=3
            ELSE
               IRHICRS=0
            END IF
         ELSE
            ANG1=ELB(KEL)
            IF (ANG1.GT.360.0) ANG1=ANG1-360.0
            IF ((ANG1.GE. 45. .AND. ANG1.LT.135.) .OR. 
     X          (ANG1.GE.225. .AND. ANG1.LT.315.)) THEN
               IRHICRS=1
            ELSE
               IRHICRS=0
            END IF
         END IF
         if(debug)write(8,*) 'TRPVOL: kel,crs,ang1,ang2=',
     +        kel,irhicrs,ang1,ang2
      END IF
C
C     Initialize IRAY with 1st and 2nd beams of the current sweep.
C     Read a beam - (1) IRAY - (1-NG) field values and housekeeping
C                       Store beam in IRAY(I=1,MAXIN,KAZC=2) 
C     BEAMIN: Sets KPEL = KEL at the beginning of scan (NRDOPT=1)
C
      CALL BEAMIN

C     NSTBM - beam read status, set in BEAMIN COMMON /IO/
C             (0) New beam, (1) End-of-sweep, (2) Error
C
      IF(NSTBM.NE.0)THEN
         if(debug)then
            if(nstbm.eq.1)write(8,*)'TRPVOL-Beamin: end-of-sweep'
            if(nstbm.eq.2)write(8,*)'TRPVOL-Beamin: read error'
         end if
         GO TO 704
      END IF
         
      KINT=0
      KSAV=KAZC
      KAZC=KAZP
      KAZP=KSAV
c-----debug (ljm)
      if(debug)then
         azp=sclaz*kaz(kazp)
         azc=sclaz*kaz(kazc)
         az=sclaz*kzlv
         write(8,*) 'TRPVOL: after call beamin#1: ',nstbm,azp,az,azc,
     +        kel,kpel
      end if
c-----debug (ljm)
C
C     Read a beam - (1) IRAY - (1-NG) field values and housekeeping
C                       Store beam in IRAY(I=1,MAXIN,KAZC=1)
C
      CALL BEAMIN

C     NSTBM - beam read status, set in BEAMIN COMMON /IO/
C             (0) New beam, (1) End-of-sweep, (2) Error
C
      IF(NSTBM.NE.0)THEN
         if(debug)then
            if(nstbm.eq.1)write(8,*)'TRPVOL-Beamin: end-of-sweep'
            if(nstbm.eq.2)write(8,*)'TRPVOL-Beamin: read error'
         end if
         GO TO 705
      END IF

      IDS=IDSNEW
      KSAV=KL
      KL=KH
      KH=KSAV
      NRGP=IRAY(8,KAZP)
      NRGC=IRAY(8,KAZC)
      JL=2.+(IDS*0.5)
      KZLO=IDS*32767
      KZHI=KZLO

c-----debug (ljm)
      if(debug)then
         azp=sclaz*kaz(kazp)
         azc=sclaz*kaz(kazc)
         az=sclaz*kzlv
         write(8,*) 'TRPVOL: after call beamin#2: ',nstbm,azp,az,azc,
     +        kzlo,kzhi
      end if
c-----debug (ljm)

      IF (ICOPLANE.EQ.1) THEN
         Z1=ELB(1)
         Z2=ELB(IEL2)
         NZ=IEL2
         ZD=ELB(3)-ELB(2)
         IF (ZD.EQ.0.0 .AND. IPPI.EQ.0) THEN
            WRITE(*,*)'*** INVALID ZD IN TRPVOL: Z1,Z2,ZD,NZ=',
     +           Z1,Z2,ZD,NZ
            STOP
         ELSE
            ZD=(Z2-Z1)/(NZ+1)
            WRITE(*,*)'*** VALID ZD IN TRPVOL: Z1,Z2,ZD,NZ=',
     +           Z1,Z2,ZD,NZ
         END IF
         IF (ZD.EQ.0.0 .AND. ILLZ.EQ.1) THEN
            WRITE(*,*)'*** INVALID ZD IN TRPVOL: Z1,Z2,ZD,NZ=',
     +           Z1,Z2,ZD,NZ
            STOP
         ELSE
            ZD=(Z2-Z1)/(NZ+1)
            WRITE(*,*)'*** VALID ZD IN TRPVOL: Z1,Z2,ZD,NZ=',
     +           Z1,Z2,ZD,NZ
         END IF
      END IF
      IF(KEL.EQ.IEL2 .AND. ICOPLANE.NE.1) GO TO 20
C     
C     INITIALIZATION FOR OUTPUT GRID LOCATIONS ABOVE THIS RADAR SCAN
C        Note: Initially KINT = 0, then KINT = 2 always
C     
      KINT=KINT+2

c-----debug (ljm)
      if(debug)then
         write(8,*)'Trp:  irhicrs,icoplane=',irhicrs,icoplane
         write(8,*)'Trp:    ippi,ille,illz=',ippi,ille,illz
         write(8,*)'Trp: kint,kel,kpel,isd=',kint,kel,kpel,isd
         write(8,*)'Trp:     el(hi),el(lo)=',elb(kpel+isd),elb(kpel)
      end if
c-----debug (ljm)

      IF (ICOPLANE.EQ.0 .OR. ICOPLANE.EQ.3) THEN
         TANLO=TAN(ELB(KPEL)*ATR)
         TANHI=TAN(ELB(KPEL+ISD)*ATR)
      ELSE IF (ICOPLANE.EQ.1 .OR. ICOPLANE.EQ.2) THEN
         COLO=ELB(KPEL)
         COHI=ELB(KPEL+ISD)
      ELSE IF (ICOPLANE.EQ.4) THEN
         EL1=ELB(KPEL)
         IF (EL1.GT.360.) EL1=EL1-360.
         EL2=ELB(KPEL+ISD)
         IF (EL2.GT.360.) EL2=EL2-360.
         IF (IRHICRS.EQ.0 .OR. IRHICRS.EQ.3) THEN
            TANLO=TAN(ELB(KPEL)*ATR)
            TANHI=TAN(ELB(KPEL+ISD)*ATR)
            IF (EL1.GT.90.0 .AND. EL1.LT.270.0) THEN
               TANTMP=-TANLO
               TANLO=-TANHI
               TANHI=TANTMP
            END IF
         ELSE IF (IRHICRS.EQ.1 .OR. IRHICRS.EQ.2) THEN
            IF (EL2.LT.225.0) THEN
C     
C     IF CROSSOVER AT 90
C     
               TANHI=TAN((90.-ELB(KPEL))*ATR)
               TANLO=TAN((90.-ELB(KPEL+ISD))*ATR)
               
            ELSE
C
C     IF CROSSOVER AT 270
C
               TANLO=TAN((270.-ELB(KPEL))*ATR)
               TANHI=TAN((270.-ELB(KPEL+ISD))*ATR)
            END IF
         END IF

      END IF

C     RSCART - returns a number (IC) to start indexing over horizontal grid 
C              points.  This would be first grid point inside the radar scan.
C              IC = IX + (IY-1)*NX
C
      if(debug)write(8,*) 'Trpvol-irhicrs,nst,icoplane,ic=',
     +     irhicrs,nst,icoplane,ic
      IF (IRHICRS.EQ.0 .OR. IRHICRS.EQ.3) THEN
         CALL RSCART(ICART,MAXPLN,NRCBF,ICTAB,KAZ(KAZP),IDS,JL,IC,IHI,
     X        IBKNT(1,KH),KZHI,IS360,NST,IABOVE)
      ELSE
         JLV=2.+(IDSV*0.5)
         CALL RSCART(ICART2,MAXPLN,NRCBF,ICTAB2,KZV(KAZP),IDSV,JLV,IC,
     X        IHI,IBKNT(1,KH),KZHI,IS360,NST,IABOVE)
      END IF
      if(debug)write(8,*) 'Trpvol-irhicrs,nst,icoplane,ic=',
     +     irhicrs,nst,icoplane,ic
      IF(NST.NE.0 .AND. ICOPLANE.NE.4)GO TO 800
C
C     360 SCANS ONLY
C
      IF(IS360)ICSAV=IC

 20   CONTINUE

      if(debug)write(8,*) 'Trp-kl,ibknt(2,kl)=',kl,ibknt(2,kl)
      IF(IBKNT(2,KL).EQ.0)THEN 
         if(debug)write(8,*) 'Trp-ibknt(2,kl)=0: going to 30'
         GO TO 30
      END IF
C     
C     INITIALIZATION FOR OUTPUT GRID LOCATIONS BELOW THIS RADAR SCAN
C     
      KINT=KINT+1

      if(debug)write(8,*)'TRPVOL: IRHICRS=',irhicrs
      IF (IRHICRS.EQ.0 .OR. IRHICRS.EQ.2) THEN
         CALL SCNSET(KZBUF(1,KL),IBKNT(1,KL),IBLV(1,1,1,KL),
     X        KAZ(KAZP),IDS,JL,ILO,LPMN,LPMX,KZLO,IS360,IZAD,ICROSS,
     X        NST,IBELOW)
      ELSE
         JLV=2.+(IDSV*0.5)
         CALL SCNSET(KZBUF(1,KL),IBKNT(1,KL),IBLV(1,1,1,KL),
     X        KZV(KAZP),IDSV,JLV,ILO,LPMN,LPMX,KZLO,IS360,IZAD,ICROSS,
     X        NST,IBELOW)
      END IF
      if(debug)write(8,*) 'Trp-irhicrs,nst,icoplane,ic=',
     +     irhicrs,nst,icoplane,ic
      IF(NST.NE.0 .AND. ICOPLANE.NE.4) GO TO 800
C
C     360 SCANS ONLY
C
      IF(IS360)ILSAV=ILO

 30   CONTINUE
C     Top of processing for each XY grid point
C
C            KINT+1 = 1 ==> End of elevation scan (90)
C            KINT+1 = 2 ==> Go to 55
C            KINT+1 = 3 ==> Go to 35
C            KINT+1 = 4 ==> Go to 35
C
c-----debug (ljm)
      if(debug)then
         if((kint+1).eq.1)write(8,*) 'Trp-(kint+1=1)go to 90'
         if((kint+1).eq.2)write(8,*) 'Trp-(kint+1=2)go to 55'
         if((kint+1).eq.3)write(8,*) 'Trp-(kint+1=3)go to 35'
         if((kint+1).eq.4)write(8,*) 'Trp-(kint+1=4)go to 35'
      end if
c-----debug (ljm)
      GO TO (90,55,35,35), KINT+1
 35   CONTINUE
C     
C     INTERPOLATION OF LOCATIONS ABOVE THIS PPI SCAN
C        Note: IABOVE = 1; IC = 0; KINT-1 = 1 always
C     
      if(debug)write(8,*)'Trp-after 35: kint, iabove=',kint,iabove
      IF (IABOVE.EQ.0) THEN
c-----debug (ljm)
         if(debug)then
            if((kint-1).eq.1)
     +           write(8,*) 'Trp-iabove=0, (kint-1=1)go to 80'
            if((kint-1).eq.2)
     +           write(8,*) 'Trp-iabove=0, (kint-1=2)go to 55'
         end if
c-----debug (ljm)
         GOTO (80,55), KINT-1
      END IF

C     Get the indices (IX,IY) or (IY,IZ) associated with the
C     current output horizontal grid point at integer angle KZLV,
C     where KZLV is (scaling factor) * (floating point value).
C     Note: ICART azimuth and indices were packed into 32-bit words 
C           in IPUTCP so it must be unpacked with IGETCP2 on wkstn.
C           64-bit packing and unpacking no longer consistent.
C           No need to include WORDSZ.EQ.64 test until packing is
C           fixed for a 64-bit machine.
C
      IF (IRHICRS.EQ.0 .OR. IRHICRS.EQ.3) THEN
         CALL IGETCP2(ICART(IC),KZLV,IX,IY)
      ELSE
         CALL IGETCP2(ICART2(IC),KZLV,IY,IZ)
      END IF
c-----debug (ljm)
      if(debug)then
         az=kzlv*sclaz
         write(8,*)'Trpvol: ic,icart,kzlv,iy,ix,az=',
     +        ic,icart(ic),kzlv,iy,ix,az
      end if
c-----debug (ljm)
      KZHI=KZLV

c-----debug (ljm)--
c        (xt,yt) are relative to lower-left corner of grid.
c        (xgrid,ygrid) are actual (x,y) grid locations.
c        az is azimuth angle of output grid point (xgrid,ygrid)
c

c      if(ic.eq.1 .or. mod(ic,icmod).eq.0 .or. ic.eq.ictab(2))then
c        xt=(ix-1)*xd
c        yt=(iy-1)*yd
c         ij=ix+(iy-1)*nx
c         xt=xgrid(ij)
c         yt=ygrid(ij)
c         az=kzlv*sclaz
c         write(8,1768)ic,ij,ix,iy,kzlv,xt,yt,az
c 1768    format(1x,'Trpvol:   ic=',i8,9x,'Grid ij,ix,iy,int az = ',
c     +        4i6,2x,'Grid xya=',3f10.3)
c      end if
c-----debug (ljm)
      IF(IS360) THEN
C
C     360 SCANS ONLY
C
         IF(IABS(KZLV-KAZ(KAZC)).GT.ICROSS)
     X        KAZ(KAZC)=KAZ(KAZC)+ISIGN(IZAD,KZLV-KAZ(KAZC))
         IF(IABS(KZLV-KAZ(KAZP)).GT.ICROSS)
     X        KAZ(KAZP)=KAZ(KAZP)+ISIGN(IZAD,KZLV-KAZ(KAZP))
         ITST=(KAZ(KAZP)-KZLV)*IDS
         if(debug)then
            azp=sclaz*kaz(kazp)
            azc=sclaz*kaz(kazc)
            az=sclaz*kzlv
            write(8,*) 
     +           'Trp 360: kint,azp,az,azc,itst=',kint,azp,az,azc,itst
         end if
         IF(ITST.GT.0) THEN
c-----debug-always (go to 80)
c            if((kint-1).eq.1)write(8,*) 'Trp-itst>0, (kint-1=1)go to 80'
c            if((kint-1).eq.2)write(8,*) 'Trp-itst>0, (kint-1=2)go to 55'
c-----debug (ljm)
            GO TO (80,55), KINT-1
         END IF
      END IF

      IF (IRHICRS.EQ.0 .OR. IRHICRS.EQ.3) THEN
         ITST=(KAZ(KAZC)-KZLV)*IDS
      ELSE
         ITST=(KZV(KAZC)-KZLV)*IDSV
c         write(8,*)'***ang1,idsv,ids,itst=',ang1,idsv,ids,itst
      END IF
      IF(ITST.LT.0) THEN
c-----debug-always (go to 80)
c         if((kint-1).eq.1)write(8,*) 'Trp-itst<0, (kint-1=1)go to 80'
c         if((kint-1).eq.2)write(8,*) 'Trp-ists<0, (kint-1=2)go to 55'
c-----debug (ljm)
         GO TO (80,55), KINT-1
      END IF
C     
C     Generate locations along z-axis for every (X,Y) point between 
C     this scan and the next.  For LON/LAT grid, the (X,Y) grid values 
C     (XGRID,YGRID) were stored in routine CRTOUT before CALL SINSRT
C     to sort grid azimuths in increasing order.  The integer azimuths
C     and corresponding (I,J) grid indices are extracted from ICART and
C     used here to find the (XGRID,YGRID) address IJ=IX+(IY-1)*NX.
C     
      IF (IRHICRS.EQ.0 .OR. IRHICRS.EQ.3) THEN

C     longitude-latitude horizontal grid for (r,a,e)-->(l,l,z)
C     cartesian x-y horizontal grid for (r,a,e)-->(x,y,z)
C     cartesian x-y coplane    grid for (r,a,c)-->(x,y,c)
C     
         IJ=IX+(IY-1)*NX
         X=XGRID(IJ)
         Y=YGRID(IJ)
         RGXYSQ= X**2+Y**2
         
         RGXY=SQRT(RGXYSQ)
         DIST=RGXY
         if(debug)write(8,*) 'Trp-iyx,yx,dist=',ij,iy,y,x,dist
      ELSE

C     cartesian x-z vertical grid for (r,a,e)-->(y,z,x)
C
         Y=(IY-1)*YD
         Z=(IZ-1)*ZD
         RGYZSQ=(Y+YORTR)**2. + (Z+ZORTR)**2.
         RGYZ=SQRT(RGYZSQ)
         DIST=RGYZ
         if(debug)write(8,*) 'Trp-iyz,yz,dist=',iy,iz,y,z,dist
      END IF

      IF (ICOPLANE.EQ.3) THEN
C
C     SPECIAL CORRECTION FOR R,A,C --> CARTESIAN
C
         BETA = (KZLV*SCLAZ) - DASANG
         DIST=ABS(RGXY*SIN(BETA*ATR))
      ELSE IF (ICOPLANE.EQ.4) THEN
C
C     SPECIAL CORRECTION FOR RHI   --> CARTESIAN
C
        IF (IRHICRS.EQ.0 .OR. IRHICRS.EQ.3) THEN
            BETA = (KZLV*SCLAZ)
            DIST=RGXY*SIN(BETA*ATR)
         END IF
      END IF
      DISTSQ=DIST*DIST

      IF (ICOPLANE.EQ.0 .OR. ICOPLANE.EQ.3) THEN
C     
C     INTERPOLATING TO 3-D CARTESIAN GRID
C     
         IZ1=2.+ZDI*(DIST*(TANLO+DIST*AKZI)-ZRTAB(1,1))
         IZ2=1.+ZDI*(DIST*(TANHI+DIST*AKZI)-ZRTAB(1,1))
      ELSE IF (ICOPLANE.EQ.4) THEN
C
C     NO CURV. OF EARTH CORR. FOR RHIS CURRENTLY IMPLEMENTED
C
         IF (IRHICRS.EQ.0 .OR. IRHICRS.EQ.3) THEN
            IZ1=2.+ZDI*((ABS(DIST)*TANLO)-ZRTAB(1,1))
            IZ2=1.+ZDI*((ABS(DIST)*TANHI)-ZRTAB(1,1))
         ELSE
            IX1=2.+XDI*(TANLO*(Z+ZORTR) - XRTAB(1,1))
            IX2=1.+XDI*(TANHI*(Z+ZORTR) - XRTAB(1,1))
         END IF

      ELSE IF (ICOPLANE.EQ.1) THEN
C     
C     INTERPOLATING TO COPLANES IN WHICH DATA WAS TAKEN
C     
         ZRTAB(KPEL,1)=COLO
         IZ1=KPEL
         IZ2=KPEL
      ELSE IF (ICOPLANE.EQ.2) THEN
C     
C     INTERPOLATING TO USER-SPECIFIED COPLANES
C     
         IZ1=0
         IZ2=0
         DO 22 I=1,NZ
            IF (ZRTAB(I,1).GE.COLO .AND. ZRTAB(I,1).LT.COHI) THEN
               IZ1=I
               GOTO 23
            END IF
 22      CONTINUE
 23      CONTINUE
         DO 24 I=1,NZ
            IF (ZRTAB(I,1).LT.COHI .AND. ZRTAB(I,1).GE.COLO) IZ2=I
 24      CONTINUE
      END IF

      IF (IRHICRS.EQ.0 .OR. IRHICRS.EQ.3) THEN
c         write(8,*)'TRPVOL - not interpolating across 90 or 270'
C
C     NOT INTERPOLATING ACROSS 90 OR 270
C
         if(debug)write(8,*)' TRPVOL - iz1,iz2=',iz1,iz2
         IF (IZ1.EQ.0 .AND. IZ2.EQ.0) GOTO 38
         IZ1=MAX0(IZ1,1)
         IZ2=MIN0(IZ2,NZ)
         if(debug)write(8,*)' TRPVOL - iz1,iz2=',iz1,iz2
         IF(IZ1.GT.IZ2) GO TO 38
C     
C        VERTICAL GRID LOCATIONS (IZ1 to IZ2) FOUND BETWEEN 
C        THIS SCAN AND NEXT SCAN AT THIS (X,Y) POSITION.
C     
c-----debug (ljm)
         if(debug)then
            write(8,*)'***********************'
            write(8,*)'   Z loop - top: iz1,iz2=',iz1,iz2,
     +           ' z1,z2=',zrtab(iz1,1),zrtab(iz2,1)
         end if
c-----debug (ljm)

C     Interpolation to (x,y) within scan level (kpel)
C
         DO 37 I=IZ1,IZ2
            IF (ICOPLANE.EQ.0 .OR. 
     X          ICOPLANE.EQ.3 .OR. 
     X          ICOPLANE.EQ.4) THEN
               RNGSQ=RGXYSQ+ZRTAB(I,2)
            ELSE
               RNGSQ=RGXYSQ
            END IF
            RNG=SQRT(RNGSQ)
            IF (ICOPLANE.EQ.0 .OR. ICOPLANE.EQ.3) THEN
               ZADJ=ZRTAB(I,1)- DISTSQ*AKZI
            ELSE IF (ICOPLANE.EQ.2) THEN
               ZADJ=ZRTAB(I,1)
            ELSE IF (ICOPLANE.EQ.1) THEN
               ZADJ=ZRTAB(I,1)
            ELSE IF (ICOPLANE.EQ.4) THEN
               ZADJ=ZRTAB(I,1)
            END IF
            IF((ZADJ.GT.RNG) .AND. 
     X         (ICOPLANE.EQ.0 .OR. ICOPLANE.EQ.3)) GO TO 37

C     Find radar elevation angle and index for this vertical grid point
C
            IF (ICOPLANE.EQ.0 .OR. ICOPLANE.EQ.3) THEN
               IF (DIST.NE.0.0)THEN
                  DISTI=1./DIST
                  IEL=(RTA*ATAN(ZADJ*DISTI)-ELB(KPEL))*DEI(KPEL)+0.5
               ELSE
                  IEL=0
               END IF
            ELSE IF (ICOPLANE.EQ.1 .OR. ICOPLANE.EQ.2) THEN 
               IEL=(ZADJ-ELB(KPEL))*DEI(KPEL)+0.5
            ELSE IF (ICOPLANE.EQ.4) THEN
               IF(ZADJ .LT. 0) GO TO 37 
               ELEV=RTA*ATAN2(ZADJ,DIST)
               IF (ELEV.LT.0.0) ELEV=ELEV+360.0
               IF (ELB(KPEL+ISD).GT.360.0 .AND. ELEV.LT.ELB(KPEL)) 
     X              ELEV=ELEV+360.0
               IEL=(ELEV-ELB(KPEL))*DEI(KPEL) + 0.5
            END IF

            IRNG=RNG*UNSCRG+0.5
            RNG=IRNG*SCLRNG
            IG=(RNG-RNOT)/DRG+1.0
            IF (IG.LE.0 .OR. IG.GE.NRGP .OR. IG.GE.NRGC) GOTO 37
            IF (IEL.LT.0) THEN
               WRITE(*,*)'***IEL,ELEV,ZADJ,DIST,ELB(KPEL)=',
     X              IEL,ELEV,ZADJ,DIST,ELB(KPEL)
               WRITE(*,*)'***TANLO,TANHI,ZRTAB(1,1),ZDI=',
     X              TANLO,TANHI,ZRTAB(1,1),ZDI
               STOP
            END IF

            MN=-25
            ANGLINP=ELB(KPEL)
            VNYQINP=VNYQUIST(KPEL)
c-----------debug (ljm)
            if(debug)then
               nmb='#1'
               az=sclaz*kzlv
               eladj=rta*atan(zadj*disti)
               write(8,1769)nmb,i,az,iy,ix,kpel,iel,anglinp,vnyqinp,nst
 1769          format(' Trpvol - call trpd ',a2,': i=',i4,' az=',f8.3,
     +              ' iy,ix,kpel+isd,iel=',4i4,' elev,vnyq=',2f8.3,
     +              ' nst=',i1)
c               ibnd=idim*idim2*nrcbf*2
c               write(8,*)' IBLV=',idim,idim2,nrcbf,2,ibnd
c               jbnd=1*4*ihi*kh
c               write(8,*)' IBLV=',1,4,ihi,kh,jbnd
c               write(8,*)' IBLV=',IBLV(1,4,IHI,KH)
            end if
c-----------debug (ljm)
cqual-------print *,'TRPVOL#1: ihi,kh,4*ihi*kh=',ihi,kh,4*ihi*kh
            CALL TRPD(IDATH,IEL,IRNG,KZLV,IDATL,0,IBLV(1,4,IHI,KH),
     X           NST,MN,NUMFILT,KAZ,MAXFLD,VNYQINP,IPPI,ILLE,ILLZ)
cqual-------print *,'TRPVOL#1: iblv=',iblv(1,4,ihi,kh)
            IF (NST.NE.0) GO TO 800
            if(debug)write(8,*)'Trpvol- Iputai: i,iel,irng,ihi,kh=',
     +              i,iel,irng,ihi,kh
            IF (WORDSZ.EQ.32) THEN 
               CALL IPUTAI(IBLV(1,1,IHI,KH),I,IEL,IRNG)
               IBLV(2,1,IHI,KH)=ICART(IC)
            ELSE IF (WORDSZ.EQ.64) THEN
               CALL IPUTAI(IWRD,I,IEL,IRNG)
               IBLV(1,1,IHI,KH)=ICEDOR(IWRD,ICEDSHFT(ICART(IC),-32))
            END IF
            CALL IPKDAT(IBLV(1,2,IHI,KH),IDATH,NFLI,MAXFLD)
            ILOC=(IHI-1)/(WORDSZ/8) + 1
            INDX=(WORDSZ/8) - (ILOC*(WORDSZ/8) - IHI)
            CALL IPUT8(IVREF(ILOC,KH),INDX,MN)
C            IVREF(IHI,KH)=MN
            IF (ICOPLANE.EQ.1 .AND. KEL.EQ.IEL2) THEN
cqual----------ivv1=iblv(1,4,ihi,kh)
cqual----------write(6,*)"TRPVOL:  iel,irng,maxfld=",iel,irng,maxfld
cqual----------write(6,*)"TRPVOL: ihi,kh,ivv1,ivv2=",ihi,kh,ivv1,ivv2
               CALL COMBIN(IDATL,IDATH,IDATCB,IEL,IRNG,KZLV,
     X              IBLV(1,4,IHI,KH),IVV2,MAXFLD)
c--------------debug (ljm)
               if(debug)then
                  write(8,1771)i,sclaz*kzlv,iy,ix,iel
 1771             format('Comb - i,az,iy,ix,iel= ',i4,f8.3,3i4)
                  write(8,*)'Comb:ihi,kh=',ihi,kh
                  write(8,*)'Comb: idath=',
     +                 (sclif(nn,2)*idath(nn),nn=1,nif)
                  write(8,*)'Comb: idatl=',
     +                 (sclif(nn,2)*idatl(nn),nn=1,nif)
                  write(8,*)'Comb:idatcb=',
     +                 (sclif(nn,2)*idatcb(nn),nn=1,nof)
                  write(8,*)'Comb:idatcb=',
     +                 (sclif(nn,2)*idatcb(nn),nn=1,nfli)
                  if (ix.le.0 .or. iy.le.0 .or. iz.le.0) then
                     write(8,*)'***warning*** (ix,iy,iz)=',ix,iy,iz
                  end if
               else
                  CALL IPKDAT(ICOB(1,1,IX,IY,I),IDATCB,NOF,MAXFLD)
               end if
c--------------debug (ljm)
            END IF
            IHI=IHI+IDS
            IF(IHI.LE.0.OR.IHI.GT.NRCBF) GO TO 707
 37      CONTINUE
 38      CONTINUE
c-----debug (ljm)
         if(debug)then
            write(8,*) '***********************'
            write(8,*) '   Z loop - bottom:'
         end if
c-----debug (ljm)

C        Increment IC
C
         IC=IC+IDS
         IF(IS360) THEN
C           360 SCANS ONLY
            IF(IC.LE.0) IC=ICTAB(2)
            IF(IC.GT.ICTAB(2)) IC=1
            IF(IC.EQ.ICSAV) GO TO 45
         END IF
         IF(IC.GT.0.AND.IC.LE.ICTAB(2)) GO TO 35
      ELSE
c         write(8,*)'TRPVOL - interpolating across 90 or 270'
C
C     INTERPOLATE ACROSS 90 OR 270
C
c         write(8,*)'   X loop - pre: ix1,ix2=',ix1,ix2,
c     +        ' x1,x2=',xrtab(ix1,1),xrtab(ix2,1)
         IF (IX1.EQ.0 .AND. IX2.EQ.0) GOTO 48
         IX1=MAX0(IX1,1)
         IX2=MIN0(IX2,NX)
c         write(8,*)'   X loop - pre: ix1,ix2=',ix1,ix2,
c     +        ' x1,x2=',xrtab(ix1,1),xrtab(ix2,1)
         IF (IX1.GT.IX2) GOTO 48 

c-----debug (ljm)
         if(debug)then
            write(8,*)'***********************'
            write(8,*)'   X loop - top: ix1,ix2=',ix1,ix2,
     +           ' x1,x2=',xrtab(ix1,1),xrtab(ix2,1)
         end if
c-----debug (ljm)

C     Interpolation of RHI scans to (x,y,z): x --> horizontal range
C
         DO 47 I=IX1,IX2
            RNGSQ=RGYZSQ+XRTAB(I,2)
            RNG=SQRT(RNGSQ)
            ELEV=RTA*ATAN2((Z+ZORTR),XRTAB(I,1))
            IF (ELEV.LT.0.0) ELEV=ELEV+360.0

C     Find radar azimuth angle and index for this x (horizontal range) grid point
C
            IF (ELB(KPEL+ISD).GT.360.0 .AND. ELEV.LT.ELB(KPEL)) 
     X           ELEV=ELEV+360.0
            IEL=(ELEV-ELB(KPEL))*DEI(KPEL) + 0.5

            IRNG=RNG*UNSCRG + 0.5
            RNG=IRNG*SCLRNG
            IG=(RNG-RNOT)/DRG + 1.0
            IF (IG.LE.0 .OR. IG.GE.NRGP .OR. IG.GE.NRGC) GOTO 47
            IF (IEL.LT.0) THEN
               WRITE(*,*)'***ELEV,KPEL,ELB(KPEL),DEI(KPEL)=',
     X              ELEV,KPEL,ELB(KPEL),DEI(KPEL)
               WRITE(*,*)'***IEL=',IEL
               STOP
            END IF
            MN=-25
            ANGLINP=ELB(KPEL+ISD)
            VNYQINP=VNYQUIST(KPEL+ISD)
cqual-------print *,'TRPVOL#2: ihi,kh,4*ihi*kh=',ihi,kh,4*ihi*kh
            CALL TRPD(IDATH,IEL,IRNG,KZLV,IDATL,0,IBLV(1,4,IHI,KH),
     X           NST,MN,NUMFILT,KZV,MAXFLD,VNYQINP,IPPI,ILLE,ILLZ)
cqual-------print *,'TRPVOL#2: iblv=',iblv(1,4,ihi,kh)
c-----------debug (ljm)
            if(debug)then
               nmb='#1'
               az=sclaz*kzlv
               write(8,1769)nmb,i,az,iy,ix,kpel+isd,iel,anglinp,
     +              vnyqinp,nst
               write(8,*)'Trpvol:ihi,kh=',ihi,kh
            end if
c-----------debug (ljm)
            IF (NST.NE.0) GO TO 800
            IF (WORDSZ.EQ.32) THEN
               CALL IPUTAI(IBLV(1,1,IHI,KH),I,IEL,IRNG)
               IBLV(2,1,IHI,KH)=ICART2(IC)
            ELSE IF (WORDSZ.EQ.64) THEN
               CALL IPUTAI(IWRD,I,IEL,IRNG)
               IBLV(1,1,IHI,KH)=ICEDOR(IWRD,ICEDSHFT(ICART2(IC),-32))
            END IF
            CALL IPKDAT(IBLV(1,2,IHI,KH),IDATH,NFLI,MAXFLD)
            ILOC=(IHI-1)/(WORDSZ/8) + 1
            INDX=(WORDSZ/8) - (ILOC*(WORDSZ/8) - IHI)
            CALL IPUT8(IVREF(ILOC,KH),INDX,MN)
C            IVREF(IHI,KH)=MN
            IHI=IHI+IDSV
            IF(IHI.LE.0.OR.IHI.GT.NRCBF) GO TO 707
 47      CONTINUE
 48      CONTINUE
c-----debug (ljm)
         if(debug)then
            write(8,*) '***********************'
            write(8,*) '   X loop - bottom:'
         end if
c-----debug (ljm)

C        Increment IC
C
         IC=IC+IDSV
         IF(IC.GT.0.AND.IC.LE.ICTAB2(2)) GO TO 35
      END IF
      
 45   CONTINUE
      KINT=KINT-2
      KZHI=IDS*32767
      IF(KINT.EQ.0) GO TO 90

 55   CONTINUE
C     
C     Interpolation to (x,y) within lower scan level (kpel)
C     Finish interpolation (COMBIN) and write to output buffers.
C     
      IF (IBELOW.EQ.0) THEN
C
C        NO POINTS BELOW SCAN THAT MATCH WITH PREVIOUS UPWARD INTERP.
C
         if(debug)write(8,*)'Trpvol - no points below: go to 73'
         GOTO 73
      END IF

C     Note: For whatever reason, IGETCP (64-bit unpacking) is used for
C           IBLV.  This is not the same as ICART packing (IPUTCP) and
C           unpacking (IGETCP2) of 32-bit words.
C
      IF (IRHICRS.EQ.0 .OR. IRHICRS.EQ.2) THEN
         CALL IGETCP(IBLV(1,1,ILO,KL),KZLV,IX,IY)
      ELSE
         CALL IGETCP(IBLV(1,1,ILO,KL),KZLV,IY,IZ)
      END IF
c-----debug (ljm)
      if(debug)then
         az=kzlv*sclaz
         write(8,*)'Trpvol- below: ilo,kl,iblv,kzlv,iy,ix,az=',
     +        ilo,kl,iblv(1,1,ilo,kl),kzlv,iy,ix,az
      end if
c-----debug (ljm)
      KZLO=KZLV
      
      IF(IS360) THEN

C     360 SCANS ONLY
         IF(IABS(KZLV-KAZ(KAZC)).GT.ICROSS)
     X        KAZ(KAZC)=KAZ(KAZC)+ISIGN(IZAD,KZLV-KAZ(KAZC))
         IF(IABS(KZLV-KAZ(KAZP)).GT.ICROSS)
     X        KAZ(KAZP)=KAZ(KAZP)+ISIGN(IZAD,KZLV-KAZ(KAZP))
         ITST=(KAZ(KAZP)-KZLV)*IDS
         IF(ITST.GT.0) GO TO 80
      END IF
      IF (IRHICRS.EQ.0 .OR. IRHICRS.EQ.2) THEN
         ITST=(KAZ(KAZC)-KZLV)*IDS
      ELSE
         ITST=(KZV(KAZC)-KZLV)*IDSV
      END IF

      IF(ITST.LT.0) GO TO 80
c-----debug (ljm)
      if(debug)then
         az=kzlv*sclaz
         write(8,*)'Trpvol: ilo,kl,iblv,iz,iel,irng=',
     +        ilo,kl,iblv(1,1,ilo,kl),iz,iel,irng
      end if
c-----debug (ljm)
      IF (IRHICRS.EQ.0 .OR. IRHICRS.EQ.2) THEN
         CALL IGETAI(IBLV(1,1,ILO,KL),IZ,IEL,IRNG)
      ELSE
         CALL IGETAI(IBLV(1,1,ILO,KL),IX,IEL,IRNG)
      END IF

      RNG=IRNG*SCLRNG
      IG=(RNG-RNOT)/DRG + 1.0
      IF (IG.LE.0 .OR. IG.GE.NRGP .OR. IG.GE.NRGC) GOTO 67
      CALL IGTDAT(IBLV(1,2,ILO,KL),IDATL,NFLI,MAXFLD)
C      MN=IVREF(ILO,KL)
      ILOC=(ILO-1)/(WORDSZ/8) + 1
      INDX=(WORDSZ/8) - (ILOC*(WORDSZ/8) - ILO)
      CALL IGET8(IVREF(ILOC,KL),INDX,MN)

      ANGLINP=ELB(KPEL)
      VNYQINP=VNYQUIST(KPEL)
c-----debug (ljm)
      if(debug)then
         nmb='#2'
         az=sclaz*kzlv
         eladj=rta*atan(zadj*disti)
         write(8,1769)nmb,i,az,iy,ix,kpel,iel,anglinp,vnyqinp,nst
      end if
c-----debug (ljm)
      IF (IRHICRS.EQ.0 .OR. IRHICRS.EQ.2) THEN
         CALL TRPD(IDATH,IEL,IRNG,KZLV,IDATL,1,IVV2,NST,MN,NUMFILT,KZ,
     X        MAXFLD,VNYQINP,IPPI,ILLE,ILLZ)
      ELSE
         CALL TRPD(IDATH,IEL,IRNG,KZLV,IDATL,1,IVV2,NST,MN,NUMFILT,KZV,
     X        MAXFLD,VNYQINP,IPPI,ILLE,ILLZ)
      END IF
      IF (NST.NE.0) GO TO 800

cqual-ivv1=iblv(1,4,ilo,kl)
cqual-write(6,*)"TRPVOL:  iel,irng,maxfld=",iel,irng,maxfld
cqual-write(6,*)"TRPVOL: ilo,kl,ivv1,ivv2=",ilo,kl,ivv1,ivv2
      CALL COMBIN(IDATH,IDATL,IDATCB,IEL,IRNG,KZLV,IBLV(1,4,ILO,KL),
     X     IVV2,MAXFLD)
c--------------debug (ljm)
      if(debug)then
         write(8,1771)i,sclaz*kzlv,iy,ix,iel
         write(8,*)'Comb:ihi,kh,ilo,kl=',ihi,kh,ilo,kl
         write(8,*)'Comb: idath=',(sclif(nn,2)*idath(nn),nn=1,nif)
         write(8,*)'Comb: idatl=',(sclif(nn,2)*idatl(nn),nn=1,nif)
         write(8,*)'Comb:idatcb=',(sclif(nn,2)*idatcb(nn),nn=1,nfli)
         write(8,*)'Comb:idatcb=',(sclif(nn,2)*idatcb(nn),nn=1,nof)
      end if
c-----debug (ljm)

      IF (IX.LE.0 .OR. IY.LE.0 .OR. IZ.LE.0) THEN
         WRITE(*,*)'***WARNING*** (IX,IY,IZ)=',IX,IY,IZ
         write(*,1771)i,sclaz*kzlv,iy,ix,iel,idath(1),idatcb(1)
c         STOP
      END IF
      IF (ICOPLANE.EQ.4) THEN
C
C     SWITCH LOCATION FOR RHI SCANS BEFORE PACKING
C
         IT=IX
         IX=IZ
         IZ=IY
         IY=IT
      END IF
c-----debug (ljm)
      if (ix.le.0 .or. iy.le.0 .or. iz.le.0) then
         write(*,*)'***warning*** (ix,iy,iz)=',ix,iy,iz
      else
c-----debug (ljm)
         CALL IPKDAT(ICOB(1,1,IX,IY,IZ),IDATCB,NOF,MAXFLD)
      end if
      
 67   IF (IRHICRS.EQ.0 .OR. IRHICRS.EQ.2) THEN
         ILO=ILO+IDS
      ELSE
         ILO=ILO+IDSV
      END IF
C     360 SCANS ONLY
      IF(IS360.AND.ILO.EQ.ILSAV) GO TO 70
      IF(ILO.GE.LPMN.AND.ILO.LE.LPMX) GO TO 55
      IF(IS360) THEN
C     360 SCANS ONLY
C     ONLY ONE RECORD CONTAINING THE DATA- NO READ
         IF(ILO.LT.LPMN) ILO=LPMX
         IF(ILO.GT.LPMX) ILO=LPMN
         IF(ILO.EQ.ILSAV) GO TO 70
         GO TO 55
      END IF
 70   CONTINUE
      KINT=KINT-1
      KZLO=IDS*32767
C     
C     NO MORE LOCATIONS BELOW THE CURRENT SCAN
C     
 73   IF(KINT.EQ.0) GO TO 90
C
C     Read next beam in this sweep and GO TO 30 until end-of-sweep
C
 80   CONTINUE
      KSAV=KAZC
      KAZC=KAZP
      KAZP=KSAV
      NRGP=NRGC
      CALL BEAMIN
c-----debug (ljm)
      if(debug)then
         azp=sclaz*kaz(kazp)
         azc=sclaz*kaz(kazc)
         az=sclaz*kzlv
         write(8,*) 'TRPVOL: after call beamin#3: ',nstbm,azp,az,azc
         do ig=1,3
            raz(ig)=sclaz*kaz(ig)
         end do
         titl='Trp--Next beam:'
         write(8,1767)titl,nstbm,nrdopt,(raz(ig),ig=1,3),azp,azc,az
 1767    format(a15,' nstbm,nrdopt= ',2i8,' raz=',3f8.3,' azpc=',3f8.3)
      end if
c-----debug (ljm)

C     NSTBM - beam read status, set in BEAMIN COMMON /IO/
C             (0) New beam, (1) End-of-sweep, (2) Error
C
      IF(NSTBM.EQ.0) GO TO 30
      IF(NSTBM.EQ.2) GO TO 710
      NRGC=IRAY(8,KAZC)
C     
C     END OF SCAN (LAST BEAM HAS BEEN PROCESSED)
C     
      IF(IS360) THEN
C     360 SCANS ONLY
         IF(IFN360) THEN
            NRDOPT=2
            CALL BEAMIN
c-----------debug (ljm)
            if(debug)then
               azp=sclaz*kaz(kazp)
               azc=sclaz*kaz(kazc)
               az=sclaz*kzlv
               write(8,*) 'TRPVOL: after call beamin#4: ',
     +              nstbm,azp,az,azc
            end if
c-----------debug (ljm)
            IF(NSTBM.EQ.2) GO TO 710
            IFN360=.FALSE.
            IF(IABS(KAZ(KAZP)-KAZ(KAZC)).GT.ICROSS)
     X           KAZ(KAZC)=KAZ(KAZC)+ISIGN(IZAD,KAZ(KAZP)-KAZ(KAZC))
            IF(IABS(KAZ(KAZP)-KAZ(KAZC)).LE.MAXEND) GO TO 30
         END IF
      END IF

 90   CONTINUE
C     
C     END OF ELEVATION SCAN
C     
      IF(KEL.GE.IEL2) THEN
c--------debug (ljm)
         if(debug)then
            write(8,*) '***********************'
            write(8,*) 'Trp end of volume: kel,iel2=',kel,iel2
         end if
c--------debug (ljm)
         GO TO 95
      END IF
      CALL SCNEND(0)
C     
C     WRITE PARTIALLY FILLED INTERPOLATION BUFFER TO LEVEL FILE AND
C     UPDATE ASSOCIATED TABLES.  MORE INPUT SCANS TO BE PROCESSED.
C     
      IF (IRHICRS.EQ.0 .OR. IRHICRS.EQ.3) THEN
         IHI=IHI-IDS
      ELSE
         IHI=IHI-IDSV
      END IF
      IF(IHI.LE.0.OR.IHI.GT.NRCBF) THEN
         if(debug)write(8,*) 'K loop - near bottom: ihi=',ihi
         GO TO 10
      END IF

      IF (IRHICRS.EQ.0 .OR. IRHICRS.EQ.3) THEN
         IBKNT(2+IDS,KH)=IHI
      ELSE
         IBKNT(2+IDSV,KH)=IHI
      END IF
      IBKNT(2,KH)=1
      IBKNT(4,KH)=1
      IF (IRHICRS.EQ.1 .OR. IRHICRS.EQ.2) THEN
         JL=2.+(IDSV*0.5)
      END IF

C     Note: For whatever reason, IGETCP (64-bit unpacking is used for
C           IBLV.  This is not the same as ICART packing (IPUTCP) and
C           unpacking (IGETCP2) of 32-bit words.
C
      CALL IGETCP(IBLV(1,1,IHI,KH),KZBUF(JL,KH),IX,IY)
      K=1+(2-JL)*(NRCBF-1)
      CALL IGETCP(IBLV(1,1,K,KH),KZBUF(3-JL,KH),IX,IY)

c-----debug (ljm)
      if(debug)then
         write(8,*) '***********************'
         write(8,*) 'K loop - bottom: kel,kpel,isd,iel1,iel2=',
     +        kel,kpel,isd,iel1,iel2
         write(8,*) 'K loop - bottom: elv,vny=',elb(kel),vnyquist(kel)
      end if
c-----debug (ljm)

      GO TO 10

C     Bottom of (kel) loop over all scans in the volume
C-----------------------------------------------------------------------

 95   CONTINUE
C     
C     END OF VOLUME SCAN...
C     
      CALL SCNEND(1)
      NST=0
      RETURN

c 701  CALL TPQERX(309,1)
c      GO TO 800
 703  CALL TPQERX(311,1)
      GO TO 800
 704  CALL TPQERX(312,1)
      GO TO 800
 705  CALL TPQERX(313,1)
      GO TO 800
c 706  CALL TPQERX(314,1)
c      GO TO 800
 707  CALL TPQERX(315,0)
      GO TO 800
c 708  CALL TPQERX(316,1)
c      GO TO 800
c 709  CALL TPQERX(317,1)
c      GO TO 800
 710  CALL TPQERX(304,1)
      GO TO 800
c 711  CALL TPQERX(318,0)
c      GO TO 800
c 712  CALL TPQERX(319,1)
c      GO TO 800
 800  CONTINUE
      NST=1
      RETURN
      END
