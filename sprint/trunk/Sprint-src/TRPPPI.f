      SUBROUTINE TRPPPI(ICART,ICTAB,ICOB,NDX,NDY,NDZ,ZRTAB,
     X     IBLV,NST,DASANG,IFLAT,NUMFILT,XRTAB,IVREF,
     X     VNYQUIST,XGRID,YGRID)
C
C     DRIVER FOR INTERPOLATION. THIS SUBROUTINE IS SIMILAR TO
C     TRPVOL AND TRPAIR. IT IS DEDICATED TO INTERPOLATING SCANS
C     TO 2-D SURFACES. FOR EXAMPLE, A CONSTANT ELEVATION SCAN
C     WILL BE INTERPOLATED FIRST TO THE POINTS IN THE CONIC SURFACES.
C     THEN, IT WILL BE PROJECTED INTO A HORIZONTAL GRID. THIS
C     IS ALMOST IDENTICAL TO WHAT IS DONE WHEN DISPLAYING PPIS.
C     A CONSTANT AZIMUTH SCAN (RHI) WILL BE INTERPOLATED INTO
C     THE CONSTANT AZIMUTH SURFACE AND THEN PROJECTED INTO A PLANE.
C     
C     CURRENTLY, THE CODE IS ONLY SETUP TO DEAL WITH CONSTANT 
C     ELEVATION SCANS.
C
C     Coplane and GRIDPPI flags set in CRTSET or other routines.
C
C     ICOPLANE = 0  ==>  R,A,E SCANS, INTERPOLATING TO CART OR ELEV GRID
C     ICOPLANE = 1  ==>  COPLANE SCANS, INTERPOLATING TO ANGLES IN DATA
C     ICOPLANE = 2  ==>  COPLANE SCANS, INTERPOLATING TO USER SPEC. ANGLES
C     ICOPLANE = 3  ==>  COPLANE SCANS, INTERPOLATING TO CART GRID
C                        Set in RPNCAR (FOF RP field format) or UFNCAR (UF)
C                        according to sweep mode (2) coplane or (3) RHI
C     ICOPLANE = 4  ==>  RHI SCANS, INTERPOLATING TO CART GRID
C                        Set in RPNCAR (FOF RP field format) or UFNCAR (UF)
C                        according to sweep mode (2) coplane or (3) RHI
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
C     Note:    Z1,Z2,ZD to be used in building CEDRIC 510 word header are
C              set here ==> Z1=ELB(1); Z2=ELB(IEL2); ZD=ELB(2)-ELB(1) in
C              COMMON /TRANS/.  ELB are transferred to ZRTAB here and 
C              put into 10-word header in OUTPCK via CVAL.
C              In each of the routines that read in data, the elevation angles
C              are scaled by JRH7=100 in DORSWP and NEXSWP, and by ID(44)=100
C              in RPNCAR and UFNCAR.  ID(44)=100 set in INITVOL.
C              HEDSET: IDMUD(170, 171, 173) = (Z1, Z2, ZD)*1000 
C              CARTAP: Calls OUTPCK to write 510-word header stored in ITAPHD.
C                      IDMUD --> IDB in CARTAP --> ITAPHD in OUTPCK
C
C     See BEAMIN:
C        KAZ(KAZC) - Integer array of azimuths = NINT(AZSCALE * Floating pt)
C        KAZC      - Current  ray index in arrays IRAY and KAZ
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
C     See IPUTCP (pack ICART into 32 bit) and IGETCP2 (unpack ICART from 32 bit):
C        KZLV  - Integer scaled azimuth associated with (IX,IY) sorted output 
C                grid location.  KZLV = AZ*UNSCAZ  with UNSCAZ set in INITAL.f
C        IX,IY - Indices for the horizontal output grid point.
C        Note: KAZ(KAZC) and KAZ(KAZP) should be integer scaled azimuths to 
C              either side of current output grid location (azimuth).
C     
C     
C     Note: NPREC and MAXPLN refer to same parameter
C
      INCLUDE 'SPRINT.INC'

c-----PARAMETER (MAXEL=150,NID=129+3*MAXEL)
c-----PARAMETER (NIOB=85000,MAXIN=8500,MAXLEN=MAXIN/4)
c-----PARAMETER (IDIM=64/WORDSZ,MAXWRD=IDIM*WORDSZ/INTSZ)
c-----PARAMETER (IDIM2=4,MAXFLD=IDIM2*MAXWRD)
c-----PARAMETER (MAXPLN=65536,MAXZ=10,MAXYZ=MAXPLN*MAXZ)
c-----PARAMETER (MAXRNG=1024)
c-----PARAMETER (NRCBF=400000,IVDIM=(NRCBF+1)/(WORDSZ/8))
c-----DATA IBAD/-32768/

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

      COMMON /COPE/ IADJAZ

      COMMON /CFLDC/ IFIELD(MAXFLD), INTINF(MAXFLD,3)
      CHARACTER*8 INTINF,IFIELD

      DIMENSION VNYQUIST(MAXEL)

c      common /junk/ azc1,azp1,upels(nrcbf,2)

      LOGICAL ICOPLN
      DATA ATR /0.0174533/
      DATA IDATL/MAXFLD*0/
      DATA IDATH/MAXFLD*0/
      DATA IDATCB/MAXFLD*0/
C      DATA INITCB/ 1000004000020000100000B /
      DATA MASK/ O'100000'/

c-----debugging variables (ljm)
c      CHARACTER*11 TITL
c      DIMENSION RAZ(3)

      DATA ICMOD/50/
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

      ATR=ATAN(1.)/45.
      RTA=1./ATR
      IEL = 0.0

c-----debug (ljm)
c      if(debug)then
         ij=ndx+(ndy-1)*ndx
         write(8,*) 'TRPPPI - begin debugging output'
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
c      end if
c-----debug (ljm)

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
C
C     ISSUE ERROR MESSAGE IF SCAN IS NOT A CONSTANT ELEVATION ONE
C     
      IF (ICOPLANE.NE.0) THEN
         WRITE(*,36)
 36      FORMAT(/,5X,'+++ONLY CONSTANT ELEVATION SCANS CAN BE',
     X        ' INTERPOLATED TO 2-D SURFACES (GRIDPPI)+++')
         STOP
      END IF

C
C     XORTR,YORTR IS LOWER LEFT CORNER OF CARTESIAN SYSTEM REL. TO RADAR
C
      IF (ICOPLANE.EQ.0 .OR. ICOPLANE.EQ.3 .OR. ICOPLANE.EQ.4) THEN
         IF(ILLE.EQ.0)THEN

C           FOR REGULAR 2-D CARTESIAN GRID
C
            XORTR = X1*ACSF - Y1*ASNF - XORG
            YORTR = X1*ASNF + Y1*ACSF - YORG
            ZORTR = Z1 - ZRAD
         ELSE

C           FOR REGULAR 2-D LON/LAT GRID
C
            XORTR = XGRID(1)
            YORTR = YGRID(1)
         END IF
         if(debug)write(8,*) 'Trp: llxy=',xortr,yortr

      ELSE IF (ICOPLANE.EQ.1 .OR. ICOPLANE.EQ.2) THEN
C
C        FOR COPLANE COORD. SYSTEM
C
         ANGRD=(ANGXAX-90.)*ATR
         ASNFKP=SIN(ANGRD)
         ACSFKP=COS(ANGRD)

C
C     FIND RADAR COORDINATES IN ROTATED COORDINATE SYSTEM
C
         XRAD=XORG*ACSFKP - YORG*ASNFKP
         YRAD=XORG*ASNFKP + YORG*ACSFKP

C
C     FIND COORDINATE OF LOWER LEFT HAND PART OF GRID, RELATIVE TO RADAR
C
         XORTR = X1-XRAD
         YORTR = Y1-YRAD
         if(debug)write(8,*) 'Trp: llxy=',xortr,yortr
      END IF

C
C        INITIALIZE OUTPUT TABLES AND HEIGHT BUFFERS
C
      NCNT=0
      NDZ=IEL2
      if(debug)then
         write(8,*) 'Trp: initcb=',ndx,ndy,ndz,idim,idim2,initcb
      end if
      DO 3 K=1,NDZ
         DO 2 J=1,NDY
            DO 1 I=1,NDX
               DO 19 L=1,IDIM
                  ICOB(L,1,I,J,K)=INITCB
                  ICOB(L,2,I,J,K)=INITCB
                  ICOB(L,3,I,J,K)=INITCB
                  ICOB(L,4,I,J,K)=INITCB
 19            CONTINUE
               NCNT=NCNT+1
 1          CONTINUE
 2       CONTINUE
 3    CONTINUE

      MX_ICOB=IDIM*IDIM2*MXCRT*MXCRT*MAXZ
      if(debug)then
         write(8,*) 'Trpppi: icob dim=',idim,idim2,mxcrt,mxcrt,maxz
         write(8,*) 'Trpppi: icob dim=',idim,idim2,ndx,ndy,ndz
         write(8,*) 'Trpppi: icob dim=',mx_icob,ncnt*4*idim
      end if
      IF(NCNT.GT.MX_ICOB)THEN
         PRINT *
         PRINT *,'     ',
     X   '+++ TRPPPI: INITIALIZED NCNT=',NCNT,' VALUES IN ICOB +++'
         PRINT *,'     ',
     X   '+++ MAX ALLOWED XYZ GRID POINTS (',MX_ICOB,') EXCEEDED +++ '
         PRINT *
         STOP
      END IF
      
      Z1=ELB(1)
      Z2=ELB(IEL2)
      NZ=IEL2
      IF(NZ.EQ.1)THEN
         ZD=1.0
      ELSE
         ZD=ELB(2)-ELB(1)
      END IF

C     Check elevation step (ZD) for IPPI=0 (interpolation to XYZ)
C
      IF (ZD.EQ.0.0 .AND. IPPI.EQ.0) THEN
         WRITE(*,*)'*** INVALID ZD IN TRPPPI: Z1,Z2,ZD,NZ=',
     +        Z1,Z2,ZD,NZ
         STOP
      ELSE
         ZD=(Z2-Z1)/(NZ+1)
         WRITE(*,*)'*** VALID ZD IN TRPPPI: Z1,Z2,ZD,NZ=',Z1,Z2,ZD,NZ
      END IF

C     Check elevation step (ZD) for ILLZ=1 (interpolation to LLZ)
C
      IF (ZD.EQ.0.0 .AND. ILLZ.EQ.1) THEN
         WRITE(*,*)'*** INVALID ZD IN TRPPPI: Z1,Z2,ZD,NZ=',
     +        Z1,Z2,ZD,NZ
         STOP
      ELSE
         ZD=(Z2-Z1)/(NZ+1)
         WRITE(*,*)'*** VALID ZD IN TRPPPI: Z1,Z2,ZD,NZ=',Z1,Z2,ZD,NZ
      END IF

      DO 7 I=1,NZ
         ZRTAB(I,1)=ELB(I)
 7    CONTINUE

c-----debug (ljm)
c      write(8,*) "IN TRPPPI ",Z1,Z2,ZD,NZ
c      write(8,*) "IN TRPPPI ",(elb(i),i=1,nz)
c-----debug (ljm)

      ZDI=1./ZD
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
C     NO INTERPOLATIONS BELOW SCANS 
      IBELOW=0
C
C     Top of elevation (z) loop over kel (all elevations in volume scan)
C     Set flags before calling BEAMIN:
C        KAZC=2    ==> Implies store first beam of sweep in 2nd column of IRAY
C        NRDOPT=1  ==> go to next elev sweep within ELB(IEL1)-ELB(IEL2)
C        KEL=KEL+1 ==> Increment sweep counter (Initially KEL=IEL1-1)
C
 10   CONTINUE
      KAZP=1
      KAZC=2
C     360 SCANS ONLY
      IFN360=.TRUE.
      NRDOPT=1
      KEL=KEL+1
c-----debug (ljm)
      if(debug)then
         write(8,*)  '  '
         write(8,*)  '***********************'
         write(8,*)  'Trp top of k loop, kel,vny=',kel,vnyquist(kel)
      end if
c-----debug (ljm)
      IF(KEL.GT.IEL2) GO TO 703
C
C     FOR RHI SCANS, SEE IF FIXED ANGLE HAS CROSSED 90 OR 270;
C     IF SO, SPECIAL PROCESSING WILL HAVE TO BE DONE.
C
      IRHICRS=0

C++C---------------------------------------------------------------------------
C++C  Looks like RHIs --> (y,z,e) rather than PPIs --> (x,y,e)
C++C  This looks like the start of code to interpolate to a cartesian grid
C++C  within the original RHI scans, compared to interpolation to PPI scan.
C++C  It doesn't look like it was fully implemented before Bill Anderson left.
C++C  I assume it was not yet implemented and debugged (remove C++C when done)
C++C---------------------------------------------------------------------------
C++C      IF (ICOPLANE.EQ.4) THEN
C++C         IF (KEL.GT.1) THEN
C++C            ANG1=ELB(KEL-ISD)
C++C            ANG2=ELB(KEL)
C++C            IF (ANG1.GT.360.0) ANG1=ANG1-360.0
C++C            IF (ANG2.GT.360.0) ANG2=ANG2-360.0
C++C            IF ((ANG1.GE.45. .AND. ANG2.GE.45. .AND. ANG1.LT.135. .AND.
C++C     X            ANG2.LT.135) .OR. (ANG1.GE.225 .AND. ANG2.GE.225 .AND.
C++C     X           ANG1.LT.315. .AND. ANG2.LT.315.)) THEN
C++C               IRHICRS=1
C++C            ELSE IF ((ANG1.LT.45. .AND. ANG2.GE.45.) .OR.
C++C     X               (ANG1.LT.225 .AND. ANG2.GE.225.)) THEN
C++C               IRHICRS=2
C++C            ELSE IF ((ANG1.LE.135. .AND. ANG2.GT.135.) .OR.
C++C     X               (ANG1.LE.315. .AND. ANG2.GT.315.)) THEN
C++C               IRHICRS=3
C++C            ELSE
C++C               IRHICRS=0
C++C            END IF
C++C         ELSE
C++C            ANG1=ELB(KEL)
C++C            IF (ANG1.GT.360.0) ANG1=ANG1-360.0
C++C            IF ((ANG1.GE.45 .AND. ANG1.LT.135.) .OR. (ANG1.GE.225 .AND.
C++C     X           ANG1.LT.315.)) THEN
C++C               IRHICRS=1
C++C            ELSE
C++C               IRHICRS=0
C++C            END IF
C++C         END IF
C++C      END IF

C
C     Initialize IRAY with 1st and 2nd beams of the current sweep.
C     Read a beam - (1) IRAY - (1-NG) field values and housekeeping
C                       Store beam in IRAY(I=1,MAXIN,KAZC=2)
C
      CALL BEAMIN
c-----print *,'TRPPPI: right after beamin, nstbm=',nstbm
      IF(NSTBM.NE.0)GO TO 704
      KINT=0
      KSAV=KAZC
      KAZC=KAZP
      KAZP=KSAV
C
C     Read a beam - (1) IRAY - (1-NG) field values and housekeeping
C                       Store beam in IRAY(I=1,MAXIN,KAZC=1)
C
      CALL BEAMIN
      IF(NSTBM.NE.0) GO TO 705
      IDS=IDSNEW
      KSAV=KL
      KL=KH
      KH=KSAV
      NRGP=IRAY(8,KAZP)
      NRGC=IRAY(8,KAZC)
      JL=2.+(IDS*0.5)
      KZLO=IDS*32767
      KZHI=KZLO
      Z1=ELB(1)
      Z2=ELB(IEL2)
      NZ=IEL2
      ZD=ELB(2)-ELB(1)

C     Check elevation step (ZD) for IPPI=0 (interpolation to XYZ)
C
      IF (ZD.EQ.0.0 .AND. IPPI.EQ.0) THEN
         WRITE(*,*)'*** INVALID ZD IN TRPPPI: Z1,Z2,ZD,NZ=',
     +        Z1,Z2,ZD,NZ
         STOP
      ELSE
         ZD=(Z2-Z1)/(NZ+1)
         WRITE(*,*)'*** VALID ZD IN TRPPPI: Z1,Z2,ZD,NZ=',Z1,Z2,ZD,NZ
      END IF

C     Check elevation step (ZD) for ILLZ=1 (interpolation to LLZ)
C
      IF (ZD.EQ.0.0 .AND. ILLZ.EQ.1) THEN
         WRITE(*,*)'*** INVALID ZD IN TRPPPI: Z1,Z2,ZD,NZ=',
     +        Z1,Z2,ZD,NZ
         STOP
      ELSE
         ZD=(Z2-Z1)/(NZ+1)
         WRITE(*,*)'*** VALID ZD IN TRPPPI: Z1,Z2,ZD,NZ=',Z1,Z2,ZD,NZ
      END IF
C     
C     INITIALIZATION FOR LOCATIONS ABOVE THIS PPI SCAN
C        Note: IRHICRS = 0 until RHIs implemented
C              Initially KINT = 0, then KINT = 2 always
C     
      KINT=KINT+2
      COLO=ELB(KPEL)
      COHI=ELB(KPEL+ISD)
      write(8,*) 'Trpppi-irhicrs,nst,icoplane,ic=',
     +     irhicrs,nst,icoplane,ic
      IF (IRHICRS.EQ.0 .OR. IRHICRS.EQ.3) THEN
         CALL RSCART(ICART,MAXPLN,NRCBF,ICTAB,KAZ(KAZP),IDS,JL,IC,IHI,
     X        IBKNT(1,KH),KZHI,IS360,NST,IABOVE)
      ELSE
         JLV=2.+(IDSV*0.5)
         CALL RSCART(ICART2,MAXPLN,NRCBF,ICTAB2,KZV(KAZP),IDSV,JLV,IC,
     X        IHI,IBKNT(1,KH),KZHI,IS360,NST,IABOVE)
      END IF
      write(8,*) 'Trpppi-irhicrs,nst,icoplane,ic=',
     +     irhicrs,nst,icoplane,ic
      IF(NST.NE.0 .AND. ICOPLANE.NE.4) GO TO 800
C
C     360 SCANS ONLY
C
      IF(IS360)ICSAV=IC

 30   CONTINUE
C     Top of processing for each XY grid point
C
C            KINT+1 = 1 ==> End of elevation scan (90)
C            KINT+1 = 2 ==>
C     Always KINT+1 = 3 ==> Go to 35
C            KINT+1 = 4 ==>
C
c-----debug-always (go to 35)
c      if((kint+1).eq.1)write(8,*) 'Trp-(kint+1=1)go to 90'
c      if((kint+1).eq.2)write(8,*) 'Trp-(kint+1=2)go to 55'
c      if((kint+1).eq.3)write(8,*) 'Trp-(kint+1=3)go to 35'
c      if((kint+1).eq.4)write(8,*) 'Trp-(kint+1=4)go to 35'
c-----debug (ljm)
      GO TO (90,55,35,35), KINT+1
 35   CONTINUE
C     
C     INTERPOLATION OF LOCATIONS ABOVE THIS PPI SCAN
C        Note: IABOVE = 1; IC = 0; KINT-1 = 1 always
C              Next test does nothing
C     
      IF (IABOVE.EQ.0) THEN
c-----debug-always (go to 80)
c         if((kint-1).eq.1)write(8,*) 'Trp-iabove=0, (kint-1=1)go to 80'
c         if((kint-1).eq.2)write(8,*) 'Trp-iabove=0, (kint-1=2)go to 55'
c-----debug (ljm)
         GOTO (80,55), KINT-1
      END IF

      IF (IRHICRS.EQ.0 .OR. IRHICRS.EQ.3) THEN
         IF(WORDSZ.EQ.64)THEN
            CALL IGETCP(ICART(IC),KZLV,IX,IY)
         ELSE
            CALL IGETCP2(ICART(IC),KZLV,IX,IY)
         END IF
      ELSE
         IF(WORDSZ.EQ.64)THEN
            CALL IGETCP(ICART2(IC),KZLV,IY,IZ)
         ELSE
            CALL IGETCP2(ICART2(IC),KZLV,IY,IZ)
         END IF
      END IF
      KZH=KZLV

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
c 1768    format(1x,'Trpppi:   ic=',i8,9x,'Grid ij,ix,iy,int az = ',
c     +        4i6,2x,'Grid xya=',3f10.3)
c      end if
c-----debug (ljm)

      IF(IS360) THEN
C
C     360 SCANS ONLY
C
c--------debug (ljm)
c         az=kzlv*sclaz
c         azp=kaz(kazp)*sclaz
c         azc=kaz(kazc)*sclaz
c         flagaz=0.0
c         if( (az.gt. 87.0 .and. az.lt. 93.0) .or.
c     +       (az.gt.267.0 .and. az.lt.273.0))then
c            write(8,*) 'Trppi: azp,az,azc=',azp,az,azc
c            flagaz=1.0
c         end if
c--------debug (ljm)
         IF(IABS(KZLV-KAZ(KAZC)).GT.ICROSS)
     X        KAZ(KAZC)=KAZ(KAZC)+ISIGN(IZAD,KZLV-KAZ(KAZC))
         IF(IABS(KZLV-KAZ(KAZP)).GT.ICROSS)
     X        KAZ(KAZP)=KAZ(KAZP)+ISIGN(IZAD,KZLV-KAZ(KAZP))
c--------debug (ljm)
c         az=kzlv*sclaz
c         azp=kaz(kazp)*sclaz
c         azc=kaz(kazc)*sclaz
c         if(flagaz.eq.1.0)then
c            write(8,*) '       azp,az,azc=',azp,az,azc
c         end if
c--------debug (ljm)
         ITST=(KAZ(KAZP)-KZLV)*IDS
         IF(ITST.GT.0) THEN
c-----debug-always (go to 80)
c            if((kint-1).eq.1)write(8,*) 'Trp-itst>0, (kint-1=1)go to 80'
c            if((kint-1).eq.2)write(8,*) 'Trp-ists>0, (kint-1=2)go to 55'
c-----debug (ljm)
            GO TO (80,55), KINT-1
         END IF
      END IF

      IF (IRHICRS.EQ.0 .OR. IRHICRS.EQ.3) THEN
         ITST=(KAZ(KAZC)-KZLV)*IDS
      ELSE
         ITST=(KZV(KAZC)-KZLV)*IDSV
      END IF
      IF(ITST.LT.0) THEN
c-----debug-always (go to 80)
c         if((kint-1).eq.1)write(8,*) 'Trp-itst<0, (kint-1=1)go to 80'
c         if((kint-1).eq.2)write(8,*) 'Trp-ists<0, (kint-1=2)go to 55'
c-----debug (ljm)
         GO TO (80,55), KINT-1
      END IF
C     
C     Generate locations along z-axis for every (X,Y) point between this
C     scan and the next.  For LON/LAT grid (ILLE=1), the (X,Y) grid values 
C     (XGRID,YGRID) were stored in routine CRTOUT before CALL SINSRT
C     to sort grid azimuths in increasing order.  The integer azimuths
C     and corresponding (I,J) grid indices are extracted from ICART and
C     used here to find the (XGRID,YGRID) address IJ=IX+(IY-1)*NX.
C     
      IF (IRHICRS.EQ.0 .OR. IRHICRS.EQ.3) THEN
         IF(ILLE.EQ.0)THEN

C     cartesian x-y horizontal grid for (r,a,e)-->(x,y,e)
C     cartesian x-y coplane    grid for (r,a,c)-->(x,y,c)
C
            X=(IX-1)*XD
            Y=(IY-1)*YD
            RGXYSQ= FX(X,Y)**2+FY(X,Y)**2
         ELSE

C     longitude-latitude horizontal grid for (r,a,e)-->(l,l,e)
C
            IJ=IX+(IY-1)*NX
            X=XGRID(IJ)
            Y=YGRID(IJ)
            RGXYSQ= X**2+Y**2
         END IF
         RGXY=SQRT(RGXYSQ)
         DIST=RGXY
      ELSE

C     cartesian x-z vertical grid for (r,a,e)-->(y,z,a)
C
         Y=(IY-1)*YD
         Z=(IZ-1)*ZD
         RGYZSQ=(Y+YORTR)**2. + (Z+ZORTR)**2.
         RGYZ=SQRT(RGYZSQ)
         DIST=RGYZ
      END IF
c-----debug (ljm)l
c      write(8,*) 'Trp-xy,dist#1=',x,y,dist,icoplane
c-----debug (ljm)
C
C     GET RANGE IN SURFACE OF FIXED ANGLE
C
      IF (ICOPLANE.EQ.0) DIST=DIST/COS(ELB(KPEL)*ATR)
      IF (ICOPLANE.EQ.3) THEN
C
C        SPECIAL CORRECTION FOR R,A,C --> CARTESIAN
C
         BETA = (KZLV*SCLAZ) - DASANG
         DIST=ABS(RGXY*SIN(BETA*ATR))
      ELSE IF (ICOPLANE.EQ.4) THEN
C
C        SPECIAL CORRECTION FOR RHI   --> CARTESIAN
C
         IF (IRHICRS.EQ.0 .OR. IRHICRS.EQ.3) THEN
            BETA = (KZLV*SCLAZ)
            DIST=RGXY*SIN(BETA*ATR)
         END IF
      END IF
      
      DISTSQ=DIST*DIST

      ZRTAB(KPEL,1)=COLO
      IZ1=KPEL
      IZ2=KPEL
      IF (IRHICRS.EQ.0 .OR. IRHICRS.EQ.3) THEN
C     IF (IZ1.EQ.0 .AND. IZ2.EQ.0) GOTO 55
         IF (IZ1.EQ.0 .AND. IZ2.EQ.0) GOTO 38
         IZ1=MAX0(IZ1,1)
         IZ2=MIN0(IZ2,NZ)
         IF(IZ1.GT.IZ2) GO TO 38
C     
C        GRID LOCATIONS FOUND BETWEEN THIS AND NEXT SCAN AT THIS
C        (X,Y) POSITION.
C     
         DO 37 I=IZ1,IZ2
            RNG=DIST
            IRNG=RNG*UNSCRG+0.5
            RNG=IRNG*SCLRNG
            IG=(RNG-RNOT)/DRG+1.0
            IF (IG.LE.0 .OR. IG.GE.NRGP .OR. IG.GE.NRGC) GOTO 37
            IF (IEL.LT.0) THEN
               WRITE(*,*)'***IEL,ELEV,ZADJ,DIST,ELB(KPEL)=',
     X              IEL,ELEV,ZADJ,DIST,ELB(KPEL)
               STOP
            END IF
            MN=-25

C     Note: Store single 2-d interpolation output in IDATH --> ILWR in call to
C           COMBIN, while IDATL=IBAD --> IUPR.
C
            CALL TRPD(IDATH,IEL,IRNG,KZLV,IDATL,0,IBLV(1,4,IHI,KH),
     X           NST,MN,NUMFILT,KAZ,MAXFLD,VNYQUIST(I),IPPI,ILLE,ILLZ)
c-----debug (ljm)
c            az=kzlv*sclaz
c            write(8,1769)i,nst,iel,az,iy,ix,(idath(nn),nn=1,nof)
c 1769       format(1x,'Trppi: iz,nst,iel=',i2,1x,2i1,' az=',f8.3,
c     +           ' ji=',2i4,' flds=',15i8)
c-----debug (ljm)
            IF (NST.NE.0) GO TO 800
            IEL=0
            DO IF=1,MAXFLD
               IDATL(IF) = IBAD
            END DO

C     Note: COMBIN must be called when doing unfolding of velocities in order to complete
C           the calculations involved with the QUAL field.  If this isn't done, then the
C           interpolated output is messed up in a bad way.  IEL = 0 forces the vertical
C           weighting associated with the upper level to be FRUP=IEL*SCLEL=0 in COMBIN,
C           while FRLO=1-FRUP=1.
C           CALL COMBIN(IDATL,IDATH,IDATCB,IEL,IRNG,KZLV,IBLV(1,4,IHI,KH),IVV2,MAXFLD)
C     SUBROUTINE COMBIN(IUPR, ILWR, IOB,   IEL,IRNG,KZLV,IVV1,            IVV2,MXFLD)
C
c            IF(IPPI.EQ.1 .OR. ILLE.EQ.1)THEN
c               DO NF=1,NOF
c                  IDATCB(NF)=IDATH(NF)
c               END DO
c            ELSE
               CALL COMBIN(IDATL,IDATH,IDATCB,IEL,IRNG,KZLV,
     X              IBLV(1,4,IHI,KH),IVV2,MAXFLD)
c            END IF
            CALL IPKDAT(ICOB(1,1,IX,IY,I),IDATCB,NOF,MAXFLD)
            IHI=IHI+IDS
            IF(IHI.LE.0.OR.IHI.GT.NRCBF) GO TO 707
 37      CONTINUE
 38      CONTINUE
         IC=IC+IDS
         IF(IS360) THEN
C     360 SCANS ONLY
            IF(IC.LE.0) IC=ICTAB(2)
            IF(IC.GT.ICTAB(2)) IC=1
            IF(IC.EQ.ICSAV) GO TO 45
         END IF
         IF(IC.GT.0.AND.IC.LE.ICTAB(2)) GO TO 35

      ELSE

         if(debug)write(8,*)'Trpppi-never gets here'

C++C---------------------------------------------------------------------------
C++C  Looks like RHIs --> (y,z,e) rather than PPIs --> (x,y,e)
C++C  This looks like the start of code to interpolate to a cartesian grid
C++C  within the original RHI scans, compared to interpolation to PPI scan.
C++C  It doesn't look like it was fully implemented before Bill Anderson left.
C++C  I assume it was not yet implemented and debugged (remove C++C when done)
C++C---------------------------------------------------------------------------
C++CC     INTERPOLATE ACROSS 90 OR 270 (*INACTIVE UNTIL THE CODE IS MODIFIED FOR RHIS*)
C++CC
C++C         IF (IX1.EQ.0 .AND. IX2.EQ.0) GOTO 48
C++C         IX1=MAX0(IX1,1)
C++C         IX2=MIN0(IX2,NX)
C++C         IF (IX1.GT.IX2) GOTO 48 
C++C         
C++C         DO 47 I=IX1,IX2
C++C            RNGSQ=RGYZSQ+XRTAB(I,2)
C++C            RNG=SQRT(RNGSQ)
C++C            ELEV=RTA*ATAN2((Z+ZORTR),XRTAB(I,1))
C++C            IF (ELEV.LT.0.0) ELEV=ELEV+360.0
C++C            IF (ELB(KPEL+ISD).GT.360.0 .AND. ELEV.LT.ELB(KPEL)) 
C++C     X           ELEV=ELEV+360.0
C++C            IEL=(ELEV-ELB(KPEL))*DEI(KPEL) + 0.5
C++C            IRNG=RNG*UNSCRG + 0.5
C++C            RNG=IRNG*SCLRNG
C++C            IG=(RNG-RNOT)/DRG + 1.0
C++C            IF (IG.LE.0 .OR. IG.GE.NRGP .OR. IG.GE.NRGC) GOTO 47
C++C            IF (IEL.LT.0) THEN
C++C               WRITE(*,*)'***ELEV,KPEL,ELB(KPEL),DEI(KPEL)=',
C++C     X              ELEV,KPEL,ELB(KPEL),DEI(KPEL)
C++C               WRITE(*,*)'***IEL=',IEL
C++C               STOP
C++C            END IF
C++C            DO IF=1,MAXFLD
C++C               IDATH(IF) = IBAD
C++C               IDATL(IF) = IBAD
C++C               IDATCB(IF)= IBAD
C++C            END DO
C++C            CALL TRPD(IDATH,IEL,IRNG,KZLV,IDATL,0,IBLV(1,4,IHI,KH),
C++C     X           NST,MN,NUMFILT,KZV,MAXFLD,VNYQUIST(I),IPPI,ILLE,
C++C     X           ILLZ)
C++C            IF (NST.NE.0) GO TO 800
C++C            IF (WORDSZ.EQ.32) THEN
C++C               CALL IPUTAI(IBLV(1,1,IHI,KH),I,IEL,IRNG)
C++C               IBLV(2,1,IHI,KH)=ICART2(IC)
C++C            ELSE IF (WORDSZ.EQ.64) THEN
C++C               CALL IPUTAI(IWRD,I,IEL,IRNG)
C++C               IBLV(1,1,IHI,KH)=ICEDOR(IWRD,ICEDSHFT(ICART2(IC),-32))
C++C            END IF
C++C            CALL IPKDAT(IBLV(1,2,IHI,KH),IDATH,NFLI,MAXFLD)
C++C            ILOC=(IHI-1)/(WORDSZ/8) + 1
C++C            INDX=(WORDSZ/8) - (ILOC*(WORDSZ/8) - IHI)
C++C            CALL IPUT8(IVREF(ILOC,KH),INDX,MN)
C++CC            IVREF(IHI,KH)=MN
C++C            IHI=IHI+IDSV
C++C            IF(IHI.LE.0.OR.IHI.GT.NRCBF) GO TO 707
C++C 47      CONTINUE
C++C 48      CONTINUE
C++C         IC=IC+IDSV
C++C         IF(IC.GT.0.AND.IC.LE.ICTAB2(2)) GO TO 35

      END IF

 45   CONTINUE
      KINT=KINT-2
      KZHI=IDS*32767
      IF(KINT.EQ.0) GO TO 90

 55   CONTINUE
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
c-----debug
c      if(kazp.eq.1 .and. kazc.eq.2)then
c         do ig=1,3
c            raz(ig)=sclaz*kaz(ig)
c         end do
c         titl='Trp--Next:'
c         write(8,1767)titl,nstbm,nrdopt,kazp,kazc,(raz(ig),ig=1,3)
c 1767    format(a11,' nstbm,nrdopt,kazp,kazc= ',4i8,' raz=',3f8.3)
c      end if
c-----debug
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
      IF(KEL.GE.IEL2) GO TO 95
      CALL SCNEND(0)
C     
C     WRITE PARTIALLY FILLED INTERPOLATION BUFFER TO LEVEL FILE AND
C     UPDATE ASSOCIATED TABLES.
C     
      IF (IRHICRS.EQ.0 .OR. IRHICRS.EQ.3) THEN
         IHI=IHI-IDS
      ELSE
         IHI=IHI-IDSV
      END IF
      IF(IHI.LE.0.OR.IHI.GT.NRCBF) GO TO 10
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
      IF(WORDSZ.EQ.64)THEN
         CALL IGETCP(IBLV(1,1,IHI,KH),KZBUF(JL,KH),IX,IY)
      ELSE
         CALL IGETCP2(IBLV(1,1,IHI,KH),KZBUF(JL,KH),IX,IY)
      END IF
      K=1+(2-JL)*(NRCBF-1)
      IF(WORDSZ.EQ.64)THEN
         CALL IGETCP(IBLV(1,1,K,KH),KZBUF(3-JL,KH),IX,IY)
      ELSE
         CALL IGETCP2(IBLV(1,1,K,KH),KZBUF(3-JL,KH),IX,IY)
      END IF
C
C     Bottom of elevation (z) loop over kel 
C     (i.e. all elevations in volume scan)
C
c-----debug (ljm)
      if(debug)then
         write(8,*) '***********************'
         write(8,*) 'Trp bottom of k loop: kel=',kel
      end if
c-----debug (ljm)
      GO TO 10

 95   CONTINUE
C     
C     END OF VOLUME SCAN...
C     
      CALL SCNEND(1)
c-----debug (ljm)
      if(debug)then
         write(8,*) '***********************'
         write(8,*) 'Trp end of volume: kel=',kel
      end if
c-----debug (ljm)
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
