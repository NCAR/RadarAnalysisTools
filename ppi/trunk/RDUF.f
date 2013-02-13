c
c----------------------------------------------------------------------X
c
      SUBROUTINE RDUF(IUN,IPREC,FRSTREC,ZSTR,PLTSW,COLRFIL,
     X     VECTS,NFRAME,IFD,NDUMP,NRST,IBSWEP,IESWEP,TANGMX,
     X     ANGINP,IFORBLK,NEWDAY,IVOL,IVOLOLD,NAMUF,MXUF,
     X     ITIMBOV,ITIMEOV,AZVOL,ELVOL,NBMAX,NBVOL,NFXVOL,
     X     IEOV,IEOF,DEC,DECWR,WORDSZ)
C
C  ROUTINE TO READ UNIVERSAL FORMAT RADAR TAPES
C     DEC   - (1) Reading input on DEC,     (0) Reading input on non-DEC
C     DECWR - (1) Input was written on DEC, (0) Input was written on non-DEC
C
C     THIS ROUTINE IS CALLED AT THE BEGINNING OF A SWEEP; FXOLD
C     (FIXED ANGLE) AND ITPOLD (SCAN MODE) ARE INITIALIZED TO -99.
C     LOOPS WITHIN THE ROUTINE UNTIL EITHER END-OF-SWEEP (FIXED ANGLE
C     CHANGES) OR END-OF-TAPE (TWO EOFS IN A ROW) IS REACHED.
C     MEASURED FIELDS ARE UNPACKED AND STORED IN THEIR APPROPRIATE
C     SLOT IN THE ARRAY DAT(GATES,BEAMS,FIELDS).  ARRAYS AZA, ELA AND
C     ITM CONTAIN THE AZIMUTHS, ELEVATIONS AND TIMES OF ALL BEAMS IN
C     THE CURRENT SWEEP.  NEW FIELDS ARE DERIVED FROM INPUT FIELDS
C     BY CALLING THE ROUTINE FUNC AFTER A FULL SWEEP HAS BEEN READ.
C
C  Scanning modes:
C     ITP - (0) Calibration, (1) PPI (sector), (2) Coplane, (3) RHI, 
C           (4) Vertical, (5) Target, (6) Manual, (7) Idle, 
C           (8) Surveillance (360 dg).
C
C     IFTIME - STARTING TIME OF THE CURRENT SWEEP
C       NANG - NUMBER OF ANGLES IN THE SWEEP
C       TANG - CURRENT WIDTH OF THE SWEEP (0.0 .LE. TANG .LE. TANGMX)
C      ASCAN - CURRENT ANGLE IN THE SWEEP, BETWEEN 1ST ANGLE AND 1ST
C              ANGLE + 360 DEG
C
C     NFLD  - Number of input data fields
C     NFLDS - Number of requested fields, including those internally generated
C
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      INCLUDE 'swth.inc'
      PARAMETER (AINMN=350.0/MXA)
      PARAMETER (MXID=250)

      CHARACTER*3 LABLS
      CHARACTER*1 BGFLAG
      LOGICAL FOF,UNI
      LOGICAL COLRFIL,FRSTREC,PLTSW,VECTS

      COMMON /ORIGINCH/NETWORK
      CHARACTER*8 NETWORK
      COMMON/ORIGIN/X0,Y0,H0,AZCOR,BAZ,XRD,YRD
      COMMON /INPUTCH/NAMFLD(MXF),IRATYP,ICORD
      CHARACTER*8 NAMFLD,IRATYP,ICORD
      COMMON/ANGLS/ASCAN(MXA),ANGINC(MXA),FXELC(MXA),FXERR(MXA),AVGI
      COMMON /FILCH/FLSPAC(MXF)
      CHARACTER*8 FLSPAC
      COMMON/FIL/IFILTER(MXF),IGATE(MXF),DXX(MXF),DYY(MXF)

      CHARACTER*8 NAMUF(MXUF)
      CHARACTER*8 CHARHD(50)
      CHARACTER*8 INPFORM
      CHARACTER*8 IBCHAR(450)
      DIMENSION IA(40000),IB(80000),ID(MXID),IDATAHD(50)
      DIMENSION IZ(MXR),FLT(MXR)
      DIMENSION AZVOL(NBMAX),ELVOL(NBMAX)

      DATA IFREC/0/
      DATA NPARMX/10/
      DATA IOLDDAT/999999/
      DATA FXEPS/0.1/

      IF(FRSTREC)FRSTREC=.FALSE.
    5 NAZZ   = 0
      NANG(1)= 0
      FXOLD  = -99.
      ITPOLD = -99
      DROLD  = -99.
      ISWPOLD= -99
      NPAR   = 0
      CANG   = 0.0
      AVGI   = 0.0
      TANG   = 0.0
      NTANG  = 0
      NBAD   = 0
      NGTS   = 0
      VMAX   = -1000.0

   10 CONTINUE
C
C  BUFFER IN THE NEXT PHYSICAL RECORD FROM TAPE.  UNIT(IUN) RETURN THE
C  STATUS OF THE LAST READ AND LENGTH(IUN) RETURNS THE NUMBER OF WORDS READ.
C     RST = -1 (GOOD), 0 (EOF), 1 (PARITY ERROR)
CANNE     BUFFER IN(IUN,1) (IA(1),IA(4000))
C  FORTRAN BLOCKING
C
      IF(IFORBLK.EQ.1)THEN
         CALL RDSUNREC(IA,NWDS,DEC,DECWR)
         IF(NWDS.GT.0)NWDS=INT((NWDS-1)/8)+1
      ELSE

C  COS BLOCKING
C
         CALL RDCOSREC(IA,NWDS)
      END IF
      IPREC=IPREC+1
CANNE     RST=UNIT(IUN)
CANNE     N64=LENGTH(IUN)
      N64=NWDS
      N16IN=4*N64
c      if(iprec.eq.1)write(*,*)'RDUF: n64,n16,ia(1),=',n64,n16in,ia(1)

C     PARITY ERROR:  READ THE NEXT RECORD
C
C      IF(RST .EQ. 1.0)THEN
C         NPAR=NPAR+1
C         WRITE(6,15)IPREC
C   15    FORMAT(8X,' PARITY ERROR IN RECORD = ',I8,/)
C         IF(NPAR.GT.NPARMX)THEN
C            WRITE(6,17)
C   17       FORMAT(1X,'******  MORE THAN',I3,
C     +                ' PARITY ERRORS = EOT  ******')
C            IEOF=0
C            IEOT=1
C            RETURN
C         END IF
C         GO TO 10
C      END IF

C     END OF FILE:  CHECK IF ALSO LOGICAL EOT
C
      IF (NWDS.LE.0.)THEN

C        DOUBLE EOF:  LOGICAL END-OF-TAPE
C
         IF(IEOF.GE.2)THEN
            IEOT=1
            WRITE(6,307)IUN,IPREC
 307        FORMAT(8X,' RDUF: END OF DATA ON UNIT= ',I3,' RECORD=',I8)
            IEOF=0

C           Patch to set Nyquist velocity to biggest velocity read
C           Applicable to NEXRAD UF from Dick Oye
C
c            IF(VMAX.NE.-1000.0)VNYQ=VMAX

            RETURN
         END IF
         IEOF=IEOF+1
         NPAR=0
         GO TO 30
      ELSE

C     GOOD READ:  PROCESS THIS RECORD
C     GET THE RECORD LENGTH AND THEN UNPACK THE BUFFER ARRAY
C     AND CONVERT FROM 2'S COMPLEMENT TO INTERNAL INTEGER.
C
         NPAR=0
         IEOF=0
      END IF

      DO 48 L=1,MXID
         ID(L)=IB(L)
   48 CONTINUE

C     CHECK IF BYTE SWAPPING IS REQUIRED.
C
c      if(iprec.eq.1)write(*,*)'RDUF dec,decwr:=',dec,decwr
      IF((DEC .EQ. 1.0 .AND. DECWR .EQ. 0.0).OR.
     +   (DEC .EQ. 0.0 .AND. DECWR .EQ. 1.0))THEN
         CALL SWAP32(IA,NWDS*2)
c         if(iprec.eq.1)write(*,*)'RDUF (swapped): ia(1)=',ia(1)
      END IF

C     Get length of record in 16-bit words (2nd 16 bits of IA)
C
      CALL GBYTES(IA(1),ITER,16,16,0,1)
c      if(iprec.eq.1)write(*,*)'RDUF: iter,ia(1)=',iter,ia(1)

C     RECORD IS APPARENTLY GOOD, UNPACK ITER 16-BIT WORDS.
C     
      CALL GBYTES(IA(1),IB(1),0,16,0,ITER)
      DO 50 I=1,ITER
         IF(IB(I).GT.32767)IB(I)=IB(I)-65536
 50   CONTINUE

C     If midnight is crossed, set NEWDAY=1 so
C     that 24 hrs will be added to input times.
C
      IF(IB(26).GT.99)IB(26)=IB(26)-100
      IDATE=IB(26)*10000+IB(27)*100+IB(28)
      IF(IDATE.GT.992359)THEN
         JDATE=1000000*(IDATE/1000000)
         IDATE=IDATE-JDATE
      END IF
      IF(IOLDDAT.EQ.999999)IOLDDAT=IDATE
      IF((IDATE-IOLDDAT).EQ.1)THEN
         WRITE(6,1770)IOLDDAT,IDATE,NEWDAY
 1770    FORMAT(1X,'RDUF: IOLDDAT,IDATE,NEWDAY=',3I8)
         NEWDAY=1
      END IF
      IOLDDAT=IDATE

      ITIME=IB(29)*10000+IB(30)*100+IB(31)
      ISEC=3600*IB(29)+60*IB(30)+IB(31)

c      if(iprec.eq.1)then
c         CALL ASDPMD(IB(1),INPFORM,DEC,DECWR,WORDSZ)
c         print 339,ib(2),inpform,ib(1),ib(1)
c 339     format(1x,'UF record length (16-bit words)=',i8,
c     +        '  First word in UF record=',a8,2x,a8,2x,i8)
c         write(*,*)'RDUF: iter=',iter
c      end if

C     OBTAIN SOME BASIC HOUSEKEEPING INFORMATION
C
      IDATH = IB(5)
      NRF   = IB(6)
      IVOL  = IB(7)
      IRY   = IB(8)
      IRC   = IB(9)
      ISWP  = IB(10)
      ALAT  = FLOAT(IB(19)) + IB(20)/60. + IB(21)/(64.*3600.)
      ALON  = FLOAT(IB(22)) + IB(23)/60. + IB(24)/(64.*3600.)
      IYR   = IB(26)
      IMON  = IB(27)
      IDAY  = IB(28)
      IBAD  = IB(45)
      AZ    = IB(33)/64.
      EL    = IB(34)/64.
      AZR   = AZ
      ELR   = EL
      IF(EL.GT.180.0)EL=EL-360.0
      ITP   = IB(35)
      FXANG = IB(36)/64.
      NFH   = IB(IDATH+4)
      RMIN  = FLOAT(IB(NFH+2))+0.001*FLOAT(IB(NFH+3))
      IGSP  = IB(NFH+4)
      NRNG  = IB(NFH+5)
      IRN   = RMIN
      IRX   = RMIN+0.001*(NRNG-1)*IGSP+0.5

C*****PATCHES FOR ELDORA - PLOT AS A SURVEILLANCE WITH +Y BEING UP
C            AZIMUTH IS ROTATION ANGLE AND ELEVATION IS TILT ANGLE.
C
      IF(ICORD.EQ.'AIRBRNE ')THEN
         LUHB  = IB(4)
         PITCH = IB(LUHB+ 2)/64.
         ROLL  = IB(LUHB+ 3)/64.
         PHDG  = IB(LUHB+ 4)/64.
         VNS   = IB(LUHB+ 5)/10.
         VEW   = IB(LUHB+ 6)/10.
         WP3   = IB(LUHB+ 7)/10.
         HGME  = IB(LUHB+ 8)/10.
         UAIR  = IB(LUHB+ 9)/10.
         VAIR  = IB(LUHB+10)/10.
         WAIR  = IB(LUHB+11)/10.
         DRIFT = IB(LUHB+12)/64.
         ROT1  = IB(LUHB+13)/64.
         TILT1 = IB(LUHB+14)/64.
         ROT2  = IB(LUHB+17)/64.
         TILT2 = IB(LUHB+18)/64.
         AZ    = ROT1+ROLL
         EL    = TILT1
         AZR   = AZ
         ELR   = EL
         FXANG = FLOAT(ISWP)
         ITP   = 8
      END IF

C*****Reset Tdwr fixed angle to recorded elevation angle (970429 - sun)
C
      IF(IRATYP.EQ.'TDWR')FXANG = EL

C     WHEN NUMBER OF RANGE GATES CHANGES FROM BEAM-TO-BEAM, SET
C     THE NUMBER OF RANGE GATES TO MAX WITHIN ALL BEAMS, NOT TO
C     EXCEED MXR.
C
      IF(NRNG.GT.NGTS)NGTS=NRNG
      IF(NGTS.GT.MXR)NGTS=MXR

      IF((ITIME+NEWDAY*240000).LT.IBTIME)THEN
         IF(IFD.EQ.1 .AND. MOD(IPREC,NRST).EQ.0)
     +        WRITE(6,771)IDATE,ITIME,AZR,ELR,FXANG,IRN,IRX,IGSP,
     +                    VNYQ,IVOL,NRF,ISWP,N64,IRC,ITP,NAZZ,IPREC
 771     FORMAT(1X,'D=',I6.6,' T=',I6.6,' A=',F5.1,' E=',F5.1,
     +        ' F=',F5.1,' R=',I3,'-',I4,' Gs=',I4,' Ny=',F5.1,
     +        ' Vl=',I4,' Vr=',I5,' Sw=',I4,' Ln=',I4,' Rr=',I1,
     +        ' Md=',I1,' Na=',I4,' Tr=',I5,'s')
         IVOLOLD=IVOL
         ITPOLD=ITP
         call sflush
         GO TO 10
      END IF

C     INTERCHANGE ROLES OF AZ AND EL FOR RHI SCANS
C     OR ROTATE THE DATA IN AZIMUTH
C
      IF(ITP.EQ.3)THEN
         AZTMP=AZ
         AZ=EL
         EL=AZTMP
      ELSE
         AZTR=AZ
         AZ=AZTR+AZROT(ITP)
         IF(AZ.GT.360.0)AZ=AZ-360.0
         IF(AZ.LT.  0.0)AZ=AZ+360.0
      END IF

C     PUT ANGLE IN THE RANGE -180 TO 180 UNLESS THE PLOT GRID IS
C     ENTIRELY SOUTH OF THE RADAR, THEN ANGLE RANGES 0 TO 360.
C
      IF(AZ.GT.180..AND..NOT.IAZC)AZ=AZ-360.

 30   CONTINUE

C     STORE TIME, AZIMUTH, AND ELEVATION FOR VOLUME SCAN
C     (See PLTAE or PLTHZ)
C
      NBVOL=NBVOL+1
      IF(NBVOL.LE.NBMAX)THEN
         IF(NBVOL.EQ.1)THEN
            ITIMBOV=ITIME
            IVOLOLD=IVOL
         ELSE
            ITIMEOV=ITIME
         END IF
         AZVOL(NBVOL)=IB(33)/64.
         ELVOL(NBVOL)=IB(34)/64.
      ELSE
         NBVOL=NBMAX
      END IF

C     Reset volume begin time if NFXVOL=0 or store current time.
C
      IF(NFXVOL.EQ.0)THEN
         ITIMBOV=ITIME
      ELSE
         ITIMEOV=ITIME
      END IF

C     CHECK IF FIRST RAY IN A SWEEP OR IF THE SWEEP HAS CHANGED.
C
      IF(ABS(FXANG-FXOLD).GE.FXEPS .OR.
     +   ITP   .NE. ITPOLD .OR.
     +   TANG  .GE. TANGMX .OR.
     +   ISWP  .NE. ISWPOLD.OR.
     +   NAZZ  .GE. MXA    .OR.
     +   IEOF  .EQ. 1      .OR.
     +   (ISEC-IFSEC).GT.600.OR.
     +   IEOT  .EQ. 1    )THEN

         IF(FXOLD.EQ.-99.)THEN

C           FIRST RAY IN A SWEEP:  INITIALIZE SOME VARIABLES
C
            IF(IFD.EQ.1)THEN
               WRITE(6,772)
C  772          FORMAT(/,1X,'Begin Sweep')
 772           FORMAT(' ')

C              Patch to set Nyquist velocity to biggest velocity read
C              Applicable to NEXRAD UF from Dick Oye
C 
c               IF(VMAX.NE.-1000.0)VNYQ=VMAX

               WRITE(6,773)IDATE,ITIME,AZR,ELR,FXANG,IRN,IRX,IGSP,
     +                     VNYQ,IVOL,NRF,ISWP,N64,IRC,ITP,NAZZ,IPREC
 773           FORMAT(1X,'D=',I6.6,' T=',I6.6,' A=',F5.1,' E=',F5.1,
     +              ' F=',F5.1,' R=',I3,'-',I4,' Gs=',I4,' Ny=',F5.1,
     +              ' Vl=',I4,' Vr=',I5,' Sw=',I4,' Ln=',I4,' Rr=',I1,
     +              ' Md=',I1,' Na=',I4,' Tr=',I5)
               call sflush
            END IF
            IFTIME=ITIME
            IFSEC=ISEC
            FXOLD=FXANG
            ITPOLD=ITP
            ISWPOLD=ISWP
            TANFX=TAN(FXANG*.01745)
         ELSE

C           THE SWEEP HAS CHANGED: FIND THE AVERAGE ANGULAR INCREMENT,
C           AND RETURN TO MAIN.
C
            IF(IFD.EQ.1)THEN

C              Patch to set Nyquist velocity to biggest velocity read
C              Applicable to NEXRAD UF from Dick Oye
C 
c               IF(VMAX.NE.-1000.0)VNYQ=VMAX

               CALL PRLASTUF(ID,MXID,NAZZ,IPREC,N64,VNYQ)
               call sflush
               IF(ABS(FXANG-FXOLD).GE.FXEPS)THEN
                  print *,'Change in fixed angle'
               END IF
               IF(ITP .NE. ITPOLD)THEN
                  print *,'Change in scan mode'
               END IF
               IF(TANG .GE. TANGMX)THEN
                  print *,'Angular width of sweep (',tang,
     +                 ') exceeds maximum allowed (',tangmx,')'
               END IF
               IF(ISWP .NE. ISWPOLD)THEN
                  print *,'Sweep number has changed'
               END IF
               IF(NAZZ .GE. MXA)THEN
                  print *,'Number of angles (',nazz,
     +                 ') exceeds maximum allowed (',mxa,')'
               END IF
               IF(IEOF .EQ. 1)THEN
                  print *,'End of scan volume'
                  IEOV=1
               END IF
               IF((ISEC-IFSEC).GT.720)THEN
                  print *,'Sweep is longer than 720 sec'
               END IF
               IF(IEOT .EQ. 1)THEN
                  print *,'End of data'
                  IEOV=1
               END IF
               WRITE(6,774)
  774          FORMAT(1X,'End Sweep',/)
            END IF
            AZSUM=0.
            DO 777 I=1,NAZZ
               AZSUM=AZSUM+ANGINC(I)
 777        CONTINUE
            IF(NAZZ.GT.1)THEN
               AVGI=AZSUM/FLOAT(NAZZ-1)
               NANG(1)=NAZZ
               CALL MNMX(DROLD)
            ELSE
               AVGI=0.0
               DROLD=0.0
            END IF

            ISW=1
            PLTSW=.FALSE.
            VECTS=.FALSE.
            CALL LABELPR(ZSTR,COLRFIL,PLTSW,VECTS,NFRAME,LABLS,
     +           IGRPLT,BGFLAG)
            IF(IEOF.GT.0)WRITE(6,306)IUN,IPREC
 306        FORMAT(8X,' RDUF: END OF FILE ON UNIT= ',I3,' RECORD=',I8)
            IF(IDTIME.GT.0)THEN
               IBTIME=MAX0(IBTIME,ITIME)+100*IDTIME
               IBHR=IBTIME/10000
               IBMN=(IBTIME-10000*IBHR)/100
               IF(IBMN.GE.60)IBTIME=IBTIME+4000
            END IF

C           Patch to set Nyquist velocity to biggest velocity read
C           Applicable to NEXRAD UF from Dick Oye
C
c            IF(VMAX.NE.-1000.0)VNYQ=VMAX

            RETURN
         END IF
      END IF

C     CHECK IF THIS RAY IS THE DESIRED SCAN TYPE AND WITHIN ANGLE TOLERANCES.
C     NTANG - TOTAL NUMBER OF BEAMS, INCLUDING TRANSITION
C     NBAD  - NUMBER OF BEAMS OUTSIDE ANGLE TOLERANCE = ABS(NOMINAL - ACTUAL)
C     NAZZ  - NUMBER OF GOOD BEAMS
C
      IF(ITP.EQ.5)THEN
         NTANG=NTANG+1
         GO TO 40
      END IF
      IF(ITPFLG(ITP).EQ.0)GO TO 10
      IF(ISWP.LT.IBSWEP.OR.ISWP.GT.IESWEP)GO TO 10
      IF(FXANG.LT.FXMN(ITP).OR.FXANG.GT.FXMX(ITP))GO TO 10
      NTANG=NTANG+1
      IF(ITP.EQ.2)THEN
         ELC=57.2958*ATAN(ABS(SIN(.01745*(AZTR-BAZ)))*TANFX)
         IF(ABS(ELC-EL).GT.ANGTOL(ITP))THEN
            NBAD=NBAD+1
            IF(IFD.EQ.1)WRITE(6,775)IDATE,ITIME,AZR,ELR,FXANG,IRN,
     +           IRX,IGSP,VNYQ,IVOL,NRF,ISWP,N64,IRC,ITP,NAZZ,IPREC
 775        FORMAT(1X,'D=',I6.6,' T=',I6.6,' A=',F5.1,' E=',F5.1,
     +           ' F=',F5.1,' R=',I3,'-',I4,' Gs=',I4,' Ny=',F5.1,
     +           ' Vl=',I4,' Vr=',I5,' Sw=',I4,' Ln=',I4,' Rr=',I1,
     +           ' Md=',I1,' Na=',I4,' Tr=',I5,'b')
            call sflush
            GO TO 10
         END IF
      ELSE
         IF(ABS(FXANG-EL).GT.ANGTOL(ITP))THEN
            NBAD=NBAD+1
            IF(IFD.EQ.1)WRITE(6,775)IDATE,ITIME,AZR,ELR,FXANG,IRN,
     +           IRX,IGSP,VNYQ,IVOL,NRF,ISWP,N64,IRC,ITP,NAZZ,IPREC
            GO TO 10
         END IF
      END IF
 40   NAZZ=NAZZ+1
      IF(NAZZ .GE. MXA-1)GO TO 10
      ITM(NAZZ,1)=ITIME
      AZA(NAZZ,1)=AZ
      ELA(NAZZ,1)=EL
      IF(NAZZ.EQ.1)ASCAN(1)=AZ
      IF(ITP.EQ.2)THEN
         FXELC(NAZZ)=ELC
         FXERR(NAZZ)=ELC-EL
      ELSE IF(ITP.EQ.5)THEN
         FXERR(NAZZ)=0.0
      ELSE
         FXERR(NAZZ)=FXANG-EL
      END IF

C     ASSIGN TOTAL ANGLE SCANNED (ASCAN) FROM BEGINNING AND SUM
C     ANGULAR INCREMENTS TO FIND ITS AVERAGE VALUE (AVGI).
C     SURVEILLANCE SCANS MUST INCREMENT BY AINMN=360/MXA AND ALL
C     OTHER SCANS MUST INCREMENT BY ANGINP.
C
      IF(NAZZ.GT.1)THEN
         AINCR=AZA(NAZZ,1)-AZA(NAZZ-1,1)
         IF(AINCR.LT.-180.0)AINCR=AINCR+360.0
         IF(AINCR.GT. 180.0)AINCR=AINCR-360.0
         IF((ABS(AINCR) .LT. ANGINP) .AND. ITP.NE.5)THEN
            NAZZ=NAZZ-1
            NBAD=NBAD+1
            IF(IFD.EQ.1)WRITE(6,775)IDATE,ITIME,AZR,ELR,FXANG,IRN,
     +           IRX,IGSP,VNYQ,IVOL,NRF,ISWP,N64,IRC,ITP,NAZZ,IPREC
            GO TO 10
         END IF
         IF(ITP.EQ.8 .AND. ABS(AINCR) .LT. AINMN)THEN
            NAZZ=NAZZ-1
            NBAD=NBAD+1
            IF(IFD.EQ.1)WRITE(6,775)IDATE,ITIME,AZR,ELR,FXANG,IRN,
     +           IRX,IGSP,VNYQ,IVOL,NRF,ISWP,N64,IRC,ITP,NAZZ,IPREC
            GO TO 10
         END IF
c        if(aincr.gt.1.0)write(*,*)'   aincr=',aincr,ainmn

         IF(IFD.EQ.1 .AND. MOD(IPREC,NRST).EQ.0)THEN

C           Patch to set Nyquist velocity to biggest velocity read
C           Applicable to NEXRAD UF from Dick Oye
C 
c            IF(VMAX.NE.-1000.0)VNYQ=VMAX

            WRITE(6,773)IDATE,ITIME,AZR,ELR,FXANG,IRN,IRX,IGSP,
     +           VNYQ,IVOL,NRF,ISWP,N64,IRC,ITP,NAZZ,IPREC
         END IF
         IF(ITP.EQ.2)THEN
            FXELC(NAZZ)=ELC
            FXERR(NAZZ)=ELC-EL
         ELSE IF(ITP.EQ.5)THEN
            FXERR(NAZZ)=0.0
         ELSE
            FXERR(NAZZ)=FXANG-EL
         END IF
         ASCAN(NAZZ)=ASCAN(NAZZ-1)+AINCR
         TANG=TANG+AINCR
         IF(AINCR.NE.0.0)CANG=CANG+1.0
         ANGINC(NAZZ)=AINCR
      END IF

C     Fill NAMUF with UF field names in the current beam.
C     If the requested field name (NAMFLD) matches one of 
C     the UF field names, get the data from the UF input.
C     Otherwise, assume that NAMFLD is calculated in FUNC
C     or elsewhere.
C
   52 IDATH=IB(5)
      NREC=IB(IDATH+1)
      NFLD=IB(IDATH+2)
      IF(NFLD.GT.MXUF)THEN
         WRITE(6,53)MXUF
   53    FORMAT(8X,' MORE THAN',I3,' UNIVERSAL FORMAT FIELDS ')
         STOP
      END IF

c     Commented out this "CALL ASDPMD" (ljm-06/26/2012)
c     Don't quite understand, but NAMUF was being swapped again
c
      DO 54 I=1,NFLD
         J=IDATH+1+2*I
c         CALL ASDPMD(IB(J),NAMUF(I),DEC,DECWR,WORDSZ)
c         print *,'RDUF-loop 54: i,j,ib(j),namuf(i),dec,decwr=',
c     +        i,j,ib(j),namuf(i),dec,decwr
         if(namuf(i).eq.'\0\0'.or.namuf(i).eq.'  ')then
            write(namuf(i),531)i
 531        format('B',i1)
         end if
   54 CONTINUE

      DO 500 K=1,NFLDS

c      write(6,1771)k,itpold,xmin(itpold),xmax(itpold),
c     +     ymin(itpold),ymax(itpold)
c 1771 format(2x,'K,Itp=',i2,i2,' XmnXmx=',2f8.2,' YmnYmx=',2f8.2)

C        To allow reuse of a UF field name in other functions,
C        first check if current field name is on the UF input
C           Requested field name = UF field name
C                      NAMFLD(K) = ONE OF NAMUF'S
C
         DO 65 I=1,NFLD
            J=IDATH+1+2*I
C            CALL ASDPMD(IB(J),NAMUF(I),DEC,DECWR,WORDSZ)
            IF(NAMUF(I).EQ.NAMFLD(K))THEN
               IFLD(K)=I
               GO TO 70
            END IF
 65      CONTINUE

C        If NAMFLD(K) is not on UF (IFLD(K).LE.0),
C        then bypass reading from UF input.
C
         IFL=IFLD(K)
         IF(IFL.LE.0)GO TO 500
         
         IF(IB(IDATH).NE.NFLD) THEN
            IFREC=1
            GO TO 10
         END IF
         ICNT=ICNT+1
         IF(ICNT.LT.15) GO TO 10
         
         DO I=1,NFLD
            WRITE(6,67)I,NAMUF(I)
 67         FORMAT(1X,' UF field name: i,namuf(i):',i4,2x,a8)
         ENDDO
         WRITE (6,68)K,NAMFLD(K)
 68      FORMAT(1X,'COULD NOT FIND ',I4,2x,A8,
     +        ' FIELD IN 15 RAYS')
         STOP

C        EXTRACT THE REQUESTED FIELD FROM THE INTEGER ARRAY
C        IFLDHD - ADDRESS OF FIRST WORD IN DATA FIELD HEADER
C        IDAT   - ADDRESS OF FIRST DATA WORD IN DATA FIELD
C        NDATAHD- Number of 16-bit words in field header block
C        I      - INDEX OF THE UF FIELD
C
 70      IFLDHD=IB(J+1)
         IDAT=IB(IFLDHD)
         SCALE=FLOAT(IB(IFLDHD+1))
         R0=IB(IFLDHD+2)+IB(IFLDHD+3)*0.001
         DR=0.001*IB(IFLDHD+4)
         NRNG=IB(IFLDHD+5)
         IF(DR.LE.0.0)THEN
            PRINT *,'RDUF: NAM,K,RO,DR,NRNG=',NAMFLD(K),K,RO,DR,NRNG
            JCNT=JCNT+1
            IF(JCNT.LT.15)THEN
               GO TO 10
            ELSE
               PRINT 77
 77            FORMAT(1X,'GATE SPACING STILL ZERO AFTER 15 RAYS')
               STOP
            END IF
C            STOP
         END IF

C        Put NDATAHD values of data field header block into idatahd.
C
         NDATAHD=IDAT-IFLDHD
         NN=0
         DO N=IFLDHD,IDAT-1
            NN=NN+1
            IDATAHD(NN)=IB(N)
         END DO

C        Patches to correct problems with radars in CCOPE (1981)
C        as well as any UF coming from the ATD translators.
C        Also Patrick Air Force Base WSR-74C with Gsp=0.999
C
         IF(IYR.EQ.81.AND.IRATYP.EQ.'SKW     ')THEN
            DR=1.0
         END IF
         IF(DR .EQ. 0.149)DR=0.150
         IF(DR .EQ. 0.199)DR=0.200
         IF(DR .EQ. 0.999)DR=1.000
            
C     WHEN NUMBER OF RANGE GATES CHANGES FROM BEAM-TO-BEAM, SET
C     THE NUMBER OF RANGE GATES TO MAX WITHIN ALL BEAMS, NOT TO
C     EXCEED MXR.
C
         IF(NRNG.GT.NGTS)NGTS=NRNG
c         print 793,itime,ngts,az,el
 793     format(1x,'time,ngts,az,el= ',i7,1x,i5,2f7.1)
         IF(NGTS.GT.MXR)NGTS=MXR

         IF(DR.NE.DROLD)THEN
            DROLD=DR
            CALL RNGST
         END IF
         IF(NAMUF(I).EQ.'VE      ')VNYQ=IB(IFLDHD+19)/SCALE
         IF(NAMUF(I).EQ.'VF      ')VNYQ=IB(IFLDHD+19)/SCALE
         IF(NAMUF(I).EQ.'VR      ')VNYQ=IB(IFLDHD+19)/SCALE
         IF(NAMUF(I).EQ.'VT      ')VNYQ=IB(IFLDHD+19)/SCALE
         IF(NAMUF(I).EQ.'VX      ')VNYQ=IB(IFLDHD+19)/SCALE
         IF(NAMUF(I).EQ.'VK      ')VNYQ=IB(IFLDHD+19)/SCALE
         IF(NAMUF(I).EQ.'DM      ')RADCON=IB(IFLDHD+19)/SCALE
         W20=IB(IFLDHD+19)/SCALE

         DO 86 N=1,MXR
            IF(N.LE.NRNG)THEN
               J=IDAT+N-1
               
C              SET MISSING DATA TO BDVAL
C
               IF(IB(J).EQ.IBAD)THEN
                  R=BDVAL
               ELSE
                  R=IB(J)/SCALE
               END IF
               DAT(N,NAZZ,K)=R
               IZ(N)=IB(J)
               FLT(N)=R
            ELSE
               IZ(N)=IBAD
               FLT(N)=BDVAL
               DAT(N,NAZZ,K)=BDVAL
            END IF

C           Patch to find biggest velocity read
C           Applicable to NEXRAD UF from Dick Oye
C
c            IF(NAMUF(I)(1:1).EQ.'V'.AND.FLT(N).NE.BDVAL)THEN
c               IF(ABS(FLT(N)).GT.VMAX)VMAX=ABS(FLT(N))
c            END IF

 86      CONTINUE

C        IFLDHD - ADDRESS OF FIRST WORD IN DATA FIELD HEADER
C        IDAT   - ADDRESS OF FIRST DATA WORD IN DATA FIELD
C        K      - INDEX OF THE REQUESTED FIELD [NAMFLD(K)]
C        I      - INDEX OF THE UF FIELD [NAMUF(I)]
C
         IF(IFD.EQ.1.AND.NDUMP.GT.0)THEN
c            do i=1,450
c               CALL ASDPMD(IB(i),INPFORM,DEC,DECWR,WORDSZ)
c               ibchar(i)=inpform
c            end do
            IF(K.EQ.1)THEN
               WRITE(6,780)
 780           FORMAT(/,4X,' First 450 words of UF record')
c               CALL DMPCHAR(IBCHAR,450)
c               WRITE(6,780)
               CALL DMPINTGR(IB,450)
            END IF
            WRITE(6,781)NAMUF(I),NDATAHD,
     +           IDATAHD(1),IDATAHD(6),IDATAHD(5)
 781        FORMAT(/,4X,'UF input field: ',A4,
     +           ' field header block length=',I5, 
     +           ' fdw=',i5,' nmb=',i5,' gs=',i5)
            CALL DMPINTGR(IDATAHD,NDATAHD)
c-----------comment out for time being
            WRITE(6,782)K,NAMFLD(K),I,NAMUF(I),IFLDHD,IDAT,SCALE,W20
 782        FORMAT(/,4X,' K,NAMFLD,I,NAMUF(I),IFLDHD,IDAT,SCALE,W20=',
     +           I6,2X,A8,I6,2X,A8,2I8,2F8.2)
c            CALL DMPINTGR(IZ,NRNG)
            CALL DMPFLOAT(FLT,NRNG)
c-----------comment out for time being
         END IF

 500  CONTINUE

      IF(IFD.EQ.1)NDUMP=NDUMP-1
      GO TO 10
      END
