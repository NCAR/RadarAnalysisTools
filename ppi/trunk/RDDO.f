c
c----------------------------------------------------------------------X
c
      SUBROUTINE RDDO(IUN,IPREC,FRSTREC,ZSTR,PLTSW,COLRFIL,
     X     VECTS,NFRAME,IFD,NDUMP,NRST,IBSWEP,IESWEP,TANGMX,
     X     ANGINP,IRW,NEWDAY,IVOL,IVOLOLD,
     X     ITIMBOV,ITIMEOV,AZVOL,ELVOL,NBMAX,NBVOL,NFXVOL,
     X     IEOF,RADAR_TYPE,ISTAT)
C
C  ROUTINE TO READ DORADE FORMAT RADAR DATA 
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
C     IFTIME - STARTING TIME OF THE CURRENT SWEEP
C       NANG - NUMBER OF ANGLES IN THE SWEEP
C       TANG - CURRENT WIDTH OF THE SWEEP (0.0 .LE. TANG .LE. 360.0)
C      ASCAN - CURRENT ANGLE IN THE SWEEP, BETWEEN 1ST ANGLE AND 1ST
C              ANGLE + 360 DEG
C
C  Scanning modes:
C     ITP - (0) Calibration, (1) PPI (sector), (2) Coplane, (3) RHI, 
C           (4) Vertical, (5) Target, (6) Manual, (7) Idle, 
C           (8) Surveillance (360 dg).
C
C     Note: DORADE structure - VOLD...VOLD        (ISTAT = -2 => EOV)
C                              SWIB => end of a sweep within volumes
C           SWEEP files      - SSWB...VOLD...NULL (ISTAT = -3 => EOS)
C     Note: (MXG_DOR,MXF_DOR) in dim.inc and 
C           (MXGAT,MXFLD) in dorade.h must be the same
C
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      INCLUDE 'swth.inc'

      PARAMETER (AINMN=360.0/MXA)

      COMMON /DOR/ FLDDAT(MXG_DOR,MXF_DOR)
      CHARACTER*8 RADNAM,FLTNUM,FLDNAM(MXF_DOR)

      CHARACTER*4 PROJ_NAME
      CHARACTER*3 LABLS
      CHARACTER*2 WHY
      CHARACTER*1 BGFLAG
      INTEGER RADAR_TYPE
      LOGICAL COLRFIL,FRSTREC,PLTSW,VECTS
      LOGICAL AIRBORNE

      LOGICAL DEBUG
      DATA DEBUG /.TRUE./

      COMMON /ORIGINCH/NETWORK
      CHARACTER*8 NETWORK
      COMMON/ORIGIN/X0,Y0,H0,AZCOR,BAZ,XRD,YRD
      COMMON /INPUTCH/NAMFLD(MXF),IRATYP,ICORD
      CHARACTER*8 NAMFLD,IRATYP,ICORD
      COMMON/ANGLS/ASCAN(MXA),ANGINC(MXA),FXELC(MXA),FXERR(MXA),AVGI
      COMMON /FILCH/FLSPAC(MXF)
      CHARACTER*8 FLSPAC
      COMMON/FIL/IFILTER(MXF),IGATE(MXF),DXX(MXF),DYY(MXF)

      DIMENSION IZ(MXR),FLT(MXR)
      DIMENSION AZVOL(NBMAX),ELVOL(NBMAX)
      DATA FXEPS/0.1/
      DATA IOLDDAT/999999/

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
      ISWAP  = 0
      
      REQTIME = IBTIME
      RADNAM=IRATYP
      IF(RADNAM.EQ.'SPOL')THEN
         print *,RADNAM,' gate spacing will be reset to 150 m'
      ENDIF

c-----print *,'RDDO: before rdbeam'
      CALL FLUSH(6)

 10   CONTINUE
      CALL RDBEAM(IUN, IRW, JSTAT, IVOL, IYR, IMON, IDAY,
     X     IHR, IMIN, ISEC, MSEC, NUMRADS, ITP, NFLD,
     X     NUMFREQ, NUMIPP, NRNG, ISWP, JULDAY, IRYSTAT,
     X     radar_type, REQTIME, RADCON, SCANRATE, ALON, ALAT,
     X     VNYQ, RNGMX, RMIN, GATSPAC, AZ, EL, altmsl, PRESALT, altgnd,
     X     GNDSPDEW, GNDSPDNS, VERVEL, HEADING, ROLL, PITCH, DRIFT,
     X     ROTANG, TILT, UAIR, VAIR, WAIR, HEDCHGRT, PITCHGRT, FLDDAT,
     X     BAD, FXANG, RADNAM, FLDNAM, proj_name, FLTNUM,ISWAP)


c-----------------------------------------------------------------------
c      IDATE=(IYR-2000)*10000+IMON*100+IDAY
c      ITIME=IHR*10000+IMIN*100+ISEC
c      AZR   = AZ
c      ELR   = EL
c      WHY ='??'
c      WRITE(6,771)IDATE,ITIME,AZR,ELR,FXANG,IRN,IRX,IGSP,
c     +              IVOL,NRF,ISWP,N64,IRC,ITP,NAZZ,IPREC,WHY,
c     +              JSTAT
c-----------------------------------------------------------------------
C
C     Patch to fix data and times for Chinese radar
C
c      IMON=05
c      IDAY=18
c      IHR=05
c      IMIN=55
c      ISEC=00
c      MSEC=0
c
c     Patch to fix P3 airborne
c      GATSPAC=150.0
c      ITP=8
c      IRW=0
      IF(RADAR_TYPE .EQ. 0)THEN
         AIRBORNE=.FALSE.
      ELSE
         AIRBORNE=.TRUE.
      END IF
c-----debugging statements (ljm)
c-----if(debug)then
c         print *,'RDDO: radar_type=0 (>0) is ground based (airborne)'
c         print *,'RDDO - after rdbeam, jstat,radar_type,airborne,itp=',
c     +        jstat,radar_type,airborne,itp
c-----endif
      if(debug)then
c         write(*,*)'rddo: iun,jstat=',iun,jstat,
c     x        ' name,type=',radnam,radar_type
         if(airborne)then
            write(*,1770)iyr,imon,iday,ihr,imin,isec,msec,alat,alon,
     X           presalt,heading,drift,roll,pitch,tilt,rotang,
     X           fxang,az,el,jstat,itp
 1770       format(3x,'ymd=',i4,2i2.2,' hms=',3i2.2,'.',i3.3,' ll=',
     X           f8.4,f10.4,' z=',f8.3,' hdrptr=',f7.2,4f6.2,f6.1,
     X           ' fae=',f6.2,2f6.1,2i2)
         else
            write(*,1771)iyr,imon,iday,ihr,imin,isec,msec,fxang,az,el,
     X           jstat,irystat
 1771       format(3x,'ymd=',i4,2i2.2,' hms=',3i2.2,'.',i3.3,' fae=',
     X           f6.2,2f8.2,' jstat,irystat,=',2i3)
         end if
      end if
c-----debugging statements (ljm)

C     JSTAT = 0 ---> AN INTERMEDIATE BEAM WAS READ
C     JSTAT = 1 ---> A NEW SWEEP WAS READ
C     JSTAT = 2 ---> BEGINNING OF A NEW VOLUME 
C     JSTAT = 3 ---> END OF DATA FILE
C     JSTAT = 6 ---> END OF DATA MEDIA
C     JSTAT =-1 ---> END OF SWEEP
C     JSTAT =-2 ---> END OF VOLUME FOR DORADE FILES
C     JSTAT =-3 ---> END OF SWEEP FILE IF DORADE SWEEP FILES ARE BEING READ IN
C     JSTAT = 7 ---> ERROR READING THE DATA MEDIA
C
C     ISTAT = 0 ---> NORMAL OR END SWEEP
C     ISTAT = 1 ---> NEW SWEEP
C     ISTAT = 2 ---> NEW VOLUME
C     ISTAT = 3 ---> EOD
C     ISTAT = 4 ---> I/O ERROR
C
      IF (JSTAT .EQ. 0 .OR. JSTAT .EQ. -1)THEN
         ISTAT=0
      ELSE IF (JSTAT .EQ. 1)THEN
         ISTAT=3
      ELSE IF (JSTAT .EQ. 2)THEN
         ISTAT=1
      ELSE IF (JSTAT .EQ. 3 .OR. JSTAT .EQ. 6 .OR. JSTAT .EQ. -3)THEN
         ISTAT=3
      ELSE
         ISTAT=4
      END IF

c     temporary patch to fix problem with scms data (on sample tape from Oye)
c     rmin = rmin - 1100.

      DO I=1,NFLD
         INDX=INDEX(FLDNAM(I),' ')
         IF (INDX.GT.1) THEN
            DO J=INDX,8
               FLDNAM(I)(J:J)=' '
            END DO
         END IF
c--------if(debug)print *,'RDDO: indx,fldnam=',indx,' ',fldnam(i)
      END DO

c     patch to fix S-Pol gate spacing when 0
c
      IF(RADNAM.EQ.'SPOL')THEN
         if(nazz.le.2)then
c-----------print *,'RDDO - after CALL RDBEAM: nazz,gatspac=',
c     +           nazz,gatspac
            IF(GATSPAC.LT.50.)THEN
               GATSPAC=150.0
c--------------print *,'RDDO - after CALL RDBEAM: reset gatspac=',
c     +              gatspac
            ENDIF
         endif
      ENDIF
      GATSPAC2=GATSPAC/1000.
      RMIN2   =RMIN/1000.

c     patch to fix S-Pol gate spacing when 149m 
c
      IF(RADNAM.EQ.'SPOL')THEN
         IF(GATSPAC.EQ.149.0)THEN
            GATSPAC=150.0
            GATSPAC2=GATSPAC/1000.
            RMIN2   =RMIN/1000.
         ENDIF
      ENDIF

      IPREC=IPREC+1
C
C     CHECK STATUS OF READ (ISTAT)
C     0: NORMAL
C     1: NEW DORADE SWEEP
C     2: NEW DORADE VOLUME
C     3: EOD
C     4: MISC. FATAL ERROR
C
      IF (ISTAT.EQ.2) THEN
         IEOF=1
         WRITE(6,306)IUN,IPREC
 306     FORMAT(8X,' RDDO: END OF FILE ON UNIT= ',I3,' BEAM=',I8)
         
         NPAR=0
         GOTO 30
      ELSE IF (ISTAT.EQ.3 .OR. ISTAT.EQ.4) THEN
         IF(IEOF.NE.1)WRITE(6,306)IUN,IPREC
         IF (IEOF.EQ.1) THEN
            IEOT=1
            WRITE(6,307)IUN,IPREC
 307        FORMAT(8X,' RDDO: END OF DATA ON UNIT= ',I3,' BEAM=',I8)
            IEOF=0
            RETURN
         END IF
         IEOF=1
         NPAR=0
         GOTO 30
      ELSE
         NPAR=0
         IEOF=0
      END IF

      IF(IYR.GT.99)THEN
         IYR=IYR-((IYR/100)*100)
      END IF

C     Store current beam date and time information
C     If midnight is crossed, set NEWDAY=1 so 
C     that 24 hrs will be added to input times.

      IDATE=IYR*10000+IMON*100+IDAY
      IF(IOLDDAT.EQ.999999)IOLDDAT=IDATE
      IF((IDATE-IOLDDAT).EQ.1)THEN
         NEWDAY=1
         WRITE(6,1772)IOLDDAT,IDATE,NEWDAY
 1772    FORMAT(1X,'RDNX: IOLDDAT,IDATE,NEWDAY=',3I8)
         IHR=IHR+24
      END IF
      IOLDDAT=IDATE
      ITIME=IHR*10000+IMIN*100+ISEC

C     OBTAIN SOME BASIC HOUSEKEEPING INFORMATION
C
      NRF   = 1
      IRY   = 1
      IRC   = 1
      AZR   = AZ
      ELR   = EL
      IF(EL.GT.180.0)EL=EL-360.0
c      FXANG = TILT
      NFH   = NFLD
      IGSP  = NINT(GATSPAC2*1000.)
      IRN   = NINT(RMIN2)
      IRX   = RMIN2+(NRNG-1)*GATSPAC2

C*****Patches for Eldora - Plot as a surveillance with +y being up
c     ROLL    - Zero is horizontal, Left wing up is positive
c     PITCH   - Zero is horizontal, Nose up is positive
c     DRIFT   - Zero drift is equal to heading, positive drift
c               is arcraft's motionn vector is more CW than heading
c     HEADING - Zero heading is true North, positive is CW
c     ROTANG  - Angle between beam and vertical axis of A/C
c               Zero is along vertical stabilizer, positive is CW
c               looking forward
c     TILT    - Angle between radar beam (when it is in the plane
c               containing the longitudinal axis of the A/C) and
c               a line perpendicular to the longitudinal axis.
c               Zero is perpendicular to the longitudinal axis,
c               Positive is towards nose of the A/C (Forward)
c     The rotation angle (ROTANG) is analagous to elevation angle.
c     The tilt angle is analagous to a relative azimuth
c
c     Make the Eldora scan analagous to a SURVEILLANCE scan with ITP=8,
c     where the TILT angle is like the elevation angle and the rotation
c     angle is like an azimuth.
c     
c                    
      IF(ICORD .EQ. 'AIRBRNE')THEN
c         AZ    = ROTANG+ROLL
         AZ    = ROTANG
         EL    = TILT
         AZR   = AZ
         ELR   = EL
         ITP   = 8
c--------print *,'AIR: itp,fx,az,el=',itp,fxang,az,el
      END IF

C*****Treat SPOL near-vertical scans as surveillance (970423 - vivek)
C
      IF(ITP.EQ.4)THEN
         ITP=8
         EL=0.0
      END IF   

      IF(ITIME.LT.IBTIME)THEN
         WHY='bt'
         IF(IFD.EQ.1 .AND. MOD(IPREC,NRST).EQ.0)
     +   WRITE(6,771)IDATE,ITIME,AZR,ELR,FXANG,IRN,IRX,IGSP,
     +              IVOL,NRF,ISWP,N64,IRC,ITP,NAZZ,IPREC,WHY,
     +              JSTAT
  771    FORMAT(1X,'D=',I6.6,' T=',I6.6,' Az=',F5.1,' El=',F5.1,
     +          ' Fx=',F5.1,' Rg=',I3,'-',I4,' Gs=',I4,' Vl=',I3,
     +          ' Vr=',I5,' Sw=',I4,' Ln=',I4,' Rr=',I1,' Md=',I1,
     +          ' Na=',I4,' Tr=',I5,A2,I3)
         IVOLOLD=IVOL
         ITPOLD=ITP
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
         AZVOL(NBVOL)=AZR
         ELVOL(NBVOL)=ELR
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
c      print *
c      print *,'Tests for first ray and/or sweep change'
c      print *,'FXANG-FXOLD: fxang,fxold,fxeps=',fxang,fxold,fxeps
c      print *,'ITP,ITPOLD: itp,itpold=',itp,itpold
c      print *,'TANG,TANGMX: tang,tangmx=',tang,tangmx
c      print *,'ISWP,ISWPOLD: iswp,iswpold=',iswp,iswpold
c      print *,'NAZZ,MXA: nazz,mxa=',nazz,mxa
c      print *,'ISTAT & IEOF: istat,ieof=',istat,ieof
      IF(ABS(FXANG-FXOLD).GE.FXEPS .OR.
     +   ITP   .NE. ITPOLD .OR.
     +   TANG  .GE. TANGMX .OR.
     +   ISWP  .NE. ISWPOLD.OR.
     +   NAZZ  .GE. MXA    .OR.
     +   ISTAT .EQ. 1      .OR.
     +   IEOF  .EQ. 1          )THEN

         print *,'Passed first ray or sweep change test'

         IF(FXOLD.EQ.-99.)THEN

C           FIRST RAY IN A SWEEP:  INITIALIZE SOME VARIABLES
C
c-----------print *,'Passed first ray in a sweep test'
            IF(IFD.EQ.1)THEN
               WRITE(6,772)
  772          FORMAT(/,1X,'Begin Sweep')
               WRITE(6,773)IDATE,ITIME,AZR,ELR,FXANG,IRN,IRX,IGSP,
     +                    IVOL,NRF,ISWP,N64,IRC,ITP,NAZZ,IPREC,JSTAT,
     +                    istat
  773          FORMAT(1X,'D=',I6.6,' T=',I6.6,' Az=',F5.1,' El=',F5.1,
     +          ' Fx=',F5.1,' Rg=',I3,'-',I4,' Gs=',I4,' Vl=',I3,
     +          ' Vr=',I5,' Sw=',I4,' Ln=',I4,' Rr=',I1,' Md=',I1,
     +          ' Na=',I4,' Tr=',I5,I3,i2)
            END IF
            IFTIME=ITIME
            FXOLD=FXANG
            ITPOLD=ITP
            ISWPOLD=ISWP
            TANFX=TAN(FXANG*.01745)
         ELSE

C           THE SWEEP HAS CHANGED: FIND THE AVERAGE ANGULAR INCREMENT,
C           AND RETURN TO MAIN.
C
c-----------print *,'Passed sweep has changed test'

            IF(IFD.EQ.1)THEN
C               CALL PRLASTUF(ID,NAZZ,IPREC,N64,VNYQ)
               WRITE(6,774)
  774          FORMAT(1X,'End Sweep',/)
            END IF
            IF(NAZZ.NE.0.0)AVGI=(ASCAN(NAZZ)-ASCAN(1))/NAZZ
            NANG(1)=NAZZ
            CALL MNMX(DROLD)
            ISW=1
            PLTSW=.FALSE.
            VECTS=.FALSE.
            CALL LABELPR(ZSTR,COLRFIL,PLTSW,VECTS,NFRAME,LABLS,IGRPLT,
     +         BGFLAG)
            IF(IDTIME.GT.0)THEN
               IBTIME=MAX0(IBTIME,ITIME)+100*IDTIME
               IBHR=IBTIME/10000
               IBMN=(IBTIME-10000*IBHR)/100
               IF(IBMN.GE.60)IBTIME=IBTIME+4000
            END IF
c-----------print *,'RDDO - after call labelpr'
            RETURN
         END IF
      END IF
c-----print *,'Finished Tests for first ray and/or sweep change'

C     CHECK IF THIS RAY IS THE DESIRED SCAN TYPE AND WITHIN ANGLE TOLERANCES.
C     NTANG - TOTAL NUMBER OF BEAMS, INCLUDING TRANSITION
C     NBAD  - NUMBER OF BEAMS OUTSIDE ANGLE TOLERANCE = ABS(NOMINAL - ACTUAL)
C     NAZZ  - NUMBER OF GOOD BEAMS
C
      IF(ITPFLG(ITP).EQ.0)THEN
c--------print *,'RDDO: itp,itpflg=',itp,itpflg(itp)
         GO TO 10
      END IF
C      IF(ISWP.LT.IBSWEP.OR.ISWP.GT.IESWEP)GO TO 10
      IF(FXANG.LT.FXMN(ITP).OR.FXANG.GT.FXMX(ITP))THEN
c--------print *,'RDDO: fxmn,fxang,fxmx=',fxmn(itp),fxang,fxmx(itp)
         GO TO 10
      END IF
      NTANG=NTANG+1
      IF(ITP.EQ.2)THEN
         ELC=57.2958*ATAN(ABS(SIN(.01745*(AZTR-BAZ)))*TANFX)
         IF(ABS(ELC-EL).GT.ANGTOL(ITP))THEN
            NBAD=NBAD+1
            WHY='at'
c-----------if(debug)print *,'RDDO - scan mode'
            IF(IFD.EQ.1)WRITE(6,775)IDATE,ITIME,AZR,ELR,FXANG,IRN,
     +           IRX,IGSP,IVOL,NRF,ISWP,N64,IRC,ITP,NAZZ,IPREC,WHY,
     +           JSTAT
  775       FORMAT(1X,'D=',I6.6,' T=',I6.6,' Az=',F5.1,' El=',F5.1,
     +          ' Fx=',F5.1,' Rg=',I3,'-',I4,' Gs=',I4,' Vl=',I3,
     +          ' Vr=',I5,' Sw=',I4,' Ln=',I4,' Rr=',I1,' Md=',I1,
     +          ' Na=',I4,' Tr=',I5,A2,I3)
            GO TO 10
         END IF
      ELSE
         IF(ABS(FXANG-EL).GT.ANGTOL(ITP))THEN
            NBAD=NBAD+1
            WHY='at'
c-----------if(debug)print *,'RDDO - angtol: ',abs(fxang-el),angtol(itp)
            IF(IFD.EQ.1)WRITE(6,775)IDATE,ITIME,AZR,ELR,FXANG,IRN,
     +           IRX,IGSP,IVOL,NRF,ISWP,N64,IRC,ITP,NAZZ,IPREC,WHY,
     +           JSTAT
            GO TO 10
         END IF
      END IF
      NAZZ=NAZZ+1
      IF(NAZZ .GE. MXA-1)THEN
         print *,'RDDO - nazz .ge. mxa-1'
         GO TO 10
      ENDIF
      ITM(NAZZ,1)=ITIME
      AZA(NAZZ,1)=AZ
      ELA(NAZZ,1)=EL
      IF(NAZZ.EQ.1)ASCAN(1)=AZ
      IF(ITP.EQ.2)THEN
         FXELC(NAZZ)=ELC
         FXERR(NAZZ)=ELC-EL
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
         IF(ABS(AINCR) .LT. ANGINP)THEN
            NAZZ=NAZZ-1
            NBAD=NBAD+1
            WHY='ai'
c-----------if(debug)print *,'RDDO - itp,aincr: ',itp,abs(aincr),anginp
            IF(IFD.EQ.1)WRITE(6,775)IDATE,ITIME,AZR,ELR,FXANG,IRN,
     +           IRX,IGSP,IVOL,NRF,ISWP,N64,IRC,ITP,NAZZ,IPREC,WHY,
     +           JSTAT
            GO TO 10
         END IF
         IF(ITP.EQ.8 .AND. ABS(AINCR) .LT. AINMN)THEN
            NAZZ=NAZZ-1
            NBAD=NBAD+1
            WHY='ai'
c-----------if(debug)print *,'RDDO - itp,aincr: ',itp,abs(aincr),ainmn
            IF(IFD.EQ.1)WRITE(6,775)IDATE,ITIME,AZR,ELR,FXANG,IRN,
     +           IRX,IGSP,IVOL,NRF,ISWP,N64,IRC,ITP,NAZZ,IPREC,WHY,
     +           JSTAT
            GO TO 10
         END IF
c--------if(debug)then
c            print *,'RDDO - itp,aincr,del=',itp,abs(aincr),
c     +           abs(fxang-el)
c--------end if
         IF(ITP.EQ.2)THEN
            FXELC(NAZZ)=ELC
            FXERR(NAZZ)=ELC-EL
         ELSE
            FXERR(NAZZ)=FXANG-EL
         END IF
         ASCAN(NAZZ)=ASCAN(NAZZ-1)+AINCR
         TANG=TANG+AINCR
         IF(AINCR.NE.0.0)CANG=CANG+1.0
         ANGINC(NAZZ)=AINCR
      END IF

      IF(IFD.EQ.1 .AND. MOD(IPREC,NRST).EQ.0)THEN
         WRITE(6,773)IDATE,ITIME,AZR,ELR,FXANG,IRN,IRX,IGSP,
     +        IVOL,NRF,ISWP,N64,IRC,ITP,NAZZ,IPREC,JSTAT,
     +        istat
      END IF
      IF(NFLD.GT.MXF_DOR)THEN
         WRITE(6,53)MXF_DOR
   53    FORMAT(8X,' MORE THAN',I3,' DORADE FORMAT FIELDS ')
         STOP
      END IF
      IDATE=IYR*10000 + IMON*100 + IDAY
   41 CONTINUE
c-----if(debug)print *,'RDDO: extract fields,ifld=',ifld
      DO 500 K=1,NFLDS
         IFL=IFLD(K)
         IF(IFL.LE.0)GO TO 500
         DO 65 I=1,NFLD
c-----------if(debug)print *,'RDDO: namfld,fldnam=',namfld(k),fldnam(i)
            IF(FLDNAM(I).EQ.NAMFLD(K))GO TO 70
   65    CONTINUE
         ICNT=ICNT+1
         IF(ICNT.LT.15) GO TO 10

         PRINT 66
   66    FORMAT(1X,'COULD NOT FIND REQUESTED FIELD IN 15 RAYS')
         STOP

C        EXTRACT THE REQUESTED FIELD FROM THE INTEGER ARRAY
C        I      - INDEX OF THE UF FIELD
C
   70    CONTINUE
         R0=RMIN2

C     Truncate the gate spacing to 10s of meters.  Notoriously,
C     ATD generated files have had 0.149 instead of 0.150 km.
C
         DR=INT(1000.0*GATSPAC2+1.01)/1000.0
c        print *,'RDDO: nrng,ngts,dr,drold=',nrng,ngts,dr,drold
         if(dr.lt.drold)then
c            print *,RADNAM,' gate spacing will be reset to 150 m'
            dr=drold
         endif

C     WHEN NUMBER OF RANGE GATES CHANGES FROM BEAM-TO-BEAM, SET
C     THE NUMBER OF RANGE GATES TO MAX WITHIN ALL BEAMS, NOT TO
C     EXCEED MXR.
C
         IF(NRNG.GT.NGTS)NGTS=NRNG
         IF(NGTS.GT.MXR)NGTS=MXR
         NGTSOLD=NGTS

         IF(DR.NE.DROLD)THEN
            DROLD=DR
            CALL RNGST
         END IF

         DO 86 N=1,MXR
            IF(N.LE.NRNG)THEN
               IF(FLDDAT(N,I).EQ.BAD)THEN
                  R=BDVAL
               ELSE
                  R=FLDDAT(N,I)
               END IF
               DAT(N,NAZZ,K)=R
               FLT(N)=R
               IZ(N)=NINT(R)
            ELSE
               IZ(N)=INT(BAD)
               FLT(N)=BDVAL
               DAT(N,NAZZ,K)=BDVAL
            END IF
   86    CONTINUE

C        K      - INDEX OF THE REQUESTED FIELD [NAMFLD(K)]
C        I      - INDEX OF THE DORADE FIELD [FLDNAM(I)]
C
         IF(IFD.EQ.1.AND.NDUMP.GT.0)THEN
            WRITE(6,776)K,NAMFLD(K),I,FLDNAM(I),MXR,NRNG,NGTS
  776       FORMAT(/,4X,' K,NAMFLD(K),I,FLDNAM(I)=',I3,2X,A8,I3,2X,A8,
     +           ' Mxr,Nrng,Ngts=',3I8)
            CALL DMPINTGR(IZ,NGTS)
         END IF
         IF(IFD.EQ.1.AND.NDUMP.GT.0)THEN
            WRITE(6,776)K,NAMFLD(K),I,FLDNAM(I),MXR,NRNG,NGTS
            CALL DMPFLOAT(FLT,NGTS)
         END IF

  500 CONTINUE
      IF(IFD.EQ.1)NDUMP=NDUMP-1
      GO TO 10
      END
