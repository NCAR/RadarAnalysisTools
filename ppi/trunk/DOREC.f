c
c----------------------------------------------------------------------X
c
      SUBROUTINE DOREC(IUN,IPREC,IEOF,IEOT,FLDDOR,MXUF,NFLD,IRW,
     X     RADAR_TYPE,ISTAT)
C
C  READ A DORADE FORMAT BEAM TO INITIALIZE FIELD NAMES
C     Note: DORADE structure - VOLD...VOLD        (ISTAT = -2 => EOV)
C                              SWIB => end of a sweep within volumes
C           SWEEP files      - SSWB...VOLD...NULL (ISTAT = -3 => EOS)
C     Note: (MXG_DOR,MXF_DOR) in dim.inc and 
C           (MXGAT,MXFLD) in dorade.h must be the same
C
      INCLUDE 'dim.inc'

      COMMON /DOR/ FLDDAT(MXG_DOR,MXF_DOR)

      CHARACTER*8 FLDDOR(MXUF)
      CHARACTER*8 RADNAM,FLDNAM(MXF_DOR),FLTNUM
      COMMON /INPUTCH/NAMFLD(MXF),IRATYP,ICORD
      CHARACTER*8 NAMFLD,IRATYP,ICORD
      CHARACTER*4 PROJ_NAME
      INTEGER RADAR_TYPE
      DATA NPARMX,NZERMX,NSMLMX/10,10,10/
      LOGICAL AIRBORNE

      LOGICAL DEBUG
      DATA DEBUG /.FALSE./

      NPAR=0
      IEOF=0
      IEOT=0
      NZER=0
      NSML=0
      RADNAM = IRATYP
      ISWAP = 0

      print *,'DOREC - before rdbeam: mxg,mxf_dor,mxuf,mxf=',
     +     mxg_dor,mxf_dor,mxuf,mxf
 1    CONTINUE
      CALL RDBEAM(IUN, IRW, JSTAT, IVOL, IYR, IMON, IDAY,
     X     IHR, IMIN, ISEC, MSEC, NUMRADS, ITP, NFLD,
     X     NUMFREQ, NUMIPP, NRNG, ISWP, JULDAY, IRYSTAT,
     X     radar_type, REQTIME, RADCON, SCANRATE, ALON, ALAT,
     X     VNYQ, RNGMX, RMIN, GATSPAC, AZ, EL, altmsl, PRESALT, altgnd,
     X     GNDSPDEW, GNDSPDNS, VERVEL, HEADING, ROLL, PITCH, DRIFT,
     X     ROTANG, TILT, UAIR, VAIR, WAIR, HEDCHGRT, PITCHGRT, FLDDAT,
     X     BAD, FXANG, RADNAM, FLDNAM, proj_name, FLTNUM,ISWAP)

c-----debugging statements (ljm)
c      if(debug)then
         print *,
     +     'DOREC - type=0 (>0) ground (airborne),radar_type=',
     +      radar_type
         print *,
     +     'DOREC - after rdbeam, jstat,itp,nrng,gatspac,rmin,rngmax=',
     +      jstat,itp,nrng,gatspac,rmin,rngmax
c      endif
      IF(RADAR_TYPE .EQ. 0)THEN

c     Patch to fix RadxConvert MSG1 --> Dorade (GATSPAC=250.0
c     Patch to fix RadxConvert SMARTR --> Dorade (GATSPAC=100.0)
c                                            and (NRNG=1499). 
c     Patch to fix RadxConvert SPOLKa --> Dorade (GATSPAC=150.0)
c                                            and (NRNG=979).
         IRW=0
         ITP=8
         NRNG=979
         RMIN=75.0
         GATSPAC=150.0
         RNGMX=RMIN+(NRNG-1)*GATSPAC
         AIRBORNE=.FALSE.
      ELSE
         IRW=0
         ITP=8
         GATSPAC=150.0
         AIRBORNE=.TRUE.
      END IF
c-----debugging statements (ljm)
c      if(debug)then
         write(*,*)'DOREC: iun,jstat=',iun,jstat,
     x        ' name,type=',radnam,radar_type
         if(airborne)then
            write(*,1770)iyr,imon,iday,ihr,imin,isec,msec,alat,alon,
     X           presalt,heading,drift,roll,pitch,tilt,rotang,
     X           fxang,az,el,jstat
         endif
 1770    format(' ymd=',i4,2i2.2,' hms=',3i2.2,'.',i3.3,' ll=',f8.4,
     X        f10.4,' z=',f8.3,' hdrptr=',f7.2,4f6.2,f6.1,' fae=',
     X        f6.2,2f6.1,i2)
         print *,'DOREC: nfld=',nfld
         do i=1,nfld
            print *,'  i,fldname=',i,' ',fldnam(i),'x'
         end do
c      end if
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
c      rmin = rmin - 1100.

      IF(IYR.GT.99)THEN
         IYR=IYR-((IYR/100)*100)
      END IF

c     patch to fix S-Pol gate spacing
c
      print *,'DOREC: nrng,gatspac,rmin=',nrng,gatspac,rmin
      IF(GATSPAC.EQ.149.0)GATSPAC=150.0
      RMIN=RMIN/1000.0
      GATSPAC=GATSPAC/1000.0
      IPREC=IPREC+1

      DO I=1,NFLD
         INDX=INDEX(FLDNAM(I),' ')
         IF (INDX.GT.1) THEN
            DO J=INDX,8
               FLDNAM(I)(J:J)=' '
            END DO
         END IF
         FLDDOR(I)='        '
         FLDDOR(I)=FLDNAM(I)
      END DO
      
      IF (ISTAT.EQ.4) THEN
         NZER=NZER+1
         WRITE(6,3)IPREC
    3    FORMAT(8X,' ZERO LENGTH RECORD: PHYSICAL RECORD = ',I8)
         IF(NZER.LT.NZERMX)GO TO 1
         WRITE(6,5)NZERMX
    5    FORMAT(1X,'******  MORE THAN',I3,
     +             ' ZERO LENGTH RECORDS = EOT  ******')
         IEOF=0
         IEOT=1
         RETURN
      ELSE IF (ISTAT.EQ.3) THEN
        PRINT 7,IUN
    7   FORMAT(8X,' END OF FILE ON UNIT= ',I3)
        IEOF=0
        IEOT=1
        PRINT 9
 9      FORMAT(1X,'******  LOGICAL END OF TAPE  ******')
        RETURN
      END IF

      NPAR=0
      IEOF=0
      IEOT=0
      NZER=0
      NSML=0
C
      NRF   = IPREC
      IVOL  = 0
      IRY   = 0
      ISWP  = 0
      IDT   = IYR*10000+IMON*100+IDAY
      ITM   = IHR*10000+IMIN*100+ISEC

C*****Patches for Eldora - Plot as a surveillance with +y being up
C            azimuth is rotation angle and elevation is tilt angle.
C
      IF (ICORD.EQ.'AIRBRNE') THEN
         AZ    = ROTANG+ROLL
         EL    = TILT
         ITP   = 8
         FXANG = TILT
      END IF
      IF(EL.GT.180.0)EL=EL-360.0

      WRITE(6,15)IDT,ITM,MSEC
   15 FORMAT(1X,'DOREC: File begins at D=',I6.6,' T=',I6.6,'.',I3.3)
      WRITE(6,17)RADNAM,AZ,EL,FXANG,IVOL,NRF,ISWP,ITP,NRNG,VNYQ,RADCON
   17 FORMAT(6X,'1st DORADE RECORD: Nam=',A8,' A=',F5.1,' E=',F5.1,
     +     ' Fx=',F5.1,' Vol=',I3,' Vr=',I5,' Sw=',I2,' Md=',I1,
     +     ' Ng=',I4,' Vn=',F5.1,' Rcon=',F5.1)
C
C     FIND THE FIELD NAMES, FIRST WORD ADDRESSES,AND NUMBER OF VALUES
C
      WRITE(6,19)NFLD
   19 FORMAT(9X,' NUMBER OF DORADE FIELDS =',I8)
      IF(NFLD.GT.MXUF)THEN
         WRITE(6,21)MXUF
   21    FORMAT(8X,' MORE THAN',I3,' DORADE FORMAT FIELDS ')
         STOP
      END IF
      DO 50 I=1,NFLD
         WRITE(6,47)I,FLDDOR(I),RMIN,GATSPAC,NRNG
c         WRITE(6,47)I,FLDNAM(I),RMIN,GATSPAC
 47      FORMAT(10X,I3,' Name=',A8,
     X        ' Rmin=',F8.3,' km Gsp=',F8.3,' km Nmb gates=',i8)
   50 CONTINUE

      IPREC=IPREC
      RETURN
      END


