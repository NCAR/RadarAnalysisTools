c
c----------------------------------------------------------------------X
c
      SUBROUTINE LABEL2 (IRTYPE,PTOT,PBAR,PSTD,HRMN,HRMX,HAMN,HAMX,
     X                   HZMN,HZMX,BINC,ILAG,JLAG,PLTSW,NFRAME,FBT,
     X                   PMIN,PMAX,CCF,STDERR,C0,C1,LABLS,BGFLAG)
C
C     DOES ALL PLOT LABELING FOR PLTHIST, PLTSCAT AND PLTVAD ROUTINES
C
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      INCLUDE 'swth.inc'
      INCLUDE 'colors.inc'
      CHARACTER LAB*80,LABF*8,LABT*2,LABH*17
      CHARACTER*3 ISCTP(8),FILT(6),MONTH(12),LABLS
      CHARACTER*4 IRTYPE
      CHARACTER*8 FLSPAC,NETWORK,NAMFLD,IRATYP,ICORD
      CHARACTER*1 BGFLAG
      INTEGER DIR
      LOGICAL PLTSW

      COMMON /ORIGINCH/NETWORK
      COMMON/ORIGIN/X0,Y0,H0,AZCOR,BAZ,XRD,YRD
      COMMON /INPUTCH/NAMFLD(MXF),IRATYP,ICORD
      COMMON/ANGLS/ASCAN(MXA),ANGINC(MXA),FXELC(MXA),FXERR(MXA),AVGI
      COMMON /FILCH/FLSPAC(MXF)
      COMMON/FIL/IFILTER(MXF),IGATE(MXF),DXX(MXF),DYY(MXF)
      COMMON /LIMITS/ AZB,AZE,AZV
      COMMON /PLTWIN/XRT(26),YTP(26),SIDEX(26),SIDEY(26),DXL,DXR,DYB,
     + DYT,NROW,NCOL,NWIN,IWIN,LABX(26),LABY(26),ILFLG,XBTP(26),
     + YBTP(26),SIZBX,XSHIFT,YSHIFT

      DATA MONTH/'JAN','FEB','MAR','APR','MAY','JUN',
     +           'JUL','AUG','SEP','OCT','NOV','DEC'/
      DATA FILT/'UNI','TRI','CRE','QUA','EXP','LSQ'/
      DATA ISCTP/'PPI','COP','RHI','VER','TAR','MAN','IDL','SUR'/
      DATA XRT1,YTP1,SIDE/0.920,0.940,0.84/

C     NOTE: This labeling code could use some cleaning up
C           to separate all histogram labeling from all
C           scatter plot labeling. (LJM - Mar 11, 2011)
C           This is mostly true for character colors
C           inside the gray box in the upper righ corner. 
C

C  SETUP LABELING PARAMETERS:  ORDINARY SCAN OR SWATH
C
      IF(PLTSW)THEN
         DRLAB=DRSW
         FXLAB=FXSWA
         ITPLAB=ITPSWA
         ISW=2
         ITM1=ITIME1
         ITM2=ITIME2
      ELSE
         DRLAB=DROLD
         FXLAB=FXOLD
         ITPLAB=ITPOLD
         ISW=1
         ITM1=IFTIME
         ITM2=ITIME
      END IF

      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)

      IF(LABLS.EQ.'NON' .OR. LABLS.EQ.'ABR')GOTO 28

      WRITE (LAB, 11) IDAY,MONTH(IMON),IYR
 11   FORMAT(I2,1X,A3,1X,I2.2)
      CALL PLCHMQ (.04, .988, LAB, 12.0, 0.0, -1.0)
      WRITE (LAB, 12) HZMN,HZMX
 12   FORMAT('  HEIGHT=',F5.1,' to ',F5.1)
      XP=0.65
      YP=0.995
      CALL PLCHMQ (XP,YP, LAB, 8.0, 0.0, -1.0)
      AZ1=HAMN
      AZ2=HAMX
      WRITE (LAB, 13) AZ1,AZ2
 13   FORMAT('   ANGLE=',F5.1,' to ',F5.1)
      YP=YP-0.015
      CALL PLCHMQ (XP,YP, LAB, 8.0, 0.0, -1.0)
      IDR=(DRLAB+.00001)*1000
      WRITE (LAB, 15) HRMN,HRMX,IDR
 15   FORMAT('HORZ-RNG=',F5.1,' to ',F5.1,' DR=',I4,'m')
      YP=YP-0.015
      CALL PLCHMQ (XP,YP, LAB, 8.0, 0.0, -1.0)
      WRITE (LAB, 16) VNYQ
 16   FORMAT('    VNYQ=',F6.2,' m/s')
      YP=YP-0.015
      CALL PLCHMQ (XP,YP, LAB, 8.0, 0.0, -1.0)

      WRITE (LAB, 17) ISCTP(ITPOLD),FXLAB
 17   FORMAT(A3,' FXANG =',F6.1)
      CALL PLCHMQ (.44, .988, LAB, 12.0, 0.0, -1.0)
      WRITE (LAB, 21) NETWORK,ICORD,IRATYP
 21   FORMAT(A8,' Origin at ',A8,' Radar=',A8)
      CALL PLCHMQ (.04, .9619, LAB, 10.0, 0.0, -1.0)

      IHR1=ITM1/10000
      IMN1=(ITM1-IHR1*10000)/100
      ISEC1=ITM1-IHR1*10000-IMN1*100
      IHR2=ITM2/10000
      IMN2=(ITM2-IHR2*10000)/100
      ISEC2=ITM2-IHR2*10000-IMN2*100
      WRITE(LAB,23)IHR1,IMN1,ISEC1,IHR2,IMN2,ISEC2
 23   FORMAT(I2.2,':',I2.2,':',I2.2,' TO ',I2.2,':',I2.2,':',I2.2)
      XP=.19
      YP=0.988
      CALL PLCHMQ (XP,YP, LAB, 12.0, 0.0, -1.0)
      IF(PLTSW)THEN
         WRITE (LAB, 25)FXANG1,FXANG2
 25      FORMAT('SWATH ',F5.1,'-',F5.1,' DG')
         CALL PLCHMQ (.44, .9619, LAB, 10.0, 0.0, -1.0)
      END IF
      
 28   CONTINUE

      CALL LABLBOX(IRTYPE,XRT_dum,YTP_dum)

      IF(IRTYPE.NE.'VAD'.AND.IRTYPE.NE.'COV')THEN

C     Change color of text written in gray box
C     to background index = 0
C
         CALL SFLUSH
         CALL GSPLCI(0)

         WRITE(LABH, 29)INT(PTOT)
 29      FORMAT('NMB=',I8)
         IF(IRTYPE.EQ.'SCAT')THEN
            XP=XRT1-0.15
         ELSE
            XP=XRT1-0.08
         END IF
         YP=YTP1-0.03
         CALL PLCHMQ( XP, YP, LABH, 10.0, 0.0, -1.0)
         IF(IRTYPE.EQ.'SCAT')THEN
            WRITE(LABH, 291)CCF
 291        FORMAT('COR=',F8.2)
            YP=YP-0.02
            CALL PLCHMQ( XP, YP, LABH, 10.0, 0.0, -1.0)
            WRITE(LABH, 292)STDERR
 292        FORMAT('ERR=',F8.2)
            YP=YP-0.02
            CALL PLCHMQ( XP, YP, LABH, 10.0, 0.0, -1.0)
            WRITE(LABH, 293)C0
 293        FORMAT(' C0=',F8.2)
            YP=YP-0.02
            CALL PLCHMQ( XP, YP, LABH, 10.0, 0.0, -1.0)
            WRITE(LABH, 294)C1
 294        FORMAT(' C1=',F8.2)
            YP=YP-0.02
            CALL PLCHMQ( XP, YP, LABH, 10.0, 0.0, -1.0)
         END IF

C     Change color of text back to foreground index=1
C     
         CALL SFLUSH
         CALL GSPLCI(1)
      END IF

      IF(IRTYPE.EQ.'SCAT')THEN
         WRITE(LABH, 31)ILAG,JLAG
   31    FORMAT('LAG=',2I3)
         YP=YP-0.02
         IF(ILAG.NE.0 .AND. JLAG.NE.0)THEN
            CALL PLCHMQ( XP, YP, LABH, 10.0, 0.0, -1.0)
         END IF
      END IF
      IF(IRTYPE.EQ.'HIST')THEN

C     Change color of text written in gray box
C     to background index = 0
C
         CALL SFLUSH
         CALL GSPLCI(0)

         WRITE(LABH, 33)PBAR
   33    FORMAT('AVG=',F8.2)
         YP=YP-0.02
         CALL PLCHMQ( XP, YP, LABH, 10.0, 0.0, -1.0)
         WRITE(LABH, 35)PSTD
   35    FORMAT('STD=',F8.2)
         YP=YP-0.02
         CALL PLCHMQ( XP, YP, LABH, 10.0, 0.0, -1.0)
         WRITE(LABH, 37)BINC
   37    FORMAT('INC=',F8.2)
         YP=YP-0.02
         CALL PLCHMQ( XP, YP, LABH, 10.0, 0.0, -1.0)
         WRITE(LABH, 371)PMIN
 371     FORMAT('MIN=',F8.2)
         YP=YP-0.02
         CALL PLCHMQ( XP, YP, LABH, 10.0, 0.0, -1.0)
         WRITE(LABH, 373)PMAX
 373     FORMAT('MAX=',F8.2)
         YP=YP-0.02
         CALL PLCHMQ( XP, YP, LABH, 10.0, 0.0, -1.0)

C     Change color of text back to foreground index=1
C     
         CALL SFLUSH
         CALL GSPLCI(1)

      END IF

      IF(IRTYPE.EQ.'VAD'.OR.IRTYPE.EQ.'COV')THEN
         IF(PTOT.NE.0.0)THEN
            WRITE(LABH, 39)2*INT(PTOT)+1
   39       FORMAT('FILT=',I4,' GATES')
         ELSE
            WRITE(LABH, 41)
   41       FORMAT('NOT SMOOTHED')
         END IF
         XP=0.10
         YP=FBT-0.075
         CALL PLCHMQ( XP, YP, LABH, 10.0, 0.0, -1.0)
      END IF

      CALL MYFRAME(NFRAME,NPLT,FBT,LABLS)
      RETURN
      END



