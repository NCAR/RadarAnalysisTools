c
c----------------------------------------------------------------------X
c
      SUBROUTINE LABEL3 (PTYP,FTOT,FBAR,PRMN,PRMX,PAMN,PAMX,J,MA,FF,
     X                   FNYQ,DTREND,XRT,YTP,SIDE,PLTSW,NFRAME,
     X                   ITM1,ITM2,BGFLAG)
C
C     DOES ALL PLOT LABELING FOR PLTSPEC ROUTINE
C
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      INCLUDE 'swth.inc'
      CHARACTER LAB*80,LABF*8,LABT*2,LABH*17
      CHARACTER*3 ISCTP(8),FILT(6),MONTH(12),LABLS
      CHARACTER*4 PTYP,DTREND
      CHARACTER*1 BGFLAG
      INTEGER DIR
      LOGICAL PLTSW

      COMMON /ORIGINCH/NETWORK
      CHARACTER*8 NETWORK
      COMMON/ORIGIN/X0,Y0,H0,AZCOR,BAZ,XRD,YRD
      COMMON /INPUTCH/NAMFLD(MXF),IRATYP,ICORD
      CHARACTER*8 NAMFLD,IRATYP,ICORD
      COMMON/ANGLS/ASCAN(MXA),ANGINC(MXA),FXELC(MXA),FXERR(MXA),AVGI
      COMMON /FILCH/FLSPAC(MXF)
      CHARACTER*8 FLSPAC
      COMMON/FIL/IFILTER(MXF),IGATE(MXF),DXX(MXF),DYY(MXF)
      COMMON /LIMITS/ AZB,AZE,AZV

      DATA MONTH/'JAN','FEB','MAR','APR','MAY','JUN',
     +           'JUL','AUG','SEP','OCT','NOV','DEC'/
      DATA FILT/'UNI','TRI','CRE','QUA','EXP','LSQ'/
      DATA ISCTP/'PPI','COP','RHI','VER','TAR','MAN','IDL','SUR'/

C  SETUP LABELING PARAMETERS:  ORDINARY SCAN OR SWATH
C
      IF(PLTSW)THEN
         DRLAB=DRSW
         FXLAB=FXSWA
         ITPLAB=ITPSWA
         ISW=2
      ELSE
         DRLAB=DROLD
         FXLAB=FXOLD
         ITPLAB=ITPOLD
         ISW=1
      END IF

      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)

      WRITE (LAB, 11) IDAY,MONTH(IMON),IYR
   11 FORMAT(I2,1X,A3,1X,I2.2)
      CALL PLCHMQ (.04, .988, LAB, 12.0, 0.0, -1.0)
      IDR=(DRLAB+.00001)*1000
      WRITE (LAB, 13) IDR,VNYQ
   13 FORMAT('GSP=',I4,' M','  VNYQ=',F5.2,' m/s')
      CALL PLCHMQ (.66, .953, LAB, 12.0, 0.0, -1.0)

      WRITE (LAB, 17) FXOLD
   17 FORMAT('Fxang =',F6.1)
      CALL PLCHMQ (.44, .988, LAB, 12.0, 0.0, -1.0)
      WRITE(LAB, 19)ISCTP(ITPOLD)
   19 FORMAT(A3)
      CALL PLCHMQ (.610, .988, LAB, 12.0, 0.0, -1.0)
      WRITE (LAB, 21) NETWORK,ICORD,IRATYP
   21 FORMAT(A8,' Origin at ',A8,' Radar=',A8)
      CALL PLCHMQ (.04, .9619, LAB, 10.0, 0.0, -1.0)

      IF(PTYP.EQ.'ANGL')THEN
         IHR=ITM(NANG(1),ISW)/10000
         IMN=(ITM(NANG(1),ISW)-IHR*10000)/100
         ISEC=ITM(NANG(1),ISW)-IHR*10000-IMN*100
         WRITE (LAB, 31) IHR,IMN,ISEC
   31    FORMAT(2(I2.2,':'),I2.2)
         CALL PLCHMQ (.3, .988, LAB, 12.0, 0.0, -1.0)
         XP = 0.19
         YP = 0.988
         CSIZ = 12.0
         CALL LABTIM (ITM(1,ISW),XP,YP,CSIZ)
         WRITE (LAB, 33)RNG(J,1)
   33    FORMAT('RG = ',F7.3,' KM')
         CALL PLCHMQ (.48, .9619, LAB, 10.0, 0.0, -1.0)
      ELSE
         IF(ITM1.GE.0)THEN
            IHR1=ITM1/10000
            IMN1=(ITM1-IHR1*10000)/100
            ISEC1=ITM1-IHR1*10000-IMN1*100
            IHR2=ITM2/10000
            IMN2=(ITM2-IHR2*10000)/100
            ISEC2=ITM2-IHR2*10000-IMN2*100
            WRITE(LAB,39)IHR1,IMN1,ISEC1,IHR2,IMN2,ISEC2
   39       FORMAT(I2.2,':',I2.2,':',I2.2,'-',I2.2,':',I2.2,':',I2.2)
            CALL PLCHMQ (.19, .988, LAB, 12.0, 0.0, -1.0)
         ELSE
            IHR=ITM(J,ISW)/10000
            IMN=(ITM(J,ISW)-IHR*10000)/100
            ISEC=ITM(J,ISW)-IHR*10000-IMN*100
            WRITE (LAB, 41) IHR,IMN,ISEC
   41       FORMAT(2(I2.2,':'),I2.2)
            CALL PLCHMQ (.3, .988, LAB, 12.0, 0.0, -1.0)
         END IF
         IF(ITPOLD.EQ.3)THEN
            AZLAB=ELA(J,1)
            ELLAB=AZA(J,1)
         ELSE
            AZLAB=AZA(J,1)
            ELLAB=ELA(J,1)
            IF(AZLAB.LT.0.0)AZLAB=AZLAB+360.0
         END IF
         WRITE (LAB, 43)AZLAB,ELLAB
   43    FORMAT('AZ=',F6.2,'  EL=',F6.2)
         CALL PLCHMQ (.44, .9619, LAB, 10.0, 0.0, -1.0)
      END IF

      CALL LABLBOX('SPEC',XRT,YTP)

C     Change color of text written in gray box background
C
      CALL SFLUSH
      CALL GSPLCI(0)

      XP=XRT-0.20
      YP=YTP-0.01
      WRITE(LABH, 45)FF
   45 FORMAT('FUND FRQ=',F8.4)
      YP=YP-0.02
      CALL PLCHMQ( XP, YP, LABH, 10.0, 0.0, -1.0)
      WRITE(LABH, 46)FNYQ
   46 FORMAT(' NYQ FRQ=',F8.4)
      YP=YP-0.02
      CALL PLCHMQ( XP, YP, LABH, 10.0, 0.0, -1.0)
      WRITE(LABH, 47)NINT(FTOT)
   47 FORMAT('  NUMBER=',I8)
      YP=YP-0.02
      CALL PLCHMQ( XP, YP, LABH, 10.0, 0.0, -1.0)
      WRITE(LABH, 49)DTREND
   49 FORMAT('  DTREND=',4X,A4)
      YP=YP-0.02
      CALL PLCHMQ( XP, YP, LABH, 10.0, 0.0, -1.0)
      WRITE(LABH, 51)MA
   51 FORMAT(' PTS AVG=',I8)
      YP=YP-0.02
      CALL PLCHMQ( XP, YP, LABH, 10.0, 0.0, -1.0)
      WRITE(LABH, 53)FBAR
   53 FORMAT('AVG FREQ=',F8.4)
      YP=YP-0.02
      CALL PLCHMQ( XP, YP, LABH, 10.0, 0.0, -1.0)

C     Change text color back to foreground
C
      CALL GSPLCI(1)

      LABLS='ALL'
      FBT=YTP-SIDE
      CALL MYFRAME(NFRAME,NPLT,FBT,LABLS)
      RETURN
      END
