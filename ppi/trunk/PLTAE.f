c
c----------------------------------------------------------------------X
c
      SUBROUTINE PLTAE(JNDAT,ITIMBOV,ITIMEOV,AZVOL,ELVOL,NBMAX,NBVOL,
     X     IDAY,IMON,IYR,NETWORK,IRATYP,ICORD,IVOLOLD,ITPOLD,NFRAME,
     X     FXVOL,NFXMAX,NFXVOL,BGFLAG)
C
C  PLOT ACTUAL ANGLES FOR AN ENTIRE VOLUME SCAN
C
C     AZMN,AZMX - MIN/MAX PLOT BOUNDARIES FOR  AZIMUTH  ANGLES (DEG)
C     ELMN,ELMX -    "      "      "       "  ELEVATION    "     "
C     FXFLAG    - Add fixed angles as a dashed line
C                 ALL, ENDS (1st and last), or NONE
C     DOTSIZE   - (0) Plot points only, (>0) Size of circled dot
C     
C       NBVOL   - NUMBER OF ANGLES IN THE CURRENT VOLUME SCAN
C
      INCLUDE 'colors.inc'
      CHARACTER LAB*80,LABLS*3,IFMTX*6,IFMTY*6
      CHARACTER*3 MONTH(12),ISCTP(8)
      CHARACTER*8 JNDAT(10),NETWORK,IRATYP,ICORD
      CHARACTER*1 BGFLAG
      CHARACTER*8 FXFLAG
      CHARACTER*6 SMRK,LAB6
      LOGICAL COLRFIL,PLTSW
c-----DATA XRT,YTP,SIDE/0.92,0.93,0.82/
      DATA XRT,YTP,SIDEX,SIDEY/0.92,0.93,0.82,0.56/
      DATA ISCTP/'PPI','COP','RHI','VER','TAR','MAN','IDL','SUR'/
      DATA MONTH/'JAN','FEB','MAR','APR','MAY','JUN',
     +           'JUL','AUG','SEP','OCT','NOV','DEC'/
      DIMENSION AZVOL(NBMAX),ELVOL(NBMAX),FXVOL(NFXMAX)

      print *,'PLTAE: nbmax,nbvol,nfxmax,nfxvol=',
     +     nbmax,nbvol,nfxmax,nfxvol

      READ(JNDAT,5)AZMN,AZMX,ELMN,ELMX,FXFLAG,DOTSIZE
 5    FORMAT(/F8.0/F8.0/F8.0/F8.0/A8/F8.0)
      IF(DOTSIZE .GT. 8.0)DOTSIZE = 8.0
      CSIZE = DOTSIZE
      IF(AZMX.LE.AZMN)THEN
         AZMN = 0.0
         AZMX = 0.0
      END IF
      IF(ELMX.LE.ELMN)THEN
         ELMN = 0.0
         ELMX = 0.0
      END IF

C     Complex character for gridded data
C     SMRK='&KGL&E' - Circled dot
C          '&PRU&+' - Plus sign
C
c      SMRK='&PRU&+'
      SMRK='&KGL&E'
      WRITE(LAB6,13)SMRK
 13   FORMAT(A6)

      IF(AZMN .EQ. 0.0 .AND. AZMX .EQ. 0.0)THEN
         AZMAX = -999.
         AZMIN = +999.
         DO J=1,NBVOL
            AZ=AZVOL(J)
            IF(AZ.LT.AZMIN)AZMIN=AZ
            IF(AZ.GT.AZMAX)AZMAX=AZ
         END DO
         AZMN = 10.0*INT(AZMIN/10.0)-10.0
         AZMX = 10.0*INT(AZMAX/10.0)+20.0
      END IF
      IF(ELMN .EQ. 0.0 .AND. ELMX .EQ. 0.0)THEN
         ELMAX = -999.
         ELMIN = +999.
         DO J=1,NBVOL
            EL=ELVOL(J)
            IF(EL.LT.ELMIN)ELMIN=EL
            IF(EL.GT.ELMAX)ELMAX=EL
         END DO
         ELMN = 5.0*INT(ELMIN/5.0)-5.0
         ELMX = 5.0*INT(ELMAX/5.0)+10.0
      END IF
      
      CALL GSPLCI(1)
      CALL GSTXCI(1)

      X2=XRT
      X1=XRT-SIDEX
      Y1=YTP-SIDEY
      Y2=YTP
      CALL MAJMIN(AZMN,AZMX,IFMTX,MJRX,MNRX,IPLX)
      CALL MAJMIN(ELMN,ELMX,IFMTY,MJRY,MNRY,IPLY)
      CALL SET(X1,X2,Y1,Y2,AZMN,AZMX,ELMN,ELMX,1)
      CALL LABMOD(IFMTX,IFMTY,IPLX,IPLY,12,12,8,8,0)
      CALL GRIDAL(MJRX,MNRX,MJRY,MNRY,1,1,5,X1,Y1)

      DO 20 J=1,NBVOL
         AZ=AZVOL(J)
         EL=ELVOL(J)
         IF((AZ.GE.AZMN .AND. AZ.LE.AZMX) .AND.
     +      (EL.GE.ELMN .AND. EL.LE.ELMX) )THEN
            CALL POINT(AZ,EL)
            IF(CSIZE.GT.0.0)CALL PLCHHQ (AZ,EL,LAB6,CSIZE,0.0,0.0)
         ENDIF
 20   CONTINUE

C     Add fixed angles as a dashed line
C     FXFLAG: All - all, Ends - begin and end, None - none
C
c     CALL DASHDB (O'116347')
      CALL DASHDB (O'070707')

      IF(FXFLAG(1:1).EQ.'A')THEN
         IF(ITPOLD.EQ.3)THEN

C     RHI scans

            DO K=1,NFXVOL
               CALL LINED(FXVOL(K),ELMN,FXVOL(K),ELMX)
            END DO
            
         ELSE
            
C     All other scans

            DO K=1,NFXVOL
               CALL LINED(AZMN,FXVOL(K),AZMX,FXVOL(K))
            END DO
            
         END IF
      ELSE
         IF(ITPOLD.EQ.3)THEN

C     RHI scans

            K=1
            CALL LINED(FXVOL(K),ELMN,FXVOL(K),ELMX)
            K=NFXVOL
            CALL LINED(FXVOL(K),ELMN,FXVOL(K),ELMX)
            
         ELSE
            
C     All other scans

            K=1  
            CALL LINED(AZMN,FXVOL(K),AZMX,FXVOL(K))
            K=NFXVOL
            CALL LINED(AZMN,FXVOL(K),AZMX,FXVOL(K))
            
         END IF
      END IF

      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
      WRITE (LAB,31)IDAY,MONTH(IMON),IYR,NETWORK,IRATYP,ICORD,
     +     ISCTP(ITPOLD)
 31   FORMAT(I2,1X,A3,1X,I2.2,2X,A8,2X,A8,'ORIGIN=',A8,2X,A4)
      XP=XRT-SIDEX
      YP=0.980
      CALL PLCHMQ (XP,YP,LAB,12.0,0.0,-1.0)
      ITM1=ITIMBOV
      IHR1=ITM1/10000
      IMN1=(ITM1-IHR1*10000)/100
      ISEC1=ITM1-IHR1*10000-IMN1*100
      ITM2=ITIMEOV
      IHR2=ITM2/10000
      IMN2=(ITM2-IHR2*10000)/100
      ISEC2=ITM2-IHR2*10000-IMN2*100
      WRITE(LAB,35)IHR1,IMN1,ISEC1,IHR2,IMN2,ISEC2,NBVOL
 35   FORMAT(I2,':',I2,':',I2,' TO ',I2,':',I2,':',I2,4X,
     +     'Number of beams in the volume ',I5)
      XP=XRT-SIDEX
      YP=YP-0.02
      CALL PLCHMQ (XP,YP,LAB,12.0,0.0,-1.0)

      XP=0.5*(X1+X2)
      YP=Y1-0.05
      CALL PLCHMQ (XP,YP,'AZIMUTH ANGLE (DEG)',12.0,0.0,0.)
      XP=X1-0.05
      YP=0.5*(Y1+Y2)
      CALL PLCHMQ (XP,YP,'ELEVATION ANGLE (DEG)',12.0,90.0,0.)
      LABLS='   '
      CALL MYFRAME(NFRAME,NPLT,Y1,LABLS)

      RETURN
      END




