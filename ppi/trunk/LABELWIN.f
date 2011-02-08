c
c----------------------------------------------------------------------X
c
      SUBROUTINE LABELWIN(ZSTR,COLRFIL,PLTSW,VECTS,NFRAME,LABLS,IGRPLT,
     X                    BGFLAG,ICOLTYP)
C
C  ANNOTATE THE PLOT AND CALL FRAME.  THIS LABELING ROUTINE IS USED FOR
C  MULTIPLE PLOTS PER FRAME
C     ZSTR    - STRETCHING FACTOR FOR Y PLOT AXIS (RHI ONLY)
C     COLRFIL - .TRUE. IF THE PLOT HAS COLOR AREA FILL
C     PLTSW   - .TRUE. IF A SWATH, ISOCHRON, INTEGR, OR AVRAGE PLOT
C     NFRAME  - FRAME NUMBER, INCREMENTED IN ROUTINE MYFRAME
C     BGFLAG  - (' ') BLACK OR ('W') WHITE BACKGROUND
C
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      INCLUDE 'swth.inc'
      INCLUDE 'colors.inc'
      CHARACTER LAB*80,LABT*2,LABG*34,LABC*6
      CHARACTER*3 ISCTP(8),FILT(6),LABLS
      CHARACTER*1 BGFLAG
      CHARACTER*8 NETWORK
      CHARACTER*8 ICOLTYP
      INTEGER DIR

      LOGICAL COLRFIL,PLTSW,VECTS,FOF,UNI

      REAL NSMIN,NSMAX

      COMMON /CONTOUR / LMX,LMN
      COMMON /ORIGINCH/NETWORK
      COMMON/ORIGIN/X0,Y0,H0,AZCOR,BAZ,XRD,YRD
      COMMON/BCN/RNGMIN,RNGMAX,TYMN,TYMX,IBSCAN,TYMSCL
      COMMON/COTABLE/ICOL(100)
      COMMON /DASHPATCH/IBWFLG
      CHARACTER*2 IBWFLG
      COMMON /DASHPAT/IDPAT(5,3),JDPAT,LWSTART,LWINC
      COMMON/ANGLS/ASCAN(MXA),ANGINC(MXA),FXELC(MXA),FXERR(MXA),AVGI
      COMMON /FILCH/FLSPAC(MXF)
      CHARACTER*8 FLSPAC
      COMMON/FIL/IFILTER(MXF),IGATE(MXF),DXX(MXF),DYY(MXF)
      COMMON /INPUTCH/NAMFLD(MXF),IRATYP,ICORD
      CHARACTER*8 NAMFLD,IRATYP,ICORD
      COMMON /LIMITS/ AZB,AZE,AZV
      COMMON /MAX/ ZMAX,XX,YY,ZZ
      COMMON /PLTWIN/XRT(26),YTP(26),SIDEX(26),SIDEY(26),DXL,DXR,DYB,
     + DYT,NROW,NCOL,NWIN,IWIN,LABX(26),LABY(26),ILFLG,XBTP(26),
     + YBTP(26),SIZBX,XSHIFT,YSHIFT

      CHARACTER*3 MONTH(12)

      DATA NPLT,SPMX/0,0.04/
      DATA FILT/'UNI','TRI','CRE','QUA','EXP','LSQ'/
      DATA ISCTP/'PPI','COP','RHI','VER','TAR','MAN','IDL','SUR'/
      DATA MONTH/'JAN','FEB','MAR','APR','MAY','JUN',
     +           'JUL','AUG','SEP','OCT','NOV','DEC'/
      DATA JOV,XOFF/0,-0.025/

C  SETUP LABELING PARAMETERS:  ORDINARY SCAN OR SWATH
C
      IF(PLTSW)THEN
         DRLAB=DRSW
         FXLAB=FXSWA
         ITPLAB=ITPSWA
         ISW=2
         ITM1=ITIME1
         ITM2=ITIME2
         IFTIME1=ITIME1
      ELSE
         DRLAB=DROLD
         FXLAB=FXOLD
         ITPLAB=ITPOLD
         ISW=1
         IFTIME1=IFTIME
         IF(IWIN.EQ.2)ITM1=IFTIME
         IF(IWIN.GT.NWIN)ITM2=ITIME
      END IF

C  SET COUNTER FOR HORIZONTAL POSITIONING OF FIELD AND CONTOUR LABELS
C
      JOV=JOV+1
      XOFF=XOFF+0.025

C     DRAW PERIMETER AND CONSTANT HEIGHT LINES
C
c      WRITE(6,1770)NWIN,IWIN,BGFLAG,IGRPLT
c1770  FORMAT(1X,'    LABELWIN:NWIN,IWIN=',2I4,A1,'*',I4)
      IF(IWIN.EQ.999)GO TO 41
      CALL INIT2(ZSTR,COLRFIL,XRT(IWIN),YTP(IWIN),SIDEX(IWIN),
     X           SIDEY(IWIN),IGRPLT,BGFLAG,LABX(IWIN),LABY(IWIN),
     X           DXL,DXR,DYB,DYT,NROW,NCOL,XSHIFT,YSHIFT)
      IF(.NOT.PLTSW.AND.IARCS(ITPOLD).GT.1
     +   .AND.ITPOLD.NE.3.AND.IBSCAN.NE.1) THEN
         CALL ARC
      END IF

      IF(ZMAX .NE. -999.) THEN
         CALL FRSTPT (XX,YY)
         CALL MXMY(MXX,MYY)
      END IF

      IF(IBSCAN.NE.1)THEN
         CALL FRSTPT(GXMAX(ITPOLD),GYMIN(ITPOLD))
      ELSE
         RAZ=NANG(ISW)
         CALL FRSTPT(RAZ,RNGMIN)
      END IF
      CALL MXMY(MX1,MY1)

C     SWITCH TO DEVICE OR FRACTIONAL COORDINATES FOR LABELING
C
      CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
      IF(IWIN.EQ.2)THEN
         FL1=FL
         FR1=FR
         FB1=FB
         FT1=FT
      END IF
      FMDX=0.5*(FL+FR)
      FMDY=0.5*(FB+FT)

C     ADD TIME AND FXANG
C
      IHR=ITIME/10000
      IMN=(ITIME-IHR*10000)/100
      ISEC=ITIME-IHR*10000-IMN*100
C      write(*,*)"labl:",namfld(ifl),pltsw,iwin,iov,iftime1,itime
      WRITE (LAB, 61) IFTIME1,FXLAB
   61 FORMAT(I6,F6.1)
      XP=DXL
      YP=DYT+0.01
      IF(LABLS.NE.'ABR')CALL PLCHMQ(XP,YP,LAB,9.0,0.0,-1.0)
      IF(ICOLTYP(1:4).EQ.'DIGT')GO TO 41

C     ADD FIELD NAME
C
      IF(IFL.GT.0)THEN
         WRITE (LAB,63) NAMFLD(IFL)
 63      FORMAT(A8)
         XPT=XP+0.05+JOV*0.075
c         write(*,*)'iwin,jov=',namfld(ifl),iwin-1,jov,xp,xpt
         IF(LABLS.NE.'ABR')CALL PLCHMQ(XPT,YP,LAB,9.0,0.0,-1.0)
      END IF
      IF(VECTS)GO TO 41

C     ADD COLOR BAR AND CONTOUR LABEL
C
      WRITE(LAB, 65)NAMFLD(IFL)
 65   FORMAT(A8)
      IF(ILFLG.EQ.0.OR.IWIN.EQ.2)THEN
         IF (COLRFIL) THEN
            CALL LABCOLWIN (CL, NL,IGRPLT)
            XP=XBTP(IWIN)+0.01-XSHIFT
            YP=DYT+0.005
            IF(LABLS.NE.'ABR')CALL PLCHMQ(XP,YP,LAB,9.0,90.,-1.0)
            XOFF=-0.025
         ELSE
            IF(LMN.GT.LMX) THEN
               XP=0.5*(DXL+DXR)
               YP=0.5*(DYB+DYT)
C               CALL PLCHMQ (XP, YP, 'NO CONTOURS', 24., 0.0, 0.0)
            ELSE
               SPACE=0.9*(DYT-DYB)/LMX
               IF(SPACE.GT.SPMX)SPACE=SPMX
               YP=DYT
               X1=XBTP(IWIN)+0.65*SIZBX-XSHIFT+XOFF
               X2=XBTP(IWIN)+0.85*SIZBX-XSHIFT+XOFF
               XL=XBTP(IWIN)+0.90*SIZBX-XSHIFT+XOFF
               IF(LABLS.NE.'ABR')CALL PLCHMQ(X2,YP,LAB,9.0,90.,-1.0)
               HALFSP=0.5*SPACE
               YP=DYT-HALFSP

               DO 40 K=LMN,LMX
                  YP=YP-HALFSP
                  IF(YP.LT.DYB)GO TO 40
                  IF(ABS(CL(K)).GE.998.0)GO TO 40
                  FLTCL=CL(K)-INT(CL(K))
                  IF(FLTCL.EQ.0.0)THEN
                     WRITE (LABC, 33)INT(CL(K))
   33                FORMAT(I6)
                  ELSE
                     WRITE (LABC, 35)CL(K)
   35                FORMAT(F6.1)
                  END IF
                  CALL PLCHMQ (XL, YP, LABC, 7.0, 0.0, 1.0)
                  YP=YP-HALFSP
                  IF(MOD(K,2).EQ.0.AND.ISHADE.EQ.1) THEN
                     CALL PLCHMQ (XL, YP,'S',7.0,0.0,0.0)
                  END IF
                  IF(IBWFLG.EQ.'CL')THEN
                     CALL SFLUSH
                     CALL GSPLCI (ICOL(K))
                     CALL LINE (X1, YP,X2, YP)
                  ELSE
                     IF(JDPAT.EQ.0)THEN
                        CALL SETUSV('LW',1000)
                        CALL LINE (X1, YP,X2, YP)
                     ELSE IF(JDPAT.EQ.5)THEN
                        J=1+MOD(K-1,3)
                        KLW=LWSTART-(J-1)*LWINC
                        CALL SETUSV('LW',KLW)
                        CALL LINE (X1, YP,X2, YP)
                        CALL SETUSV('LW',1000)
                     ELSE IF(JDPAT.GE.1.AND.JDPAT.LE.3)THEN
                        IROB=MOD(K,3)+1
                        CALL DASHDB (IDPAT(JDPAT,IROB))
                        CALL LINED (X1, YP,X2, YP)
                     ELSE IF(JDPAT.EQ.4)THEN
                        IF(CL(K).GE.0.0)THEN
                           CALL LINE (X1, YP,X2, YP)
                        ELSE
                           CALL DASHDB (IDPAT(JDPAT,2))
                           CALL LINED (X1, YP,X2, YP)
                        END IF
                     END IF
                  END IF
   40          CONTINUE
            END IF
         END IF
      END IF

   41 NOSCAN=0

C  RESET FIELD LABEL POSITIONING COUNTER AND OFFSET
C
      IF(IOV.EQ.0)THEN
         JOV=0
         XOFF=-0.025
      END IF

      IF(IWIN.LE.NWIN)RETURN
      IF(IOV.NE.0)RETURN
      WRITE (LAB, 11) IDAY,MONTH(IMON),IYR,NETWORK,IRATYP,ICORD,
     +                ISCTP(ITPLAB)
   11 FORMAT(I2,1X,A3,1X,I2.2,2X,A8,2X,A8,'ORIGIN=',A8,2X,A4)
      XP=FL1
      YP=0.980-YSHIFT
      CALL PLCHMQ (XP,YP, LAB, 12.0, 0.0, -1.0)
      IHR1=ITM1/10000
      IMN1=(ITM1-IHR1*10000)/100
      ISEC1=ITM1-IHR1*10000-IMN1*100
      IHR2=ITM2/10000
      IMN2=(ITM2-IHR2*10000)/100
      ISEC2=ITM2-IHR2*10000-IMN2*100
      WRITE(LAB,45)IHR1,IMN1,ISEC1,IHR2,IMN2,ISEC2
45    FORMAT(I2.2,':',I2.2,':',I2.2,' TO ',I2.2,':',I2.2,':',I2.2)
      XP=FL1
      YP=0.960-YSHIFT
      CALL PLCHMQ (XP,YP, LAB, 12.0, 0.0, -1.0)
      IF(PLTSW)THEN
         WRITE (LAB, 55)FXANG1,FXANG2
   55    FORMAT('SWATH ',F5.1,'-',F5.1,' DG')
         XP=FL1+0.34
         YP=0.9619-YSHIFT
         CALL PLCHMQ (XP,YP, LAB, 10.0, 0.0, -1.0)
      END IF

      IF(IBSCAN.EQ.1)THEN
c********CALL LABTIM(ITM(NANG(ISW),ISW))
         ITPLAB=4
         IMN1=-1
         DO 80 I=1,NANG(ISW)
            IHR=ITM(I,ISW)/10000
            IMN=(ITM(I,ISW)-IHR*10000)/100
            IF(IMN.NE.IMN1)THEN
               RX=0.065+.855*FLOAT(I)/FLOAT(NANG(ISW))
               WRITE(LABT, 71)IMN
   71          FORMAT(I2)
               IF(IMN1.NE.-1)CALL PLCHMQ(RX,.03,LABT,8.,0.,0.)
               IMN1=IMN
            END IF
   80    CONTINUE
      END IF

  120 NPLT=NPLT+1
      IF(IOV.EQ.0.AND.IWIN.GT.NWIN)CALL MYFRAME(NFRAME,NPLT,FB,LABLS)
      CALL SET (FL,FR,FB,FT,UL,UR,UB,UT,LLL)
      CALL SETUSV('LW',1000)
      RETURN
      END
