c     
c----------------------------------------------------------------------X
c
      SUBROUTINE LABEL (ZSTR,COLRFIL,PLTSW,VECTS,NFRAME,LABLS,IGRPLT,
     X                  BGFLAG,NOLAB,ICOLTYP)
C
C  ANNOTATE THE PLOT AND CALL FRAME
C     ZSTR    - STRETCHING FACTOR FOR Y PLOT AXIS (RHI ONLY)
C     COLRFIL - .TRUE. IF THE PLOT HAS COLOR AREA FILL
C     PLTSW   - .TRUE. IF A SWATH, ISOCHRON, INTEGR, OR AVRAGE PLOT
C     NFRAME  - FRAME NUMBER, INCREMENTED IN ROUTINE MYFRAME
C     BGFLAG  - (' ') BLACK OR ('W') WHITE BACKGROUND
C     IFTIME  - Beginning time of scan
C     ITIME1  - Beginning time of the swath
C     ITIME   - Ending time of scan or swath
C
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      INCLUDE 'swth.inc'
      INCLUDE 'colors.inc'
      CHARACTER LAB*80,LABG*34,LABC*6,ICOLTYP*8
      CHARACTER LABR*21
      CHARACTER*3 ISCTP(8),FILT(6),LABLS
      CHARACTER*1 BGFLAG
      CHARACTER*8 NETWORK

      LOGICAL COLRFIL,PLTSW,VECTS

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
C     
C     See PLT_CLOCK for COMMON block: 
C        PLTCLOCK - NEXRAD data clock is plotted if .TRUE.
C        TIME_BEG - Beginning hour of NEXRAD data
C        TIME_END - Ending    hour of NEXRAD data
C        REQ_RADS - Number of NEXRADs requested
C        REC_RADS - Number of NEXRADs received
C        REC_HOURS- Total number of hours received
C        REQ_HOURS- Total number of hours requested
C        TPERCENT - Total percent requested/received
C        DATE_BEG - Beginning date of NEXRAD data
C        DATE_END - Ending    date of NEXRAD data
C        NEX_NAME - NEXRAD radar name for plot label
C        PLTPERCNT- NEXRAD data percent is plotted if .TRUE.
C
      PARAMETER (MXKK=1000)
      LOGICAL PLTCLOCK,PLTPERCNT
      INTEGER TIME_BEG,TIME_END,REQ_RADS,REC_RADS,REC_HOURS,REQ_HOURS
      CHARACTER*11 DATE_BEG,DATE_END
      CHARACTER*14 NEX_NAME(MXKK)
      COMMON/NEXCLOCK/PLTCLOCK,PLTPERCNT,TIME_BEG,TIME_END,
     X     REQ_RADS,REC_RADS,REQ_HOURS,REC_HOURS,TPERCENT,
     X     DATE_BEG,DATE_END,NEX_NAME

C     See PLTNET: PLTRING - Range ring is plotted if .TRUE.
C
      LOGICAL PLTRING
      COMMON/PLTRNG/PLTRING

      CHARACTER*3 MONTH(12)

      DATA MNANG/5/
      DATA NPLT,SPMX/0,0.04/
      DATA FILT/'UNI','TRI','CRE','QUA','EXP','LSQ'/
      DATA ISCTP/'PPI','COP','RHI','VER','TAR','MAN','IDL','SUR'/
      DATA MONTH/'JAN','FEB','MAR','APR','MAY','JUN',
     +           'JUL','AUG','SEP','OCT','NOV','DEC'/
      DATA JOV,XOFF,XOFF1,XOFF2/0,0.018,0.025,0.008/

c      write(*,*)'label: ',labls
C  SETUP LABELING PARAMETERS:  ORDINARY SCAN OR SWATH
C
      IF(NOLAB.EQ.1)THEN
        CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)
        CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
        GO TO 120
      END IF
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
c      write(*,*)'label: ',pltsw,drlab,fxlab,itplab,isw
c      write(*,*)'  itpold,itpswa=',itpold,itpswa

C     Set counter for positioning of field and contour labels.
C
      IF(ICOLTYP.NE.'SAMPLOC ' .AND. ICOLTYP(1:4).NE.'DIGT')JOV=JOV+1
         
C     DRAW PERIMETER AND CONSTANT HEIGHT LINES
C
      CALL INIT2(ZSTR,COLRFIL,XRT(IWIN),YTP(IWIN),SIDEX(IWIN),
     X     SIDEY(IWIN),IGRPLT,BGFLAG,LABX(IWIN),LABY(IWIN),
     X     DXL,DXR,DYB,DYT,NROW,NCOL,XSHIFT,YSHIFT)
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

      FMDX=0.5*(FL+FR)
      FMDY=0.5*(FB+FT)
c      IF(LABLS.EQ.'NON')THEN
c         CALL LABCOL_NON (CL, NL,IGRPLT)
c         GO TO 112
c      END IF
c      IF(LABLS.EQ.'NON')GO TO 112
      IF(LABLS.EQ.'NON')GO TO 32
      IF(ICOLTYP(1:4).EQ.'DIGT')GO TO 41

C     Bypass labeling for map of ringed subnet
C
      IF(PLTRING)GO TO 112

C     Add station clock for NEXRAD data availability
C     and NEXRAD names along right hand side
C
      IF(PLTCLOCK)THEN

         XP=.04
         YP=.988
         WRITE(LAB,11)DATE_BEG,TIME_BEG,DATE_END,TIME_END
 11      FORMAT(A11,1X,'[',I4.4,']',' -- ',A11,1X,'[',I4.4,']')
         CALL PLCHMQ (XP, YP, LAB, 12.0, 0.0, -1.0)

         XP=FR
         WRITE(LABR,12)REC_RADS,REQ_RADS
 12      FORMAT('WSR88Ds REC/REQ ',I2,'/',I2)
         CALL PLCHMQ (XP, YP, LABR, 12.0, 0.0, 1.0)

         DYP=0.018
         XP=FL
         YP=FT+DYP
         WRITE(LAB,13)REQ_HOURS
 13      FORMAT(I2,' Hr period - Day ')
         CALL PLCHMQ (XP, YP, LAB, 12.0, 0.0, -1.0)

         XP=XP+0.22
         DO L=1,3
            WRITE(LAB,14)L
 14         FORMAT('[',I1,'] ')
            CALL PLCHMQ (XP, YP, LAB, 12.0, 0.0, -1.0)
            CALL DASHDB (IDPAT(5,L))
            CALL LINED (XP+0.04,YP,XP+0.075,YP)
            XP=XP+0.10
         END DO

         XP=FR+0.006
         YP=FT+DYP
         WRITE(LAB,15)
 15      FORMAT('  RADR-Hrs')
         CALL PLCHMQ (XP, YP, LAB, 10.0, 0.0, -1.0)

         XP=FR+0.006
         YP=FT
         DO L=1,REQ_RADS
            WRITE(LAB,17)NEX_NAME(L)
 17         FORMAT(A9)
            CALL PLCHMQ (XP, YP, LAB, 10.0, 0.0, -1.0)
            YP=YP-DYP
         END DO

         GO TO 112
      END IF

C     Add station clock for NEXRAD data availability
C     and NEXRAD names along right hand side
C
      IF(PLTPERCNT)THEN

         XP=.04
         YP=.988
         WRITE(LAB,21)REC_HOURS,REQ_HOURS,TPERCENT
 21      FORMAT(
     X        '1998 SEASON - PERCENT DATA FOR WSR88Ds: Hours rec/req=',
     X        I4.4,'/',I4.4,' ==> ',F4.1,'%')
         CALL PLCHMQ (XP, YP, LAB, 12.0, 0.0, -1.0)

         DYP=0.016
         XP=FR+0.005
         YP=FT+3*DYP
         WRITE(LAB,23)
 23      FORMAT('      HOURS ')
         CALL PLCHMQ (XP, YP, LAB, 8.0, 0.0, -1.0)

         YP=YP-DYP
         WRITE(LAB,25)
 25      FORMAT('RADR-REC/REQ')
         CALL PLCHMQ (XP, YP, LAB, 8.0, 0.0, -1.0)

         YP=YP-DYP
         DO L=1,REQ_RADS
            WRITE(LAB,27)NEX_NAME(L)
 27         FORMAT(A14)
            CALL PLCHMQ (XP, YP, LAB, 8.0, 0.0, -1.0)
            YP=YP-DYP
            IF(L.EQ.42)THEN
               YP=YP-1.5*DYP
            END IF
         END DO

         GO TO 112
      END IF

C     ADD FIELD NAME
C
      IF(IFL.GT.0)THEN
         WRITE(LAB,31)NAMFLD(IFL)
 31      FORMAT(A8)
         YP=DYT+0.006
         IF(COLRFIL)THEN
            XP=.985
            XP=XRT(IWIN)+.006
         ELSE
            XP=XRT(IWIN)+JOV*XOFF+XOFF1
         END IF
c         write(*,*)'jov,xp=',namfld(ifl),xrt(iwin),jov,xp
         IF(ICOLTYP.NE.'SAMPLOC ' .AND. ICOLTYP(1:4).NE.'DIGT')THEN
            CALL PLCHMQ(XP, YP, LAB, 7.0, 90., -1.0)
         END IF
      END IF
      IF(VECTS)GO TO 41

C     ADD COLOR BAR AND CONTOUR LABEL
C
 32   CONTINUE
      IF (COLRFIL) THEN
         IF(LABLS.EQ.'NON')THEN

C           Add field name [LAB = NAMFLD(IFL)] just above
C           upper left-hand corner when LABLS = 'NON'
C
            IF(IFL.GT.0)WRITE(LAB,31)NAMFLD(IFL)
            XP = XRT(IWIN)-SIDEX(IWIN)
            YP = YTP(IWIN)+0.015
            CALL PLCHMQ(XP, YP, LAB, 12.0, 0., -1.0)
            CALL LABCOL_NON (CL, NL,IGRPLT)
            GO TO 112
         ELSE
            CALL LABCOL (CL, NL,IGRPLT)
c            YP=DYT+0.006
c            CALL PLCHMQ(XRT(IWIN)+0.006, YP, LAB, 7.0, 90., -1.0)
         END IF
      ELSE
         IF(LABLS.EQ.'NON')GO TO 112
         IF(LMN.GT.LMX) THEN
            XPNC=0.5*(DXL+DXR)
            YPNC=0.5*(DYB+DYT)
C            CALL PLCHMQ (XPNC, YPNC, 'NO CONTOURS', 24., 0.0, 0.0)
         ELSE
            SPACE=(DYT-DYB)/LMX
            IF(SPACE.GT.SPMX)SPACE=SPMX
            YP=DYT+0.006
            CALL PLCHMQ(XP, YP, LAB, 7.0, 90., -1.0)
            HALFSP=0.5*SPACE
            YP=DYT-HALFSP

            DO 40 K=LMN,LMX
               YP=YP-HALFSP
               IF(YP.LT.DYB)GO TO 40
               IF(ABS(CL(K)).GE.998.0)GO TO 40
               FLTCL=CL(K)-INT(CL(K))
               IF(FLTCL.EQ.0.0)THEN
                  WRITE (LABC,33)INT(CL(K))
 33               FORMAT(I6)
               ELSE
                  WRITE (LABC,35)CL(K)
 35               FORMAT(F6.1)
               END IF
               CALL PLCHMQ (XP+XOFF2, YP, LABC, 5.0, 0.0, 1.0)
               YP=YP-HALFSP
               IF(MOD(K+1,2).EQ.0.AND.ISHADE.EQ.1) THEN
                  CALL PLCHMQ (XP+XOFF2+.002, YP, 'X', 5.0, 0.0, 1.0)
               END IF
               IF(IBWFLG.EQ.'CL')THEN
                  CALL SFLUSH
                  CALL GSPLCI (ICOL(K))
                  CALL LINE (XP-XOFF2, YP, XP+XOFF2, YP)
               ELSE
                  IF(JDPAT.EQ.0)THEN
                     CALL SETUSV('LW',1000)
                     CALL LINE (XP-XOFF2, YP, XP+XOFF2, YP)
                  ELSE IF(JDPAT.EQ.5)THEN
                     J=1+MOD(K-1,3)
                     KLW=LWSTART-(J-1)*LWINC
                     CALL SETUSV('LW',KLW)
                     CALL LINE (XP-XOFF2, YP, XP+XOFF2, YP)
                     CALL SETUSV('LW',1000)
                  ELSE IF(JDPAT.GE.1.AND.JDPAT.LE.3)THEN
                     IROB=MOD(K,3)+1
                     CALL DASHDB (IDPAT(JDPAT,IROB))
                     CALL LINED (XP-XOFF2, YP, XP+XOFF2, YP)
                  ELSE IF(JDPAT.EQ.4)THEN
                     IF(CL(K).GE.0.0)THEN
                        CALL LINE (XP-XOFF2, YP, XP+XOFF2, YP)
                     ELSE
                        CALL DASHDB (IDPAT(JDPAT,2))
                        CALL LINED (XP-XOFF2, YP, XP+XOFF2, YP)
                     END IF
                  END IF
               END IF
 40         CONTINUE
         END IF
      END IF

 41   NOSCAN=0

      IF(NPLT.GE.1)GO TO 120
      WRITE (LAB,51) IDAY,MONTH(IMON),IYR
 51   FORMAT(I2,1X,A3,1X,I2.2)
      IF(LABLS.EQ.'ALL')THEN
         XP = 0.04
         YP = 0.988
         CSIZ = 12.0
      ELSE IF(LABLS.EQ.'ABR')THEN
         XP = XRT(IWIN)-SIDEX(IWIN)
         YP = YTP(IWIN)+0.015
         CSIZ = 14.0
      END IF      
c      print *,'LABEL: xp,yp,csiz=',xp,yp,csiz
      CALL PLCHMQ (XP,YP, LAB, CSIZ, 0.0, -1.0)

      IF(LABLS.EQ.'ABR')GO TO 60

      IDR=(DRLAB+.00001)*1000
      AZX=90.0-AZROT(ITPLAB)
      IF(AZX.LT.  0.0)THEN
         AZX=AZX+360.0
      ELSE IF(AZX.GT.360.0)THEN
         AZX=AZX-360.0
      END IF
      WRITE (LAB,53) IDR,AZX
 53   FORMAT('GSP=',I4,' M  AZ(+X) =',F5.1,' DG')
      CALL PLCHMQ (.64, .965, LAB, 8.0, 0.0, -1.0)
      WRITE (LAB,54) VNYQ
 54   FORMAT('VNYQ = ',F5.2,' m/s')
      CALL PLCHMQ (.64, .951, LAB, 8.0, 0.0, -1.0)
      AZ1=AZA(1,ISW)
      AZ2=AZA(NANG(ISW),ISW)
      IF(AZ1.LT.0.) AZ1=AZ1+360.
      IF(AZ2.LT.0.) AZ2=AZ2+360.
      WRITE (LAB,55) AZ1,AZ2
 55   FORMAT('ANG=',F6.1,' to ',F6.1)
      CALL PLCHMQ (.64, .979, LAB, 8.0, 0.0, -1.0)
      
      IF(VECTS)GO TO 60

C  Mark max value with an "X"

      IF(ZMAX .NE. -999.) THEN
C         CALL PLCHMQ (FLOAT(MXX)/1024.,FLOAT(MYY)/1024.,
C     +                'X', 12.0, 0.0, 0.0)
         WRITE (LAB, 57) ZMAX,XX,YY
 57      FORMAT('MAX=',F6.1,' (X,Y)=',2F6.1)
         CALL PLCHMQ (.64, .993, LAB, 8.0, 0.0, -1.0)
      END IF

 60   WRITE (LAB, 61) ISCTP(ITPLAB),FXLAB
 61   FORMAT(A3,' FXANG=',F6.1)
      IF(LABLS.EQ.'ALL')THEN
         XP = 0.44
         YP = 0.988
         CSIZ = 12.0
      ELSE IF(LABLS.EQ.'ABR')THEN
         XP = XRT(IWIN)-SIDEX(IWIN)+0.45
         YP = YTP(IWIN)+0.015
         CSIZ = 14.0
      END IF      
      CALL PLCHMQ (XP,YP, LAB, CSIZ, 0.0, -1.0)

      IF(.NOT.PLTSW.AND.ITPOLD.EQ.3.AND.LABLS.EQ.'ALL')THEN
         WRITE (LAB, 64) ZSTR
 64      FORMAT('Height X',F4.1)
         CALL PLCHMQ (.44, .9625, LAB, 10.0, 0.0, -1.0)
      END IF

      IF(PLTSW)THEN
         WRITE (LAB, 65)FXANG1,FXANG2
 65      FORMAT('SWATH ',F5.1,'-',F5.1,' DG')
         CALL PLCHMQ (.44, .9625, LAB, 10.0, 0.0, -1.0)
      END IF

      IHR=ITIME/10000
      IMN=(ITIME-IHR*10000)/100
      ISEC=ITIME-IHR*10000-IMN*100
      WRITE (LAB, 71) IHR,IMN,ISEC
 71   FORMAT(2(I2.2,':'),I2.2)
      IF(LABLS.EQ.'ALL')THEN
         XP = 0.3
         YP = 0.988
         CSIZ = 12.0
      ELSE IF(LABLS.EQ.'ABR')THEN
         XP = XRT(IWIN)-SIDEX(IWIN)+0.275
         YP = YTP(IWIN)+0.015
         CSIZ = 14.0
      END IF      
c      print *,'LABEL: xp,yp,csiz=',xp,yp,csiz
      CALL PLCHMQ (XP,YP, LAB,CSIZ, 0.0, -1.0)
      IF(LABLS.EQ.'ALL')THEN
         XP = 0.19
         YP = 0.988
         CSIZ = 12.0
      ELSE IF(LABLS.EQ.'ABR')THEN
         XP = XRT(IWIN)-SIDEX(IWIN)+0.15
         YP = YTP(IWIN)+0.015
         CSIZ = 14.0
      END IF      
c      print *,'LABEL: xp,yp,csiz=',xp,yp,csiz

C     IFTIME  - Beginning time of scan
C     ITIME1  - Beginning time of the swath
C     ITIME   - Ending time of scan or swath
C
c      print *,'LABEL - scan: iftime,itime=',iftime,itime
c      print *,'LABEL - swth: itime1,itime=',itime1,itime

      IF(IFLD(IFL).LE.-1)THEN
         CALL LABTIM(ITIME1,XP,YP,CSIZ)
      ELSE
         CALL LABTIM(IFTIME,XP,YP,CSIZ)
      END IF

      IF(LABLS.EQ.'ABR')GO TO 112

      WRITE (LAB, 101) NETWORK,ICORD,IRATYP
  101 FORMAT(A8,' Origin at ',A8,' Radar=',A8)
      CALL PLCHMQ (.04, .9625, LAB, 10.0, 0.0, -1.0)

      X1=FL
      Y1=FB-0.060
      WRITE (LABG, 103) ITERGT
  103 FORMAT('PLOT',I3,' GTS')
      CALL PLCHMQ (X1, Y1, LABG,10.0, 0.0, -1.0)

      Y1=FB-0.080
      IF(IGATE(IFL).LE.1) THEN
         CALL PLCHMQ(X1,Y1,'NOT SMOOTHED   ',10.0,0.0,-1.0)
      ELSE
         IFLTYP=IFILTER(IFL)
         IF(FLSPAC(IFL).EQ.'RADR')THEN
            WRITE(LABG,105)FLSPAC(IFL),FILT(IFLTYP),DXX(IFL),DYY(IFL)
  105       FORMAT(A4,'-',A3,':',F5.2,' GTS BY',F5.2,' BMS')
            CALL PLCHMQ(X1,Y1,LABG,10.0,0.0,-1.0)
         ELSE IF(FLSPAC(IFL).EQ.'CART')THEN
            WRITE(LABG,107)FLSPAC(IFL),FILT(IFLTYP),DXX(IFL),DYY(IFL)
  107       FORMAT(A4,'-',A3,':',F5.2,' KM BY',F5.2,' KM')
            CALL PLCHMQ(X1,Y1,LABG,10.0,0.0,-1.0)
         ELSE
            WRITE(LABG,109)FLSPAC(IFL),FILT(IFLTYP),IGATE(IFL)
  109       FORMAT(A4,'-',A3,':',I3,' GTS')
            CALL PLCHMQ(X1,Y1,LABG,10.0, 0.0,-1.0)
         END IF
      END IF

  112 X1=FL-0.065
c      Y1=FB-0.065
      Y1=FB-0.050
      IF(IBSCAN.EQ.1)ITPLAB=4
c      IF(LABLS.EQ.'NON')GOTO 120
      IF(ITPLAB.EQ.3)THEN
         CALL PLCHMQ (FMDX,Y1,'RANGE (KM)',12.0,0.0,0.0)
         CALL PLCHMQ (X1,FMDY,'HEIGHT (KM)',12.0,90.0,0.0)
      ELSE IF(ITPLAB.EQ.4)THEN
         CALL PLCHMQ (FMDX,Y1,'TIME (SEC)',12.0,0.0,0.0)
         CALL PLCHMQ (X1,FMDY,'RANGE (KM)',12.0,90.0,0.0)
      ELSE
         CALL PLCHMQ (FMDX,Y1,'X-DISTANCE (KM)',12.0,0.0,0.0)
         CALL PLCHMQ (X1,FMDY,'Y-DISTANCE (KM)',12.0,90.0,0.0)
      END IF
  120 NPLT=NPLT+1
      IF(IOV.EQ.0)CALL MYFRAME(NFRAME,NPLT,FB,LABLS)
      CALL SET (FL,FR,FB,FT,UL,UR,UB,UT,LLL)
      CALL SETUSV('LW',1000)

C  RESET FIELD LABEL POSITIONING COUNTER AND OFFSET
C
      IF(IOV.EQ.0)THEN
         JOV=0
      END IF

      RETURN
c
c-----ENTRY POINT FOR PRINTOUT-----------------------------------------X
c
      ENTRY LABELPR(ZSTR,COLRFIL,PLTSW,VECTS,NFRAME,LABLS,IGRPLT,
     X     BGFLAG)

C  SETUP LABELING PARAMETERS:  ORDINARY SCAN OR SWATH
C
      IF(PLTSW)THEN
         DRLAB=DRSW
         FXLAB=FXSWA
         ITPLAB=ITPSWA
         ISWLAB=-99
         ISW=2
         NGTLAB=NGTSOLD
      ELSE
         DRLAB=DROLD
         FXLAB=FXOLD
         ITPLAB=ITPOLD
         ISWLAB=ISWPOLD
         NGTLAB=NGTSOLD
         ISW=1
      END IF
      IHR1= IFTIME/10000
      IMN1=(IFTIME-IHR1*10000)/100
      ISC1= IFTIME-IHR1*10000-IMN1*100
      IHR2= ITIME/10000
      IMN2=(ITIME-IHR2*10000)/100
      ISC2= ITIME-IHR2*10000-IMN2*100
      IDR=(DRLAB+.00001)*1000
      AZ1=AZA(1,ISW)

      IF(NANG(ISW).GT.0)THEN
         AZ2=AZA(NANG(ISW),ISW)
      ELSE
         AZ2=AZ1
      END IF

      IF(AZ1.LT.0.) AZ1=AZ1+360.
      IF(AZ2.LT.0.) AZ2=AZ2+360.

C     NTANG     - TOTAL NUMBER OF BEAMS, INCLUDING TRANSITION
C     NBAD      - NUMBER OF BEAMS OUTSIDE
C                 ANGLE TOLERANCE = ABS(NOMINAL - ACTUAL)
C     NANG(ISW) - NUMBER OF GOOD BEAMS
C     NTRAN     - NUMBER OF BEAMS IN TRANSITION
C
c-----write(*,*)'labelpr: ',iyr,month(imon),iday
      NTRAN=NTANG-NANG(ISW)-NBAD
      IF(NANG(ISW).GT.MNANG)THEN
         WRITE(6,123)IBTIME,ISCTP(ITPLAB),IDAY,MONTH(IMON),IYR,
     +        IFTIME,ITIME,FXLAB,NTANG,NANG(ISW),NBAD+NTRAN,
     +        AZ1,AZ2,AVGI,NGTLAB,IDR,VNYQ,ISWLAB
 123     FORMAT(1X,I6.6,': ',A3,'-',I2,A3,I2.2,1X,I6.6,'-',I6.6,': Fx=',
     +        F5.1,' Na=',I4,' Ng=',I4,' Nb=',I3,' fr ',F6.1,
     +        ' to ',F6.1,' da=',F5.2,' Nr=',I4,' dr=',I4,'m Nyq=',F5.2,
     +        ' Sw=',I3)
      ELSE
         WRITE(6,125)ISCTP(ITPLAB),IDAY,MONTH(IMON),IYR,
     +        IFTIME,ITIME,FXLAB,NTANG,NANG(ISW),NBAD+NTRAN,
     +        AZ1,AZ2,AVGI,NGTLAB,IDR,VNYQ,ISWLAB
 125     FORMAT('*SKIPIT: ',A3,'-',I2,A3,I2.2,1X,I6.6,'-',I6.6,': Fx=',
     +        F5.1,' Na=',I4,' Ng=',I4,' Nb=',I3,' fr ',F6.1,
     +        ' to ',F6.1,' da=',F5.2,' Nr=',I4,' dr=',I4,'m Nyq=',F5.2,
     +        ' Sw=',I3)
      END IF

C     Compute the average elevation angle
C
      cnt_elev=0.0
      sum_elev=0.0
      DO J=1,NANG(ISW)
         
C     RHI scan (ITPOLD=3) ==> Elevation angle is in AZA array
C     
         IF(ITPOLD.EQ.3)THEN
            ELRAD=AZA(J,ISW)
         ELSE
            ELRAD=ELA(J,ISW)
         END IF
         cnt_elev=cnt_elev+1.0
         sum_elev=sum_elev+elrad
      end do
      if(cnt_elev.gt.0.0)then
         avg_elev=sum_elev/cnt_elev
         scl_elev=avg_elev*8.0*4096.0/180.0
         write (6,*)'EOS: Elev - cnt,avg,scl=',
     +        cnt_elev,avg_elev,scl_elev
      else
         write (6,*)'EOS: cnt_elev is 0'
      end if


      RETURN
c
c-----ENTRY POINT FOR OUTPUT TO FILE-----------------------------------X
c
      ENTRY LABELFL(PLTSW)

C  SETUP LABELING PARAMETERS:  ORDINARY SCAN OR SWATH
C
      print *,'Inside LABEL at LABELFL entry'
      IF(PLTSW)THEN
         DRLAB=DRSW
         FXLAB=FXSWA
         ITPLAB=ITPSWA
         ISWLAB=-99
         ISW=2
         NGTLAB=NGTSOLD
         IHR1= ITIME1/10000
         IMN1=(ITIME1-IHR1*10000)/100
         ISC1= ITIME1-IHR1*10000-IMN1*100
      ELSE
         DRLAB=DROLD
         FXLAB=FXOLD
         ITPLAB=ITPOLD
         ISWLAB=ISWPOLD
         NGTLAB=NGTSOLD
         ISW=1
         IHR1= IFTIME/10000
         IMN1=(IFTIME-IHR1*10000)/100
         ISC1= IFTIME-IHR1*10000-IMN1*100
      END IF
      IHR2= ITIME/10000
      IMN2=(ITIME-IHR2*10000)/100
      ISC2= ITIME-IHR2*10000-IMN2*100
      IDR=(DRLAB+.00001)*1000

      AZ1=AZA(1,ISW)
      AZ2=AZA(NANG(ISW),ISW)
      IF(AZ1.LT.0.) AZ1=AZ1+360.
      IF(AZ2.LT.0.) AZ2=AZ2+360.

C     NTANG     - TOTAL NUMBER OF BEAMS, INCLUDING TRANSITION
C     NBAD      - NUMBER OF BEAMS OUTSIDE
C                 ANGLE TOLERANCE = ABS(NOMINAL - ACTUAL)
C     NANG(ISW) - NUMBER OF GOOD BEAMS
C     NTRAN     - NUMBER OF BEAMS IN TRANSITION
C
      write(*,*)'labelfl: ',iyr,month(imon),iday
      NTRAN=NTANG-NANG(ISW)-NBAD
      print *,'labelfl: isw,nang(isw),mnang=',isw,nang(isw),mnang
      IF(NANG(ISW).GT.MNANG)THEN
         WRITE(99,123)IBTIME,ISCTP(ITPLAB),IDAY,MONTH(IMON),IYR,
     +        IFTIME,ITIME,FXLAB,NTANG,NANG(ISW),NBAD+NTRAN,
     +        AZ1,AZ2,AVGI,NGTLAB,IDR,VNYQ,ISWLAB
      ELSE
         WRITE(99,125)ISCTP(ITPLAB),IDAY,MONTH(IMON),IYR,
     +        IFTIME,ITIME,FXLAB,NTANG,NANG(ISW),NBAD+NTRAN,
     +        AZ1,AZ2,AVGI,NGTLAB,IDR,VNYQ,ISWLAB
      END IF
      print *,'labelfl: about to return'
      RETURN
      END
