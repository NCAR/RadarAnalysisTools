c
c----------------------------------------------------------------------X
c
      SUBROUTINE STORE(IOUT,IIN1,NFUN,C1,C2,NOUT,NSWPAVG,H0)
C
C  ROUTINE TO STORE VALUES - CALLED FROM FUNC FOR:
C
C     (##) Function:      Output = Operation on input
C     ( 3) SWATH:         F(OUT) = MAX F(IN)
C          SWATH(ANGLE):  F(OUTs)= NIN2(1:4) + (amax and angl)
C          SWATH(HEIGHT): F(OUTs)= NIN2(1:4) + (zmax and zalt)
C          SWATH(STATS):  F(OUTs)= NIN2(1:4) + (min, max, mean, sdev, and npts)
C     ( 4) ISOCHRON:      F(OUT) = TIME OF MAX F(IN)
C     ( 8) INTEGR:        F(OUT) = TIME INTEGRATION (AVERAGE) OF F(IN)
C     (64) AVRAGE:        F(OUT) = AVERAGE THROUGH CURRENT SWEEP OF F(IN)
C     (65) GRID:          F(OUT) = F(IN)
C     
C     LOOP OVER THE REGULAR RANGE-ANGLE GRID AND FIND THE
C     MEASUREMENT RANGE-ANGLE LOCATION TO BE USED.
C     
C     IFTIME,ITIME   - STARTING, ENDING HHMMSS OF THE CURRENT SWEEP
C     IFTBEG         - STARTING            "    "  "  INTEGRATION
C     ITIME1,ITIME2  -     "        "      "    "  "  INTEGRATION
C     FXANG1,FXANG2  - STARTING, ENDING FIXED ANGLES FOR SWATH OR INTEGRATION
C
C     ISW         - (1) Original radar scan positions 
C                   (2) Regular range-angle grid positions
C
C     AZA,ELA     - AZIMUTH AND ELEVATION ANGLES OF RADAR BEAMS
C     ITPOLD      - SCANNING MODE [RHI (3): AZA CONTAINS ELEVATION ANGLE;
C                                           ELA     "     AZIMUTH    "  ]
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      INCLUDE 'swth.inc'

      COMMON /INPUTCH/NAMFLD(MXF),IRATYP,ICORD
      CHARACTER*8 NAMFLD,IRATYP,ICORD
      CHARACTER*6 CTIM1,CTIM2,CTIM3,CTIM4
      COMMON/ANGLS/ASCAN(MXA),ANGINC(MXA),FXELC(MXA),FXERR(MXA),AVGI
      DIMENSION PREVDAT(MXR,MXA),FMINS(MXA)
      DATA DMAX/45./
      DATA RE,REI/17000.0,1.17647E-04/
      DATA TORAD,TODEG/0.017453293,57.29577951/

      CHARACTER*8 NFUN,NOUT

C     To prevent radial streaks, code should allow regular grid angle spacing
C     to be at least the average spacing or somewhat farther away.  Otherwise,
C     all interior angles may not get filled with swath'd data.
C
c-----debug (ljm)
c      write(6,1769)nfun,nout,iout,iin1,namfld(iout),namfld(iin1),
c     +     nint(c1),nint(c2)
c 1769 format(' STORE: ',2a8,2i8,2a8,2i8)
c-----debug (ljm)
      IF(ABS(AVGI).NE.0.0)THEN
         AZCLO_MX=AMAX1(AZCLO_MX1,ABS(AVGI))
      ELSE
         AZCLO_MX=AZCLO_MX1
      END IF
c-----debug (ljm)
c      write(6,1770)abs(avgi),azclo_mx1,azclo_mx,itpold
c 1770 format(1x,'STORE: avgi,azclo_mx=',3f8.2,i8)
c-----debug (ljm)

C     GENERATE STARTING TIME (HH,MM,SS,MIN) FOR SWATH AND ISOCHRON
C     SET STARTING TIME FOR CURRENT INTEGRATION INTERVAL.  CLEAR
C     PREVIOUS DATA AND CURRENT DATA ARRAYS WHEN INTEGRATING.
C
      IF(MSCAN.EQ.1.OR.MOD(MSCAN,NSWPAVG).EQ.1)THEN

         IFTBEG=IFTIME
         ITIME1=IFTIME
         ITIME2=ITIME
         ITOLD=ITIME2
         IHR1= ITIME1/10000
         IMN1=(ITIME1-IHR1*10000)/100
         ISC1= ITIME1-IHR1*10000-IMN1*100
         FMIN1=FLOAT(IHR1*60+IMN1)+FLOAT(ISC1)/60.0
         IHR2= ITOLD/10000
         IMN2=(ITOLD-IHR2*10000)/100
         ISC2= ITOLD-IHR2*10000-IMN2*100
         FMIN2=FLOAT(IHR2*60+IMN2)+FLOAT(ISC2)/60.0
         FXANG1=FXOLD
         FXANG2=FXOLD
         MSCANOLD=MSCAN

         IF(NFUN.EQ.'INTEGR  ')THEN
            DO J=1,NANG(2)
               DO I=1,MXR
                  PREVDAT(I,J)=BDVAL
                  DAT(I,J,IOUT)=BDVAL
               END DO
            END DO
         END IF

      END IF
c-----print *,'STORE:   mscan,mscanold,nswpavg=',mscan,mscanold,nswpavg
c-----print *,'STORE:   itim1,iftim,itim,itim2=',
c----+     itime1,iftime,itime,itime2
c-----print *,'STORE:      fxang1,fxang,fxang2=',fxang1,fxang,fxang2

C     DETERMINE IF INPUT FIELD IS A SWATH'd FIELD or OTHERWISE
C        ISW - (2) Regular range-angle grid positions
C              (1) Original radar scan positions
C
      IF(IFLD(IIN1).LT.0)THEN
         ISW=2
         MANG=NANG(2)
         DR_STORE=DRSW
      ELSE
         ISW=1
         MANG=NANG(1)
         DR_STORE=DROLD
      END IF
      MN=1
      MX=NRSW

C     Horizontally project range gate spacing - see UFREC for all
C     scanning modes.  ITPOLD: (1) Sector, (3) RHI, and (8) Surv
C
      SINE=SIN(FXOLD*TORAD)
      COSE=COS(FXOLD*TORAD)
c      IF(ITPOLD.EQ.3)THEN
         DRINV=1.0/DR_STORE
c      ELSE
c         DRINV=COSE/DR_STORE
c      END IF
c-----print *,'STORE:   mn,mx,drsw,drold,drinv=',mn,mx,drsw,drold,drinv
      
C  COMPUTE INTEGRATION TIME INTERVALS IN MINUTES 
C
C     FMIN1 - MINUTES FROM BEGINNING TIME OF INTEGRATION
C             CONVERT FIRST SWEEP BEGINNING TIME (IFTIME - HHMMSS)
C     FMIN  - MINUTES FOR CURRENT INTEGRATION ENDING TIME 
C             CONVERT CURRENT SWEEP ENDING TIME (ITIME - HHMMSS)
C     FMIN2 - MINUTES FOR CURRENT INTEGRATION BEGINNING TIME
C             CONVERT PREVIOUS SWEEP ENDING TIME (ITOLD - HHMMSS)
C     DMIN1 - TOTAL TIME FROM THE BEGINNING OF THE INTEGRATION 
C     DMIN2 - CURRENT INTEGRATION INTERVAL (CURRENT END - PREVIOUS END)
C
      IHR= ITIME/10000
      IMN=(ITIME-IHR*10000)/100
      ISC= ITIME-IHR*10000-IMN*100
      IHRM=IHR
      IF((IHRM-IHR1).LT.0)IHRM=IHRM+24
      FMIN=FLOAT(IHRM*60+IMN)+FLOAT(ISC)/60.0
C      DMIN1=12.0*(FMIN-FMIN1)
      DMIN1=FMIN-FMIN1

      IF(MSCAN.EQ.1)DMIN2=DMIN1
      IF(MSCAN.NE.MSCANOLD)THEN
         IHR2= ITOLD/10000
         IMN2=(ITOLD-IHR2*10000)/100
         ISC2= ITOLD-IHR2*10000-IMN2*100
         FMIN2=FLOAT(IHR2*60+IMN2)+FLOAT(ISC2)/60.0
         IHRM=IHR
         IF((IHRM-IHR2).LT.0)IHRM=IHRM+24
         FMIN=FLOAT(IHRM*60+IMN)+FLOAT(ISC)/60.0
C         DMIN2=12.0*(FMIN-FMIN2)
         DMIN2=FMIN-FMIN2
         FXANG2=FXOLD
         ITIME2=ITIME
      END IF

c-----debug (ljm)
c      write(ctim1,1771)iftime
c      write(ctim2,1771)itime
c      write(ctim3,1771)itime1
c      write(ctim4,1771)itime2
c 1771 format(i6)
c      do n=1,6
c         if(ctim1(n:n).eq.' ')ctim1(n:n)='0'
c         if(ctim2(n:n).eq.' ')ctim2(n:n)='0'
c         if(ctim3(n:n).eq.' ')ctim3(n:n)='0'
c         if(ctim4(n:n).eq.' ')ctim4(n:n)='0'
c      end do
c      write(6,1772)nfun,mscan,mscanold,ctim1,ctim2,ctim3,
c     +     ctim4,fmin1,fmin2,fmin,dmin2,dmin1,fxang1,fxang2
c 1772 FORMAT(7x,a7,2i4,1x,'cur='a6,'-',a6,' tot=',a6,'-',a6,7f10.4)
c-----debug (ljm)

C     LOOP OVER THE REGULAR (RANGE,ANGLE - I,J) GRID LOCATIONS 
C     AND USE THE PROPER (IDX,JDX) MEASUREMENT LOCATION.
C     Note: Put both scan angle and regular grid angle into
C           range 0-360 when computing angle indices.
C
c-----debug (ljm)
C      IF(NOUT.EQ.'STATS   ')THEN
C         do n=2,5
C            niout=iout+n-1
C            write(6,1773)nout,namfld(niout),namfld(iin1),
C     +           niout,iin1,mscan,nswpavg
C 1773       format(16x,3a8,4i8)
C         end do
C      END IF
C      IF(NOUT.EQ.'ANGLE   ' .OR. NOUT.EQ.'HEIGHT  ')THEN
C         niout=iout-1
C         write(6,1774)nout,namfld(niout),namfld(iin1),
C     +        niout,iin1,mscan,nswpavg
C 1774    format(16x,3a8,4i8)
C      END IF
c-----debug (ljm)

C     Generate statistics on angular increments 
C     between original and gridded data locations.
C
      AZMN= 9999.0
      AZMX=-9999.0
      SUMAZCLO=0.0
      NUMAZCLO=0

C     Generate fixed angle to be stored for 'ANGLE'
C
c      IF(FXOLD.GE.100.0)THEN
c         FXSTORE=FXOLD-100.0*INT(FXOLD/100.0)
c      ELSE
         FXSTORE=FXOLD
c      END IF

C     When gridding an input field to the regular range-angle 
C     grid, clear the output storage array before proceeding.
C
      IF(NFUN.EQ.'GRID    ')THEN
         DO J=1,NANG(2)
            DO I=1,MXR
               DAT(I,J,IOUT)=BDVAL
            END DO
         END DO
      END IF

C     Loop over regularly-spaced output angles [AZA(J,2)] and ranges [RNG(I,2)]
C     and find closest beam and range indices (JDX and IDX) where output data 
C     will be stored.  Put both output and input angles in the range 0-360.
C
      DO 200 J=1,NANG(2)
         AZCLO=1000.
         JDX=0
         IF(ITPOLD.NE.3 .AND. AZA(J,2).LT.0.0)THEN
            ANGLE_GRD = AZA(J,2) + 360.0
         ELSE
            ANGLE_GRD = AZA(J,2)
         END IF            
            
         DO 110 J1=1,MANG
            IF(ITPOLD.NE.3 .AND. AZA(J1,ISW).LT.0.0)THEN
               ANGLE_DAT = AZA(J1,ISW) + 360.0
            ELSE
               ANGLE_DAT = AZA(J1,ISW)
            END IF          
            ANGLE_DIF = ABS(ANGLE_DAT-ANGLE_GRD)
            IF(ANGLE_DIF .LT. AZCLO)THEN
               JDX = J1
               AZCLO = ANGLE_DIF
            END IF
 110     CONTINUE

         IF(MSCAN.EQ.1)FMINS(J)=FMIN
         ELA(J,2)=ELA(JDX,1)
         ITM(J,2)=ITM(JDX,1)

c--------debug (ljm)
c         if(itpold.ne.3 .and. aza(jdx,isw).lt.0.0)then
c            angle_dat = aza(jdx,isw) + 360.0
c         else
c            angle_dat = aza(jdx,isw)
c         end if          
c         if(azclo.le.azclo_mx)then
c            write(6,1775)j,angle_grd,jdx,angle_dat,azclo
c 1775       format(' azclo: ',i4,f8.2,i4,f8.2,f8.2)
c         else
c            write(6,1776)j,angle_grd,jdx,angle_dat,azclo
c 1776       format('*azclo: ',i4,f8.2,i4,f8.2,f8.2)
c         end if
c--------debug (ljm)

         IF(JDX.EQ.0)GO TO 200
         IF(AZCLO.GT.AZCLO_MX)GO TO 200
         IF(AZCLO.LT.AZMN)AZMN=AZCLO
         IF(AZCLO.GT.AZMX)AZMX=AZCLO
         SUMAZCLO=SUMAZCLO+AZCLO
         NUMAZCLO=NUMAZCLO+1
         DMIN2=ABS(FMIN-FMINS(J))

         IF(NFUN.EQ.'SWATH   ')THEN

C           Store either statistical quantities,  maximum value, 
C           or maximum value and angle of maximum value.

            IF(NOUT.EQ.'STATS   ')THEN

C              Store quantitites for statistics - max,min,mean,sdev,npts

               DO 112 I=MN,MX
                  IDX=0
                  IDX=NINT(1.0+DRINV*(RNG(I,2)-R0))
                  IF(IDX.LE.0)GO TO 112
                  IF(DAT(IDX,JDX,IIN1) .EQ. BDVAL)GO TO 112

C              Maximum (max) and minimum (min) values

                  DATMAX=AMAX1(DAT(I,J,IOUT),  DAT(IDX,JDX,IIN1))
                  DATMIN=AMIN1(DAT(I,J,IOUT+1),DAT(IDX,JDX,IIN1))
                  DAT(I,J,IOUT  )=DATMAX
                  DAT(I,J,IOUT+1)=DATMIN

C              Accumulators for mean and mean-squared values

                  DATMP=DAT(IDX,JDX,IIN1)
                  IF(DAT(I,J,IOUT+2) .EQ. BDVAL)THEN
                     DAT(I,J,IOUT+2)=DATMP
                     DAT(I,J,IOUT+3)=DATMP**2
                     DAT(I,J,IOUT+4)=1.0
                  ELSE
                     DAT(I,J,IOUT+2)=DAT(I,J,IOUT+2)+DATMP
                     DAT(I,J,IOUT+3)=DAT(I,J,IOUT+3)+DATMP**2
                     DAT(I,J,IOUT+4)=DAT(I,J,IOUT+4)+1.0
                  END IF

 112           CONTINUE

            ELSE IF(NOUT.EQ.'ANGLE   ')THEN

C              Store the maximum value and the fixed angle where it 
C              occurs provided the field value is between C1 and C2.

               DO 114 I=MN,MX
                  IDX=0
                  IDX=NINT(1.0+DRINV*(RNG(I,2)-R0))
                  IF(IDX.LE.0)GO TO 114
                  IF(DAT(IDX,JDX,IIN1).EQ.BDVAL)GO TO 114
                  IF(DAT(IDX,JDX,IIN1).GT.DAT(I,J,IOUT).AND.
     X              (DAT(IDX,JDX,IIN1).GE.C1 .AND.
     X               DAT(IDX,JDX,IIN1).LE.C2))THEN
                     DAT(I,J,IOUT)=DAT(IDX,JDX,IIN1)
                     DAT(I,J,IOUT+1)=FXSTORE
                  END IF
 114           CONTINUE

            ELSE IF(NOUT.EQ.'HEIGHT  ')THEN

C              Store the maximum value and the height where it 
C              occurs provided the field value is between C1 and C2.

               DO 116 I=MN,MX
                  IDX=0
                  IDX=NINT(1.0+DRINV*(RNG(I,2)-R0))
                  IF(IDX.LE.0)GO TO 116
                  IF(DAT(IDX,JDX,IIN1).EQ.BDVAL)GO TO 116
                  IF(DAT(IDX,JDX,IIN1).GT.DAT(I,J,IOUT).AND.
     X              (DAT(IDX,JDX,IIN1).GE.C1 .AND.
     X               DAT(IDX,JDX,IIN1).LE.C2))THEN
                     DAT(I,J,IOUT)=DAT(IDX,JDX,IIN1)
                     HRNG=RNG(IDX,ISW)*COSE
                     Z=H0+RNG(IDX,ISW)*SINE+0.5*HRNG*HRNG*REI
                     DAT(I,J,IOUT+1)=Z
                  END IF
 116           CONTINUE

            ELSE

C              Store the maximum value in the output array

               DO 120 I=MN,MX
                  IDX=0
                  IDX=NINT(1.0+DRINV*(RNG(I,2)-R0))
                  IF(IDX.LE.0)GO TO 120
                  IF(DAT(IDX,JDX,IIN1).EQ.BDVAL)GO TO 120
                  DATMP=DAT(I,J,IOUT)
                  DAT(I,J,IOUT)=AMAX1(DATMP,DAT(IDX,JDX,IIN1))
c                  print *,'STORE: ',i,idx,dat(idx,jdx,iin1),datmp,
c     +                 dat(i,j,iout)
 120           CONTINUE
            END IF

         ELSE IF(NFUN.EQ.'ISOCHRN ')THEN

C           Store the time of first occurrence in the output array

            DO 130 I=MN,MX
               IDX=0
               IDX=NINT(1.0+DRINV*(RNG(I,2)-R0))
               IF(IDX.LE.0)GO TO 130
               IF(DAT(IDX,JDX,IIN1).EQ.BDVAL)GO TO 130
               IF(DAT(I,J,IOUT).EQ.BDVAL.AND.
     X            DAT(IDX,JDX,IIN1).GE.C1.AND.
     X            DAT(IDX,JDX,IIN1).LE.C2)DAT(I,J,IOUT)=DMIN1
  130       CONTINUE

         ELSE IF(NFUN.EQ.'INTEGR  ')THEN

C           Store the time integral of measurements in the output arrays

            DO 140 I=MN,MX
               IDX=0
               IDX=NINT(1.0+DRINV*(RNG(I,2)-R0))
               IF(IDX.LE.0)GO TO 140
               IF(MSCAN.EQ.1)THEN
                  PREVDAT(I,J)=DAT(IDX,JDX,IIN1)
               ELSE
                  IF(DAT(IDX,JDX,IIN1).EQ.BDVAL.OR.
     X             PREVDAT(I,J).EQ.BDVAL.OR.
     X             DMIN2.GT.DMAX)THEN
                     PREVDAT(I,J)=DAT(IDX,JDX,IIN1)
                  ELSE
                     IF(DAT(I,J,IOUT).NE.BDVAL)THEN
                        DAT(I,J,IOUT)=DAT(I,J,IOUT)+DMIN2*
     X                    (0.5*DAT(IDX,JDX,IIN1)+0.5*PREVDAT(I,J))
                        DAT(I,J,IOUT+1)=DAT(I,J,IOUT+1)+DMIN2
                     ELSE
                        DAT(I,J,IOUT)=DMIN2*(0.5*DAT(IDX,JDX,IIN1)
     X                                +0.5*PREVDAT(I,J))
                        DAT(I,J,IOUT+1)=DMIN2
                     END IF
                     PREVDAT(I,J)=DAT(IDX,JDX,IIN1)
                  END IF
               END IF
  140       CONTINUE
            FMINS(J)=FMIN

         ELSE IF(NFUN.EQ.'AVRAGE  ')THEN

C           Store the running sum and number of values in the output arrays

            DO 150 I=MN,MX
               IDX=0
               IDX=NINT(1.0+DRINV*(RNG(I,2)-R0))
               IF(IDX.LE.0)GO TO 150
               IF(DAT(IDX,JDX,IIN1).EQ.BDVAL)GO TO 150

C              Accumulators for average value

                  DATMP=DAT(IDX,JDX,IIN1)
                  IF(DAT(I,J,IOUT) .EQ. BDVAL)THEN
                     DAT(I,J,IOUT  )=DATMP
                     DAT(I,J,IOUT+1)=1.0
                  ELSE
                     DAT(I,J,IOUT  )=DAT(I,J,IOUT  )+DATMP
                     DAT(I,J,IOUT+1)=DAT(I,J,IOUT+1)+1.0
                  END IF

  150       CONTINUE

         ELSE IF(NFUN.EQ.'GRID    ')THEN

C           Store a closest-point gridded value in the output array

            DO 160 I=MN,MX
               IDX=0
               IDX=NINT(1.0+DRINV*(RNG(I,2)-R0))
               DAT(I,J,IOUT)=BDVAL
               IF(IDX.LE.0)GO TO 160
               DAT(I,J,IOUT)=DAT(IDX,JDX,IIN1)
  160       CONTINUE
         END IF
  200 CONTINUE

C     Output statistics on angular increments 
C     between original and gridded data locations.
C
      IF(NUMAZCLO.GT.0)SUMAZCLO=SUMAZCLO/NUMAZCLO

c-----debug (ljm)
c      write(6,1779)azmn,azmx,numazclo,sumazclo
c 1779 format(1x,'STORE:  azclo-mn-mx-n-avg=',2f8.2,i8,f8.2)
c-----debug (ljm)

      ITPSWA=ITPOLD
      FXSWA=FXOLD

C     SET TIMES AND ANGLES BEFORE READING NEXT SCAN
C
      IF(MSCAN.NE.MSCANOLD)THEN
         ITOLD=ITIME2
         MSCANOLD=MSCAN
      END IF

      RETURN
      END
