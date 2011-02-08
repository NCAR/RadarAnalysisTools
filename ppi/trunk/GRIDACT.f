c
c----------------------------------------------------------------------X
c
      SUBROUTINE GRIDACT(DAT,IOUT,IIN1,NIN1,NIN2,C1,BDVAL,MXR,MXA,MXF,
     X     MNGATE,MXGATE,MANG,AZA,ELA,R0,DROLD,FXOLD,ITPOLD,X0,Y0,H0,
     X     XACT,YACT,ZACT,UACT,VACT,WACT,CACT,QACT,DACT,TACT,DTAC,
     X     IACT,MXL,IBTIME,IETIME)
C
C  Grid aircraft measurements to nearest radar (R, A, E) location.
C     Note: When the scan is an RHI, the position is projected into the
C           plane of the azimuth (FXOLD) angle being scanned.  For that
C           case "X" is range and the "Y" is height.
C     Note: Plot aircraft track from IBTIME-ABS(DTAC) to IETIME+ABS(DTAC)
C           when plotting onto projection (PLTPROJ).
C
C     NIN2                 - 'RADR' - extract a radar field at the aircraft 
C                                     position
C                            'ACFT' - grid an aircraft field to the nearest 
C                                     radar pulse-volume position
C     NIN1                 - Name of radar (aircraft) field to be extracted
C     ACFIELD              - Names of possible aircraft fields: 
C                            'CONfssp','LWCfssp','DBZfssp'
C     IIN1                 - Index of the radar field to be extracted
C     DA (C1)              - All aircraft positions within DA to either
C                            side of the current radar scan are gridded
C                            to the nearest radar (r,a,e) position.
C                            and aircraft position for gridding.
C     XACT,YACT,ZACT,TACT  - AIRCRAFT POSITION (KM,KM,KM,SEC)
C     UACT,VACT,WACT       - (U,V,W)
C     CACT,QACT,DACT       - FSSP CONC, LWC, and DBZ (#/cc, g/m3, dBZ)
C     IACT                 - NUMBER OF AIRCRAFT POSITIONS
C     DTAC                 - TIME WINDOW (RADAR CENTRAL TIME +/- DTAC SEC)
C     IBTIME,IETIME        - TIME  BOUNDS OF THE RADAR SCAN (HHMMSS)
C     X0,Y0,HO             - RADAR POSITION (KM,KM,KM)
C     FXOLD                - FIXED ANGLE (ELEVATION OR AZIMUTH) OF SCAN 
C     ITPOLD               - SCAN MODE [PPI(1), COP(2), RHI(3), or SUR(8)]
C
      CHARACTER*8 NIN1,NIN2,ACFIELD(3)
      CHARACTER*9 LABL
      LOGICAL DECR
      REAL FIRST,LAST

      DIMENSION DAT(MXR,MXA,MXF),AZA(MXA,2),ELA(MXA,2)
      DIMENSION XACT(MXL),YACT(MXL),ZACT(MXL),TACT(MXL)
      DIMENSION UACT(MXL),VACT(MXL),WACT(MXL)
      DIMENSION CACT(MXL),QACT(MXL),DACT(MXL)
      DATA TORAD,TODEG/0.017453293,57.29577951/
      DATA ACFIELD/'CONfssp','LWCfssp','DBZfssp'/
      DATA CONCFMIN,DIFFMIN/50.0,10.0/

C     Don't grid if the aircraft time segment [TACT(1) to TACT(IACT)]
C     is completely outside the gridding time segment [SECMN to SECMX]. 
C
      IHRB= IBTIME/10000
      IMNB=(IBTIME-IHRB*10000)/100
      ISCB= IBTIME-IHRB*10000-IMNB*100
      IBSEC=IHRB*3600+IMNB*60+ISCB

      IHRE= IETIME/10000
      IMNE=(IETIME-IHRE*10000)/100
      ISCE= IETIME-IHRE*10000-IMNE*100
      IESEC=IHRE*3600+IMNE*60+ISCE

      IMSEC=0.5*(IBSEC+IESEC)
      IHRM= IMSEC/3600.0
      IMNM= (IMSEC-3600.0*IHRM)/60.0
      ISCM= IMSEC-3600.0*IHRM-60.0*IMNM
      RMSEC=FLOAT(IHRM*3600+IMNM*60+ISCM)

      IF(DTAC.LT.0.0)THEN
         SECMN=FLOAT(IBSEC)-ABS(DTAC)
         SECMX=FLOAT(IESEC)+ABS(DTAC)
      ELSE
         SECMN=RMSEC-DTAC
         SECMX=RMSEC+DTAC
      END IF
      
      IF(SECMX.LE.TACT(1))RETURN
      IF(SECMN.GE.TACT(IACT))RETURN
c      write(*,*)'gacftsec=',tact(1),tact(iact),secmn,secmx

C     Clear the output array
C
      DO J=1,MANG
         DO I=MNGATE,MXGATE
            DAT(I,J,IOUT)=BDVAL
         END DO
      END DO

C     Determine if radar scanning in increasing (decreasing) angle direction
C
      FIRST=AZA(1,1)
      LAST =AZA(MANG,1)
      IF(FIRST.GT.LAST)THEN
         DECR=.TRUE.
      ELSE
         DECR=.FALSE.
      END IF

C     Calculate bounds of radar scan.
C
      RMIN=R0+DROLD*(MNGATE-1)
      RMAX=R0+DROLD*(MXGATE-1)

C     RHI: AZA contains elevation angles scanned.
C
      IF(ITPOLD.EQ.3)THEN

         IF(DECR)THEN
            ELMIN=AZA(MANG,1)
            ELMAX=AZA(1,1)
            IF(ELMIN.GT.180.0)ELMIN=ELMIN-360.0
            IF(ELMAX.GT.180.0)ELMAX=ELMAX-360.0
         ELSE
            ELMIN=AZA(1,1)
            ELMAX=AZA(MANG,1)
            IF(ELMIN.GT.180.0)ELMIN=ELMIN-360.0
            IF(ELMAX.GT.180.0)ELMAX=ELMAX-360.0
         END IF

         DE=(ELMAX-ELMIN)/(MANG-1)
         DA=C1
         AZMIN=FXOLD-DA
         AZMAX=FXOLD+DA

C     Otherwise: AZA contains azimuth angles scanned.
C
      ELSE

         IF(DECR)THEN
            AZMIN=AZA(MANG,1)
            AZMAX=AZA(1,1)
         ELSE
            AZMIN=AZA(1,1)
            AZMAX=AZA(MANG,1)
         END IF

         DA=(AZMAX-AZMIN)/(MANG-1)
         DE=C1
         ELMIN=FXOLD-DE
         ELMAX=FXOLD+DE

      END IF

c      if(NIN2.EQ.'RADR    ')then
c         write(*,*)'nin2,nin1,c1=',nin2,nin1,c1
c         write(*,*)'    range:',rmin,rmax,drold
c         write(*,*)'  azimuth:',azmin,azmax,da
c         write(*,*)'elevation:',elmin,elmax,de
c         write(*,*)'time(sec):',secmn,secmx
c      end if

C     USE VALUES PROJECTED INTO AZIMUTH-PLANE WHEN RHI SCAN MODE.
C
      DO 200 I=1,IACT
         
         TC=TACT(I)
         IF(TC.LT.SECMN .OR. TC.GT.SECMX)GO TO 200
         IHR= TACT(I)/3600.0
         IMN=(TACT(I)-IHR*3600.0)/60.0
         ISC= TACT(I)-IHR*3600.0-IMN*60.0

         X=XACT(I)-X0
         Y=YACT(I)-Y0
         Z=ZACT(I)-H0
         HRNG=SQRT(X*X+Y*Y)
         SRNG=SQRT(X*X+Y*Y+Z*Z)
         IF(X.EQ.0.0 .AND. Y.EQ.0.0)THEN
            AZ=0.0
         ELSE IF(X.GT.0.0 .AND. Y.EQ.0.0)THEN
            AZ=90.0
         ELSE IF(X.EQ.0.0 .AND. Y.LT.0.0)THEN
            AZ=180.0
         ELSE IF(X.LT.0.0 .AND. Y.EQ.0.0)THEN
            AZ=270.0
         ELSE IF(X.NE.0.0 .AND. Y.NE.0.0)THEN
            AZ=TODEG*ATAN2(X,Y)
         ELSE
            AZ=0.0
         END IF
         IF(AZ.LT.0.0)AZ=AZ+360.0
         IF(HRNG.GT.0.0)EL=TODEG*ATAN2(Z,HRNG)
         
C        Check aircraft positions against radar data bounds.
C
         IF(SRNG.LT.RMIN .OR. SRNG.GT.RMAX)GO TO 200
         IF(AZ.LT.AZMIN  .OR. AZ.GT.AZMAX )GO TO 200
         IF(EL.LT.ELMIN  .OR. EL.GT.ELMAX )GO TO 200

         IR=NINT(1.0+(SRNG-R0)/DROLD)
         IF(ITPOLD.EQ.3)THEN
            ACRFT=EL
         ELSE
            ACRFT=AZ
         END IF

         JR=0
         DO 100 J=1,MANG-1

            IF(DECR)THEN
               ANGL1=AZA(J+1,1)
               ANGL2=AZA(J,1)
            ELSE
               ANGL1=AZA(J,1)
               ANGL2=AZA(J+1,1)
            END IF

            IF(ITPOLD.EQ.3)THEN
               IF(ANGL1.GT.180.0)ANGL1=ANGL1-360.0
               IF(ANGL2.GT.180.0)ANGL2=ANGL2-360.0
            ELSE
               IF(ANGL1.LT.0.0)ANGL1=ANGL1+360.0
               IF(ANGL2.LT.0.0)ANGL2=ANGL2+360.0
            END IF
            IF(ANGL2.GE.ANGL1)THEN
               IF(ACRFT.GE.ANGL1.AND.ACRFT.LE.ANGL2)THEN
                  ADIF1=ABS(ACRFT-ANGL1)
                  ADIF2=ABS(ACRFT-ANGL2)
                  IF(ADIF1.LT.ADIF2)THEN
                     JR=J
                  ELSE
                     JR=J+1
                  END IF
                  GO TO 110
               END IF
            ELSE
               IF(ACRFT.GE.ANGL2.AND.ACRFT.LE.ANGL1)THEN
                  ADIF1=ABS(ACRFT-ANGL1)
                  ADIF2=ABS(ACRFT-ANGL2)
                  IF(ADIF1.LT.ADIF2)THEN
                     JR=J
                  ELSE
                     JR=J+1
                  END IF
                  GO TO 110
               END IF
            END IF
 100     CONTINUE
 110     CONTINUE

         IF(IR.GT.0 .AND. JR.GT.0)THEN

            IF(NIN2.EQ.'ACFT    ')THEN
               IF(NIN1.EQ.'CONfssp')THEN
                  ACFT_DAT=CACT(I)
c                  rrg=r0+(ir-1)*drold
c                  write(labl,129)
c 129              format(' +gacft: ')
c                  if(cact(i).ge.concfmin)then
c                     write(6,1769)labl,ihr,imn,isc,fxold,
c     +                    az,srng,el,zact(i),cact(i),qact(i),dact(i)
c 1769                format(a9,3i2.2,'  fx=',f5.1,
c     +                    '    az-rg-el-ht=',4f5.1,
c     +                    '    con-lwc-dbz=',3f7.1)
c                  end if
               ELSE IF(NIN1.EQ.'LWCfssp')THEN
                  ACFT_DAT=QACT(I)
               ELSE IF(NIN1.EQ.'DBZfssp')THEN
                  ACFT_DAT=DACT(I)
               END IF
                  
               IF(DAT(IR,JR,IOUT).EQ.BDVAL)THEN
                  DAT(IR,JR,IOUT)=ACFT_DAT
               ELSE
                  IF(ACFT_DAT.GT.DAT(IR,JR,IOUT))THEN
                     DAT(IR,JR,IOUT)=ACFT_DAT
                  END IF
               END IF
            ELSE IF(NIN2.EQ.'RADR    ')THEN
               IF(DAT(IR,JR,IIN1).GE.DAT(IR,JR,IOUT))THEN
                  DAT(IR,JR,IOUT)=DAT(IR,JR,IIN1)
               END IF
               IF(DAT(IR,JR,IOUT).NE.BDVAL)THEN
                  dbz_diff=dat(ir,jr,iout)-dact(i)
               ELSE
                  dbz_diff=bdval
               END IF

               rrg=r0+(ir-1)*drold
               if(abs(dbz_diff).le.diffmin)then
                  write(labl,131)
 131              format(' *gacft: ')
               else
                  write(labl,133)
 133              format('  gacft: ')
               end if
               if(cact(i).ge.concfmin)then
                  write(6,1770)labl,ihr,imn,isc,fxold,az,srng,el,
     +                 zact(i),cact(i),qact(i),dact(i),dat(ir,jr,iout),
     +                 dbz_diff
 1770             format(a9,3i2.2,'  fx=',f5.1,'    az-rg-el-ht=',4f5.1,
     +                 '    con-lwc-dbz-radar=',4f7.1,'    diff=',f7.1)
               end if

            END IF

         END IF

 200    CONTINUE

      RETURN
      END

