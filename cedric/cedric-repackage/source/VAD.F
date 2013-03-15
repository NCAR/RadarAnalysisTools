c
c----------------------------------------------------------------------X
c
      SUBROUTINE VAD(KVD,VDAT,ZDAT,MXR,MXA,RNG,Rring,AZA,NANG,BAD,
     X     FXOLD,NAMINF,NAMOUF,TYPE,C1,C2,C3,C4,NPRNT,X0,Y0,H0,
     X     VNYQ_VOL)
C
C   FUNCTION - COMPUTE MEAN RADIAL VELOCITY
C     Analysis results are stored in COMMON /VADWINDS/.  Can do as many
C     as MXVD=10 separate VAD analyses in one run, indexed by KVD and
C     input field name.
C
C     Either Fourier series analysis:
C        FOUT(I,J) = A0 + A(1)*COS(Az) + A(2)*COS(2Az)
C                       + B(1)*SIN(Az) + B(2)*SIN(2Az)
C
C     Or linear wind with divergence, deformations, and vorticity.
C        FOUT(I,J) = [u(x,y)*sin(a) + v(x,y)*cos(a)]*cos(e)
C           where
C           u(x,y) = ue + ux*(x-xe) + uy*(y-ye)
C           v(x,y) = ve + vx*(x-xe) + vy*(y-ye)
C           u(x,y) = ue + 0.5*(div+str)*(x-xe) + 0.5*(shr-vor)*(y-ye)
C           v(x,y) = ve + 0.5*(shr+vor)*(x-xe) + 0.5*(div-str)*(y-ye)
C        and
C           div = du/dx + dv/dy = ux + vy
C           str = du/dx - dv/dy = ux - vy
C           shr = du/dy + dv/dx = uy + vx
C           vor = dv/dx - du/dy = vx - uy
C
C     Linear equations to be solved:
C     
C        ue*Aue + ve*Bue + ux*Cue + uy*Due + vx*Eue + vy*Fue = Gue
C        ue*Ave + ve*Bve + ux*Cve + uy*Dve + vx*Eve + vy*Fve = Gve
C        ue*Aux + ve*Bux + ux*Cux + uy*Dux + vx*Eux + vy*Fux = Gux
C        ue*Auy + ve*Buy + ux*Cuy + uy*Duy + vx*Euy + vy*Fuy = Guy
C        ue*Avx + ve*Bvx + vx*Cvx + uy*Dvx + vx*Evx + vy*Fvx = Gvx
C        ue*Avy + ve*Bvy + ux*Cvy + uy*Dvy + vx*Evy + vy*Fvy = Gvy
C
C     Linear equations in matrix form:
C
C        |Aue Bue Cue Due Eue Fue| |ue|   |Gue|
C        |Ave Bve Cve Dve Eve Fve| |ve|   |Gve|
C        |Aux Bux Cux Dux Eux Fux| |ux|   |Gux|
C        |                       | |  | = |   |
C        |Auy Buy Cuy Duy Euy Fuy| |uy|   |Guy|
C        |Avx Bvx Cvx Dvx Evx Fvx| |vx|   |Gvx|
C        |Avy Bvy Cvy Dvy Evy Fvy| |vy|   |Gvy|
C
C
C        |a11 a12 a13 a14 a15 a16| |x1|   |b1|
C        |a21 a22 a23 a24 a25 a26| |x2|   |b2|
C        |a31 a32 a33 a34 a35 a36| |x3|   |b3|
C        |                       | |  | = |  |
C        |a41 a42 a43 a44 a45 a46| |x4|   |b4|
C        |a51 a52 a53 a54 a55 a56| |x5|   |b5|
C        |a61 a62 a63 a64 a65 a66| |x6|   |b6|
C
C     100: Outer do loop over ranges 
C     VDAT   -  INPUT ARRAY OF RADIAL VELOCITIES
C     ZDAT   -  INPUT ARRAY OF REFLECTIVITIES

C     TYPE   - TYPE OF OUTPUT ('FOUR', 'LSQR','RESD', 'MEASff.f')
C              FOUR     - RADIAL VELOCITY FROM FOURIER SERIES FIT
C              LSQR     - RADIAL VELOCITY FROM LEAST-SQUARES FIT
C              RESD     - VR(IN) - VR(VAD)
C              MEASff.f - VR(IN), ONLY IF ABS(RESD) < ff.f
C     C1     - MINIMUM NUMBER OF GOOD DATA POINTS FOR VAD ANALYSIS
C     C2     - MAXIMUM ALLOWED AZIMUTH GAP         "   "      "
C     C3     -    "       "    RMS DIFFERENCE BETWEEN INPUT AND VAD WINDS
C     C4     - ORDER OF FIT [LINEAR INCLUDES A(1), A(2), B(1), B(2)]
C
C     Note:  The number of good data points must also be equally 
C            distributed across all four azimuthal quadrants.
C            Test CQUAD1-4 against CQUADMN = 0.25*CNTMN
C
C     NPRNT  - PRINT FLAG FOR VAD WINDS ('    ') NO, ('P') YES,
C              ('F') YES and COEFFICIENTS TO ASCII FILE (fort.999).
C     X0,Y0,H0 - Coordinates of the radar
C
C  VAD MEAN OUTPUT QUANTITIES STORED IN COMMON/VADWINDS/:
C     U0,V0  - HORIZONTAL WINDS       FOR THE ITH RANGE GATE
C     SPD    -     "      WIND SPEED   "   "   "    "     "
C     DIR    -     "       "   DIREC   "   "   "    "     "
C     DIV    -     "      DIVERGENCE   "   "   "    "     "
C     STR    - STRETCHING DEFORMATION  "   "   "    "     "
C     SHR    - SHEARING        "       "   "   "    "     "
C     ERR    - RMS DIFFERENCE BETWEEN FOURIER FIT AND INPUT RADIAL VEL
C     DBZ    - MEAN REFLECTIVITY FACTOR FOR THE ITH RANGE GATE
C     U_VD   - AMOUNT TO SUBTRACT FROM U-COMPONENT
C     V_VD   -    "    "     "      "  V    "
C     AZMVD  - AZIMUTH OF POSITIVE U DIRECTION
C     IFLVAD - FIELD INDEX FROM NAMFLD TO BE ASSOCIATED WITH VAD FIELD
C              NAMVD, WHERE NAMVD IS SUBSET OF NAMFLD.
C
      parameter(np = 6)
c      real al(np,np),ul(np,np),wl(np),vl(np,np),bl(np),xl(np)
      DOUBLE PRECISION al(np,np),ul(np,np),wl(np),vl(np,np)
      DOUBLE PRECISION bl(np),xl(np)

      INCLUDE 'CEDRIC.INC'
      INCLUDE 'vadwinds.inc'

      PARAMETER (MXFC=7)
      DIMENSION A(MXFC),B(MXFC)

      DIMENSION RNG(MXR),Rring(MXR,MXA),AZA(MXR,MXA),NANG(MXR)
      DIMENSION VDAT(MXR,MXA),ZDAT(MXR,MXA)

      CHARACTER*2 NAMINF(4),NAMOUF(4)

      DATA RE,REI/17000.0,1.17647E-04/
      DATA TORAD,TODEG/0.017453293,57.29577951/
      DATA CNT0,GAP0,RMS0/15.0,30.0,999.9/
      DATA EPS/0.1/
      CHARACTER*8 TYPE,AVTYP
      CHARACTER*1 NPRNT
      CHARACTER*4 TYPOUT

      CHARACTER*8 AVNAM
      DATA AVNAM/'??????? '/
      DATA IPR/6/

      AVTYP='XYE'
      BDVAL = BAD
      VNYQ=VNYQ_VOL
      KFIT = INT(C4)
      WRITE(IPR,115) (NAMINF(I),I=1,4),(NAMOUF(I),I=1,4),
     X     TYPE,C1,C2,C3,C4
 115  FORMAT(
     X     /,4X,'  ++++++++++ROUTINE VAD++++++++++  ',/,
     X     /,4X,'  ++++VAD ANALYSIS PARAMETERS++++  ',/,
     X     /,4x,'Input Field: ',4A2,' Output Field: ',4A2,
     X     /,4X,'Type (Fourier series or LstSqr)   : ',A8,
     X     /,4X,'Minimum number of good data points: ',F8.0,
     X     /,4X,'Maximum allowed azimuthal gap     : ',F8.0,
     X     /,4X,'Maximum allowed rms difference    : ',F8.0,
     X     /,4X,'Order of Fourier series fit       : ',F8.0)

      IF(FXOLD.GT.60.0)THEN
         WRITE(6,5)FXOLD
 5       FORMAT(1X,'*** CANNOT DO VAD ANALYSIS: E> ',F6.2,' DEG ***')
         RETURN
      END IF

c-----debug
c      print *,'X0,Y0,H0=',X0,Y0,H0,
c     +        ' kvd,uv_vd=',kvd,U_VD(KVD),V_VD(KVD)
c-----debug
      IF(C1.GT.0.0)THEN
         CNTMN=C1
      ELSE
         CNTMN=CNT0
      END IF
      CQUADMN=0.25*CNTMN
      IF(C2.GT.0.0)THEN
         TGAP=C2
      ELSE
         TGAP=GAP0
      END IF
      IF(C3.GT.0.0)THEN
         RMSMX=C3
      ELSE
         RMSMX=RMS0
      END IF
      IF(C4.GT.0.0)THEN
         KFIT=MIN0(MXFC,INT(C4))
      ELSE
         KFIT=2
      END IF
      READ(TYPE,7)TYPOUT,DIFMX
 7    FORMAT(A4,F4.0)
      IF(DIFMX.EQ.0.0)DIFMX=VNYQ

      SINE=SIN(TORAD*FXOLD)
      COSE=COS(TORAD*FXOLD)
      AZMVD(KVD)=90.0
      SINA=SIN(TORAD*AZMVD(KVD))
      COSA=COS(TORAD*AZMVD(KVD))

      MING=1
      MAXG=NR
c-----debug
c      print *,'VAD: c1-4=',cntmn,tgap,rmsmx,kfit,difmx
c      print *,'VAD: ming,maxg=',ming,maxg
c      print *,'VAD: maxx,maxy=',maxx,maxy
c      print *,'VAD: mxr,mxg=',mxr,mxg
c-----debug

C     Initialize scratch and output arrays to bad data value
C     Initialize VAD variables at each range to bad data value
C     Maximum number of range rings (MXG=976, set in vadwinds.inc)
C
      DO 10 I=1,MXG
         U0(I,KVD) =BDVAL
         V0(I,KVD) =BDVAL
         SPD(I,KVD)=BDVAL
         DIR(I,KVD)=BDVAL
         DIV(I,KVD)=BDVAL
         STR(I,KVD)=BDVAL
         SHR(I,KVD)=BDVAL
         ERR(I,KVD)=BDVAL
         DBZ(I,KVD)=BDVAL
         AVAD0(I,KVD)=BDVAL
         DO K=1,2
            AVAD(I,KVD,K)=BDVAL
            BVAD(I,KVD,K)=BDVAL
         END DO
 10   CONTINUE

C     Write VAD analysis output headers
C
      WRITE(6,13)KVD,FXOLD,AVTYP,
     X     (NAMINF(I),I=1,4),(NAMOUF(I),I=1,4)

      IF(NPRNT.EQ.'F')THEN
         WRITE(999,13)KVD,FXOLD,AVTYP,
     X        (NAMINF(I),I=1,4),(NAMOUF(I),I=1,4)
         WRITE(999,15)
 13   FORMAT(2X,'SCAN #',I3,2X,'ELEV=',F6.2,' CORD=',A8,
     X     2X,'VAD: NameIn=',4A2,' NameOut=',4A2)
 15   FORMAT(10X,'R    Z       A0      A1      A2      B1      B2  ',
     X     '    U0      V0      SPD     DIR     CON     STR  ',
     X     '   SHR     ERR     DBZ')
      END IF

C     Radial velocities are stored in VDAT array (Range x Azimuth).
C     100: Outer-loop over ranges.  For each range gate, execute
C          inner do-loop (90) around azimuths for summations of
C          all variables used in computing Fourier and linear
C          coefficients.
C
      DO 100 I=1,NR
         if(i.le.10)then
            print *,'Ring #',I
         end if

         IF(RNG(I).LE.EPS)GO TO 100

         HRNG=RNG(I)*COSE
c         Z=H0+RNG(I)*SINE+0.5*HRNG*HRNG*REI
         Z=H0+RNG(I)*SINE
         GAPMX=-999.0
         GAPMN=999.0
         ALFT=BDVAL

C     Initialize all variables for this range 
C     before looping around an azimuthal circle.
C
         CNT=0.0
         CQUAD1=0.0
         CQUAD2=0.0
         CQUAD3=0.0
         CQUAD4=0.0
         SUMDBZ=0.0
         CNTDBZ=0.0

C     Variables for Fourier series
C
         A0=0.0
         DO K=1,MXFC
            A(K)=0.0
            B(K)=0.0
         END DO

C     Variables and matrix elements for linear least-squares fit
C
         ue   = bdval
         ve   = bdval
         divl = bdval
         conl = bdval
         strl = bdval
         shrl = bdval
         vorl = bdval
         do m=1,6
            bl(m) = 0.0
            xl(m) = bdval
            do n=1,6
               al(m,n) = 0.0
            end do
         end do

C        Inner-loop over azimuth angles.  Compute all summations.
C
         CNTBEAMS = NANG(I)
         DO 90 J=1,NANG(I)
c            if(i.le.10)then
c               write(6,1700)J,RNG(I),NANG(I),Rring(I,J),
c     +              AZA(I,J),VDAT(I,J),ZDAT(I,J)
c 1700          format(1x,'J=',i4,' Ring,n=',f8.2,I4,' RA=',2f8.3,
c     +              ' VZ=',2f10.3)
c            end if
C           Summation for average reflectivity around circle
C
            IF(ZDAT(I,J) .NE. BDVAL)THEN
               SUMDBZ=SUMDBZ+ZDAT(I,J)
               CNTDBZ=CNTDBZ+1.0
            END IF

C           Summation for Fourier and linear coefficients
C           only if radial velocity is good.
C
            IF(VDAT(I,J).NE.BDVAL)THEN
               ANG=AZA(I,J)
C               IF(ANG.LT.0.0)ANG=ANG+360.0
               IF(ALFT.EQ.BDVAL)THEN
                  ALFT=ANG
               ELSE
                  GAP=ABS(ANG-ALFT)
                  ALFT=ANG
                  IF(GAP.GT.180.0)GAP=ABS(GAP-360.0)
                  IF(GAP.LT.GAPMN)GAPMN=GAP
                  IF(GAP.GT.GAPMX)GAPMX=GAP
               END IF
               ANGR=ANG*TORAD
               SINAZ=SIN(ANGR)
               COSAZ=COS(ANGR)
               X=X0+HRNG*SINAZ
               Y=Y0+HRNG*COSAZ

C              Count the number of good radial velocities in each quadrant.
C
               IF(ANG.LT.0.0)THEN
                  AZM=ANG+360.0
               ELSE
                  AZM=ANG
               END IF
               IF(AZM .GE.   0.0 .AND. AZM .LT.  90.0)CQUAD1=CQUAD1+1.0
               IF(AZM .GE.  90.0 .AND. AZM .LT. 180.0)CQUAD2=CQUAD2+1.0
               IF(AZM .GE. 180.0 .AND. AZM .LT. 270.0)CQUAD3=CQUAD3+1.0
               IF(AZM .GE. 270.0 .AND. AZM .LT. 360.0)CQUAD4=CQUAD4+1.0

               CNT=CNT+1.0
               A0=A0+VDAT(I,J)
               DO K=1,KFIT
                  A(K)=A(K)+VDAT(I,J)*COS(ANGR*K)
                  B(K)=B(K)+VDAT(I,J)*SIN(ANGR*K)
               END DO

C        Set variables for linear least-squares fitting.
C
               Vri = VDAT(I,J)
               Ri = RNG(I)
               RiVri = RNG(I)*VDAT(I,J)

               xi = x
               yi = y
               xr = x0
               yr = y0
               xe = x0
               ye = y0
c-----debug
c               write(6,1700)TYPE(1:4),ri,ang,xi,yi,vri,gap,gapmx
c 1700          format(1x,a4': r,a,x,y=',4f10.3,'  vr=',f8.3,
c     X              '  gap=',2f8.3)
c-----debug

C     Row 1: ue equation
C
               AL(1,1) = AL(1,1) + (xi-xr)*(xi-xr)
               AL(1,2) = AL(1,2) + (yi-yr)*(xi-xr)
               AL(1,3) = AL(1,3) + (xi-xe)*(xi-xr)*(xi-xr)
               AL(1,4) = AL(1,4) + (yi-ye)*(xi-xr)*(xi-xr)
               AL(1,5) = AL(1,5) + (xi-xe)*(yi-yr)*(xi-xr)
               AL(1,6) = AL(1,6) + (yi-ye)*(yi-yr)*(xi-xr)
               BL(1)   = BL(1)   + RiVri*(xi-xr)
               
C     Row 2: ve equation
C
               AL(2,1) = AL(2,1) + (xi-xr)*(yi-yr)
               AL(2,2) = AL(2,2) + (yi-yr)*(yi-yr)
               AL(2,3) = AL(2,3) + (xi-xe)*(xi-xr)*(yi-yr)
               AL(2,4) = AL(2,4) + (yi-ye)*(xi-xr)*(yi-yr)
               AL(2,5) = AL(2,5) + (xi-xe)*(yi-yr)*(yi-yr)
               AL(2,6) = AL(2,6) + (yi-ye)*(yi-yr)*(yi-yr)
               BL(2)   = BL(2)   + RiVri*(yi-yr)
               
C     Row 3: ux equation
C
               AL(3,1) = AL(3,1) + (xi-xr)*(xi-xe)*(xi-xr)
               AL(3,2) = AL(3,2) + (yi-yr)*(xi-xe)*(xi-xr)
               AL(3,3) = AL(3,3) + (xi-xe)*(xi-xr)*(xi-xe)*(xi-xr)
               AL(3,4) = AL(3,4) + (yi-ye)*(xi-xr)*(xi-xe)*(xi-xr)
               AL(3,5) = AL(3,5) + (xi-xe)*(yi-yr)*(xi-xe)*(xi-xr)
               AL(3,6) = AL(3,6) + (yi-ye)*(yi-yr)*(xi-xe)*(xi-xr)
               BL(3)   = BL(3)   + RiVri*(xi-xe)*(xi-xr)
               
C     Row 4: uy equation
C
               AL(4,1) = AL(4,1) + (xi-xr)*(yi-ye)*(xi-xr)
               AL(4,2) = AL(4,2) + (yi-yr)*(yi-ye)*(xi-xr)
               AL(4,3) = AL(4,3) + (xi-xe)*(xi-xr)*(yi-ye)*(xi-xr)
               AL(4,4) = AL(4,4) + (yi-ye)*(xi-xr)*(yi-ye)*(xi-xr)
               AL(4,5) = AL(4,5) + (xi-xe)*(yi-yr)*(yi-ye)*(xi-xr)
               AL(4,6) = AL(4,6) + (yi-ye)*(yi-yr)*(yi-ye)*(xi-xr)
               BL(4) = BL(4) + RiVri*(yi-ye)*(xi-xr)
               
C     Row 5: vx equation
C
               AL(5,1) = AL(5,1) + (xi-xr)*(xi-xe)*(yi-yr)
               AL(5,2) = AL(5,2) + (yi-yr)*(xi-xe)*(yi-yr)
               AL(5,3) = AL(5,3) + (xi-xe)*(xi-xr)*(xi-xe)*(yi-yr)
               AL(5,4) = AL(5,4) + (yi-ye)*(xi-xr)*(xi-xe)*(yi-yr)
               AL(5,5) = AL(5,5) + (xi-xe)*(yi-yr)*(xi-xe)*(yi-yr)
               AL(5,6) = AL(5,6) + (yi-ye)*(yi-yr)*(xi-xe)*(yi-yr)
               BL(5)   = BL(5)   + RiVri*(xi-xe)*(yi-yr)
               
C     Row 6: vy equation
C
               AL(6,1) = AL(6,1) + (xi-xr)*(yi-ye)*(yi-yr)
               AL(6,2) = AL(6,2) + (yi-yr)*(yi-ye)*(yi-yr)
               AL(6,3) = AL(6,3) + (xi-xe)*(xi-xr)*(yi-ye)*(yi-yr)
               AL(6,4) = AL(6,4) + (yi-ye)*(xi-xr)*(yi-ye)*(yi-yr)
               AL(6,5) = AL(6,5) + (xi-xe)*(yi-yr)*(yi-ye)*(yi-yr)
               AL(6,6) = AL(6,6) + (yi-ye)*(yi-yr)*(yi-ye)*(yi-yr)
               BL(6)   = BL(6)   + RiVri*(yi-ye)*(yi-yr)

            END IF
 90      CONTINUE

C     CALCULATE THE VAD MEAN PARAMETERS AND VAD ANALYTIC WINDS 
C     FOR THIS RANGE GATE, THEN GO ON TO NEXT RANGE GATE.
C     
c-----debug
c         if(i.le.10)then
c            print *,TYPE(1:4),
c     X           ': kvd,beams,cnt,gapmx=',kvd,cntbeams,cnt,gapmx
c            print *,TYPE(1:4),
c     X           ': uvdthv=',ue,ve,divl,strl,shrl,vorl
c         end if
c-----debug
         IF((CNT.GE.CNTMN.AND.GAPMX.LE.TGAP) .AND.
     X       CQUAD1.GE.CQUADMN .AND. CQUAD2.GE.CQUADMN .AND.
     X       CQUAD3.GE.CQUADMN .AND. CQUAD4.GE.CQUADMN)THEN

C           Acceptable numbers of points (CNT) and maximum gap (GAPMX)
C
            IF(CNTDBZ.NE.0.0)THEN
               DBZ(I,KVD)=SUMDBZ/CNTDBZ
            ELSE
               DBZ(I,KVD)=BDVAL
            END IF

            IF(TYPE.EQ.'FOUR')THEN
c-----debug
c               if(i.le.10)then
c                  print *,'FOUR- Cnt,gapmx=',cnt,gapmx
c               end if
c-----debug

C     TYPE='FOUR': Calculate wind parameters from Fourier
C                  fit and put these in /VADWINDS/ arrays
C
               A0=A0/CNT
               AVAD0(I,KVD)=A0
               DO K=1,KFIT
                  A(K)=2.0*A(K)/CNT
                  B(K)=2.0*B(K)/CNT
                  IF(K.LE.2)THEN
                     AVAD(I,KVD,K)=A(K)
                     BVAD(I,KVD,K)=B(K)
                  END IF
               END DO

c-----debug
c               if(i.le.10)then
c                  print *,'FOUR- A0,A,B=',a0,(a(k),k=1,2),(b(k),k=1,2)
c               end if
c-----debug

               UMN=(B(1)/COSE)-U_VD(KVD)
               VMN=(A(1)/COSE)-V_VD(KVD)
               DIV(I,KVD)= 2.0*A0/(RNG(I)*COSE*COSE)
               STR(I,KVD)=-2.0*A(2)/(RNG(I)*COSE*COSE)
               SHR(I,KVD)= 2.0*B(2)/(RNG(I)*COSE*COSE)

            ELSE IF(TYPE.EQ.'LSQR')THEN
c-----debug
c               if(i.le.10)then
c                  print *,'LSQR- Cnt,gapmx=',cnt,gapmx
c               end if
c-----debug

C     TYPE='LSQR': Calculate wind parameters from least-squares 
C                  fit and put these in /VADWINDS/ arrays
c     Copy al into ul so as not to destroy al.
c
               n = np            
               do jj=1,n
                  wl(jj)=0.0
                  do ii=1,n
                     ul(ii,jj)=al(ii,jj)
                  enddo
               enddo
               
c     Compute the singular value decomposition of u=a.
c               call svdcmp(ul,n,n,np,np,wl,vl)
               call dsvdcmp(ul,n,n,np,np,wl,vl)

c     Set the maximum (wmax) singular value allowed.
c     
               wmax=0.
               do jj=1,n
                  if(wl(jj).gt.wmax)wmax=wl(jj)
               end do
               
c     Set the threshold (wmin) for singular values allowed
c     to be nonzero.
c
               wmin=wmax*1.0e-6
               do jj=1,n
                  if(wl(jj).lt.wmin)wl(jj)=0.
               enddo
c-----debug
c               print *,'wmax,wmin=',wmax,wmin
c-----debug

c     Finally, solve |A||x| = |B| for the vector |x|
c     using the backsubstitution method.
c
c               call svbksb(ul,wl,vl,n,n,np,np,bl,xl)
               call dsvbksb(ul,wl,vl,n,n,np,np,bl,xl)

c-----debug
c               do m=1,6
c                  write(6,1701)m,BL(M),(ul(m,n),n=1,6),xl(m)
c 1701             format('m,bl,ul=',i1,7f10.4,' xl=',f10.4)
c               end do
c-----debug
               
c     Linear winds from least-squares solution.
c
c        ue = xl(1), ux = xl(3), and uy = xl(4)
c        ve = xl(2), vx = xl(5), and vy = xl(6)
c           divl = ux+vy = xl(3)+xl(6) = -conl
c           strl = ux-vy = xl(3)-xl(6)
c           shrl = uy+vx = xl(4)+xl(5)
c           vorl = vx-uy = xl(5)-xl(4)
c
               ue   = xl(1)            
               ve   = xl(2)
               divl =  (xl(3)+xl(6))            
               conl = -(xl(3)+xl(6))
               strl = xl(3)-xl(6)
               shrl = xl(4)+xl(5)
               vorl = xl(5)-xl(4)
               umn=ue-u_vd(kvd)
               vmn=ve-v_vd(kvd)
               div(i,kvd)=divl
               str(i,kvd)=strl
               shr(i,kvd)=shrl
            END IF
c-----debug
               if(i.le.10)then
                  print *,TYPE(1:4),'- uvdthv=',ue,ve,divl,strl,
     +                 shrl,vorl
               end if
c-----debug
               
            
            U0(I,KVD)= UMN*SINA + VMN*COSA
            V0(I,KVD)=-UMN*COSA + VMN*SINA
            SPD(I,KVD)=SQRT(U0(I,KVD)*U0(I,KVD)+V0(I,KVD)*V0(I,KVD))
            IF(V0(I,KVD).EQ.0.0.AND.U0(I,KVD).EQ.0.0)THEN
               DIR(I,KVD)=180.0
            ELSE IF(V0(I,KVD).EQ.0.0.AND.U0(I,KVD).GT.0.0)THEN
               DIR(I,KVD)=90.0
            ELSE IF(V0(I,KVD).EQ.0.0.AND.U0(I,KVD).LT.0.0)THEN
               DIR(I,KVD)=270.0
            ELSE
               DIR(I,KVD)=TODEG*ATAN2(U0(I,KVD),V0(I,KVD))
               IF(DIR(I,KVD).LT.0.0)DIR(I,KVD)=DIR(I,KVD)+360.0
            END IF
            DIR(I,KVD)=DIR(I,KVD)+180.0
            IF(DIR(I,KVD).GE.360.0)DIR(I,KVD)=DIR(I,KVD)-360.0
            
C     Inner-loop over azimuth angles.  Compute RMS difference
C     between the input and VAD-derived radial velocities.
C
            SUMSQDIF=0.0
            CNTDIF=0.0
            DO 92 J=1,NANG(I)
               ANG=AZA(I,J)
C              IF(ANG.LT.0.0)ANG=ANG+360.0
               ANGR=ANG*TORAD
               SINAZ=SIN(ANGR)
               COSAZ=COS(ANGR)
               X=X0+HRNG*SINAZ
               Y=Y0+HRNG*COSAZ

               VRVAD=BDVAL
               IF(TYPE.EQ.'FOUR')THEN
                  IF(A0.NE.BDVAL)THEN
                     VRVAD=A0
                     DO K=1,KFIT
                        VRVAD=VRVAD+A(K)*COS(ANGR*K)+B(K)*SIN(ANGR*K)
                     END DO
                  END IF
               ELSE IF(TYPE.EQ.'LSQR')THEN
                  IF(UE.NE.BDVAL)THEN
                     UXY=UE+0.5*(DIVL+STRL)*(X-XE)
     X                     +0.5*(SHRL-VORL)*(Y-YE)
                     VXY=VE+0.5*(SHRL+VORL)*(X-XE)
     X                     +0.5*(DIVL-STRL)*(Y-YE)
                     VRVAD=(UXY*SINAZ+VXY*COSAZ)*COSE
                  END IF
               END IF
               
               VRINP=VDAT(I,J)
               IF(VRINP.NE.BDVAL.AND.VRVAD.NE.BDVAL)THEN
                  CNTDIF=CNTDIF+1.0
                  VRDIF=VRINP-VRVAD
                  SUMSQDIF=SUMSQDIF+VRDIF*VRDIF
               ELSE
                  VRDIF=BDVAL
               END IF
               
 92         CONTINUE
C     End azimuth angle loop for RMS difference calculation.

C           CHECK RMS DIFFERENCE BETWEEN VAD AND INPUT WINDS.
C           IF BIGGER THAN RMSMX=C3, THEN SET VAD WINDS TO BDVAL.
C
            IF(CNTDIF.GT.CNTMN)THEN
               RMSDIF=SQRT(SUMSQDIF/CNTDIF)
               ERR(I,KVD)=RMSDIF
            ELSE
               ERR(I,KVD)=BDVAL
            END IF
            IF(RMSDIF.GT.RMSMX)THEN
               U0(I,KVD) =BDVAL
               V0(I,KVD) =BDVAL
               DIR(I,KVD)=BDVAL
               SPD(I,KVD)=BDVAL
               DIV(I,KVD)=BDVAL
               STR(I,KVD)=BDVAL
               SHR(I,KVD)=BDVAL
               DBZ(I,KVD)=BDVAL
            END IF
         ELSE

C           Too few points (CNT) or gap too big (GAPMX)
C
            U0(I,KVD) =BDVAL
            V0(I,KVD) =BDVAL
            DIR(I,KVD)=BDVAL
            SPD(I,KVD)=BDVAL
            DIV(I,KVD)=BDVAL
            STR(I,KVD)=BDVAL
            SHR(I,KVD)=BDVAL
            ERR(I,KVD)=BDVAL
            DBZ(I,KVD)=BDVAL
            A0=BDVAL
            AVAD0(I,KVD)=BDVAL
            DO K=1,KFIT
               A(K)=BDVAL
               B(K)=BDVAL
               IF(K.LE.2)THEN
                  AVAD(I,KVD,K)=BDVAL
                  BVAD(I,KVD,K)=BDVAL
               END IF
            END DO
         END IF
         
 100  CONTINUE

C     100: END OUTER LOOP OVER RANGE GATES
      
C     LINEARLY INTERPOLATE TO FILL MISSING GATES
C
      print *,TYPE(1:4),': VAD-level= ',kvd
      DO 110 I=1,MAXG
         Z=H0+RNG(I)*SINE
         IF(I.LE.MAXG)GO TO 102
c         IF(I .EQ.1.OR.I.EQ.MAXG)GO TO 102
         IL=I-1
         IR=I+1
         IF(U0( I,KVD).EQ.BDVAL.AND.
     X      U0(IL,KVD).NE.BDVAL.AND.
     X      U0(IR,KVD).NE.BDVAL)THEN
            U0(I,KVD) =0.5*(U0(IL,KVD)+U0(IR,KVD))
            V0(I,KVD) =0.5*(V0(IL,KVD)+V0(IR,KVD))
            SPD(I,KVD)=0.5*(SPD(IL,KVD)+SPD(IR,KVD))
            IF((DIR(IL,KVD)-DIR(IR,KVD)).GT.180.0)THEN
               DIR(IR,KVD)=DIR(IR,KVD)+360.0
            ELSE IF ((DIR(IR,KVD)-DIR(IL,KVD)).GT.180.0)THEN
               DIR(IL,KVD)=DIR(IL,KVD)+360.0
            END IF
            DIR(I,KVD)=0.5*(DIR(IL,KVD)+DIR(IR,KVD))
            IF(DIR(I,KVD).GT.360.0)DIR(I,KVD)=DIR(I,KVD)-360.0
            DIV(I,KVD)=0.5*(DIV(IL,KVD)+DIV(IR,KVD))
            STR(I,KVD)=0.5*(STR(IL,KVD)+STR(IR,KVD))
            SHR(I,KVD)=0.5*(SHR(IL,KVD)+SHR(IR,KVD))
            ERR(I,KVD)=0.5*(ERR(IL,KVD)+ERR(IR,KVD))
            DBZ(I,KVD)=0.5*(DBZ(IL,KVD)+DBZ(IR,KVD))
         END IF
         IF(AVAD0( I,KVD).EQ.BDVAL.AND.
     X      AVAD0(IL,KVD).NE.BDVAL.AND.
     X      AVAD0(IR,KVD).NE.BDVAL)THEN
            AVAD0(I,KVD)=0.5*(AVAD0(IL,KVD)+AVAD0(IR,KVD))
            DO K=1,2
               AVAD(I,KVD,K)=0.5*(AVAD(IL,KVD,K)+AVAD(IR,KVD,K))
               BVAD(I,KVD,K)=0.5*(BVAD(IL,KVD,K)+BVAD(IR,KVD,K))
            END DO
         END IF

 102     CONTINUE

         IF(U0(I,KVD).NE.BDVAL)THEN
            IF(NPRNT.EQ.'P'.OR.NPRNT.EQ.'F')THEN
               WRITE(6,103)TYPE,I,RNG(I),Z,U0(I,KVD),
     X              V0(I,KVD),SPD(I,KVD),DIR(I,KVD),-DIV(I,KVD),
     X              STR(I,KVD),SHR(I,KVD),ERR(I,KVD),DBZ(I,KVD)
 103           FORMAT(1X,A4,':    IRZ=',I4,2F8.3,'  UVSD=',4F8.2,
     X              '  CTH=',3F8.2,' ERR=',F8.2,' DBZ=',F8.2)
            END IF
            IF(NPRNT.EQ.'F')THEN
               WRITE(999,105)KVD,I,RNG(I),Z,AVAD0(I,KVD),
     X              AVAD(I,KVD,1),AVAD(I,KVD,2),BVAD(I,KVD,1),
     X              BVAD(I,KVD,2),U0(I,KVD),V0(I,KVD),SPD(I,KVD),
     X              DIR(I,KVD),-DIV(I,KVD),STR(I,KVD),SHR(I,KVD),
     X              ERR(I,KVD),DBZ(I,KVD)
 105           FORMAT('I=',I2,I3,F6.2,F6.3,14F8.2)
            END IF
         END IF

 110  CONTINUE

      RETURN
      END
c
c----------------------------------------------------------------------X
c
      SUBROUTINE svdcmp(a,m,n,mp,np,w,v)
c     
c     Given a matrix a(1:m,1:n), with physical dimension mp by np, this
c     routine computes its singular value decomposition, A = U*W*V^T.  
c     The martrix U replaces a on output.  The diagaonal matrix of 
c     singular values W is output as a vector w(1:n).  The matrix V 
c     (not the transpose V^T) is output as v(1:n,1:n).
c
c        m,n    - logical dimensions of a, and will be equal for 
c                 square matrices
c        mp,np  - physical dimensions of a
c        NMAX   - maximum anticipated value of n
c        a(1:m) - the input left-hand side
c        w(1:m) - the output singular values
c        v(1:m) - the output matrix, not its transpose
c
      INTEGER m,mp,n,np,NMAX
      REAL a(mp,np),v(np,np),w(np)
      PARAMETER (NMAX=500)
CU    USES pythag
      INTEGER i,its,j,jj,k,l,nm
      REAL anorm,c,f,g,h,s,scale,x,y,z,rv1(NMAX),pythag
      g=0.0
      scale=0.0
      anorm=0.0
      do 25 i=1,n
        l=i+1
        rv1(i)=scale*g
        g=0.0
        s=0.0
        scale=0.0
        if(i.le.m)then
          do 11 k=i,m
            scale=scale+abs(a(k,i))
11        continue
          if(scale.ne.0.0)then
            do 12 k=i,m
              a(k,i)=a(k,i)/scale
              s=s+a(k,i)*a(k,i)
12          continue
            f=a(i,i)
            g=-sign(sqrt(s),f)
            h=f*g-s
            a(i,i)=f-g
            do 15 j=l,n
              s=0.0
              do 13 k=i,m
                s=s+a(k,i)*a(k,j)
13            continue
              f=s/h
              do 14 k=i,m
                a(k,j)=a(k,j)+f*a(k,i)
14            continue
15          continue
            do 16 k=i,m
              a(k,i)=scale*a(k,i)
16          continue
          endif
        endif
        w(i)=scale *g
        g=0.0
        s=0.0
        scale=0.0
        if((i.le.m).and.(i.ne.n))then
          do 17 k=l,n
            scale=scale+abs(a(i,k))
17        continue
          if(scale.ne.0.0)then
            do 18 k=l,n
              a(i,k)=a(i,k)/scale
              s=s+a(i,k)*a(i,k)
18          continue
            f=a(i,l)
            g=-sign(sqrt(s),f)
            h=f*g-s
            a(i,l)=f-g
            do 19 k=l,n
              rv1(k)=a(i,k)/h
19          continue
            do 23 j=l,m
              s=0.0
              do 21 k=l,n
                s=s+a(j,k)*a(i,k)
21            continue
              do 22 k=l,n
                a(j,k)=a(j,k)+s*rv1(k)
22            continue
23          continue
            do 24 k=l,n
              a(i,k)=scale*a(i,k)
24          continue
          endif
        endif
        anorm=max(anorm,(abs(w(i))+abs(rv1(i))))
25    continue
      do 32 i=n,1,-1
        if(i.lt.n)then
          if(g.ne.0.0)then
            do 26 j=l,n
              v(j,i)=(a(i,j)/a(i,l))/g
26          continue
            do 29 j=l,n
              s=0.0
              do 27 k=l,n
                s=s+a(i,k)*v(k,j)
27            continue
              do 28 k=l,n
                v(k,j)=v(k,j)+s*v(k,i)
28            continue
29          continue
          endif
          do 31 j=l,n
            v(i,j)=0.0
            v(j,i)=0.0
31        continue
        endif
        v(i,i)=1.0
        g=rv1(i)
        l=i
32    continue
      do 39 i=min(m,n),1,-1
        l=i+1
        g=w(i)
        do 33 j=l,n
          a(i,j)=0.0
33      continue
        if(g.ne.0.0)then
          g=1.0/g
          do 36 j=l,n
            s=0.0
            do 34 k=l,m
              s=s+a(k,i)*a(k,j)
34          continue
            f=(s/a(i,i))*g
            do 35 k=i,m
              a(k,j)=a(k,j)+f*a(k,i)
35          continue
36        continue
          do 37 j=i,m
            a(j,i)=a(j,i)*g
37        continue
        else
          do 38 j= i,m
            a(j,i)=0.0
38        continue
        endif
        a(i,i)=a(i,i)+1.0
39    continue
      do 49 k=n,1,-1
        do 48 its=1,30
          do 41 l=k,1,-1
            nm=l-1
            if((abs(rv1(l))+anorm).eq.anorm)  goto 2
            if((abs(w(nm))+anorm).eq.anorm)  goto 1
41        continue
1         c=0.0
          s=1.0
          do 43 i=l,k
            f=s*rv1(i)
            rv1(i)=c*rv1(i)
            if((abs(f)+anorm).eq.anorm) goto 2
            g=w(i)
            h=pythag(f,g)
            w(i)=h
            h=1.0/h
            c= (g*h)
            s=-(f*h)
            do 42 j=1,m
              y=a(j,nm)
              z=a(j,i)
              a(j,nm)=(y*c)+(z*s)
              a(j,i)=-(y*s)+(z*c)
42          continue
43        continue
2         z=w(k)
          if(l.eq.k)then
            if(z.lt.0.0)then
              w(k)=-z
              do 44 j=1,n
                v(j,k)=-v(j,k)
44            continue
            endif
            goto 3
          endif
          if(its.eq.30) pause 'no convergence in svdcmp'
          x=w(l)
          nm=k-1
          y=w(nm)
          g=rv1(nm)
          h=rv1(k)
          f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0*h*y)
          g=pythag(f,1.0)
          f=((x-z)*(x+z)+h*((y/(f+sign(g,f)))-h))/x
          c=1.0
          s=1.0
          do 47 j=l,nm
            i=j+1
            g=rv1(i)
            y=w(i)
            h=s*g
            g=c*g
            z=pythag(f,h)
            rv1(j)=z
            c=f/z
            s=h/z
            f= (x*c)+(g*s)
            g=-(x*s)+(g*c)
            h=y*s
            y=y*c
            do 45 jj=1,n
              x=v(jj,j)
              z=v(jj,i)
              v(jj,j)= (x*c)+(z*s)
              v(jj,i)=-(x*s)+(z*c)
45          continue
            z=pythag(f,h)
            w(j)=z
            if(z.ne.0.0)then
              z=1.0/z
              c=f*z
              s=h*z
            endif
            f= (c*g)+(s*y)
            x=-(s*g)+(c*y)
            do 46 jj=1,m
              y=a(jj,j)
              z=a(jj,i)
              a(jj,j)= (y*c)+(z*s)
              a(jj,i)=-(y*s)+(z*c)
46          continue
47        continue
          rv1(l)=0.0
          rv1(k)=f
          w(k)=x
48      continue
3       continue
49    continue
      return
      END
c
c----------------------------------------------------------------------X
c
      FUNCTION pythag(a,b)
      REAL a,b,pythag
      REAL absa,absb
      absa=abs(a)
      absb=abs(b)
      if(absa.gt.absb)then
        pythag=absa*sqrt(1.+(absb/absa)**2)
      else
        if(absb.eq.0.)then
          pythag=0.
        else
          pythag=absb*sqrt(1.+(absa/absb)**2)
        endif
      endif
      return
      END
c
c----------------------------------------------------------------------X
c
      SUBROUTINE svbksb(u,w,v,m,n,mp,np,b,x)
c
c     Solves |A||x| = |B|, where A is specified by the arrays u,w,v
c     as returned by svdcmp.  
c        m,n    - logical dimensions of a, and will be equal for 
c                 square matrices
c        mp,np  - physical dimensions of a
c        b(1:m) - the input right-hand side
c        x(1:n) - the output soulution vector
c        NMAX   - maximum anticipated value of n
c
c     No input quantities are destroyed, so the routine may be called 
c     sequentially with different b's.
c     
      INTEGER m,mp,n,np,NMAX
      REAL b(mp),u(mp,np),v(np,np),w(np),x(np)
      PARAMETER (NMAX=500)
      INTEGER i,j,jj
      REAL s,tmp(NMAX)
      do 12 j=1,n
        s=0.
        if(w(j).ne.0.)then
          do 11 i=1,m
            s=s+u(i,j)*b(i)
11        continue
          s=s/w(j)
        endif
        tmp(j)=s
12    continue
      do 14 j=1,n
        s=0.
        do 13 jj=1,n
          s=s+v(j,jj)*tmp(jj)
13      continue
        x(j)=s
14    continue
      return
      END
c
c----------------------------------------------------------------------X
c
      SUBROUTINE dsvdcmp(a,m,n,mp,np,w,v)
c     
c     Given a matrix a(1:m,1:n), with physical dimension mp by np, this
c     routine computes its singular value decomposition, A = U*W*V^T.  
c     The martrix U replaces a on output.  The diagaonal matrix of 
c     singular values W is output as a vector w(1:n).  The matrix V 
c     (not the transpose V^T) is output as v(1:n,1:n).
c
c        m,n    - logical dimensions of a, and will be equal for 
c                 square matrices
c        mp,np  - physical dimensions of a
c        NMAX   - maximum anticipated value of n
c        a(1:m) - the input left-hand side
c        w(1:m) - the output singular values
c        v(1:m) - the output matrix, not its transpose
c
      INTEGER m,mp,n,np,NMAX
      DOUBLE PRECISION a(mp,np),v(np,np),w(np)
      PARAMETER (NMAX=500)
CU    USES dpythag
      INTEGER i,its,j,jj,k,l,nm
      DOUBLE PRECISION anorm,c,f,g,h,s,scale,x,y,z,rv1(NMAX),dpythag
      g=0.0d0
      scale=0.0d0
      anorm=0.0d0
      do 25 i=1,n
        l=i+1
        rv1(i)=scale*g
        g=0.0d0
        s=0.0d0
        scale=0.0d0
        if(i.le.m)then
          do 11 k=i,m
            scale=scale+abs(a(k,i))
11        continue
          if(scale.ne.0.0d0)then
            do 12 k=i,m
              a(k,i)=a(k,i)/scale
              s=s+a(k,i)*a(k,i)
12          continue
            f=a(i,i)
            g=-sign(sqrt(s),f)
            h=f*g-s
            a(i,i)=f-g
            do 15 j=l,n
              s=0.0d0
              do 13 k=i,m
                s=s+a(k,i)*a(k,j)
13            continue
              f=s/h
              do 14 k=i,m
                a(k,j)=a(k,j)+f*a(k,i)
14            continue
15          continue
            do 16 k=i,m
              a(k,i)=scale*a(k,i)
16          continue
          endif
        endif
        w(i)=scale *g
        g=0.0d0
        s=0.0d0
        scale=0.0d0
        if((i.le.m).and.(i.ne.n))then
          do 17 k=l,n
            scale=scale+abs(a(i,k))
17        continue
          if(scale.ne.0.0d0)then
            do 18 k=l,n
              a(i,k)=a(i,k)/scale
              s=s+a(i,k)*a(i,k)
18          continue
            f=a(i,l)
            g=-sign(sqrt(s),f)
            h=f*g-s
            a(i,l)=f-g
            do 19 k=l,n
              rv1(k)=a(i,k)/h
19          continue
            do 23 j=l,m
              s=0.0d0
              do 21 k=l,n
                s=s+a(j,k)*a(i,k)
21            continue
              do 22 k=l,n
                a(j,k)=a(j,k)+s*rv1(k)
22            continue
23          continue
            do 24 k=l,n
              a(i,k)=scale*a(i,k)
24          continue
          endif
        endif
        anorm=max(anorm,(abs(w(i))+abs(rv1(i))))
25    continue
      do 32 i=n,1,-1
        if(i.lt.n)then
          if(g.ne.0.0d0)then
            do 26 j=l,n
              v(j,i)=(a(i,j)/a(i,l))/g
26          continue
            do 29 j=l,n
              s=0.0d0
              do 27 k=l,n
                s=s+a(i,k)*v(k,j)
27            continue
              do 28 k=l,n
                v(k,j)=v(k,j)+s*v(k,i)
28            continue
29          continue
          endif
          do 31 j=l,n
            v(i,j)=0.0d0
            v(j,i)=0.0d0
31        continue
        endif
        v(i,i)=1.0d0
        g=rv1(i)
        l=i
32    continue
      do 39 i=min(m,n),1,-1
        l=i+1
        g=w(i)
        do 33 j=l,n
          a(i,j)=0.0d0
33      continue
        if(g.ne.0.0d0)then
          g=1.0d0/g
          do 36 j=l,n
            s=0.0d0
            do 34 k=l,m
              s=s+a(k,i)*a(k,j)
34          continue
            f=(s/a(i,i))*g
            do 35 k=i,m
              a(k,j)=a(k,j)+f*a(k,i)
35          continue
36        continue
          do 37 j=i,m
            a(j,i)=a(j,i)*g
37        continue
        else
          do 38 j= i,m
            a(j,i)=0.0d0
38        continue
        endif
        a(i,i)=a(i,i)+1.0d0
39    continue
      do 49 k=n,1,-1
        do 48 its=1,30
          do 41 l=k,1,-1
            nm=l-1
            if((abs(rv1(l))+anorm).eq.anorm)  goto 2
            if((abs(w(nm))+anorm).eq.anorm)  goto 1
41        continue
1         c=0.0d0
          s=1.0d0
          do 43 i=l,k
            f=s*rv1(i)
            rv1(i)=c*rv1(i)
            if((abs(f)+anorm).eq.anorm) goto 2
            g=w(i)
            h=dpythag(f,g)
            w(i)=h
            h=1.0d0/h
            c= (g*h)
            s=-(f*h)
            do 42 j=1,m
              y=a(j,nm)
              z=a(j,i)
              a(j,nm)=(y*c)+(z*s)
              a(j,i)=-(y*s)+(z*c)
42          continue
43        continue
2         z=w(k)
          if(l.eq.k)then
            if(z.lt.0.0d0)then
              w(k)=-z
              do 44 j=1,n
                v(j,k)=-v(j,k)
44            continue
            endif
            goto 3
          endif
          if(its.eq.30) pause 'no convergence in svdcmp'
          x=w(l)
          nm=k-1
          y=w(nm)
          g=rv1(nm)
          h=rv1(k)
          f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0d0*h*y)
          g=dpythag(f,1.0d0)
          f=((x-z)*(x+z)+h*((y/(f+sign(g,f)))-h))/x
          c=1.0d0
          s=1.0d0
          do 47 j=l,nm
            i=j+1
            g=rv1(i)
            y=w(i)
            h=s*g
            g=c*g
            z=dpythag(f,h)
            rv1(j)=z
            c=f/z
            s=h/z
            f= (x*c)+(g*s)
            g=-(x*s)+(g*c)
            h=y*s
            y=y*c
            do 45 jj=1,n
              x=v(jj,j)
              z=v(jj,i)
              v(jj,j)= (x*c)+(z*s)
              v(jj,i)=-(x*s)+(z*c)
45          continue
            z=dpythag(f,h)
            w(j)=z
            if(z.ne.0.0d0)then
              z=1.0d0/z
              c=f*z
              s=h*z
            endif
            f= (c*g)+(s*y)
            x=-(s*g)+(c*y)
            do 46 jj=1,m
              y=a(jj,j)
              z=a(jj,i)
              a(jj,j)= (y*c)+(z*s)
              a(jj,i)=-(y*s)+(z*c)
46          continue
47        continue
          rv1(l)=0.0d0
          rv1(k)=f
          w(k)=x
48      continue
3       continue
49    continue
      return
      END
c
c----------------------------------------------------------------------X
c
      FUNCTION dpythag(a,b)
      DOUBLE PRECISION a,b,dpythag
      DOUBLE PRECISION absa,absb
      absa=abs(a)
      absb=abs(b)
      if(absa.gt.absb)then
        dpythag=absa*sqrt(1.0d0+(absb/absa)**2)
      else
        if(absb.eq.0.0d0)then
          dpythag=0.0d0
        else
          dpythag=absb*sqrt(1.0d0+(absa/absb)**2)
        endif
      endif
      return
      END
c
c----------------------------------------------------------------------X
c
      SUBROUTINE dsvbksb(u,w,v,m,n,mp,np,b,x)
c
c     Solves |A||x| = |B|, where A is specified by the arrays u,w,v
c     as returned by svdcmp.  
c        m,n    - logical dimensions of a, and will be equal for 
c                 square matrices
c        mp,np  - physical dimensions of a
c        b(1:m) - the input right-hand side
c        x(1:n) - the output soulution vector
c        NMAX   - maximum anticipated value of n
c
c     No input quantities are destroyed, so the routine may be called 
c     sequentially with different b's.
c     
      INTEGER m,mp,n,np,NMAX
      DOUBLE PRECISION b(mp),u(mp,np),v(np,np),w(np),x(np)
      PARAMETER (NMAX=500)
      INTEGER i,j,jj
      DOUBLE PRECISION s,tmp(NMAX)
      do 12 j=1,n
        s=0.0d0
        if(w(j).ne.0.0d0)then
          do 11 i=1,m
            s=s+u(i,j)*b(i)
11        continue
          s=s/w(j)
        endif
        tmp(j)=s
12    continue
      do 14 j=1,n
        s=0.0d0
        do 13 jj=1,n
          s=s+v(j,jj)*tmp(jj)
13      continue
        x(j)=s
14    continue
      return
      END


