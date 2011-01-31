      SUBROUTINE DOFILT(DATA,IFLDS,OFLDS,IFLTYP,IPROC,FSPACE,C3,C4,
     X     NSCTP,NMFILT,ICOPLANE,VNYQUIST)
C
C     THIS SUBROUTINE PERFORMS 2-D FILTERING OF THE DATA BEFORE
C     INTERPOLATION. IT READS ALL THE DATA FOR A SINGLE SWEEP INTO
C     AN ARRAY AND FILTERS THE BEAMS, ONE BY ONE. AFTER EACH BEAM IS
C     FILTERED, THE BEAM IS WRITTEN TO A SCRATCH DISK UNIT (MTMP=3).
C
C     DATA    -  WILL HOLD THE DATA FOR A SINGLE SWEEP
C     FFLDS   -  NAMES OF FIELDS TO BE FILTERED
C     VNYQ    -  NYQUIST VELOCITY TO BE USED FOR LOCAL UNFOLDING
C     VREF    -  REFERENCE VELOCITY TO BE USED FOR LOCAL UNFOLDING
C     IFLTYP  -  FILTER WEIGHTS (UNIFORM, TRIANGULAR, CRESSMAN,
C                QUADRATIC, OR EXPONENTIAL)
C     IPROC   -  INDICATES ANY SPECIAL PROCESSING ('LINEAR', 'UNFOLD')
C     ISPEC   -  Special processing flag: 
C                   (0) None, (1) Unfold, (2) Linear
C     DRG,DA  -  RANGE GATE SPACING (KM), AVERAGE ANGLE SPACING (DEG)
C     FSPACE  -  FILTER SPACE('RADR' OR 'CART')
C     C3,C4   -  FILTER DIMENSIONS: RADR - (2*C3+1) GATES BY (2*C4+1)BEAMS
C                                   CART - LINEAR RADII (KM BY KM)
C     BDVAL   -  BDVAL FOR FIELDS
C     NSCTP   -  IF 'NOFILL' THEN NO FILLING IN WHEN CENTRAL POINT IS BDVAL
C             -  IF 'FILL' THEN FILL IN WHEN CENTRAL POINT IS BDVAL
C             -  If 'REPLACE' then replace central point with local
C                standard deviation computed over filtering region.
C     NMFILT  -  Number of fields to filter (input)
C     NUMFILT -  Number of fields to filter (local)
C     ICOPLANE-  TYPE OF SCAN AND OUTPUT COORDINATE SYSTEM
C     DDX,DDY -  RADIUS OF THE FILTER IS SQRT(DDX*DDX+DDY*DDY)
C                RADR - [DDX=(C3+1)*DRG, DDY=(C4+1)*DA*TORAD*RANGE]
C                CART - [DDX=C3,        DDY=C4]
C     DXX,DYY -  TOTAL DIMENSIONS OF FILTER AREA (Variables not used)
C                RADR - [DXX=2*C3+1, DYY=2*C4+1] (GATES BY BEAMS)
C                CART - [DXX=2*C3,   DYY=2*C4]   (KM BY KM)
C
C     MAXFLD  - Maximum number of input or output fields.
C     MAXVAL  - Maximum number of (RANGE GATES + 10)*MAXFLD that can
C               be stored in memory
C     MAXRNG  - Maximum number of range gates
C     MAXBM   - Maximum number of beams that can be stored in memory
C**********************************************************************
C     Interpretation of Coplane and (GRIDPPI,GRIDLLE,GRIDLLZ) flags.
C     These flags are set in CRTSET.
C
C     ICOPLANE = 0  ==>  R,A,E SCANS, INTERPOLATING TO CART OR ELEV GRID
C     ICOPLANE = 1  ==>  COPLANE SCANS, INTERPOLATING TO ANGLES IN DATA
C     ICOPLANE = 2  ==>  COPLANE SCANS, INTERPOLATING TO USER SPEC. ANGLES
C     ICOPLANE = 3  ==>  COPLANE SCANS, INTERPOLATING TO CART GRID
C                        Set in RPNCAR (FOF RP field format) or UFNCAR (UF)
C                        according to sweep mode (2) coplane or (3) RHI
C     ICOPLANE = 4  ==>  RHI SCANS, INTERPOLATING TO CART GRID
C                        Set in RPNCAR (FOF RP field format) or UFNCAR (UF)
C                        according to sweep mode (2) coplane or (3) RHI
C                        Uses COPLANE interpolation code so that azimuth
C                        in the RHIs is 90 - elevation angle.
C     ICOPLANE = 5  ==>  AIRBORNE SWEEPS, INTERPOLATING TO CART GRID
C                        Set in RADAR (DORADE format)
C     IPPI     = 0  ==>  Normal interpolations to cartesian grid
C     IPPI     = 1  ==>  XY interpolations to original elevation surfaces
C     ILLE     = 1  ==>  LonLat interpolations to original elevation surfaces
C     ILLZ     = 1  ==>  LonLat interpolations to constant height surfaces
C**********************************************************************

      INCLUDE 'SPRINT.INC'
c      PARAMETER (MAXEL=150,NID=129+3*MAXEL)
c      PARAMETER (NIOB=85000,MAXIN=8500,MAXLEN=MAXIN/4)
c      PARAMETER (MAXRNG=1024,MAXFLD=16)
c      PARAMETER (BDVAL=-32768.)

C     2004.0527 (ljm) - Changed MAXBM from 51 to 400 so that the entire
C                       sweep could be read into memory with a single
C                       call to LOADBMS.  The partial load, followed by
C                       additional calls to LOADBMS in the DO 500 loop
C                       was preventing the filtered data from extending
C                       out to ranges of the unfiltered and interpolated
C                       data.  Unable to find exactly what caused this
C                       problem, but chaning MAXBM fixed it.
C
      PARAMETER (MAXVAL=(MAXRNG+10)*MAXFLD,MAXBM=400)
      DIMENSION DATA(MAXVAL,MAXBM),ISPEC(MAXFLD)
      DIMENSION KPCK2(NIOB)
      CHARACTER*8 IFLDS(MAXFLD),OFLDS(MAXFLD),NSCTP(MAXFLD)
      CHARACTER*8 FSPACE(MAXFLD),IPROC(MAXFLD),NAMTIM
      COMMON /IDBLK/  ID(NID)
      COMMON /SCRDSK/ LTMP
      COMMON /FORMAT/ IRP,IBLOCK
      COMMON /FILT/ ISTFLG,NRAYS,IDIR,C,NLEN,NUMFILT,INPFLD(MAXFLD),
     X              SCLFLD(MAXFLD),MTMP
      COMMON /IO/KPCK(NIOB),KOUT(MAXIN),IBUF(MAXIN),NBUF(MAXLEN,4),
     X     IZ8(17),ILSTREC
      DATA NAMTIM/'TIME'/

      COMMON /CFLD/NFLDS
      COMMON /CFLDC/ IFIELD(MAXFLD), INTINF(MAXFLD,3)
      CHARACTER*8 INTINF,IFIELD

      COMMON /CRNG/ELTUS,MNBEM,RUSR1,RUSR2,RG1,RG2,DRG,NRG,VNYQ,
     X     RNOTUS,DRGUS,CFAC1,CFAC2,CFAC3
      COMMON /CRNGC/ IRCFXL
      CHARACTER*8 IRCFXL

      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X   ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      DIMENSION IFLTYP(MAXFLD),C3(MAXFLD),C4(MAXFLD)
      DIMENSION VNYQUIST(MAXEL)
      DIMENSION BMOUT(MAXVAL)
      INTEGER CNT,CNTMN,RCNT
      CHARACTER*8 CTEMP1
      DATA TORAD,CNTMN,EPS/0.017453293, 3, 1.0E-6/
      DATA SF/100.0/

C
C     THE FOLLOWING FUNCTION CALCULATES HOR. AZ. FROM COP. ANG. AND AZ. IN COP
C     The variables IDIR (scanning direction) and C (fixed scan angle,
C     usually the elevation angle of the sweep) are set inside LOADBMS.
C
      AZBM(C,A)=ATAN(COS(C*TORAD)*TAN(A*TORAD))/TORAD

      DATMXSIZ=NRCBF*8.
C
C     TRANSFER NUMBER OF FILTERS TO COMMON BLOCK VARIABLE
C
      NUMFILT =NMFILT
C
C     ENCODE CHAR. VARS AS INTS FOR VECTORIZATION PURPOSES
C
      DO I=1,MAXFLD
         ISPEC(I)=0
         IF (IPROC(I).EQ.'UNFOLD') ISPEC(I)=1
         IF (IPROC(I).EQ.'LINEAR') ISPEC(I)=2
      END DO

      NFLINP=NFLDS
      IF (IFIELD(NFLDS).EQ.NAMTIM) NFLINP=NFLINP-1
C
C     CLEAR ARRAY TO ZEROES
C
      DO I=1,MAXBM
         DO J=1,MAXVAL
            DATA(J,I)=0.0
         END DO
      END DO
      NSWPS=ID(35)
      NRHD=ID(37)
      NLEN=NRHD+(ID(75)*MAXRNG)
      NFLDS=ID(75)
      NOFLDS=NFLDS
c-----debug (ljm)
c      if(debug)then
         write(8,*)'Dofilt: nswps,nrhd,nlen,nflds=',
     +        nswps,nrhd,nlen,nflds
         write(8,*)'Dofilt: nflds,nmfilt,icoplane=',
     +        nflds,numfilt,icoplane
         write(8,*)'Dofilt: mxcrt,maxpln,nrcbf=',mxcrt,maxpln,nrcbf
         do j=1,numfilt
            write(8,*)'Dofilt: ifl,ofl,iflt=',
     +           iflds(j),oflds(j),ifltyp(j)
            write(8,*)'Dofilt: iproc,fspace,nsctp=',
     +           iproc(j),fspace(j),nsctp(j)
            write(8,*)'Dofilt: c3,c4=',c3(j),c4(j)
         end do
         write(8,*)'Dofilt: nswps,vnyq=',nswps,vnyq
         write(8,*)'Dofilt: vnyquist=',(vnyquist(n),n=1,nswps)
         call flush(6)
c      end if
c-----debug (ljm)
C     
C     SCRATCH DISK IS UNIT 3
C     
      MTMP=3
      WRITE(MTMP) MTMP
      REWIND MTMP
C
C     IDENTIFY ADDRESSES IN BEAMS AND SCALE FACTORS OF INPUT FIELDS
C
      WRITE(*,15)(IFLDS(I),I=1,NUMFILT)
 15   FORMAT(/,1X,'FILTERING ABOUT TO BEGIN FOR FIELDS: ',16A8/)
      DO 60 I=1,NUMFILT
c         WRITE(*,17)IFLDS(I)
c 17      FORMAT(5X,A8)
         DO J=1,NFLDS
c            WRITE(CTEMP1,21)ID(76+(J-1)*5),ID(77+(J-1)*5)
            JJ=(J-1)*5
            WRITE(CTEMP1,21)ID(IDPTR_INT+JJ),ID(IDPTR_INT+1+JJ)
 21         FORMAT(2A4)
            IF (CTEMP1.EQ.IFLDS(I)) GOTO 50
         END DO
 50      CONTINUE
         IF (J.LE.NFLDS .AND. J.GT.0) THEN
            INPFLD(I)=ID(37) + (J-1)*MAXRNG + 1
            JJ=(J-1)*5
c            SCLFLD(I)=ID(80+(J-1)*5)
            SCLFLD(I)=ID(IDPTR_INT+4+JJ)
c-----------VNYQUIST(I)=ID(78+(J-1)*5)/100.0
c-----------VNYQUIST(I)=ID(IDPTR_INT+2+JJ)/100.0
         ELSE
            WRITE(*,55)IFLDS(I)
 55         FORMAT(//,5X,'+++ERROR LOCATING FIELD:',A8,' FOR ',
     X           'FILTERING+++')
            STOP
         END IF
 60   CONTINUE

      NRAYS=0
      CALL WRRYDK(KPCK2,KOUT,NST,MTMP,-9,NLEN)
C
C     600: Top of loop over sweeps
C
c-----debug(ljm)
      print *,'DOFILT: Loop (600) over nswps=',nswps
c-----debug(ljm)
      DO 600 I=1,NSWPS
         VNY=VNYQUIST(I)
         VNY2=2.0*VNYQUIST(I)
         IF (I.EQ.1) THEN
c            NRAYS=ID(131)
            NRAYS=ID(IPTR_INT+2)
         ELSE
c            NRAYS=ID(131+3*(I-1))-ID(131+3*(I-2))
            NRAYS=ID(IPTR_INT+2+3*(I-1))-ID(IPTR_INT+2+3*(I-2))
         END IF
c--------debug(ljm)
         print *,' '
         print *,'DOFILT: swp#,nrays,vny,vny2=',i,nrays,vny,vny2
         if(debug)then
            write(8,*)'Dofilt: i-swp#,nrays,vny,vny2=',i,nrays,vny,vny2
         end if
c--------debug(ljm)
C     
C     Read MAXBM beams into memory (Stored in DATA)
C
         IOP=0
         CALL LOADBMS(IOP,I,DATA,MAXVAL,MAXBM)
         NREAD=MIN(MAXBM,NRAYS)
C     
C     CALCULATE AVERAGE HORIZONTAL AZIMUTH SPACING
C     
         IF (ICOPLANE.EQ.0 .OR. ICOPLANE.EQ.3 .OR. ICOPLANE.EQ.4)THEN

C           PPI, COPLANE, or RHI scans --> Cartesian grid
C
            AZ1=DATA(1,1)
            AZ2=DATA(1,MIN(MAXBM,NRAYS))

         ELSE IF (ICOPLANE.EQ.1 .OR. ICOPLANE.EQ.2) THEN

C           COPLANE --> Grid within coplanes
C
            AZ1=AZBM(C,DATA(1,1))
            AZ2=AZBM(C,DATA(1,MIN(MAXBM,NRAYS)))
         END IF

         IF (IDIR.EQ.1 .AND. (AZ2.GE.AZ1)) THEN
            DIFF=AZ2-AZ1
         ELSE IF (IDIR.EQ.-1 .AND. (AZ2.GE.AZ1)) THEN
            DIFF = 360.0 - (AZ2-AZ1)
         ELSE IF (IDIR.EQ.1 .AND. (AZ2.LT.AZ1)) THEN
            AZ2=AZ2+360.0
            DIFF = (AZ2-AZ1)
         ELSE
            AZ2=AZ2+360.0
            DIFF = 360.0 - (AZ2-AZ1)
         END IF
c--------debug(ljm)
         print *,'DOFILT-RorC:i,idir,c,vny=',i,idir,c,vny
         print *,'DOFILT-RorC: maxbm,nrays=',maxbm,nrays
         print *,'DOFILT-RorC:   az12,diff=',az1,az2,diff
c--------debug(ljm)
         DA=ABS(DIFF)/MIN(MAXBM,NRAYS)

C     
C     NOW DO THE FILTERING OVER RAYS (outer J-loop)
C     Top of rays-within-the-sweep loop
C     
         INDXBM=0
         DO 500 J=1,NRAYS
            INDXBM=INDXBM+1

C     Continue loading one beam at a time if past
C     the initial call to LOADBMS of MAXBM rays.
C
            IF (INDXBM.GT.MAXBM) THEN
               IOP=1
               CALL LOADBMS(IOP,I,DATA,MAXVAL,MAXBM)
               NREAD=NREAD+1
               INDXBM=INDXBM-1
            END IF
            IF (ICOPLANE.EQ.0 .OR. ICOPLANE.EQ.3 .OR. ICOPLANE.EQ.4)THEN
               SINIJ=SIN(DATA(1,INDXBM)*TORAD)
               COSIJ=COS(DATA(1,INDXBM)*TORAD)
            ELSE
C     
C     GET HORIZONTAL AZIMUTH 
C     
               IF (DATA(1,INDXBM).NE.90.0) THEN
                  AZ=AZBM(C,DATA(1,INDXBM))
               ELSE
                  AZ=90.0
               END IF
               SINIJ=SIN(AZ*TORAD)
               COSIJ=COS(AZ*TORAD)
            END IF
            DO INIT=1,NLEN
               BMOUT(INIT)=DATA(INIT,INDXBM)
            END DO
C     
C     Top of loop (inner LR-loop) over range within the current ray
C     
            NRG=INT(DATA(8,INDXBM))
c-----------debug(ljm)
            if(debug)then
               write(8,*)'Dofilt: begin rng loop-nrg=',nrg
            end if
c-----------debug(ljm)
            DO 480 LR=1,NRG
C     
C     SET ALL FIELDS TO BE FILTERED TO BAD INITIALLY
C        Distances to central gate #LR:
C           RIJ, XIJ, YIJ - slant range, X(km), Y(km)
C
               DO K=1,NUMFILT
                  BMOUT(LR+INPFLD(K)-1)=BDVAL
               END DO
               RIJ=RG1 + (LR-1)*DRG
               XIJ=RIJ*SINIJ
               YIJ=RIJ*COSIJ
               IF (RIJ.LE.0.0) GOTO 480
               
               IF (FSPACE(1).EQ.'RADR') THEN
                  IDEL=NINT(C3(1))
                  IF (IDEL.LE.0) THEN
                     DDX=0.0
                  ELSE
                     DDX=(C3(1)+1.0)*DRG
                  END IF
                  JDEL=NINT(C4(1))
C     
C     LIMIT JDEL TO HALF THE MAX. # OF BEAMS IN MEMORY AT A TIME
C     
                  JDEL=MIN(JDEL,(MAXBM-1)/2)
                  IF (JDEL.LE.0) THEN
                     DDY=0.0
                  ELSE
                     DDY=(C4(1)+1.0)*DA*TORAD*RIJ
                  END IF
c-----------------debug(ljm)
                  if(j.eq.1 .and. lr.eq.1)then
                     print *,'DOFILT-RADR:  nrg,drg,da=',nrg,drg,da
                     print *,'DOFILT-RADR:     rg1,rg2=',rg1,rg2
                     print *,'DOFILT-RADR:       c3,c4=',c3(1),c4(1)
                     print *,'DOFILT-RADR:   idel,jdel=',idel,jdel
                     print *,'DOFILT-RADR:radii(ddx&y)=',ddx,ddy
                  end if
c-----------------debug(lj)m
               ELSE
                  IDEL=NINT(C3(1)/DRG)
                  IF (IDEL.LE.0) THEN
                     DDX=0.0
                  ELSE
                     DDX=C3(1)
                  END IF
                  JDEL=NINT(C4(1)/(RIJ*DA*TORAD))
                  JDEL=MIN(JDEL,(MAXBM-1)/2)
                  IF (JDEL.LE.0) THEN
                     DDY=0.0
                  ELSE
                     DDY=C4(1)
                  END IF
c-----------------debug(ljm)
                  if(j.eq.1 .and. lr.eq.1)then
                     print *,'DOFILT-CART:  nrg,drg,da=',nrg,drg,da
                     print *,'DOFILT-CART:     rg1,rg2=',rg1,rg2
                     print *,'DOFILT-CART:       c3,c4=',c3(1),c4(1)
                     print *,'DOFILT-CART:   idel,jdel=',idel,jdel
                     print *,'DOFILT-CART:radii(ddx&y)=',ddx,ddy
                  end if
c-----------------debug(ljm)
               END IF
C
C     Compute inner range-loop indices for filter centered on current location
C               
               I1=LR-IDEL
               I2=LR+IDEL
               IF (I1.LT.1) I1=1
               IF (I2.LT.1) I2=1
               IF (I1.GT.NRG) I1=NRG
               IF (I2.GT.NRG) I2=NRG
               I1SAV=I1
               I2SAV=I2
C
C     Compute inner angle-loop indices for filter centered on current location
C               
               J1=INDXBM-JDEL
               J2=INDXBM+JDEL
               IF (J1.LT.1) J1=1
               IF (J2.LT.1) J2=1
               IF (J1.GT.NRAYS) J1=NRAYS
               IF (J2.GT.NRAYS) J2=NRAYS
               IF (J2.GT.MAXBM) THEN
                  IF (NREAD.LT.NRAYS) THEN
                     IOP=1
                     CALL LOADBMS(IOP,I,DATA,MAXVAL,MAXBM)
                     NREAD=NREAD+1
                     INDXBM=INDXBM-1
                     J1=J1-1
                     J2=J2-1
                     IF (J1.LT.1 .OR. J2.GT.MAXBM .OR. 
     X                    J2.GT.NRAYS) THEN
                        WRITE(*,65)J1,J2,MAXBM,NRAYS
 65                     FORMAT(/,5X,' +++INVALID STATE IN DOFILT.'
     X                       ,' CONTACT PROGRAMMER+++')
                        STOP
                     END IF
                  ELSE
                     J2=MAXBM
                  END IF
               END IF
               BIGR=DDX*DDX+DDY*DDY
               IF (BIGR.EQ.0.0) THEN
                  WRITE(*,67)DDX,DDY
 67               FORMAT(//,5X,'+++ERROR: CHECK FILTER SPECS,',
     X                 'BIGR IS ZERO+++',' DDX,DDY=',2F8.3/)
                  STOP
               END IF
C     
C     Inner loop over fields to be filtered at the current (i,j) sample point
C     
               DO 450 K=1,NUMFILT
                  L=LR+INPFLD(K)-1
                  IOP=1
                  IF (DATA(L,INDXBM).EQ.BDVAL .AND. 
     X                 NSCTP(K).EQ.'NOFILL') GOTO 450
                  IF (ISPEC(K).EQ.1)THEN
                     IF(DATA(L,INDXBM).NE.BDVAL)THEN
                        VREF=DATA(L,INDXBM)
                     ELSE IF(DATA(L-1,INDXBM).NE.BDVAL)THEN
                        VREF=DATA(L-1,INDXBM)
                     ELSE IF(DATA(L+1,INDXBM).NE.BDVAL)THEN
                        VREF=DATA(L+1,INDXBM)
                     ELSE
                        VREF=0.5*VNY
                     END IF
                  END IF
C     
C     EXECUTE THE INNER SUMMATIONS TO OBTAIN WEIGHTED SUM OF
C     SAMPLES WITHIN SQRT(BIGR) OF THE CURRENT (I,J) SAMPLE POINT.
C     
                  CNT=0
                  SUMW=0.0
                  WSUM=0.0
                  SUM=0.0
                  SUMSQ=0.0

C                 300: Outer loop over scanning angles surrounding
C                      the central point at (R,A)=(I,J)
C
                  DO 300 JJ=J1,J2
                     IOP=1
                     IF (ICOPLANE.EQ.0 .OR. ICOPLANE.EQ.3 .OR. 
     X                    ICOPLANE.EQ.4) THEN
                        SINA=SIN(DATA(1,JJ)*TORAD)
                        COSA=COS(DATA(1,JJ)*TORAD)
                     ELSE
                        IF (DATA(1,JJ).NE.90.0) THEN
                           AZ=AZBM(C,DATA(1,JJ))
                        ELSE
                           AZ=90.0
                        END IF
                        SINA=SIN(AZ*TORAD)
                        COSA=COS(AZ*TORAD)
                     END IF
                     RCNT=0

C                    280: Outer loop over range gates surrounding
C                      the central point at (R,A)=(I,J)
C
                     DO 280 II=I1,I2
                        DTMP=DATA(II+INPFLD(K)-1,JJ)
                        IF (DTMP.NE.BDVAL) THEN
                           RNG=RG1+(II-1)*DRG
                           XX=RNG*SINA
                           YY=RNG*COSA
                           SMALR=(XX-XIJ)**2+(YY-YIJ)**2
                           IF(SMALR.LE.BIGR.AND.RNG.GT.0.0)THEN
C     
C     PROCEED WITH SUMMATION OF (II,JJ) LOCATIONS SURROUNDING CURRENT
C     LOCATION.  UNFOLD VELOCITIES IF NECESSARY.
C     
                              IF (ISPEC(K).EQ.2) THEN
                                 DMEA=10.0**(0.1*DTMP)
                              ELSE IF (ISPEC(K).EQ.1) THEN
                                 VDIF=VREF-DTMP
                                 IF(ABS(VDIF).GT.VNY)THEN
                                    FAC=VDIF/VNY2
                                    IF(FAC.GT.0.0)THEN
                                       DMEA=DTMP+VNY2
                                    ELSE IF(FAC.LT.0.0)THEN
                                       DMEA=DTMP-VNY2
                                    END IF
c-----------------------------------debug(ljm)
                                    if(debug)then
                                       write(8,*)
     +                                      'Dofilt: a,r,vref,vin,vuf=',
     +                                      j,lr,vny,vref,dtmp,dmea
                                    end if
c-----------------------------------debug(ljm)
                                 ELSE
                                    DMEA=DTMP
                                 END IF
                              ELSE
                                 DMEA=DTMP
                              END IF
                              
                              IF (IFLTYP(K).EQ.1) THEN
                                 DWT=1.0
                              ELSE IF (IFLTYP(K).EQ.2) THEN
                                 DWT=1.0 - SQRT(SMALR/BIGR)
                              ELSE IF (IFLTYP(K).EQ.3) THEN
                                 DWT=(BIGR-SMALR)/(BIGR+SMALR)
                              ELSE IF (IFLTYP(K).EQ.4) THEN
                                 DWT=1-(SMALR/BIGR)
                              ELSE
                                 DWT=EXP(-4.0*SMALR/BIGR)
                              END IF
                              RCNT=RCNT+1.0
                              CNT=CNT+1
                              WSUM=WSUM+DMEA*DWT
                              SUMW=SUMW+DWT
                              SUM=SUM+DMEA
                              SUMSQ=SUMSQ+DMEA*DMEA
                           ENDIF
                        ENDIF
 280                 CONTINUE
 300              CONTINUE
C     
C 280/300: End loop over local filtering region surrounding
C          the central input data point at (R,A)=(I,J).
C               
                  IF (CNT.LT.CNTMN .OR. SUMW.LT.EPS) GOTO 450
                  IF(NSCTP(K).EQ.'REPLACE')THEN
                     AVG=SUM/CNT
                     VAR=(SUMSQ-CNT*AVG*AVG)/(CNT-1)
                     IF(VAR.GE.0.0)BMOUT(L)=SQRT(VAR)
                  ELSE
                     BMOUT(L)=WSUM/SUMW
                  END IF
C     
C     UNDO ANY SPECIAL PRE-FILTER TRANSFORMATION
C     
                  IF (ISPEC(K).EQ.2) THEN
                     IF (BMOUT(L).GT.0.0) THEN
                        BMOUT(L)=10.0*ALOG10(BMOUT(L))
                     ELSE
                        BMOUT(L)=BDVAL
                     END IF
                  END IF
 450           CONTINUE
 480        CONTINUE
C     
C     450/480: End field/range loop within the current ray
C     A BEAM IS FINISHED; SCALE THINGS BACK
C     
            NRG=NINT(BMOUT(8))
c-----------debug(ljm)
            if(debug)then
               write(8,*)'Dofilt: end fld/rng loop-nrg=',nrg
            end if
c-----------debug(ljm)
            DO IF=1,NUMFILT
               ISTART=INPFLD(IF)
               DO IRN=1,NRG
                  IRM=ISTART+IRN-1
                  IF (BMOUT(IRM).NE.BDVAL)THEN
                     BMOUT(IRM)=SCLFLD(IF)*BMOUT(IRM)
                  END IF
               END DO
            END DO

C     SCALE AZIMUTH
C     
            KOUT(1)=NINT(BMOUT(1)*64.)
C
C     TRANSFER TO KOUT AND "COMPRESS" BACK TO ACTUAL NUMBER OF GATES
C
c-----------print *,'DOFILT: #gates-ID(37),nrg=',id(37),nrg
            DO IT=2,ID(37)
               KOUT(IT)=NINT(BMOUT(IT))
            END DO
            DO IT=1,NFLINP
               ISTRT=(IT-1)*KOUT(8) + ID(37)
               JSTRT=(IT-1)*MAXRNG  + ID(37)
               DO K=1,NRG
                  KOUT(ISTRT+K)=NINT(BMOUT(JSTRT+K))
               END DO
           END DO
C
C     WRITE OUT BEAM
C     
            NLEN=ID(37) + NFLINP*(KOUT(8))
c-----------debug(ljm)
            if(j.eq.1 .or. j.eq.nrays)then
               print *,'DOFILT: #,j,len,nf,nrg,mxrg=',
     +              mtmp,j,nlen,nflinp,kout(8),maxrng
            end if
c-----------debug(ljm)
            CALL WRRYDK(KPCK2,KOUT,NST,MTMP,0,NLEN)

 500     CONTINUE
C
C     500: Bottom of rays-within-the-sweep loop
C
 600  CONTINUE
C
C     600: Bottom of loop over sweeps
C     FLUSH LAST RECORD TO DISK
C
      CALL WRRYDK(KPCK2,KOUT,NST,MTMP,9,NLEN)
C     
C     SWAP FILE POINTERS
C     
      LTMP=MTMP
      REWIND LTMP
      
      RETURN
      END 

 




