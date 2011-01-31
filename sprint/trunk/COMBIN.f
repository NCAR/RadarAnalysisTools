      SUBROUTINE COMBIN(IUPR,ILWR,IOB,IEL,IRNG,KZLV,IVV1,IVV2,MXFLD)
C
C     Finishes interpolation between the upper and lower scan planes
C        IUPR (IDATH)  - Previously interpolated values for upper plane
C        ILWR (IDATL)  -      "           "         "    "  lower   "
C                        except TRPPPI reverses the order IDATH --> ILWR 
C                                                and IDATL=IBAD --> IUPR
C        IOB  (IDATCB) - Output of final interpolation at a XY-point for
C                        NFLI fields.
C        Note: Both IDATH and IDATL were bilinear (4-pt) interpolations
C              done by calls to TRPD from one of the main interpolators:
C              TRPVOL (ground-based scans --> xyz), TRPARVOL (airborne 
C              scans --> xyz), or TRPPPI (ground-based scans --> xye).
C
C     ICOPLANE = 0  ==>  R,A,E SCANS, INTERPOLATING TO CART GRID
C     ICOPLANE = 1  ==>  COPLANE SCANS, INTERPOLATING TO ANGLES IN DATA
C     ICOPLANE = 2  ==>  COPLANE SCANS, INTERPOLATING TO USER SPEC. ANGLES
C     ICOPLANE = 3  ==>  COPLANE SCANS, INTERPOLATING TO CART GRID
C     ICOPLANE = 4  ==>  RHI SCANS, INTERPOLATING TO CART GRID
C     ICOPLANE = 5  ==>  AIRBORNE SWEEPS, INTERPOLATING TO CART GRID
C     IPPI     = 0  ==>  Normal interpolations to cartesian grid
C     IPPI     = 1  ==>  XY interpolations to constant elevation surfaces
C     ILLE     = 1  ==>  LonLat interpolations to constant elevation surfaces
C     ILLZ     = 1  ==>  LonLat interpolations to constant height surfaces
C
      INCLUDE 'SPRINT.INC'
c-----PARAMETER (MAXEL=150,NID=129+3*MAXEL)
c-----PARAMETER (IDIM=64/WORDSZ,MAXWRD=IDIM*WORDSZ/INTSZ)
c-----PARAMETER (MAXFLD=16) 
c-----DATA IBAD/-32768/

      DIMENSION IUPR(MXFLD),ILWR(MXFLD),IOB(MXFLD),IVV1(IDIM),IVV2(IDIM)
      LOGICAL IS360
      COMMON/ADJUST/INFLD(MAXFLD,3),SCLIF(MAXFLD,2),NIF,IOFLD(MAXFLD,3),
     X     SCLOF(MAXFLD,2),NOF,SCLAZ,SCLRNG,SCLEL,UNSCAZ,UNSCRG,UNSCEL,
     X     LOWAZ,IZAD,IS360,MINAZ,MAXAZ,METSPC(MAXFLD),IWFLD(MAXFLD),
     X     NFLI,SCLNYQ,UNSNYQ
      COMMON /FNAMES/ CINFLD(MAXFLD),CIOFLD(MAXFLD)
      CHARACTER*8 CINFLD, CIOFLD
      COMMON /SCNDAT/ ELB(MAXEL),DEI(MAXEL),KDIR(MAXEL),KNDREC(MAXEL),
     X     NEL,IEL1,IEL2,KEL,KPEL,ISD,ELTOL,RNOT,DRG,NG,MAXRD,
     X     CFAC(MAXFLD)
      COMMON /SCAN/ ICOPLANE,IFLGBAS,IPPI,ILLE,ILLZ
      EQUIVALENCE (METSPC(1),METH),(METSPC(4),MXREST)
      CHARACTER*8 NAMQAL
      DATA ATR/0.017453293/
      DATA NAMQAL/'QUAL'/

      FRUP=IEL*SCLEL
      FRLO=1.-FRUP
c-----debug (ljm)
c      write(8,*)'Combin: kpel,isd,iel,nfli,frup,frlo=',
c     +                   kpel,isd,iel,nfli,frup,frlo
c-----debug (ljm)

      IF(FRUP.GE.0.5) THEN
C     
C     CLOSER TO UPPER SCAN PLANE: MXELD = maximum clst-pt distance
C     
         IUPL=1
         MXELD=IRNG*SIN(ABS(ELB(KPEL)-ELB(KPEL-ISD))*FRLO*ATR)
      ELSE
C     
C     CLOSER TO LOWER SCAN PLANE: MXELD = maximum clst-pt distance
C     
         IUPL=0
         MXELD=IRNG*SIN(ABS(ELB(KPEL)-ELB(KPEL-ISD))*FRUP*ATR)
      END IF
C
C     COMPUTE FINAL CARTESIAN ESTIMATE
C
      DO 22 I=1,NFLI
         IUPEST=IUPR(I)
         ILWEST=ILWR(I)
         iupper=iupr(i)
         ilower=ilwr(i)
         IF(METH.EQ.2) GO TO 19
         L=IWFLD(I)
         IF(L.LT.0) THEN
            IUPEST=NINT(IUPEST*FRUP+ILWEST*FRLO)
            GO TO 20
         END IF
         IF(INFLD(L,3).NE.0.AND.INFLD(L,2).EQ.3) THEN
C
C     NORMALIZE VELOCITIES
C
            VNYQ=ABS(CFAC(L))
            IVIND=I
            VSCL=0.125
         END IF
         IF(ICEDAND(IUPEST,1).EQ.0.OR.ICEDAND(ILWEST,1).EQ.0) GO TO 19
C
C     GOOD ESTIMATES- PROCEED WITH INTERPOLATION
C
         IF(INFLD(L,3).NE.0.AND.INFLD(L,2).LE.2) THEN
C
C     10(LOG) FIELD TO BE CONVERTED TO LINEAR
C
            SFAC=0.1*SCLIF(L,2)
            IUPEST=ALOG(EXP(IUPEST*SFAC)*FRUP+
     X                  EXP(ILWEST*SFAC)*FRLO) * SCLIF(L,1) * 10.
         ELSE
C
C     NO CONVERSION
C
            IUPEST=NINT(IUPEST*FRUP+ILWEST*FRLO)
         END IF
         GO TO 20
 19      CONTINUE
C
C     SELECT THE CLOSEST POINT IF WITHIN RANGE
C           Larger of user-specified distance or gate spacing.
C           INTERP: DMIN*UNSCRG --> METSPC(6)
C           METHOD: MAX(SCLRNG*METSPC(6),DRG) --> METSPC(4)
C           TRPD:   METSPC(4) --> MXREST
C
         IF(IUPL.EQ.0) IUPEST=ILWEST
         IF(MXELD.GT.MXREST) IUPEST=IBAD
 20      IUPR(I)=IUPEST
c-----debug (ljm)
c         write(8,*)'Combin: meth,i,up,lw,trp=',meth,i,
c     +        iupper,ilower,iupest
c-----debug (ljm)
 22   CONTINUE

C
C     DERIVE FIELDS IF NECESSARY AND THRESHOLD FINAL RESULTS
C
      DO 30 I=1,NOF

C     Miller, Mohr, and Weinheimer, 1986: The Simple Rectification to 
C     Cartesian Space of Folded Radial Velocities from Doppler Radar
C     Sampling. JTECH, 3, 162-174.
C        QUAL is a measure of the normalized variance of velocity 
C        measurements that are used for the output grid estimate.
C        Velocities from a pulse-pair estimator should be uniformly
C        distributed between +Vnyquist and -Vnyquist.  The expected 
C        value of variance would then be VarN=Vn**2/3
C
C     QUAL = 1-Var(v)/VarN, where Var(v) is variance of velocity
C        The standard deviations are used instead of variance to
C        prevent overflow of packed values.  QUAL is also multiplied
C        by 10.
C
         IF(CIOFLD(I).EQ.NAMQAL) THEN
            IOB(I)=IBAD
            IF(IUPR(IVIND).EQ.IBAD) GO TO 30
            CALL IVVUNP(IVV1,IDIM,DF1,SUM1,SUMSQ1)
            CALL IVVUNP(IVV2,IDIM,DF2,SUM2,SUMSQ2)
c            IOB(I)=0
            CNT1=IFIX(DF1)
            CNT2=IFIX(DF2)
            DF1=DF1-CNT1
            DF2=DF2-CNT2
c-----------debug (ljm)
cqual-------write(6,*)'COMBIN: mxfld,nof,ibad=',mxfld,nof,ibad
cqual-------write(6,*)'COMBIN:    i,ciofld(i)=',i,ciofld(i)
cqual-------write(6,*)'COMBIN: ivind,icoplane=',ivind,icoplane
cqual-------write(6,*)'COMBIN:      ivv1,ivv2=',ivv1,ivv2
cqual-------write(6,*)'COMBIN:        df1,df2=',df1,df2
cqual-------write(6,*)'COMBIN:      cnt1,cnt2=',cnt1,cnt2
c-----------debug (ljm)
            IF ((ICOPLANE.EQ.0 .OR. ICOPLANE.EQ.2 .OR. 
     X           ICOPLANE.EQ.3 .OR. ICOPLANE.EQ.4) .AND. 
     X           IPPI.EQ.0) THEN
C
C     FOR 3-D INTERPOLATIONS
C
               IF(CNT1.EQ.0.0.OR.CNT2.EQ.0.0) GO TO 30
               CNT=CNT1+CNT2
               DIV=1./CNT
               AVG=(SUM1+SUM2)*DIV
               TEST=(SUMSQ1+SUMSQ2)*DIV-(AVG*AVG)
               IF(CNT.GT.1.0) TEST=(TEST*CNT)/(CNT-1.0)
cqual----------print *,'COMBIN:cnt,avg,test=',cnt,avg,test
            ELSE IF (ICOPLANE.EQ.1 .OR. IPPI.EQ.1) THEN
C     
C     FOR 2-D INTERPOLATION (IN COPLANES)
C     
               IF(CNT1.EQ.0.0) GO TO 30
               CNT=CNT1
               DIV=1./CNT
               AVG=SUM1*DIV
               TEST=SUMSQ1*DIV - (AVG*AVG)
               IF(CNT.GT.1.0) TEST=(TEST*CNT)/(CNT-1.0)
            END IF
C
C+++Use standard deviation instead of variance 
C+++when forming the QUAL field.  6/89 cgm
C 
               
            IF(TEST.GT.0.0) TEST = SQRT(3.0*TEST)
cqual-------write(6,*)'COMBIN:vnyq,test=',vnyq,test
            TEST = 1.0 - (TEST/VNYQ)
cqual-------write(6,*)'COMBIN:vnyq,test=',vnyq,test
            TEST=AMAX1(TEST,-3.0)
            TEST=IFIX(TEST*100.0)
            FRAC=(FRLO*DF1)**2 + (FRLO*(1.0-DF1))**2
     X         + (FRUP*DF2)**2 + (FRUP*(1.0-DF2))**2
            FRAC=AMAX1(0.0,FRAC-0.001)
            FRAC=SIGN(FRAC,TEST)
            IOB(I)=(TEST+FRAC)*SCLOF(I,1)
cqual-------print *,'COMBIN:cnt,vnyq,test,frac=',cnt,vnyq,test,frac
            GO TO 30
         END IF
         J=IABS(IOFLD(I,2))
         IUPEST=IUPR(J)
         IOB(I)=IUPEST
         IF(IOFLD(I,2).GE.0.OR.IUPEST.EQ.IBAD) GO TO 30
C
C     DERIVE REFLECTIVITY FIELD FROM POWER FIELD
C
         L=IWFLD(J)
         IF ((IRNG*SCLRNG).GT.0.0) THEN
            IOB(I)=(CFAC(L) + (IUPEST*SCLIF(L,2)) + 
     X           20.*ALOG10(IRNG*SCLRNG)) * SCLOF(I,1)
         ELSE
            IOB(I)=IBAD
         END IF
 30   CONTINUE
      RETURN
      END




