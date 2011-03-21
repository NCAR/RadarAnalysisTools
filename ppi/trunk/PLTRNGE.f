c
c----------------------------------------------------------------------X
c
      SUBROUTINE PLTRNGE(NAMFLD,NFLDS,PRMN,PRMX,PAMN,PAMX,ASKIP,IRNAM,
     X                   RFMN,RFMX,RREF,RPRNT,NRP,COLRFIL,PLTSW,NFRAME,
     X                   IAZC,IFLD,BGFLAG,LTYP)
C
C  PLOT SEVERAL ANGLES AS A FUNCTION OF RANGE WITHIN THE CURRENT SWEEP
C
C     NRP       - NUMBER OF FIELDS PER FRAME
C     IRNAM     - NAMES   "    "
C     RFMN,RFMX - MINIMUM AND MAXIMUM FIELD VALUES FOR PLOT BOUNDARIES
C     RREF      - DASHED REFERENCE LINE(S) ON PLOTS
C     IRTYPE    - TYPE OF PLOT (RNGE OR ANGL)
C     PRMN,PRMX - MINIMUM AND MAXIMUM RANGES (KM) FOR PLOT BOUNDARIES
C     PAMN,PAMX -    "     "     "    ANGLES TO BE PLOTTED
C     ASKIP     - ANGLE SKIPPING FACTOR WITHIN THE SWEEP
C     LTYP      - TYPE OF CONNECTION BETWEEN POINTS (LINE or DOTS)
C               - DOTS is the default
C       NANG(1) - NUMBER OF ANGLES IN THE CURRENT SWEEP
C
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'colors.inc'

      CHARACTER*1 BGFLAG
      CHARACTER*6 IFMTX,IFMTY
      LOGICAL COLRFIL,PLTSW,IAZC
      CHARACTER*8 NAMFLD(MXF),IRNAM(MXF),RPRNT(MXF)
      CHARACTER*8 LTYP
      CHARACTER*4 IRTYPE
      DIMENSION RFMN(MXF),RFMX(MXF),RREF(MXF,5),RFYB(MXF),RFYT(MXF)
      DIMENSION FLD(MXR),IFLD(MXF)
      DATA XRT,YTP,SIDE/0.970,0.940,0.84/

      CALL DASHDB (O'052525')
      IRTYPE='RNGE'
      COLRFIL=.FALSE.
      PLTSW=.FALSE.

C     Store PRMN and reset to 0.0
C
      PRMN_KP=PRMN
      PRMN=0.0

      IRB=1.001+(PRMN-R0)/DROLD
      IRE=1.001+(PRMX-R0)/DROLD
      IF(IRB.LT.1)IRB=1
      IF(IRE.GE.MXR)IRE=MXR-1
      X2=XRT
      X1=XRT-SIDE
      DDX=PRMX-PRMN
      CALL MAJMIN(PRMN,PRMX,IFMTX,MJRX,MNRX,IPLX)
      JSKIP=ASKIP
      DY=SIDE/NRP
      DO 10 N=1,NRP
         RFYT(N)=YTP-(N-1)*DY
         RFYB(N)=YTP-N*DY+0.02
   10 CONTINUE

      CALL GSPLCI(1)
      CALL GSTXCI(1)

C     LOOP OVER ALL ANGLES IN THE CURRENT SWEEP
C
      DO 200 J=1,NANG(1),JSKIP
         PANG=AZA(J,1)
         IF(PANG.LT.0.0)THEN
            TANG=PANG+360.0
         ELSE
            TANG=PANG
         END IF
         IF(TANG.LT.PAMN.OR.TANG.GT.PAMX)GO TO 200

C        LOOP OVER ALL FIELDS IN THE CURRENT BEAM
C
         DO 100 N=1,NRP
            Y2=RFYT(N)
            Y1=RFYB(N)
            DDY=RFMX(N)-RFMN(N)
            CALL MAJMIN(RFMN(N),RFMX(N),IFMTY,MJRY,MNRY,IPLY)
            CALL SET(X1,X2,Y1,Y2,PRMN,PRMX,RFMN(N),RFMX(N),1)
            CALL LABMOD(IFMTX,IFMTY,IPLX,IPLY,12,12,8,8,0)
            CALL GRIDAL(MJRX,MNRX,MJRY,MNRY,0,1,5,X1,Y1)

C           EXTRACT FIELD FOR PLOTTING AS A FUNCTION OF RANGE
C
            IFL=IFIND(IRNAM(N),NAMFLD,MXF)
            IF(IFLD(IFL).LT.0)THEN
               ISW=2
               PLTSW=.TRUE.
            ELSE
               ISW=1
               PLTSW=.FALSE.
            END IF
            DO M=1,5
               IF (RREF(N,M).EQ.BDVAL)GO TO 12
               IF(RREF(N,M).GT.RFMN(N).AND.RREF(N,M).LT.RFMX(N))THEN
                  IF(M.EQ.1)THEN
                     CALL LINE (PRMN, RREF(N,M), PRMX, RREF(N,M))
                  ELSE
                     CALL LINED (PRMN, RREF(N,M), PRMX, RREF(N,M))
                  END IF
               END IF
            END DO
 12         CONTINUE
            IF(RPRNT(N)(1:4).EQ.'PRNT')THEN
               READ(RPRNT(N)(5:8),14)IMOD,JMOD
 14            FORMAT(2I2)
               IF(IMOD.EQ.0)IMOD=9999
               IF(JMOD.EQ.0)JMOD=9999
            END IF
            DO 20 I=IRB,IRE
               RNG1=RNG(I,1)
               RNG2=RNG(I+1,1)
               FLD1=DAT(I,J,IFL)
               FLD2=DAT(I+1,J,IFL)
               IF(RPRNT(N)(1:4).EQ.'PRNT')THEN
                  IF(MOD(I,IMOD).EQ.0 .AND. MOD(J,JMOD).EQ.0)THEN
                     WRITE(6,17)I,J,IRNAM(N),RNG1,AZA(J,ISW),ELA(J,ISW),
     +                          DAT(I,J,IFL)
 17                  FORMAT(1X,'IJ=',2I5,' NAME=',A8,' RAE=',3F8.2,
     +                         ' DAT=',F8.2)
                  END IF
               END IF
               IF(FLD1.EQ.BDVAL)GO TO 20
               IF(FLD2.EQ.BDVAL)GO TO 20
               IF(RNG1.GE.PRMN    .AND. RNG1.LE.PRMX   .AND.
     +            RNG2.GE.PRMN    .AND. RNG2.LE.PRMX   .AND.
     +            FLD1.GE.RFMN(N) .AND. FLD1.LE.RFMX(N).AND.
     +            FLD2.GE.RFMN(N) .AND. FLD2.LE.RFMX(N))THEN
                  IF(LTYP(1:4).EQ.'LINE')THEN
                     CALL LINE(RNG1,FLD1,RNG2,FLD2)
                     ELSE
                        CALL PLCHMQ(RNG1,FLD1,'+',6.0,0.0,0.0)
                     ENDIF
                  ENDIF
   20       CONTINUE
  100    CONTINUE
         CALL GRIDAL(MJRX,MNRX,MJRY,MNRY,1,-1,5,X1,Y1)
         CALL LABEL1(IRTYPE,RFYB,RFYT,IRNAM,NRP,J,NFRAME,
     X               XRT,YTP,SIDE)
  200 CONTINUE

C     Restore input PRMN
C
      PRMN=PRMN_KP
      RETURN
      END
