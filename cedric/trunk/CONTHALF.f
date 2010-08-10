      SUBROUTINE CONTHALF(CAPA,NX,NPX,NPY,CLB,NL,NHIC,ILABC,
     X                  CABDBZ,IOFFP,BAD,NPATC,IPLTYP,RMAX,RMIN,
     X                  XL,YT,YB,IAMA,IGREY,THKLIN,LABFLG)
C
C     GENERATES THE CONTOUR AND AREA FILL PLOTS USING NCAR GRAPHICS, V3.0
C       IPLTYP: 1- CONTOUR PLOT ONLY  (CONT)
C               2- HAFTONE PLOT ONLY  (HAFT)
C               3- CONTOUR AND HALFTONE COMBINED (CHAFT)
C               4- COLOR AREA FILL WITH FULL COLOR TABLE  (FALL)
C               5-  "     "    "    "  "HOT" HALF OF TABLE (FHOT)
C               6-  "     "    "    "  "COLD" "   "   "    (FCOLD)
C               7- COLOR AREA FILL WITH FULL COLOR TABLE AND CONT. LINES (CFALL)
C               8-  "     "    "    "   "HOT" HALF OF TABLE AND CONT. LINES (CFHOT)
C               9-  "     "    "    "   "COLD" HALF OF TABLE AND CONT. LINES (CFCOLD)
C              10- SHADES OF GREY WITHOUT CONTOUR LINES  (GREYS)
C              11- SHADES OF GREY WITH CONTOUR LINES ALSO DRAWN IN (CGREYS)
C              12- SHADES OF GREY WITH CONTOUR LINES AND MIDDLE BIN BLANK
C                  AND A THREE-PEAT PATTERN MOVING IN BOTH DIRECTIONS
C                  FROM MIDDLE BIN (CGREYS2)
C              13- HALFTONES WITH CONTOUR LINES AND MIDDLE BIN BLANK
C                  AND A THREE-PEAT PATTERN MOVING IN BOTH DIRECTIONS
C                  FROM MIDDLE BIN (CHAFT2)
C              14- SHADES OF GREY WITH CONTOUR LINES AND MIDDLE BIN THE
C                  LIGHTEST SHADE OF WHITE (CGREYS3)
C              15- SHADES OF GREY WITH A THREE-PEAT PATTERN STARTING
C                  FROM LOWEST BIN (CGREYS4)
C

C
C     INITIALIZE VARIABLES USED FOR PLOTS
C
      EXTERNAL DRAWCL
      INCLUDE 'CEDRIC.INC'
c      PARAMETER (MXVSCT=20)
      PARAMETER (MAXLEV=61, MXHILO=1000)
      PARAMETER (RNULL= -32768.)
      PARAMETER (NRWRK=512000,NIWRK=192000,NAWRK=4000000,NIARA=3200)
      COMMON /COLORS/ ICOL(MAXLEV),IAIM
      DIMENSION CLB(MAXLEV),CABDBZ(MAXLEV),CAPA(NX,NPY)
C    NEXT 2 DIM. STATEMENTS ARE FOR ARRAYS USED INTERNALLY BY SYSTEM GRAPH.
C    ROUTINES
      DIMENSION RWRK(NRWRK),IWRK(NIWRK),IAMA(NAWRK),XCRA(NIWRK),
     X          YCRA(NIWRK)
      DIMENSION IARA(NIARA),IGRA(NIARA)
C     ARRAYS FOR HI/LO LABELS
      DIMENSION XCRDS(MXHILO),YCRDS(MXHILO),TYPHILO(MXHILO),
     X           VALS(MXHILO)
      DIMENSION LEVUSE(3),IAS(13),ICOLT(MAXLEV)
      DIMENSION LNCOLORS(10)
      CHARACTER*6 LLBS(MAXLEV)
      CHARACTER*16 DASHES(3)
      DIMENSION IPAT(4)
      INTEGER LFIN(MAXLEV)
      EXTERNAL SHADAM,COLRAM
      LOGICAL OVRLAY,HALF,GREYS,CFILL,CNTLINES
c      DATA DASHES/'''''''''$$''''$$''''$$''''','''''$$$$$$$$$$$$$$',
c     X            '''''$$''''''''$$$$'''''''''/
      DATA DASHES/'$$''''$$''''$$''''$$''''','$$$$$$$$$$$$$$$$',
     X            '$$$$''''''''$$$$'''''''''/
      DATA IPAT/12,16,20,7/
      DATA LEVUSE/1,3,1/
      DATA LNCOLORS/1,1,0,0,61,61,8,8,38,38/
      DATA IOLD/0/
      XL=XL+.03
      YT=YT-.07
      XR=XL+.07
      RMN=CLB(1)
      RMX=CLB(NL)


C
C     FORCE AREA FILL
C
      CALL GSFAIS(1)

      IF (IOLD.EQ.0) THEN
C     ONLY NEED TO CALL ONCE
         CALL CPSETC('ILT',' ')
         CALL CPSETI('WSO',1)
         CALL CPSETI('SET',0)
         CALL CPSETI('CLS - Contour level selection',0)
         CALL CPSETC('HIT',' ')
         CALL CPSETC('LOT',' ')
         IOLD=1
      END IF
      
C
C     SET LOGICAL VARIABLES DEPENDING ON PLOT TYPE AND NPATC
C
      IF (NPATC.LT.0) THEN
         OVRLAY=.TRUE.
      ELSE
         OVRLAY=.FALSE.
      END IF

      IF (IPLTYP.EQ.1  .OR. IPLTYP.EQ.3  .OR. IPLTYP.EQ.7 .OR.
     X    IPLTYP.EQ.8  .OR. IPLTYP.EQ.9  .OR. IPLTYP.EQ.11 .OR.
     X    IPLTYP.EQ.12 .OR. IPLTYP.EQ.13 .OR. IPLTYP.EQ.14 .OR.
     X    IPLTYP.EQ.15) THEN
         CNTLINES=.TRUE.
      ELSE
         CNTLINES=.FALSE.
      END IF

      IF (IPLTYP.EQ.10  .OR. IPLTYP.EQ.11 .OR. IPLTYP.EQ.12 .OR. 
     X     IPLTYP.EQ.14 .OR. IPLTYP.EQ.15) THEN
         GREYS=.TRUE.
      ELSE
         GREYS=.FALSE.
      END IF

      IF (IPLTYP.EQ.4 .OR. IPLTYP.EQ.5 .OR. IPLTYP.EQ.6 .OR.
     X    IPLTYP.EQ.7 .OR. IPLTYP.EQ.8 .OR. IPLTYP.EQ.9) THEN
         CFILL=.TRUE.
      ELSE
         CFILL=.FALSE.
      END IF

      IF (IPLTYP.EQ.2 .OR. IPLTYP.EQ.3 .OR. IPLTYP.EQ.13) THEN
         HALF=.TRUE.
      ELSE
         HALF=.FALSE.
      END IF


C       Turn on line around missing value areas
C     CALL CPSETI('PAI - Parameter array index',-2)
C      CALL CPSETI('CLU - Contour level use',1)

      IF (IOFFP.EQ.1) THEN
         CALL CPSETR('SPV',BAD)
      ELSE
         CALL CPSETR('SPV',0.0)
      END IF
C     Set specs according to flags:

      CALL CPSETI('NCL - Number of contour levels',NL)
C
C     SETUP COLORS FOR CONTOUR LINES, IF REQUESTED
C
      IF (.NOT.OVRLAY .AND. CNTLINES) THEN
         IF (GREYS .OR. IGREY.EQ.1) THEN
C
C     MODIFY LINE REQUESTS TO TAKE INTO ACCOUNT A DIFFERENT COLOR TABLE
C
            IF (NPATC.NE.4 .AND. NPATC.NE.5) THEN
               IF (NPATC.EQ.0) THEN
                  NPATC=2
               ELSE IF (NPATC.EQ.1) THEN
                  NPATC=3
               ELSE IF (NPATC.EQ.2) THEN
                  NPATC=0
               ELSE IF (NPATC.EQ.3) THEN
                  NPATC=1
               END IF
               DO I=1,NL
                  ICOL(I)=LNCOLORS(NPATC+1)
               END DO
            ELSE IF (NPATC.EQ.4 .OR. NPATC.EQ.5) THEN
C
C     CHANGING SHADES FROM WHITE TO BLACK
C
               CALL SETCOL(CLB,NL,RMAX,RMIN,IPLTYP)
               DO 300 I=1,NL
                  ICOLT(I)=ICOL(NL-I+1)
 300           CONTINUE
               DO 310 I=1,NL
                  ICOL(I)=ICOLT(I)
 310           CONTINUE
            END IF
         ELSE
            DO 320 I=1,NL
               ICOL(I)=LNCOLORS(NPATC+1)
 320        CONTINUE
         END IF
      ELSE IF (OVRLAY .AND. CNTLINES) THEN
         MPATC=-NPATC
         IF (MPATC.GT.4 .AND. MPATC.LT.27) THEN
            ICCOL=MPATC-5
         ELSE IF (MPATC.GT.27 .AND. MPATC.LT.256) THEN
            ICCOL=MPATC-28
         ELSE IF (MPATC.GT.256 .AND. MPATC.LT.3125) THEN
            ICCOL=MPATC-257
         ELSE IF (MPATC.GT.3125 .AND. MPATC.LT.46656) THEN
            ICCOL=MPATC-3126
         ELSE IF(MPATC.GT.46656) THEN
            ICCOL=MPATC-46657
         END IF
         IF (GREYS .OR. IGREY.EQ.1) THEN
C
C     REVERSE BLACK AND WHITE CONTOUR LINE COLORS FOR OVERLAYS WITH GREY SCALES
C
            IF (ICCOL.NE.2) THEN
               IF (ICCOL.EQ.0) THEN
                  ICCOL=1
               ELSE IF (ICCOL.EQ.1) THEN
                  ICCOL=0
               END IF
               DO I=1,NL
                  ICOL(I)=LNCOLORS((ICCOL+1)*2)
               END DO
            ELSE IF (ICCOL.EQ.2) THEN
C
C     CHANGING SHADES FROM WHITE TO BLACK
C
               CALL SETCOL(CLB,NL,RMAX,RMIN,IPLTYP)
               DO 400 I=1,NL
                  ICOLT(I)=ICOL(NL-I+1)
 400           CONTINUE
               DO 410 I=1,NL
                  ICOL(I)=ICOLT(I)
 410           CONTINUE
            END IF
         ELSE
            DO 420 I=1,NL
               ICOL(I)=LNCOLORS((ICCOL+1)*2)
 420        CONTINUE
         END IF
      END IF
C     CONTOUR LINE LABELS
      CALL CPSETI('LLP',0)

      DO 25 I=1,NL
         CALL CPSETI('PAI - Parameter array ID',I)
         CALL CPSETR('CLV - Contour level value',CLB(I))
         IF (NPATC.EQ.0 .OR. NPATC.EQ.2 .OR. NPATC.EQ.4 .OR. 
     X        NPATC.EQ.6 .OR. NPATC.EQ.8) THEN
            IF (CLB(I).LT.0) THEN
               CALL CPSETC('CLD',DASHES(1))
            ELSE IF (CLB(I).GE.0) THEN
               CALL CPSETC('CLD',DASHES(2))
            END IF
         ELSE IF (NPATC.LT.0) THEN
C
C     GET DASHED PATTERN FOR OVERLAY PLOTS
C
            MPATC=-NPATC
            IF (MPATC.GT.4 .AND. MPATC.LT.27) THEN
               CALL CPSETC('CLD',DASHES(MOD(I,3)+1))
            ELSE IF (MPATC.GT.27 .AND. MPATC.LT.256) THEN
               IF (CLB(I).LT.0) THEN
                  CALL CPSETC('CLD',DASHES(1))
               ELSE IF (CLB(I).GE.0) THEN
                  CALL CPSETC('CLD',DASHES(2))
               END IF
             ELSE IF (MPATC.GT.256 .AND. MPATC.LT.3125) THEN
               CALL CPSETC('CLD',DASHES(3))
            ELSE IF (MPATC.GT.3125 .AND. MPATC.LT.46656) THEN
               CALL CPSETC('CLD',DASHES(1))
            ELSE IF(MPATC.GT.46656) THEN
               CALL CPSETC('CLD',DASHES(2))
            END IF
         ELSE
            CALL CPSETC('CLD',DASHES(MOD(I,3)+1))
         END IF
         IF (ILABC.NE.0) CALL CPSETI('LLC',ICOL(I))
         IF (CNTLINES) THEN
            CALL CPSETI('CLC - CONTOUR LINE COLOR',ICOL(I))
         END IF
C
C     THE NEXT CALL DETERMINES WHETHER JUST THE CONT. LINE IS TO BE
C     DRAWN OR IF THE LABEL ON THE LINE IS ALSO TO BE DRAWN
C
         CALL CPSETI('CLU - Contour level use',LEVUSE(MOD(I,3)+1))
 25   CONTINUE
C     
C     Draw the mess, and find the max and min values of the field
C     
      CALL GSCLIP(1)
      


      CALL ARINAM(IAMA,NAWRK)
      IF (NHIC.GT.0) THEN
         CALL MINMAX(CAPA,XCRDS,YCRDS,TYPHILO,VALS,NHILO,
     X               NX,NPX,NPY,NHIC)
         CALL LABBOX(XCRDS,YCRDS,VALS,TYPHILO,NHILO,IAMA)
      END IF         


      CALL CPRECT(CAPA,NX,NPX,NPY,RWRK,NRWRK,IWRK,NIWRK)
      CALL CPGETR('ZMX',RMAX)
      CALL CPGETR('ZMN',RMIN)
      IF (CFILL .OR. GREYS .OR. HALF) THEN
C
C     DETERMINE AREA IDENTIFIER BELOW WHICH NO FILLING SHOULD BE DONE
C
         IAIM = 1
         DO 20 I=1,NL-1
            IF (RMIN.GT.CLB(I) .AND. RMIN.LT.CLB(I+1)) THEN
               IAIM=I
               GOTO 23
            END IF
 20      CONTINUE
 23      CONTINUE

C     
C     IF OTHER THAN JUST CONTOUR LINES
C
         IF (CFILL .OR. GREYS) THEN
            CALL SFSETI('TYpe of fill',0)
C     THE NEXT CALL CHOOSES THE COLORS THAT WILL BE USED FOR AREA FILL
            CALL SETCOL(CLB,NL,RMAX,RMIN,IPLTYP)
         ELSE IF (HALF) THEN
            CALL SETPAT(CLB,NL,RMAX,RMIN,IPLTYP)
            CALL SFSETI('DO-DOT-FILL',1)
            CALL SFSETI('TYpe of fill',-4)
         END IF
         CALL SFSETI('ANgle of fill lines',15)

         CALL CPCLAM(CAPA,RWRK,IWRK,IAMA)
         IF (CFILL .OR. GREYS) THEN
            CALL ARSCAM(IAMA,XCRA,YCRA,NIWRK,IARA,IGRA,NIARA,COLRAM)
         ELSE IF (HALF) THEN
            CALL ARSCAM(IAMA,XCRA,YCRA,NIWRK,IARA,IGRA,NIARA,SHADAM)
         END IF
         CALL GSCLIP(0)

C
C     Add label bar
C
         CALL LBSETR('WFL',2.0)
         NLBS=1
         NBOX=0
         LLBS(1)=' '
         IFLTFLG=0
         DO I=1,NL
C
C     DETERMINE IF INTS CAN BE USED FOR LABELS OR IF FLOATS ARE NEEDED
C
            IF (CLB(NL-I+1).NE.0.0) THEN
               FVAL=FLOAT(INT(CLB(NL-I+1)))/CLB(NL-I+1)
            ELSE
               FVAL=1.0
            END IF
            IF (FVAL.NE.1.0) IFLTFLG=1
         END DO
            
         DO 160 I=1,NL
            IF (((NL-I+1).GE.IAIM .AND. CLB(NL-I+1).LE.RMAX) .OR. 
     X           LABFLG.EQ.1) THEN
               NBOX=NBOX+1
               NLBS=NLBS+1
               IF (CFILL .OR. GREYS) THEN
                  LFIN(NBOX)=ICOL(NL-I+1)
               ELSE IF (HALF) THEN
                  LFIN(NBOX)=IPAT(ICOL(NL-I+1))
               END IF
               IF (IFLTFLG.EQ.1) THEN
                  WRITE(LLBS(NLBS),'(F6.1)')CLB(NL-I+1)
               ELSE
                  IVAL=CLB(NL-I+1)
                  IF (ABS(IVAL).LT.10) THEN
                     WRITE(LLBS(NLBS),'(I2)')IVAL                     
                  ELSE IF (ABS(IVAL).LT.100) THEN
                     WRITE(LLBS(NLBS),'(I3)')IVAL
                  ELSE IF (ABS(IVAL).LT.1000) THEN
                     WRITE(LLBS(NLBS),'(I4)')IVAL
                  ELSE
                     WRITE(LLBS(NLBS),'(I5)')IVAL
                  END IF
               END IF
            END IF
               
            
 160     CONTINUE
         IF (NLBS.NE.0 .AND. NBOX.NE.0) THEN
            IF (CFILL .OR. GREYS) THEN
               CALL LBLBAR(1,XL,XR,YB,YT,NBOX,0.2,1.0,LFIN,0,LLBS,
     X              NLBS,1)
            ELSE IF (HALF) THEN
               CALL LBLBAR(1,XL,XR,YB,YT,NBOX,0.2,1.0,LFIN,1,LLBS,
     X              NLBS,1)
            END IF
         ELSE 
C
C     NLBS=0 => NO DATA THIS LEVEL
C
            CALL PLCHMQ(CFUX(0.5),CFUY(0.5),'NO DATA THIS LEVEL',
     X           12.,0.,0.)
         END IF
      END IF
C
C     DRAW CONTOUR LINES IN NOW
C
      IF (CNTLINES) THEN
         IF (THKLIN.NE.1.0) THEN
            CALL GQLWSC(IERR,XSC)
            CALL GSLWSC(THKLIN*XSC)
            IF (NHIC.GT.0) THEN
C     MUST REINITIALIZE AREAS BECAUSE OF NCAR GRAPHICS PROBLEM WHEN
C     DRAWING DASHED CONTOUR LINES MASKED BY LABEL BOXES
               CALL ARINAM(IAMA,NAWRK)
               IF (NHILO.GT.0) THEN
                  CALL LABBOX(XCRDS,YCRDS,VALS,TYPHILO,NHILO,IAMA)
                  CALL CPCLDM(CAPA,RWRK,IWRK,IAMA,DRAWCL)
               ELSE
                  CALL CPCLDR(CAPA,RWRK,IWRK)
               END IF
               IF (ILABC.NE.0) THEN
                  CALL CPSETI('LLP',2)
                  CALL CPRECT(CAPA,NX,NPX,NPY,RWRK,NRWRK,IWRK,NIWRK)
                  CALL CPLBDR(CAPA,RWRK,IWRK)
               END IF
            ELSE IF (ILABC.NE.0) THEN
               CALL CPSETI('LLP',2)
               CALL CPRECT(CAPA,NX,NPX,NPY,RWRK,NRWRK,IWRK,NIWRK)
               CALL CPCLDR(CAPA,RWRK,IWRK)
               CALL CPLBDR(CAPA,RWRK,IWRK)
            ELSE
               CALL CPCLDR(CAPA,RWRK,IWRK)
            END IF
            CALL GSLWSC(XSC)
         ELSE
            IF (NHIC.GT.0) THEN
C     MUST REINITIALIZE AREAS BECAUSE OF NCAR GRAPHICS PROBLEM WHEN
C     DRAWING DASHED CONTOUR LINES MASKED BY LABEL BOXES
               CALL ARINAM(IAMA,NAWRK)
               IF (NHILO.GT.0) THEN
                  CALL LABBOX(XCRDS,YCRDS,VALS,TYPHILO,NHILO,IAMA)
                  CALL CPCLDM(CAPA,RWRK,IWRK,IAMA,DRAWCL)
               ELSE
                  CALL CPCLDR(CAPA,RWRK,IWRK)
               END IF
               IF (ILABC.NE.0) THEN
                  CALL CPSETI('LLP',2)
                  CALL CPRECT(CAPA,NX,NPX,NPY,RWRK,NRWRK,IWRK,NIWRK)
                  CALL CPLBDR(CAPA,RWRK,IWRK)
               END IF
            ELSE IF (ILABC.NE.0) THEN
               CALL CPSETI('LLP',2)
               CALL CPRECT(CAPA,NX,NPX,NPY,RWRK,NRWRK,IWRK,NIWRK)
               CALL CPCLDR(CAPA,RWRK,IWRK)
               CALL CPLBDR(CAPA,RWRK,IWRK)
            ELSE
               CALL CPCLDR(CAPA,RWRK,IWRK)
            END IF
         END IF
       END IF

C     DIGITIZE BACKGROUND IF REQUESTED
      IF(NHIC.LT.0) CALL SIMPLDIG(CAPA,NX,NPX,NPY,BAD)


      RETURN

      END


      SUBROUTINE DRAWCL (XCS,YCS,NCS,IAI,IAG,NAI)

      DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)

      IDR=1

      DO I=1,NAI
         IF (IAI(I).LT.0) IDR=-0
      END DO

      IF (IDR.NE.0) CALL CURVED(XCS,YCS,NCS)


      RETURN

      END
