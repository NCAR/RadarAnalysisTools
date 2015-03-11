c
c----------------------------------------------------------------------X
c
      SUBROUTINE CONLEV (JNDAT,NAMFLD,IFLD,IFL,ISW,COLRFIL,ICOLTYP,
     X                   ITERGT,CL,NL,CMIN,CMAX,CINC,ISHADE,IOV,
     X                   DRSW,DROLD,IBSCAN,PLTSW,IGRPLT,IGRYCON,
     X                   JMAP,JACT,JMRK,JNLD,JLMA,NOLAB,THIK,MXF,SINDAT,
     X                   MP,MXPLT,PROCESS,ITERBM,DIGMIN,DIGMAX,DIGOFF,
     X                   DIGCOLR,DIGSIZE,ROTATE,BGFLAG)
C
C  SET UP CONTOUR LINE TYPE, CONTOUR LEVELS AND COLOR TABLES FOR EACH PLOT
C     ICOLTYP:
C             BB#  = BLACK CONTOUR LINES USING (#=1-4) DASHED PATTERN
C             WW#  = WHITE    "      "     "      "       "      "
C             GG#  = GRAY     "      "     "      "       "      "
C             GG#S =   "      "      "   ,WITH EVERY OTHER CONTOUR
C                    INTERVAL SHADED.  USES BW# CONTOUR LINE PATTERN.
C             CL(1 OR 2) = COLOR CONTOURS USING COLOR TABLE 1 (2) (SOLID LINES)
C             LQCOL(1 OR 2) = LOW QUALITY COLOR AREA FILL USING TABLE (1 OR 2)
C             HQCOL(1 OR 2) =HIGH    "      "     "    "    "     "       "
C             LQGRAY(1 OR 2)= LOW    "    GRAY    "    "    "     "       "
C             HQGRAY(1 OR 2)=HIGH    "      "     "    "    "     "       "
C             DIGTsscc = Display digital values with size (ss) and color (cc)
C
C     OVERLAY FLAGS:
C             IOVER = 'OVLY', NEXT CONTOUR COMMAND CONTAINS FIELD TO BE OVERLAID
C             JMAP  = (0) NO MAP OVERLAY, (1) SOLID LINE, (2) DOTTED LINE
C             JMRK  = (0) NO LANDMARKS, (1) NAMES, (2) NAMES IN COLORED BOXES
C             JACT  = (0) NO AIRCRAFT TRACK, (1) PLOT TRACK, (2) PLOT INSERT, 
C                     (3) PLOT AIRCRAFT TRACK AND TIME SERIES INSERT
C             JNLD  = (0) NO CG STRIKES, (1) CG POLARITY, (2) CG POLARITY IN 
C                     COLORED BOXES
C             JLMA  = (0) NO CG STRIKES, (1) LMA 
C
      CHARACTER*8 JNDAT(10),KNDAT(10),SINDAT(10,MXPLT)
      CHARACTER ISHADFL*1,DIGCOLR*2
      LOGICAL COLRFIL,PLTSW,PROCESS

      CHARACTER*8 NAMFLD(MXF)
      CHARACTER*3 ROTATE
      DIMENSION CL(100),DATA(10),IFLD(MXF)
      COMMON/COTABLE/ICOL(100)

C                 ......(3).......  ......(1).......  ......(2).......
C                 ..SHORT DASHED..  ......SOLID.....  ..LONG DASHED...
C     PATTERN #1 =1  1  1  1  1  1  1  7  7  7  7  7  1  7  0  7  0  7
C                 1001001001001001  1111111111111111  1111000111000111
C     PATTERN #2 =1  1  1  1  1  1  1  7  7  7  7  7  1  1  6  3  4  7
C                 1001001001001001  1111111111111111  1001110011100111
C     PATTERN #3 =0  5  2  5  2  5  1  7  7  7  7  7  1  1  6  3  4  7
C                 0101010101010101  1111111111111111  1001110011100111
C     PATTERN #4 =1  7  7  7  7  7  1  1  6  3  4  7  0  0  0  0  0  0
C                 1111111111111111  1001110011100111  0000000000000000
C     PATTERN #5 =1  7  7  7  7  7  0  5  2  5  2  5  1  7  0  7  0  7
C                 1111111111111111  0101010101010101  1111000111000111
C     NOTE: CEDRIC USES PATTERN #3.  IF JDPAT=0, ALL LINES ARE SOLID.
C           If JDPAT=5, Use 3-peat line thicknesses 3000,2000,1000.
C
      CHARACTER*2 IBWFLG
      CHARACTER*8 ICOLTYP,IGRYCON,NAM,IOVER,ICTYP

      COMMON /DASHPATCH/IBWFLG
      COMMON /DASHPAT/IDPAT(5,3),JDPAT,LWSTART,LWINC

      DATA IDPAT(1,1),IDPAT(1,2),IDPAT(1,3)
     +     /O'111111',O'177777',O'170707'/
      DATA IDPAT(2,1),IDPAT(2,2),IDPAT(2,3)
     +     /O'111111',O'177777',O'116347'/
      DATA IDPAT(3,1),IDPAT(3,2),IDPAT(3,3)
     +     /O'052525',O'177777',O'116347'/
      DATA IDPAT(4,1),IDPAT(4,2),IDPAT(4,3)
     +     /O'177777',O'116347',O'000000'/
      DATA IDPAT(5,1),IDPAT(5,2),IDPAT(5,3)
     +     /O'177777',O'052525',O'170707'/
      DATA LWSTART,LWINC/3000,1000/

      COLRFIL=.FALSE.
      PLTSW=.FALSE.
      ISHADE=0
      IOV=0
      JDPAT=0
      IGRPLT=0
      ITABLE=1
      CALL SETBCKGRND(BGFLAG)
      ISW=1
      DRR=DROLD
      IGRYCON=' '
      IBWFLG='  '
c----------------------------------------------------------------------X
      IF(JNDAT(4).EQ.'SAMPLOC '.OR.JNDAT(4)(1:4).EQ.'DIGT')THEN
c         READ(JNDAT,27)NAM,B,ICOLTYP,IOVER,JMAP,JACT,JMRK,KNLD,
c     +        NOLAB,RTERGT,RTERBM,ICTYP,C1,C2,C3
         READ(JNDAT,27)NAM,B,ICOLTYP,IOVER,JMAP,JACT,JMRK,KNLD,
     +        RTERGT,RTERBM,ICTYP,C1,C2,C3
 27      FORMAT(/A8/F8.0/A8/A4,I1,I1,I1,I1/F4.0,
     +        F4.0/A8/F8.0/F8.0/F8.0)
         IF(RTERBM.LE.0.0)THEN
            ITERBM=1
         ELSE
            ITERBM=RTERBM
         END IF
         ROTATE=ICTYP(1:3)
         print *,'CONLEV: rotate=',rotate
      ELSE
c         READ(JNDAT,29)NAM,B,ICOLTYP,IGRYCON,IOVER,JMAP,JACT,JMRK,
c     +        NOLAB,RTERGT,ICTYP,C1,C2,C3
         READ(JNDAT,29)NAM,B,ICOLTYP,IGRYCON,IOVER,JMAP,JACT,JMRK,KNLD,
     +        RTERGT,ICTYP,C1,C2,C3
 29      FORMAT(/A8/F8.0/A7,A1/A4,I1,I1,I1,I1/F8.0/A8/F8.0/F8.0/F8.0)
      END IF

C     KNLD (0) Neither on
C          (1) NLD on, LMA off
C          (2) NLD on in a box, LMA off
C          (3) NLD off, LMA on
C          (4) NLD on, LMA on
C          (5) NLD on in a box, LMA on

      IF(KNLD.EQ.0)THEN
         JNLD=0
         JLMA=0
      ELSE IF(KNLD.EQ.1)THEN
         JNLD=1
         JLMA=0
      ELSE IF(KNLD.EQ.2)THEN
         JNLD=2
         JLMA=0
      ELSE IF(KNLD.EQ.3)THEN
         JNLD=0
         JLMA=1
      ELSE IF(KNLD.EQ.4)THEN
         JNLD=1
         JLMA=1
      ELSE IF(KNLD.EQ.5)THEN
         JNLD=2
         JLMA=1
      END IF      

      IBSCAN=B
      IF(JNDAT(5)(1:4).EQ.'OVLY')IOV=1
      IF(RTERGT.LE.0.0)THEN
         ITERGT=1
      ELSE
         ITERGT=RTERGT
      END IF
      IF(ICOLTYP .EQ.'SAMPLOC' .OR. ICOLTYP(1:4).EQ.'DIGT')THEN
         DIGMIN=C1
         DIGMAX=C2
         DIGOFF=C3
         CMIN=C1
         CMAX=C2
         CINC=0.0
         write(*,*)'digtize: ',nam,digmin,digmax,digoff,itergt,iterbm
         IF(JNDAT(1).EQ.'CNTSWTH ')THEN
            PLTSW=.TRUE.
            DRR=DRSW
            ISW=2
         ELSE
            PLTSW=.FALSE.
            DRR=DROLD
            ISW=1
         END IF
      END IF

      IF(ICOLTYP.EQ.'SAMPLOC')THEN
         DIGSIZE=12.0
         DIGCOLR='BB'
c     Debug print - july 10
c         print *,'CONLEV: drr =', drr
         IF(PROCESS)CALL MNMX(DRR)
         RETURN
      END IF
      IF(ICOLTYP(1:4).EQ.'DIGT')THEN
         READ(ICOLTYP(5:6),30)DIGSIZE
 30      FORMAT(F2.0)
         DIGCOLR=ICOLTYP(7:8)
         IF(DIGSIZE.LT.6.0 .OR. DIGSIZE.GT.18.0)DIGSIZE=12.0
         GO TO 160
      END IF

      IF(ICOLTYP .EQ.'LQCOL1 '.OR.ICOLTYP.EQ.'HQCOL1 '.OR.
     +   ICOLTYP .EQ.'LQGRAY1'.OR.ICOLTYP .EQ.'HQGRAY1')THEN
         COLRFIL=.TRUE.
         ITABLE=1
         IF(ICOLTYP .EQ.'LQGRAY1'.OR.ICOLTYP .EQ.'HQGRAY1')IGRPLT=1
      ELSE IF(ICOLTYP .EQ.'LQCOL2 '.OR.ICOLTYP .EQ.'HQCOL2 '.OR.
     +   ICOLTYP .EQ.'LQGRAY2'.OR.ICOLTYP .EQ.'HQGRAY2')THEN
         COLRFIL=.TRUE.
         ITABLE=2
         IF(ICOLTYP .EQ.'LQGRAY2'.OR.ICOLTYP .EQ.'HQGRAY2')IGRPLT=1
      ELSE
         READ(JNDAT(4),31)IBWFLG,THIK
   31    FORMAT(A2,2X,F4.0)
         IF(THIK.LE.0.0)THIK=1.0
         IF(IBWFLG.EQ.'WW'.OR.IBWFLG.EQ.'BB'.OR.IBWFLG.EQ.'GG'.OR.
     +      IBWFLG.EQ.'rr'.OR.IBWFLG.EQ.'gg'.OR.IBWFLG.EQ.'bb'.OR.
     +      IBWFLG.EQ.'cy'.OR.IBWFLG.EQ.'mg'.OR.IBWFLG.EQ.'yy'.OR.
     +      IBWFLG.EQ.'ww')THEN
            READ(JNDAT(4),33)JDPAT,ISHADFL
   33       FORMAT(2X,I1,A1)
            IF(JDPAT.LT.1 .OR. JDPAT.GT.5)JDPAT=0
            IF(ISHADFL.EQ.'S')ISHADE=1
         ELSE IF(IBWFLG.EQ.'CL')THEN
            READ(JNDAT(4),35)ITABLE
   35       FORMAT(2X,I1)
         ELSE
            WRITE(6,37)
   37       FORMAT(1X,'*** DO NOT UNDERSTAND CONTOUR TYPE ***')
            STOP
         END IF
      END IF

      IF(ICTYP(1:3).EQ.'NON')THEN
         NL=0
 55      CONTINUE
         MP=MP+1
         DO I=1,10
            KNDAT(I)=SINDAT(I,MP)
         END DO
         IF(KNDAT(1).EQ.'        ')THEN
            READ(KNDAT,61)DATA(1),DATA(2),DATA(3),DATA(4),DATA(5),
     +           DATA(6),DATA(7),DATA(8),DATA(9)
 61         FORMAT(/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0)
            DO 100 I=1,9
               NL=NL+1
               IF(NL.GT.100)THEN
                  print *,'*** NUMBER OF CONTOUR LEVELS (',NL,
     X                 ') EXCEEDS 100 ***'
                  STOP
               END IF
               CL(NL)=DATA(I)
               IF(ABS(CL(NL)).EQ.999.9)THEN
                  NL=NL-1
                  GO TO 55
               END IF
 100        CONTINUE
            GO TO 55
         ELSE IF(KNDAT(1).EQ.'END')THEN
            IF(NL.EQ.0)THEN
               WRITE(6,101)
 101           FORMAT(1X,'*** ERROR - ',
     +              'NO NON-UNIFORM CONTOURS SPECIFIED *** ')
               STOP 
            END IF
         ELSE
            WRITE(6,103)
 103        FORMAT(1X,'*** ERROR - NO END TO NON-UNIFORM CONTOURS *** ')
            STOP
         END IF
         CMIN=CL(1)
         CMAX=CL(NL)
         CINC=(CMAX-CMIN)/NL
         DO 110 I=2,NL
            CINC=CL(I)-CL(I-1)
            IF(CINC.LT.0.0)THEN
               WRITE(6,105)
 105           FORMAT(1X,'*** ERROR - ',
     +              'CONTOURS MUST BE MONOTONICALLY INCREASING *** ')
               WRITE(6,107)I,CL(I-1),CL(I)
 107           FORMAT(1X,'*** I,CL(I-1),CL(I)=',I3,2F8.1,' *** CHECK ',
     +              'TERMINATION FOR NON-UNIFORM CONTOURS, ',
     +              'MUST BE 999.9 ***')
               STOP
            END IF
 110     CONTINUE
      ELSE
         CMIN=C1
         CMAX=C2
         CINC=C3
         IF(CMAX.LE.CMIN)CINC=0.0
         IF(CINC.GT.0.0)THEN
            NL=(CMAX-CMIN)/CINC+1.1
            IF(NL.GT.100)THEN
               print *,'*** NUMBER OF CONTOUR LEVELS (',NL,
     X              ') EXCEEDS 100 ***'
               STOP
            END IF
         ELSE
            NL=2
            IF(CMAX.GT.CMIN)THEN
               CINC=CMAX-CMIN
            ELSE
               CINC=1000.0*ABS(CMIN)
               CMAX=CMIN+CINC
            END IF
         END IF
         DO 120 I=1,NL
  120    CL(I)=CMIN+CINC*(I-1)
      END IF

      IF(COLRFIL.OR.IBWFLG.EQ.'CL')THEN
         IF(NL.GT.61)THEN
            print *,'*** NUMBER OF COLOR LEVELS (',NL,
     X           ') EXCEEDS 61 ***'
            STOP
         END IF
         CALL COLTAB(CL,NL,ITABLE)
      END IF

      IF(.NOT.PROCESS)THEN
c         print *,'CONLEV: Contour levels'
c         CALL DMPFLOAT(CL,NL)
         IF(COLRFIL.OR.IBWFLG.EQ.'CL')THEN
            DO N=1,NL
               ICOL(N)=ICOL(N)+1
            END DO
c            CALL DMPINTGR(ICOL,NL)
         END IF
      END IF

C     Reset contour levels to start contouring at value C0=ICTYP(4:8),
C     usually C0>CMIN.  Useful for setting color tables the same for 
C     different fields while having different starting contour levels.
C     The first color index in ICOL will be used for all colors below C0.
C
      IF(ICTYP(4:8).NE.'     ')THEN
         READ(ICTYP(4:8),155)C0
 155     FORMAT(F4.0)
         IF(C0.GT.CMIN)THEN
            INDX=ICOL(1)
            DO I=1,NL
               IF(CL(I).LT.C0)THEN
                  CL(I)=-1000.
                  ICOL(I)=INDX
               END IF
            END DO
         END IF 
      END IF

 160  CONTINUE
      IF(.NOT.PROCESS)RETURN

      IFL=IFIND(NAM,NAMFLD,MXF)
      IF(IFLD(IFL).LE.-1)THEN
         PLTSW=.TRUE.
         DRR=DRSW
         ISW=2
      ELSE
         PLTSW=.FALSE.
         DRR=DROLD
         ISW=1
      END IF
c      print *,'CONLEV calling MNMX,nam,ifl,drr=',nam,ifl,drr
c      print *,'CONLEV: drr =', drr
      CALL MNMX(DRR)

      RETURN
      END
