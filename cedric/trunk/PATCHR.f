      SUBROUTINE PATCHR(KRD,IBUF,OBUF,RBUF,SBUF,MAXBSZ,IPR,NST)
C
C        PERFORMS DATA FILL AND DECIMATION FUNCTIONS ON 2-D PLANE DATA
C
C          IBUF- SCRATCH BUFFER F0R I/O
C          OBUF- AUXILLIARY BUFFER FOR DATA MANIPULATION
C          RBUF- I/O DATA BUFFER
C        MAXPLN- MAXIMUM DIMENSION OF RBUF,OBUF
C           IPR- PRINT FILE UNIT NUMBER
C           NST- STATUS FLAG:  0- O.K.
C
C     August 29, 1997 (LJM) - fixed to have dimensions of region be radius,
C                             was using NBOX/2 where NBOX was input radius not diam.
C                             changed DECILOC to have local standard deviation in
C                             the output if the radius is .lt. zero
C
      INCLUDE 'CEDRIC.INC'
      DIMENSION IBUF(MAXPLN),OBUF(MAXPLN),RBUF(MAXBSZ),INTPAR(5),
     X          MAXIPR(5),MAXIPR2(3),PAR(3),NAX(3),SBUF(MAXBSZ)
      CHARACTER*(*) KRD(10)
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X                IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X                NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF,NAMINF(4),NAMOUF(4),NAMTHR(4),NONAME(4)
      CHARACTER*1 IWOP,IFAX,IBL
      CHARACTER*8 IMPAT(6),MPAT
      COMMON /EDINFO/ IEDW(2,3),PEDW(2,3)
      CHARACTER IGLOTP(3)*28
      LOGICAL NEWJ,MISV
      EQUIVALENCE (INTPAR(1),ITMAX),(INTPAR(2),NQUAD),
     X            (INTPAR(3),MINPTS),(INTPAR(4),IADJ),(INTPAR(5),NMIN)
      EQUIVALENCE (NCX(1),NX),(NCX(2),NY),(NCX(3),NZ)
      EQUIVALENCE (NAX(1),I1),(NAX(2),I2),(NAX(3),I3)
      DATA IGLOTP/'1-DIMENSIONAL (BY LINES)',
     X            '2-DIMENSIONAL ( BY  PLANES)',
     X            '3-DIMENSIONAL (FULL VOLUME)'/
      DATA NONAME/'NO','NE',' ',' '/
      DATA IMPAT/'FILLCON','FILLALL','DECILOC','DECIGLO','FILLCON3',
     X           'LINFIT2'/
      DATA MAXIPR/MAXZLEV,4,MAXPLN,MAXZLEV,MAXPLN/
      DATA MAXIPR2/86, 8, 655360/
      DATA ISW3D/0/
      DATA MISV/.FALSE./
      DATA IBL/'  '/

      MAXIPR2(3) = MAXPLN * 10
C
C        INITIALIZATION OF PATCHER PARAMETERS
C
      READ (KRD,100)NAMOUF,NAMINF,MPAT,PAR,NAMTHR,IFAX,IWOP
C  100 FORMAT(8X,8A2,A7,1X,3F8.0,4A2,A1,7X,A1)
 100  FORMAT(/4A2/4A2/A8/F8.0/F8.0/F8.0/4A2/A1/A1)
      IFLD=LOCFLDID(NAMINF,ID(176),5,NFL,4)
      IF(IFLD.EQ.0) THEN
         CALL CEDERX(501,1)
         RETURN
      END IF
      KFLD=LOCFLDID(NAMTHR,ID(176),5,NFL,4)
      IF(KFLD.EQ.0) CALL COPCX(NAMTHR,NONAME,4)
      IOPT=IFINDC(MPAT,IMPAT,6,0)
      IF(IOPT.EQ.0) THEN
         CALL CEDERX(509,1)
         RETURN
      END IF
      IF(NAMOUF(1).EQ.IBL) CALL COPCX(NAMOUF,NAMINF,4)
      IV=MAPVID(IFLD,2)
      ISCL=SCLFLD(IV)
      I=IADFLD(NAMOUF,ISCL,IPR)
      IF (I.LT.0) RETURN
      IF(I.EQ.0) THEN
         NEWJ=.FALSE.
      ELSE
         NEWJ=.TRUE.
         IFLD=LOCFLDID(NAMINF,ID(176),5,NFL,4)
         KFLD=LOCFLDID(NAMTHR,ID(176),5,NFL,4)
      END IF
      JFLD=LOCFLDID(NAMOUF,ID(176),5,NFL,4)
C
C     NEXT CALL ONLY HAPPENS FOR 2 OR 3 D FILLING
C
      IF (IOPT.NE.2 .OR. PAR(1).NE.1.0) CALL SETAXS(NAX,IFAX,IPR)
      N1=NCX(I1)
      N2=NCX(I2)
      N3=NCX(I3)
      IF (IOPT.EQ.2 .AND. PAR(1).EQ.1.0) THEN
C
C     1-D LEISE FILTERING
C
         IF (IFAX.EQ.'X') THEN
            I3=3
            N1=NX
            N2=0
            N3=NZ
         ELSE IF (IFAX.EQ.'Y') THEN
            I3=3
            N1=0
            N2=NY
            N3=NZ
         ELSE
            I3=2
            N1=NZ
            N2=0
            N3=NY
         END IF
      END IF
      NPLIN=N1*N2
      CALL WINSET(IEDW,PEDW,IWOP)
      SD= (FLOAT(ID(304))/FLOAT(ID(68))) * 0.50
      SDFAC=2.0
c      NBOX=3
      IADJ=1
      NMIN=4
      ITMAX=5
      NQUAD=3
      MINPTS=4
      GO TO (40,50,60,70,35,17), IOPT
 17   CONTINUE
C
C     CHECK/SET PARAMETERIZATION FOR 2-D GLOBAL LEAST SQUARES FILLING
C
      I=PAR(3)
      IF (I.LT.4) I=4
      MINPTS=I
      GOTO 81
      
 35   CONTINUE
C
C     3-D LEAST SQUARES FILLING
C
      ISW3D=1
      DO 37 IP=1,3
         I=PAR(IP)
         IF (I.LE.0 .OR. I.GT.MAXIPR2(IP)) GOTO 37
         INTPAR(IP)=I
 37   CONTINUE
      GOTO 81
   40 CONTINUE
C
C     SET PARAMETERIZATION FOR 2-D LOCAL LEAST SQUARES
C
      DO 45 IP=1,3
         I=PAR(IP)
         IF(I.LE.0.OR.I.GT.MAXIPR(IP)) GO TO 45
         INTPAR(IP)=I
   45 CONTINUE
      GO TO 81
   50 CONTINUE
C
C        GLOBAL DATA FILL
C
      ISW3D=0
      IF(PAR(1).EQ.3.0) ISW3D=1
   60 CONTINUE
C
C        LOCAL MEANS DECIMATION PROCEDURE
C        Pass search radius as negative number to store local standard
C        deviation in output instead of locally decimated input.
C
      DO 65 IP=1,3
         IF(IP.LT.3) THEN
            I=ABS(PAR(IP))
            IF (IP.EQ.1) I=I*2 + 1
C            IF(IP.EQ.1.AND.MOD(I,2).NE.1) GO TO 65
            IF(I.GT.0.AND.I.LE.MAXIPR(IP+3)) THEN
               IF(IP.EQ.1) THEN
                  INTPAR(IP+3)=PAR(IP)
               ELSE
                  INTPAR(IP+3)=I
               END IF
            END IF
         ELSE
            IF(PAR(IP).GT.0.0) SD=PAR(IP)
         END IF
   65 CONTINUE
      GO TO 81
   70 CONTINUE
C
C        GLOBAL DECIMATION
C
      IF(PAR(1).GT.0.0) SDFAC=PAR(1)
   81 CONTINUE
C
C        INITIATE THE APPROPRIATE PROCEDURE
C
C
C        ESTABLISH REGION OVER WHICH TO DECIMATE/FILL
C
      JBEG=IEDW(1,I2)
      JEND=IEDW(2,I2)
      IBEG=IEDW(1,I1)
      IEND=IEDW(2,I1)
c      IADJ=NBOX/2
      NBOX=2*IADJ
      VPTS=NMIN
C
C        SEND A SUMMARY OF THIS EDITING STEP TO THE PRINT FILE
C
         CALL SHOEDW(IPR)
         CALL SHOEDF(IPR)
         WRITE(IPR,116) (NAMINF(I),I=1,4),(NAMOUF(I),I=1,4)
  116    FORMAT(/'  INPUT FIELD: ',4A2/' OUTPUT FIELD: ',4A2)
         IF(IOPT.EQ.1 .OR. IOPT.EQ.5) THEN 
            WRITE(IPR,103) (INTPAR(I),I=1,3),(NAMTHR(I),I=1,4)
 103  FORMAT(/3X,'CONSTRAINED DATA FILL(LST. SQR.)-  PARAMETERS ...'/
     X8X,'1- MAXIMUM NUMBER OF GRID STEPS TO SEARCH OUTWARD FOR DATA:',
     X       I5/
     X8X,'2- MIN. ACCEPTABLE NUM. OF QUADRANTS(OCTANTS) CONTAINING 
     X DATA:',
     X       I6/
     X8X,'3- MINIMUM ACCEPTABLE NUMBER OF POINTS WITHIN SEARCH REGION:',
     X       I4/
     X8X,'4- OPTIONAL DECISION FIELD --ONLY FILL IF CORRESPONDING'/
     X8X,' VALUE IN THIS FIELD IS ALSO MISSING(FOR 2-D ONLY): ',
     X       4A2)
           WRITE(IPR,123)IGLOTP(ISW3D+2)
 123       FORMAT(8X,'5- DIMENSION OF FILL: ',A28)
        END IF
        
      IF(IOPT.EQ.2) WRITE(IPR,106) IGLOTP(INT(PAR(1)))
  106 FORMAT(/3X,'GLOBAL DATA FILL-  PARAMETERS ...'/
     X 8X,'1- TYPE OF DATA FILLING: ',A28)
      
      IF(IOPT.EQ.3) WRITE(IPR,107) NBOX,NBOX,NMIN,SD
  107 FORMAT(/3X,'DECIMATION USING LOCAL MEANS-  PARAMETERS ...'/
     X 8X,'1- DIMENSION OF LOCAL DECIMATION REGIONS:',8X,'(',I2,' BY )',
     X        I4/
     X 8X,'2- MINIMUM ACCEPTABLE NUMBER OF POINTS IN EACH REGION:',I7/
     X 8X,'3- MAXIMUM DEVIATION OF VALUES FROM THE MEAN:',F15.2)

      IF(IOPT.EQ.4) WRITE(IPR,109) SDFAC
  109 FORMAT(/3X,'DECIMATION USING GLOBAL MEAN-  PARAMETERS ...'/
     X8X,'1- REJECT ANY VALUE GREATER THEN',F6.2,' SIGMA FROM THE MEAN')

      IF (IOPT.EQ.6) WRITE(IPR,112)MINPTS,(NAMTHR(I),I=1,4)
 112  FORMAT(/3X,'2-D GLOBAL LIN. LST. SQR- PARAMETERS ...'/
     X8X,'1- MINIMUM ACCEPTABLE NUM. OF POINTS WITHIN SEARCH REGION:',
     X     I8/,
     X8X,'2- OPTIONAL DECISION FIELD --ONLY FILL IF CORRESPONDING'/
     X8X,' VALUE IN THIS FIELD IS ALSO MISSING: ',4A2)
      
      IF (IOPT.EQ.5) THEN
C
C     3-D LEAST SQUARES FILLING- FIRST CHECK FOR SUFFICIENT STORAGE 
C
         M1=IEDW(2,1)-IEDW(1,1)+1
         M2=IEDW(2,2)-IEDW(1,2)+1
         M3=IEDW(2,3)-IEDW(1,3)+1
         IF(M1*M2*M3.GT.MAXBSZ) THEN
            WRITE(IPR,117) M1,M2,M3,MAXBSZ
            CALL CEDERX(518,1)
            RETURN
         END IF
         IOPT=0
         CALL GET3D(IEDFL,IBUF,OBUF,NX,NY,NZ,IFLD,JFLD,NEWJ,MISV,
     X              RBUF,IEDW,NST)
         IF (NST.NE.0) THEN
            WRITE(*,*)'+++TRANSFER PROBLEMS IN PATCHR+++'
            CALL FLUSH_STDOUT
         END IF
         CALL FILL3D(RBUF,SBUF,INTPAR(1),INTPAR(2),FLOAT(INTPAR(3)),
     X               M1,M2,M3,BAD,IEDW,IOPT)
         CALL PUT3D(IEDFL,IBUF,OBUF,NX,NY,NZ,IFLD,JFLD,NEWJ,MISV,
     X              RBUF,IEDW,NST)
         IF (NST.NE.0) THEN
            WRITE(*,*)'+++TRANSFER PROBLEMS IN PATCHR+++'
            CALL FLUSH_STDOUT
         END IF
         GO TO 90
      END IF

      IF(IOPT.EQ.2.AND.ISW3D.NE.0) THEN
C
C        3-D GLOBAL METHOD- FIRST CHECK IF SUFFICIENT STORAGE
C
         M1=IEDW(2,1)-IEDW(1,1)+1
         M2=IEDW(2,2)-IEDW(1,2)+1
         M3=IEDW(2,3)-IEDW(1,3)+1
         IF(M1*M2*M3.GT.MAXBSZ) THEN
            WRITE(IPR,117) M1,M2,M3,MAXBSZ
  117       FORMAT(/5X,'+++  (X,Y,Z) WINDOWED REGION:  (',I3,',',I3,
     X         ',',I3,')'/10X,'MAY NOT CONTAIN MORE THAN ',I7,
     X         ' POINTS  +++')
            CALL CEDERX(518,1)
            RETURN
         END IF
         CALL GET3D(IEDFL,IBUF,OBUF,NX,NY,NZ,IFLD,JFLD,NEWJ,MISV,
     X              RBUF,IEDW,NST)
         CALL EXTEND(RBUF,M1,M2,M3,BAD)
         CALL PUT3D(IEDFL,IBUF,OBUF,NX,NY,NZ,IFLD,JFLD,NEWJ,MISV,
     X              RBUF,IEDW,NST)
         GO TO 90
      END IF
      IF (IOPT.EQ.2 .AND. PAR(1).EQ.1.0) THEN
C
C     1-D LEISE
C
         DO LEV=1,N3
            IF (LEV.LT.IEDW(1,I3) .OR. LEV.GT.IEDW(2,I3)) THEN
C
C     PLANE IS OUTSIDE EDITING WINDOW
C
               IF (NEWJ) THEN
                  CALL FETCHD(IEDFL,ID,LEV,IFLD,IBUF,RBUF,NIX,
     X                 NIY,I3,BAD,ZLEV,NST)
                  CALL PLACED(IEDFL,ID,LEV,JFLD,IBUF,RBUF,NIX,
     X                 NIY,I3,BAD,NST)
               END IF
            ELSE
               CALL FETCHD(IEDFL,ID,LEV,IFLD,IBUF,RBUF,NIX,NIY,
     X              I3,BAD,ZLEV,NST)
               IF (IFAX.EQ.'X') THEN
                  DO L1=IEDW(1,2),IEDW(2,2)
                     LV=NX*(L1-1) + 1
                     CALL COPRX(OBUF,RBUF(LV),NX)
                     CALL LSFILW(RBUF(LV),OBUF,NX,1,IEDW(1,1),
     X                   IEDW(2,1),1,1,BAD)
                  END DO
               ELSE IF (IFAX.EQ.'Y') THEN
                  DO L1=IEDW(1,1),IEDW(2,1)
                     DO L2=1,NY
                        LV=NX*(L2-1) + L1
                        SBUF(L2)=RBUF(LV)
                     END DO
                     CALL COPRX(OBUF,SBUF,NY)
                     CALL LSFILW(SBUF,OBUF,NY,1,IEDW(1,2),
     X                   IEDW(2,2),1,1,BAD)
                     DO L2=1,NY
                        LV=NX*(L2-1) + L1
                        RBUF(LV)=SBUF(L2)
                     END DO
                  END DO
               ELSE IF (IFAX.EQ.'Z') THEN
                  DO L1=IEDW(1,1),IEDW(2,1)
                     DO L2=1,NZ
                        LV=NX*(L2-1) + L1
                        SBUF(L2)=RBUF(LV)
                     END DO
                     CALL COPRX(OBUF,SBUF,NZ)
                     CALL LSFILW(SBUF,OBUF,NZ,1,IEDW(1,3),
     X                   IEDW(2,3),1,1,BAD)
                     DO L2=1,NZ
                        LV=NX*(L2-1) + L1
                        RBUF(LV)=SBUF(L2)
                     END DO
                  END DO
               END IF
               CALL PLACED(IEDFL,ID,LEV,JFLD,IBUF,RBUF,NIX,
     X              NIY,I3,BAD,NST)
            END IF
         END DO
         GOTO 90
      END IF
C
C        LOOP THROUGH CONSTANT-Z PLANES
C
      DO 89 LEV=1,N3
         IF(LEV.GE.IEDW(1,I3).AND.LEV.LE.IEDW(2,I3)) GO TO 82
C
C           PLANE IS OUTSIDE EDITING WINDOW
C
            IF(.NOT.NEWJ) GO TO 89
               CALL FETCHD(IEDFL,ID,LEV,IFLD,IBUF,RBUF,
     X                     NIX,NIY,I3,BAD,ZLEV,NST)
               GO TO 88
   82    CONTINUE
C
C           PROCEED WITH THE PATCHER FUNCTION
C
         CALL FETCHD(IEDFL,ID,LEV,IFLD,IBUF,RBUF,NIX,NIY,I3,BAD,
     X               ZLEV,NST)
         IF(IOPT.NE.4) CALL COPRX(OBUF,RBUF,NPLIN)
         GO TO (83,84,85,86,87,87), IOPT
   83    CONTINUE
C
C           BOUNDED DATA FILL
C
            IF(KFLD.NE.0) THEN
C
C              FETCH IN A DECISION FIELD
C
               CALL FETCHD(IEDFL,ID,LEV,KFLD,IBUF,RBUF(MAXPLN+1),
     X                     NIX,NIY,I3,BAD,ZLEV,NST)
            ELSE
               CALL COPRX(RBUF(MAXPLN+1),RBUF,NPLIN)
            END IF
C
            CALL BNDFIL(RBUF,OBUF,RBUF(MAXPLN+1),N1,N2,
     X                  IBEG,IEND,JBEG,JEND,ITMAX,NQUAD,MINPTS,BAD)
            GO TO 88
   84    CONTINUE
C
C           GLOBAL DATA FILL
C
            CALL LSFILW(RBUF,OBUF,N1,N2,IBEG,IEND,JBEG,JEND,BAD)
            GO TO 88
   85    CONTINUE
C
C           LOCAL MEANS DECIMATION
C
            CALL UNPAUT(OBUF,RBUF,N1,N2,IBEG,IEND,JBEG,JEND,
     X                  IADJ,0.0,VPTS,SD,BAD,1)
            GO TO 88
   86    CONTINUE
C
C           GLOBAL DECIMATION
C
            CALL GLODEC(RBUF,N1,N2,IBEG,IEND,JBEG,JEND,SDFAC,BAD)
            GO TO 88
 87         CONTINUE
C
C     GLOBAL LINEAR LEAST SQUARES FIT OF DATA
C
            IF(KFLD.NE.0) THEN
C
C              FETCH IN A DECISION FIELD
C
               CALL FETCHD(IEDFL,ID,LEV,KFLD,IBUF,RBUF(MAXPLN+1),
     X                     NIX,NIY,I3,BAD,ZLEV,NST)
            ELSE
               CALL CONFLD(RBUF(MAXPLN+1),NPLIN,BAD)
            END IF
C
            CALL LINFIT2(RBUF,OBUF,RBUF(MAXPLN+1),N1,N2,
     X                  IBEG,IEND,JBEG,JEND,ITMAX,NQUAD,MINPTS,BAD)
   88    CONTINUE
C
C           FINISHED WITH THIS PLANE- WRITE OUT RESULTS
C
         CALL PLACED(IEDFL,ID,LEV,JFLD,IBUF,RBUF,N1,N2,I3,BAD,NST)
   89 CONTINUE
      NST=0
   90 CONTINUE
      RETURN
      END
