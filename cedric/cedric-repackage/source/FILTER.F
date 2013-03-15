      SUBROUTINE FILTER(KRD,IBUF,OBUF,RBUF,SBUF,MAXBSZ,IPR,NST)
C
C        PERFORMS FILTERING FUNCTIONS ON TWO CONSTANT Z-PLANE DATA
C
C          IBUF- SCRATCH BUFFER F0R I/O
C          OBUF- AUXILLIARY BUFFER FOR DATA MANIPULATION
C          RBUF- I/O DATA BUFFER
C        MAXPLN- MAXIMUM DIMENSION OF RBUF,OBUF
C           IPR- PRINT FILE UNIT NUMBER
C           NST- STATUS FLAG:  0- O.K.
C
      INCLUDE 'CEDRIC.INC'
      PARAMETER (NFLTS=9)
      DIMENSION IBUF(MAXPLN),OBUF(MAXPLN),RBUF(MAXBSZ),WEIGHT(3),
     X          NAX(3),SBUF(MAXBSZ)
      CHARACTER*(*) KRD(10)
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X                IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X                NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF,NAMINF(4),NAMOUF(4),IBL
      CHARACTER*3 MFLT,IMFILT(NFLTS)
      CHARACTER*1 IFAX,IWOP
      COMMON /EDINFO/ IEDW(2,3),PEDW(2,3)
      CHARACTER NOFILT(NFLTS)*15
      LOGICAL NEWJ,MISV
      EQUIVALENCE (NCX(1),NX),(NCX(2),NY),(NCX(3),NZ)
      EQUIVALENCE (NAX(1),I1),(NAX(2),I2),(NAX(3),I3)
      DATA NOFILT / 'UNIFORM        ','HANNING        ',
     X              'INVERSE HANNING','LEISE (2-D)    ',
     X              'LST. SQR. (2-D)','TWO-WAY HANNING',
     X              'LEISE (3-D)    ','LST. SQR. (3-D)',
     X              'LEISE (1-D)    '/
      DATA IMFILT/'UNI','HAN','INV','LEI','LEA','TWO','L3D','LT3',
     X     'L1D'/
      DATA MISV/.TRUE./
      DATA IBL/'  '/
C
C        INITIALIZATION OF FILTERING PARAMETERS
C
      READ (KRD,100)NAMOUF,NAMINF,MFLT,P1,IFAX,IWOP
C  100 FORMAT(8X,8A2,A3,5X,F8.0,24X,A1,7X,A1)
 100  FORMAT(/4A2/4A2/A3/F8.0////A1/A1)
      IFLD=LOCFLDID(NAMINF,ID(176),5,NFL,4)
      IF(IFLD.EQ.0) THEN
         CALL CEDERX(501,1)
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
      END IF
      JFLD=LOCFLDID(NAMOUF,ID(176),5,NFL,4)
      IP=IFINDC(MFLT,IMFILT,NFLTS,0)
      IF(IP.EQ.0) THEN
         CALL CEDERX(506,1)
         RETURN
      END IF
      CALL WINSET(IEDW,PEDW,IWOP)
      IF (IP.NE.9) CALL SETAXS(NAX,IFAX,IPR)
      N1=NCX(I1)
      N2=NCX(I2)
      N3=NCX(I3)
      NPLIN=N1*N2
      GO TO (46,56,66,70,76,81,70,91,70), IP
 46   CONTINUE
C
C        UNIFORM FILTER
C
      WEIGHT(1)=1./9.
      WEIGHT(2)=1./9.
      WEIGHT(3)=1./9.
      GO TO 81
 56   CONTINUE
C
C        HANNING FILTER
C
      WEIGHT(1)=0.25
      WEIGHT(2)=0.125
      WEIGHT(3)=0.0625
      GO TO 81
 66   CONTINUE
C
C        INVERSE HANNING
C
      WEIGHT(1)=2.89
      WEIGHT(2)=-0.595
      WEIGHT(3)=0.1225
      GO TO 81
 91   CONTINUE
C
C     3-D LEAST SQUARES
C
      NSTEP=MAX1(P1,1.0)
      ITMAX=NSTEP
      GOTO 81
   70 CONTINUE
C
C        LEISE FILTERING
C
      IF (IP.EQ.9) THEN
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
      NSTEP=MAX1(P1,1.0)
      NSTMAX=0
      CHECK=AMAX0(N1,N2)*0.2
   71 CONTINUE
         NSTMAX=NSTMAX+1
         TEST=2**NSTMAX
         IF(TEST.LE.CHECK) GO TO 71
      NSTEP=MIN0(NSTEP,NSTMAX)
      WEIGHT(1)=NSTEP
      ITMAX=MAX1(P1,1.0)
      GOTO 81
 76   CONTINUE
C
C     2-D LEAST SQUARES
C
      ITMAX=MAX1(P1,1.0)
      WEIGHT(1)=ITMAX
 81   CONTINUE
C
C        INITIATE THE FILTERING PROCESS
C
C
C        SEND A SUMMARY OF THIS EDITING STEP TO THE PRINT FILE
C
         CALL SHOEDW(IPR)
         CALL SHOEDF(IPR)
         WRITE(IPR,108) (NAMINF(I),I=1,4),(NAMOUF(I),I=1,4)
  108    FORMAT(/'  INPUT FIELD: ',4A2/' OUTPUT FIELD: ',4A2)
         WRITE (IPR,115) NOFILT(IP)
  115    FORMAT (/3X,A15,' FILTERING PERFORMED')
      IF(IP.EQ.4.OR.IP.EQ.7.OR.IP.EQ.9) WRITE(IPR,117) NSTEP
  117 FORMAT('   NUMBER OF FILTER STEPS AS DEFINED BY LEISE:',I3)
         IF(IP.EQ.5 .OR. IP.EQ.8) WRITE(IPR,110) ITMAX
  110 FORMAT('   NO. OF GRID STEPS OUTWARD FROM CENTER IN LEAST SQUARES'
     X      ,' REGION:',I3)
C
      IF(IP.EQ.7) THEN
C
C     3-D LEISE FILTERING- FIRST CHECK IF SUFFICIENT STORAGE
C
         M1=IEDW(2,1)-IEDW(1,1)+1
         M2=IEDW(2,2)-IEDW(1,2)+1
         M3=IEDW(2,3)-IEDW(1,3)+1
         IF(M1*M2*M3.GT.MAXBSZ) THEN
            WRITE(IPR,118) M1,M2,M3,MAXBSZ
  118       FORMAT(/5X,'+++  (X,Y,Z) WINDOWED REGION:  (',I3,',',I3,
     X         ',',I3,')'/10X,'MAY NOT CONTAIN MORE THAN ',I7,
     X         ' POINTS  +++')
            CALL CEDERX(518,1)
            RETURN
         END IF
C
         CALL GET3D(IEDFL,IBUF,OBUF,NX,NY,NZ,IFLD,JFLD,NEWJ,MISV,
     X              RBUF,IEDW,NST)
         CALL EXTEND(RBUF,M1,M2,M3,BAD)
         CALL T5FLTR(RBUF,M1,M2,M3,NSTEP)
         CALL PUT3D(IEDFL,IBUF,OBUF,NX,NY,NZ,IFLD,JFLD,NEWJ,MISV,
     X              RBUF,IEDW,NST)
         GO TO 190
      END IF
      IF (IP.EQ.8) THEN
C
C     3-D LEAST SQUARES- FIRST CHECK IF SUFFICIENT STORAGE
C
         IOPT=1
         NOCT=0
         RMNPTS=0.0
         M1=IEDW(2,1)-IEDW(1,1)+1
         M2=IEDW(2,2)-IEDW(1,2)+1
         M3=IEDW(2,3)-IEDW(1,3)+1
         IF(M1*M2*M3.GT.MAXBSZ) THEN
            WRITE(IPR,118) M1,M2,M3,MAXBSZ
            CALL CEDERX(518,1)
            RETURN
         END IF

         CALL GET3D(IEDFL,IBUF,OBUF,NX,NY,NZ,IFLD,JFLD,NEWJ,MISV,
     X              RBUF,IEDW,NST)
         CALL COPRX(SBUF,RBUF,MAXBSZ)
         CALL EXTEND(SBUF,M1,M2,M3,BAD)
         CALL FILL3D(RBUF,SBUF,NSTEP,NOCT,RMNPTS,M1,M2,M3,BAD,IEDW,
     X               IOPT)
         CALL PUT3D(IEDFL,IBUF,OBUF,NX,NY,NZ,IFLD,JFLD,NEWJ,MISV,
     X              RBUF,IEDW,NST)
         GOTO 190
      END IF

      IF (IP.EQ.9) THEN
C
C     1-D LEISE FILTERING
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
                     CALL DOFILT(OBUF,IBUF,RBUF(LV),NX,1,BAD,
     X                    WEIGHT,IEDW(1,1),IEDW(2,1),1,1,IP)
                  END DO
               ELSE IF (IFAX.EQ.'Y') THEN
                  DO L1=IEDW(1,1),IEDW(2,1)
                     DO L2=1,NY
                        LV=NX*(L2-1) + L1
                        SBUF(L2)=RBUF(LV)
                     END DO
                     CALL DOFILT(OBUF,IBUF,SBUF,NY,1,BAD,
     X                    WEIGHT,IEDW(1,2),IEDW(2,2),1,1,IP)
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
                     CALL DOFILT(OBUF,IBUF,SBUF,NZ,1,BAD,
     X                    WEIGHT,IEDW(1,3),IEDW(2,3),1,1,IP)
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
         GOTO 190
      END IF
         
C
C        LOOP THROUGH CONSTANT-Z PLANES
C
      IS=IEDW(1,I1)
      IF=IEDW(2,I1)
      JS=IEDW(1,I2)
      JF=IEDW(2,I2)
      DO 189 LEV=1,N3
         IF(LEV.GE.IEDW(1,I3).AND.LEV.LE.IEDW(2,I3)) GO TO 82
C
C           PLANE IS OUTSIDE EDITING WINDOW
C
            IF(.NOT.NEWJ) GO TO 189
               CALL FETCHD(IEDFL,ID,LEV,IFLD,IBUF,RBUF,
     X                     NIX,NIY,I3,BAD,ZLEV,NST)
               GO TO 188
   82    CONTINUE
C
C           PROCEED WITH THE FILTERING OF THIS PLANE
C
         CALL FETCHD(IEDFL,ID,LEV,IFLD,IBUF,RBUF,NIX,NIY,I3,BAD,
     X               ZLEV,NST)
         CALL DOFILT(OBUF,IBUF,RBUF,N1,N2,BAD,WEIGHT,IS,IF,JS,JF,IP)
 188     CONTINUE
C
C           FINISHED WITH FILTERING THIS PLANE- WRITE OUT RESULTS
C
         CALL PLACED(IEDFL,ID,LEV,JFLD,IBUF,RBUF,N1,N2,I3,BAD,NST)
  189 CONTINUE
C
  190 CONTINUE
C
      NST=0
      RETURN
      END
