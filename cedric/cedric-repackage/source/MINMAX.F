      SUBROUTINE MINMAX(AIN,XCRDS,YCRDS,TYPHILO,VALS,NHILO,
     .                  MI,NI,NJ,NHIC)
C     
C     THIS SUBROUTINE DETERMINES THE HIGH AND LOW REGIONS OF
C     A 2-D FIELD 
C
      INCLUDE 'CEDRIC.INC'
      PARAMETER (MXHILO=1000)
      DIMENSION AIN(MI,NJ)
      DIMENSION TMP1(MAXX),RBUF(MAXX),XCRDS(MXHILO)
      DIMENSION YCRDS(MXHILO),TYPHILO(MXHILO),VALS(MXHILO)
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X                IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X                NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF

C     CALCULATE BOX SIZE BASED ON USER SUPPLIED PARAMETER
      NROW=2*NHIC + 6

C
C     COMPUTE GLOBAL STANDARD DEVIATION AND MEAN OF FIELD
C
      FACC=0.0
      FSQR=0.0
      N=0
      DO J=1,NJ
         DO I=1,NI
            IF (AIN(I,J).NE.BAD) THEN
               FACC=FACC+AIN(I,J)
               FSQR=FSQR+AIN(I,J)**2
               N=N+1
            END IF
         END DO
      END DO
      FAVG=FACC/N
      FSQR=FSQR/N
      FSTD=FSQR-FAVG**2
      IF (FSTD.LT.0.) FSTD=0.
      FSTD=SQRT(FSTD)

C     SEARCH FOR HIGHS AND LOWS
C     REQUIREMENTS FOR HIGH ARE:
C     (1) THE FUNCTIONAL VALUE AT THE POINT MUST BE GREATER 
C         THAN OR EQUAL TO THE VALUE IN THE SQUARE REGION +/- NROW POINTS
C     (2) THE FUNCTIONAL VALUE MUST BE GREATER THAN THE GLOBAL MEAN + STDEV
C

      NHILO=0
      DO J=(2+NROW/2),NJ-1-(NROW/2)
         DO I=(2+NROW/2),NI-1-(NROW/2)
            IMIN=MAX(2,I-NROW)
            IMAX=MIN(NI-1,I+NROW)
            JMIN=MAX(2,J-NROW)
            JMAX=MIN(NJ-1,J+NROW)
            IFLAG=0
            ICNT =0
            IHIGH=0
            ILOW =0
            IF (AIN(I,J).NE.BAD) THEN
               DO JJ=JMIN,JMAX
                  DO II=IMIN,IMAX
                     IF (AIN(II,JJ).NE.BAD) THEN
                        ICNT=ICNT+1
                        IF (AIN(II,JJ).LE.AIN(I,J))   IHIGH=IHIGH+1
                        IF (AIN(II,JJ).GE.AIN(I,J))   ILOW =ILOW+1
                     END IF
                  END DO
               END DO
               IF (AIN(I,J).LE.(FAVG+FSTD) .AND. 
     X              AIN(I,J).GE.(FAVG-FSTD))  IFLAG=1

               IF (IFLAG.EQ.0 .AND. IHIGH.EQ.ICNT) THEN
C     A HIGH HAS BEEN FOUND
                  NHILO=NHILO+1
                  IF (NHILO.LE.MXHILO) THEN
                     XCRDS(NHILO)=I
                     YCRDS(NHILO)=J
                     VALS(NHILO) =AIN(I,J) 
                     TYPHILO(NHILO)=1
                  ELSE IF (NHILO.EQ.MXHILO+1) THEN
                     WRITE(*,30)MXHILO
 30                  FORMAT(/,5X,'+++MAXIMUM NUMBER OF HI/LO LABELS',
     X                    ' PER PLOT IS ',I4,'. LABELS IN EXCESS OF',
     X                    ' THIS QUANTITY HAVE NOT BEEN PLOTTED+++')
                  END IF
               ELSE IF (IFLAG.EQ.0 .AND. ILOW.EQ.ICNT) THEN
C     A LOW HAS BEEN FOUND
                  NHILO=NHILO+1
                  IF (NHILO.LE.MXHILO) THEN
                     XCRDS(NHILO)=I
                     YCRDS(NHILO)=J
                     VALS(NHILO) =AIN(I,J) 
                     TYPHILO(NHILO)=-1
                  ELSE IF (NHILO.EQ.MXHILO+1) THEN
                     WRITE(*,30)MXHILO
                  END IF
               END IF
            END IF
         END DO
      END DO

C     RESET NHILO IF IT OVERFLOWED
      IF (NHILO.GT.MXHILO) NHILO=MXHILO

      RETURN

      END
