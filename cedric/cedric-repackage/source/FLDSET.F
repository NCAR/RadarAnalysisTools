c------------------------------------------------------------
c     If more than one call to FLDSET is in the same routine,
c     Linux can give you a warning about disagreement between
c     parameter types.  
c     For example - SUBROUTINE PLTDRV:
c        Call FLDSET  if 3rd and 4th parameters are integers.
c             FLDSETR if 3rd and 4th parameters are floats.
c------------------------------------------------------------
c
      SUBROUTINE FLDSET(NAMIN,NAMF2,IARY,ISET,NST)
C     
C     ELEMENTS IN IARY ARE SET TO ISET DEPENDING UPON
C     THE SPECIFICATION OF NAMIN. IF NAMIN IS A FIELD NAME
C     THAT FIELD WILL BE RESET IN IARY AT THE CORRESPONDING
C     POSITION. OTHER VALUES OF NAMIN ARE: PRIMARY,SECONDARY AND ALL.
C     NST=0, O.K.   =1, INDIVIDUAL FIELD CANNOT BE FOUND.
C     
      INCLUDE 'CEDRIC.INC'
      PARAMETER (MXPLTS=3*MXVSCT+3*NFMAX)
      COMMON /SETWND/ ISETW(2,3),PSETW(2,3),ISETFL(NFMAX)
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X                IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X                NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      COMMON /PLTORD/ IPLOTTP(MXPLTS),IPLTFLD(MXPLTS),NPLTS,IPLTFLG,
     X     IPLTCUR
      DIMENSION IARY(NFMAX)
      CHARACTER*8 CTEMP
      CHARACTER*2 NAME(4),IBL
      CHARACTER*3 IAPS(3),ITEST
      CHARACTER*(*) NAMIN,NAMF2(4,NFMAX)
      INTEGER CVMGZ
      INTEGER CVMGN
      DATA IAPS/'PRI','SEC','ALL'/
      DATA IBL/' '/
      NST=0
      
      
      WRITE (CTEMP,50)NAMIN
 50   FORMAT(A8)
      READ (CTEMP,100)ITEST
 100  FORMAT(A3)
      L=IFINDC(ITEST,IAPS,3,0)
      IF(NAMIN.EQ.IBL) L=1
      IF(L.EQ.0) GO TO 10
C     
C     FIELD GROUP
C     
      IF(L.NE.3) THEN
         DO 5 I=1,NFMAX
            IARY(I)=CVMGZ(ISET,IARY(I),ISETFL(I)-L)
            IF (IPLTFLG.EQ.1) THEN
C
C     FLDSET WAS CALLED FOR PLOTTING PURPOSES; SETUP PLOT ARRAYS
C
               IF ((ISETFL(I)-L).EQ.0) THEN
                  MATCH=0
                  DO J=1,NPLTS
                     IF (IPLOTTP(J).EQ.IPLTCUR) THEN
                        IF (IPLTFLD(J).EQ.MAPVID(I,1)) MATCH=1
                     END IF
                  END DO
                  IF (MATCH.EQ.0) THEN
C
C     NEW PLOT; ADD IT TO ARRAYS
C
                     NPLTS=NPLTS+1
                     IF (NPLTS.GT.MXPLTS) THEN
                        CALL CEDERX(541,1)
                        NPLTS=NPLTS-1
                        NST=1
                        RETURN
                     END IF
                     IPLOTTP(NPLTS) =IPLTCUR
                     IPLTFLD(NPLTS)=MAPVID(I,1)
                  END IF
               END IF
            END IF

    5    CONTINUE
      ELSE
         DO 6 I=1,NFMAX
            IARY(I)=CVMGN(ISET,IARY(I),ISETFL(I))
            IF (IPLTFLG.EQ.1) THEN
C
C     FLDSET WAS CALLED FOR PLOTTING PURPOSES; SETUP PLOT ARRAYS
C
               IF (ISETFL(I).NE.0) THEN
                  MATCH=0
                  DO J=1,NPLTS
                     IF (IPLOTTP(J).EQ.IPLTCUR) THEN
                        IF (IPLTFLD(J).EQ.MAPVID(I,1)) MATCH=1
                     END IF
                  END DO
                  IF (MATCH.EQ.0) THEN
C
C     NEW PLOT; ADD IT TO ARRAYS
C
                     NPLTS=NPLTS+1
                     IF (NPLTS.GT.MXPLTS) THEN
                        CALL CEDERX(541,1)
                        NPLTS=NPLTS-1
                        NST=1
                        RETURN
                     END IF
                     IPLOTTP(NPLTS) =IPLTCUR
                     IPLTFLD(NPLTS)=MAPVID(I,1)
                  END IF
               END IF
            END IF

    6    CONTINUE
      END IF
      RETURN
 10   CONTINUE
C     
C     INDIVIDUAL FIELD
C     
      WRITE (CTEMP,50)NAMIN
      READ (CTEMP,101)NAME
 101  FORMAT(4A2)
      I=LOCFLD(NAME,NAMF,4,NFMAX,4)
      IF(I.EQ.0) THEN
         CALL CEDERX(507,0)
         NST=1
      ELSE
         IARY(I)=ISET
         IF (IPLTFLG.EQ.1) THEN
C
C     FLDSET WAS CALLED FOR PLOTTING PURPOSES; SETUP PLOT ARRAYS
C
            MATCH=0
            DO J=1,NPLTS
               IF (IPLOTTP(J).EQ.IPLTCUR) THEN
                  IF (IPLTFLD(J).EQ.MAPVID(I,1)) MATCH=1
               END IF
            END DO
            IF (MATCH.EQ.0) THEN
C
C     NEW PLOT; ADD IT TO ARRAYS
C
               NPLTS=NPLTS+1
               IF (NPLTS.GT.MXPLTS) THEN
                  CALL CEDERX(541,1)
                  NPLTS=NPLTS-1
                  NST=1
                  RETURN
               END IF
               IPLOTTP(NPLTS) =IPLTCUR
               IPLTFLD(NPLTS)=MAPVID(I,1)
            END IF
         END IF
      END IF
      RETURN
      END

      SUBROUTINE FLDSETR(NAMIN,NAMF2,IARY,ISET,NST)
C     
C     ELEMENTS IN IARY ARE SET TO ISET DEPENDING UPON
C     THE SPECIFICATION OF NAMIN. IF NAMIN IS A FIELD NAME
C     THAT FIELD WILL BE RESET IN IARY AT THE CORRESPONDING
C     POSITION. OTHER VALUES OF NAMIN ARE: PRIMARY,SECONDARY AND ALL.
C     NST=0, O.K.   =1, INDIVIDUAL FIELD CANNOT BE FOUND.
C     
      INCLUDE 'CEDRIC.INC'
      PARAMETER (MXPLTS=3*MXVSCT+3*NFMAX)
      COMMON /SETWND/ ISETW(2,3),PSETW(2,3),ISETFL(NFMAX)
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X                IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X                NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      COMMON /PLTORD/ IPLOTTP(MXPLTS),IPLTFLD(MXPLTS),NPLTS,IPLTFLG,
     X     IPLTCUR
      DIMENSION IARY(NFMAX)
      CHARACTER*8 CTEMP
      CHARACTER*2 NAME(4),IBL
      CHARACTER*3 IAPS(3),ITEST
      CHARACTER*(*) NAMIN,NAMF2(4,NFMAX)
      INTEGER CVMGZ
      INTEGER CVMGN
      DATA IAPS/'PRI','SEC','ALL'/
      DATA IBL/' '/
      NST=0
      
      
      WRITE (CTEMP,50)NAMIN
 50   FORMAT(A8)
      READ (CTEMP,100)ITEST
 100  FORMAT(A3)
      L=IFINDC(ITEST,IAPS,3,0)
      IF(NAMIN.EQ.IBL) L=1
      IF(L.EQ.0) GO TO 10
C     
C     FIELD GROUP
C     
      IF(L.NE.3) THEN
         DO 5 I=1,NFMAX
            IARY(I)=CVMGZ(ISET,IARY(I),ISETFL(I)-L)
            IF (IPLTFLG.EQ.1) THEN
C
C     FLDSET WAS CALLED FOR PLOTTING PURPOSES; SETUP PLOT ARRAYS
C
               IF ((ISETFL(I)-L).EQ.0) THEN
                  MATCH=0
                  DO J=1,NPLTS
                     IF (IPLOTTP(J).EQ.IPLTCUR) THEN
                        IF (IPLTFLD(J).EQ.MAPVID(I,1)) MATCH=1
                     END IF
                  END DO
                  IF (MATCH.EQ.0) THEN
C
C     NEW PLOT; ADD IT TO ARRAYS
C
                     NPLTS=NPLTS+1
                     IF (NPLTS.GT.MXPLTS) THEN
                        CALL CEDERX(541,1)
                        NPLTS=NPLTS-1
                        NST=1
                        RETURN
                     END IF
                     IPLOTTP(NPLTS) =IPLTCUR
                     IPLTFLD(NPLTS)=MAPVID(I,1)
                  END IF
               END IF
            END IF

    5    CONTINUE
      ELSE
         DO 6 I=1,NFMAX
            IARY(I)=CVMGN(ISET,IARY(I),ISETFL(I))
            IF (IPLTFLG.EQ.1) THEN
C
C     FLDSET WAS CALLED FOR PLOTTING PURPOSES; SETUP PLOT ARRAYS
C
               IF (ISETFL(I).NE.0) THEN
                  MATCH=0
                  DO J=1,NPLTS
                     IF (IPLOTTP(J).EQ.IPLTCUR) THEN
                        IF (IPLTFLD(J).EQ.MAPVID(I,1)) MATCH=1
                     END IF
                  END DO
                  IF (MATCH.EQ.0) THEN
C
C     NEW PLOT; ADD IT TO ARRAYS
C
                     NPLTS=NPLTS+1
                     IF (NPLTS.GT.MXPLTS) THEN
                        CALL CEDERX(541,1)
                        NPLTS=NPLTS-1
                        NST=1
                        RETURN
                     END IF
                     IPLOTTP(NPLTS) =IPLTCUR
                     IPLTFLD(NPLTS)=MAPVID(I,1)
                  END IF
               END IF
            END IF

    6    CONTINUE
      END IF
      RETURN
 10   CONTINUE
C     
C     INDIVIDUAL FIELD
C     
      WRITE (CTEMP,50)NAMIN
      READ (CTEMP,101)NAME
 101  FORMAT(4A2)
      I=LOCFLD(NAME,NAMF,4,NFMAX,4)
      IF(I.EQ.0) THEN
         CALL CEDERX(507,0)
         NST=1
      ELSE
         IARY(I)=ISET
         IF (IPLTFLG.EQ.1) THEN
C
C     FLDSET WAS CALLED FOR PLOTTING PURPOSES; SETUP PLOT ARRAYS
C
            MATCH=0
            DO J=1,NPLTS
               IF (IPLOTTP(J).EQ.IPLTCUR) THEN
                  IF (IPLTFLD(J).EQ.MAPVID(I,1)) MATCH=1
               END IF
            END DO
            IF (MATCH.EQ.0) THEN
C
C     NEW PLOT; ADD IT TO ARRAYS
C
               NPLTS=NPLTS+1
               IF (NPLTS.GT.MXPLTS) THEN
                  CALL CEDERX(541,1)
                  NPLTS=NPLTS-1
                  NST=1
                  RETURN
               END IF
               IPLOTTP(NPLTS) =IPLTCUR
               IPLTFLD(NPLTS)=MAPVID(I,1)
            END IF
         END IF
      END IF
      RETURN
      END
