c
c----------------------------------------------------------------------X
c
      SUBROUTINE COLTAB (CL,NL,ITABLE)
C
C   ROUTINE TO ASSIGN COLORS TO CONTOUR INTERVALS
C   LJM - Modified May 03, 1996 to allow non-uniform contour intervals
C
      COMMON/COTABLE/ICOL(100)
      DIMENSION CL(100)

      CL(NL+1)=9999.0
      DO I=1,100
         ICOL(I)=0
      END DO
      
      IF(ITABLE .EQ. 1) THEN
         
C        COLOR TABLE 1 - RAINBOW (GRAY) with gray (darkest gray) 
C                        for values above the highest contour level.
C     
         IC=1
         ICOL(1)=1
         ICOL(NL)=61
         IF(NL.LE.2)RETURN
         NLEV=NL-2
         ILEV=2
 10      INCOL=NINT((60.-FLOAT(IC))/FLOAT(NLEV))
         IF(INCOL.LT.1)INCOL=1
         IC=IC+INCOL
         ICOL(ILEV)=IC
         ILEV=ILEV+1
         NLEV=NLEV-1
         IF(ILEV.LT.NL.OR.NLEV.GT.0)GO TO 10
         RETURN
         
      ELSE

C        COLOR TABLE 2 - RAINBOW (GRAY) with gray (white) bracketing 
C                        zero values. Used to distinguish negative and 
C                        positive ranges.
C
C        Below first contour level; set color index to background.
C
         ILEV=2
         RLEVMX=31.
         IC=1
         J=1
         C1=CL(J)
         C2=CL(J+1)
         IF(C1.GT.0.)THEN
            IC=32
            RLEVMX=60.
         END IF
         ICOL(1)=IC
         J=J+1
         C1=CL(J)
         C2=CL(J+1)

C        Set color indices within the three separate ranges
C           clow .lt. 0 and chigh .le. 0
C           clow .le. 0 and chigh .gt. 0 [Use gray (white) here]
C           clow .gt. 0 and chigh .gt. 0
C

 50      IF(C1.LT.0..AND.C2.LE.0.)THEN
            CINC=C2-C1
            IF(CINC.GT.0.0)THEN
               INCOL=(RLEVMX-IC)/FLOAT(INT(-C1/CINC+.0001))
               IF(INCOL.LT.1)INCOL=1
               IC=IC+INCOL
               ICOL(ILEV)=IC
            END IF
            GO TO 60
         END IF

C        Set index bracketing zero and first index past zero
C
         IF(C1.LE.0..AND.C2.GT.0.)THEN
            ICOL(ILEV)=61
            IC=32
            ILEV=ILEV+1
            ICOL(ILEV)=32
            J=J+1
            C1=CL(J)
            C2=CL(J+1)
            RLEVMX=60.
            GO TO 60
         END IF

         IF(C1.GT.0..AND.C2.GT.0.)THEN
            CINC=C2-C1
            IF(CINC.GT.0.0)THEN
               INCOL=(RLEVMX-IC)/FLOAT(INT((CL(NL)-C1)/CINC+1.0001))
               IF(INCOL.LT.1)INCOL=1
               IC=IC+INCOL
               ICOL(ILEV)=IC
            END IF
         END IF

 60      J=J+1
         C1=CL(J)
         C2=CL(J+1)
         ILEV=ILEV+1
         IF(ILEV.LE.NL)GO TO 50
         RETURN

      END IF

      END
