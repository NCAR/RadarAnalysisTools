      SUBROUTINE SCLCAL(CL,NL,SCLSET)
C
C        SETS UP PARAMETERIZATION FOR THE DIGITIZATION OF DATA VALUES
C             ON A CONTOUR PLOT
C
      DIMENSION CL(NL)
      CHARACTER*24 JTIT
      COMMON /CONDTZ/ SCL,NCHR,IENMIN,IENMAX,NACCR,IFMT(8)
C
C        SCL- SCALE FACTOR TO MULTIPLY THE DATA BY
C       NCHR- NUMBER OF CHARACTERS PER DIGITIZED DATUM
C      NACCR- WIDTH IN (1024) CRT UNITS ACROSS THE PLOTTING WINDOW
C              (SEE RGINI FOR THIS INFORMATION)
C
      DATA SCL,NCHR,NACCR/1.0, 5, 885/
      DATA IENMIN,IENMAX/ -10000, 100000 /
      DATA IFMT/'(','I','5',')',' ',' ',' ',' ' /
      IF(SCLSET.EQ.0.0) GO TO 1
            SCL=SCLSET
            GO TO 20
    1 CONTINUE
      TEST=(ABS(CL(NL)-CL(1))/NL) * 1.5
      IF(TEST.EQ.0.0) TEST=10.0
      SCL=1.0
    5 CONTINUE
C
C        ENSURE THAT THE DIGITIZED NUMBER IS LESS THAN 500.
C
      IF(TEST*SCL.LT.500.) GO TO 10
      SCL=SCL*0.1
      GO TO 5
   10 CONTINUE
C
C           ... AND GREATER THAN 5.
C
      IF(TEST*SCL.GE.5.0) GO TO 20
      SCL=SCL*10.0
      GO TO 10
   20 CONTINUE
      WRITE (JTIT,101)SCL
  101 FORMAT('( X ',F7.2,' )')
      CALL PLCHMQ(CPUX(230),CPUY(985),JTIT(1:13),12.,0.,-1.)
      RETURN
      END
