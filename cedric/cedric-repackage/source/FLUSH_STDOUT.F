      SUBROUTINE FLUSH_STDOUT
C
C     Routine to flush contents of stdout (Unit 6) 
C     when a run-time error occurs.
C
      INTEGER LUNIT
      DATA LUNIT /6/
C
C This EXTERNAL was used when building with g77, I think related to the
C option -funix-intrinsics-enable being the default.  However, it is not
C needed when building with gfortran, I suspect because gfortran is more
C strict about not exposing unix instrinsics, and so the EXTERNAL is not
C needed to bind to the FORTRAN standard FLUSH statement.
C 
C      EXTERNAL FLUSH

      CALL FLUSH (LUNIT)
      STOP

      END
