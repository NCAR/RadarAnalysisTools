      SUBROUTINE FLUSH_STDOUT
C
C     Routine to flush contents of stdout (Unit 6) 
C     when a run-time error occurs.
C
      INTEGER LUNIT
      DATA LUNIT /6/
      EXTERNAL FLUSH

      CALL FLUSH (LUNIT)
      STOP

      END
