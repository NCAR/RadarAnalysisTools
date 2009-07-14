c     
c----------------------------------------------------------------------X
c
      LOGICAL FUNCTION FINDSTRING (STRING,STRINGIN,LEN)
C
C  Check if STRING is contained within the variable STRINGIN
C
C     Returns .FALSE. if STRING is not within STRINGIN
C             .TRUE.   "    "    "        "       "
C
C     STRING   - Declared as CHARACTER IN CALLING ROUTINE
C     STRINGIN -     "     " CHARACTER  "    "       "
C
      CHARACTER *(*) STRING,STRINGIN

C     Compare LEN characters of STRINGIN with STRING
C
      LLEN=LEN-1
      MLEN=LEN+1
      DO 10 I=1,MLEN
         IF(STRINGIN(I:I+LLEN).EQ.STRING(1:LEN))THEN
            FINDSTRING=.TRUE.
            RETURN
         END IF
   10 CONTINUE

C     STRING not found in STRINGIN
C
      FINDSTRING=.FALSE.

      RETURN
      END
