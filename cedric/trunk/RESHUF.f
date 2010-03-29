c------------------------------------------------------------
c     If more than one call to RESHUF is in the same routine,
c     Linux can give you a warning about disagreement between
c     parameter types.  
c     For example - SUBROUTINE SYNNER:
c        Call RESHUF  if 1st parameter is an integer.
c             RESHUFR if 1st parameter is a floating point.
c------------------------------------------------------------
c
      SUBROUTINE RESHUF(ROUT,NX,NY,RIN,NIX,NIY,MAP,MAXAXIS,BAD)
C
C         MAPS ONE CARTESIAN SYSTEM INTO ANOTHER BASED UPON
C            INFORMATION IN MAP, WHICH CONTAINS THE INDICES
C            OF THE INPUT ARRAY FOR EACH LOCATION IN THE OUTPUT ARRAY.
C
      DIMENSION ROUT(NX,NY),RIN(NIX,NIY),MAP(MAXAXIS,3)
C
      DO 25 JOT=1,NY
      J=MAP(JOT,2)
C
      IF(J.GT.0) THEN
         DO 10 IOT=1,NX
c            if (rin(iot,jot).ne.0.0 .and. rin(iot,jot).ne.bad) then
c               write(*,*)'***iot,jot,rin(iot,jot)=',iot,jot,
c     X              rin(iot,jot)
c            end if
            I=MAP(IOT,1)
            IF(I.LE.0) THEN
               ROUT(IOT,JOT)=BAD
            ELSE
               ROUT(IOT,JOT)=RIN(I,J)
            END IF
   10    CONTINUE
C
      ELSE
         DO 20 IOT=1,NX
            ROUT(IOT,JOT)=BAD
   20    CONTINUE
C
      END IF
C
   25 CONTINUE
C
      RETURN
      END

      SUBROUTINE RESHUFR(ROUT,NX,NY,RIN,NIX,NIY,MAP,MAXAXIS,BAD)
C
C         MAPS ONE CARTESIAN SYSTEM INTO ANOTHER BASED UPON
C            INFORMATION IN MAP, WHICH CONTAINS THE INDICES
C            OF THE INPUT ARRAY FOR EACH LOCATION IN THE OUTPUT ARRAY.
C
      DIMENSION ROUT(NX,NY),RIN(NIX,NIY),MAP(MAXAXIS,3)
C
      DO 25 JOT=1,NY
      J=MAP(JOT,2)
C
      IF(J.GT.0) THEN
         DO 10 IOT=1,NX
c            if (rin(iot,jot).ne.0.0 .and. rin(iot,jot).ne.bad) then
c               write(*,*)'***iot,jot,rin(iot,jot)=',iot,jot,
c     X              rin(iot,jot)
c            end if
            I=MAP(IOT,1)
            IF(I.LE.0) THEN
               ROUT(IOT,JOT)=BAD
            ELSE
               ROUT(IOT,JOT)=RIN(I,J)
            END IF
   10    CONTINUE
C
      ELSE
         DO 20 IOT=1,NX
            ROUT(IOT,JOT)=BAD
   20    CONTINUE
C
      END IF
C
   25 CONTINUE
C
      RETURN
      END
