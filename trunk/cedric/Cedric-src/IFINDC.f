      FUNCTION IFINDC(ICON,ISEAR,N,IOCR)
C
C        LOCATES THE IOCR OCCURRENCE OF ICON IN AN ARRAY ISEAR
C                IFIND WILL CONTAIN THE INDEX OF THE REQUESTED
C                ELEMENT IN ISEAR.
C                RETURNS 0 IF REQUEST CANNOT BE SATISFIED.
C
      CHARACTER*(*) ICON, ISEAR(N)
      IF(N.LE.0) GO TO 20
      IC=IOCR
      IF(IC.LE.0) IC=1
      KC=0
      DO 10 I=1,N
         L=I
         IF(ICON.NE.ISEAR(I)) GO TO 10
            KC=KC+1
            IF(KC.GE.IC) GO TO 30
   10 CONTINUE
   20 CONTINUE
      L=0
   30 IFINDC=L
      RETURN
      END
