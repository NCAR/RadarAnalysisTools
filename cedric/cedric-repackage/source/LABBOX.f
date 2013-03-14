      SUBROUTINE LABBOX(XCRDS,YCRDS,VALS,TYPHILO,NHILO,IAMA)
C
C     THIS SUBROUTINE CONSTRUCTS LITTLE BOXES FOR THE HIGH & LOW
C     LABELS AND ADDS THEM TO THE AREA MAP SO THAT AREA FILLING
C     AND CONTOUR LINES WON'T PASS THROUGH THE BOXES WITH THE
C     LABELS IN THEM.
C
      PARAMETER (MXHILO=1000, NAWRK=2000000)
      DIMENSION XCRDS(MXHILO),YCRDS(MXHILO),VALS(MXHILO),
     X          TYPHILO(MXHILO),XCRA(5),YCRA(5),IAMA(NAWRK)
      DIMENSION XFRA(MXHILO,2),YFRA(MXHILO,2)
      CHARACTER*20 STRING


      CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LL)
      SCALEX=FR-FL
      SCALEY=FT-FB


      DO I=1,NHILO
         
         XLOC=CUFX(XCRDS(I))
         YLOC=CUFY(YCRDS(I))
         TMPVAL=VALS(I)
         IF (TMPVAL.EQ.0.0) TMPVAL=1.0
         NDIG=MAX(0,INT(ALOG10(ABS(TMPVAL)))) + 3
         IF (VALS(I).LT.0.0) NDIG=NDIG+1
C     DEFINE CORNERS OF BOX
         XCRA(1)=MAX(CFUX(XLOC-.015*SCALEX),0.)
         YCRA(1)=MAX(CFUY(YLOC-.018*SCALEY),0.)
         XCRA(2)=CFUX(CUFX(XCRA(1)) + (.018+.011*NDIG)*SCALEX)
         YCRA(2)=YCRA(1)
         XCRA(3)=XCRA(2)
         YCRA(3)=CFUY(CUFY(YCRA(1)) + .035*SCALEY)
         XCRA(4)=XCRA(1)
         YCRA(4)=YCRA(3)
         XCRA(5)=XCRA(1)
         YCRA(5)=YCRA(1)
         XFRA(I,1)=XCRA(1)
         XFRA(I,2)=XCRA(2)
         YFRA(I,1)=YCRA(1)
         YFRA(I,2)=YCRA(3)
         
         IOVERLAP=0
         DO K=1,(I-1)
            IF (((XCRA(1).GE.XFRA(K,1) .AND. XCRA(1).LE.XFRA(K,2)) .OR.
     X          (XCRA(2).GE.XFRA(K,1) .AND. XCRA(2).LE.XFRA(K,2))) .AND.
     X         ((YCRA(1).GE.YFRA(K,1) .AND. YCRA(1).LE.YFRA(K,2)) .OR.
     X          (YCRA(3).GE.YFRA(K,1) .AND. YCRA(3).LE.YFRA(K,2))))THEN 
               IOVERLAP=1
               XFRA(I,1)=0.0
               XFRA(I,2)=0.0
               YFRA(I,1)=0.0
               YFRA(I,2)=0.0
            END IF
         END DO

         IF (IOVERLAP.EQ.0) THEN
C     ADD TO AREA MAP
            CALL AREDAM(IAMA,XCRA,YCRA,5,3,-1,0)

C     CREATE LABEL
            IF (TYPHILO(I).EQ.1) THEN
               STRING='H&B&'
            ELSE
               STRING='L&B&'
            END IF
            IF (NDIG.EQ.3) THEN
               WRITE(STRING(5:),20)VALS(I)
 20            FORMAT(F3.1,'&E')
            ELSE IF (NDIG.EQ.4) THEN
               WRITE(STRING(5:),30)VALS(I)
 30            FORMAT(F4.1,'&E')
            ELSE IF (NDIG.EQ.5) THEN
               WRITE(STRING(5:),40)VALS(I)
 40            FORMAT(F5.1,'&E')
            ELSE IF (NDIG.EQ.6) THEN
               WRITE(STRING(5:),50)VALS(I)
 50            FORMAT(F6.1,'&E')
            ELSE IF (NDIG.EQ.7) THEN
               WRITE(STRING(5:),60)VALS(I)
 60            FORMAT(F7.1,'&E')
            ELSE 
               WRITE(STRING(5:),70)VALS(I)
 70            FORMAT(F8.1,'&E')
            END IF
C     PLOT LABEL INTO BOX
            CALL PLCHHQ(XCRDS(I),YCRDS(I),STRING,10.,0.,0.)
         END IF
            
      END DO

      RETURN

      END
