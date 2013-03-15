      SUBROUTINE SETCOL(CLB,NL,ZMX,ZMN,IPLTYP)
C     
C     THIS SUBROUTINE SETS UP THE ACTUAL COLORS THAT WILL BE USED 
C     BY THE AREA FILL ROUTINES BY CHOOSING COLORS FROM THE 
C     COLOR TABLE DEFINED BY SUBROUTINE DFCLRS
C     
      
      PARAMETER (MAXLEV=61)
      COMMON /COLORS/ ICOL(MAXLEV),IAIM
      DIMENSION CLB(MAXLEV)
      REAL NAL,NMEAN,NALLM,NALGM
      DATA NUMCOLORS /30/
      DIMENSION IPAT3(4)
      DATA IPAT3/2,10,18,0/
      
      CMX=ZMX
      CMN=ZMN
      NAL=0.0
      NALLM=0.0
      NALGM=0.0
      NMEAN=-1.0
      CLBMEAN=(CLB(1)+CLB(NL))/2
      DO 25 I=1,NL
         NAL=NAL+1
 9       IF (I.NE.NL) THEN
            IF (NMEAN.EQ.-1 .AND. (CLBMEAN.GE.CLB(I) .AND.
     X           CLBMEAN.GE.CLB(I+1))) NALLM=NALLM+1
            IF (NMEAN.EQ.-1 .AND. (CLBMEAN.GE.CLB(I) .AND. 
     X           CLBMEAN.LT.CLB(I+1))) NMEAN=NAL
         END IF
         IF (NMEAN.NE.-1 .AND. NMEAN.NE.NAL) NALGM=NALGM+1
 25   CONTINUE
      
      DO 50 I=1,MAXLEV
         ICOL(I)=0
 50   CONTINUE
      
      NUMACT=0
      NUMTOT=NALLM+NALGM

      IF (IPLTYP.EQ.1 .OR. IPLTYP.EQ.3 .OR. 
     X    IPLTYP.EQ.10 .OR. IPLTYP.EQ.11 .OR. IPLTYP.EQ.14) THEN
C     
C     CHOOSE GREY SHADES
C
         DO 45 I=1,NL
            NUMACT=NUMACT+1
            IF (NUMACT.EQ.1) THEN
               ICOL(I)=2
            ELSE IF (NUMACT.EQ.NAL) THEN
               ICOL(I)=63
            ELSE
               ICOL(I)=(2 + (NUMCOLORS*2/NAL)*(NUMACT)) 
            END IF
 45      CONTINUE
         
C     SET MIDDLE BIN TO A VERY LIGHT GREY FOR IPLTYP=14
         IF (IPLTYP.EQ.14) ICOL(((NL+1)/2))=65

      ELSE IF (IPLTYP.EQ.12) THEN
         NLDIV2=(NL+1)/2
C     TRY TO SET THE BIN THAT BRACKETS 0.0 TO BE THE VERY LIGHT GREY;
C     OTHERWISE, WE'LL JUST USE THE MIDDLE OF THE RANGE
         DO I=2,NL
            IF (CLB(I-1).LT.0.0 .AND. CLB(I).GT.0.0) NLDIV2=I-1
         END DO
         DO I=1,NL
            IF (I.LT.NLDIV2) THEN
               ICOL(I)=IPAT3(MOD(NLDIV2-I+2,3)+1)
            ELSE IF (I.GT.NLDIV2) THEN
               ICOL(I)=IPAT3(MOD(I-NLDIV2+2,3)+1)
            ELSE
               ICOL(I)=IPAT3(4)
            END IF
         END DO
            
      ELSE IF (IPLTYP.EQ.15) THEN
         DO I=1,NL
            ICOL(I)=IPAT3(MOD(I+2,3)+1)
         END DO
      ELSE IF (IPLTYP.EQ.4 .OR. IPLTYP.EQ.7) THEN
C
C     CHOOSE COLORS USING THE FULL TABLE
C
         DO 100 I=1,NL
            NUMACT=NUMACT+1
            IF (NUMACT.EQ.NMEAN) THEN
               ICOL(I)=61
            ELSE IF (NUMACT.EQ.1 .AND. NUMACT.NE.NMEAN) THEN
               ICOL(I)=2
            ELSE IF (NUMACT.EQ.NAL .AND. NUMACT.NE.NMEAN) THEN
               ICOL(I)=60
            ELSE IF (NUMACT.LT.NMEAN) THEN
               ICOL(I)=(2 + (NUMCOLORS/NALLM)*(NUMACT-1))
            ELSE IF (NUMACT.GT.NMEAN) THEN
               ICOL(I)=(34+((NUMCOLORS-4)/NALGM)*(NUMACT-NMEAN))
            ELSE
               WRITE(*,*)'***INVALID STATE IN ROUTINE SETCOL***'
               CALL FLUSH_STDOUT
            END IF
 100     CONTINUE
      ELSE IF (IPLTYP.EQ.5 .OR. IPLTYP.EQ.8) THEN
C
C     CHOOSE COLORS USING THE HOT PART OF THE TABLE
C
         DO 150 I=1,NL
            NUMACT=NUMACT+1
            IF (NUMACT.EQ.1) THEN
               ICOL(I)=29
            ELSE IF (NUMACT.EQ.NAL) THEN
               ICOL(I)=60
            ELSE
               ICOL(I)=(29 + (NUMCOLORS/NAL)*(NUMACT-1))
            END IF
 150     CONTINUE
      ELSE IF (IPLTYP.EQ.6 .OR. IPLTYP.EQ.9) THEN
C
C     CHOOSE COLORS USING THE COLD PART OF THE TABLE
C
         DO 200 I=1,NL
            NUMACT=NUMACT+1
            IF (NUMACT.EQ.1) THEN
               ICOL(I)=2
            ELSE IF (NUMACT.EQ.NAL) THEN
               ICOL(I)=33
            ELSE
               ICOL(I)=(2 + (NUMCOLORS/NAL)*(NUMACT-1))
            END IF
 200     CONTINUE
      ELSE
         WRITE(*,*)'***INVALID STATE REACHED IN SETCOL***'
         CALL FLUSH_STDOUT
      END IF
      
      DO 250 I=1,MAXLEV
         IF (ICOL(I).GT.65 .OR. ICOL(I).LT.0) THEN
            WRITE(*,*)'***INVALID COLOR SETTING IN SETCOL***'
            CALL FLUSH_STDOUT
         END IF
 250  CONTINUE
      
      RETURN
      
      END
