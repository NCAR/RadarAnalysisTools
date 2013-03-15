      SUBROUTINE DFCLRS(IOPT)
C     
C     GENERATE COLOR TABLE:   (By John Tuttle)
C     Routine GSCR sets Red-Green-Blue mix for color table.
C     64 COLORS ASSOCIATED WITH INDICES 0-63
C     Indices: (0) Background, (1) Foreground
C              Rainbow - 
C                 (2-60) Normal rainbow graduated color table
C                   (61) Light gray for mid-level rainbow
C                   (62) Reddish ?
C                   (63) White
C                  (255) Black
C              Gray - 
C                 (2-63) Normal gray graduated color table
C                   (65) Almost white for mid-level gray
C
C     IOPT=0: Set RAINBOW color table (Background - Black or White)
C          1: Set GRAY color table (Background - White)
C          2: Set background (0) to WHITE and foreground (1) to BLACK
C          3: Set background (0) to BLACK and foreground (1) to WHITE
C
C     BACKGROUND (COLOR INDEX 0): BLACK - CALL GSCR(1,0,0.,0.,0.)
C                                 WHITE - CALL GSCR(1,0,1.,1.,1.)
C     FOREGROUND (COLOR INDEX 1): BLACK - CALL GSCR(1,1,0.,0.,0.)
C                                 WHITE - CALL GSCR(1,1,1.,1.,1.)
C     
      PARAMETER (BLACK=0, WHITE=1)
      DATA BACK/-1/
      
      IF (IOPT.EQ.0) THEN

C        Define RAINBOW color table
C
         IF (BACK.EQ.-1 .OR. BACK.EQ.BLACK) THEN

C           SET BACKGROUND (0) = Black (0,0,0)
C               FOREGROUND (1) = White (1,1,1)
C     
            R=0.0
            G=0.0
            B=0.0
            IC=0
            CALL GSCR(1,IC,R,G,B)
            R=1.0
            G=1.0
            B=1.0
            IC=1
            CALL GSCR(1,IC,R,G,B)

         ELSE IF (BACK.EQ.WHITE) THEN

C           SET BACKGROUND (0) = White (1,1,1)
C               FOREGROUND (1) = Black (0,0,0)
C     
            R=1.0
            G=1.0
            B=1.0
            IC=0
            CALL GSCR(1,IC,R,G,B)
            R=0.0
            G=0.0
            B=0.0
            IC=1
            CALL GSCR(1,IC,R,G,B)
         END IF
            
C        DEFINE COLORS ASSOCIATED WITH INDICES IC=2-6
C     
         IC=1
         R=0.8
         G=0.0
         B=0.6
         DO 50 I=2,6
            IC=IC+1
            CALL GSCR(1,IC,R,G,B)
            R=R-0.8/7.
 50      CONTINUE
            
C        DEFINE COLORS ASSOCIATED WITH INDICES IC=7-10
C     
         R=0.0
         G=0.0
         B=0.6
         DO 51 I=1,4
            IC=IC+1
            B=B+.4/5.
            CALL GSCR(1,IC,R,G,B)
 51      CONTINUE
            
C        DEFINE COLORS ASSOCIATED WITH INDICES IC=11-17
C     
         R=0.0
         B=1.0
         G=0.2
         DO 52 I=1,7
            IC=IC+1
            G=G+0.8/7.
            CALL GSCR(1,IC,R,G,B)
 52      CONTINUE
            
C        DEFINE COLORS ASSOCIATED WITH INDICES IC=18-31
C     
         R=0.0
         G=0.4
         B=0.0
         DO 53 I=1,14
            IC=IC+1
            CALL GSCR(1,IC,R,G,B)
            G=G+.4/13.
 53      CONTINUE
            
C        DEFINE COLORS ASSOCIATED WITH INDICES IC=32-38
C     
         R=0.8
         G=0.45
         B=0.0
         DO 54 I=1,7
            IC=IC+1
            CALL GSCR(1,IC,R,G,B)
            G=G+.2/6.
 54      CONTINUE
            
C        DEFINE COLORS ASSOCIATED WITH INDICES IC=39-45
C     
         R=0.8
         G=0.65
         B=0.0
         DO 55 I=1,7
            IC=IC+1
            G=G+.34/7.
            CALL GSCR(1,IC,R,G,B)
            R=R+0.19/6.
 55      CONTINUE
            
C        DEFINE COLORS ASSOCIATED WITH INDICES IC=46-53
C     
         R=1.0
         G=0.30
         B=0.0
         DO 56 I=1,8
            IC=IC+1
            CALL GSCR(1,IC,R,G,B)
            G=G+.45/7.
 56      CONTINUE
            
C        DEFINE COLORS ASSOCIATED WITH INDICES IC=54-60
C     
         R=0.6
         G=0.0
         B=0.0
         DO 57 I=1,7
            IC=IC+1
            CALL GSCR(1,IC,R,G,B)
            R=R+.4/6.
 57      CONTINUE
            
C        DEFINE COLORS ASSOCIATED WITH INDICES IC=61-63, 255
C           LIGHT GRAY - 61
C           REDDISH    - 62
C           WHITE      - 63
C           BLACK      - 255
C     
         CALL GSCR(1,61,0.8,0.8,0.8)
         CALL GSCR(1,62,1.,.2,.2)
         CALL GSCR(1,63,1.,1.,1.)
         CALL GSCR(1,255,0.,0.,0.)
         
      ELSE IF (IOPT.EQ.1) THEN

C        Define GRAY COLOR TABLE
C        SET BACKGROUND (0) = White (1,1,1)
C            FOREGROUND (1) = Black (0,0,0)
C     
         R=1.0
         G=1.0
         B=1.0
         IC=0
         CALL GSCR(1,IC,R,G,B)
         R=0.0
         G=0.0
         B=0.0
         IC=1
         CALL GSCR(1,IC,R,G,B)

C        DEFINE GRAY SHADES (Indices 2-63)
C     
         DO 100 I=2,63
            IC=I
            R = 0.95 - (I-1)*(.65/63.)
c            R = 0.99 - (I-1)*(.99/63.)
            CALL GSCR(1,IC,R,R,R)
 100     CONTINUE

C        Special entry for GRAY mid-level value (Index=65)
C        Almost white (1,1,1)
C
         R=0.975
         CALL GSCR(1,65,R,R,R)
         
      ELSE IF (IOPT.EQ.2) THEN

C        SET BACKGROUND (0) = White (1,1,1)
C            FOREGROUND (1) = Black (0,0,0)
C
         R=1.0
         G=1.0
         B=1.0
         IC=0
         CALL GSCR(1,IC,R,G,B)
         R=0.0
         G=0.0
         B=0.0
         IC=1
         CALL GSCR(1,IC,R,G,B)
         BACK=WHITE

      ELSE IF (IOPT.EQ.3) THEN

C        SET BACKGROUND (0) = Black (0,0,0)
C            FOREGROUND (1) = White (1,1,1)
C
            R=0.0
            G=0.0
            B=0.0
c-----------------------------------------------------------
c     Temporarily replace black background with a light blue
c     for plotting radial velocities as vectors, along with 
c     the final (U,V) winds to illustrate orthogonalization
c     geometry.
c            R=0.0
c            B=1.0
c            IG=17
c            G=0.2+(IG-11)*(0.8/7.0)
c-----------------------------------------------------------
         IC=0
         CALL GSCR(1,IC,R,G,B)
         R=1.0
         G=1.0
         B=1.0
         IC=1
         CALL GSCR(1,IC,R,G,B)
         BACK=BLACK
      END IF
      
      RETURN
      END
