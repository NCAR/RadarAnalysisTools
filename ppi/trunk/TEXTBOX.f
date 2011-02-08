c
c----------------------------------------------------------------------X
c
      SUBROUTINE TEXTBOX(XLL,YLL,TEXT,CSIZ)
C
C     Blanks an area with background, draws a box, and puts text in it.
C     Plots with fractional coordinates and restores user coordinates.
C
C        TEXT - text string
C        XLL  - fractional x-coordinate for lower left corner of box
C        YLL  -      "     y-    "       "    "     "     "    "  "
C        CSIZ - character size
C
      CHARACTER*80 TEXT
      INTEGER GETLEN      
      DIMENSION XBOX(5),YBOX(5)

      DATA FACTOR/1.12/

C     Convert to fractional coordinates for plotting text inside a box
C     
      CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)
      CALL SET(FL,FR,FB,FT,FL,FR,FB,FT,LLL)
      FDX=FR-FL
      FDY=FT-FB
      UDX=UR-UL
      UDY=UT-UB
      
C     Convert character size (CSIZ) to fractions of 1024.
C      
      WSIZ=FACTOR*CSIZ/1024.
      HSIZ=2.0*WSIZ
      
C     Get length of character string to be plotted
C     
      KK=GETLEN(TEXT)
      
C     Get the bounds of the box around a character string,
C     fill it with color (index=icolbx) and write string.
C     
      FX1=XLL
      FY1=YLL
      FXL=FX1-WSIZ*0.5
      FXR=FX1+WSIZ*FLOAT(KK+1)
      FYB=FY1-0.51*HSIZ
      FYT=FY1+0.5*HSIZ
      XBOX(1)=FXL
      XBOX(2)=FXR
      XBOX(3)=FXR
      XBOX(4)=FXL
      XBOX(5)=FXL
      YBOX(1)=FYB
      YBOX(2)=FYB
      YBOX(3)=FYT
      YBOX(4)=FYT
      YBOX(5)=FYB

      CALL SFLUSH
c      CALL GSFACI (0)
c      CALL GFA (5,XBOX,YBOX)
      CALL FAREA (0,XBOX,YBOX,5)
      CALL SFLUSH
      
      CALL LINE(XBOX(1),YBOX(1),XBOX(2),YBOX(2))
      CALL LINE(XBOX(2),YBOX(2),XBOX(3),YBOX(3))
      CALL LINE(XBOX(3),YBOX(3),XBOX(4),YBOX(4))
      CALL LINE(XBOX(4),YBOX(4),XBOX(5),YBOX(5))
      CALL PLCHHQ (XLL,YLL,TEXT,CSIZ,0.0,-1.0)      

C     Restore user's coordinates in set call
C
      CALL SET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)
      CALL SFLUSH

      RETURN
      END
