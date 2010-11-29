	Subroutine Contour(count,delta)

	Implicit none
	
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

        Character*1 moveto 
        Integer i,j,ix,iy
        Integer count,line_count
	Real qline_flag1,qline_flag2,qmove_flag1,qmove_flag2
	Real delta
	
C**************************************************

C      Z4______Z3
C       |\    /|
C       | \3 / |
C       |4 \/ 2|
C       |  /\  |
C       | /1 \ |
C       |/____\|
C      Z1      Z2     
C       
C  Each corner of this box represents a data point, with the
C  center of the box being the average of the 4 corners.
C  The box is divided into quadrants.  Initially, the routine 
C  checks to see if there is a contour line on any side of
C  the box (Subroutine FindCont).  It then checks the quadrants 
C  and calculates where on the quadrant the contour is located.
C  (Subroutines LBranch & RBranch).
C
C  
C**************************************************
C CONSTANTS
	bottom_s = 110 !bottom side
	right_s = 120  !right side
	top_s = 130    !top side
	left_s = 140   !left side
        error = 150
	incy = 210   !increment y index
	decx = 220   !decrement x index
	decy = 230   !decrement y index
	incx = 240   !increment x index
	q1_clock=221 !clockwise quad 1
	q2_clock=231 !clockwise quad 2
	q3_clock=241 !clockwise quad 3
	q4_clock=211 !clockwise quad 4
	q1_cclock=242 !counter clockwise quad 1
	q2_cclock=212 !counter clockwise quad 2
	q3_cclock=222 !counter clockwise quad 3
	q4_cclock=232 !counter clockwise quad 4
	bottom_b = 310 !bottom side
	right_b = 320  !right side
	top_b = 330    !top side
	left_b = 340   !left side

C FILL IN THE DATA SO THE LAST ROW=FIRST ROW (360 DEG=0 DEG)
        IF (scan_mode.EQ.'POLA') THEN
	   DO i=1,ii
	      fld_data(i,jj+1,cont)=fld_data(i,1,cont)
	   ENDDO
	   jj=jj+1
	ENDIF

	line_count=1
        moveto='N'

	Write (600,*)  '      % contour level is ',zct 

C SET LINE WIDTH FOR 0 CONTOUR
	IF (zct.EQ.0) THEN
	 Write (600,10) zero_linewidth
10	 Format (3X, F4.2,' setlinewidth')
	ENDIF

C SET DASH PATTERN FOR NEGATIVE CONTOURS
	IF (zct.LT.0) THEN
	 Write (600,11) dash_on,dash_off,dash_beg 
11	 Format (6X, '['I2,1X,I2,'] ',I2,' setdash')
	ENDIF

C OFFSET THE CONTOUR VALUE SO THAT ALL VALUES ARE GREATER
C THAN 0..OTHERWISE CROSSING CAN CAUSE PROBLEMS
	zct = zct+cont_offset 

C IM SIGNIFIES IF A CONTOUR IS LOCATED WITHIN A PARTICULAR INDEX
       DO 5 I=1,II-1
        DO 5 J=1,JJ-1
5 	 IM(I,J) = 0 

C SEARCH FOR A SQUARE THAT CONTAINS A CONTOUR LINE
98 	Continue
	IX = 0
99 	IX = IX + 1
	IY = 0
100 	Continue
	IY = IY + 1
	IF (IY.GE.JJ)        go to  99
	IF (IX.GE.II)        go to 998
	IF (IM(IX,IY).GE.15) go to 100
	Call Getz (ix,iy,delta)
	zmx = amax1(Z1,Z2,Z3,Z4)
	zmn = amin1(Z1,Z2,Z3,Z4)
	IF (zct.GT.zmx.OR.zct.LT.zmn) THEN
	  im(IX,IY) = 16
	  go to 100
	ENDIF

C LOOK AT THE SIDES OF THE SQUARE TO FIND THE START OF A CONTOUR:
C (ALWAYS DRAW CLOCKWISE CONTOURS AROUND A "HIGH" AND
C COUNTER-CLOCKWISE CONTOURS AROUND A "LOW")
        IF ((z1.LT.zct.AND.z2.GT.zct).OR.
     $      (z1.GT.zct.AND.z2.LT.zct)) THEN
           side = bottom_s 
        ELSEIF((z2.LT.zct.AND.z3.GT.zct).OR.
     $         (z2.GT.zct.AND.z3.LT.zct)) THEN
           side = right_s 
        ELSEIF((z3.LT.zct.AND.z4.GT.zct).OR.
     $         (z3.GT.zct.AND.z4.LT.zct)) THEN
           side = top_s 
        ELSEIF((z1.LT.zct.AND.z4.GT.zct).OR.
     $         (z1.GT.zct.AND.z4.LT.zct)) THEN
           side = left_s 
        ENDIF

C FIND THE CONTOUR AND MOVE TO IT
	Call FindCont (ix,iy,moveto)

C GET THE BEGINNING LINE SEGMENT
210	Call EnterSide (ix,iy)

C GET THE DATA VALUES 
	Call Getz (ix,iy,delta)

C SEARCH EACH QUADRANT CLOCKWISE
211	Call LBranch (ix,iy,line_count,moveto)

C END THE LOOP
        IF (endloop.EQ.980) go to 980

        IF (boundary.EQ.bottom_b.OR.boundary.EQ.right_b.OR.
     $      boundary.EQ.top_b.OR.boundary.EQ.left_b) THEN
            go to 310
        ENDIF
        IF (index_inc.EQ.incy.OR.index_inc.EQ.decx.OR.
     $      index_inc.EQ.decy.OR.index_inc.EQ.incx) THEN
           go to 210
        ENDIF
        IF (quad_cclock.EQ.q2_cclock.OR.quad_cclock.EQ.q3_cclock.OR.
     $      quad_cclock.EQ.q4_cclock.OR.quad_cclock.EQ.q1_cclock) THEN
           go to 212
        ENDIF

C SEARCH EACH QUADRANT COUNTER-CLOCKWISE
212	Call  RBranch (ix,iy,line_count,moveto)

C END THE LOOP
	IF (endloop.EQ.980) go to 980

        IF (boundary.EQ.bottom_b.OR.boundary.EQ.right_b.OR.
     $      boundary.EQ.top_b.OR.boundary.EQ.left_b) THEN
           go to 310
        ENDIF
        IF (index_inc.EQ.incy.OR.index_inc.EQ.decx.OR.
     $      index_inc.EQ.decy.OR.index_inc.EQ.incx) THEN
           go to 210
        ENDIF
	IF (endloop.EQ.980) go to 980

C HIT THE BORDER (BOUNDARY)
310	Call HitBord (ix,iy,line_count,moveto,delta)

C END THE LOOP
        IF (endloop.EQ.980) go to 980

        IF (boundary.EQ.bottom_b.OR.boundary.EQ.right_b.OR.
     $      boundary.EQ.top_b.OR.boundary.EQ.left_b) THEN
           go to 310
        ENDIF
        IF (index_inc.EQ.incy.OR.index_inc.EQ.decx.OR.
     $      index_inc.EQ.decy.OR.index_inc.EQ.incx) THEN
           go to 210
        ENDIF

980	Continue
	endloop=0

C........END LINE SEGMENT - DRAW, FILL, PAINT, ETC.
	
         clspath = 0

C PRINT OUT THE LINES TO THE POSTSCRIPT FILE
	 Call PrintLines(line_count)

C CLOSE THE PATH
	 Call Close (count,line_count)

C LABEL THE CONTOURS IF NOT FILLED
	 IF ((fill_flag.EQ.'N'.OR.fill_flag.EQ.'n').AND.
     $   (zct-cont_offset).NE.0) THEN
	    Call LabelContour (count,line_count)
	 ENDIF

C INITIALIZE THE ARRAYS THAT CONTAIN THE LINE INFO
	 Call Initialize (line_count)
	 
C SEARCH FOR NEXT CONTOUR
         go to 98

C IF NECESSARY, DRAW "NON-ZERO WINDING NUMBER RULE" BORDER
C (for PostScript printers)

998	Continue
	
	qmove_flag1=ix1
	qmove_flag2=iy1
       	IF (iedge.EQ.0.AND.fld_data(1,1,cont).GE.zct) THEN
	  Call Qmove(qmove_flag1,qmove_flag2)
	  qline_flag1=ix1
	  qline_flag2=iy2
	  Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
	  qline_flag1=ix2
	  qline_flag2=iy2
	  Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
	  qline_flag1=ix2
	  qline_flag2=iy1
	  Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
	  Call Close (count,line_count)
        ENDIF

C THAT'S ALL FOLKS!
51      Continue
50      Continue
999     Continue	

C FILL THE CONTOUR
        IF (fill_flag.EQ.'Y') Write (600,*)  '      ',count,' o fill'
      	IF (zct-cont_offset.LT.0) THEN
       	 Write (600,*) '      [] 0 setdash'
      	ENDIF

C SET THE LINEWIDTH FOR ZERO CONTOUR 
       IF (zct-cont_offset.EQ.0) THEN
          Write (600,22)  linewidth
22        Format (3X, F4.2,' setlinewidth')
      ENDIF

C SETGRAY BACK TO BLACK
        Write (600,*)  '      0 setgray'

C RETURN NUMBER OF VERTICAL POINTS BACK TO ORIGINAL NUMBER
C (ALTERED AT BEGINNING OF PROGRAM TO FILL IN THE DATA)
        IF (scan_mode.EQ.'POLA') jj=jj-1 

	Return
	End
**************************************************
	Subroutine  RBranch (ix,iy,line_count,moveto)

	Implicit none

	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'
	
	Character*1 moveto
	Integer ix,iy,line_count
	Real qline_flag1,qline_flag2

*	Print*, 'Calling RBranch'

C SEARCH EACH QUADRANT COUNTER CLOCKWISE

10	Continue

C LOOK AT QUADRANT 2
	IF (quad_cclock.EQ.q2_cclock) THEN
C LOOK AT LEFT SIDE OF QUADRANT 2
	 IF ((Z2.LT.ZCT.AND.Z5.GT.ZCT).OR.(Z2.GT.ZCT.AND.Z5.LT.ZCT)) THEN
C CALCULATE END OF LINE SEGMENT
          KX2 = IX1 + NINT(XINT*(IX     - 0.5*(ZCT-Z2)/(Z5-Z2)))
          KY2 = IY1 + NINT(YINT*((IY-1) + 0.5*(ZCT-Z2)/(Z5-Z2)))
C CHECK FOR BAD DATA
          IF (z1.LT.-900.OR.z2.LT.-900.OR.z3.LT.-900.OR.z4.LT.-900) THEN
           moveto = 'Y' 
	   clspath = 1
          ENDIF
	  qline_flag1=kx2
	  qline_flag2=ky2
C STORE COORDINATES
	  Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
C BEGINNING OF LINE SEGMENT=END OF PREVIOUS LINE SEGMENT
          KX1 = KX2
          KY1 = KY2
C LOOK AT BOTTOM OF QUADRANT 2 (RIGHT SIDE OF BOX)
	  IF ((z2.LT.zct.AND.z3.GT.zct).OR.(z2.GT.zct.AND.z3.LT.zct)) THEN
C CALCULATE END OF LINE SEGMENT
           KX2 = IX1 + NINT(XINT*IX)
           KY2 = IY1 + NINT(YINT*((IY-1) + (ZCT-Z2)/(Z3-Z2)))
C END THE LOOP IF BACK AT THE BEGINNING
           IF (KX2.EQ.ISTX.AND.KY2.EQ.ISTY) THEN 
	       endloop = 980
	       go to 11
	   ENDIF
C LOOK FOR BAD DATA
           IF (z1.LT.-900.OR.z2.LT.-900.OR.z3.LT.-900.
     X         OR.z4.LT.-900) THEN
             moveto = 'Y'
	     clspath = 1
          ENDIF
	  qline_flag1=kx2
	  qline_flag2=ky2
C STORE COORDINATES
          Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
C LOOK FOR RIGHT BOUNDARY
           IF (IX.GE.II-1) THEN
            boundary = right_b
	    index_inc = 0
	    quad_cclock = 0
	    go to 11
	   ENDIF
	   index_inc = incx
	   boundary = 0
	   quad_cclock = 0
	   go to 11
          ENDIF
C LOOK AT RIGHT SIDE OF QUADRANT 2
	  IF ((z3.LT.zct.AND.z5.GT.zct).OR.(z3.GT.zct.AND.z5.LT.zct)) THEN
	   quad_cclock = q3_cclock
	   go to 10
          ENDIF
          WRITE(*,812)
812       FORMAT(2X,'PROBLEM:  BLOCK 210-R')
         ENDIF
	 index_inc = decx 
	 boundary = 0
	ENDIF

C LOOK AT QUADRANT 3
	IF (quad_cclock.EQ.q3_cclock) THEN
C LOOK AT LEFT SIDE OF QUADRANT 3
	 IF ((Z3.LT.ZCT.AND.Z5.GT.ZCT).OR.(Z3.GT.ZCT.AND.Z5.LT.ZCT)) THEN
C CALCULATE END OF LINE SEGMENT
          KX2 = IX1 + NINT(XINT*(IX     - 0.5*(ZCT-Z3)/(Z5-Z3)))
          KY2 = IY1 + NINT(YINT*(IY     - 0.5*(ZCT-Z3)/(Z5-Z3)))
C CHECK FOR BAD DATA
          IF (z1.LT.-900.OR.z2.LT.-900.OR.z3.LT.-900.OR.z4.LT.-900) THEN
             moveto = 'Y' 
	     clspath = 1
          ENDIF
	  qline_flag1=kx2
	  qline_flag2=ky2
C STORE COORDINATES
          Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
C BEGINNING OF LINE SEGMENT=END OF PREVIOUS LINE SEGMENT
          KX1 = KX2
          KY1 = KY2
C LOOK AT BOTTOM OF QUADRANT 3 (TOP SIDE OF BOX)
	  IF ((z3.LT.zct.AND.z4.GT.zct).OR.(z3.GT.zct.AND.z4.LT.zct)) THEN
C CALCULATE END OF LINE SEGMENT
           KX2 = IX1 + NINT(XINT*((IX-1) + (ZCT-Z4)/(Z3-Z4)))
           KY2 = IY1 + NINT(YINT*IY)
C END THE LOOP IF BACK AT THE BEGINNING
           IF (KX2.EQ.ISTX.AND.KY2.EQ.ISTY) THEN 
	    endloop = 980
	    go to 11
	   ENDIF
C LOOK FOR BAD DATA
           IF (z1.LT.-900.OR.z2.LT.-900.OR.z3.LT.-900.
     X         OR.z4.LT.-900) THEN
             moveto = 'Y' 
	     clspath = 1
           ENDIF
	   qline_flag1=kx2
	   qline_flag2=ky2
C STORE COORDINATES
           Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
C LOOK FOR TOP BOUNDARY
           IF (IY.GE.JJ-1) THEN
	       boundary = top_b
	       index_inc = 0
	       quad_cclock = 0
	       go to 11
	   ENDIF
           index_inc = incy
	   boundary = 0
	   quad_cclock = 0
	   go to 11
          ENDIF
C LOOK AT RIGHT SIDE OF QUADRANT 3
	  IF ((z4.LT.zct.AND.z5.GT.zct).OR.(z4.GT.zct.AND.z5.LT.zct)) THEN
	   quad_cclock = q4_cclock
	   go to 10
          ENDIF
          WRITE(*,822)
822       FORMAT(2X,'PROBLEM:  BLOCK 220-R')
         ENDIF
	 index_inc = decy 
	 boundary = 0
	ENDIF

C LOOK AT QUADRANT 4
	IF (quad_cclock.EQ.q4_cclock) THEN
C LOOK AT LEFT SIDE OF QUADRANT 4
	 IF ((z4.LT.zct.AND.z5.GT.zct).OR.(z4.GT.zct.AND.z5.LT.zct)) THEN
C CALCULATE END OF LINE SEGMENT
          KX2 = IX1 + NINT(XINT*((IX-1) + 0.5*(ZCT-Z4)/(Z5-Z4)))
          KY2 = IY1 + NINT(YINT*(IY     - 0.5*(ZCT-Z4)/(Z5-Z4)))
C CHECK FOR BAD DATA
          IF (z1.LT.-900.OR.z2.LT.-900.OR.z3.LT.-900.OR.z4.LT.-900) THEN
           moveto = 'Y' 
	   clspath = 1
          ENDIF
	  qline_flag1=kx2
	  qline_flag2=ky2
C STORE COORDINATES
          Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
C BEGINNING OF LINE SEGMENT=END OF PREVIOUS LINE SEGMENT
          KX1 = KX2
          KY1 = KY2
C LOOK AT BOTTOM OF QUADRANT 4 (LEFT SIDE OF BOX)
	  IF ((z1.LT.zct.AND.z4.GT.zct).OR.(z1.GT.zct.AND.z4.LT.zct)) THEN
C CALCULATE END OF LINE SEGMENT
           KX2 = IX1 + NINT(XINT*(IX-1))
           KY2 = IY1 + NINT(YINT*((IY-1) + (ZCT-Z1)/(Z4-Z1)))
C END THE LOOP IF BACK AT THE BEGINNING
           IF (KX2.EQ.ISTX.AND.KY2.EQ.ISTY) THEN
	    endloop = 980
	    go to 11
	   ENDIF 
C LOOK FOR BAD DATA
           IF (z1.LT.-900.OR.z2.LT.-900.OR.z3.LT.-900.
     X         OR.z4.LT.-900) THEN
              moveto = 'Y' 
	      clspath = 1
           ENDIF
	   qline_flag1=kx2
	   qline_flag2=ky2
C STORE COORDINATES
           Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
C LOOK FOR LEFT BOUNDARY
           IF (IX.LE.1) THEN
	    boundary = left_b
	    index_inc = 0
	    quad_cclock = 0
	    go to 11
	   ENDIF
	   index_inc = decx
	   boundary = 0
	   quad_cclock = 0
	   go to 11
          ENDIF
C LOOK AT RIGHT SIDE OF QUADRANT 4
	  IF ((z1.LT.zct.AND.z5.GT.zct).OR.(z1.GT.zct.AND.z5.LT.zct)) THEN
	   quad_cclock = q1_cclock
	   go to 10
          ENDIF
          WRITE(*,832)
832       FORMAT(2X,'PROBLEM:  BLOCK 230-R')
         ENDIF
	 index_inc = incx
	 boundary = 0
	ENDIF

C LOOK AT QUADRANT 1
	IF (quad_cclock.EQ.q1_cclock) THEN
C LOOK AT LEFT SIDE OF QUADRANT 1
	 IF ((Z1.LT.ZCT.AND.Z5.GT.ZCT).OR.(Z1.GT.ZCT.AND.Z5.LT.ZCT)) THEN
C CALCULATE END OF LINE SEGMENT
          KX2 = IX1 + NINT(XINT*((IX-1) + 0.5*(ZCT-Z1)/(Z5-Z1)))
          KY2 = IY1 + NINT(YINT*((IY-1) + 0.5*(ZCT-Z1)/(Z5-Z1)))
C CHECK FOR BAD DATA
          IF (z1.LT.-900.OR.z2.LT.-900.OR.z3.LT.-900.OR.z4.LT.-900) THEN
           moveto = 'Y'
	   clspath = 1
          ENDIF
	  qline_flag1=kx2
	  qline_flag2=ky2
C STORE COORDINATES
          Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
C BEGINNING OF LINE SEGMENT=END OF PREVIOUS LINE SEGMENT
          KX1 = KX2
          KY1 = KY2
C LOOK AT BOTTOM OF QUADRANT 1 (BOTTOM SIDE OF BOX)
	  IF ((z1.LT.zct.AND.z2.GT.zct).OR.(z1.GT.zct.AND.z2.LT.zct)) THEN
C CALCULATE END OF LINE SEGMENT
           KX2 = IX1 + NINT(XINT*((IX-1) + (ZCT-Z1)/(Z2-Z1)))
           KY2 = IY1 + NINT(YINT*(IY-1))
C END THE LOOP IF BACK AT THE BEGINNING
           IF (KX2.EQ.ISTX.AND.KY2.EQ.ISTY) THEN
	    endloop = 980
	    go to 11
	   ENDIF 
C LOOK FOR BAD DATA
           IF (z1.LT.-900.OR.z2.LT.-900.OR.z3.LT.-900.
     X         OR.z4.LT.-900) THEN
             moveto = 'Y'
	     clspath = 1
           ENDIF
	   qline_flag1=kx2
	   qline_flag2=ky2
C STORE COORDINATES
           Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
C LOOK FOR BOTTOM BOUNDARY
           IF (IY.LE.1) THEN
	    boundary = bottom_b
	    index_inc = 0
	    quad_cclock = 0
	    go to 11
	   ENDIF
	   index_inc = decy
	   boundary = 0
	   quad_cclock = 0
	   go to 11
          ENDIF
C LOOK AT RIGHT SIDE OF QUADRANT 1
	  IF ((z2.LT.zct.AND.z5.GT.zct).OR.(z2.GT.zct.AND.z5.LT.zct)) THEN
	   quad_cclock = q2_cclock
	   go to 10
          ENDIF
          WRITE(*,842)
842       FORMAT(2X,'PROBLEM:  BLOCK 240-R')
         ENDIF
	 boundary = bottom_b
	 index_inc = 0
	ENDIF

11	Continue

	moveto = 'N'

	Return
	End
**************************************************
	Subroutine LBranch (ix,iy,line_count,moveto)

	Implicit none

	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Character*1 moveto
	Integer ix,iy,line_count
	Real qline_flag1,qline_flag2

C SEARCH EACH QUADRANT CLOCKWISE

	qline_flag1=kx2
	qline_flag2=ky2

10	Continue

C LOOK AT QUADRANT 4
	IF (quad_clock.EQ.q4_clock) THEN
C LOOK AT RIGHT SIDE OF QUADRANT 4 
	   IF ((z1.LT.zct.AND.z5.GT.zct).OR.
     X         (z1.GT.zct.AND.z5.LT.zct)) THEN
C CALCULATE END OF LINE SEGMENT 
              KX2 = IX1 + NINT(XINT*((IX-1) + 0.5*(ZCT-Z1)/(Z5-Z1)))
              KY2 = IY1 + NINT(YINT*((IY-1) + 0.5*(ZCT-Z1)/(Z5-Z1)))
C CHECK FOR BAD DATA 
	      IF (z1.LT.-900.OR.z2.LT.-900.OR.z3.LT.-900.
     X            OR.z4.LT.-900) THEN
	         moveto = 'Y'
	         clspath = 1
	      ENDIF
	      qline_flag1=kx2
	      qline_flag2=ky2
C STORE COORDINATES 
              Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
C BEGINNING OF LINE SEGMENT=END OF PREVIOUS LINE SEGMENT
              KX1 = KX2
              KY1 = KY2
C LOOK AT BOTTOM OF QUADRANT 4 (LEFT SIDE OF BOX)
	      IF ((z1.LT.zct.AND.z4.GT.zct).OR.
     X            (z1.GT.zct.AND.z4.LT.zct)) THEN
C CALCULATE END OF LINE SEGMENT 
                 KX2 = IX1 + NINT(XINT*(IX-1))
                 KY2 = IY1 + NINT(YINT*((IY-1) + (ZCT-Z1)/(Z4-Z1)))
C END THE LOOP IF BACK AT THE BEGINNING 
	         IF (KX2.EQ.ISTX.AND.KY2.EQ.ISTY) THEN
	           endloop = 980
	           go to 11
	         ENDIF
C LOOK FOR BAD DATA
	         IF (z1.LT.-900.OR.z2.LT.-900.OR.z3.LT.-900.OR.
     X               z4.LT.-900) THEN
	            moveto = 'Y'
	            clspath = 1
	         ENDIF
	         qline_flag1=kx2
	         qline_flag2=ky2
C STORE COORDINATES 
                 Call CalcLine (qline_flag1,qline_flag2,moveto,
     X                          line_count)
C LOOK FOR LEFT BOUNDARY
	         IF (ix.LE.1) THEN
	            boundary = left_b
	            index_inc = 0
	            quad_cclock = 0
	            go to 11
	         ENDIF
	         index_inc = decx
	         boundary = 0
	         quad_cclock = 0
	         go to 11
             ENDIF
C LOOK AT LEFT SIDE OF QUADRANT 4 
	     IF ((z4.LT.zct.AND.z5.GT.zct).OR.
     X           (z4.GT.zct.AND.z5.LT.zct)) THEN
	        quad_clock = q3_clock
	        go to 10
             ENDIF
             WRITE(*,811)
811          FORMAT(2X,'PROBLEM:  BLOCK 210-L')
           ENDIF
	   quad_cclock = q2_cclock
	   boundary = 0
	   index_inc = 0
	ENDIF

C LOOK AT QUADRANT 1
	IF (quad_clock.EQ.q1_clock) THEN
C LOOK AT RIGHT SIDE OF QUADRANT 1
	 IF ((z2.LT.zct.AND.z5.GT.zct).OR.(z2.GT.zct.AND.z5.LT.zct)) THEN
C CALCULATE END OF LINE SEGMENT
	  KX2 = IX1 + NINT(XINT*(IX     - 0.5*(ZCT-Z2)/(Z5-Z2)))
	  KY2 = IY1 + NINT(YINT*((IY-1) + 0.5*(ZCT-Z2)/(Z5-Z2)))
C CHECK FOR BAD DATA
	  IF (z1.LT.-900.OR.z2.LT.-900.OR.z3.LT.-900.OR.z4.LT.-900) THEN
	   moveto = 'Y'
	   clspath = 1
	  ENDIF
	  qline_flag1=kx2
	  qline_flag2=ky2
C STORE COORDINATES
          Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
C BEGINNING OF LINE SEGMENT=END OF PREVIOUS LINE SEGMENT
          KX1 = KX2
          KY1 = KY2
C LOOK AT BOTTOM OF QUADRANT 1 (BOTTOM SIDE OF BOX)
	  IF ((Z1.LT.ZCT.AND.Z2.GT.ZCT).OR.(Z1.GT.ZCT.AND.Z2.LT.ZCT)) THEN
C CALCULATE END OF LINE SEGMENT
	   KX2 = IX1 + NINT(XINT*((IX-1) + (ZCT-Z1)/(Z2-Z1)))
	   KY2 = IY1 + NINT(YINT*(IY-1))
C END THE LOOP IF BACK AT THE BEGINNING
           IF (KX2.EQ.ISTX.AND.KY2.EQ.ISTY) THEN 
	    endloop = 980
	    go to 11
	   ENDIF
C LOOK FOR BAD DATA
	   IF (z1.LT.-900.OR.z2.LT.-900.OR.z3.LT.-900.OR.z4.LT.-900) THEN
	    moveto = 'Y'
	    clspath = 1
	   ENDIF
	  qline_flag1=kx2
	  qline_flag2=ky2
C STORE COORDINATES
          Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
C LOOK FOR BOTTOM BOUNDARY
	    IF (iy.LE.1) THEN
	     boundary = bottom_b
	     index_inc = 0
	     quad_cclock = 0
	     go to 11
	    ENDIF
	    index_inc = decy
	    boundary = 0
	    quad_cclock = 0
	    go to 11
          ENDIF
C LOOK AT SIDE OF LEFT QUADRANT 1
	  IF ((Z1.LT.ZCT.AND.Z5.GT.ZCT).OR.(Z1.GT.ZCT.AND.Z5.LT.ZCT)) THEN
	   quad_clock = q4_clock
	   go to 10
          ENDIF
          WRITE(*,821)
821       FORMAT(2X,'PROBLEM:  BLOCK 220-L')
         ENDIF
	 quad_cclock = q3_cclock
	 boundary = 0
	 index_inc = 0
	ENDIF

C LOOK AT QUADRANT 2
	IF (quad_clock.EQ.q2_clock) THEN
C LOOK AT RIGHT SIDE OF QUADRANT 2
	 IF ((Z3.LT.ZCT.AND.Z5.GT.ZCT).OR.(Z3.GT.ZCT.AND.Z5.LT.ZCT)) THEN
C CALCULATE END OF LINE SEGMENT
	  KX2 = IX1 + NINT(XINT*(IX     - 0.5*(ZCT-Z3)/(Z5-Z3)))
	  KY2 = IY1 + NINT(YINT*(IY     - 0.5*(ZCT-Z3)/(Z5-Z3)))
C CHECK FOR BAD DATA
	  IF (z1.LT.-900.OR.z2.LT.-900.OR.z3.LT.-900.OR.z4.LT.-900) THEN
	   moveto = 'Y'
	   clspath = 1
	  ENDIF
	  qline_flag1=kx2
	  qline_flag2=ky2
C STORE COORDINATES
          Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
C BEGINNING OF LINE SEGMENT=END OF PREVIOUS LINE SEGMENT
          KX1 = KX2
          KY1 = KY2
C LOOK AT BOTTOM OF QUADRANT 2 (RIGHT SIDE OF BOX)
	  IF ((Z2.LT.ZCT.AND.Z3.GT.ZCT).OR.(Z2.GT.ZCT.AND.Z3.LT.ZCT)) THEN
C CALCULATE END OF LINE SEGMENT
	   KX2 = IX1 + NINT(XINT*IX)
	   KY2 = IY1 + NINT(YINT*((IY-1) + (ZCT-Z2)/(Z3-Z2)))
C END THE LOOP IF BACK AT THE BEGINNING
           IF (KX2.EQ.ISTX.AND.KY2.EQ.ISTY) THEN 
	    endloop = 980
	    go to 11
	   ENDIF
C LOOK FOR BAD DATA
	   IF (z1.LT.-900.OR.z2.LT.-900.OR.z3.LT.-900.OR.z4.LT.-900) THEN
	    moveto = 'Y'
	    clspath = 1
	   ENDIF
	  qline_flag1=kx2
	  qline_flag2=ky2
C STORE COORDINATES
          Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
C LOOK FOR RIGHT BOUNDARY
	    IF (ix.GE.ii-1) THEN
	     boundary = right_b
	     index_inc = 0
	     quad_cclock = 0
	     go to 11
	    ENDIF
	    index_inc = incx
	    boundary = 0
	    quad_cclock = 0
	    go to 11
          ENDIF
C LOOK AT LEFT SIDE OF QUADRANT 2
	  IF ((Z2.LT.ZCT.AND.Z5.GT.ZCT).OR.(Z2.GT.ZCT.AND.Z5.LT.ZCT)) THEN
	   quad_clock = q1_clock
	   go to 10
          ENDIF
	  WRITE(*,831)
831       FORMAT(2X,'PROBLEM:  BLOCK 230-L')
         ENDIF
	 quad_cclock = q4_cclock
	 boundary = 0
	 index_inc = 0
	ENDIF

C LOOK AT QUADRANT 3
	IF (quad_clock.EQ.q3_clock) THEN
C LOOK AT RIGHT SIDE OF QUADRANT 3
	 IF ((Z4.LT.ZCT.AND.Z5.GT.ZCT).OR.(Z4.GT.ZCT.AND.Z5.LT.ZCT)) THEN
C CALCULATE END OF LINE SEGMENT
	  KX2 = IX1 + NINT(XINT*((IX-1) + 0.5*(ZCT-Z4)/(Z5-Z4)))
          KY2 = IY1 + NINT(YINT*(IY     - 0.5*(ZCT-Z4)/(Z5-Z4)))
C CHECK FOR BAD DATA
	  IF (z1.LT.-900.OR.z2.LT.-900.OR.z3.LT.-900.OR.z4.LT.-900) THEN
	   moveto = 'Y'
	   clspath = 1
	  ENDIF
	  qline_flag1=kx2
	  qline_flag2=ky2
C STORE COORDINATES
          Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
C BEGINNING OF LINE SEGMENT=END OF PREVIOUS LINE SEGMENT
          KX1 = KX2
          KY1 = KY2
C LOOK AT BOTTOM OF QUADRANT 3 (TOP SIDE OF BOX)
	  IF ((z4.LT.zct.AND.z3.GT.zct).OR.(z4.GT.zct.AND.z3.LT.zct)) THEN
C CALCULATE END OF LINE SEGMENT
           KX2 = IX1 + NINT(XINT*((IX-1) + (ZCT-Z4)/(Z3-Z4)))
           KY2 = IY1 + NINT(YINT*IY)
C END THE LOOP IF BACK AT THE BEGINNING
           IF (KX2.EQ.ISTX.AND.KY2.EQ.ISTY) THEN 
	    endloop = 980
	    go to 11
	   ENDIF
C LOOK FOR BAD DATA
	   IF (z1.LT.-900.OR.z2.LT.-900.OR.z3.LT.-900.OR.z4.LT.-900) THEN
	    moveto = 'Y'
	    clspath = 1
	   ENDIF
	  qline_flag1=kx2
	  qline_flag2=ky2
C STORE COORDINATES
          Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
C LOOK FOR TOP BOUNDARY
	    IF (iy.GE.jj-1) THEN
	     boundary = top_b
	     index_inc = 0
	     quad_cclock = 0
	     go to 11
	    ENDIF
	    index_inc = incy
	    boundary = 0
	    quad_cclock = 0
	    go to 11
          ENDIF
C LOOK AT LEFT SIDE OF QUADRANT 3
	  IF ((z3.LT.zct.AND.z5.GT.zct).OR.(z3.GT.zct.AND.z5.LT.zct)) THEN
	   quad_clock = q2_clock
	   go to 10
          ENDIF
          WRITE(*,850)
850       FORMAT(2X,'PROBLEM:  BLOCK 240-L')
         ENDIF
	 quad_cclock = q1_cclock
	 boundary = 0
	 index_inc = 0
	ENDIF

11	Continue

	moveto = 'N'

	Return
	End
**************************************************
	Subroutine HitBord (ix,iy,line_count,moveto,delta)

	Implicit none

	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Character*1 moveto
	Integer ix,iy,line_count
	Real qline_flag1,qline_flag2
	Real delta

	qline_flag1=kx2
	qline_flag2=ky2

	IF ((scan_mode.EQ.'POLA').AND. 
     $	    (fill_flag.EQ.'Y'.OR.fill_flag.EQ.'y')) THEN
	   moveto = 'A' 
	ELSE
	   moveto = 'Y' 
	ENDIF
	clspath = 1

10	Continue

        IEDGE = 1
        KX1 = KX2
        KY1 = KY2

20	Continue

C BOTTOM BOUNDARY
	IF (boundary.EQ.bottom_b) THEN
C LOWER LEFT CORNER
	   IF (IX.LE.1) THEN
	      KX2 = IX1
C END THE LOOP IF BACK AT THE BEGINNING
              IF (KX2.EQ.ISTX.AND.KY2.EQ.ISTY) THEN
	         endloop = 980
	         go to 11
	      ENDIF 
	      qline_flag1=kx2
	      qline_flag2=ky2
C STORE COORDINATES
              Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
              IY = IY-1
C LOOK AT LEFT BOUNDARY
	      boundary = left_b
	      index_inc = 0
	      go to 10
            ENDIF
	    IX = IX - 1
	    Call BorderGetz(ix,iy,delta)
C FOLLOW BOTTOM BOUNDARY UNTIL CONTOUR IS FOUND
            IF (z1.LT.zct.AND.z2.GT.zct) THEN
	       KX2 = IX1 + NINT(XINT*((IX-1) + (ZCT-Z1)/(Z2-Z1)))
C END THE LOOP IF BACK AT THE BEGINNING
               IF (KX2.EQ.ISTX.AND.KY2.EQ.ISTY) THEN 
	          endloop = 980
	          go to 11
	       ENDIF 
	       qline_flag1=kx2
	       qline_flag2=ky2
C STORE COORDINATES
               Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
               IY = IY-1
	       index_inc=incy
	       boundary = 0
	       go to 11
            ELSE
	       boundary = bottom_b
	       go to 20
	    ENDIF
	ENDIF

C RIGHT BOUNDARY
	IF (boundary.EQ.right_b) THEN
C LOWER RIGHT CORNER
	   IF (IY.LE.1) THEN
	      KY2 = IY1
C END THE LOOP IF BACK AT THE BEGINNING
              IF (KX2.EQ.ISTX.AND.KY2.EQ.ISTY) THEN 
	         endloop = 980
	         go to 11
	      ENDIF 
	      qline_flag1=kx2
	      qline_flag2=ky2
C STORE COORDINATES
              Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
              IX = IX+1
C LOOK AT BOTTOM BOUNDARY
	      boundary = bottom_b
	      index_inc = 0
	      go to 10
            ENDIF
	    IY = IY - 1
            Call BorderGetz(ix,iy,delta)
C FOLLOW RIGHT BOUNDARY UNTIL CONTOUR IS FOUND
            IF (z2.LT.zct.AND.z3.GT.zct) THEN
	       KY2 = IY1 + NINT(YINT*((IY-1) + (ZCT-Z2)/(Z3-Z2)))
C END THE LOOP IF BACK AT THE BEGINNING
               IF (KX2.EQ.ISTX.AND.KY2.EQ.ISTY) THEN 
	          endloop = 980
	          go to 11
	       ENDIF 
	       qline_flag1=kx2
	       qline_flag2=ky2
C STORE COORDINATES
               Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
               IX = IX+1
	       index_inc= decx
	       boundary = 0
	       go to 11
            ELSE
	       boundary = right_b
	       go to 20
            ENDIF
	ENDIF

C TOP BOUNDARY
	IF (boundary.EQ.top_b) THEN
C UPPER RIGHT CORNER
	   IF (IX.GE.II-1) THEN
	      KX2 = IX2
C END THE LOOP IF BACK AT THE BEGINNING
              IF (KX2.EQ.ISTX.AND.KY2.EQ.ISTY) THEN 
	         endloop = 980
	         go to 11
	      ENDIF 
	      qline_flag1=kx2
	      qline_flag2=ky2
C STORE COORDINATES
              Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
              IY = IY+1
C LOOK AT RIGHT BOUNDARY
	      boundary = right_b
	      index_inc = 0
	      go to 10
            ENDIF
	    IX = IX + 1
            Call BorderGetz(ix,iy,delta)
C FOLLOW TOP BOUNDARY UNTIL CONTOUR IS FOUND
            IF (z3.LT.zct.AND.z4.GT.zct) THEN
	       KX2 = IX1 + NINT(XINT*((IX-1) + (ZCT-Z4)/(Z3-Z4)))
C END THE LOOP IF BACK AT THE BEGINNING
               IF (KX2.EQ.ISTX.AND.KY2.EQ.ISTY) THEN 
	          endloop = 980
	          go to 11
	       ENDIF 
	       qline_flag1=kx2
	       qline_flag2=ky2
C STORE COORDINATES
               Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
               IY = IY+1
	       index_inc=decy
	       boundary = 0
	       go to 11
            ELSE
	       boundary = top_b
	       go to 20
            ENDIF
	ENDIF

C LEFT BOUNDARY
	IF (boundary.EQ.left_b) THEN
C UPPER LEFT CORNER
	   IF (IY.GE.JJ-1) THEN
              KY2 = IY1 + NINT(YINT*IY)
C END THE LOOP IF BACK AT THE BEGINNING
              IF (KX2.EQ.ISTX.AND.KY2.EQ.ISTY) THEN 
	         endloop = 980
	         go to 11
	      ENDIF 
	      qline_flag1=kx2
	      qline_flag2=ky2
C STORE COORDINATES
              Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
              IX = IX-1
C LOOK AT TOP BOUNDARY
	      boundary = top_b
	      index_inc = 0
	      go to 10
           ENDIF
	   IY = IY + 1
           Call BorderGetz(ix,iy,delta)
C FOLLOW LEFT BOUNDARY UNTIL CONTOUR IS FOUND
           IF (z1.GT.zct.AND.z4.LT.zct) THEN
	      KY2 = IY1 + NINT(YINT*((IY-1) + (ZCT-Z1)/(Z4-Z1)))
C END THE LOOP IF BACK AT THE BEGINNING
              IF (KX2.EQ.ISTX.AND.KY2.EQ.ISTY) THEN 
	         endloop = 980
	         go to 11
	      ENDIF 
	      qline_flag1=kx2
	      qline_flag2=ky2
C STORE COORDINATES
              Call CalcLine (qline_flag1,qline_flag2,moveto,line_count)
              IX = IX-1
	      index_inc=incx
	      boundary = 0
	      go to 11
           ELSE
	      boundary = left_b 
	      go to 20
           ENDIF
	ENDIF

11	Continue

	moveto = 'N'

	Return
	End
**************************************************
	Subroutine FindCont (ix,iy,moveto)

	Implicit none

	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Character*1 moveto
	Integer ix,iy
	Real qmove_flag1,qmove_flag2

	qmove_flag1=kx2
	qmove_flag2=ky2

C FIND THE CONTOUR AND MOVE TO IT

10 	Continue

C LOOK AT THE BOTTOM SIDE OF SQUARE
	IF (side.EQ.bottom_s) THEN
	   IF (Z1.GT.Z2) THEN  !LOOK AT THE RIGHT SIDE
              side = right_s
	      go to 10
	   ENDIF
C CALCULATE POSITION OF THE CONTOUR
           ISTX = IX1 + NINT(XINT*((IX-1) + (ZCT-Z1)/(Z2-Z1)))
           ISTY = IY1 + NINT(YINT*(IY-1))
           IY = IY - 1
	   index_inc = incy
	ENDIF

C LOOK AT THE RIGHT SIDE OF SQUARE
	IF (side.EQ.right_s) THEN
	   IF ((Z2.LT.ZCT.AND.Z3.GT.ZCT).OR.(Z2.GT.ZCT.AND.Z3.LT.ZCT)) THEN
              IF (z2.GT.z3) THEN  !LOOK AT THE TOP SIDE
                 side = top_s 
	         go to 10
	      ENDIF
C CALCULATE POSITION OF THE CONTOUR
              ISTX = IX1 + NINT(XINT*IX)
              ISTY = IY1 + NINT(YINT*((IY-1) + (ZCT-Z2)/(Z3-Z2)))
	      IX = IX + 1
	      index_inc = decx
	   ELSE
	      side = top_s
	   ENDIF
	ENDIF

C LOOK AT THE TOP SIDE OF SQUARE
	IF (side.EQ.top_s) THEN
	   IF ((Z3.LT.ZCT.AND.Z4.GT.ZCT).OR.(Z3.GT.ZCT.AND.Z4.LT.ZCT)) THEN
              IF (z3.GT.z4) THEN  !LOOK AT THE LEFT SIDE
                 side = left_s
	         go to 10
	      ENDIF
C CALCULATE POSITION OF THE CONTOUR
              ISTX = IX1 + NINT(XINT*((IX-1) + (ZCT-Z4)/(Z3-Z4)))
              ISTY = IY1 + NINT(YINT*IY)
              IY = IY + 1
	      index_inc = decy
	   ELSE
	     side = left_s
	   ENDIF
	ENDIF

C LOOK AT THE LEFT SIDE OF SQUARE
	IF (side.EQ.left_s) THEN
	   IF ((Z1.LT.ZCT.AND.Z4.GT.ZCT).OR.(Z1.GT.ZCT.AND.Z4.LT.ZCT)) THEN
              IF (z4.GT.z1) THEN !CAN'T FIND ANYTHING!
                 side = error
	         go to 10
	      ENDIF
              ISTX = IX1 + NINT(XINT*(IX-1))
              ISTY = IY1 + NINT(YINT*((IY-1) + (ZCT-Z1)/(Z4-Z1)))
	      IX = IX - 1
	      index_inc = incx
	   ELSE
	      side = error
	   ENDIF
	ENDIF

	IF (side.EQ.error) THEN
	   WRITE(*,880)
880        FORMAT(2X,'PROGRAM GOT LOST LOOKING AT SQUARES') 
	ELSE
           KX2 = ISTX  !FIRST HORIZONTAL COORDINATE OF LINE 
           KY2 = ISTY  !FIRST VERTICAL COORDINATE OF LINE
	ENDIF

	qmove_flag1=kx2
	qmove_flag2=ky2

	Call Qmove (qmove_flag1,qmove_flag2)

	Return
	End
**************************************************
	Subroutine EnterSide (ix,iy)

	Implicit none

	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer ix,iy

C INCREMENT THE INDEXES OF THE ARRAY

	IF (index_inc.EQ.incy) THEN
	   IY = IY + 1		
	   quad_clock = q4_clock
	ENDIF

	IF (index_inc.EQ.decx) THEN
	   IX = IX - 1		
	   quad_clock = q1_clock
	ENDIF

	IF (index_inc.EQ.decy) THEN
	   IY = IY - 1		
	   quad_clock = q2_clock
	ENDIF

	IF (index_inc.EQ.incx) THEN
	   IX = IX + 1		
	   quad_clock = q3_clock
	ENDIF

	IM(IX,IY) = 16

C BEGINNING HORIZONTAL COORDINATE=END OF PREVIOUS LINE SEGMENT
	KX1 = KX2 
C BEGINNING VERTICAL COORDINATE=END OF PREVIOUS LINE SEGMENT
	KY1 = KY2


	Return 
	End
**************************************************
	Subroutine Getz (ix,iy,delta)

	Implicit none

	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer num_pt,ix,iy
	Real delta

	z5=0
	num_pt=0

*	Print*, 'Calling Getz'

C GET THE DATA POINTS..DON'T INCLUDE THE BAD DATA
C IN THE CALCULATION FOR Z5 (CENTER OF THE BOX)
*        Z1 = fld_data(IX,IY,cont)
*	IF (Z1.GT.-900) THEN
*	   Z5=Z5+Z1
*	   num_pt=num_pt+1
*	ENDIF
*        Z2 = fld_data(IX+1,IY,cont)
*	IF (Z2.GT.-900) THEN
*	   Z5=Z5+Z2
*	   num_pt=num_pt+1
*	ENDIF
*        Z3 = fld_data(IX+1,IY+1,cont)
*	IF (Z3.GT.-900) THEN
*	   Z5=Z5+Z3
*	   num_pt=num_pt+1
*	ENDIF
*        Z4 = fld_data(IX,IY+1,cont)
*	IF (Z4.GT.-900) THEN
*	   Z5=Z5+Z4
*	   num_pt=num_pt+1
*	ENDIF
*	IF (num_pt.GT.0) THEN
*	   Z5=Z5/num_pt
*	ELSE
*	   Z5=-999.00
*	ENDIF
*
*	IF (zct.LT.0) THEN
*	 delta = delta*(-1)
*	ENDIF

        Z1 = fld_data(IX,IY,cont)
        Z2 = fld_data(IX+1,IY,cont)
        Z3 = fld_data(IX+1,IY+1,cont)
        Z4 = fld_data(IX,IY+1,cont)
        Z5 = (Z1+Z2+Z3+Z4)/4.0

C ADD SMALL AMOUNT SO THAT THE CONTOURS DON'T MATCH *EXACTLY*
        IF (Z1.EQ.ZCT) Z1 = Z1 + DELTA
        IF (Z2.EQ.ZCT) Z2 = Z2 + DELTA
        IF (Z3.EQ.ZCT) Z3 = Z3 + DELTA
        IF (Z4.EQ.ZCT) Z4 = Z4 + DELTA
        IF (Z5.EQ.ZCT) Z5 = Z5 + DELTA

	Return
	End
**************************************************
	Subroutine BorderGetz (ix,iy,delta)

	Implicit none

	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer ix,iy
	Real delta

C GET THE DATA POINTS ON THE BOUNDARY

	IF (boundary.EQ.bottom_b) THEN
          Z1 = fld_data(IX,1,cont)
          Z2 = fld_data(IX+1,1,cont)
          Z3 = fld_data(IX+1,2,cont)
          Z4 = fld_data(IX,2,cont)
	ELSEIF (boundary.EQ.right_b) THEN
          Z1 = fld_data(II-1,IY,cont)
          Z2 = fld_data(II,IY,cont)
          Z3 = fld_data(II,IY+1,cont)
          Z4 = fld_data(II-1,IY+1,cont)
	ELSEIF (boundary.EQ.top_b) THEN
          Z1 = fld_data(IX,JJ-1,cont)
          Z2 = fld_data(IX+1,JJ-1,cont)
          Z3 = fld_data(IX+1,JJ,cont)
          Z4 = fld_data(IX,JJ,cont)
	ELSEIF (boundary.EQ.left_b) THEN
          Z1 = fld_data(1,IY,cont)
          Z2 = fld_data(2,IY,cont)
          Z3 = fld_data(2,IY+1,cont)
          Z4 = fld_data(1,IY+1,cont)
	ENDIF

	IF (zct.LT.0) THEN
	 delta = delta*(-1)
	ENDIF

        IF (Z1.EQ.ZCT) Z1 = Z1 + DELTA
        IF (Z2.EQ.ZCT) Z2 = Z2 + DELTA
        IF (Z3.EQ.ZCT) Z3 = Z3 + DELTA
        IF (Z4.EQ.ZCT) Z4 = Z4 + DELTA

	return
	end
**************************************************
	Subroutine CalcLine (qline_flag1,qline_flag2,moveto,line_count)

	Implicit none

	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Character*1 moveto
	Integer line_count
	Real x2,y2,qline_flag1,qline_flag2
	
        x2 = qline_flag1/1000.0
        y2 = qline_flag2/1000.0


C KEEP TRACK OF NUMBER OF LINES & INSERT 'CLOSEPATH' SO THAT THE
C PRINTER WON'T RUN OUT OF MEMORY

	line_count = line_count + 1

C IF NOT FILLING THE POLYGON, THEN YOU MUST USE A MOVETO WHEN
C BADDATA OR A BOUNDARY IS ENCOUNTERED
	IF (fill_flag.EQ.'N'.OR.fill_flag.EQ.'n') THEN
	 xline(line_count)=X2
	 yline(line_count)=Y2
C STORE A MOVETO IF BADDATA OR A BORDER
	 IF (moveto.EQ.'Y') THEN
	  pscommand(line_count)='m'
	 ELSE
	  pscommand(line_count)='l'
	 ENDIF
	ENDIF

	IF (fill_flag.EQ.'Y'.OR.fill_flag.EQ.'y') THEN
	   xline(line_count)=X2
	   yline(line_count)=Y2
C DRAW AN ARC IF ON THE BOUNDARY
	   IF (moveto.EQ.'A') THEN
	      pscommand(line_count)='a'
	   ELSE
	      pscommand(line_count)='l'
	   ENDIF
	ENDIF

        RETURN
        END
**************************************************
	Subroutine Qmove (qmove_flag1,qmove_flag2)

	Implicit none

	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

      	Real ix1,iy1,x1,y1,qmove_flag1,qmove_flag2

C STORE THE BEGINNING OF THE LINE SEGMENT 

      	x1 = qmove_flag1/1000.0
      	y1 = qmove_flag2/1000.0

	xline(1)=x1
	yline(1)=y1
	pscommand(1)='m'
	
      	RETURN
      	END
**************************************************
	Subroutine PrintLines(line_count)

	Implicit none

	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer line_count,temp,i,num
	Real maxmem
	character*3 end_flag

C FINALLY, PRINT OUT THE LINES
	IF (scan_mode.EQ.'POLA') THEN
C ONLY CAN USE A LEVEL 2 PRINTER!
C	   maxmem=10000.
	   maxmem=20000.
	ELSE 
	   maxmem = 1450.
	ENDIF
	end_flag='no'
	num=0

C IF TOO MANY LINES, THEN CACULATE HOW MANY LINES TO DELETE 
10	IF (end_flag.EQ.'no ') THEN
	 num=num+1
	 IF (line_count.LT.(maxmem*num)) end_flag='yes'
	 go to 10
	ENDIF 

	IF (num.EQ.1.OR.num.GT.2) THEN
	 IF (scan_mode.EQ.'POLA') THEN
	    DO i=1,line_count,num
	       IF (pscommand(i).EQ.'a') THEN
		  Write (600,19) xline(i),yline(i-1),yline(i)
	       ELSE
                  Write (600,20) xline(i),yline(i),pscommand(i)
	       ENDIF
            ENDDO
	 ELSE
	    DO i=1,line_count,num
	       Write (600,21) xline(i),hkm,yline(i),vkm,pscommand(i)
	    ENDDO
         ENDIF
	ELSEIF (num.EQ.2) THEN
	 temp=nint(maxmem/(line_count-maxmem))
	 IF (temp.EQ.1) temp=temp+1
	 DO i=1,line_count
	  IF (mod(i,temp).NE.0) THEN
	     IF (scan_mode.EQ.'POLA') THEN
	       IF (pscommand(i).EQ.'a') THEN
		  Write (600,19) xline(i),yline(i-1),yline(i)
	       ELSE
                  Write (600,20) xline(i),yline(i),pscommand(i)
	       ENDIF
             ELSE
	       Write (600,21) xline(i),hkm,yline(i),vkm,pscommand(i)
             ENDIF
	  ELSEIF (mod(i,temp).EQ.0) THEN
*	   Print*, 'MAXIMUM MEMORY EXCEEDED: SKIPPING LINE'
	  ENDIF
	 ENDDO
	ENDIF
	
C if previous command moveto, the end with moveto else lineto
	IF (scan_mode.NE.'POLA') THEN
	   IF (pscommand(line_count-1).EQ."m") THEN
 	      Write (600,814) xline(1),hkm,yline(1),vkm,pscommand(1)
	   ELSE
	      Write (600,815) xline(1),hkm,yline(1),vkm
	   ENDIF
	ENDIF

19      Format (3X,F10.2,1X,F10.2,1X,F10.2,1X,'DrawArc')
20      Format (3X,F10.2,1X,F10.2,1X,'Polar',1X,A)
21	Format (3X,F10.2,1X,A,1X,F10.2,1X,A,1X,A)
814	Format (3X,F10.2,1X,A,1X,F10.2,1X,A,1X,A) 
815	Format (3X,F10.2,1X,A,1X,F10.2,1X,A,' l') 

	Return
	End
**************************************************
	Subroutine LabelContour (count,line_count)
	
	Implicit none

        Include 'common1.h'
        Include 'common2.h'
        Include 'common3.h'

        Integer line_count,i,label_count,label_every,count
	Real xtext_pos(100),ytext_pos(100),theta(100)

*	Print*, 'Calling LabelContour'

	label_count=0

	IF (line_count.LE.10) go to 10
*	IF (line_count.GT.0.AND.line_count.LE.10) label_every=5
	IF (line_count.GT.10.AND.line_count.LE.100) label_every=10
	IF (line_count.GT.100) label_every=ifix(line_count/25.)
	
C Initialize
        DO i=1,100
	 xtext_pos(i)=0. 
	 ytext_pos(i)=0. 
	 theta(i)=0.
        ENDDO

        DO i = 1,line_count
	 IF (mod(i,label_every).EQ.0.AND.pscommand(i).EQ.'l'.
     $       AND.pscommand(i-1).EQ.'l') THEN
	  label_count=label_count+1
	  Call CalcPosLabel (i,xtext_pos,ytext_pos,theta,label_count)
	 ENDIF
        ENDDO

*	IF (mod(count,2).EQ.0) THEN
	Call WriteLabel (xtext_pos,ytext_pos,theta,label_count,count)
*	ENDIF

10	Continue

	Return
	End
**************************************************
	Subroutine Initialize (line_count)
	
	Implicit none

        Include 'common1.h'
        Include 'common2.h'
        Include 'common3.h'

	Integer i,line_count


	DO i=1,line_count
	 xline(i)=0.
	 yline(i)=0.
	 pscommand(i)=''
	ENDDO

	line_count=1

	Return
	End
**************************************************
	Subroutine CalcPosLabel (i,xtext_pos,ytext_pos,theta,label_count)

	Implicit none

        Include 'common1.h'
        Include 'common2.h'
        Include 'common3.h'

	integer i,label_count
	Real xtext_pos(100),ytext_pos(100),theta(100),xlength,ylength,r

*	Print*, 'Calling CalcPosLabel'

C Calculate length in horizontal & vertical directions
	IF (pscommand(i-3).EQ."l".AND.pscommand(i+3).EQ."l") THEN
	 xlength=xline(i+3)-xline(i-3)
	 ylength=yline(i+3)-yline(i-3)
	ELSEIF (pscommand(i-2).EQ."l".AND.pscommand(i+2).EQ."l") THEN
	 xlength=xline(i+2)-xline(i-2)
	 ylength=yline(i+2)-yline(i-2)
	ELSEIF (pscommand(i).EQ."l".AND.pscommand(i-1).EQ."l") THEN
	 xlength=xline(i)-xline(i-1)
	 ylength=yline(i)-yline(i-1)
	ENDIF
C Calculate radius
	r=sqrt(xlength**2+ylength**2)
C Calculate theta
	IF (r.GT.0) THEN
	 theta(label_count)=asin(abs(ylength/r))*57.3 
	 IF ((xlength.LT.0.AND.ylength.LT.0).OR.
     $   (xlength.GT.0.AND.ylength.LT.0)) THEN
	  theta(label_count)=theta(label_count)*(-1)
	 ENDIF
C Calculate text position
	 xtext_pos(label_count)=xline(i)
	 ytext_pos(label_count)=yline(i)
	ELSE
	 theta(label_count)=-999
	 xtext_pos(label_count)=-999
	 ytext_pos(label_count)=-999
	ENDIF

	Return
	End
**************************************************
	Subroutine WriteLabel (xtext_pos,ytext_pos,theta,label_count,count)
	
	Implicit none

        Include 'common1.h'
        Include 'common2.h'
        Include 'common3.h'

*	Integer label_count,label,i,frmat,zct_int,count
	Integer label_count,label,i,frmat,count
	Real xtext_pos(100),ytext_pos(100),theta(100)
	Real zct_int
	Character comment

*	Print*, 'Calling WriteLabel'

	label=nint(label_count*.50)
*	zct_int=nint(zct-cont_offset)
	zct_int=(zct-cont_offset)/real(scalefld(cont))

C Calculate which format to use
	IF (zct_int.GE.0.AND.zct_int.LT.10) frmat=10
	IF ((zct_int.GE.10.AND.zct_int.LT.100).OR.
     $      (zct_int.LT.0.AND.zct_int.GT.-10)) frmat=20
	IF ((zct_int.GT.-100.AND.zct_int.LE.-10).OR.(zct_int.GE.100)) frmat=30
	IF (zct_int.LE.-100) frmat=40
C Write out every label, but comment out all but the middle one...so user 
C can choose which label to print

	DO i=1,label_count
	 IF (i.EQ.label) THEN
	  IF (theta(i).NE.-999.AND.xtext_pos(i).NE.-999.AND.
     $    ytext_pos(i).NE.-999) THEN
	   comment=''
	   IF (mod(count,2).NE.0) comment='%' 
	  ENDIF
	 ELSE
	  IF (theta(i).NE.-999.AND.xtext_pos(i).NE.-999.AND.
     $    ytext_pos(i).NE.-999) THEN
	   comment='%'
	  ENDIF
	 ENDIF
	 IF (xtext_pos(i).NE.-999.AND.ytext_pos(i).NE.-999) THEN

	 IF (frmat.EQ.10) THEN
	    IF (scan_mode.EQ.'POLA') THEN
	       Write (600,10) comment,xtext_pos(i),ytext_pos(i),
     $         theta(i),zct_int
	    ELSE
	       Write (600,11) comment,xtext_pos(i),hkm,ytext_pos(i),vkm,
     $         theta(i),zct_int
	    ENDIF
	 ELSEIF (frmat.EQ.20) THEN
	    IF (scan_mode.EQ.'POLA') THEN
	       Write (600,20) comment,xtext_pos(i),ytext_pos(i),
     $         theta(i),zct_int
	    ELSE
	       Write (600,21) comment,xtext_pos(i),hkm,ytext_pos(i),vkm,
     $         theta(i),zct_int
	    ENDIF
	 ELSEIF (frmat.EQ.30) THEN
	    IF (scan_mode.EQ.'POLA') THEN
	       Write (600,30) comment,xtext_pos(i),ytext_pos(i),
     $         theta(i),zct_int
	    ELSE
	       Write (600,31) comment,xtext_pos(i),hkm,ytext_pos(i),vkm,
     $         theta(i),zct_int
	    ENDIF
	 ELSEIF (frmat.EQ.40) THEN
	    IF (scan_mode.EQ.'POLA') THEN
	       Write (600,40) comment,xtext_pos(i),ytext_pos(i),
     $         theta(i),zct_int
	    ELSE
	       Write (600,41) comment,xtext_pos(i),hkm,ytext_pos(i),vkm,
     $         theta(i),zct_int
	    ENDIF
	 ENDIF

	 ENDIF
	ENDDO

C10	   Format (A,F10.2,1X,F10.2,' Polar',1X,F6.2,'(',I1,') 4 4 DrawText')
C11	   Format (A,F10.2,1X,A,1X,F10.2,1X,A,1X,F6.2,'(',I1,') 4 4 DrawText')
C20	   Format (A,F10.2,1X,F10.2,' Polar',1X,F6.2,'(',I2,') 4 4 DrawText')
C21	   Format (A,F10.2,1X,A,1X,F10.2,1X,A,1X,F6.2,'(',I2,') 4 4 DrawText')
C30	   Format (A,F10.2,1X,F10.2,' Polar',1X,F6.2,'(',I3,') 4 4 DrawText')
C31	   Format (A,F10.2,1X,A,1X,F10.2,1X,A,1X,F6.2,'(',I3,') 4 4 DrawText')
C40	   Format (A,F10.2,1X,F10.2,' Polar',1X,F6.2,'(',I4,') 4 4 DrawText')
C41	   Format (A,F10.2,1X,A,1X,F10.2,1X,A,1X,F6.2,'(',I4,') 4 4 DrawText')

 10	Format (3X,A,F10.2,1X,F10.2,' Polar',1X,F6.2,'(',F3.1,
     &  ') 4 4 DrawText')
 11	Format (3X,A,F10.2,1X,A,1X,F10.2,1X,A,1X,F6.2,'(',F3.1,
     &  ') 4 4 DrawText')
 20	Format (3X,A,F10.2,1X,F10.2,' Polar',1X,F6.2,'(',F4.1,
     &  ') 4 4 DrawText')
 21	Format (3X,A,F10.2,1X,A,1X,F10.2,1X,A,1X,F6.2,'(',F4.1,
     &  ') 4 4 DrawText')
 30	Format (3X,A,F10.2,1X,F10.2,' Polar',1X,F6.2,'(',F5.1,
     &  ') 4 4 DrawText')
 31	Format (3X,A,F10.2,1X,A,1X,F10.2,1X,A,1X,F6.2,'(',F5.1,
     &  ') 4 4 DrawText')
 40	Format (3X,A,F10.2,1X,F10.2,' Polar',1X,F6.2,'(',F6.1,
     &  ') 4 4 DrawText')
 41	Format (3X,A,F10.2,1X,A,1X,F10.2,1X,A,1X,F6.2,'(',F6.1,
     &  ') 4 4 DrawText')

	Return
	End
**************************************************
	Subroutine Close (count,line_count)

	Implicit none

	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer count,line_count

*	Print*, 'Calling Close'

500	IF (fill_flag.EQ.'Y') THEN
	  Write (600,*) '      closepath'
C      	 write(600,814) count
C814 	 format(1x,'closepath ',I2,' o')
	ELSE
	 Write (600,*) 'stroke'
	ENDIF

      	return
      	end
**************************************************
