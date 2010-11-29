	Subroutine ReadVar( panel, flag )

	Implicit none
	Include 'common1.h'	
	Include 'common2.h'	
	Include 'common3.h'	

	Integer flag, panel

	IF (flag.EQ.1) THEN
	   Call ReadInp1 (panel)
	ELSEIF (flag.EQ.2) THEN
	   Call ReadInp2 (panel)
	ELSE
	   Write(200,*) 'Invalid flag for subroutine ReadVar' 
	   stop
	ENDIF

	Return
	End
**************************************************
	Subroutine ReadInp1 (panel)

	Implicit none

	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer panel, i, j 
	Character exit_flag
	Character*100 buf 

C This routine reads the input file for the program parameters
	exit_flag = 'N'
10	IF (exit_flag.EQ.'N') THEN
	   Read (500,20), buf
	   IF (buf(1:12).EQ.'#OUTPUT_NAME') THEN 
	      IF (panel.EQ.1) THEN
	         Read (500,*),output_name
	      ELSE
	         Call SkipLines(1)
	      ENDIF
	   ELSEIF (buf(1:10).EQ.'#DATA_FILE') THEN
	      Read (500,*),data_file
	   ELSEIF (buf(1:11).EQ.'#INPUT_FLAG') THEN
	      Read (500,*),input_flag
	      IF (input_flag.EQ.'c') THEN
	         input_flag='C'
	      ELSEIF (input_flag.EQ.'t') THEN
	         input_flag='T'
	      ENDIF
	   ELSEIF (buf(1:6).EQ.'#COLOR') THEN
	      Read (500,*),color_flag
	      Call Change2Caps(color_flag)
	   ELSEIF (buf(1:9).EQ.'#FIX_AXIS') THEN
	      Read (500,*),fix_axis
	      IF (fix_axis.EQ.'z') THEN
	         fix_axis='Z'
	      ELSEIF (fix_axis.EQ.'y') THEN
	         fix_axis='Y'
	      ELSEIF (fix_axis.EQ.'x') THEN
	         fix_axis='X'
	      ENDIF
	   ELSEIF (buf(1:13).EQ.'#CONTOUR_TYPE') THEN
	      Read (500,*),cont_type
	   ELSEIF (buf(1:7).EQ.'#FIELD1') THEN
	      Read (500,*),str(1)
	   ELSEIF (buf(1:7).EQ.'#FIELD2') THEN
	      Read (500,*),str(2)
	   ELSEIF (buf(1:7).EQ.'#FIELD3') THEN
	      Read (500,*),str(3)
	   ELSEIF (buf(1:7).EQ.'#FIELD4') THEN
	      Read (500,*),str(4)
	   ELSEIF (buf(1:10).EQ.'#PLOT_TYPE') THEN
	      Read (500,*),plot_type
	   ELSEIF (buf(1:10).EQ.'#FILL_FLAG') THEN
	      Read (500,*),fill_flag
	      Call Change2Caps(fill_flag)
	   ELSEIF (buf(1:9).EQ.'#FILL_BEG') THEN
	      Read (500,*),fill_beg
	   ELSEIF (buf(1:9).EQ.'#FILL_END') THEN
	      Read (500,*),fill_end
	   ELSEIF (buf(1:9).EQ.'#FILL_INC') THEN
	      Read (500,*),fill_inc
	   ELSEIF (buf(1:11).EQ.'#NOFILL_BEG') THEN
	      Read (500,*),nofill_beg
	   ELSEIF (buf(1:11).EQ.'#NOFILL_END') THEN
	      Read (500,*),nofill_end
	   ELSEIF (buf(1:11).EQ.'#NOFILL_INC') THEN
	      Read (500,*),nofill_inc
	   ELSEIF (buf(1:12).EQ.'#PANEL_TITLE') THEN
	      Read (500,*),panel_title
	   ELSEIF (buf(1:12).EQ.'#SCALE_TITLE') THEN
	      IF (panel.EQ.1) THEN
	         Read (500,*),scale_title
	      ELSE
	         Call SkipLines(1)
	      ENDIF
	   ELSEIF (buf(1:6).EQ.'#TITLE') THEN
	      IF (panel.EQ.1) THEN
	         Read (500,*),title
	      ELSE
	         Call SkipLines(1)
	      ENDIF
	   ELSEIF (buf(1:11).EQ.'#LEVEL_FLAG') THEN
	      Read (500,*),level_flag
	      IF (level_flag.EQ.'d') level_flag = 'D'
	      IF (level_flag.EQ.'l') level_flag = 'L'
	   ELSEIF (buf(1:6).EQ.'#PLOTS'.OR.
     &             buf(1:9).EQ.'#DISTANCE') THEN
	      IF (level_flag.EQ.'D') THEN
	         Read (500,*),plots(panel)
	      ELSEIF (level_flag.EQ.'L') THEN
	         Read (500,*),level
	      ENDIF
	   ELSEIF (buf(1:13).EQ.'#CATALOG_NAME') THEN
	      Read (500,*),catalog_name
	      IF (catalog_name(1:4).EQ.'none') catalog_name='NONE'
	   ELSEIF (buf(1:12).EQ.'#SAVE_CONFIG') THEN
	      IF (panel.EQ.1) THEN
	         Read (500,*)
	         Read (500,*), save_config
	      ELSE
		 Call SkipLines(3)
	      ENDIF
	   ELSEIF (buf(1:10).EQ.'#PLANE_INC') THEN
	      Read (500,*),plane_inc
	      exit_flag='Y'
   	   ENDIF
	   go to 10
	ENDIF

C	Skip the comment lines
	Call SkipLines(3)
C   	Store the names of the fields in the array fld_name
   	Do i=1,4
   	   Do j=1,4
	      fld_name(j,i) = str(i)(2*j-1:2*j)
   	   Enddo
   	Enddo

20	Format(A)

	Return
	End
**************************************************
	Subroutine ReadInp2 (panel)

	Implicit none
	Include 'common1.h'	
	Include 'common2.h'	
	Include 'common3.h'	
	  
	Integer panel
	Character exit_flag,default
	Character*100 buf 
	
	exit_flag = 'N'
10	IF (exit_flag.EQ.'N') THEN
	   Read (500,20), buf
	   default = 'N'
	   IF (buf(1:19).EQ.'#DEFAULT COORDINATE') THEN
C             COORDINATE PARAMETERS 
	      Read (500,*),default 
	      Call Change2Caps(default)
	      Call ReadCoord(default)
	   ELSEIF (buf(1:13).EQ.'#DEFAULT GRID') THEN
C             GRID PARAMETERS 
	      Read (500,*),default 
	      Call Change2Caps(default)
	      Call ReadGrid(default)
	   ELSEIF (buf(1:14).EQ.'#DEFAULT SCALE') THEN
C             SCALE PARAMETERS 
	      Read (500,*),default 
	      Call Change2Caps(default)
	      Call ReadScale(default, panel)
	   ELSEIF (buf(1:13).EQ.'#DEFAULT LINE') THEN
C             LINE PARAMETERS 
	      Read (500,*),default 
	      Call Change2Caps(default)
	      Call ReadLine(default, panel)
	   ELSEIF (buf(1:15).EQ.'#DEFAULT VECTOR') THEN
C             VECTOR PARAMETERS 
	      Read (500,*),default 
	      Call Change2Caps(default)
	      Call ReadVector(default)
	   ELSEIF (buf(1:13).EQ.'#DEFAULT FONT') THEN
C             FONT PARAMETERS 
	      Read (500,*),default 
	      Call Change2Caps(default)
	      Call ReadFont(default, panel)
	   ELSEIF (buf(1:13).EQ.'#DEFAULT PLOT') THEN
C             PLOT PARAMETERS 
	      Read (500,*),default 
	      Call Change2Caps(default)
	      Call ReadPlot(default, panel)
	   ELSEIF (buf(1:13).EQ.'#DEFAULT PAGE') THEN
C             PAGE PARAMETERS 
	      Read (500,*),default 
	      Call Change2Caps(default)
	      Call ReadPage(default, panel)
	      exit_flag = 'Y'
	   ENDIF
	   go to 10
	ENDIF

20	Format(A)

	Return
	End
**************************************************
	Subroutine ReadCoord (default)

	Implicit none
	Include 'common1.h'	
	Include 'common2.h'	
	Include 'common3.h'	

	Character*100 buf
	Character default

C GET THE COORDINATE PARAMETERS
	IF (default.EQ.'Y') THEN
	   Call SkipLines(8)
	ELSEIF (default.EQ.'N') THEN
	   Read (500,20), buf
           IF ( buf(1:10).EQ.'#HORIZ_BEG') THEN
	      Read (500,*),horiz_beg
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:10).EQ.'#HORIZ_END') THEN
	      Read (500,*),horiz_end
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:9).EQ.'#VERT_BEG') THEN
	      Read (500,*),vert_beg
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:9).EQ.'#VERT_END') THEN
	      Read (500,*),vert_end
	   ELSE
	      Call SkipLines(1)
           ENDIF
	ENDIF

20	Format(A)

	Return
	End
**************************************************
	Subroutine ReadGrid (default)

	Implicit none
	Include 'common1.h'	
	Include 'common2.h'	
	Include 'common3.h'	

	Character*100 buf
	Character default

C GET THE GRID PARAMETERS
	IF (default.EQ.'Y') THEN
	   Call SkipLines(8)
	ELSEIF (default.EQ.'N') THEN
	   Read (500,20), buf
           IF ( buf(1:10).EQ.'#PRI_GRID1') THEN
	      Read (500,*),pri_grid1
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:10).EQ.'#SEC_GRID1') THEN
	      Read (500,*),sec_grid1
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:10).EQ.'#PRI_GRID2') THEN
	      Read (500,*),pri_grid2
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:10).EQ.'#SEC_GRID2') THEN
	      Read (500,*),sec_grid2
	   ELSE
	      Call SkipLines(1)
           ENDIF
	ENDIF

20	Format(A)

	Return
	End
**************************************************
	Subroutine ReadFont(default, panel)

	Implicit none
	Include 'common1.h'	
	Include 'common2.h'	
	Include 'common3.h'	

	Character*100 buf
	Character default
	Integer panel

C GET THE FONT PARAMETERS
	IF (default.EQ.'Y') THEN
	   Call SkipLines(8)
	ELSEIF (default.EQ.'N'.AND.panel.EQ.1) THEN

	   Read (500,20), buf
           IF ( buf(1:11).EQ.'#FONT_TITLE') THEN
	      Read (500,*), font_title
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:9).EQ.'#FONT_NUM') THEN
	      Read (500,*), font_num
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:11).EQ.'#FONT_LABEL') THEN
	      Read (500,*), font_label
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:10).EQ.'#FONT_NAME') THEN
	      Read (500,*), font_name
	   ELSE
	      Call SkipLines(1)
           ENDIF

	ENDIF

20	Format(A)

	Return
	End
**************************************************
	Subroutine ReadVector(default)

	Implicit none
	Include 'common1.h'	
	Include 'common2.h'	
	Include 'common3.h'	

	Character*100 buf
	Character default

C GET THE VECTOR PARAMETERS
	IF (default.EQ.'Y') THEN
	   Call SkipLines(12)
	ELSEIF (default.EQ.'N') THEN

	   Read (500,20), buf
           IF ( buf(1:7).EQ.'#PTSKIP') THEN
	      Read (500,*), ptskip 
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:10).EQ.'#VEC_SPEED') THEN
	      Read (500,*), vec_speed 
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:11).EQ.'#VEC_LENGTH') THEN
	      Read (500,*), vec_length 
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:8).EQ.'#ARROW_H') THEN
	      Read (500,*), arrow_h 
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:9).EQ.'#ARROW_HH') THEN
	      Read (500,*), arrow_hh 
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:9).EQ.'#ARROW_HT') THEN
	      Read (500,*), arrow_ht 
	   ELSE
	      Call SkipLines(1)
           ENDIF

	ENDIF

20	Format(A)

	Return
	End
**************************************************
	Subroutine ReadLine(default, panel)

	Implicit none
	Include 'common1.h'	
	Include 'common2.h'	
	Include 'common3.h'	

	Character*100 buf
	Character default
	Integer panel

C GET THE LINE PARAMETERS
	IF (default.EQ.'Y') THEN
	   Call SkipLines(10)
	ELSEIF (default.EQ.'N'.AND.panel.EQ.1) THEN

	   Read (500,20), buf
           IF ( buf(1:15).EQ.'#ZERO_LINEWIDTH') THEN
	      Read (500,*), zero_linewidth 
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:10).EQ.'#LINEWIDTH') THEN
	      Read (500,*), linewidth 
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:8).EQ.'#DASH_ON') THEN
	      Read (500,*), dash_on
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:9).EQ.'#DASH_OFF') THEN
	      Read (500,*), dash_off
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:9).EQ.'#DASH_BEG') THEN
	      Read (500,*), dash_beg
	   ELSE
	      Call SkipLines(1)
           ENDIF
	ENDIF

20	Format(A)

	Return
	End
**************************************************
	Subroutine ReadScale(default, panel)

	Implicit none
	Include 'common1.h'	
	Include 'common2.h'	
	Include 'common3.h'	

	Character*100 buf
	Character default
	Integer panel

C GET THE SCALE PARAMETERS
	IF (default.EQ.'Y') THEN
	   Call SkipLines(8)
	ELSEIF (default.EQ.'N'.AND.panel.EQ.1) THEN

	   Read (500,20), buf
           IF ( buf(1:11).EQ.'#SCALE_FLAG') THEN
	      Read (500,*), scale_flag
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:10).EQ.'#SCALE_POS') THEN
	      Read (500,*), scale_pos
	      IF (scale_pos.EQ.'hor') scale_pos = 'HOR'
	      IF (scale_pos.EQ.'ver') scale_pos = 'VER'
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:8).EQ.'#SCALE_W') THEN
	      Read (500,*), scale_w
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:8).EQ.'#SCALE_H') THEN
	      Read (500,*), scale_h
	   ELSE
	      Call SkipLines(1)
           ENDIF
	ENDIF

20	Format(A)

	Return
	End
**************************************************
	Subroutine ReadPage(default, panel)

	Implicit none
	Include 'common1.h'	
	Include 'common2.h'	
	Include 'common3.h'	

	Character*100 buf
	Character default
	Integer panel

C GET THE PAGE PARAMETERS
	IF (default.EQ.'Y') THEN
	   Call SkipLines(12)
	ELSEIF (default.EQ.'N'.AND.panel.EQ.1) THEN
	   Read (500,20), buf
           IF ( buf(1:7).EQ.'#PAGE_W') THEN
	      Read (500,*), page_w
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:7).EQ.'#PAGE_H') THEN
	      Read (500,*), page_h
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:7).EQ.'#L_MARG') THEN
	      Read (500,*), l_marg
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:7).EQ.'#R_MARG') THEN
	      Read (500,*), r_marg
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:7).EQ.'#T_MARG') THEN
	      Read (500,*), t_marg
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:7).EQ.'#B_MARG') THEN
	      Read (500,*), b_marg
	   ELSE
	      Call SkipLines(1)
           ENDIF

	ENDIF

20	Format(A)

	Return
	End
**************************************************
	Subroutine ReadPlot(default, panel)

	Implicit none
	Include 'common1.h'	
	Include 'common2.h'	
	Include 'common3.h'	

	Character*100 buf
	Character default
	Integer panel

C GET THE PLOT PARAMETERS
	IF (default.EQ.'Y') THEN
	   Call SkipLines(24)
	ELSEIF (default.EQ.'N') THEN

	   Read (500,20), buf
           IF ( buf(1:14).EQ.'#STRETCH_HORIZ') THEN
	      Read (500,*), stretch_horiz 
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:13).EQ.'#STRETCH_VERT') THEN
	      Read (500,*), stretch_vert
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:8).EQ.'#SP_TEXT'.AND.panel.EQ.1) THEN
	      Read (500,*), sp_text 
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:9).EQ.'#SP_SCALE'.AND.panel.EQ.1) THEN
	      Read (500,*), sp_scale
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:7).EQ.'#SP_ROW'.AND.panel.EQ.1) THEN
	      Read (500,*), sp_row
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:7).EQ.'#SP_COL'.AND.panel.EQ.1) THEN
	      Read (500,*), sp_col
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:9).EQ.'#SP_TITLE'.AND.panel.EQ.1) THEN
	      Read (500,*), sp_title
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:11).EQ.'#TITLE_FLAG') THEN
	      Read (500,*), title_flag 
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:10).EQ.'#GRID_FLAG') THEN
	      Read (500,*), grid_flag 
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:8).EQ.'#PANEL_W') THEN
	      Read (500,*), box_w 
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:8).EQ.'#PANEL_H') THEN
	      Read (500,*), box_h 
	   ELSE
	      Call SkipLines(1)
           ENDIF

	   Read (500,20), buf
           IF ( buf(1:12).EQ.'#COLOR_TABLE') THEN
	      Read (500,*), ct_fname
	      IF (ct_fname.EQ.'none') ct_fname = 'NONE'
	   ELSE
	      Call SkipLines(1)
           ENDIF
	ENDIF

20	Format(A)

	Return
	End
**************************************************
	Subroutine SkipLines(num)

	Implicit none
	
	Integer i,num

	Do 10 i=1,num
	 Read (500,*)
10	Continue

	Return
	End	
**************************************************
	Subroutine Change2Caps(var)

	Character var

	IF (var.EQ.'y') THEN
           var='Y'
	ELSEIF (var.EQ.'n') THEN
           var='N'
	ELSEIF (var.NE.'Y'.AND.var.NE.'N') THEN
	   var = 'Y'
	ENDIF
	
	Return
	End
**************************************************
