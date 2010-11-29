	Subroutine AssignVar(id,nid,panel)

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer nid,id(nid),panel

	Call AssignId(id,nid)
	IF (panel.EQ.1.OR.save_config.EQ.0) THEN
	   Call AssignDefault
	ENDIF

	Return
	End
**************************************************
	Subroutine AssignId(id,nid)
	
	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'
	
	Integer nid,id(nid)

C Assign scale factors
	Call AssignScale(id,nid)
C Assign data from array id
C Scan mode (ie: COP,AIR...etc)
	Call GetScanMode(id,nid)
C Coordinate origin data 
	Call GetOrig(id,nid)
C Time & date info
	Call GetTime(id,nid)
C Coordinate info 
	Call GetCoord(id,nid)
C Calculate variables used in contour routine
	Call CalcContour(id,nid)

	Return
	End
**************************************************
	Subroutine AssignDefault
	
	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer ierr
	
C The default values for horiz_beg,horiz_end,horiz_inc,horiz_num,vert_beg,
C vert_end,vert_inc,vert_num,fix_beg,fix_end,fix_inc,fix_num are based
C on the header info & are assigned in AssignId.f

	Open(unit = 100, file = 'grid2ps.default', status = 'old',
     &  Iostat = ierr)

	IF (ierr.EQ.0) THEN
C Read the default values in from a file
   	   Read(100,*)
   	   Read(100,*)
   	   Read(100,*), pri_grid1
   	   Read(100,*), sec_grid1
   	   Read(100,*), pri_grid2
   	   Read(100,*), sec_grid2
   	   Read(100,*), orient
   	   Read(100,*), scale_flag 
   	   Read(100,*), scale_pos 
   	   Read(100,*), scale_w 
   	   Read(100,*), scale_h 
   	   Read(100,*), zero_linewidth
   	   Read(100,*), linewidth
   	   Read(100,*), dash_on 
   	   Read(100,*), dash_off
   	   Read(100,*), dash_beg
   	   Read(100,*), ptskip 
   	   Read(100,*), vec_speed 
   	   Read(100,*), vec_length 
   	   Read(100,*), arrow_h 
   	   Read(100,*), arrow_hh 
   	   Read(100,*), arrow_ht 
   	   Read(100,*), font_title 
   	   Read(100,*), font_num 
   	   Read(100,*), font_label 
   	   Read(100,*), font_name 
   	   Read(100,*), stretch_horiz 
   	   Read(100,*), stretch_vert 
   	   Read(100,*), sp_text 
   	   Read(100,*), sp_scale 
   	   Read(100,*), sp_row 
   	   Read(100,*), sp_col 
   	   Read(100,*), sp_title
   	   Read(100,*), title_flag
   	   Read(100,*), grid_flag
   	   Read(100,*)
   	   Read(100,*)
   	   Read(100,*)
   	   Read(100,*), page_w
   	   Read(100,*), page_h
   	   Read(100,*), l_marg
   	   Read(100,*), r_marg
   	   Read(100,*), t_marg
   	   Read(100,*), b_marg
	   Close(100)
	ELSE
	   pri_grid1 = 10
	   sec_grid1 = 1
	   pri_grid2 = 10
	   sec_grid2 = 1
	   orient = 'P'
	   scale_flag = 'ON'
	   scale_pos = 'HOR'
	   scale_w = 10.0
	   scale_h = .50
	   zero_linewidth = 1.5
	   linewidth = .5
	   dash_on = 6
	   dash_off = 3
	   dash_beg = 0
	   ptskip = -1
	   vec_speed = 20.0
	   vec_length = 5.0
	   arrow_h = .2
	   arrow_hh = .18
	   arrow_ht = .05
	   font_title = 15
	   font_num = 8
	   font_label = 10
	   font_name = 'Helvetica'
	   stretch_horiz = 1.0
	   stretch_vert = 1.0
	   sp_text = .25
	   sp_scale = .5
	   sp_row = 1.0
	   sp_col = 1.0
	   sp_title = 1.0
	   title_flag = 'ON'
	   grid_flag = 'BOTH'
	   page_w = 21.59
	   page_h = 27.94
	   l_marg = 1.0
	   r_marg = 1.0
	   t_marg = 1.0
	   b_marg = 1.0
	ENDIF

   	Data ct_fname /'NONE'/
   	Data math_flag /'Y'/
   	Data box_w, box_h /-999., -999./
   	Data spacex, spacey/1, 1/
   	Data num_text_flds/16/
   	Data num_colors/42/
   	IF (plot_type.EQ.1) scale_flag = 'OFF'


	Return
	End
**************************************************
	Subroutine AssignScale(id,nid)

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer nid,id(nid)

C header info multiplied by scale factor 
	sf=id(68)
C header info maultiplied by angle scaling factor 
        af=id(69)
C m to km conversion factor (1/1000)
	m2km=.001

	Return	
	End
**************************************************
	Subroutine GetScanMode(id,nid)

	Implicit none

	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'
	Include 'structure.h'

	Integer nid,id(nid)


C	scan.id16=id(16)
C	scan.id17=id(17)
C       scan_mode=scan.mode

	IF (id(16).EQ.1095303168.AND.id(17).EQ.1377828864) scan_mode='AIR'
	IF (id(16).EQ.1129250816.AND.id(17).EQ.1347158016) scan_mode='COP'
	IF (id(16).EQ.1129447424.AND.id(17).EQ.1411383296) scan_mode='CRT'

	Return
	End
**************************************************
	Subroutine GetOrig(id,nid)

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer nid,id(nid)

	lat_deg=id(33) 
	lat_min=id(34)
	lat_sec=nint(id(35)/sf)
	long_deg=id(36)
	long_min=id(37)
	long_sec=nint(id(38)/sf)
	xhoriz_orig=id(41)
	yhoriz_orig=id(42)
	clockwise_deg=id(40)

	Return
	End
**************************************************
	Subroutine GetTime(id,nid)

	Implicit none
	Include 'common1.h'

	Integer nid,id(nid)

	year_beg=id(116)
	month_beg=id(117)
	day_beg=id(118)
	hour_beg=id(119)
	min_beg=id(120)
	sec_beg=id(121)
	year_end=id(122)
	month_end=id(123)
	day_end=id(124)
	hour_end=id(125)
	min_end=id(126)
	sec_end=id(127)

	Return
	End
**************************************************
	Subroutine GetCoord(id,nid)

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer nid,id(nid),i

*The choice of fixed axis determines the index of the following
	IF (fix_axis.EQ.'Z'.OR.fix_axis.EQ.'z') THEN
	 horiz_beg=id(160)/sf
	 horiz_end=id(161)/sf
	 horiz_inc=id(163)*m2km
	 horiz_num=id(162)
	 IF (scan_mode.EQ.'POLA') THEN
	    vert_beg=id(165)/af
	    vert_end=id(166)/af
	    vert_inc=id(168)/af
	 ELSE
	    vert_beg=id(165)/sf
	    vert_end=id(166)/sf
	    vert_inc=id(168)*m2km
	 ENDIF
	 vert_num=id(167)
	 fix_beg=id(170)*m2km
	 fix_end=id(171)*m2km
	 fix_inc=id(173)*m2km
	 fix_num=id(172)
	ELSEIF (fix_axis.EQ.'Y'.OR.fix_axis.EQ.'y') THEN
	 horiz_beg=id(160)/sf
	 horiz_end=id(161)/sf
	 horiz_inc=id(163)*m2km
	 horiz_num=id(162)
	 vert_beg=id(170)*m2km
	 vert_end=id(171)*m2km
	 vert_inc=id(173)*m2km
	 vert_num=id(172)
	 IF (scan_mode.EQ.'POLA') THEN
	    fix_beg=id(165)/af
	    fix_end=id(166)/af
	    fix_inc=id(168)/af
	 ELSE
	    fix_beg=id(165)/sf
	    fix_end=id(166)/sf
	    fix_inc=id(168)*m2km
	 ENDIF
	 fix_num=id(167)
	ELSEIF (fix_axis.EQ.'X'.OR.fix_axis.EQ.'x') THEN
	 IF (scan_mode.EQ.'POLA') THEN
	    horiz_beg=id(165)/af
	    horiz_end=id(166)/af
	    horiz_inc=id(168)/af
	 ELSE
	    horiz_beg=id(165)/sf
	    horiz_end=id(166)/sf
	    horiz_inc=id(168)*m2km
    	 ENDIF
	 horiz_num=id(167)
	 vert_beg=id(170)*m2km
	 vert_end=id(171)*m2km
	 vert_inc=id(173)*m2km
	 vert_num=id(172)
	 fix_beg=id(160)/sf
	 fix_end=id(161)/sf
	 fix_inc=id(163)*m2km
	 fix_num=id(162)
	ENDIF

	save_hbeg=horiz_beg
	save_vbeg=vert_beg

	Return
	End
**************************************************
	Subroutine CalcContour(id,nid)

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer nid,id(nid)
        Real km2m

        km2m=1000.0

C UNIT=METERS
        IF (fix_axis.EQ.'Z'.OR.fix_axis.EQ.'z') THEN
           ix1 = (id(160)/sf)*km2m
           ix2 = (id(161)/sf)*km2m
           IF (scan_mode.EQ.'POLA') THEN
              iy1 = (id(165)/af)*km2m
              iy2 = (id(166)/af)*km2m
              yint = (id(168)/af)*km2m
           ELSE
              iy1 = (id(165)/sf)*km2m
              iy2 = (id(166)/sf)*km2m
              yint = id(168)
           ENDIF
           xint = id(163)
           ii=id(162)
           jj=id(167)
        ELSEIF (fix_axis.EQ.'Y'.OR.fix_axis.EQ.'y') THEN
           ix1 = (id(160)/sf)*km2m
           ix2 = (id(161)/sf)*km2m
           iy1 = id(170)
           iy2 = id(171)
           xint = id(163)
           yint = id(173)
           ii=id(162)
           jj=id(172)
        ELSEIF (fix_axis.EQ.'X'.OR.fix_axis.EQ.'x') THEN
           IF (scan_mode.EQ.'POLA') THEN
              ix1 = (id(165)/af)*km2m
              ix2 = (id(166)/af)*km2m
              xint = id(168)/af
           ELSE
              ix1 = (id(165)/sf)*km2m
              ix2 = (id(166)/sf)*km2m
              xint = id(168)
           ENDIF
           iy1 = id(170)
           iy2 = id(171)
           yint = id(173)
           ii=id(167)
           jj=id(172)
        ENDIF
        iedge = 0


	Return
	End
**************************************************
