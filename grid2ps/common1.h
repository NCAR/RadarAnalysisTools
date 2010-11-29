C This common file contains the integer variables
	
	Integer num_fld,lat_deg,lat_min,lat_sec,long_deg,long_min
	Integer long_sec,xhoriz_orig,yhoriz_orig,year_beg,month_beg
	Integer day_beg,hour_beg,min_beg,sec_beg,year_end,month_end
	Integer day_end,hour_end,min_end,sec_end
	Integer pri_grid1,pri_grid2,sec_grid1,sec_grid2
	Integer template,ptskip,num_plot
	Integer cont_offset,dash_on,dash_off,dash_beg,cont_type,midval
	Integer font_title,font_num,font_label,plot_type,level_type
	Integer lev_beg,lev_end,lev_inc,level
	Integer plane_inc
	Integer vec1,vec2,cont1,cont2,cont
        Integer ii,jj,itest,side,bottom_s,right_s,left_s,top_s,error
        Integer index_inc,incy,decx,decy,incx
        Integer quad_clock,q1_clock,q2_clock,q3_clock,q4_clock
        Integer quad_cclock,q1_cclock,q2_cclock,q3_cclock,q4_cclock
        Integer boundary,left_b,right_b,bottom_b,top_b
        Integer test6,endloop
        Integer clspath,iedge
C	Integer im(256,256)
	Integer im(512,512)
	Integer clockwise_deg
	Integer num_box,num_text_flds,num_colors
	Integer spacex,spacey
	Integer scalefld(4)
	Integer save_config

C Common block PsInt contains the integer variables in the PostScript file
C Common block Orig contains the latitude, longitude and origin information
C Common block Time contains the date and time information 
C Common block Grid contains the primary & secondary tick marks (in km) 
C Common block Temp contains the type of template to use
C Common block ContInt contains the integer variables used for the contours
C Common block Skip contains the number of point to skip to plot vectors and
C  the increment of planes to plot 
C Common block Font contains the font sizes for the text & numbers 
C Common block PltInfo contains information such as type of plot, how many 
C levels to process, etc.
C Common block Lev contains beginning, ending & increment level 
C Common block Field contains the index numbers for the vector & contour fields 
C Common block ContI contains the number of pts in horizontal & vertical
C Common block Space contains the horizontal & vertical white space needed
C when labeling contour lines

	Common/PsInt/num_fld
	Common/Orig/lat_deg,lat_min,lat_sec,long_deg,long_min,long_sec,
     +   xhoriz_orig,yhoriz_orig,clockwise_deg
	Common/Time/year_beg,month_beg,day_beg,hour_beg,min_beg,
     +   sec_beg,year_end,month_end,day_end,hour_end,min_end,sec_end
	Common/Grid/pri_grid1,pri_grid2,sec_grid1,sec_grid2
	Common/Temp/template
	Common/ContInt/cont_offset,dash_on,dash_off,dash_beg,
     +  cont_type,midval
	Common/Skip/ptskip,plane_inc
	Common/Font/font_title,font_num,font_label
	Common/PltInfo/plot_type,level_type,num_plot,num_box,num_colors
	Common/Lev/lev_beg,lev_end,lev_inc,level
	Common/Field/vec1,vec2,cont1,cont2,cont
        Common/ContI/ii,jj,itest,
     +  side,bottom_s,right_s,left_s,top_s,error,
     +  index_inc,incy,decx,decy,incx,
     +  quad_clock,q1_clock,q2_clock,q3_clock,q4_clock,
     +  quad_cclock,q1_cclock,q2_cclock,q3_cclock,q4_cclock,
     +  boundary,left_b,right_b,bottom_b,top_b,
     +  test6,endloop,clspath,iedge,im,scalefld
	Common/Space/spacex,spacey,num_text_flds
	Common/SaveIt/save_config

