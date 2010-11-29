C This common file contains the real variables 
	Real horiz_beg,horiz_end, horiz_inc,horiz_num,save_hbeg,save_vbeg
	Real vert_beg,vert_end,vert_inc,vert_num 
	Real fix_beg,fix_end,fix_inc,fix_num
	Real zero_linewidth,linewidth
	Real vec_speed,vec_length,vec_scale,arrow_h,arrow_hh,arrow_ht
	Real stretch_horiz,stretch_vert,sp_text,sp_scale
	Real sp_title
C	Real fld_data (256,256,4)
	Real fld_data (256,500,4)
	Real fill_beg,fill_end,fill_inc,fill_num
	Real nofill_beg,nofill_end,nofill_inc,nofill_num
	Real dist_beg,dist_end,dist_inc
	Real plots(10)
	Real shift_horiz,shift_vert,page_w,page_h
	Real label_ht,title_ht,num_ht,num_w,scale_w,scale_h
	Real l_marg,r_marg,t_marg,b_marg,box_w,box_h
	Real max_size,max_w,max_h
	Real horiz_dist,vert_dist,dist_ratio
	Real xkm,ykm,zkm
	Real xpos1,ypos1,xpos2,ypos2,xpos3,ypos3,xpos4,ypos5
	Real xpos6,ypos6,xpos7,ypos7,xpos8,ypos8,sp_row,sp_col
	Real tick_hbeg1,tick_vbeg1
	Real tick_hbeg2,tick_vbeg2
        Real z1,z2,z3,z4,z5,zct,ix1,ix2,iy1,iy2,kx1,kx2,ky1,ky2
C        Real ix1,ix2,iy1,iy2,kx1,kx2,ky1,ky2
C	Real xint,yint,istx,isty,xline(5000),yline(5000),zmx,zmn
C	Real xint,yint,istx,isty,xline(20000),yline(20000),zmx,zmn
	Real xint,yint,istx,isty,xline(40000),yline(40000),zmx,zmn
	Real sp_right_label,sp_left_label,sp_top_label,sp_bottom_label
	Real sf,m2km,af
	Real xc, yc

C Common block Coord contains the coordinate information
C Common block Data is the array which contains the field data
C Common block Line contains the PostScript line width parameters 
C Common block Vectors contains the parameters for the PostScript arrow
C  routine
C Common block Stretch contains the horizontal & vertical stretch factors 
C Common block Text contains the amount of sp in cm between the grid & text 
C Common block Fill contains the values of contour levels 
C Common block Dist contains the beginning,ending & incremental distance
C Common block Plt contains each level or distance to plot 
C Common block Shift contains the amount to shift the origin
C Common block Box contains the w & h of the box and the variables
C  concerned with the text h 
C Common block Page contains the w & h of the page & margins
C Common block KM contains the value of km in each direction
C Common block Pos contains the x & y positions of the panels
C Common block Tick contains the beginning horizontal & vertical tick marks
C Common block ContR contains the real variables for contour routine


	Common/Coord/horiz_beg,horiz_end,horiz_inc,horiz_num,vert_beg,
     +   vert_end,vert_inc,vert_num,fix_beg,fix_end,fix_inc,fix_num,
     +   save_hbeg,save_vbeg 
	Common/Line/zero_linewidth,linewidth
	Common/Vectors/vec_speed,vec_length,vec_scale,
     +   arrow_h,arrow_hh,arrow_ht
	Common/Data/fld_data
	Common/Stretch/stretch_horiz,stretch_vert
	Common/Text/sp_text,sp_scale,sp_title
	Common/Fill/fill_beg,fill_end,fill_inc,nofill_beg, 
     +   nofill_end,nofill_inc,fill_num,nofill_num
	Common/Dist/dist_beg,dist_end,dist_inc
	Common/Plt/plots
	Common/Shift/shift_horiz,shift_vert
	Common/Box/label_ht,title_ht,num_ht,num_w,scale_w,scale_h,
     +   horiz_dist,vert_dist,dist_ratio
	Common/Page/l_marg,r_marg,t_marg,b_marg,page_w,page_h,
     +   box_w,box_h,max_size,max_w,max_h,
     +   sp_right_label,sp_left_label,sp_top_label,sp_bottom_label
	Common/KM/xkm,ykm,zkm
	Common/Pos/xpos1,ypos1,xpos2,ypos2,xpos3,ypos3,xpos4,ypos5,
     +  xpos6,ypos6,xpos7,ypos7,xpos8,ypos8,sp_row,sp_col
	Common/Tick/tick_hbeg1,tick_vbeg1,tick_hbeg2,tick_vbeg2
        Common/ContR/z1,z2,z3,z4,z5,zct,ix1,ix2,iy1,iy2,
     +  kx1,kx2,ky1,ky2,xint,yint,istx,isty,xline,
     +  yline,zmx,zmn
	Common/Scale/sf,m2km,af
	Common/WCL/xc, yc
