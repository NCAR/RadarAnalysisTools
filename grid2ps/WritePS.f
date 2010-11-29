	Subroutine WritePS(panel)

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer panel, i1

	IF (panel.EQ.1) THEN
	   i1 = index(output_name, '  ')
	   Open (Unit=600,file=output_name(1:i1),status='unknown')
	   Write(600,10) '%!PS'
	   Call ShortenCommands
	   Call Constants
	   Call GlobalVar
	   Call GlobalSub
	   Call DeclareDict 
	   Call TextDict
	   Call PageDict
	   Call PanelDict
	   Call TickDict
	   Call ScaleDict
	   Call PolarDict
	   Call VectorDict
	   Call TrackDict
*	   IF (fix_axis.EQ.'Z'.AND.scan_mode(1:3).EQ.'AIR') THEN
*	      IF (catalog_name(1:4).NE.'NONE') THEN
*	         Call TrackDict
* 	      ENDIF
*	   ENDIF
	   Call MyFunctDict
	ENDIF

	Call PSMain(panel)

	IF (panel.EQ.num_plot) Close(600)

10	Format(A)

	Return
	End
**************************************************
	Subroutine ShortenCommands

	Write(600,10) '%********************'
	Write(600,10) '% SHORTEN COMMANDS'
	Write(600,10) '%********************'
	Write(600,10) '/l {lineto} def'
	Write(600,10) '/m {moveto} def'
	Write(600,10) '/gs {gsave} def'
	Write(600,10) '/gr {grestore} def'
	Write(600,10) '/cp {closepath} def'
	Write(600,10) '/rl {rlineto} def'
	Write(600,10) '/tr {translate} def'
	Write(600,10) '/ed {exch def} def'
	Write(600,10) '/s {stroke} def'
	Write(600,10) '/rl {rlineto} def'
	Write(600,10) '/inch {72 mul} def'
	Write(600,10) '/cm {72 2.54 div mul} def'
	Write(600,10)  '/numdict countdictstack def'

10	Format(A)

	Return
	End
**************************************************
	Subroutine Constants

	Write(600,10) '%********************'
	Write(600,10) '% CONSTANTS'
	Write(600,10) '%********************'
	Write(600,10) '0/portrait ed'
	Write(600,10) '1/landscape ed'
	Write(600,10) '0/HOR ed'
	Write(600,10) '1/VER ed'
	Write(600,10) '0/OFF ed'
	Write(600,10) '1/ON ed'
	Write(600,10) '0/CART ed'
	Write(600,10) '1/POLAR ed'
	Write(600,10) '0/PRIM ed'
	Write(600,10) '1/BOTH ed'
	Write(600,10)  'OFF/draw_scale ed'

10	Format(A)

	Return
	End
**************************************************
	Subroutine GlobalSub

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer i1

	i1 = index(font_name, ' ')

	Write(600,10)  '%********************'
	Write(600,10)  '% GLOBAL SUBROUTINES'
	Write(600,10)  '%********************'
	IF (ct_fname(1:4).EQ.'NONE') THEN
*	   IF (plot_type.NE.1.AND.color_flag.EQ.'Y') THEN
*	      Call DefineColorTable
*           ELSEIF (plot_type.NE.1.AND.color_flag.EQ.'N'.OR.
*     $             plot_type.EQ.1) THEN
*	      Call DefineGrayScale
*	   ENDIF
	   IF (color_flag.EQ.'Y') THEN
	      Call DefineColorTable
	   ELSE
	      Call DefineGrayScale
	   ENDIF
	ELSE
	   Call DefineUserTable
	ENDIF
	Write(600,10)  '/Largest {'
	Write(600,10)  ''
	Write(600,10)  '   % RETURN THE LARGEST OF VAL1 & VAL2'
	Write(600,10)  '   /val2 ed  /val1 ed'
	Write(600,10)  ''
	Write(600,10)  '   val1 val2 gt {val1} {val2} ifelse'
	Write(600,10)  ''
	Write(600,10)  '} def'
	Write(600,10)  '/Font {'
	Write(600,10)  '   '
	Write(600,10)  '   % SELECT FONT'
	Write(600,10)  '   findfont exch scalefont setfont'
	Write(600,10)  '   '
	Write(600,10)  '} def'
	Write(600,10)  '/DrawText {'
	Write(600,10)  ''
	Write(600,10)  '   %--------------------'
	Write(600,10)  '   % POSITION & DRAW TEXT'
	Write(600,10)  '   % XLOC=HORIZONTAL LOCATION OF TEXT'
	Write(600,10)  '   % YLOC=VERTICAL LOCATION OF TEXT'
	Write(600,10)  '   % ANGLE=ANGLE TO ROTATE TEXT'
	Write(600,10)  '   % STR=TEXT'
	Write(600,10)  '   % XFLAG=FLAG TO POSITION TEXT HORIZONTALLY'
	Write(600,10)  '   %   1=LEFT JUSTIFIED'
	Write(600,10)  '   %   2=CENTERED'
	Write(600,10)  '   %   3=RIGHT JUSTIFIED'
	Write(600,10)  '   %   4=CENTERED WITH BOX'
	Write(600,10)  '   % YFLAG=FLAG TO POSITION TEXT VERTICALLY'
	Write(600,10)  '   %   1=BOTTOM OF TEXT IS AT XLOC,YLOC'
	Write(600,10)  '   %   2=CENTERED VERTICALLY'
	Write(600,10)  '   %   3=TOP OF TEXT IS AT XLOC,YLOC'
	Write(600,10)  '   %   4=CENTERED WITH BOX'
	Write(600,10)  '   %--------------------'
	Write(600,10)  ''
	Write(600,10)  '   /yflag ed /xflag ed'
	Write(600,10)  '   /str ed /angle ed'
	Write(600,10)  '   /yloc ed /xloc ed'
	Write(600,10)  '   (\()/search_for ed'
	Write(600,10)  ''
	Write(600,10)  '   % WIDTH OF STRING'
	Write(600,10)  '   /strw str stringwidth pop def'
	Write(600,10)  ''
	Write(600,10)  '   % IF STRING HAS A "(" THEN ADJUST STRH'
	Write(600,10)  '   str (\() search {'
	Write(600,10)  '      /strh strh .80 mul def'
	Write(600,10)  '   } if'
	Write(600,10)  ''
	Write(600,10)  '   % LOCATE & DRAW STRING'
	Write(600,10)  '   gsave'
	Write(600,10)  '      xloc yloc translate angle rotate'
	Write(600,10)  '      xflag 1 eq {/xx 0 def} if'
	Write(600,10)  '      xflag 2 eq xflag 4 eq or '//
     $	'{/xx strw 2 div neg def} if'
	Write(600,10)  '      xflag 3 eq {/xx strw neg def} if'
	Write(600,10)  '      yflag 1 eq {/yy 0 def} if'
	Write(600,10)  '      yflag 2 eq yflag 4 eq or '//
     $	'{/yy strh 2 div neg def} if'
	Write(600,10)  '      yflag 3 eq {/yy strh neg def} if'
	Write(600,10)  '      xflag 4 eq yflag 4 eq or {'
*	Write(600,10)  '         num_font/Helvetica Font'
	Write(600,10)  '         num_font/'//font_name(1:i1-1)//' Font'
	Write(600,10)  '         xx yy m'
	Write(600,10)  '         str stringwidth pop 0 rl'
	Write(600,10)  '         0 num_ht rl'
	Write(600,10)  '         str stringwidth pop neg 0 rl'
c	Write(600,10)  '         cp gs 1 o fill gr'
	Write(600,10)  '         cp gs 1 setgray fill gr'
	Write(600,10)  '      } if'
	Write(600,10)  '      xx yy m str show'
	Write(600,10)  '   grestore'
	Write(600,10)  ''
	Write(600,10)  '} def'
	Write(600,10)  '/ClearStack {'
	Write(600,10)  '   '
	Write(600,10)  '   % CLEAR THE STACK IF ANYTHING IS ON IT'
	Write(600,10)  '    { count 0 gt'
	Write(600,10)  '      {pop}'
	Write(600,10)  '      {exit} ifelse'
	Write(600,10)  '    } loop'
	Write(600,10)  '   '
	Write(600,10)  '} def'
	Write(600,10)  '/ClearDictStack {'
	Write(600,10)  ''
	Write(600,10)  '   % CLEAR THE DICTIONARY STACK'
	Write(600,10)  '   countdictstack numdict sub {end} repeat'
	Write(600,10)  ''
	Write(600,10)  '} def'


10	Format(A)

	Return
	End
**************************************************
	Subroutine GlobalVar

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer i1,i2,i3,i4

	Write(600,10) '%********************'
	Write(600,10) '% GLOBAL VARIABLES'
	Write(600,10) '%********************'
	Write(600,11) font_title, '/title_font ed'
	Write(600,11) font_label, '/label_font ed'
	Write(600,11) font_num, '/num_font ed'
	Write(600,11) template, '/template ed'
	Write(600,12) arrow_h, '/pct_hh ed'
	Write(600,12) arrow_hh, '/pct_hw ed'
	Write(600,12) arrow_ht, '/pct_vw ed'
	Write(600,12) sp_text, ' cm/sp_text ed'
	Write(600,12) sp_title, ' cm/sp_title ed'
	Write(600,12) sp_scale, ' cm/sp_scale ed'
	Write(600,12) scale_w, ' cm/scale_w ed'
	Write(600,12) scale_h, ' cm/scale_h ed'
	IF (amod(fill_beg,1.).NE.0) THEN
	   Write(600,12) fill_beg,'/scale_beg ed'
	ELSE
	   Write(600,11) nint(fill_beg),'/scale_beg ed'
	ENDIF
	IF (amod(fill_end,1.).NE.0) THEN
	   Write(600,12) fill_end,'/scale_end ed'
	ELSE
	   Write(600,11) nint(fill_end),'/scale_end ed'
	ENDIF
	Write(600,12) fill_inc,'/scale_inc ed'
	Write(600,12) l_marg,' cm/l_marg ed'
	Write(600,12) r_marg,' cm/r_marg ed'
	Write(600,12) t_marg,' cm/t_marg ed'
	Write(600,12) b_marg,' cm/b_marg ed'
	Write(600,12) page_w,' cm/page_w ed'
	Write(600,12) page_h,' cm/page_h ed'
	Write(600,12) sp_row,' cm/sp_row ed'
	Write(600,12) sp_col,' cm/sp_col ed'
C TITLES
	i1 = index(scale_title, '  ')-1
	Write(600,13) scale_title(1:i1),'/scale_title ed'
	i1 = index(title, '  ')-1
	Write(600,13) title(1:i1),'/plot_title ed'
C BOTTOM LABEL
	i1=index(str(1),' ')-1
	i2=index(str(2),' ')-1
	i3=index(str(3),' ')-1
	i4=index(str(4),' ')-1
	IF (plot_type.EQ.1) THEN
  	   Write(600,14) str(1)(1:i1),str(2)(1:i2)
	ELSEIF (plot_type.EQ.2) THEN
	   Write(600,15) str(3)(1:i3)
	ELSEIF (plot_type.EQ.3) THEN
  	   Write(600,16) str(1)(1:i1),str(2)(1:i2),str(3)(1:i3)
	ELSEIF (plot_type.EQ.4) THEN
 	   Write(600,17) str(3)(1:i3),str(4)(1:i4)
	ELSEIF (plot_type.EQ.5) THEN
  	   Write(600,18) str(1)(1:i1),str(2)(1:i2),str(3)(1:i3),
     $                   str(4)(1:i4) 
	ENDIF
	Write(600,19) lat_deg,lat_min,lat_sec
	Write(600,20) long_deg,long_min,long_sec
	Write(600,21) xhoriz_orig,yhoriz_orig
	IF (plot_type.EQ.1.OR.plot_type.EQ.3.OR.plot_type.EQ.5) THEN
	   IF (ptskip.EQ.1) THEN
	      Write(600,22) ptskip 
	   ELSE
	      Write(600,23) ptskip   
	   ENDIF
        ELSE
           Write(600,10) '-999/points_label ed' 
	ENDIF
	IF (catalog_name.NE.'NONE'.AND.fix_axis.EQ.'Z') THEN
	   IF (plane_inc.EQ.1) THEN
	      Write(600,10) '(PLOT AIRCRAFT EVERY )/plane_label1 ed'
	      Write(600,10) '( SCAN)/plane_label2 ed'
	   ELSE
	      Write(600,10) '(PLOT AIRCRAFT EVERY )/plane_label1 ed'
	      Write(600,10) '( SCANS)/plane_label2 ed'
	   ENDIF
	ELSE
           Write(600,10) '-999/plane_label1 ed'
           Write(600,10) '-999/plane_label2 ed'
	ENDIF
	i1=index(scale_pos,' ')-1
	Write(600,25) scale_pos(1:i1),'/scale_pos ed'
	i1=index(title_flag,' ')-1
	Write(600,25) title_flag(1:i1),'/title_flag ed'
	i1=index(scale_flag,' ')-1
	Write(600,25) scale_flag(1:i1),'/scale_flag ed'
 
10	Format(A)
11	Format(I4,A)
12	Format(F7.2,A)
13	Format('(',A,')',A)
14	Format('(VECTOR: ',A,1X,A,')/field_label ed')
15	Format('(CONTOUR: ',A,')/field_label ed')
16	Format('(VECTOR: ',A,1X,A,' CONTOUR: ',A,')/field_label ed')
17	Format('(CONTOUR: ',A,1X,A,')/field_label ed')
18	Format('(VECTOR: ',A,1X,A,' CONTOUR: ',A,1X,A,
     $         ')/field_label ed')
19	Format('(LATITUDE: ',I4,' DEG ',I3,' MIN ',I3,' SEC')
20	Format('  LONGITUDE: ',I4,' DEG ',I3,' MIN ',I3,' SEC)',
     $         '/LatLon_label ed')
21	Format('(ORIGIN: (',F5.2,',',F5.2,'))/ori_label ed')
22	Format('(EVERY ',I1,' POINT PLOTTED)/points_label ed')
23	Format('(EVERY ',I2,' POINTS PLOTTED)/points_label ed')
24	Format('(PLOT AIRCRAFT EVERY SCAN)')
25	Format(A,A)

	Return
	End
**************************************************
	Subroutine DeclareDict 
	
	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Write(600,10) '%********************'
	Write(600,10) '% DICTIONARIES'
	Write(600,10) '%********************'
	Write(600,10) '/PageDict 1 dict def'
	Write(600,10) '/TextDict 1 dict def'
	Write(600,10) '/PanelDict 1 dict def'
	Write(600,10) '/TickDict 1 dict def'
	Write(600,10) '/ScaleDict 1 dict def'
	Write(600,10) '/PolarDict 1 dict def'
	Write(600,10) '/TrackDict 1 dict def'
	Write(600,10) '/VectorDict 1 dict def'
	Write(600,10) '/MyFunction 1 dict def'
	Write(600,*)

10	Format(A)

	Return
	End
**************************************************
	Subroutine TextDict
	
	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer i1

	i1 = index(font_name, ' ')

	Write(600,10) 'TextDict begin'
	Write(600,10) '   /CharHeight {'
	Write(600,10) '   '
	Write(600,10) '      % HEIGHT OF A CHARACTER'
	Write(600,10) '      gs'
	Write(600,10) '         newpath'
	Write(600,10) '         0 0 moveto'
	Write(600,10) '         false charpath'
	Write(600,10) '         flattenpath'
	Write(600,10) '         pathbbox'
	Write(600,10) '         exch pop'
	Write(600,10) '         3 -1 roll pop'
	Write(600,10) '      gr'
	Write(600,10) '   '
	Write(600,10) '   } def'
	Write(600,10) '   /TextHt {'
	Write(600,10) '   '
	Write(600,10) '      % HEIGHT OF TEXT LABELS'
	Write(600,10) '      /fnt ed'
	Write(600,10) '   '
	Write(600,10) '      gs'
*	Write(600,10) '         fnt/Helvetica Font'
	Write(600,10) '         fnt/'//font_name(1:i1-1)//' Font'
	Write(600,10) '         (X)/Char ed'
	Write(600,10) '         Char CharHeight exch pop'
	Write(600,10) '      gr'
	Write(600,10) '   '
	Write(600,10) '   } def'
	Write(600,10) '   /NumWidth {'
	Write(600,10) '   '
	Write(600,10) '      % CONVERT NUMBER TO A STRING '//
     &  '& RETURN ITS WIDTH'
	Write(600,10) '      /num ed'
	Write(600,10) '      /numstr 4 string def'
	Write(600,10) '   '
	Write(600,10) '      gs'
*	Write(600,10) '         num_font/Helvetica Font'
	Write(600,10) '         num_font/'//font_name(1:i1-1)//' Font'
	Write(600,10) '         num numstr cvs stringwidth pop'
	Write(600,10) '      gr'
	Write(600,10) '   '
	Write(600,10) '   } def'
	Write(600,10) '   /LabelAxis {'
	Write(600,10) '   '
	Write(600,10) '      %--------------------'
	Write(600,10) '      % LABEL THE AXIS'
	Write(600,10) '      %--------------------'
	Write(600,10) '   '
*	Write(600,10) '      label_font/Helvetica Font'
	Write(600,10) '      label_font/'//font_name(1:i1-1)//' Font'
	Write(600,10) '      /strh label_ht def'
	Write(600,10) ''
	Write(600,10) '      % LABEL THE HORIZONTAL AXIS'
	Write(600,10) '      /x h_beg hkm h_end hkm h_beg hkm '//
     &  'sub 2 div add def'
	Write(600,10) '      /y v_beg vkm num_ht sub sp_text '//
     &  '2 mul sub def'
	Write(600,10) '      x y 0 h_label 2 3 DrawText'
	Write(600,10) ''
	Write(600,10) '      % LABEL THE VERTICAL AXIS'
	Write(600,10) '      /x h_beg hkm sp_text 2 mul sub nw sub def'
	Write(600,10) '      /y v_beg vkm v_end vkm v_beg vkm sub '//
     &  '2 div add def'
	Write(600,10) '      x y 90 v_label 2 1 DrawText'
	Write(600,10) '   '
	Write(600,10) '   } def'
	Write(600,10) '   /LabelPanel {'
	Write(600,10) ''
*	Write(600,10) '      label_font/Helvetica Font'
	Write(600,10) '      label_font/'//font_name(1:i1-1)//' Font'
	Write(600,10) '      /strh label_ht def'
	Write(600,10) ''
	Write(600,10) '      grid CART eq {'
	Write(600,10) '         /y v_end vkm sp_text add def'
	Write(600,10) '      } { % GRID IS POLAR'
	Write(600,10) '         /y h_end hkm sp_text add def'
	Write(600,10) '      } ifelse'
	Write(600,10) ''
	Write(600,10) '      % LABEL THE DATE'
	Write(600,10) '      grid CART eq {'
	Write(600,10) '         /x h_beg hkm def'
	Write(600,10) '      } { % GRID IS POLAR'
	Write(600,10) '         /x h_end hkm neg def'
	Write(600,10) '      } ifelse'
	Write(600,10) '      x y 0 date 1 1 DrawText'
	Write(600,10) ''
	Write(600,10) '      % LABEL THE TIME'
	Write(600,10) '      grid CART eq {'
	Write(600,10) '         /x h_end hkm h_beg hkm sub 3 '//
     &  'div h_beg hkm add def'
	Write(600,10) '      } { % GRID IS POLAR'
	Write(600,10) '         /x h_end hkm 2 mul 3 div h_end hkm '//
     &  'neg add def'
	Write(600,10) '      } ifelse'
	Write(600,10) '      x y 0 time 2 1 DrawText'
	Write(600,10) '   '
	Write(600,10) '      % LABEL THE PANEL'
	Write(600,10) '      grid CART eq {'
	Write(600,10) '         /x h_end hkm h_beg hkm sub 3 div '//
     &  '2 mul h_beg hkm add def'
	Write(600,10) '      } { % GRID IS POLAR'
	Write(600,10) '         /x h_end hkm 2 mul 3 div 2 mul '//
     &  'h_end hkm neg add def'
	Write(600,10) '      } ifelse'
	Write(600,10) '      x y 0 panel_title 2 1 DrawText'
	Write(600,10) '   '
	Write(600,10) '      % LABEL THE LEVEL'
	Write(600,10) '      grid CART eq {'
	Write(600,10) '         /x h_end hkm h_beg hkm sub h_beg '//
     &  'hkm add def'
	Write(600,10) '      } { % GRID IS POLAR'
	Write(600,10) '         /x h_end hkm def'
	Write(600,10) '      } ifelse'
	Write(600,10) '      x y 0 level 3 1 DrawText'
	Write(600,10) ''
	Write(600,10) '   } def'
	Write(600,10) ''
	Write(600,10) 'end % TextDict'

10	Format(A)

	Return
	End
**************************************************
	Subroutine PageDict 

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer i1

	i1 = index(font_name, ' ')

	Write(600,10) 'PageDict begin'
	Write(600,10) ''
	Write(600,10) '   /OrientPage {'
	Write(600,10) '   '
	Write(600,10) ''
	Write(600,10) '      % CALCULATE ORIENTATION OF PAGE'
	Write(600,10) '      /pw page_w def'
	Write(600,10) '      /ph page_h def'
	Write(600,10) ''
c	Write(600,10) '      template 10 le {'
c	Write(600,10) '         /orientation portrait def'
c	Write(600,10) '      } {'
c	Write(600,10) '         /orientation landscape def'
c	Write(600,10) '      } ifelse'
c	Write(600,10) '   '
c -- sjs add 12/29/97
	Write(600,10) '     template 10 le {/orientation portrait'
	Write(600,10) '                    def} if' 
	Write(600,10) '     template 11 ge template 13 le and'
	Write(600,10) '       {/orientation landscape def} if'
	Write(600,10) '     template 14 ge template 16 le and' 
	Write(600,10) '       {/orientation portrait def} if' 
	Write(600,10) '     template 17 ge template 19 le and' 
	Write(600,10) '       {/orientation landscape def} if'
c -- sjs end add 12/29/97
	Write(600,10) '      orientation landscape eq {'
	Write(600,10) '         % CHANGE TO LANDSCAPE ORIENTATION'
	Write(600,10) '         21.59 cm 0 tr 0 0 m 90 rotate'
	Write(600,10) '         /temp ph def'
	Write(600,10) '         /ph pw def'
	Write(600,10) '         /pw temp def'
	Write(600,10) '      } if'
	Write(600,10) '   '
	Write(600,10) '      % RETURN PAGE WIDTH & HEIGHT'
	Write(600,10) '      ph pw'
	Write(600,10) ''
	Write(600,10) '   } def'
	Write(600,10) '   /CalcNumPanel {'
	Write(600,10) '   '
	Write(600,10) '      % CALCULATE THE NUMBER OF PANELS BASED '
	Write(600,10) '      % ON THE TEMPLATE & RETURN NUM'
	Write(600,10) '      template 1 eq {1} if'
	Write(600,10) '      template 2 eq {2} if'
	Write(600,10) '      template 3 eq {3} if'
	Write(600,10) '      template 4 eq {4} if'
	Write(600,10) '      template 5 eq {5} if'
	Write(600,10) '      template 6 eq {2} if'
	Write(600,10) '      template 7 eq {4} if'
	Write(600,10) '      template 8 eq {6} if'
	Write(600,10) '      template 9 eq {8} if'
	Write(600,10) '      template 10 eq {10} if'
	Write(600,10) '      template 11 eq {1} if'
	Write(600,10) '      template 12 eq {2} if'
	Write(600,10) '      template 13 eq {4} if'
C -- sjs add 12/29/97
	Write(600,10) '      template 14 eq {3} if'
	Write(600,10) '      template 15 eq {6} if'
	Write(600,10) '      template 16 eq {9} if'
	Write(600,10) '      template 17 eq {3} if'
	Write(600,10) '      template 18 eq {6} if'
	Write(600,10) '      template 19 eq {9} if'
	Write(600,10) '      template 20 eq {12} if'
c -- % sjs end add 12/29/97
	Write(600,10) '   '
	Write(600,10) '   } def'
	Write(600,10) '   /CalcScaleDim {'
	Write(600,10) '   '
	Write(600,10) '      % MAKE SURE THE SCALE DIMENSIONS '//
     &  'ARE CORRECT'
	Write(600,10) '   '
	Write(600,10) '      /sh scale_h def'
	Write(600,10) '      /sw scale_w def'
	Write(600,10) '   '
	Write(600,10) '      /bigdim sh sw Largest def'
	Write(600,10) '   '
	Write(600,10) '      scale_pos VER eq {'
	Write(600,10) '         sh bigdim ne {'
	Write(600,10) '            /sw sh def'
	Write(600,10) '            /sh bigdim def'
	Write(600,10) '         } if'
	Write(600,10) '      } { % scale_pos = HOR'
	Write(600,10) '         sw bigdim ne {'
	Write(600,10) '            /sh sw def'
	Write(600,10) '            /sw bigdim def'
	Write(600,10) '         } if'
	Write(600,10) '      } ifelse'
	Write(600,10) '   '
	Write(600,10) '      % RETURN THE WIDTH & HEIGHT OF SCALE'
	Write(600,10) '      sh sw'
	Write(600,10) '   '
	Write(600,10) '   } def'
	Write(600,10) '   /CalcMarg {'
	Write(600,10) '   '
	Write(600,10) '      % CALCULATE SIZE OF MARGINS'
	Write(600,10) '      /tm t_marg def'
	Write(600,10) '      /bm b_marg def'
	Write(600,10) '      /rm r_marg def'
	Write(600,10) '      /lm l_marg def'
	Write(600,10) '      /nw TextDict begin scale_end cvi '//
     &  'NumWidth end def'
	Write(600,10) ''
	Write(600,10) '      /tm'
	Write(600,10) '         tm sp_title add title_ht add def'
	Write(600,10) '   '
	Write(600,10) '      title_flag ON eq { % IF TITLE_FLAG IS ON'
	Write(600,10) '      % ADD SPACE OF BOTTOM TITLE TO '//
     &  'BOTTOM MARGIN'
	Write(600,10) '         /bm'
	Write(600,10) '            bm label_ht 5 mul add label_ht '//
     &  '.75 mul 4 mul add'
	Write(600,10) '            sp_text 2 mul add def'
	Write(600,10) '      } if'
	Write(600,10) '   '
	Write(600,10) '      scale_flag ON eq {'
	Write(600,10) '         scale_pos HOR eq { % IF SCALE_POS='//
     &  'HORIZONTAL'
	Write(600,10) '            /bm'
	Write(600,10) '               bm sh add sp_text add num_ht add'
	Write(600,10) '               sp_scale add def'
	Write(600,10) '         } {  % ELSE IF SCALE_POS=VERTICAL'
	Write(600,10) '            % CONVERT SCALE_END TO A STRING '//
     &  '& CALULATE ITS WIDTH'
	Write(600,10) '            /rm'
	Write(600,10) '               rm nw add sp_text add'
	Write(600,10) '               sw add sp_scale add def'
	Write(600,10) '         } ifelse'
	Write(600,10) '      } if'
	Write(600,10) '   '
	Write(600,10) '      % RETURN TOP, BOTTOM & RIGHT MARGINS'
	Write(600,10) '      tm bm rm'
	Write(600,10) '   '
	Write(600,10) '   } def'
	Write(600,10) '   /CalcPanelSize {'
	Write(600,10) '   '
	Write(600,10) '      % CALCULATE THE INITIAL SIZE OF EACH '//
     &  'PANEL'
	Write(600,10) '      % RETURN: pnw, pnh'
	Write(600,10) '      % CALCULATE THE PANEL HEIGHT'
	Write(600,10) '      /pnh'
	Write(600,10) '         ph tm sub bm sub def'
	Write(600,10) ''
c -- sjs add 12/29/97
	Write(600,10) '      /tmp_num 0 def'
	Write(600,10) '      template 2 eq {/tmp_num 2 def} if'  
	Write(600,10) '      template 3 eq {/tmp_num 3 def} if'  
	Write(600,10) '      template 4 eq {/tmp_num 4 def} if'  
	Write(600,10) '      template 5 eq {/tmp_num 5 def} if'  
	Write(600,10) '      template 7 eq {/tmp_num 2 def} if'  
	Write(600,10) '      template 8 eq {/tmp_num 3 def} if'  
	Write(600,10) '      template 9 eq {/tmp_num 4 def} if'  
	Write(600,10) '      template 10 eq {/tmp_num 5 def} if'  
	Write(600,10) '      template 13 eq {/tmp_num 2 def} if'  
	Write(600,10) '      template 15 eq {/tmp_num 2 def} if'  
	Write(600,10) '      template 16 eq {/tmp_num 3 def} if'  
	Write(600,10) '      template 18 eq {/tmp_num 2 def} if'  
	Write(600,10) '      template 19 eq {/tmp_num 3 def} if'  

	Write(600,10) '      tmp_num 0 gt {/pnh pnh tmp_num '//
     &  'div def} if'
c -- sjs end add 12/29/97
c	Write(600,10) '      template 10 le {  % portrait orientation'
c	Write(600,10) '         template 2 eq template 7 eq or {'
c	Write(600,10) '            /pnh pnh 2 div def'
c	Write(600,10) '         } if'
c	Write(600,10) '         template 3 eq template 8 eq or {'
c	Write(600,10) '            /pnh pnh 3 div def'
c	Write(600,10) '         } if'
c	Write(600,10) '         template 4 eq template 9 eq or {'
c	Write(600,10) '            /pnh pnh 4 div def'
c	Write(600,10) '         } if'
c	Write(600,10) '         template 5 eq template 10 eq or {'
c	Write(600,10) '            /pnh pnh 5 div def'
c	Write(600,10) '         } if'
c	Write(600,10) '      } { %landscape orientation'
c	Write(600,10) '         template 13 eq {'
c	Write(600,10) '            /pnh pnh 2 div def'
c	Write(600,10) '         } if'
c	Write(600,10) '      } ifelse'
c	Write(600,10) '   '
cC-- Added by wcl to accomodate 3 colume configurations 11/26/97
c	Write(600,10) '      template 13 gt template 20 lt and'
c        Write(600,10) ' {  % 3 columes'
c	Write(600,10) '         template 15 eq template 18 eq or {'
c	Write(600,10) '            /pnh pnh 2 div def'
c	Write(600,10) '         } if'
c	Write(600,10) '         template 16 eq template 19 eq or {'
c	Write(600,10) '            /pnh pnh 3 div def'
c	Write(600,10) '         } if'
c	Write(600,10) '      } ifelse'
c	Write(600,10) '   '
cC-- End additions 11/26/97
	Write(600,10) '      % CALCULATE THE PANEL WIDTH '
	Write(600,10) '      /pnw'
	Write(600,10) '         pw lm sub rm sub def'
	Write(600,10) '      template 5 gt template 10 le and {'
	Write(600,10) '         /pnw pnw 2 div def'
	Write(600,10) '      } if'
	Write(600,10) '      template 12 eq template 13 eq or {'
	Write(600,10) '         /pnw pnw 2 div def'
	Write(600,10) '      } if'
	Write(600,10) '   '
C-- Added by wcl to accomodate 3 colume configurations 11/26/97
	Write(600,10) '      template 13 gt template 19 lt and {'
	Write(600,10) '         /pnw pnw 3 div def'
	Write(600,10) '      } if'
C-- End additions 11/26/97
	Write(600,10) '      % RETURN PANEL HEIGHT & WIDTH'
	Write(600,10) '      pnh pnw'
	Write(600,10) '   '
	Write(600,10) '   } def'
	Write(600,10) '   /BottomLabel {'
	Write(600,10) '   '
*	Write(600,10) '      label_font/Helvetica Font'
	Write(600,10) '      label_font/'//font_name(1:i1-1)//' Font'
	Write(600,10) '      /strh label_ht def'
	Write(600,10) ''
	Write(600,10) '      % BOTTOM LABEL'
	Write(600,10) '      title_flag ON eq {'
	Write(600,10) '         % CALCULATE THE POSITION'
	Write(600,10) '         /xpos l_marg def'
	Write(600,10) '         /ypos'
	Write(600,10) '            YPOS num_panel 1 sub get sp_text '//
     &  '4 mul sub'
	Write(600,10) '            num_ht sub label_ht sub def'
	Write(600,10) '         scale_pos HOR eq scale_flag ON eq and {'
	Write(600,10) '            /ypos'
	Write(600,10) '               ypos sp_scale sub scale_h sub'
	Write(600,10) '               sp_text sub num_ht sub def'
	Write(600,10) '         } if'
	Write(600,10) '   '
	Write(600,10) '         % PRINT OUT THE TEXT'
	Write(600,10) '         /ypos ypos label_ht sub def'
	Write(600,10) '         xpos ypos 0 field_label 1 1 DrawText'
	Write(600,10) '         /ypos ypos label_ht sub label_ht '//
     &  '.75 mul sub def'
	Write(600,10) '         xpos ypos 0 LatLon_label 1 1 DrawText'
	Write(600,10) '         /ypos ypos label_ht sub label_ht '//
     &  '.75 mul sub def'
	Write(600,10) '         xpos ypos 0 ori_label 1 1 DrawText'
	Write(600,10) '         points_label -999 ne {'
	Write(600,10) '            /ypos ypos label_ht sub label_ht '//
     &  '.75 mul sub def'
	Write(600,10) '            xpos ypos 0 points_label 1 1 '//
     &  'DrawText'
	Write(600,10) '         } if'
	Write(600,10) '         plane_label1 -999 ne plane_label2 '//
     &  '-999 ne and {'
	Write(600,10) '            /planestr 3 string def'
	Write(600,10) '            /ypos ypos label_ht sub label_ht '//
     &  '.75 mul sub def'
	Write(600,10) '            xpos ypos 0 plane_label1 1 1 '//
     &  'DrawText'
	Write(600,10) '            /xpos xpos plane_label1 '//
     &  'stringwidth pop add def'
	Write(600,10) '            xpos ypos 0 plane_inc planestr '//
     &  'cvs 1 1 DrawText'
	Write(600,10) '            /xpos xpos plane_inc planestr '//
     &  'cvs stringwidth pop add def'
	Write(600,10) '            xpos ypos 0 plane_label2 1 1 '//
     &  'DrawText'
	Write(600,10) '         } if'
	Write(600,10) '      } if'
	Write(600,10) '   '
	Write(600,10) '   } def'
	Write(600,10) '   /WriteTitle {'
	Write(600,10) '   '
	Write(600,10) '      % WRITE TITLE OF PLOT'
*	Write(600,10) '      title_font/Helvetica Font'
	Write(600,10) '      title_font/'//font_name(1:i1-1)//' Font'
	Write(600,10) '      /strh title_ht def'
	Write(600,10) ' '
	Write(600,10) '      % GET THE WIDTH & HEIGHT OF BOX'
	Write(600,10) '      % TO POSITION TITLE'
	Write(600,10) ''
*	Write(600,10) '      /bw BOX_W panel 1 sub get def'
*	Write(600,10) '      /bh BOX_H panel 1 sub get def'
	Write(600,10) '      /bw BOX_W 0 get def'
	Write(600,10) '      /bh BOX_H 0 get def'
	Write(600,10) ''
	Write(600,10) '      template 5 gt template 13 le and '//
     &  'template 11 ne and {'
*	Write(600,10) '         XPOS 1 get sp_row 2 div sub'
	Write(600,10) '         XPOS 1 get sp_text 2 mul num_ht '//
     &	'add label_ht add'
	Write(600,10) '         sp_col add 2 div sub'
	Write(600,10) '	 YPOS 1 get YPOS 0 get Largest bh add '
	Write(600,10) '	 sp_text add label_ht add sp_title add'
	Write(600,10) '      } if'
	Write(600,10) ''
	Write(600,10) '      template 5 le template 11 eq or {'
	Write(600,10) '	 XPOS 0 get bw 2 div add'
	Write(600,10) '	 YPOS 0 get bh add sp_text add'
	Write(600,10) '	 sp_title add label_ht add'
	Write(600,10) '      } if'
	Write(600,10) ''
C-- Added by wcl to accomodate 3 column configuration 11/26/97
	Write(600,10) '      template 13 gt template 20 lt and {'
	Write(600,10) '	 XPOS 1 get bw 2 div add'
	Write(600,10) '	 YPOS 1 get bh add sp_text add'
	Write(600,10) '	 sp_title add label_ht add'
	Write(600,10) '      } if'
	Write(600,10) ''
C- End additions 11/26/97
	Write(600,10) '      0 plot_title 2 1 DrawText'
	Write(600,10) '   '
	Write(600,10) '   } def'
	Write(600,10) 'end % PageDict'

10	Format(A)

	Return
	End
**************************************************
	Subroutine PanelDict

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer i1

	i1 = index(font_name, ' ')

	Write(600,10) 'PanelDict begin'
	Write(600,10) ''
	Write(600,10) '   /SetUp {'
	Write(600,10) '   '
	Write(600,10) '      % INITIAL SETUP OF PANEL '
	Write(600,10) ''
	Write(600,10) '      % WIDTH OF VERTICAL END VALUE'
	Write(600,10) '       panel 1 eq {'
	Write(600,10) '	  v_beg 0 lt {'
	Write(600,10) '	     /nw TextDict begin v_beg '//
     &  'cvi NumWidth end def'
	Write(600,10) '	  } {'
	Write(600,10) '	     /nw TextDict begin v_end '//
     &  'cvi NumWidth end def'
	Write(600,10) '	  } ifelse'
	Write(600,10) '       } if '
	Write(600,10) ''
	Write(600,10) '      panel 1 eq {'
	Write(600,10) '         /XPOS 10 array def'
	Write(600,10) '         /YPOS 10 array def'
	Write(600,10) '         /BOX_W 10 array def'
	Write(600,10) '         /BOX_H 10 array def'
	Write(600,10) '      } if'
	Write(600,10) ''
	Write(600,10) '      % CALCULATE MAXIMUM WIDTH OF THE BOX'
	Write(600,10) '      CalcMaxWidth'
	Write(600,10) '      box_w -999 ne box_w max_w le and {'
	Write(600,10) '         /max_w box_w def'
	Write(600,10) '      } if'
	Write(600,10) ''
	Write(600,10) '      % CALCULATE THE MAXIMUM HEIGHT OF THE BOX'
	Write(600,10) '      CalcMaxHeight'
	Write(600,10) '   '
	Write(600,10) '      box_h -999 ne box_h max_h le and {'
	Write(600,10) '         /max_h box_h def'
	Write(600,10) '      } if'
	Write(600,10) ''
	Write(600,10) '      % CALCULATE THE HORIZONTAL & VERTICAL '//
     &  'RANGE'
	Write(600,10) '      grid CART eq {'
	Write(600,10) '         /h_range h_end h_beg sub def'
	Write(600,10) '         /v_range v_end v_beg sub def'
	Write(600,10) '      } { % IF GRID IS POLAR'
	Write(600,10) '         /h_range h_end 2 mul def'
	Write(600,10) '         /v_range h_range def'
	Write(600,10) '      } ifelse'
	Write(600,10) ''
	Write(600,10) '      % CALCULATE THE HORIZONTAL & VERTICAL '//
     &  'VALUE OF KM'
	Write(600,10) '      CalcKm'
	Write(600,10) ''
	Write(600,10) '      % CALCULATE THE WIDTH & HEIGHT OF BOX '//
     &  'IF USER DOES NOT SPECIFY'
	Write(600,10) '      box_w -999 eq {'
	Write(600,10) '	 BOX_W panel 1 sub h_range hkm put'
	Write(600,10) '      } {'
	Write(600,10) '	 BOX_W panel 1 sub h_range hkm put'
	Write(600,10) '      } ifelse'
	Write(600,10) ''
	Write(600,10) '      box_h -999 eq {'
	Write(600,10) '	 BOX_H panel 1 sub v_range vkm put'
	Write(600,10) '      } {'
	Write(600,10) '	 BOX_H panel 1 sub v_range vkm put'
	Write(600,10) '      } ifelse'
	Write(600,10) ''
	Write(600,10) '      % CALCULATE THE HORIZONTAL & VERTICAL '//
     &  'COORDINATES OF THE GRID'
	Write(600,10) '      XPOS panel 1 sub CalcXPos put '
	Write(600,10) '      YPOS panel 1 sub CalcYPos put '
	Write(600,10) ''
	Write(600,10) '   } def'
	Write(600,10) '   /CalcMaxWidth {'
	Write(600,10) '   '
	Write(600,10) '      %--------------------'
	Write(600,10) '      % CALCULATE MAXIMUM WIDTH OF BOX'
	Write(600,10) '      % V_END=VERTICAL END COORDINATE'
	Write(600,10) '      %--------------------'
	Write(600,10) '   '
	Write(600,10) '      % MAXIMUM WIDTH OF THE BOX'
	Write(600,10) '      /max_w'
	Write(600,10) '         panel_w label_ht sub nw sub'
	Write(600,10) '         sp_text 2 mul sub def'
	Write(600,10) '   '
	Write(600,10) '      % SUBTRACT SPACE FOR COLUMNS'
	Write(600,10) '      template 6 ge template 11 ne and {'
	Write(600,10) '         /max_w'
	Write(600,10) '            max_w sp_col 2 div sub def'
	Write(600,10) '      } if'
	Write(600,10) '   '
C-- Add by wcl to accomodate 3 colume configuration 11/26/97
	Write(600,10) '      template 13 gt template 20 lt and {'
	Write(600,10) '         /max_w'
	Write(600,10) '            max_w sp_col 2 div sub def'
	Write(600,10) '      } if'
	Write(600,10) '   '
C-- End addition 11/26/97
	Write(600,10) '   } def'
	Write(600,10) '   /CalcMaxHeight {'
	Write(600,10) '   '
	Write(600,10) '      % CALCULATE MAXIMUM HEIGHT OF BOX'
	Write(600,10) '      /max_h'
	Write(600,10) '         panel_h label_ht 2 mul sub num_ht sub'
	Write(600,10) '         sp_text 3 mul sub sp_row sub def'
	Write(600,10) '   '
	Write(600,10) '   } def'
	Write(600,10) '   /CalcKm {'
	Write(600,10) '   '
	Write(600,10) '      %--------------------'
	Write(600,10) '      % CALCULATE THE VALUE OF KM'
	Write(600,10) '      % BOX_W=LENGTH OF HORIZONTAL BOUNDARY'
	Write(600,10) '      % BOX_H=LENGTH OF VERTICAL BOUNDARY'
	Write(600,10) '      % H_STRETCH=HORIZONTAL STRETCH FACTOR'
	Write(600,10) '      % V_STRETCH=VERTICAL STRETCH FACTOR'
	Write(600,10) '      % H_RANGE=HORIZONTAL RANGE'
	Write(600,10) '      % V_RANGE=VERTICAL RANGE'
	Write(600,10) '      %--------------------'
	Write(600,10) '   '
	Write(600,10) '      % CALCULATE THE HORIZONTAL AND '//
     &  'VERTICAL VALUE OF THE KM'
	Write(600,10) '      /h_km max_w h_range div def'
	Write(600,10) '      /v_km max_h v_range div def'
	Write(600,10) '   '
	Write(600,10) '      % CALCULATE THE SCALING FACTOR IF '//
     &  'USER DOES NOT'
	Write(600,10) '      % ENTER SPECIFIC BOX SIZE'
	Write(600,10) '      box_w -999 eq box_h -999 eq and {'
	Write(600,10) '         % Only use the smallest value'
	Write(600,10) '         h_km v_km lt {'
	Write(600,10) '            /v_km h_km def'
	Write(600,10) '         } {'
	Write(600,10) '            /h_km v_km def'
	Write(600,10) '         } ifelse'
	Write(600,10) '      } if'
	Write(600,10) '   '
	Write(600,10) '      % CHECK TO MAKE SURE THAT THE STRETCH '//
     &  'FACTOR'
	Write(600,10) '      % DOES NOT EXCEED THE MAXIMUM BOX SIZE'
	Write(600,10) '      h_range h_km mul h_stretch mul max_w gt {'
	Write(600,10) '         /h_stretch max_w h_range h_km mul '//
     &  'div def'
	Write(600,10) '      } if'
	Write(600,10) '      v_range v_km mul v_stretch mul max_h '//
     &  'gt {'
	Write(600,10) '         /v_stretch max_h v_range v_km mul '//
     &  'div def'
	Write(600,10) '      } if'
	Write(600,10) '   '
	Write(600,10) '      % APPLY THE STRETCH FACTORS'
	Write(600,10) '      grid CART eq {'
	Write(600,10) '         /h_km h_km h_stretch mul def'
	Write(600,10) '         /v_km v_km v_stretch mul def'
	Write(600,10) '      } if'
	Write(600,10) ''
	Write(600,10) '      /hkm {h_km mul} def'
	Write(600,10) '      /vkm {v_km mul} def'
	Write(600,10) '   '
	Write(600,10) '   } def'
	Write(600,10) '   /CalcXPos {'
	Write(600,10) ''
	Write(600,10) '      % CALCULATE HORIZONTAL POSITION OF'
	Write(600,10) '      % LEFT CORNER OF BOX'
	Write(600,10) '      /bw BOX_W panel 1 sub get def'
	Write(600,10) ''
	Write(600,10) '      % 1 COLUMN '
	Write(600,10) '      template 5 le template 11 eq or {'
	Write(600,10) '  '
*	Write(600,10) '	 grid CART eq {'
*	Write(600,10) '	    /xpos xpos sp_text 2 mul add nw '//
*     &  'add label_ht add def'
*	Write(600,10) '	    panel_w bw sp_text 2 mul add nw '//
*     &  'add label_ht add sub '
*	Write(600,10) '	    1 gt { '
*	Write(600,10) '	       % CENTER THE BOX IN THE COLUMN'
*	Write(600,10) '	       /xpos xpos panel_w 2 div add '//
*     &  'bw 2 div sub def'
*	Write(600,10) '	    } if'
*	Write(600,10) '	 } if'
	Write(600,10) '	 /xpos l_marg def'
	Write(600,10) '         grid CART eq {'
	Write(600,10) '            %/xpos xpos sp_text 2 mul '//
     &  'add nw add label_ht add def'
	Write(600,10) '            /pos1 l_marg label_ht add nw '//
     &  'add sp_text 2 mul add def'
	Write(600,10) '            /pos2 xpos panel_w 2 div add '//
     &  'bw 2 div sub def'
	Write(600,10) '         } {'
	Write(600,10) '            /pos1 l_marg def'
	Write(600,10) '            /pos2 xpos panel_w 2 div add '//
     &  'bw 2 div sub def'
	Write(600,10) '         } ifelse'
	Write(600,10) '         % CENTER THE PLOT'
	Write(600,10) '         pos2 pos1 gt {'
	Write(600,10) '            /xpos pos2 def'
	Write(600,10) '         } {'
	Write(600,10) '            /xpos pos1 def'
	Write(600,10) '         } ifelse'


	Write(600,10) ''
	Write(600,10) '      } { % template > 5 && <= 10; '//
     &  'template = 12 or 13  '
	Write(600,10) '	    % PANEL 1, 3, 5, 7, 9'
	Write(600,10) '            panel 2 mod 0 ne {'
	Write(600,10) '	       /xpos l_marg def'
	Write(600,10) '	       %/xpos l_marg panel_w add '//
     &  'BOX_W panel 1 sub get sub def'
*	Write(600,10) '	       grid CART eq {'
	Write(600,10) '	          /xpos xpos sp_text 2 mul '//
     &  'add nw add label_ht add def'
*	Write(600,10) '	       } if'
	Write(600,10) '	    } { %PANEL 2, 4, 6, 8, 10'
	Write(600,10) '	       /xpos XPOS panel 2 sub get '//
     &  'BOX_W panel 2 sub get add '
	Write(600,10) '	       sp_col add label_ht add nw '//
     &  'add sp_text 2 mul add def'
	Write(600,10) '	    } ifelse'
c --%** sjs add 12/29/97
	Write(600,10) '	    template 14 ge {'
	Write(600,10) '	       panel 3 mod 1 eq {'
	Write(600,10) '	          /xpos l_marg def'
	Write(600,10) '	          /xpos xpos sp_text 2 mul' 
	Write(600,10) '             add nw add label_ht add def'
	Write(600,10) '	       } {'	
	Write(600,10) '	          /xpos XPOS panel 2 sub' 
	Write(600,10) '            get BOX_W panel 2 sub get add' 
	Write(600,10) '		  sp_col add label_ht add' 
	Write(600,10) '            nw add sp_text 2 mul add def'
	Write(600,10) '	       } ifelse'
	Write(600,10) '            } if'
c --%** sjs end add 12/29/97
	Write(600,10) '      } ifelse'
	Write(600,10) ''
	Write(600,10) '      % RETURN LOWER LEFT HORIZONTAL COORDINATE'
	Write(600,10) '      xpos'
	Write(600,10) ''
	Write(600,10) '   } def'
	Write(600,10) '   /CalcYPos {'
	Write(600,10) ''
	Write(600,10) '      % CALCULATE VERTICAL POSITION OF'
	Write(600,10) '      % LEFT CORNER OF BOX'
	Write(600,10) ''
	Write(600,10) '      template 5 le template 11 eq or {'
	Write(600,10) ''
	Write(600,10) '	 panel 1 eq {'
	Write(600,10) '	    /ypos'
	Write(600,10) '	       page_h t_marg sub label_ht '//
     &  'sub sp_text sub '
	Write(600,10) '	       BOX_H 0 get sub def'
	Write(600,10) '	 } {'
	Write(600,10) '	    /ypos '
	Write(600,10) '	       YPOS panel 2 sub get sp_text '//
     &  '2 mul sub '
	Write(600,10) '	       num_ht sub label_ht sub '//
     &  'sp_row sub '
	Write(600,10) '	       BOX_H panel 1 sub get sub def'
	Write(600,10) ''
	Write(600,10) '	 } ifelse'
	Write(600,10) ''
	Write(600,10) '      } { % template > 5 && <= 10; '//
     &  'template = 12 or 13  '
	Write(600,10) '	 % PANEL 1, 3, 5, 7, 9'
	Write(600,10) '         panel 2 mod 0 ne {'
	Write(600,10) '	    panel 1 eq {'
	Write(600,10) '	       /ypos'
	Write(600,10) '	          page_h t_marg sub '//
     &  'label_ht sub sp_text sub '
	Write(600,10) '	          BOX_H 0 get sub def'
	Write(600,10) '	    } { % panel > 1'
	Write(600,10) '	       /ypos '
	Write(600,10) '	          YPOS panel 2 sub get '//
     &  'sp_text 2 mul sub '
	Write(600,10) '	          num_ht sub label_ht sub '//
     &  'sp_row sub '
	Write(600,10) '	          BOX_H panel 1 sub get sub def'
	Write(600,10) '	    } ifelse'
	Write(600,10) '	 } { %PANEL 2, 4, 6, 8, 10'
	Write(600,10) '	    /ypos YPOS panel 2 sub get def'
	Write(600,10) '	 } ifelse'
	Write(600,10) '      } ifelse'
	Write(600,10) '      '
c --%** sjs add 12/29/97
	Write(600,10) '     template 14 ge {'
	Write(600,10) '	panel 3 le {'
	Write(600,10) '	   /ypos'
	Write(600,10) '	      page_h t_marg sub label_ht sub '
	Write(600,10) '                sp_text sub' 
	Write(600,10) '	      BOX_H 0 get sub def'
	Write(600,10) '	} {'
	Write(600,10) '	   /ypos'
	Write(600,10) '	      YPOS panel 4 sub get sp_text 2' 
	Write(600,10) '                mul sub' 
	Write(600,10) '	      num_ht sub label_ht sub sp_row sub' 
	Write(600,10) '	      BOX_H panel 1 sub get sub def'
	Write(600,10) '	} ifelse'
	Write(600,10) '     } if'
c --%** sjs end add 12/29/97
	Write(600,10) '      % RETURN LOWER LEFT VERTICAL COORDINATE'
	Write(600,10) '      ypos'
	Write(600,10) ''
	Write(600,10) '   } def'
	Write(600,10) '   /Trans2Origin {'
	Write(600,10) '   '
	Write(600,10) '      %--------------------'
	Write(600,10) '      % TRANSLATE TO ORIGIN OF GRID'
	Write(600,10) '      % XPOS=HORIZONTAL POSITION OF LOWER'
	Write(600,10) '      %      LEFT CORNER OF BOX'
	Write(600,10) '      % YPOS=VERTICAL POSITION OF LOWER'
	Write(600,10) '      %      LEFT CORNER OF BOX'
	Write(600,10) '      % H_BEG=HORIZONTAL BEGIN COORDINATE'
	Write(600,10) '      % V_BEG=VERTICAL BEGIN COORDINATE'
	Write(600,10) '      %--------------------'
	Write(600,10) '   '
	Write(600,10) '      /xpos XPOS panel 1 sub get def'
	Write(600,10) '      /ypos YPOS panel 1 sub get def'
	Write(600,10) ''
	Write(600,10) '      % CALCULATE HORIZONTAL & VERTICAL ORIGIN'
	Write(600,10) '      grid CART eq {'
	Write(600,10) '         /orix xpos h_beg hkm sub def'
	Write(600,10) '         /oriy ypos v_beg vkm sub def'
	Write(600,10) '      } { % IF GRID IS POLAR'
	Write(600,10) '         /orix h_range 2 div hkm xpos add def'
	Write(600,10) '         /oriy v_range 2 div vkm ypos add def'
	Write(600,10) '      } ifelse'
	Write(600,10) '   '
	Write(600,10) '      % TRANSLATE TO THE HORIZONTAL & '//
     &  'VERTICAL ORIGIN'
	Write(600,10) '      orix oriy tr'
	Write(600,10) '   '
	Write(600,10) '   } def'
	Write(600,10) '   /ClipIt {'
	Write(600,10) '   '
	Write(600,10) '      % DEFINE CLIPPING PATH'
	Write(600,10) '   '
	Write(600,10) '      grid CART eq {'
	Write(600,10) '         newpath'
	Write(600,10) '         h_beg hkm v_beg vkm m'
	Write(600,10) '         h_end hkm v_beg vkm l'
	Write(600,10) '         h_end hkm v_end vkm l'
	Write(600,10) '         h_beg hkm v_end vkm l'
	Write(600,10) '         cp clip newpath'
	Write(600,10) '      } { % IF GRID IS POLAR'
	Write(600,10) '         newpath'
	Write(600,10) '         0 0 h_end hkm 0 360 arc clip newpath'
	Write(600,10) '      } ifelse'
	Write(600,10) '   '
	Write(600,10) '   } def'
	Write(600,10) '   /Draw&LabelBoundary {'
	Write(600,10) '  '
	Write(600,10) '      %--------------------'
	Write(600,10) '      % DRAW & LABEL THE BOUNDARY'
	Write(600,10) '  '
	Write(600,10) '      % DRAW THE BOUNDARY'
	Write(600,10) '      DrawBoundary'
	Write(600,10) ''
	Write(600,10) '      grid CART eq {'
	Write(600,10) '         % DRAW THE TICK MARKS & LABEL THEM'
	Write(600,10) '         TickDict begin DrawTick end % TickDict'
	Write(600,10) '         % LABEL THE AXIS'
	Write(600,10) '         TextDict begin LabelAxis '//
     &  'end  % TextDict'
	Write(600,10) '         % DRAW THE VECTOR SCALE'
*	Write(600,10) '         plot_type 1 eq plot_type 3 eq or {'
	Write(600,10) '         plot_type 2 mod 0 ne {'
	Write(600,10) '            DrawScaleVector'
	Write(600,10) '         } if'
	Write(600,10) '      } { % GRID IS POLAR'
	Write(600,10) '         TickDict begin DrawDistScale '//
     &  'end %TickDict'
	Write(600,10) '      } ifelse'
	Write(600,10) ''
	Write(600,10) '      % LABEL THE PANEL IDENTIFIERS'
	Write(600,10) '      TextDict begin LabelPanel end'
	Write(600,10) '  '
	Write(600,10) '   } def'
	Write(600,10) '   /DrawBoundary {'
	Write(600,10) '   '
	Write(600,10) '      %--------------------'
	Write(600,10) '      % DRAW BOUNDARY'
	Write(600,10) '   '
	Write(600,10) '      % DRAW THE BOUNDARY'
	Write(600,10) '      grid CART eq {'
	Write(600,10) '         h_beg hkm v_beg vkm m'
	Write(600,10) '         h_end hkm v_beg vkm l'
	Write(600,10) '         h_end hkm v_end vkm l'
	Write(600,10) '         h_beg hkm v_end vkm l'
	Write(600,10) '         cp s'
	Write(600,10) '      } { % IF GRID IS POLAR'
	Write(600,10) '         newpath 0 0 h_end hkm 0 '//
     &  '360 arc stroke newpath'
	Write(600,10) '      } ifelse'
	Write(600,10) '   '
	Write(600,10) '   } def'
	Write(600,10) ''
	Write(600,10) '   /DrawScaleVector {'
	Write(600,10) '   '
	Write(600,10) '      %--------------------'
	Write(600,10) '      % DRAW & LABEL THE SCALE VECTOR'
	Write(600,10) '      % H_END=HORIZONTAL END COORDINATE'
	Write(600,10) '      % V_BEG=VERTICAL BEGIN COORDINATE'
	Write(600,10) '      % VEC_SPEED=VECTOR SCALE SPEED (M/S)'
	Write(600,10) '      % VEC_DIST=VECTOR SCALE DISTANCE (KM)'
	Write(600,10) '      %--------------------'
	Write(600,10) '   '
	Write(600,10) '      % HORIZONTAL & VERTICAL COORDINATES FOR'
	Write(600,10) '      % END OF VECTOR'
	Write(600,10) '      /x h_end vec_dist sub hkm def'
	Write(600,10) '      /y v_beg vkm num_ht sub sp_text 2 mul sub '
	Write(600,10) '	 label_ht 2 div sub def'
	Write(600,10) ''
	Write(600,10) '      % SCALE OF VECTOR'
	Write(600,10) '      /scale vec_dist vec_speed div def'
	Write(600,10) '   '
	Write(600,10) '      % DRAW THE VECTOR'
	Write(600,10) '      x y vec_speed 0 scale VectorDict begin '//
     &  'Vector end % VectorDict'
	Write(600,10) '   '
	Write(600,10) '      % LABEL THE VECTOR'
*	Write(600,10) '      num_font/Helvetica Font'
	Write(600,10) '      num_font/'//font_name(1:i1-1)//' Font'
	Write(600,10) '      /vecstr 7 string def'
	Write(600,10) '      ( m/s)/labstr ed'
	Write(600,10) '   '
	Write(600,10) '      % CONVERT SPEED TO INTEGER IF IT '//
     &  'IS A WHOLE NUMBER'
	Write(600,10) '      vec_speed ceiling vec_speed div 1 eq'
	Write(600,10) '         {/vec_speed vec_speed cvi def} if'
	Write(600,10) '   '
	Write(600,10) '      % DRAW THE VECTOR SPEED'
*	Write(600,10) '      label_font/Helvetica Font'
	Write(600,10) '      label_font/'//font_name(1:i1-1)//' Font'
	Write(600,10) '      /strh label_ht def'
	Write(600,10) '      /m_send x 7 sub def'
	Write(600,10) '      /m_sbeg m_send labstr stringwidth '//
     &  'pop sub def'
	Write(600,10) '      m_send y 0 labstr 3 2 DrawText '
	Write(600,10) '      m_sbeg y 0 vec_speed vecstr cvs '//
     &  '3 2 DrawText '
	Write(600,10) '   '
	Write(600,10) '   } def'

	Write(600,10) 'end % PanelDict'

10	Format(A)

	Return
	End
**************************************************
	Subroutine TickDict

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer i1

	i1 = index(font_name, ' ')

	Write(600,10) 'TickDict begin'
	Write(600,10) ''
	Write(600,10) '   /DrawTick {'
	Write(600,10) ''
*	Write(600,10) '      num_font/Helvetica Font'
	Write(600,10) '      num_font/'//font_name(1:i1-1)//' Font'
	Write(600,10) '      /strh num_ht def'
	Write(600,10) ''
	Write(600,10) '      % CALCULATE THE HORIZONTAL & '//
     &  'VERTICAL OFFSETS '
	Write(600,10) '      /h_offset h_beg sec_tick1 CalcOffset def'
	Write(600,10) '      /v_offset v_beg sec_tick2 CalcOffset def'
	Write(600,10) ''
	Write(600,10) '      % DRAW THE HORIZONTAL TICK MARKS'
	Write(600,10) '      /val_beg h_beg h_offset add def'
	Write(600,10) '      /val_end h_end def'
	Write(600,10) '      /inc sec_tick1 def'
	Write(600,10) '      /y1 v_beg def'
	Write(600,10) '      /y2 v_end def'
	Write(600,10) '      val_beg inc val_end {'
	Write(600,10) '         cvi/x ed % CONVERT TO INT TO USE '//
     &  'MOD FUNCTION'
	Write(600,10) '         % DRAW THE PRIMARY TICK MARKS'
	Write(600,10) '         x pri_tick1 mod 0 eq {'
	Write(600,10) '            x hkm y1 vkm m 0 7 rl s'
	Write(600,10) '            x hkm y2 vkm m 0 -7 rl s'
	Write(600,10) '         } if'
	Write(600,10) '         grid_flag BOTH eq {'
	Write(600,10) '            x hkm y1 vkm m 0 4 rl s'
	Write(600,10) '            x hkm y2 vkm m 0 -4 rl s'
	Write(600,10) '         } if'
	Write(600,10) '      } for'
	Write(600,10) ''
	Write(600,10) '      % LABEL THE PRIMARY HORIZONTAL TICK '//
     &  'MARKS'
	Write(600,10) '      h_beg cvi pri_tick1 mod 0 ne {'
	Write(600,10) '         /h_offset h_beg pri_tick1 '//
     &  'CalcOffset def'
	Write(600,10) '      } {'
	Write(600,10) '         /h_offset 0 def'
	Write(600,10) '      } ifelse'
	Write(600,10) '      /val_beg h_beg h_offset add def'
	Write(600,10) '      /val_end h_end def'
	Write(600,10) '      /inc pri_tick1 def'
	Write(600,10) '      /y v_beg def'
	Write(600,10) '      /xstr 4 string def'
	Write(600,10) '      val_beg inc val_end {'
	Write(600,10) '         cvi/x ed'
	Write(600,10) '         x hkm y vkm sp_text sub 0 x xstr '//
     &  'cvs 2 3 '
	Write(600,10) '         DrawText '
	Write(600,10) '      } for'
	Write(600,10) '      % DRAW THE VERTICAL TICK MARKS'
	Write(600,10) '      /val_beg v_beg v_offset add def'
	Write(600,10) '      /val_end v_end def'
	Write(600,10) '      /inc sec_tick2 def'
	Write(600,10) '      /x1 h_beg def'
	Write(600,10) '      /x2 h_end def'
	Write(600,10) '      val_beg inc val_end inc sub {'
	Write(600,10) '         cvi/y ed % CONVERT TO INT TO USE '//
     &  'MOD FUNCTION'
	Write(600,10) '         % Draw the primary tick marks'
	Write(600,10) '         y pri_tick2 mod 0 eq {'
	Write(600,10) '            x1 hkm y vkm m 7 0 rl s'
	Write(600,10) '            x2 hkm y vkm m -7 0 rl s'
	Write(600,10) '         } if'
	Write(600,10) '         grid_flag BOTH eq {'
	Write(600,10) '            x1 hkm y vkm m 4 0 rl s'
	Write(600,10) '            x2 hkm y vkm m -4 0 rl s'
	Write(600,10) '         } if'
	Write(600,10) '      } for'
	Write(600,10) ''
	Write(600,10) '      % LABEL THE PRIMARY VERTICAL TICK MARKS'
	Write(600,10) '      v_beg cvi pri_tick2 mod 0 ne {'
	Write(600,10) '         /v_offset v_beg pri_tick2 '//
     &  'CalcOffset def'
	Write(600,10) '      } {'
	Write(600,10) '         /v_offset 0 def'
	Write(600,10) '      } ifelse'
	Write(600,10) '      /val_beg v_beg v_offset add def'
	Write(600,10) '      /val_end v_end def'
	Write(600,10) '      /inc pri_tick2 def'
	Write(600,10) '      /x h_beg def'
	Write(600,10) '      /ystr 4 string def'
	Write(600,10) '      val_beg inc val_end {'
	Write(600,10) '         cvi/y ed'
	Write(600,10) '         x hkm sp_text sub y vkm 0 y '//
     &  'ystr cvs 3 2 DrawText '
	Write(600,10) '      } for'
	Write(600,10) ''
	Write(600,10) '   } def'
	Write(600,10) '   /CalcOffset {'
	Write(600,10) '   '
	Write(600,10) '      %--------------------'
	Write(600,10) '      % CALCULATE THE OFFSET TO POSITION THE'
	Write(600,10) '      % FIRST TICK MARK'
	Write(600,10) '      % BEG=BEGIN COORDINATE (IN KM)'
	Write(600,10) '      % TICK=TICK INCREMENT (IN KM)'
	Write(600,10) '      %--------------------'
	Write(600,10) '   '
	Write(600,10) '      /tick ed /beg ed'
	Write(600,10) '   '
	Write(600,10) '      % FIND THE CLOSEST VALUE OF THE TICK MARK'
	Write(600,10) '      /inc'
	Write(600,10) '         beg truncate 1 add cvi def'
	Write(600,10) '   '
	Write(600,10) '      /i 1 def'
	Write(600,10) '      { inc i 1 sub add tick mod 0 ne'
	Write(600,10) '         {/i i 1 add def}'
	Write(600,10) '         {exit} ifelse'
	Write(600,10) '      } loop'
	Write(600,10) '   '
	Write(600,10) '      % CALCULATE THE OFFSET'
	Write(600,10) '      /offset  inc i 1 sub add beg sub def'
	Write(600,10) '   '
	Write(600,10) '      % RETURN THE OFFSET'
	Write(600,10) '      offset'
	Write(600,10) '   '
	Write(600,10) '   } def'
	Write(600,10) '   /DrawDistScale {'
	Write(600,10) ''
	Write(600,10) '      %--------------------'
	Write(600,10) '      % DRAW THE DISTANCE SCALE FOR POLAR PLOTS'
	Write(600,10) '      %--------------------'
	Write(600,10) ''
*	Write(600,10) '      num_font/Helvetica Font'
	Write(600,10) '      num_font/'//font_name(1:i1-1)//' Font'
	Write(600,10) '      /strh num_ht def'
	Write(600,10) '      /dist h_end 2 div truncate def'
	Write(600,10) '      /x h_end hkm neg def'
	Write(600,10) '      /y h_end hkm neg def'
	Write(600,10) '      /scalestr 4 string def'
	Write(600,10) '      gs'
	Write(600,10) '         x y tr'
	Write(600,10) '         0 0 m dist hkm 0 rl s'
	Write(600,10) '         0 dist 2 div dist {'
	Write(600,10) '            cvi/i ed'
	Write(600,10) '            i hkm 0 m 0 8 rl s'
	Write(600,10) '            i hkm sp_text neg 0 i '//
     &  'scalestr cvs 2 3 DrawText'
	Write(600,10) '         } for'
	Write(600,10) '            i hkm 15 add sp_text neg 0 '//
     &  '(km) 4 3 DrawText'
	Write(600,10) '      gr'
	Write(600,10) '      '
	Write(600,10) '   } def'
	Write(600,10) 'end % TickDict'
		
10	Format(A)

	Return
	End
**************************************************
	Subroutine ScaleDict

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer i1

	i1 = index(font_name, ' ')

	Write(600,10) 'ScaleDict begin'
	Write(600,10) ''
	Write(600,10) '   /DrawScale {'
	Write(600,10) '   '
	Write(600,10) '      % DRAW & FILL THE SCALE'
	Write(600,10) '   '
	Write(600,10) '      % NUMBER OF INCREMENTS'
	Write(600,10) '      /num_inc scale_end scale_beg sub '//
     &  'scale_inc div cvi def'
	Write(600,10) '   '
	Write(600,10) '      % CALCULATE THE POSITION OF THE '//
     &  'SCALE BASED ON'
	Write(600,10) '      % THE TEMPLATE AND SCALE POSITION'
	Write(600,10) '      CalcScalePos'
	Write(600,10) '      /ypos ed /xpos ed'
	Write(600,10) '   '
	Write(600,10) '      % FINALLY, DRAW THE SCALE!'
	Write(600,10) '      /scalestr 4 string def'
	Write(600,10) '   '
*	Write(600,10) '      num_font/Helvetica Font'
	Write(600,10) '      num_font/'//font_name(1:i1)//' Font'
	Write(600,10) '      /strh num_ht def'
	Write(600,10) '      scale_pos VER eq {'
	Write(600,10) '         0 1 num_inc 1 sub {'
	Write(600,10) '            /i ed'
	Write(600,10) '            /x xpos def'
	Write(600,10) '            /y scale_h num_inc div i mul '//
     &  'ypos add def'
	Write(600,10) '            x y m'
	Write(600,10) '            scale_w 0 rl'
	Write(600,10) '            0 scale_h num_inc div rl'
	Write(600,10) '            scale_w neg 0 rl'
	Write(600,10) '            cp'
	Write(600,10) '            1 1 2 {'
	Write(600,10) '               % STROKE & FILL'
	Write(600,10) '               /j ed'
	Write(600,10) '               j 1 eq {gs i 2 add o fill gr} '//
     &  '{s} ifelse'
	Write(600,10) '            } for'
	Write(600,10) '         } for'
	Write(600,10) ''
	Write(600,10) '         % LABEL THE SCALE'
	Write(600,10) '         1 1 num_inc 1 sub  {'
	Write(600,10) '            /i ed'
	Write(600,10) '            /val scale_beg scale_inc i mul '//
     &  'add def'
	Write(600,10) '            /valint val cvi def'
	Write(600,10) '            val valint sub 0.0 eq {'
	Write(600,10) '               /val val cvi def'
	Write(600,10) '            } if'
	Write(600,10) '            /y scale_h num_inc div i mul '//
     &  'ypos add def'
	Write(600,10) '            xpos scale_w add sp_text add '//
     &  'y 0 val scalestr cvs 1 2 DrawText'
	Write(600,10) '         } for'
	Write(600,10) ''
	Write(600,10) '         % SCALE TITLE'
*	Write(600,10) '         label_font/Helvetica Font'
	Write(600,10) '         label_font/'//font_name(1:i1-1)//' Font'
	Write(600,10) '	 /strh label_ht def'
	Write(600,10) '         xpos scale_w 2 div add ypos '//
     &  'scale_h add sp_text add 0'
	Write(600,10) '         scale_title 2 1 DrawText'
	Write(600,10) '      } { % SCALE_POS=HOR'
	Write(600,10) '         0 1 num_inc 1 sub {'
	Write(600,10) '            /i ed'
	Write(600,10) '            /x scale_w num_inc div i mul '//
     &  'xpos add def'
	Write(600,10) '            /y ypos def'
	Write(600,10) '            x y m'
	Write(600,10) '            scale_w num_inc div 0 rl'
	Write(600,10) '            0 scale_h neg rl'
	Write(600,10) '            scale_w num_inc div neg 0 rl'
	Write(600,10) '            cp'
	Write(600,10) '            1 1 2 {'
	Write(600,10) '               % STROKE & FILL'
	Write(600,10) '               /j ed'
	Write(600,10) '               j 1 eq {gs i 2 add o fill gr} '//
     &  '{s} ifelse'
	Write(600,10) '            } for'
	Write(600,10) '         } for'
	Write(600,10) '         % LABEL THE SCALE'
	Write(600,10) '         /y ypos scale_h sub sp_text sub def'
	Write(600,10) '         1 1 num_inc 1 sub  {'
	Write(600,10) '            /i ed'
	Write(600,10) '            /val scale_beg scale_inc i mul '//
     &  'add def'
	Write(600,10) '            /valint val cvi def'
	Write(600,10) '            val valint sub 0.0 eq {'
	Write(600,10) '               /val val cvi def'
	Write(600,10) '            } if'
	Write(600,10) '            /x scale_w num_inc div i mul '//
     &  'xpos add def'
	Write(600,10) '            x y 0 val scalestr cvs 2 3 DrawText'
	Write(600,10) '         } for'
	Write(600,10) '         % SCALE TITLE'
*	Write(600,10) '         label_font/Helvetica Font'
	Write(600,10) '         label_font/'//font_name(1:i1-1)//' Font'
	Write(600,10) '	 /strh label_ht def'
	Write(600,10) '         xpos scale_w add sp_text add ypos '//
     &  'scale_h 2 div sub 0'
	Write(600,10) '         scale_title 1 2 DrawText'
	Write(600,10) '      } ifelse'
	Write(600,10) '   '
	Write(600,10) '   } def'
	Write(600,10) '   /GetMaxBoxW {'
	Write(600,10) ''
	Write(600,10) '      template 5 le template 11 eq or { '
	Write(600,10) '	 /indx 0 def'
	Write(600,10) '         /mw BOX_W indx get def'
	Write(600,10) '         1 1 num_panel 1 sub {'
	Write(600,10) '	    /i ed'
	Write(600,10) '	    BOX_W i get mw gt {'
	Write(600,10) '	       /mw BOX_W i get def'
	Write(600,10) '	       /indx i def'
	Write(600,10) '	    } if'
	Write(600,10) '         } for'
	Write(600,10) '      } {'
	Write(600,10) '	 /indx 1 def'
	Write(600,10) '         /mw BOX_W 0 get BOX_W 1 get add def'
	Write(600,10) '         1 1 num_panel 1 sub {'
	Write(600,10) '	    /i ed'
	Write(600,10) '	    BOX_W i get BOX_W i 1 sub get '//
     &  'add mw gt {'
	Write(600,10) '	       /mw BOX_W i get BOX_W i 1 '//
     &  'sub get add def'
	Write(600,10) '	       /indx i def'
	Write(600,10) '	    } if'
	Write(600,10) '         } for'
	Write(600,10) '      } ifelse'
	Write(600,10) ''
	Write(600,10) '   } def'
	Write(600,10) '   /CalcScalePos {'
	Write(600,10) '   '
	Write(600,10) '      % CALCULATE POSITION OF SCALE'
	Write(600,10) '      % xp=HORIZONTAL POSITION'
	Write(600,10) '      % yp=VERTICAL POSITION'
	Write(600,10) '      GetMaxBoxW'
	Write(600,10) '      scale_pos VER eq {'
	Write(600,10) '	 /xp XPOS indx get BOX_W indx get add '
	Write(600,10) '	 sp_scale add def'
c --% sjs add 12/29/97		
	Write(600,10) '	 template 14 ge {'
	Write(600,10) '	    /xp page_w r_marg sub' 
	Write(600,10) '	    TextDict begin scale_end NumWidth'//
     &  'scale_beg NumWidth end' 
	Write(600,10) '	    Largest sub sp_text sub scale_w' 
	Write(600,10) '               sub def'
	Write(600,10) '	 } if'
c --% sjs end add 12/29/97		
	Write(600,10) '	 /yp1 YPOS num_panel 1 sub get def'
	Write(600,10) '	 template 5 le template 11 eq or {'
	Write(600,10) '	    /yp2 YPOS 0 get BOX_H 0 get add def'
	Write(600,10) '	 } {'
	Write(600,10) '	    /yp2 YPOS 1 get BOX_H 1 get add def'
	Write(600,10) '	 } ifelse'
	Write(600,10) '	 /midpoint yp2 yp1 sub 2 div yp1 '//
     &  'add def'
	Write(600,10) '         /yp midpoint scale_h 2 div sub def'
	Write(600,10) '      } { % SCALE_POS==HOR'
	Write(600,10) '         template 5 le template 11 eq or {'
	Write(600,10) '            % HORIZONTAL MIDPOINT OF SCALE'
	Write(600,10) '	    /midpoint XPOS panel 1 sub get '//
     &  'BOX_W panel 1 sub get'
	Write(600,10) '	    2 div add def '
	Write(600,10) '         } {'
	Write(600,10) '            template 11 ne {'
	Write(600,10) '               % HORIZONTAL MIDPOINT OF SCALE'
	Write(600,10) '	       /midpoint XPOS panel 1 sub '//
     &  'get sp_col 2 div sub def'
	Write(600,10) '	       grid CART eq {'
	Write(600,10) '		  /midpoint midpoint sp_text '//
     &  '2 mul sub label_ht sub'
	Write(600,10) '		  nw sub def'
	Write(600,10) '	       } if'
	Write(600,10) '            } if'
c --%** sjs add 12/29/97
	Write(600,10) '            template 14 ge {'
	Write(600,10) '	       /midpoint XPOS panel 2 sub get' 
	Write(600,10) '	       BOX_W panel 2 sub get 2 div add def'
	Write(600,10) '	       grid CART eq {'
	Write(600,10) '		  /midpoint midpoint sp_text 2'//
     &  ' mul sub label_ht sub'
	Write(600,10) '		  nw sub def'
	Write(600,10) '	       } if'
	Write(600,10) '            } if'
c --%** sjs end add 12/29/97

	Write(600,10) '         } ifelse'
	Write(600,10) '         /yp '
	Write(600,10) '            YPOS num_panel 1 sub get sp_text '//
     &  '2 mul sub'
	Write(600,10) '            num_ht sub label_ht sub sp_scale '//
     &  'sub def '
	Write(600,10) '         /xp midpoint scale_w 2 div sub def'
	Write(600,10) '      } ifelse'
	Write(600,10) '   '
	Write(600,10) '      % RETURN HORIZONTAL & VERTICAL '//
     &  'POSITIONS OF THE SCALE'
	Write(600,10) '      xp yp '
	Write(600,10) ''
	Write(600,10) '   } def'
	Write(600,10) 'end % ScaleDict'
	
10	Format(A)

	Return
	End
**************************************************
	Subroutine PolarDict

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Write(600,10) 'PolarDict begin'
	Write(600,10) '   /Polar {'
	Write(600,10)
	Write(600,10) '      %--------------------'
	Write(600,10) '      % CONTOUR LINES FOR POLAR COORDINATES'
	Write(600,10) '      %--------------------'
	Write(600,10)
	Write(600,10) '      /theta exch def /radius exch def'
	Write(600,10)
	Write(600,10) '      /x {radius theta cos mul} def'
	Write(600,10) '      /y {radius theta sin mul} def'
	Write(600,10) '      x hkm y vkm'
	Write(600,10)
	Write(600,10) '   } def'
	Write(600,10) '   /DrawArc {'
	Write(600,10)
	Write(600,10) '      %--------------------'
	Write(600,10) '      % BOUNDARY IN POLAR COORDINATES'
	Write(600,10) '      %--------------------'
	Write(600,10)
	Write(600,10) '      /theta2 exch def /theta1 exch def'
	Write(600,10) '      /radius exch def'
	Write(600,10)
	Write(600,10) '      0 0 radius hkm theta1 theta2'
	Write(600,10) '      theta1 theta2 gt {arcn} {arc} ifelse'
	Write(600,10)
	Write(600,10) '   } def'
	Write(600,10) 'end % PolarDict'

10	Format(A)

	Return
	End
**************************************************
	Subroutine TrackDict

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Write(600,10) 'TrackDict begin'
	Write(600,10) '   /Plane {'
	Write(600,10)
	Write(600,10) '      % DRAW AN AIRPLANE'
	Write(600,10) '      /i {10 mul} def'
	Write(600,10) '      /angle exch def'
	Write(600,10) '      /y exch def'
	Write(600,10) '      /x exch def'
	Write(600,10) '      scan plane_inc mod 0 eq { '
	Write(600,10) '         gs newpath'
	Write(600,10) '         x hkm y vkm tr'
	Write(600,10) '         angle 180 add rotate'
	Write(600,10) '         .125 i .125 i m'
	Write(600,10) '         .125 i .0625 i .0625 i 180 90 arcn'
	Write(600,10) '         .125 i .125 i m'
	Write(600,10) '         .1875 i .0625 i rl'
	Write(600,10) '         .50 i 0 rl'
	Write(600,10) '         .1875 i .5 i rl'
	Write(600,10) '         .125 i 0 rl'
	Write(600,10) '         .125 i neg .50 i neg rl'
	Write(600,10) '         .375 i 0 rl'
	Write(600,10) '         .0625 i .125 i rl'
	Write(600,10) '         .0625 i 0 rl'
	Write(600,10) '         .0625 i neg .25 i neg rl'
	Write(600,10) '         .0625 i .25 i neg rl'
	Write(600,10) '         .0625 i neg 0 rl'
	Write(600,10) '         .0625 i neg .125 i rl'
	Write(600,10) '         .375 i neg 0 rl'
	Write(600,10) '         .125 i .50 i neg rl'
	Write(600,10) '         .125 i neg 0 rl'
	Write(600,10) '         .1875 i neg .5 i rl'
	Write(600,10) '         .50 i neg 0 rl'
	Write(600,10) '         .1875 i neg .0625 i rl'
	Write(600,10) '         .125 i .0625 i .0625 i 270 180 arcn'
	Write(600,10) '         cp fill gr '
	Write(600,10) '         /scan scan 1 add def'
	Write(600,10) '      } {'
	Write(600,10) '         /scan scan 1 add def'
	Write(600,10) '      } ifelse'
	Write(600,10)
	Write(600,10) '   } def'
*	Call FlightTrack
	Write(600,10) 'end % TrackDict'

10	Format(A)

	Return
	End
**************************************************
	Subroutine MyFunctDict 

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer i1

	i1 = index(font_name, ' ')

	Write(600,10) 'MyFunction begin'
	Write(600,10) '   /DrawSquare {'
	Write(600,10) '      /h ed /w ed /y ed /x ed'
	Write(600,10) ''
	Write(600,10) '      % x = horizontal begin of square '
	Write(600,10) '      % y = vertical begin of square'
	Write(600,10) '      % w = width of square'
	Write(600,10) '      % h = height of square'
	Write(600,10) ''
	Write(600,10) '      x y m'
	Write(600,10) '      w 0 rl'
	Write(600,10) '      0 h rl'
	Write(600,10) '      w neg 0 rl'
	Write(600,10) '      cp s'
	Write(600,10) ''
	Write(600,10) '   } def'
	Write(600,10) '   /DrawCircle {'
	Write(600,10) ''
	Write(600,10) '      /rad ed /y ed /x ed'
	Write(600,10) ''
	Write(600,10) '      % x: x origin of circle'
	Write(600,10) '      % y: y origin of circle'
	Write(600,10) '      % rad: radius of circle'
	Write(600,10) ''
	Write(600,10) '      newpath x y rad 0 360 arc stroke newpath'
	Write(600,10) ''
	Write(600,10) '   } def'
	Write(600,10) '   /DrawArrow {'
	Write(600,10) ''
	Write(600,10) '      /vec_len ed /angle ed /y ed /x ed'
	Write(600,10) ''
	Write(600,10) '      % x: horizontal position of tail of arrow'
	Write(600,10) '      % y: vertical position of tail of arrow'
	Write(600,10) '      % ang: direction of arrow'
	Write(600,10) '      % len: length of arrow'
	Write(600,10) ''
	Write(600,10) '      /head_h vec_len pct_hh mul def'
	Write(600,10) '      /head_w vec_len pct_hw mul def'
	Write(600,10) '      /vec_w vec_len pct_vw mul def'
	Write(600,10) '      /base vec_len head_h sub def'
	Write(600,10) '      % DRAW THE VECTOR'
	Write(600,10) ''
	Write(600,10) '      gs newpath'
	Write(600,10) '      x y translate angle rotate'
	Write(600,10) '      0 vec_w 2 div neg m  '
	Write(600,10) '      0 vec_w rl'
	Write(600,10) '      base 0 rl'
	Write(600,10) '      0 head_w 2 div vec_w 2 div sub rl'
	Write(600,10) '      head_h head_w 2 div neg rl'
	Write(600,10) '      head_h neg head_w 2 div neg rl'
	Write(600,10) '      0 head_w 2 div vec_w 2 div sub rl'
	Write(600,10) '      cp fill gr'
	Write(600,10) ''
	Write(600,10) '   } def'
	Write(600,10) ''
	Write(600,10) '   /typhoon{'
	Write(600,10) '      gs'
	Write(600,10) '      /size ed /y ed /x ed'
	Write(600,10) '      x y translate size size scale'
	Write(600,10) '      0 setgray'
	Write(600,10) '      0 0 0.2 inch 0 360 arc gs fill gr stroke'
	Write(600,10) '      0.2 inch 0 inch 0.4 inch 95 180 arc' 
	Write(600,10) '      0.27 inch 0 inch 0.4 inch 180 107 arcn gs fill'
	Write(600,10) '        gr stroke'
	Write(600,10) '      -0.2 inch 0 inch 0.4 inch 275 360 arc' 
	Write(600,10) '      -0.27 inch 0 inch 0.4 inch 360 287 arcn gs fill'
	Write(600,10) '       gr stroke'
	Write(600,10) '      gr} def'
	Write(600,10) ''
	Write(600,10) '   /Radar {'
	Write(600,10) '    %xscale, yscale: scale factors in x and y'
	Write(600,10) '    %lon, lat: x and y positions'
	Write(600,10) '    %angle: Orientation of the radar'
	Write(600,10) ''
	Write(600,10) ''
	Write(600,10) '    /yscale ed /xscale ed /lat ed /lon ed'
	Write(600,10) '    /rad 7 def'
	Write(600,10) '    % DRAW THE SYMBOL'
	Write(600,10) '    gs'
	Write(600,10) '    lon lat tr'
	Write(600,10) '    xscale yscale scale'
	Write(600,10) '    % Draw the base'
	Write(600,10) '    gs'
	Write(600,10) '    0 0 m'
	Write(600,10) '    0 rad .50 mul neg rl'
	Write(600,10) '    0 0 rmoveto'
	Write(600,10) '    rad .25 mul neg 0 rl'
	Write(600,10) '    rad .25 mul neg rad 1.25 mul neg rl'
	Write(600,10) '    rad 0 rl'
	Write(600,10) '    rad .25 mul neg rad 1.25 mul rl'
	Write(600,10) '    closepath fill'
	Write(600,10) '    gr '
	Write(600,10) '    % Draw the bell'
	Write(600,10) '    10 rotate'
	Write(600,10) '    0 0 m'
	Write(600,10) '    rad .25 mul 0 rl s'
	Write(600,10) '    rad 1.25 mul 0 translate'
	Write(600,10) '    0 0 rad 120 240 arc'
	Write(600,10) '    /rad2 7 sqrt rad mul def'
	Write(600,10) '    rad 2 mul 0 rad2 198.125 161.875 arcn fill'
	Write(600,10) '   gr'
	Write(600,10) ' } def'
	Write(600,10) ''
	Write(600,10) ''
	Write(600,10) '   /Annotate {'
	Write(600,10) ''
	Write(600,10) '      /angle ed /white ed /fntsize ed /flag ed'// 
     &  ' /txt ed /y ed /x ed'
	Write(600,10) ''
	Write(600,10) '      % x: horizontal position of text'
	Write(600,10) '      % y: vertical position of text'
	Write(600,10) '      % txt: text'
	Write(600,10) '      % flag: 1=left justified; '//
     &  '2=right justified; 3=centered '
	Write(600,10) '      % fntsize: size of font'
	Write(600,10) '      % white: space between text'//
     &  'surroundings.  Negative number means no white out.'
	Write(600,10) '      % angle: Orientation of the text'
	Write(600,10) ''
*	Write(600,10) '      fntsize/Helvetica Font'
*	Write(600,10) '      fntsize/'//font_name(1:i1)//' Font'
	Write(600,10) ' '
	Write(600,10) '      /strw txt stringwidth pop def'
	Write(600,10) '      flag 2 eq {/x x strw sub def} if'
	Write(600,10) '      flag 3 eq {/x x strw 2 div sub def} if'
	Write(600,10) ''
	Write(600,10) '      gs'
	Write(600,10) '         x y translate angle rotate'
	Write(600,10) '         white 0 ge {'
	Write(600,10) '           white neg white neg m'
	Write(600,10) '           strw 2 white mul add 0 rl'
	Write(600,10) '           0 fntsize TextDict begin TextHt '//
     &  ' end 2 white mul add rl'
	Write(600,10) '           strw 2 white mul add neg 0 rl'
	Write(600,10) '           cp gs 1 setgray fill gr } if'
	Write(600,10) ''
	Write(600,10) '         0 0 m txt show'
	Write(600,10) '      gr'
	Write(600,10) '   } def'
	Write(600,10) 'end % MyFunction'

10	Format(A)

	Return
	End
**************************************************
	Subroutine FlightTrack

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer scan_num,i
	Integer hr, min, sec, day, mon, yr
	Integer DecodeMon
	Character*3 month
        Character*4  temp
	Real lat,lon,head
	Real origx,origy,meanrad,phi,theta
	Real latscale,lonscale,r0,deg2rad
	Real x,y,x1,y1,dtheta,angle
	Real time1, time2, time


 	Open (unit=50,file=catalog_name,status='old')

C CALCULATE THE SCALE FACTOR 
	deg2rad=3.1415926 / 180.
        origx=real(long_deg+(long_min/60.)+(long_sec/3600.))
	origy=real(lat_deg+(lat_min/60.)+(lat_sec/3600.))
	meanrad=6356.766
	phi=1.
	theta=0.
	latscale=meanrad*sin(phi*deg2rad)*cos(theta*deg2rad)
	r0=meanrad*cos(origy*deg2rad)
	lonscale=2*r0*sin(.5*deg2rad)

C TIME INCREMENTS OF DATA
	time1 = year_beg + (float(month_beg)/12.0) +
     &  (float(day_beg)/365.0) + hour_beg + 
     &  (float(min_beg)/60.0) + (float(sec_beg)/3600.0)
	time2 = year_end + (float(month_end)/12.0) +
     &  (float(day_end)/365.0) + hour_end + 
     &  (float(min_end)/60.0) + (float(sec_end)/3600.0)

C READ THE LAT, LON & HEADING FROM THE CATALOG FILE
	scan_num=0
*	Write(600,10) '   /DrawPlane {'
*	Write(600,10) '      %--------------------'
*	Write(600,10) '      % FLIGHT TRACK'
*	Write(600,10) '      % PLANE_INC=INCREMENT OF PLANE'
*	Write(600,10) '      %--------------------'
	Write(600,10) '      TrackDict begin'   
	Write(600,10) '      /scan 0 def'
100	Read (50,*,END=200), temp
	IF (temp.EQ.'SCAN') THEN
           scan_num=scan_num+1 
	   Read(50,*)
	   Read(50,15), day, month, yr
	   Read(50,16), hr, min,sec
	   mon = DecodeMon(month)
	   time = yr-1900 + (float(mon)/12.0) +
     &     (float(day)/365.0) + hr + 
     &     (float(min)/60.0) + (float(sec)/3600.0)

 	   Read(50,*)  !Start_angle
	   Read(50,*)  !Unambiguous velocity
	   Read(50,11),lat  !Start_latitude
	   Read(50,12),lon  !Start_longitude
 	   Read(50,*)  !Start_altitude_(km)
 	   Read(50,*)  !Start_altitude_agl(km)
	   Read(50,13),head  !Start_heading_(deg)
C 	   IF (mod(scan_num,plane_inc).EQ.0) THEN
	   IF (time.GE.time1.AND.time.LE.time2) THEN 
	      print*, 'time: ', time, time1, time2, hr, min, sec
	      x=(lon-origx)*lonscale
      	      y=(lat-origy)*latscale
              dtheta=90. - float(clockwise_deg)/64. 
	      x1 = x*cos(dtheta*deg2rad) + y*sin(dtheta*deg2rad)
	      y1 = -x*sin(dtheta*deg2rad) + y*cos(dtheta*deg2rad)
	      angle = mod((450.-head),360.)-dtheta
	      Write(600,14) x1,y1,angle
	   ENDIF
C	   ENDIF
	ENDIF
	goto 100 !Read next line
200	Write(600,10) '      end % TrackDict'
*200	Write(600,10) '   } def'
	Close(50)
	
10	Format(A)
11	Format(15X,F8.4)
12	Format(16X,F8.4)
13	Format(20X,F5.1)
14	Format(3X,F10.4,1X,F10.4,1X,F10.4,1X,'Plane')
15	Format(11X,I2,1X,A3,1X,I4)
16	Format(11X,I2,1X,I2,1X,I2)

	Return
	End
**************************************************
	Integer Function DecodeMon(month)

	Character*3 month

	IF (month.EQ.'JAN') THEN
	   DecodeMon = 1
	ELSEIF (month.EQ.'FEB') THEN
	   DecodeMon = 2
	ELSEIF (month.EQ.'MAR') THEN
	   DecodeMon = 3
	ELSEIF (month.EQ.'APR') THEN
	   DecodeMon = 4
	ELSEIF (month.EQ.'MAY') THEN
	   DecodeMon = 5
	ELSEIF (month.EQ.'JUN') THEN
	   DecodeMon = 6
	ELSEIF (month.EQ.'JUL') THEN
	   DecodeMon = 7
	ELSEIF (month.EQ.'AUG') THEN
	   DecodeMon = 8
	ELSEIF (month.EQ.'SEP') THEN
	   DecodeMon = 9
	ELSEIF (month.EQ.'OCT') THEN
	   DecodeMon = 10
	ELSEIF (month.EQ.'NOV') THEN
	   DecodeMon = 11
	ELSEIF (month.EQ.'DEC') THEN
	   DecodeMon = 12
	ENDIF

	Return
	End
**************************************************
	Subroutine VectorDict


	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Write(600,10) 'VectorDict begin'
	Write(600,10) '   /Vector {'
	Write(600,10) '   '
	Write(600,10) '      %--------------------'
	Write(600,10) '      % DRAW A VECTOR'
	Write(600,10) '      % X=HORIZONTAL POSITION (IN KM)'
	Write(600,10) '      % Y=VERTICAL POSITION (IN KM)'
	Write(600,10) '      % U=HORIZONTAL WIND SPEED (IN M/S)'
	Write(600,10) '      % V=VERTICAL WIND SPEED (IN M/S)'
	Write(600,10) '      % SCALE=SCALE FACTOR FOR VECTOR'
	Write(600,10) '      %--------------------'
	Write(600,10) '      /scale ed '
	Write(600,10) '      v_stretch mul/v ed  h_stretch mul/u ed'
	Write(600,10) '      /y ed  /x ed'
	Write(600,10) '   '
	Write(600,10) '      % POSITION OF TIP & TAIL OF VECTOR'
	Write(600,10) '      /tailx x def'
	Write(600,10) '      /taily y def'
	Write(600,10) '      /tipx u scale mul def'
	Write(600,10) '      /tipy v scale mul def'
	Write(600,10) '   '
	Write(600,10) '      % LENGTH OF VECTOR'
	Write(600,10) '      /vec_len tipx tipx mul tipy tipy '//
     &  'mul add sqrt hkm def'
	Write(600,10) '   '
	Write(600,10) '      % HEIGHT OF HEAD'
	Write(600,10) '      pct_hh 0 lt {'
	Write(600,10) '         /head_h pct_hh abs def'
	Write(600,10) '      } {'
	Write(600,10) '         /head_h vec_len pct_hh mul def'
	Write(600,10) '      } ifelse'
	Write(600,10) '   '
	Write(600,10) '      % WIDTH OF HEAD'
	Write(600,10) '      pct_hw 0 lt {'
	Write(600,10) '         /head_w pct_hw abs def'
	Write(600,10) '      } {'
	Write(600,10) '         /head_w vec_len pct_hw mul def'
	Write(600,10) '      } ifelse'
	Write(600,10) ''
	Write(600,10) '      % WIDTH OF STEM'
	Write(600,10) '      pct_vw 0 lt {'
	Write(600,10) '         /vec_w pct_vw abs def '
	Write(600,10) '      } {'
	Write(600,10) '         /vec_w vec_len pct_vw mul def '
	Write(600,10) '      } ifelse'
        Write(600,10)
	Write(600,10) '      u 0 eq v 0 eq and'
	Write(600,10) '         {/angle 0 def}'
	Write(600,10) '         {/angle tipy tipx atan def} ifelse'
	Write(600,10) '   '
	Write(600,10) '      % LENGTH OF STEM'
	Write(600,10) '      /base vec_len head_h sub def'
	Write(600,10) '   '
	Write(600,10) '      % DRAW THE VECTOR'
	Write(600,10) '      gs newpath'
	Write(600,10) '      tailx taily translate angle rotate'
	Write(600,10) '      0 vec_w 2 div neg m  '
	Write(600,10) '      0 vec_w rl'
	Write(600,10) '      base 0 rl'
	Write(600,10) '      0 head_w 2 div vec_w 2 div sub rl'
	Write(600,10) '      head_h head_w 2 div neg rl'
	Write(600,10) '      head_h neg head_w 2 div neg rl'
	Write(600,10) '      0 head_w 2 div vec_w 2 div sub rl'
	Write(600,10) '      cp fill gr'
	Write(600,10) '   '
	Write(600,10) '   }  def'
	Write(600,10) ''
	Write(600,10) 'end % VectorDict'

10	Format(A)

	Return
	End
**************************************************
	Subroutine PSMain(panel)

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer panel
	Character*2 num

	Data vec1,vec2,cont1,cont2/1,2,3,4/
	Data hkm,vkm /'hkm','vkm'/

C Convert panel number into a string
	Call CalcNum(panel,num)

	IF (panel.EQ.1) THEN
	   Write(600,10) '%80 45 {180 mul cos exch 180 mul cos add 2 div} ' 
	   Write(600,10) '%setscreen'
	   Write(600,10) '%********************'
	   Write(600,10) '% MAIN PROGRAM'
	   Write(600,10) '%********************'
	   Write(600,10) 'page_h 27.94 cm gt {'
	   Write(600,10) '   0 page_h 27.94 cm sub neg translate'
	   Write(600,10) '} if'
	   Write(600,10) '% HEIGHT OF TEXT'
	   Write(600,10) 'TextDict begin'
	   Write(600,10) '   num_font TextHt'
	   Write(600,10) '   label_font TextHt'
	   Write(600,10) '   title_font TextHt'
	   Write(600,10) 'end % TextDict'
	   Write(600,10) '/title_ht ed'
	   Write(600,10) '/label_ht ed'
	   Write(600,10) '/num_ht ed'
	   Write(600,10) ''
	   Write(600,10) '% PAGE SETUP'
	   Write(600,10) 'DefineColorTable'
	   Write(600,10) 'PageDict begin'
	   Write(600,10) ''
	   Write(600,10) '   OrientPage'
	   Write(600,10) '   CalcNumPanel'
	   Write(600,10) '   CalcScaleDim'
	   Write(600,10) '   CalcMarg'
	   Write(600,10) '   CalcPanelSize'
	   Write(600,10) '  '
	   Write(600,10) 'end % PageDict'
	   Write(600,10) '% STORE THE VARIABLES IN USERDICT'
	   Write(600,10) '/panel_w ed /panel_h ed'
	   Write(600,10) '/r_marg ed /b_marg ed /t_marg ed'
	   Write(600,10) '/scale_w ed /scale_h ed'
	   Write(600,10) '/num_panel ed'
	   Write(600,10) '/page_w ed /page_h ed'
	ENDIF

C WRITE THE PANEL VARIABLES
 	Write(600,10) 'PanelDict begin'	
	Call WritePanelVar(panel, num)

*C TEST TO MAKE SURE SCALE_FLAG IS CORRECT
*	Write(600,10) '   % CHECK PLOT_TYPE AGAINST SCALE_FLAG'
*	Write(600,10) '   plot_type 1 gt {'
*	Write(600,10) '      userdict begin ON/scale_flag ed end'
*	Write(600,10) '   } if'
	Write(600,10) '   plot_type 1 gt {ON/draw_scale ed} if'

C FINALLY, WRITE OUT THE CODE!
        Write(200,*) 'calling WriteInitialSetUp'
	Write(200,*)
 	Call WriteInitialSetUp(num)

C CONTOUR LINES
	IF (plot_type.NE.1) THEN
           Write(200,*) 'calling WriteContour'
	   Write(200,*)
	   Call WriteContour(num)
 	ENDIF

C VECTORS
	IF (plot_type.EQ.1.OR.plot_type.eq.3.
     $      OR.plot_type.EQ.5) THEN
           Write(200,*) 'calling WriteVector'
	   Write(200,*)
	   Call WriteVector(num)
	ENDIF

C FLIGHT TRACK 
*	IF (fix_axis.EQ.'Z'.AND.scan_mode(1:3).EQ.'AIR') THEN
	IF (fix_axis.EQ.'Z'.OR.scan_mode(1:3).EQ.'AIR') THEN
           IF (catalog_name(1:4).NE.'NONE') THEN
              Write(200,*) 'calling WriteFlightTrack'
	      Write(200,*)
	      Call WriteFlightTrack(num)
 	   ENDIF
	ENDIF

C USER FUNCTIONS
        Write(200,*) 'calling WriteUserFunct'
	Write(200,*)
	Call WriteUserFunct(num)
       
C DRAW & LABEL BOUNDARY
        Write(200,*) 'calling WriteDrawBoundary'
	Write(200,*)
	Call WriteDrawBoundary(num)


C FINISH THE PLOT
        Write(200,*) 'calling WriteFinishPlot'
	Write(200,*)
	Call WriteFinishPlot

C END THE PANEL DICTIONARY
	Write(600,10) 'end % PanelDict'
	Write(600,*)

C CLEAN UP
	Write(600,*) '% CLEAR THE STACK'
	Write(600,*) 'ClearStack'
	Write(600,*) '% CLEAR THE DICTIONARY STACK'
	Write(600,*) 'ClearDictStack'
	Write(600,*) '% FINALLY, SHOWPAGE!!'
	Write(600,*) '/panel_num PanelDict begin panel end def'
	Write(600,*) 'panel_num num_panel eq {showpage} if'

10	Format(A)

	Return
	End
**************************************************
	Subroutine WritePanelVar(panel, num)

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer panel, i1
	Character*2 num
	Character*5 hlabel,vlabel
	Real dist

	Write(600,10) '   %******************'
	Write(600,10) '   % PANEL '//num
	Write(600,10) '   %******************'

C LOCAL POSTSCRIPT VARIABLES
	Write(600,11) panel,'/panel ed'
	Write(600,11) plot_type,'/plot_type ed'
	Write(600,11) pri_grid1,'/pri_tick1 ed'
	Write(600,11) sec_grid1,'/sec_tick1 ed'
	Write(600,11) pri_grid2,'/pri_tick2 ed'
	Write(600,11) sec_grid2,'/sec_tick2 ed'
C ASSIGN HORIZONTAL & VERTICAL LABELS
	Call CalcLabel(hlabel,vlabel)
	Write(600,12) hlabel,'/h_label ed'
	Write(600,12) vlabel,'/v_label ed'
C--modified by wcl 12/30/97: do not write out date and time if missing
        if(abs(year_beg) .gt. 100 .or. abs(month_beg) .gt. 12 .or.
     $                abs(day_beg) .gt. 31) then
          Write(600,*) '( )/date ed'
        else
          Write(600,13) year_beg,'/',month_beg,'/',day_beg,
     $                '/date ed'
        endif
        if(abs(hour_beg) .gt. 24 .or. abs(min_beg) .gt. 60 .or.
     $    abs(sec_beg) .gt. 60 .or. abs(hour_end) .gt. 24 .or. 
     $    abs(min_end) .gt. 60 .or. abs(sec_end) .gt. 60) then
          Write(600,*) '( )/time ed'
        else
          Write(600,14) hour_beg,':',min_beg,':',sec_beg,'-',
     $                hour_end,':',min_end,':',sec_end,
     $                '/time ed'
        endif
	dist=fix_beg+((level-1)*fix_inc)
	Write(600,15) fix_axis,'=',dist,'/level ed'
	i1 = index(panel_title, '  ')-1
	Write(600,16) panel_title(1:i1),'/panel_title ed'
	Write(600,17) vec_length,'/vec_dist ed'
	Write(600,17) vec_speed,'/vec_speed ed'
	Write(600,17) stretch_horiz,'/h_stretch ed'
	Write(600,17) stretch_vert,'/v_stretch ed'
	Write(600,17) horiz_beg,'/h_beg ed'
	Write(600,17) horiz_end,'/h_end ed'
	Write(600,17) vert_beg,'/v_beg ed'
	Write(600,17) vert_end,'/v_end ed'
	IF (box_w.NE.-999) THEN
	   Write(600,18) box_w,' cm/box_w ed'
	ELSE
	   Write(600,18) box_w,'/box_w ed'
	ENDIF
	IF (box_h.NE.-999) THEN
	   Write(600,18) box_h,' cm/box_h ed'
	ELSE
	   Write(600,18) box_h,'/box_h ed'
	ENDIF
        Write(600,11) plane_inc,'/plane_inc ed'
*	IF (fix_axis.EQ.'Z'.AND.scan_mode(1:3).EQ.'AIR') THEN
*	   IF (catalog_name(1:4).NE.'NONE') THEN
*              Write(600,11) plane_inc,'/plane_inc ed'
* 	   ENDIF
*	ENDIF

        IF (scan_mode.EQ.'POLA') THEN
           Write(600,10) '   POLAR/grid ed' 
	ELSE
           Write(600,10) '   CART/grid ed'
	ENDIF
	Write(600,10) '   '//grid_flag(1:4)//'/grid_flag ed' 
	Write(600,*)

10	Format(A)
11	Format(3X,I2,A)
12	Format(3X,'(',A,')',A)
13	Format(3X,'(',I2,A,I2,A,I2,')',A)
14	Format(3X,'(',I2,A,I2,A,I2,A,I2,A,I2,A,I2,')',A)
15	Format(3X,'(',A,A,F6.2,')',A)
16	Format(3X,'(',A,')',A)
17	Format(3X,F7.2,A)
18	Format(3X,F7.2,A)
19	Format(3X,A,1X,A,1X,A,1X,A)

	Return
	End
**************************************************
 	Subroutine WriteInitialSetUp(num)

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Character*2 num

	Write(600,10) '   % INITIAL SETUP OF PANEL'
	Write(600,10) '   SetUp'
	Write(600,10) '   % LOWER LEFT CORNER OF BOUNDARY'
	Write(600,10)'   /xpos'//num//' XPOS panel 1 sub get def'
	Write(600,10)'   /ypos'//num//' YPOS panel 1 sub get def'
	Write(600,*)

10	Format(A)

	Return
	End
**************************************************
	Subroutine WriteContour(num)

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer num_iter,i,j,cont_num,count, i1
	Character plot_scale 
	Character*2 num
	Real cont_beg,cont_inc,delta 
	Real rad2deg, pi, angle

	pi = 3.1415927
	rad2deg = 180/pi

	i1 = index(font_name, ' ')

C FIGURE OUT HOW MANY TIMES TO ITERATE THROUGH THE CONTOUR ROUTINE
	 fill_num=(fill_end-fill_beg)/fill_inc
	 nofill_num=(nofill_end-nofill_beg)/nofill_inc
         IF (plot_type.EQ.2.OR.plot_type.EQ.3) THEN
            num_iter=1
	    cont=cont1
         ELSEIF (plot_type.EQ.4.OR.plot_type.EQ.5) THEN
            num_iter=2
         ENDIF

C ADD THE CONTOUR OFFSET TO THE DATA
*	 Call AddOffSet(num_iter)

         DO i=1,num_iter
	    Write(600,*)
            Write(600,10) '   %CONTOURS'
	    Write(600,10) '   gs'
	    Call WriteTrans2Origin(num)
	    Call WriteClipIt(num)
*	    Write(600,10) '      num_font/Helvetica Font'
	    Write(600,10) '      num_font/'//font_name(1:i1)//' Font'
	    Write(600,10) '      /strh num_ht def'
	    IF (scan_mode.EQ.'POLA'.AND.math_flag.EQ.'N') THEN
C  Rotate coordinate system if data isn't in math coordinates
	       angle = atan2(-yc, -xc) * rad2deg
	       Write(600,11)  angle
	    ENDIF
	    Write (600,10) '     PolarDict begin' 
            IF (i.EQ.1.AND.num_iter.NE.1) THEN
               fill_flag='Y'
               cont=cont1
            ELSEIF (i.EQ.2.AND.num_iter.NE.1) THEN
               fill_flag='N'
               cont=cont2
            ENDIF
            IF (fill_flag.EQ.'Y'.OR.fill_flag.EQ.'y') THEN
               cont_beg=fill_beg
               cont_inc=fill_inc
               cont_num=nint(fill_num)
               plot_scale='Y'
               cont_offset=abs(cont_beg)+1

*               IF (cont_offset.LT.abs(cont_beg)) THEN
*                  cont_offset=abs(cont_beg)+1
*               ENDIF
	       Call AddOffSet(i)
            ELSEIF (fill_flag.EQ.'N'.OR.fill_flag.EQ.'n') THEN
               cont_beg=nofill_beg
               cont_inc=nofill_inc
               cont_num=nint(nofill_num)
               cont_offset=abs(cont_beg)+1
*               IF (cont_offset.LT.abs(cont_beg)) THEN
*                  cont_offset=abs(cont_beg)+1
*               ENDIF
	       Call AddOffSet(i)
            ENDIF
            zct=cont_beg
	    delta=cont_inc*.0001
            DO j=1,cont_num
                count=j+1
                Call Contour(count,delta)
                zct=zct+cont_inc-cont_offset
	    ENDDO
	    Write(600,10) '   gr'
	    IF (scan_mode.EQ.'POLA') THEN
	       Write(600,10) '      end %PolarDict'
	    ENDIF
	ENDDO

C ADD THE CONTOUR OFFSET TO THE DATA
	Call SubOffSet(num_iter)

10	Format(A)
11	Format(3X,F7.2,' rotate')

	Return
	End
**************************************************
	Subroutine WriteVector(num)

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer h1,h2,v1,v2,i,j
	Integer Indx 
	Character*2 num

	Write(600,*)
        Write(600,10) '   %VECTORS'
	Write(600,10) '   gs'
	Call WriteTrans2Origin(num)
	Call WriteClipIt(num)
	Write(600,10) '      VectorDict begin'
	Write(600,10) '       /scale {vec_dist vec_speed div} def'
	
	IF ( (horiz_inc.EQ.0).OR.(vert_inc.EQ.0) ) THEN
	   print*, 'ERROR:  divide by zero'
	   print*, 'horiz_inc: ',horiz_inc 
	   print*, 'vert_inc: ',vert_inc 
	ELSE 
	   h1 = Indx(horiz_beg, save_hbeg, horiz_inc)
	   v1 = Indx(vert_beg, save_vbeg, vert_inc)
	   h2 = Indx(horiz_end, save_hbeg, horiz_inc)
	   v2 = Indx(vert_end, save_vbeg, vert_inc)
	ENDIF

	IF (ptskip.EQ.-1) THEN
      	   DO i=h1,h2
      	      DO j=v1,v2
	         IF ( (mod(i,2).NE.0.AND.mod(j,2).NE.0).OR. 
     $           (mod(i,2).EQ.0.AND.mod(j,2).EQ.0) ) THEN
      	             IF (fld_data(i,j,1).GT.-990.AND.
     $            	fld_data(i,j,2).GT.-990) THEN 
     	          	Write(600,21) save_hbeg+((i-1)*horiz_inc),
     $              	'hkm',save_vbeg+((j-1)*vert_inc),'vkm',
     $              	fld_data(i,j,vec1),fld_data(i,j,vec2),
     $              	'scale Vector'
              	     ENDIF
                 ENDIF
      	      ENDDO
      	   ENDDO
	ELSE
      	   DO i=h1,h2,ptskip
      	      DO j=v1,v2,ptskip
      	         IF (fld_data(i,j,1).GT.-990.AND.
     $           fld_data(i,j,2).GT.-990) THEN 
     	            Write(600,21) save_hbeg+((i-1)*horiz_inc),
     $              'hkm',save_vbeg+((j-1)*vert_inc),'vkm',
     $              fld_data(i,j,vec1),fld_data(i,j,vec2),
     $              'scale Vector'
              	 ENDIF
      	      ENDDO
      	   ENDDO
	ENDIF
	Write(600,10) '     end % VectorDict' 
	Write(600,10) '   gr'

10	Format(A)
21	Format(6X,2(F7.2,1X,A,1X),2(F7.2,1X),A)

	Return
	End
**************************************************
	Integer Function Indx( stop_val, save_beg, inc)

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Real stop_val, save_beg, inc, val
	Integer i

	i = 1
	val = save_beg

		
10	IF (val.LT.stop_val) THEN
	   i = i + 1
	   val = val + inc
	   go to 10
	ENDIF

	Indx = i

	Return
	End
**************************************************
	Subroutine WriteUserFunct(num)

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Character*2 num

	Write(600,10) '   % USER FUNCTIONS'
	Write(600,10) '   gs'
	Call WriteTrans2Origin(num)
	Call WriteClipIt(num)
	Write(600,10) '      MyFunction begin'
	Write(600,10) '         % User called functions'
	Write(600,10) '         % Relative to origin of grid'
	Write(600,10) '         %'
	Write(600,10) '         % Function to draw a square'
	Write(600,10) '         % Syntax: xbeg ybeg box_w '//
     &  'box_h DrawSquare'
	Write(600,10) '         %   xbeg, ybeg: begin '//
     &  'coordinates for box'
	Write(600,10) '         %   box_w, box_h:  width & height of box'
	Write(600,10) '         %   ie: 40 hkm -20 vkm 1 inch 1 inch '//
     &  'DrawSquare'
	Write(600,10) '   '
	Write(600,10) '         % Function to draw a circle'
	Write(600,10) '         % Syntax: xori yori radius DrawCircle'
	Write(600,10) '         %   xori, yori: center of circle'
	Write(600,10) '         %   radius = radius of circle'
	Write(600,10) '         %   ie: -40 hkm -60 vkm .50 inch DrawCircle'
	Write(600,10) '   '
	Write(600,10) '         % Function to draw an arrow'
	Write(600,10) '         % Syntax: tailx taily angle length DrawArrow'
	Write(600,10) '         %   tailx, taily: position of tail of arrow'
	Write(600,10) '         %   angle: direction of arrow'
	Write(600,10) '         %   length: length of arrow'
	Write(600,10) '         %   ie: 120 hkm 60 vkm 25 .25 inch DrawArrow'
	Write(600,10) '   '
	Write(600,10) '        % Function to draw typhoon symbol'
	Write(600,10) '        % Syntax: x y size Typhoon'
	Write(600,10) '        % lat: y position'
	Write(600,10) '        % lon: x position' 
	Write(600,10) '        % size: magnification of the symbol, try 0.5'
	Write(600,10) '        % ie: 10 hkm 10 vkm 0.5 Typhoon'
	Write(600,10) ''
	Write(600,10) '        % Function to draw radar symbol '
	Write(600,10) '        % Syntax:'
	Write(600,10) '        % x hkm y vkm angle (radar name) xscale '// 
     &  ' yscale Radar  -- for cartesian coord'
	Write(600,10) '        % lon lat angle (radar name) xscale yscale '//
     &  ' Radar -- for lat lon'
	Write(600,10) '        %xscale, yscale: scale factors in x and y'
	Write(600,10) '        %lon, lat: x and y positions'
	Write(600,10) '        %angle: Orientation of the radar'
	Write(600,10) '        % ie: 0 hkm 0 vkm 0 1.0 1.0 Radar'
	Write(600,10) ''
	Write(600,10) '         % Function to draw text'
	Write(600,10) '         % Syntax: xbeg ybeg (text) justify fontsize'//
     &  ' white angle Annotate'
	Write(600,10) '         %   xbeg, ybeg: position of text'
	Write(600,10) '         %   text: text [must be enclosed in ()]'
	Write(600,10) '         %   justify: 1 = left; 2 = right; 3 = center'
	Write(600,10) '         %   fontsize: size of font'
	Write(600,10) '         %   white: points, neg num (no), pos num '//
     &  '(yes)  '
	Write(600,10) '         %   angle: degree'
	Write(600,10) '         %   ie: 20 hkm 80 vkm (some text) 3 10 1 '//
     &  '30 Annotate'
	Write(600,10) '         %   Need to setfont, i.e., 14/Helvetica Font'
	Write(600,10) '      end % MyFunction'
	Write(600,10) '   gr'
10	Format(A)

	Return
	End
**************************************************
	Subroutine WriteDrawBoundary(num)

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Character*2 num

C DRAW AND LABEL THE BOUNDARY
	Write(600,*)	
	Write(600,10) '% DRAW & LABEL BOUNDARY'
	Write(600,10) '   gs'
	Call WriteTrans2Origin(num)
	Call WriteBoundary(num)
	Write(600,10) '   gr'

10	Format(A)

	Return
	End
**************************************************
	Subroutine WriteFlightTrack(num)

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Character*2 num

	Write(600,*)	
	Write(600,10) '   % FLIGHT TRACK'
	Write(600,10) '   gs'
	Call WriteTrans2Origin(num)
	Call WriteClipIt(num)
	Call FlightTrack
	Write(600,10) '   gr'

10	Format(A)

	Return
	End
**************************************************
	Subroutine WriteFinishPlot

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

C FINISH THE PLOT
	Write(600,10)
	Write(600,10) '   panel num_panel eq {'
	IF (fill_flag.EQ.'Y'.OR.plot_type.GE.4) THEN
	   Write(600,10) '      scale_flag ON eq draw_scale ON eq and {'
	   Write(600,10) '         ScaleDict begin DrawScale end'
	   Write(600,10) '      } if'
	ENDIF
	Write(600,10) '      PageDict begin'
	Write(600,10) '         BottomLabel'
	Write(600,10) '         WriteTitle'
	Write(600,10) '      end % PageDict'
	Write(600,10) '   } if'


10	Format(A)

	Return
	End
**************************************************
	Subroutine AddOffSet(iter)

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer iter,i,j,k

	IF (k.EQ.1) cont=cont1
	IF (k.EQ.2) cont=cont2
	DO i=1,ii
	   DO j=1,jj
	      fld_data(i,j,cont)=fld_data(i,j,cont)+
     $        cont_offset
	   ENDDO
	ENDDO

	Return
	End
**************************************************
	Subroutine SubOffSet(num_iter)

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer num_iter,i,j,k

	DO k=1,num_iter
	   IF (k.EQ.1) cont=cont1
	   IF (k.EQ.2) cont=cont2
	   DO i=1,ii
	      DO j=1,jj
		 fld_data(i,j,cont)=fld_data(i,j,cont)-
     $           cont_offset
	      ENDDO
	   ENDDO
	ENDDO

	Return
	End
**************************************************
	Subroutine WriteTrans2Origin(num)

	Character*2 num 

	Write(600,10) 'Trans2Origin'

10	Format(6X,A)

	Return
	End
**************************************************
	Subroutine WriteClipIt(num)

	Character*2 num 

	Write(600,10) 'ClipIt'

10	Format(6X,A)

	Return
	End
**************************************************
	Subroutine WriteBoundary(num)
	
	Character*2 num

	Write(600,10) 'Draw&LabelBoundary'

10	Format(6X,A)

	Return
	End
**************************************************
	Subroutine CalcNum(panel,num)

	Integer panel
	Character*2 num

C Turn panel number into a string
 
	IF (panel.EQ.1) THEN 
	   num='1'
	ELSEIF (panel.EQ.2) THEN 
	   num='2'
	ELSEIF (panel.EQ.3) THEN 
	   num='3'
	ELSEIF (panel.EQ.4) THEN 
	   num='4'
	ELSEIF (panel.EQ.5) THEN 
	   num='5'
	ELSEIF (panel.EQ.6) THEN 
	   num='6'
	ELSEIF (panel.EQ.7) THEN 
	   num='7'
	ELSEIF (panel.EQ.8) THEN 
	   num='8'
	ELSEIF (panel.EQ.9) THEN 
	   num='9'
	ELSEIF (panel.EQ.10) THEN 
	   num='10'
	ENDIf

	Return
	End
**************************************************
	Subroutine CalcLabel(hlabel,vlabel)

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Character*(*) hlabel,vlabel

C Calculate horizontal & vertical labels
	IF (fix_axis.EQ.'Z') THEN
	   hlabel='X(KM)'
	   vlabel='Y(KM)'
	ELSEIF (fix_axis.EQ.'Y') THEN
	   hlabel='X(KM)'
	   vlabel='Z(KM)'
	ELSEIF (fix_axis.EQ.'X') THEN
	   hlabel='Y(KM)'
	   vlabel='Z(KM)'
	ENDIF

	Return
	End
**************************************************
	Subroutine DefineColorTable

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer i,k,count,indx
	Real val1
	Integer colorbeg,colorend
	Real colorinc
	Character*14 rgbval(45)

C ASSIGN THE RGB VALUES FOR THE COLOR TABLE..BASED ON CARBONE 42
        rgbval(1)='1.00 1.00 1.00'
        rgbval(2)='.469 .020 .640'
        rgbval(3)='.403 .227 .559'
        rgbval(4)='.164 .055 .582'
        rgbval(5)='.227 .055 .672'
        rgbval(6)='.289 .055 .766'
        rgbval(7)='.352 .141 .898'
        rgbval(8)='.414 .375 .996'
        rgbval(9)='.445 .559 .996'
        rgbval(10)='.281 .590 .602'
        rgbval(11)='.188 .523 .371'
        rgbval(12)='.004 .445 .000'
        rgbval(13)='.000 .492 .000'
        rgbval(14)='.000 .539 .000'
        rgbval(15)='.059 .586 .059'
        rgbval(16)='.176 .633 .176'
        rgbval(17)='.289 .680 .289'
        rgbval(18)='.402 .723 .402'
        rgbval(19)='.520 .770 .520'
        rgbval(20)='.633 .816 .633'
        rgbval(21)='.750 .863 .750'
	rgbval(22)='.863 .910 .863'
        rgbval(23)='.938 .906 .703'
        rgbval(24)='.938 .859 .352'
        rgbval(25)='.938 .812 .000'
        rgbval(26)='.938 .766 .023'
        rgbval(27)='.938 .719 .055'
        rgbval(28)='.926 .672 .086'
        rgbval(29)='.871 .625 .117'
        rgbval(30)='.816 .578 .148'
        rgbval(31)='.758 .531 .180'
        rgbval(32)='.703 .484 .211'
        rgbval(33)='.648 .438 .242'
        rgbval(34)='.590 .391 .250'
        rgbval(35)='.535 .344 .250'
        rgbval(36)='.485 .328 .297'
        rgbval(37)='.629 .312 .375'
        rgbval(38)='.625 .003 .000'
        rgbval(39)='.718 .086 .188'
        rgbval(40)='.813 .148 .273'
        rgbval(41)='.879 .211 .355'
        rgbval(42)='.949 .273 .355'
        rgbval(43)='1.000 .012 .000'
        rgbval(44)='0.00 0.00 0.00'

C CALCULATE THE MID VALUE SO THAT THE GREY COLOR CORRESPONDS
C WITH 0 VALUE
	fill_num=(fill_end-fill_beg)/fill_inc
	colorinc=num_colors/fill_num
	IF (cont_type.EQ.0) THEN
          k=0
          val1=fill_beg
10        IF (val1.LT.0) THEN
            val1=val1+fill_inc
            k=k+1
            goto 10
          ENDIF
          midval=k
	ELSE
	  midval=int(fill_num/2)+1
	ENDIF

	IF (cont_type.EQ.0.OR.cont_type.EQ.1) THEN
           rgbval(2+((midval-1)*colorinc))='.863 .863 .863'
	ENDIF

C CALCULATE THE BEGIN & END COLORS
        colorbeg=2
	colorend=43
	Write(600,20) '/DefineColorTable {'
	Write(600,*)
	Write(600,20) '   % COLOR TABLE..EACH ELEMENT IN '//
     &  'CT IS THE RGB VALUE'
        Write(600,20) '   /ct 256 array def'
        Write(600,20) '   /o {ct exch get aload pop setrgbcolor} def'
	count=1
	Write(600,21) count,rgbval(1)
	DO i=1,fill_num
	   count=count+1
	   indx=2+int((i-1)*colorinc)
	   Write(600,21) count,rgbval(indx)
	ENDDO
	count=44
	Write(600,21) count,rgbval(44)
	Write(600,*)
	Write(600,20) '} def'

20	Format(A)
21	Format(3X,'ct ',I2,' [ ',A,' ] put')

	Return
	End
**************************************************
	Subroutine DefineUserTable

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer i1, i, fillnum 
	Real r(45), g(45), b(45)
	Character*80 temp


C Open the user defined color table
	i1 = index(ct_fname, '  ')
	Open(Unit = 300, File = ct_fname(1:i1), Status = 'old')
	
C Read the file (user must have the correct # of colors!!!
	fill_num=(fill_end-fill_beg)/fill_inc
	fillnum=nint(fill_num)
	Read(300,*)
	DO i=1, fill_num
	   Read(300,*), r(i), g(i), b(i)
	ENDDO
	Close(300)

C Write out the PostScript code
	Write(600,10) '/DefineColorTable {'
	Write(600,*)
	Write(600,10) '   % GRAY TABLE..EACH ELEMENT IN CT IS THE RGB VALUE'
        Write (600,10) '   /ct 256 array def'
        Write (600,10) '   /o {ct exch get aload pop setrgbcolor} def'
	Write(600,20)  1, 1.000, 1.000, 1.000
	DO i = 1, fill_num
	   Write(600,20) i+1, r(i), g(i), b(i)
	ENDDO
	Write(600,20)  44, 0.000, 0.000, 0.000
	Write(600,*)
	Write(600,10) '} def'

10	Format(A)
20	Format(3X, 'ct ',I2,' [',3F6.3,'] put')

	Return
	End
**************************************************
	Subroutine DefineGrayScale

	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer fillnum

	fill_num=(fill_end-fill_beg)/fill_inc
	fillnum=nint(fill_num)

	Write(600,10) '/DefineColorTable {'
	Write(600,*)
	Write(600,10) '   % GRAY TABLE..EACH ELEMENT IN CT IS THE RGB VALUE'
        Write (600,10) '   /ct 256 array def'
        Write (600,10) '   /o {ct exch get aload pop setrgbcolor} def'
        IF (fillnum.EQ.4.) THEN
           Write (600,10) '   ct 1 [ 1.00  1.00  1.00 ] put'
           Write (600,10) '   ct 2 [ 0.90  0.90  0.90 ] put'
           Write (600,10) '   ct 3 [ 0.75  0.75  0.75 ] put'
           Write (600,10) '   ct 4 [ 0.55  0.55  0.55 ] put'
           Write (600,10) '   ct 5 [ .300  .300  .300 ] put'
           Write (600,10) '   ct 15 [ .00  .00  .00 ] put'
        ELSEIF (fillnum.EQ.5.) THEN
           Write (600,10) '   ct 1 [ 1.00  1.00  1.00 ] put'
           Write (600,10) '   ct 2 [ 0.99  0.99  0.99 ] put'
           Write (600,10) '   ct 3 [ 0.90  0.90  0.90 ] put'
           Write (600,10) '   ct 4 [ 0.75  0.75  0.75 ] put'
           Write (600,10) '   ct 5 [ 0.55  0.55  0.55 ] put'
           Write (600,10) '   ct 6 [ .300  .300  .300 ] put'
           Write (600,10) '   ct 15 [ .00  .00  .00 ] put'
        ELSEIF (fillnum.EQ.6.) THEN
           Write (600,10) '   ct 1 [ 1.00  1.00  1.00 ] put'
           Write (600,10) '   ct 2 [ 0.99  0.99  0.99 ] put'
           Write (600,10) '   ct 3 [ 0.94  0.94  0.94 ] put'
           Write (600,10) '   ct 4 [ 0.80  0.80  0.80 ] put'
           Write (600,10) '   ct 5 [ 0.62  0.60  0.62 ] put'
           Write (600,10) '   ct 6 [ 0.40  0.40  0.40 ] put'
           Write (600,10) '   ct 7 [ .200  .200  .200 ] put'
           Write (600,10) '   ct 15 [ .00  .00  .00 ] put'
        ELSEIF (fillnum.EQ.7.) THEN
           Write (600,10) '   ct 1 [ 1.00  1.00  1.00 ] put'
           Write (600,10) '   ct 2 [ 0.99  0.99  0.99 ] put'
           Write (600,10) '   ct 3 [ 0.90  0.90  0.90 ] put'
           Write (600,10) '   ct 4 [ 0.80  0.80  0.80 ] put'
           Write (600,10) '   ct 5 [ 0.70  0.70  0.70 ] put'
           Write (600,10) '   ct 6 [ 0.50  0.50  0.50 ] put'
           Write (600,10) '   ct 7 [ 0.35  0.35  0.35 ] put'
           Write (600,10) '   ct 8 [ .200  .200  .200 ] put'
           Write (600,10) '   ct 15 [ .00  .00  .00 ] put'
        ELSEIF (fillnum.EQ.8.) THEN
           Write (600,10) '   ct 1 [ 1.00  1.00  1.00 ] put'
           Write (600,10) '   ct 2 [ 0.99  0.99  0.99 ] put'
           Write (600,10) '   ct 3 [ 0.90  0.90  0.90 ] put'
           Write (600,10) '   ct 4 [ 0.80  0.80  0.80 ] put'
           Write (600,10) '   ct 5 [ 0.65  0.65  0.65 ] put'
           Write (600,10) '   ct 6 [ 0.50  0.50  0.50 ] put'
           Write (600,10) '   ct 7 [ 0.45  0.45  0.45 ] put'
           Write (600,10) '   ct 8 [ 0.35  0.35  0.35 ] put'
           Write (600,10) '   ct 9 [ .200  .200  .200 ] put'
           Write (600,10) '   ct 15 [ .00  .00  .00 ] put'
       ELSEIF (fillnum.EQ.9.) THEN
           Write (600,10) '   ct 1 [ 1.00  1.00  1.00 ] put'
           Write (600,10) '   ct 2 [ 0.99  0.99  0.99 ] put'
           Write (600,10) '   ct 3 [ 0.94  0.94  0.94 ] put'
           Write (600,10) '   ct 4 [ 0.90  0.90  0.90 ] put'
           Write (600,10) '   ct 5 [ 0.85  0.85  0.85 ] put'
           Write (600,10) '   ct 6 [ 0.80  0.80  0.80 ] put'
           Write (600,10) '   ct 7 [ 0.75  0.75  0.75 ] put'
           Write (600,10) '   ct 8 [ 0.65  0.65  0.65 ] put'
           Write (600,10) '   ct 9 [ 0.45  0.45  0.45 ] put'
           Write (600,10) '   ct 10 [ .200  .200  .200 ] put'
           Write (600,10) '   ct 15 [ .00  .00  .00 ] put'
       ELSEIF (fillnum.EQ.10.) THEN
           Write (600,10) '   ct 1 [ 1.00  1.00  1.00 ] put'
           Write (600,10) '   ct 2 [ 0.995  0.995  0.995 ] put'
           Write (600,10) '   ct 3 [ 0.97  0.97  0.97 ] put'
           Write (600,10) '   ct 4 [ 0.90  0.90  0.90 ] put'
           Write (600,10) '   ct 5 [ 0.85  0.85  0.85 ] put'
           Write (600,10) '   ct 6 [ 0.80  0.80  0.80 ] put'
           Write (600,10) '   ct 7 [ 0.75  0.75  0.75 ] put'
           Write (600,10) '   ct 8 [ 0.65  0.65  0.65 ] put'
           Write (600,10) '   ct 9 [ 0.50  0.50  0.50 ] put'
           Write (600,10) '   ct 10 [ 0.35  0.35  0.35 ] put'
           Write (600,10) '   ct 11 [ .200  .200  .200 ] put'
           Write (600,10) '   ct 15 [ .00  .00  .00 ] put'
        ELSE
           Write(200,10) 'ERROR: NUMBER OF GRAY SCALE INCREMENTS '
	   Write(200,10) 'MUST BE BETWEEN 4 AND 10'
        ENDIF
	Write(600,*)
	Write(600,10) '} def'

10	Format(A)

	Return
	End
**************************************************
