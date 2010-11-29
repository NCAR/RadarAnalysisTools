	Program main

	Implicit none

	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'
	Include 'structure.h'

	Integer nid 
	Integer id(510),panel,iun
	Character*80 batch_name(10)


C Dimension of array id
	nid=510

C Initialize data array
	Call InitializeArray(256,500,4)

C Open the error file
	Open(unit=200,file='grid2ps.log',status='unknown')

C Get the arguments
	Call GetArgs(batch_name)
   	
C Calculate the number of panels
	Call CalcNumOfPlots

	DO panel=1,num_plot
           iun=10*panel
	   Open (unit=500,file=batch_name(panel),status='old')
C Read in the first part of the input file
           Write(200,*)  'Reading ', batch_name(panel)
	   Call ReadVar(panel, 1)

C Read Cedric or Text file 
	   Write(200,*)  'Reading ', 
     &     data_file(1:index(data_file,'  '))
	   Write(200,*)
           IF (input_flag.EQ.'C'.OR.input_flag.EQ.'c') THEN 
              Call CEDREAD(id,nid,iun,panel)
           ELSEIF (input_flag.EQ.'W'.OR.input_flag.EQ.'w') THEN
C  User supplied routine to read a text file (ascii)
	      Call WenChau(id,nid,panel)  
	   ELSE
	      Call ReadUserDat(id, nid, panel)
           ENDIF

C Assign values to variables:  from header id & assign default values 
	   Write(200,*)  'Calling AssignVar'
	   Write(200,*)
	   Call AssignVar(id,nid,panel)

C Calculate the level	   
	   Call CalcLevel(panel)

C Read in rest of batch input
	   IF (panel.EQ.1.OR.save_config.EQ.0) THEN
	     print*, 'calling readvar, flag = 2'
	     print*, 'save_config: ', save_config
	     Call ReadVar(panel, 2 )
	   ENDIF

C Print out input parameters & check for goofs..
	   Call PrintInp(panel) 
	   Write(200,*)

C Massage the data (if necessary)..
C	   Call MassageData(id, nid)

C Write the postscript file
 	   Write(200,*)  'Calling WritePS'
	   Write(200,*)
	   Call WritePS(panel)

C Close the input file
	   Close(unit=500)
	ENDDO

C Close the log file
	Close(unit=200)

	End
**************************************************
        Subroutine CalcNumOfPlots

        Implicit none

        Include 'common1.h'
        Include 'common2.h'
        Include 'common3.h'

        IF (template.EQ.1.OR.template.EQ.11) THEN
         num_plot=1
        ELSEIF (template.EQ.2.OR.template.EQ.6.
     $          OR.template.EQ.12) THEN
         num_plot=2
        ELSEIF (template.EQ.3) THEN
         num_plot=3
        ELSEIF (template.EQ.4.OR.template.EQ.7.
     $          OR.template.EQ.13) THEN
         num_plot=4
        ELSEIF (template.EQ.5) THEN
         num_plot=5
        ELSEIF (template.EQ.8) THEN
         num_plot=6
        ELSEIF (template.EQ.9) THEN
         num_plot=8
        ELSEIF (template.EQ.10) THEN
         num_plot=10
        ELSEIF (template.EQ.14) THEN
         num_plot=3
        ELSEIF (template.EQ.15) THEN
         num_plot=6
        ELSEIF (template.EQ.16) THEN
         num_plot=9
        ELSEIF (template.EQ.17) THEN
         num_plot=3
        ELSEIF (template.EQ.18) THEN
         num_plot=6
        ELSEIF (template.EQ.19) THEN
         num_plot=9
        ENDIF

        Return
        End
**************************************************
	Subroutine PrintInp(panel) 

        Implicit none

        Include 'common1.h'
        Include 'common2.h'
        Include 'common3.h'

	Integer panel, ierr, i1
	Character err_flag

	err_flag = "N"

C PostScript file name

	i1 = index(output_name, '  ')-1
	Write(200,10)  'output_name: ', output_name(1:i1)
	Open(Unit = 300, File = output_name(1:i1), Status = 'unknown',
     &  Iostat = ierr) 
	IF (ierr.NE.0) THEN
	   Write(200,*)  "Unable to open ", 
     &     output_name(1:i1),' for writing..'
	   err_flag = "Y"
	ELSE
	   close(300)
	ENDIF

C Data file name
	i1 = index(data_file, '  ')-1
	Write(200,10)  'data_file: ', data_file(1:i1)
	Open(Unit = 300, File = data_file(1:i1), Status = 'old',
     &  Iostat = ierr) 
	IF (ierr.NE.0) THEN
	   Write(200,*)  "Can't find ", data_file(1:i1)
	   err_flag = "Y"
	ELSE
	   close(300)
	ENDIF

C Color flag
	Write(200,10)  'color_flag: ', color_flag 
	IF (color_flag.NE.'Y'.AND.color_flag.NE.'N') THEN
	   Write(200,*)  'Incorrect color flag'
	   Write(200,*)  'Must be a "Y" or "N"..'
	   err_flag = "Y"
	ENDIF

C Fixed axis
	Write(200,10)  'fix_axis: ', fix_axis
	IF (fix_axis.NE.'Z'.AND.fix_axis.NE.'Y'.AND.fix_axis.NE.'X') THEN
	   Write(200,*)  'Incorrect fixed axis'
	   Write(200,*)  'Must be a "X", "Y" or "Z"..'
	   err_flag = "Y"
	ENDIF

C Contour type
	Write(200,11)  'cont_type: ', cont_type 
	IF (cont_type.NE.0.AND.cont_type.NE.1.AND.cont_type.NE.2) THEN
	   Write(200,*)  'Incorrect contour type'
	   Write(200,*)  'Must be 0, 1, or 2..changing to 2'
	   cont_type = 2
	ENDIF

C Field names & plot type
	Write(200,10)  'HORIZONTAL WIND FIELD NAME (U): ', str(1)
	Write(200,10)  'VERTICAL WIND FIELD (V): ', str(2)
	Write(200,10)  'CONTOUR FIELD 1: ', str(3)
	Write(200,10)  'CONTOUR FIELD 2: ', str(4)
	Write(200,11)  'plot_type: ', plot_type 
	IF (plot_type.LT.0.OR.plot_type.GT.5) THEN
	   Write(200,*)  'Incorrect plot type'
	   Write(200,*)  '1.  Vector [Field1, Field2]'
	   Write(200,*)  '2.  Contour [Field3]'
	   Write(200,*)  '3.  Vector, Contour [Field1, Field2, Field3]'
	   Write(200,*)  '4.  Contour, Contour [Field3, Field4]'
	   Write(200,*)  '5.  Vector, Contour, Contour [Field1, Field2,'// 
     &     'Field3, Field4]'
	ENDIF

C Fill info 
	Write(200,10)  'fill_flag: ', fill_flag
	Write(200,12)  'fill_beg: ', fill_beg
	Write(200,12)  'fill_end: ', fill_end
	Write(200,12)  'fill_inc: ', fill_inc
	Write(200,12)  'nofill_beg: ', nofill_beg
	Write(200,12)  'nofill_end: ', nofill_end
	Write(200,12)  'nofill_inc: ', nofill_inc

C Panel title
	i1 = index(panel_title, '  ')-1
	Write(200,10)  'panel_title: ', panel_title(1:i1)

C Scale title
	i1 = index(scale_title, '  ')-1
	Write(200,10)  'scale_title: ', scale_title(1:i1)

C Title
	i1 = index(title, '  ')-1
	Write(200,10)  'title: ', title(1:i1)

C Level flag
	Write(200,10)  'level_flag: ',level_flag
	IF (level_flag.NE."L".AND.level_flag.NE."D") THEN
	   Write(200,*)  'Incorrect level flag'
	   Write(200,*)  'Must be "D" or "L"'
	ENDIF
	IF (level_flag.EQ."L") THEN
	   Write(200,11)  'level: ', level
	ELSEIF (level_flag.EQ."D") THEN
	   Write(200,12)  'dist: ', plots(panel)
	ENDIF

C catalog file name

	i1 = index(catalog_name, '  ')-1
	Write(200,10)  'cat: ', catalog_name(1:i1)
	IF ( index(catalog_name, 'cat').NE.0) THEN
	   Open(Unit = 300, File = catalog_name(1:i1), Status = 'old',
     &     Iostat = ierr) 
	   IF (ierr.NE.0) THEN
	      Write(200,*)  "Can't find ", 
     &        catalog_name(1:i1)
	      Write(200,*)  'Changing to "NONE"'
	      catalog_name(1:4) = "NONE"
	   ELSE
	      close(300)
	   ENDIF
	ENDIF

C plane increment
	Write(200,11)  'plane_inc: ', plane_inc 
	IF (plane_inc.LE.0) THEN
	   Write(200,*)  'Incorrect plane increment..changing to 1'
	   plane_inc = 1
	ENDIF

C Coordinate parameters
	Write(200,12)  'horiz_beg: ', horiz_beg
	Write(200,12)  'horiz_end: ', horiz_end
	Write(200,12)  'vert_beg: ', vert_beg
	Write(200,12)  'vert_end: ', vert_end
C Grid parameters
	Write(200,11)  'pri_grid1: ', pri_grid1
	Write(200,11)  'pri_grid2: ', pri_grid2
	Write(200,11)  'sec_grid1: ', sec_grid1
	Write(200,11)  'sec_grid2: ', sec_grid2
C Scale Parameters
	Write(200,10)  'scale_flag: ', scale_flag
	Write(200,10)  'scale_pos: ', scale_pos
	Write(200,12)  'scale_w: ', scale_w 
	Write(200,12)  'scale_h: ', scale_h 
C Line Parameters
	Write(200,12)  'zero_linewidth: ', zero_linewidth 
	Write(200,12)  'linewidth: ', linewidth 
	Write(200,11)  'dash_on: ', dash_on 
	Write(200,11)  'dash_off: ', dash_off
	Write(200,11)  'dash_beg: ', dash_beg
C Vector Parameters
	Write(200,11)  'ptskip:', ptskip 
	Write(200,12)  'vec_speed:', vec_speed 
	Write(200,12)  'vec_length:', vec_length
	Write(200,12)  'arrow_h:', arrow_h 
	Write(200,12)  'arrow_hh:', arrow_hh 
	Write(200,12)  'arrow_ht:', arrow_ht 
C Font Parameters
	Write(200,11)  'font_title:', font_title 
	Write(200,11)  'font_num:', font_num
	Write(200,11)  'font_label:', font_label 
C Plot Parameters
	Write(200,12)  'stretch_horiz:', stretch_horiz
	Write(200,12)  'stretch_vert:', stretch_vert 
        IF (stretch_horiz.LE.0) THEN
	   Write(200,10)  'Setting horizontal stretch to 1.0'
	   stretch_horiz = 1.0
	ENDIF
        IF (stretch_vert.LE.0) THEN
	   Write(200,10)  'Setting vertical stretch to 1.0'
	   stretch_vert = 1.0
	ENDIF
	Write(200,12)  'sp_text:', sp_text
	Write(200,12)  'sp_scale:', sp_scale
	Write(200,12)  'sp_row:', sp_row
	Write(200,12)  'sp_col:', sp_col
	Write(200,12)  'sp_title:', sp_title
	Write(200,10)  'title_flag:', title_flag
	Write(200,10)  'grid_flag:', grid_flag
	Write(200,12)  'panel_w:', box_w 
	Write(200,12)  'panel_h:', box_h
	i1 = index(ct_fname, '  ')-1
	Write(200,10)  'ct_fname: ', ct_fname(1:i1)
	IF (ct_fname(1:4).NE.'NONE') THEN
	   Open(Unit = 300, File = ct_fname(1:i1), Status = 'old',
     &     Iostat = ierr) 
	   IF (ierr.NE.0) THEN
	      Write(200,*)  "Unable to open ", 
     &        ct_fname(1:i1),' for reading..'
	      err_flag = "Y"
	   ELSE
	      close(300)
	   ENDIF
	ENDIF
C Page Parameters
	Write(200,12)  'page_w:', page_w
	Write(200,12)  'page_h:', page_h
	Write(200,12)  'l_marg:', l_marg
	Write(200,12)  'r_marg:', r_marg
	Write(200,12)  't_marg:', t_marg
	Write(200,12)  'b_marg:', b_marg

	IF (err_flag.EQ."Y") THEN
	   print*, 'Error in input file..see grid2ps.log for details..'
	   stop
	ENDIF

10	Format(3X, A, A)
11	Format(3X, A, I3) 
12	Format(3X, A, F8.2) 
13	Format(3X, A, F8.2,', ',F8.2,', ',F8.2)

	Return
	End
**************************************************
	Subroutine InitializeArray(num_x, num_y, num_f)

        Implicit none

        Include 'common1.h'
        Include 'common2.h'
        Include 'common3.h'

	Integer num_x, num_y, num_f
	Integer x, y, fld

C INITIALIZE THE DATA ARRAY
	DO fld = 1, num_f
	   DO x = 1, num_x
	      DO y = 1, num_y
		 fld_data(x, y, fld) = -999.0
	      ENDDO
	   ENDDO
	ENDDO

	Return
	End
**************************************************
	Subroutine GetArgs( batch_name )


        Implicit none

        Include 'common1.h'
        Include 'common2.h'
        Include 'common3.h'

	Character*80 batch_name(10)
	Character*80 arg
	Integer nargs, i, iargc, min_args 

C Get command line arguments 
	nargs = iargc()

	IF (nargs.GT.0) THEN

           DO i = 1, nargs
              Call getarg(i, arg)
              IF (i.EQ.1) THEN
   	      Call Char2Int(arg, template) 
              ELSE
   	      batch_name(i-1) = arg
              ENDIF
           ENDDO

C Make sure template is valid
   	   IF (template.LE.0.OR.template.GT.13) THEN
   	      print*, 'Error: invalid template number'
   	      print*, 'Must be between 1 & 13'
   	      stop
   	   ENDIF

C Make sure # of input files is correct
           IF (template.EQ.1.OR.template.EQ.11) THEN
              min_args=2
           ELSEIF (template.EQ.2.OR.template.EQ.6.
     $             OR.template.EQ.12) THEN
              min_args=3
           ELSEIF (template.EQ.3) THEN
              min_args=4
           ELSEIF (template.EQ.4.OR.template.EQ.7.
     $             OR.template.EQ.13) THEN
              min_args=5
           ELSEIF (template.EQ.5) THEN
              min_args=6
           ELSEIF (template.EQ.8) THEN
              min_args=7
           ELSEIF (template.EQ.9) THEN
              min_args=9
           ELSEIF (template.EQ.10) THEN
              min_args=11
           ENDIF

   	   IF (nargs.LT.min_args) THEN
   	      Print 10,  min_args-1, template 
   	      Write(200,10)   min_args-1, template 
   	      stop
   	   ENDIF


	ELSE
C Get the template
       	   Print*, '**************************************************'
   	   Print*, '1.  1 plot (1 row, 1 column); portrait'//
     $     ' orientation'
   	   Print*, '2.  2 plots (2 rows, 1 column); portrait'//
     $     ' orientation'
   	   Print*, '3.  3 plots (3 rows, 1 column); portrait'//
     $     ' orientation'
   	   Print*, '4.  4 plots (4 rows, 1 column); portrait'//
     $     ' orientation'
   	   Print*, '5.  5 plots (5 rows, 1 column); portrait'//
     $     ' orientation'
   	   Print*, '6.  2 plots (1 rows, 2 columns); portrait'//
     $     ' orientation'
   	   Print*, '7.  4 plots (2 rows, 2 columns); portrait'//
     $     ' orientation'
   	   Print*, '8.  6 plots (3 rows, 2 columns); portrait'//
     $     ' orientation'
   	   Print*, '9.  8 plots (4 rows, 2 columns); portrait'//
     $     ' orientation'
   	   Print*, '10. 10 plots (5 rows, 2 columns); portrait'//
     $     ' orientation'
   	   Print*, '11.  1 plot (1 row, 1 column); landscape'//
     $     ' orientation'
   	   Print*, '12.  2 plots (1 rows, 2 columns); landscape'//
     $     ' orientation'
   	   Print*, '13.  4 plots (2 rows, 2 columns); landscape'//
     $     ' orientation'
   	   Print*, '14.  3 plots (1 rows, 3 columns); portrait'//
     $     ' orientation'
   	   Print*, '15.  6 plots (2 rows, 3 columns); portrait'//
     $     ' orientation'
   	   Print*, '16.  9 plots (3 rows, 3 columns); portrait'//
     $     ' orientation'
   	   Print*, '17.  3 plots (1 rows, 3 columns); landscape'//
     $     ' orientation'
   	   Print*, '18.  6 plots (2 rows, 3 columns); landscape'//
     $     ' orientation'
   	   Print*, '19.  9 plots (3 rows, 3 columns); landscape'//
     $     ' orientation'
   	   Print*, 'Enter template number:'
   	   Read*, template
C Default name of input file(s)
   	   batch_name(1)='inp1'
   	   batch_name(2)='inp2'
   	   batch_name(3)='inp3'
   	   batch_name(4)='inp4'
   	   batch_name(5)='inp5'
   	   batch_name(6)='inp6'
   	   batch_name(7)='inp7'
   	   batch_name(8)='inp8'
   	   batch_name(9)='inp9'
   	   batch_name(10)='inp10'
	ENDIF

10	Format('Error: need ',I2, ' input file(s) for template = ', I2)

	Return
	End
**************************************************
	Subroutine PrintArray(id, nid)

        Implicit none

	Integer nid
	Integer id(nid)
	Integer i, j, k

        Include 'common1.h'
        Include 'common2.h'
        Include 'common3.h'

	DO i = 1, id(162)
	   DO j = 1, id(167)
	      IF (fld_data(i,j,1).GT.-900) print*, fld_data(i,j,1)
	   ENDDO
	ENDDO

	Return
	End
**************************************************
	Subroutine CalcLevel(panel)

        Implicit none

        Include 'common1.h'
        Include 'common2.h'
        Include 'common3.h'

	Integer panel

C CALCULATE LEVEL
        IF (level_flag.EQ.'D'.OR.level_flag.EQ.'d') THEN
           level=nint(((plots(panel)-fix_beg)/fix_inc)+1)
        ENDIF

	Return
	End
