	Subroutine TextRead(id,nid,panel)

	Implicit none

	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

        Integer nid,id(nid),imax,jmax,kmax,data_format,panel
	Real temp_array(256,256)

	Open (unit=601,file=data_file,status="unknown")

	Print*, '*************************'
	Print*, '1.  P3'
	Print*, '2.  HURRICANE'
	Print*, '3.  PRESSURE'
	Print*, '4.  WEN-CHAU'
	Print*
	Print*, 'Please enter the data format you wish to plot:'
	Read*, data_format

	IF (data_format.LT.4) THEN
C Read the initial parameters for 1st 3 formats
	   Call ReadData(id,nid,imax,jmax,kmax,panel)
 	ENDIF

	IF (data_format.EQ.1) THEN  !PROCESS P3
C Read data from the P3 format
	  Call StoreDataP3(id,nid,imax,jmax,kmax,temp_array,data_format)
C Calc stats for P3 data
	  Call Calc_Stats(imax,jmax,kmax)
	ELSEIF (data_format.EQ.2) THEN
	  Call StoreData(id,nid,imax,jmax,temp_array)
        ELSEIF (data_format.EQ.3) THEN  
	  Call StoreDataP3(id,nid,imax,jmax,kmax,temp_array,data_format)
        ELSEIF (data_format.EQ.4) THEN  
	  Call WenChau(id,nid,panel)
	ENDIF

	Close(unit=601)

	Return
	End
**************************************************
	Subroutine ReadData(id,nid,imax,jmax,kmax,panel)

	Implicit none

	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

        Integer nid,id(nid),imax,jmax,kmax,Nmosm
	Integer yr,mon,day,hr,mn,sec,panel
	Real sx,sy,sz,Olat,Olong,z0,Su,Sv,Baddata

* Read array header & store in id
        Read (601,15),imax,jmax,kmax,sx,sy,sz,Olat,Olong,z0,Nmosm,Su,Sv,
     $                Baddata,yr,mon,day,hr,mn,sec
15      Format (3I5,6F8.2,I5,3F8.2,1X,3I2,1X,3I2)

        id(162) = imax
        id(167) = jmax
        id(172) = kmax
        id(163) = int(sx*1000)
        id(168) = int(sy*1000)
        id(173) = int(sz*1000)
        id(33) = int(Olat)
        id(34) = nint((Olat-id(33))*60)
        id(35) = 0
        id(36) = int(Olong)
        id(37) = nint((Olong-id(36))*60)
        id(38) = 0
        id(170) = z0*1000
        id(116) = yr
        id(117) = mon
        id(118) = day
        id(119) = hr
        id(120) = mn
        id(121) = sec
        id(160) = 0*100
        id(161) = id(160) + (id(163)*.1*(id(162)-1))
*        id(165) = 0*100
        id(165) = 30*100
        id(166) = id(165) + (id(168)*.1*(id(167)-1))
        id(171) = id(170) + (id(173)*(id(172)-1))

C CALCULATE THE LEVEL

        IF (fix_axis.EQ.'Z'.OR.fix_axis.EQ.'z') THEN
           fix_beg=id(170)/float(1000) !convert to km
           fix_inc=id(173)/float(1000) !convert to km
        ELSEIF (fix_axis.EQ.'Y'.OR.fix_axis.EQ.'y') THEN
           fix_beg=id(165)/float(id(68)) !scale factor
           fix_inc=id(168)/float(1000) !convert to km
        ELSEIF (fix_axis.EQ.'X'.OR.fix_axis.EQ.'x') THEN
           fix_beg=id(160)/float(id(68)) !scale factor
           fix_inc=id(163)/float(1000)   !convert to km
        ENDIF

        IF (level_flag.EQ.'D'.OR.level_flag.EQ.'d') THEN
           level=nint(((plots(panel)-fix_beg)/fix_inc)+1)
	ENDIF

	Return
	End
**************************************************
	Subroutine StoreData(id,nid,imax,jmax,temp_array)

	Implicit none

	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	integer id,nid,imax,jmax,i,j,k,str_index(4),num
	Real temp_array(imax,jmax)
	Character*25 field
	Character*3 match

C The names of the fields are stored in the array str

	field='EMPTY_FIELD'
	k=0
	num=1

C Initialize the data array
	DO k=1,4
	 DO i=1,imax
	  DO j=1,jmax
	   fld_data(i,j,k)=0.0
	  ENDDO
	 ENDDO
C count the characters in the fields
	 str_index(k)=index(str(k),' ')-1
	ENDDO

	k=0

C Read the data & store it into a temporary array
10	IF (field(1:8).NE.'colornum'.AND.num.LE.4) THEN	
	 Read (601,*),field
C Check for correct field
	 IF (field(1:str_index(num)).EQ.str(num)(1:str_index(num))) THEN
	  Read (601,*),temp_array
	  k=k+1
	  match='yes' 
	 ELSE
	  match='no'
	 ENDIF

	 IF (match.EQ.'yes') THEN
C Threshold the data 
	  IF (field(1:7).EQ.'meanref') THEN
	   DO i=1,imax
	    DO j=1,jmax
	     IF (temp_array(i,j).GT.50) temp_array(i,j) = -999.0
	    ENDDO
	   ENDDO
	  ELSEIF (field(1:6).EQ.'twinds') THEN
	   DO i=1,imax
	    DO j=1,jmax
	     IF (temp_array(i,j).LT.0) temp_array(i,j) = -999.0
	    ENDDO
	   ENDDO
	  ELSEIF (field(1:7).EQ.'ang_mom') THEN
	   DO i=1,imax
	    DO j=1,jmax
	     temp_array(i,j)=temp_array(i,j)*.0001
	    ENDDO
	   ENDDO
	  ENDIF
C Store the data in fld_data
	  DO i=1,imax
	   DO j=1,jmax
	    fld_data(i,j,k)=temp_array(i,j)
	   ENDDO
	  ENDDO
	  Rewind(unit=601)
	  Read (601,*)
	  num=num+1
	 ELSE 
*	  DO i=1,imax
*	   Read (601,*)
	   Read (601,*),temp_array
*	  ENDDO
	 ENDIF

	 go to 10
	ENDIF

	Return
	End
**************************************************
	Subroutine StoreDataP3(id,nid,imax,jmax,kmax,temp_array,data_format)


	Implicit none

	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer id,nid,imax,jmax,kmax,i,j,k,m,level,field
	Integer data_pos(4),data_format
	Real temp_array(imax,jmax)

	Open (unit=602,file="data.out",status="unknown")

C  ORDER OF P3 DATA
C  1=U (horizontal wind)
C  2=V (vertical wind)
C  3=W (vertical velocity)
C  4=Z (Reflectivity)
C  5=D (Adjusted divergence)

C Figure out the position in the data file and where to store it in
C the array 

	IF (data_format.EQ.1) THEN
     	  DO i=1,4	
    	   IF (fld_name(1,i).EQ."U") THEN
    	     data_pos(i)=1
    	   ELSEIF (fld_name(1,i).EQ."V") THEN
    	     data_pos(i)=2
    	   ELSEIF (fld_name(1,i).EQ."W") THEN
    	     data_pos(i)=3
    	   ELSEIF (fld_name(1,i).EQ."Z") THEN
    	     data_pos(i)=4
    	   ELSEIF (fld_name(1,i).EQ."D") THEN
    	     data_pos(i)=5
    	   ENDIF
    	  ENDDO
	  DO k=1,kmax
C Read the array
	    DO m=1,5
	     IF (m.GE.1.AND.m.LT.5) THEN
	       Read (601,11),temp_array
	     ELSE
	       Read (601,12),temp_array
	     ENDIF
C Store the array in fld_data
             IF (fix_axis.EQ.'Z'.OR.fix_axis.EQ.'z') THEN
	        DO field=1,4
	          IF (m.EQ.data_pos(field).AND.k.EQ.level) THEN
	            DO j=1,jmax
	              DO i=1,imax
	                fld_data(i,j,field)=temp_array(i,j)
	              ENDDO
	            ENDDO
	          ENDIF
	        ENDDO
             ELSEIF (fix_axis.EQ.'Y'.OR.fix_axis.EQ.'y') THEN
               DO field=1,4
                 IF (m.EQ.data_pos(field)) THEN
		   DO i=1,imax
		      fld_data(i,k,field)=temp_array(i,level)
		   ENDDO
	         ENDIF
               ENDDO
             ELSEIF (fix_axis.EQ.'X'.OR.fix_axis.EQ.'x') THEN
               DO field=1,4
                 IF (m.EQ.data_pos(field)) THEN
		    DO j=1,jmax
		       fld_data(j,k,field)=temp_array(level,j)
		    ENDDO
	         ENDIF
               ENDDO
             ENDIF
	    ENDDO
	  ENDDO

	ELSEIF (data_format.EQ.3) THEN
     	  DO i=1,4	
    	   IF (fld_name(1,i).EQ."U") THEN
    	     data_pos(i)=1
    	   ELSEIF (fld_name(1,i).EQ."V") THEN
    	     data_pos(i)=2
    	   ELSEIF (fld_name(1,i).EQ."W") THEN
    	     data_pos(i)=3
    	   ELSEIF (fld_name(1,i).EQ."Z") THEN
    	     data_pos(i)=4
    	   ELSEIF (fld_name(1,i).EQ."P") THEN
    	     data_pos(i)=5
    	   ELSEIF (fld_name(1,i).EQ."T") THEN
    	     data_pos(i)=6
    	   ENDIF
    	  ENDDO
	  DO k=1,kmax
*C Read the array
	    DO m=1,6
	     Read (601,*)
             DO j=1,jmax
              read (601,11),( temp_array(i,j),i=1,imax)
             ENDDO
C Store the array in fld_data
             IF (fix_axis.EQ.'Z'.OR.fix_axis.EQ.'z') THEN
	       DO field=1,4
	         IF (m.EQ.data_pos(field).AND.k.EQ.level) THEN
	           DO j=1,jmax
	             DO i=1,imax
	               fld_data(i,j,field)=temp_array(i,j)
	             ENDDO
	           ENDDO
	         ENDIF
	       ENDDO
             ELSEIF (fix_axis.EQ.'Y'.OR.fix_axis.EQ.'y') THEN
               DO field=1,4
                 IF (m.EQ.data_pos(field)) THEN
		   DO i=1,imax
		      fld_data(i,k,field)=temp_array(i,level)
		   ENDDO
	         ENDIF
               ENDDO
             ELSEIF (fix_axis.EQ.'X'.OR.fix_axis.EQ.'x') THEN
               DO field=1,4
                 IF (m.EQ.data_pos(field)) THEN
		    DO j=1,jmax
		       fld_data(j,k,field)=temp_array(level,j)
		    ENDDO
	         ENDIF
               ENDDO
             ENDIF
	    ENDDO
	  ENDDO
  	ENDIF

10	Format(75f8.2)
11	Format(10f8.3)
12	Format(75f12.6)

	Return
	End
**************************************************
	Subroutine Calc_Stats(imax,jmax,kmax)


	Implicit none
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

	Integer imax,jmax,kmax,i,j,k
	Integer begx1,endx1,begy1,endy1 
	Integer begx2,endx2,begy2,endy2 
	Integer u_num,v_num,w_num,div_num
	Real u_av,v_av,w_av,div_av
	Real u_std,v_std,w_std,div_std
	Real u_sum,v_sum,w_sum,div_sum

C Initialize values
	u_num=0
	v_num=0
	w_num=0
	div_num=0
	u_sum=0.0
	v_sum=0.0
	w_sum=0.0
	div_sum=0.0
    	u_av=0.0
    	v_av=0.0
    	w_av=0.0
    	div_av=0.0
    	u_std=0.0
    	v_std=0.0
    	w_std=0.0
    	div_std=0.0

	begx1=41
	endx1=61
	begy1=28
	endy1=41
	begx2=21
	endx2=48
	begy2=34
	endy2=48


	DO k=1,4
	  DO j=1,jmax
	    DO i=1,imax
	      IF (fld_data(i,j,k).GT.-100) THEN
  	       u_sum=u_sum+fld_data(i,j,k)       
	       u_num=u_num+1
	      ELSEIF (fld_data(i,j,k).GT.-100) THEN
  	       v_sum=v_sum+fld_data(i,j,k)       
	       v_num=v_num+1
	      ELSEIF (fld_data(i,j,k).GT.-100) THEN
  	       w_sum=w_sum+fld_data(i,j,k)       
	       w_num=w_num+1
	      ELSEIF (fld_data(i,j,k).GT.-100) THEN
  	       div_sum=div_sum+fld_data(i,j,k)       
	       div_num=div_num+1
	      ENDIF
	    ENDDO
	  ENDDO
	ENDDO

	Return
	End
**************************************************
	Subroutine WenChau(id,nid,panel)

	Implicit none

	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'
	Include 'structure.h'

        Integer nid,id(nid),panel,i,j,k,m,levnum,azimuth
	Integer numfield,numlev,numaz,size,num
        Integer TRUE
        Integer FALSE
        Integer datafill(4)
	Character*8 field
	Character match
	Real array(256)

        TRUE=1
        FALSE=0

	Open (unit=601,file=data_file,status="unknown")

C READ THE ID HEADER ARRAY
	Read(601,*),id

C ASSIGN SCAN MODE
	scan.id16=id(16)
	scan.id17=id(17)
	scan_mode=scan.mode
	
	Print*, 'Processing panel ', panel
	IF (scan_mode.EQ.'POLA') THEN
10	   Print*, 'Is data in math coordinates?? [Y/N]'
	   Read*, math_flag
	   IF (math_flag.EQ.'y') math_flag = 'Y'
	   IF (math_flag.EQ.'n') math_flag = 'N'
	   IF (math_flag.NE.'Y'.AND.math_flag.NE.'N') go to 10
C CALCULATE THE CENTER OF THE STORM IN RELATIONSHIP TO THE ORIGIN 
           IF (math_flag.EQ.'N') THEN 
              xc = real(id(309) / id(68))
              yc = real(id(310) / id(68))
           ENDIF 
	ENDIF
C CALCULATE THE LEVEL
        IF (fix_axis.EQ.'Z'.OR.fix_axis.EQ.'z') THEN
           fix_beg=id(170)/float(1000) !convert to km
           fix_inc=id(173)/float(1000) !convert to km
        ELSEIF (fix_axis.EQ.'Y'.OR.fix_axis.EQ.'y') THEN
	   IF (scan_mode.EQ.'POLA') THEN
              fix_beg=id(165)/float(id(69))  !angle scale factor
           ELSE
              fix_beg=id(165)/float(id(68))  !general scale factor
	   ENDIF 
           fix_inc=id(168)/float(1000) !convert to km
        ELSEIF (fix_axis.EQ.'X'.OR.fix_axis.EQ.'x') THEN
           fix_beg=id(160)/float(id(68))  !general scale factor
           fix_inc=id(163)/float(1000) !convert to km
        ENDIF

        IF (level_flag.EQ.'D'.OR.level_flag.EQ.'d') THEN
           level=nint(((plots(panel)-fix_beg)/fix_inc)+1)
	ENDIF

    	numlev=id(172)
	numaz=id(167)
	numfield=id(175)
	size=id(162)
	DO i=1,numlev
	   Read(601,20),levnum
	   DO j=1,numaz
              DO m=1,4 
	         datafill(m)=FALSE 
	      ENDDO
	      Read(601,21),azimuth
c              print *, azimuth, levnum
	      DO k=1,numfield
	         Read(601,22),field
		 match='N'
		 num=1
*11		 IF (match.EQ.'N'.AND.num.LE.4) THEN
11		 IF (num.LE.4) THEN
		    IF (field(1:2).EQ.fld_name(1,num).AND.
     $	 	        field(3:4).EQ.fld_name(2,num).AND.
     $			field(5:6).EQ.fld_name(3,num).AND.
     $			field(7:8).EQ.fld_name(4,num).AND.
     $                  datafill(num).NE.TRUE) THEN
			   IF (match.EQ.'N') THEN
 	                      Call GetData(array,size)
			   ENDIF
 			   match='Y'
     			   datafill(num)=TRUE
C			   IF ( (fix_axis.EQ.'Z'.AND.levnum.EQ.level)
C     $  			 .OR.fix_axis.EQ.'Y'
C     $				 .OR.fix_axis.EQ.'X') THEN
C		 	      scalefld(num)=id(180+(5*(k-1)))
C			      Call FillArray(id,nid,array,size,azimuth,
C     $					     levnum,num)
C			   ENDIF
			   IF ( fix_axis.EQ.'Z'.AND.levnum.EQ.level ) THEN
		 	      scalefld(num)=id(180+(5*(k-1)))
			      Call FillArray(id,nid,array,size,azimuth,
     $					     levnum,num)
			   ELSEIF ( fix_axis.EQ.'Y'.AND.azimuth.EQ.level ) THEN
		 	      scalefld(num)=id(180+(5*(k-1)))
			      Call FillArray(id,nid,array,size,azimuth,
     $					     levnum,num)
			   ELSEIF ( fix_axis.EQ.'X' ) THEN
		 	      scalefld(num)=id(180+(5*(k-1)))
			      Call FillArray(id,nid,array,size,azimuth,
     $					     levnum,num)
			   ENDIF
 		    ELSE
 		       num=num+1
 		    ENDIF
 		    go to 11
		 ENDIF
 		 IF (match.EQ.'N') THEN
 	            Call GetData(array,size)
 		 ENDIF
	      ENDDO
	   ENDDO
	ENDDO

20	Format(5x,i2) 
21	Format(7x,i3) 
22	Format(A8) 
23	Format(8E10.3)

	Close(unit = 601)

	Return
	End
**************************************************
	Subroutine GetData(array,size)

	Integer size,i
	Real array(size)
	
	Read(601,10),array

10	Format(8E10.3)

	Return
	End
**************************************************
	Subroutine FillArray(id,nid,array,numx,y,z,fieldnum)

	Implicit none

	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

        Integer nid,id(nid),fieldnum,x,y,z,lev,numx
	Real array(numx)

	
C FIXED AXIS=Z
	IF (fix_axis.EQ.'Z'.OR.fix_axis.EQ.'z') THEN
	      DO X=1,numx
	         fld_data(x,y,fieldnum)=array(x)
	      ENDDO
C FIXED AXIS=Y
	ELSEIF (fix_axis.EQ.'Y'.OR.fix_axis.EQ.'y') THEN
	   DO X=1,numx
	      fld_data(x,z,fieldnum)=array(x)
	   ENDDO
C FIXED AXIS=X
	ELSEIF (fix_axis.EQ.'X'.OR.fix_axis.EQ.'x') THEN
           fld_data(y,z,fieldnum)=array(level)
	ENDIF

	Return
	End
