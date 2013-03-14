
       SUBROUTINE MDVOPEN(INUNIT,IHED,NST)

       include 'mf_mdv.inc'
       
       INCLUDE 'CEDRIC.INC'
       COMMON /LEVELS/ VALLEV(MAXZLEV),VALNYQ(MAXZLEV),VNYQ_VOL
       COMMON /MDVGRIB/ MDVXMIFLD,MDVXMAX,MDVYMIN,MDVYMAX,MDVSP,
     x                  MDVFLG,GRIBFLG
       COMMON /MDVLEVELS/ NFLDLVLS(MAXZLEV),MINFLDLVL(MAXZLEV),
     x                    FLDLVLSP(MAXZLEV)

C      The common block MDVLEVELS saves the number of 
C      vertical levels each field has associated with it.
       REAL VALLEV
       REAL MDVXMIN,MDVXMAX,MDVYMIN,MDVYMAX,MDVSP
       REAL MINFLDLVL,FLDLVLSP
       CHARACTER*8 GFIELD(NFMAX)
       integer INUNIT,NST,IHED,mostnz,currnz
       integer MDVNX,MDVNY,GRIBFLGMI
       integer NFLDLVLS
       DIMENSION IHED(NID)

      
CThe following information is recieved from the MDV master header
       integer*4 mhdrints(MDVMSI32)
       REAL*4    mhdrReal(MDVMFL32)
       REAL  location(3),SEC,rtemp
       integer*4 begtime,endtime,tarray(9)
       integer*2 DEG,MIN
       integer IGENSCL,IANGSCL,IFLD,ilnd
       integer nflds, numxyz(3),mdvorder
       integer proj_type,lhincl
       DOUBLE PRECISION VAR
       CHARACTER*(MDVINFOL) dataseti
       CHARACTER*(MDVNAMEL) dataset
       CHARACTER*(MDVNAMEL) datasets


CThe following information is recieved from the MDV field header
       integer*4 fhdrints(MDVFSI32)
       integer index,nx,ny,nz
       integer ispcx,ispcy,ispcz,iscale
       REAL*4 dx,dy,dz,x1,y1,z1,x2,y2,z2,altitude
       
       REAL*4    fhdrReal(MDVFFL32)
       CHARACTER*(MDVLFLDL) fnlong
       CHARACTER*(MDVSFLDL) fnshort
       CHARACTER*(MDVUNITL) funits
       CHARACTER*(MDVTRANL) ftrans
       CHARACTER*(MDVUNITL) funschar
       CHARACTER*2 CSCAN1,CSCAN2

      CHARACTER*6 sciname,namlnd(7),radarnam
      CHARACTER*8 volnam,source,ctemp
      character*8 thetime,theday
      integer  xlnd(7),ylnd(7),zlnd(7)
      integer  numlnd,numrad,nfld,scantype
      integer  projection,readlvls
      integer  rstatus,reverse
      real     scale,bias,bad,missing
      real     templvl(MAXZLEV)    

      INTEGER*4      VLEVEL_HDR_INTS(MDV_NUM_VLEVEL_HEADER_SI32)
      REAL*4         VLEVEL_HDR_REALS(MDV_NUM_VLEVEL_HEADER_FL32)

       integer i, j
       integer fieldnum,baseangle

      data sciname/'unknw '/
      data numlnd,numrad/1,1/

      IGENSCL = 100
      IANGSCL = 64

c
c---------------------Read the master header--------------------
c

       call rdmaster(inunit,mhdrints,mhdrReal,
     x               dataseti,datasetn,datasets,
     x               valnyq,rstatus,volnam)

       if (rstatus .ne. MDVROK) then
         print *, "ERROR READING THE MASTER MDV HEADER"
         stop
       end if


       call MF_pm_print_master_hdr(mhdrints, mhdrReals,
     &                             dataseti,datasetn,datasets)

c   Get information from the master header that is needed by the 
c   510 word Cedric header.     
       call loadcedi(mhdrints, mhdrReal,
     x               dataseti,datasetn,datasets,
     x               scantype,location,
     x               begtime,endtime,nflds,numxyz,
     x               mdvorder)


       if(datasets(1:3) .eq. "RUC") then
          source = "RUC 2"
          radarnam = "UNKNWN"
       else 
          source = "UNKNOWN"
          call mdvradnm(radarnam,baseangle)
       endif

       baseangle = 90
       ihed(40) = baseangle * IANGSCL
       

CX AND Y COORDINATES OF THE ORIGIN
       ihed(39) = 0
       ihed(40) = 90 * 64
       ihed(41) = 0
       ihed(42) = 0
       ihed(61) = 510
       ihed(63) = 16
       ihed(64) = 2
       ihed(65) = 3200
       ihed(67) = -32768
       ihed(68) = 100
       ihed(69) = 64

       ctemp = "MDVFMT"
       read(ctemp,15)(ihed(I),I=1,4)
       read(radarnam,10)(ihed(i),I = 13,15)
 10    format(3A2)
       read(source,15)(ihed(I),I=71,74) 
 15    format(4A2)

c  Put latitude and longitude information into Cedric header

       ihed(33) = 0.0
       ihed(34) = 0.0
       ihed(35) = 0.0
       if(location(1) .ne. 0.0) then
          var = location(1)
          call DEGMINSC(deg,min,sec,var)
          ihed(33) = deg
          ihed(34) = min
          ihed(35) = sec * 100
       endif 
       if(location(1) .eq. 0 .and.location(2) .eq. 0) then
          print *,"WARNING: lat and lon are both 0 in the data source"
       endif

       ihed(36) = 0.0
       ihed(37) = 0.0
       ihed(38) = 0.0 
       if(location(2) .ne. 0.0) then        
          var = location(2)
          call DEGMINSC(deg,min,sec,var)
          ihed(36) = deg
          ihed(37) = min
          ihed(38) = sec * 100
       endif 
       var = location(3)
       altitude = var


c  Put beginning time and ending time into cedric header.
       call gm_time(begtime,tarray)
       ihed(21) = tarray(6)
       ihed(22) = tarray(5) + 1
       ihed(23) = tarray(4)
       ihed(24) = tarray(3)
       ihed(25) = tarray(2)
       ihed(26) = tarray(1)

       ihed(116) = tarray(6)
       ihed(117) = tarray(5) + 1
       ihed(118) = tarray(4)
       ihed(119) = tarray(3)
       ihed(120) = tarray(2)
       ihed(121) = tarray(1)  


       print *,'MDV returned VOLNAM=',volnam
       write(ctemp,20)volnam
       read(ctemp,15)(ihed(I),I=101,104)


       call gm_time(endtime,tarray)
       ihed(27) = tarray(6)
       ihed(28) = tarray(5) + 1
       ihed(29) = tarray(4)
       ihed(30) = tarray(3)
       ihed(31) = tarray(2)
       ihed(32) = tarray(1)

       ihed(122) = tarray(6)
       ihed(123) = tarray(5) + 1
       ihed(124) = tarray(4)
       ihed(125) = tarray(3)
       ihed(126) = tarray(2)
       ihed(127) = tarray(1)      
 


      call datee(theday)
      write(CTEMP,20)theday
      read(CTEMP,15)(ihed(I),I = 51,54)

      call clock(thetime)
      write(CTEMP,20),thetime
      read(CTEMP,15)(ihed(I),I = 55,58)
 20   format(A8)

      ctemp = "WK"
      read(ctemp,25),ihed(62)
 25   format(A2)

       ihed(175) = nflds 
       call mdvgordr(ihed,mdvorder,nid)
       MDVNX = numxyz(1)
       MDVNY = numxyz(2)

       lhincl = mhdrints(16)
C      If lhincl is 1 then a level header is included
C      in the data set for each field.
C      If lhincl is 0 then no level headers exits for
C      any field in the data set.

C------------------Read in field header--------------------------
c
c      Read in and process each of the fields in the file
c
       NFLD = ihed(175)
       
       do index = 1, NFLD

         fieldnum = index - 1

c
c        Read in and process the field header
c
         gfield(index) = "        "

         call rdfieldh(INUNIT, fieldnum,fhdrints,fhdrReal,
     &                 fnlong, fnshort,funits,
     &                 ftrans,funschar,gfield(index),
     &                 rstatus)


         if (rstatus .ne. MDVROK) then
           print *, "Error ", rstatus, " Reading field ",
     &           fieldnum, " header"

           stop
         end if

         call MF_pf_print_field_hdr(fhdrints, fhdrReal,
     &                              fnlong, fnshort,funits,
     &                              ftrans,funschar)


         NFLDLVLS(index) = fhdrints(MDVFNZIX)
         if(lhincl .eq. 1) then
            call MF_RV_READ_VLEVEL_HDR(fieldnum,VLEVEL_HDR_INTS,
     &                                 VLEVEL_HDR_REALS,rstatus)
            if (rstatus .ne. MDVROK) then
                print *,"Error reading field level header #  ",index
                stop
            end if
            call MF_pv_print_vlevel_hdr(VLEVEL_HDR_INTS,
     &                                  VLEVEL_HDR_REALS,
     &                                  NFLDLVLS(index),
     &                                  gfield(index))

            MINFLDLVL(index) = 100000.
            do i = 1,fhdrints(MDVFNZIX)
               rtemp = VLEVEL_HDR_REALS(i)
               if(rtemp < MINFLDLVL(index)) then
                  MINFLDLVL(index) = rtemp
               end if
           end do

           FLDLVLSP(index) = 0
           if(NFLDLVLS(index) .gt. 1) then
             FLDLVLSP(index) = VLEVEL_HDR_REALS(2) - VLEVEL_HDR_REALS(1)
           end if


         end if


         call sclinfo(fhdrReal(MDV_FH_SCALE_INDEX),
     &        fhdrReal(MDV_FH_BIAS_INDEX),
     &        fhdrReal(MDV_FH_BAD_DATA_VALUE_INDEX),
     &        fhdrReal(MDV_FH_MISSING_DATA_VALUE_INDEX),index)

         
         projection = fhdrints(MDV_FH_PROJ_TYPE_INDEX)
         if(index .eq. 1) then
            call ldscanty(ihed,nid,scantype,projection)
         end if
          

CIf the lat,lon and alt information was not in the master
Ccheck for it in the field header.
         if(ihed(33) .eq. 0) then
            var = fhdrReal(MDV_FH_PROJ_ORIGIN_LAT_INDEX)
            call DEGMINSC(deg,min,sec,var)
            ihed(33) = deg
            ihed(34) = min
            ihed(35) = sec * 100
         endif

         if(ihed(36) .eq. 0.0) then
            var = fhdrReal(MDV_FH_PROJ_ORIGIN_LON_INDEX)
            call DEGMINSC(deg,min,sec,var)
            ihed(36) = deg
            ihed(37) = min
            ihed(38) = sec * 100
         endif 
         end do
CEnd of loop for reading in the field headers.
C-------------------------------------------------------

         if(MDVFLG .EQ. 0) then
            nx=fhdrints(MDVFNXIX)
            ny=fhdrints(MDVFNYIX)
         ELSE
            NX = (MDVXMAX - MDVXMIN) * (1/MDVSP)
            NY = (MDVYMAX - MDVYMIN) * (1/MDVSP)
            NX = NX + 1
            NY = NY + 1
         ENDIF

         ihed(162) = nx
         ihed(167) = ny


         nz = mhdrints(22)
         if(nx * ny * nz .gt.  MAXX * MAXY * MAXZLEV) then
            print *,"THE NUMBER OF POINTS IS TOO LARGE"
            STOP
         end if
         

         ihed(154) = nz
         ihed(106) = nz
         ihed(172) = nz

         if(MDVFLG .eq. 0) then         
            dx=fhdrReal(MDVFGDX)
            dy=fhdrReal(MDVFGDY)
         else
            dx = mdvsp
            dy = mdvsp
         endif


         if(MDVFLG .eq. 0) then 
            x1=fhdrReal(MDVFXMIN)
            y1=fhdrReal(MDVFYMIN)
         else
            x1 = mdvxmin
            y1 = mdvymin
         endif

CThe fields do not need to have the same number of 
Clevels and they do not need to be on the same
Ctype of level.  If we have a level header then 
Cwe have saved the mininum level value for each
Cfield.  We use this information to determine z1.
         if(lhincl .eq. 1) then 
            do i = 1, NFLD
               if(NFLDLVLS(i) .eq. mhdrints(22)) then
                  z1 = MINFLDLVL(i)
                  if(FLDLVLSP(i) .lt. 0) then
                     dz = -FLDLVLSP(i)
                  else
                     dz = FLDLVLSP(i)
                  end if
               end if
            end do
          else          !We do not have level information
             z1 = fhdrReal(MDVFZMIN)
             dz = fhdrReal(MDVFGDZ)
          end if


         IGENSCL = 100
         ihed(160) = NINT(x1 * IGENSCL)
         ihed(165) = NINT(y1 * IGENSCL)
         ihed(170) = NINT(z1 * 1000)
         
         if(MDVFLG .eq. 0) then 
            x2=x1+(nx-1)*dx
            y2=y1+(ny-1)*dy
         else
            x2 = mdvxmax
            y2 = mdvymax
         endif
         z2=z1+(nz-1)*dz

         ihed(161) = NINT(x2 * IGENSCL)
         ihed(166) = NINT(y2 * IGENSCL)
         ihed(171) = NINT(z2 * 1000.)


         vallev(1) = z1
         vallev(nz) = z2
         do i = 2,nz
            vallev(i) = vallev(1) + (i - 1)*dz
         end do

         ispcx=nint(1000*dx)
         ispcy=nint(1000*dy)
         ispcz=nint(1000*dz)
         ihed(163) = ispcx
         ihed(168) = ispcy
         ihed(173) = ispcz

         ihed(301) = nx * ny
         ihed(96)  = (ihed(301) - 1)/(3200 - 1)
         ihed(97) = ihed(96) * ihed(175)
         ihed(98) = ihed(97) * nz
         ihed(99) = ihed(98) + nz + 1

C---------------------------------------------------------------------
C FIELD INFORMATION
C---------------------------------------------------------------------
         CALL MDVCINFO(ihed,NID)
         IFLD = 176
         DO 100 I = 1,NFLD
            call pfldsum(I,iscale,gfield(i))
            READ(GFIELD(I),15),(ihed(IFLD + (I - 1)*5+ J-1),J=1,4)
            IHED(IFLD + (I - 1)*5 + 4) = iscale
 100  CONTINUE


C---------------------------------------------------------------------
C LANDMARK INFORMATION
C---------------------------------------------------------------------
      ihed(302) = 2  
      NUMLND = 2   
      ihed(303) = 1
      NAMLND(1) = 'ORIGIN'
      NAMLND(2) = '      '
      ILND=306
      ZLND(1) = 0
      ZLND(2) = ALTITUDE
      XLND(1) = 0
      YLND(1) = 0
      XLND(2) = 0
      YLND(2) = 0
      DO 200 I = 1,NUMLND
         READ(NAMLND(I),250)(ihed(ILND + (I-1)*6 + J-1),J= 1,3)
         ihed(ILND + (I - 1)*6 + 3) = XLND(I)
         ihed(ILND + (I - 1)*6 + 4) = YLND(I)
         ihed(ILND + (I - 1)*6 + 5) = ZLND(I)
 250     FORMAT(3A2)
 200  CONTINUE

         
Cclose the mdv file.
       call mdvclose("in")
       return
       end



C*****************************************************************
         subroutine ldscanty(ihed,nid,scantype,projection)

         integer ihed(nid),scantype,swap,projection
         character*2 cscan,name(2)

C  The vertical types availabe on MDV tapes are in MDV_MARCOS.h
         if(scantype .eq. 0) then
            cscan =  'CR'
            read(cscan,10),ihed(16)
            cscan = 'T '
            name(1) = "CR"
            name(2) = "T "
            read(cscan,10),ihed(17) 
         else if(scantype .eq. 3 .and. projection .eq. 0) then
            print *,"Lat/Lon projection"
            print *,"Vertical type is Pressure(mb)"
            cscan =  'LL'
            read(cscan,10),ihed(16)
            cscan = 'P '
            read(cscan,10),ihed(17)             
            name(1) = "LL"
            name(2) = "P "
         else if(scantype .eq. 4) then
            cscan =  'CR'
            read(cscan,10),ihed(16)
            cscan = 'T '
            read(cscan,10),ihed(17)
            name(1) = "CR"
            name(2) = "T "
         else if (scantype .eq. 9) then
            print *,"Scantyp is elevation angles"
            if( projection .eq. 8) then
               print *,"Projection type is Cartesian, x,y in km"
               cscan = 'XY'
               read(cscan,10),ihed(16)
               cscan = 'E '
               read(cscan,10),ihed(17)
               name(1) = "XY"
               name(2) = "E "
            else
               cscan =  'EL'
               read(cscan,10),ihed(16)
               cscan = 'EV'
               read(cscan,10),ihed(17) 
               name(1) = "EL"
               name(2) = "EV"
            endif
         else if (scantype .eq. 8) then
               cscan =  'CR'
               read(cscan,10),ihed(16)
               cscan = 'T '
               name(1) = "CR"
               name(2) = "T "
               read(cscan,10),ihed(17) 
         else
            PRINT *,"MDV COORDINATE SYSTEM IS: ",scantype
            PRINT *,"COORDINATE SYSTEM CANNOT BE HANDELED BY CEDRIC"
            stop
         endif

 10         format(A2)

         return
         end
         
C*****************************************************************
         subroutine mdvgordr(ihed,mdvorder,nid)

         integer ihed(nid),mdvorder
         

CThe following grid orderings are defined in mdv_macros.h
C  MDV_ORDER_XYZ   0
C  MDV_ORDER_YXZ   1
C  MDV_ORDER_XZY   2
C  MDV_ORDER_YZX   3
C  MDV_ORDER_ZXY   4
C  MDV_ORDER_ZYX   5

CThe cedric header ihed(164) = x coordinate index number
CThe cedric header ihed(169) = y coordinate index number
CThe cedric header ihed(174) = z coordinate index number
        if(mdvorder .eq. 0) then
           ihed(164) = 1
           ihed(169) = 2
           ihed(174) = 3
        else if(mdvorder .eq. 1) then
           ihed(164) = 2
           ihed(169) = 1
           ihed(174) = 3
        else if(mdvorder .eq. 2) then
           ihed(164) = 1
           ihed(169) = 3
           ihed(174) = 2
        else if(mdvorder .eq. 3) then
           ihed(164) = 3
           ihed(169) = 1
           ihed(174) = 2
        else if(mdvorder .eq. 4) then
           ihed(164) = 3
           ihed(169) = 1
           ihed(174) = 2  
        else if(mdvorder .eq. 5) then
           ihed(164) = 3
           ihed(169) = 2
           ihed(174) = 1 
        end if
         return
         end  

C------------------------------------------------------------------------  
       SUBROUTINE MDVCINFO(ihed,NID)
     
       integer IHED(NID)
       CHARACTER*80 PERSON
       CHARACTER*6 SCINAME,TAPE,program
       CHARACTER*4 project
       CHARACTER*8 temp

       temp = "CEDRIC"
       read(temp,25)(ihed(i),i=5,7)
       project = '    '
       SCINAME = '      '
       TAPE    = '      '
       program  = 'CEDRIC  ' 
       project  = '      '      
      read(program,25)(ihed(i),i=5,7)
      read(project,25)(ihed(i),i=8,9)
      READ(SCINAME,25)(IHED(I),I=10,12)
      READ(TAPE,25)(IHED(I),I=18,20)
      CALL GETENV('QSUB_REQNAME',PERSON)
      READ(PERSON,25)(IHED(I),I=45,47)
      PERSON = '       '
      CALL GETENV('LOGNAME',PERSON)
      READ (PERSON,25)(IHED(I),I=48,50)
 24   FORMAT(A2)
 25   FORMAT(3A2)
 26   FORMAT(2A2)

      RETURN
      END
C------------------------------------------------------------------------ 

       SUBROUTINE FTCHMDV(RBUF,NLEV,NUMFLD,NX,NY,orignx,
     x                    origny,INUNIT)

       INCLUDE 'CEDRIC.INC'
       include "./mf_mdv.inc"
       COMMON /LEVELS/ VALLEV(MAXZLEV),VALNYQ(MAXZLEV),VNYQ_VOL
       COMMON /MDVGRIB/ MDVXMIN,MDVXMAX,MDVYMIN,MDVYMAX,MDVSP,
     x                  MDVFLG,GRIBFLG
       REAL  MDVXMIN,MDVXMAX,MDVYMIN,MDVYMAX,MDVSP
       INTEGER MDVFLG,SRCFLG,orignx,origny,GRIBFLG

       DIMENSION RBUF(NX,NY)
       REAL RIBUF(NX*NY)
       REAL RBUF2(orignx * origny),rtemp
       REAL OLDGRID(6),arg1
       integer oldnx,oldny,reverse
       integer INUNIT,NLEV,NUMFLD,RDIM,INDEX
       integer   fieldnum,NUMPTS
       integer   HDF
       integer   toprow,row1,row2,n,m
       integer   temp1(orignx),temp2(orignx)
       integer   nrows,ncols

       if(nx .gt. MAXX) then
          print *,"the number of points in the x direction > ",MAXX
          STOP
       ENDIF 

       if(ny .gt. MAXY) then
          print *,"the number of points in the y direction > ",MAXY
          STOP
       ENDIF 

       HDF = 2
       
       RDIM = orignx * origny
       do i = 1,RDIM
            rbuf2(i) = -32768.
       end do 
       

       do i = 1,nx*ny
            ribuf(i) = -32768.
       end do 

         fieldnum = numfld - 1
         numpts = NX*NY
         call gmdvdata(RBUF2,fieldnum,numpts,nlev,mdvflg,
     X                 srcflg,oldgrid,reverse)

         if(reverse .eq. 1) then
            nrows = origny
            ncols = orignx
            toprow = nrows;
            do j = 1, nrows/2
               row1 = 1 + ((j-1) * ncols)
               row2 = (toprow * ncols) - (ncols - 1)
               m = row1
               n = row2
               do i = 1,ncols
                  temp1(i) = rbuf2(m)
                  m = m + 1
                  temp2(i) = rbuf2(n)
                  n = n + 1
               enddo

               do i = 1,ncols 
                  rbuf2(row1) = temp2(i)
                  row1 = row1 + 1
                  rbuf2(row2) = temp1(i)
                  row2 = row2 + 1
               enddo
               toprow = toprow - 1
            end do
         endif

                
         icount = 1
         IF(MDVFLG .EQ. 1) THEN
            arg1 = OLDGRID(2) - OLDGRID(1)
            OLDNX = (arg1) * (1/OLDGRID(5))
            OLDNX = OLDNX + 1
            arg1 = OLDGRID(4) - OLDGRID(3)
            OLDNY = (arg1) * (1/OLDGRID(6))
            OLDNY = OLDNY + 1

C            if(fieldnum .eq. 0 .and. nlev .eq. 1) then 
               CALL MDVINTRP(NX,NY,oldnx,oldny,srcflg,
     X                       OLDGRID,RBUF2,RIBUF)
C            endif
            do JJ = 1,NX
               do II = 1,NY
                  rbuf(jj,ii) = RIBUF(icount)
                  icount = icount + 1
                enddo
             enddo
          ELSE
            do ii = 1,NY
               do jj = 1,NX
                  rbuf(jj,ii) = RBUF2(icount)
                  icount = icount + 1
               enddo
            enddo 
        ENDIF    

      RETURN
      END
C------------------------------------------------------------
       SUBROUTINE MDVINTRP(NEWNX,NEWNY,OLDNX,OLDNY,srcflg,
     X                     OLDGRID,RBUF2,RIBUF)

       INCLUDE 'CEDRIC.INC'
       include "./mf_mdv.inc"
       COMMON /MDVGRIB/ MDVXMIN,MDVXMAX,MDVYMIN,MDVYMAX,MDVSP,
     X                  MDVFLG,GRIBFLG

       REAL  MDVXMIN,MDVXMAX,MDVYMIN,MDVYMAX,MDVSP
       INTEGER MDVFLG,srcflg,GRIBFLG
       REAL ANGXAX
       REAL WXY(NEWNX,NEWNY,3),IXY(NEWNX,NEWNY,3),OLDGRID(6)
       REAL X1,X2,XDI,Y1,Y2,YDI,rl,rm,xval,yval
       REAL VALX,VALY,FVAL,VALL
       REAL AL,BL,CL
       INTEGER OLDNX,OLDNY,INDEX,OUTINDEX,HDF
       INTEGER L,M,jj,ii
       REAL BAD
       REAL  RBOT(OLDNX,OLDNY),RBUF2(OLDNX*OLDNY)
       REAL  RIBUF(NEWNX*NEWNY)
       REAL VAL(4)

       DATA EPS/0.000001/
       DATA SF/100./        
       HDF = 2

      print *,'MDVINTRP:       nx,ny=',nx,ny
      print *,'MDVINTRP: newnx,newny=',newnx,newny
      print *,'MDVINTRP: oldnx,oldny=',oldnx,oldny
       if(srcflg .eq. HDF) then
          jbeg = OLDNY
          jend = 1
          jinc = -1
       else
          jbeg = 1
          jend = OLDNY
          jinc = 1
       endif

       BAD = -32768.
       OUTINDEX = 1
       INDEX = 1
       DO JJ = jbeg,jend,jinc
          DO II = 1,OLDNX
             RBOT(II,JJ) = RBUF2(INDEX)
             if(rbot(ii,jj) .ne. -32768.) then
C                 print *,ii,jj,rbot(ii,jj)
C                 call sleep(1)
             endif
             INDEX = INDEX + 1
          enddo
       enddo


       X1=OLDGRID(1)
       X2=OLDGRID(2)
       XDI=1/OLDGRID(5)
       Y1=OLDGRID(3)
       Y2=OLDGRID(4)
       YDI=1/OLDGRID(6) 

       ANGXAX = 90.
       DO 20 J=1,NEWNY
          DO 30 K=1,NEWNX
               WXY(K,J,1)=0.0
               WXY(K,J,2)=0.0
               WXY(K,J,3)=0.0
               IXY(K,J,1)=0
               IXY(K,J,2)=0
               IXY(K,J,3)=0
CNEW LAT LON VALUES FOR EACH GRID POINT IN THE NEW GRID               
               xval = MDVXMIN + (k - 1)* MDVSP
               yval = MDVYMIN + (j - 1)* MDVSP
CCALCULATE THE INDICES L AND M AND THE WEIGHTS
               RL = (xval-X1)*XDI + 1.0+EPS
               RM = (yval-Y1)*YDI + 1.0+EPS
               l  = INT(rl)
               m  = INT(rm)
               WXY(K,J,1) = RL - INT(RL)
               WXY(K,J,2) = RM - INT(RM)
               WXY(K,J,3) = 0.0
               IXY(K,J,1)=L
               IXY(K,J,2)=M
               IXY(K,J,3)=1
 30         CONTINUE
 20      CONTINUE  

         nz = 1
         DO 130 K=1,NEWNX
            DO 120 J=1,NEWNY
               FVAL=BAD
               L=IXY(K,J,1)
               M=IXY(K,J,2)
               N=IXY(K,J,3)
               IF ((L.LT.1 .OR. L.GT.OLDNX) .OR.
     X             (M.LT.1 .OR. M.GT.OLDNY) .OR.
     X             (N.LT.1 .OR. N.GT.NZ)) THEN
                    VALL = BAD
                    GOTO 200
               ENDIF
               AL=WXY(K,J,1)
               BL=WXY(K,J,2)
               CL=WXY(K,J,3)
C     
C     2-D INTERPOLATION
C     
C     
C     IF WE ARE OUTSIDE OF ORIGINAL GRID IN ANY DIRECTION, DON'T INTERP.
C     
               IF (L.EQ.OLDNX) THEN
                   IF (AL.LT.1.0E-5) THEN
                        L=OLDNX-1
                        AL=1.0 - 1.0E-5
                   ELSE IF (AL.GT.1.0E-5) THEN
                        VALL = BAD
                        GOTO 200
                   END IF
               END IF

               IF (M.EQ.OLDNY) THEN
                   IF (BL.LT.1.0E-5) THEN
                        M=OLDNY-1
                        BL=1.0 - 1.0E-5
                   ELSE IF (BL.GT.1.0E-5) THEN
                        VALL = BAD
                        GOTO 200
                   END IF
               END IF


C     
C     OBTAIN FOUR VALUES FROM BOTTOM LEVEL
C     
               VAL(1)=RBOT(L,M)
               VAL(2)=RBOT(L+1,M)
               VAL(3)=RBOT(L,M+1)
               VAL(4)=RBOT(L+1,M+1)
C               print *,VAL(1),VAL(2),VAL(3),VAL(4)
C
C     DO THE INTERPOLATION
C
                  IC1=0
                  IF (VAL(1).NE.BAD .AND. VAL(2).NE.BAD) THEN
C
C     BILINEAR METHOD
C
                     VALX=VAL(1)*(1-AL) + VAL(2)*AL
                  ELSE
C                    VALX = BAD
C                     IC1=1
                 endif


                  IC2=0
                  IF (VAL(3).NE.BAD .AND. VAL(4).NE.BAD) THEN
                     VALY=VAL(3)*(1-AL) + VAL(4)*AL
                  ELSE
                     IC2 = 1
                     VALY =BAD
                  ENDIF
 

C                  PRINT *,"valx and valy  ",VALX,VALY 

                  IF ((VALX.NE.BAD .AND. VALY.NE.BAD) .AND. 
     X                 (IC1.NE.1 .OR. IC2.NE.1)) THEN
                     VALL=VALX*(1-BL) + VALY*BL
C                     print *,VALL
                  ELSE
                     VALL = BAD
                  ENDIF    

 200    CONTINUE

        IF(VALL .NE. BAD) THEN
           FVAL = VALL
        ELSE
           FVAL = BAD
        ENDIF 

           RIBUF(OUTINDEX) = FVAL
C           if(RIBUF(OUTINDEX) .ne. bad)print *,outindex,RIBUF(OUTINDEX)
           OUTINDEX = OUTINDEX + 1
 120       CONTINUE
 130    CONTINUE

       return
       end
       
C------------------------------------------------------------
CPrepare the master header for writing to mdv output file.
      SUBROUTINE PREPMHD(LOUT,ID)
      include 'mf_mdv.inc'
      INCLUDE 'CEDRIC.INC'
      character*8 LOUTNM,thedate,thetime
      character*6 radarnam
      character*8 fldnm(NFMAX)
      INTEGER ID(NID)
      COMMON /LEVELS/ VALLEV(MAXZLEV),VALNYQ(MAXZLEV),VNYQ_VOL
      integer nfields
      integer lout
      integer numxyz(3),times(6),dates(6)
      real    position(3)
      real    temp(7)

      nfields   = ID(175)
CStart date and time
      dates(1) = ID(21)
      dates(2) = ID(22)      
      dates(3) = ID(23)
      times(1) = ID(24)
      times(2) = ID(25)
      times(3) = ID(26)
CEnd date and time
      dates(4) = ID(27)
      dates(5) = ID(28)      
      dates(6) = ID(29)
      times(4) = ID(30)
      times(5) = ID(31)
      times(6) = ID(32)


C radar lat, lon and altitude
      I1 = 33
      do I = 1,6
            temp(I) = ID(I1)
            I1 = I1 + 1
      end do
      temp(7) = ID(317)
      position(1)  = temp(1) + temp(2)/60. 
     x          + (temp(3)/3600.)/real(ID(68))

       neg = 0
       if(temp(4) .lt. 0) neg = 1
       position(2) = temp(4) + temp(5)/60. 
     x          + (temp(6)/3600.)/real(ID(68)) 
       position(3) = temp(7)/1000.

      numxyz(1) = ID(162)
      numxyz(2) = ID(167)
      numxyz(3) = ID(172)

      write(radarnam,5)(id(i),i = 13,15)
 5    format(3A2)

      print *,valnyq
C write out the master header and field headers.
      call wmdvmhdr(lout,numxyz,times,dates,position,
     x              valnyq,nfields,radarnam)

      return
      end

C-----------------------------------------------------------------

      subroutine wmdvfile(id)

      include 'mf_mdv.inc'
      INCLUDE 'CEDRIC.INC'
      integer*4 fldhints(MDVFSI32)
      real*4    fldhreal(MDVFFL32)
      real      sf
      integer   ID(NID),nf,badll,iscale(NFMAX)
      character*8 fldnm(NFMAX)
      integer   llflag,n
      COMMON /UNITS/ LIN,LOUT,LPR,LSPOOL
      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      COMMON /LEVELS/ VALLEV(MAXZLEV),VALNYQ(MAXZLEV),VNYQ_VOL


      nf = id(175)
      IFLD = 176
      do I = 1,nf
         fldnm(I) = '        '
         WRITE(fldnm(I),5)id(176+(I-1)*5),id(177+(I-1)*5),
     x                      id(178+(I-1)*5),id(179+(I-1)*5)
         
 5       FORMAT(4A2)
        iscale(I) = ID(IFLD + (I - 1)*5 + 4)
       enddo

      call prepfldh(fldhints,fldhreal,id)
      call cedwmdv(nf,fldhints,fldhreal,fldnm)

CFinish writing out the output file summary
      sf = 1./ID(68)
      n = ID(302)
      write(LPR,10) N,ID(303)
 10   format(/'  LANDMARKS PRESENT: ',I2,5X,'(',I2,' RADAR) ...'
     X   /4X,'NO.',3X,'NAME',6X,'X (KM)',4X,'Y (KM)',4X,'Z (KM)')
      K=306
      DO I=1,n
        R1=ID(K+3)*SF
        R2=ID(K+4)*SF
        R3=ID(K+5)*0.001
        write(LPR,15) I,ID(K),ID(K+1),ID(K+2), R1,R2,R3
 15     format(4X,I3,3X,3A2,2F10.2,F10.3)
        K=K+6
      END DO
      IF(ID(303).NE.1) GO TO 105
C
C        WRITE OUT RADAR SPECS IF SINGLE RADAR
C
      R1=ID(304)*SF
      R2=ID(305)*SF
      WRITE(LPR,20) R1,R2
 20   FORMAT('  NYQUIST VELOCITY:',F8.2,7X,'RADAR CONSTANT:',F8.2)
 105  CONTINUE

      R1=ID(35)*SF
      R2=ID(38)*SF
      CALL PRINTLL(BADLL)
      IF(BADLL .EQ. 1) THEN
         WRITE(LPR,25) ID(33),ID(34),R1,ID(36),ID(37),R2
 25   FORMAT(/'  ORIGIN  LATITUDE:',I8,' DEG',I6,' MIN',F9.2,' SEC'
     X       /9X,      'LONGITUDE:',I8,' DEG',I6,' MIN',F9.2,' SEC')

      END IF


      WRITE(LPR,30)
 30   FORMAT(/'  CARTESIAN COORDINATE SYSTEM SPECIFICATIONS...'
     X/3X,'AXIS',11X,'MINIMUM    ',5X,'MAXIMUM     ',5X,'DELTA ',
     X7X,'NO. OF PTS.')
      K=160
C        CALCULATE KILOMETERS FROM METERS FOR Z-AXIS
      CKM=1.0
      DO I=1,3
C        MARK BRADFORD PATCH TO ACCOUNT FOR ID(68).NE.100
         IF(I.EQ.3) CKM=FLOAT(ID(68))/1000.
         R1=ID(K)*SF*CKM
         R2=ID(K+1)*SF*CKM
         R3=ID(K+3)*0.001
      WRITE(LPR,35) AXNAM(I),R1,LABAXS(I,1),R2,LABAXS(I,1),
     X                R3,LABAXS(I,1),ID(K+2)
 35   FORMAT(5X,A1,6X,F10.3,1X,A3,3X,F10.3,1X,A3,4X,F8.3,1X,A3,3X,I5)
         K=K+5
      END DO
      L1=ID(164)
      L2=ID(169)
      L3=ID(174)
      CF=1./ID(69)
      R1=ID(40)*CF
      XOR=ID(41)*SF
      YOR=ID(42)*SF
      PRINT *,"  "
      PRINT *,"  GENERAL SCALING FACTOR = ",ID(68)
      PRINT *,"    ANGLE SCALING FACTOR = ",ID(69)
      WRITE(LPR,40) AXNAM(L1),AXNAM(L2),AXNAM(L3),R1,XOR,YOR
 40   FORMAT(/3X,'AXIS ORDER IS   ',3A3,
     X    /3X,'ANGLE OF THE X-AXIS RELATIVE TO NORTH: ',F9.2,4X,'DEG.',
     X    /3X,'(X,Y)  AXIS ARE SPECIFIED RELATIVE TO:  (',
     X                F7.2,',',F7.2,')')

      WRITE(LPR,45) 
 45   FORMAT('MDV ENCODING TYPE IS: MDV_INT8') 
      return
      end

C------------------------------------------------------------
      subroutine prepfldh(fldhints,fldhreal,id)


      include 'mf_mdv.inc'
      INCLUDE 'CEDRIC.INC'
      integer*4 fldhints(MDVFSI32)
      real*4    fldhreal(MDVFFL32)
      integer   ID(NID)
      real      igenscl
      COMMON /UNITS/ LIN,LOUT,LPR,LSPOOL



      igenscl = real(id(68))
      fldhints(MDVFNXIX) = id(162)
      fldhints(MDVFNYIX) = id(167)
      fldhints(MDVFNZIX) = id(172)

CMin x,y and z values
      fldhreal(MDVFXMIN) = id(160)/igenscl
      fldhreal(MDVFYMIN) = id(165)/igenscl
      fldhreal(MDVFZMIN) = id(170)/1000.


C spacing in x,y and z
      fldhreal(MDVFGDX) = ID(163)/1000.
      fldhreal(MDVFGDY) = ID(168)/1000.
      fldhreal(MDVFGDZ) = ID(173)/1000.


      print *,'VOL. NAME=',(ID(I),I=101,104)
      WRITE(LPR,5) (ID(I),I=1,4),ID(117),ID(118),ID(116),
     X   (ID(I),I=71,74),(ID(I),I=10,12),(ID(I),I=119,121),
     X   (ID(I),I=13,15),(ID(I),I=48,50),(ID(I),I=125,127),
     X   (ID(I),I=5,7),  (ID(I),I=51,54),(ID(I),I=101,104),
     X    ID(8),ID(9),(ID(I),I=55,58),(ID(I),I=16,20),
     X    (ID(I),I=45,47)

 5    FORMAT(/' MUDRAS (.MUD)  VOLUME HEADER',15X,4A2
     X   /'  GENERAL INFORMATION...'
     X/' DATE:      ',I2.2,2('/',I2.2),5X,
     X 'SOURCE:  ',4A2,3X,'SCIENTIST: ',3A2,
     X/' BEG TIME:  ',I2.2,2(':',I2.2),5X,
     X 'RADAR:   ',3A2,5X,'SUBMITTER: ',3A2,
     X/' END TIME:  ',I2.2,2(':',I2.2),5X,
     X 'PROGRAM: ',3A2,5X,'DATE RUN:  ',4A2,
     X/' VOL. NAME: ', 4A2,      5X,
     X 'PROJECT: ',2A2,7X,'TIME RUN:  ',4A2,
     X/' COORD SYS: ', 2A2,      9X,'TAPE:    ',3A2,5X,
     X 'SEQUENCE:  ',3A2)

      id(96)  = id(301)/3200
      id(97)  = id(96) * id(175)
      id(98)  = id(97)*id(106)
      id(99)  = id(98) + id(106) + 1


      WRITE(LPR,10) ID(62),ID(96),ID(301),ID(63),ID(97),ID(106),
     X                ID(65),ID(99),ID(67),ID(451),ID(452),ID(453)
 10   FORMAT(/'  DATA CHARACTERISTICS...'
     X/' COMPUTER:   ',3X,A2,5X,'RECS/FIELD:  
     X',I3,5X,'PTS/FIELD:  ',I6
     X/' BITS/DATUM: ',I5,   5X,'RECS/PLANE:  ',
     X  I5,5X,'NO. PLANES: ',I6
     X/' BLOCK SIZE: ',I5,   5X,'RECS/VOLUME: ',
     X I5,5X,'BAD DATA:   ',I6
     X/' WORDS/PLANE:',I5,   5X,'WORDS/FIELD: ',
     X I5,5X,'MAX FIELDS: ',I6)   
  

      N=ID(175)
      WRITE(LPR,15) N
 15   FORMAT(/'  FIELDS PRESENT: ',I2,' ...'
     X       /4X,'NO.',3X,'NAME',7X,'SCALE FACTOR',7x,'FIELD BIAS')


      return
      end
