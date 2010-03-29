      SUBROUTINE WRFOPN(IUNIT,IBUF,NST,IBTIM,IETIM)
C
C     THIS SUBROUTINE OPENS A NETCDF FILE FOR READING AND RETURNS HEADER
C
C      INCLUDE '/usr/local/netcdf/include/netcdf.inc'
      INCLUDE '/opt/local/netcdf-3/include/netcdf.inc'
      INCLUDE 'CEDRIC.INC'
      PARAMETER (MAXVARS = 50)
      PARAMETER (NCMAXDIM = 100)
      PARAMETER (NCMAXVAR = 2000)
      COMMON /CDFNET/ ICDFID(MXCDF),IDIMDAT(4,MXCDF),
     X     IDIMID(1,MXCDF),IVAR(NFMAX+1,MXCDF),IFILE,ISYNFLG,
     X     ICDUNT(MXCDF)

      COMMON /WRF/ WRFINTRP,CALPLACD,wrfdbg,XMIN,XMAX,YMIN,YMAX,ZMIN,
     X             ZMAX,DX,DY,DZ

C-----------------------------------------------------------------
CWRFDIMS is a common block which holds the various dimensions.
C UX,UY -- dimensions of the U wind in the original data 
C VX,VY -- dimensions of the V wind in the original data 
C WX,WY,WZ -- dimensions of the vertical wind in the original 
C             data 
C THERMX,THERMY,TZ -- dimensions of the thermodynamic variables
C UVZ  -- the number of vertical levels for the east-west and
C         north-south winds in the original data.
C numz -- the number of vertical levels
C minz -- the first vertical level
C
C Arakawa C-grid (http://meted.comet.ucar.edu/nwp/pcu2/ruchres1.htm)
C     This grid has basic (T) and staggered (U,V,W) grids within it:
C        (T) Thermodynamic - (T,BT,SN,WE) @ (dX,dY,dZ) resolutions
C        (U) EW wind   - (T,BT,SN,WE_st) ==> shift East  1/2 dX
C        (V) NS wind   - (T,BT,SN_st,WE) ==> shift North 1/2 dY
C        (W) Vert wind - (T,BT_st,SN,WE) ==> shift Up    1/2 dZ
C            Note: Winds are parallel to the grid and may not be 
C                  true meteorological winds.
C
C             Y-direction
C               ^
C               | DV/DY @ T-grid pt
C               |       ^
C               |       |
C               V       V       V
C               |       |            
C               |       |            
C               T   U---T---U   T ==> DU/DX @ T-grid pt
C               |       |            
C               |       |            
C               V       V       V       
C               |                             
C               |                            
C               T---U---T---U---T---> X-direction
C         
C-----------------------------------------------------------------
      COMMON /WRFDIMS/ UX,UY,VX,VY,WX,WY,THERMX,THERMY,UVZ,WZ,TZ,
     X                 numz,minz


      COMMON /LEVELS/ VALLEV(MAXZLEV),VALNYQ(MAXZLEV),VNYQ_VOL
CITEM IS A 510 WORD ARRAY TO STORE HEADER VALUES FOR PLACING IN
CIBUF
      INTEGER ITEM(NID)
      INTEGER IBUF(NID)
      INTEGER DIMCNT,IGENSCL,iscale
      INTEGER CUNIT
      INTEGER NX,NY,NZ,NFLD,I,wrfdbg
      INTEGER IFLD,NTIMES,TIMESTR,IBTIM,IETIM
      INTEGER UX,UY,VX,VY,WY,WX,wind,uvz,WZ,TZ
      integer thermx,thermy,numz,minz,calplacd
      REAL XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,DX,DY,DZ
      CHARACTER*8 FLDNAM(NFMAX)
      CHARACTER*8 ctemp,VOLNAME
      CHARACTER*2 CSCAN1,CSCAN2
      INTEGER DTTM(6)

      IFILE=IFILE+1
      ICDUNT(IFILE)=IUNIT
      IGENSCL = 100

      DO I = 1,NID
         ITEM(I) = 0
      END DO
      CALL COPENWRF(CUNIT,IUNIT,DIMCNT) 
      ICDFID(IFILE) = CUNIT
      CALL RWRFDIMS(DIMCNT,NX,NY,NZ,NTIMES,TIMESTR)

c NOTE: FOR WRF THE IETIM IS REALLY THE DATE THE
C       USER IS INTERESTED IN SINCE THERE CAN BE
C       MORE THAN ONE DAY IN A WRF DATA SET.

      CALL WRFDATE(DTTM,NTIMES,TIMESTR,IBTIM,VOLNAME)
      CALL LDTIMES(DTTM,ITEM,VOLNAME)

CGET ALL OF THE DIFFERENT FIELD DIMENSIONS.  These dimensions
Care passed into the rest of the fortran in this file in 
Cthe common block wrfdims
      wind = 1 !U
      CALL winddim(ux,uy,uvz,wind)    
      wind = 2 !V
      CALL winddim(vx,vy,uvz,wind) 
      wind = 3 !W
      CALL winddim(wx,wy,wz,wind)  
      call thermodim(thermx,thermy)
      call gthermoz(tz)
C
C   
CSet up the Grid
      if(wrfintrp .ne. 0) then
         ITEM(160) = xmin * 100                   !xmin value
         ITEM(161) = xmax * 100                   !xmax value
         ITEM(162) = ((xmax - xmin)/dx) + 1  !number of x grid points
         ITEM(163) = dx * 1000               !xspacing in meters
      else         
         ITEM(160) = 0
         ITEM(161) = NX*100
         ITEM(162) = NX
         ITEM(163) = 1000
      endif
      ITEM(164) = 1


      minz = zmin
      vscale = 1.
      if(wrfintrp .eq. 2) vscale = 1000.
      IF(WRFINTRP .GT. 0) THEN
        VALLEV(1) = zmin/vscale
        nz = (zmax - zmin)/dz
        nz = nz + 1
        numz = nz
        do i = 2,nz
           VALLEV(i) = vallev(i - 1) + (dz/vscale)
        enddo
      ENDIF


      if(WRFINTRP .EQ. 0) THEN
         j = 1
         dz = 1000.
         vscale = 1000.
         DO I = 1,NZ
            VALLEV(I) = J
            j = i + 1
         ENDDO
      endif

      ITEM(165) = 0
      ITEM(166) = NY*100
      ITEM(167) = NY
      ITEM(168) = 1000 
      ITEM(169) = 2
      ITEM(154) = NZ
      ITEM(106) = NZ
      ITEM(170) = VALLEV(1)*vscale
      ITEM(171) = vallev(nz)*vscale
      ITEM(172) = NZ
      ITEM(173) = dz
      ITEM(174) = 3
      ITEM(301) = NX*NY
      IFLD = 176
      ITEM(96)  = ITEM(301)/3200
      IF(ITEM(96) .EQ. 0) ITEM(96) = 1
      ITEM(97)  = ITEM(96) * ITEM(175)
      ITEM(98)  = ITEM(97)*ITEM(106)
      ITEM(99)  = ITEM(98) + ITEM(106) + 1



CGet the user input field names.
      CALL FIELDNMS(FLDNAM,NFLD) 
      ITEM(175) = NFLD

      DO  I = 1,NFLD
         CALL CHKFLDN(FLDNAM(I),I,ISCALE)
         READ(FLDNAM(I),10),(ITEM(IFLD + (I - 1)*5+ J-1),J=1,4)
 10      FORMAT(4A2)
         ITEM(IFLD + (I - 1)*5 + 4) = ISCALE
      END DO      

      write(*,35)
 35   FORMAT(/)

      if(wrfdbg .eq. 1) then
c         print *,'---STATISTICS FOR WRF VARIABLES USED TO CALCULATE',
c     +        ' DIAGNOSTIC FIELDS---'
         print *,' '
        CALL FSTATS()
      endif

      CTEMP = "WRFMODEL"
      READ(CTEMP,25)(ITEM(I),I=1,4)

      CTEMP = "CEDRIC"
      READ(CTEMP,10)(ITEM(I),I=5,7)

      CTEMP = "WRF  "
      READ(CTEMP,15)(ITEM(I),I=8,9)     

      CTEMP = "??????"
      READ(CTEMP,30)(ITEM(I),I=10,12)

      CTEMP = "NONE  "
      READ (CTEMP,30)(ITEM(I),I=13,15)  

      CTEMP = "WRF  " 
      READ(CTEMP,25)(ITEM(I),I=71,74)  

      CTEMP = "     "
      CALL GETENV('LOGNAME',CTEMP)
      READ (CTEMP,30)(ITEM(I),I=48,50)    
 
    
 15   FORMAT(2A2)
 25   FORMAT(4A2)
 30   FORMAT(3A2)

      ITEM(40) = 90 * 64
      ITEM(41) = 0
      ITEM(42) = 0
      ITEM(61) = 510
      ITEM(63) = 16
      ITEM(64) = 2
      ITEM(65) = 3200
      ITEM(67) = -32768
      ITEM(68) = 100
      ITEM(69) = 64
      ITEM(39) = 0

      CSCAN1 = "WR"
      CSCAN2 = "F "
      READ(CSCAN1,20) ITEM(16)
      READ(CSCAN2,20) ITEM(17)
 20   FORMAT(A2)

      DO I=1,510
         IBUF(I)=ITEM(I)
      END DO
      END
C*********************************************************************************
      SUBROUTINE LDTIMES(DTTM,ITEM,VOLNAME)


      INCLUDE 'CEDRIC.INC'
      INTEGER ITEM(NID)
      INTEGER DTTM(6)
      CHARACTER*8 VOLNAME
      CHARACTER*8 THEDATE,THETIME

C     DATA YEAR
      ITEM(21)  = dttm(1)
      ITEM(27)  = dttm(1)
      ITEM(116) = dttm(1)
      if(item(116) .gt. 100) item(116) = MOD(item(116),100)

      ITEM(122) = dttm(1)

C     DATA MONTH
      ITEM(22)  = DTTM(2)
      ITEM(28)  = DTTM(2)
      ITEM(117) = DTTM(2)   
      ITEM(123) = DTTM(2)

C     DATA DAY
      ITEM(23)  = DTTM(3)
      ITEM(29)  = DTTM(3)
      ITEM(118) = DTTM(3)
      ITEM(124) = DTTM(5)


C     DATA TIME
      ITEM(119) = DTTM(4)/10000
      itmp = DTTM(4) - ITEM(119)*10000
      itmp = itmp/100
      ITEM(120) = itmp
      ITEM(121) = 0

      ITEM(24)  = DTTM(4)/10000
      ITEM(25)  = itmp
      ITEM(26)  = 0     

      ITEM(30)  = DTTM(4)/10000
      ITEM(31)  = itmp
      ITEM(32)  = 0     
       
      ITEM(125)  = DTTM(4)/10000
      ITEM(126)  = itmp
      ITEM(127)  = 0        


      READ(VOLNAME,10)(ITEM(I),I=101,104)
 10      FORMAT(4A2) 


C     CURRENT DATE AND TIME
      CALL RUNDT(THEDATE,THETIME)
      READ(THEDATE,10)(ITEM(I),I = 51,54)
      READ(THETIME,10)(ITEM(I),I = 55,58)

      RETURN
      END
C*********************************************************************************

      SUBROUTINE FTCHWRF(NLEV,NUMFLD,NX,NY,NZ,IHED,NAMFLD)
C
C---------------------------------------------------------------------------------
C  DATE: August 2002.
C        Routine FTCHWRF either reads or calculates variables that have been
C        requested with the GETVAR command to be input to the CEDRIC edit file.
C  Note: WRF mixing ratios (QVAPOR, QCLOUD, QICE, QSNOW, QRAIN, and QGRAUP) 
C        are converted from gm/gm to gm/kg.
C  Calculated variables not within the WRF netCDF file:
C
C     Routine calctheta for THETA (potential temperature in degrees K)
C                       for TC (temperature in degrees C)
C             calcp     for P (pressure in mb)
C             calcdewpt for TD (dew point in degrees C)
C             calcslvl  for SLVL (surface pressure in mb).
C             calcref   for ZRAIN and ZTOTAL
C                       (reflectivities in dB from mixing ratios)
C             relhumid  for RH (relative humidity in percent)
C
C  All other variables that appear in the GETVAR list are assumed to be in the 
C  WRF netcdf file.  WRF rho-coupled winds (RHO_U, RHO_V, RW) will be decoupled 
C  from density when (U,V,W) are requested.  Winds are always destaggered to
C  the thermodynamic or mass grid.
C
C  The variable calplacd which is in the WRF common block is set to 0
C  in this routine.  This is so PLACED will not be called in READVL
C  after returning from this routine.  The subroutine PLACED is instead
C  called in this subroutine for each variable asked for.  
C
C   In this subroutine RBUF is the array which holds the final values
C   of the variable asked for.  It is sent to PLACED where the data values
C   are put into the CEDRIC edit file on disk.
C---------------------------------------------------------------------------------
C
      INCLUDE 'CEDRIC.INC'
      PARAMETER (NCMAXDIM = 100)
      COMMON /WRFDIMS/ UX,UY,VX,VY,WX,WY,THERMX,THERMY,UVZ,WZ,TZ,
     x                  numz,minz
      COMMON /WRF/ WRFINTRP,CALPLACD,wrfdbg,XMIN,XMAX,YMIN,YMAX,
     X             ZMIN,ZMAX,DX,DY,DZ
      integer wrfdbg
      INTEGER thermx,thermy
      INTEGER NX,NY,NZ,iname
      INTEGER NUMFLD,NLEV,PTYPE,varrhou,varrhov
      integer pstats
      integer calplacd,vdims
      integer ux,uy,vx,vy,wx,wy,uvz,tz,numz,minz
      DIMENSION RBUF(NX*NY)
      dimension IHED(NID),ibuf(MAXPLN)
      REAL RBUF2(thermx,thermy),THETA(NX*NY),P(NX*NY)
      real bad
      real favg,fsqr,fstd,rmin,rmax
      CHARACTER*2 NAMFLD(4)
      DATA ZIPAK/-32768./
      
      calplacd = 0
      if(nlev .eq. 1) call opntemp()
      if(wrfintrp .gt. 0 .and. nlev .gt. 1) return
      favg = 0.0
      fsqr = 0.0
      FSTD = 0.0
      rmin = 1000.0
      rmax = -1000.0

CINITIALIZE THE RBUF ARRAY TO ALL BAD.
      DO I = 1,nx*ny
         RBUF(I)  = -1000.
      END DO

C---------CALCULATION OF DIAGNOSTIC VARIABLES------------
      if(namfld(1) .eq. "TH" .and. namfld(2) .eq. 'ET') then
         if(wrfintrp .gt. 0) then
            iname = 4
            call intrpwrf(iname,numfld,nlev,nx,ny,nz,ihed,rbuf)
         else
           call calctheta(nx,ny,nlev,rbuf,theta)
           call placed(0,ihed,nlev,numfld,ibuf,rbuf,nx,ny,3,bad,nst)
           if(nst .ne. 0) then
             print *,"error writing THETA to edit file"
             stop
           endif
         endif
         CALL WRFSTATS(NLEV,NAMFLD,NUMFLD,RBUF,NX,NY,NZ,BAD)
         return

C--------------------------------------------------------
      else if(namfld(1) .eq. "TC" .and. namfld(2) .eq. ' ') then
         if(wrfintrp .gt. 0) then
            iname = 13
            call intrpwrf(iname,numfld,nlev,nx,ny,nz,ihed,rbuf)
         else
            call calctheta(nx,ny,nlev,rbuf,theta)
            ptype = 2
            call calcp(ptype,nx,ny,nlev,p)
            do I = 1,nx*ny
               if(p(i) .ne. -1000.) then
                  rbuf(i) = p(i) * theta(i) - 273.16
               endif
             enddo
            call placed(0,ihed,nlev,numfld,ibuf,rbuf,nx,ny,3,bad,nst)
            if(nst .ne. 0) then
               print *,"error writing TC to edit file"
               stop
            endif             
         endif 
         CALL WRFSTATS(NLEV,NAMFLD,NUMFLD,RBUF,NX,NY,NZ,BAD)
         return

C--------------------------------------------------------
      else if(namfld(1) .eq. "TD" .and. namfld(2) .eq. ' ') then
         if(wrfintrp .gt. 0) then
            iname = 15
            call intrpwrf(iname,numfld,nlev,nx,ny,nz,ihed,rbuf)
         else
            call calcdewpt(nx,ny,nlev,rbuf)
            call placed(0,ihed,nlev,numfld,ibuf,rbuf,nx,ny,3,bad,nst)
            if(nst .ne. 0) then
               print *,"error writing TD to edit file"
               stop
            endif                         
         endif
         CALL WRFSTATS(NLEV,NAMFLD,NUMFLD,RBUF,NX,NY,NZ,BAD)
         return

C--------------------------------------------------------
      else if(namfld(1) .eq. "RH" .and. namfld(2) .eq. ' ') then
         if(wrfintrp .gt. 0) then
            iname = 16
            call intrpwrf(iname,numfld,nlev,nx,ny,nz,ihed,rbuf)
         else
            call relhumid(nx,ny,nlev,rbuf)
            call placed(0,ihed,nlev,numfld,ibuf,rbuf,nx,ny,3,bad,nst)
            if(nst .ne. 0) then
               print *,"error writing RH to edit file"
               stop
            endif                 
         endif
         CALL WRFSTATS(NLEV,NAMFLD,NUMFLD,RBUF,NX,NY,NZ,BAD)
         return

C--------------------------------------------------------
      else if(namfld(1) .eq. "P " .and. namfld(2) .eq. ' ') then
         if(wrfintrp .gt. 0) then
            iname = 14
            call intrpwrf(iname,numfld,nlev,nx,ny,nz,ihed,rbuf)
         else 
           ptype = 1
           call calcp(ptype,nx,ny,nlev,p)
           do I = 1,nx*ny
              if(p(i) .ne. -1000.) then
                 rbuf(i) = p(i) * 0.01
              endif
           enddo
         endif
         call placed(0,ihed,nlev,numfld,ibuf,rbuf,nx,ny,3,bad,nst)
         if(nst .ne. 0) then
            print *,"error writing Pressure to edit file"
            stop
         endif               
         CALL WRFSTATS(NLEV,NAMFLD,NUMFLD,RBUF,NX,NY,NZ,BAD)
         return

C--------------------------------------------------------
      else if(namfld(1) .eq. "W " .and. namfld(2) .eq. ' ') then
         if(wrfintrp .gt. 0) then
            iname = 1
            call intrpwrf(iname,numfld,nlev,nx,ny,nz,ihed,rbuf)
            CALL WRFSTATS(NLEV,NAMFLD,NUMFLD,RBUF,NX,NY,NZ,BAD)
         else
            if(nlev .gt. 1) return
            call wdestagr(nx,ny,nz,numfld,ihed)
         endif
         return

C--------------------------------------------------------
      else if(namfld(1) .eq. "U " .and. namfld(2) .eq. ' ') then
         if(wrfintrp .gt. 0) then
            iname = 1
            call intrpwrf(iname,numfld,nlev,nx,ny,nz,ihed,rbuf)
         else 
            if(nlev .gt. 1) return
            varrhou = 0
            call uwindops(nx,ny,nz,numfld,ihed,wrfdbg,varrhou)
         endif
         return

C--------------------------------------------------------
      else if(namfld(1) .eq. "RH" .and. namfld(2) .eq. "O_"
     X        .and. namfld(3) .eq. "U ") then
           if(nlev .gt. 1) return
           varrhou = 1
           call uwindops(nx,ny,nz,numfld,ihed,wrfdbg,varrhou)
           return
C--------------------------------------------------------
      else if(namfld(1) .eq. "V " .and. namfld(2) .eq. ' ') then
         if(wrfintrp .gt. 0) then
            iname = 2
            call intrpwrf(iname,numfld,nlev,nx,ny,nz,ihed,rbuf)
         else      
            if(nlev .gt. 1) return 
            varrhov = 0
            CALL vwindops(nx,ny,nz,numfld,ihed,wrfdbg,varrhov)
         endif
         return

C--------------------------------------------------------
      else if(namfld(1) .eq. "RH" .and. namfld(2) .eq. "O_"
     X        .and. namfld(3) .eq. "V ") then
           if(nlev .gt. 1) return
           varrhov = 1
           call vwindops(nx,ny,nz,numfld,ihed,wrfdbg,varrhov)
           return
C--------------------------------------------------------

      else if(namfld(1) .eq. "SL" .and. namfld(2) .eq. "VL") then
          calplacd = 0
          if(nlev .gt. 1) return
          call calcslvl(numfld,nx,ny,nz,ihed)
         return
C--------------------------------------------------------
      else if(namfld(1) .eq. "Z " .and. namfld(2) .eq. ' ') then
             iname = 11
             if(wrfintrp .gt. 0) then
                call intrpwrf(iname,numfld,nlev,nx,ny,nz,ihed,rbuf)
                return
             endif

       else if(namfld(1) .eq. "ZT" .and. namfld(2) .eq. "OT") then
             if(nlev .gt. 1) return
             iname = 17
             call calcref(iname,namfld,numfld,nx,ny,nz,wrfdbg,ihed)
             return
       else if (namfld(1) .eq. "ZR" .and. namfld(2) .eq. "AI") then
            if(nlev .gt. 1) return
            iname = 21
            call calcref(iname,namfld,numfld,nx,ny,nz,wrfdbg,ihed)
            return
       endif

C--------------------------------------------------------------
       CALL RWRFVARS(vdims,NUMFLD,RBUF2,NLEV,NAMFLD)

       pstats = 1
       if(namfld(1) .eq. "RT" .and. namfld(2) .eq. 'B') pstats = 0
       if(namfld(1) .eq. "RT" .and. namfld(2) .eq. 'P') pstats = 0
       if(namfld(1) .eq. "RR" .and. namfld(2) .eq. ' ') pstats = 0
c      if(namfld(1) .eq. "QV" .and. namfld(2) .eq. 'AP') pstats = 0
       
       bad = -1000.
       if (nlev .eq. 1 .and. pstats .eq. 1) then
          WRITE(*,20)
 20       FORMAT(/)
          write(*,10)NAMFLD
 10       FORMAT(14X,
     +         '---STATISTICS FOR WRF VARIABLE INPUT TO CEDRIC---',//,
     +         4A2,4X,'NLEV',10X,'MEAN',11x,'STDV',11x,'MIN',12X,'MAX')
       endif
C     
C     The Q's are of order 10^-3 == gm/gm. We convert to gm/kg by
C     the Q's * 1000. 
C
      qscale = 1.0
      if(NAMFLD(1)(1:1) .eq. "Q") qscale = 1000.
      if( nlev .gt. 1) return
      DO M = 1,NY
	 DO n = 1,NX
            j = (m - 1) * nx + n
            RBUF(j) = RBUF2(n,m) * qscale
            if(m .eq. 1 .and. n .eq. 1) then
               rmin = rbuf(j)
               rmax = rbuf(j)
            else
               if(rbuf(j) .lt. rmin) rmin = rbuf(j)
               if(rbuf(j) .gt. rmax) rmax = rbuf(j)
            endif
            favg = favg + RBUF(j)
            fsqr = fsqr + rbuf(j)**2
         ENDDO
      ENDDO
      favg = favg/(nx*ny)
      fsqr = fsqr/(nx*ny)
      FSTD=FSQR-FAVG**2
      IF(FSTD.LT.0.0) FSTD=0.0
      FSTD=SQRT(FSTD)      
      RLEV = 1.0
      if(pstats .eq. 1) THEN
         WRITE(*,15) rLEV,FAVG,fstd,rmin,rmax
      endif
 15   FORMAT(8X,F8.3,4F15.5)
      if(vdims .eq. 3) then
       do m = 1,nz
          call placed(0,ihed,m,numfld,ibuf,rbuf,nx,ny,3,bad,nst)
          if(nst .ne. 0) then
             print *,"error writing variable to edit file"
             stop
           endif
        enddo
      endif


      if(vdims .gt. 3) then
          favg = 0.0
          fsqr = 0.0
          call placed(0,ihed,nlev,numfld,ibuf,rbuf,nx,ny,3,bad,nst)
          if(nst .ne. 0) then
             print *,"error writing variable to edit file"
             stop
           endif
          do k = 2,nz
             rk = k
             CALL RWRFVARS(vdims,numfld,RBUF2,k,NAMFLD)
             DO M = 1,NY
                DO n = 1,NX
                   j = (m - 1) * nx + n
                   RBUF(j) = RBUF2(n,m) * qscale
                   if(m .eq. 1 .and. n .eq. 1) then
                      rmin = rbuf(j)
                      rmax = rbuf(j)
                   else
                      if(rbuf(j) .lt. rmin) rmin = rbuf(j)
                      if(rbuf(j) .gt. rmax) rmax = rbuf(j)
                   endif
                   favg = favg + rbuf(j)
                   fsqr = fsqr + rbuf(j)**2
                ENDDO
            ENDDO
            favg = favg/(nx*ny)
            fsqr = fsqr/(nx*ny)
            FSTD=FSQR-FAVG**2
            IF(FSTD.LT.0.0) FSTD=0.0
            FSTD=SQRT(FSTD)                      

            if(pstats .eq. 1) then
               WRITE(*,15) rk,FAVG,fstd,rmin,rmax
            endif
            call placed(0,ihed,k,numfld,ibuf,rbuf,nx,ny,3,bad,nst)
            if(nst .ne. 0) then
              print *,"error writing variable to edit file"
              stop
            endif
          end do !k loop
       endif     !vdim gt 3

      RETURN
      END


C*********************************************************************************
      subroutine udecouple(nx,ny,nz,NLEV,decouple)
C
C     Decouple the U-wind from density (RR - total dry density)
C
      COMMON /WRFDIMS/ UX,UY,VX,VY,WX,WY,THERMX,THERMY,UVZ,WZ,TZ,
     x                  numz,minz

      INCLUDE 'CEDRIC.INC'
      integer UX,UY,nx,ny,nz,thermx,thermy
      real density(thermx,thermy)      !RR
      real rhou(ux,uy)                 !RHO_U
      real decouple(ux,uy)
      integer nlev
      integer i,j,ii,im1
      real  arg1

      iname = 7   !RR
      call fvarid(iname,ivar)
      call wrfusrvar(ivar,nlev,density)

      iname = 1   !RHO_U
      call fvarid(iname,ivar)
      call wrfusrvar(ivar,nlev,rhou)

      DO j = 1,uy
         DO i = 1,ux
            ii  = min0(i,ux-1)
            im1 = max0(1,i-1)               
            arg1 = density(ii,j)+density(im1,j)
            if(arg1 .eq. 0) then
                   print *,"Unable to calculate u at ",i,j
                   print *,"Density value is 0"
                   stop
            endif
            decouple(i,j) = 2*rhou(i,j)/arg1
         end do
       end do

       return
       end
C*********************************************************************************


      subroutine uwindops(nx,ny,nz,numfld,ihed,debug,varrhou)

      COMMON /WRFDIMS/ UX,UY,VX,VY,WX,WY,THERMX,THERMY,UVZ,WZ,TZ,
     x                  numz,minz

      INCLUDE 'CEDRIC.INC'
      dimension ihed(NID)
      DIMENSION IBUF(MAXPLN)
      integer debug,varrhou
      integer nx,ny,thermx,thermy,ux,uy,numfld
      real density(nx,ny,nz)      !RR
      real rhou(ux,uy,nz)         !RHO_U
      real new(ux,uy,nz)
      real utemp(ux*uy)
      real rbuf(nx*ny)
      real temp(nx*ny)
      integer m,n,k
      integer i,j,ii,im1 
      integer version
      real  arg1
      real rlev,rmin,rmax
      DOUBLE PRECISION fstd,favg,fsqr


C If we have version one of the model we need to decouple the U field from
C the density RHO.
C If we have version two of the model U is already decoupled from RHO.
      call wrfversion(version)

      if(version .eq. 1) then 
      write(*,5) ux,uy
 5    format(/,"DENSITY COUPLED U: NX = ",i3," NY = ",I3)
      write(*,15)
 15   FORMAT(14X,
     +     '---STATISTICS FOR WRF VARIABLE INPUT TO CEDRIC---',//,
     +     'RHO_U',7X,'NLEV',10X,'MEAN',11x,'STDV',11x,'MIN',12X,
     +     'MAX')

      iname = 1   !U
      call fvarid(iname,ivar)
      do k = 1, nz
         rlev = k
         favg = 0.0
         fsqr = 0.0
         call wrfusrvar(ivar,k,utemp)
         i = 1
         DO M = 1,uy
	    DO n = 1,ux  
                rhou(n,m,k) = utemp(i)         
                favg = rhou(n,m,k) + favg
                fsqr = fsqr + rhou(n,m,k)**2
                if(m .eq. 1 .and. n  .eq. 1) then
                   rmin = rhou(n,m,k)
                   rmax = rhou(n,m,k)
                else
                   if(rhou(n,m,k) .lt. rmin) rmin = rhou(n,m,k)
                   if(rhou(n,m,k) .gt. rmax) rmax = rhou(n,m,k)
                 endif
                i = i + 1
            enddo
         enddo   
         favg = favg/(ux*uy)
         fsqr = fsqr/(ux*uy)
         FSTD=FSQR-FAVG**2
         IF(FSTD.LT.0.0) FSTD=0.0
         FSTD=SQRT(FSTD)   
         WRITE(*,20) rlev,favg,fstd,rmin,rmax
 20     FORMAT(8X,F8.3,4F15.5)

        if(varrhou .eq. 1) then
          do j=1,ny
          do i=1,nx
             index = (j - 1) * nx + i
             rbuf(index) =  rhou(i,j,k)
          enddo
          enddo
          call placed(0,ihed,k,numfld,ibuf,rbuf,nx,ny,3,
     x                bad,nst)
          if(nst .ne. 0) then
              print *,"error writing U destaggered data to edit file"
              stop
          endif
       endif          

      enddo
      if(varrhou .eq. 1) return


Cget the density
      iname = 7   !RR 
      call fvarid(iname,ivar)
      do k = 1,nz
         call wrfusrvar(ivar,k,temp)
         i = 1
         DO M = 1,ny
	    DO n = 1,nx 
                density(n,m,k) = temp(i)
                i = i + 1
            enddo
         enddo
      enddo

      
CDecouple U
      do k = 1,nz
         DO j = 1,uy
	    DO i = 1,ux
                 ii  = min0(i,ux-1)
                 im1 = max0(1,i-1)
                 arg1 = density(ii,j,k)+density(im1,j,k)
                 if(arg1 .eq. 0) then
                   print *,"Unable to calculate u at ",i,j,k
                   print *,"Density value is 0"
                   stop
                 end if
                 new(i,j,k) = 2*rhou(i,j,k)/arg1
            end do
         end do 
       enddo

       endif !End of the if for the test of version one

       if(version .eq. 2) then
          iname = 1   !U
          call fvarid(iname,ivar)
          do k = 1,nz
             call wrfusrvar(ivar,k,temp)
             index = 1
             DO j = 1,uy
	        DO i = 1,ux      
                   new(i,j,k) = temp(index)
                   index = index + 1
                enddo
             enddo  
          enddo
          
       endif

CDestagger U
       do k=1,nz
          do j=1,ny
          do i=1,nx
             new(i,j,k) = 0.5*(new(i,j,k)+new(i+1,j,k))
          enddo
          enddo
   
          do j=1,ny
          do i=1,nx
             index = (j - 1) * nx + i
             rbuf(index) =  new(i,j,k)
          enddo
          enddo
          call placed(0,ihed,k,numfld,ibuf,rbuf,nx,ny,3,
     x                bad,nst)
          if(nst .ne. 0) then
              print *,"error writing U destaggered data to edit file"
              stop
          endif
       enddo       

       RETURN
       END
C*********************************************************************************
      subroutine vdecouple(nx,ny,nz,NLEV,decouple)
C
C     Decouple the V-wind from density (RR - total dry density)
C
      COMMON /WRFDIMS/ UX,UY,VX,VY,WX,WY,THERMX,THERMY,UVZ,WZ,TZ,
     x                  numz,minz

      INCLUDE 'CEDRIC.INC'
      integer vy,vx,nx,ny,nz,thermx,thermy
      real density(thermx,thermy)      !RR
      real rhov(vx,vy)                !RHO_V
      real decouple(vx,vy)
      integer nlev
      integer i,j,jj,jm1
      real  arg1


      iname = 7   !RR
      call fvarid(iname,ivar)
      call wrfusrvar(ivar,nlev,density)

      iname = 2   !RHOV
      call fvarid(iname,ivar)
      call wrfusrvar(ivar,nlev,rhov)

       do j=1,vy
         jj  = min0(j,vy-1)
         jm1 = max0(1,j-1)
         do i=1,vx
            arg1 = density(i,jj)+ density(i,jm1)
            if(arg1 .eq. 0) then
               print *,"Unable to calculate v at ",i,j
               print *,"Density value is 0"
               stop
            end if
            decouple(i,j) = 2*rhov(i,j)/arg1
         end do
       end do
       RETURN
       END

C*********************************************************************************
      subroutine vwindops(nx,ny,nz,numfld,ihed,wrfdbg,varrhov)


      INCLUDE 'CEDRIC.INC'
      COMMON /WRFDIMS/ UX,UY,VX,VY,WX,WY,THERMX,THERMY,UVZ,WZ,TZ,
     x                  numz,minz

      dimension ihed(NID)
      DIMENSION IBUF(MAXPLN)      
      integer nx,ny,nz,vx,vy,numfld
      integer thermx,thermy,varrhov,wrfdbg
      real density(nx,ny,nz)   !RR
      real rbuf(nx*ny)
      real rhov(vx,vy,nz)         !RHO_U
      real vtemp(vx*vy)
      real temp(nx*ny)
      integer m,n
      integer version
      integer i,j,jj,jm1 
      real  arg1
      real rlev,rmin,rmax
      DOUBLE PRECISION fstd,favg,fsqr


C If we have version one of the model we need to decouple the U field from
C the density RHO.
C If we have version two of the model U is already decoupled from RHO.
      call wrfversion(version)

      if(version .eq. 1) then
      write(*,5) vx,vy
 5    format(/,"DENSITY COUPLED V: NX = ",i3," NY = ",I3)
      write(*,15)
 15   FORMAT(14X,
     +     '---STATISTICS FOR WRF VARIABLE INPUT TO CEDRIC---',//,
     +     'RHO_V',7X,'NLEV',10X,'MEAN',11x,'STDV',11x,'MIN',12X,
     +     'MAX')

      iname = 2   !V
      call fvarid(iname,ivar)
      do k = 1, nz
         rlev = k
         fav = 0
         fsqr = 0
         call wrfusrvar(ivar,k,vtemp)
         i = 1
         DO M = 1,vy
	    DO n = 1,vx  
                rhov(n,m,k) = vtemp(i)
                i = i + 1
                favg = rhov(n,m,k) + favg
                fsqr = fsqr + rhov(n,m,k)**2
                if(m .eq. 1 .and. n  .eq. 1) then
                   rmin = rhov(n,m,k)
                   rmax = rhov(n,m,k)
                else
                   if(rhov(n,m,k) .lt. rmin) rmin = rhov(n,m,k)
                   if(rhov(n,m,k) .gt. rmax) rmax = rhov(n,m,k)
                 endif
            enddo
         enddo   

         favg = favg/(vx*vy)
         fsqr = fsqr/(vx*vy)
         FSTD=FSQR-FAVG**2
         IF(FSTD.LT.0.0) FSTD=0.0
         FSTD=SQRT(FSTD)   
         WRITE(*,20) rlev,favg,fstd,rmin,rmax
 20     FORMAT(8X,F8.3,4F15.5)   


        if(varrhov .eq. 1) then
          do j=1,ny
          do i=1,nx
             index = (j - 1) * nx + i
             rbuf(index) =  rhov(i,j,k)
          enddo
          enddo
          call placed(0,ihed,k,numfld,ibuf,rbuf,nx,ny,3,
     x                bad,nst)
          if(nst .ne. 0) then
              print *,"error writing V destaggered data to edit file"
              stop
          endif
        endif    
      enddo 
      if(varrhov .eq. 1) return

CGet the density field
      iname = 7   !RR 
      call fvarid(iname,ivar)
      do k = 1,nz
         call wrfusrvar(ivar,k,temp)
         i = 1
         DO M = 1,ny
	    DO n = 1,nx 
                density(n,m,k) = temp(i)
                i = i + 1
            enddo
         enddo
      enddo
      
CDecouple v
      do k = 1,nz
        do j=1,vy
         jj  = min0(j,ny-1)
         jm1 = max0(1,j-1)
         do i=1,vx
              arg1 = density(i,jj,k)+ density(i,jm1,k)
              if(arg1 .eq. 0) then
                 print *,"Unable to calculate v at ",i,j
                 print *,"Density value is 0"
                 stop
              endif
              rhov(i,j,k) = 2*rhov(i,j,k)/arg1
         end do
        end do 
       end do
       endif !End for the version check

       if(version .eq. 2) then 
          iname = 2   !V
          call fvarid(iname,ivar)
          do k = 1,nz
             call wrfusrvar(ivar,k,temp)
             index = 1
             DO j = 1,uy
	        DO i = 1,ux      
C                   rhov(i,j,k) = temp(index)
                   index = index + 1
                enddo
             enddo  
          enddo
       endif

CDestagger v
       do k=1,nz 
       do j=1,ny 
        do i=1,nx
           rhov(i,j,k) = 0.5*(rhov(i,j,k)+rhov(i,j+1,k)) 
        enddo 
       enddo 
       enddo 


       do k=1,nz
          do j=1,ny
             do i=1,nx
               index = (j - 1) * nx + i 
               rbuf(index) = rhov(i,j,k)
             enddo
          enddo   

          call placed(0,ihed,k,numfld,ibuf,rbuf,nx,ny,3,
     x                bad,nst)
          if(nst .ne. 0) then
              print *,"error writing V destaggered data to edit file"
              stop
          endif
          enddo       

       RETURN
       END
C*********************************************************************************
      subroutine wdestagr(nx,ny,nz,numfld,ihed)
C
C     Decouple the W-wind from density (RR - total dry density)
C     as well as destagger it.
C
      INCLUDE 'CEDRIC.INC'
      COMMON /WRFDIMS/ UX,UY,VX,VY,WX,WY,THERMX,THERMY,UVZ,WZ,TZ,
     x                  numz,minz

      dimension ihed(NID)
      DIMENSION IBUF(MAXPLN)
      integer nx,ny,nz,wx,wy,wz,numfld
      integer thermx,thermy,iname,ivar,k,m,n,i
      integer kk,km1
      real density(nx,ny,nz)      !RR
      real temp(nx,ny)
      real w(wx,wy,wz)
      real new(nx,ny,nz)
      real rbuf(nx*ny)

      iname = 7   !RR
      call fvarid(iname,ivar)
      do k = 1,nz
         call wrfusrvar(ivar,k,temp)
         DO M = 1,ny
            DO n = 1,nx
                density(n,m,k) = temp(n,m)
            enddo
         enddo
      enddo


      iname = 3  !RW
      call fvarid(iname,ivar)
      do k = 1, wz
         call wrfusrvar(ivar,k,temp)
         DO M = 1,wy
            DO n = 1,wx
                w(n,m,k) = temp(n,m)
            enddo
         enddo
      enddo

CDecouple w
      do k=1,wz
        kk  = min0(k,nz-1)
        km1 = max0(1,k-1)
      do j=1,ny
      do i=1,nx
        w(i,j,k) = 2*w(i,j,k)/(density(i,j,kk)+density(i,j,km1))
      enddo
      enddo
      enddo

CDestagger W
      do k = 2,wz
      do j=1,ny
      do i=1,nx
         new(i,j,k-1) = 0.5 * (w(i,j,k-1) + w(i,j,k)) + 1.0E-6
      enddo
      enddo
      enddo

       do k = 1,nz  
          do j=1,ny
             do i=1,nx
               index = (j - 1) * nx + i 

               rbuf(index) = new(i,j,k)
             enddo
          enddo           
          call placed(0,ihed,k,numfld,ibuf,rbuf,nx,ny,3,
     x                bad,nst)
          if(nst .ne. 0) then
              print *,"error writing W destaggered data to edit file"
              stop
          endif
       enddo
       return
       end
C*********************************************************************************
       subroutine pintrpvar(KRD)

       CHARACTER*(*) KRD(10)
       COMMON /WRF/ WRFINTRP,CALPLACD,wrfdbg,XMIN,XMAX,YMIN,YMAX,
     X              ZMIN,ZMAX,DX,DY,DZ
       REAL XMIN,XMAX,DX,YMIN,YMAX,DY,ZMIN,ZMAX,DZ
       integer WRFINTRP,CALPLACD,wrfdbg
      READ (KRD,100)XMIN,XMAX,DX,YMIN,YMAX,DY,ZMIN,ZMAX,DZ
 100  FORMAT(/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0)
      WRFINTRP = 1
       
       RETURN
       END
C*********************************************************************************
       subroutine hintrpvar(KRD)

       CHARACTER*(*) KRD(10)
       COMMON /WRF/ WRFINTRP,CALPLACD,wrfdbg,XMIN,XMAX,YMIN,YMAX,
     X              ZMIN,ZMAX,DX,DY,DZ
       REAL XMIN,XMAX,DX,YMIN,YMAX,DY,ZMIN,ZMAX,DZ
       integer WRFINTRP,CALPLACD,wrfdbg

      READ (KRD,100)XMIN,XMAX,DX,YMIN,YMAX,DY,ZMIN,ZMAX,DZ
 100  FORMAT(/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0)
      WRFINTRP = 2
       
       RETURN
       END
C***************************************************************
       subroutine calctheta(nx,ny,nlev,rbuf,theta)
C
C     Calculate THETA = (RTB + RTP)/(RHO*(1+1.61*QVAPOR))
C        All variables are on thermodynamic grid (T,BT,SN,WE)
C        RTB    - rho-coupled theta base state
C        RTP    - rho-coupled theta perturbation
C        RR     - total dry density (rho)
C        QVAPOR - water vapor mixing ratio
C     
C
       COMMON /WRF/ WRFINTRP,CALPLACD,wrfdbg,XMIN,XMAX,YMIN,YMAX,
     X              ZMIN,ZMAX,DX,DY,DZ
       COMMON /WRFDIMS/ UX,UY,VX,VY,WX,WY,THERMX,THERMY,UVZ,WZ,TZ,
     X                 numz,minz
       real rbuf(nx*ny)
       real theta(nx*ny)

       integer wrfdbg
       integer thermx,thermy
       real rtprtb(thermx,thermy)        
       real density(thermx,thermy)      !RR
       real qv(thermx,thermy)           !QVAPOR
       real arg2   
       integer  WRFINTRP
       real favg,fsqr,fstd,rmin,rmax
       integer nz,nx,ivar,iname,lev


       favg = 0.0
       fsqr = 0.0
       FSTD = 0.0
       rmin = 0.0
       rmax = 0.0
       lev = nlev  

C     Get all WRF variables needed for this calculation
C
       ivar = 1  !RTB + RTP
       call tmpvar(ivar,lev,rtprtb)
    
       iname = 7   !RR 
       call fvarid(iname,ivar)
       call wrfusrvar(ivar,lev,density)

       iname = 8   !QV
       call fvarid(iname,ivar)
       call wrfusrvar(ivar,lev,qv)


       DO M = 1,NY
	 DO n = 1,NX
            index = (m - 1)*nx + n
            if(wrfintrp .eq. 0) RBUF(index) = -1000.
            theta(index) = -1000.
            arg2 = density(n,m) * (1. + 1.61 * qv(n,m))
            if(arg2 .ne. 0) then
                if(wrfintrp .eq. 0) rbuf(index) = rtprtb(n,m)/arg2
                theta(index) = rtprtb(n,m)/arg2
            endif
           ENDDO
        ENDDO       
       
       return
       end
C**********************************************************
       subroutine calctmpp(thermox,thermoy,rhotheta,p)
       
       integer thermox,thermoy
       real p(thermox*thermoy)
       real rhotheta(thermox*thermoy)
       real p1000mb, r_d, cp, cpovcv
       parameter ( p1000mb = 100000., r_d = 287., cp = 7.*r_d/2.,
     &             cpovcv=cp/(cp-r_d) )         


       i = 1
       DO M = 1,thermoy
	 DO n = 1,thermox
            p(i) = p1000mb*(r_d*rhotheta(I)/p1000mb)**cpovcv
            i = i + 1
         enddo
       enddo
       
       return
       end

C**********************************************************
       subroutine calcp(ptype,nx,ny,nlev,P)


       COMMON /WRFDIMS/ UX,UY,VX,VY,WX,WY,THERMX,THERMY,UVZ,WZ,TZ,
     X                 numz,minz
       real P(nx*ny)            !PRESSURE

       integer thermx,thermy
       real tmpp(thermx,thermy)         !TEMP PRESSURE ALREADY CALCULATED
       real temp1(thermx,thermy)
       real temp,ptemp
       real r_d
       integer nx,ny,ptype,nlev
       integer version,iname


       call wrfversion(version)

       if(version .eq. 1) then
          lev = nlev       
          ivar = 0   !TEMP PRESSURE ALREADY CALCULATED
          call tmpvar(ivar,lev,tmpp)

           ivar = 2   !RHOTHETA ALREADY CALCULATED
           call tmpvar(ivar,lev,temp1)

           r_d = 287.
           DO M = 1,NY
	     DO n = 1,NX
                index = (m - 1) * nx + n
                temp = tmpp(n,m)
                if(ptype .eq. 2 .and. temp .ne. 0) then
                      ptemp = temp/(r_d * temp1(n,m))
                      p(index) = ptemp
                endif
              if(ptype .eq. 1) p(index) = temp
             ENDDO
           ENDDO       
         else if(version .eq. 2) then 
           iname = 21
           call fvarid(iname,ivar)
           call wrfusrvar(ivar,nlev,tmpp)  !Pressure from data set
           iname = 22  
           call fvarid(iname,ivar)
           call wrfusrvar(ivar,nlev,temp1)  !BASE STATE PRESSURE AT HALF LEVELS
           DO M = 1,NY
	     DO n = 1,NX
                index = (m - 1) * nx + n
                ptemp = tmpp(n,m) + temp1(n,m)
                p(index) = ptemp 
             enddo
           enddo
         endif

       return
       end
       
C**********************************************************

       subroutine calcdewpt(nx,ny,nlev,rbuf)

       COMMON /WRFDIMS/ UX,UY,VX,VY,WX,WY,THERMX,THERMY,UVZ,WZ,TZ,
     X                 numz,minz

       real rbuf(nx*ny)
       integer m,n,thermx,thermy
       real qv(thermx,thermy)
       real tmpp(thermx,thermy)   !TEMP PRESSURE ALREADY CALCULATED
       real vp,arg1
       integer nx,ny,nlev,iname,ivar



       lev = nlev       

       iname = 8   !WATER VAPOR QVAPOR 
       call fvarid(iname,ivar)
       call wrfusrvar(ivar,nlev,qv)

       ivar = 0   !PRESSURE
       call tmpvar(ivar,lev,tmpp)

       DO M = 1,NY
	 DO n = 1,NX
            index = (m -1)*nx + n
            temp = tmpp(n,m)
            temp = temp * .01
            if(qv(n,m) .lt. 1.0e-04)qv(n,m)=1.0e-04
            vp = qv(n,m)*temp/(.622+qv(n,m)) !vapor pressure calc
            arg1 = (243.5*log(vp)-440.8)
            rbuf(index) = arg1/(19.48-log(vp))
           ENDDO
        ENDDO      

       return
       end
 
C**********************************************************
       subroutine relhumid(nx,ny,nlev,rbuf)


       COMMON /WRFDIMS/ UX,UY,VX,VY,WX,WY,THERMX,THERMY,UVZ,WZ,TZ,
     X                 numz,minz

       real rbuf(nx*ny)

       integer thermx,thermy
       real density(thermx,thermy)      !RR
       real qv(thermx,thermy)           !QVAPOR
       real rtprtb(thermx,thermy)
       real tmpp(thermx,thermy)
       real rhotheta(thermx,thermy)   
       real temp,theta,rrxqv
       real varthetap
       real p,pi
       integer nz,nx,ivar,iname,lev
       real    qvs,es             
       REAL    svp1, svp2, svp3, svpt0
       parameter (SVP1=0.6112,SVP2=17.67,SVP3=29.65,SVPT0=273.15)
       REAL    ep_2, r_v,r_d
       PARAMETER (r_d = 287.,r_v=461.6, EP_2=R_d/R_v)
       

       lev = nlev  
    
       iname = 7   !RR 
       call fvarid(iname,ivar)
       call wrfusrvar(ivar,lev,density)
 
       iname = 8   !QV
       call fvarid(iname,ivar)
       call wrfusrvar(ivar,lev,qv)

       ivar = 2   !RHOTHETA
       call tmpvar(ivar,lev,rhotheta)

       ivar = 1   !RTP+RTB
       call tmpvar(ivar,lev,rtprtb)
       
       ivar = 0   !PRESSURE
       call tmpvar(ivar,lev,tmpp)
        
       DO M = 1,NY
	 DO n = 1,NX
            index = (m - 1) * nx + n
            temp = tmpp(n,m)
            pi = temp/(r_d * rhotheta(n,m))
            p  = temp
            rrxqv = density(n,m) * (1. + 1.61 * qv(n,m))
            if(rrxqv .ne. 0) then 
               theta = rtprtb(n,m)/rrxqv
            endif
            if(rrxqv .eq. 0) then
               print *,"unable to calculate theta density*qv is 0"
               stop
            endif
            varthetap = theta * pi
            es  = 1000.*svp1*
     &      exp(svp2*(varthetap-svpt0)/(varthetap-svp3))
            qvs = ep_2*es/(p-es)
            rbuf(index) = 100.*qv(n,m)/qvs
           ENDDO
        ENDDO     
        return
        end  


C**********************************************************
CThe code for the following subroutine was taken from 
Cwrf_user_fortran_util_0.f which is part of the WRF
CNCL release.
        subroutine calcslvl(ifld,nx,ny,nz,ihed)

       INCLUDE 'CEDRIC.INC'

      COMMON /WRFDIMS/ UX,UY,VX,VY,WX,WY,THERMX,THERMY,UVZ,WZ,TZ,
     X                 numz,minz

       dimension IHED(NID)
       DIMENSION IBUF(MAXPLN)
       integer thermx,thermy,tz,ux,uy,vx,vy,wx,wy,wz,numz,minz
       integer nx,ny,nz
       real rbuf(nx*ny)
       real var1(thermx,thermy,nz) !theta or grid point height(z)
       real p(thermx,thermy,nz)    !pressure
       real tk(thermx,thermy,nz)   !temperature in Kelvin
       real qv(thermx,thermy,nz)   !water vapor
       real temp(thermx*thermy)
       real tmpp(thermx*thermy)
       real surf(thermx,thermy)
       real t_surf(thermx,thermy)
       real t_sea(thermx,thermy)
       real bad
       integer level(thermx,thermy)
       integer ptype,varx,vary
       integer ivar,iname,tindex,nplane
       integer ifld
       real p1000mb, r_d, cp, cpovcv
       parameter ( p1000mb = 100000., r_d = 287., cp = 7.*r_d/2.,
     &             cpovcv=cp/(cp-r_d) )
       REAL R, G, GAMMA
       PARAMETER (R=287.04, G=9.81, GAMMA=0.0065)
c     Specific constants for assumptions made in this routine:

       REAL    TC, PCONST
       PARAMETER (TC=273.16+17.5, PCONST = 10000)
       LOGICAL ridiculous_mm5_test
       PARAMETER (ridiculous_mm5_test = .TRUE.)
c      PARAMETER (ridiculous_mm5_test = .false.)

      REAL plo , phi , tlo, thi , zlo , zhi
      REAL p_at_pconst , t_at_pconst , z_at_pconst
      REAL z_half_lowest

       LOGICAL  l1 , l2 , l3, found
c---------------------------------------------------------------

       nplane = thermx*thermy

CCalculate theta
       varx = thermx
       vary = thermy
       do k = 1,nz
          call calctheta(varx,vary,k,rbuf,temp)
          tindex = 1 
          do j = 1,thermy
             do i = 1,thermx
                var1(i,j,k) = temp(tindex) !var1 is theta in this case
                tindex = tindex + 1
             enddo
          enddo    


CCalculate the temperature in kelvin-TK
          ptype = 2
          varx = thermx
          vary = thermy
          call calcp(ptype,varx,vary,k,tmpp)
          tindex = 1 
          do j = 1,thermy
             do i = 1,thermx
                tk(i,j,k) = tmpp(tindex) * var1(i,j,k) !pressure*theta
                tindex = tindex + 1
             enddo
          enddo   

CCalculate the pressure
          ptype = 1
          varx = thermx
          vary = thermy
          call calcp(ptype,varx,vary,k,tmpp)
          tindex = 1 
          do j = 1,thermy
             do i = 1,thermx
                p(i,j,k) = tmpp(tindex) 
                tindex = tindex + 1
             enddo
          enddo   

CGet the grid point height
        iname = 11 !zhgt
        call fvarid(iname,ivar)
        varx = thermx
        vary = thermy        
        call wrfusrvar(ivar,k,temp)
        tindex = 1 
        do j = 1,thermy
           do i = 1,thermx
                var1(i,j,k) = temp(tindex) !in this case var1 is z
                tindex = tindex + 1
           enddo
        enddo   


CGet the water vapor
       iname = 8   !QV
       call fvarid(iname,ivar)
       varx = thermx
       vary = thermy  
       call wrfusrvar(ivar,k,temp)
       tindex = 1 
       do j = 1,thermy
          do i = 1,thermx
             qv(i,j,k) = temp(tindex)
             tindex = tindex + 1
          enddo
       enddo          

       enddo !end of k loop

C---------------------------------------------------------

      DO j = 1 , thermy
         DO i = 1 , thermx
            level(i,j) = -1

            k = 1
            found = .false.
            do while( (.not. found) .and. (k .le. nz))
               IF ( p(i,j,k) .LT. p(i,j,1)-PCONST ) THEN
                  level(i,j) = k
                  found = .true.
               END IF
               k = k+1
            END DO 
            IF ( level(i,j) .EQ. -1 ) THEN
            PRINT '(A,I4,A)','Troubles finding level ',
     &                  NINT(PCONST)/100,' above ground.'
            PRINT '(A,I4,A,I4,A)',
     &            'Problems first occur at (',i,',',j,')'
            PRINT '(A,F6.1,A)',
     &            'Surface pressure = ',p(i,j,1)/100,' hPa.'
            STOP 'Error_in_finding_100_hPa_up'
         END IF

         END DO
      END DO

           
c     Get temperature PCONST Pa above surface.  Use this to extrapolate 
c     the temperature at the surface and down to sea level.

      DO j = 1 , thermy
         DO i = 1 , thermx

            klo = MAX ( level(i,j) - 1 , 1      )
            khi = MIN ( klo + 1        , nz - 1 )

            IF ( klo .EQ. khi ) THEN
               PRINT '(A)','Trapping levels are weird.'
               PRINT '(A,I3,A,I3,A)','klo = ',klo,', khi = ',khi,
     &                      ': and they should not be equal.'
               STOP 'Error_trapping_levels'
            END IF

         plo = p(i,j,klo)
         phi = p(i,j,khi)
         tlo = tk(i,j,klo) + 0.608 * qv(i,j,klo)
         thi = tk(i,j,khi) + 0.608 * qv(i,j,khi)
         zlo = var1(i,j,klo)         
         zhi = var1(i,j,khi)
         p_at_pconst = p(i,j,1) - pconst
         t_at_pconst = thi-(thi-tlo)*LOG(p_at_pconst/phi)*LOG(plo/phi)
         z_at_pconst = zhi-(zhi-zlo)*LOG(p_at_pconst/phi)*LOG(plo/phi)
         t_surf(i,j) = t_at_pconst*(p(i,j,1)/p_at_pconst)**(gamma*R/g)
         t_sea(i,j) = t_at_pconst+gamma*z_at_pconst
         END DO
      END DO

      IF ( ridiculous_mm5_test ) THEN
         DO j = 1 , thermy
            DO i = 1 , thermx
               l1 = t_sea(i,j) .LT. TC 
               l2 = t_surf     (i,j) .LE. TC
               l3 = .NOT. l1
               IF ( l2 .AND. l3 ) THEN
                  t_sea(i,j) = TC
               ELSE
                  t_sea(i,j) = TC - 0.005*(t_surf(i,j)-TC)**2
               END IF
            END DO
         END DO
      END IF


      tindex = 1
      DO j = 1 , thermy
      DO i = 1 , thermx
         z_half_lowest=var1(i,j,1)
         surf(i,j) = p(i,j,1) *
     &                         EXP((2.*g*z_half_lowest)/
     &                             (R*(t_sea(i,j)+t_surf(i,j))))
         surf(i,j) = surf(i,j) * 0.01
      END DO
      END DO
   
      do j = 1,ny
         do i = 1,nx
            tindex = (j - 1)*nx + i
            rbuf(tindex) = surf(i,j)
         enddo
       enddo

       bad = -1000.
       do I = 1, nz
          call placed(0,ihed,i,ifld,ibuf,rbuf,nx,ny,3,bad,nst)
          if(nst .ne. 0) then
              print *,"error writing slvl data to edit file"
              stop
           endif
       enddo

       return
       end
C**********************************************************           
C      INTERPOLATION ROUTINES
C**********************************************************               
       subroutine intrpwrf(iname,fldnum,nlev,nx,ny,nz,ihed,rbuf)

       COMMON /WRF/ WRFINTRP,CALPLACD,wrfdbg,XMIN,XMAX,YMIN,YMAX,
     X              ZMIN,ZMAX,DX,DY,DZ
       COMMON /WRFDIMS/ UX,UY,VX,VY,WX,WY,THERMX,THERMY,UVZ,WZ,TZ,
     x                  numz,minz

       INCLUDE 'CEDRIC.INC'

       integer ux,uy,uvz,vx,vy,tz,nz,wrfdbg
       integer thermx,thermy     !dimensions of the vert. coordinate
       integer varx,vary,varz       !dimensions of the variable being interpolated
       integer fldnum,iname,nlev
       DIMENSION RBUF(NX*NY),ihed(NID)
       integer numz,minz
 
       if(iname .eq. 1) then !U
          varx = ux
          vary = uy
          varz = uvz
       endif

       if(iname .eq. 2) then !V
          varx = vx
          vary = vy
          varz = uvz
       endif

       if(iname .gt. 3) then !The thermodynamic variables
         varx = thermx
         vary = thermy
         varz = tz
       endif


       call setupintrp(nx,ny,nz,varx,vary,varz,iname,fldnum,nlev,
     x                 ihed,rbuf)

       return
       end

C**********************************************************
       subroutine setupintrp(nx,ny,nz,varx,vary,varz,iname,fldnum,
     x                       nlev,ihed,rbuf)


       INCLUDE 'CEDRIC.INC'
       COMMON /WRF/ WRFINTRP,CALPLACD,wrfdbg,XMIN,XMAX,YMIN,YMAX,
     X              ZMIN,ZMAX,DX,DY,DZ
       COMMON /WRFDIMS/ UX,UY,VX,VY,WX,WY,THERMX,THERMY,UVZ,WZ,TZ,
     x                  numz,minz
       DIMENSION RBUF(NX*NY)
       DIMENSION IBUF(MAXPLN),ihed(nid)
       integer varx,vary,varz,thermy,thermx,tz
       integer wrfx,wrfy,wrfdbg
       integer nx,ny,nlev,nz,thermz
       integer iname,ivar,index,i,j,k,fldnum,ifld
       integer staggered,varn,ptype,vdims
       real bad
       real decouple(varx,vary)
       real intpvar(varx,vary,varz)
       real znew(varx,vary,varz)
       real z(thermx,thermy,tz)
       real hgt(thermx,thermy),athree(numz)
       real temp(varx*vary)
       real tmpp(thermx*thermy)
       real v2d(varx,vary)
       character*8 intrpnam

       thermz = tz
       bad = -1000.
       varn = iname
CThese are the levels we want to interpolate to.
       athree(1) = minz
       do i = 2,numz
          athree(i) = athree(i-1) + dz
       enddo

C----------------------------------------------------------------       
       if(wrfintrp .eq. 1) then 
CGet the already calculated pressure for each level.
          do k = 1,tz
             ivar = 0   !PRESSURE ALREADY CALCULATED
             call tmpvar(ivar,k,tmpp)
             index = 1
             do j = 1,thermy
                do i = 1,thermx
                   z(i,j,k) = tmpp(index) * .01
                   index = index + 1
                enddo
             enddo
          enddo

       else if(wrfintrp .eq. 2) then
CGet gridpoint height 
          iname = 11 !hgt
          call fvarid(iname,ivar)
          do k = 1,tz
             call wrfusrvar(ivar,k,tmpp)
             tindex = 1
             do j = 1,thermy
                do i = 1,thermx
                   z(i,j,k) =  tmpp(tindex)
                   tindex = tindex + 1
                 enddo
             enddo             
         enddo
         iname = varn
       endif
C----------------------------------------------------------------
CDecouple the variable
       do k = 1,varz
          if(iname .eq. 1 .or. iname .eq. 2) then 
             if(iname .eq. 1) then
               call udecouple(nx,ny,nz,k,decouple)
             endif
             if(iname .eq. 2) then
               call vdecouple(nx,ny,nz,k,decouple)
             endif
             do j = 1,vary
                do i = 1,varx
                   intpvar(i,j,k) = decouple(i,j)
                enddo
             enddo
          else if(iname .gt. 3) then
            wrfx = thermx
            wrfy = thermy
            nplane = wrfx*wrfy
C----------------------------------------------------------------
            if(iname .eq. 4) then !THETA
               call calctheta(varx,vary,k,tmpp,temp)
               tindex = 1 
               do j = 1,vary
                  do i = 1,varx
                    intpvar(i,j,k) = temp(tindex)
                    tindex = tindex + 1
                  enddo
               enddo    
            endif
C----------------------------------------------------------------
            if(iname .eq. 13) then !TC
               call calctheta(varx,vary,k,tmpp ,temp)
               ptype = 2
               call calcp(ptype,varx,vary,k,tmpp)
               tindex = 1 
               do j = 1,vary
                  do i = 1,varx
                    intpvar(i,j,k) = tmpp(tindex) * temp(tindex) - 273.16
                    tindex = tindex + 1
                  enddo
               enddo   
            end if
C----------------------------------------------------------------
           if(iname .eq. 11) then !grid point height
              intrpnam = 'Z'
              wrfx = thermx
              wrfy = thermy
              CALL RWRFVARS(vdims,fldnum,temp,k,intrpnam)
              tindex = 1
               do j = 1,wrfy
                  do i = 1,wrfx
                    intpvar(i,j,k) =  temp(tindex) * 0.1
                    tindex = tindex + 1
                  enddo
               enddo
          endif
C----------------------------------------------------------------
           if(iname .eq. 14) then !pressure
              if(wrfintrp .eq. 2) then
                ivar = 0   !PRESSURE ALREADY CALCULATED
                call tmpvar(ivar,k,tmpp) 
              endif
              tindex = 1
               do j = 1,thermy
                  do i = 1,thermx
                    if(wrfintrp .eq. 1) then
                       intpvar(i,j,k) =   z(i,j,k)
                    endif
                    if(wrfintrp .eq. 2) then
                       intpvar(i,j,k) = tmpp(tindex) * .01
                    endif
                    tindex = tindex + 1
                  enddo
               enddo
           endif
C----------------------------------------------------------------
           if(iname .eq. 15) then !TD Dew point
                call calcdewpt(varx,vary,k,temp)
                tindex = 1 
                do j = 1,vary
                  do i = 1,varx
                    intpvar(i,j,k) = temp(tindex)
                    tindex = tindex + 1
                  enddo
                enddo    
           endif
C----------------------------------------------------------------
           if(iname .eq. 16) then !RH Relative Humidity
              call relhumid(varx,vary,k,temp)
                tindex = 1 
                do j = 1,vary
                  do i = 1,varx
                    intpvar(i,j,k) = temp(tindex)
                    tindex = tindex + 1
                  enddo
                enddo   
           endif


        endif
       end do
C----------------------------------------------------------------
CGet the height variable
        iname = 12 !hgt
        call fvarid(iname,ivar)
        call wrfsrfvar(ivar,hgt)
        
        staggered = 0
        if(varx .gt. thermx) then
           staggered = 1
           call z_stag(thermx,thermy,thermz,varx,vary,varz,
     X                 z,znew,staggered)
        endif
           
        if(vary .gt. thermy) then
           staggered = 2
           call z_stag(thermx,thermy,thermz,varx,vary,varz,
     X                 z,znew,staggered)
        endif


        do kk = 1,numz
CInitialize the interpolated array to all bad.
           do i = 1,vary
           do j = 1,varx
              v2d(j,i) = -1000.
           enddo
           enddo

          if(staggered .gt. 0) then
             call interp_3d(intpvar,v2d,znew,athree(kk),varx,vary,varz)
          else
             call interp_3d(intpvar,v2d,z,athree(kk),varx,vary,varz)
          endif

           do j = 1,ny
           do i = 1,nx
              index = (j-1)*nx + i
              rbuf(index) = v2d(i,j)
           enddo
           enddo

CWrite the interpolated array to the cedric edit file.
           ifld = fldnum
           level = kk
           iname = varn
           call placed(0,ihed,level,ifld,ibuf,rbuf,nx,ny,3,
     x                 bad,nst)
           if(nst .ne. 0) then
              print *,"error writing interpolated data to edit file"
              stop
           endif

        enddo
        return
       end

C**********************************************************
       subroutine z_stag(thermx,thermy,thermz,nx,ny,nz,
     X                   z,znew,staggered)



       integer  thermx,thermy,thermz
       integer  staggered    
       real znew(nx,ny,nz)
       real z(thermx,thermy,thermz)

       integer i,j,k, ii, im1, jj, jm1


       if(staggered .eq. 1) then  ! for x and y stag, avg z to x, y, point

          do k=1,nz
          do j=1,ny
          do i=1,nx
             ii = min0(i,thermx)
             im1 = max0(i-1,1)
             znew(i,j,k) = 0.5*(z(ii,j,k)+z(im1,j,k))
          enddo
          enddo
          enddo       

       endif


       if(staggered .eq. 2) then  ! for x and y stag, avg z to x, y, point
          do k=1,nz
             do j=1,ny
                jj = min0(j,thermy)
                jm1 = max0(j-1,1)
                do i=1,nx
                   znew(i,j,k) = 0.5*(z(i,jj,k)+z(i,jm1,k))
                enddo
             enddo
          enddo       

       endif

       return
       end

C**********************************************************
       subroutine interp_3d(v3d,v2d,z,loc,nx,ny,nz)

      integer nx,ny,nz
      real    v3d(nx,ny,nz), v2d(nx,ny)
      real    z(nx,ny,nz)
      real    loc

      integer i,j,kp, ip, im
      logical interp
      real    height, w1, w2

      height = loc

c does vertical coordinate increase of decrease with increasing k?
c set offset appropriately

      ip = 0
      im = 1
      if (z(1,1,1) .gt. z(1,1,nz)) then
        ip = 1
        im = 0
      endif

      DO i=1,nx
      DO j=1,ny
        interp = .false.
        kp = nz
        DO WHILE ( interp .and. (kp .ge. 2) )
          IF(   ((z(i,j,kp-im) .le. height) .and. 
     &           (z(i,j,kp-ip) .gt. height))             )   THEN
            w2 = (height-z(i,j,kp-im))/(z(i,j,kp-ip)-z(i,j,kp-im))
            w1 = 1.-w2
            v2d(i,j) = w1*v3d(i,j,kp-im) + w2*v3d(i,j,kp-ip)
            interp = .true.
          END IF
        kp = kp-1
        ENDDO
      ENDDO
      ENDDO

      RETURN
      END

C*********************************************************************************
      subroutine READINTRP(fldnum,iname,varx,vary,nx,ny,NLEV,RBUF)

      integer NLEV,iname,fldnum,ifld
      integer varx,vary,index,i,j
      DIMENSION RBUF(NX*NY)
      real array(varx*vary)

      ifld = fldnum
      call getintfld(ifld,varx,vary,nlev,array)

      index = 1
      jindex = 1
      do i = 1,ny
        do j = 1,nx
         if(i .le. vary .and. j .le. varx) then
            rbuf(jindex) = array(index)
            index = index + 1
         else
            rbuf(jindex) = -1000.
         endif
         jindex = jindex + 1
        enddo
      enddo
      return
      end


C********************************************************************************
       subroutine fstats()

      COMMON /WRFDIMS/ UX,UY,VX,VY,WX,WY,THERMX,THERMY,UVZ,WZ,TZ,
     X                 numz,minz
       integer thermx,thermy
       real a(thermx*thermy)
       real rlev,rmin,rmax
       DOUBLE PRECISION fstd,favg,fsqr
       integer nlev,ivar,iname
       integer ux,uy,vx,vy,wx,wy,uvz,wz,tz,numz,minz
       character*6 name

        name = "RTB   "
        print *,'---STATISTICS FOR WRF VARIABLE USED TO CALCULATE',
     +       ' DIAGNOSTIC FIELDS---'
        write(*,15)NAME
 15     FORMAT(A6,6X,'NLEV',10X,'MEAN',11x,'STDV',11x,'MIN',12X,'MAX')

        iname = 5  !RTB
        call fvarid(iname,ivar)
        do i = 1,tz 
           rlev = i
           favg = 0.0
           fsqr = 0.0
           call wrfusrvar(ivar,i,a)
           do j = 1,thermx*thermy
              if(j .eq. 1) then
                 rmin = a(j)
                 rmax = a(j)
              else
                if(a(j) .lt. rmin) rmin = a(j)
                if(a(j) .gt. rmax) rmax = a(j)
              endif
              favg = a(j) + favg
              fsqr = fsqr + a(j)**2
           end do
           favg = favg/(thermx*thermy)
           fsqr = fsqr/(thermx*thermy)
           FSTD=FSQR-FAVG**2
           IF(FSTD.LT.0.0) FSTD=0.0
           FSTD=SQRT(FSTD)   
           WRITE(*,20) rlev,favg,fstd,rmin,rmax
 20     FORMAT(8X,F8.3,4F15.5)
        enddo
        WRITE(*,10)
 10     FORMAT(/)


        name = "RTP   "
        print *,'---STATISTICS FOR WRF VARIABLE USED TO CALCULATE',
     +       ' DIAGNOSTIC FIELDS---'
        write(*,15)NAME
        iname = 6  !RTP
        call fvarid(iname,ivar)
        do i = 1,tz 
           rlev = i
           favg = 0.0
           fsqr = 0.0
           call wrfusrvar(ivar,i,a)
           do j = 1,thermx*thermy
              favg = a(j) + favg
              fsqr = fsqr + a(j)**2
              if(j .eq. 1) then
                 rmin = a(j)
                 rmax = a(j)
              else
                if(a(j) .lt. rmin) rmin = a(j)
                if(a(j) .gt. rmax) rmax = a(j)
              endif
           end do
           favg = favg/(thermx*thermy)
           fsqr = fsqr/(thermx*thermy)
           FSTD=FSQR-FAVG**2
           IF(FSTD.LT.0.0) FSTD=0.0
           FSTD=SQRT(FSTD)   
           WRITE(*,20) rlev,favg,fstd,rmin,rmax
        enddo
        WRITE(*,10)  


        name = "RR    "
        print *,'---STATISTICS FOR WRF VARIABLE USED TO CALCULATE',
     +       ' DIAGNOSTIC FIELDS---'
        write(*,15)NAME
        iname = 7  !RR
        call fvarid(iname,ivar)
        do i = 1,tz 
           rlev = i
           favg = 0.0
           fsqr = 0.0
           call wrfusrvar(ivar,i,a)
           do j = 1,thermx*thermy
              favg = a(j) + favg
              fsqr = fsqr + a(j)**2
              if(j .eq. 1) then
                 rmin = a(j)
                 rmax = a(j)
              else
                if(a(j) .lt. rmin) rmin = a(j)
                if(a(j) .gt. rmax) rmax = a(j)
              endif
           end do
           favg = favg/(thermx*thermy)
           fsqr = fsqr/(thermx*thermy)
           FSTD=FSQR-FAVG**2
           IF(FSTD.LT.0.0) FSTD=0.0
           FSTD=SQRT(FSTD)     
           WRITE(*,20) rlev,favg,fstd,rmin,rmax
        enddo
        WRITE(*,10)     

        name = "QVAPOR"
        print *,'---STATISTICS FOR WRF VARIABLE USED TO CALCULATE',
     +       ' DIAGNOSTIC FIELDS---'
        write(*,15)NAME
        iname = 8  !QVAPOR
        call fvarid(iname,ivar)
        do i = 1,tz 
           rlev = i
           favg = 0.0
           fsqr = 0.0
           call wrfusrvar(ivar,i,a)
           do j = 1,thermx*thermy
              favg = a(j) + favg
              fsqr = fsqr + a(j)**2
              if(j .eq. 1) then
                 rmin = a(j)
                 rmax = a(j)
              else
                if(a(j) .lt. rmin) rmin = a(j)
                if(a(j) .gt. rmax) rmax = a(j)
              endif
           end do
           favg = favg/(thermx*thermy)
           fsqr = fsqr/(thermx*thermy)
           FSTD=FSQR-FAVG**2
           IF(FSTD.LT.0.0) FSTD=0.0
           FSTD=SQRT(FSTD)     
           WRITE(*,20) rlev,favg,fstd,rmin,rmax
        enddo
        WRITE(*,10)                
        return
        end

C********************************************************************************

      SUBROUTINE WRFSTATS(NL,NAMFLD,NF,RBUF,NX,NY,NZ,BAD)
C
C     Routine to compute statistics on WRF variable
C     that is being read into CEDRIC edit file.
C        NL     - Index of current level
C        NF     - Index of current field
C        NAMFLD - Name of current field
C        RBUF   - Array of values for current field, current level
C        NX,NY  - Numbers of Xs and Ys for the current level
C        NZ     - Total number of Zs
C        BAD    - Bad data value (-1000.0)
C
      DIMENSION RBUF(NX*NY)
      CHARACTER*2 NAMFLD(4)

      PARAMETER (NFMX=50,NLMX=50)
      DIMENSION FMIN(NFMX,NLMX),FMAX(NFMX,NLMX)
      DIMENSION CNT(NFMX,NLMX),FAVG(NFMX,NLMX),FSTD(NFMX,NLMX)
C     
C     The Q's are of order 10^-3 == gm/gm. We convert to gm/kg by
C     the Q's * 1000. 
C
      QSCALE = 1.0
      IF(NAMFLD(1)(1:1) .EQ. "Q") QSCALE = 1000.
      CNT(NF,NL)  = 0.0
      FAVG(NF,NL) = BAD
      FSTD(NF,NL) = BAD
      FMIN(NF,NL) = BAD
      FMAX(NF,NL) = BAD
      FSUM = 0.0
      FSUM2 = 0.0
      DO M = 1,NY
	 DO N = 1,NX
            J = (M - 1) * NX + N
            IF(RBUF(J).NE.BAD)THEN
               IF(M .EQ. 1 .AND. N .EQ. 1) THEN
                  FMIN(NF,NL) = RBUF(J)
                  FMAX(NF,NL) = RBUF(J)
               ELSE
                  IF(RBUF(J) .LT. FMIN(NF,NL)) FMIN(NF,NL) = RBUF(J)
                  IF(RBUF(J) .GT. FMAX(NF,NL)) FMAX(NF,NL) = RBUF(J)
               ENDIF
               FSUM  = FSUM + RBUF(J)
               FSUM2 = FSUM2 + RBUF(J)**2
               CNT(NF,NL)=CNT(NF,NL)+1.0
            END IF
         ENDDO
      ENDDO
      IF(CNT(NF,NL).NE.0.0)THEN
         FAVG(NF,NL) = FSUM/CNT(NF,NL)
         FSQR        = FSUM2/CNT(NF,NL)
         IF(FSQR.GT.FAVG(NF,NL)**2)THEN
            FSTD(NF,NL)=SQRT(FSQR-FAVG(NF,NL)**2)
         ELSE
            FSTD(NF,NL)=0.0
         END IF
      END IF
      IF(NL.LT.NZ)RETURN

C     Output all levels of accumulated statistics for field number NF
C
      WRITE(*,11)
 11   FORMAT(/)
      WRITE(*,13)(NAMFLD(I),I=1,4)
 13   FORMAT(14X,
     +     '---STATISTICS FOR WRF VARIABLE INPUT TO CEDRIC---',//,
     +     4A2,4X,'NLEV',10X,'MEAN',11X,'STDV',11X,'MIN',12X,'MAX')
      DO L=1,NZ
         RLEV = L
         WRITE(*,15)RLEV,FAVG(NF,L),FSTD(NF,L),
     X        FMIN(NF,L),FMAX(NF,L),INT(CNT(NF,L))
 15      FORMAT(8X,F8.3,4F15.5,I15)
      END DO

      RETURN
      END


C************************************************************************
      subroutine calcref(iname,namfld,numfld,nx,ny,nz,wrfdbg,ihed)

c From Lou Wicker, who stole it from Jerry Straka. 
c 17 December 2001 
c Computes radar reflectivity from model water variables. 
c 
c Algorithm is derived from Jerry Straka's code 
c NOTE:  An important trick to get these calculations to work is to 
c use double precision variables for all the intermediate calculations. 
c If you dont, you get overflow and underflow error when computing 
c the logrithms.  Ugh.  
c 
c Units are MKS, and for most accurate results, make sure that the 
c number concentrations and densities of rain, snow and hail are the 
c same as the model used in producing the fields. 
c----------------------------------------------------------------------- 

      include 'CEDRIC.INC'
      COMMON /WRFDIMS/ UX,UY,VX,VY,WX,WY,THERMX,THERMY,UVZ,WZ,TZ,
     X                 numz,minz

      integer UX,UY,VX,VY,WX,WY,THERMX,THERMY,UVZ,WZ,TZ,numz,minz
      integer iname,numfld
      dimension ihed(nid),ibuf(MAXPLN)
      integer nx,ny,nz,nm 

      integer i,j,k,ivar,n,m,ptype,wrfdbg
      integer qname
      real rlev,rmin,rmax
      DOUBLE PRECISION fstd,favg,fsqr
      double precision qr, qs, qh, den, temp, reflect
      double precision xcnoh,xcnos,zsdryc,zswetc,zhdryc,zhwetc
      double precision dadr,dads,dadh   
      double precision cnoh,cnos,cnor,cr1,hwdn,swdn,rwdn,hwdnsq
      double precision swdnsq,rwdnsq
      double precision dhmin,dielf,pie,qrmin,qsmin,qhmin,tfr,tfrh,zrc
      double precision tem,gmh,gms,gmr,zrain,zswet,zhwet,zsdry,zhdry
      double precision reflectmin
      real ztotal,thelog
      real density(thermx,thermy,tz)
      real qr_in(thermx,thermy,tz)
      real qs_in(thermx,thermy,tz)
      real qg_in(thermx,thermy,tz)
      real ref(thermx,thermy,tz)
      real tc(thermx,thermy,tz)
      real vtemp(thermx*thermy)
      real tmpp(thermx*thermy)
      real rbuf(nx*ny)
      character*6 name
      CHARACTER*2 NAMFLD(4)
c     
c     constants 
c     
      reflectmin = -30.0
      cnoh = 4.0e+04 
      cnor = 8.0e+06 
      cnos = 3.0e+06 
      cr1  = 7.2e+20 
      hwdn = 917.0 
      swdn = 100.0 
      rwdn = 1000.0 
      hwdnsq = hwdn**2 
      swdnsq = swdn**2 
      rwdnsq = rwdn**2 
      dhmin = 0.005 
      dielf = 0.21/0.93 
      pie   = 4.0*atan(1.0) 
      qrmin = 1.0e-05 
      qsmin = 1.0e-06 
      qhmin = 1.0e-05 
      tfr   = 273.16 
      tfrh  = tfr - 8.0 
      zrc   = cr1*cnor 
      
      qname = iname
      if(qname .eq. 17) name = "ZTOTAL"
      if(qname .eq. 21) name = "ZRAIN "
      
      if(wrfdbg .eq. 1) then 
        print *,' '
        print *,
     +  '---BEGIN STATISTICS FOR REFLECTIVITY FIELD CALCULATION---',
     +  '---',name,'---'
      endif 
      
C.....get the air density
C
      iname = 7                 !RR 
      call fvarid(iname,ivar)
      do k = 1,nz
         call wrfusrvar(ivar,k,vtemp)
         i = 1
         DO M = 1,ny
            DO n = 1,nx 
               density(n,m,k) = vtemp(i)
               i = i + 1
            enddo
         enddo
      enddo
      
c.....get the qrain
C
      iname = 18                !QRAIN
      if(wrfdbg .eq. 1) then
         WRITE(*,10)
 10      FORMAT(/)
         print *,'NOTE: The following stats are for alog10(QRAIN)'
         print *,'      BAD is the number of QRAIN = 0 values'
         name = "QRAIN"
         write(*,15)NAME
 15      FORMAT(A6,6X,'NLEV',10X,'MEAN',11X,'STDV',11X,'MIN',12X,'MAX',
     x        9X,'BAD')
      endif
      call fvarid(iname,ivar)
      do k = 1,nz
         call wrfusrvar(ivar,k,vtemp)
         i = 1
         favg = 0.0
         fsqr = 0.0
         rmin = 1000.
         rmax = -1000.
         icount = 0
         lcount = 0
         DO M = 1,ny
            DO n = 1,nx 
               if(vtemp(i) .gt. 0.0) then
                  icount = icount + 1
                  thelog = alog10(vtemp(i))
                  favg = thelog + favg
                  fsqr = fsqr + thelog**2
                  if( thelog .lt. rmin) rmin =  thelog
                  if( thelog .gt. rmax) rmax =  thelog
               else
                  lcount = lcount + 1
               endif
               if(qname .eq. 21) then !ZRAIN
                  qs_in(n,m,k) = vtemp(i)
                  qg_in(n,m,k) = vtemp(i)
               endif
               qr_in(n,m,k) = vtemp(i)
               vtemp(i) = -1000.
               i = i + 1
            enddo
         enddo
         if( icount .ne. 0) then 
            favg = favg/(icount)
            fsqr = fsqr/(icount)
            FSTD=FSQR-FAVG**2
            IF(FSTD.LT.0.0) FSTD=0.0
            FSTD=SQRT(FSTD)
         else
            favg = -1000.0
            FSTD = -1000.0
            rmin = -1000.0
            rmax = -1000.0
         endif 
         rlev = k     
         if(wrfdbg .eq. 1) WRITE(*,20) rlev,favg,fstd,rmin,rmax,lcount
 20      FORMAT(8X,F8.3,4F15.5,I10)
      enddo
      
c.....get the qsnow
C
      if(qname .eq. 21) GOTO 100
      if(wrfdbg .eq. 1) then
         WRITE(*,10)
         print *,'NOTE: The following stats are for alog10(QSNOW)'
         print *,'      BAD is the number of QSNOW = 0 values'
         name = "QSNOW"
         write(*,15)NAME
      endif
      iname = 19                !Qsnow
      call fvarid(iname,ivar)
      do k = 1,nz
         favg = 0.0
         fsqr = 0.0
         FSTD = 0.0
         lcount = 0
         icount = 0
         call wrfusrvar(ivar,k,vtemp)
         i = 1
         rmin =  1000.
         rmax =  -1000.
         DO M = 1,ny
            DO n = 1,nx 
               if(vtemp(i) .gt. 0.0) then
                  icount = icount + 1
                  thelog = alog10(vtemp(i))
                  favg = thelog + favg
                  fsqr = fsqr + thelog**2
                  if( thelog .lt. rmin) rmin =  thelog
                  if( thelog .gt. rmax) rmax =  thelog
               else
                  lcount = lcount + 1 
               endif

               qs_in(n,m,k) = vtemp(i)
               vtemp(i) = -1000.
               i = i + 1
            enddo
         enddo
         if(icount .ne. 0.0) then
            favg = favg/(icount)
            fsqr = fsqr/(icount)
            FSTD=FSQR-FAVG**2
            IF(FSTD.LT.0.0) FSTD=0.0
            FSTD=SQRT(FSTD)     
         else
            favg = -1000.0
            FSTD = -1000.0
            rmin = -1000.0
            rmax = -1000.0
         endif            
         rlev = k
         if(wrfdbg .eq. 1) WRITE(*,20) rlev,favg,fstd,rmin,rmax,lcount
      enddo

c.....get the qgraup
c
      if(wrfdbg .eq. 1) then
         WRITE(*,10)
         print *,'NOTE: The following stats are for alog10(QGRAUP)'
         print *,'      BAD is the number of QGRAUP = 0 values'
         name = "QGRAUP"
         write(*,15)NAME
      endif 
      iname = 20                !Qgraup
      call fvarid(iname,ivar)
      favg = 0.0
      fsqr = 0.0
      FSTD = 0.0
      do k = 1,nz
         call wrfusrvar(ivar,k,vtemp)
         i = 1
         icount = 0
         lcount = 0
         rmin =  1000.
         rmax =  -1000.
         DO M = 1,ny
            DO n = 1,nx 
               
               if(vtemp(i) .gt. 0.0) then
                  icount = icount + 1
                  thelog = alog10(vtemp(i))
                  favg = thelog + favg
                  fsqr = fsqr + thelog**2
                  if( thelog .lt. rmin) rmin =  thelog
                  if( thelog .gt. rmax) rmax =  thelog
               else
                  lcount = lcount + 1
               end if
               
               qg_in(n,m,k) = vtemp(i)
               vtemp(i) = -1000.
               i = i + 1
            enddo
         enddo
         if( icount .ne. 0) then
            favg = favg/(icount)
            fsqr = fsqr/(icount)
            FSTD=FSQR-FAVG**2
            IF(FSTD.LT.0.0) FSTD=0.0
            FSTD=SQRT(FSTD)
         else
            favg = -1000.0
            FSTD = -1000.0
            rmin = -1000.0
            rmax = -1000.0
         endif  
         rlev=k
         if(wrfdbg .eq. 1) WRITE(*,20) rlev,favg,fstd,rmin,rmax,lcount
      enddo
      
 100  continue
      
c     compute air temperature in degrees C
c
      if(wrfdbg .eq. 1) then
         WRITE(*,10)
         name = "TEMP C"
         write(*,15)NAME 
         lcount = 0 
      endif   
      do k = 1,nz 
         favg = 0.0
         fsqr = 0.0
         FSTD = 0.0
         call calctheta(thermx,thermy,k,tmpp,vtemp)
         ptype = 2
         call calcp(ptype,thermx,thermy,k,tmpp)
         index = 1 
         do j = 1,thermy
            do i = 1,thermx
               tc(i,j,k) = tmpp(index) * vtemp(index) - 273.16
               favg =  tc(i,j,k) + favg
               fsqr = fsqr + tc(i,j,k)**2
               if(j .eq. 1) then
                  rmin =  tc(i,j,k)
                  rmax =  tc(i,j,k)
               else
                  if( tc(i,j,k) .lt. rmin) rmin =  tc(i,j,k)
                  if( tc(i,j,k) .gt. rmax) rmax =  tc(i,j,k)
               endif          
               index = index + 1
            enddo
         enddo   
         favg = favg/(thermx*thermy)
         fsqr = fsqr/(thermx*thermy)
         FSTD=FSQR-FAVG**2
         IF(FSTD.LT.0.0) FSTD=0.0
         FSTD=SQRT(FSTD)   
         rlev = k 
         if(wrfdbg .eq. 1) WRITE(*,20) rlev,favg,fstd,rmin,rmax,lcount 
      enddo
      
      if(wrfdbg .eq. 1) then
         WRITE(*,10)
         if(qname .eq. 17) name = "ZTOTAL"
         if(qname .eq. 21) name = "ZRAIN "
         write(*,15)NAME  
      endif   
      
c     define double precision mixing ratios 
c
      if(qname .eq. 17) nm = 3  !ZTOTAL
      if(qname .eq. 21) nm = 1  !ZRAIN
      do k=1,nz 

         favg = 0.0
         fsqr = 0.0
         FSTD = 0.0
         rmin = 1000.0
         rmax = -1000.0
         lcount = 0
         icount = 0

         do j=1,ny 
            do i=1,nx 
               qr = amax1(0.,qr_in(i,j,k)) 
               qs = 0. 
               qh = 0. 
               if( nm .gt. 1 ) then 
                  qs = amax1(0., qs_in(i,j,k)) 
                  qh = amax1(0., qg_in(i,j,k)) 
               endif 
               den = density(i,j,k)
               temp = tc(i,j,k)

               if ( temp .lt. tfr ) then
                  xcnoh = cnoh*exp(-0.025*(temp-tfr))
                  xcnos = cnos*exp(-0.038*(temp-tfr))
               else
                  xcnoh = cnoh*exp(-0.075*(temp-tfr))
                  xcnos = cnos*exp(-0.088*(temp-tfr))
               end if 
               
               dadh = ( den /(pie*hwdn*xcnoh) )**.25
               dads = ( den /(pie*swdn*xcnos) )**.25
               dadr = ( den /(pie*rwdn*cnor)  )**.25
c     
               zhdryc = dielf*cr1*(hwdnsq/rwdnsq)*xcnoh
               zhwetc = cr1*(xcnoh)**.95
               zsdryc = dielf*cr1*swdnsq/rwdnsq*xcnos
               zswetc = cr1*xcnos
               
c     Initialize all linear reflectivities to 0.
c
               zrain = 0.
               zswet = 0.
               zsdry = 0.
               zhwet = 0.
               zhdry = 0.
               gmr = 0.0
               gms = 0.0
               gmh = 0.0
              
c-----------------------------------------------------------------------
c     If nm .ge. 1, compute rain reflectivity (always)
c-----------------------------------------------------------------------
               if( nm .ge. 1 ) then
                  
c..   slope for rain
                  
                  if ( qr .gt. qrmin ) then
                     gmr = dadr*(qr)**.25
                  end if
                  zrain = zrc*gmr**7
               endif
c-----------------------------------------------------------------------
c     If nm .gt. 1, compute addition snow and graupel reflectivities
c-----------------------------------------------------------------------
               if( nm .gt. 1) then
                  
c..slope for snow
                  if ( qs .gt. qsmin ) then
                     gms = dads*(qs)**.25
                  endif
                  
c..computation for dry and wet snow reflectivity
                  
                  if ( temp .lt. tfr ) then
                     zsdry = zsdryc*gms**7
                  else
                     zswet = zswetc*gms**7
                  end if
                  
c..slope for hail/graupel
                  
                  if ( qh .gt. qhmin ) then
                     gmh = dadh*(qh)**.25
                  endif
                  
c..computation for dry and wet hail reflectivity including
c..mie scattering parameterization (Smith, 1975) 
                  
                  if ( temp .gt. tfr ) then
                     zhwet = (zhwetc*(gmh**7))**.95
                  else
                     zhdry = zhdryc*gmh**7
                  endif
                  
               endif
c-----------------------------------------------------------------------
c     Compute final reflectivity value (ztotal).  This variable is linear.
c     When user-requested field is ZRAIN, all other reflectivities are 0.
c-----------------------------------------------------------------------
               ztotal = zrain + zswet + zsdry + zhwet + zhdry
               
               if( ztotal .gt. 0. ) then
                  reflect = 10.0 * alog10(ztotal)
                  icount = icount + 1
                  favg =  reflect + favg
                  fsqr = fsqr + reflect**2
                  if( reflect .lt. rmin) rmin =  reflect
                  if( reflect .gt. rmax) rmax =  reflect
                  if(reflect .lt. reflectmin)then
                     ref(i,j,k)=reflectmin
                  else
                     ref(i,j,k)=reflect
                  end if
               else
                  ref(i,j,k) = reflectmin
                  lcount = lcount + 1
               endif
            enddo
         enddo
         
         if( icount .ne. 0) then 
            favg = favg/(icount)
            fsqr = fsqr/(icount)
            FSTD=FSQR-FAVG**2
            IF(FSTD.LT.0.0) FSTD=0.0
            FSTD=SQRT(FSTD)
         else
            favg = -1000.0
            FSTD = -1000.0
            rmin = -1000.0
            rmax = -1000.0
         endif 
         rlev = k 
         if(wrfdbg .eq. 1) WRITE(*,20) rlev,favg,fstd,rmin,rmax,lcount
         do m = 1,ny
            do n = 1,nx
               index = (m-1)*nx + n
               rbuf(index) = ref(n,m,k)
c               print *,rbuf(index)
            enddo
         enddo
         call placed(0,ihed,k,numfld,ibuf,rbuf,nx,ny,3,bad,nst)
         CALL WRFSTATS(NLEV,NAMFLD,NUMFLD,RBUF,NX,NY,NZ,BAD)
         if(nst .ne. 0) then
            print *,"error writing ref to edit file"
            stop
         endif
      enddo
      
      if(wrfdbg .eq. 1) then 
        print *,
     +  '-----END STATISTICS FOR REFLECTIVITY FIELD CALCULATION---',
     +  '---',name,'---'
      endif 

      return
      end
      
