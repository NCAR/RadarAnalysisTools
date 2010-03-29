      subroutine genmap(xl,xr,yb,yt,xr1,xr2,yr1,yr2,ipltyp,id,imptyp,
     x     latlon,thkmap)
*
* By Mark Bradford, GALE Project, 2/87, updated for Cray 7/87.
*
* PURPOSE:  Produce map backgrounds for CEDRIC
* INPUT  :  XL, XR, YB, and YT are the extrema of the plot frame.  XR1,
*           XR2, YR1, and YR2 are the extrema of CEDRIC's coordinates for
*           the frame.  IPLTYP is the kind of plot being done.  ID is the
*           CEDRIC data header.  IMPTYP indicates what is desired of the
*           routine.
*
*     NOTE: if IMPTYP = 0   ==>  Do nothing
*                     = 1   ==>  Map background only
*                     = 2   ==>  Landmarks only
*                     = 3   ==>  Both
*
* OUTPUT :  The backgrounds are plotted.
* NOTE   :  Routine is used with EZMAP modified to utilize /MAPCED/ common
*           block.  
*           The EZMAP sign convention for longitudes are:
*               -180 < LON < 0   ==>  WESTERN HEMISPHERE
*               0    < LON < 180 ==>  EASTERN HEMISPHERE
*
*           The CEDRIC internal sign convention for longitude are:
*               0    < LON < 180 ==>  WESTERN HEMISPHERE
*               -180 < LON < 0   ==>  EASTERN HEMISPHERE
*           Therefore, input -lon_sign*abs(long) into EZMAP routines
*
      logical land,latlon

      character*2 ctemp1,ctemp2
      real lllat,lllong,urlat,urlong,reflat,reflon,xx,yy
      integer id(510)
C
C     COMMON block variables returned from LAT_LON or uses defaults.
C
      COMMON /HEMISPHERE/LATSPHERE,LONSPHERE,LAT_SIGN,LON_SIGN
      CHARACTER*8 LATSPHERE,LONSPHERE
      REAL LAT_SIGN,LON_SIGN

      ulced=xl
      urced=xr
      vbced=yb
      vtced=yt
c
c     the following call was added so that we don't need to modify the
c     EZMAP routines -- bill anderson -- 09/27/90
c
      call getset(fl,fr,fb,ft,ul,ur,ub,ut,ll)

      angxax=id(40)/real(id(69))
      call mappos(ulced,urced,vbced,vtced)
      reflat=id(33)+id(34)/60.0+id(35)/(3600.0 * ID(68))
      if (id(36).lt.0 .and. id(37).gt.0) then
c
c     deal with kludge of only degree portion being negative
c
         reflon=id(36)-id(37)/60.0-id(38)/(3600.0 * ID(68))
      else
         reflon=id(36)+id(37)/60.0+id(38)/(3600.0 * ID(68))
      end if
      reflon=lon_sign*abs(reflon)
c
c     Political map overlays when grid is latlon, not xy:
c        Change map origin to latitude of equator (reflat=0.0).
c        Set ll- and ur-lat/lon to actual lat/lon grid corners 
c          (xr1,yr1) and (xr2,yr2).  Otherwise transform
c          cartesian corners to lat/lon values.
c        Change from 'OR' to 'CE' projection.
c
      if(latlon)then
         reflat=0.0
         call maproj('CE',reflat,-reflon,(90.-angxax))
      else
         call maproj('OR',reflat,-reflon,(90.-angxax))
      end if

C     In MAPSTC: 'CO' - continental outlines only, 'US' - US state outlines
C                'PS' - continental, US state, and international outlines,
C                'PO' - continental and international outlines

      call mapstl('PE',.FALSE.)
c      CALL MAPSTL('DO',.TRUE.)
      CALL MAPSTL('DO',.FALSE.)
      CALL MAPSTL('LA',.FALSE.)
c      CALL MAPSTC('OU','US')
      CALL MAPSTC('OU','PS')
      if(latlon)then
         lllat=yr1
         lllong=abs(xr1)
         urlat=yr2
         urlong=abs(xr2)
      else
         call xy2lldrv(lllat,lllong,xr1,yr1,reflat,reflon,angxax)
         call xy2lldrv(urlat,urlong,xr2,yr2,reflat,reflon,angxax)
         lllong=lon_sign*abs(lllong)
         urlong=lon_sign*abs(urlong)
      end if

C      print *,'Genmap:',reflat,reflon,angxax
C      print *,'Genmap:',lllat,-lllong,urlat,-urlong

      call mapset('CO',lllat,-lllong,urlat,-urlong)
      call mapint
c      CALL MAPSTL('DO',.TRUE.)
      CALL MAPSTL('DO',.FALSE.)

C     Get default line thickness (ILW) and reset to JLW.
C     Restore line thickness before leaving routine.
C
      CALL GETUSV('LW',ILW)
      JLW=THKMAP*ILW
      IF(JLW.LT.ILW)JLW=ILW
      CALL SETUSV('LW',JLW)
c      print *,'GENMAP: ilw,jlw=',ilw,jlw

      land=.false.
      if (imptyp.le.0 .or. imptyp.gt.6) then
         goto 1000                     ! Bypass map plot completely
      else if (imptyp.eq.1) then
         iusout=3                      ! Plot map only (US state outlines)
      else if (imptyp.eq.2) then
         iusout=-2                     ! Plot 'invisible' map and landmarks
         land=.true.
      else if (imptyp.ge.3) then
         iusout=3                      ! Plot map and landmarks
         land=.true.
      end if
*
*     Check latitude and longitude; note that we assume Western Hemisphere
*     (i.e., lat and long of 0,0 are invalid)
*
      if (     id(33).eq.id(67) .or. id(36).eq.id(67) .or.
     .         id(33).eq. 0     .or. id(36).eq. 0) THEN
         call cederx(569,0)
         call set(fl,fr,fb,ft,ul,ur,ub,ut,ll)
         RETURN
      end if
*
*
      if (iusout.eq.3) then
         call maplot
      end if
      CALL SETUSV('LW',ILW)
*
*  If and only if GALE project, plot specific radar locations (regardless
*  of landmark status).
*
      write(ctemp1,23)id(8)
 23   format(a2)
      write(ctemp2,23)id(9)
      if (ctemp1.eq.'GA' .and. ctemp2.eq.'LE') then
         call maptrn(33.95,-83.317,x,y)
         call plchmq(x,y,'''RKU''A',12.,0.,0.)
         call maptrn(35.267,-75.5,x,y)
         call plchmq(x,y,'''RKU''H',12.,0.,0.)
         call maptrn(32.90,-80.0,x,y)
         call plchmq(x,y,'''RKU''C',12.,0.,0.)
         call maptrn(34.20,-77.9,x,y)
         call plchmq(x,y,'''RKU''I',12.,0.,0.)
         call maptrn(36.9,-78.684,x,y)
         call plchmq(x,y,'''RKU''V',12.,0.,0.)
      end if
*
* Plot all landmarks.
*
      call set(fl,fr,fb,ft,xr1,xr2,yr1,yr2,ll)
      if (land) then
         call gsclip(1)  ! no markers off grid
         do 100 i=0,id(302)-1
            xx=id(309 + i*6)/(1.0 * id(68))
            yy=id(310 + i*6)/(1.0 * id(68))
            call plchhq(xx,yy,'&KRL&G',12.,0.,0.)
100      continue
         call gsclip(0)
      end if            ! LAND
1000  continue          ! Routine exit point
      
      call set(fl,fr,fb,ft,ul,ur,ub,ut,ll)
      RETURN
      END


