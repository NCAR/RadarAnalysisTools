      PROGRAM DRIVER_RDWRT
C
C     NOTICE
C     Copyright 1995 University Corporation for Atmospheric Research
C     National Center for Atmospheric Research
C
C This software was developed by NCAR, which is operated by UCAR and sponsored 
C by the National Science Foundation.
C
C Access and use of this software shall impose the following obligations on the
C user.  The user is granted the right, without any fee or cost, to use, copy,
C modify, alter, enhance and distribute this software, and any derivative works
C thereof, and its supporting documentation for any purpose whatsoever, except
C commercial sales, provided that this entire notice appears in all copies of 
C the software, derivative works and supporting documentation.  Further, the 
C user agrees to credit UCAR/NCAR in any publications that result from the use
C of this software or in any software package that includes this software.  
C The names UCAR/NCAR, however, may not be used in any advertising or 
C publicity to endorse  or promote any products or commercial entity unless 
C specific written permission is obtained from UCAR/NCAR.  The user also 
C understands that UCAR/NCAR is not obligated to provide the user with any 
C support, consulting, training or assistance of any kind with regard to the 
C use, operation and performance of this software nor to provide the user with 
C any updates, revisions, new versions or ``bug fixes.''
C
C THIS SOFTWARE IS PROVIDED BY UCAR/NCAR ``AS IS'' AND ANY EXPRESS OR IMPLIED
C WARRANTIES, INCLUDING BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
C MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO
C EVENT SHALL UCAR/NCAR BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
C DAMAGES OR ANY DAMAGES WHATSOEVER, INCLUDING BUT NOT LIMITED TO CLAIMS
C ASSOCIATED WITH THE LOSS OF DATA OR PROFITS, WHICH MAY RESULT FROM AN ACTION
C IN CONTRACT, NEGLIGENCE OR OTHER TORTIOUS CLAIM THAT ARISES OUT OF OR IN
C CONNECTION WITH THE ACCESS, USE OR PERFORMANCE OF THIS SOFTWARE.
C
C     DRIVER ROUTINE FOR READING/WRITING CEDRIC FILES.
C     REPLACE THIS WITH YOUR OWN PROGRAM.
C     Note: CEDREAD calls user-replaceable PLACEPLANE where the 
C           CEDRIC-format data can be stored in internal array FDAT.
C           This example stores all CEDRIC fields.
C     Note: WRITCED calls user-replaceable FETCHPLANE where the 
C           user-generated data for each z-level can be retrieved.
C           This example fetchs data from the previously read data
C           stored in FDAT.
C
      INCLUDE 'CEDRIC.INC'
      PARAMETER (MXLND=7)
      DIMENSION ID(NID)
      COMMON /DAT/ FDAT(MAXX,MAXY,MAXZ,MAXF)
      CHARACTER*8 NAMFLD,LABLAX
      DATA SCFAC/1000.0/

      CHARACTER*4 PROJECT
      CHARACTER*6 SCINAME,NAMLND(MXLND),RADSTN,TAPE
      CHARACTER*8 VOLNAM,FLDNAM(MAXFLD),SOURCE
      DIMENSION ISCLFLD(MAXFLD),XLND(MXLND),YLND(MXLND),ZLND(MXLND)
      DIMENSION IDAT(MAXPLN)

      CALL VERSOUT
      IUN = 20

      do nvol=1,1
        write(*,*)' '
         write(*,*)'********************'
         write(*,*)'Read CEDRIC volume =',nvol
         write(*,*)'********************'

         CALL CEDREAD(IUN,NX,NY,NZ,NFLDS,NPLIN,BAD,ID)

         write(*,*)'********************'
         write(*,*)'Number of fields  =',nflds
         write(*,*)'Nx, Ny, Nz        =',nx,ny,nz
         write(*,*)'No. points each z =',nplin
         write(*,*)'Bad data flag     =',bad
         write(*,*)'********************'

C        calculate statistics for all NFLDS fields.
C
         IGENSCL=ID(68)
         XMIN=ID(160)/IGENSCL
         XMAX=ID(161)/IGENSCL
         NMBX=ID(162)
         SPCX=ID(163)/SCFAC
         
         YMIN=ID(165)/IGENSCL
         YMAX=ID(166)/IGENSCL
         NMBY=ID(167)
         SPCY=ID(168)/SCFAC
         
         ZMIN=ID(170)/SCFAC
         ZMAX=ID(171)/SCFAC
         NMBZ=ID(172)
         SPCZ=ID(173)/SCFAC

         lablax=' x (km):'
         write(6,9)lablax,xmin,xmax,spcx,nmbx
         lablax=' y (km):'
         write(6,9)lablax,ymin,ymax,spcy,nmby
         lablax=' z (km):'
         write(6,9)lablax,zmin,zmax,spcz,nmbz
 9       format(a8,3f10.2,i10)
         write(*,*)'********************'

         do n=1,nflds
            INF=171+(5*N)
            WRITE(NAMFLD,11)ID(INF),ID(INF+1),ID(INF+2),ID(INF+3)
 11         FORMAT(4A2)
            write(6,15)NAMFLD
 15         format(a8,'  Z (km)     Avg     Std  Number     Min    Max')
            do k=1,nz
               z=zmin+(k-1)*spcz
               sum   = 0.0
               sum2  = 0.0
               numb  = 0
               fmin  = 1000.0
               fmax  =-1000.0
               avg   = 0.0
               stdev = 0.0
               do j=1,ny
                  do i=1,nx
                     if(fdat(i,j,k,n).ne.bad)then
                        rdat=fdat(i,j,k,n)
                        sum=sum+rdat
                        sum2=sum2+rdat*rdat
                        numb=numb+1
                        if(rdat.le.fmin)fmin=rdat
                        if(rdat.ge.fmax)fmax=rdat
                     end if
                  end do
               end do
               if(numb.ge.2)then
                  avg   = sum/numb
                  avgsqr= avg*avg
                  avg2  = sum2/numb
                  if(avg2.ge.avgsqr)stdev = sqrt(avg2-avg*avg)
               end if
               write(6,17)z,avg,stdev,numb,fmin,fmax
 17            format(8x,3f8.2,i8,2f8.2)
            end do
         end do
      
C     Fill part of the 510-word output CEDRIC header.
C     Replace with your own information.
C
         CALL CEDHEAD(SCINAME,BASANG,VOLNAM,IBEGYR,
     X        IBEGMNT,IBEGDAY,IBEGHR,IBEGMIN,IBEGSEC,IENDYR,
     X        IENDMNT,IENDDAY,IENDHR,IENDMIN,IENDSEC,XMIN,
     X        XMAX,NUMX,ISPCX,YMIN,YMAX,NUMY,ISPCY,ZMIN,
     X        ZMAX,NUMZ,ISPCZ,NFLD,FLDNAM,ISCLFLD,NUMLND,
     X        NUMRAD,NAMLND,XLND,YLND,ZLND,MXLND,RADSTN,SOURCE,
     X        PROJECT,TAPE,RADCON,VNYQ,LATDEG,LATMIN,LATSEC,
     X        LONDEG,LONMIN,LONSEC,BAD)
         
         write(*,*)' '
         write(*,*)'********************'
         write(*,*)'Write CEDRIC volume =',nvol
         write(*,*)'********************'

         nplin = numx * numy
         spcx = ispcx/scfac
         spcy = ispcy/scfac
         spcz = ispcz/scfac
         zmnkm = zmin/scfac
         zmxkm = zmax/scfac

         write(*,*)'********************'
         write(*,*)'Number of fields  =',nfld
         write(*,*)'Nx, Ny, Nz        =',numx,numy,numz
         write(*,*)'No. points each z =',nplin
         write(*,*)'Bad data flag     =',bad
         write(*,*)'********************'
         lablax=' x (km):'
         write(6,9)lablax,xmin,xmax,spcx,numx
         lablax=' y (km):'
         write(6,9)lablax,ymin,ymax,spcy,numy
         lablax=' z (km):'
         write(6,9)lablax,zmnkm,zmxkm,spcz,numz
         write(*,*)'********************'

C        Write a CEDRIC-format data file to unit 30.
C
         CALL WRITCED(SCINAME,BASANG,VOLNAM,IBEGYR,
     X        IBEGMNT,IBEGDAY,IBEGHR,IBEGMIN,IBEGSEC,IENDYR,
     X        IENDMNT,IENDDAY,IENDHR,IENDMIN,IENDSEC,XMIN,
     X        XMAX,NUMX,ISPCX,YMIN,YMAX,NUMY,ISPCY,ZMIN,
     X        ZMAX,NUMZ,ISPCZ,NFLD,FLDNAM,ISCLFLD,NUMLND,
     X        NUMRAD,NAMLND,XLND,YLND,ZLND,MXLND,IDAT,RADSTN,
     X        SOURCE,PROJECT,TAPE,RADCON,VNYQ,LATDEG,LATMIN,
     X        LATSEC,LONDEG,LONMIN,LONSEC)
         
      end do

      END

      SUBROUTINE PLACEPLANE(RBUF,LF,KOT,NAMFLD,NX,NY)
C
C     SAMPLE ROUTINE. THIS ROUTINE SIMPLY STORES
C     ALL THE DATA IN A SINGLE 4-D ARRAY. REPLACE
C     THIS ROUTINE WITH YOUR OWN.
C
C     LF  - Field number 
C     KOT - Z-level number

      INCLUDE 'CEDRIC.INC'
      COMMON /DAT/ FDAT(MAXX,MAXY,MAXZ,MAXF)
      DIMENSION RBUF(MAXPLN)
      CHARACTER*8 NAMFLD

C     Store CEDRIC data fields in FDAT, the internal dataset.
C
c      WRITE(6,5)NX*NY,NAMFLD,KOT
c 5    FORMAT(8X,'Storing ',I8,' horiz grid point values of field ',
c     +     A8,' for z-level =',I3)


      DO J=1,NY
         DO I=1,NX
            IND =I + (J-1)*NX
            FDAT(I,J,KOT,LF)=RBUF(IND)
         END DO
      END DO

      RETURN

      END

      SUBROUTINE FETCHPLANE(IDAT,NUMX,NUMY,IF,ILEV,XMN,YMN,ZMN,
     X     DX,DY,DZ)
C
C     SAMPLE ROUTINE FOR FETCHING Z PLANES OF CEDRIC DATA.
C     FILLS IN ARRAY WITH DATA FROM LARGE ARRAY FDAT.
C
C     IF   - Field number
C     ILEV - Z-level number
C
      INCLUDE 'CEDRIC.INC'
      COMMON /DAT/ FDAT(MAXX,MAXY,MAXZ,MAXF)

      DIMENSION IDAT(MAXPLN)

      SCALE=100.

      WRITE(6,5)NUMX*NUMY,IF,ILEV
 5    FORMAT(8X,'Fetching ',I8,' horiz grid point values of field ',i3,
     +     ' for z-level =',I3)

      DO 100 J=1,NUMY
         DO 50 I=1,NUMX
            IND =I + (J-1)*NUMX
            IDAT(IND)=NINT(SCALE*(FDAT(I,J,ILEV,IF)))
 50      CONTINUE
 100  CONTINUE
      
      RETURN

      END
         
