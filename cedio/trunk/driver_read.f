      PROGRAM DRIVER_READ
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
C     DRIVER ROUTINE FOR READING CEDRIC FILES.
C     REPLACE THIS WITH YOUR OWN PROGRAM.
C     Note: CEDREAD calls user-replaceable PLACEPLANE where the 
C           CEDRIC-format data can be stored in internal array FDAT.
C           This example stores all CEDRIC fields.
C
      INCLUDE 'CEDRIC.INC'
      DIMENSION ID(NID)
      COMMON /DAT/ FDAT(MAXX,MAXY,MAXZ,MAXF)
      CHARACTER*8 FLDNAM,LABLAX
      DATA SCFAC/1000.0/

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
            WRITE(FLDNAM,11)ID(INF),ID(INF+1),ID(INF+2),ID(INF+3)
 11         FORMAT(4A2)
c            write(6,15)fldnam
c 15         format(a8,'  Z (km)     Avg     Std  Number     Min    Max')

            WRITE(6,15)FLDNAM
 15         FORMAT(/1X,A8,' Z  KM',7X,'MEAN',4X,'STDV',6X,'N',3X,
     X           'I1  I2  J1  J2',8X,'MIN',5X,'MAX')

            do k=1,nz
               z=zmin+(k-1)*spcz
               sum   = 0.0
               sum2  = 0.0
               numb  = 0
               fmin  = 1000.0
               fmax  =-1000.0
               avg   = 0.0
               stdev = 0.0
               imin=1
               imax=nx
               jmin=1
               jmax=ny
               do j=1,ny
                  do i=1,nx
                     if(fdat(i,j,k,n).ne.bad)then
                        rdat=fdat(i,j,k,n)
                        sum=sum+rdat
                        sum2=sum2+rdat*rdat
                        numb=numb+1
                        if(rdat.le.fmin)fmin=rdat
                        if(rdat.ge.fmax)fmax=rdat
                        if(i.lt.imin)imin=i
                        if(i.gt.imax)imax=i
                        if(j.lt.jmin)jmin=j
                        if(j.gt.jmax)jmax=j
                     end if
                  end do
               end do
               if(numb.ge.2)then
                  avg   = sum/numb
                  avgsqr= avg*avg
                  avg2  = sum2/numb
                  if(avg2.ge.avgsqr)stdev = sqrt(avg2-avg*avg)
               end if
c               write(6,17)z,avg,stdev,numb,fmin,fmax
c 17            format(8x,3f8.2,i8,2f8.2)
               i1=imin
               i2=imax
               j1=jmin
               j2=jmax
               WRITE(6,17)Z,AVG,STDEV,NUMB,I1,I2,J1,J2,FMIN,FMAX
 17            FORMAT(8X,F8.3,F10.2,F8.2,I7,1X,4I4,3X,2F8.2)

            end do
         end do
      end do

      END

      SUBROUTINE PLACEPLANE(RBUF,LF,KOT,FLDNAM,NX,NY)
C
C     SAMPLE ROUTINE. THIS ROUTINE SIMPLY STORES
C     ALL THE DATA IN A SINGLE 4-D ARRAY. REPLACE
C     THIS ROUTINE WITH YOUR OWN.
C
C     LF: FIELD NUMBER 
C     KOT: Z-LEVEL NUMBER

      INCLUDE 'CEDRIC.INC'
      COMMON /DAT/ FDAT(MAXX,MAXY,MAXZ,MAXF)
      DIMENSION RBUF(MAXPLN)
      CHARACTER*8 FLDNAM

C     STORE CEDRIC DATA FIELDS IN FDAT, THE INTERNAL DATASET.
C
c      WRITE(6,5)NX*NY,FLDNAM,KOT
c 5    FORMAT(8X,'Storing ',I8,' horiz grid point values of field ',
c     +     A8,' for z-level =',I3)


      DO J=1,NY
         DO I=1,NX
            IND=I + (J-1)*NX
            FDAT(I,J,KOT,LF)=RBUF(IND)
         END DO
      END DO

      RETURN

      END





