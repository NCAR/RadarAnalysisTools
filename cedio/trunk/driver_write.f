      PROGRAM DRIVER_WRITE
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
C     DRIVER ROUTINE FOR WRITING CEDRIC FILES.
C     REPLACE THIS WITH YOUR OWN PROGRAM.
C     Note: WRITCED calls user-replaceable FETCHPLANE where the 
C           user-generated data for each z-level can be retrieved.
C           This example fetchs data from the previously read data
C           stored in FDAT.
C
      INCLUDE 'CEDRIC.INC'
      PARAMETER (MXLND=7)
      CHARACTER*4 PROJECT
      CHARACTER*6 SCINAME,SUBNAME,NAMLND(MXLND),RADSTN,TAPE
      CHARACTER*8 VOLNAM,FLDNAM(MAXFLD),SOURCE
      CHARACTER*8 LABLAX
      DATA SCFAC/1000.0/

      COMMON /DAT/ FDAT(MAXX,MAXY,MAXZ,MAXF)
      DIMENSION ISCLFLD(MAXFLD),XLND(MXLND),YLND(MXLND),ZLND(MXLND)
      DIMENSION IDAT(MAXPLN)

      CALL VERSOUT

C     Fill part of the 510-word output CEDRIC header.
C     Replace with your own information.
C
      CALL CEDHEAD(SCINAME,SUBNAME,BASANG,VOLNAM,IBEGYR,
     X     IBEGMNT,IBEGDAY,IBEGHR,IBEGMIN,IBEGSEC,IENDYR,
     X     IENDMNT,IENDDAY,IENDHR,IENDMIN,IENDSEC,XMIN,
     X     XMAX,NUMX,ISPCX,YMIN,YMAX,NUMY,ISPCY,ZMIN,
     X     ZMAX,NUMZ,ISPCZ,NFLD,FLDNAM,ISCLFLD,NUMLND,
     X     NUMRAD,NAMLND,XLND,YLND,ZLND,MXLND,RADSTN,SOURCE,
     X     PROJECT,TAPE,RADCON,VNYQ,LATDEG,LATMIN,LATSEC,
     X     LONDEG,LONMIN,LONSEC,BAD)

      do nvol=1,1
         write(*,*)' '
         write(*,*)'********************'
         write(*,*)'Write CEDRIC volume =',nvol
         write(*,*)'********************'
      
C        Write a CEDRIC-format data file to unit 30.
C
         CALL WRITCED(SCINAME,SUBNAME,BASANG,VOLNAM,IBEGYR,
     X        IBEGMNT,IBEGDAY,IBEGHR,IBEGMIN,IBEGSEC,IENDYR,
     X        IENDMNT,IENDDAY,IENDHR,IENDMIN,IENDSEC,XMIN,
     X        XMAX,NUMX,ISPCX,YMIN,YMAX,NUMY,ISPCY,ZMIN,
     X        ZMAX,NUMZ,ISPCZ,NFLD,FLDNAM,ISCLFLD,NUMLND,
     X        NUMRAD,NAMLND,XLND,YLND,ZLND,MXLND,IDAT,RADSTN,
     X        SOURCE,PROJECT,TAPE,RADCON,VNYQ,LATDEG,LATMIN,
     X        LATSEC,LONDEG,LONMIN,LONSEC)

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
 9       format(a8,3f10.2,i10)
         write(*,*)'********************'

C        calculate statistics for all NFLD fields.
C
         do n=1,nfld
c            INF=171+(5*N)
c            WRITE(FLDNAM,11)ID(INF),ID(INF+1),ID(INF+2),ID(INF+3)
c 11         FORMAT(4A2)
            write(6,15)fldnam(n)
 15         format(a8,'  Z (km)     Avg     Std  Number     Min    Max')
            do k=1,numz
               z=zmnkm+(k-1)*spcz
               sum   = 0.0
               sum2  = 0.0
               numb  = 0
               fmin  = 1000.0
               fmax  =-1000.0
               avg   = 0.0
               stdev = 0.0
               do j=1,numy
                  do i=1,numx
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
      end do
     
      END


      SUBROUTINE FETCHPLANE(IDAT,NUMX,NUMY,IF,ILEV,XMN,YMN,ZMN,
     X     DX,DY,DZ)
C
C     SAMPLE ROUTINE FOR FETCHING Z PLANES OF CEDRIC DATA.
C     FILLS IN ARRAY WITH BOGUS DATA
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

      Z=ZMN+(ILEV-1)*DZ
      DO 100 J=1,NUMY
         Y=YMN+(J-1)*DY
         DO 50 I=1,NUMX
            X=XMN+(I-1)*DX
            FXYZ=SQRT(X*X+Y*Y+Z*Z)
            IND=I + (J-1)*NUMX
            IF(IF.EQ.1)THEN
               IDAT(IND)=NINT(X*SCALE)
               FDAT(I,J,ILEV,IF)=X
            END IF
            IF(IF.EQ.2)THEN
               IDAT(IND)=NINT(Y*SCALE)
               FDAT(I,J,ILEV,IF)=Y
            END IF
            IF(IF.EQ.3)THEN
               IDAT(IND)=NINT(FXYZ*SCALE)
               FDAT(I,J,ILEV,IF)=FXYZ
            END IF
 50      CONTINUE
 100  CONTINUE

      RETURN

      END

