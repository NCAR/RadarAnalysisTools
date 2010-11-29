      PROGRAM DRIVER
C
C     DRIVER ROUTINE FOR READING CEDRIC FILES.
C     REPLACE THIS WITH YOUR OWN PROGRAM.
C
      PARAMETER (MAXX=512,MAXY=512,NUMZ=20,NFLD=2,NID=510)

      DIMENSION ID(NID)
      COMMON /DAT/ FDAT(MAXX,MAXY,NUMZ,NFLD)
      INTEGER INF, FLD
      CHARACTER*30 filenam

      print *, 'Enter output filename:'
      read *,filenam
      strlen=index(filenam,' ')-1
      open(unit=16,file=filenam(1:strlen),status='unknown')

      CALL CEDREAD(ID)

C Change iori,jori header info
      if(id(160).lt.0) then
         iori=(((0-id(160))*1000/id(163))+100)
      else
         iori=(((0-id(160))*1000/id(163))-100)
      endif
      if(id(165).lt.0) then
         jori=(((0-id(165))*1000/id(168))+100)
      else
         jori=(((0-id(165))*1000/id(168))-100)
      endif
      id(309)=iori
      id(310)=jori

C     WRITE HEADER TO ASI
      write(16,900) id
 900  format(10i8)

C WRITE OUT FIELD INFO
      do kot = 1,id(172)
         write(16,901) kot
 901     format('level',i2)
         do J = 1,id(167)
            write(16,902)j
 902        format('azimuth',i3)
            do FLD = 1,id(175)
               INF=171+(5*FLD)
               WRITE(16,903)ID(INF),ID(INF+1),ID(INF+2),ID(INF+3)
 903           format(4A2)
               write(16,904)(fdat(i,j,kot,FLD),i=1,id(162))
 904           format(8E10.3)
            enddo
         enddo
      enddo


      END

      SUBROUTINE PLACEPLANE(RBUF,LF,KOT,FLDNAM,NX,NY)
C
C     SAMPLE ROUTINE. THIS ROUTINE SIMPLY STORES
C     ALL THE DATA IN A SINGLE 4-D ARRAY. REPLACE
C     THIS ROUTINE WITH YOUR OWN.
C
      PARAMETER (MAXX=512,MAXY=512,NUMZ=20,NFLD=2)

      COMMON /DAT/ FDAT(MAXX,MAXY,NUMZ,NFLD)
      DIMENSION RBUF(MAXX,MAXY)
      CHARACTER*8 FLDNAM

      DO J=1,NY
         DO I=1,NX
            FDAT(I,J,KOT,LF)=RBUF(I,J)
         END DO
      END DO

      RETURN

      END


      
