      SUBROUTINE VADRINGS(RADVEL,REFLEC,SRNG,AZM,ELEV,MAXX,MAXY,NX,NY,
     X     RNG,NANG,Rring,Aring,VDAT,ZDAT,MXR,MXA,NR,DR)
C
C     Extract datasets for input to the VAD analysis.  Get azimuth, 
C     radial velocity, and reflectivity data for NR slant range rings 
C     centered at RNG and having width DR.

C     Regular grid:
C        MAXX, MAXY - maximum numbers of grid points
C        NX,NY  - actual numbers of grid points
C        RADVEL - gridded radial velocities
C        REFLEC - gridded reflectivities
C        SRNG   - slant ranges 
C        AZM    - azimuth angles
C        ELEV   - elevation angles
C
      DIMENSION RADVEL(MAXX,MAXY),REFLEC(MAXX,MAXY)
      DIMENSION SRNG(MAXX,MAXY),AZM(MAXX,MAXY),ELEV(MAXX,MAXY)

C     Quantities within range rings:
C        MXR  - maximum number of range rings
C        MXA  - maximum number of grid points 
C        NR   - actual number of range rings
C     One-dimensional arrays with NR values
C        RNG  - ranges to the centers of the range rings
C        NANG - number of grid points within the current ring
C     Two-dimensional arrays with NR x NANG(NR) values
C        AZA  - azimuth angles within the current range ring
C        VDAT - radial velocities within range ring
C        ZDAT - reflectivities within range ring
C
      DIMENSION RNG(MXR),Rring(MXR,MXA),Aring(MXR,MXA),NANG(MXR)
      DIMENSION VDAT(MXR,MXA),ZDAT(MXR,MXA)

      print *,'VADRINGS: maxx,maxy=',maxx,maxy
      print *,'VADRINGS:   mxr,mxa=',mxr,mxa
      print *,'VADRINGS: nxy,nr,dr=',nx,ny,nr,dr

      DO N = 1,NR
         R1 = RNG(N)-DR
         R2 = RNG(N)+DR
         NANG(N)=0

C     Loop over regular grid and extract information
C
         DO J = 1,NY
            DO I = 1,NX
               IF(SRNG(I,J) .GE. R1 .AND. SRNG(I,J) .LE. R2)THEN
                  NANG(N)=NANG(N)+1
                  Rring(N,NANG(N))=SRNG(I,J)
                  Aring(N,NANG(N))=AZM(I,J)
                  VDAT(N,NANG(N))=RADVEL(I,J)
                  ZDAT(N,NANG(N))=REFLEC(I,J)
               END IF
               IF(NANG(N).GE.MXA)GO TO 100
            END DO
         END DO
 100     CONTINUE
c         print *,'VADRINGS: n,r1,rng,r2,nang=',n,r1,rng(n),r2,nang(n)
      END DO

      RETURN
      END
