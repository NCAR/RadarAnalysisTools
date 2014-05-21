C
C     ORIGINAL PARAMETERIZATION (MXR=768,MXA=450,MXF=20)
C                        PPIDIM (MXR=950,MXA=425,MXF=26)
C                        RHIDIM (MXR=512,MXA=200,MXF=50)
C                        LAIRD  (MXR=512,MXA=750,MXF=12)
C
C     Set (NSMX,NDMX) in scat.inc for PPI_MMM.f
C
C     PARAMETER (NSMX=100,NDMX=75000,MXDS=NSMX*NDMX) - DEC/Alpha only
C     PARAMETER (NSMX= 50,NDMX=50000,MXDS=NSMX*NDMX) - Rest
C
C     DOREC.f and RDDO.f: (MXG_DOR,MXF_DOR) in dim.inc
C                         (MXGAT,MXFLD)=(1500,100) in dorade.h
C     RDNEXRAD.f:         (MXG_NEX,MXF_NEX)
C                         (MXGAT,MXFLD)=(1837, 3) in nexrad.h
C                With 460 reflectivity gates, MXGAT = 4*(460-1)+1 = 1837
C
      PARAMETER (MXG_NEX=1837,MXF_NEX=3)
      PARAMETER (MXG_DOR=1500,MXF_DOR=100)
      PARAMETER (MXA=400,MXR=1500,MXF=100)