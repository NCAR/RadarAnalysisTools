C
C	MXG  - Maximum number of allowed ranges (heights).  This
C              should be the same as MXR = 512 set in VADDRV.
C       MXVD - Maximum number of original gridded vertical levels.
C              This parameter should be the same as MAXZLEV = 51
C              set in CEDRIC.INC
C
      PARAMETER (MXVD=51,MXG=512)

      COMMON/VADWINDS/MVD,NAMVD(MXVD),U0(MXG,MXVD),V0(MXG,MXVD),
     +     SPD(MXG,MXVD),DIR(MXG,MXVD),DIV(MXG,MXVD),ERR(MXG,MXVD),
     +     DBZ(MXG,MXVD),WVD(MXG,MXVD),STR(MXG,MXVD),SHR(MXG,MXVD),
     +     U_VD(MXVD),V_VD(MXVD),AZMVD(MXVD),IFLVAD(MXVD),
     +     NAMINVD(MXVD),NAMPLVD(MXVD),VADTYPE(MXVD),VARUVW(MXG,MXVD),
     +     COV_UW(MXG,MXVD),COV_VW(MXG,MXVD),COV_UV(MXG,MXVD),
     +     AVAD0(MXG,MXVD),AVAD(MXG,MXVD,2),BVAD(MXG,MXVD,2),
     +     RBEG,REND,RDEL,NR,VTYPE,KFIT,DXY
      CHARACTER*8 NAMVD,NAMINVD,NAMPLVD
      CHARACTER*4 VADTYPE
      CHARACTER*8 VTYPE

