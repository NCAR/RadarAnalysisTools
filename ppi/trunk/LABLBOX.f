c
c----------------------------------------------------------------------X
c
      SUBROUTINE LABLBOX (IRTYPE,XRT,YTP)
C
C     Upper right-hand corner labels:  Add a color-filled box
C     followed by the statitical quantities (NMB, AVT, STD, ...).
C        ICOLBX  - Color index for the color-filled box
C        FXP,FYP - Arrays for the (X,Y) locations of the corners of 
C                  the box (CCW: UR --> UL --> LL --> LR --> UR)
C     Note: XRT_typ and YTP_typ are set here accordingto their
C           values in PLTSCAT, PLTHIST, and PLTSPEC
C           In PLTSPEC YTP2 is the variable needed, not
C           YTP as in the other PLT... subroutines.
C
      INCLUDE 'colors.inc'
      CHARACTER*4 IRTYPE
      DATA XRT_sct,YTP_sct/0.90,0.94/
      DATA XRT_hst,YTP_hst/0.97,0.94/
      DATA XRT_spc,YTP_spc/0.96,0.71/

C     Array for storing the corners of a color-filled box
C     before labels are done in the upper right-hand corner
C
      DIMENSION FXP(5),FYP(5)

C     Add labeling of quantities in the upper right-hand
C     corner for scatter plots and histograms.
C     IRTYPE - SCAT and HIST
C
C     BOUNDS OF BOX (With _typ = _sct or _hst or _spc)
C
C        Upper right (XUR=XRT_typ,      YUR=YTP_typ)
C        Upper left  (XUL=XRT_typ-XDEL, YUL=YTP_typ)
C        Lower left  (XLL=XRT_typ-XDEL, YLL=YTP_typ-YDEL)
C        Lower right (XLR=XRT_typ,      YLR=YTP_typ-YDEL)
C
C     Set line (GSPLCI) color (also used for characters
C     inside the color-filled box)
C     Background (Color index 0): CALL GSCR(1,0,...)
C     Foreground (Color index 1): CALL GSCR(1,1,...)
C
c-----print *,'LABLBOX: irtype,xrt,ytp=',irtype,xrt,ytp
      CALL SFLUSH
      ICOLBX = IGRAY-1
      CALL GSPLCI(1)
      IF(IRTYPE.EQ.'SCAT')THEN
c         XRT_sct = XRT
c         YTP_sct = YTP
         Xdel = 0.15
         Ydel = 0.03+5*0.02
         XUR = XRT_sct
         YUR = YTP_sct
         XUL = XRT_sct-Xdel
         YUL = YTP_sct
         XLL = XRT_sct-Xdel
         YLL = YTP_sct-Ydel
         XLR = XRT_sct
         YLR = YLL
         FXP(1) = XUR
         FXP(2) = XUL
         FXP(3) = XLL
         FXP(4) = XLR
         FYP(1) = YUR
         FYP(2) = YUL
         FYP(3) = YLL
         FYP(4) = YLR
         CALL FAREA(ICOLBX,FXP,FYP,5)
         CALL LINE(XUR,YUR,XUL,YUL)
         CALL LINE(XUL,YUL,XLL,YLL)
         CALL LINE(XLL,YLL,XLR,YLR)
         CALL LINE(XLR,YLR,XUR,YUR)
      ENDIF
      IF(IRTYPE.EQ.'HIST')THEN
c         XRT_hst = XRT
c         YTP_hst = YTP
         Xdel = 0.15
         Ydel = 0.03+6*0.02
         XUR = XRT_hst
         YUR = YTP_hst
         XUL = XRT_hst-Xdel
         YUL = YTP_hst
         XLL = XRT_hst-Xdel
         YLL = YTP_hst-Ydel
         XLR = XRT_hst
         YLR = YLL
         FXP(1) = XUR
         FXP(2) = XUL
         FXP(3) = XLL
         FXP(4) = XLR
         FYP(1) = YUR
         FYP(2) = YUL
         FYP(3) = YLL
         FYP(4) = YLR
         CALL FAREA(ICOLBX,FXP,FYP,5)
         CALL LINE(XUR,YUR,XUL,YUL)
         CALL LINE(XUL,YUL,XLL,YLL)
         CALL LINE(XLL,YLL,XLR,YLR)
         CALL LINE(XLR,YLR,XUR,YUR)
      ENDIF
      IF(IRTYPE.EQ.'SPEC')THEN
         XRT_spc = XRT
         YTP_spc = YTP
         Xdel = 0.25
         Ydel = 0.01+7*0.02
         XUR = XRT_spc
         YUR = YTP_spc
         XUL = XRT_spc-Xdel
         YUL = YTP_spc
         XLL = XRT_spc-Xdel
         YLL = YTP_spc-Ydel
         XLR = XRT_spc
         YLR = YLL
         FXP(1) = XUR
         FXP(2) = XUL
         FXP(3) = XLL
         FXP(4) = XLR
         FYP(1) = YUR
         FYP(2) = YUL
         FYP(3) = YLL
         FYP(4) = YLR
         CALL FAREA(ICOLBX,FXP,FYP,5)
         CALL LINE(XUR,YUR,XUL,YUL)
         CALL LINE(XUL,YUL,XLL,YLL)
         CALL LINE(XLL,YLL,XLR,YLR)
         CALL LINE(XLR,YLR,XUR,YUR)
      ENDIF

C     Reset line color to index (1)
C
      CALL SFLUSH
      CALL GSPLCI(1)
      
      RETURN
      END
