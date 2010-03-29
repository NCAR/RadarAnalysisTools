C
C $Id: hstopl.f,v 1.6 2000/08/22 15:04:49 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
C *************************************************************
C
      SUBROUTINE HSTOPL(IOPT)
C
C *************************************************************
C
C  THESE THREE ROUTINES ARE OPTION ROUTINES TO BE USED WITH
C  THE HISTOGRAM ROUTINE -- HISTGR
C
C  SET THE HISTOGRAM OPTIONS
C
C  INPUT
C     IOPT-CHARACTER STRING OF OPTION VALUE
C
C  SET COMMON DATA EQUAL TO INPUT DATA
C
C
      COMMON /HSTGC1/ HORZNT, PERCNT, MIDVAL, SHADE, MEDIAN, PERIM,
     -       HFRAME, LISTOP, WINDOW, COLORS, HSTFOR, TITLE, LABEL,
     -       FREQNC, HWIND(4), COLSHA, COLREC, COLAXI, COLMED, COLTEX,
     -       COLTIT, COLPER, DRAWL, SPACE, LABMAX, CHARL, HEIGHT,
     -       ORIENT, COLSH2, SETSPA, SETSP2, MVALU, SETMVA, SETEPS,
     -       NMVAL, PMVAL, PERTIT
      LOGICAL HORZNT, PERCNT, MIDVAL, SHADE, MEDIAN, PERIM, HFRAME,
     -        LISTOP, WINDOW, COLORS, HSTFOR, TITLE, LABEL, FREQNC,
     -        DRAWL, SPACE, CHARL, MVALU, NMVAL, PMVAL, PERTIT
      COMMON /HSTGC2/ STRFOR, STRTIT, STRLAB, STRFRE, STRPER, LABTEX
      CHARACTER*96  STRTIT
      CHARACTER*55  STRFOR, STRLAB, STRFRE, STRPER
      CHARACTER*15 LABTEX(30)
      CHARACTER*7  IOPT
      CHARACTER*2  TAG, OPT
      INTEGER  COLSHA, COLREC, COLAXI, COLMED, COLTEX
      INTEGER  COLTIT, COLPER, COLSH2, HEIGHT, ORIENT
C
      NERR = 0
C
C  DETERMINE OPTION AND ITS VALUE
C
      TAG = IOPT(1:2)
      IF (IOPT(3:3) .EQ. '=') THEN
          OPT = IOPT(4:5)
      ELSE
          OPT = IOPT(5:6)
      ENDIF
C
C  'DEF' FLAG,  SET ALL OPTIONS TO DEFAULT VALUES
C
      IF (TAG .EQ. 'DE') THEN
C
C  SET DEFAULT FORMAT
C
        HORZNT = .FALSE.
        PERCNT = .TRUE.
        MIDVAL = .TRUE.
        SPACE = .TRUE.
        SHADE = .TRUE.
        DRAWL = .FALSE.
        MEDIAN = .FALSE.
	MVALU  = .FALSE.
        PERIM = .FALSE.
        HFRAME = .TRUE.
        LISTOP = .FALSE.
        WINDOW = .FALSE.
        HWIND(1) = 0.
        HWIND(2) = 1.
        HWIND(3) = 0.
        HWIND(4) = 1.
        COLORS = .FALSE.
        COLSHA = 1
        COLSH2 = 1
        COLREC = 1
        COLAXI = 1
        COLTEX = 1
        COLMED = 1
        COLTIT = 1
        COLPER = 1
        SETSPA = 2.
        SETSP2 = -1.
	NMVAL = .TRUE.
	PMVAL = .TRUE.
	SETMVA = -999.
	SETEPS = 1.E-10
        HSTFOR = .FALSE.
        STRFOR = '(G10.3)'
	STRTIT = ' '
	STRFRE = 'FREQUENCY'
	STRPER = 'PERCENT OCCURRENCE'
        TITLE = .FALSE.
        LABEL = .FALSE.
        FREQNC = .FALSE.
	PERTIT = .FALSE.
        CHARL = .FALSE.
        HEIGHT = 2
        ORIENT = 0
      RETURN
C
C  HORIZONTAL FLAG
C
      ELSEIF (TAG .EQ. 'HO') THEN
C
C  SWITCH = ON
C
          IF (OPT .EQ. 'ON') THEN
            HORZNT = .TRUE.
            RETURN
C
C  SWITCH = OFF
C
          ELSEIF (OPT .EQ. 'OF') THEN
            HORZNT = .FALSE.
            RETURN
          ELSE
                GOTO 900
          ENDIF
C
C  PERCENT FLAG
C
      ELSEIF (TAG .EQ. 'PE') THEN
C
C  SWITCH = ON
C
          IF (OPT .EQ. 'ON') THEN
            PERCNT = .TRUE.
            RETURN
C
C  SWITCH = OFF
C
          ELSEIF (OPT .EQ. 'OF') THEN
            PERCNT = .FALSE.
            RETURN
          ELSE
                GOTO 900
          ENDIF
C
C  MIDVALUE FLAG
C
      ELSEIF (TAG .EQ. 'MI') THEN
C
C  SWITCH = ON
C
          IF (OPT .EQ. 'ON') THEN
            MIDVAL = .TRUE.
           RETURN
C
C  SWITCH = OFF
C
          ELSEIF (OPT .EQ. 'OF') THEN
           MIDVAL = .FALSE.
           RETURN
          ELSE
                GOTO 900
          ENDIF
C
C  SHADE FLAG
C
      ELSEIF (TAG .EQ. 'SH') THEN
C
C  SWITCH ON
C
          IF (OPT .EQ. 'ON') THEN
            SHADE = .TRUE.
            RETURN
C
C  SWITCH OFF
C
          ELSEIF (OPT .EQ. 'OF') THEN
            SHADE = .FALSE.
            RETURN
          ELSE
                GOTO 900
          ENDIF
C
C  DRAWLINE FLAG
C
      ELSEIF (TAG .EQ. 'DR') THEN
C
C  SWITCH ON
C
          IF (OPT .EQ. 'ON') THEN
            DRAWL = .TRUE.
            RETURN
C
C  SWITCH OFF
C
          ELSEIF (OPT .EQ. 'OF') THEN
            DRAWL = .FALSE.
            RETURN
          ELSE
                GOTO 900
          ENDIF
C
C  MEDIAN FLAG
C
      ELSEIF (TAG .EQ. 'ME') THEN
C
C  SWITCH ON
C
          IF (OPT .EQ. 'ON') THEN
            MEDIAN = .TRUE.
            RETURN
C
C  SWITCH OFF
C
          ELSEIF (OPT .EQ. 'OF') THEN
            MEDIAN = .FALSE.
            RETURN
          ELSE
                GOTO 900
          ENDIF
C
C  PERIMETER FLAG
C
      ELSEIF (TAG .EQ. 'PR') THEN
C
C  SWITCH ON
C
          IF (OPT .EQ. 'ON') THEN
            PERIM = .TRUE.
            RETURN
C
C  SWITCH OFF
C
          ELSEIF (OPT .EQ. 'OF') THEN
            PERIM = .FALSE.
            RETURN
          ELSE
                GOTO 900
          ENDIF
C
C  FRAME FLAG
C
      ELSEIF (TAG .EQ. 'FR') THEN
C
C  SWITCH ON
C
          IF (OPT .EQ. 'ON') THEN
            HFRAME = .TRUE.
            RETURN
C
C  SWITCH OFF
C
          ELSEIF (OPT .EQ. 'OF') THEN
            HFRAME = .FALSE.
            RETURN
          ELSE
                GOTO 900
          ENDIF
C
C  Missing Value normalization flag
C
      ELSEIF (TAG .EQ. 'NM') THEN
C
C  SWITCH ON
C
          IF (OPT .EQ. 'ON') THEN
	    NMVAL = .TRUE.
            RETURN
C
C  SWITCH OFF
C
          ELSEIF (OPT .EQ. 'OF') THEN
	    NMVAL = .FALSE.
            RETURN
          ELSE
                GOTO 900
          ENDIF
C
C  Missing value print flag
C
      ELSEIF (TAG .EQ. 'PM') THEN
C
C  SWITCH ON
C
          IF (OPT .EQ. 'ON') THEN
	    PMVAL = .TRUE.
            RETURN
C
C  SWITCH OFF
C
          ELSEIF (OPT .EQ. 'OF') THEN
	    PMVAL = .FALSE.
            RETURN
          ELSE
                GOTO 900
          ENDIF
C
C  LIST OPTION GET VALUE OF SWITCH
C
      ELSEIF (TAG .EQ. 'LI') THEN
C
C  ON SET LIST OPTIONS FLAG
C
          IF (OPT .EQ. 'ON') THEN
            LISTOP = .TRUE.
              RETURN
C
C  TURN OFF LIST OPTIONS FLAG
C
          ELSEIF (OPT .EQ. 'OF') THEN
            LISTOP = .FALSE.
              RETURN
          ELSE
                GOTO 900
          ENDIF
      ELSE
          GOTO 900
      ENDIF
C
C  ERROR UNDEFINED OPTION DETECTED
C
 900  NERR = NERR + 1
      CALL SETER (' HSTOPL -- UNDEFINED OPTION',NERR,IREC)
      RETURN
C
      END
