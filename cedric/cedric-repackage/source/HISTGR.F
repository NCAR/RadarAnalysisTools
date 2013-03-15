      SUBROUTINE HISTGR( DAT1,NDIM,NPTS,IFLAG,CLASS,NCLASS,WRK,NWRK,
     X     NPPTS,IYMED,VMED,TPER)
C     
C     +-----------------------------------------------------------------+
C     |                                                                 |
C     |                Copyright (C) 1990 by UCAR                       |
C     |        University Corporation for Atmospheric Research          |
C     |                    All Rights Reserved                          |
C     |                                                                 |
C     |                 NCARGRAPHICS  Version 3.01                      |
C     |                                                                 |
C     +-----------------------------------------------------------------+
C     
C     PURPOSE       HISTGR plots a histogram with various options
C     including specification of class values,
C     spacing between histogram bars, shading of
C     bars, windowing (i.e. scaling), specification
C     of color, labels, titles, etc.  Data values
C     are partitioned into classes; histogram bars
C     represent either number of occurrences within
C     each class, or a Y-value associated with
C     that class (user choice).  Options are set
C     by calls to subroutines HSTOPL, HSTOPR, HSTOPC,
C     and HSTOPI before the call to HISTGR.
C     
C     USAGE         To draw a basic histogram (one utilizing
C     the default features):
C     
C     CALL HISTGR (DAT1,NDIM,NPTS,IFLAG,CLASS,NCLASS,WRK,NWRK)
C     
C     Error handling is performed by the ERPRT77
C     centralized error-handling package.  To
C     recover from a warning (recoverable error),
C     call ENTSR(IDUM,1) before calling HISTGR.
C     Otherwise an error message is printed and
C     the run is terminated.
C     
C     ARGUMENTS
C     
C     ON INPUT     DAT1   Two dimensional real array containing data of one
C     of two types, either values to be collected into
C     class intervals before plotting, or values which
C     have already been assigned to class intervals and
C     only need to be displayed.  See argument IFLAG
C     for a more complete description of HISTGR input
C     data options.  DAT1 is dimensioned: DAT1(NDIM,2).
C     
C     NDIM   The size of the first dimension of DAT1 as set
C     in the dimension statement of the calling program.
C     
C     NPTS   Number of values actually stored into DAT1 on this
C     call.  NPTS must always be less than or equal to
C     NDIM.
C     
C     IFLAG  An integer flag which selects one of four options
C     provided by the HISTGR utility.  The options are:
C     
C     IFLAG = 0;  A single array of length NPTS is loaded
C     into the DAT1 array.  HISTGR computes
C     NCLASS equally sized class intervals
C     that vary from the minimum value in
C     DAT1 to the maximum value in steps of
C     (MAX-MIN)/NCLASS.
C     
C     All values of DAT1 that fall in each
C     class interval are separately accumulated
C     for that interval.  The final tabulations
C     are plotted as a histogram of NCLASS bars.
C     The bar height can be labeled with the
C     number of points that fall within this
C     particular class interval (bin size),
C     or it can be given as a percentage of
C     the number of values input, NPTS.
C     
C     Note that under this option the user
C     has no control over the range of the
C     class intervals.  They are internally
C     determined from the range of the data.
C     
C     IFLAG = 1   This option is similar to the IFLAG = 0
C     option except that the user can select
C     the range of the class intervals into
C     which the data are collected.  For
C     example, say the user wants to collect
C     the number of occurrences of the DAT1
C     values that fall within 5 equally spaced
C     intervals in the value range from 0. to
C     10.  The user would then input NCLASS+1
C     class interval end points into array
C     CLASS, namely 0., 2., 4., 6., 8., and
C     10.  These values need not be entered in
C     monotonically increasing order and need
C     not be equally spaced.
C     
C     IFLAG = 2   This option allows the user to enter
C     and display data which has already
C     been accumulated into class intervals,
C     i.e., already available histograms.
C     The data input to DAT1 thus have
C     percentge of total, or number of
C     occurrences values.  In this case the
C     number of points in DAT1, NPTS, is
C     equal to the number of class intervals
C     (histogram bars), NCLASS.  The NCLASS
C     class interval midpoints are loaded
C     into array CLASS.  They do not have to
C     be of equal width.
C     
C     IFLAG = 3   This option is the same as option
C     IFLAG = 2 except that two histograms
C     can be displayed for comparison purposes.
C     The first histogram is loaded into
C     DAT1(NPTS,1).  The second histogram is
C     loaded into DAT1(NPTS,2).  The first
C     histogram can partially shade or obscure
C     the second histogram by the appropriate
C     selection of the SPAC and OVERLP options.
C     
C     Note that NPTS = NCLASS when IFLAG = 2 or 3.
C     
C     CLASS  Real array containing class values, dimensioned
C     (NCLASS+1).  This array has the following IFLAG
C     dependencies:
C     IFLAG = 0   CLASS is not used.
C     IFLAG = 1   NCLASS+1 class interval end points are
C     loaded into array CLASS.  They will be
C     sorted into a monotonically increasing
C     order, if not input in that order.  The
C     intervals need not be of equal width.
C     IFLAG = 2   NCLASS midpoint intervals are loaded
C     into array CLASS.  They must be in
C     monotonically increasing order, but
C     need not be of equal widths.  The
C     histogram bars will however be displayed
C     with equal widths.
C     IFLAG = 3   Same as for IFLAG = 2.
C     
C     NCLASS Number of class intervals (histogram bars) specified.
C     NCLASS must be .GE. 1.
C     
C     WRK    Real scratch array, dimensioned by NWRK
C     in the dimension statement of the calling program.
C     
C     NWRK   The dimension size of array WRK determined from:
C     NDIM + 3 * (NCLASS + 1)
C     
C     Note: The frequency axis label values will be integers
C     if the maximum Y-value (calculated within HISTGR)
C     is an integer multiple of 4.  Otherwise, real
C     values with format F11.1 are used.
C     
C     ON OUTPUT  All arguments remain unchanged except the scratch
C     array WRK.
C     
C     ENTRY POINTS  HSTBKD, HSTEXP, HSTMED, HSTOPC, HSTOPL, HSTOPR,
C     HSTOPI, HSTSTR, HISTGR, HSTLST, NWTSTR
C     
C     COMMON BLOCKS HSTGC1, HSTGC2
C     
C     I/O  Plots histograms using various options.
C     
C     PRECISION  Single
C     
C     REQUIRED  The ERPRT77 package and the SPPS.
C     LIBRARY
C     FILES
C     
C     REQUIRED GKS  0A
C     LEVEL
C     
C     LANGUAGE  FORTRAN 77
C     
C     HISTORY  First written in the fall of 1984.  Revised summer
C     of 1987.
C     
C     ALGORITHM  Calculates occurrence of Y-values within classes, or
C     assigns Y-values to X-values (class mid-values),
C     depending on which flag is chosen by the user.
C     If Y-values are assigned to classes, class interval
C     boundaries are handled as follows.  Y-values which
C     fall on the left boundary of a class interval are
C     counted in that interval.  This accounts for all
C     Y-values at an interval boundary except for the right
C     boundary of the rightmost class interval.  Such
C     occurrences are added to the rightmost interval,
C     thus only this interval contains Y-values which
C     fall on either boundary.
C     
C     Draws a histogram incorporating various user-defined
C     options.
C     
C     PORTABILITY  FORTRAN 77
C     
C     
C     A description of the option-setting entries of the package follows.
C     
C-----------------------------------------------------------------------
C     SUBROUTINE HSTOPL
C-----------------------------------------------------------------------
C     
C     PURPOSE  To set various options for the package.
C     
C     USAGE  CALL HSTOPL(STRING)
C     
C     ARGUMENTS  STRING      A character string (which may have up
C     to seven characters) where the first
C     2 or 3 characters are abbreviations for
C     the option followed by '=', followed by
C     'ON' or 'OFF'.  'OFF' may be abbreviated
C     by 'OF'.   Imbedded spaces are not allowed.
C     
C     Example:  CALL HSTOPL('PER=OFF')
C     
C     The following options are turned 'ON' or 'OFF' by this
C     routine (all defaults are listed at the end of the
C     package documentation):
C     
C     HOR   Horizontal.
C     The histogram bars are drawn horizontally if 'HOR=ON'.
C     If 'HOR=OFF', they are drawn vertically.
C     
C     PER   Percent axis.
C     If 'PER=ON', a labeled percent axis is drawn on the
C     right side of the histogram (or on top if
C     horizontal.)  If 'PER=OFF', the percent axis is
C     not drawn.
C     
C     MID   Midvalues.
C     If 'MID=ON', the class labels are put at the
C     midpoint of each interval. If 'MID=OFF', the class
C     interval end points are labeled.  MID defaults
C     to ON if IFLAG = 2 or 3.
C     
C     SHA   Shading.
C     If 'SHA=ON', the histogram rectangles are shaded.
C     If 'SHA=OFF', the bars are not shaded.  Laser printers
C     may vary as to how they interpret shading;
C     some will shade the bars black, others
C     will only draw the outline, etc., depending
C     on whether or not they support FILL AREA in
C     hardware.  On color terminals, the shading
C     will correspond to whatever the RGB color
C     table assigns to color index 1, even when
C     the color option, 'COL=OFF', (and all
C     other portions of the histogram are white).
C     Terminals which do not support FILL AREA or
C     color will show the outline of the 'shaded'
C     histogram bars in white (they will appear to
C     be unshaded).
C     
C     DRL   Draw lines.
C     If 'DRL=ON', lines are drawn through the histogram
C     rectangles where Y-axis tick marks would occur.
C     When IFLAG = 3 (when comparing two datasets
C     in one histogram), lines are drawn through
C     the bars associated with the first dataset
C     only.  If 'DRL=OFF', lines are not drawn.
C     
C     MED   Median.
C     If 'MED=ON', a line is drawn through the median
C     of all points.  If 'MED=OFF', this line is not drawn.
C     MED does not apply when assigning Y-values to
C     X-values; it is valid only for IFLAG = 1 or 2.
C     
C     PRM   Perimeter.
C     If 'PRM=ON', a perimeter is drawn around the
C     histogram.  If 'PRM=OFF', no perimeter is drawn.
C     
C     FRA   Frame advance.
C     If 'FRA=ON', the frame is advanced automatically
C     after the histogram is drawn.
C     If 'FRA=OFF', the frame is not advanced, and the
C     user must call FRAME.
C     
C     LIS   List options.
C     If 'LIS=ON', all the options along with their values
C     are printed on the standard output.
C     If 'LIS=OFF', nothing is printed on the standard output.
C     
C     DEF   Global defaults.
C     If 'DEF=ON', all the options are set to their default
C     values;  see list of default values below.
C     'DEF=OFF' has no effect.
C     
C-----------------------------------------------------------------------
C     SUBROUTINE HSTOPR
C-----------------------------------------------------------------------
C     
C     
C     PURPOSE  To specify various REAL arrays to be used by the
C     HISTGR package.
C     
C     USAGE  CALL HSTOPR(STRING,ARRAY,LARR)
C     
C     STRING  A character string specifying which
C     option is to be set.  Valid options are:
C     
C     'WIN=ON', or 'WIN=OFF'
C     'SPA=ON', or 'SPA=OFF'
C     
C     WIN defines the portion of the frame which will
C     receive the histogram.  SPA determines the spacing
C     between histogram bars.
C     
C     ARRAY  A real array of length LARR.
C     
C     LARR  Dimension of ARRAY.
C     
C     The following arrays may be defined by this routine:
C     
C     Windowing:
C     
C     STRING is 'WIN=ON'
C     LARR = 4
C     ARRAY(1) = XMIN
C     ARRAY(2) = XMAX
C     ARRAY(3) = YMIN
C     ARRAY(4) = YMAX
C     
C     Assumptions:  These coordinates define a rectangular
C     region of the total frame where the current histogram
C     is to be plotted.  The range of allowed values is:
C     
C     0. <= XMIM < XMAX <= 1.
C     0. <= YMIM < YMAX <= 1.
C     
C     For example, XMIN=0., XMAX=0.5, YMIN=0.5, and YMAX=1.0
C     would specify the upper left quadrant of the frame.
C     
C     If 'WIN=OFF', the default window for the histogram
C     will be the entire frame.
C     
C     EXAMPLE:  REAL WINDOW(4)
C     DATA WINDOW /.3,.7,.3,.7/
C     CALL HSTOPR('WIN=ON',WINDOW,4)
C     
C     Spacing:
C     
C     STRING is 'SPA=ON'
C     LARR = 2
C     ARRAY(1) = SPAC
C     ARRAY(2) = OVERLP
C     
C     SPAC  Real value used to set spacing of histogram bars; valid
C     values are 0.0 (no spacing) to 4.0 (maximum spacing).
C     Default spacing is SPAC = 2.0.  If 'SPA=OFF', the result
C     is the same as if SPAC = 0.0, and the value supplied in
C     ARRAY(1) is ignored by HISTGR.  Lines will be drawn
C     around the histogram bars when 'SPA=OFF' by default.
C     These may not be visible if 'SHA=ON' and the first
C     color index is set to white in the color table; it is
C     a good idea to set color index 1 to gray if 'SPA=OFF',
C     and 'SHA=ON' (for terminals without color capabilty, this
C     does not apply; see notes on 'SHA' above). If IFLAG = 3,
C     there is a minimum amount of spacing supplied, even if
C     SPAC = 0.0, to allow room for the histogram bars of the
C     second dataset.
C     
C     OVERLP  Real value used to set overlap of adjacent histogram
C     bars when comparing two datasets in one histogram;
C     valid values are -4.0 (maximum overlap) to 4.0 (little
C     or no overlap).  OVERLP applies only when IFLAG = 3.
C     Default overlap is OVERLP = 0.0.  If 'SPA=OFF',
C     OVERLP is ignored by HISTGR.  If no overlap is desired,
C     set OVERLP to 4.0, and SPAC to 3.0 or greater.
C     
C     EXAMPLE:  REAL ARRAY(2)
C     IFLAG = 3
C     ARRAY(1) = 2.0
C     ARRAY(2) = -1.5
C     CALL HSTOPR('SPA=ON',ARRAY,2)
C     
C     The above example would cause two sets of histograms
C     to have overlapping bars for comparison purposes.
C     
C-----------------------------------------------------------------------
C     SUBROUTINE HSTOPC
C-----------------------------------------------------------------------
C     
C     PURPOSE  To specify various CHARACTER variables to be used by the
C     HISTGR package.
C     
C     USAGE  CALL HSTOPC(STRING,STRNG2,NUMBER,ILCH)
C     
C     STRING  A character string specifying which
C     option is to be set.  Valid options are:
C     
C     'FOR=ON', or 'FOR=OFF'
C     'TIT=ON', or 'TIT=OFF'
C     'LAB=ON', or 'LAB=OFF'
C     'FQN=ON', or 'FQN=OFF'
C     'CHR=ON', or 'CHR=OFF'
C     
C     By choosing the ON form for an option, the user
C     can override the default setting of that option.
C     A subsequent call to HSTOPC with the OFF form
C     for an option returns the option to the default
C     setting.  All defaults are listed below.
C     
C     STRNG2  A character string up to 45 characters long.
C     
C     This argument depends upon the ON form being
C     selected for an option.  For example, if 'TIT=ON',
C     a main title is input through argument STRNG2.
C     
C     NUMBER  An integer variable which only applies to the
C     following three options:
C     
C     'FOR=ON'  The maximum number of class intervals
C     (histogram bars) which will be labeled.
C     
C     'FOR=OFF'  Defaults to 9 labels for vertical bars
C     and 15 labels for horizontal bars.
C     
C     'CHR=ON'  Must be set to NCLASS, an argument of
C     the subsequent call to be made to HISTGR.
C     
C     NUMBER is not used under any other option setting.
C     
C     Calls to HSTOPC with both 'FOR=ON' and 'CHR=ON'
C     may be performed in any order; the parameters set
C     by NUMBER are mutually exclusive in either case.
C     
C     ILCH  An integer variable specifying the number of
C     characters in each label of a class interval
C     (histogram bar).  This argument is only used with
C     the 'CHR=ON' option.
C     
C     ILCH cannot be greater than 15.
C     
C     The following options are defined by this subroutine:
C     
C     FOR     Format for class labels.  The 'FOR=OFF' default
C     format is '(G10.3)'.  Although class values
C     are real numbers, integer formats are allowed,
C     in which case HISTGR will convert from real to
C     integer before plotting labels.
C     
C     TIT     A main title.  The 'TIT=OFF' default is no title.
C     
C     LAB     A label for the class interval(histogram bar) axis.  The
C     'LAB=OFF' default value is 'CLASS INTERVALS' when
C     the HSTOPL option 'MID=OFF' is selected, and 'CLASS
C     MIDVALUES' otherwise.
C     
C     In order to delete this axis label select 'LAB=ON'
C     for STRING and 'NOLABEL' for STRNG2.
C     
C     FQN     The frequencyaxis label.  The 'FQN=OFF' default value
C     is 'FREQUENCY'.
C     
C     In order to delete this axis label select 'FQN=ON'
C     for STRING and 'NOLABEL' for STRNG2.
C     
C     CHR     Character labels.
C     Use a character string containing ILCH*NUMBER
C     characters to specify alphanumeric labels for the
C     class intervals (histogram bars).  This is a packed
C     sting of ILCH characters per class interval label.
C     
C     The character string must contain NUMBER(=NCLASS)
C     class interval labels, even though all may not be
C     used.  See the definition of argument NUMBER.
C     
C     EXAMPLE:     PARAMETER (NCLASS=12, ILCH=3)
C     CHARACTER*27 LABEL
C     CALL HSTOPC ('TIT=ON','MONTHLY PRECIPITATION in 1987',12,3)
C     LABEL = 'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'
C     CALL HSTOPC ('CHR=ON',LABEL,12,3)
C     CALL HSTOPC ('FOR=ON','(F3.0)',12,3)
C     
C     In the above example, there will be 12 alphanumeric
C     class interval labels, each containing 3 characters
C     to specify the months of the year.  The main title
C     indicates that the histogram depicts monthly precipitation
C     in 1987.
C     
C     The call to HSTOPC with 'FOR=ON' and NUMBER = 12
C     overrides the default number (9) of labels which would
C     be plotted.  Note that the '(F3.0)' format is ignored
C     because 'CHR=ON'.
C     
C-----------------------------------------------------------------------
C     SUBROUTINE HSTOPI
C-----------------------------------------------------------------------
C     
C     PURPOSE  To specify various INTEGER variables to be used by the
C     HISTGR package.
C     
C     USAGE  CALL HSTOPI( STRING, PARAM1, PARAM2, ICOL, LCOL )
C     
C     ARGUMENTS
C     
C     STRING  A character string specifying which
C     option is to be set.  Valid options are:
C     
C     'COL=ON', or 'COL=OFF'
C     'CLA=ON', or 'CLA=OFF'
C     
C     By choosing the ON form for an option, the user
C     can override the default setting of that option.
C     A subsequent call to HSTOPI with the OFF form
C     for an option returns the option to the default
C     setting.  All defaults are listed below.
C     
C     PARAM1  Integer variable used to set character height of
C     class labels when 'CLA=ON'; 1 = small, 2 = medium
C     3 = large; default is 2 when 'CLA=OFF'.
C     
C     Not used when 'COL=ON', or 'COL=OFF'.
C     
C     PARAM2  Integer variable used to set orientation of class
C     labels, from 0 (horizontal) to 90 (vertical) degrees
C     when 'CLA=ON'; default is 0 degrees when 'CLA=OFF'.
C     
C     Not used when 'COL=ON', or 'COL=OFF'.
C     
C     ICOL    Integer array containing values defining color
C     indices 1-8, for use with 'COL=ON'.
C     
C     The eight components of the plot for which color
C     indicies can be set are:
C     
C     ICOL(1) = color index used for shading rectangles
C     ICOL(2) = color index used for shading second set
C     of rectangles (comparing two datasets
C     when IFLAG=3).
C     ICOL(3) = color index used for rectangle outlines
C     ICOL(4) = color index used for drawing axes
C     ICOL(5) = color index used for drawing median line
C     ICOL(6) = color index used for text ouput (labels)
C     ICOL(7) = color index used for title
C     ICOL(8) = color index used for drawing perimeter
C     
C     The default color index is 1 for all (when 'COL=OFF').
C     
C     If 'COL=ON', the color indices and their associated
C     colors are as follows (these may be changed by setting
C     an RGB color table prior to your call to HSTOPI):
C     
C     0 = BLACK
C     1 = WHITE
C     2 = RED
C     3 = GREEN
C     4 = BLUE
C     5 = CYAN
C     6 = MAGENTA
C     7 = YELLOW
C     8 = ORANGE
C     
C     ICOL is not used when 'CLA=ON', or 'CLA=OFF'.
C     
C     LCOL    Integer variable specifying length of array ICOL.
C     LCOL must be set to 8.
C     
C     LCOL is not used when 'CLA=ON', or 'CLA=OFF'.
C     
C-----------------------------------------------------------------------
C     SUMMARY OF OPTION DEFAULTS:
C-----------------------------------------------------------------------
C     
C     HSTOPL:
C     'HOR=OFF', histogram bars will be vertical.
C     'PER=ON',  a labeled percent axis is drawn on the right side
C     of the histogram (or on top if horizontal.)
C     'MID=ON',  labels are placed at the midpoint of each histogram bar.
C     'SHA=ON',  the histogram rectangles are shaded.
C     'DRL=OFF', lines corresponding to plotted scale values are not
C     drawn through the histogram bars.
C     'MED=OFF', a line denoting the median of all points is not
C     drawn on the histogram.
C     'PRM=OFF', a perimeter around the histogram is not drawn.
C     'FRA=ON',  a frame advance will occur after the call to HISTGR.
C     'LIS=OFF', nothing is printed on the standard output unit.
C     
C     HSTOPR:
C     'WIN=OFF', the histogram will be drawn within the maximum viewport
C     of 0.0 to 1.0 in both the horizontal and vertical.
C     'SPA=ON',  default values: spacing = 2.0, overlap = -1.0
C     This will yield spacing between bars on a single
C     histogram and overlap of bars in the comparison
C     of two histograms.
C     
C     HSTOPC:
C     'FOR=OFF', class label format = '(G10.3)'.
C     'TIT=OFF', no title.
C     'LAB=OFF', label = 'CLASS INTERVALS' or 'CLASS MIDVALUES'
C     if 'MID=ON'.
C     'FQN=OFF', frequency label = 'FREQUENCY'.
C     'CHR=OFF', numeric (real or integer) class labels.
C     
C     HSTOPI:
C     'COL=OFF', color index 1 is used for all output.
C     'CLA=ON',  default values: 2 = medium, 0 = horizontal labels.
C     
C-----------------------------------------------------------------------
C     
      EXTERNAL HSTBKD

C********************************************************************
C     Replaced this code with COMMONs in hstbkd.f of NGARG-4.3.0
C     These are causing Linux version to not load (collect2 error)
C     LJM (0911/2002).
C********************************************************************
C      COMMON /HSTGC1/ HORZNT, PERCNT, MIDVAL, SHADE, MEDIAN, PERIM,
C     -     HFRAME, LISTOP, WINDOW, COLORS, HSTFOR, TITLE, LABEL,
C     -     FREQNC, HWIND(4), COLSHA, COLREC, COLAXI, COLMED, COLTEX,
C     -     COLTIT, COLPER, DRAWL, SPACE, LABMAX, CHARL, HEIGHT,
C     -     ORIENT, COLSH2, SETSPA, SETSP2
C      LOGICAL HORZNT, PERCNT, MIDVAL, SHADE, MEDIAN, PERIM, HFRAME,
C     -     LISTOP, WINDOW, COLORS, HSTFOR, TITLE, LABEL, FREQNC,
C     -     DRAWL, SPACE, CHARL
C      COMMON /HSTGC2/ STRFOR, STRTIT, STRLAB, STRFRE, LABTEX
C      CHARACTER*55  STRFOR, STRTIT, STRLAB, STRFRE
C      CHARACTER*15 LABTEX(30), TEX, TESTCH
C********************************************************************
C     Replaced COMMONs with those in hstbkd.f of NGARG-4.3.0
C     LJM (0911/2002).
C********************************************************************
      COMMON /HSTGC1/ HORZNT, PERCNT, MIDVAL, SHADE, MEDIAN, PERIM,
     -     HFRAME, LISTOP, WINDOW, COLORS, HSTFOR, TITLE, LABEL,
     -     FREQNC, HWIND(4), COLSHA, COLREC, COLAXI, COLMED, COLTEX,
     -     COLTIT, COLPER, DRAWL, SPACE, LABMAX, CHARL, HEIGHT,
     -     ORIENT, COLSH2, SETSPA, SETSP2, MVALU, SETMVA, SETEPS, 
     -     NMVAL, PMVAL, PERTIT
      LOGICAL HORZNT, PERCNT, MIDVAL, SHADE, MEDIAN, PERIM, HFRAME,
     -        LISTOP, WINDOW, COLORS, HSTFOR, TITLE, LABEL, FREQNC,
     -        DRAWL, SPACE, CHARL, MVALU, NMVAL, PMVAL, PERTIT
      COMMON /HSTGC2/ STRFOR, STRTIT, STRLAB, STRFRE, STRPER, LABTEX
      CHARACTER*96  STRTIT
      CHARACTER*55  STRFOR, STRLAB, STRFRE, STRPER
      CHARACTER*15 LABTEX(30), TEX, TESTCH
C********************************************************************
     
      INTEGER  NDIM, NPTS, NCLASS, NWRK
      INTEGER  COLSHA, COLREC, COLAXI, COLMED, COLTEX
      INTEGER  OTEXCI, OCOLI
      INTEGER  COLTIT, COLPER, HEIGHT, ORIENT, COLSH2
      INTEGER  FIRST, LAST, LASF(13)
      INTEGER  OLDALH, OLDALV
      REAL     DAT1(NDIM,2), HI, LOW, MED, CLASS(NCLASS+1)
      REAL     WRK(NWRK)
      REAL     PX(7), PY(7), NEXTX, YTICKS(5), TICINT, PER(20)
      REAL     OWIND(4), OVIEW(4), VUPORT(4)
      REAL     NEWWIN(4), LABMAX, LASTLB
      LOGICAL  DONE, COMPAR, SPEC, ASSIGN
C     
C     THE FOLLOWING IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C     
      CALL Q8QST4('GRAPHX','HISTGR','HISTGR','VERSION 01')
C     
C     INTIALIZE ERROR COUNT
C     
      NERR = 0
C     
C     GET STANDARD ERROR MESSAGE UNIT
C     
      IERUNT = I1MACH(4)
C     
C     PERFORM ERROR CHECKING
C     
      IF (NPTS .LT. 1) THEN
         NERR = NERR + 1
         CALL SETER(' HISTGR--NPTS LESS THAN 1',NERR,2)
      ENDIF
      IF (NCLASS .LT. 1) THEN
         NERR = NERR + 1
         CALL SETER(' HISTGR--NCLASS LESS THAN 1',NERR,2)
      ENDIF
      IF (NPTS .GT. NDIM) THEN
         NERR = NERR + 1
         CALL SETER(' HISTGR--NPTS .GT. NDIM',NERR,2)
      ENDIF
      IF (NWRK .LT. (NDIM+3*NCLASS+3)) THEN
         NERR = NERR + 1
         CALL SETER(' HISTGR--NWRK .LT. NDIM+3*NCLASS+3',NERR,2)
      ENDIF
      IF (IFLAG .EQ. 2 .AND. NPTS .NE. NCLASS) THEN
         NERR = NERR + 1
         CALL SETER(' HISTGR--NCLASS .NE. NPTS (IFLAG=2)',NERR,2)
      ENDIF
      IF (IFLAG .EQ. 3 .AND. NPTS .NE. NCLASS) THEN
         NERR = NERR + 1
         CALL SETER(' HISTGR--NCLASS .NE. NPTS (IFLAG=3)',NERR,2)
      ENDIF
C     
C     IF LISTOP IS ON, LIST ALL OPTION VALUES
C     
      IF (LISTOP) THEN
         CALL HSTLST
      ENDIF
C     
C     SET POLYLINE COLOR ASF TO INDIVIDUAL
C     
      CALL GQASF(IERR,LASF)
      OPLASF = LASF(3)
      LASF(3) = 1
      OTXASF = LASF(10)
      LASF(10) = 1
C     
C     Set ASF parameters to 1 (individual)
C     
      DO 100 I = 1,13
         LASF(I) = 1
 100  CONTINUE
      CALL GSASF(LASF)
C     
C     INQUIRE CURRENT POLYLINE COLOR
C     
      CALL GQPLCI(IERR,OCOLI)
C     
C     INQUIRE CURRENT NORMALIZATION NUMBER
C     
      CALL GQCNTN(IERR,ICNT)
C     
C     INQUIRE CURRENT WINDOW AND VIEWPORT LIMITS
C     
      CALL GQNT(ICNT,IERR,OWIND,OVIEW)
C     
C     INQUIRE AND SAVE TEXT ATTRIBUTES
C     
      CALL GQCHH(IERR,OLDCHH)
      CALL GQCHUP(IERR,CHUPX,CHUPY)
      CALL GQTXAL(IERR,OLDALH,OLDALV)
      CALL GQTXCI (IERR,OTEXCI)
C     
C     SET TEXT COLOR
C     
      IF (COLORS) CALL GSTXCI(COLTEX)
C     
C     INITIALIZE VARIABLES
C     
      IF (.NOT. HSTFOR) THEN
         IF (.NOT. HORZNT) THEN
            LABMAX = 9.
         ELSE
            LABMAX = 15.
         ENDIF
      ENDIF
      DONE = .FALSE.
C     
      LENWRK = NDIM+3*NCLASS+3
      INDY1 = NDIM + NCLASS + 1
      INDY2 = INDY1 + NCLASS + 1
      DO 110 I = 1,LENWRK
         WRK(I) = 0.
 110  CONTINUE
C     
C     Check to see if CLASS values have been assigned
C     
      SPEC = .FALSE.
      ASSIGN = .FALSE.
      COMPAR = .FALSE.
      IF (IFLAG .EQ. 1) THEN
         SPEC = .TRUE.
      ELSEIF (IFLAG .EQ. 2) THEN
         ASSIGN = .TRUE.
      ELSEIF (IFLAG .EQ. 3) THEN
         COMPAR = .TRUE.
      ELSEIF (IFLAG .NE. 0) THEN
         NERR = NERR + 1
         CALL SETER(' HISTGR--IFLAG NOT SPECIFIED OR INVALID',NERR,1)
         CALL ERROF
         WRITE(IERUNT,1001)
 1001    FORMAT(' HISTGR--IFLAG NOT SPECIFIED OR INVALID')
      ENDIF
 150  CONTINUE
      IF ((ASSIGN .OR. COMPAR) .AND. (NCLASS .NE. NPTS)) THEN
         NERR = NERR + 1
         CALL SETER(' HISTGR--NCLASS MUST EQUAL NPTS ',NERR,1)
         CALL ERROF
         WRITE(IERUNT,1002)
 1002    FORMAT(' HISTGR--NCLASS MUST EQUAL NPTS ')
      ENDIF
C     
C     DO SHELL SORT, FIND MEDIAN OF ALL DATA POINTS
C     
      DO 155 I = 1,NPTS
         WRK(I)=DAT1(I,1)
         IF (ASSIGN .OR. COMPAR) WRK(I)=ABS(DAT1(I,1))
 155  CONTINUE
      CALL HSTMED(WRK,NPTS,WRK,MED)
C     
C     Determine Upper and Lower Bounds of DAT1, and
C     sort values in CLASS, if supplied
C     
      IF (SPEC) THEN
         CALL HSTMED(CLASS,NCLASS+1,WRK(NPTS+1),AMUD1)
         HI = WRK(NPTS+NCLASS+1)
         LOW = WRK(NPTS+1)
         DO 170 I = 1,NCLASS + 1
            CLASS(I) = WRK(NPTS+I)
 170     CONTINUE
      ELSEIF (ASSIGN .OR. COMPAR) THEN
         HI = CLASS(NPTS)
         LOW = CLASS(1)
      ELSE
         HI = WRK(NPTS)
         LOW =  WRK(1)
      ENDIF
C     
C     Compute X intervals
C     
      IF (ASSIGN .OR. COMPAR) THEN
         XINT = (HI - LOW) / FLOAT(NCLASS-1)
         HAFINT = XINT / 2.
      ELSE
         XINT = (HI - LOW) / FLOAT(NCLASS)
         HAFINT = XINT / 2.
      ENDIF
C     
C     Set Extra Spacing if specified in HSTOPI
C     
      SPAC1 = ABS(SETSPA*(XINT/10.))
      IF (COMPAR) SPAC1 = ABS(SETSPA*(XINT/16.))+XINT/8.
      SPAC2 = SETSP2*(XINT/16.)
C     
C     DETERMINE XMIN,XMAX
C     
      IF ( ASSIGN .OR. COMPAR ) THEN
         XMIN = LOW - XINT
         IF ( COMPAR ) XMIN = LOW - XINT/2 - SPAC1
      ELSE
         XMIN = LOW - HAFINT
      ENDIF
C     
C     FILL IN X ARRAY, WRK(NPTS+1), FOR SHADING (LATER)
C     
      XPOS = XMIN + XINT
      IF ( COMPAR ) XPOS = XMIN + XINT/2 + SPAC1
      DO 200 I = 1,NCLASS + 1
         WRK(NPTS+I) = XPOS
 200     XPOS = XPOS + XINT
         XMAX = WRK(NPTS+NCLASS + 1)
         IF ( COMPAR ) XMAX = AMIN1(WRK(NPTS+NCLASS+1) + SPAC2,
     1        WRK(NPTS+NCLASS+1))
         IF ( XMIN .EQ. XMAX ) THEN
            NERR = NERR + 1
            CALL SETER(' HISTGR--MIN. X-VALUE .EQ. MAX. X-VALUE',NERR,1)
            CALL ERROF
            WRITE(IERUNT,210)
 210        FORMAT(' HISTGR--MIN. X-VALUE .EQ. MAX. X-VALUE')
         ENDIF
C     
C     COMPUTE NUMBER OF RECTANGLES TO BE LABELLED
C     
         IF (MIDVAL .OR. (COMPAR .OR. ASSIGN)) THEN
            NUMLAB = NCLASS
         ELSE
            NUMLAB = NCLASS + 1
         ENDIF
C     
C     Y ARRAY (DETERMINE FREQUENCY FOR EACH CLASS)
C     
         DO 310 I = 1,NCLASS
            IF (.NOT. (ASSIGN .OR. COMPAR)) THEN
               IF ( SPEC ) THEN
                  TM1 = CLASS(I)
                  TM2 = CLASS(I+1)
               ELSE
                  TM1 = WRK(NPTS+I) - HAFINT
                  TM2 = WRK(NPTS+I) + HAFINT
               ENDIF
               DO 300 J = 1,NPTS
                  TM3 = DAT1(J,1)
                  IF ((TM3 .GE. TM1) .AND. (TM3 .LT. TM2)) THEN
                     WRK(INDY1+I) = WRK(INDY1+I) + 1.
                  ELSE
C     
C     The last histogram class interval receives DAT1 values which fall
C     on either the right or left bar boundary.  All others receive only
C     values that fall on the left boundary.
C     
                     IF (I .EQ. NCLASS .AND. TM3 .EQ. TM2) THEN
                        WRK(INDY1+I) = WRK(INDY1+I) + 1.
                     ENDIF
                  ENDIF
 300           CONTINUE
            ELSE
               TM1 = WRK(NPTS+I) - HAFINT
               TM2 = WRK(NPTS+I) + HAFINT
               TM3 = CLASS(I)
               IF ((TM3 .GE. TM1) .AND. (TM3 .LT. TM2)) THEN
                  WRK(INDY1+I) = ABS(DAT1(I,1))
                  IF (COMPAR) WRK(INDY2+I) = ABS(DAT1(I,2))
               ENDIF
            ENDIF
 310     CONTINUE
C     
C     FIND MAX Y AND ADJUST Y WINDOW LIMIT
C     
         YBOUND = WRK(INDY1+1)
         DO 400  I = 1,NCLASS
            YBOUND = AMAX1(YBOUND,WRK(INDY1+I),WRK(INDY2+I))
 400     CONTINUE
         IF (YBOUND .EQ. 0.) THEN
            NERR = NERR + 1
C     CALL SETER(' HISTGR--MAXIMUM Y-VALUE .EQ. 0.0',NERR,1)
C     CALL ERROF
C     WRITE(IERUNT,410)
            XMID=0.5*(XMAX-XMIN)
            YMID=0.5*(YMAX-YMIN)
            CALL PLCHMQ(XMID,YMID,'NO DATA THIS LEVEL',
     X                  24.,0.,0.)
            RETURN
 410        FORMAT(' HISTGR--MAXIMUM Y-VALUE .EQ. 0.0')
         ENDIF
         IF (YBOUND .LT. 1.) THEN
            YMAX = 0.02*INT(YBOUND/.02 + 1.)
         ELSEIF (YBOUND .LT. 10.) THEN
            YMAX = 0.2*INT(YBOUND/.2 + 1.)
         ELSEIF (YBOUND .LT. 100.) THEN
            YMAX = 2.*INT(YBOUND/2. + 1.)
         ELSEIF (YBOUND .LT. 1000.) THEN
            YMAX = 20.*INT(YBOUND/20. + 1.)
         ELSEIF (YBOUND .LT. 10000.) THEN
            YMAX = 200.*INT(YBOUND/200. + 1.)
         ELSEIF (YBOUND .LT. 100000.) THEN
            YMAX = 2000.*INT(YBOUND/2000. + 1.)
         ELSEIF (YBOUND .LT. 1000000.) THEN
            YMAX = 20000.*INT(YBOUND/20000. + 1.)
         ELSEIF (YBOUND .LT. 10000000.) THEN
            YMAX = 200000.*INT(YBOUND/20000. + 1.)
         ELSE
            YMAX = 100000000.
         ENDIF
C     
C     DETERMINE Y-AXIS TICK SPACING AND EQUIVALENT PERCENTAGES
C     
         TICINT = YMAX / 5.
C     
C     MAKE YMAX A MULTIPLE OF 2,5, AND 10
C     
         DO 420 I=1,9
            IF (AMOD(YMAX,2.).EQ.0. .AND. AMOD(YMAX,5.).EQ.0. .AND.
     X           AMOD(YMAX,10.).EQ.0.) GOTO 425
            YMAX=YMAX+1
 420     CONTINUE
 425     CONTINUE
         YMAX=TPER*NPPTS/100.0
         TICINT = YMAX / 5.
         YTICKS(1) = TICINT
         YTICKS(2) = TICINT * 2.
         YTICKS(3) = TICINT * 3.
         YTICKS(4) = TICINT * 4.
         YTICKS(5) = YMAX 
         IF (PERCNT) THEN
            PERC = YMAX / FLOAT(NPTS) * 100.
            IF (ASSIGN .OR. COMPAR) PERC = (YMAX/YBOUND)*100.
            PER(1) = PERC / 4.
            PER(2) = PER(1) * 2.
            PER(3) = PER(1) * 3.
            PER(4) = PERC
         ENDIF
C     
C     SET UP WINDOW COORDINATES
C     
         IF (.NOT. HORZNT) THEN
            CALL GSWN(1,XMIN,XMAX,0.,YMAX)
            VUPORT(1) = .15
            VUPORT(2) = .95
            VUPORT(3) = .15
            VUPORT(4) = .85
         ELSE
            CALL GSWN(1,0.,YMAX,XMIN,XMAX)
            VUPORT(1) = .20
            VUPORT(2) = .90
            VUPORT(3) = .1
            VUPORT(4) = .85
         ENDIF
         IF (WINDOW) THEN
            DO 500 I = 1,4
               IF (HWIND(I) .LT. 0. .OR. HWIND(I) .GT. 1.) THEN
                  NERR = NERR + 1
           CALL SETER(' HISTGR--WINDOW OPTION ERROR, RANGE IS 0. - 1.',
     -                 NERR,1)
                  CALL ERROF
                  HWIND(1) = 0.
                  HWIND(2) = 1.
                  HWIND(3) = 0.
                  HWIND(4) = 1.
                  GOTO 510
               ENDIF
 500        CONTINUE
 510        CONTINUE
            WXRANG = HWIND(2) - HWIND(1)
            WYRANG = HWIND(4) - HWIND(3)
            VUPORT(1) = HWIND(1) + VUPORT(1)*WXRANG
            VUPORT(2) = HWIND(1) + VUPORT(2)*WXRANG
            VUPORT(3) = HWIND(3) + VUPORT(3)*WYRANG
            VUPORT(4) = HWIND(3) + VUPORT(4)*WYRANG
         ELSE
            HWIND(1) = 0.
            HWIND(2) = 1.
            HWIND(3) = 0.
            HWIND(4) = 1.
         ENDIF
         CALL GSVP(1,VUPORT(1),VUPORT(2),VUPORT(3),VUPORT(4))
         CALL GSELNT(1)
C     
C     EXPAND WINDOW AND VIEWPORT FOR LABELING
C     DETERMINE CHARACTER HEIGHT AND TICK LENGTHS
C     
         CALL HSTEXP(HWIND,NEWWIN)
         FRACT = 18./1024.
         YRANGE = NEWWIN(4) - NEWWIN(3)
         XRANGE = NEWWIN(2) - NEWWIN(1)
         IF (.NOT. HORZNT) THEN
            XTIC = FRACT * YRANGE
            YTIC = FRACT * XRANGE
         ELSE
            XTIC = FRACT * XRANGE
            YTIC = FRACT * YRANGE
         ENDIF
         XDEC = .8 * XTIC
         YDEC = .8 * YTIC
         CHARH = .014 * YRANGE
         CALL GSCHH(CHARH)
c     
C     DRAW FREQUENCY-AXIS
c     
         IF (COLORS) CALL GSPLCI(COLAXI)
         PX(1) = XMIN
         PX(2) = XMIN
         PY(1) = 0.
         PY(2) = YMAX
C         IF (.NOT. HORZNT) THEN
C            CALL GPL(2,PX,PY)
C     RIGHT,HALF
C            CALL GSTXAL(3,3)
C         ELSE
C            CALL GPL(2,PY,PX)
C     CENTER,TOP
C            CALL GSTXAL(2,1)
C         ENDIF
         PX(1) = XMIN
         PX(2) = XMIN + 0.6*HAFINT
         IF ( COMPAR ) PX(2) = XMIN + 0.3*HAFINT
         IF(HORZNT) THEN
            XPOS = XMIN - 1.5*YDEC
         ELSE
            XPOS = XMIN - 1.0*YDEC
         ENDIF
C     
C     Test to see if Frequency labels are all integers
C     
         JJ = 0
        DO 520 I = 1,5
            PY(1) = YTICKS(I)
            IF (PY(1) .EQ. INT(PY(1))) JJ = JJ + 1
 520     CONTINUE
         IF (IFLAG.NE.3) THEN
C
C     ONLY PLOT A FREQUENCY LABEL FOR NON-OVERLAY HISTO PLOTS
C
            DO 530 I = 1,5
               PY(1) = YTICKS(I)
               PY(2) = PY(1)
               YPOS = PY(1)
               FNUM = PY(1)
               IFNUM = INT(PY(1))
     
C     Choose integer format for axis labels
C     
C               IF (JJ .EQ. 5) THEN
C                  IF (YMAX .GT. 199998) THEN
C                     CHARH = .010 * YRANGE
C                     CALL GSCHH(CHARH)
C                  ENDIF
C                  WRITE(TEX,'(I11)')IFNUM
C               ELSE
C     
C     Choose real format for axis labels
C     
C                  IF (YMAX .GT. 1998) THEN
C                     CHARH = .010 * YRANGE
C                     CALL GSCHH(CHARH)
C                  ENDIF
C                  WRITE(TEX,'(F11.1)')FNUM
C               ENDIF
C               CALL HSTSTR(TEX,FIRST,LAST)
C               IF (.NOT. HORZNT) THEN
C                  CALL GPL(2,PX,PY)
C                  CALL NWTSTR(XPOS,YPOS,TEX(FIRST:LAST))
C               ELSE
C                  CALL GPL(2,PY,PX)
C                  CALL NWTSTR(YPOS,XPOS,TEX(FIRST:LAST))
C               ENDIF
 530        CONTINUE
         END IF
         CHARH = .014 * YRANGE
         CALL GSCHH(CHARH)
C     
C     DRAW and SHADE RECTANGLES
C     
         IF ( COLORS ) CALL GSPLCI(COLREC)
         CALL GSFACI(COLSHA)
         IF (SPACE .OR. COMPAR) THEN
C     
C     DRAW SPACED RECTANGLES
C     
            CURX = WRK(NPTS+1) - HAFINT
            NEXTX = CURX + XINT
            DO 540 I = 1,NCLASS
               PX(1) = CURX + SPAC1
               PX(2) = CURX + SPAC1
               PX(3) = NEXTX - SPAC1
               PX(4) = NEXTX - SPAC1
               PY(1) = 0.
               PY(2) = WRK(INDY1+I)
               PY(3) = WRK(INDY1+I)
               PY(4) = 0.
               IF (PY(2).GT.YMAX) THEN
                  PY(2)=YMAX
                  PY(3)=YMAX
                  CALL PLCHLQ(PX(1),PY(2),'***',12.0,0.,0.)
C                  CALL GTX(PX(1),PY(2),'**')
               ENDIF
               IF (SHADE) THEN
                  CALL GSFAIS(1)
               ELSE
                  CALL GSFAIS(0)
               ENDIF
               IF (.NOT. HORZNT) THEN
                  CALL GFA(4,PX,PY)
                  CALL GPL(4,PX,PY)
               ELSE
                  CALL GFA(4,PY,PX)
                  CALL GPL(4,PY,PX)
               ENDIF
C     
C     CALL PLOTIT TO FLUSH OUT BUFFER (FINISH DRAWING SHADE LINES)
C     
               CALL PLOTIT(0,0,0)
C     
C     DRAW LINE through Rectangle Where Frequency Tick would be
C     
               IF (DRAWL) THEN
                  DO 550 J = 1,3
                     IF (WRK(INDY1+I) .GT. YTICKS(J)) THEN
                        PY(1) = YTICKS(J)
                        PY(2) = YTICKS(J)
                        PX(1) = CURX + SPAC1
                        PX(2) = NEXTX - SPAC1
                        IF (.NOT. HORZNT) THEN
                           CALL GPL(2,PX,PY)
                        ELSE
                           CALL GPL(2,PY,PX)
                        ENDIF
                     ENDIF
 550              CONTINUE
               ENDIF
               CURX = NEXTX
               NEXTX = NEXTX + XINT
 540        CONTINUE
C     
            IF (COMPAR) THEN
C     
C     DRAW SPACED, DOUBLE RECTANGLES
C     
               CURX = WRK(NPTS+1) - XINT/2
               NEXTX = CURX + XINT
               DO 590 I = 1,NCLASS
                  IF (WRK(INDY2+I) .GT. WRK(INDY1+I)) THEN
                     PX(1) = CURX + XINT/2 + SPAC2
                     PX(2) = CURX + XINT/2 + SPAC2
            PX(3) = AMIN1(NEXTX + XINT/2 + SPAC2 - 2*SPAC1,NEXTX+SPAC1)
            PX(4) = AMIN1(NEXTX + XINT/2 + SPAC2 - 2*SPAC1,NEXTX+SPAC1)
                     PX(5) = AMAX1(NEXTX - SPAC1,CURX+XINT/2+SPAC2)
                     PX(6) = AMAX1(NEXTX - SPAC1,CURX+XINT/2+SPAC2)
                     PX(7) = CURX + XINT/2 + SPAC2
                     IF (PX(3) .LE. NEXTX - SPAC1) THEN
                        PX(3) = NEXTX - SPAC1 - SPAC2/8.
                        PX(4) = NEXTX - SPAC1 - SPAC2/8.
                        PX(5) = NEXTX - SPAC1
                        PX(6) = NEXTX - SPAC1
                     ENDIF
                     IF (PX(1) .LE. CURX + SPAC1) THEN
                        PX(1) = CURX + SPAC1 - SPAC2/8.
                        PX(2) = CURX + SPAC1 - SPAC2/8.
                        PX(7) = CURX + SPAC1 - SPAC2/8.
                     ENDIF
                     PY(1) = WRK(INDY1+I)
                     PY(2) = WRK(INDY2+I)
                     PY(3) = WRK(INDY2+I)
                     PY(4) = 0.
                     PY(5) = 0.
                     PY(6) = WRK(INDY1+I)
                     PY(7) = WRK(INDY1+I)
                     CALL GSFACI (COLSH2)
                     IF (SHADE) THEN
                        CALL GSFAIS (1)
                     ELSE
                        CALL GSFAIS (0)
                     ENDIF
                     IF (.NOT. HORZNT) THEN
                        CALL GFA(6,PX,PY)
                        CALL GPL(7,PX,PY)
                     ELSE
                        CALL GFA(6,PY,PX)
                        CALL GPL(7,PY,PX)
                     ENDIF
                  ELSE
                     PX(1) = AMAX1(NEXTX - SPAC1,CURX+XINT/2+SPAC2)
                     PX(2) = AMAX1(NEXTX - SPAC1,CURX+XINT/2+SPAC2)
         PX(3) = AMIN1(NEXTX + XINT/2 + SPAC2 - 2*SPAC1,NEXTX+SPAC1)
         PX(4) = AMIN1(NEXTX + XINT/2 + SPAC2 - 2*SPAC1,NEXTX+SPAC1)
                     PY(1) = 0.
                     PY(2) = WRK(INDY2+I)
                     PY(3) = WRK(INDY2+I)
                     PY(4) = 0.
                     IF (PX(1) .LE. CURX + SPAC1) THEN
                        PX(1) = CURX + SPAC1 - SPAC2/8.
                        PX(2) = CURX + SPAC1 - SPAC2/8.
                     ENDIF
                     IF (PX(3) .LE. NEXTX - SPAC1) THEN
                        PX(3) = NEXTX - SPAC1 - SPAC2/8.
                        PX(4) = NEXTX - SPAC1 - SPAC2/8.
                     ENDIF
                     CALL GSFACI (COLSH2)
                     IF (SHADE) THEN
                        CALL GSFAIS (1)
                     ELSE
                        CALL GSFAIS (0)
                     ENDIF
                     IF (.NOT. HORZNT) THEN
                        CALL GFA(4,PX,PY)
                        CALL GPL(4,PX,PY)
                     ELSE
                        CALL GFA(4,PY,PX)
                        CALL GPL(4,PY,PX)
                     ENDIF
                  ENDIF
C     
C     CALL PLOTIT TO FLUSH OUT BUFFER (FINISH DRAWING SHADE LINES)
C     
                  CALL PLOTIT(0,0,0)
C     
                  CURX = NEXTX
                  NEXTX = NEXTX + XINT
 590           CONTINUE
            ENDIF
         ELSE
C     
C     DRAW UNSPACED RECTANGLES, ONE AT A TIME
C     
            KK = 0
            DO 610 KK = 1,2
               CURX = WRK(NPTS+1) - HAFINT
               NEXTX = CURX + XINT
               NUM = 4
               PX(1) = CURX
               PX(2) = CURX
               PX(3) = NEXTX
               PX(4) = NEXTX
               PY(1) = 0.
               PY(2) = WRK(INDY1+1)
               PY(3) = WRK(INDY1+1)
               PY(4) = 0.
               IF ( SHADE ) CALL GSFAIS (1)
               IF (.NOT. HORZNT) THEN
                  IF (PY(2).GT.YMAX) THEN
                     PY(2)=YMAX
                     PY(3)=YMAX
                     CALL PLCHLQ(PX(1),PY(2),'***',12.0,0.,0.)
                  END IF
                  IF ( SHADE .AND. KK .EQ. 1 ) CALL GFA(NUM,PX,PY)
                  IF ( KK .EQ. 2 ) CALL GPL(NUM,PX,PY)
               ELSE
                  IF ( SHADE .AND. KK .EQ. 1 ) CALL GFA(NUM,PY,PX)
                  IF ( KK .EQ. 2 ) CALL GPL(NUM,PY,PX)
               ENDIF
C     
C     Call PLOTIT To Flush out Buffer (Finish Drawing Shade Lines)
C     
               CALL PLOTIT(0,0,0)
C     
C     
               DO 600 I = 2,NCLASS
                  CURX = NEXTX
                  NEXTX = NEXTX + XINT
C     
C     NEXT RECTANGLE IS LOWER THEN LAST ONE
C     
                  IF (WRK(INDY1+I) .LT. WRK(INDY1+I-1)) THEN
                     NUM = 3
                     NUM2 = 4
                     PX(1) = CURX
                     PX(2) = NEXTX
                     PX(3) = NEXTX
                     PX(4) = CURX
                     PY(1) = WRK(INDY1+I)
                     PY(2) = WRK(INDY1+I)
                     PY(3) = 0.
                     PY(4) = 0.
                     IF (PY(1).GT.YMAX) THEN
                        PY(1)=YMAX
                        PY(2)=YMAX
                        CALL PLCHLQ(PX(1),PY(2),'***',12.0,0.,0.)
                     END IF
C     
C     NEXT RECTANGLE IS HIGHER THAN LAST ONE
C     
                  ELSE
                     NUM = 4
                     NUM2 = 5
                     PX(1) = CURX
                     PX(2) = CURX
                     PX(3) = NEXTX
                     PX(4) = NEXTX
                     PX(5) = CURX
                     PY(1) = WRK(INDY1+I-1)
                     PY(2) = WRK(INDY1+I)
                     PY(3) = WRK(INDY1+I)
                     PY(4) = 0.
                     PY(5) = 0.
                     IF (PY(2).GT.YMAX .AND. PY(1).LE.YMAX) THEN
                        PY(2)=YMAX
                        PY(3)=YMAX
                        CALL PLCHLQ(PX(1),PY(2),'***',12.0,0.,0.)
                     ELSE IF (PY(1).GT.YMAX .AND. PY(2).GT.YMAX) THEN
                        PY(1)=YMAX
                        PY(2)=YMAX
                        PY(3)=YMAX
                        CALL PLCHLQ(PX(1),PY(2),'***',12.0,0.,0.)
                     END IF
                  ENDIF
                  CALL GSFAIS(1)
                  IF ( .NOT. HORZNT ) THEN
                     IF ( SHADE .AND. KK .EQ. 1 ) CALL GFA(NUM2,PX,PY)
                     IF ( KK .EQ. 2 ) CALL GPL(NUM,PX,PY)
                  ELSE
                     IF ( SHADE .AND. KK .EQ. 1 ) CALL GFA(NUM2,PY,PX)
                     IF ( KK .EQ. 2 ) CALL GPL(NUM,PY,PX)
                  ENDIF
C     
C     Call PLOTIT To Flush out Buffer (Finish Drawing Shade Lines)
C     
                  CALL PLOTIT(0,0,0)
C     
 600           CONTINUE
 610        CONTINUE
C     
C     DRAW LINE THROUGH RECTANGLE WHERE FREQUENCY TICK WOULD BE
C     
            IF (DRAWL) THEN
               CURX = WRK(NPTS+1) - HAFINT
               NEXTX = CURX + XINT
               DO 650 I = 1,NCLASS
                  DO 620 J = 1,3
                     IF (WRK(INDY1+I) .GT. YTICKS(J)) THEN
                        PY(1) = YTICKS(J)
                        PY(2) = YTICKS(J)
                        PX(1) = CURX
                        PX(2) = NEXTX
                        IF (.NOT. HORZNT) THEN
                           CALL GPL(2,PX,PY)
                        ELSE
                           CALL GPL(2,PY,PX)
                        ENDIF
                     ENDIF
 620              CONTINUE
                  CURX = NEXTX
                  NEXTX = NEXTX + XINT
 650           CONTINUE
            ENDIF
         ENDIF
C     
C     DONE DRAWING RECTANGLES
C     
C     SET UP VALUES FOR LABELING AND DRAWING TICKS ON X-AXIS
C     
         IF (COLORS) CALL GSPLCI(COLAXI)
         IF (HEIGHT .EQ. 1) THEN
            CHGHT = .010
         ELSEIF (HEIGHT .EQ. 2) THEN
            CHGHT = .012
         ELSEIF (HEIGHT .EQ. 3) THEN
            CHGHT = .014
         ELSE
            CHGHT = .012
         ENDIF
         CHARH = CHGHT * YRANGE
         CALL GSCHH(CHARH)
         PX(1) = XMIN
         PX(2) = XMAX
         PY(1) = 0.
         PY(2) = 0.
         IF (MIDVAL .OR. (COMPAR .OR. ASSIGN)) THEN
            XPOS = WRK(NPTS+1)
         ELSE
            XPOS = WRK(NPTS+1) - HAFINT
         ENDIF
         IF (.NOT. HORZNT) THEN
            CALL GPL(2,PX,PY)
            IF (ORIENT .LT. 5) THEN
C     ************************* CENTER,TOP
               CALL GSTXAL(2,1)
            ELSE
C     ************************* RIGHT,TOP
               CALL GSTXAL(3,1)
            ENDIF
            RAD = (3.141592654/180)*(90-ORIENT)
            ORNTY = SIN(RAD)
            ORNTX = -COS(RAD)
            CALL GSCHUP(ORNTX,ORNTY)
         ELSE
            CALL GPL(2,PY,PX)
C     ************************* RIGHT,HALF
            CALL GSTXAL(3,3)
         ENDIF
         IF (ORIENT .LT. 5) THEN
            YPOS = 0. - 2.6 * XTIC
         ELSE
            YPOS = 0. - 1.3 * XTIC
         ENDIF
         PY(1) = 0.
         PY(2) = 0. - XTIC
         PX(1) = XPOS
         PX(2) = PX(1)
         DO 700 I = 1,NUMLAB
            IF (.NOT. HORZNT) THEN
               CALL GPL(2,PX,PY)
            ELSE
               CALL GPL(2,PY,PX)
            ENDIF
            PX(1) = PX(1) + XINT
            PX(2) = PX(1)
 700     CONTINUE
C     
C     IF MORE THAN LABMAX LABELS TO BE LABELED THEN
C     COMPUTE LABEL INTERVAL SO THAT THERE ARE NO MORE
C     THAN LABMAX LABELS
C     
         IF (FLOAT(NUMLAB) .GT. LABMAX) THEN
            XDIV = FLOAT(NUMLAB)/LABMAX
            IF ((AINT(XDIV) - XDIV) .EQ. 0.) THEN
               NDIV = INT(XDIV)
            ELSE
               NDIV = INT(XDIV + 1.)
            ENDIF
         ELSE
            NDIV = 1
         ENDIF
         IF (MIDVAL .OR. (COMPAR .OR. ASSIGN)) THEN
            LASTLB = WRK(NPTS+NCLASS)
            LASTL = NCLASS
         ELSE
            LASTL = NCLASS + 1
            LASTLB = WRK(NPTS+NCLASS) + HAFINT
         ENDIF
         I = 1
 710     CONTINUE
C     
C     CHECK TO MAKE SURE CURRENT LABEL IS NOT TOO CLOSE TO
C     LAST LABEL ( WRK(NPTS+NCLASS) )
C     
         XPOS = WRK(NPTS+I)
         IF (MIDVAL .OR. (COMPAR .OR. ASSIGN)) THEN
            IF (SPEC) XPOS2 = (CLASS(I+1) + CLASS(I))/2
         ELSE
            XPOS = XPOS - HAFINT
            IF (SPEC) XPOS2 = CLASS(I)
         ENDIF
         IF (I + NDIV .GT. LASTL) THEN
            DONE = .TRUE.
            XPOS = LASTLB
            IF (SPEC) XPOS2 = XPOS
            I = NCLASS
         ENDIF
         TESTCH = 'I'
         IF ( STRFOR(2:2) .EQ. TESTCH ) THEN
            IXPOS = NINT(XPOS)
            IF ( SPEC ) IXPOS2 = NINT(XPOS2)
            IF ( SPEC ) THEN
               WRITE(TEX,STRFOR)IXPOS2
            ELSE
               WRITE(TEX,STRFOR)IXPOS
            ENDIF
         ELSE
            IF ( SPEC ) THEN
               WRITE(TEX,STRFOR)XPOS2
            ELSE
               WRITE(TEX,STRFOR)XPOS
            ENDIF
         ENDIF
         IF ( CHARL ) THEN
            CALL HSTSTR(LABTEX(I),FIRST,LAST)
            IF (.NOT. HORZNT) THEN
c commented out
c               CALL NWTSTR(XPOS,YPOS,LABTEX(I)(FIRST:LAST))
               call plchmq(xpos,ypos,labtex(i)(first:last),12.,90.,1.)
            ELSE
c commented out
c               CALL NWTSTR(YPOS,XPOS,LABTEX(I)(FIRST:LAST))
               call plchmq(ypos,xpos,labtex(i)(first:last),12.,90.,1.)
            ENDIF
         ELSE
            CALL HSTSTR(TEX,FIRST,LAST)
            IF (.NOT. HORZNT) THEN
c commented out
c               CALL NWTSTR(XPOS,YPOS,TEX(FIRST:LAST))
               call plchmq(xpos,ypos,tex(first:last),12.,90.,1.)
            ELSE
c commented out
c               CALL NWTSTR(YPOS,XPOS,TEX(FIRST:LAST))
               call plchmq(ypos,xpos,tex(first:last),12.,90.,1.)

            ENDIF
         ENDIF
         IF (DONE) GOTO 720
         I = I + NDIV
         GOTO 710
 720     CONTINUE
         IF (.NOT. HORZNT) THEN
            CALL GSCHUP(0.,1.)
         ENDIF
         CHARH = .014 * YRANGE
         CALL GSCHH(CHARH)
C     
C     DRAW PERCENT AXIS
C     
         IF (COLORS) CALL GSPLCI(COLAXI)
         IF (PERCNT) THEN
            PX(1) = XMIN
            PX(2) = XMIN
            PY(1) = 0.
            PY(2) = YMAX
            IF (.NOT. HORZNT) THEN
C     (LEFT,HALF)
               CALL GSTXAL(1,3)
               CALL GPL(2,PX,PY)
            ELSE
C     (CENTER,BOTTOM)
               CALL GSTXAL(2,4)
               CALL GPL(2,PY,PX)
            ENDIF
            PX(1) = XMIN
            PX(2) = XMIN + 0.6*HAFINT
            IF ( COMPAR ) PX(2) = XMIN + 0.3*HAFINT
            XPOS = XMIN - (XMAX - XMIN)*.08/.8
            MAXPER=TPER
c            MAXPER=(YMAX/NPPTS)*100.0
            IF (MAXPER.GE.5) THEN
C
C     DRAW TICK MARKS FOR THE 5%, 10%, etc. locations
c
               DO 730 J = 1,20
                  PERR = 5.*J
                  IF (PERR.GT.MAXPER) GOTO 735
                  YVAL=(PERR*NPPTS)/100.0
                  PY(1)=YVAL
                  PY(2)=YVAL
                  WRITE(TEX,'(F5.1)')PERR
                  CALL HSTSTR(TEX,FIRST,LAST)
                  YPOS = PY(1)
                  IF (.NOT. HORZNT) THEN
                     CALL GPL(2,PX,PY)
c commented out
c                     CALL NWTSTR(XPOS,YPOS,TEX(FIRST:LAST)
                     call plchmq(xpos,ypos,tex(first:last),12.,0.,-1.)
                  ELSE
                     CALL GPL(2,PY,PX)
c commented out
c                     CALL NWTSTR(YPOS,XPOS,TEX(FIRST:LAST))
                    call plchmq(xpos,ypos,tex(first:last),12.,0.,-1.)
                  ENDIF
 730           CONTINUE
 735           CONTINUE
            ELSE
C
C     DRAW TICK MARKS FOR THE 1-5 PERCENT LOCATIONS
C
               DO 733 J=1,5
                  PERR = 1.*J
                  IF (PERR.GT.MAXPER) GOTO 737
                  YVAL=(PERR*NPPTS)/100.0
                  PY(1)=YVAL
                  PY(2)=YVAL
                  WRITE(TEX,'(F5.1)')PERR
                  CALL HSTSTR(TEX,FIRST,LAST)
                  YPOS = PY(1)
                  IF (.NOT. HORZNT) THEN
                     CALL GPL(2,PX,PY)
c commented out
                     CALL NWTSTR(XPOS,YPOS,TEX(FIRST:LAST))
                  ELSE
                     CALL GPL(2,PY,PX)
c commented out
                     CALL NWTSTR(YPOS,XPOS,TEX(FIRST:LAST))
                  ENDIF
 733           CONTINUE
 737           CONTINUE
            END IF
         ENDIF
C     
C     DRAW MEDIAN
C     
         IF (IYMED.EQ.1) THEN
            IF (COLORS) CALL GSPLCI(COLMED)
            PX(1) = VMED
            PX(2) = VMED
            PY(1) = 0.
            PY(2) = YMAX
            IF (.NOT. HORZNT) THEN
               VMED = (VMED - NEWWIN(1))/XRANGE
               CALL GPL(2,PX,PY)
            ELSE
               VMED = (VMED - NEWWIN(3))/YRANGE
               CALL GPL(2,PY,PX)
            ENDIF
         ENDIF
C     
C     DRAW PERIMETER
C     
         IF (WINDOW) THEN
            CALL GSWN(1,0.,1.,0.,1.)
            CALL GSVP(1,HWIND(1),HWIND(2),HWIND(3),HWIND(4))
            CALL GSELNT(1)
         ELSE
            CALL GSELNT(0)
         ENDIF
         IF (PERIM) THEN
            IF (COLORS) CALL GSPLCI(COLPER)
            PX(1)=0.
            PX(2)=0.
            PX(3)=1.
            PX(4)=1.
            PX(5)=0.
            PY(1)=0.
            PY(2)=1.
            PY(3)=1.
            PY(4)=0.
            PY(5)=0.
            CALL GPL(5,PX,PY)
         ENDIF
C     
C     OUTPUT LABELS
C     
         CHARH = .015
         CALL GSCHH(CHARH)
C     CENTER,HALF
         CALL GSTXAL(2,3)
         XPOS = .5
         YPOS = .04
         NCHAR = LEN(STRLAB)
         IF (.NOT. LABEL) THEN
            IF (MIDVAL .OR. (COMPAR .OR. ASSIGN)) THEN
               TEX = 'CLASS MIDVALUES'
            ELSE
               TEX = 'CLASS INTERVALS'
            ENDIF
            NCHAR = 15
            IF (.NOT. HORZNT) THEN
c commented out
               CALL NWTSTR(XPOS,YPOS,TEX(1:NCHAR))
            ELSE
               CALL GSCHUP(-1.,0.)
c commented out
               CALL NWTSTR(YPOS,XPOS,TEX(1:NCHAR))
            ENDIF
         ELSE
            IF (STRLAB .EQ. 'NOLABEL') Goto 740
            CALL HSTSTR(STRLAB,FIRST,LAST)
            IF (.NOT. HORZNT) THEN
c commented out
               CALL NWTSTR(XPOS,YPOS,STRLAB(FIRST:LAST))
            ELSE
               CALL GSCHUP(-1.,0.)
c commented out
               CALL NWTSTR(YPOS,XPOS,STRLAB(FIRST:LAST))
            ENDIF
         ENDIF
 740     CONTINUE
         IF (FREQNC) THEN
            IF (STRFRE .EQ. 'NOLABEL') Goto 750
            NCHAR = LEN(STRFRE)
            CALL HSTSTR(STRFRE,FIRST,LAST)
            IF (.NOT. HORZNT) THEN
               CALL GSCHUP(-1.,0.)
c commented out
               CALL NWTSTR(YPOS,XPOS,STRFRE(FIRST:LAST))
            ELSE
               XPOS = .5
               CALL GSCHUP(0.,1.)
c commented out
               CALL NWTSTR(XPOS,YPOS,STRFRE(FIRST:LAST))
            ENDIF
         ELSE
C            IF (.NOT. HORZNT) THEN
C               CALL GSCHUP(-1.,0.)
C               CALL NWTSTR(YPOS,XPOS,'FREQUENCY')
C            ELSE
C               XPOS = .5
C               CALL GSCHUP(0.,1.)
C               CALL NWTSTR(XPOS,YPOS,'FREQUENCY')
C            ENDIF
         ENDIF
 750     CONTINUE
C     
C     LABEL PERCENT AXIS
C     
         IF (PERCNT) THEN
            IF (.NOT. HORZNT) THEN
               YPOS = .40
               XPOS = .05
               IF ( ASSIGN .OR. COMPAR) THEN
c                  CALL NWTSTR(YPOS,XPOS,'PERCENT of TOTAL')
                  CALL PLCHMQ(CFUX(XPOS),CFUY(YPOS),
     X                        'PERCENT of TOTAL',12.,90.,0.)
               ELSE
                  CALL NWTSTR(YPOS,XPOS,'PERCENT OCCURRENCE')
               ENDIF
            ELSE
               YPOS = .92
               XPOS = .5
               IF ( ASSIGN .OR. COMPAR) THEN
C                  CALL NWTSTR(XPOS,YPOS,'PERCENT of TOTAL')
                  CALL PLCHMQ(CFUX(.56),CFUY(XPOS),
     X                        'PERCENT of TOTAL',120.,90.,0.)
               ELSE
                  CALL NWTSTR(XPOS,YPOS,'PERCENT OCCURRENCE')
               ENDIF
            ENDIF
         ENDIF
C     
C     LABEL MEDIAN
C     
         IF (IYMED.EQ.1) THEN
            XPOS = VMED - .018
            YPOS = .85
            IF (.NOT. HORZNT) THEN
               YPOS = .81
               CALL PLCHMQ(CFUX(XPOS),CFUY(YPOS-.2),'MEDIAN',12.,90.,0.)
C               CALL NWTSTR(XPOS,YPOS,'MEDIAN')
            ELSE
               CALL NWTSTR(YPOS,XPOS,'MEDIAN')
            ENDIF
         ENDIF
C     
C     OUTPUT TITLE
C     
         IF (TITLE) THEN
            IF (COLORS) CALL GSTXCI(COLTIT)
            CHARH = .016
            CALL HSTSTR(STRTIT,FIRST,LAST)
            CALL GSCHH(CHARH)
            CALL GSCHUP(0.,1.)
            CALL NWTSTR(.5,.965,STRTIT(FIRST:LAST))
         ENDIF
C     
C     CALL FRAME UNLESS .HFRAME. IS FALSE
C     
         IF (HFRAME) CALL FRAME
C     
C     RESET NORMALIZATION TRANSFORMATION TO WHAT IT WAS UPON ENTRY
C     TO HISTOGRAM
C     
         IF (ICNT .NE. 0) THEN
            CALL GSWN(ICNT,OWIND(1),OWIND(2),OWIND(3),OWIND(4))
            CALL GSVP(ICNT,OVIEW(1),OVIEW(2),OVIEW(3),OVIEW(4))
         ENDIF
         CALL GSELNT(ICNT)
C     
C     RESTORE TEXT ATTRIBUTES
C     
         CALL GSCHH(OLDCHH)
         CALL GSCHUP(CHUPX,CHUPY)
         CALL GSTXAL(OLDALH,OLDALV)
         CALL GSTXCI(OTEXCI)
C     
C     RESTORE ORIGINAL COLOR
C     
         CALL GSPLCI(OCOLI)
C     
C     RESTORE POLYLINE COLOR ASF TO WHAT IT WAS ON ENTRY TO GRIDAL
C     
         LASF(10) = OTXASF
         LASF(3) = OPLASF
         CALL GSASF(LASF)
C     
         CALL PLOTIT(0,0,0)
         RETURN
         END
