      SUBROUTINE STRMLN (U,V,WORK,IMAX,IPTSX,JPTSY,NSET,ISPAC,BAD,IER)
C
C        MODIFICATIONS FOR CEDRIC MADE BY CARL MOHR 6/22/83
C
C                                                                         STRMLN
C SUBROUTINE STRMLN (U,V,WORK,IMAX,IPTSX,JPTSY,NSET,IER)                  STRMLN
C                                                                         STRMLN
C DIMENSION OF           U(IMAX,JPTSY) , V(IMAX,JPTSY) ,                  STRMLN
C ARGUMENTS              WORK(2*IMAX*JPTSY)                               STRMLN
C                                                                         STRMLN
C WRITTEN                OCTOBER 1979                                     STRMLN
C                                                                         STRMLN
C PURPOSE                STRMLN DRAWS A STREAMLINE REPRESENTATION OF      STRMLN
C                        THE FLOW FIELD. THE REPRESENTATION IS            STRMLN
C                        INDEPENDENT OF THE FLOW SPEED.                   STRMLN
C                                                                         STRMLN
C USAGE                  IF THE FOLLOWING ASSUMPTIONS ARE MET, USE        STRMLN
C                            CALL EZSTRM  (U,V,WORK,IMAX,JMAX)            STRMLN
C                          ASSUMPTIONS:                                   STRMLN
C                            THE WHOLE ARRAY IS TO BE PROCESSED.          STRMLN
C                            THE ARRAYS ARE DIMENSIONED                   STRMLN
C                            U(IMAX,JMAX) , V(IMAX,JMAX) AND              STRMLN
C                            WORK(2*IMAX*JMAX).                           STRMLN
C                            SET AND PERIM ARE TO BE CALLED BY STRMLN.    STRMLN
C                                                                         STRMLN
C                        IF THESE ASSUMPTIONS ARE NOT MET, USE            STRMLN
C                            CALL STRMLN (U,V,WORK,IMAX,IPTSX,JPTSY       STRMLN
C                                        ,NSET,IER)                       STRMLN
C                                                                         STRMLN
C                        THE USER MUST CALL FRAME IN THE CALLING          STRMLN
C                        ROUTINE.                                         STRMLN
C                                                                         STRMLN
C                        THE USER MAY CHANGE VARIOUS INTERNAL             STRMLN
C                        PARAMETERS VIA COMMON BLOCKS. SEE BELOW.         STRMLN
C                                                                         STRMLN
C ARGUMENTS                                                               STRMLN
C                                                                         STRMLN
C ON INPUT               U, V                                             STRMLN
C                          THE TWO DIMENSIONAL ARRAYS CONTAINING THE      STRMLN
C                          VELOCITY FIELDS TO BE PLOTTED.                 STRMLN
C                          (NOTE : IF THE U AND V COMPONENTS              STRMLN
C                          ARE, FOR EXAMPLE, DEFINED IN CARTESIAN         STRMLN
C                          COORDINATES AND THE USER WISHES TO PLOT THEM   STRMLN
C                          ON, FOR EXAMPLE, A POLAR STEREOGRAPHIC         STRMLN
C                          PROJECTION THEN AN APPROPRIATE                 STRMLN
C                          TRANSFORMATION MUST BE MADE TO THE U AND V     STRMLN
C                          COMPONENTS VIA THE FUNCTIONS FU AND FV         STRMLN
C                          (LOCATED IN DRWSTR) . SEE THE                  STRMLN
C                          NCAR GRAPHICS MANUAL FOR MORE INFO ON          STRMLN
C                          COORDINATE TRANSFORMATIONS.)                   STRMLN
C                                                                         STRMLN
C                        WORK                                             STRMLN
C                          USER PROVIDED WORK ARRAY.  THE DIMENSION       STRMLN
C                          OF THIS ARRAY MUST BE .GE. 2*IMAX*JPTSY.       STRMLN
C                          CAUTION : THIS ROUTINE DOES NOT CHECK THE      STRMLN
C                          SIZE OF THE WORK ARRAY.                        STRMLN
C                                                                         STRMLN
C                        IMAX                                             STRMLN
C                          THE FIRST DIMENSION OF U AND V IN THE          STRMLN
C                          CALLING PROGRAM. (X DIRECTION)                 STRMLN
C                                                                         STRMLN
C                        IPTSX                                            STRMLN
C                          THE NUMBER OF POINTS TO BE PLOTTED IN THE      STRMLN
C                          FIRST SUBSCRIPT DIRECTION.  (X DIRECTION)      STRMLN
C                                                                         STRMLN
C                        JPTSY                                            STRMLN
C                          THE NUMBER OF POINTS TO BE PLOTTED IN THE      STRMLN
C                          SECOND SUBSCRIPT DIRECTION. (Y DIRECTION)      STRMLN
C                                                                         STRMLN
C                          NOTE: SEE THE SECTION IN THE GRAPHICS          STRMLN
C                          MANUAL ON USING THESE ARGUMENTS TO PLOT        STRMLN
C                          A PORTION OF AN ARRAY.                         STRMLN
C                                                                         STRMLN
C                        NSET                                             STRMLN
C                          FLAG TO CONTROL SCALING                        STRMLN
C                          > 0  STRMLN  ASSUMES THAT SET HAS BEEN         STRMLN
C                               CALLED BY THE USER IN SUCH A WAY AS TO    STRMLN
C                               PROPERLY SCALE THE PLOTTING INSTRUCTIONS  STRMLN
C                               GENERATED BY STRMLN . PERIM IS NOT        STRMLN
C                               CALLED.                                   STRMLN
C                          = 0  STRMLN  CALLS SET TO PROPERLY SCALE THE   STRMLN
C                               PLOTTING INSTRUCTIONS TO THE STANDARD     STRMLN
C                               CONFIGURATION. PERIM IS CALLED TO DRAW    STRMLN
C                               THE BORDER.                               STRMLN
C                          < 0  STRMLN  CALLS SET IN SUCH A WAY AS TO     STRMLN
C                               PLACE THE STREAMLINES WITHIN THE LIMITS   STRMLN
C                               OF THE USERS LAST SET CALL. PERIM IS      STRMLN
C                               NOT CALLED                                STRMLN
C                                                                         STRMLN
C ON OUTPUT              ONLY THE IER ARGUMENT MAY BE CHANGED. ALL        STRMLN
C                        OTHER ARGUMENTS ARE UNCHANGED.                   STRMLN
C                                                                         STRMLN
C                                                                         STRMLN
C                        IER                                              STRMLN
C                          =  0 WHEN NO ERRORS ARE DETECTED               STRMLN
C                          = -1 WHEN THE ROUTINE IS CALLED WITH ICYC      STRMLN
C                               .NE. 0  AND THE DATA ARE NOT CYCLIC.      STRMLN
C                               (ICYC IS AN INTERNAL PARAMETER AND        STRMLN
C                               IS DESCRIBED BELOW.)                      STRMLN
C                               IN THIS CASE THE ROUTINE WILL DRAW THE    STRMLN
C                               STREAMLINES WITH THE NON-CYCLIC           STRMLN
C                               INTERPOLATION FORMULAS.                   STRMLN
C                                                                         STRMLN
C ENTRY POINTS           STRMLN, DRWSTR, EZSTRM, GNEWPT, CHKCYC           STRMLN
C                                                                         STRMLN
C SPACE REQUIRED         APPROXIMATELY  5650 OCTAL (CDC 7600), NOT        STRMLN
C                        INCLUDING THE REQUIRED RESIDENT ROUTINES OR      STRMLN
C                        THE SYSTEM PLOT PACKAGE. THIS PROGRAM MAY BE     STRMLN
C                        MADE SMALLER BY DELETING  PORTIONS OF GNEWPT OR  STRMLN
C                        MAKING THE CIRCULAR LIST ARRAYS IN DRWSTR        STRMLN
C                        SMALLER.                                         STRMLN
C                                                                         STRMLN
C COMMON BLOCKS          STR01  (20 OCTAL)                                STRMLN
C                          (USED FOR COMMUNICATION                        STRMLN
C                          BETWEEN VARIOUS STREAMLINE ROUTINES)           STRMLN
C                                                                         STRMLN
C                        STR02  (6 OCTAL)                                 STRMLN
C                          (THIS BLOCK CONTAINS PLOTTER INFORMATION       STRMLN
C                          AND MAY BE USED BY THE USER TO ALTER           STRMLN
C                          THE DEFAULT SIZE OF THE STRMLN PLOT.)          STRMLN
C                                                                         STRMLN
C                        STR03  (14 OCTAL)                                STRMLN
C                          (THIS BLOCK IS PROBABLY THE MOST               STRMLN
C                          USEFUL TO THE USER.  MANY INTERNAL             STRMLN
C                          PARAMETERS, DISCUSSED BELOW, MAY               STRMLN
C                          BE ALTERED BY THE USER BY INSERTING            STRMLN
C                          THIS BLOCK IN THE USERS CODE AND               STRMLN
C                          CHANGING THE DEFAULT VALUES.)                  STRMLN
C                                                                         STRMLN
C                        STR04  (3720 OCTAL)                              STRMLN
C                          (PRIMARILY INTERNAL TO DRWSTR. THIS BLOCK      STRMLN
C                          CONTAINS THE CIRCULAR LISTS USED TO PREVENT    STRMLN
C                          STREAMLINE CROSSOVER.)                         STRMLN
C                                                                         STRMLN
C I/O                    DRAWS STREAMLINES                                STRMLN
C                                                                         STRMLN
C PORTABILITY            PORTABLE                                         STRMLN
C                          (NOTE: ALL VARIABLE NAMES ARE UNIQUE TO        STRMLN
C                          TO FIVE CHARACTERS. THIS MAKES IT              STRMLN
C                          COMPATIBLE WITH DATA GENERAL FORTRAN.)         STRMLN
C                                                                         STRMLN
C PRECISION              SINGLE                                           STRMLN
C                                                                         STRMLN
C TIMING                 HIGHLY VARIABLE                                  STRMLN
C                          IT DEPENDS ON THE COMPLEXITY OF THE            STRMLN
C                          FLOW FIELD AND THE PARAMETERS : DISPL,         STRMLN
C                          DISPC , CSTOP , INITA , INITB , ITERC ,        STRMLN
C                          AND IGFLG. (SEE BELOW FOR A DISCUSSION         STRMLN
C                          OF THE LATTER PARAMETERS.) IF ALL VALUES       STRMLN
C                          ARE DEFAULT THEN A SIMPLE                      STRMLN
C                          LINEAR FLOW FIELD FOR A 40 X 40 GRID           STRMLN
C                          WILL TAKE SLIGHTLY LESS THAN ONE SECOND        STRMLN
C                          CDC7600  AND ABOUT 0.4 SEC ON THE CRAY 1.      STRMLN
C                          A FAIRLY COMPLEX FLOW FIELD WILL TAKE ABOUT    STRMLN
C                          FOUR SECONDS ON THE CDC7600 AND ABOUT 1.5 SEC  STRMLN
C                          ON THE CRAY 1. AS A GENERAL RULE THIS          STRMLN
C                          ROUTINE RUNS ABOUT 2.5 TIMES FASTER ON THE     STRMLN
C                          CRAY 1. NO ATTEMPT HAS BEEN MADE TO VECTORIZE  STRMLN
C                          SINCE MANY IF CHECKS MUST BE MADE AT           STRMLN
C                          VIRTUALLY EVERY STEP.                          STRMLN
C                                                                         STRMLN
C                          TIMING TESTS INDICATE THAT THIS ROUTINE IS     STRMLN
C                          APPROXIMATELY 20 PER-CENT FASTER THAN THE      STRMLN
C                          PREVIOUS ULIB ROUTINE STRML2.                  STRMLN
C                                                                         STRMLN
C REQUIRED ULIB          NONE                                             STRMLN
C ROUTINES                                                                STRMLN
C                                                                         STRMLN
C SPECIALIST             DENNIS SHEA, NCAR, BOULDER, COLO. 80307 (X 214)  STRMLN
C                                                                         STRMLN
C LANGUAGE               FORTRAN                                          STRMLN
C                                                                         STRMLN
C HISTORY                THE TECHNIQUES UTILIZED HERE ARE DESCRIBED       STRMLN
C                        IN AN ARTICLE BY THOMAS WHITTAKER (U. OF         STRMLN
C                        WISCONSIN) WHICH APPEARED IN THE NOTES AND       STRMLN
C                        CORRESPONDENCE SECTION OF MONTHLY WEATHER        STRMLN
C                        REVIEW, JUNE 1977.                               STRMLN
C                                                                         STRMLN
C ALGORITHM              WIND COMPONENTS ARE NORMALIZED TO THE VALUE      STRMLN
C                        OF DISPL. THEN THE LEAST SIGNIFICANT TWO         STRMLN
C                        BITS OF THE WORK ARRAY ARE                       STRMLN
C                        UTILIZED AS FLAGS FOR EACH GRID BOX. FLAG 1      STRMLN
C                        INDICATES WHETHER ANY STREAMLINE HAS             STRMLN
C                        PREVIOUSLY PASSED THROUGH THIS BOX.  FLAG 2      STRMLN
C                        INDICATES WHETHER A DIRECTIONAL ARROW HAS        STRMLN
C                        ALREADY APPEARED IN A BOX. JUDICIOUS USE OF      STRMLN
C                        THESE FLAGS PREVENTS OVERCROWDING OF             STRMLN
C                        STREAMLINES AND DIRECTIONAL ARROWS.              STRMLN
C                        EXPERIENCE INDICATES THAT A FINAL PLEASING       STRMLN
C                        PICTURE IS PRODUCED WHEN STREAMLINES ARE         STRMLN
C                        INITIATED IN THE CENTER OF A GRID BOX. THE       STRMLN
C                        STREAMLINES ARE DRAWN IN ONE DIRECTION THEN      STRMLN
C                        IN THE OPPOSITE DIRECTION.                       STRMLN
C                                                                         STRMLN
C PLOTTING ROUTINES      STRMLN: GETSET , SET , PERIM                     STRMLN
C USED                   DRWSTR: FRSTPT , VECTOR                          STRMLN
C                                                                         STRMLN
C REQUIRED RESIDENT      ATAN2, COS, SIN, IFIX, SQRT, MOD, MIN0           STRMLN
C ROUTINES               AND, OR, COMPL , AINT                            STRMLN
C                                                                         STRMLN
C                        NOTICE TO DATA GENERAL ECLIPSE USERS :           STRMLN
C                                                                         STRMLN
C                        THE AND / OR STATEMENT FUNCTIONS DO NOT HAVE A   STRMLN
C                        COUNTERPART ON THE DG THUS SPECIALLY WRITTEN     STRMLN
C                        ROUTINES  (VIA LIN THIEL) ARE USED IN THEIR      STRMLN
C                        PLACE. THESE ROUTINES ARE NAMED ANDF AND ORF.    STRMLN
C                        THE COMPL IS REPLACED BY THE DG ROUTINE NOT.     STRMLN
C                                                                         STRMLN
C INTERNAL PARAMETERS                                                     STRMLN
C                                                                         STRMLN
C                        CRTX,CRTY 1024      MAX NO. PLOTTER PTS. IN X A  STRMLN
C                                                                         STRMLN
C                        EXT       0.25      LENGTHS OF THE SIDES OF THE  STRMLN
C                                            PLOT ARE PROPORTIONAL TO     STRMLN
C                                            IPTSX AND JPTSY EXCEPT IN    STRMLN
C                                            THE CASE WHEN MIN(IPTSX,JPT  STRMLN
C                                            / MAX(IPTSX,JPTSY) .LT. EXT  STRMLN
C                                            IN WHICH CASE A SQUARE       STRMLN
C                                            GRAPH IS PLOTTED.            STRMLN
C                                                                         STRMLN
C                        NAND      3         THE COMPLIMENT OF NAND IS    STRMLN
C                                            USED TO MASKING OUT THE      STRMLN
C                                            LOW ORDER BITS AS FLAGS.     STRMLN
C                                                                         STRMLN
C                        SIDE      0.90      LENGTH OF LONGER EDGE OF     STRMLN
C                                            PLOT. (SEE ALSO EXT.)        STRMLN
C                                                                         STRMLN
C                        XLT       0.05      LEFT HAND EDGE OF THE PLOT.  STRMLN
C                                            (0.0 = LEFT EDGE OF FRAME)   STRMLN
C                                            (1.0 = RIGHT EDGE OF FRAME)  STRMLN
C                                                                         STRMLN
C                        YBT       0.05      BOTTOM EDGE OF THE PLOT.     STRMLN
C                                            (0.0 = BOTTOM ; 1.0 = TOP)   STRMLN
C                                                                         STRMLN
C                                            (YBT+SIDE AND XLT+SIDE MUST  STRMLN
C                                            BE .LE. 1. )                 STRMLN
C                                                                         STRMLN
C                        INITA      2        USED TO PRECONDITION GRID    STRMLN
C                                            BOXES TO BE ELIGIBLE TO      STRMLN
C                                            START A STREAMLINE. (E.G.,   STRMLN
C                                            A VALUE OF FOUR              STRMLN
C                                            MEANS THAT EVERY FOURTH      STRMLN
C                                            GRID BOX IS ELIGIBLE ; A     STRMLN
C                                            VALUE OF 2 MEANS YHAT EVERY  STRMLN
C                                            OTHER GRID BOX IS ELIGIBLE.  STRMLN
C                                                                         STRMLN
C                        INITB      2        USED TO PRECONDITION GRID B  STRMLN
C                                            TO BE ELIGIBLE FOR DIRECTIO  STRMLN
C                                            ARROWS.                      STRMLN
C                                                                         STRMLN
C                                            (IF THE USER CHANGES THE DE  STRMLN
C                                            VALUES OF INITA AND/OR INIT  STRMLN
C                                            IT SHOULD BE DONE            STRMLN
C                                            SUCH THAT  MOD(INITA,INITB)  STRMLN
C                                            IF THE USER HAS A REASONABL  STRMLN
C                                            DENSE GRID TRY INITA = 4 ,   STRMLN
C                                            INITB = 2 . IT WILL CUT CPU  STRMLN
C                                            DOWN.)                       STRMLN
C                                                                         STRMLN
C                        AROWL      0.33     LENGTH OF DIRECTIONAL ARROW  STRMLN
C                                            (0.33 , FOR EXAMPLE , MEANS  STRMLN
C                                            EACH DIRECTIONAL ARROW WILL  STRMLN
C                                            UP A THIRD OF A GRID BOX.)   STRMLN
C                                                                         STRMLN
C                        ITERP      35       EVERY 'ITERP' ITERATIONS     STRMLN
C                                            THE STREAMLINE PROGRESS      STRMLN
C                                            IS CHECKED.                  STRMLN
C                                                                         STRMLN
C                        ITERC      -99      THE DEFAULT VALUE OF THIS    STRMLN
C                                            PARAMETER IS SUCH THAT IT    STRMLN
C                                            IT HAS NO EFFECT ON THE      STRMLN
C                                            CODE. WHEN SET TO SOME       STRMLN
C                                            POSITIVE VALUE THE PROGRAM   STRMLN
C                                            WILL CHECK FOR STREAMLINE    STRMLN
C                                            CROSSOVER EVERY 'ITERC'      STRMLN
C                                            ITERATIONS. (THE ROUTINE     STRMLN
C                                            CURRENTLY DOES THIS EVERY    STRMLN
C                                            TIME IT ENTERS A NEW GRID    STRMLN
C                                            BOX.) CAUTION : WHEN         STRMLN
C                                            THIS PARAMETER IS ACTIVATED  STRMLN
C                                            CPU TIME WILL INCREASE.      STRMLN
C                                                                         STRMLN
C                        IGFLG      0        A VALUE OF ZERO MEANS THAT   STRMLN
C                                            THE SIXTEEN POINT BESSEL     STRMLN
C                                            INTERPOLATION FORMULA WILL   STRMLN
C                                            UTILIZED WHERE POSSIBLE. WH  STRMLN
C                                            NEAR THE GRID EDGES QUADRAT  STRMLN
C                                            BLINEAR INTERPOLATION  WILL  STRMLN
C                                            USED. THIS MIXING OF         STRMLN
C                                            INTERPOLATION SCHEME CAN CA  STRMLN
C                                            SLIGHT RAGGEDNESS NEAR THE   STRMLN
C                                            EDGES. THIS, HOWEVER, DOES   STRMLN
C                                            OCCUR VERY OFTEN. IF IGFLG   STRMLN
C                                            THEN THE BILINEAR INTERPOLA  STRMLN
C                                            FORMULA (ONLY) IS TO BE USE  STRMLN
C                                            THIS WILL RESULT IN SLIGHTL  STRMLN
C                                            FASTER PLOT TIMES. HOWEVER,  STRMLN
C                                            GENERAL THE PLOTS WILL BE L  STRMLN
C                                            PLEASING.                    STRMLN
C                                                                         STRMLN
C                        IMSG       0        IF ZERO THEN NO MISSING      STRMLN
C                                            U AND V COMPONENTS ARE       STRMLN
C                                            PRESENT.  IF .NE. 0 THEN     STRMLN
C                                            STRMLN WILL UTILIZE THE      STRMLN
C                                            BI-LINEAR INTERPOLATION      STRMLN
C                                            SCHEME AND IF ANY OF THE     STRMLN
C                                            POINTS ARE MISSING THE       STRMLN
C                                            THE STREAMLINE WILL BE       STRMLN
C                                            TERMINATED.                  STRMLN
C                                                                         STRMLN
C                        UVMSG      1.E+36   VALUE ASSIGNED TO A MISSING  STRMLN
C                                            POINT.                       STRMLN
C                                                                         STRMLN
C                        ICYC       0        ZERO MEANS THE DATA ARE      STRMLN
C                                            NON-CYCLIC IN THE X          STRMLN
C                                            DIRECTION. IF .NE. 0 THEN    STRMLN
C                                            THE CYCLIC INTERPOLATION     STRMLN
C                                            FORMULAS WILL BE USED.       STRMLN
C                                            (NOTE:  EVEN IF THE DATA     STRMLN
C                                            ARE CYCLIC IN X LEAVING      STRMLN
C                                            ICYC = 0 WILL DO NO HARM.)   STRMLN
C                                                                         STRMLN
C                        DISPL      0.33     THE WIND SPEED IS            STRMLN
C                                            NORMALIZED TO THIS VALUE.    STRMLN
C                                            (SEE BELOW FOR A DISCUSSION  STRMLN
C                                            OF THE DISPL, DISPC AND      STRMLN
C                                            CSTOP PARAMETERS.)           STRMLN
C                                                                         STRMLN
C                        DISPC      0.67     THE CRITICAL DISPLACEMENT.   STRMLN
C                                            IF AFTER 'ITERP' ITERATIONS  STRMLN
C                                            THE STREAMLINE HAS NOT       STRMLN
C                                            MOVED THIS DISTANCE THEN     STRMLN
C                                            THE STREAMLINE WILL BE       STRMLN
C                                            TERMINATED.                  STRMLN
C                                                                         STRMLN
C                        CSTOP      0.50     THIS PARAMETER CONTROLS      STRMLN
C                                            THE SPACING BETWEEN          STRMLN
C                                            STREAMLINES.  THE CHECKING   STRMLN
C                                            IS DONE WHEN A NEW GRID      STRMLN
C                                            BOX IS ENTERED.              STRMLN
C                                                                         STRMLN
C DISCUSSION OF          ASSUME A VALUE OF 0.33 FOR DISPL. THIS           STRMLN
C DISPL,DISPC            MEANS THAT IT WILL TAKE THREE STEPS TO MOVE      STRMLN
C AND CSTOP              ACROSS ONE GRID BOX IF THE FLOW WAS ALL IN       STRMLN
C                        X DIRECTION. IF THE FLOW IS ZONAL THEN A         STRMLN
C                        LARGER VALUE OF DISPL IS IN ORDER. IF ,          STRMLN
C                        HOWEVER , THE FLOW IS HIGHLY TURBULENT THEN      STRMLN
C                        A SMALLER VALUE IS IN ORDER. NOTE--THE SMALLER   STRMLN
C                        DISPL THE MORE THE COMPUTER TIME. A VALUE        STRMLN
C                        OF 2 TO 4 TIMES DISPL IS A REASONABLE VALUE      STRMLN
C                        FOR DISPC.  DISPC SHOULD ALWAYS BE GREATER       STRMLN
C                        THAN DISPL. A VALUE OF 0.33 FOR CSTOP WOULD      STRMLN
C                        MEAN THAT A MAXIMUM OF THREE STREAM-             STRMLN
C                        LINES WILL BE DRAWN PER GRID BOX. THIS MAX       STRMLN
C                        WILL NORMALLY ONLY OCCUR IN AREAS OF SINGULAR    STRMLN
C                        POINTS.                                          STRMLN
C                                                                         STRMLN
C                                            ***************************  STRMLN
C                                            ANY OR ALL OF THE ABOVE      STRMLN
C                                            PARAMETERS MAY BE CHANGED    STRMLN
C                                            BY UTILIZING COMMON BLOCKS   STRMLN
C                                            STR02 AND/OR STR03           STRMLN
C                                            ***************************  STRMLN
C                                                                         STRMLN
C                      UXSML        1.E-50   SEE SUB DRWSTR FOR A DIS-    STRMLN
C                                            CUSSION OF THIS PARAMETER.   STRMLN
C                                            THIS MAY NEED TO BE CHANGED  STRMLN
C                                            DEPENDING UPON THE TARGET    STRMLN
C                                            COMPUTER.BASICALY IT SHOULD  STRMLN
C                                            BE SOME VERY SMALL NUMBER.   STRMLN
C                                                                         STRMLN
C                      NCHK         750      THIS PARAMETER IS LOCATED    STRMLN
C                                            IN DRWSTR. IT SPECIFIES THE  STRMLN
C                                            LENGTH OF THE CIRCULAR       STRMLN
C                                            LISTS  USED FOR CHECKING     STRMLN
C                                            FOR STRMLN CROSSOVERS.       STRMLN
C                                            FOR MOST PLOTS THIS NUMBER   STRMLN
C                                            MAY BE REDUCED TO 500        STRMLN
C                                            OR LESS AND THE PLOTS WILL   STRMLN
C                                            NOT BE ALTERED.              STRMLN
C                                                                         STRMLN
C                                                                         STRMLN
      DIMENSION       U(IMAX,JPTSY)         ,V(IMAX,JPTSY)           ,    STRMLN
     1                WORK(1)                                             STRMLN
C                                                                         STRMLN
      COMMON /STR01/  IS         ,IEND      ,JS        ,JEND              STRMLN
     1             ,  IEND1      ,JEND1     ,I         ,J                 STRMLN
     2             ,  X          ,Y         ,DELX      ,DELY              STRMLN
     3             ,  NAND       ,ICYC1     ,IMSG1     ,IGFL1             STRMLN
      COMMON /STR02/  CRTX , CRTY , EXT , SIDE , XLT , YBT                STRMLN
      COMMON /STR03/  INITA , INITB , AROWL , ITERP , ITERC , IGFLG       STRMLN
     1             ,  IMSG , UVMSG , ICYC , DISPL , DISPC , CSTOP         STRMLN
C                                                                         STRMLN
      DATA      CRTX     /1024./                                          STRMLN
      DATA      CRTY     /1024./                                          STRMLN
      DATA      EXT      /0.25/                                           STRMLN
      DATA      SIDE     /0.90/                                           STRMLN
      DATA      XLT      /0.05/                                           STRMLN
      DATA      YBT      /0.05/                                           STRMLN
C                                                                         STRMLN
      DATA      AROWL    /0.33/                                           STRMLN
      DATA      ITERP    /35/                                             STRMLN
      DATA      ITERC    /-99/                                            STRMLN
      DATA      IGFLG    /0/                                              STRMLN
      DATA      ICYC     /0/                                              STRMLN
      DATA      IMSG     /1/                                              STRMLN
      DATA      DISPL    /0.33/                                           STRMLN
      DATA      DISPC    /0.67/                                           STRMLN
      DATA      CSTOP    /0.50/                                           STRMLN
C                                                                         STRMLN
C THE FOLLOWING CALL IS FOR MONITORING LIBRARY USE AT NCAR                STRMLN
C                                                                         STRMLN
      AROWL=AMAX0(IPTSX,JPTSY)/80.0
      INITA=ISPAC
      INITB=ISPAC
      UVMSG=BAD
C                                                                         STRMLN
      IER = 0                                                             STRMLN
C                                                                         STRMLN
C SET 3 TO ITS COMPLEMENT VIA THE COMPL FUNCTION (FOR PORTABILITY)        STRMLN
C                                                                         STRMLN
      NAND = 3                                                            STRMLN
      NAND = NOT ( NAND )                                                STRMLN
C                                                                         STRMLN
C LOAD THE COMMUNICATION COMMON BLOCK WITH PARAMETERS                     STRMLN
C                                                                         STRMLN
      IS = 1                                                              STRMLN
      IEND = IPTSX                                                        STRMLN
      JS = 1                                                              STRMLN
      JEND = JPTSY                                                        STRMLN
      IEND1 = IEND-1                                                      STRMLN
      JEND1 = JEND-1                                                      STRMLN
      IEND2 = IEND-2                                                      STRMLN
      JEND2 = JEND-2                                                      STRMLN
      XNX = FLOAT(IEND-IS+1)                                              STRMLN
      XNY = FLOAT(JEND-JS+1)                                              STRMLN
      ICYC1 = ICYC                                                        STRMLN
      IGFL1 = IGFLG                                                       STRMLN
      IMSG1 = 0                                                           STRMLN
C                                                                         STRMLN
C IF ICYC .NE. 0 THEN CHECK TO MAKE SURE THE CYCLIC CONDITION EXISTS.     STRMLN
C                                                                         STRMLN
      IF (ICYC1.NE.0) CALL CHKCYC  (U,V,IMAX,JPTSY,IER)                   STRMLN
C                                                                         STRMLN
C SET UP SCALING                                                          STRMLN
C                                                                         STRMLN
      IF (NSET) 10 , 20 , 60                                              STRMLN
   10 CALL GETSET (FX1,FX2,FY1,FY2,X3,X4,Y3,Y4,ITYPE)                     STRMLN
      IX1=KFPX(FX1)                                                       STRMLN
      IX2=KFPX(FX2)                                                       STRMLN
      IY1=KFPY(FY1)                                                       STRMLN
      IY2=KFPY(FY2)                                                       STRMLN 
      X1 = FLOAT(IX1)/CRTX                                                STRMLN
      X2 = FLOAT(IX2)/CRTX                                                STRMLN
      Y1 = FLOAT(IY1)/CRTY                                                STRMLN
      Y2 = FLOAT(IY2)/CRTY                                                STRMLN
      X3 = IS                                                             STRMLN
      X4 = IEND                                                           STRMLN
      Y3 = JS                                                             STRMLN
      Y4 = JEND                                                           STRMLN
      GO TO  55                                                           STRMLN
C                                                                         STRMLN
   20 ITYPE = 1                                                           STRMLN
      X1 = XLT                                                            STRMLN
      X2 = (XLT+SIDE)                                                     STRMLN
      Y1 = YBT                                                            STRMLN
      Y2 = (YBT+SIDE)                                                     STRMLN
      X3 = IS                                                             STRMLN
      X4 = IEND                                                           STRMLN
      Y3 = JS                                                             STRMLN
      Y4 = JEND                                                           STRMLN
      IF (AMIN1(XNX,XNY)/AMAX1(XNX,XNY).LT.EXT) GO TO  50                 STRMLN
      IF (XNX-XNY)  30, 50, 40                                            STRMLN
   30 X2 = (SIDE*(XNX/XNY) + XLT)                                         STRMLN
      GO TO  50                                                           STRMLN
   40 Y2 = (SIDE*(XNY/XNX) + YBT)                                         STRMLN
   50 CONTINUE                                                            STRMLN
C                                                                         STRMLN
C CENTER THE PLOT                                                         STRMLN
C                                                                         STRMLN
      DX = 0.25*( 1. - (X2-X1) )                                          STRMLN
      DY = 0.25*( 1. - (Y2-Y1) )                                          STRMLN
      X1 = (XLT+DX)                                                       STRMLN
      X2 = (X2+DX )                                                       STRMLN
      Y1 = (YBT+DY)                                                       STRMLN
      Y2 = (Y2+DY )                                                       STRMLN
C                                                                         STRMLN
   55 CONTINUE                                                            STRMLN
C                                                                         STRMLN
      CALL SET (X1,X2,Y1,Y2,X3,X4,Y3,Y4,ITYPE)                            STRMLN
      IF (NSET.EQ.0) CALL PERIM (1,0,1,0)                                 STRMLN
C                                                                         STRMLN
   60 CONTINUE                                                            STRMLN
C                                                                         STRMLN
C DRAW THE STREAMLINES                                                    STRMLN
C .   BREAK THE WORK ARRAY INTO TWO PARTS.  SEE DRWSTR FOR FURTHER        STRMLN
C .   COMMENTS ON THIS.                                                   STRMLN
C                                                                         STRMLN
      CALL DRWSTR (U,V,WORK(1),WORK(IMAX*JPTSY+1),IMAX,JPTSY)             STRMLN
C                                                                         STRMLN
      RETURN                                                              STRMLN
      END                                                                 STRMLN
