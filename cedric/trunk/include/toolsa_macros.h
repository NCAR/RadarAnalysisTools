/* *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* */
/* ** Copyright UCAR (c) 1992 - 2001 */
/* ** University Corporation for Atmospheric Research(UCAR) */
/* ** National Center for Atmospheric Research(NCAR) */
/* ** Research Applications Program(RAP) */
/* ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA */
/* ** 2001/11/19 23:15:6 */
/* *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* */
/*
 * toolsa_macros.h
 */

#ifndef toolsa_macros_h
#define toolsa_macros_h

#ifdef __cplusplus
extern "C" {
#endif

#include <math.h>

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef M_PI

#define M_E		2.7182818284590452354
#define M_LOG2E		1.4426950408889634074
#define M_LOG10E	0.43429448190325182765
#define M_LN2		0.69314718055994530942
#define M_LN10		2.30258509299404568402
#define M_PI		3.14159265358979323846
#define M_PI_2		1.57079632679489661923
#define M_PI_4		0.78539816339744830962
#define M_1_PI		0.31830988618379067154
#define M_2_PI		0.63661977236758134308
#define M_2_SQRTPI	1.12837916709551257390
#define M_SQRT2		1.41421356237309504880
#define M_SQRT1_2	0.70710678118654752440

#endif 

#ifndef PI
#define PI 3.14159265358979323846
#endif

#ifndef TWO_PI
#define TWO_PI 6.283185308
#endif

#ifndef EARTH_RADIUS
#define EARTH_RADIUS 6371.204	/* kilometers */
#endif

/*
 * Temperature conversions
 */

#ifndef TEMP_F_TO_C
#define TEMP_F_TO_C(f)  (.55555556 * (f - 32.0))
#endif

#ifndef TEMP_C_TO_F
#define TEMP_C_TO_F(c)   (.5555556 * c + 32.0)
#endif

#ifndef TEMP_C_TO_K
#define TEMP_C_TO_K(c)   (c + 273.15)
#endif

#ifndef TEMP_K_TO_C
#define TEMP_K_TO_C(k)   (k - 273.15)
#endif

/*
 * Angle conversions
 */

#ifndef RAD_TO_DEG
#define RAD_TO_DEG 57.29577951308092
#endif

#ifndef DEG_PER_RAD
#define DEG_PER_RAD 57.29577951308092
#endif

#ifndef DEG_TO_RAD
#define DEG_TO_RAD 0.01745329251994372
#endif

#ifndef RAD_PER_DEG
#define RAD_PER_DEG 0.01745329251994372
#endif

/*
 * Length conversions
 */

#ifndef KM_PER_NM
#define KM_PER_NM 1.85196
#endif

#ifndef KM_PER_DEG_AT_EQ
#define KM_PER_DEG_AT_EQ 111.198487
#endif

#ifndef DEG_PER_KM_AT_EQ
#define DEG_PER_KM_AT_EQ 0.008992928
#endif

#ifndef KM_PER_MI
#define KM_PER_MI   1.609344         /* Kilometers per mile */
#endif

#ifndef FT_PER_MI
#define FT_PER_MI   5280.0           /* Feet per mile */
#endif

#ifndef INCHES_TO_MM
#define INCHES_TO_MM     25.4
#endif

#ifndef FEET_TO_MM
#define FEET_TO_MM       304.8
#endif


/*
 * Speed conversions
 */

#ifndef MPERSEC_TO_KMPERHOUR
#define MPERSEC_TO_KMPERHOUR   3.6     /* m/s to km/h conversion */
#endif

#ifndef NMH_PER_MS
#define NMH_PER_MS   1.9438445   /* Nautical Miles per hour per meters/sec */
#endif

#ifndef KNOTS_TO_MS
#define KNOTS_TO_MS    0.51444 /* ((1000*1.852)/(60*60)) (nmile/hr) to (m/s) */
#endif

#ifndef MS_TO_KNOTS
#define MS_TO_KNOTS    1.94384 /* ((60*60)/(1000*1.852)) (m/s) to (nmile/hr) */
#endif

/*
 * Extreme values
 */

#ifndef LARGE_DOUBLE
#define LARGE_DOUBLE 1.0e99
#endif

#ifndef SMALL_DOUBLE
#define SMALL_DOUBLE 1.0e-99
#endif

#ifndef LARGE_LONG
#define LARGE_LONG 2000000000L
#endif

/*
 * MAX and MIN macros
 */
   
#ifndef MIN
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#endif

#ifndef MAX
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#endif

#ifdef __cplusplus
}
#endif

#endif
