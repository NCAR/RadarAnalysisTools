#include <stdio.h>
#include <stdlib.h>
#include <math.h>

void process_kdp_(no_gates,gate_spacing,mfl,nn,dev,
           zdat,phi_unfld,kdp,delta,phi_int,near_arr)
/* inputs :-
   no_gates = Number of gates in beam
   gate_spacing = gate spacing in metres
   mfl = Median filter length ( using mfl =9)
   nn  = Number of points for least squares analysis ( using nn = 7)
   dev = Allowed deviation for kdp consensus ( using dev = .5)
   zdat = raw reflectivity field
   phi_unfld = unfolded phidp
*/

/* outputs :-
   kdp
   delta
   phi_int = integrated phidp filed from processed kdp field
   near_arr = an array of number in consensus for each point
*/
int *no_gates, *mfl,  *nn;
float *dev, *gate_spacing;
float *zdat, *phi_unfld, *kdp, *delta, *phi_int;
int   *near_arr;
{
    int  iter, iz;
    float phidp[2000];
    float av, av1;
    int  kmed, mm, nn1, nkk, ipick[20], jpick[20],
         near, mear,n_mov;
    int  float_compare();
    float  w[20], median, 
           az[20], ph[20], zm, sumx, sumy, sumxy, sumx2, sumy2,
           delt1, ks[20], cons, consen, ks_diff;
    int    k,ii,jj;
    FILE *fd_out;


    for (k=0; k < *no_gates; k++)
     {
      phidp[k] = phi_unfld[k];
     }
    

    kmed = (*mfl -1)/2;
    mm = *no_gates - *mfl + 1;

    for (k = 0; k < mm; k++)
    {
     ii = -1;
     for (jj = k; jj < (k + *mfl); jj++)
     {
      ii++;
      w[ii] = phi_unfld[jj];
     }
     qsort(w,*mfl,sizeof(float),float_compare);
     median = w[(*mfl - 1)/2];  
     phidp[k + kmed] = median;
    }
/*  Add moving average to smooth unflod phidp; n_mov -> # of average */

    n_mov=5;
    for (k = 0; k < *no_gates-n_mov+1; k++)
    {
     ii = -1;
     sumx = 0.;
     for (jj = k; jj < (k+n_mov); jj++)
     {
      ii++;
      w[ii] = phidp[jj];
      sumx = sumx + w[ii];
     }
     qsort(w,n_mov,sizeof(float),float_compare);
     if (fabs(w[n_mov-1] - w[0]) < 30)
       phidp[k+(n_mov-1)/2] = sumx/n_mov;      
    } 
    
/*  To check the phidp after median filter */
    for (k =0 ; k < *no_gates ; k++)
    {
     phi_unfld[k] = phidp[k];
    }

     nkk = (*nn - 1)/2;
     for (k = 0; k < *no_gates; k++)
     {
         kdp[k] = 0.;
         delta[k] = 0.; 
         near_arr[k] = 0;
     }

     for (k = 0; k < *no_gates - *nn + 1; k++)
     {
         zm = 0.;
         ii = -1;
         for (jj = k; jj < (k + *nn); jj++)
         {
             ii++;
             az[ii] = zdat[jj]; 
             ph[ii] = phidp[jj]; 
             zm = zm +  zdat[jj];
         }
         zm = zm/(*nn);
         if (zm < 15.) 
         {
            kdp[k + nkk] = 0.;
            delta[k + nkk] = 0.; 
            continue;
         }
         ii = 0;
         sumx = 0.;
         sumy = 0.;
         sumxy = 0.;
         sumx2 = 0.;
         sumy2 = 0.;
         for (jj = k; jj < (k + *nn); jj++)
         {
             ii++;
             sumx = sumx + ii;
             sumy = sumy + phidp[jj];
             sumxy = sumxy + ii*phidp[jj];
             sumx2 = sumx2 + ii*ii;
             sumy2 = sumy2 + phidp[jj]*phidp[jj];
         }

         delt1 = (*nn)*sumx2 - sumx*sumx;
         kdp[k + nkk] = (sumxy*(*nn) - sumx*sumy)/(*gate_spacing * .001 * 2 * delt1);

         for (jj = 0; jj < (*nn - 1); jj++)
         {
            if ((ph[jj+1] < -900.) || (ph[jj] < -900.))
                ks[jj] = -32768.;
            else
                ks[jj] = ph[jj+1] - ph[jj];
         }
         nn1 = *nn - 1;
         qsort(ks,nn1,sizeof(float),float_compare);

         /* dev is the allowed deviation for consensus average */
         near = 0;
         cons = 0.0;
         for (ii = 0; ii < nn1; ii++) ipick[ii] = 0;
         for (ii = 0; ii < nn1; ii++)
         {
             mear = 0;
             consen = 0.0;
             for (jj = 0; jj < nn1; jj++)
             {
                 if ((ks[ii] == -32768.) || (ks[jj] == -32768.))
                     ks_diff = -32768.;
                 else
                     ks_diff = ks[ii] - ks[jj];

                 if (ks_diff < 0) ks_diff = -ks_diff;
                 if (ks_diff < *dev)
                 {
                     mear++;
                     jpick[mear-1] = jj;
                     consen = consen + ks[jj];
                 }
             }

             if (mear > near)
             {
                 near = mear;
                 for (jj = 0; jj < near; jj++) ipick[jj] = jpick[jj];
                 cons = consen;
             }
         }

         near_arr[k + nkk] = near;
         if ((near > (nn1 - 3)) && (near > 0))
         {
            cons = cons/near;
            if (near < nn1) kdp[k + nkk] = cons/(2*.001*(*gate_spacing));
         }
/*       Seems it is not reasonable to set the value to previous one
         if consensus value cannot be retrieved here.
         if ((near < (nn1 - 2)) || (near == 0)) kdp[k + nkk] = kdp[k + nkk - 1];
*/       if ((near < (nn1 - 2)) || (near == 0)) kdp[k + nkk] = 0.0;
         delta[k + nkk] = (phidp[k + nkk + 1] - phidp[k + nkk])
                          - kdp[k + nkk]*2*.001*(*gate_spacing); 
     }
/*  Integrated phidp only when phidp great than -0.5  */

     phi_int[0] = 0.0;
     for (k = 1; k < *no_gates; k++)
     {
        if (kdp[k] > -0.5)
          phi_int[k] = phi_int[k-1] + kdp[k]*2*(*gate_spacing)*.001;
	else 
	  phi_int[k] = phi_int[k-1];
     }

/* &&&&&&&&&&&&&& END OF CREATE KDP AND DELTA FIELDS &&&&&&&&&&&&&&&&&&& */
}


int float_compare(a,b)
float *a, *b;
{
     float x;
     int i;
     x = *a - *b;
     if(x < 0.)  i = -1;
     if(x == 0.) i = 0;
     if(x > 0.)  i = 1;
     return(i);
}
