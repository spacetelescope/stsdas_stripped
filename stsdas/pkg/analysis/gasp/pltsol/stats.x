include "pls.h"
#STATS -- Procedure to calculate simple statistical parameters from
#	  the plate solution results

procedure stats (pn, x_coeff, y_coeff, x_array, y_array, nref_stars, nterm, 
		xi_res,	eta_res, xi_sig, eta_sig, xi_mean, eta_mean)

pointer	pn
double  x_array[nref_stars,nterm]
double	 y_array[nref_stars,nterm]
double  x_coeff[nterm], y_coeff[nterm]
double  xi_res[nref_stars], eta_res[nref_stars]
double  xi_sig, eta_sig, xi_mean, eta_mean
int	nref_stars, nterm

double  x, e, xvar, evar
int	i ,j, n
pointer sp, pxi, peta
double  xim, etam, xsig, esig, dtemp

begin
	do i = 1, nref_stars {
	   dtemp = 0.0
	   do j = 1, nterm
	      dtemp = dtemp + x_coeff[j]*x_array[i,j]
	   Memd[PXIC(pn)+i-1] = dtemp
	}
	do i = 1, nref_stars {
	   dtemp = 0.0
	   do j = 1, nterm
	      dtemp = dtemp + y_coeff[j]*y_array[i,j]
	   Memd[PETAC(pn)+i-1] = dtemp
	}
	x = 0.0
	e = 0.0
	n = 0
	call asubd (XI(pn), XI_CALC(pn), xi_res, nref_stars)
	call asubd (ETA(pn), ETA_CALC(pn), eta_res, nref_stars)
	do i = 1, nref_stars {
	   if (Memr[PW(pn)+i] > 0.0) {
	      x = x + xi_res[i]
	      e = e + eta_res[i]
	      n = n + 1
	   }
	}
	xi_mean = x/n
	eta_mean = e/n

	xvar = 0.0
	evar = 0.0
	do i = 1, nref_stars {
	   if (Memr[PW(pn)+i] > 0.0) {
	      x = xi_res[i] - xi_mean
	      e = eta_res[i] - eta_mean
	      xvar = xvar + x*x
	      evar = evar + e*e
	   }	      	      
	}
	xi_sig = sqrt(xvar/(n-1))	
	eta_sig = sqrt(evar/(n-1))	
	call smark (sp)
	call salloc (pxi, nref_stars, TY_DOUBLE)
	call salloc (peta, nref_stars, TY_DOUBLE)
	call asubd (XI(pn), XI_CALC(pn), Memd[pxi], nref_stars)
	call asubd (ETA(pn), ETA_CALC(pn), Memd[peta], nref_stars)
	call aavgd (Memd[pxi], nref_stars, xim, xsig)
	call aavgd (Memd[peta], nref_stars, etam, esig)
	call sfree (sp)
end
