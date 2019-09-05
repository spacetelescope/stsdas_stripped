import iraf

no  = iraf.no
yes = iraf.yes

from axe import axesrc
#import axesrc

# Point to default parameter file for task
_parfile  = 'axe$axecore.par'
_taskname = 'axecore'

######
# Set up Python IRAF interface here
######
def axecore_iraf(inlist,
                 configs,
                 fconfigs,
                 back,
                 extrfwhm,
                 drzfwhm,
                 backfwhm,
                 orient,
                 slitless_geom,
                 exclude,
                 lambda_mark,
                 cont_model,
                 model_scale,
                 inter_type,
                 lamdbda_psf,
                 np,
                 interp,
                 niter_med,
                 niter_fit,
                 kappa,
                 smooth_length,
                 smooth_fwhm,
                 spectr,
                 adj_sens,
                 weights,
                 sampling):

    # properly format the strings
    inlist   = axesrc.straighten_string(inlist)
    configs  = axesrc.straighten_string(configs)
    fconfigs = axesrc.straighten_string(fconfigs)

    # transform the IF booleans to python
    if back == yes:
        back = True
    else:
        back = False
    if orient == yes:
        orient = True
    else:
        orient = False
    if slitless_geom == yes:
        slitless_geom = True
    else:
        slitless_geom = False
    if exclude == yes:
        exclude = True
    else:
        exclude = False
    if spectr == yes:
        spectr = True
    else:
        spectr = False
    if adj_sens == yes:
        adj_sens = True
    else:
        adj_sens = False
    if weights == yes:
        weights = True
    else:
        weights = False

    # check whether something should be done
    if inlist != None and configs != None:
        # run the main code
        axesrc.axecore(inlist,
                       configs,
                       fconfigs,
                       back,
                       extrfwhm,
                       drzfwhm,
                       backfwhm,
                       lambda_mark,
                       slitless_geom,
                       orient,
                       exclude,
                       cont_model,
                       model_scale,
                       inter_type,
                       lamdbda_psf,
                       np,
                       interp,
                       niter_med,
                       niter_fit,
                       kappa,
                       smooth_length,
                       smooth_fwhm,
                       spectr,
                       adj_sens,
                       weights,
                       sampling)
    else:
        # print the help
        iraf.help(_taskname)

parfile = iraf.osfn(_parfile)
multid = iraf.IrafTaskFactory(taskname=_taskname, value=parfile,
        pkgname=PkgName, pkgbinary=PkgBinary, function=axecore_iraf)
