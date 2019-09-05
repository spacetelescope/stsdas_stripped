import iraf

no = iraf.no
yes = iraf.yes

from axe import axesrc
#import axesrc

# Point to default parameter file for task
_parfile = 'axe$af2pet.par'
_taskname = 'af2pet'

######
# Set up Python IRAF interface here
######
def af2pet_iraf(grism,
                config,
                back,
                in_af,
                out_pet):

    # properly format the strings
    grism   = axesrc.straighten_string(grism)
    config  = axesrc.straighten_string(config)
    in_af   = axesrc.straighten_string(in_af)
    out_pet = axesrc.straighten_string(out_pet)

    # transform the IF booleans to python
    if back == iraf.yes:
        back = True
    else:
        back = False

    # check whether there is something to start
    if grism != None and config != None:
        axesrc.af2pet(grism=grism,
                      config=config,
                      back=back,
                      in_af=in_af,
                      out_pet=out_pet)

    else:
        # print the help
        iraf.help(_taskname)

# Initialize IRAF Task definition now...
parfile = iraf.osfn(_parfile)
a = iraf.IrafTaskFactory(taskname=_taskname,value=parfile,
            pkgname=PkgName, pkgbinary=PkgBinary, function=af2pet_iraf)
