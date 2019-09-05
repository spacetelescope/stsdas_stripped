if ( deftask("tables")) {
    if ( !defpac( "tables" )) {
tables motd- 
;
    }
}
;
# {LOAD.CL - LOAD SOME OF THE STSDAS TASKS

# Load some of the package so the user can access them without loading.
# Avoid printing menu, but do not change the
# default value of the menus switch.
      fitsio
      analysis
      toolbox
      graphics
      sdisplay
      stplot 
      toolbox
      imgtools
      convfile
      tools
      headers
      ttools
keep
