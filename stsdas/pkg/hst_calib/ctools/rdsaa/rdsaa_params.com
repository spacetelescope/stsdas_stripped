#---------------------------------------------------------------------------
.help rdsaa_params.com Jun92 tools
.ih
NAME
rdsaa_params.com -- Global parameters for the saa_rdsaa task.
.endhelp
#---------------------------------------------------------------------------

int     model                   # Model to add to the table, all if not
                                # specified.

char    input[SZ_LINE]          # PDB SAA Vertex Description file.
char    output[SZ_LINE]         # Table version of the SAA.

common  /rdsaa_params/  model, input, output

#---------------------------------------------------------------------------
# End of rdsaa_params.com
#---------------------------------------------------------------------------
