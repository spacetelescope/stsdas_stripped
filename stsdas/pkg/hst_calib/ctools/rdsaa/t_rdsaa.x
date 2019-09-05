#---------------------------------------------------------------------------
.help t_rdsaa Jun92 tools
.in
NAME
t_rdsaa -- Convert the PDB SAA Vertex Description file to a table.
.endhelp
#---------------------------------------------------------------------------

procedure t_rdsaa

# Declarations
include "rdsaa_params.com"

# Function prototypes.
int     clgeti()

begin

        call clgstr( "input", input, SZ_LINE )
        call clgstr( "output", output, SZ_LINE )
        model = clgeti( "model" )

        call saa_rdsaa
        
end
#---------------------------------------------------------------------------
# End of t_rdsaa
#---------------------------------------------------------------------------
