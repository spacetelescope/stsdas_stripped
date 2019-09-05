#---------------------------------------------------------------------------
.help ma_plot_data
.ih
NAME
ma_plot_data -- Plot data
.endhelp
#---------------------------------------------------------------------------
procedure ma_plot_data (gp, input_name, in_data, len)

pointer gp                      # I:  Graphics descriptor.
char    input_name[ARB]         # I:  Input file name.
real    in_data[len]            # I:  Input data.
int     len                     # I:  Length of input data.

real    r                       # Generic.

begin
        r = len
        call gploto (gp, in_data, len, 1., r, input_name)
end
#---------------------------------------------------------------------------
# End of ma_plot_data
#---------------------------------------------------------------------------
