# get the pipeline/calibration data quality anomalies, this is a "wrapper" 
# task for opp_trlsum.

include	"opp.h"

procedure t_otrlsum ()

char	input[SZ_FNAME]
char	pipeline[SZ_LINE, MAX_SUMMARY]
char	caldq[SZ_LINE, MAX_SUMMARY]

int	i
char	tmp1[SZ_LINE], tmp2[SZ_LINE]

begin
	# read input file name
	call clgstr ("input", input, SZ_LINE)

	# get the pipeline and calibration data quality anomalies
	call opp_trlsum (input, pipeline, caldq)

	do i = 1, MAX_SUMMARY {
	    call sprintf (tmp1, SZ_LINE, "pipeline%d")
		call pargi (i)
	    call sprintf (tmp2, SZ_LINE, "caldq%d")
		call pargi (i)
	    call pp_swapchar (pipeline[1,i], '\'', '`')
	    call pp_swapchar (caldq[1,i], '\'', '`')

	    call clpstr (tmp1, pipeline[1,i])
	    call clpstr (tmp2, caldq[1,i])
	}
end
