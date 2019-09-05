# get the pedigree info

include	"jpp.h"

procedure t_jpedigree ()

char	input[SZ_FNAME]
char	pedigree[SZ_LINE, NREF]

begin
	# read input file name and output igi script file name
	call clgstr ("input", input, SZ_LINE)

	# get the pedigree info
	call jpp_pedigree (input, pedigree)

	call clpstr ("bpixtab",  pedigree[1,BPIXTAB])
	call clpstr ("ccdtab",   pedigree[1,CCDTAB])
	call clpstr ("atodtab",  pedigree[1,ATODTAB])
	call clpstr ("oscntab",  pedigree[1,OSCNTAB])
	call clpstr ("biasfile", pedigree[1,BIASFILE])

	call clpstr ("crrejtab", pedigree[1,CRREJTAB])
	call clpstr ("shadfile", pedigree[1,SHADFILE])
	call clpstr ("darkfile", pedigree[1,DARKFILE])
	call clpstr ("pfltfile", pedigree[1,PFLTFILE])
	call clpstr ("dfltfile", pedigree[1,DFLTFILE])
	call clpstr ("lfltfile", pedigree[1,LFLTFILE])

	call clpstr ("phottab",  pedigree[1,PHOTTAB])
	call clpstr ("idctab",   pedigree[1,IDCTAB])
	call clpstr ("dithtab",  pedigree[1,DITHTAB])
	call clpstr ("mlintab",  pedigree[1,MLINTAB])

end
