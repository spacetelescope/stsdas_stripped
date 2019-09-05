# get the pedigree info

include	"opp.h"

procedure t_opedigree ()

char	input[SZ_FNAME]
char	pedigree[SZ_LINE, NREF]

begin
	# read input file name and output igi script file name
	call clgstr ("input", input, SZ_LINE)

	# get the pedigree info
	call opp_pedigree (input, pedigree)

	call clpstr ("bpixtab",  pedigree[1,BPIXTAB])
	call clpstr ("mofftab",  pedigree[1,MOFFTAB])
	call clpstr ("mlintab",  pedigree[1,MLINTAB])
	call clpstr ("atodtab",  pedigree[1,ATODTAB])
	call clpstr ("ccdtab",   pedigree[1,CCDTAB])

	call clpstr ("biasfile", pedigree[1,BIASFILE])
	call clpstr ("crrejtab", pedigree[1,CRREJTAB])
	call clpstr ("darkfile", pedigree[1,DARKFILE])
	call clpstr ("apdestab", pedigree[1,APDESTAB])
	call clpstr ("pfltfile", pedigree[1,PFLTFILE])
	call clpstr ("dfltfile", pedigree[1,DFLTFILE])
	call clpstr ("lfltfile", pedigree[1,LFLTFILE])

	call clpstr ("shadfile", pedigree[1,SHADFILE])
	call clpstr ("phottab",  pedigree[1,PHOTTAB])
	call clpstr ("idctab",   pedigree[1,IDCTAB])
	call clpstr ("sdctab",   pedigree[1,SDCTAB])
	call clpstr ("xtractab", pedigree[1,XTRACTAB])
	call clpstr ("sptrctab", pedigree[1,SPTRCTAB])
	call clpstr ("lamptab",  pedigree[1,LAMPTAB])

	call clpstr ("disptab",  pedigree[1,DISPTAB])
	call clpstr ("inangtab", pedigree[1,INANGTAB])
	call clpstr ("apertab",  pedigree[1,APERTAB])
	call clpstr ("sdstfile", pedigree[1,SDSTFILE])
	call clpstr ("pctab", 	 pedigree[1,PCTAB])
	call clpstr ("wbiafile", pedigree[1,WBIAFILE])
end
