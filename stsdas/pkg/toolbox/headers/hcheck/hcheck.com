# HCHECK.COM -- Common block used to pass information to vheader and fheader

pointer	imc			# Image descriptor
char	keyc[SZ_KEYWORD]	# Keyword name (used to print missing keywords)

common /check/  imc, keyc

