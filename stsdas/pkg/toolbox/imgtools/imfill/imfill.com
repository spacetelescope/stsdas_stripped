define	SZ_VAR		32

# IMFILL.COM -- Common block used by get_mskline

pointer	mask_line		# Pointer to mask line
int	mask_len		# Length of mask line
char	mask_var[SZ_VAR]	# Variable name used in expression

common	/getmsk/	mask_line, mask_len, mask_var
