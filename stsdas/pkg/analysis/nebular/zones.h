#  zones.h --	Definition of zones data structure		25-Aug-2008

#  Symbolics
define	N_ZONES		3		# Number of ionization zones
define	LOW		1		# index of low I.P. zone
define	MED		2		# index of medium I.P. zone
define	HI		3		# index of high I.P. zone

#  Structure for nebular stratification
define	Z_ATL		Memi[($1+0)]	# Atomic data object list
define	Ne_Ciii		Memr[($1+1)]	# Density for med ionization region
define	Ne_Oii		Memr[($1+2)]	# Density for low ionization region
#define	Ne_NEii		Memr[($1+3)]	# Density for low ionization region
define	Ne_NEiv		Memr[($1+4)]	# Density high low ionization region
define	Ne_Sii		Memr[($1+5)]	# Density for low ionization region
define	Ne_CLiii	Memr[($1+6)]	# Density med low ionization region
#define	Ne_ARii		Memr[($1+7)]	# Density for low ionization region
define	Ne_ARiv		Memr[($1+8)]	# Density for med ionization region
define	Te_Nii		Memr[($1+9)]	# Temperature for low ionization region
define	Te_Oii		Memr[($1+10)]	# Temperature for low ionization region
define	Te_Oiii		Memr[($1+11)]	# Temperature for med ionization region
define	Te_NEiii	Memr[($1+12)]	# Temperature for med ionization region
#define	Te_NEiv		Memr[($1+13)]	# Temperature for high ionization region
define	Te_NEv		Memr[($1+14)]	# Temperature for high ionization region
define	Te_Sii		Memr[($1+15)]	# Temperature for low ionization region
define	Te_Siii		Memr[($1+16)]	# Temperature for med ionization region
define	Te_ARiii	Memr[($1+17)]	# Temperature for med ionization region
define	Te_ARiv		Memr[($1+18)]	# Temperature for med ionization region
define	Te_ARv		Memr[($1+19)]	# Temperature for high ionization region

define	Ne_Low		Memr[($1+20)]	# Density for low ionization region
define	Ne_Med		Memr[($1+21)]	# Density for medium ionization region
define	Ne_Hi		Memr[($1+22)]	# Density for high ionization region
define	Te_Low		Memr[($1+23)]	# Temperature for low ionization region
define	Te_Med		Memr[($1+24)]	# Temp. for medium ionization region
define	Te_Hi		Memr[($1+25)]	# Temperature for high ionization region
define	LEN_ZONES	26		# size of ZONES structure

