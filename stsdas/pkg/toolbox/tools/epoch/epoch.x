include	"epoch.h"

#  EPOCH -- Perform conversions among different time formats
#
#  Description:
#  ------------
#
#  Date		Author			Description
#  ----		------			-----------
#  01-Aug-1990  J.-C. Hsu		Design and coding
#  09-Dec-1994  J.-C. Hsu		Add AM/PM option (fracd.x)
#------------------------------------------------------------------------------

procedure epoch ()

char	instr[SZ_LINE]		# input time string
char	dmystyle[SZ_LINE]	# day month year format style
int	length			# length of the input string
double	zone			# time zone
int	informat
int	style
bool	flag1900		# add 1900 to the two digit year
bool	bcflag			# year is BC?
char	printout[SZ_LINE]	# output selections
double	mjd			# modified Julian date
#==============================================================================
begin

	# get the input string and other parameters
	call epoch_get (instr, length, informat, dmystyle, zone, style, 
			flag1900, bcflag, printout)

	# determine which class of time format the input string is
	call which_class (instr, length, informat, dmystyle, style, flag1900,
				bcflag, zone, mjd)

	# convert to output format
	call epoch_back (mjd, printout)
end
