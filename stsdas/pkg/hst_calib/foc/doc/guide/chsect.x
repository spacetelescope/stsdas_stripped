include	<ctype.h>

task chsect 

procedure chsect()

char	input[SZ_FNAME]
char	output[SZ_FNAME]
char	sect[SZ_LINE]
char	guide[SZ_LINE]
#--

char	line[SZ_LINE], tline[SZ_LINE]
pointer infd, outfd, open()
int	getline(), linect, ip, junk, putline(), i

begin

	call clgstr( "input", input, SZ_FNAME)
	call clgstr( "sect", sect, SZ_LINE)
	call clgstr( "guide", guide, SZ_LINE)
	call clgstr( "output", output, SZ_FNAME)
	
	infd  = open( input, READ_ONLY, TEXT_FILE)
	outfd = open( output, NEW_FILE, TEXT_FILE)

	linect   = 0
	while ( getline( infd, line) != EOF ) {
	    linect = linect + 1
	    if ( linect == 1 ) {
		ip = 0
		do i = 1, 3 {
		    ip = ip + 1
		    for ( ip = ip; !IS_WHITE(line[ip])  && line[ip] != EOS; 
								ip= ip+1){
		    }
		}
		line[ip] = EOS
		call strcpy( " \"", tline, SZ_LINE)
		call strcat( guide, tline, SZ_LINE)
		call strcat( " ",   tline, SZ_LINE)
		call strcat( sect,  tline, SZ_LINE)
		call strcat( "\"\n",tline, SZ_LINE)
		call strcat( tline, line, SZ_LINE)
	    }
	    junk = putline( outfd, line)
	}

	call close( infd)
	call close( outfd)
end
