# GENTOP -- Print top portion of header for HST instruments

procedure gentop (im, ipp, root, instrument, ra, dec, date, equinox)

int       imaccf()
double    ra, dec
char      root[ARB], instrument[ARB]
char      targ1[SZ_FNAME], targc[SZ_FNAME], proposal[SZ_FNAME]
char      exposure[SZ_FNAME], ftype[SZ_FNAME], targname[SZ_FNAME]
char      date[ARB], equinox[ARB]
string    dash "________________________________________"
string    unavail "unavailable"

pointer   im
bool      ipp

begin

          call printf("%s%s\n\n")
               call pargstr (dash)
               call pargstr (dash)
          if (imaccf (im, "TARGNAME") == YES)
             call imgstr (im, "TARGNAME", targname, SZ_FNAME)
          else {
             if (imaccf(im,"TARGNAM1") == YES && imaccf(im,"TARGNAMC") == YES) {
                call imgstr (im, "TARGNAM1", targ1, SZ_FNAME)
                call imgstr (im, "TARGNAMC", targc, SZ_FNAME)
                call strcpy (targ1, targname, 18)
                call strcat (targc, targname, 30)
             } else call strcpy (unavail, targname, 11)
          }
          iferr (call imgstr(im, "PROPOSID", proposal, SZ_FNAME))
             proposal[1] = EOS
          if (imaccf (im, "PEP_EXPO") == YES)
              call imgstr (im, "PEP_EXPO", exposure, SZ_FNAME)
          else iferr(call imgstr (im, "LINENUM", exposure, SZ_FNAME))
	      exposure[1] = EOS
          iferr (call imgstr(im, "FILETYPE", ftype, SZ_FNAME))
             ftype[1] = EOS
          call printf("Rootname%20tInstrument%46tTarget Name\n")
          call printf("%s%20t%s%46t%s\n\n")
               call pargstr (root)
               call pargstr (instrument)
               call pargstr (targname)
          if (ipp) {
             call printf ("Program%17t= %c%c%c%40tObs Date%56t= %s\n")
                call pargc (root[2])
                call pargc (root[3])
                call pargc (root[4])
                call pargstr (date)
             call printf ("Observation set = %c%c%40tProposal ID     = %s\n")
                call pargc (root[5])
                call pargc (root[6])
                call pargstr (proposal)
             call printf ("Observation     = %c%c%40tExposure ID     = %s\n")
                call pargc (root[7])
                call pargc (root[8])
                call pargstr (exposure)
       
             ra = ra / 15. 

             # Print data source according to last character in IPPPSSOOT name

             switch (root[9]) {
  
             case 'M':
                  call printf (
                  "Source%17t= Merged%40tRight ascension = %11.1h\n")
                     call pargd (ra)
   
             case 'N':
                  call printf (
                  "Source%17t= Retransmitted M%40tRight ascension = %11.1h\n")
                     call pargd (ra)

             case 'O':
                  call printf (
                  "Source%17t= Retransmitted RT%40tRight ascension = %11.1h\n")
                     call pargd (ra)

             case 'P':
                  call printf (
                  "Source%17t= Retransmitted TR%40tRight ascension = %11.1h\n")
                     call pargd (ra)

             case 'R':
                  call printf (
                  "Source%17t= Real Time%40tRight ascension = %11.1h\n")
                     call pargd (ra)

             case 'T':
                  call printf (
                  "Source%17t= Tape Recorded%40tRight ascension = %11.1h\n")
                     call pargd (ra)
 
             default:
                  call printf (
                  "Source%17t= Unknown%40tRight ascension = %11.1h\n")
                     call pargd (ra)

	     }

             } else {

             call printf ("%40tObs Date       = %s\n")
                  call pargstr (date)
             call printf ("%40tProposal ID    = %s\n")
                  call pargstr (proposal)
             call printf ("%40tExposure ID     = %s\n")
                   call pargstr (exposure)
             call printf ("%40tRight ascension = %11.1h\n")
                   call pargd (ra)
             }

             call printf (
             "File Type%17t= %s%40tDeclination%56t= %9.0h\n")
                call pargstr (ftype)
                call pargd (dec)
             call printf ("%40tEquinox%56t= %s\n\n")
                  call pargstr (equinox)

end

