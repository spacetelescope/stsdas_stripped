# Create the explanation page text for NICMOS paper product 

procedure npp_expl (fd)

int	fd		# output file pointer

real	x1, x2, x3, x4, x5, x6, x7
real	large, medium, small
real	yoff

begin

        # initialize the page
        call fprintf (fd, 
                "reset; fontset hard; vpage 0.0 1 0.05 0.98; expand 1.\n")
        call fprintf (fd, "location 0 1 0 0.97\n")

        call fprintf (fd, "limits 1 80 40 1\n")

	x1 = 1.
	x2 = 3.
	x3 = 5.
	x4 = 10.
	x5 = 41.
	x6 = 43.
	x7 = 60.

	large = 1.5
	medium = 1.0
	small = 0.8

	yoff = 3.5

	# draw a dividing line
	call pp_move (fd, 40., 3.)
	call fprintf (fd, "draw 40 40\n")

        # Print the text
        call fprintf (fd, "expand %0.2f\n")
	    call pargr (large)
        call pp_move (fd, x1, yoff)
        call fprintf (fd, "label '%sfBDescription of Visit Summaries'\n")
            call pargstr ("\\")
	yoff = yoff + 3

        call fprintf (fd, "expand %0.2f\n")
	    call pargr (medium)
        call pp_move (fd, x1, yoff)
        call fprintf (fd, "label '%sfBTarget List'\n")
            call pargstr ("\\")
	yoff = yoff + 2

        call fprintf (fd, "expand %0.2f\n")
	    call pargr (small)
	call pp_label (fd, x2, yoff, 
	  "The Target List contains the target name, the coordinates for the") 
	call pp_label (fd, x2, yoff+1, 
	  "target as calculated by the ground system based on the target") 
	call pp_label (fd, x2, yoff+2, 
	  "information taken from the proposal, and the text description of") 
	call pp_label (fd, x2, yoff+3, 
	  "the target given in the proposal.  Note that the coordinates") 
	call pp_label (fd, x2, yoff+4, 
	  "listed represent the predicted position of the target in the sky") 
	call pp_label (fd, x2, yoff+5, 
	  "and do not give the pointing of HST at the time of the observation.")
	yoff = yoff + 7

        call fprintf (fd, "expand %0.2f\n")
	    call pargr (medium)
        call pp_move (fd, x1, yoff)
        call fprintf (fd, 
		"label '%sfBObservation List with Data Quality Flags'\n")
            call pargstr ("\\")
	yoff = yoff + 2

        call fprintf (fd, "expand %0.2f\n")
	    call pargr (small)
	call pp_label (fd, x2, yoff, 
	  "The Observation List contains information that uniquely identifies")
	call pp_label (fd, x2, yoff+1, 
	  "individual exposures as specified in the observing proposal.")
	call pp_label (fd, x2, yoff+2, 
	  "Additionally, the status of the spacecraft and ground-system")
	call pp_label (fd, x2, yoff+3, 
	  "performance during the execution of the observation are summarized")
	call pp_label (fd, x2, yoff+4, 
	  "by the Procedural Quality Flags:")

	call pp_label (fd, x3, yoff+5, "OBS")
	call pp_label (fd, x4, yoff+5, "Status of the performance of HST.")
	call pp_label (fd, x3, yoff+6, "PROC")	
	call pp_label (fd, x4, yoff+6, 
	  "Status of the pipeline processing of the observations.")
	call pp_label (fd, x3, yoff+7, "CAL")	
	call pp_label (fd, x4, yoff+7, 
	  "Status of the reference data used in calibration.")

	call pp_label (fd, x2, yoff+8,
	  "Symbols used to indicate the status of the Procedural Quality are:")
	call pp_move (fd, x3+1, yoff+9)
	call fprintf (fd, "ptype 25 0\n")
	call fprintf (fd, "dot\n")
	call pp_label (fd, x4, yoff+9, "OK.")
	call pp_move (fd, x3+1, yoff+10)
	call fprintf (fd, "ptype 25 3\n")
	call fprintf (fd, "dot\n")
	call pp_label (fd, x4, yoff+10,
	  "Not OK-Refer to the Data Quality Summary for details.")
	call pp_label (fd, x3, yoff+11, "Blank")
	call pp_label (fd, x4, yoff+11, "Status unknown.")
	yoff = yoff + 13

        call fprintf (fd, "expand %0.2f\n")
	    call pargr (medium)
        call pp_move (fd, x1, yoff)
        call fprintf (fd, 
		"label '%sfBObservation List-Optional Parameters'\n")
            call pargstr ("\\")
	yoff = yoff + 2

        call fprintf (fd, "expand %0.2f\n")
	    call pargr (small)
	call pp_label (fd, x2, yoff, 
	  "The Observation List contains additional instrument configuration")
	call pp_label (fd, x2, yoff+1, 
	  "information. Entries in the table reflect the values of the")
	call pp_label (fd, x2, yoff+2, 
	  "Optional Parameters specified in the observing proposal.")
	yoff = yoff + 4

        call fprintf (fd, "expand %0.2f\n")
	    call pargr (medium)
        call pp_move (fd, x1, yoff)
        call fprintf (fd, 
		"label '%sfBObserving Strategy'\n")
            call pargstr ("\\")
	yoff = yoff + 2

        call fprintf (fd, "expand %0.2f\n")
	    call pargr (small)
	call pp_label (fd, x2, yoff, 
	  "This Observing Pattern Strategy summarizes the optional")
	call pp_label (fd, x2, yoff+1, 
	  "parameters used to specify the dithering/chopping pattern.")

	yoff = 3.5

        call fprintf (fd, "expand %0.2f\n")
	    call pargr (large)
        call pp_move (fd, x5, yoff)
        call fprintf (fd, "label '%sfBDescription of Exposure Summaries'\n")
            call pargstr ("\\")
	yoff = yoff + 3

        call fprintf (fd, "expand %0.2f\n")
	    call pargr (medium)
        call pp_move (fd, x5, yoff)
        call fprintf (fd, "label '%sfBPlots for Each Exposure'\n")
            call pargstr ("\\")
	yoff = yoff + 2

        call fprintf (fd, "expand %0.2f\n")
	    call pargr (small)

	call npp_expl2 (fd, x6, yoff)
	yoff = yoff + 12

        call fprintf (fd, "expand %0.2f\n")
	    call pargr (medium)
        call pp_move (fd, x5, yoff)
        call fprintf (fd, "label '%sfBData Quality Summary for Each Exposure'\n")
            call pargstr ("\\")
	yoff = yoff + 2

        call fprintf (fd, "expand %0.2f\n")
	    call pargr (small)
	call pp_label (fd, x6, yoff, 
          "The Data Quality Summary contains details of problems flagged by")
	call pp_label (fd, x6, yoff+1, 
	  "the Data Quality flags. More information can be found in the PDQ")
	call pp_label (fd, x6, yoff+2, 
	  "and TRL files, if problems are flagged. Exposure information taken")
	call pp_label (fd, x6, yoff+3, 
	  "from the headers of the data files is also provided.")
	yoff = yoff + 5

        call fprintf (fd, "expand %0.2f\n")
	    call pargr (medium)
        call pp_move (fd, x5, yoff)
        call fprintf (fd, "label '%sfBCalibration Status Summary for Each Exposure'\n")
            call pargstr ("\\")
	yoff = yoff + 2

        call fprintf (fd, "expand %0.2f\n")
	    call pargr (small)
	call pp_label (fd, x6, yoff, 
	  "The calibration summary gives detailed information about the")
	call pp_label (fd, x6, yoff+1, 
	  "calibration of the observations. Individual calibration steps are")
	call pp_label (fd, x6, yoff+2, 
	  "listed with completion status.  Reference files used are listed by")
	call pp_label (fd, x6, yoff+3, 
	  "name and information about the pedigree of the calibration data is")
	call pp_label (fd, x6, yoff+4, 
	  "provided.")
	yoff = yoff + 6

	yoff = 36.
        call fprintf (fd, "justify 5\n")
        call fprintf (fd, "expand %0.2f\n")
	    call pargr (medium)
        call pp_move (fd, x7, yoff)
        call fprintf (fd, "label '%sfBNeed Help?'\n")
            call pargstr ("\\")
	yoff = yoff + 2

        call fprintf (fd, "expand %0.2f\n")
	    call pargr (small)
	call pp_label (fd, x7, yoff, "Send e-mail to your contact scientist or")
	call pp_label (fd, x7, yoff+1, "help@stsci.edu")

	# draw a box
        call pp_move (fd, x7-12., yoff-3.)
	call fprintf (fd, "draw %0.2f %0.2f\n")
	    call pargr (x7+12.)
	    call pargr (yoff-3.)
	call fprintf (fd, "draw %0.2f %0.2f\n")
	    call pargr (x7+12.)
	    call pargr (yoff+2.)
	call fprintf (fd, "draw %0.2f %0.2f\n")
	    call pargr (x7-12.)
	    call pargr (yoff+2.)
	call fprintf (fd, "draw %0.2f %0.2f\n")
	    call pargr (x7-12.)
	    call pargr (yoff-3.)
end

procedure npp_expl2 (fd, x6, yoff)

int	fd		# output file pointer
real	x6, yoff

begin
	call pp_label (fd, x6, yoff, 
          "Plots are created for each exposure. Grey-scale plots are produced")
	call pp_label (fd, x6, yoff+1, 
          "as appropriate for the instrument configuration and observing mode")
	call pp_label (fd, x6, yoff+2, 
          "for each exposure. Exposure information taken from the headers of")
	call pp_label (fd, x6, yoff+3, 
          "the data files is also provided.  For associations using a chopping")
	call pp_label (fd, x6, yoff+4, 
	  "or dithering-chopping patterns, the mosaicked averaged background")
	call pp_label (fd, x6, yoff+5, 
	  "image is also plotted.  In the case of associations with several")
	call pp_label (fd, x6, yoff+6, 
	  "positions or repeated observations in a single position, each") 
	call pp_label (fd, x6, yoff+7, 
	  "individual observation is plotted in a stamp-size format.  The") 
	call pp_label (fd, x6, yoff+8, 
	  "position in the pattern will be indicated at the left top of each") 
	call pp_label (fd, x6, yoff+9, 
	  "plot.  The number of repeated plots per position indicates the") 
	call pp_label (fd, x6, yoff+10, 
	  "requested number of iterations per pointing.")
end
