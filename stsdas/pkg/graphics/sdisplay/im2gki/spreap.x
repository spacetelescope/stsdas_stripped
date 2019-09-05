include <ctotok.h>
include <ctype.h>
include <psiescape.h>

define	NONE	0
define	RED	1
define	GREEN	2
define	BLUE	3

define  MIN_INTEN 0
define  MAX_INTEN 255

procedure sp_read_colormap( cmapname, rlut, glut, blut, lut_size )

char    cmapname[ARB]
short   rlut[lut_size]
short   glut[lut_size]
short   blut[lut_size]
int     lut_size

define  LOW_VERB        1
define  MED_VERB        2
define  HIGH_VERB       3


# Based on:
#  t_scmapc -- Parse colormap table as written by SAOimage.  Produces a
#  colormap image suitable as input to vdisplay.celco.  Optionally write
#  the colormap as integer triples to STDOUT.

#  Based on SAOimage C code, see copyright notice included
#  Z. G. Levay  28 January 1992

# * Copyright:	1989 Smithsonian Astrophysical Observatory
# *		You may do anything you like with this file except remove
# *		this copyright.  The Smithsonian Astrophysical Observatory
# *		makes no representations about the suitability of this
# *		software for any purpose.  It is provided "as is" without
# *		express or implied warranty.

pointer	sp
pointer	line, word
int	ip, jp
int	nch
int     scm
bool	pscf
real	value
int	color
real	gamma
int	tok
bool	index
pointer	inds, ints
int	id
int     i
pointer	cmap[3]

int	getline(), ctowrd(), fscan(), ctotok(), open()
bool	streq()
real	ctor(), ggamav()

begin

   scm = open( cmapname, READ_ONLY, TEXT_FILE )

   call smark (sp)
   call salloc (line, SZ_LINE, TY_CHAR)
   call salloc (word, SZ_LINE, TY_CHAR)
   
   call malloc (inds, lut_size, TY_REAL)
   call malloc (ints, lut_size, TY_REAL)
   
   call malloc (cmap[RED],   lut_size, TY_REAL)
   call malloc (cmap[GREEN], lut_size, TY_REAL)
   call malloc (cmap[BLUE],  lut_size, TY_REAL)
   
   pscf  = false
   color = NONE
   nch   = 0
   index = true
   
   #  Initialize color map to zero
   call amovkr (0.0, Memr[cmap[RED]],   lut_size)
   call amovkr (0.0, Memr[cmap[GREEN]], lut_size)
   call amovkr (0.0, Memr[cmap[BLUE]],  lut_size)
   
   while (fscan (scm) != EOF) {
      # For each line
      call gargwrd (Memc[word], SZ_LINE)
      if (streq (Memc[word], "PSEUDOCOLOR")) {
         pscf = true
         break
      }
   }
   
   if (!pscf){
      call close( scm )
      call error (0, "PSEUDOCOLOR keyword not found")
   }
   
   while (getline (scm, Memc[line]) != EOF) {
      # Scan lines for Color keyword, RED, GREEN, or BLUE
      
      ip = 1
      nch = ctowrd (Memc[line], ip, Memc[word], SZ_LINE)
      
      if (streq (Memc[word], "RED:")) {
         color = RED
         gamma = ggamav (Memc[line], ip, false)
         id = 0
         
      } else if (streq (Memc[word], "GREEN:")) {
          call inttab (Memr[inds], Memr[ints], id, Memr[cmap[color]],
                       lut_size, MIN_INTEN, MAX_INTEN, gamma, false)
         
         color = GREEN
         gamma = ggamav (Memc[line], ip, false)
         id = 0
         
      } else if (streq (Memc[word], "BLUE:")) {
          call inttab (Memr[inds], Memr[ints], id, Memr[cmap[color]],
                       lut_size, MIN_INTEN, MAX_INTEN, gamma, false)
         
         color = BLUE
         gamma = ggamav (Memc[line], ip, false)
         id = 0
         
      } else if (Memc[word] == '\#')
         next
      
      else {
         # Working on a color
         ip = 1
         repeat {	# To end of line
            tok = ctotok (Memc[line], ip, Memc[word], SZ_LINE)
            
            if (tok == TOK_NUMBER) {
               jp = 1
               nch = ctor (Memc[word], jp, value)
               
               if (index) {
                  #  An index value
                  id = id + 1
                  call fillim (Memr[inds], id, value)
                  index = false
                  
               } else {
                  #  An intensity value
                  call fillim (Memr[ints], id, value)
                  index = true
               }
               
            }
            
         } until (tok == TOK_NEWLINE)
      }
   }
   
   # Fill the last (Blue) color map line
        call inttab (Memr[inds], Memr[ints], id, Memr[cmap[color]],
                     lut_size, MIN_INTEN, MAX_INTEN, gamma, false)

   # Now convert to shorts.
   do i = 1, lut_size {
      rlut[i] = PS_PACKLUT(Memr[cmap[RED]+i-1])
      glut[i] = PS_PACKLUT(Memr[cmap[GREEN]+i-1])
      blut[i] = PS_PACKLUT(Memr[cmap[BLUE]+i-1])
   }
   
   
end

real procedure ggamav (line, ip, debug)

char	line[ARB]
int	ip
int	debug

int	nch
real	gamma
char	word[SZ_LINE]

int	ctowrd()
real	ctor()
bool	streq()

begin
	nch = ctowrd (line, ip, word, SZ_LINE)

	gamma = 1.0

	if (streq (word, "gamma")) {
	    nch = ctor (line, ip, gamma)

	    if (debug > LOW_VERB) {
		call printf ("gamma: %d %d %f\n")
		    call pargi (ip)
		    call pargi (nch)
		    call pargr (gamma)
	    }
	}

	return (gamma)
end


procedure fillim (vec, id, val)

real	vec[ARB]
int	id
real	val

begin
	vec[id] = val
end


procedure inttab (inds, ints, nnode, cmap, cmapsize, ci1, ci2, gamma, debug)

real	inds[ARB]
real	ints[ARB]
int	nnode
real	cmap[ARB]
int	cmapsize
int	ci1, ci2
real	gamma
int	debug

real	x, x1, x2, dx
real	y, y1, y2, dy
int	i, i1, i2
real	sl, b
int	node
real	di

begin
	i1 = ci1
	i2 = ci2
	di = real (i2 - i1)

	node = 1

	x1 = inds[node]
	x2 = inds[node+1]
	dx = x2 - x1

	y1 = ints[node]
	y2 = ints[node+1]
	dy = y2 - y1

	sl = dy / dx
	b  = y1 - sl * x1

	if (debug > LOW_VERB) {
	    call printf ("Node %d\n")
	    call pargi (node)

	    call printf ("X: %f %f %f\n")
		call pargr (x1)
		call pargr (x2)
		call pargr (dx)

	    call printf ("Y: %f %f %f\n")
		call pargr (y1)
		call pargr (y2)
		call pargr (dy)

	    call printf ("Slope %f Intercept %f\n")
		call pargr (sl)
		call pargr (b)
	}

	do i = i1, i2 {
	    x = real (i - i1) / di

	    if (x <= x1)
		y = y1

	    else if (x > x2) {
		if ((node + 1) == nnode)
		    y = y2

		else {
		    node = node + 1
	
		    x1 = inds[node]
		    x2 = inds[node+1]
		    dx = x2 - x1
	
		    y1 = ints[node]
		    y2 = ints[node+1]
		    dy = y2 - y1
	
		    sl = dy / dx
		    b  = y1 - sl * x1

		    if (debug > LOW_VERB) {
			call printf ("Node %d\n")
			    call pargi (node)

			call printf ("X: %f %f %f\n")
			    call pargr (x1)
			    call pargr (x2)
			    call pargr (dx)

			call printf ("Y: %f %f %f\n")
			    call pargr (y1)
			    call pargr (y2)
			    call pargr (dy)

			call printf ("Slope %f Intercept %f\n")
			    call pargr (sl)
			    call pargr (b)
		    }

		    y = x * sl + b
		}

	    } else {

		y = x * sl + b
	    }

	    y = min (max (y, 0.0), 1.0)
	    y = y ** (1.0 / gamma)

	    cmap[i+1] = y

	    if (debug >= LOW_VERB) {
		call printf ("%d  x: %f;  y: %f\n")
		    call pargi (i)
		    call pargr (x)
		    call pargr (y)
		call flush (STDOUT)
	    }
	}

end
