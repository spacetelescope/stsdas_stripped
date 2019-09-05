        SUBROUTINE YOSIZE(GRNDMD,PFLAGS,DTYPE,ISTAT)
*
*  Module number:
*
*  Module name: YOSIZE
*
*  Keyphrase:
*  ----------
*       output file sizes
*
*  Description:
*  ------------
*       This routine determines the number of groups and the
*       size of the output files and initializes the BUNITS keyword.
*	Returns DTYPE which is used to tell yclwrt where to write things
*
*  FORTRAN name: yosize.for
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Jul 89  D. Lindler      Designed and coded
*	1.1	Sep 90	S. Hulbert	Added BUNITS
*	2	May 91	S. Hulbert	Fixed bug in determing output file sizes
*					Modified calling sequence for wtype
*	2.1	Sep 91	S. Hulbert	Modified number of output background
*					spectra to account for scaling of 
*					reference background
*		Jan 92	S. Hulbert	Bug fix: use tot*** instead of n***.
*	2.2	Apr 93  H. Bushouse	Declare passed arrays as (*), not (1).
*	3	Mar 94	H. Bushouse	Modify PFLAGS indexes for SCT_CORR
*	4	Mar 94	H. Bushouse	Mods to handle PEDIGREE keywords 
*					(change PFLAGS to CHAR datatype);
*					Moved calculation of NOBJ,NBCK,NSKY
*					to YCONFG routine.
*	4.1	Oct 94	H. Bushouse	Mods to handle new aper, focus, flux,
*					and time corrections.
*       4.2     Mar 97  M. De La Pena   Added KYDEPLOY to CONFG1
*       4.3     Sep 98  M. De La Pena   Removed dead statements.
*-------------------------------------------------------------------------------
*
* Inputs:
*       grndmd - ground mode
*       pflags - processing flags
*
* Outputs:
*	wtype - yclwrt flag to determine where things should be written
*       istat - error status
*
        INTEGER ISTAT
        CHARACTER * 3 DTYPE
        CHARACTER*18 GRNDMD
        CHARACTER*8 PFLAGS(*)
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C Common block containing input/output file descriptions
C
C       IDS - file id numbers
C       GCOUNT - group counts
C       NAXIS - naxis
C       NAXIS1 - first dimensions
C       NAXIS2 - second dimensions
C       FILL - Fill values
C
        INTEGER IDS(30),NAXIS(30),NAXIS1(30),NAXIS2(30),GCOUNT(30)
        REAL FILL(30)
        COMMON /IOCOM/IDS,GCOUNT,NAXIS,NAXIS1,NAXIS2,FILL
C
C Common block containing confiquration parameters
C
        CHARACTER*5 DET
        CHARACTER*3 FGWAID,APERID,YTYPE(3)
        CHARACTER*1 POLID
        INTEGER FCHNL,NCHNLS,OVERSN,NXSTEP,YBASE,YRANGE,YSTEPS,
     *          INTS,SLICES,NPAT,NREAD,NCLEAR,LVTIME
        LOGICAL HEADER,TRAILR,DEFDDT,KYDPLY
        COMMON /CONFG1/KYDPLY,DET,FGWAID,APERID,POLID,YTYPE
        COMMON /CONFG2/FCHNL,NCHNLS,OVERSN,NXSTEP,YBASE,YRANGE,YSTEPS,
     *          INTS,SLICES,NPAT,NREAD,NCLEAR,LVTIME,HEADER,TRAILR,
     *          DEFDDT
        INTEGER NX,NBCK,NOBJ,NSKY
        COMMON /CONFG3/NX,NOBJ,NSKY,NBCK
C
C brightness units for output files
C
        CHARACTER * 20 BUNITS(10)
        COMMON /BUNITS/ BUNITS
C
C local variables
C
        INTEGER LAST,I
	INTEGER TOTOBJ,TOTSKY,TOTBCK
C------------------------------------------------------------------------
C
C Determine last step of standard processing
C
        LAST=0
        DO 30 I=1,13
30              IF(PFLAGS(I).EQ.'PERFORM')LAST=I
C
C reset counts based on calibration switches
C
        IF (LAST.LT.4) THEN
            TOTOBJ = YSTEPS
            TOTSKY = 0
            TOTBCK = 0
            DTYPE = 'ALL'
        ELSEIF (LAST.LT.7) THEN
            TOTOBJ = NOBJ + NSKY
            TOTSKY = 0
            TOTBCK = NBCK
C
C 'OSO': write object and sky only
C
            DTYPE = 'OSO'
        ELSE
            TOTOBJ = NOBJ
            TOTSKY = NSKY
            TOTBCK = NBCK
            DTYPE = 'OBJ'
        ENDIF
C
C determine size of output spectra
C
        NX = (NCHNLS+OVERSN-1)*NXSTEP
C
C Determine number of groups in each output data set and
C size of each group
C
        DO 20 I=11,20
                NAXIS(I)=1
                NAXIS1(I)=NX
                NAXIS2(I)=1
20      CONTINUE
C
C For final results, only output objects
C
        DO 21 I=11,14
21                GCOUNT(I)=TOTOBJ*SLICES*GCOUNT(1)
        GCOUNT(16)=GCOUNT(1)*YSTEPS*SLICES
        GCOUNT(17)=GCOUNT(11)
        GCOUNT(18)=TOTSKY*GCOUNT(1)*SLICES
        IF(TOTBCK.EQ.0)THEN
	    IF(PFLAGS(15).EQ.'PERFORM')THEN
		GCOUNT(19)=GCOUNT(1)*YSTEPS*SLICES
	    ELSE
                GCOUNT(19)=1
	    ENDIF
        ELSE
            GCOUNT(19)=TOTBCK*GCOUNT(1)*SLICES
        ENDIF
        GCOUNT(20)=GCOUNT(11)
C
C special statistic file
C
        IF(PFLAGS(14).EQ.'PERFORM')THEN
                IF(GRNDMD.EQ.'SPECTROPOLARIMETRY')THEN
                    IF(TOTOBJ.EQ.1)THEN
                        GCOUNT(15)=14
                      ELSE
                        GCOUNT(15)=14*4
                    ENDIF
                ENDIF
                IF(GRNDMD.EQ.'TIME-RESOLVED') THEN
        	    GCOUNT(15) = 2 + SLICES*2
        	ENDIF
                IF(GRNDMD.EQ.'RAPID-READOUT')THEN
                    GCOUNT(15)=2
                    NAXIS1(15)=GCOUNT(1)
                    IF(NAXIS1(15).GT.80000)NAXIS1(15)=80000
                ENDIF
        ENDIF
C
C Mark unused files by setting GCOUNT to 0
C
        IF(PFLAGS(8).EQ.'OMIT')GCOUNT(11)=0
C                                    --->no wavelength computation
        IF(PFLAGS(13).EQ.'OMIT')GCOUNT(13)=0
C                                    --->no error propagation
        IF(PFLAGS(14).EQ.'OMIT')GCOUNT(15)=0
C                                    --->no special processing
        IF(TOTSKY.EQ.0)GCOUNT(18)=0
C                                    --->no sky spectra
        IF((TOTBCK.EQ.0) .AND. (PFLAGS(4).EQ.'OMIT')) GCOUNT(19)=0
C                                    --->no background spectra
        IF(TOTSKY.EQ.0)GCOUNT(20)=0
C                                    --->no sky subtraction
C
C initialize brightness units array
C
        BUNITS(1) = '        '
        DO 101 I = 2, 10
            BUNITS(I) = 'COUNTS  '
101        CONTINUE
        BUNITS(4) = '        '
C
        ISTAT=0
1000    RETURN
        END
