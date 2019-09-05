        SUBROUTINE SPOSIZ(GRNDMD,PFLAGS,DTYPE,ISTAT)
*
*  Module number:
*
*  Module name: SPOSIZ
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
*  FORTRAN name: sposiz.for
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput
*
*  History:
*  --------
*  Version      Date        Author          Description
*	1	Sep 93	H. Bushouse	Modified version of YOSIZE
*	2	Jun 94	H. Bushouse	Modify PFLAGS indexes for SCT_CORR
*					and change to CHAR data type.
*	3	Nov 94	H. Bushouse	Modify PFLAGS indexes for new flux
*					cal steps.
*       3.1     Feb 98  M. De La Pena   Added KYDPLY to CONFG1.
*       3.2     Sep 98  M. De La Pena   Removed dead statements.
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
        INTEGER LAST
	INTEGER TOTOBJ,TOTSKY,TOTBCK
C------------------------------------------------------------------------
C
C Determine last step of standard processing
C
        LAST=0
c       DO 30 I=1,13
c30              IF(PFLAGS(I).EQ.'PERFORM')LAST=I
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
c       DO 20 I=11,20
c               NAXIS(I)=1
c               NAXIS1(I)=NX
c               NAXIS2(I)=1
c20      CONTINUE
        NAXIS(15)=1
        NAXIS1(15)=NX
        NAXIS2(15)=1
C
C For final results, only output objects
C
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
	ENDIF
C
        ISTAT=0
1000    RETURN
        END
