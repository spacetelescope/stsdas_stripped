C 
        SUBROUTINE POACOMPLY(GRNDMD,ISTAT)
*
*  Module number:
*
*  Module name: POACOMPLY
*
*  Keyphrase:
*  ----------
*       Compare data header keyword values to those that the
*       POA software is able to process.
*
*  Description:
*  ------------
*       This program uses the data header keyword values which are
*       stored in the confiquration common block, to check to
*       see if the data fits the POA processing criteria.  If the
*       data is not of the right detector, mode, fgwa_is, or aper_id,
*       then the software should not be run on this dataset.
*
*  FORTRAN name: poacomply.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       <rootname>.d0h          I       FOS data file
*
*  Subroutines Called:
*  -------------------
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1.1     Feb 01   A. Alexov      Designed and coded
*-------------------------------------------------------------------------------
        INTEGER ISTAT
C                                    --->ERROR status
        CHARACTER*18 GRNDMD
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C Common block containing input file descriptions
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
        INTEGER FCHNL,NCHNLS,OVERSN,XSTEPS,YBASE,YRANGE,YSTEPS,
     *          INTS,SLICES,NPAT,NREAD,NCLEAR,LIVETM
        LOGICAL HEADER,TRAILR,DEFDDT,KYDPLY
        COMMON /CONFG1/KYDPLY,DET,FGWAID,APERID,POLID,YTYPE
        COMMON /CONFG2/FCHNL,NCHNLS,OVERSN,XSTEPS,YBASE,YRANGE,YSTEPS,
     *          INTS,SLICES,NPAT,NREAD,NCLEAR,LIVETM,HEADER,TRAILR,
     *          DEFDDT
        INTEGER NX,NBCK,NOBJ,NSKY
        COMMON /CONFG3/NX,NOBJ,NSKY,NBCK
        INTEGER DEADTM
        COMMON /CONFG5/DEADTM
        INTEGER PKTFMT
        COMMON /CONFG6/ PKTFMT
C
C Local variables
C
        CHARACTER*80 CONTXT
C----------------------------------------------------------------------
C

C check on the keywords
C

C List of datasets for which poa_calfos can run on:
C Detector:  BLUE
C
C Grndmode:  SPECTROSCOPY, RAPID-READOUT, IMAGE
C
C Fgwa_id:   H13, H19, H27, H40, H57, L15, L65    
C
C Aper_id:   A-1, A-2, A-3, A-4, B-1, B-2, B-3, B-4

C Find the datasets which it cannot run on, due to the above
C criteria, exit with an error if found.

        IF((DET.EQ.'BLUE').AND.((GRNDMD.EQ.'SPECTROSCOPY').OR.
     *  (GRNDMD.EQ.'RAPID-READOUT').OR.(GRNDMD.EQ.'IMAGE')).AND.
     *  ((FGWAID.EQ.'H13').OR.(FGWAID.EQ.'H19').OR.
     *  (FGWAID.EQ.'H27').OR.(FGWAID.EQ.'H40').OR.
     *  (FGWAID.EQ.'H57').OR.(FGWAID.EQ.'L65').OR.
     *  (FGWAID.EQ.'L15')).AND.
     *  ((APERID.EQ.'A-1').OR.(APERID.EQ.'A-2').OR.
     *  (APERID.EQ.'A-3').OR.
     *  (APERID.EQ.'A-4').OR.(APERID.EQ.'B-1').OR.
     *  (APERID.EQ.'B-2').OR.
     *  (APERID.EQ.'B-3').OR.(APERID.EQ.'B-4')))THEN

c   do nothing
           ISTAT=0
           GO TO 1000
        ELSE

           CONTXT='********************* POA **************************'
           CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
           CONTXT='ERROR:  Dataset cannot be processed by poa_calfos.'
           CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
           CONTXT='poa_calfos only works on the following FOS datasets:'
           CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
           CONTXT='DETECTOR:  BLUE'
           CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
           CONTXT='GRNDMODE:  SPECTROSCOPY, RAPID-READOUT, IMAGE'
           CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
           CONTXT='FGWA_ID:   H13, H19, H27, H40, H57, L15, L65'
           CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
           CONTXT='APER_ID:   A-1, A-2, A-3, A-4, B-1, B-2, B-3, B-4'
           CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
           CONTXT='Your dataset keyword values do not match the above:'
           CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
           CONTXT='DET='//DET//', FGWA='//FGWAID//
     *     ', APER='//APERID//', GRNDMD='//GRNDMD
           CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
           CONTXT='User stsdas.hst_cal.fos.calfos to process your data.'
           CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
           CONTXT='********************* POA **************************'
           GO TO 999

        ENDIF
C
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
