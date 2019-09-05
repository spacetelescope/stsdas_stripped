C 
        SUBROUTINE STCOMPLY(GRNDMD,ISTAT)
*
*  Module number:
*
*  Module name: STCOMPLY
*
*  Keyphrase:
*  ----------
*       Examine the specified header keyword values to 
*       determine if the data should be processed by
*       POA CALFOS or STSDAS CALFOS.
*
*  Description:
*  ------------
*       This program uses the data header keyword values which are
*       stored in the confiquration common block, to check to
*       see if the data fits the POA processing criteria.  If the
*       data is of the right detector, mode, fgwa_id, or aper_id,
*       then the POA_CALFOS version of the pipeline should be run
*       on this dataset for proper processing.  In this instance,
*       generate a warning message for the user, but the processing
*       via STSDAS CALFOS will continue.  If the data does not 
*       conform to the above noted criteria, it is appropriate to
*       use STSDAS CALFOS for processing, and no warnng is issued.
*
*  FORTRAN name: stcomply.f
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
*       1.1     Feb2002   M.D. De La Pena  This routine is entirely based upon 
*                                          the POACOMPLY.F routine written by 
*                                          A.Alexov for the POA CALFOS software.
*                                          The logic is a complement to the
*                                          logic of the original routine.
*       1.2     Mar2002   M.D. De La Pena  Merged messages from yclfos.f here.
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
     *        INTS,SLICES,NPAT,NREAD,NCLEAR,LIVETM
      LOGICAL HEADER,TRAILR,DEFDDT,KYDPLY
      COMMON /CONFG1/KYDPLY,DET,FGWAID,APERID,POLID,YTYPE
      COMMON /CONFG2/FCHNL,NCHNLS,OVERSN,XSTEPS,YBASE,YRANGE,YSTEPS,
     *        INTS,SLICES,NPAT,NREAD,NCLEAR,LIVETM,HEADER,TRAILR,
     *        DEFDDT
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

C Find the datasets which really should be processed
C using POA CALFOS and generate a warning message.
C However, processing will continue with STSDAS CALFOS.
     
      ISTAT = 0

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

C Generate the warning...


         CONTXT=' '
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT='****************** WARNING ************************'
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT='For the best processing of this dataset use '
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT='poa_calfos in the STPOA package distributed by '
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT='the ST-ECF Post-Operational Archives group.  See'
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT='http://www.stecf.org/poa/pcrel/POA_CALFOS.html.'
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT='The poa_calfos works on the following FOS datasets:'
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT='DETECTOR:  BLUE'
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT='GRNDMODE:  SPECTROSCOPY, RAPID-READOUT, IMAGE'
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT='FGWA_ID:   H13, H19, H27, H40, H57, L15, L65'
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT='APER_ID:   A-1, A-2, A-3, A-4, B-1, B-2, B-3, B-4'
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT=' '
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT='Your dataset keyword values match the above:'
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT='DET='//DET//', FGWA='//FGWAID//
     *     ', APER='//APERID//', GRNDMD='//GRNDMD
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT=' '
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT='                    BEWARE! '
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT='Calibration files used by calfos and poa_calfos are'
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT='different in format and content.  It is possible that'
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT='you are using calibration files intended for'
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT='poa_calfos instead.  Please check'
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT='http://www.stecf.org/poa/FOS/fos_refguide_access.html'
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT='to ensure you are using the reference files intended'
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT='for calfos.  (Do not rely on bestref/getref for the '
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT='correct files.)'
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT='CHECKING IS IMPERATIVE FOR PROPER CALFOS PROCESSING.'
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT=' '
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT='Processing of this dataset by calfos will continue.'
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT='****************** WARNING ************************'
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         CONTXT=' '
         CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         GO TO 999

C ...otherwise this dataset is OK to process with STSDAS CALFOS.

      ELSE
         GO TO 1000

      ENDIF
C
999   CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
1000  RETURN
      END
