        SUBROUTINE YCONFG(ISTAT)
*
*  Module number:
*
*  Module name: YCONFG
*
*  Keyphrase:
*  ----------
*       Read header configuration keywords
*
*  Description:
*  ------------
*       This routine reads the configuration keywords from the
*       input .d0h file and places the values in the confiquration
*       common block.
*       It also performs some checking as to their validity
*
*  FORTRAN name: yconfg.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       <rootname>.d0h          I       FOS data file
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput
*  SDAS:
*       uhdgs*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Jul 89  D. Lindler      Designed and coded
*	1.1	May 91	S. Hulbert	Also get deadtime and packet format code
*	2	Mar 94	H. Bushouse	Moved calculation of NOBJ,NBCK,NSKY
*					from YOSIZE to YCONFG.
*       2.1     Mar 97  M. De La Pena   Added KYDEPLOY to CONFIG1
*       2.2     Feb 98  M. De La Pena   Modified warning when KYDEPLOY missing.
*       2.3     Jun 98  M. De La Pena   Removed unnecessary check of KYDPLY.
*-------------------------------------------------------------------------------
        INTEGER ISTAT
C                                    --->ERROR status
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
        INTEGER ISTATS(23),ID,I,N,NDATA
        CHARACTER*80 CONTXT
        CHARACTER*8 NAMES(23)
        DATA NAMES/'DETECTOR','APER_ID','POLAR_ID','FGWA_ID','FCHNL',
     *          'NCHNLS','OVERSCAN','NXSTEPS','INTS','YBASE',
     *          'YRANGE','SLICES','NPAT','NREAD','NMCLEARS',
     *          'YSTEP1','YSTEP2','YSTEP3','LIVETIME','YSTEPS',
     *        	'DEADTIME','PKTFMT','KYDEPLOY'/
C----------------------------------------------------------------------
C
C GET values from .d0h header
C
        ID=IDS(1)
        CALL UHDGST(ID,NAMES(1),DET,ISTATS(1))
        CALL UHDGST(ID,NAMES(2),APERID,ISTATS(2))
        CALL UHDGST(ID,NAMES(3),POLID,ISTATS(3))
        CALL UHDGST(ID,NAMES(4),FGWAID,ISTATS(4))
        CALL UHDGSI(ID,NAMES(5),FCHNL,ISTATS(5))
        CALL UHDGSI(ID,NAMES(6),NCHNLS,ISTATS(6))
        CALL UHDGSI(ID,NAMES(7),OVERSN,ISTATS(7))
        CALL UHDGSI(ID,NAMES(8),XSTEPS,ISTATS(8))
        CALL UHDGSI(ID,NAMES(9),INTS,ISTATS(9))
        CALL UHDGSI(ID,NAMES(10),YBASE,ISTATS(10))
        CALL UHDGSI(ID,NAMES(11),YRANGE,ISTATS(11))
        CALL UHDGSI(ID,NAMES(12),SLICES,ISTATS(12))
        CALL UHDGSI(ID,NAMES(13),NPAT,ISTATS(13))
        CALL UHDGSI(ID,NAMES(14),NREAD,ISTATS(14))
        CALL UHDGSI(ID,NAMES(15),NCLEAR,ISTATS(15))
        CALL UHDGST(ID,NAMES(16),YTYPE(1),ISTATS(16))
        CALL UHDGST(ID,NAMES(17),YTYPE(2),ISTATS(17))
        CALL UHDGST(ID,NAMES(18),YTYPE(3),ISTATS(18))
        CALL UHDGSI(ID,NAMES(19),LIVETM,ISTATS(19))
        CALL UHDGSI(ID,NAMES(20),YSTEPS,ISTATS(20))
        CALL UHDGSI(ID,NAMES(21),DEADTM,ISTATS(21))
        CALL UHDGSI(ID,NAMES(22),PKTFMT,ISTATS(22))
        CALL UHDGSB(ID,NAMES(23),KYDPLY,ISTATS(23))
        DO 100 I=1,22
            IF(ISTATS(I).NE.0)THEN
                CONTXT='ERROR reading keyword '//NAMES(I)//
     *                  ' from .d0h file'
                GO TO 999
            ENDIF
100     CONTINUE
C
C check on the KYDEPLOY keyword
C
        IF(ISTATS(23).NE.0)THEN
            CONTXT='WARNING: KYDEPLOY not available from .d0h file.'
            CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
            CONTXT='Using the default of KYDEPLOY = F.'
            CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
            KYDPLY = .FALSE.
        ENDIF
C
C check validity
C
        IF(YBASE.GT.32767)YBASE=YBASE-65536
        IF((FCHNL.LT.0).OR.(FCHNL.GT.510))THEN
                CONTXT='ERROR: invalid FCHNL in .d0h header'
                GO TO 999
        ENDIF
        IF((NCHNLS.LT.1).OR.(NCHNLS.GT.512))THEN
                CONTXT='ERROR: invalid NCHNLS in .d0h header'
                GO TO 999
        ENDIF
        IF(OVERSN.LT.1)THEN
                CONTXT='ERROR: invalid OVERSCAN in .d0h header'
                GO TO 999
        ENDIF
        IF((XSTEPS.LT.1).OR.(XSTEPS.GT.16))THEN
                CONTXT='ERROR: invalid NXSTEPS in .d0h header'
                GO TO 999
        ENDIF
        IF(INTS.LT.1)THEN
                CONTXT='ERROR: invalid INTS in .d0h header'
                GO TO 999
        ENDIF
        IF(SLICES.LT.1)THEN
                CONTXT='ERROR: invalid SLICES in .d0h header'
                GO TO 999
        ENDIF
        IF(NPAT.LT.1)THEN
                CONTXT='ERROR: invalid NPAT in .d0h header'
                GO TO 999
        ENDIF
        IF(NREAD.LT.1)THEN
                CONTXT='ERROR: invalid NREAD in .d0h header'
                GO TO 999
        ENDIF
        IF(NCLEAR.LT.1)THEN
                CONTXT='ERROR: invalid NMCLEARS in .d0h header'
                GO TO 999
        ENDIF
        IF((YSTEPS.LT.1).OR.(YSTEPS.GT.256))THEN
                CONTXT='ERROR: invalid YSTEPS in .d0h header'
                GO TO 999
        ENDIF
        IF(LIVETM.LE.0)THEN
                CONTXT='ERROR: invalid LIVETIME in .d0h header'
                GO TO 999
        ENDIF
        IF(DEADTM.LE.0)THEN
                CONTXT='ERROR: invalid DEADTIME in .d0h header'
                GO TO 999
        ENDIF
        IF(PKTFMT.LE.0)THEN
                CONTXT='ERROR: invalid PKTFMT in .d0h header'
                GO TO 999
        ENDIF
C
C Check file sizes
C
        NDATA = (NCHNLS + OVERSN-1)*XSTEPS * SLICES*YSTEPS
        N = NAXIS1(1)
        IF(NAXIS(1).EQ.2) N = N*NAXIS2(1)
        IF(N.NE.NDATA)THEN
                CONTXT='ERROR:d0h dimension inconsistent with '//
     *                  'NCHNLS, OVSCAN, YSTEPS, XSTEPS, SLICES'
                GO TO 999
        ENDIF
        IF(IDS(3).GT.0)THEN
            IF(NAXIS1(3).NE.(XSTEPS*OVERSN*YSTEPS*SLICES))THEN
                CONTXT='ERROR:d0h dimension inconsistent with '//
     *                  'OVERSCAN, YSTEPS, XSTEPS, SLICES'
                GO TO 999
            ENDIF
        ENDIF
C
C determine number of sky, background and object spectra
C
        NBCK=0
        NSKY=0
        NOBJ=0
        IF (YSTEPS.GT.3)THEN
                NOBJ=YSTEPS
        ELSE
                DO 10 I=1,YSTEPS
                        IF(YTYPE(I).EQ.'OBJ')NOBJ=NOBJ+1
                        IF(YTYPE(I).EQ.'SKY')NSKY=NSKY+1
                        IF(YTYPE(I).EQ.'BCK')NBCK=NBCK+1
10              CONTINUE
        ENDIF
        IF((NOBJ+NSKY+NBCK).EQ.0)THEN
            CONTXT='ERROR: No object, sky, or background data found'
            GO TO 999
	END IF
C
        ISTAT=0
        GO TO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
