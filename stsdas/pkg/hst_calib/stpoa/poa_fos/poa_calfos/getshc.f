C
C       SUBROUTINE GETSHC (IU, FSPEC, NMAX, ERAD, GH, IER)
       SUBROUTINE GETSHC (IU, FSPEC, NMAX, ERAD, GH, ISTAT, L)
c  ------------------------------------------------------------------------
c  Part of "poa_calfos" (STECF/IPMG)
c  Part of "NEWBILCAL" program to produce IGRF geomagnetic model (GSFC)
c
c  History:
c  --------
c  Version   Date        Author          Description
c      1.0       Jul 00  M. Rosa         POA calibration code
c      1.2       Jun 01  A. Alexov       Added IDS's common block for file
c                                        header access
c  ------------------------------------------------------------------------
c       15 Jul 00       reads from STSDAS tables
c
C ===============================================================
C       Version 1.01
C
C       Reads spherical harmonic coefficients from the specified
C       file into an array.
C
C       Input:
C           IU    - Logical unit number
C           FSPEC - File specification
C
C       Output:
C           NMAX  - Maximum degree and order of model
C           ERAD  - Earth's radius associated with the spherical
C                   harmonic coefficients, in the same units as
C                   elevation
C           GH    - Schmidt quasi-normal internal spherical
C                   harmonic coefficients
C           IER   - Error number: =  0, no error
C                                 = -2, records out of order
C                                 = FORTRAN run-time error number
C
C       A. Zunde
C       USGS, MS 964, Box 25046 Federal Center, Denver, CO  80225
C ===============================================================
       INTEGER  I,N,M,NN,MM,NMAX,IER,IU,L
c       REAL*4   DATE,ERAD,G,H
       REAL*4   ERAD,G,H
       REAL*4   GH(*)                        
c       CHARACTER*10 NAME
       CHARACTER FSPEC*(*)      
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
      CHARACTER*80 CONTXT
      
C
C FILE I/O ACCESS MODES
C
        INTEGER   RDONLY
        PARAMETER (RDONLY = 1)

      INTEGER   TBNROW
      PARAMETER (TBNROW = 21)
      INTEGER IDIN,COLIDS(4),I,II,JJ,ISTATS(4),ISTAT,NROWS
      CHARACTER*8 COLNAM(4)
      LOGICAL NULL
      DATA COLNAM/'NDEGREE','MORDER','GCOEFF','HCOEFF'/
C              
C ---------------------------------------------------------------
C       Open coefficient file. Read past first header record.        
C       Read degree and order of model and Earth's radius.           
C ---------------------------------------------------------------             
c       IU = 81 
c       OPEN (UNIT=IU,FILE='dgrf90.dat',ACCESS='SEQUENTIAL',
c     1       FORM='FORMATTED',STATUS='OLD',IOSTAT=IER,ERR=999)
cc 991   type *,'opened ',IU,FSPEC,IER
c 881   READ (IU,*) NAME
c 882   READ (IU,*,IOSTAT=IER, ERR=999) NMAX, ERAD, DATE                 
C ---------------------------------------------------------------
C       Read the coefficient file, arranged as follows:              
C                                                                    
C                                   N     M     G     H          
C                                   ----------------------       
C                                /   1     0    GH(1)  -          
C                               /       1     1    GH(2) GH(3)       
C                              /       2     0    GH(4)  -          
C                             /       2     1    GH(5) GH(6)       
C           NMAX*(NMAX+3)/2        /       2     2    GH(7) GH(8)       
C              records              \       3     0    GH(9)  -          
C                             \      .     .     .     .          
C                              \       .     .     .     .          
C           NMAX*(NMAX+2)          \       .     .     .     .          
C           elements in GH           \  NMAX  NMAX   .     .          
C                                                                             
C       N and M are, respectively, the degree and order of the       
C       coefficient.                                                 
C ---------------------------------------------------------------         
c 883   I = 0                                                        
c       DO 2211 NN = 1, NMAX                                              
c         DO 2233 MM = 0, NN                                            
c           READ (IU,*,IOSTAT=IER, ERR=999) N, M, G, H         
c           IF (NN .NE. N .OR. MM .NE. M) THEN                   
c             IER = -2                                         
c             GOTO 999                                         
c           ENDIF                                                
c           I = I + 1                                            
c           GH2(I) = G                                            
c           IF (M .NE. 0) THEN                                   
c             I = I + 1                                        
c             GH2(I) = H                                        
c           ENDIF                                                
c2233     CONTINUE                                                    
c2211   CONTINUE                                                    
    
c999    CLOSE (IU)                                                   
c       RETURN                                                       
c       END                                                          

C
C Open table
C
        CALL UTTOPN(FSPEC,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening dgrf table '//FSPEC
                GO TO 998
        ENDIF
C
C get NMAX and ERAD header keywords
C
        CALL UTHGTI(IDIN,'NMAX',NMAX,ISTAT)
        CALL UTPGTI(IDIN,TBNROW,NROWS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading NMAX key in dgrf table '//FSPEC
                GO TO 999
        ENDIF
        CALL UTHGTR(IDIN,'ERAD',ERAD,ISTAT)
        CALL UTPGTI(IDIN,TBNROW,NROWS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading ERAD key in dgrf table '//FSPEC
                GO TO 999
        ENDIF
C
C get number of rows
C
        CALL UTPGTI(IDIN,TBNROW,NROWS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading dgrf table '//FSPEC
                GO TO 999
        ENDIF
C
C Get column ids.
        CALL UTCFND(IDIN,COLNAM,4,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR locating needed columns in dgrf table '//
     *                  FSPEC
                GO TO 999
        ENDIF

c get the data
        I = 0 
        II = 1  
        DO 2214 NN = 1, NMAX
           DO 2235 MM = 0, NN
              CALL UTRGTI(IDIN,COLIDS(1),1,II,N,NULL,ISTATS(1))
              CALL UTRGTI(IDIN,COLIDS(2),1,II,M,NULL,ISTATS(2))
              CALL UTRGTR(IDIN,COLIDS(3),1,II,G,NULL,ISTATS(3))
              CALL UTRGTR(IDIN,COLIDS(4),1,II,H,NULL,ISTATS(4))
              DO 30 JJ=1,4
                 IF(ISTATS(JJ).NE.0)THEN
                    CONTXT='ERROR reading dgrf table '//FSPEC
                    GO TO 999
                 ENDIF
30            CONTINUE
              IF (NN .NE. N .OR. MM .NE. M) THEN                   
                IER = -2                                         
                GOTO 999                                         
              ENDIF                                                
              I = I + 1                                            
              GH(I) = G                                            
              IF (M .NE. 0) THEN                                   
                I = I + 1                                        
                GH(I) = H                                        
              ENDIF                 
              II = II + 1
 2235      CONTINUE                                                    
 2214   CONTINUE      

C close the table
        CALL UTTCLO(IDIN,ISTAT)
        ISTAT=0
        GO TO 1000
C
999     CALL UTTCLO(IDIN,ISTAT)
998     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
