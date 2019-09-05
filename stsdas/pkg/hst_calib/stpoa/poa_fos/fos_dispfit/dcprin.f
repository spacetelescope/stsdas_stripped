C @(#)dcprin.for	15.1.1.1 (ES0-DMD) 01/12/01 16:57:09
C===========================================================================
C Copyright (C) 1995 European Southern Observatory (ESO)
C
C This program is free software; you can redistribute it and/or 
C modify it under the terms of the GNU General Public License as 
C published by the Free Software Foundation; either version 2 of 
C the License, or (at your option) any later version.
C
C This program is distributed in the hope that it will be useful,
C but WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C GNU General Public License for more details.
C
C You should have received a copy of the GNU General Public 
C License along with this program; if not, write to the Free 
C Software Foundation, Inc., 675 Massachusetss Ave, Cambridge, 
C MA 02139, USA.
C
C Corresponding concerning ESO-MIDAS should be addressed as follows:
C	Internet e-mail: midas@eso.org
C	Postal address: European Southern Observatory
C			Data Management Division 
C			Karl-Schwarzschild-Strasse 2
C			D 85748 Garching bei Muenchen 
C			GERMANY
C===========================================================================
C
      SUBROUTINE DCPRIN(A,N,NDIM)   
*
*  Module number:
*
*  Module name: DCPRIN
*
*  Keyphrase:
*  ----------
*       performs the printing of the corr matrix
*
*  Description:
*  ------------
*
*  FORTRAN name: dcprin.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  
*
*  Subroutines Called:
*  -------------------
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Nov 01  A. Alexov       Copy of dcprin.for from midas
*------------------------------------------------------------------------------
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                   
C.COPYRIGHT: Copyright (c) 1987 European Southern Observatory,                  
C                                         all rights reserved                   
C                                                                               
C.VERSION: 1.0  ESO-FORTRAN Conversion, AA  17:25 - 13 JAN 1988                 
C                                                                               
C.LANGUAGE: F77+ESOext                                                          
C                                                                               
C.AUTHOR: J.D.PONZ                                                              
C                                                                               
C.IDENTIFICATION                                                                
C                                                                               
C  FITLIB.FOR   VERSION 1.0  27 MAR 1984                                        
C                                                                               
C.PURPOSE                                                                       
C                                                                               
C  INTERFACE ROUTINES FOR THE FITTING STRUCTURES                                
C                                                                               
C.ALGORITHM                                                                     
C                                                                               
C  USE MIDAS I/O INTERFACES TO FRAMES AND TABLES                                
C                                                                               
C.KEYWORDS                                                                      
C                                                                               
C  NON LINEAR FITTING                                                           
C                                                                               
C                                                                               
C----------------------------------------------------------------               
C                                                                               
C PRINT CORRELATION MATRIX                                                      
C      IMPLICIT NONE                                                            
C                                                                               
C INPUT PARAMS                                                                  
C A DBLE INPUT MATRIX (COVARIANCE)                                              
C N INTG NUMBER OF PARAMS                                                       
C NDIM INTG DIMENSION OF THE MATRIX                                             
C                                                                               
      INTEGER N,NDIM,K,ISTAT,I,J,II,IC(129)
      DOUBLE PRECISION A(NDIM,NDIM)                                             
      REAL C(129),B,D                                        
      CHARACTER*80 TEXT*22
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
      CHARACTER*80 CONTXT

c      DATA ITEXT/1H-,1HI,1HH,1HG,1HF,1HE,1HD,1HC,1HB,1HA,1H0,1H1,1H2,           
c     +     1H3,1H4,1H5,1H6,1H7,1H8,1H9,1H+,1H//                                 
                                                                                
      TEXT(1:) = '-IHGFEDCBA0123456789+/'
      IF (N.LT.2) RETURN                                                        
      K      = 5                                                                
      IF (N.GT.76) GO TO 120                                                    
      IF (N.GT.10) K      = 1                                                   
      IF (N.GT.15) K      = 2                                                   
      IF (N.GT.25) K      = 3                                                   
      IF (N.GT.38) K      = 4                                                   
      CONTXT=' Estimated correlations matrix '                      
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
      CONTXT=' _____________________________ '                    
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
      CONTXT=' '                                                    
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
      DO 110 I = 2,N                                                            
          B      = SNGL(A(I,I))                                                 
          II     = I - 100* (I/100)                                             
          DO 10 J = 1,I - 1                                                     
              D      = SNGL(A(J,J))*B                                           
              IF (D.LE.0.0) THEN                                                
                  C(J)   = 0.0                                                  
                                                                                
              ELSE                                                              
                  C(J)   = SNGL(A(I,J))/SQRT(D)                                 
                  IF (ABS(C(J)).GT.1.0) GO TO 130                               
              END IF                                                            
                                                                                
   10     CONTINUE                                                              
          GO TO (20,30,50,70,90),K                                              
                                                                                
   20     WRITE (CONTXT,9010) I, (C(J),J=1,I-1)    
          CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)   
          GO TO 100                                                             
                                                                                
   30     DO 40 J = 1,I - 1                                                     
              IC(J)  = INT(100.0*C(J))                                          
   40     CONTINUE    
          CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)                  
          WRITE (CONTXT,9020) I, (IC(J),J=1,I-1)                                  
          GO TO 100                                                             
                                                                                
   50     DO 60 J = 1,I - 1                                                     
              IC(J)  = INT(10.0*C(J))                                           
   60     CONTINUE                                                              
          WRITE (CONTXT,9030) I, (IC(J),J=1,I-1)    
          CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
          GO TO 100                                                             
                                                                                
   70     DO 80 J = 1,I - 1                                                     
              IF (C(J).GT.99999.9) C(J)   = 1.1                                 
              IC(J)  = INT(10.0* (1.0+C(J))) + 1                                
   80     CONTINUE                                                              
cc          WRITE (CONTXT,9040) II, (ITEXT(IC(J)),J=1,I-1)                          
          WRITE(CONTXT,9040)II,(TEXT(IC(J):IC(J)),J=1,I-1)
          CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
          GO TO 100                                                             
                                                                                
   90     WRITE (CONTXT,9000) I, (C(J),J=1,I-1)                                   
  100     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)                                       
  110 CONTINUE                                                                  
      CONTXT=' '                                                    
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
      RETURN                                                                    
                                                                                
  120 CONTXT=' '                                                    
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT) 
      CONTXT='*** WARN-CORR : Correlation matrix is too '//                
     +            'big ***'                                        
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT) 
      RETURN                                                                    
                                                                                
  130 CONTXT=' '                                                    
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)         
      CONTXT='*** ERR-CORR : Error in correlation matrix ***'                              
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)         
      WRITE (*,*) I,J      
cc      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)                      
      RETURN                                                                    
                                                                                
 9000 FORMAT (1X,I2,' |',10F7.4)                                                
 9010 FORMAT (1X,I2,' |',15F5.2)                                                
 9020 FORMAT (1X,I2,' |',25I3)                                                  
 9030 FORMAT (1X,I2,1X,38I2)                                                    
 9040 FORMAT (1X,I2,1X,A)                                                    
      END                                                                       
