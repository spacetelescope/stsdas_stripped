C @(#)dmatin.for	15.1.1.1 (ES0-DMD) 01/12/01 16:57:09
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
      SUBROUTINE DMATIN(A,N,NDIM,FIXED,ISTAT)   
*
*  Module number:
*
*  Module name: DMATIN
*
*  Keyphrase:
*  ----------
*       double matrix invert
*
*  Description:
*  ------------
*
*  FORTRAN name: dmatin.for
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
*       1       Nov 01  A. Alexov       Copy of dmatin.for from midas
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
C Special matrix inversion routine to cope with constraint                      
C      IMPLICIT NONE                                                            
C parameters in the normal equation matrix.                                     
C The matrix is first compressed to ignore non-significant                      
C columns and rows. Then it is inverted usind the Cholesky                      
C algorithm. Afterwards it is expanded back to the original                     
C structure.                                                                    
C                                                                               
C                                                                               
      INTEGER NDIM,N,ISTAT,NN,M,I,J,K,L
      INTEGER FIXED(N)                                                          
      DOUBLE PRECISION A(NDIM,NDIM)                                             
C                                                                               
C      warning: these variables are never used
C      DOUBLE PRECISION P,Q,H(400)                                               
C                                                                               
C ... Test for fixed parameters                                                 
C                                                                               
      NN     = N                                                                
      M      = 0                                                                
      DO 10 I = 1,N                                                             
          M      = M + FIXED(I)                                                 
   10 CONTINUE                                                                  
      IF (M.EQ.-N) GO TO 80                                                     
C                                                                               
C ... First compress the matrix                                                 
C                                                                               
      ISTAT  = 3                                                                
      L      = N                                                                
   20 IF (FIXED(L).GE.0) THEN                                                   
          L      = L - 1                                                        
          IF (L.GT.0) GO TO 20                                                  
      END IF                                                                    
                                                                                
      IF (L.EQ.0) RETURN                                                        
      NN     = L                                                                
      IF (L.EQ.1) GO TO 80                                                      
      DO 70 I = L - 1,1,-1                                                      
          IF (FIXED(I).GE.0) THEN                                               
              NN     = NN - 1                                                   
              DO 40 J = I,NN                                                    
                  DO 30 K = 1,NN + 1                                            
                      A(K,J) = A(K,J+1)                                         
   30             CONTINUE                                                      
   40         CONTINUE                                                          
              DO 60 J = I,NN                                                    
                  DO 50 K = 1,NN                                                
                      A(J,K) = A(J+1,K)                                         
   50             CONTINUE                                                      
   60         CONTINUE                                                          
          END IF                                                                
                                                                                
   70 CONTINUE                                                                  
C                                                                               
C ... Now invert it                                                             
C                                                                               
   80 CALL CHOREC(A,NN,50,ISTAT)                                                
      IF (ISTAT.NE.0 .OR. M.EQ.-N .OR. L.EQ.NN) RETURN                          
C                                                                               
C ... Finally expand it back again                                              
C                                                                               
      DO 140 I = 1,L - 1                                                        
          IF (FIXED(I).GE.0) THEN                                               
              DO 100 J = L - 1,I,-1                                             
                  DO 90 K = 1,L                                                 
                      A(K,J+1) = A(K,J)                                         
   90             CONTINUE                                                      
  100         CONTINUE                                                          
              DO 120 J = L - 1,I,-1                                             
                  DO 110 K = 1,L                                                
                      A(J+1,K) = A(J,K)                                         
  110             CONTINUE                                                      
  120         CONTINUE                                                          
              DO 130 J = 1,L                                                    
                  A(I,J) = 0.0D0                                                
                  A(J,I) = 0.0D0                                                
  130         CONTINUE                                                          
              A(I,I) = 1.0D0                                                    
          END IF                                                                
                                                                                
  140 CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
                                                                                
      END                                                                       
