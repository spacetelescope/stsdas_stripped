C @(#)chorec.for	15.1.1.1 (ES0-DMD) 01/12/01 16:57:08
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
      SUBROUTINE CHOREC(A,N,NDIM,ISTAT)     
*
*  Module number:
*
*  Module name: CHOREC
*
*  Keyphrase:
*  ----------
*       perform inversion
*
*  Description:
*  ------------
*
*  FORTRAN name: chorec.for
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
*       1       Nov 01  A. Alexov       Copy of chorec.for from midas
*-----------------------------------------------------------------------------
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
C INVERSION OF A SYMMETRIC POSITIVE MATRIX                                      
C      IMPLICIT NONE                                                            
C USING THE CHOLESKY ALGORITHM                                                  
C                                                                               
C INPUT PARAMS                                                                  
C A DBLE INPUT MATRIX                                                           
C N INTG ACTUAL MATRIX RANGE                                                    
C NDIM INTG TRUE DIMENSION IN MATRIX                                            
C                                                                               
C OUTPUT PARAMS                                                                 
C ISTAT INTG 0 - NO ERROR                                                       
C                                                                               
C THE MATRIX IS INVERTED IN-PLACE                                               
C                                                                               
C                                                                               
      INTEGER NDIM,N,ISTAT,I,J,K,KK
      DOUBLE PRECISION A(NDIM,NDIM)                                             
C                                                                               
      DOUBLE PRECISION P,Q,H(400)                                               
C REAL*8 X,Y,Z                                                                  
C                                                                               
      ISTAT  = 0                                                                
      IF (N.GT.NDIM) GO TO 70                                                   
C                                                                               
C MATRIX INVERSION USING THE GAUSS-JORDAN METHOD                                
C  MODIFIED VERSION BY F.L. BAUER AND C. REINSCH                                
C                                                                               
      DO 40 K = 1,N                                                             
          IF (A(1,1).LE.0.0D0) THEN                                             
              ISTAT  = 1                                                        
              GO TO 70                                                          
                                                                                
          END IF                                                                
                                                                                
          KK     = N + 1 - K                                                    
          P      = 1.0D0/A(1,1)                                                 
          DO 20 I = 2,N                                                         
              Q      = A(I,1)                                                   
              H(I)   = Q*P                                                      
              IF (I.LE.KK) H(I)   = -H(I)                                       
              DO 10 J = 2,I                                                     
                  A(I-1,J-1) = A(I,J) + Q*H(J)                                  
   10         CONTINUE                                                          
   20     CONTINUE                                                              
          A(N,N) = P                                                            
          DO 30 I = 2,N                                                         
              A(N,I-1) = H(I)                                                   
   30     CONTINUE                                                              
   40 CONTINUE                                                                  
      DO 60 I = 2,N                                                             
          DO 50 J = 1,I                                                         
              A(J,I) = A(I,J)                                                   
   50     CONTINUE                                                              
   60 CONTINUE                                                                  
C                                                                               
C  ... Routine below does not work !                                            
C                                                                               
C DO I = 1,N                                                                    
C   DO J = I,N                                                                  
C     X = A(I,J)                                                                
C     IF (I.NE.1) THEN                                                          
C       DO K = 1,I-1                                                            
C         X = X-A(J+1,I-K)*A(I+1,I-K)                                           
C       ENDDO                                                                   
C     ENDIF                                                                     
C     IF (I.NE.J) THEN                                                          
C       A(J+1,I) = X*Y                                                          
C     ELSE                                                                      
C       IF (X.LE.0.0D0) THEN                                                    
C  ISTAT = 1                                                                    
C  GO TO 1000                                                                   
C       ENDIF                                                                   
C       Y = 1.0D0/DSQRT(X)                                                      
C       A(I+1,I) = Y                                                            
C     ENDIF                                                                     
C   ENDDO                                                                       
C ENDDO                                                                         
C DO I = 2,N                                                                    
C   DO J = I,N                                                                  
C     Z = 0.0D0                                                                 
C     DO K = I,J-1                                                              
C       Z = Z-A(J+1,K)*A(K+1,I-1)                                               
C     ENDDO                                                                     
C     A(J+1,I-1) = Z*A(J+1,J)                                                   
C   ENDDO                                                                       
C ENDDO                                                                         
C DO I = 1,N                                                                    
C   DO J = I,N                                                                  
C     Z = 0.0D0                                                                 
C     DO K = J,N                                                                
C       Z = Z+A(K+1,J)*A(K+1,I)                                                 
C     ENDDO                                                                     
C     A(J+1,I) = Z                                                              
C   ENDDO                                                                       
C ENDDO                                                                         
C DO I = 2,N                                                                    
C   DO J = I,N                                                                  
C     A(I-1,J) = A(J,I-1)                                                       
C   ENDDO                                                                       
C ENDDO                                                                         
      RETURN                                                                    
                                                                                
   70 ISTAT  = ISTAT + 1                                                        
      RETURN                                                                    
                                                                                
      END                                                                       
