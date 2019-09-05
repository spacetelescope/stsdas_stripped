       SUBROUTINE AINFO(ORD)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C  writes information on the noise statistics and filter performance
C  to the terminal:
C      ORD   : order
C      EG    : exspectation value by noise of gradients at the given order
C      SG    : standard deviation of gradients
C      ES    : exspectation value of Laplace-terms
C      EL    : standard deviation of Laplace-terms
C      NSG   : number of pixels wich became significant by the gradient
C      NSL   : number of pixels wich became significant by Laplace
C      NREST : number of pixels wich did not become significant at
C      any order. They are set by the maximal filter size.
C---------------------------------------------------------------------
*       IMPLICIT NONE
C
       INTEGER       NSG,NSL,NREST,ORD,I,ST
       REAL          EG,SG,EL,SL
       CHARACTER*80  A
C
       COMMON        /TEST/EG(16),SG(16),EL(16),SL(16),NSG(16),
     >                     NSL(16),NREST
 100   FORMAT(' ',1X,'ORD',7X,'EG',11X,'SG',11X,'EL',11X,
     >        'SL',11X,'NSG',7X,'NSL')
 200   FORMAT (' ',1X,I3,4(1X,G12.3),2(I10))
 300   FORMAT (' ',60X,'NREST:',I10)
C
       WRITE(A,100)
       CALL UMSPUT (A, 1, 0, ST)
C
       DO 10 I=2,ORD
          WRITE (A,200) I,EG(I),SG(I),EL(I),SL(I),NSG(I),NSL(I)
          CALL UMSPUT (A, 1, 0, ST)
10     CONTINUE
C
       WRITE (A,300) NREST
       CALL UMSPUT (A, 1, 0, ST)
C
       RETURN
       END
