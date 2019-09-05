C------Subroutine WINDAT.
C
C	This routine takes the raw input spectrum in INDATA, fits a 5th order
C	polynomial to it to remove low wave-number shapes, and applies a
C	cosine bell window to the ends of the spectrum over a range at either
C	end that is HAN * NBIN long.
C
C	Originally due to Paul Schechter, 197?.
C	Possible modifications by Brad Whitmore and Eliot Malumuth, 1978-1982.
C	Modified by Gerard Kriss 1983 to let NBIN be a free parameter.
C	Modified by Gerard Kriss 1987 to pass data as explicit parameters
C		rather than in a catch-all COMMON block in the course of
C		modifications for using this routine in IRAF.
C
      SUBROUTINE WINDAT(INDATA,OUTDAT,NBIN,ISHIFT,IORD,HAN,AVE) 
      REAL INDATA(1),X(64),Y(64),B(11),OUTDAT(1) 
      REAL POLY
C   GENERATE SIZE DEPENDENT PARAMETERS.
        NB2=NBIN/2
        NB64=NBIN/64
      NUM=NBIN-IABS(ISHIFT) 
      IL=MAX0(-ISHIFT,0)+1 
      IR=IL+NUM-1 
      DO 10 I=1,64 
        X(I)=(I-32.5)/32 
        Y(I)=INDATA(NB64*(I-1)+1)/NB64 
        DO 10 J=2,NB64 
          Y(I)=Y(I)+INDATA(NB64*(I-1)+J)/NB64 
10        CONTINUE 
      CALL EARLIN(X,Y,64,B,-IORD)
      II=IABS(ISHIFT) 
      DO 20 I=1,II 
        OUTDAT(I)=0. 
        OUTDAT((NBIN+1)-I)=0. 
20      CONTINUE 
      AVE=B(1)+B(3)/3 
      DO 30 I=IL,IR 
        Z=(I-NB2-0.5)/NB2
        OUTDAT(I)=(INDATA(I) - POLY(IORD,B,Z)) / AVE
30    CONTINUE
      IHAN=HAN*NUM 
      T1=3.14159265/IHAN 
      DO 40 I=1,IHAN 
        FACT=(1-COS(T1*I))/2 
        OUTDAT((IR+1)-I)=FACT*OUTDAT((IR+1)-I) 
        OUTDAT((IL-1)+I)=FACT*OUTDAT((IL-1)+I) 
40      CONTINUE 
      RETURN 
      END 
