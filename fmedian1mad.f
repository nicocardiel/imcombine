C------------------------------------------------------------------------------
C Version 07-July-2017                                      File: fmedian1mad.f
C------------------------------------------------------------------------------
C Copyright N. Cardiel, Departamento de Astrofisica
C Universidad Complutense de Madrid, 28040-Madrid, Spain
C E-mail: cardiel@ucm.es
C------------------------------------------------------------------------------
C This routine is free software; you can redistribute it and/or modify it
C under the terms of the GNU General Public License as published by the Free
C Software Foundation; either version 2 of the License, or (at your option) any
C later version. See the file gnu-public-license.txt for details.
C------------------------------------------------------------------------------
Comment
C
C REAL FUNCTION FMEDIAN1MAD(N,X,FMAD)
C
C Input: N,X
C Output: FMEDIAN (function), X (sorted), FMAD
C
C Calculate the median value of X(N) and the Median Absolute Deviation FMAD (a
C robust measure of the variability of the data X). It is important to note 
C that this subroutine rearranges the matrix X which is returned sorted.
C
C INTEGER N -> no. of elements
C REAL    X(N) -> input matrix
C REAL    FMAD -> median absolute deviation
C
Comment
C------------------------------------------------------------------------------
        REAL FUNCTION FMEDIAN1MAD(N,X,FMAD)
        IMPLICIT NONE
        INTEGER N
        REAL X(N)
        REAL FMAD
C
        INCLUDE 'dimensions.inc'
C
        REAL FMEDIAN1
C
        INTEGER I,NN
        REAL FABS(NMAXFRAMES)
C------------------------------------------------------------------------------
        IF(N.GT.NMAXFRAMES)THEN
          STOP('FATAL ERROR: N.GT.NMAXFRAMES in function FMEDIAN1MAD.')
        END IF
        IF(N.EQ.0) STOP 'FATAL ERROR: in function FMEDIAN1MAD: N=0.'
C
        CALL ORDENA1F(N,X)
        NN=N/2
        IF(MOD(N,2).EQ.0)THEN
          FMEDIAN1MAD=(X(NN)+X(NN+1))/2.
        ELSE
          FMEDIAN1MAD=X(NN+1)
        END IF
C
        DO I=1,N
          FABS(I)=ABS(X(I)-FMEDIAN1MAD)
        END DO
        FMAD=FMEDIAN1(N,FABS)
C
        END
