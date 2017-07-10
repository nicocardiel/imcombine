C Calcula la fecha juliana para el an~o YYYY, mes MM, dia DD y segundos SS
C (notar que los segundos recorren los valores desde 0 hasta 86400). Esta
C funcion utiliza el metodo descrito en Jean Meeus, pagina 23. El metodo no
C vale para fechas negativas.
        DOUBLE PRECISION FUNCTION FFJ(YYYY,MM,DD,SS)
        IMPLICIT NONE
        INTEGER YYYY,MM,DD,SS
C
        INTEGER FECHA
        DOUBLE PRECISION Y,M
        INTEGER A,B
C------------------------------------------------------------------------------
        FECHA=YYYY*10000+MM*100+DD
        IF(FECHA.LT.0) STOP 'Negative date in Julian Date function!'
C
        IF(MM.GT.2)THEN
          Y=DBLE(YYYY)
          M=DBLE(MM)
        ELSE
          Y=DBLE(YYYY-1)
          M=DBLE(MM+12)
        END IF
C
        IF(FECHA.GE.15821015)THEN
          A=INT(Y/100.D0)
          B=2-A+INT(A/4)
        ELSE
          A=0
          B=0
        END IF
C
        FFJ=DINT(365.25*Y)+DINT(30.6001D0*(M+1.D0))+
     +   DBLE(DD)+DBLE(SS)/86400.D0+1720994.5D0+DBLE(B)
        END
