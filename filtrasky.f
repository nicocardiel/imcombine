C Filtra la imagen FRAMEIN, usando un filtro de media/mediana con una caja de
C tamaño NX x NY, e ignorando los pixels en la mascara MASK. Para poder
C realizar esto ultimo, la subrutina amplia el tamaño de la caja del filtro
C para garantizar un numero minimo de pixels NPIXMIN para el calculo de la 
C mediana. Uno de los parametros de la subrutina es PIXEL, un array
C unidimensional que se utiliza para almacenar los pixels que queremos
C calcular, y que pasamos a la rutina como parametro para no tener que
C redimensionar una nueva matriz. La imagen filtrada retorna en FRAMEOUT.
        SUBROUTINE FILTRASKY(CFILTER,FRAMEIN,FRAMEOUT,PIXEL,MASK,
     +   NXMAXD,NYMAXD,NAXIS1,NAXIS2,
     +   NX,NY,NPIXMIN)
        IMPLICIT NONE
        CHARACTER*1 CFILTER
        INTEGER NXMAXD,NYMAXD
        INTEGER NAXIS1,NAXIS2
        INTEGER NX,NY,NPIXMIN
        REAL FRAMEIN(NXMAXD,NYMAXD)
        REAL FRAMEOUT(NXMAXD,NYMAXD)
        REAL PIXEL(NXMAXD*NYMAXD)
        LOGICAL MASK(NXMAXD,NYMAXD)
C
        INTEGER SYSTEMFUNCTION
        REAL FMEDIAN1
C
        INTEGER I,J
        INTEGER K,KK
        INTEGER I1,I2,J1,J2
        INTEGER II1,II2,JJ1,JJ2
        INTEGER II,JJ
        INTEGER NX2,NY2
        INTEGER NPIXELS,NPIXTOT
        INTEGER ISYSTEM
        INTEGER NQUAD,NX1Q,NX2Q,NY1Q,NY2Q
        REAL SUM
        LOGICAL LOK
C------------------------------------------------------------------------------
C numero total de pixels en la imagen a filtrar
        NPIXTOT=NAXIS1*NAXIS2
C semianchuras de la caja
        NX2=NX/2
        NY2=NY/2
C recorremos toda la imagen
        WRITE(*,100) 'Filtering...'
        ISYSTEM=SYSTEMFUNCTION('date')
        WRITE(*,101) '# = 10 pixels in the Y-direction'
        DO NQUAD=1,4
          WRITE(*,*)
          WRITE(*,100) 'Quadrant #'
          WRITE(*,*) NQUAD
          IF(NQUAD.EQ.1)THEN
            NX1Q=1
            NX2Q=(NAXIS1)/2
            NY1Q=1
            NY2Q=(NAXIS2)/2
          ELSEIF(NQUAD.EQ.2)THEN
            NX1Q=(NAXIS1)/2+1
            NX2Q=NAXIS1
            NY1Q=1
            NY2Q=(NAXIS2)/2
          ELSEIF(NQUAD.EQ.3)THEN
            NX1Q=1
            NX2Q=(NAXIS1)/2
            NY1Q=(NAXIS2)/2+1
            NY2Q=NAXIS2
          ELSE
            NX1Q=(NAXIS1)/2+1
            NX2Q=NAXIS1
            NY1Q=(NAXIS2)/2+1
            NY2Q=NAXIS2
          END IF
          DO I=NY1Q,NY2Q
            I1=I-NY2
            I2=I+NY2
            IF(I1.LT.NY1Q) I1=NY1Q
            IF(I2.GT.NY2Q) I2=NY2Q
            DO J=NX1Q,NX2Q
              J1=J-NX2
              J2=J+NX2
              IF(J1.LT.NX1Q) J1=NX1Q
              IF(J2.GT.NX2Q) J2=NX2Q
              !comprobamos si con los limites iniciales tenemos, al menos,
              !NPIXMIN pixels en la caja para calcular la mediana; si no es
              !asi, aumentamos los limites de la caja por los cuatro lados
              II1=I1
              II2=I2
              JJ1=J1
              JJ2=J2
              LOK=.FALSE.
              DO WHILE(.NOT.LOK)
                NPIXELS=(JJ2-JJ1+1)*(II2-II1+1)
                IF(NPIXELS.EQ.NPIXTOT)THEN
                  LOK=.TRUE.
                ELSE
                  IF(NPIXELS.GE.NPIXMIN)THEN
                    K=0 !contamos pixels a ignorar
                    DO II=II1,II2
                      DO JJ=JJ1,JJ2
                        IF(MASK(JJ,II)) K=K+1
                      END DO
                    END DO
                    LOK=((NPIXELS-K).GE.NPIXMIN)
                  END IF
                  IF(.NOT.LOK)THEN
                    II1=II1-1
                    IF(II1.LT.NY1Q) II1=NY1Q
                    II2=II2+1
                    IF(II2.GT.NY2Q) II2=NY2Q
                    JJ1=JJ1-1
                    IF(JJ1.LT.NX1Q) JJ1=NX1Q
                    JJ2=JJ2+1
                    IF(JJ2.GT.NX2Q) JJ2=NX2Q
                  END IF
                END IF
              END DO
              !ya tenemos suficiente numero de pixels para calcular mediana
              K=0
              DO II=II1,II2
                DO JJ=JJ1,JJ2
                  IF(.NOT.MASK(JJ,II))THEN
                    K=K+1
                    PIXEL(K)=FRAMEIN(JJ,II)
                  END IF
                END DO
              END DO
              IF(CFILTER.EQ.'1')THEN !mean
                SUM=0.0
                DO KK=1,K
                  SUM=SUM+PIXEL(K)
                END DO
                FRAMEOUT(J,I)=SUM/REAL(K)
              ELSEIF(CFILTER.EQ.'2')THEN !median
                FRAMEOUT(J,I)=FMEDIAN1(K,PIXEL)
              ELSE
                WRITE(*,100) 'CFILTER: '
                WRITE(*,*) CFILTER
                STOP 'FATAL ERROR: invalid CFILTER value in FILTRASKY'
              END IF
            END DO
            IF(MOD(I,10).EQ.0) WRITE(*,100) '#'
            IF(MOD(I,100).EQ.0) THEN
              WRITE(*,*) I
            END IF
          END DO
        END DO
        IF(MOD(NAXIS2,100).NE.0) WRITE(*,*)
C
100     FORMAT(A,$)
101     FORMAT(A)
        END
