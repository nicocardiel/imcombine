C Calcula la media y el rms en las regiones centrales de los cuatros cuadrantes
C de una imagen, usando para ello SOLO una region pequen~a de cada cuadrante.
        SUBROUTINE SONDEA4C(FRAME,NXMAX,NYMAX,NX,NY,FMEAN,FSIGMA)
        IMPLICIT NONE
C
        INTEGER NXMAX,NYMAX
        REAL FRAME(NXMAX,NYMAX)
        INTEGER NX,NY
        REAL FMEAN,FSIGMA
C
        REAL FMEAN0
C
        INTEGER I,J,K
        INTEGER I1,I2,J1,J2
        INTEGER NPIXELS,NPIXELS_REMOVE
        REAL PIXEL(484) !4 cuadrantes x 11 x 11 pixels
C------------------------------------------------------------------------------
        IF(NX*NY.LT.484)THEN
          K=0
          DO I=1,NY
            DO J=1,NX
              K=K+1
              PIXEL(K)=FRAME(J,I)
            END DO
          END DO
          NPIXELS=NX*NY
        ELSE
          K=0
          !cuadrante #1 (abajo izquierda)
          J1=NX/4-5
          J2=J1+10
          I1=NY/4-5
          I2=I1+10
          DO I=I1,I2
            DO J=J1,J2
              K=K+1
              PIXEL(K)=FRAME(J,I)
            END DO
          END DO
          !cuadrante #2 (abajo derecha)
          J1=NX/4+NX/2-5
          J2=J1+10
          I1=NY/4-5
          I2=I1+10
          DO I=I1,I2
            DO J=J1,J2
              K=K+1
              PIXEL(K)=FRAME(J,I)
            END DO
          END DO
          !cuadrante #3 (arriba izquierda)
          J1=NX/4-5
          J2=J1+10
          I1=NY/4+NY/2-5
          I2=I1+10
          DO I=I1,I2
            DO J=J1,J2
              K=K+1
              PIXEL(K)=FRAME(J,I)
            END DO
          END DO
          !cuadrante #4 (arriba derecha)
          J1=NX/4+NX/2-5
          J2=J1+10
          I1=NY/4+NY/2-5
          I2=I1+10
          DO I=I1,I2
            DO J=J1,J2
              K=K+1
              PIXEL(K)=FRAME(J,I)
            END DO
          END DO
        END IF
        NPIXELS=K
C
        CALL ORDENA1F(NPIXELS,PIXEL)
C
        NPIXELS_REMOVE=NINT(0.1*REAL(NPIXELS))
        FMEAN=FMEAN0(NPIXELS-2*NPIXELS_REMOVE,PIXEL(NPIXELS_REMOVE+1),
     +   FSIGMA)
C
        END
