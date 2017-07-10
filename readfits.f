C Subrutina para leer imagenes fits. Si LCHECK=.TRUE., el valor de entrada
C de NAXIS1 y NAXIS2 es comparado con las dimensiones de la imagen, obteniendo
C un mensaje de error si las dimensiones no coinciden. En la salida, NAXIS1 y
C NAXIS2 contienen las dimensiones de la imagen leida. La imagen es almacenada
C en la matriz IMAGE, cuyas dimensiones fisicas son NXMAX y NYMAX.
C
C Si LNORM=.TRUE., la rutina lee el keyword EXPTIME y normaliza la señal.
        SUBROUTINE READFITS(FILENAME,NXMAXD,NYMAXD,NAXIS1,NAXIS2,
     +   IMAGE,LCHECK,EXPTIME)
        IMPLICIT NONE
C
        INCLUDE 'dimensions.inc'
C
        CHARACTER*(*) FILENAME
        INTEGER NXMAXD,NYMAXD
        INTEGER NAXIS1,NAXIS2
        REAL IMAGE(NXMAXD,NYMAXD)
        LOGICAL LCHECK
        REAL EXPTIME
C funciones
        INTEGER TRUELEN
C variables locales
        INTEGER I,J
        INTEGER FIRSTPIX
        INTEGER IREADWRITE,BLOCKSIZE,ISTATUS,BITPIX
        INTEGER NAXIS_(0:2),NFOUND
        INTEGER JROW(NXMAX)
        REAL FROW(NXMAX)
        DOUBLE PRECISION DROW(NXMAX)
        CHARACTER*50 COMMENT
        LOGICAL ANYNULL
        LOGICAL LROW(NXMAX)
C------------------------------------------------------------------------------
        ISTATUS=0
        IREADWRITE=0
        ANYNULL=.FALSE.
C
        CALL FTOPEN(80,FILENAME,IREADWRITE,BLOCKSIZE,ISTATUS)
        CALL FTGKYJ(80,'BITPIX',BITPIX,COMMENT,ISTATUS)
        CALL FTGKYJ(80,'NAXIS',NAXIS_(0),COMMENT,ISTATUS)
        CALL FTGKYE(80,'EXPTIME',EXPTIME,COMMENT,ISTATUS)
        IF(ISTATUS.EQ.202)THEN
          EXPTIME=0.0
          ISTATUS=0
        END IF
        IF(NAXIS_(0).NE.2)THEN
          CALL FTCLOS(80,ISTATUS)
          WRITE(*,100) 'ERROR: in image '
          WRITE(*,101) FILENAME(1:TRUELEN(FILENAME))
          WRITE(*,100) '=> NAXIS='
          WRITE(*,*) NAXIS_(0)
          CALL PGEND
          STOP
        END IF
        CALL FTGKNJ(80,'NAXIS',1,2,NAXIS_(1),NFOUND,ISTATUS)
C
        IF(LCHECK)THEN
          IF(NAXIS1.NE.NAXIS_(1))THEN
            CALL FTCLOS(80,ISTATUS)
            WRITE(*,100) 'ERROR: in image '
            WRITE(*,101) FILENAME(1:TRUELEN(FILENAME))
            WRITE(*,100) '=> NAXIS1='
            WRITE(*,*) NAXIS_(1)
            WRITE(*,100) '=> Expected NAXIS1='
            WRITE(*,*) NAXIS1
            CALL PGEND
            STOP
          END IF
          IF(NAXIS2.NE.NAXIS_(2))THEN
            CALL FTCLOS(80,ISTATUS)
            WRITE(*,100) 'ERROR: in image '
            WRITE(*,101) FILENAME(1:TRUELEN(FILENAME))
            WRITE(*,100) '=> NAXIS2='
            WRITE(*,*) NAXIS_(2)
            WRITE(*,100) '=> Expected NAXIS2='
            WRITE(*,*) NAXIS2
            CALL PGEND
            STOP
          END IF
        ELSE
          NAXIS1=NAXIS_(1)
          NAXIS2=NAXIS_(2)
        END IF
C
        IF((BITPIX.EQ.16).OR.(BITPIX.EQ.32))THEN
          DO I=1,NAXIS2
            FIRSTPIX=(I-1)*NAXIS1+1
            CALL FTGPFJ(80,1,FIRSTPIX,NAXIS1,JROW,LROW,ANYNULL,ISTATUS)
            DO J=1,NAXIS1
              IMAGE(J,I)=REAL(JROW(J))
            END DO
          END DO
        ELSEIF(BITPIX.EQ.-32)THEN
          DO I=1,NAXIS2
            FIRSTPIX=(I-1)*NAXIS1+1
            CALL FTGPFE(80,1,FIRSTPIX,NAXIS1,FROW,LROW,ANYNULL,ISTATUS)
            DO J=1,NAXIS1
              IMAGE(J,I)=FROW(J)
            END DO
          END DO
        ELSEIF(BITPIX.EQ.-64)THEN
          DO I=1,NAXIS2
            FIRSTPIX=(I-1)*NAXIS1+1
            CALL FTGPFD(80,1,FIRSTPIX,NAXIS1,DROW,LROW,ANYNULL,ISTATUS)
            DO J=1,NAXIS1
              IMAGE(J,I)=REAL(DROW(J))
            END DO
          END DO
        ELSE
          WRITE(*,100) 'BITPIX='
          WRITE(*,*) BITPIX
          WRITE(*,101) 'ERROR: BITPIX option not implemented.'
          CALL PGEND
          STOP
        END IF
        CALL FTCLOS(80,ISTATUS)
        IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
        CALL PRINTANYNULL(ANYNULL)
C
100     FORMAT(A,$)
101     FORMAT(A)
        END
