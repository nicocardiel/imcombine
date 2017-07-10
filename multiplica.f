C Multiplica la imagen en el fichero 1 por la imagen en el fichero 2,
C salvando el resultado en el fichero 3. Se utilizan temporalmente los arrays
C ARRAY1 y ARRAY2 para ahorrar memoria.
        SUBROUTINE MULTIPLICA(FILE1,FILE2,FILE3,ARRAY1,ARRAY2)
        IMPLICIT NONE
C
        INCLUDE 'dimensions.inc'
        CHARACTER*(*) FILE1
        CHARACTER*(*) FILE2
        CHARACTER*(*) FILE3
        REAL ARRAY1(NXMAX,NYMAX)
        REAL ARRAY2(NXMAX,NYMAX)
C
        INTEGER TRUELEN
        INTEGER SYSTEMFUNCTION
C
        INTEGER I,J
        INTEGER ISYSTEM
        INTEGER NAXIS(2)
        INTEGER BITPIX,BLOCKSIZE,ISTATUS
        REAL EXPTIME
        CHARACTER*255 FILE3_
        LOGICAL LOGFILE
        LOGICAL SIMPLE,EXTEND
C------------------------------------------------------------------------------
C chequeamos la presencia de los ficheros
        INQUIRE(FILE=FILE1,EXIST=LOGFILE)
        IF(.NOT.LOGFILE)THEN
          WRITE(*,100) 'ERROR: the file '
          WRITE(*,100) FILE1(1:TRUELEN(FILE1))
          WRITE(*,101) ' does not exist.'
          STOP
        END IF
C
        INQUIRE(FILE=FILE2,EXIST=LOGFILE)
        IF(.NOT.LOGFILE)THEN
          WRITE(*,100) 'ERROR: the file '
          WRITE(*,100) FILE2(1:TRUELEN(FILE2))
          WRITE(*,101) ' does not exist.'
          STOP
        END IF
C
        FILE3_=FILE3
        INQUIRE(FILE=FILE3_,EXIST=LOGFILE)
        IF(LOGFILE)THEN
          ISYSTEM=SYSTEMFUNCTION('rm -f '//FILE3_(1:TRUELEN(FILE3_)))
        END IF
C------------------------------------------------------------------------------
C leemos las imágenes y las multiplicamos
        CALL READFITS(FILE1,NXMAX,NYMAX,NAXIS(1),NAXIS(2),ARRAY1,
     +   .FALSE.,EXPTIME)
        CALL READFITS(FILE2,NXMAX,NYMAX,NAXIS(1),NAXIS(2),ARRAY2,
     +   .TRUE.,EXPTIME)
        DO I=1,NAXIS(2)
          DO J=1,NAXIS(1)
            ARRAY1(J,I)=ARRAY1(J,I)*ARRAY2(J,I)
          END DO
        END DO
C------------------------------------------------------------------------------
C salvamos el resultado
        SIMPLE=.TRUE.
        BITPIX=-32
        EXTEND=.FALSE.
        BLOCKSIZE=1
        ISTATUS=0
        CALL FTINIT(80,FILE3,BLOCKSIZE,ISTATUS)
        CALL FTPHPR(80,SIMPLE,BITPIX,2,NAXIS,0,1,EXTEND,ISTATUS)
        CALL FTP2DE(80,1,NXMAX,NAXIS(1),NAXIS(2),ARRAY1,ISTATUS)
        CALL FTCLOS(80,ISTATUS)
        IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
