C Lee el fichero con la mascara y genera una matriz con dicha mascara
        SUBROUTINE MASKFRAME(FILENAME, NAXISF1, NAXISF2)
        IMPLICIT NONE
C
        CHARACTER*(*) FILENAME
        INTEGER NAXISF1
        INTEGER NAXISF2
C
        INCLUDE 'dimensions.inc'
C
        INTEGER TRUEBEG,TRUELEN
        INTEGER READI
C
        INTEGER I,J
        INTEGER I1,I2,J1,J2
        INTEGER IMIN,IMAX,JMIN,JMAX
        INTEGER L,L1,L2
        INTEGER ISTATUS,IREADWRITE,IUNIT,BLOCKSIZE
        INTEGER BITPIX,FIRSTPIX,NFOUND
        INTEGER NAXIS_(0:2)
        INTEGER NEW_HDU,HDUTYPE
        INTEGER JROW(NXMAXF)
        CHARACTER*5 FILETYPE
        CHARACTER*50 COMMENT
        CHARACTER*255 CLINEA
        LOGICAL MASK_FRAME(NXMAXF,NYMAXF)
        LOGICAL ANYNULL,LANYNULL,EXTEND
        LOGICAL LROW(NXMAXF)
C
        COMMON/BLK_MASK_FRAME/MASK_FRAME
C------------------------------------------------------------------------------
C inicializamos mascara
        DO I=1,NYMAXF
          DO J=1,NXMAXF
            MASK_FRAME(J,I)=.FALSE.
          END DO
        END DO
        IF(FILENAME.EQ.'none') RETURN
C------------------------------------------------------------------------------
C si es una imagen FITS, suponemos 0=.FALSE. y >0=.TRUE.
        L1=TRUEBEG(FILENAME)
        L2=TRUELEN(FILENAME)
        IF(L2.GT.4)THEN
          FILETYPE=FILENAME(L2-4:L2)
          IF((FILETYPE.EQ.'.fits').OR.(FILETYPE.EQ.'.FITS'))THEN
            ISTATUS=0           !controla posibles errores durante la ejecucion
            IREADWRITE=0                  !la imagen se abrira en modo READONLY
            LANYNULL=.FALSE.
            IUNIT=80
            !abrimos el fichero
            CALL FTOPEN(IUNIT,FILENAME,IREADWRITE,BLOCKSIZE,ISTATUS)
            !miramos si tiene extensiones
            CALL FTGKYL(IUNIT,'EXTEND',EXTEND,COMMENT,ISTATUS)
            IF(ISTATUS.EQ.202)THEN
              EXTEND=.FALSE.
              ISTATUS=0
            END IF
            IF(EXTEND)THEN
              WRITE(*,*)
              WRITE(*,101) '***WARNING***'
              WRITE(*,101) 'MASKFRAME> this file contains extensions'
              NEW_HDU=READI('Extension number to be read (1=primary)',
     +         '2')
              CALL FTMAHD(IUNIT,NEW_HDU,HDUTYPE,ISTATUS)
            END IF
            !leemos BITPIX
            CALL FTGKYJ(IUNIT,'BITPIX',BITPIX,COMMENT,ISTATUS)
            !comprobamos que NAXIS=2
            CALL FTGKYJ(IUNIT,'NAXIS',NAXIS_(0),COMMENT,ISTATUS)
            IF(NAXIS_(0).GT.2)THEN
              WRITE(*,*)
              WRITE(*,101) '***FATAL ERROR***'
              WRITE(*,100) 'MASKFRAME> NAXIS='
              WRITE(*,*) NAXIS_(0)
              WRITE(*,101) 'MASKFRAME> NAXIS > 2'
              CALL FTCLOS(IUNIT,ISTATUS)
              STOP
            ELSEIF(NAXIS_(0).EQ.1)THEN
              NAXIS_(2)=1
            END IF
            !leemos NAXIS1 y NAXIS2
            CALL FTGKNJ(IUNIT,'NAXIS',1,2,NAXIS_(1),NFOUND,ISTATUS)
            !comprobamos si tenemos los valores esperados de NAXIS
            IF((NAXISF1.NE.NAXIS_(1)).OR.(NAXISF2.NE.NAXIS_(2)))THEN
              WRITE(*,*)
              WRITE(*,101) '***FATAL ERROR***'
              WRITE(*,100) 'MASKFRAME> NAXIS1 (expected, read): '
              WRITE(*,*) NAXISF1, NAXIS_(1)
              WRITE(*,100) 'MASKFRAME> NAXIS2 (expected, read): '
              WRITE(*,*) NAXISF2, NAXIS_(2)
              CALL FTCLOS(IUNIT,ISTATUS)
            END IF
            !leemos la imagen
            IF((BITPIX.EQ.8).OR.(BITPIX.EQ.16))THEN
              DO I=1,NAXIS_(2)
                FIRSTPIX=(I-1)*NAXIS_(1)+1
                CALL FTGPFJ(IUNIT,1,FIRSTPIX,NAXIS_(1),JROW(1),LROW(1),
     +           ANYNULL,ISTATUS)
                DO J=1,NAXIS_(1)
                  IF(JROW(J).GT.0)THEN
                    MASK_FRAME(J,I)=.TRUE.
                  END IF
                END DO
                IF(ANYNULL)THEN
                  LANYNULL=.TRUE.
                END IF
              END DO
            ELSE
              WRITE(*,*)
              WRITE(*,101) '***FATAL ERROR***'
              WRITE(*,100) 'MASKFRAME> Unexpected BITPIX ='
              WRITE(*,*) BITPIX
              CALL FTCLOS(IUNIT,ISTATUS)
              STOP
            END IF
            !cerramos el fichero
            CALL FTCLOS(IUNIT,ISTATUS)
          END IF
          RETURN
        END IF
C------------------------------------------------------------------------------
C fichero ASCII con el formato historico
        OPEN(10,FILE=FILENAME,STATUS='OLD',FORM='FORMATTED')
80      READ(10,101,END=90) CLINEA
        L=TRUELEN(CLINEA)
        IF(L.GT.0)THEN
          IF(CLINEA(1:1).EQ.'#')THEN
            !ignore comment line
          ELSEIF(CLINEA(1:3).EQ.'p: ')THEN
            READ(CLINEA(4:L),*) J1,I1
              IF((1.LE.J1).AND.(NXMAXF.GE.J1).AND.
     +           (1.LE.I1).AND.(NYMAXF.GE.I1))THEN
                MASK_FRAME(J1,I1)=.TRUE.
              END IF
          ELSEIF(CLINEA(1:3).EQ.'r: ')THEN
            READ(CLINEA(4:L),*) J1,I1,J2,I2
            JMIN=MIN0(J1,J2)
            JMAX=MAX0(J1,J2)
            IF(JMIN.LT.1) JMIN=1
            IF(JMAX.GT.NXMAXF) JMAX=NXMAXF
            IMIN=MIN0(I1,I2)
            IMAX=MAX0(I1,I2)
            IF(IMIN.LT.1) IMIN=1
            IF(IMAX.GT.NYMAXF) IMAX=NYMAXF
            DO I=IMIN,IMAX
              DO J=JMIN,JMAX
                MASK_FRAME(J,I)=.TRUE.
              END DO
            END DO
          ELSE
            WRITE(*,100) 'Mask file: '
            WRITE(*,101) FILENAME(1:TRUELEN(FILENAME))
            WRITE(*,100) 'Line: '
            L=TRUELEN(CLINEA)
            WRITE(*,101) CLINEA(1:L)
            WRITE(*,101) 'ERROR: syntax in mask file.'
            WRITE(*,100) 'Press <CR> to continue...'
            READ(*,*)
          END IF
        END IF
        GOTO 80
90      CLOSE(10)
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
