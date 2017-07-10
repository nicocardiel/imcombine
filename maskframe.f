C Lee el fichero con la mascara y genera una matriz con dicha mascara
        SUBROUTINE MASKFRAME(FILENAME)
        IMPLICIT NONE
C
        CHARACTER*(*) FILENAME
C
        INCLUDE 'dimensions.inc'
C
        INTEGER TRUELEN
C
        INTEGER I,J
        INTEGER I1,I2,J1,J2
        INTEGER IMIN,IMAX,JMIN,JMAX
        INTEGER L
        CHARACTER*255 CLINEA
        LOGICAL MASK_FRAME(NXMAXF,NYMAXF)
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
C
        OPEN(10,FILE=FILENAME,STATUS='OLD',FORM='FORMATTED')
10      READ(10,101,END=20) CLINEA
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
        GOTO 10
20      CLOSE(10)
C
100     FORMAT(A,$)
101     FORMAT(A)
        END
