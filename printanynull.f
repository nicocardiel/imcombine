C
C******************************************************************************
C
        SUBROUTINE PRINTANYNULL(ANYNULL)
        IMPLICIT NONE
        LOGICAL ANYNULL
C
        IF(ANYNULL)THEN
          WRITE(*,101) 'ANYNULL=.TRUE.'
          WRITE(*,101) 'ERROR: option not implemented.'
          STOP
        END IF
C
101     FORMAT(A)
        END
