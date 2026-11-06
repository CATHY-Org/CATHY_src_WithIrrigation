C
C*************************** CONCINI_NODE ******************************
C
C  reads initial concentration
C
C***********************************************************************
C
      SUBROUTINE CONCINI_NODE(N,COLD)
C
      IMPLICIT NONE
      INCLUDE 'IOUNITS.H'
      INTEGER N,K,INDC
      REAL*8  COLD(N)
C     
C  Read unit IIN21 = transport initial conditions
C
      READ(IIN21,*) INDC
      IF (INDC .EQ. 0) THEN
         READ(IIN21,*) COLD(1)
         DO K=2,N
            COLD(K)=COLD(1)
         END DO
      ELSE
         READ(IIN21,*)(COLD(K),K=1,N)
      END IF
C
      RETURN
 1070 FORMAT(/,5X,'CONCENTRAZIONE INIZIALE COSTANTE = ',1PE15.5)
 1090 FORMAT(/,1X,'CONDIZIONE INIZIALE: CONCENTRAZIONE',
     1       /,(4(I6,2X,1PE11.3)))
      END
