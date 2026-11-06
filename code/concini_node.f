C
C*************************** CONCINI_NODE ******************************
C
C  reads initial concentration
C
C***********************************************************************
C
      SUBROUTINE CONCINI_NODE(N,CNOLD)
C
      IMPLICIT NONE
      INCLUDE 'IOUNITS.H'
      INTEGER  K
      INTEGER  N,INDC
      REAL*8   CNOLD(*)
C     
C  Read unit IIN21 = transport initial conditions
C
      READ(IIN21,*) INDC
      IF (INDC .EQ. 0) THEN
         READ(IIN21,*) CNOLD(1)
         WRITE(IOUT2,1000) CNOLD(1)
         DO K=2,N
            CNOLD(K)=CNOLD(1)
         END DO
      ELSE
         READ(IIN21,*) (CNOLD(K),K=1,N)
         WRITE(IOUT2,1020) (K,CNOLD(K),K=1,N)
      END IF
C
      RETURN
 1000 FORMAT(/,5X,'Constant initial concentration = ',1PE15.5)
 1020 FORMAT(/,1X,'Initial concentration',/,(4(I6,2X,1PE11.3)))
      END
