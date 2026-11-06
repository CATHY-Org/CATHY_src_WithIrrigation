C
C*************************** LEL_KEL ***********************************
C
C  Set up element-wise LEL and KEL arrays from the input arrays
C  LAMBDA and KD.
C
C***********************************************************************
C
      SUBROUTINE LEL_KEL(NT,NTRI,TETRA,LEL,LAMBDA,KEL,KD)
C
      IMPLICIT NONE
      INCLUDE 'CATHY.H'
      INTEGER  K,IR,ISTR,MTYPE
      INTEGER  NT,NTRI
      INTEGER  TETRA(5,*)
      REAL*8   KD(MAXSTR,*),LAMBDA(MAXSTR,*)
      REAL*8   LEL(*),KEL(*)
C
      DO K=1,NT
         MTYPE=TETRA(5,K)
         ISTR=1+K/(NTRI*3)
         IR=MOD(K,NTRI*3)
         IF (IR .EQ. 0) ISTR=ISTR-1
         LEL(K)=LAMBDA(ISTR,MTYPE)
         KEL(K)=KD(ISTR,MTYPE)
      END DO
C
      RETURN
      END
