C
C*************************** MOISTAB_IRR *******************************
C
C Calculate element-wise water saturation (S_w) needed for the
C irrigation module by interpolating the moisture tables.
C
C***********************************************************************
C
      SUBROUTINE MOISTAB_IRR(NLKP,NT,NTRI,TETRA,PNEW,SWE)
C
      IMPLICIT NONE
      INTEGER  IEL,ISTR,IR,MTYPE,I,INOD
      INTEGER  NLKP,NT,NTRI
      INTEGER  TETRA(5,*)
      REAL*8   PAVG,SWLOC
      REAL*8   PNEW(*),SWE(*)
      real*8   zero,quarter
      parameter (zero=0.0d0,quarter=0.25d0)
C
      DO IEL=1,NT
         ISTR=1+IEL/(NTRI*3)
         IR=MOD(IEL,NTRI*3)
         IF (IR .EQ. 0) ISTR=ISTR-1
         MTYPE=TETRA(5,IEL)
         PAVG = ZERO
         DO I=1,4
            INOD = TETRA(I,IEL)
            PAVG = PAVG + PNEW(INOD)
         END DO
         PAVG = PAVG*QUARTER
         CALL INTERP_IRR(NLKP,ISTR,MTYPE,PAVG,SWLOC)
         SWE(IEL)=SWLOC
      END DO
C
      RETURN
      END
