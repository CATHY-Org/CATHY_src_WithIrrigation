C
C**************************  ATMBAK ************************************
C
C  special handling of atmospheric boundary conditions for
C  the case where ATMTIM(1) < TIME <= ATMTIM(2) during back-stepping
C
C***********************************************************************
C
      SUBROUTINE ATMBAK(NNOD,N,NT,NTRI,IVGHU,NLKP,NSTEP,
     1                  IETO,TIME,DELTAT,
     2                  TETRA,TP,IFATM,
     3                  SNODI,PNODI,SW,SWE,ARENOD,ATMPOT,ATMACT,
     4                  ATMTIM,ATMINP,PNEW)
C
      IMPLICIT  NONE
      INCLUDE  'CATHY.H'
      INTEGER   I
      INTEGER   NNOD,N,NT,NTRI,IVGHU,NLKP,NSTEP,IETO
      INTEGER   TETRA(5,*),TP(*),IFATM(*)
      REAL*8    SLOPE
      REAL*8    TIME,DELTAT
      REAL*8    SNODI(*),PNODI(*),SW(*),SWE(*)
      REAL*8    ARENOD(*),ATMPOT(*),ATMACT(*)
      REAL*8    ATMTIM(*),ATMINP(3,*),PNEW(*)
      INCLUDE  'IOUNITS.H'
      INCLUDE  'SOILCHAR.H'
      INCLUDE  'IRR_CBLOCK.H'
C
      IF (IRR_FLAG .EQ. 1) THEN
         CALL VCOPYR(NNOD,ATMPOT,IRR_ATMPOT_SAV)
      END IF
C
C  we don't need to do anything if first time step or if atmospheric
C  inputs are constant in time
C
      IF (ATMTIM(1) .GE. ATMTIM(2)) GO TO 800
C
      DO I=1,NNOD
         SLOPE=(ATMINP(2,I) - ATMINP(1,I))/(ATMTIM(2) - ATMTIM(1))
         IF (IETO.NE.0) SLOPE=0.0d0
         ATMPOT(I)=(ATMINP(1,I) + SLOPE*(TIME - ATMTIM(1)))*ARENOD(I)
      END DO
C
  800 CONTINUE
C
C Irrigation management
C
      IF (IRR_FLAG .EQ. 1) THEN
         CALL VCOPYR(NNOD,IRR_ATMPOT_SAV,ATMPOT)
         CALL IRR_CH(NLKP,N,NT,NTRI,IVGHU,NNOD,
     1               TETRA,TP,PNEW,SNODI,PNODI,SW,SWE)
         IRR_POT_ATM = 0.0D0
         IRR_POT_IRRWATER = 0.0D0
         IRR_POT_ATM_IRR = 0.0D0
         IRR_IRRWATER_NOT_USED = 0.0D0
         DO I=1,NNOD
            IRR_POT_ATM = IRR_POT_ATM + ATMPOT(I)
            IRR_POT_IRRWATER = IRR_POT_IRRWATER +
     1                         (IRR_NNOD(I) * ARENOD(I))
            IF (SW(I) .LT. IRR_SW) THEN
               IRR_POT_ATM_IRR = IRR_POT_ATM_IRR + ATMPOT(I) +
     1                           (IRR_NNOD(I) * ARENOD(I))
               ATMPOT(I) = ATMPOT(I) + (IRR_NNOD(I) * ARENOD(I))
            ELSE
               IRR_POT_ATM_IRR = IRR_POT_ATM_IRR + ATMPOT(I)
               IRR_IRRWATER_NOT_USED = IRR_IRRWATER_NOT_USED +
     1                                 (IRR_NNOD(I) * ARENOD(I))
            ENDIF
         END DO
         IRR_POT_ATM_CUMV = IRR_POT_ATM_CUMV +
     1                      (IRR_POT_ATM * DELTAT)
         IRR_POT_IRRWATER_CUMV = IRR_POT_IRRWATER_CUMV +
     1                           (IRR_POT_IRRWATER * DELTAT)
         IRR_POT_ATM_IRR_CUMV = IRR_POT_ATM_IRR_CUMV +
     1                          (IRR_POT_ATM_IRR * DELTAT)
         IRR_IRRWATER_NOT_USED_CUMV = IRR_IRRWATER_NOT_USED_CUMV +
     1                                (IRR_IRRWATER_NOT_USED * DELTAT)
         WRITE(IOUT33,1200) NSTEP, DELTAT, TIME, IRR_POT_ATM,
     1                      IRR_POT_IRRWATER, IRR_POT_ATM_IRR,
     2                      IRR_IRRWATER_NOT_USED, IRR_POT_ATM_CUMV,
     3                      IRR_POT_IRRWATER_CUMV, IRR_POT_ATM_IRR_CUMV,
     4                      IRR_IRRWATER_NOT_USED_CUMV
      END IF            
C
      DO I=1,NNOD
         IF (IFATM(I) .EQ. 0) THEN
            IF (ATMPOT(I).GE.0.0d0) THEN
                ATMACT(I)=ATMPOT(I)
            ELSE
                ATMACT(I)=(1.0d0-SCF)*ATMPOT(I)
            END IF
         END IF
      END DO
C
      RETURN
 1200 FORMAT(I11,1PE14.6,1PE21.14,8(1PE14.6))
      END
