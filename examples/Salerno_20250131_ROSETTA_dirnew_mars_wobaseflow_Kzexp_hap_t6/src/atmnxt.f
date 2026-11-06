C
C**************************  ATMNXT ************************************
C
C  input (if necessary) and interpolate atmospheric boundary 
C  conditions for next time level
C
C***********************************************************************
C
      SUBROUTINE ATMNXT(NNOD,N,NT,NTRI,IVGHU,NLKP,NSF,NP,NQ,NSTEP,
     1                  HSPATM,HTIATM,IETO,TIME,DELTAT,
     2                  TETRA,TP,CONTP,CONTQ,IFATM,
     3                  NSFNUM,NSFNOD,
     4                  SNODI,PNODI,SW,SWE,ARENOD,ATMPOT,ATMACT,
     5                  ATMTIM,ATMINP,PNEW)
C
      IMPLICIT  NONE
      INCLUDE  'CATHY.H'
      INTEGER   I,J,INOD
      INTEGER   NNOD,N,NT,NTRI,IVGHU,NLKP,NSF,NP,NQ,NSTEP
      INTEGER   HSPATM,HTIATM,IETO
      INTEGER   TETRA(5,*),TP(*),CONTP(*),CONTQ(*),IFATM(*)
      INTEGER   NSFNUM(*),NSFNOD(NSFMAX,*)
      REAL*8    TIMEIN,SLOPE
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
      IF (HTIATM .NE. 0) GO TO 800
  200 IF (TIME .LE. ATMTIM(3)) GO TO 300
      ATMTIM(1)=ATMTIM(2)
      ATMTIM(2)=ATMTIM(3)
      DO I=1,NNOD
         ATMINP(1,I)=ATMINP(2,I)
         ATMINP(2,I)=ATMINP(3,I)
      END DO
      READ(IIN11,*,END=700) TIMEIN
      ATMTIM(3)=TIMEIN
      IF (HSPATM .EQ. 0) THEN
         READ(IIN11,*) (ATMINP(3,I),I=1,NNOD)
      ELSE
         READ(IIN11,*) ATMINP(3,1)
         DO I=2,NNOD
            ATMINP(3,I)=ATMINP(3,1)
         END DO
      END IF
      GO TO 200
  300 CONTINUE
      IF (ATMTIM(3) .GT. ATMTIM(2)) THEN
         DO I=1,NNOD
            SLOPE=(ATMINP(3,I) - ATMINP(2,I))/(ATMTIM(3) - ATMTIM(2))
            IF (IETO.NE.0) SLOPE=0.0D0
            ATMPOT(I)=(ATMINP(2,I) + SLOPE*(TIME - ATMTIM(2)))*ARENOD(I)
         END DO
      ELSE
         DO I=1,NNOD
            ATMPOT(I)=ATMINP(3,I)*ARENOD(I)
         END DO
      END IF
      GO TO 800
C
  700 HTIATM=1
      GO TO 300
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
C  resets IFATM to exclude non atmospheric surface nodes
c  in case some boundary nodes have changed in BCNXT
C
      DO I=1,NP
         INOD=CONTP(I)
         IF (INOD .LE. NNOD) THEN
            IFATM(INOD) =-1
         END IF
      END DO
      DO I=1,NQ
         INOD=CONTQ(I)
         IF (INOD .LE. NNOD) THEN
            IFATM(INOD) =-1
         END IF
      END DO
      DO I=1,NSF
         DO J=1,NSFNUM(I)
            INOD=NSFNOD(I,J)
            IF (INOD .LE. NNOD) THEN
               IFATM(INOD) =-1
            END IF
         END DO
      END DO
C
      IF (IRR_FLAG .EQ. 1) THEN
         J = 0
         DO I = 1,NNOD
            IF (IFATM(I) .EQ. -1) J = J + 1
         END DO
         IF (J .NE. 0) THEN
            WRITE(IOUT33,1220) J
         END IF
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
 1220 FORMAT(/,5X,'Warning: the irrigation module is selected but',
     1            ' there are ',I7,'  surface nodes that are not',
     2            ' atmospheric forcing BC nodes')
      END
