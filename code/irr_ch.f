C
C*************************** IRR_CH ************************************
C
C Calculate water saturation (S_w) corresponding to the current
C pressure heads at the surface nodes.
C
C***********************************************************************
C
      SUBROUTINE IRR_CH(NLKP,N,NT,NTRI,IVGHU,NNOD,
     1                  TETRA,TP,PNEW,SNODI,PNODI,SW,SWE)
C
      IMPLICIT NONE
      INCLUDE 'CATHY.H'
      INCLUDE 'SOILCHAR.H'
      INTEGER  I
      INTEGER  NLKP,N,NT,NTRI,IVGHU,NNOD
      INTEGER  TETRA(5,*),TP(*)
      REAL*8   FVGSE,FXVMC,FHUSE,FBCSE
      REAL*8   PSI,SE
      REAL*8   PNEW(*),SNODI(*),PNODI(*),SW(*),SWE(*)
C
      IF (IVGHU .EQ. -1) THEN
         CALL MOISTAB_IRR(NLKP,NT,NTRI,TETRA,PNEW,SWE)
         CALL ELTNOD(N,NT,TP,TETRA,SWE,SW)
      ELSE IF (IVGHU .EQ. 0) THEN
         DO I=1,NNOD
            PSI      = PNEW(I)
            SE       = FVGSE(PSI,I)
            SW(I)    = VGPNOT(I)*SE + VGRMC(I)/PNODI(I)
         END DO 
      ELSE IF (IVGHU .EQ. 1) THEN
         DO I=1,NNOD
            PSI      = PNEW(I)
            SW(I)    = FXVMC(PSI,SNODI(I),PNODI(I),I)/PNODI(I)
         END DO
      ELSE IF (IVGHU .EQ. 2) THEN
         DO I=1,NNOD
            PSI      = PNEW(I)
            SE       = FHUSE(PSI)
            SW(I)    = HUSWR1*SE + HUSWR
         END DO
      ELSE IF (IVGHU .EQ. 3) THEN
         DO I=1,NNOD
            PSI      = PNEW(I)
            SE       = FHUSE(PSI)
            SW(I)    = HUSWR1*SE + HUSWR
         END DO
      ELSE 
         DO I=1,NNOD
            PSI      = PNEW(I)
            SE       = FBCSE(PSI)
            SW(I)    = BCPORM(I)*SE + BCRMC/PNODI(I)
         END DO 
      END IF
C
      RETURN
      END
