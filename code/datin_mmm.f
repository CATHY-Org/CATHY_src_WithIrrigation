C
C*************************** DATIN_MMM *********************************
C
C Read in data for the MMM scheme.
C
C***********************************************************************
C
      SUBROUTINE DATIN_MMM
C
      IMPLICIT  NONE
      INCLUDE  'CATHY.H'
      INCLUDE  'IOUNITS.H'
      INCLUDE  'MMM_CBLOCK.H'
      INTEGER   I,J,K
C     
      READ(IIN24,*) MMM_NRC
      IF (MMM_NRC .LT. 1) THEN
         WRITE(IOUT2,2000)
         CALL CLOSIO
         STOP
      END IF
      IF (MMM_NRC .GT. MMM_MAX_NRC) THEN
         WRITE(IOUT2,2020) MMM_NRC, MMM_MAX_NRC
         CALL CLOSIO
         STOP
      END IF
      DO I = 1,MMM_NRC
         READ(IIN24,*) MMM_RC_NAMES(I)
      END DO
      READ(IIN24,*) MMM_NM
      IF (MMM_NM .LT. 1) THEN
         WRITE(IOUT2,2040)
         CALL CLOSIO
         STOP
      END IF
      IF (MMM_NM .GT. MMM_MAX_NM) THEN
         WRITE(IOUT2,2060) MMM_NM, MMM_MAX_NM
         CALL CLOSIO
         STOP
      END IF
      DO I = 1,MMM_NM
         READ(IIN24,*) MMM_MOL_NAMES(I)
      END DO
      READ(IIN24,*) MMM_NBG
      IF (MMM_NBG .GE. 1) THEN
         IF (MMM_NBG .GT. MMM_MAX_NBG) THEN
            WRITE(IOUT2,2080) MMM_NBG, MMM_MAX_NBG
            CALL CLOSIO
            STOP
         END IF
         DO I = 1,MMM_NBG
            READ(IIN24,*) MMM_BG_NAMES(I)
         END DO
      ELSE
         MMM_NBG = 0
      END IF
C
      MMM_NCOMP = MMM_NM + MMM_NBG
C
      DO I = 1,MMM_NCOMP
         READ(IIN24,*) MMM_MOB_FLAG(I)
         IF (MMM_MOB_FLAG(I) .NE. 0) MMM_MOB_FLAG(I) = 1
      END DO
      DO I = 1,MMM_NCOMP
         READ(IIN24,*) MMM_UPTAKE_FLAG(I)
         IF (MMM_UPTAKE_FLAG(I) .NE. 0) MMM_UPTAKE_FLAG(I) = 1
      END DO
C
      READ(IIN24,*) (MMM_CONVERT(I),I=1,MMM_NM)
      DO I = 1,MMM_NRC
         READ(IIN24,*) (MMM_ST_COEFF(I,J),J=1,MMM_NM)
         READ(IIN24,*) MMM_RC_RATE(I)
         READ(IIN24,*) (MMM_INHIB(I,J),J=1,MMM_NM)
         READ(IIN24,*) (MMM_K_M(I,J),J=1,MMM_NM)
         DO J=1,MMM_NM
            READ(IIN24,*) (MMM_INTER(I,J,K),K=1,MMM_NM)
         END DO
      END DO
      IF (MMM_NBG .NE. 0) THEN
         DO I = 1,MMM_NRC
            READ(IIN24,*) (MMM_BIO_Y(I,J),J=1,MMM_NBG)
         END DO
         READ(IIN24,*) (MMM_BMORT(I),I=1,MMM_NBG)
      END IF
C
      WRITE(IOUT82,3000) MMM_NRC
      DO I = 1,MMM_NRC
         WRITE(IOUT82,3020) MMM_RC_NAMES(I)
      END DO
      WRITE(IOUT82,3040) MMM_NM
      DO I = 1,MMM_NM
         WRITE(IOUT82,3020) MMM_MOL_NAMES(I)
      END DO
      WRITE(IOUT82,3060) MMM_NBG
      IF (MMM_NBG .NE. 0) THEN
         WRITE(IOUT82,3080)
         DO I = 1,MMM_NBG
            WRITE(IOUT82,3020) MMM_BG_NAMES(I)
         END DO
      END IF
      WRITE(IOUT82,3100) MMM_NM,MMM_NBG
      DO I = 1,MMM_NCOMP
         WRITE(IOUT82,3110) MMM_MOB_FLAG(I)
      END DO
      WRITE(IOUT82,3120) MMM_NM,MMM_NBG
      DO I = 1,MMM_NCOMP
         WRITE(IOUT82,3110) MMM_UPTAKE_FLAG(I)
      END DO
      WRITE(IOUT82,3130)
      WRITE(IOUT82,3160) (MMM_CONVERT(I),I=1,MMM_NM)
      DO I = 1,MMM_NRC
         WRITE(IOUT82,3140) I
         WRITE(IOUT82,3160) (MMM_ST_COEFF(I,J),J=1,MMM_NM)
         WRITE(IOUT82,3180) MMM_RC_RATE(I)
         WRITE(IOUT82,3160) (MMM_INHIB(I,J),J=1,MMM_NM)
         WRITE(IOUT82,3160) (MMM_K_M(I,J),J=1,MMM_NM)
         DO J=1,MMM_NM
            WRITE(IOUT82,3160) (MMM_INTER(I,J,K),K=1,MMM_NM)
         END DO
      END DO
      IF (MMM_NBG .NE. 0) THEN
         DO I = 1,MMM_NRC
            WRITE(IOUT82,3200) I
            WRITE(IOUT82,3160) (MMM_BIO_Y(I,J),J=1,MMM_NBG)
         END DO
         WRITE(IOUT82,3220)
         WRITE(IOUT82,3160) (MMM_BMORT(I),I=1,MMM_NBG)
      END IF
C
      RETURN
 2000 FORMAT(/,5X,'For MMM_FLAG = 1, MMM_NRC must be at least = 1 ')
 2020 FORMAT(/,5X,'MMM_NRC = ',I7,'  is larger than MMM_MAX_NRC = ',I7)
 2040 FORMAT(/,5X,'For MMM_FLAG = 1, MMM_NM must be at least = 1 ')
 2060 FORMAT(/,5X,'MMM_NM = ',I7,'  is larger than MMM_MAX_NM = ',I7)
 2080 FORMAT(/,5X,'MMM_NBG = ',I7,'  is larger than MMM_MAX_NBG = ',I7)
 3000 FORMAT(  '#  Number of reactions MMM_NRC = ',I7,
     1       /,'#  Reaction names:')
 3020 FORMAT(  '#    ',A60)
 3040 FORMAT(  '#  Number of molecules MMM_NM = ',I7,
     1       /,'#  Molecule names:')
 3060 FORMAT(  '#  Number of bacteria groups MMM_NBG = ',I7)
 3080 FORMAT(  '#  Bacteria group names:')
 3100 FORMAT(  '#  MMM_MOB_FLAG for ',I4,' molecules followed by ',
     1         I4,' bacteria groups:')
 3110 FORMAT(  '#  ',I2)
 3120 FORMAT(  '#  MMM_UPTAKE_FLAG for ',I4,' molecules followed by ',
     1         I4,' bacteria groups:')
 3130 FORMAT(  '#  MMM_CONVERT values for the MMM_NM molecules:')
 3140 FORMAT(  '#  MMM_ST_COEFF (MMM_NM values), MMM_RC_RATE (1 v), ',
     1         'MMM_INHIB (MMM_NM v), MMM_K_M (MMM_NM v), and ',
     2         'MMM_INTER (MMM_NM x MMM_NM v) for reaction ',I4,':')
 3160 FORMAT(  '#  ',10(1PE14.6))
 3180 FORMAT(  '#  ',1PE14.6)
 3200 FORMAT(  '#  MMM_BIO_Y (MMM_NBG values) for reaction ',I4,':')
 3220 FORMAT(  '#  MMM_BMORT values for the MMM_NBG bacteria groups:')
      END
