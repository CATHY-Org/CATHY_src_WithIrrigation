C
C*************************** IRR_READ_NXT ******************************
C
C Read in (if necessary) irrigation data for the next time level.
C
C***********************************************************************
C
      SUBROUTINE IRR_READ_NXT(NROW,NCOL,NNOD,NTRI,DOSTEP,
     1                        TIME,TRIANG,TP2D,DEM_MAP)
C
      IMPLICIT  NONE
      INCLUDE  'CATHY.H'
      INCLUDE  'IOUNITS.H'
      INCLUDE  'IRR_CBLOCK.H'
      INTEGER   I,J,IRR_I
      INTEGER   NROW,NCOL,NNOD,NTRI,DOSTEP
      INTEGER   TRIANG(4,*),TP2D(*)
      REAL*8    TIMEIN,KRATE
      REAL*8    TIME
      REAL*8    DEM_MAP(ROWMAX,*)
C     
      IF (IRR_HT .EQ. 1) GO TO 800
  200 IF (TIME .LE. IRR_TIM(3)) GO TO 300
      IRR_TIM(1)=IRR_TIM(2)
      IRR_TIM(2)=IRR_TIM(3)
      DO I = 1,NROW
         DO J = 1,NCOL
            IRR_INP_RAST(1,I,J) = IRR_INP_RAST(2,I,J)
            IRR_INP_RAST(2,I,J) = IRR_INP_RAST(3,I,J)
         END DO
      END DO
      READ(IINDEM12,*,END=400) TIMEIN
      IRR_TIM(3)=TIMEIN
      IF (IRR_HS .EQ. 0) THEN
         DO I = 1,NROW
            READ(IINDEM12,*) (IRR_INP_RAST(3,I,J),J=1,NCOL)
         END DO
      ELSE
         READ(IINDEM12,*) KRATE
         DO I = 1,NROW
            DO J = 1,NCOL
               IRR_INP_RAST(3,I,J) = KRATE
            END DO
         END DO
      END IF
      GO TO 200
  300 CONTINUE
      IF (IRR_TIM(3) .GT. IRR_TIM(2)) THEN
         IRR_I = 2
      ELSE
         IRR_I = 3
      END IF
      CALL IRR_DEMCELL_TO_NODE(NROW,NCOL,NNOD,NTRI,DOSTEP,IRR_I,
     1                         TRIANG,TP2D,DEM_MAP)
      GO TO 800
C
  400 IRR_HT = 1
      GO TO 300
C
  800 RETURN
      END
