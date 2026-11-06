C
C*************************** IRR_READ_BAK ******************************
C
C Special handling of irrigation data for the case where
C IRR_TIM(1) < TIME <= IRR_TIM(2) during back-stepping.
C
C***********************************************************************
C
      SUBROUTINE IRR_READ_BAK(NROW,NCOL,NNOD,NTRI,DOSTEP,
     1                        TRIANG,TP2D,DEM_MAP)
C
      IMPLICIT  NONE
      INCLUDE  'CATHY.H'
      INCLUDE  'IRR_CBLOCK.H'
      INTEGER   IRR_I
      INTEGER   NROW,NCOL,NNOD,NTRI,DOSTEP
      INTEGER   TRIANG(4,*),TP2D(*)
      REAL*8    DEM_MAP(ROWMAX,*)
C     
      IF (IRR_TIM(1) .GE. IRR_TIM(2)) GO TO 800
C
      IRR_I = 1
      CALL IRR_DEMCELL_TO_NODE(NROW,NCOL,NNOD,NTRI,DOSTEP,IRR_I,
     1                         TRIANG,TP2D,DEM_MAP)
C
  800 RETURN
      END
