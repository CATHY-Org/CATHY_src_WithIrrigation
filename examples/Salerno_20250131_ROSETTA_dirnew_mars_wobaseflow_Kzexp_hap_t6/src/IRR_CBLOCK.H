C
C*************************** COMMON BLOCK IRR_CBLOCK *******************
C            
C Common block for all shared variables associated specifically with
C the irrigation management scheme.
C            
C***********************************************************************
C
      INTEGER IRR_FLAG,IRR_HT,IRR_HS
      REAL*8  IRR_SW,IRR_POT_ATM,IRR_POT_IRRWATER
      REAL*8  IRR_POT_ATM_IRR,IRR_IRRWATER_NOT_USED
      REAL*8  IRR_POT_ATM_CUMV,IRR_POT_IRRWATER_CUMV
      REAL*8  IRR_POT_ATM_IRR_CUMV,IRR_IRRWATER_NOT_USED_CUMV
      REAL*8  IRR_TIM(3),IRR_NNOD(NODMAX),IRR_NELT(NTRMAX)
      REAL*8  IRR_ATMPOT_SAV(NODMAX),IRR_INP_RAST(3,ROWMAX,COLMAX)
C
      COMMON  /IRR_CBLOCK/
     1        IRR_SW,IRR_POT_ATM,IRR_POT_IRRWATER,
     2        IRR_POT_ATM_IRR,IRR_IRRWATER_NOT_USED,
     3        IRR_POT_ATM_CUMV,IRR_POT_IRRWATER_CUMV,
     4        IRR_POT_ATM_IRR_CUMV,IRR_IRRWATER_NOT_USED_CUMV,
     5        IRR_TIM,IRR_NNOD,IRR_NELT,
     6        IRR_ATMPOT_SAV,IRR_INP_RAST,
     7        IRR_FLAG,IRR_HT,IRR_HS
