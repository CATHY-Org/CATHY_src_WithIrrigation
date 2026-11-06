C
C*************************** MMM_SAVE **********************************
C
C Save MMM variables ahead of the first time step (i.e., at the end of
C the "MMM_NCOMP initialization" block), and also at the bottom of the
C "MMM_NCOMP loop" in cathy_main as these have just been updated
C by solving the ADR parts of the subsurface transport equation.
C
C***********************************************************************
C
      SUBROUTINE MMM_SAVE(iii,N,NT,NNOD,NCELL,
     1                    MASSINTOTADV,MASSOUTTOTADV,
     2                    MASSINTOTCUM,MASSOUTTOTCUM,
     3                    CNNEW,CNOLD,CNEW,COLD,
     4                    COLDOLD,CDIFF,FLUX,
     5                    CONCNOD_PERDIR,CONCNOD_PERDIR_TIMEP,
     6                    CONCNOD_PERDIR_INT,TRAFLNOD,
     7                    TRAFLNOD_FLOW,SOURCE_MIXING,
     8                    CONCNODUPD,MIX_CORRECT)
C
      IMPLICIT  NONE
      INCLUDE  'CATHY.H'
      INCLUDE  'MMM_CBLOCK.H'
      INTEGER   i
      INTEGER   iii,N,NT,NNOD,NCELL
      REAL*8    MASSINTOTADV,MASSOUTTOTADV
      REAL*8    MASSINTOTCUM,MASSOUTTOTCUM
      REAL*8    CNNEW(*),CNOLD(*),CNEW(*),COLD(*)
      REAL*8    COLDOLD(*),CDIFF(*),FLUX(*)
      REAL*8    CONCNOD_PERDIR(*),CONCNOD_PERDIR_TIMEP(*)
      REAL*8    CONCNOD_PERDIR_INT(*),TRAFLNOD(*)
      REAL*8    TRAFLNOD_FLOW(*),SOURCE_MIXING(*)
      REAL*8    CONCNODUPD(*),MIX_CORRECT(*)
C     
      do i = 1,N
         MMM_CNNEW(iii,i) = CNNEW(i)
         MMM_CNOLD(iii,i) = CNOLD(i)
      end do
      do i = 1,NT
         MMM_CNEW(iii,i) = CNEW(i)
         MMM_COLD(iii,i) = COLD(i)
         MMM_COLDOLD(iii,i) = COLDOLD(i)
         MMM_CDIFF(iii,i) = CDIFF(i)
         MMM_FLUX(iii,i) = FLUX(i)
      end do
      do i = 1,NNOD
         MMM_CONCNOD_PERDIR(iii,i) = CONCNOD_PERDIR(i)
         MMM_CONCNOD_PERDIR_TIMEP(iii,i) = CONCNOD_PERDIR_TIMEP(i)
         MMM_CONCNOD_PERDIR_INT(iii,i) = CONCNOD_PERDIR_INT(i)
         MMM_TRAFLNOD(iii,i) = TRAFLNOD(i)
         MMM_TRAFLNOD_FLOW(iii,i) = TRAFLNOD_FLOW(i)
         MMM_SOURCE_MIXING(iii,i) = SOURCE_MIXING(i)
         MMM_CONCNODUPD(iii,i) = CONCNODUPD(i)
      end do
      do i = 1,NCELL
         MMM_MIX_CORRECT(iii,i) = MIX_CORRECT(i)
      end do
      MMM_MASSINTOTADV(iii) = MASSINTOTADV
      MMM_MASSOUTTOTADV(iii) = MASSOUTTOTADV
      MMM_MASSINTOTCUM(iii) = MASSINTOTCUM
      MMM_MASSOUTTOTCUM(iii) = MASSOUTTOTCUM
C
      RETURN
      END
