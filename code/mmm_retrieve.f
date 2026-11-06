C
C*************************** MMM_RETRIEVE ******************************
C
C Retrieve variables for the current component (or species) at the top
C of the "MMM_NCOMP loop" in cathy_main, which is the start of the block
C that will solve the ADR parts of the subsurface transport equation. 
C At the start of a new time step, the solute concentration variables
C retrieved will have been updated by the MMM scheme that was executed
C at the end of the previous time step.
C
C***********************************************************************
C
      SUBROUTINE MMM_RETRIEVE(iii,N,NT,NNOD,NCELL,
     1                        MASSINTOTADV,MASSOUTTOTADV,
     2                        MASSINTOTCUM,MASSOUTTOTCUM,
     3                        CNNEW,CNOLD,CNEW,COLD,
     4                        COLDOLD,CDIFF,FLUX,
     5                        CONCNOD_PERDIR,CONCNOD_PERDIR_TIMEP,
     6                        CONCNOD_PERDIR_INT,TRAFLNOD,
     7                        TRAFLNOD_FLOW,SOURCE_MIXING,
     8                        CONCNODUPD,MIX_CORRECT)
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
         CNNEW(i) = MMM_CNNEW(iii,i)
         CNOLD(i) = MMM_CNOLD(iii,i)
      end do
      do i = 1,NT
         CNEW(i) = MMM_CNEW(iii,i)
         COLD(i) = MMM_COLD(iii,i)
         COLDOLD(i) = MMM_COLDOLD(iii,i)
         CDIFF(i) = MMM_CDIFF(iii,i)
         FLUX(i) = MMM_FLUX(iii,i)
      end do
      do i = 1,NNOD
         CONCNOD_PERDIR(i) = MMM_CONCNOD_PERDIR(iii,i)
         CONCNOD_PERDIR_TIMEP(i) = MMM_CONCNOD_PERDIR_TIMEP(iii,i)
         CONCNOD_PERDIR_INT(i) = MMM_CONCNOD_PERDIR_INT(iii,i)
         TRAFLNOD(i) = MMM_TRAFLNOD(iii,i)
         TRAFLNOD_FLOW(i) = MMM_TRAFLNOD_FLOW(iii,i)
         SOURCE_MIXING(i) = MMM_SOURCE_MIXING(iii,i)
         CONCNODUPD(i) = MMM_CONCNODUPD(iii,i)
      end do
      do i = 1,NCELL
         MIX_CORRECT(i) = MMM_MIX_CORRECT(iii,i)
      end do
      MASSINTOTADV = MMM_MASSINTOTADV(iii)
      MASSOUTTOTADV = MMM_MASSOUTTOTADV(iii)
      MASSINTOTCUM = MMM_MASSINTOTCUM(iii)
      MASSOUTTOTCUM = MMM_MASSOUTTOTCUM(iii)
C
      RETURN
      END
