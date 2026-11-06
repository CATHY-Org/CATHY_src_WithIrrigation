C
C*************************** BKSTEP ************************************
C
C  Reset variables & arrays for back-stepping.
C
C***********************************************************************
C
      SUBROUTINE BKSTEP(IPRT1,N,NSTR,NDIR,NDIRC,NP,NNEU,NNEUC,NQ,HSPATM,
     1                  HTIATM,IETO,HTIDIR,HTINEU,ITER,NITERT,KBACKT,
     2                  KBACK,NNOD,NSF,NUMRES,NCELNL,
     3                  CONTP,CONTQ,IFATM,IFATMP,NSFNUM,SFEX,SFEXIT,
     4                  SFEXP,SURF,PONDING,PONDP,DTGMIN,
     5                  TIME,TIMEP,DELTAT,DTMIN,DTREDM,DTREDS,TETAF,
     6                  OVFLNOD,OVFLP,PRESC,PTIM,PINP,Q,QTIM,QINP,
     7                  ATMTIM,ATMINP,ARENOD,
     8                  POLD,PTOLD,PNEW,PTNEW,PTIMEP,
     9                  ATMACT,ATMPOT,ATMOLD,
     A                  QPNEW,QPOLD,SFQ,SFQP,ANP,ANQ,ACONTP,ACONTQ,
     B                  NSFNOD,SFV,SFVNUM,SFVNOD,SFVTIM,TRANSP,
     C                  NT,NTRI,IVGHU,NLKP,NSTEP,TETRA,TP,
     D                  SNODI,PNODI,SW,SWE,
     E                  NROW,NCOL,DOSTEP,TRIANG,TP2D,DEM_MAP)
C
      IMPLICIT NONE
      INCLUDE 'CATHY.H'
      INTEGER  I,J
      INTEGER  IPRT1,N,NSTR
      INTEGER  HSPATM,HTIATM,IETO,HTIDIR,HTINEU
      INTEGER  ANP,ANQ
      INTEGER  NNOD,NSF,NUMRES,NCELNL
      INTEGER  ITER,NITERT,KBACKT,KBACK
      INTEGER  NT,NTRI,IVGHU,NLKP,NSTEP
      INTEGER  NROW,NCOL,DOSTEP
      INTEGER  NDIR(3),NDIRC(3),NP(3),NQ(3),NNEU(3),NNEUC(3)
      INTEGER  NSFNOD(NSFMAX,NNSFMX)
      INTEGER  ACONTP(*),ACONTQ(*)
      INTEGER  CONTP(3,*),CONTQ(3,*)
      INTEGER  IFATM(*),IFATMP(*)
      INTEGER  NSFNUM(*)
      INTEGER  SFEX(NSFMAX,*),SFEXIT(NSFMAX,*),SFEXP(NSFMAX,*)
      INTEGER  SFV(2),SFVNUM(2,*),SFVNOD(2,NSFMAX,*)
      INTEGER  TETRA(5,*),TP(*)
      INTEGER  TRIANG(4,*),TP2D(*)
      LOGICAL  SURF,PONDING,PONDP,DTGMIN,TRANSP
      REAL*8   TIME,TIMEP,DELTAT,DTMIN,DTREDM,DTREDS
      REAL*8   TETAF
      REAL*8   OVFLNOD(*),OVFLP(*)
      REAL*8   PRESC(*),PTIM(*),PINP(3,*)
      REAL*8   Q(*),QTIM(*),QINP(3,*)
      REAL*8   ATMTIM(*),ATMINP(3,*)
      REAL*8   ARENOD(*)
      REAL*8   POLD(*),PTOLD(*),PNEW(*),PTNEW(*),PTIMEP(*)
      REAL*8   ATMACT(*),ATMPOT(*),ATMOLD(*)
      REAL*8   QPNEW(*),QPOLD(*),SFQ(NSFMAX,*),SFQP(NSFMAX,*)
      REAL*8   SFVTIM(2)
      REAL*8   SNODI(*),PNODI(*),SW(*),SWE(*)
      REAL*8   DEM_MAP(ROWMAX,*)
      INCLUDE 'SOILCHAR.H'
      INCLUDE 'SURFWATER.H'
      INCLUDE 'TRANSPSURF.H'
      INCLUDE 'IRR_CBLOCK.H'
      INCLUDE 'IOUNITS.H'
C           
      CALL VCOPYR(N,PNEW,PTIMEP)   
      CALL VCOPYR(ANP,QPNEW,QPOLD)
      DO I=1,NSF
         DO J=1,NSFNUM(I)
            SFEX(I,J)=SFEXP(I,J)
            SFEXIT(I,J)=SFEXP(I,J)
         END DO
      END DO
      DO I=1,NSF 
         DO J=1,NSFNUM(I)
            SFQ(I,J)=SFQP(I,J)
         END DO
      END DO
      CALL VCOPYI(NNOD,IFATM,IFATMP)
      CALL VCOPYR(NNOD,ATMACT,ATMOLD)
      TIME=TIMEP
      DELTAT=DELTAT*DTREDM - DTREDS
      IF (DELTAT .LE. DTMIN) THEN
         DELTAT=DTMIN
         DTGMIN=.FALSE. 
      ELSE
         DTGMIN=.TRUE.
      END IF
      TIME=TIME + DELTAT
      KBACKT=KBACKT + 1
      KBACK=KBACK + 1
      ITER=1  
      NITERT=0
C  
C  we need to check whether PTIM(1) < TIME <= PTIM(2), in which
C  case we need special handling for the back-stepping case
C        
C     IF (NP .NE. 0) THEN
         IF (TIME .GT. PTIM(2)) THEN
            CALL BCNXT('BACK-STEP NATM,NSF DIR',IIN13,IOUT2,IOUT19,NNOD,
     1                 NPMAX,IPRT1,NDIR,NDIRC,NP,NSTR,HTIDIR,TIME,
     2                 PTIM,PINP,CONTP,PRESC,ANP,ACONTP)
         ELSE
            CALL BCBAK('BACK-STEP NATM,NSF DIR',IOUT19,
     1                 IPRT1,NP,TIME,PTIM,PINP,CONTP,PRESC,ANP,ACONTP)
         END IF
C     END IF
C        
C  we need to check whether QTIM(1) < TIME <= QTIM(2), in which
C  case we need special handling for the back-stepping case
C        
C     IF (NQ .NE. 0) THEN
C     CALL INIT0I(3,ZERO)
C     CALL INIT0I(3,ZEROC)
         IF (TIME .GT. QTIM(2)) THEN
            CALL BCNXT('BACK-STEP NATM,NSF NEU',IIN14,IOUT2,IOUT20,NNOD,
     1                 NQMAX,IPRT1,NNEU,NNEUC,NQ,NSTR,HTINEU,TIME,
     2                 QTIM,QINP,CONTQ,Q,ANQ,ACONTQ)
         ELSE
            CALL BCBAK('BACK-STEP NATM,NSF NEU',IOUT20,
     1                 IPRT1,NQ,TIME,QTIM,QINP,CONTQ,Q,ANQ,ACONTQ)
         END IF
C     END IF
C           
C  we need to check whether ATMTIM(1) < TIME <= ATMTIM(2), in which
C  case we need special handling for the back-stepping case.
C  Note: SWITCH_OLD is the version of SWITCH for the uncoupled,
C  subsurface flow only version of the model. It should be superceded
C  by the new ADRSTN routine, but for the time being we keep the old
C  version of SWITCH active as well.
C           
         if (time.lt.sfvtim(1)) then
             call sfvbak(nsfmax,nnsfmx,nsf,nsfnum,nsfnod,
     1                   sfv,sfvnum,sfvnod)
         else
             call sfvrec(nsfmax,nnsfmx,nsf,nsfnum,nsfnod,
     1                   sfv,sfvnum,sfvnod)
         end if
      IF (TIME .GT. ATMTIM(2)) THEN
         IF (IRR_FLAG .EQ. 1) THEN   
            CALL IRR_READ_NXT(NROW,NCOL,NNOD,NTRI,DOSTEP,
     1                        TIME,TRIANG,TP2D,DEM_MAP)
         END IF   
         CALL ATMNXT(NNOD,N,NT,NTRI,IVGHU,NLKP,NSF,ANP,ANQ,NSTEP,
     1               HSPATM,HTIATM,IETO,TIME,DELTAT,
     2               TETRA,TP,ACONTP,ACONTQ,IFATM,
     3               NSFNUM,NSFNOD,
     4               SNODI,PNODI,SW,SWE,ARENOD,ATMPOT,ATMACT,
     5               ATMTIM,ATMINP,PNEW)
      ELSE
         IF (IRR_FLAG .EQ. 1) THEN   
            CALL IRR_READ_BAK(NROW,NCOL,NNOD,NTRI,DOSTEP,
     1                        TRIANG,TP2D,DEM_MAP)
         END IF   
         CALL ATMBAK(NNOD,N,NT,NTRI,IVGHU,NLKP,NSTEP,
     1               IETO,TIME,DELTAT,
     2               TETRA,TP,IFATM,
     3               SNODI,PNODI,SW,SWE,ARENOD,ATMPOT,ATMACT,
     4               ATMTIM,ATMINP,PNEW)
      END IF
      IF (.NOT. SURF) THEN
         CALL SWITCH_OLD(NNOD,IFATM,ATMACT,ATMPOT,PNEW)
      ELSE                
         CALL ADRSTN(NNOD,IFATM,ATMPOT,ATMACT,PNEW)
      END IF
C
C
      CALL VCOPYR(N,POLD,PNEW)
      CALL WEIGHT(N,TETAF,PNEW,PTIMEP,PTNEW)
      CALL VCOPYR(N,PTOLD,PTNEW)
C    
C  variables and arrays for SURF_ROUTE
C
      IF (SURF) THEN
         PONDING = PONDP
         CALL VCOPYR(NNOD,OVFLNOD,OVFLP)
         AK_MAX = AK_MAX_p
         CALL VCOPYR(MAXCEL,Q_IN_KK_SN,Q_IN_KK_SN_P)
         CALL VCOPYR(MAXCEL,Q_OUT_KK_SN_1,Q_OUT_KK_SN_1_P)
         CALL VCOPYR(MAXCEL,Q_OUT_KK_SN_2,Q_OUT_KK_SN_2_P)
         CALL VCOPYR(MAXCEL,VOLUME_KK_SN,VOLUME_KK_SN_P)
         CALL VCOPYR(NUMRES,H_POOL_KK_VEC,H_POOL_KK_VEC_P)
         IF (TRANSP) THEN
            CALL VCOPYR(MAXCEL,QMASS_IN_KK_SN,QMASS_IN_KK_SN_P)
            CALL VCOPYR(MAXCEL,QMASS_OUT_KK_SN_1,QMASS_OUT_KK_SN_1_P)
            CALL VCOPYR(MAXCEL,QMASS_OUT_KK_SN_2,QMASS_OUT_KK_SN_2_P)
            CALL VCOPYR(MAXCEL,MASS_KK_SN,MASS_KK_SN_P)
         END IF
      END IF
C
      RETURN
      END
