C
C**************************  DATIN_TRA  ************************************
C
C  initialization of parameters for transport > READ in the transp input file
C
C***************************************************************************
C
      SUBROUTINE DATIN_TRA(NSTR,NZONE,NTRI,NTRI3,NADV,NADVFL,LUMPC,
     1     CDIFFUS,FLAG_TEMP,FLAG_INTERP,FLAG_LIMITER,CFLINP,TETAC,
     2     DIFFUS,GAMMAS,IPARM,ALFAL,ALFAT,KD,LAMBDA,MIXPART,REACFLAG)
C     
      IMPLICIT  NONE
      INCLUDE  'CATHY.H'
      INTEGER   I,J
      INTEGER   NSTR,NZONE,NTRI
      INTEGER   NTRI3,NADV,NADVFL,LUMPC,CDIFFUS
      INTEGER   IPRTCG,IPREC,IMAX,ISOL,IEXIT
      INTEGER   FLAG_TEMP,FLAG_INTERP,FLAG_LIMITER
      INTEGER   IPARM(10)
      LOGICAL   REACFLAG
      REAL*8    CFLINP,TETAC,DIFFUS,GAMMAS,MIXPART
      REAL*8    ALFAL(MAXSTR,MAXZON),ALFAT(MAXSTR,MAXZON)
      REAL*8    KD(MAXSTR,MAXZON),LAMBDA(MAXSTR,MAXZON)
      INCLUDE  'IOUNITS.H'
C      
      REACFLAG=.FALSE.
      READ(IIN20,*) TETAC,LUMPC,CDIFFUS
      WRITE(IOUT2,1000) TETAC,LUMPC,CDIFFUS
      READ(IIN20,*) IPRTCG,IPREC,IMAX,ISOL,IEXIT
      WRITE(IOUT2,1020) IPRTCG,IPREC,IMAX,ISOL,IEXIT
      DO I=1,10
         IPARM(I)=0
      END DO
      IPARM(1) = 98
      IPARM(2) = IPRTCG
      IPARM(3) = IPREC
      IPARM(6) = IMAX
      IPARM(7) = ISOL
      IPARM(10) = IEXIT
      READ(IIN20,*) FLAG_TEMP,FLAG_INTERP,FLAG_LIMITER
      WRITE(IOUT2,1040) FLAG_TEMP,FLAG_INTERP,FLAG_LIMITER
      READ(IIN20,*) CFLINP,NADV,NADVFL         
      WRITE(IOUT2,1060) CFLINP,NADV,NADVFL
      READ(IIN20,*) MIXPART         
      WRITE(IOUT2,1080) MIXPART
      WRITE(IOUT2,1100)
      DO I=1,NSTR
         DO J=1,NZONE
            READ(IIN20,*) ALFAL(I,J),ALFAT(I,J),KD(I,J),LAMBDA(I,J)
            WRITE(IOUT2,1120) I,J,ALFAL(I,J),ALFAT(I,J),KD(I,J),
     1                        LAMBDA(I,J)
            IF (KD(I,J).NE.0) THEN
               REACFLAG=.TRUE.
            END IF
            IF (LAMBDA(I,J).NE.0) THEN
               REACFLAG=.TRUE.
            END IF
         END DO
      END DO
      READ(IIN20,*) DIFFUS, GAMMAS         
      WRITE(IOUT2,1140) DIFFUS, GAMMAS
C
      NTRI3 = 3 * NTRI
C     
      RETURN
 1000 FORMAT(//,
     1       /,5X,'TETAC         = ',1PE15.5,
     2       /,5X,'LUMPC         = ',I6,
     3       /,5X,'CDIFFUS       = ',I6)
 1020 FORMAT(  5X,'IPRTCG        = ',I6,
     1       /,5X,'IPREC         = ',I6,
     2       /,5X,'IMAX          = ',I6,
     3       /,5X,'ISOL          = ',I6,
     4       /,5X,'IEXIT         = ',I6)
 1040 FORMAT(  5X,'FLAG_TEMP     = ',I6,
     1       /,5X,'FLAG_INTERP   = ',I6,
     2       /,5X,'FLAG_LIMITER  = ',I6)
 1060 FORMAT(  5X,'CFLINP        = ',1PE15.5,
     1       /,5X,'NADV          = ',I6,
     2       /,5X,'NADVFL        = ',I6)
 1080 FORMAT(  5X,'MIXPART       = ',1PE15.5)
 1100 FORMAT(//,'  LAYER   ZONE          ALFAL          ALFAT',
     1          '             KD         LAMBDA')
 1120 FORMAT(2(I7),4(2X,1PE13.5))
 1140 FORMAT(/,5X,'DIFFUS        = ',1PE15.5,
     1       /,5X,'GAMMAS        = ',1PE15.5)
      END
