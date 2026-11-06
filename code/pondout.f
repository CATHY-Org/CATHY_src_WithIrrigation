C
C*************************** PONDOUT ***********************************
C
C  Performs the same calculations as subroutine ALTEZZE, and outputs
C  the results.
C  (cxp: fix redundancy with ALTEZZE)
C
C***********************************************************************
C
      SUBROUTINE PONDOUT(TIPO_R,DELTA_T,NUM_R,NSTEP,TIME) 
C
      IMPLICIT NONE
      INCLUDE 'CATHY.H'
      INCLUDE 'IOUNITS.H'
      INCLUDE 'RIVERNETWORK.H'
      INCLUDE 'SURFWATER.H'
      INTEGER INDEX_C,AA
      INTEGER NSTEP
      INTEGER TIPO_R(*),NUM_R(*)
      REAL*8  DELTA_V,VOLUME_KK,VOLUME_KKP1
      REAL*8  SURFACE_WATER,H_WATER_KKP1,Q_IN_KK,Q_IN_KKP1
      REAL*8  Q_OUT_KK_1,Q_OUT_KK_2,Q_OUT_KKP1_1,Q_OUT_KKP1_2
      REAL*8  DELTA_T,TIME
C
      WRITE(IOUT71,1010) NSTEP, TIME
      WRITE(IOUT71,2000) 
      
      DO INDEX_C=1,MAXCEL
 
       IF(TIPO_R(INDEX_C).EQ.0) THEN

          Q_IN_KK=Q_IN_KK_SN(INDEX_C)
          Q_IN_KKP1=Q_IN_KKP1_SN(INDEX_C)
          Q_OUT_KK_1=Q_OUT_KK_SN_1(INDEX_C)
          Q_OUT_KK_2=Q_OUT_KK_SN_2(INDEX_C)
          Q_OUT_KKP1_1=Q_OUT_KKP1_SN_1(INDEX_C)
          Q_OUT_KKP1_2=Q_OUT_KKP1_SN_2(INDEX_C)
          SURFACE_WATER=SURFACE_WATER_SN(INDEX_C)

C   INITIAL VOLUME

          VOLUME_KK=VOLUME_KK_SN(INDEX_C)

C   DELTA_V CALCULATION

          DELTA_V= (Q_IN_KK+Q_IN_KKP1)/2*DELTA_T +
     &              SURFACE_WATER*DELTA_T -
     &             (Q_OUT_KK_1+Q_OUT_KK_2)/2*DELTA_T -
     &             (Q_OUT_KKP1_1+Q_OUT_KKP1_2)/2*DELTA_T

C   FINAL VOLUME 
 
          VOLUME_KKP1=VOLUME_KK + DELTA_V
          VOLUME_KKP1_SN(INDEX_C)=VOLUME_KKP1

C   CALCULATION OF WATER HEIGHTS

          IF(VOLUME_KKP1 .GE. 0.D0) THEN
             H_WATER_KKP1=VOLUME_KKP1/(DELTA_X*DELTA_Y)
          ELSE
             VOLUME_KKP1_SN(INDEX_C)=0.0D0
             H_WATER_KKP1=0.0D0
          END IF

          H_WATER_KKP1_SN(INDEX_C)=H_WATER_KKP1
          
       
       ELSE IF(TIPO_R(INDEX_C).LT.1000) THEN
          
          H_WATER_KKP1_SN(INDEX_C)=H_POOL_KKP1_VEC(NUM_R(INDEX_C))-
     &                            ELEVATION(INDEX_C) 

          IF(H_WATER_KKP1_SN(INDEX_C).LT.0) THEN
              H_WATER_KKP1_SN(INDEX_C)=0.0D0
          END IF

       ELSE  
 
          AA=NUM_R(TIPO_R(INDEX_C)-1000)
          
          H_WATER_KKP1=H_POOL_KKP1_VEC(AA)-
     &                 ELEVATION(INDEX_C)

          IF(H_WATER_KKP1.GT.0.D0) THEN
             
             H_WATER_KKP1_SN(INDEX_C)=H_WATER_KKP1
             
          ELSE 

            H_WATER_KKP1_SN(INDEX_C)=0.0D0

          END IF

        END IF

      WRITE(IOUT71,1020) INDEX_C, H_WATER_KKP1_SN(INDEX_C)
      END DO
C
      RETURN
 1010 FORMAT(I7,1PE16.8,'     NSTEP   TIME')
 2000 FORMAT(' NCELL   PONDING')
 1020 FORMAT(I6,1PE15.6E3)
      END
