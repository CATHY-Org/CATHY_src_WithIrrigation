C
C*************************** CELLDETQ **********************************
C 
C  Output of the discharge in all the cells of the catchment at the
C  times specified in the "parm" input file.
C
C***********************************************************************
C
      SUBROUTINE CELLDETQ(TIME,NSTEP)
C
      IMPLICIT NONE   
      INCLUDE 'CATHY.H'
      INCLUDE 'IOUNITS.H'
      INCLUDE 'RIVERNETWORK.H'
      INCLUDE 'SURFWATER.H'
      INTEGER I,NSTEP
      REAL*8  TIME
C
C      WRITING STATEMENT DIFFERENT IF THE CONSIDERED CELL IS THE OUTLET OR NOT.
C      FOR THE INTERNAL CELLS OF THE BASIN WE'RE 'OBLIGED' TO CONSIDER 
C      THE Q_IN_KKP1_SN IN CASE THERE IS DISPERSION IN THE FLUXES        
C
      WRITE(IOUT70,1010) NSTEP, TIME
      WRITE(IOUT70,2000)
      DO I=1,MAXCEL
         WRITE(IOUT70,1020) I, Q_IN_KKP1_SN(I), Q_OUT_KKP1_SN_1(I),
     1      Q_OUT_KKP1_SN_2(I), (Q_OUT_KKP1_SN_1(I)+Q_OUT_KKP1_SN_2(I))
      END DO
C
      RETURN
 1010 FORMAT(I7,1PE16.8,'     NSTEP   TIME')
 2000 FORMAT(' NCELL  INDIS  CARODIS  DIAODIS  TOTODIS')
 1020 FORMAT(I6,4000E16.7E3)
      END
