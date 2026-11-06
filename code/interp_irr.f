C
C*************************** INTERP_IRR ********************************
C
C Use the lookup table to interpolate water saturation values.
C
C***********************************************************************
C
      SUBROUTINE INTERP_IRR(NLKP,ISTR,MTYPE,PAVG,SWLOC)
C
      IMPLICIT NONE
      INCLUDE 'CATHY.H'
      INCLUDE 'SOILCHAR.H'
      INTEGER  I
      INTEGER  NLKP,ISTR,MTYPE
      REAL*8   PAVG,SWLOC
C
      IF (PAVG .GT. -1.0D-14) THEN
         SWLOC=1.0D0
      ELSE IF (PAVG .LE. PCAP(ISTR,MTYPE,1)) THEN
         SWLOC=SATC(ISTR,MTYPE,1)
      ELSE IF (PAVG .GE. PCAP(ISTR,MTYPE,NLKP)) THEN
         SWLOC=SATC(ISTR,MTYPE,NLKP)
      ELSE
         I=1
         DO WHILE ((PAVG .LT. PCAP(ISTR,MTYPE,I))  .OR.
     1             (PAVG .GE. PCAP(ISTR,MTYPE,I+1)))
            I=I+1
         END DO
         SWLOC=SATC(ISTR,MTYPE,I) +
     1         (SATC(ISTR,MTYPE,I+1) - SATC(ISTR,MTYPE,I))/
     2         (PCAP(ISTR,MTYPE,I+1) - PCAP(ISTR,MTYPE,I)) *
     3         (PAVG - PCAP(ISTR,MTYPE,I))
      END IF
C
      RETURN
      END
