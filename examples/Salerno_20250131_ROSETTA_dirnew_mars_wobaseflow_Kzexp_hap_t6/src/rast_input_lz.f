C
C*************************** RAST_INPUT_LZ *****************************
C
C  Reads an integer raster map in GRASS (ASCII) format from unit IIN
C  according to matrix convention. This subroutine is used to read in
C  lakes_map and zone_map.
C
C***********************************************************************
C
      SUBROUTINE RAST_INPUT_LZ(IIN,NROW,NCOL,
     1                         NORTH,SOUTH,EAST,WEST,RAST)
C
      IMPLICIT   NONE
      INCLUDE   'CATHY.H'
      INTEGER    IIN,NROW,NCOL
      INTEGER    IROW,ICOL
      INTEGER    RAST(ROWMAX,*)
      REAL*8     NORTH,SOUTH,EAST,WEST
      CHARACTER  RDWR*80,LINE*80
c
c  reads the first four lines (north,south,east,west)
c
      READ(IIN,'(A80)') LINE
      WRITE(RDWR,'(A74)') LINE(8:80)
      READ(RDWR,*) NORTH
      READ(IIN,'(A80)') LINE
      WRITE(RDWR,'(A74)') LINE(8:80)
      READ(RDWR,*) SOUTH
      READ(IIN,'(A80)') LINE
      WRITE(RDWR,'(A74)') LINE(8:80)
      READ(RDWR,*) EAST 
      READ(IIN,'(A80)') LINE
      WRITE(RDWR,'(A74)') LINE(8:80)
      READ(RDWR,*) WEST 
c
c  reads in NROW and NCOL
c
      READ(IIN,'(A80)') LINE
      WRITE(RDWR,'(A75)') LINE(7:80)
      READ(RDWR,*) NROW
      READ(IIN,'(A80)') LINE
      WRITE(RDWR,'(A75)') LINE(7:80)
      READ(RDWR,*) NCOL
c  reads the raster map 
      DO IROW=1,NROW
         READ(IIN,*) (RAST(IROW,ICOL),ICOL=1,NCOL)
      END DO
C
      RETURN
      END
