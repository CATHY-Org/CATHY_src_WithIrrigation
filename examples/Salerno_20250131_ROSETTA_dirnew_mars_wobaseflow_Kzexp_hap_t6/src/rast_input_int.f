C
C*************************** RAST_INPUT_INT ****************************
C
C  Reads an integer raster map in GRASS (ASCII) format from unit IIN
C  according to a Cartesian reference frame. This subroutine is used
C  to read integer surface routing module input files.
C
C***********************************************************************
C
      SUBROUTINE RAST_INPUT_INT(IIN,NROW,NCOL,
     1                          NORTH,SOUTH,EAST,WEST,RAST)
C
      IMPLICIT   NONE
      INCLUDE   'CATHY.H'
      INTEGER    IIN,NROW,NCOL
      INTEGER    IROW,ICOL
      INTEGER*4  RAST(COLMAX,*)
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
c
c  reads the raster map 
c
      DO IROW=NROW,1,-1
         READ(IIN,*) (RAST(ICOL,IROW),ICOL=1,NCOL)
      END DO
C
      RETURN
      END
