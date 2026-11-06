C        
C*************************** IRR_DEMCELL_TO_NODE ***********************
C        
C Transfer and averaging of irrigation rates from cell (surface DEM) to element 
C (surface triangles of the subsurface grid) and then from element to (surface)
C node.
C Handling of DOSTEP > 1 (grid coarsening) cases is included.
C This subroutine is modeled after subroutines TRIANGOLI and TPNODI2D.
C        
C***********************************************************************
C           
      SUBROUTINE IRR_DEMCELL_TO_NODE(NROW,NCOL,NNOD,NTRI,DOSTEP,IRR_I,
     1                               TRIANG,TP2D,DEM_MAP)
C
      IMPLICIT  NONE
      INCLUDE  'CATHY.H'
      INCLUDE  'IRR_CBLOCK.H'
      INTEGER   I,J,K,L,M,N,ITRIA,INOD,AAA,NONNULL
      INTEGER   NROW,NCOL,NNOD,NTRI,DOSTEP,IRR_I
      INTEGER   TRIANG(4,*),TP2D(*)
      REAL*8    DEM_MAP(ROWMAX,*)
C
      ITRIA = 0
      DO I = 1,NROW,DOSTEP  
         DO J = 1,NCOL,DOSTEP  
            DO M = 1,DOSTEP
               DO N = 1,DOSTEP
                  IF (DEM_MAP(I+M-1,J+N-1) .NE. 0
     1                .AND. (I + DOSTEP - 1) .LE. NROW
     2                .AND. (J + DOSTEP - 1) .LE. NCOL) THEN
                     AAA = 1
                     GO TO 200
                  ELSE
                     AAA = 0
                  END IF
               END DO
            END DO
  200       CONTINUE
            IF (AAA .EQ. 1) THEN  
               ITRIA = ITRIA + 1
               IRR_NELT(ITRIA) = 0.0D0
               NONNULL = 0
               DO K = 1,DOSTEP
                  DO L = 1,DOSTEP
                     IF (DEM_MAP(I+K-1,J+L-1) .NE. 0) THEN
                        NONNULL = NONNULL + 1
                        IRR_NELT(ITRIA) = IRR_NELT(ITRIA) +
     1                                  IRR_INP_RAST(IRR_I,I+K-1,J+L-1)
                     END IF
                  END DO
               END DO
               IRR_NELT(ITRIA) = IRR_NELT(ITRIA)/NONNULL
               ITRIA = ITRIA + 1
               IRR_NELT(ITRIA) = 0.0D0
               NONNULL = 0
               DO K = 1,DOSTEP
                  DO L = 1,DOSTEP
                     IF (DEM_MAP(I+K-1,J+L-1) .NE. 0) THEN
                        NONNULL = NONNULL + 1
                        IRR_NELT(ITRIA) = IRR_NELT(ITRIA) +
     1                                  IRR_INP_RAST(IRR_I,I+K-1,J+L-1)
                     END IF
                  END DO
               END DO
               IRR_NELT(ITRIA) = IRR_NELT(ITRIA)/NONNULL
            END IF
         END DO
      END DO
      CALL INIT0R(NNOD,IRR_NNOD)
      DO K = 1,NTRI
         DO I = 1,3
            INOD = TRIANG(I,K)
            IRR_NNOD(INOD) = IRR_NNOD(INOD) + IRR_NELT(K)
         END DO
      END DO
      DO K = 1,NNOD
         IRR_NNOD(K) = IRR_NNOD(K) / TP2D(K)
      END DO
C
      RETURN
      END
