C
C*************************** RECHARGE **********************************
C
C  Calculates water table recharge in terms of both flux and volume.
C
C***********************************************************************
C
      SUBROUTINE RECHARGE(NNOD,NSTR,N,WNOD,ARENOD,VOLNOD,PNEW,PTIMEP,Z,
     1                    SNODI,PNODI,TIME,DELTAT,RECFLOW,RECVOL,RECNOD)
C
      IMPLICIT  NONE
      INCLUDE  'CATHY.H'
      INCLUDE  'MB_HGRAPH.H'
      INCLUDE  'SOILCHAR.H'
      INTEGER   I,J,INOD1,INOD2
      INTEGER   NNOD,NSTR,N
      INTEGER   FLAG(NNOD)
      REAL*8    FVGSE
      REAL*8    ZERO,REC,RC
      REAL*8    TIME,DELTAT,RECFLOW,RECVOL
      REAL*8    SWNEW(N),SWTIMEP(N)
      REAL*8    WNOD(*),ARENOD(*),VOLNOD(*),PNEW(*),PTIMEP(*),Z(*)
      REAL*8    SNODI(*),PNODI(*),RECNOD(*)
      PARAMETER (ZERO=0.0D0)
C
      RECFLOW = ZERO
      IF (TIME.EQ.0.0D0) THEN
         RECVOL = ZERO
      END IF
      CALL INIT0R(NNOD,RECNOD)
      CALL INIT0I(NNOD,FLAG)
      SSTORE1 = ZERO
      SSTORE2 = ZERO
      REC = ZERO
C
      DO I=1,NNOD
         DO J=NSTR,1,-1
            INOD1=I+J*NNOD
            INOD2=I+(J-1)*NNOD
            SWNEW(INOD1)=VGPNOT(INOD1)*FVGSE(PNEW(INOD1),INOD1)+
     &                   VGRMC(INOD1)/PNODI(INOD1)
            SWNEW(INOD2)=VGPNOT(INOD2)*FVGSE(PNEW(INOD2),INOD2)+
     &                   VGRMC(INOD2)/PNODI(INOD2)
            SWTIMEP(INOD1)=VGPNOT(INOD1)*FVGSE(PTIMEP(INOD1),INOD1)+
     &                   VGRMC(INOD1)/PNODI(INOD1)
            SWTIMEP(INOD2)=VGPNOT(INOD2)*FVGSE(PTIMEP(INOD2),INOD2)+
     &                   VGRMC(INOD2)/PNODI(INOD2)
            IF (PNEW(INOD1).GE.ZERO.AND.PNEW(INOD2).GE.ZERO
     &         .AND.FLAG(I).EQ.0) THEN
               IF (J.EQ.1) THEN
CM Totally saturated soil column 
                  FLAG(I)=-1
CM                IF (WNOD(INOD2).LE.ZERO) THEN
CM                   RECNOD(I)=-1.0d0*WNOD(INOD2)*ARENOD(I)
CM                   RECNOD(I)=0.0d0
CM                END IF
                  REC = REC+VOLNOD(INOD2)*(SNODI(INOD2)*PNEW(INOD2)+
     &                  PNODI(INOD2)*SWNEW(INOD2))
                  SSTORE1 = SSTORE1 + SNODI(INOD2)*VOLNOD(INOD2)*
     &                     (PNEW(INOD2)-PTIMEP(INOD2)) +
     &                     PNODI(INOD2)*VOLNOD(INOD2)*
     &                     (SWNEW(INOD2)-SWTIMEP(INOD2))
               ELSE
                  REC = REC+VOLNOD(INOD1)*(SNODI(INOD1)*PNEW(INOD1)+
     &                  PNODI(INOD1)*SWNEW(INOD1))
                  SSTORE1 = SSTORE1 + SNODI(INOD1)*VOLNOD(INOD1)*
     &                     (PNEW(INOD1)-PTIMEP(INOD1)) +
     &                     PNODI(INOD1)*VOLNOD(INOD1)*
     &                     (SWNEW(INOD1)-SWTIMEP(INOD1))
               END IF
            ELSE IF (PNEW(INOD1).GE.ZERO.AND.PNEW(INOD2).LT.ZERO
     &              .AND.FLAG(I).EQ.0) THEN
               FLAG(I)=1
               RC=(Z(INOD1)-Z(INOD2))/(PNEW(INOD1)-PNEW(INOD2))
               SSTORE1 = SSTORE1 + SNODI(INOD1)*VOLNOD(INOD1)*RC*
     &                   (PNEW(INOD1)-PTIMEP(INOD1)) + 
     &                   PNODI(INOD1)*VOLNOD(INOD1)*RC*
     &                   (SWNEW(INOD1)-SWTIMEP(INOD1))
               SSTORE2 = SSTORE2 + PNODI(INOD1)*
     &                   (PNEW(INOD1)-PTIMEP(INOD1))*ARENOD(I)
               REC = REC+RC*VOLNOD(INOD1)*(SNODI(INOD1)*PNEW(INOD1)+
     &                  PNODI(INOD1)*SWNEW(INOD1))
CM             IF (WNOD(INOD1).LE.ZERO) THEN
                  RECNOD(I)=-1.0d0*WNOD(INOD1)*ARENOD(I)
CM                RECFLOW=RECFLOW+RECNOD(I)
CM             END IF
CM Perched water table
            ELSE IF (PNEW(INOD1).GE.ZERO.AND.PNEW(INOD2).LT.ZERO
     &         .AND.FLAG(I).GT.0) THEN
               FLAG(I)=FLAG(I)+1
CM Totally unsaturated soil column 
            ELSE IF (J.EQ.1.AND.FLAG(I).EQ.0) THEN
               FLAG(I)=-2
            END IF
         END DO
         RECFLOW=RECFLOW+RECNOD(I)
      END DO
      RECVOL=RECVOL+RECFLOW*DELTAT
      SSTORE1=SSTORE1/DELTAT
      SSTORE2=SSTORE2/DELTAT
      WRITE(1133,*)TIME,SSTORE1,SSTORE2,REC
      RETURN
      END
