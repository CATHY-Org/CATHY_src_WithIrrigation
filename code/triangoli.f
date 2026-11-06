      subroutine triangoli(nrow,ncol,delta_x,delta_y,west,south,
     1                     dem,zone,RESCALE_FACTOR,nnod,ntri,dostep,
     2                     ncell_coarse,nodi,triang,tp2d,x,y,z,
     3                     eltria,cell)
C
      implicit none
      include 'CATHY.H'
      integer dostep,l,k,m,n,aaa,nonnull
      integer itria,i,j,icell,inod
      integer nrow,ncol,nnod,ntri,ncell_coarse
      integer tp2d(*)
      integer nodi(rowmax+1,*),triang(4,*)
      integer cell(5,*)
      real*8  RESCALE_FACTOR,south,west
      real*8  delta_x,delta_y
      real*8  x(*),y(*),z(*),eltria(*)
      real*8  dem(rowmax,*)
      integer zone(rowmax,*)
C
      do i=1,nrow+1
         do j=1,ncol+1
            nodi(i,j) = 0
         end do
      end do
      do i=1,nrow,dostep
         do j=1,ncol,dostep
          do m=1,dostep
           do n=1,dostep
            if(dem(i+m-1,j+n-1).ne.0 .and. i+dostep-1.le.nrow
     &                         .and. j+dostep-1.le.ncol) then
               nodi(i,j)=1
               nodi(i+dostep,j)=1
               nodi(i+dostep,j+dostep)=1
               nodi(i,j+dostep)=1
            end if
           end do
          end do
         end do
      end do

      inod=0
      do i=1,nrow+1
         do j=1,ncol+1
            if(nodi(i,j).eq.1) then
               inod=inod+1
               x(inod) =west + (j-1)*delta_x
               y(inod) =south + (nrow-i+1)*delta_y
               nodi(i,j) = inod
            end if
         end do
      end do

      nnod=inod

      itria = 0
      icell=0
      do i=1,nrow,dostep  
         do j=1,ncol,dostep  
          do m=1,dostep
           do n=1,dostep
            if(dem(i+m-1,j+n-1).ne.0 .and. i+dostep-1.le.nrow
     &                      .and. j+dostep-1.le.ncol) then
               aaa=1
               go to 200
            else
               aaa=0
            end if
           end do
          end do
  200     continue
            if (aaa.eq.1) then  
               itria=itria+1
               eltria(itria) = 0.0d0
               nonnull=0
               do k=1,dostep
                  do l=1,dostep
                    if (dem(i+k-1,j+l-1).ne.0) then
                     nonnull=nonnull+1
                     eltria(itria) = eltria(itria) + dem(i+k-1,j+l-1)
                    end if
                  end do
               end do
               eltria(itria) = eltria(itria)*RESCALE_FACTOR/nonnull
               triang(1,itria) = nodi(i,j)
               triang(2,itria) = nodi(i+dostep,j)
               triang(3,itria) = nodi(i+dostep,j+dostep)
               triang(4,itria) = zone(i+m-1,j+n-1)
               itria=itria+1
               eltria(itria) = 0.0d0
               nonnull=0
               do k=1,dostep
                  do l=1,dostep
                    if (dem(i+k-1,j+l-1).ne.0) then
                     nonnull=nonnull+1
                     eltria(itria) = eltria(itria) + dem(i+k-1,j+l-1)
                    end if
                  end do
               end do
               eltria(itria) = eltria(itria)*RESCALE_FACTOR/nonnull
               triang(1,itria) = nodi(i,j)
               triang(2,itria) = nodi(i+dostep,j+dostep)
               triang(3,itria) = nodi(i,j+dostep)
               triang(4,itria) = zone(i+m-1,j+n-1)

               icell=icell+1
               cell(1,icell)=nodi(i,j)
               cell(2,icell)=nodi(i+dostep,j)
               cell(3,icell)=nodi(i+dostep,j+dostep)
               cell(4,icell)=nodi(i,j+dostep)
               cell(5,icell)= zone(i,j)
            end if
         end do
      end do

      ntri=itria
      ncell_coarse=icell
      call TPNODI2d(nnod,ntri,TRIANG,z,TP2d,eltria)
C
      return
      end
