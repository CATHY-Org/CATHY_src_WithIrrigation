C
      subroutine vtk_tran_cnod(ntetra,nnode,iunit,time,tetra,
     1                         u,x,y,z)
C
      implicit none
      integer i,j,numb,field
      integer ntetra,nnode,iunit
      integer tetra(5,*)
      real*8 time
      real*8 u(*),x(*),y(*),z(*)
      character*17 name,file_vtk
C
      field=1 
      write(name,'(a4,i5,a4)') 'cnod',iunit,'.vtk'
      file_vtk='vtk'//'/'//name
      open(iunit,file=file_vtk)

      write(iunit,78) 
 78   FORMAT('# vtk DataFile Version 2.0',
     1     /,'3D Unstructured Grid of Linear Triangles', 
     2     /,'ASCII')

      write(iunit,77) field,field,field,time
 77   FORMAT('DATASET UNSTRUCTURED_GRID',
     1     /, 'FIELD FieldData',1x,i2,
     2     /, 'TIME',1x,i1,1x,i1,1x,'double',
     3     /, 1f18.5)

      write(iunit,79) nnode
 79   FORMAT('POINTS',1x,i8,' float')
      do i=1,nnode
         write(iunit,100) x(i),y(i),z(i)
      end do
 100  format(4x,3(1pe16.4))

      numb=ntetra*5
      write(iunit, 80) ntetra,numb
 80   format('CELLS',1x,i8,1x,i8)
      j=4
      do i=1,ntetra
         write(iunit,101) j,tetra(1,i)-1,tetra(2,i)-1, 
     1                    tetra(3,i)-1,tetra(4,i)-1
      end do
 101  format(i1,3x,i8,3x,i8,3x,i8,3x,i8)

      write(iunit,81) ntetra
 81   format('CELL_TYPES',i8)
      do i=1,ntetra
         write(iunit,*) 10
      end do

      write(iunit,82) nnode
 82   format('POINT_DATA',1x,i8, 
     1    /, 'SCALARS conc_node float',
     2    /, 'LOOKUP_TABLE default' )
      do i=1,nnode
         write(iunit,102) u(i)
      end do
 102  format(f16.7)
C
      close(iunit)
C
      return
      end
