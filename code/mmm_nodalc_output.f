C
      subroutine mmm_nodalc_output(n,iunit,time,mmm_ncomp,mmm_cnnew)
C
      implicit      none
      include      'CATHY.H'
      integer       i,iii
      integer       n,iunit,mmm_ncomp
      real*8        time
      real*8        mmm_cnnew(mmm_max_ncomp,*)
      character*22  name,file_nodalc
C
      write(name,'(a10,i5)') 'mmm_nodalc',iunit
      file_nodalc='output'//'/'//name
      open(iunit,file=file_nodalc)
C
      write(iunit,1000) time,mmm_ncomp
      do i=1,n
         write(iunit,1020) i,(mmm_cnnew(iii,i),iii=1,mmm_ncomp)
      end do
C
      close(iunit)
C
      return
 1000 format(  '#  Time =',1PE21.14,
     1       /,'#   Node   Concentration for components 1, ..., ',I4)
 1020 format(I8,100(F15.7))
      end
