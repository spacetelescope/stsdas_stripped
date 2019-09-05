       subroutine apomak
*++
*
* APOMASK V1.0 - create an apodising mask for image restoration
*
* This is an IRAF application written using the F77/VOS.
*
* Purpose:
*
* Create an image which is zero everywhere except for a central circular region
* where it is one. The region outside the central circle fades off as a gaussian.
* The user supplies the radius of the central circle and the sigma of the fade-off.
*
* History:
*
* First version, Richard Hook, ST-ECF, Jan 92
* Added limb darkening, Richard Hook, ST-ECF, Apr 94
* Renamed APOMASK, Richard Hook, Mar 99
* Given version number, changed parameter names to all lowercase,
*  Richard Hook, February 2000
*
*--

* Iraf global space
*
      real Memr(1)
      integer Memi(1)
      equivalence (Memr,Memi)
      common /Mem/Memr

* Local variables
      integer nx,ny,dims(2),id
      character*80 image
      real rad,sigma,xpos,ypos,k

* Get the name of the output image
      call uclgst('output',image,istat)

* and its dimensions
      call uclgsi('xdim',nx,istat)
      call uclgsi('ydim',ny,istat)

      dims(1)=nx
      dims(2)=ny

* and the size and fade-off parameters
      call uclgsr('radius',rad,istat)
      call uclgsr('sigma',sigma,istat)
      call uclgsr('k',k,istat)

* and the position for the centre of the mask
      call uclgsr('xpos',xpos,istat)
      call uclgsr('ypos',ypos,istat)

* Create the image
      call uimcre(image,6,2,dims,id,istat)
      if(istat.ne.0) then
         call umsput('Unable to create output image',
     :               1,0,istat)
         go to 99
      endif

* Allocate memory for the image
      call udmget(nx*ny,6,ipnt,istat)
      if(istat.ne.0) then
         call umsput('Unable to allocate memory for data',
     :               1,0,istat)
         go to 99
      endif

* Fill the image
      call circle(Memr(ipnt),nx,ny,rad,sigma,k,xpos,ypos)

* Write out the result
      call uips2r(id,1,nx,1,ny,Memr(ipnt),istat)

 99   continue
      call uimclo(id,istat)
      call udmfre(ipnt,6,istat)

      end

      subroutine circle(data,nx,ny,rad,sigma,k,xcen,ycen)
*
* Fill in a circular region with 1s and the rest zero except for
* a fade off around the circle with a specified sigma.
*
      implicit none

      integer nx,ny
      real data(nx,ny),rad,sigma,dx,dy,xcen,ycen,k,r
      integer i,j

      do j=1,ny
          dy=real(j)-ycen
          do i=1,nx
             dx=real(i)-xcen
             r=sqrt(dx*dx+dy*dy)
             if(r.le.rad) then
                data(i,j)=(1.0-(r/rad)**2)**(k-0.5)
             else if(r.le.rad+5*sigma) then
                data(i,j)=exp(-((r-rad)**2.0/(2.0*sigma*sigma)))
             else
                data(i,j)=0.0
             endif
          enddo
      enddo

      end
