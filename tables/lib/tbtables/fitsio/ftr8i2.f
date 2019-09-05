C----------------------------------------------------------------------
        subroutine ftr8i2(input,n,scale,zero,tofits,
     &          chktyp,setval,flgray,anynul,output,status)

C       copy input r*8 values to output i*2 values, doing optional
C       scaling and checking for null values

C       input   d  input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       setval  i*2 value to set output array to if value is undefined
C       flgray  l   array of logicals indicating if corresponding value is null
C       anynul  l   set to true if any nulls were set in the output array
C       output  i*2 returned array of values
C       status  i  output error status (0 = ok)

        double precision input(*)
        integer*2 output(*),setval,maxi2,mini2
        integer n,i,chktyp,status
        double precision scale,zero,dval,i2max,i2min
        logical tofits,flgray(*),anynul,noscal
        logical fttdnn
        parameter (i2max=3.276749D+04)
        parameter (i2min=-3.276849D+04)

        parameter (maxi2=32767)
        parameter (mini2=-32768)
        external fttdnn

        if (status .gt. 0)return

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
C                           trap any values that overflow the I*2 range
                            if (input(i) .le. i2max .and. 
     &                          input(i) .ge. i2min)then
                                    output(i)=nint(input(i))
                            else if (input(i) .gt. i2max)then
                                    status=-11
                                    output(i)=maxi2
                            else
                                    status=-11
                                    output(i)=mini2
                            end if
10                      continue
                else
                        do 20 i=1,n
                            dval=(input(i)-zero)/scale
C                           trap any values that overflow the I*2 range
                            if (dval.lt.i2max .and. dval.gt.i2min)then      
                                output(i)=nint(dval)
                            else if (dval .ge. i2max)then
                                status=-11
                                output(i)=maxi2
                            else
                                status=-11
                                output(i)=mini2
                            end if
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                          do 30 i=1,n
C                           trap any values that overflow the I*2 range
                            if (input(i) .le. i2max .and. 
     &                          input(i) .ge. i2min)then
                                    output(i)=nint(input(i))
                            else if (input(i) .gt. i2max)then
                                    status=-11
                                    output(i)=maxi2
                            else
                                    status=-11
                                    output(i)=mini2
                            end if
30                        continue
                        else
                            do 40 i=1,n
                              dval=input(i)*scale+zero
C                             trap any values that overflow the I*2 range
                              if (dval.lt.i2max .and. dval.gt.i2min)then
                                  output(i)=nint(dval)
                              else if (dval .ge. i2max)then
                                  status=-11
                                  output(i)=maxi2
                              else
                                  status=-11
                                  output(i)=mini2
                              end if
40                          continue
                        end if
                else 
C                   must test for null values
                    if (noscal)then
                        do 50 i=1,n
                            if (fttdnn(input(i)))then
                                anynul=.true.
                                if (chktyp .eq. 1)then
                                     output(i)=setval
                                else
                                     flgray(i)=.true.
                                end if
                            else
C                               trap any values that overflow the I*2 range
                                if (input(i) .le. i2max .and. 
     &                              input(i) .ge. i2min)then
                                        output(i)=nint(input(i))
                                else if (input(i) .gt. i2max)then
                                        status=-11
                                        output(i)=maxi2
                                else
                                        status=-11
                                        output(i)=mini2
                                end if
                            end if
50                      continue
                        else
                          do 60 i=1,n
                            if (fttdnn(input(i)))then
                                anynul=.true.
                                if (chktyp .eq. 1)then
                                    output(i)=setval
                                else
                                    flgray(i)=.true.
                                end if
                            else
                              dval=input(i)*scale+zero
C                             trap any values that overflow the I*2 range
                              if (dval.lt.i2max .and. dval.gt.i2min)then
                                  output(i)=nint(dval)
                              else if (dval .ge. i2max)then
                                  status=-11
                                  output(i)=maxi2
                              else
                                  status=-11
                                  output(i)=mini2
                              end if
                            end if
60                        continue
                        end if
                end if
        end if
        end
