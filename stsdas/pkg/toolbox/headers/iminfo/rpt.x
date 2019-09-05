# RPT -- Print the repeat codes for the HRS 
            
procedure rpt (every, evcnt, every2, ev2cnt, every4, ev4cnt, every8, ev8cnt)

int     every[ARB], every2[ARB], every4[ARB], every8[ARB]
int     evcnt, ev2cnt, ev4cnt, ev8cnt, i

begin

      if (evcnt > 0) {
         call printf (" time        ")
         do i = 1, evcnt {
            if (i != evcnt ) {
               call printf (" %d,")
                  call pargi (every[i])
            } else {
                call printf (" %d\n")
                   call pargi (every[i])
            }
         } 
         evcnt = 0 

     } else if (ev2cnt > 0) {
         call printf (" second time ")
         do i = 1, ev2cnt {
             if (i != ev2cnt ) {
               call printf (" %d,")
                  call pargi (every2[i])
            } else { 
               call printf (" %d\n")
               call pargi (every2[i])
            }
         }
         ev2cnt = 0

     } else if (ev4cnt > 0) {
         call printf (" fourth time ")
         do i = 1, ev4cnt {
            if (i != ev4cnt ) {
               call printf (" %d,")
                  call pargi (every4[i])
            } else {
               call printf (" %d\n")
                  call pargi (every4[i])
            }
         }
         ev4cnt = 0

     } else if (ev8cnt > 0) {
         call printf (" eighth time ")
         do i = 1, ev8cnt {
            if (i != ev8cnt ) {
               call printf (" %d,")
                  call pargi (every8[i])
            } else {  
               call printf (" %d\n")
                  call pargi (every8[i])
            }
         }
         ev8cnt = 0
     }
  
end

