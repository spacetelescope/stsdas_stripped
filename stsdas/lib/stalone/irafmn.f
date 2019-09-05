      subroutine irafmn (ainchn, aoutcn, aerrcn, adrivr, adevte, prtype,
     * bkgfie, jobcoe)
      integer ainchn
      integer aoutcn
      integer aerrcn
      integer adrivr
      integer adevte
      integer prtype
      integer jobcoe
      integer*2 bkgfie(32767+1)
      logical netwog
      integer inchan
      integer outchn
      integer errchn
      integer driver
      integer devtye
      integer*2 cmd(161 +1)
      integer*2 taskne(32+1)
      integer*2 bkgfne(63 +1)
      integer arglit
      integer timeit
      integer junk
      integer intere
      integer builtk
      integer jumpbf(16 )
      integer status
      integer state
      integer intert
      integer i
      integer*4 savete(2)
      logical Memb(1)
      integer*2 Memc(1)
      integer*2 Mems(1)
      integer Memi(1)
      integer*4 Meml(1)
      real Memr(1)
      double precision Memd(1)
      complex Memx(1)
      equivalence (Memb, Memc, Mems, Memi, Meml, Memr, Memd, Memx)
      common /Mem/ Memd
      integer sp
      integer clcmar
      logical streq
      external finit
      integer sysged
      integer sysruk
      integer oscmd
      integer xfaccs
      integer envscn
      integer onenty
      logical xerpop
      logical xerflg
      common /xercom/ xerflg
      common /zjucom / jumpbf
      integer*2 nullfe(9)
      common /zzfakm/ state
      integer*2 st0001(35)
      integer*2 st0002(13)
      integer*2 st0003(18)
      integer*2 st0004(23)
      integer*2 st0005(28)
      integer*2 st0006(36)
      save
      integer iyy
      data (nullfe(iyy),iyy= 1, 8) /100,101,118, 36,110,117,108,108/
      data (nullfe(iyy),iyy= 9, 9) / 0/
      data (st0001(iyy),iyy= 1, 8) /102, 97,116, 97,108, 32,101,114/
      data (st0001(iyy),iyy= 9,16) /114,111,114, 32,100,117,114,105/
      data (st0001(iyy),iyy=17,24) /110,103, 32,112,114,111, 99,101/
      data (st0001(iyy),iyy=25,32) /115,115, 32,115,116, 97,114,116/
      data (st0001(iyy),iyy=33,35) /117,112, 0/
      data (st0002(iyy),iyy= 1, 8) /122,122,115,101,116,101,110,118/
      data (st0002(iyy),iyy= 9,13) / 46,100,101,102, 0/
      data (st0003(iyy),iyy= 1, 8) /115,101,116, 32, 64,122,122,115/
      data (st0003(iyy),iyy= 9,16) /101,116,101,110,118, 46,100,101/
      data (st0003(iyy),iyy=17,18) /102, 0/
      data (st0004(iyy),iyy= 1, 8) /104,111,115,116, 36,104,108,105/
      data (st0004(iyy),iyy= 9,16) / 98, 47,122,122,115,101,116,101/
      data (st0004(iyy),iyy=17,23) /110,118, 46,100,101,102, 0/
      data (st0005(iyy),iyy= 1, 8) /115,101,116, 32, 64,104,111,115/
      data (st0005(iyy),iyy= 9,16) /116, 36,104,108,105, 98, 47,122/
      data (st0005(iyy),iyy=17,24) /122,115,101,116,101,110,118, 46/
      data (st0005(iyy),iyy=25,28) /100,101,102, 0/
      data (st0006(iyy),iyy= 1, 8) /102, 97,116, 97,108, 32,101,114/
      data (st0006(iyy),iyy= 9,16) /114,111,114, 32,100,117,114,105/
      data (st0006(iyy),iyy=17,24) /110,103, 32,112,114,111, 99,101/
      data (st0006(iyy),iyy=25,32) /115,115, 32,115,104,117,116,100/
      data (st0006(iyy),iyy=33,36) /111,119,110, 0/
      data netwog /.true./
         state = 0
         call zsvjmp (jumpbf, status)
         if (.not.(status .ne. 0)) goto 110
            call syspac (0, st0001)
110      continue
         call maideh()
         inchan = ainchn
         outchn = aoutcn
         errchn = aerrcn
         driver = adrivr
         devtye = adevte
         call envint()
         call fmtint (0)
         call xerret()
         call erract (0)
         if (xerflg) goto 100
         call onerrr (finit )
         call onexit (finit )
         call finit()
         call clseti (1 , prtype)
         call clcint()
         call clcmak (clcmar)
         call strupk (bkgfie, bkgfne, 63 )
         intere = 0
         if (.not.(prtype .eq. 3 )) goto 120
            intere = 1
            if (.not.(xfaccs(st0002,0,0) .eq. 1)) goto 130
               call xerpsh
               junk = envscn (st0003)
               if (.not.xerpop()) goto 140
140            continue
               goto 131
130         continue
            if (.not.(xfaccs(st0004,0,0) .eq. 1)) goto 150
               call xerpsh
               junk = envscn (st0005)
               if (.not.xerpop()) goto 160
160            continue
150         continue
131         continue
120      continue
         call smark (sp)
         call zsvjmp (jumpbf, status)
         if (.not.(status .ne. 0)) goto 170
            if (.not.(state .eq. 1)) goto 180
               call syspac (0, st0006)
180         continue
            call erract (-99 )
            if (xerflg) goto 100
            call xerpsh
            call xonerr (status)
            if (xerflg) goto 192
            call maideh()
            call xffluh(5)
            do 200 i = 1 , 8
               call fseti (i, 8 , 0)
200         continue
201         continue
            call fioclp (status)
            if (xerflg) goto 192
            call fmtint (0)
            call sfree (sp)
192         if (.not.xerpop()) goto 190
               call erract (1 )
               if (xerflg) goto 100
190         continue
            call erract (0)
            if (xerflg) goto 100
            call xerret ()
            status = 0
170      continue
         if (.not.(state .eq. 0 )) goto 210
            if (.not.(onenty (prtype, bkgfne) .eq. 1 )) goto 220
               intert = 0
               goto 91
220         continue
               intert = 1
221         continue
210      continue
91       state = 1
100      return
      end
c     ainchn  a_inchan
c     aoutcn  a_outchan
c     intert  interpret
c     envint  env_init
c     onerrr  onerror
c     errchn  errchan
c     onenty  onentry
c     bkgfne  bkgfname
c     savete  save_time
c     clcmar  clc_marker
c     clcmak  clc_mark
c     adevte  a_devtype
c     sysruk  sys_runtask
c     envscn  envscan
c     clcint  clc_init
c     netwog  networking
c     intere  interactive
c     outchn  outchan
c     syspac  sys_panic
c     maideh  ma_ideh
c     zzfakm  zzfakecom
c     adrivr  a_driver
c     aerrcn  a_errchan
c     jumpbf  jumpbuf
c     irafmn  iraf_main
c     xerret  xer_reset
c     taskne  taskname
c     xonerr  xonerror
c     bkgfie  bkgfile
c     jobcoe  jobcode
c     devtye  devtype
c     arglit  arglist_offset
c     sysged  sys_getcommand
c     fioclp  fio_cleanup
c     fmtint  fmt_init
c     builtk  builtin_task
c     nullfe  nullfile
