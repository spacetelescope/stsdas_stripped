      subroutine wofins (inlist, wavelt, offses, linetb, corsie, previw,
     * apply, gp)
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
      integer inlist
      integer wavelt
      integer offses
      integer linetb
      integer corsie
      logical previw
      logical apply
      integer gp
      integer clindx
      integer clsize
      integer dir
      integer ext
      integer infile
      integer ksectn
      integer root
      integer sectin
      integer wavefe
      integer data
      integer image
      integer imaccs
      double precision imgetd
      integer imgl1d
      integer immap
      integer impl1d
      integer imtgem
      integer waveda
      integer waveie
      integer waveme
      integer ingrot
      integer wavegt
      logical tpfeth
      integer tpopen
      integer pred
      integer len
      integer maxlen
      double precision dc
      double precision deltas
      integer i
      double precision offset
      double precision soffst
      integer sp
      logical streq
      integer xstrln
      integer sx
      double precision value
      double precision woffst
      logical xerpop
      logical xerflg
      common /xercom/ xerflg
      integer*2 st0001(5)
      integer*2 st0002(65)
      integer*2 st0003(5)
      integer*2 st0004(4)
      integer*2 st0005(100)
      integer*2 st0006(69)
      integer*2 st0007(74)
      integer*2 st0008(127)
      integer*2 st0009(93)
      integer*2 st0010(9)
      integer*2 st0011(4)
      integer*2 st0012(7)
      integer*2 st0013(52)
      save
      integer iyy
      data st0001 / 46, 99, 49,104, 0/
      data (st0002(iyy),iyy= 1, 8) / 87, 65, 82, 78, 73, 78, 71, 58/
      data (st0002(iyy),iyy= 9,16) / 32,119, 97,118,101,111,102,102/
      data (st0002(iyy),iyy=17,24) / 58, 32, 73,109, 97,103,101, 32/
      data (st0002(iyy),iyy=25,32) / 37,115, 32,100,111,101,115,110/
      data (st0002(iyy),iyy=33,40) / 39,116, 32,101,120,105,115,116/
      data (st0002(iyy),iyy=41,48) / 44, 32,115,107,105,112,112,105/
      data (st0002(iyy),iyy=49,56) /110,103, 32,116,111, 32,110,101/
      data (st0002(iyy),iyy=57,64) /120,116, 32,111,110,101, 46, 10/
      data (st0002(iyy),iyy=65,65) / 0/
      data st0003 / 46, 99, 48,104, 0/
      data st0004 / 87, 67, 83, 0/
      data (st0005(iyy),iyy= 1, 8) / 87, 65, 82, 78, 73, 78, 71, 58/
      data (st0005(iyy),iyy= 9,16) / 32,119, 97,118,101,111,102,102/
      data (st0005(iyy),iyy=17,24) / 58, 32, 87, 97,118,101,108,101/
      data (st0005(iyy),iyy=25,32) /110,103,116,104, 32,102,105,108/
      data (st0005(iyy),iyy=33,40) /101, 32, 37,115, 32,100,111,101/
      data (st0005(iyy),iyy=41,48) /115,110, 39,116, 32,101,120,105/
      data (st0005(iyy),iyy=49,56) /115,116, 32,102,111,114, 32,105/
      data (st0005(iyy),iyy=57,64) /109, 97,103,101, 32, 37,115, 46/
      data (st0005(iyy),iyy=65,72) / 10, 32, 32, 32, 32, 83,107,105/
      data (st0005(iyy),iyy=73,80) /112,112,105,110,103, 32,116,111/
      data (st0005(iyy),iyy=81,88) / 32,110,101,120,116, 32,105,110/
      data (st0005(iyy),iyy=89,96) /112,117,116, 32,105,109, 97,103/
      data (st0005(iyy),iyy=97,100) /101, 46, 10, 0/
      data (st0006(iyy),iyy= 1, 8) / 87, 65, 82, 78, 73, 78, 71, 58/
      data (st0006(iyy),iyy= 9,16) / 32, 67,111,117,108,100, 32,110/
      data (st0006(iyy),iyy=17,24) /111,116, 32,111,112,101,110, 32/
      data (st0006(iyy),iyy=25,32) /119, 97,118,101,108,101,110,103/
      data (st0006(iyy),iyy=33,40) /116,104, 32,102,105,108,101, 32/
      data (st0006(iyy),iyy=41,48) / 37,115, 46, 32, 32, 83,107,105/
      data (st0006(iyy),iyy=49,56) /112,112,105,110,103, 32,116,111/
      data (st0006(iyy),iyy=57,64) / 32,110,101,120,116, 32,105,110/
      data (st0006(iyy),iyy=65,69) /112,117,116, 10, 0/
      data (st0007(iyy),iyy= 1, 8) / 87, 65, 82, 78, 73, 78, 71, 58/
      data (st0007(iyy),iyy= 9,16) / 32,119, 97,118,101,111,102,102/
      data (st0007(iyy),iyy=17,24) / 58, 32, 67,111,117,108,100, 32/
      data (st0007(iyy),iyy=25,32) /110,111,116, 32,111,112,101,110/
      data (st0007(iyy),iyy=33,40) / 32,105,110,112,117,116, 32,105/
      data (st0007(iyy),iyy=41,48) /109, 97,103,101, 32, 37,115, 44/
      data (st0007(iyy),iyy=49,56) / 32,115,107,105,112,112,105,110/
      data (st0007(iyy),iyy=57,64) /103, 32,116,111, 32,110,101,120/
      data (st0007(iyy),iyy=65,72) /116, 32,105,109, 97,103,101, 46/
      data (st0007(iyy),iyy=73,74) / 10, 0/
      data (st0008(iyy),iyy= 1, 8) / 87, 65, 82, 78, 73, 78, 71, 58/
      data (st0008(iyy),iyy= 9,16) / 32,119, 97,118,101,111,102,102/
      data (st0008(iyy),iyy=17,24) / 58, 32, 76,101,110,103,116,104/
      data (st0008(iyy),iyy=25,32) / 32,111,102, 32,105,110,112,117/
      data (st0008(iyy),iyy=33,40) /116, 32,115,112,101, 99,116,114/
      data (st0008(iyy),iyy=41,48) /117,109, 32, 37,115, 32, 61, 32/
      data (st0008(iyy),iyy=49,56) / 37,100, 10, 32, 32, 32, 32,105/
      data (st0008(iyy),iyy=57,64) /115, 32,110,111,116, 32,116,104/
      data (st0008(iyy),iyy=65,72) /101, 32,115, 97,109,101, 32, 97/
      data (st0008(iyy),iyy=73,80) /115, 32,116,104,101, 32,119, 97/
      data (st0008(iyy),iyy=81,88) /118,101,108,101,110,103,116,104/
      data (st0008(iyy),iyy=89,96) / 32, 61, 32, 37,100, 46, 32, 32/
      data (st0008(iyy),iyy=97,104) / 83,107,105,112,112,105,110,103/
      data (st0008(iyy),iyy=105,112) / 32,116,111, 32,110,101,120,116/
      data (st0008(iyy),iyy=113,120) / 32,105,110,112,117,116, 32,105/
      data (st0008(iyy),iyy=121,127) /109, 97,103,101, 46, 10, 0/
      data (st0009(iyy),iyy= 1, 8) / 79,102,102,115,101,116, 32, 98/
      data (st0009(iyy),iyy= 9,16) /101,116,119,101,101,110, 32,112/
      data (st0009(iyy),iyy=17,24) /114,101,100,105, 99,116,101,100/
      data (st0009(iyy),iyy=25,32) / 32,119, 97,118,101,108,101,110/
      data (st0009(iyy),iyy=33,40) /103,116,104,115, 32, 37,115, 32/
      data (st0009(iyy),iyy=41,48) / 97,110,100, 10, 32, 32, 32, 32/
      data (st0009(iyy),iyy=49,56) /111, 98,115,101,114,118, 97,116/
      data (st0009(iyy),iyy=57,64) /105,111,110, 32, 37,115, 32, 61/
      data (st0009(iyy),iyy=65,72) / 32, 37, 55, 46, 51,102, 44, 32/
      data (st0009(iyy),iyy=73,80) / 99,111,114,114,101,108, 97,116/
      data (st0009(iyy),iyy=81,88) /105,111,110, 32, 61, 32, 37, 55/
      data (st0009(iyy),iyy=89,93) / 46, 51,102, 10, 0/
      data (st0010(iyy),iyy= 1, 8) /105,110,115,116,114,117,109,101/
      data (st0010(iyy),iyy= 9, 9) / 0/
      data st0011 / 72, 82, 83, 0/
      data st0012 /100,101,108,116, 97,115, 0/
      data (st0013(iyy),iyy= 1, 8) /119, 97,118,101,111,102,102, 58/
      data (st0013(iyy),iyy= 9,16) / 32, 67,111,114,114,101, 99,116/
      data (st0013(iyy),iyy=17,24) /105,110,103, 32, 97,110, 32, 87/
      data (st0013(iyy),iyy=25,32) / 67, 83, 32,105,115, 32,110,111/
      data (st0013(iyy),iyy=33,40) /116, 32,105,109,112,108,101,109/
      data (st0013(iyy),iyy=41,48) /101,110,116,101,100, 32,121,101/
      data (st0013(iyy),iyy=49,52) /116, 33, 10, 0/
         call smark (sp)
         call salloc (dir, 127 , 2)
         call salloc (ext, 127 , 2)
         call salloc (infile, 127 , 2)
         call salloc (ksectn, 127 , 2)
         call salloc (root, 127 , 2)
         call salloc (sectin, 127 , 2)
         call salloc (sx, max (127 , 161 ), 2)
         call salloc (wavefe, 127 , 2)
         if (.not.(apply)) goto 110
            waveme = 2
            goto 111
110      continue
            waveme = 1
111      continue
         len = 0
         maxlen = 0
         pred = 0
120      if (.not.(imtgem (inlist, memc(infile), 127 ) .ne. -2)) goto 
     *   121
            call fparse (memc(infile), memc(dir), 127 , memc(root), 127 
     *      , memc(ext), 127 , clindx, clsize, memc(sectin), 127 , memc(
     *      ksectn), 127 )
            if (.not.(xstrln(memc(ext)) .le. 0)) goto 130
               call fbuild (memc(dir), memc(root), st0001, clindx, 
     *         clsize, memc(sectin), memc(ksectn), memc(infile), 127 )
130         continue
            if (.not.(imaccs (memc(infile), 1 ) .eq. 0)) goto 140
               call eprinf (st0002)
               call pargsr (memc(infile))
               goto 120
140         continue
            if (.not.(imtgem (wavelt, memc(sx), 127 ) .eq. -2)) goto 150
               call fbuild (memc(dir), memc(root), st0003, clindx, 
     *         clsize, memc(sectin), memc(ksectn), memc(sx), 127 )
150         continue
            if (.not.(streq (memc(sx), st0004))) goto 160
               call xstrcy(memc(sx), memc(wavefe), 127 )
               wavegt = 0
               waveie = 0
               goto 161
160         continue
               if (.not.(imaccs (memc(sx), 1 ) .eq. 0)) goto 170
                  if (.not.(xstrln(memc(wavefe)) .le. 0)) goto 180
                     call eprinf (st0005)
                     call pargsr (memc(sx))
                     call pargsr (memc(infile))
                     goto 120
180               continue
                  goto 171
170            continue
                  call xstrcy(memc(sx), memc(wavefe), 127 )
171            continue
               wavegt = tpopen (memc(wavefe), 0, i)
161         continue
            ingrot = tpopen (memc(infile), 0, i)
190         if (.not.(tpfeth (ingrot, memc(infile)))) goto 191
               if (.not.(wavegt .ne. 0)) goto 200
                  if (.not.(tpfeth (wavegt, memc(sx)))) goto 210
                     call xerpsh
                     waveie = immap (memc(sx), waveme, 0)
                     if (.not.xerpop()) goto 220
                        call eprinf (st0006)
                        call pargsr (memc(sx))
                        goto 191
220                  continue
                     call xstrcy(memc(sx), memc(wavefe), 127 )
                     waveda = imgl1d (waveie)
                     if (.not.(meml(waveie+200 +1+6-1) .gt. maxlen)) 
     *               goto 230
                        call xrealc(pred, meml(waveie+200 +1+6-1) , 7)
                        maxlen = meml(waveie+200 +1+6-1)
230                  continue
                     len = meml(waveie+200 +1+6-1)
                     call wopres (memd(waveda+1-1), len, linetb, memd(
     *               pred))
                     dc = (memd(waveda+len-1) - memd(waveda+1-1)) / len
210               continue
200            continue
               call xerpsh
               image = immap (memc(infile), 1 , 0)
               if (.not.xerpop()) goto 240
                  call eprinf (st0007)
                  call pargsr (memc(infile))
                  goto 191
240            continue
               if (.not.(wavegt .eq. 0)) goto 250
                  if (.not.(meml(image+200 +1+6-1) .gt. maxlen)) goto 
     *            260
                     call xrealc(pred, meml(image+200 +1+6-1) , 7)
                     maxlen = meml(image+200 +1+6-1)
260               continue
                  len = meml(image+200 +1+6-1)
                  call wowcsd (image, linetb, memd(pred))
250            continue
               if (.not.(meml(image+200 +1+6-1) .ne. len)) goto 270
                  call eprinf (st0008)
                  call pargsr (memc(infile))
                  call pargi (meml(image+200 +1+6-1) )
                  call pargi (len)
                  goto 191
270            continue
               data = imgl1d (image)
               call woofft (memc(infile), memd(data+1-1), memd(pred), 
     *         len, corsie, previw, gp, offset, value)
               call xprinf(st0009)
               call pargsr (memc(wavefe))
               call pargsr (memc(infile))
               call pargd (offset)
               call pargd (value)
               if (.not.(((offset).eq.1.6d38))) goto 280
                  woffst = 1.6d38
                  soffst = 1.6d38
                  goto 281
280            continue
                  woffst = dc * offset
                  soffst = 1.6d38
                  call xerpsh
                  call imgstr (image, st0010, memc(sx), 161 )
                  if (xerpop()) goto 290
                     if (.not.(streq (memc(sx), st0011))) goto 300
                        call xerpsh
                        deltas = imgetd (image, st0012)
                        if (xerpop()) goto 310
                           soffst = deltas * offset
310                     continue
300                  continue
290               continue
281            continue
               call woadds (offses, memc(infile), offset, value, woffst,
     *          soffst)
               if (.not.(apply .and. .not.((woffst).eq.1.6d38))) goto 
     *         320
                  if (.not.(waveie .ne. 0)) goto 330
                     call aaddkd (memd(waveda+1-1), woffst, memd(impl1d 
     *               (waveie)), len)
                     goto 331
330               continue
                     call eprinf (st0013)
331               continue
320            continue
               if (.not.(waveie .ne. 0)) goto 340
                  call imunmp (waveie)
340            continue
               call imunmp (image)
               goto 190
191         continue
            goto 120
121      continue
         call sfree (sp)
100      return
      end
c     wavefe  wave_file
c     corsie  cor_size
c     ksectn  ksection
c     imtgem  imtgetim
c     waveme  wave_mode
c     wofins  wo_find_offsets
c     maxlen  max_len
c     wopres  wo_pred_obs
c     linetb  line_tab
c     waveie  wave_image
c     clsize  cl_size
c     woadds  wo_add_offsets
c     wowcsd  wo_wcs_pred
c     clindx  cl_index
c     inlist  in_list
c     imaccs  imaccess
c     sectin  section
c     tpopen  tp_open
c     woofft  wo_offset
c     wavelt  wave_list
c     tpfeth  tp_fetch
c     imunmp  imunmap
c     eprinf  eprintf
c     ingrot  in_group_list
c     offses  offsets
c     soffst  soffset
c     wavegt  wave_group_list
c     woffst  woffset
c     waveda  wave_data
c     infile  in_file
c     previw  preview
c     pargsr  pargstr
