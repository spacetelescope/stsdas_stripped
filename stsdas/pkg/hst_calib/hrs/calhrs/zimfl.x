include <imio.h>

procedure zimfl (im)

pointer	im

begin
        IM_UPDATE(im) = YES
	call imflush (im)
        call flush (IM_HFD(im))
        call iki_updhdr (im)
end
