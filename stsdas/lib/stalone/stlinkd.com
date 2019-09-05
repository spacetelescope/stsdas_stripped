$ link/debug 'p1,disk$sdas:[stsdasx.syslib]stalone/lib,-
	         disk$sdas:[irafx.lib]libex/lib,-
	         disk$sdas:[irafx.lib]libsys/lib,-
                 disk$sdas:[irafx.lib]libvops/lib,-
	         disk$sdas:[irafx.vms.hlib]libos/lib
