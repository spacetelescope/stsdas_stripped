# POA_FOS -- Post Operation Archive, FOS package

cl < "poa_fos$lib/zzsetenv.def"

        package poa_fos

        set  poa_calfos         = "poa_fos$poa_calfos/"
        set  poa_preproc_fos    = "poa_fos$poa_preproc_fos/"
        set  poa_procfos_all    = "poa_fos$poa_procfos_all/"
        set  poa_spec           = "poa_fos$data/"

        task poa_calfos         = "poa_fos$x_poa_calfos.e"
        task fos_pix2wav        = "poa_fos$x_fos_pix2wav.e"
        task pfos_pix2wav       = "poa_fos$pfos_pix2wav.cl"
        task fos_dispfit        = "poa_fos$x_fos_dispfit.e"
        task pfos_dispfit       = "poa_fos$pfos_dispfit.cl"
        task poa_preproc_fos    = "poa_preproc_fos$poa_preproc_fos.cl"
        task processfos         = "poa_procfos_all$processfos.cl"

        hidetask fos_pix2wav
        hidetask fos_dispfit

clbye
