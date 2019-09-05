# HRSBIN -- Read BINID's and RPTCD's for the High Resolution Spectrograph

procedure hrsbin (im, binid, rptcd)

int     imgeti(), imaccf()
int     binid[ARB], rptcd[ARB]

pointer im
 
begin

        if (imaccf (im, "BINID1") == YES)
           binid[1] = imgeti (im, "BINID1")
        else if (imaccf (im, "BINID(1)") == YES)
           binid[1] = imgeti (im, "BINID(1)")
        else binid[1] = 0

        if (imaccf (im, "BINID2") == YES)
           binid[2] = imgeti (im, "BINID2")
        else if (imaccf (im, "BINID(2)") == YES)
           binid[2] = imgeti (im, "BINID(2)")
        else binid[2] = 0

        if (imaccf (im, "BINID3") == YES)
           binid[3] = imgeti (im, "BINID3")
        else if (imaccf (im, "BINID(3)") == YES)
           binid[3] = imgeti (im, "BINID(3)")
        else binid[3] = 0

        if (imaccf (im, "BINID4") == YES)
           binid[4] = imgeti (im, "BINID4")
        else if (imaccf (im, "BINID(4)") == YES)
           binid[4] = imgeti (im, "BINID(4)")
        else binid[4] = 0

        if (imaccf (im, "BINID5") == YES)
           binid[5] = imgeti (im, "BINID5")
        else if (imaccf (im, "BINID(5)") == YES)
           binid[5] = imgeti (im, "BINID(5)")
        else binid[5] = 0

        if (imaccf (im, "BINID6") == YES)
           binid[6] = imgeti (im, "BINID6")
        else if (imaccf (im, "BINID(6)") == YES)
           binid[6] = imgeti (im, "BINID(6)")
        else binid[6] = 0

        if (imaccf (im, "BINID7") == YES)
           binid[7] = imgeti (im, "BINID7")
        else if (imaccf (im, "BINID(7)") == YES)
           binid[7] = imgeti (im, "BINID(7)")
        else binid[7] = 0

        if (imaccf (im, "RPTCD1") == YES)
           rptcd[1] = imgeti (im, "RPTCD1")
        else if (imaccf (im, "RPTCD(1)") == YES)
           rptcd[1] = imgeti (im, "RPTCD(1)")
        else rptcd[1] = 0

        if (imaccf (im, "RPTCD2") == YES)
           rptcd[2] = imgeti (im, "RPTCD2")
        else if (imaccf (im, "RPTCD(2)") == YES)
           rptcd[2] = imgeti (im, "RPTCD(2)")
        else rptcd[2] = 0

        if (imaccf (im, "RPTCD3") == YES)
           rptcd[3] = imgeti (im, "RPTCD3")
        else if (imaccf (im, "RPTCD(3)") == YES)
           rptcd[3] = imgeti (im, "RPTCD(3)")
        else rptcd[3] = 0

        if (imaccf (im, "RPTCD4") == YES)
           rptcd[4] = imgeti (im, "RPTCD4")
        else if (imaccf (im, "RPTCD(4)") == YES)
           rptcd[4] = imgeti (im, "RPTCD(4)")
        else rptcd[4] = 0

        if (imaccf (im, "RPTCD5") == YES)
           rptcd[5] = imgeti (im, "RPTCD5")
        else if (imaccf (im, "RPTCD(5)") == YES)
           rptcd[5] = imgeti (im, "RPTCD(5)")
        else rptcd[5] = 0

        if (imaccf (im, "RPTCD6") == YES)
           rptcd[6] = imgeti (im, "RPTCD6")
        else if (imaccf (im, "RPTCD(6)") == YES)
           rptcd[6] = imgeti (im, "RPTCD(6)")
        else rptcd[6] = 0

 end
