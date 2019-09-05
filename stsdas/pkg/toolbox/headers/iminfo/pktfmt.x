# PKTFMT -- Print packet format string accoring to packet format code  (HRS)

procedure pktprnt (pktfmt)

int     pktfmt

begin

        call printf ("Packet format code = ")
 
        switch (pktfmt) {
        
        case 101: 
           call printf ("Launch Configuration\n\n")

        case 102:
           call printf ("Aliveness Test\n\n")

        case 103:
           call printf ("PDMaps\n\n")

        case 104:
           call printf ("Other Ground Cmd-generated Data\n\n")

        case 111:
           call printf ("ZMETEST: Reset Test\n\n")
 
        case 112:
           call printf ("ZMETEST: Deflection/Carrousel Test\n\n")

        case 113:
           call printf ("ZMETEST: Integration Test\n\n")

        case 114:
           call printf ("ZMETEST: Anti-coincidence/Pulsar Test\n\n")
   
        case 115:
           call printf ("ZMETEST: Lamp/Door/Shutter Test\n\n")

        case 116:
           call printf ("Configurations and Mode Transitions\n\n")

        case 117:
           call printf ("Other Stored Command Tests\n\n")

        case 118:
           call printf ("ZMATEST: Aliveness Test\n\n")

        case 121:
           call printf ("Standard PHA\n\n")

        case 122:
           call printf ("Ion Test\n\n")

        case 123:
           call printf ("Other PHA data\n\n")

        case 131:
           call printf ("Configurations\n\n")

        case 132:
           call printf ("Darks (Accumulation Mode)\n\n")

        case 133:
           call printf ("Y scans\n\n")

        case 134:
           call printf ("X scans\n\n")

        case 135:
           call printf ("Wavelength Calibrations (SC lamps)\n\n")

        case 136:
           call printf ("Software and Other Functional Tests\n\n")

        case 137:
           call printf ("Mask Scan\n\n")

        case 138:
           call printf ("Rapid Readout Mode: Wavelength Calibration\n\n")

        case 139:
           call printf ("Rapid Readout Mode: Flatfield and Darks\n\n")

        case 141:
           call printf ("Large Aperture Target Acquisitions\n\n")

        case 142:
           call printf ("Small Aperture Target Acquisitions\n\n")

        case 143:
           call printf ("Image Mode Field Maps (Mirrors and Gratings)\n\n")

        case 144:
           call printf ("Software and Other Functional Tests\n\n")

        case 151:
           call printf ("Calibrations\n\n")

        case 152:
           call printf ("Rapid Readout Mode (External Source)\n\n")

        case 153:
           call printf ("Accumulation Mode\n\n")

        case 154:
           call printf ("FP-Split Observations\n\n")

        default:
           call printf ("Unknown\n\n")

        }
end

