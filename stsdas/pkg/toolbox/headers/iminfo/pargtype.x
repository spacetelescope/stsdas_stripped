# PARGTYPE -- Generate a string for the image data type

procedure pargtype (dtype)

int     dtype

begin   
         
        switch (dtype) {
     
        case TY_UBYTE:
            call pargstr ("ubyte")

        case TY_BOOL:
            call pargstr ("bool")

        case TY_CHAR:
            call pargstr ("char")

        case TY_SHORT:
            call pargstr ("short")

        case TY_USHORT:
            call pargstr ("ushort")

        case TY_INT:
            call pargstr ("int")

        case TY_LONG:
            call pargstr ("long")

        case TY_REAL:
            call pargstr ("real")

        case TY_DOUBLE:
            call pargstr ("double")

        case TY_COMPLEX:
            call pargstr ("complex")

        case TY_POINTER:
            call pargstr ("pointer")

        case TY_STRUCT:
            call pargstr ("struct")

        default:
            call pargstr ("unknown datatype")
        }
end      

