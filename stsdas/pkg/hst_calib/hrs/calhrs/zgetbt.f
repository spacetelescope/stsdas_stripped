        SUBROUTINE ZGETBT(INT,BIT1,BIT2,RESULT)
*
*  Module number:
*
*  Module name: ZGETBT
*
*  Keyphrase:
*  ----------
*       Extract specified bits
*  Description:
*  ------------
*       This routine will extract bits BIT1 through BIT2 from
*       UNSIGNED*2 integer value stored in integer*4 value INT
*       and place the result into RESULT.
*       Bit numbers start at true bit 16 since they are i*2
*       bit numbers in an i*4 word.
*       ALSO NOTE - all numbers are positive and thus 1 or 2's
*               complement hardware is not an issue.
*
*  FORTRAN name: zgetbt.for
*
*
*  Subroutines Called:
*  -------------------
*
*  History:
*  --------
*  Version      Date        Author          Description
*    1        D. Lindler  Feb. 1989     Designed and coded
*-------------------------------------------------------------------------------
*
* Inputs
*       INT - value to extract bits from (integer)
*       bit1 - FIRST BIT (starting at sign bit = 0)
*       bit2 - LAST BIT (integer 0 to 15)
* Outputs
*       RESULT - integer result containing the extracted bits
*------------------------------------------------------------------------------
C
C LOCAL VARIABLES
C
        INTEGER NBITS,BIT1,BIT2,RESULT,INT,RSHIFT
        REAL SHIFT
C
C------------------------------------------------------------------------------
        NBITS=BIT2-BIT1+1
C
C SHIFT INPUT TO PLACE BIT2 IN THE LAST BIT
C
        RESULT=INT
        RESULT=RESULT/(2**(15-BIT2))
C
C SHIFT NBITS OFF OF RESULT AND THEN SHIFT BACK ZEROS
C
        SHIFT=2**NBITS
        RSHIFT=RESULT/SHIFT
        RSHIFT=RSHIFT*SHIFT
C
C NOW SUBTRACT UPPER ORDER BITS
C
        RESULT=RESULT-RSHIFT
        RETURN
        END
