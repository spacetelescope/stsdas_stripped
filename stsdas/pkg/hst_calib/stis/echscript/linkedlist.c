#include <stdio.h>
#include <c_iraf.h>

#include "echscript.h"

/*****************************************************************************
 *
 * File:     linkedlist.c
 * Purpose:  Contains routines for manipulation of a linear linked list.
 *
 * Routines: CreateNode - create and initialize a new node
 *           CreateList - generate a linked list containing wavelength and
 *                        associated DQ bits
 *           DecodeDQ   - decompose each DQ value into bit settings
 *           AddToFront - add a new node to the front of the list
 *           PrintList  - print specified values from each node
 *           DeleteList - delete the entire linked list
 *
 * Author:   Michele D. De La Pena
 * Date:     24 October 1997
 * Mods:     04 December 1998 - MDD: File name changed and included header 
 *           file changed from e_echscript.h -> echscript.h
 *
******************************************************************************/

/*****************************************************************************
 *
 * Routine: CreateNode
 * Purpose: Routine to allocate and initialize a node in a linear linked list.
 * Output:  This routine returns a pointer to a structure.
 * Author:  Michele D. De La Pena
 * Date:    24 October 1997
 *
******************************************************************************/

DataDQ *
CreateNode (void) {

   /* Declare a new pointer to a Data Quality structure */
   DataDQ *nodeptr;
   
   /* Allocate memory for a new node */
   nodeptr = (DataDQ *) malloc (sizeof(DataDQ));
 
   /* Check the new pointer and initialize */
   if (nodeptr != NULL) {
     nodeptr -> waveDQ  = 0.0;
     nodeptr -> flagsDQ = 0;
     nodeptr -> next    = NULL;
   }
   else {
     sprintf (ErrMsg, "No memory to allocate new DQ node.\n");
     e_error (ErrMsg);
   }

   return (nodeptr);

}

/*****************************************************************************
 *
 * Routine: CreateList.c
 * Purpose: Loop over the DQ array and generate a linked list whose nodes
 *          contain the wavelength and decomposed DQ bits.
 * Output:  This routine returns a pointer to a DQ structure.
 * Author:  Michele D. De La Pena
 * Date:    08 October 1997
 *
******************************************************************************/

DataDQ *
CreateList (short *dq, double *wave, int num, DataDQ *headptr) {

   int i, j;                      /* Loop indices                        */
   int numbits;                   /* Number of bit settings per DQ pixel */
   int bitarray[15];              /* Array of bit settings per DQ pixel  */
   DataDQ *newptr = NULL;         /* Pointer for new node                */

   /* Loop over entire DQ array */
   for (i = 0; i < num; i++) {

        /* Initialize bit array */
        for (j = 0; j < 15; bitarray[j] = 0, j++)
             ;

        numbits = DecodeDQ (*(dq + i), bitarray);

        /* Create a DataDQ node for every wavelength/flag setting */
        for (j = 0; j < numbits; j++) {
             newptr = CreateNode();
             if (newptr != NULL) {
                 newptr -> waveDQ  = (*(wave + i));
                 newptr -> flagsDQ = bitarray[j];
                 newptr -> next    = NULL;
                 headptr = AddToFront (headptr, newptr);
             }
             else 
                 return  (headptr);
        }
   }

   return (headptr);
}

/*****************************************************************************
 *
 * Routine: DecodeDQ.c
 * Purpose: Subroutine to decode a DQ value into the component error
 *          conditions according to bit settings.
 * Output:  This routine returns an array containing the bits set and the
 *          number of bits set.
 * Author:  Michele D. De La Pena
 * Date:    08 October 1997
 *
******************************************************************************/

int 
DecodeDQ (int invalue, int bits[]) {

   int i  = 0;          /* Number of bits actually set       */
   int bc;              /* Loop index                        */

   /* Bit Values */
   int bitcode[] = {1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024,
                    2048, 4096, 8192, 16384};
   
   for (bc = 14; bc >= 0; bc--) {
        if (invalue & bitcode[bc]) {
            bits[i] = bc;
            i++; 
        }
   }

   return (i);
}

/*****************************************************************************
 *
 * Routine: AddToFront
 * Purpose: Routine to add a new node to the front of a linked list.
 * Output:  This routine returns a pointer to a structure.
 * Author:  Michele D. De La Pena
 * Date:    24 October 1997
 *
******************************************************************************/

DataDQ *
AddToFront (DataDQ *headptr, DataDQ *newptr) {

   /* Declare the temporary pointer */
   DataDQ *tempptr = NULL;

   /* Check if the head pointer is set.  If not, create the node and assign *
    * the head pointer.  If it exists, add the new node to the front.       */
   if (headptr == NULL)
       headptr = newptr;
   else {
       tempptr = headptr;
       headptr = newptr;
       headptr -> next = tempptr;
   }
 
   return (headptr);

}

/*****************************************************************************
 *
 * Routine: PrintList
 * Purpose: Print the specified members of each node in a linear linked list.
 * Output:  This routine returns a void.
 * Author:  Michele D. De La Pena
 * Date:    24 October 1997
 *
******************************************************************************/

void
PrintList (FILE *fp, DataDQ *headptr) {

   /* Initialize the DQ pointer */
   DataDQ *currptr = NULL;
 
   /* Walk the list; print the wavelength and DQ flag for each node * 
    * to the IGI output script.                                     */
   for (currptr = headptr; currptr != NULL; currptr = currptr -> next )
        fprintf (fp, "move %.4f %d; dot\n", currptr -> waveDQ, 
                      currptr -> flagsDQ);
 
   return;

 }
 
/*****************************************************************************
 *
 * Routine: DeleteList
 * Purpose: Routine to de-allocate each node in a linear linked list.
 * Output:  This routine returns a void.
 * Author:  Michele D. De La Pena
 * Date:    24 October 1997
 *
******************************************************************************/

void
DeleteList (DataDQ *headptr) {

   /* Initialize the DQ pointers */
   DataDQ *currentptr = headptr -> next;
   DataDQ *nextptr    = NULL;
 
   /* Walk the list and free each node */
   while( currentptr != NULL ) {
       nextptr = currentptr -> next;
       free (currentptr);
       currentptr = nextptr;
   }
 
   /* Free the memory of the head pointer node */
   free (headptr);

   return;

}
