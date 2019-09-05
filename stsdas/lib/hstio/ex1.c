# include <hstio.h>
# include <stdio.h>

/***************************************************************************

	Illustration of the low-level I/O routines
			using
	Data Structures and I/O Function Declarations for
	STScI Calibration Pipeline Software for STIS and NICMOS

Input data structures, such as FloatTwoDArray and Hdr, each have
initilization, allocation, and free routines: initFloatData,
allocFloatData, freeFloatData. 

On input, if the data structures already have storage allocated and that
storage is the same size as the requested data, that data storage is
reused.  If it is not the same size, the old storage is freed and new
storage is allocated.  If storage has not been allocated, then the input
routines allocate it. 

It is the responsibility of the application program to free the
allocated storage. 

***************************************************************************/

void detect_err() {
	printf("HSTIO Error (%d): %s\n",hstio_err(),hstio_errmsg());
	exit(0);
}

int main(int argc, char **argv) {
	IODescPtr in;
	FloatTwoDArray x = IFloatData;
	Hdr h = IHdr;
	IODescPtr out;
	FloatTwoDArray z = IFloatData;
	int i, j;

	/* install the error handler */
	push_hstioerr(detect_err);

	initFloatData(&x); /* Initialize the data structures.  Failure to */
	initFloatData(&z); /* initilize the data structures increases the */
	initHdr(&h);	 /* probablilty of getting a core dump.         */

	/* This section illustrates the input case. */
	in = openInputImage("infile.fit","",0);
	getFloatData(in,&x);
	getHeader(in,&h);
	closeImage(in);
	for (j = x.ny - 1; j >= 0; --j) {	/* Do something with 	*/
	    for (i = 0; i < x.nx; ++i)		/* the data,		*/
		printf("%g ", Pix(x,i,j));
	    printf("\n");
        }
	for (i = 0; i < h.nlines; ++i)		/* and the header.	*/
		printf("%s\n", h.array[i]);
	freeFloatData(&x);			/* The application must	*/
	freeHdr(&h);				/* free storage.	*/

	/* This section illustrates the output case. */
	allocFloatData(&z,5,5);			/* Storage must be 	*/
	allocHdr(&h,25);			/* allocated.		*/
	for (j = z.ny - 1; j >= 0; --j) 	/* Fill the data array 	*/
	    for (i = 0; i < z.nx; ++i)		
		Pix(z,i,j) = (i + 1) * 1000.0 + j + 1;
	/* And, also fill the header array */
	addFloatKw(&h,"CRVAL1",1.5," "); 
	addFloatKw(&h,"CRVAL2",2.5," "); 
	addFloatKw(&h,"CRPIX1",3.5," "); 
	addFloatKw(&h,"CRPIX2",4.5," "); 
	addFloatKw(&h,"CD1_1",5.5," "); 
	addFloatKw(&h,"CD1_2",6.5," "); 
	addFloatKw(&h,"CD2_1",7.5," "); 
	addFloatKw(&h,"CD2_2",8.5," "); 
	addIntKw(&h,"TEST1",34," "); 
	addBoolKw(&h,"TEST2",True," "); 

	/*
	out = openOutputImage("outfile1.fit","XYZ",1,&h,5,5,0);
	*/
	out = openOutputImage("outfile1.fit","",0,&h,5,5,0);
	/* the openOutputImage routine writes the header */
	putFloatData(out,&z);			/* write the data.	*/
	closeImage(out);
	freeFloatData(&z);			/* Then, free the 	*/
	freeHdr(&h);				/* storage.		*/

	/* This section illustrates copying data from one file to another */
	in = openInputImage("infile.fit","sci",0);
	getHeader(in,&h);
	getFloatData(in,&x);
	closeImage(in);
	/*
	out = openOutputImage("outfile2.fit","ABC",1,&h,
		getNaxis1(in),getNaxis2(in),0);
	*/
	out = openOutputImage("outfile2.fit","",0,&h,
		getNaxis1(in),getNaxis2(in),0);
	putFloatData(out,&x);
	closeImage(out);
	freeFloatData(&x);
	freeHdr(&h);

	return 0;
}

