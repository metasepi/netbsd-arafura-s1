/*	$NetBSD: mfptoms.c,v 1.1.1.1 2009/12/13 16:55:03 kardel Exp $	*/

/*
 * mfptoms - Return an asciized signed long fp number in milliseconds
 */
#include "ntp_fp.h"
#include "ntp_stdlib.h"

char *
mfptoms(
	u_long fpi,
	u_long fpf,
	short ndec
	)
{
	int isneg;

	if (M_ISNEG(fpi, fpf)) {
		isneg = 1;
		M_NEG(fpi, fpf);
	} else
	    isneg = 0;

	return dolfptoa(fpi, fpf, isneg, ndec, 1);
}
