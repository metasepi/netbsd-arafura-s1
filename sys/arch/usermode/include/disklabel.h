/* $NetBSD: disklabel.h,v 1.12 2012/02/05 17:40:08 reinoud Exp $ */

/*
 * Automatically generated by genheaders.sh on Sun Feb  5 18:39:34 CET 2012
 * Do not modify directly!
 */
#ifndef _USERMODE_DISKLABEL_H
#define _USERMODE_DISKLABEL_H

#if defined(__i386__)
#include "../../i386/include/disklabel.h"
#elif defined(__x86_64__)
#include "../../amd64/include/disklabel.h"
#else
#error port me
#endif
#include <machine/types.h>
#ifndef __HAVE_OLD_DISKLABEL
#undef DISKUNIT
#undef DISKPART
#undef DISKMINOR
#endif

#endif
