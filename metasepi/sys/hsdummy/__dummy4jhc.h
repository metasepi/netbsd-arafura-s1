#ifndef __DUMMY_4_JHC_H
#define __DUMMY_4_JHC_H
#include <sys/param.h>

#define _GNU_SOURCE
#define NDEBUG
#define _JHC_GC		_JHC_GC_JGC
#define _JHC_CONC	_JHC_CONC_NONE
#define _JHC_USE_OWN_STDIO
#define _JHC_STANDALONE	0

#define LC_ALL		0
typedef _BSD_WCHAR_T_	wchar_t; // xxx Simple "char" type is better?
typedef label_t *	jmp_buf;
typedef int		FILE;

void __dummy4jhc_init(void);
void abort(void);
void *malloc(size_t size);
void *realloc(void *ptr, size_t size);
void free(void *ptr);
int posix_memalign(void **ptr, size_t alignment, size_t size);
char *setlocale(int category, const char *locale);
void exit(int status);

#endif /* __DUMMY_4_JHC_H */
