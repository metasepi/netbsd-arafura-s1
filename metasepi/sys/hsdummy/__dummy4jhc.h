#ifndef __DUMMY_4_JHC_H
#define __DUMMY_4_JHC_H
#include <sys/param.h>

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
