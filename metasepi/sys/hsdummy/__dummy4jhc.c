#include "jhc_rts_header.h"
#include <__dummy4jhc.h>
#include <sys/syslog.h>
#include <sys/kprintf.h>
#include <sys/malloc.h>
#undef malloc
#undef free
#undef realloc

MALLOC_DECLARE(M_HASKELL);
MALLOC_DEFINE(M_HASKELL, "hsalloc", "alloc for haskell");

void
__dummy4jhc_init(void)
{
}

void *
malloc(size_t size)
{
	return kern_malloc(size, M_HASKELL, M_WAITOK);
}

void *
realloc(void *ptr, size_t size)
{
	return kern_realloc(ptr, size, M_HASKELL, M_WAITOK);
}

void
free(void *ptr)
{
	kern_free(ptr, M_HASKELL);
}

int
posix_memalign(void **ptr, size_t alignment, size_t size)
{
	intptr_t p;

	assert(BLOCK_SIZE == alignment);
	assert(MEGABLOCK_SIZE == size);

	p = (intptr_t) malloc(MEGABLOCK_SIZE + BLOCK_SIZE);
	*ptr = (void *) ((p + BLOCK_SIZE - 1) & ~(BLOCK_SIZE - 1));
	return 0;
}

void
abort(void)
{
	panic("abort@dummy4jhc: abort");
	/* NOTREACHED */
}

char *
setlocale(int category, const char *locale)
{
	return NULL;
}

void
exit(int status)
{
	panic("exit@dummy4jhc: exit with status=%d", status);
	/* NOTREACHED */
}

int
jhc_printf_stderr(const char *fmt, ...)
{
	// Copy from sys/kern/subr_prf.c
	va_list ap;

	va_start(ap, fmt);
	vprintf(fmt, ap);
	va_end(ap);
	return 0;
}

int
jhc_fputs_stderr(const char *s)
{
	printf(s);
	return 0;
}

int
jhc_fflush_stdout(void)
{
	return 0;
}

void
jhc_print_profile(void)
{
}

int
jhc_utf8_getchar(void)
{
	return 0;
}

int
jhc_utf8_getc(FILE *f)
{
	return 0;
}

int
jhc_utf8_putchar(int ch)
{
	return 0;
}

int
jhc_utf8_putc(int ch, FILE *f)
{
	return 0;
}

int
uname(struct utsname *name)
{
	return -1;
}
