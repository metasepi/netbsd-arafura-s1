#include "jhc_rts_header.h"
#include "rts/conc.h"
#include <__dummy4jhc.h>
#include <sys/syslog.h>
#include <sys/kprintf.h>
#include <ddb/ddb.h>
#include <machine/cpufunc.h>
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

#if _JHC_PROFILE || _JHC_PROFILE_GCONLY
struct profile_stack {
	u_int64_t count;
	u_int64_t t_total;
	u_int64_t t_max;
	u_int64_t t_start;
};

/*
 * xxx If there are many contexts, use this share and conflict.
 * xxx The implementation has only one context.
 */
struct profile_stack gc_alloc_time;
struct profile_stack gc_gc_time;

void
jhc_profile_push(struct profile_stack *ps)
{
        jhc_rts_lock();
	ps->count++;
	__asm __volatile("rdtsc":"=A"(ps->t_start)); // x86 only
        jhc_rts_unlock();
}

void
jhc_profile_pop(struct profile_stack *ps)
{
	u_int64_t t_now;
        jhc_rts_lock();
	__asm __volatile("rdtsc":"=A"(t_now)); // x86 only
	t_now -= ps->t_start;
	ps->t_total += t_now;
	if (t_now > ps->t_max) {
		ps->t_max = t_now;
	}
        jhc_rts_unlock();
}

void jhc_profile_reset(void);
void
jhc_profile_reset(void)
{
	memset(&gc_alloc_time, 0, sizeof(struct profile_stack));
	memset(&gc_gc_time, 0, sizeof(struct profile_stack));
}

void jhc_profile_show(void);
void
jhc_profile_show(void)
{
	db_printf("_JHC_JGC_NAIVEGC=%d _JHC_JGC_MEGABLOCK_SHIFT=%d _JHC_JGC_BLOCK_SHIFT=%d\n",
	    _JHC_JGC_NAIVEGC, _JHC_JGC_MEGABLOCK_SHIFT, _JHC_JGC_BLOCK_SHIFT);
	db_printf("gc_alloc_time = {count=%llx t_total=%llx t_max=%llx}\n",
	    gc_alloc_time.count, gc_alloc_time.t_total, gc_alloc_time.t_max);
	db_printf("gc_gc_time = {count=%llx t_total=%llx t_max=%llx}\n",
	    gc_gc_time.count, gc_gc_time.t_total, gc_gc_time.t_max);
}
#endif
