#include <sys/param.h>
#include <sys/systm.h>
#include <sys/kernel.h>
#include <sys/kmem.h>
#include <sys/device.h>

#include <dev/pci/auich_extern_SNATCHED.h>

static inline void
funptr_apply_p1(void (*funptr)(void), void *p1)
{
	void (*lock)(void *) = (void (*)(void *)) funptr;
	(*lock)(p1);
}
