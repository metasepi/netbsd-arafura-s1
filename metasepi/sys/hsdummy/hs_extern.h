#include <sys/param.h>
#include <sys/systm.h>
#include <sys/kernel.h>
#include <sys/kmem.h>
#include <sys/device.h>

#include <dev/pci/auich_extern_SNATCHED.h>

static inline void
funptr_ac97_codec_if_vtbl_lock(void (*funptr)(void), struct ac97_codec_if *codec_if)
{
	void (*lock)(struct ac97_codec_if *) = (void (*)(struct ac97_codec_if *)) funptr;
	(*lock)(codec_if);
}
