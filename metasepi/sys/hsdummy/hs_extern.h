#include <sys/param.h>
#include <sys/systm.h>
#include <sys/kernel.h>
#include <sys/kmem.h>
#include <sys/device.h>

#include <dev/pci/auich_extern_SNATCHED.h>
#include <dev/pci/hdaudio/hdaudiovar.h>
#include <dev/pci/hdaudio/hdaudioreg.h>
#include <dev/pci/hdaudio/hdaudioio.h>

int	auich_set_rate(struct auich_softc *, int, u_long);
int	auich_write_codec(void *, uint8_t, uint16_t);
