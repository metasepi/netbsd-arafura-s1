#ifndef _DEV_PCI_AUICH_EXTERN_H_
#define _DEV_PCI_AUICH_EXTERN_H_
#include <sys/cdefs.h>

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/kernel.h>
#include <sys/kmem.h>
#include <sys/device.h>
#include <sys/fcntl.h>
#include <sys/proc.h>
#include <sys/sysctl.h>
#include <sys/audioio.h>
#include <sys/bus.h>

#include <dev/pci/pcidevs.h>
#include <dev/pci/pcivar.h>
#include <dev/pci/auichreg.h>

#include <dev/audio_if.h>
#include <dev/mulaw.h>
#include <dev/auconv.h>

#include <dev/ic/ac97reg.h>
#include <dev/ic/ac97var.h>

struct auich_dma {
	bus_dmamap_t map;
	void *addr;
	bus_dma_segment_t segs[1];
	int nsegs;
	size_t size;
	struct auich_dma *next;
};

#define	DMAADDR(p)	((p)->map->dm_segs[0].ds_addr)
#define	KERNADDR(p)	((void *)((p)->addr))

struct auich_cdata {
	struct auich_dmalist ic_dmalist_pcmo[ICH_DMALIST_MAX];
	struct auich_dmalist ic_dmalist_pcmi[ICH_DMALIST_MAX];
	struct auich_dmalist ic_dmalist_mici[ICH_DMALIST_MAX];
};

#define	ICH_CDOFF(x)		offsetof(struct auich_cdata, x)
#define	ICH_PCMO_OFF(x)		ICH_CDOFF(ic_dmalist_pcmo[(x)])
#define	ICH_PCMI_OFF(x)		ICH_CDOFF(ic_dmalist_pcmi[(x)])
#define	ICH_MICI_OFF(x)		ICH_CDOFF(ic_dmalist_mici[(x)])

struct auich_softc {
	device_t sc_dev;
	void *sc_ih;
	kmutex_t sc_lock;
	kmutex_t sc_intr_lock;

	device_t sc_audiodev;
	audio_device_t sc_audev;

	pci_chipset_tag_t sc_pc;
	pcitag_t sc_pt;
	bus_space_tag_t iot;
	bus_space_handle_t mix_ioh;
	bus_size_t mix_size;
	bus_space_handle_t aud_ioh;
	bus_size_t aud_size;
	bus_dma_tag_t dmat;
	pci_intr_handle_t intrh;

	struct ac97_codec_if *codec_if;
	struct ac97_host_if host_if;
	int sc_codecnum;
	int sc_codectype;
	int sc_fixedrate;
	enum ac97_host_flags sc_codecflags;
	bool sc_spdif;

	/* multi-channel control bits */
	int sc_pcm246_mask;
	int sc_pcm2;
	int sc_pcm4;
	int sc_pcm6;

	/* DMA scatter-gather lists. */
	bus_dmamap_t sc_cddmamap;
#define	sc_cddma	sc_cddmamap->dm_segs[0].ds_addr

	struct auich_cdata *sc_cdata;

	struct auich_ring {
		int qptr;
		struct auich_dmalist *dmalist;

		uint32_t start, p, end;
		int blksize;

		void (*intr)(void *);
		void *arg;
	} pcmo, pcmi, mici;

	struct auich_dma *sc_dmas;

	/* SiS 7012 hack */
	int  sc_sample_shift;
	int  sc_sts_reg;
	/* 440MX workaround */
	int  sc_dmamap_flags;
	/* flags */
	int  sc_iose	:1,
		     	:31;

	/* sysctl */
	struct sysctllog *sc_log;
	uint32_t sc_ac97_clock;
	int sc_ac97_clock_mib;

	int	sc_modem_offset;

#define AUICH_AUDIO_NFORMATS	3
#define AUICH_MODEM_NFORMATS	1
	struct audio_format sc_audio_formats[AUICH_AUDIO_NFORMATS];
	struct audio_format sc_modem_formats[AUICH_MODEM_NFORMATS];
	struct audio_encoding_set *sc_encodings;
	struct audio_encoding_set *sc_spdif_encodings;
};

#endif /* _DEV_PCI_AUICH_EXTERN_H_ */
