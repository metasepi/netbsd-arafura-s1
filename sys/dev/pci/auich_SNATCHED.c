/*	$NetBSD: auich.c,v 1.140 2011/11/24 03:35:58 mrg Exp $	*/

/*-
 * Copyright (c) 2000, 2004, 2005, 2008 The NetBSD Foundation, Inc.
 * All rights reserved.
 *
 * This code is derived from software contributed to The NetBSD Foundation
 * by Jason R. Thorpe and by Charles M. Hannum.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE NETBSD FOUNDATION, INC. AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE FOUNDATION OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * Copyright (c) 2000 Michael Shalayeff
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR OR HIS RELATIVES BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF MIND, USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 *
 *	from OpenBSD: ich.c,v 1.3 2000/08/11 06:17:18 mickey Exp
 */

/*
 * Copyright (c) 2000 Katsurajima Naoto <raven@katsurajima.seya.yokohama.jp>
 * Copyright (c) 2001 Cameron Grant <cg@freebsd.org>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHERIN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THEPOSSIBILITY OF
 * SUCH DAMAGE.
 *
 * auich_calibrate() was from FreeBSD: ich.c,v 1.22 2002/06/27 22:36:01 scottl Exp
 */


/* #define	AUICH_DEBUG */
/*
 * AC'97 audio found on Intel 810/820/440MX chipsets.
 *	http://developer.intel.com/design/chipsets/datashts/290655.htm
 *	http://developer.intel.com/design/chipsets/manuals/298028.htm
 * ICH3:http://www.intel.com/design/chipsets/datashts/290716.htm
 * ICH4:http://www.intel.com/design/chipsets/datashts/290744.htm
 * ICH5:http://www.intel.com/design/chipsets/datashts/252516.htm
 * AMD8111:
 *	http://www.amd.com/us-en/assets/content_type/white_papers_and_tech_docs/24674.pdf
 *	http://www.amd.com/us-en/assets/content_type/white_papers_and_tech_docs/25720.pdf
 *
 * TODO:
 *	- Add support for the dedicated microphone input.
 *
 * NOTE:
 *      - The 440MX B-stepping at running 100MHz has a hardware erratum.
 *        It causes PCI master abort and hangups until cold reboot.
 *        http://www.intel.com/design/chipsets/specupdt/245051.htm
 */

#include <sys/cdefs.h>
__KERNEL_RCSID(0, "$NetBSD: auich.c,v 1.140 2011/11/24 03:35:58 mrg Exp $");

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

#include <dev/pci/auich_extern_SNATCHED.h>

/* Debug */
#ifdef AUICH_DEBUG
#define	DPRINTF(l,x)	do { if (auich_debug & (l)) printf x; } while(0)
int auich_debug = 0xfffe;
#define	ICH_DEBUG_CODECIO	0x0001
#define	ICH_DEBUG_DMA		0x0002
#define	ICH_DEBUG_INTR		0x0004
#else
#define	DPRINTF(x,y)	/* nothing */
#endif

static int	auich_match(device_t, cfdata_t, void *);
static void	auich_attach(device_t, device_t, void *);
static int	auich_detach(device_t, int);
static void	auich_childdet(device_t, device_t);

CFATTACH_DECL2_NEW(auich, sizeof(struct auich_softc),
    auich_match, auich_attach, auich_detach, NULL, NULL, auich_childdet);

static int	auich_trigger_input(void *, void *, void *, int,
		    void (*)(void *), void *, const audio_params_t *);

static int	auich_alloc_cdata(struct auich_softc *);

static bool	auich_resume(device_t, const pmf_qual_t *);
static int	auich_sysctl_verify(SYSCTLFN_ARGS);
static void	auich_finish_attach(device_t);
static void	auich_calibrate(struct auich_softc *);
static void	auich_clear_cas(struct auich_softc *);

extern int	auichIntr(void *);
extern void	auichIntrPipe(struct auich_softc *, int, struct auich_ring *);

static int	auich_attach_codec(void *, struct ac97_codec_if *);
static int	auich_read_codec(void *, uint8_t, uint16_t *);
static int	auich_reset_codec(void *);
static enum ac97_host_flags	auich_flags_codec(void *);
static void	auich_spdif_event(void *, bool);

extern int	auichOpen(void *, int);
extern void	auichClose(void *);
extern int	auichQueryEncoding(void *, struct audio_encoding *);
extern int	auichSetParams(void *, int, int, audio_params_t *, audio_params_t *, stream_filter_list_t *, stream_filter_list_t *);
extern int	auichWriteCodec(void *, uint8_t, uint16_t);
extern int	auichRoundBlocksize(void *, int, int, const audio_params_t *);
extern void	auichHaltPipe(struct auich_softc *, int);
extern int	auichHaltOutput(void *);
extern int	auichHaltInput(void *);
extern int	auichGetdev(void *, struct audio_device *);
extern int	auichSetPort(void *, mixer_ctrl_t *);
extern int	auichGetPort(void *, mixer_ctrl_t *);
extern int	auichQueryDevinfo(void *, mixer_devinfo_t *);
extern int	auichAllocmem(struct auich_softc *, size_t, size_t,
		    struct auich_dma *);
extern int	auichFreemem(struct auich_softc *, struct auich_dma *);
extern void	*auichAllocm(void *, int, size_t);
extern void	auichFreem(void *, void *, size_t);
extern size_t	auichRoundBuffersize(void *, int, size_t);
extern paddr_t	auichMappage(void *, void *, off_t, int);
extern int	auichGetProps(void *);
extern void	auichGetLocks(void *, kmutex_t **, kmutex_t **);
extern void	auichTriggerPipe(struct auich_softc *, int, struct auich_ring *);
extern int	auichTriggerOutput(void *, void *, void *, int,
		    void (*)(void *), void *, const audio_params_t *);

static const struct audio_hw_if auich_hw_if = {
	auichOpen,
	auichClose,
	NULL,			/* drain */
	auichQueryEncoding,
	auichSetParams,
	auichRoundBlocksize,
	NULL,			/* commit_setting */
	NULL,			/* init_output */
	NULL,			/* init_input */
	NULL,			/* start_output */
	NULL,			/* start_input */
	auichHaltOutput,
	auichHaltInput,
	NULL,			/* speaker_ctl */
	auichGetdev,
	NULL,			/* getfd */
	auichSetPort,
	auichGetPort,
	auichQueryDevinfo,
	auichAllocm,
	auichFreem,
	auichRoundBuffersize,
	auichMappage,
	auichGetProps,
	auichTriggerOutput,
	auich_trigger_input,
	NULL,			/* dev_ioctl */
	auichGetLocks,
};

#define AUICH_FORMATS_1CH	0
#define AUICH_FORMATS_4CH	1
#define AUICH_FORMATS_6CH	2
static const struct audio_format auich_audio_formats[AUICH_AUDIO_NFORMATS] = {
	{NULL, AUMODE_PLAY | AUMODE_RECORD, AUDIO_ENCODING_SLINEAR_LE, 16, 16,
	 2, AUFMT_STEREO, 0, {8000, 48000}},
	{NULL, AUMODE_PLAY, AUDIO_ENCODING_SLINEAR_LE, 16, 16,
	 4, AUFMT_SURROUND4, 0, {8000, 48000}},
	{NULL, AUMODE_PLAY, AUDIO_ENCODING_SLINEAR_LE, 16, 16,
	 6, AUFMT_DOLBY_5_1, 0, {8000, 48000}},
};

static const struct audio_format auich_spdif_formats[AUICH_SPDIF_NFORMATS] = {
	{NULL, AUMODE_PLAY | AUMODE_RECORD, AUDIO_ENCODING_SLINEAR_LE, 16, 16,
	 2, AUFMT_STEREO, 1, {48000}},
};

static const struct audio_format auich_modem_formats[AUICH_MODEM_NFORMATS] = {
	{NULL, AUMODE_PLAY | AUMODE_RECORD, AUDIO_ENCODING_SLINEAR_LE, 16, 16,
	 1, AUFMT_MONAURAL, 0, {8000, 16000}},
};

#define PCI_ID_CODE0(v, p)	PCI_ID_CODE(PCI_VENDOR_##v, PCI_PRODUCT_##v##_##p)
#define PCIID_ICH		PCI_ID_CODE0(INTEL, 82801AA_ACA)
#define PCIID_ICH0		PCI_ID_CODE0(INTEL, 82801AB_ACA)
#define PCIID_ICH2		PCI_ID_CODE0(INTEL, 82801BA_ACA)
#define PCIID_440MX		PCI_ID_CODE0(INTEL, 82440MX_ACA)
#define PCIID_ICH3		PCI_ID_CODE0(INTEL, 82801CA_AC)
#define PCIID_ICH4		PCI_ID_CODE0(INTEL, 82801DB_AC)
#define PCIID_ICH5		PCI_ID_CODE0(INTEL, 82801EB_AC)
#define PCIID_ICH6		PCI_ID_CODE0(INTEL, 82801FB_AC)
#define PCIID_ICH7		PCI_ID_CODE0(INTEL, 82801G_ACA)
#define PCIID_I6300ESB		PCI_ID_CODE0(INTEL, 6300ESB_ACA)
#define PCIID_SIS7012		PCI_ID_CODE0(SIS, 7012_AC)
#define PCIID_NFORCE		PCI_ID_CODE0(NVIDIA, NFORCE_MCP_AC)
#define PCIID_NFORCE2		PCI_ID_CODE0(NVIDIA, NFORCE2_MCPT_AC)
#define PCIID_NFORCE2_400	PCI_ID_CODE0(NVIDIA, NFORCE2_400_MCPT_AC)
#define PCIID_NFORCE3		PCI_ID_CODE0(NVIDIA, NFORCE3_MCPT_AC)
#define PCIID_NFORCE3_250	PCI_ID_CODE0(NVIDIA, NFORCE3_250_MCPT_AC)
#define PCIID_NFORCE4		PCI_ID_CODE0(NVIDIA, NFORCE4_AC)
#define	PCIID_NFORCE430 	PCI_ID_CODE0(NVIDIA, NFORCE430_AC)
#define PCIID_AMD768		PCI_ID_CODE0(AMD, PBC768_AC)
#define PCIID_AMD8111		PCI_ID_CODE0(AMD, PBC8111_AC)

#define	PCIID_ICH3MODEM		PCI_ID_CODE0(INTEL, 82801CA_MOD)
#define PCIID_ICH4MODEM		PCI_ID_CODE0(INTEL, 82801DB_MOD)
#define PCIID_ICH6MODEM 	PCI_ID_CODE0(INTEL, 82801FB_ACM)

struct auich_devtype {
	pcireg_t	id;
	const char	*name;
	const char	*shortname;	/* must be less than 11 characters */
};

static const struct auich_devtype auich_audio_devices[] = {
	{ PCIID_ICH,	"i82801AA (ICH) AC-97 Audio",	"ICH" },
	{ PCIID_ICH0,	"i82801AB (ICH0) AC-97 Audio",	"ICH0" },
	{ PCIID_ICH2,	"i82801BA (ICH2) AC-97 Audio",	"ICH2" },
	{ PCIID_440MX,	"i82440MX AC-97 Audio",		"440MX" },
	{ PCIID_ICH3,	"i82801CA (ICH3) AC-97 Audio",	"ICH3" },
	{ PCIID_ICH4,	"i82801DB/DBM (ICH4/ICH4M) AC-97 Audio", "ICH4" },
	{ PCIID_ICH5,	"i82801EB (ICH5) AC-97 Audio",	"ICH5" },
	{ PCIID_ICH6,	"i82801FB (ICH6) AC-97 Audio",	"ICH6" },
	{ PCIID_ICH7,	"i82801GB/GR (ICH7) AC-97 Audio",	"ICH7" },
	{ PCIID_I6300ESB,	"Intel 6300ESB AC-97 Audio",	"I6300ESB" },
	{ PCIID_SIS7012, "SiS 7012 AC-97 Audio",	"SiS7012" },
	{ PCIID_NFORCE,	"nForce MCP AC-97 Audio",	"nForce" },
	{ PCIID_NFORCE2, "nForce2 MCP-T AC-97 Audio",	"nForce2" },
	{ PCIID_NFORCE2_400, "nForce2 400 MCP-T AC-97 Audio",	"nForce2" },
	{ PCIID_NFORCE3, "nForce3 MCP-T AC-97 Audio",	"nForce3" },
	{ PCIID_NFORCE3_250, "nForce3 250 MCP-T AC-97 Audio", "nForce3" },
	{ PCIID_NFORCE4, "nForce4 AC-97 Audio",		"nForce4" },
	{ PCIID_NFORCE430, "nForce430 (MCP51) AC-97 Audio", "nForce430" },
	{ PCIID_AMD768,	"AMD768 AC-97 Audio",		"AMD768" },
	{ PCIID_AMD8111,"AMD8111 AC-97 Audio",		"AMD8111" },
	{ 0,		NULL,				NULL },
};

static const struct auich_devtype auich_modem_devices[] = {
#ifdef AUICH_ATTACH_MODEM
	{ PCIID_ICH3MODEM, "i82801CA (ICH3) AC-97 Modem", "ICH3MODEM" },
	{ PCIID_ICH4MODEM, "i82801DB (ICH4) AC-97 Modem", "ICH4MODEM" },
	{ PCIID_ICH6MODEM, "i82801FB (ICH6) AC-97 Modem", "ICH6MODEM" },
#endif
	{ 0,		NULL,				NULL },
};

struct audio_format *
get_auich_spdif_formats(void) {
	return (struct audio_format *) auich_spdif_formats;
}

static const struct auich_devtype *
auich_lookup(struct pci_attach_args *pa, const struct auich_devtype *auich_devices)
{
	const struct auich_devtype *d;

	for (d = auich_devices; d->name != NULL; d++) {
		if (pa->pa_id == d->id)
			return d;
	}

	return NULL;
}

static int
auich_match(device_t parent, cfdata_t match, void *aux)
{
	struct pci_attach_args *pa;

	pa = aux;
	if (auich_lookup(pa, auich_audio_devices) != NULL)
		return 1;
	if (auich_lookup(pa, auich_modem_devices) != NULL)
		return 1;

	return 0;
}

static void
auich_attach(device_t parent, device_t self, void *aux)
{
	struct auich_softc *sc = device_private(self);
	struct pci_attach_args *pa;
	pcireg_t v, subdev;
	const char *intrstr;
	const struct auich_devtype *d;
	const struct sysctlnode *node, *node_ac97clock;
	int err, node_mib, i;

	sc->sc_dev = self;
	pa = aux;

	if ((d = auich_lookup(pa, auich_modem_devices)) != NULL) {
		sc->sc_modem_offset = 0x10;
		sc->sc_codectype = AC97_CODEC_TYPE_MODEM;
	} else if ((d = auich_lookup(pa, auich_audio_devices)) != NULL) {
		sc->sc_modem_offset = 0;
		sc->sc_codectype = AC97_CODEC_TYPE_AUDIO;
	} else
		panic("auich_attach: impossible");

	if (sc->sc_codectype == AC97_CODEC_TYPE_AUDIO)
		aprint_naive(": Audio controller\n");
	else
		aprint_naive(": Modem controller\n");

	sc->sc_pc = pa->pa_pc;
	sc->sc_pt = pa->pa_tag;

	aprint_normal(": %s\n", d->name);

	if (d->id == PCIID_ICH4 || d->id == PCIID_ICH5 || d->id == PCIID_ICH6
	    || d->id == PCIID_ICH7 || d->id == PCIID_I6300ESB
	    || d->id == PCIID_ICH4MODEM) {
		/*
		 * Use native mode for Intel 6300ESB and ICH4/ICH5/ICH6/ICH7
		 */

		if (pci_mapreg_map(pa, ICH_MMBAR, PCI_MAPREG_TYPE_MEM, 0,
		    &sc->iot, &sc->mix_ioh, NULL, &sc->mix_size)) {
			goto retry_map;
		}
		if (pci_mapreg_map(pa, ICH_MBBAR, PCI_MAPREG_TYPE_MEM, 0,
		    &sc->iot, &sc->aud_ioh, NULL, &sc->aud_size)) {
			goto retry_map;
		}
		goto map_done;
	} else
		goto non_native_map;

retry_map:
	sc->sc_iose = 1;
	v = pci_conf_read(pa->pa_pc, pa->pa_tag, ICH_CFG);
	pci_conf_write(pa->pa_pc, pa->pa_tag, ICH_CFG,
		       v | ICH_CFG_IOSE);

non_native_map:
	if (pci_mapreg_map(pa, ICH_NAMBAR, PCI_MAPREG_TYPE_IO, 0,
			   &sc->iot, &sc->mix_ioh, NULL, &sc->mix_size)) {
		aprint_error_dev(self, "can't map codec i/o space\n");
		return;
	}
	if (pci_mapreg_map(pa, ICH_NABMBAR, PCI_MAPREG_TYPE_IO, 0,
			   &sc->iot, &sc->aud_ioh, NULL, &sc->aud_size)) {
		aprint_error_dev(self, "can't map device i/o space\n");
		return;
	}

map_done:
	sc->dmat = pa->pa_dmat;

	/* enable bus mastering */
	v = pci_conf_read(pa->pa_pc, pa->pa_tag, PCI_COMMAND_STATUS_REG);
	pci_conf_write(pa->pa_pc, pa->pa_tag, PCI_COMMAND_STATUS_REG,
	    v | PCI_COMMAND_MASTER_ENABLE | PCI_COMMAND_BACKTOBACK_ENABLE);

	mutex_init(&sc->sc_lock, MUTEX_DEFAULT, IPL_NONE);
	mutex_init(&sc->sc_intr_lock, MUTEX_DEFAULT, IPL_AUDIO);

	/* Map and establish the interrupt. */
	if (pci_intr_map(pa, &sc->intrh)) {
		aprint_error_dev(self, "can't map interrupt\n");
		return;
	}
	intrstr = pci_intr_string(pa->pa_pc, sc->intrh);
	sc->sc_ih = pci_intr_establish(pa->pa_pc, sc->intrh, IPL_AUDIO,
	    auichIntr, sc);
	if (sc->sc_ih == NULL) {
		aprint_error_dev(self, "can't establish interrupt");
		if (intrstr != NULL)
			aprint_error(" at %s", intrstr);
		aprint_error("\n");
		return;
	}
	aprint_normal_dev(self, "interrupting at %s\n", intrstr);

	snprintf(sc->sc_audev.name, MAX_AUDIO_DEV_LEN, "%s AC97", d->shortname);
	snprintf(sc->sc_audev.version, MAX_AUDIO_DEV_LEN,
		 "0x%02x", PCI_REVISION(pa->pa_class));
	strlcpy(sc->sc_audev.config, device_xname(self), MAX_AUDIO_DEV_LEN);

	/* SiS 7012 needs special handling */
	if (d->id == PCIID_SIS7012) {
		sc->sc_sts_reg = ICH_PICB;
		sc->sc_sample_shift = 0;
		sc->sc_pcm246_mask = ICH_SIS_PCM246_MASK;
		sc->sc_pcm2 = ICH_SIS_PCM2;
		sc->sc_pcm4 = ICH_SIS_PCM4;
		sc->sc_pcm6 = ICH_SIS_PCM6;
		/* Un-mute output. From Linux. */
		bus_space_write_4(sc->iot, sc->aud_ioh, ICH_SIS_NV_CTL,
		    bus_space_read_4(sc->iot, sc->aud_ioh, ICH_SIS_NV_CTL) |
		    ICH_SIS_CTL_UNMUTE);
	} else {
		sc->sc_sts_reg = ICH_STS;
		sc->sc_sample_shift = 1;
		sc->sc_pcm246_mask = ICH_PCM246_MASK;
		sc->sc_pcm2 = ICH_PCM2;
		sc->sc_pcm4 = ICH_PCM4;
		sc->sc_pcm6 = ICH_PCM6;
	}

	/* Workaround for a 440MX B-stepping erratum */
	sc->sc_dmamap_flags = BUS_DMA_COHERENT;
	if (d->id == PCIID_440MX) {
		sc->sc_dmamap_flags |= BUS_DMA_NOCACHE;
		aprint_normal_dev(self, "DMA bug workaround enabled\n");
	}

	/* Set up DMA lists. */
	sc->pcmo.qptr = sc->pcmi.qptr = sc->mici.qptr = 0;
	auich_alloc_cdata(sc);

	DPRINTF(ICH_DEBUG_DMA, ("auich_attach: lists %p %p %p\n",
	    sc->pcmo.dmalist, sc->pcmi.dmalist, sc->mici.dmalist));

	/* Modem codecs are always the secondary codec on ICH */
	sc->sc_codecnum = sc->sc_codectype == AC97_CODEC_TYPE_MODEM ? 1 : 0;

	sc->host_if.arg = sc;
	sc->host_if.attach = auich_attach_codec;
	sc->host_if.read = auich_read_codec;
	sc->host_if.write = auichWriteCodec;
	sc->host_if.reset = auich_reset_codec;
	sc->host_if.flags = auich_flags_codec;
	sc->host_if.spdif_event = auich_spdif_event;

	subdev = pci_conf_read(pa->pa_pc, pa->pa_tag, PCI_SUBSYS_ID_REG);
	switch (subdev) {
	case 0x202f161f:	/* Gateway 7326GZ */
	case 0x203a161f:	/* Gateway 4028GZ */
	case 0x204c161f:	/* Kvazar-Micro Senator 3592XT */
	case 0x8144104d:	/* Sony VAIO PCG-TR* */
	case 0x8197104d:	/* Sony S1XP */
	case 0x81c0104d:	/* Sony VAIO type T */
	case 0x81c5104d:	/* Sony VAIO VGN-B1XP */
		sc->sc_codecflags = AC97_HOST_INVERTED_EAMP;
		break;
	default:
		sc->sc_codecflags = 0;
		break;
	}

	if (ac97_attach_type(&sc->host_if, self, sc->sc_codectype,
	    &sc->sc_lock) != 0)
		return;

	mutex_enter(&sc->sc_lock);
	sc->codec_if->vtbl->unlock(sc->codec_if);
	sc->sc_fixedrate = AC97_IS_FIXED_RATE(sc->codec_if);

	/* setup audio_format */
	if (sc->sc_codectype == AC97_CODEC_TYPE_AUDIO) {
		memcpy(sc->sc_audio_formats, auich_audio_formats, sizeof(auich_audio_formats));
		if (!AC97_IS_4CH(sc->codec_if))
			AUFMT_INVALIDATE(&sc->sc_audio_formats[AUICH_FORMATS_4CH]);
		if (!AC97_IS_6CH(sc->codec_if))
			AUFMT_INVALIDATE(&sc->sc_audio_formats[AUICH_FORMATS_6CH]);
		if (AC97_IS_FIXED_RATE(sc->codec_if)) {
			for (i = 0; i < AUICH_AUDIO_NFORMATS; i++) {
				sc->sc_audio_formats[i].frequency_type = 1;
				sc->sc_audio_formats[i].frequency[0] = 48000;
			}
		}
		mutex_exit(&sc->sc_lock);
		if (0 != auconv_create_encodings(sc->sc_audio_formats, AUICH_AUDIO_NFORMATS,
						 &sc->sc_encodings))
			return;
		if (0 != auconv_create_encodings(auich_spdif_formats, AUICH_SPDIF_NFORMATS,
						 &sc->sc_spdif_encodings))
			return;
	} else {
		mutex_exit(&sc->sc_lock);
		memcpy(sc->sc_modem_formats, auich_modem_formats, sizeof(auich_modem_formats));
		if (0 != auconv_create_encodings(sc->sc_modem_formats, AUICH_MODEM_NFORMATS,
						 &sc->sc_encodings))
			return;
	}

	/* Watch for power change */
	if (!pmf_device_register(self, NULL, auich_resume))
		aprint_error_dev(self, "couldn't establish power handler\n");

	config_interrupts(self, auich_finish_attach);

	/* sysctl setup */
	if (sc->sc_fixedrate && sc->sc_codectype == AC97_CODEC_TYPE_AUDIO)
		return;

	err = sysctl_createv(&sc->sc_log, 0, NULL, NULL, 0,
			     CTLTYPE_NODE, "hw", NULL, NULL, 0, NULL, 0,
			     CTL_HW, CTL_EOL);
	if (err != 0)
		goto sysctl_err;
	err = sysctl_createv(&sc->sc_log, 0, NULL, &node, 0,
			     CTLTYPE_NODE, device_xname(self), NULL, NULL, 0,
			     NULL, 0, CTL_HW, CTL_CREATE, CTL_EOL);
	if (err != 0)
		goto sysctl_err;
	node_mib = node->sysctl_num;

	if (!sc->sc_fixedrate) {
		/* passing the sc address instead of &sc->sc_ac97_clock */
		err = sysctl_createv(&sc->sc_log, 0, NULL, &node_ac97clock,
				     CTLFLAG_READWRITE,
				     CTLTYPE_INT, "ac97rate",
				     SYSCTL_DESCR("AC'97 codec link rate"),
				     auich_sysctl_verify, 0, sc, 0,
				     CTL_HW, node_mib, CTL_CREATE, CTL_EOL);
		if (err != 0)
			goto sysctl_err;
		sc->sc_ac97_clock_mib = node_ac97clock->sysctl_num;
	}

	return;

 sysctl_err:
	printf("%s: failed to add sysctl nodes. (%d)\n",
	       device_xname(self), err);
	return;			/* failure of sysctl is not fatal. */
}

static void
auich_childdet(device_t self, device_t child)
{
	struct auich_softc *sc = device_private(self);

	KASSERT(sc->sc_audiodev == child);
	sc->sc_audiodev = NULL;
}

static int
auich_detach(device_t self, int flags)
{
	struct auich_softc *sc = device_private(self);

	/* audio */
	if (sc->sc_audiodev != NULL)
		config_detach(sc->sc_audiodev, flags);

	/* sysctl */
	sysctl_teardown(&sc->sc_log);

	mutex_enter(&sc->sc_lock);

	/* audio_encoding_set */
	auconv_delete_encodings(sc->sc_encodings);
	auconv_delete_encodings(sc->sc_spdif_encodings);

	/* ac97 */
	if (sc->codec_if != NULL)
		sc->codec_if->vtbl->detach(sc->codec_if);

	mutex_exit(&sc->sc_lock);
	mutex_destroy(&sc->sc_lock);
	mutex_destroy(&sc->sc_intr_lock);

	/* PCI */
	if (sc->sc_ih != NULL)
		pci_intr_disestablish(sc->sc_pc, sc->sc_ih);
	if (sc->mix_size != 0)
		bus_space_unmap(sc->iot, sc->mix_ioh, sc->mix_size);
	if (sc->aud_size != 0)
		bus_space_unmap(sc->iot, sc->aud_ioh, sc->aud_size);
	return 0;
}

static int
auich_sysctl_verify(SYSCTLFN_ARGS)
{
	int error, tmp;
	struct sysctlnode node;
	struct auich_softc *sc;

	node = *rnode;
	sc = rnode->sysctl_data;
	if (node.sysctl_num == sc->sc_ac97_clock_mib) {
		tmp = sc->sc_ac97_clock;
		node.sysctl_data = &tmp;
		error = sysctl_lookup(SYSCTLFN_CALL(&node));
		if (error || newp == NULL)
			return error;

		if (tmp < 48000 || tmp > 96000)
			return EINVAL;
		mutex_enter(&sc->sc_lock);
		sc->sc_ac97_clock = tmp;
		mutex_exit(&sc->sc_lock);
	}

	return 0;
}

static void
auich_finish_attach(device_t self)
{
	struct auich_softc *sc = device_private(self);

	mutex_enter(&sc->sc_lock);
	if (!AC97_IS_FIXED_RATE(sc->codec_if))
		auich_calibrate(sc);
	mutex_exit(&sc->sc_lock);

	sc->sc_audiodev = audio_attach_mi(&auich_hw_if, sc, sc->sc_dev);

	return;
}

static int
auich_read_codec(void *v, uint8_t reg, uint16_t *val)
{
	struct auich_softc *sc;
	int i;
	uint32_t status;

	sc = v;
	/* wait for an access semaphore */
	for (i = ICH_SEMATIMO / ICH_CODECIO_INTERVAL; i-- &&
	    bus_space_read_1(sc->iot, sc->aud_ioh,
		ICH_CAS + sc->sc_modem_offset) & 1;
	    DELAY(ICH_CODECIO_INTERVAL));

	if (i > 0) {
		*val = bus_space_read_2(sc->iot, sc->mix_ioh,
		    reg + (sc->sc_codecnum * ICH_CODEC_OFFSET));
		DPRINTF(ICH_DEBUG_CODECIO,
		    ("auich_read_codec(%x, %x)\n", reg, *val));
		status = bus_space_read_4(sc->iot, sc->aud_ioh,
		    ICH_GSTS + sc->sc_modem_offset);
		if (status & ICH_RCS) {
			bus_space_write_4(sc->iot, sc->aud_ioh,
					  ICH_GSTS + sc->sc_modem_offset,
					  status & ~(ICH_SRI|ICH_PRI|ICH_GSCI));
			*val = 0xffff;
			DPRINTF(ICH_DEBUG_CODECIO,
			    ("%s: read_codec error\n", device_xname(sc->sc_dev)));
			if (reg == AC97_REG_GPIO_STATUS)
				auich_clear_cas(sc);
			return -1;
		}
		if (reg == AC97_REG_GPIO_STATUS)
			auich_clear_cas(sc);
		return 0;
	} else {
		aprint_normal_dev(sc->sc_dev, "read_codec timeout\n");
		if (reg == AC97_REG_GPIO_STATUS)
			auich_clear_cas(sc);
		return -1;
	}
}

static int
auich_attach_codec(void *v, struct ac97_codec_if *cif)
{
	struct auich_softc *sc;

	sc = v;
	sc->codec_if = cif;

	return 0;
}

static int
auich_reset_codec(void *v)
{
	struct auich_softc *sc;
	int i;
	uint32_t control, status;

	sc = v;
	control = bus_space_read_4(sc->iot, sc->aud_ioh,
	    ICH_GCTRL + sc->sc_modem_offset);
	if (sc->sc_codectype == AC97_CODEC_TYPE_AUDIO) {
		control &= ~(ICH_ACLSO | sc->sc_pcm246_mask);
	} else {
		control &= ~ICH_ACLSO;
		control |= ICH_GIE;
	}
	control |= (control & ICH_CRESET) ? ICH_WRESET : ICH_CRESET;
	bus_space_write_4(sc->iot, sc->aud_ioh,
	    ICH_GCTRL + sc->sc_modem_offset, control);

	for (i = 500000; i >= 0; i--) {
		status = bus_space_read_4(sc->iot, sc->aud_ioh,
		    ICH_GSTS + sc->sc_modem_offset);
		if (status & (ICH_PCR | ICH_SCR | ICH_S2CR))
			break;
		DELAY(1);
	}
	if (i <= 0) {
		aprint_error_dev(sc->sc_dev, "auich_reset_codec: time out\n");
		return ETIMEDOUT;
	}
#ifdef AUICH_DEBUG
	if (status & ICH_SCR)
		printf("%s: The 2nd codec is ready.\n",
		       device_xname(sc->sc_dev));
	if (status & ICH_S2CR)
		printf("%s: The 3rd codec is ready.\n",
		       device_xname(sc->sc_dev));
#endif
	return 0;
}

static enum ac97_host_flags
auich_flags_codec(void *v)
{
	struct auich_softc *sc = v;
	return sc->sc_codecflags;
}

static void
auich_spdif_event(void *addr, bool flag)
{
	struct auich_softc *sc;

	sc = addr;
	sc->sc_spdif = flag;
}

static int
auich_trigger_input(void *v, void *start, void *end, int blksize,
    void (*intr)(void *), void *arg, const audio_params_t *param)
{
	struct auich_softc *sc;
	struct auich_dma *p;
	size_t size;

	DPRINTF(ICH_DEBUG_DMA,
	    ("auich_trigger_input(%p, %p, %d, %p, %p, %p)\n",
	    start, end, blksize, intr, arg, param));
	sc = v;

	for (p = sc->sc_dmas; p && KERNADDR(p) != start; p = p->next)
		continue;
	if (!p) {
		printf("auich_trigger_input: bad addr %p\n", start);
		return EINVAL;
	}

	size = (size_t)((char *)end - (char *)start);

	sc->pcmi.intr = intr;
	sc->pcmi.arg = arg;
	sc->pcmi.start = DMAADDR(p);
	sc->pcmi.p = sc->pcmi.start;
	sc->pcmi.end = sc->pcmi.start + size;
	sc->pcmi.blksize = blksize;

	bus_space_write_4(sc->iot, sc->aud_ioh, ICH_PCMI + ICH_BDBAR,
	    sc->sc_cddma + ICH_PCMI_OFF(0));
	auichTriggerPipe(sc, ICH_PCMI, &sc->pcmi);

	return 0;
}

static int
auich_alloc_cdata(struct auich_softc *sc)
{
	bus_dma_segment_t seg;
	int error, rseg;

	/*
	 * Allocate the control data structure, and create and load the
	 * DMA map for it.
	 */
	if ((error = bus_dmamem_alloc(sc->dmat,
				      sizeof(struct auich_cdata),
				      PAGE_SIZE, 0, &seg, 1, &rseg, 0)) != 0) {
		aprint_error_dev(sc->sc_dev, "unable to allocate control data, error = %d\n", error);
		goto fail_0;
	}

	if ((error = bus_dmamem_map(sc->dmat, &seg, rseg,
				    sizeof(struct auich_cdata),
				    (void **) &sc->sc_cdata,
				    sc->sc_dmamap_flags)) != 0) {
		aprint_error_dev(sc->sc_dev, "unable to map control data, error = %d\n", error);
		goto fail_1;
	}

	if ((error = bus_dmamap_create(sc->dmat, sizeof(struct auich_cdata), 1,
				       sizeof(struct auich_cdata), 0, 0,
				       &sc->sc_cddmamap)) != 0) {
		aprint_error_dev(sc->sc_dev, "unable to create control data DMA map, "
		    "error = %d\n", error);
		goto fail_2;
	}

	if ((error = bus_dmamap_load(sc->dmat, sc->sc_cddmamap,
				     sc->sc_cdata, sizeof(struct auich_cdata),
				     NULL, 0)) != 0) {
		aprint_error_dev(sc->sc_dev, "unable tp load control data DMA map, "
		    "error = %d\n", error);
		goto fail_3;
	}

	sc->pcmo.dmalist = sc->sc_cdata->ic_dmalist_pcmo;
	sc->pcmi.dmalist = sc->sc_cdata->ic_dmalist_pcmi;
	sc->mici.dmalist = sc->sc_cdata->ic_dmalist_mici;

	return 0;

 fail_3:
	bus_dmamap_destroy(sc->dmat, sc->sc_cddmamap);
 fail_2:
	bus_dmamem_unmap(sc->dmat, (void *) sc->sc_cdata,
	    sizeof(struct auich_cdata));
 fail_1:
	bus_dmamem_free(sc->dmat, &seg, rseg);
 fail_0:
	return error;
}

static bool
auich_resume(device_t dv, const pmf_qual_t *qual)
{
	struct auich_softc *sc = device_private(dv);
	pcireg_t v;

	mutex_enter(&sc->sc_lock);
	mutex_spin_enter(&sc->sc_intr_lock);

	if (sc->sc_iose) {
		v = pci_conf_read(sc->sc_pc, sc->sc_pt, ICH_CFG);
		pci_conf_write(sc->sc_pc, sc->sc_pt, ICH_CFG,
			       v | ICH_CFG_IOSE);
	}

	auich_reset_codec(sc);
	mutex_spin_exit(&sc->sc_intr_lock);
	DELAY(1000);
	(sc->codec_if->vtbl->restore_ports)(sc->codec_if);
	mutex_exit(&sc->sc_lock);

	return true;
}

/*
 * Calibrate card (some boards are overclocked and need scaling)
 */
static void
auich_calibrate(struct auich_softc *sc)
{
	struct timeval t1, t2;
	uint8_t ociv, nciv;
	uint64_t wait_us;
	uint32_t actual_48k_rate, bytes, ac97rate;
	void *temp_buffer;
	struct auich_dma *p;
	u_int rate;

	/*
	 * Grab audio from input for fixed interval and compare how
	 * much we actually get with what we expect.  Interval needs
	 * to be sufficiently short that no interrupts are
	 * generated.
	 */

	/* Force the codec to a known state first. */
	sc->codec_if->vtbl->set_clock(sc->codec_if, 48000);
	rate = sc->sc_ac97_clock = 48000;
	sc->codec_if->vtbl->set_rate(sc->codec_if, AC97_REG_PCM_LR_ADC_RATE,
	    &rate);

	/* Setup a buffer */
	bytes = 64000;
	temp_buffer = auichAllocm(sc, AUMODE_RECORD, bytes);

	for (p = sc->sc_dmas; p && KERNADDR(p) != temp_buffer; p = p->next)
		continue;
	if (p == NULL) {
		printf("auich_calibrate: bad address %p\n", temp_buffer);
		return;
	}
	sc->pcmi.dmalist[0].base = DMAADDR(p);
	sc->pcmi.dmalist[0].len = (bytes >> sc->sc_sample_shift);

	/*
	 * our data format is stereo, 16 bit so each sample is 4 bytes.
	 * assuming we get 48000 samples per second, we get 192000 bytes/sec.
	 * we're going to start recording with interrupts disabled and measure
	 * the time taken for one block to complete.  we know the block size,
	 * we know the time in microseconds, we calculate the sample rate:
	 *
	 * actual_rate [bps] = bytes / (time [s] * 4)
	 * actual_rate [bps] = (bytes * 1000000) / (time [us] * 4)
	 * actual_rate [Hz] = (bytes * 250000) / time [us]
	 */

	/* prepare */
	ociv = bus_space_read_1(sc->iot, sc->aud_ioh, ICH_PCMI + ICH_CIV);
	bus_space_write_4(sc->iot, sc->aud_ioh, ICH_PCMI + ICH_BDBAR,
			  sc->sc_cddma + ICH_PCMI_OFF(0));
	bus_space_write_1(sc->iot, sc->aud_ioh, ICH_PCMI + ICH_LVI,
			  (0 - 1) & ICH_LVI_MASK);

	/* start */
	kpreempt_disable();
	microtime(&t1);
	bus_space_write_1(sc->iot, sc->aud_ioh, ICH_PCMI + ICH_CTRL, ICH_RPBM);

	/* wait */
	nciv = ociv;
	do {
		microtime(&t2);
		if (t2.tv_sec - t1.tv_sec > 1)
			break;
		nciv = bus_space_read_1(sc->iot, sc->aud_ioh,
					ICH_PCMI + ICH_CIV);
	} while (nciv == ociv);
	microtime(&t2);

	/* stop */
	bus_space_write_1(sc->iot, sc->aud_ioh, ICH_PCMI + ICH_CTRL, 0);
	kpreempt_enable();

	/* reset */
	DELAY(100);
	bus_space_write_1(sc->iot, sc->aud_ioh, ICH_PCMI + ICH_CTRL, ICH_RR);

	/* turn time delta into us */
	wait_us = ((t2.tv_sec - t1.tv_sec) * 1000000) + t2.tv_usec - t1.tv_usec;

	auichFreem(sc, temp_buffer, bytes);

	if (nciv == ociv) {
		printf("%s: ac97 link rate calibration timed out after %"
		       PRIu64 " us\n", device_xname(sc->sc_dev), wait_us);
		return;
	}

	actual_48k_rate = (bytes * UINT64_C(250000)) / wait_us;

	if (actual_48k_rate < 50000)
		ac97rate = 48000;
	else
		ac97rate = ((actual_48k_rate + 500) / 1000) * 1000;

	aprint_verbose_dev(sc->sc_dev, "measured ac97 link rate at %d Hz",
	       actual_48k_rate);
	if (ac97rate != actual_48k_rate)
		aprint_verbose(", will use %d Hz", ac97rate);
	aprint_verbose("\n");

	sc->sc_ac97_clock = ac97rate;
}

static void
auich_clear_cas(struct auich_softc *sc)
{
	/* Clear the codec access semaphore */
	(void)bus_space_read_2(sc->iot, sc->mix_ioh,
	    AC97_REG_RESET * (sc->sc_codecnum * ICH_CODEC_OFFSET));

	return;
}
