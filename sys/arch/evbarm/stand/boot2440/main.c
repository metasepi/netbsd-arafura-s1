/*-
 * Copyright (c) 2012 The NetBSD Foundation, Inc.
 * All rights reserved.
 *
 * This code is derived from software contributed to The NetBSD Foundation
 * by Paul Fleischer <paul@xpg.dk>
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
#include <sys/types.h>

#include <arm/armreg.h>
#include <arm/s3c2xx0/s3c2440reg.h>

#include <netinet/in.h>
#include <netinet/in_systm.h>

#include <lib/libkern/libkern.h>
#include <lib/libsa/stand.h>
#include <lib/libsa/loadfile.h>
#include <lib/libsa/iodesc.h>

#include <arch/evbarm/mini2440/mini2440_bootinfo.h>

#define CSR_READ(reg) \
	*(volatile uint32_t *)(reg)
#define CSR_WRITE(reg, val) do { \
	    *(volatile uint32_t *)((reg)) = val; \
	} while (0)

#define UART_BAUDRATE		115200
#define S3C2XX0_XTAL_CLK	12000000
#define BOOTINFO_ADDR		0x31500000

/* Macros to turn on/off LEDs. Numbering is 1-4. */
#define LED_REG (volatile uint16_t*)(S3C2440_GPIO_BASE+GPIO_PBDAT)
#define CLEAR_LEDS() *LED_REG = *LED_REG | 0x1e0
#define LED_ON(led) *LED_REG = *LED_REG & ( ~(1<<(led+4)) & 0x1E0 )
#define LED_OFF(led) *LED_REG = *LED_REG | ( ~(1<<(led+4)) & 0x1E0 )

/* Local variables */
static time_t	wallclock = 0;
static uint32_t timer_inc_rate;
void *bootinfo;
int bi_size;
char *bi_next;

#define STR_EXPAND(tok) #tok
#define STR(tok) STR_EXPAND(tok)

#if defined(DEFAULT_BOOTFILE)
static char *default_boot=STR(DEFAULT_BOOTFILE);
#else
static char *default_boot="net:";
#endif

time_t getsecs();
time_t getusecs();

/* Local functions */
static void s3c24x0_clock_freq2(vaddr_t clkman_base, int *fclk, int *hclk,
				int *pclk);
static void uart_init(uint32_t pclk);
static void time_init(uint32_t pclk);
static void bi_init(void *addr);
static void bi_add(void *new, int type, int size);
static void parse_mac_address(const char *str, uint8_t *enaddr);

extern void* dm9k_init(unsigned int tag, void *macaddr);

/* External variables */
extern char bootprog_name[], bootprog_rev[];

/* External functions */
extern void netif_match(unsigned int tag, uint8_t *macaddr);
/*  extern int sdif_init(unsigned int tag);*/

/* Global variables */
int pclk;
struct btinfo_rootdevice	bi_rdev;

/* This is not very flexible, as only one net device is allowed */
struct btinfo_net		bi_net;

struct btinfo_bootpath		bi_path;

void
main(int argc, char *argv[])
{
	int fclk, hclk;
	int fd;
	unsigned long marks[MARK_MAX];
	unsigned char hdr[0x26];
	void (*entry)(void*);
	unsigned elfpriv;
	char *bootfile;
	char *bf;
	bool kernel_loaded;
	uint8_t enaddr[6] = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

	/* Give some indication that main() has been reached */
	CLEAR_LEDS();
	LED_ON(4);

	/* Next, we setup the clock of the S3C2440 such that we are not
	   dependent on any other bootloader in this regard.
	   Target FCLK is 405MHz, and we assume an input crystal of 12MHz
	*/
	*(volatile uint32_t*)(S3C2440_CLKMAN_BASE+CLKMAN_MPLLCON) =
		((0x7F << PLLCON_MDIV_SHIFT) & PLLCON_MDIV_MASK) |
		((2 << PLLCON_PDIV_SHIFT) & PLLCON_PDIV_MASK) |
		((1 << PLLCON_SDIV_SHIFT) & PLLCON_SDIV_MASK);
	*(volatile uint32_t*)(S3C2440_CLKMAN_BASE+CLKMAN_UPLLCON) =
		((0x38 << PLLCON_MDIV_SHIFT) & PLLCON_MDIV_MASK) |
		((2 << PLLCON_PDIV_SHIFT) & PLLCON_PDIV_MASK) |
		((2 << PLLCON_SDIV_SHIFT) & PLLCON_SDIV_MASK);

	LED_ON(1);

	s3c24x0_clock_freq2(S3C2440_CLKMAN_BASE, &fclk, &hclk, &pclk);

	uart_init(pclk);
	time_init(pclk);

	/* Let the user know we are alive */
	printf("\n");
	printf(">> %s boot2440, revision %s\n", bootprog_name, bootprog_rev);

	bootinfo = (void*) BOOTINFO_ADDR;
	bi_init(bootinfo);

	bi_net.devname[0] = 0;
	bi_path.bootpath[0] = 0;

	/* Try to get boot arguments from any previous boot-loader */
	{
		struct btinfo_bootstring ba;
		int j, i;

		printf("Argument count: %d\n", argc);

		j = 0;
		for (i = 0; i < argc; i++) {
			if (j == MAX_BOOT_STRING-1) {
				ba.bootstring[j] = '\0';
				continue;
			}
			if (strncmp(argv[i], "mac=", 4) == 0) {
				parse_mac_address(argv[i]+4, enaddr);
			} else {
				if (j != 0)
					ba.bootstring[j++] = ' ';

				strncpy(ba.bootstring+j, argv[i], MAX_BOOT_STRING-j);
				j += strlen(argv[i]);
			}
		}

		printf("Boot string: %s\n", ba.bootstring);

		bi_add(&ba, BTINFO_BOOTSTRING, sizeof(ba));
	}

	LED_ON(3);

	if (argc > 1) {
		bf = argv[argc-1];
	} else {
		bf = default_boot;
	}

	/* Detect networking devices */
	netif_match(0, enaddr);

	kernel_loaded = FALSE;
	do {
		bootfile = strsep(&bf, ";");
		printf("Trying \"%s\"...\n", bootfile);
		fd = open(bootfile, 0);
		if (fd < 0) {
			printf("Failed: %d\n", errno);
			close(fd);
			continue;
		}

		if (fdloadfile(fd, marks, LOAD_ALL) == 0) {
			kernel_loaded = TRUE;
			break;
		}
	} while(bf != NULL);

	if (!kernel_loaded) {
		panic("Failed to load kernel\n");
		_rtt();
	}

#if 1
	/* Set MAC address of the 'dme' net device, if
	 * it isn't set already */
	if (bi_net.devname[0] == 0) {
		uint8_t en[6] = {DM9000MAC};
		snprintf(bi_net.devname, sizeof(bi_net.devname), "dme");
		bi_net.cookie = 0;

		memcpy(bi_net.mac_address, en, sizeof(bi_net.mac_address));
	}
#endif
	/*
	 * ARM ELF header has a distinctive value in "private flags"
	 * field of offset [0x24:25];
	 * - NetBSD 02 06
	 * - Linux  02 00 (2.4) or 02 02 (2.6)
	 */
	lseek(fd, (off_t)0, SEEK_SET);
	read(fd, &hdr, sizeof(hdr));
	elfpriv = *(unsigned short *)&hdr[0x24];

	entry = (void *)marks[MARK_ENTRY];
	if (elfpriv == 0x0602) {
		struct btinfo_symtab bi_syms;

		bi_syms.nsym = marks[MARK_NSYM];
		bi_syms.ssym = (void*)marks[MARK_SYM];
		bi_syms.esym = (void*)marks[MARK_END];
		bi_add(&bi_syms, BTINFO_SYMTAB, sizeof(bi_syms));
		if (bi_path.bootpath[0] != 0)
		  bi_add(&bi_path, BTINFO_BOOTPATH, sizeof(bi_path));
		bi_add(&bi_rdev, BTINFO_ROOTDEVICE, sizeof(bi_rdev));
		if (bi_net.devname[0] != 0 )
			bi_add(&bi_net, BTINFO_NET, sizeof(bi_net));
	} else {
		printf("Loaded object is not NetBSD ARM ELF");
		_rtt();
	}

	printf("entry=%p, nsym=%lu, ssym=%p, esym=%p\n",
	       (void *)marks[MARK_ENTRY],
	       marks[MARK_NSYM],
	       (void *)marks[MARK_SYM],
	       (void *)marks[MARK_END]);
	(*entry)(bootinfo);

	printf("exec returned, restarting...\n");
	_rtt();
}

void
uart_init(uint32_t pclk)
{
	/* Setup UART0 clocking: Use PCLK */
	*(volatile uint32_t*)(S3C2440_UART_BASE(0)+SSCOM_UBRDIV) =
		(pclk/(UART_BAUDRATE*16)) - 1;

	*(volatile uint32_t*)(S3C2440_UART_BASE(0)+SSCOM_UCON) =
		UCON_TXMODE_INT | UCON_RXMODE_INT;

	*(volatile uint32_t*)(S3C2440_UART_BASE(0)+SSCOM_ULCON) =
		ULCON_PARITY_NONE | ULCON_LENGTH_8;

	*(volatile uint32_t*)(S3C2440_UART_BASE(0)+SSCOM_UFCON) =
		UFCON_TXTRIGGER_0 | UFCON_TXFIFO_RESET | UFCON_FIFO_ENABLE;
}

static uint32_t countdown_duration;

static
void time_init(uint32_t pclk)
{
	/* Configure timer0 to be as slow as possible:
	   Prescaler = 255
	   Divider = 16
	 */

	/* First, configure the prescaler */
	*(volatile uint32_t*)(S3C2440_TIMER_BASE+TIMER_TCFG0) = 0xff;

	/* Next, the divider */
	*(volatile uint32_t*)(S3C2440_TIMER_BASE+TIMER_TCFG1) |=
		(TCFG1_MUX_DIV16 <<TCFG1_MUX_SHIFT(0)) & TCFG1_MUX_MASK(0);

		*(volatile uint32_t*)(S3C2440_TIMER_BASE+TIMER_TCON) =
			TCON_MANUALUPDATE(0);
		*(volatile uint32_t*)(S3C2440_TIMER_BASE+TIMER_TCNTB(0)) =
			0xffff;
		*(volatile uint32_t*)(S3C2440_TIMER_BASE+TIMER_TCON) =
			TCON_START(0);


	/* Timer count down duration */
	countdown_duration = 65535/(pclk/256/16);
	timer_inc_rate = pclk/256/16;
	//	printf("Countdown duration is: %ds\n", countdown_duration);
#if 0
	{
		/* Timer test */
		time_t time, old_time;

		while(1) {
			time = old_time = getsecs();
			do {
				time = getsecs();
			} while(time == old_time);
			printf("Count %u\n", (int)time);
		}
	}
#endif
}

time_t
getsecs()
{
	time_t secs = getusecs()/1000000;
	return secs;
}

time_t
getusecs() {
	uint32_t count;
	//do {
		count = *(volatile uint32_t*)(S3C2440_TIMER_BASE+TIMER_TCNTO(0));
//} while( count > 65500);

	*(volatile uint32_t*)(S3C2440_TIMER_BASE+TIMER_TCON) =
		TCON_MANUALUPDATE(0);
	*(volatile uint32_t*)(S3C2440_TIMER_BASE+TIMER_TCNTB(0)) =
		0xffff;
	*(volatile uint32_t*)(S3C2440_TIMER_BASE+TIMER_TCON) =
		TCON_START(0);

	wallclock += ((65535-count)*1000000) / timer_inc_rate;

	return wallclock;
}

void
usleep(int us) {
	uint32_t count;
	uint32_t target_clock = wallclock+us;

	while( wallclock < target_clock) {
		do {
			count = *(volatile uint32_t*)(S3C2440_TIMER_BASE+TIMER_TCNTO(0));
		} while( count > 65500);

		*(volatile uint32_t*)(S3C2440_TIMER_BASE+TIMER_TCON) =
			TCON_MANUALUPDATE(0);
		*(volatile uint32_t*)(S3C2440_TIMER_BASE+TIMER_TCNTB(0)) =
			0xffff;
		*(volatile uint32_t*)(S3C2440_TIMER_BASE+TIMER_TCON) =
			TCON_START(0);

		wallclock += ((65535-count)*1000000) / timer_inc_rate;
	}
}


void
mini2440_panic()
{
	int i, l;
	int v;
	while(1) {
		CLEAR_LEDS();
		for(l=0; l<0xffffff; l++) {
			v = *((int*)(S3C2440_TIMER_BASE+TIMER_TCNTO(0)));
		}
		for(i=1; i<=4; i++) {
			LED_ON(i);
		}
		for(l=0; l<0xffffff; l++) {
			v = *((int*)(S3C2440_TIMER_BASE+TIMER_TCNTO(0)));
		}
	}
}

void
s3c24x0_clock_freq2(vaddr_t clkman_base, int *fclk, int *hclk, int *pclk)
{
	uint32_t pllcon, divn, camdivn;
	int mdiv, pdiv, sdiv;
	uint32_t f, h, p;

	pllcon = *(volatile uint32_t *)(clkman_base + CLKMAN_MPLLCON);
	divn = *(volatile uint32_t *)(clkman_base + CLKMAN_CLKDIVN);
	camdivn = *(volatile uint32_t *)(clkman_base + CLKMAN_CAMDIVN);

	mdiv = (pllcon & PLLCON_MDIV_MASK) >> PLLCON_MDIV_SHIFT;
	pdiv = (pllcon & PLLCON_PDIV_MASK) >> PLLCON_PDIV_SHIFT;
	sdiv = (pllcon & PLLCON_SDIV_MASK) >> PLLCON_SDIV_SHIFT;

	f = ((mdiv + 8) * S3C2XX0_XTAL_CLK) / ((pdiv + 2) * (1 << sdiv)) * 2;
	h = f;

	/* HDIVN of CLKDIVN can have 4 distinct values */
	switch( (divn & CLKDIVN_HDIVN_MASK) >> CLKDIVN_HDIVN_SHIFT )
		{
		case 0:
			/* 00b: HCLK = FCLK/1*/
			break;
		case 1:
			/* 01b: HCLK = FCLK/2*/
			h /= 2;
			break;
		case 2:
			/* 10b: HCLK = FCLK/4 when CAMDIVN[9] (HCLK4_HALF) = 0
			 *      HCLK = FCLK/8 when CAMDIVN[9] (HCLK4_HALF) = 1 */
			if( camdivn & CLKCAMDIVN_HCLK4_HALF )
				h /= 8;
			else
				h /= 4;
			break;
		case 3:
			/* 11b: HCLK = FCLK/3 when CAMDIVN[8] (HCLK3_HALF) = 0
			 *      HCLK = FCLK/6 when CAMDIVN[8] (HCLK3_HALF) = 1 */
			if( camdivn & CLKCAMDIVN_HCLK3_HALF )
				h /= 6;
			else
				h /= 3;
			break;
		}

	p = h;

	if (divn & CLKDIVN_PDIVN)
		p /= 2;

	if (fclk) *fclk = f;
	if (hclk) *hclk = h;
	if (pclk) *pclk = p;
}

void
putchar(int c)
{
	uint32_t stat;

	if (c == '\n')
		putchar('\r');
	
	do {
		stat = CSR_READ(S3C2440_UART_BASE(0) + SSCOM_UTRSTAT);
	} while ((stat & UTRSTAT_TXEMPTY) == 0);

	CSR_WRITE(S3C2440_UART_BASE(0) + SSCOM_UTXH, c);
}

void
_rtt()
{
	int cpsr_save, tmp;
	/* Disable interrupts */
	__asm volatile("mrs %0, cpsr;"
		       "orr %1, %0, %2;"
		       "msr cpsr_c, %1;"
		       : "=r" (cpsr_save), "=r" (tmp)
		       : "I" (I32_bit)
		       );

	/* Disable MMU */
	__asm volatile("mrc p15, 0, %0, c1, c0, 0;"
		       "bic %0, %0, %1;"
		       "mcr p15, 0, %0, c1, c0, 0;"
		       : "=r" (tmp)
		       : "I" (CPU_CONTROL_MMU_ENABLE)
		       );

	/* Configure watchdog to fire now */
	*(volatile uint32_t *)(S3C2440_WDT_BASE + WDT_WTCON) =
		(0 << WTCON_PRESCALE_SHIFT) | WTCON_ENABLE |
		WTCON_CLKSEL_16 | WTCON_ENRST;
}

void
bi_init(void *addr)
{
	struct btinfo_magic bi_magic;

	memset(addr, 0, BOOTINFO_MAXSIZE);
	bi_next = (char*) addr;
	bi_size = 0;

	bi_magic.magic = BOOTINFO_MAGIC;
	bi_add(&bi_magic, BTINFO_MAGIC, sizeof(bi_magic));
}


void
bi_add(void *new, int type, int size)
{
	struct btinfo_common *bi;

	if (bi_size + size > BOOTINFO_MAXSIZE)
		return;

	bi = new;
	bi->next = size;
	bi->type = type;
	memcpy(bi_next, new, size);
	bi_next += size;
}

static void
parse_mac_address(const char *str, uint8_t *enaddr)
{
	int i;
	char *next = (char*)str;

	for(i=0;i<6;i++) {
		str = next;
		enaddr[i] = (unsigned char)strtoll(str, &next, 16);
		if( *next == ':' ) {
			next++;
		} else {
			break;
		}
	}
}
