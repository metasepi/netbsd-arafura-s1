/*	$NetBSD: partman.c,v 0.000 0000/00/00 00:00:00 uzixls Exp $ */

/*
 * Copyright 2012 Eugene Lozovoy
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
 * 3. The name of Eugene Lozovoy may not be used to endorse
 *    or promote products derived from this software without specific prior
 *    written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY PIERMONT INFORMATION SYSTEMS INC. ``AS IS''
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL PIERMONT INFORMATION SYSTEMS INC. BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

/* partman.c - extended partitioning */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <util.h>

#include "defs.h"
#include "md.h"
#include "msg_defs.h"
#include "menu_defs.h"


typedef struct {
	int retvalue;
	int dev_num;
	pm_devs_t *dev_pm;
	char fullname[SSTRSIZE];
	enum {PM_DISK_T, PM_PART_T, PM_RAID_T, PM_VND_T, PM_CGD_T,
		PM_LVM_T, PM_LVMLV_T} type;
} part_entry_t;

#define MNTS_MAX 96
struct {
    const char *diskdev, *mnt_opts, *fsname;
    char *pi_mount;
    int pm_part;
} mnts[MNTS_MAX];

#define MAX_RAID 16 // TODO: replace by true way
#define MAX_IN_RAID 48
struct raids_t {
	int enabled;
	char pm_name[MAX_IN_RAID][SSTRSIZE];
	int pm_part[MAX_IN_RAID];
	int pm_is_spare[MAX_IN_RAID];
	int numRow, numCol, numSpare;
	int sectPerSU, SUsPerParityUnit, SUsPerReconUnit, raid_level;
	pm_devs_t *pm[MAX_IN_RAID];
} raids[MAX_RAID];

#define MAX_VND 4
struct vnds_t {
	int enabled;
	char filepath[STRSIZE];
	int size;
	int readonly;
	int is_exist;
	int manual_geom;
	int secsize, nsectors, ntracks, ncylinders;
} vnds[MAX_VND];

#define MAX_CGD 4
struct cgds_t {
	int enabled;
	pm_devs_t *pm;
	char pm_name[SSTRSIZE];
	int pm_part;
	const char *keygen_type;
	const char *verify_type;
	const char *enc_type;
	const char *iv_type;
	int key_size;
} cgds[MAX_CGD];

#define MAX_LVM_VG 16
#define MAX_LVM_PV 255
#define MAX_LVM_LV 255
struct lvms_t {
	int enabled;
	char name[SSTRSIZE];
	int maxlogicalvolumes;
	int maxphysicalvolumes;
	int physicalextentsize;
	struct pv_t {
		pm_devs_t *pm;
		char pm_name[SSTRSIZE];
		int pm_part;
		int metadatasize;
		int metadatacopies;
		int labelsector;
		int setphysicalvolumesize;
	} pv[MAX_LVM_PV];
	struct lv_t {
		uint size;
		char name[SSTRSIZE];
		int readonly;
		int contiguous;
		int chunksize;
		char extents[SSTRSIZE];
		int minor;
		int mirrors;
		int regionsize;
		int persistent;
		int readahead;
		int stripes;
		int stripesize;
		int zero;
	} lv[MAX_LVM_LV];
} lvms[MAX_LVM_VG];

typedef struct partman_upddevlist_adv_t {
	const char *create_msg;
	int (*action)(menudesc *, void *);
	int pe_type;
	int max;
	int entry_size;
	void *entry_enabled;
	struct partman_upddevlist_adv_t *sub;
} partman_upddevlist_adv_t;

int manage_newdev;
int manage_curdev;
int manage_raid_spare_cur;

enum { /* Raid menu enum */
	PMR_MENU_DEVS, PMR_MENU_DEVSSPARE, PMR_MENU_RAIDLEVEL, PMR_MENU_NUMROW,
	PMR_MENU_NUMCOL, PMR_MENU_NUMSPARE,	PMR_MENU_SECTPERSU,	PMR_MENU_SUSPERPARITYUNIT,
	PMR_MENU_SUSPERRECONUNIT, PMR_MENU_REMOVE, PMR_MENU_END
};

enum { /* VND menu enum */
	PMV_MENU_FILEPATH, PMV_MENU_EXIST, PMV_MENU_SIZE, PMV_MENU_RO, PMV_MENU_MGEOM,
	PMV_MENU_SECSIZE, PMV_MENU_NSECTORS, PMV_MENU_NTRACKS, PMV_MENU_NCYLINDERS,
	PMV_MENU_REMOVE, PMV_MENU_END
};

enum { /* CGD menu enum */
	PMC_MENU_DEV, PMC_MENU_ENCTYPE, PMC_MENU_KEYSIZE, PMC_MENU_IVTYPE,
	PMC_MENU_KEYGENTYPE, PMC_MENU_VERIFYTYPE, PMC_MENU_REMOVE, PMC_MENU_END
};

enum { /* LVM menu enum */
	PML_MENU_PV, PML_MENU_NAME, PML_MENU_MAXLOGICALVOLUMES,
	PML_MENU_MAXPHYSICALVOLUMES, PML_MENU_PHYSICALEXTENTSIZE, 
	PML_MENU_REMOVE, PML_MENU_END
};

enum { /* LVM submenu (logical volumes) enum */
	PMLV_MENU_SIZE, PMLV_MENU_NAME, PMLV_MENU_READONLY, PMLV_MENU_CONTIGUOUS,
	PMLV_MENU_CHUNKSIZE, PMLV_MENU_EXTENTS, PMLV_MENU_MINOR, PMLV_MENU_MIRRORS,
	PMLV_MENU_REGIONSIZE, PMLV_MENU_PERSISTENT, PMLV_MENU_READAHEAD,
	PMLV_MENU_STRIPES, PMLV_MENU_STRIPESIZE, PMLV_MENU_ZERO, 
	PMLV_MENU_REMOVE, PMLV_MENU_END
};


static int partman_manage_edit(int, void (*)(menudesc *, int, void *),
	int (*)(menudesc *, void *), int (*)(int), void (*)(void *),
	void *, int, int, int , void *);
part_entry_t partman_manage_getdev(int);
static int partman_raid_disk_add(menudesc *, void *);
static int partman_raid_disk_del(menudesc *, void *);
static int partman_cgd_disk_set(part_entry_t *);
static void partman_select(pm_devs_t *);
static int partman_mountall_sort(const void *, const void *);
static int partman_mountall(void);
static int partman_upddevlist(menudesc *, void *);
static int partman_commit(menudesc *, void *);
static int partman_submenu(menudesc *, void *);
static void partman_menufmt(menudesc *, int, void *);


/* Menu for RAID/VND/CGD/LVM entry edit */
static int
partman_manage_edit(int menu_entries_count, void (*menu_fmt)(menudesc *, int, void *),
	int (*action)(menudesc *, void *), int (*check_fun)(int),
	void (*entry_init)(void *),	void* entry_init_arg,
	int dev_num, int max, int entry_size, void *entry_enabled)
{
	int i, retvalue = 0, ok = 0;

	if (dev_num < 0) {
		/* We should create new device */
		for (i = 0; i < max && !ok; i++)
			if (*(int*)((char*)entry_enabled + entry_size * i) == 0) {
				manage_curdev = i;
				entry_init(entry_init_arg);
				*(int*)((char*)entry_enabled + entry_size * i) = 1;
				ok = 1;
			}
		if (!ok) {
			msg_prompt_win("Limit for the devices count was reached!", -1, 18, 0, 0, NULL, NULL, 0);
			return -1;
		}
	} else {
		/* ... or edit existent */
		if (*(int*)((char*)entry_enabled + entry_size * dev_num) == 0) {
			if (logfp)
				fprintf(logfp, "partman_manage_edit: invalid drive\n");
				msg_prompt_win("Invalid device!", -1, 18, 0, 0, NULL, NULL, 0);
			return -2;
		}
		manage_curdev = dev_num;
	}

	menu_ent menu_entries[menu_entries_count];
	for (i = 0; i < menu_entries_count - 1; i++)
		menu_entries[i] = (menu_ent) {NULL, OPT_NOMENU, 0, action};
	menu_entries[i] = (menu_ent) {"REMOVE", OPT_NOMENU, OPT_EXIT, action};

	int menu_no = -1;
	menu_no = new_menu(NULL, menu_entries, menu_entries_count,
		-1, -1, 0, 40, MC_NOCLEAR | MC_SCROLL,
		NULL, menu_fmt, NULL, NULL, MSG_DONE);
	
	process_menu(menu_no, &retvalue);
	free_menu(menu_no);
	if (check_fun != NULL)
		check_fun(manage_curdev);

	return retvalue;
}

/* Show filtered partitions menu */
part_entry_t
partman_manage_getdev(int type)
{
	pm_devs_t *pm_i;
	int dev_num = -1, num_devs;
	int i, ok;
	int menu_no;
	menu_ent menu_entries[MAX_DISKS*MAXPARTITIONS];
	part_entry_t disk_entries[MAX_DISKS*MAXPARTITIONS];

	for (pm_i = pm_head->next, num_devs = 0; pm_i != NULL; pm_i = pm_i->next)
		for (i = 0; i < MAXPARTITIONS; i++) {
			ok = 0;
			switch (type) {
				case PM_RAID_T:
					if (pm_i->bsdlabel[i].pi_fstype == FS_RAID)
						ok = 1;
					break;
				case PM_CGD_T:
					if (pm_i->bsdlabel[i].pi_fstype == FS_CGD)
						ok = 1;
					break;
				case PM_LVM_T:
					if (pm_i->bsdlabel[i].lvmpv)
						ok = 1;
					break;
			}
			if (ok) {
				disk_entries[num_devs].dev_pm = pm_i;
				disk_entries[num_devs].dev_num = 'a' + i;
				snprintf(disk_entries[num_devs].fullname, SSTRSIZE, "%s%c",
						pm_i->diskdev, 'a' + i);
				menu_entries[num_devs] = (struct menu_ent) {
					.opt_name = disk_entries[num_devs].fullname,
					.opt_action = set_menu_select,
					.opt_menu = OPT_NOMENU,
					.opt_flags = OPT_EXIT,
				};
				num_devs++;
			}
		}

	menu_no = new_menu("Available disks:",
		menu_entries, num_devs, -1, -1, (num_devs+1<3)?3:num_devs+1, 13, MC_SCROLL | MC_NOCLEAR,
		NULL, NULL , NULL, NULL, NULL);
	if (menu_no == -1)
		return (part_entry_t) { .retvalue = -1, };
	process_menu(menu_no, &dev_num);
	free_menu(menu_no);

	if (dev_num < 0 || dev_num >= num_devs)
		return (part_entry_t) { .retvalue = -1, };

	disk_entries[dev_num].retvalue = dev_num;
	return disk_entries[dev_num];
}

/*** RAIDs ***/

static void
partman_raid_menufmt(menudesc *m, int opt, void *arg)
{
	int i, num, ok = 0;
	
	num = ((part_entry_t *)arg)[opt].dev_num;

	if (raids[num].enabled != 0) {
		wprintw(m->mw, "   RAID%d - ", raids[num].raid_level);
		for (i = 0; i < MAX_IN_RAID; i++)
			if (raids[num].pm[i] != NULL) {
				wprintw(m->mw, "%s ", raids[num].pm_name[i]);
				ok = 1;
			}
		if (!ok)
			wprintw(m->mw, "no components");
	}
	return;
}

static void
partman_raid_edit_menufmt(menudesc *m, int opt, void *arg)
{
	int i;
	char buf[STRSIZE];
	buf[0] = '\0';

	switch (opt){
		case PMR_MENU_DEVS:
			for (i = 0; i < MAX_IN_RAID; i++)
				if (raids[manage_curdev].pm[i] != NULL && raids[manage_curdev].pm_is_spare[i] == 0)
					snprintf(buf, STRSIZE, "%s %s", buf, raids[manage_curdev].pm_name[i]);
			wprintw(m->mw, "Disks: %33s", buf);
			break;
		case PMR_MENU_DEVSSPARE:
			for (i = 0; i < MAX_IN_RAID; i++)
				if (raids[manage_curdev].pm[i] != NULL && raids[manage_curdev].pm_is_spare[i] != 0)
					snprintf(buf, STRSIZE, "%s %s", buf, raids[manage_curdev].pm_name[i]);
			wprintw(m->mw, "Spares: %32s", buf);
			break;
		case PMR_MENU_RAIDLEVEL:
			wprintw(m->mw, "RAID level:       %22d", raids[manage_curdev].raid_level);
			break;
		case PMR_MENU_NUMROW:
			wprintw(m->mw, "numRow:           %22d", raids[manage_curdev].numRow);
			break;
		case PMR_MENU_NUMCOL:
			wprintw(m->mw, "numCol:           %22d", raids[manage_curdev].numCol);
			break;
		case PMR_MENU_NUMSPARE:
			wprintw(m->mw, "numSpare:         %22d", raids[manage_curdev].numSpare);
			break;
		case PMR_MENU_SECTPERSU:
			wprintw(m->mw, "sectPerSU:        %22d", raids[manage_curdev].sectPerSU);
			break;
		case PMR_MENU_SUSPERPARITYUNIT:
			wprintw(m->mw, "SUsPerParityUnit: %22d", raids[manage_curdev].SUsPerParityUnit);
			break;
		case PMR_MENU_SUSPERRECONUNIT:
			wprintw(m->mw, "SUsPerReconUnit:  %22d", raids[manage_curdev].SUsPerReconUnit);
			break;
	}
	return;
}

static int
partman_raid_set_value(menudesc *m, void *arg)
{
	char buf[SSTRSIZE];
	const char *msg_to_show = NULL;
	int retvalue = -1;
	int *out_var = NULL;
	static menu_ent menuent_disk_adddel[] = {
	    {"Add", OPT_NOMENU, OPT_EXIT, partman_raid_disk_add}, 
	    {"Remove", OPT_NOMENU, OPT_EXIT, partman_raid_disk_del}
	};
	static int menu_disk_adddel = -1;
	if (menu_disk_adddel == -1) {
		menu_disk_adddel = new_menu(NULL, menuent_disk_adddel, nelem(menuent_disk_adddel),
			-1, -1, 0, 10, MC_NOCLEAR, NULL, NULL, NULL, NULL, NULL);
	}
	
	switch (m->cursel) {
		case PMR_MENU_DEVS:
			manage_raid_spare_cur = 0;
			process_menu(menu_disk_adddel, NULL);
			return 0;
		case PMR_MENU_DEVSSPARE:
			manage_raid_spare_cur = 1;
			process_menu(menu_disk_adddel, NULL);
			return 0;
		case PMR_MENU_RAIDLEVEL:
			process_menu(MENU_raidlevel, &retvalue);
			if (retvalue > 0)
				raids[manage_curdev].raid_level = retvalue;
			return 0;
		case PMR_MENU_NUMROW:
			msg_prompt_win("Multi-dimensional arrays are NOT supported!",
							-1, 18, 0, 0, NULL, NULL, 0);
			return 0;
			msg_to_show = "numRow?";
			out_var = &(raids[manage_curdev].numRow);
			break;
		case PMR_MENU_NUMCOL:
			msg_to_show = "numCol?";
			out_var = &(raids[manage_curdev].numCol);
			break;
		case PMR_MENU_NUMSPARE:
			msg_to_show = "numSpare?";
			out_var = &(raids[manage_curdev].numSpare);
			break;
		case PMR_MENU_SECTPERSU:
			msg_to_show = "sectPerSU?";
			out_var = &(raids[manage_curdev].sectPerSU);
			break;
		case PMR_MENU_SUSPERPARITYUNIT:
			msg_to_show = "SUsPerParityUnit?";
			out_var = &(raids[manage_curdev].SUsPerParityUnit);
			break;
		case PMR_MENU_SUSPERRECONUNIT:
			msg_to_show = "SUsPerReconUnit?";
			out_var = &(raids[manage_curdev].SUsPerReconUnit);
			break;
		case PMR_MENU_REMOVE:
			raids[manage_curdev].enabled = 0;
			return 0;
	}
	if (out_var == NULL || msg_to_show == NULL)
		return -1;
	snprintf(buf, SSTRSIZE, "%d", *out_var);
	msg_prompt_win(msg_to_show, -1, 18, 0, 0, buf, buf, SSTRSIZE);
	if (atoi(buf) >= 0)
		*out_var = atoi(buf);
	return 0;
}

static void
partman_raid_new_init(void* none)
{
	memset(&(raids[manage_curdev]), 0, sizeof raids[manage_curdev]);
	raids[manage_curdev] = (struct raids_t) {
		.enabled = 1,
		.sectPerSU = 32,
		.SUsPerParityUnit = 1,
		.SUsPerReconUnit = 1,
	};
	return;
}

static int
partman_raid_check(int dev_num)
{
	int ok = 0, i;
	for (i = 0; i < MAX_IN_RAID; i++)
		if (raids[dev_num].pm[i] != NULL && 
				raids[dev_num].pm_is_spare[i] != 1) {
			ok = 1;
			break;
		}
	if (! ok)
		raids[dev_num].enabled = 0;
	return raids[dev_num].enabled;
}

static int
partman_raid_disk_add(menudesc *m, void *arg)
{
	int i;
	part_entry_t disk_entries = partman_manage_getdev(PM_RAID_T);
	if (disk_entries.retvalue < 0)
		return disk_entries.retvalue;

	for (i = 0; i < MAX_IN_RAID; i++)
		if (raids[manage_curdev].pm[i] == NULL) {
			raids[manage_curdev].pm[i] = disk_entries.dev_pm;
			raids[manage_curdev].pm_part[i] = disk_entries.dev_num;
			raids[manage_curdev].pm_is_spare[i] = manage_raid_spare_cur;
			strncpy(raids[manage_curdev].pm_name[i], disk_entries.fullname, SSTRSIZE);
			if (manage_raid_spare_cur)
				raids[manage_curdev].numSpare++;
			else
				raids[manage_curdev].numCol++;
			raids[manage_curdev].numRow = 1;
			break;
		}
	return 0;
}

static int
partman_raid_disk_del(menudesc *m, void *arg)
{
	int retvalue = -1, num_devs = 0;
	int i, pm_cur;
	int menu_no;
	menu_ent menu_entries[MAX_IN_RAID];
	part_entry_t submenu_args[MAX_IN_RAID];

	for (i = 0; i < MAX_IN_RAID; i++) {
		if (raids[manage_curdev].pm[i] == NULL ||
			raids[manage_curdev].pm_is_spare[i] != manage_raid_spare_cur)
			continue;
		menu_entries[num_devs] = (struct menu_ent) {
			.opt_name = raids[manage_curdev].pm_name[i],
			.opt_action = set_menu_select,
			.opt_menu = OPT_NOMENU,
			.opt_flags = OPT_EXIT,
		};
		submenu_args[num_devs].dev_num = i;
		num_devs++;
	}

	menu_no = new_menu("Disks in RAID:",
		menu_entries, num_devs, -1, -1, (num_devs+1<3)?3:num_devs+1, 13,
		MC_SCROLL | MC_NOCLEAR, NULL, NULL, NULL, NULL, NULL);
	if (menu_no == -1)
		return -1;
	process_menu(menu_no, &retvalue);
	free_menu(menu_no);

	if (retvalue < 0 || retvalue >= num_devs)
		return -1;

	pm_cur = submenu_args[retvalue].dev_num;

	if (raids[manage_curdev].pm_is_spare[pm_cur])
		raids[manage_curdev].numSpare--;
	else
		raids[manage_curdev].numCol--;
	raids[manage_curdev].numRow = (raids[manage_curdev].numCol)?1:0;
	raids[manage_curdev].pm[pm_cur] = NULL;

	return 0;
}

static int
partman_raid_edit(menudesc *m, void *arg)
{
	((part_entry_t *)arg)[0].retvalue = m->cursel + 1;

	return partman_manage_edit(PMR_MENU_END, partman_raid_edit_menufmt,
		partman_raid_set_value,	partman_raid_check, partman_raid_new_init,
		NULL, ((part_entry_t *)arg)[m->cursel].dev_num, MAX_RAID,
		sizeof raids[0], &(raids[0].enabled));
}

static int
partman_raid_commit(void)
{
	int i, ii, raid_devnum;
	FILE *f;
	char f_name[STRSIZE];

	for (raid_devnum = 0, i = 0; raid_devnum < MAX_RAID && i < MAX_RAID;
			raid_devnum++, i++) {
		if (! partman_raid_check(i))
			continue;

		/* Trying to detect free raid device (yes, ugly way) */
		while (run_program(RUN_SILENT | RUN_ERROR_OK, "raidctl -G raid%d",
				raid_devnum) == 0)
			raid_devnum++;

		/* Generating configure file for our raid */
		snprintf(f_name, SSTRSIZE, "/tmp/raid.%d.conf", raid_devnum);
		f = fopen(f_name, "w");
		if (f == NULL) {
			endwin();
			(void)fprintf(stderr, "Could not open %s for writing\n", f_name);
			if (logfp)
				(void)fprintf(logfp, "Could not open %s for writing\n", f_name);
			return 1;
		}
		scripting_fprintf(NULL, "cat <<EOF >%s\n", f_name);
		scripting_fprintf(f, "START array\n%d %d %d\n", raids[i].numRow,
							raids[i].numCol, raids[i].numSpare);

		scripting_fprintf(f, "\nSTART disks\n");
		for (ii = 0; ii < MAX_IN_RAID; ii++)
			if (raids[i].pm[ii] != NULL && raids[i].pm_is_spare[ii] == 0) {
				scripting_fprintf(f,  "/dev/%s\n", raids[i].pm_name[ii]);
			}

		scripting_fprintf(f, "\nSTART spare\n");
		for (ii = 0; ii < MAX_IN_RAID; ii++)
			if (raids[i].pm[ii] != NULL && raids[i].pm_is_spare[ii] != 0) {
				scripting_fprintf(f,  "/dev/%s\n", raids[i].pm_name[ii]);
			}

		scripting_fprintf(f, "\nSTART layout\n%d %d %d %d\n", raids[i].sectPerSU,
						raids[i].SUsPerParityUnit, raids[i].SUsPerReconUnit,
						raids[i].raid_level);

		scripting_fprintf(f, "\nSTART queue\nfifo 100\n\n");
		scripting_fprintf(NULL, "EOF\n");
		fclose (f);
		fflush(NULL);

		/* Raid initialization */
		if (
			run_program(RUN_DISPLAY, "raidctl -C %s raid%d",
							f_name, raid_devnum) == 0 &&
			run_program(RUN_DISPLAY, "raidctl -I %d raid%d",
							rand(), raid_devnum) == 0 &&
			run_program(RUN_DISPLAY, "raidctl -vi raid%d",
							raid_devnum) == 0
			)
			raids[i].enabled = 0; /* RAID creation done, remove it from list to 
									 prevent it's repeated reinitialization */
	}
	return 0;
}

/*** VND ***/

static void
partman_vnd_menufmt(menudesc *m, int opt, void *arg)
{
	int num = ((part_entry_t *)arg)[opt].dev_num;

	if (vnds[num].enabled != 0) {
		if (strlen(vnds[num].filepath) < 1)
			wprintw(m->mw, "   PATH NOT DEFINED!");
		else
			wprintw(m->mw, "   %s (%dMB)", vnds[num].filepath, vnds[num].size);
	}
	return;
}

static void
partman_vnd_edit_menufmt(menudesc *m, int opt, void *arg)
{
	char buf[SSTRSIZE];
	strcpy(buf, "-");

	switch (opt){
		case PMV_MENU_FILEPATH:
			wprintw(m->mw, "File path: %29s", vnds[manage_curdev].filepath);
			break;
		case PMV_MENU_EXIST:
			wprintw(m->mw, "Assign exist image: %20s", vnds[manage_curdev].is_exist?"yes":"no");
			break;
		case PMV_MENU_SIZE:
			if (!vnds[manage_curdev].is_exist)
				snprintf(buf, SSTRSIZE, "%d", vnds[manage_curdev].size);
			wprintw(m->mw, "Size (MB):        %22s", buf);
			break;
		case PMV_MENU_RO:
			wprintw(m->mw, "Read-only:        %22s", vnds[manage_curdev].readonly?"yes":"no");
			break;
		case PMV_MENU_MGEOM:
			if (!vnds[manage_curdev].is_exist)
				snprintf(buf, SSTRSIZE, "%s", vnds[manage_curdev].manual_geom?"yes":"no");
			wprintw(m->mw, "Set geometry by hand: %18s", buf);
			break;
		case PMV_MENU_SECSIZE:
			if (vnds[manage_curdev].manual_geom && !vnds[manage_curdev].is_exist)
				snprintf(buf, SSTRSIZE, "%d", vnds[manage_curdev].secsize);
			wprintw(m->mw, "Bytes per Sector:     %18s", buf);
			break;
		case PMV_MENU_NSECTORS:
			if (vnds[manage_curdev].manual_geom && !vnds[manage_curdev].is_exist)
				snprintf(buf, SSTRSIZE, "%d", vnds[manage_curdev].nsectors);
			wprintw(m->mw, "Sectors per Track:    %18s", buf);
			break;
		case PMV_MENU_NTRACKS:
			if (vnds[manage_curdev].manual_geom && !vnds[manage_curdev].is_exist)
				snprintf(buf, SSTRSIZE, "%d", vnds[manage_curdev].ntracks);
			wprintw(m->mw, "Tracks per Cylinder:  %18s", buf);
			break;
		case PMV_MENU_NCYLINDERS:
			if (vnds[manage_curdev].manual_geom && !vnds[manage_curdev].is_exist)
				snprintf(buf, SSTRSIZE, "%d", vnds[manage_curdev].ncylinders);
			wprintw(m->mw, "Cylinders:        %22s", buf);
			break;

	}
	return;
}

static int
partman_vnd_set_value(menudesc *m, void *arg)
{
	char buf[SSTRSIZE];
	const char *msg_to_show = NULL;
	int *out_var = NULL;
	
	switch (m->cursel) {
		case PMV_MENU_FILEPATH:
			msg_prompt_win("File path?", -1, 18, 0, 0, vnds[manage_curdev].filepath,
				vnds[manage_curdev].filepath, STRSIZE);
			return 0;
		case PMV_MENU_EXIST:
			vnds[manage_curdev].is_exist = !vnds[manage_curdev].is_exist;
			return 0;
		case PMV_MENU_SIZE:
			if (vnds[manage_curdev].is_exist)
				return 0;
			msg_to_show = "Size (MB)";
			out_var = &(vnds[manage_curdev].size);
			break;
		case PMV_MENU_RO:
			vnds[manage_curdev].readonly = !vnds[manage_curdev].readonly;
			return 0;
		case PMV_MENU_MGEOM:
			if (vnds[manage_curdev].is_exist)
				return 0;
			vnds[manage_curdev].manual_geom = !vnds[manage_curdev].manual_geom;
			return 0;
		case PMV_MENU_SECSIZE:
			if (!vnds[manage_curdev].manual_geom || vnds[manage_curdev].is_exist)
				return 0;
			msg_to_show = "Bytes per Sector";
			out_var = &(vnds[manage_curdev].secsize);
			break;
		case PMV_MENU_NSECTORS:
			if (!vnds[manage_curdev].manual_geom || vnds[manage_curdev].is_exist)
				return 0;
			msg_to_show = "Sectors per Track";
			out_var = &(vnds[manage_curdev].nsectors);
			break;
		case PMV_MENU_NTRACKS:
			if (!vnds[manage_curdev].manual_geom || vnds[manage_curdev].is_exist)
				return 0;
			msg_to_show = "Tracks per Cylinder";
			out_var = &(vnds[manage_curdev].ntracks);
			break;
		case PMV_MENU_NCYLINDERS:
			if (!vnds[manage_curdev].manual_geom || vnds[manage_curdev].is_exist)
				return 0;
			msg_to_show = "Cylinders";
			out_var = &(vnds[manage_curdev].ncylinders);
			break;
		case PMV_MENU_REMOVE:
			vnds[manage_curdev].enabled = 0;
			return 0;
	}
	if (out_var == NULL || msg_to_show == NULL)
		return -1;
	snprintf(buf, SSTRSIZE, "%d", *out_var);
	msg_prompt_win(msg_to_show, -1, 18, 0, 0, buf, buf, SSTRSIZE);
	if (atoi(buf) >= 0)
		*out_var = atoi(buf);
	return 0;
}

static void
partman_vnd_new_init(void * none)
{
	memset(&(vnds[manage_curdev]), 0, sizeof vnds[manage_curdev]);
	vnds[manage_curdev] = (struct vnds_t) {
		.enabled = 1,
		.filepath[0] = '\0',
		.is_exist = 0,
		.size = 1024,
		.readonly = 0,
		.manual_geom = 0,
		.secsize = 512,
		.nsectors = 18,
		.ntracks = 2,
		.ncylinders = 80
	};
	return;
}

static int
partman_vnd_check(int dev_num)
{
	if (strlen(vnds[dev_num].filepath) < 1 ||
			vnds[dev_num].size < 1)
		vnds[dev_num].enabled = 0;
	return vnds[dev_num].enabled;
}

static int
partman_vnd_edit(menudesc *m, void *arg)
{
	((part_entry_t *)arg)[0].retvalue = m->cursel + 1;

	return partman_manage_edit(PMV_MENU_END, partman_vnd_edit_menufmt,
		partman_vnd_set_value, partman_vnd_check, partman_vnd_new_init,
		NULL, ((part_entry_t *)arg)[m->cursel].dev_num, MAX_VND,
		sizeof vnds[0], &(vnds[0].enabled));
}

/* TODO: should use only unallocated devices */
/* TODO: vnconfig always return 0? */
static int
partman_vnd_commit(void)
{
	int i, not_ok;
	char r_o[3];

	for (i = 0; i < MAX_VND; i++) {
		not_ok = 0;
		if (! partman_vnd_check(i))
			continue;
		strcpy(r_o, vnds[i].readonly?"-r":"");
		/* If this is a new image */
		if (!vnds[i].is_exist)
			not_ok += run_program(RUN_DISPLAY,
						"dd if=/dev/zero of=%s bs=1m count=%d progress=100 msgfmt=human",
						target_expand(vnds[i].filepath), vnds[i].size);
		if (not_ok)
			continue;

		/* If this is a new image with manual geometry */
		if (!vnds[i].is_exist && vnds[i].manual_geom)
			not_ok += run_program(RUN_DISPLAY,
						"vnconfig %s vnd%d %s %d %d %d %d", r_o, i,
						target_expand(vnds[i].filepath), vnds[i].secsize,
						vnds[i].nsectors, vnds[i].ntracks,	vnds[i].ncylinders);
		/* If this is a existent image or image without manual geometry */
		else
			not_ok += run_program(RUN_DISPLAY, "vnconfig %s vnd%d %s",
						r_o, i, target_expand(vnds[i].filepath));

		if (! not_ok)
			vnds[i].enabled = 0;
	}
	return 0;
}

/*** CGD ***/

static void
partman_cgd_menufmt(menudesc *m, int opt, void *arg)
{
	int num = ((part_entry_t *)arg)[opt].dev_num;

	if (cgds[num].enabled != 0) {
		if (cgds[num].pm == NULL)
			wprintw(m->mw, "   DISK NOT DEFINED!");
		else
			wprintw(m->mw, "   %s%c (%s-%d)", cgds[num].pm->diskdev,
				cgds[num].pm_part, cgds[num].enc_type, cgds[num].key_size);
	}
	return;
}

static void
partman_cgd_edit_menufmt(menudesc *m, int opt, void *arg)
{
	switch (opt){
		case PMC_MENU_DEV:
			wprintw(m->mw, "Base device:    %24s", cgds[manage_curdev].pm_name);
			break;
		case PMC_MENU_ENCTYPE:
			wprintw(m->mw, "Encyption:      %24s", cgds[manage_curdev].enc_type);
			break;
		case PMC_MENU_KEYSIZE:
			wprintw(m->mw, "Key size:       %24d", cgds[manage_curdev].key_size);
			break;
		case PMC_MENU_IVTYPE:
			wprintw(m->mw, "IV algorithm:   %24s", cgds[manage_curdev].iv_type);
			break;
		case PMC_MENU_KEYGENTYPE:
			wprintw(m->mw, "Key generation: %24s", cgds[manage_curdev].keygen_type);
			break;
		case PMC_MENU_VERIFYTYPE:
			wprintw(m->mw, "Verification method:   %17s", cgds[manage_curdev].verify_type);
			break;
	}
	return;
}

static int
partman_cgd_set_value(menudesc *m, void *arg)
{
	char *retstring;

	switch (m->cursel) {
		case PMC_MENU_DEV:
			partman_cgd_disk_set(NULL);
			return 0;
		case PMC_MENU_ENCTYPE:
			process_menu(MENU_cgd_enctype, &retstring);
			cgds[manage_curdev].enc_type = retstring;
			if (! strcmp(retstring, "blowfish-cbc"))
				cgds[manage_curdev].key_size = 128;
			if (! strcmp(retstring, "3des-cbc"))
				cgds[manage_curdev].key_size = 192;
			return 0;
		case PMC_MENU_KEYSIZE:
			if (! strcmp(cgds[manage_curdev].enc_type, "aes-cbc"))
				cgds[manage_curdev].key_size +=
					(cgds[manage_curdev].key_size < 256)? 64 : -128;
			if (! strcmp(cgds[manage_curdev].enc_type, "blowfish-cbc"))
				cgds[manage_curdev].key_size = 128;
			if (! strcmp(cgds[manage_curdev].enc_type, "3des-cbc"))
				cgds[manage_curdev].key_size = 192;
			return 0;
		case PMC_MENU_IVTYPE:
			process_menu(MENU_cgd_ivtype, &retstring);
			cgds[manage_curdev].iv_type = retstring;
			return 0;
		case PMC_MENU_KEYGENTYPE:
			process_menu(MENU_cgd_keygentype, &retstring);
			cgds[manage_curdev].keygen_type = retstring;
			return 0;
		case PMC_MENU_VERIFYTYPE:
			process_menu(MENU_cgd_verifytype, &retstring);
			cgds[manage_curdev].verify_type = retstring;
			return 0;
		case PMC_MENU_REMOVE:
			cgds[manage_curdev].enabled = 0;
			return 0;
	}
	return -1;
}

static void
partman_cgd_new_init(void *assign_pm)
{
	part_entry_t disk_entrie;

	memset(&(cgds[manage_curdev]), 0, sizeof cgds[manage_curdev]);
	cgds[manage_curdev] = (struct cgds_t) {
		.enabled = 1,
		.pm = NULL,
		.pm_name[0] = '\0',
		.pm_part = 0,
		.keygen_type = "pkcs5_pbkdf2/sha1",
		.verify_type = "disklabel",
		.enc_type = "aes-cbc",
		.iv_type = "encblkno1",
		.key_size = 192,
	};
	if (assign_pm != NULL) {
			disk_entrie.dev_pm = assign_pm;
			disk_entrie.dev_num = 'a' + PART_E;
			snprintf(disk_entrie.fullname, SSTRSIZE, "%s%c",
				pm->diskdev, 'a' + PART_E);
		partman_cgd_disk_set(&disk_entrie);
	}
	return;
}

static int
partman_cgd_check(int dev_num)
{
	if (cgds[dev_num].pm == NULL)
		cgds[dev_num].enabled = 0;
	return cgds[dev_num].enabled;
}

static int
partman_cgd_disk_set(part_entry_t *disk_entrie)
{
	int alloc_disk_entrie = 0;
	if (disk_entrie == NULL) {
		alloc_disk_entrie = 1;
		disk_entrie = malloc (sizeof(part_entry_t));
		if (disk_entrie == NULL)
			return -2;
		*disk_entrie = partman_manage_getdev(PM_CGD_T);
		if (disk_entrie->retvalue < 0) {
			free(disk_entrie);
			return -1;
		}
	}
	cgds[manage_curdev].pm = disk_entrie->dev_pm;
	cgds[manage_curdev].pm_part = disk_entrie->dev_num;
	strncpy(cgds[manage_curdev].pm_name, disk_entrie->fullname, SSTRSIZE);

	if (alloc_disk_entrie)
		free(disk_entrie);
	return 0;
}

int
partman_cgd_edit_adddisk(int cursel, pm_devs_t *assign_pm)
{
	if (assign_pm != NULL)
		cursel = -1;
	return partman_manage_edit(PMC_MENU_END, partman_cgd_edit_menufmt,
		partman_cgd_set_value, partman_cgd_check, partman_cgd_new_init,
		assign_pm, cursel, MAX_CGD, sizeof cgds[0], &(cgds[0].enabled));
}

static int
partman_cgd_edit(menudesc *m, void *arg)
{
	((part_entry_t *)arg)[0].retvalue = m->cursel + 1;
	return partman_cgd_edit_adddisk(((part_entry_t *)arg)[m->cursel].dev_num, NULL);
}

/* TODO: should use only unallocated devices */
static int
partman_cgd_commit(void)
{
	int i;
	for (i = 0; i < MAX_CGD; i++) {
		if (! partman_cgd_check(i))
			continue;
		if (run_program(RUN_DISPLAY, "cgdconfig -g -i %s -k %s -o /tmp/%s %s %d",
			cgds[i].iv_type, cgds[i].keygen_type, cgds[i].pm_name,
			cgds[i].enc_type, cgds[i].key_size) != 0)
			continue;
		if (run_program(RUN_DISPLAY, "cgdconfig -V re-enter cgd%d /dev/%s /tmp/%s",
			i, cgds[i].pm_name, cgds[i].pm_name) != 0)
			continue;
		cgds[i].enabled = 0;
	}

	return 0;
}

/*** LVM ***/

static int
partman_lvm_disk_add(menudesc *m, void *arg)
{
	int i;
	part_entry_t disk_entries = partman_manage_getdev(PM_LVM_T);
	if (disk_entries.retvalue < 0)
		return disk_entries.retvalue;

	for (i = 0; i < MAX_LVM_PV; i++)
		if (lvms[manage_curdev].pv[i].pm == NULL) {
			lvms[manage_curdev].pv[i].pm = disk_entries.dev_pm;
			lvms[manage_curdev].pv[i].pm_part = disk_entries.dev_num;
			strncpy(lvms[manage_curdev].pv[i].pm_name, disk_entries.fullname, SSTRSIZE);
			break;
		}
	return 0;
}

static int
partman_lvm_disk_del(menudesc *m, void *arg)
{
	int i, retvalue = -1, num_devs = 0;
	int menu_no;
	menu_ent menu_entries[MAX_LVM_PV];
	part_entry_t submenu_args[MAX_LVM_PV];

	for (i = 0; i < MAX_LVM_PV; i++) {
		if (lvms[manage_curdev].pv[i].pm == NULL)
			continue;
		menu_entries[num_devs] = (struct menu_ent) {
			.opt_name = lvms[manage_curdev].pv[i].pm_name,
			.opt_action = set_menu_select,
			.opt_menu = OPT_NOMENU,
			.opt_flags = OPT_EXIT,
		};
		submenu_args[num_devs].dev_num = i;
		num_devs++;
	}

	menu_no = new_menu("Disks in VG:",
		menu_entries, num_devs, -1, -1, (num_devs+1<3)?3:num_devs+1, 13,
		MC_SCROLL | MC_NOCLEAR, NULL, NULL, NULL, NULL, NULL);
	if (menu_no == -1)
		return -1;
	process_menu(menu_no, &retvalue);
	free_menu(menu_no);

	if (retvalue < 0 || retvalue >= num_devs)
		return -1;

	lvms[manage_curdev].pv[submenu_args[retvalue].dev_num].pm = NULL;

	return 0;
}

static void
partman_lvm_menufmt(menudesc *m, int opt, void *arg)
{
	int i, num, ok = 0;

	num = ((part_entry_t *)arg)[opt].dev_num;

	if (lvms[num].enabled != 0) {
		wprintw(m->mw, "   Volume Group '%s' - ", lvms[num].name);
		for (i = 0; i < MAX_LVM_PV; i++)
			if (lvms[num].pv[i].pm != NULL) {
				wprintw(m->mw, "%s ", lvms[num].pv[i].pm_name);
				ok = 1;
			}
		if (!ok)
			wprintw(m->mw, "no components");
	}
	return;
}

static void
partman_lvm_edit_menufmt(menudesc *m, int opt, void *arg)
{
	int i;
	char buf[STRSIZE];
	buf[0] = '\0';

	switch (opt){
		case PML_MENU_PV:
			for (i = 0; i < MAX_LVM_PV; i++)
				if (lvms[manage_curdev].pv[i].pm != NULL)
					snprintf(buf, STRSIZE, "%s %s", buf, lvms[manage_curdev].pv[i].pm_name);
			wprintw(m->mw, "PV's: %34s", buf);
			break;
		case PML_MENU_NAME:
			wprintw(m->mw, "Name: %34s", lvms[manage_curdev].name);
			break;
		case PML_MENU_MAXLOGICALVOLUMES:
			wprintw(m->mw, "MaxLogicalVolumes:  %20d", lvms[manage_curdev].maxlogicalvolumes);
			break;
		case PML_MENU_MAXPHYSICALVOLUMES:
			wprintw(m->mw, "MaxPhysicalVolumes: %20d", lvms[manage_curdev].maxphysicalvolumes);
			break;
		case PML_MENU_PHYSICALEXTENTSIZE:
			wprintw(m->mw, "PhysicalExtentSize: %18dMB", lvms[manage_curdev].physicalextentsize);
			break;
	}
	return;
}

static int
partman_lvm_set_value(menudesc *m, void *arg)
{
	static menu_ent menuent_disk_adddel[] = {
	    {"Add", OPT_NOMENU, OPT_EXIT, partman_lvm_disk_add}, 
	    {"Remove", OPT_NOMENU, OPT_EXIT, partman_lvm_disk_del}
	};
	static int menu_disk_adddel = -1;
	if (menu_disk_adddel == -1) {
		menu_disk_adddel = new_menu(NULL, menuent_disk_adddel, nelem(menuent_disk_adddel),
			-1, -1, 0, 10, MC_NOCLEAR, NULL, NULL, NULL, NULL, NULL);
	}
	
	switch (m->cursel) {
		case PML_MENU_PV:
			manage_raid_spare_cur = 0;
			process_menu(menu_disk_adddel, NULL);
			return 0;
		case PML_MENU_NAME:
			msg_prompt_win("Name?", -1, 18, 0, 0, lvms[manage_curdev].name,
				lvms[manage_curdev].name, SSTRSIZE);
			return 0;
		case PML_MENU_REMOVE:
			lvms[manage_curdev].enabled = 0;
			return 0;
		case PML_MENU_MAXLOGICALVOLUMES:
			// TODO: fsdgf
			break;
		case PML_MENU_MAXPHYSICALVOLUMES:

			break;
		case PML_MENU_PHYSICALEXTENTSIZE:

			break;
	}
	return 0;
}

static void
partman_lvm_new_init(void* none)
{
	memset(&(lvms[manage_curdev]), 0, sizeof lvms[manage_curdev]);
	lvms[manage_curdev] = (struct lvms_t) {
		.enabled = 1,
		.maxlogicalvolumes = MAX_LVM_PV,
		.maxphysicalvolumes = MAX_LVM_LV,
		.physicalextentsize = 4,
	};
	sprintf(lvms[manage_curdev].name, "vg%.2d", manage_curdev);
	return;
}

static int
partman_lvm_check(int dev_num)
{
	int ok = 0, i;
	for (i = 0; i < MAX_LVM_PV; i++)
		if (lvms[dev_num].pv[i].pm != NULL) {
			ok = 1;
			break;
		}
	if (! ok)
		lvms[dev_num].enabled = 0;
	return lvms[dev_num].enabled;
}

static int
partman_lvm_edit(menudesc *m, void *arg)
{
	((part_entry_t *)arg)[0].retvalue = m->cursel + 1;

	return partman_manage_edit(PML_MENU_END, partman_lvm_edit_menufmt,
		partman_lvm_set_value, partman_lvm_check, partman_lvm_new_init,
		NULL, ((part_entry_t *)arg)[m->cursel].dev_num, MAX_LVM_VG,
		sizeof lvms[0], &(lvms[0].enabled));
}

static void
partman_lvmlv_menufmt(menudesc *m, int opt, void *arg)
{
	int num, ok = 0;

	num = ((part_entry_t *)arg)[opt].dev_num;

		if (!ok)
			wprintw(m->mw, "no components");
	
	return;
}

static void
partman_lvmlv_edit_menufmt(menudesc *m, int opt, void *arg)
{
	switch (opt){
		case PMLV_MENU_NAME:
			wprintw(m->mw, "Name: %34s", lvms[manage_curdev].lv[manage_curdev].name);
			break;
	}
	return;
}

static int
partman_lvmlv_set_value(menudesc *m, void *arg)
{
	switch (m->cursel) {
		case PMLV_MENU_REMOVE:
			lvms[manage_curdev].lv[manage_curdev].size = 0;
			return 0;
			break;
	}
	return 0;
}

static void
partman_lvmlv_new_init(void* none)
{
	memset(&(lvms[manage_curdev]), 0, sizeof lvms[manage_curdev]);
	lvms[manage_curdev] = (struct lvms_t) {
		.enabled = 1,
		.maxlogicalvolumes = MAX_LVM_PV,
		.maxphysicalvolumes = MAX_LVM_LV,
		.physicalextentsize = 4,
	};
	sprintf(lvms[manage_curdev].name, "vg%.2d", manage_curdev);
	return;
}

static int
partman_lvmlv_check(int dev_num)
{
	if (lvms[manage_curdev].lv[dev_num].size > 0 ||
		strlen(lvms[manage_curdev].lv[dev_num].name) > 0)
		return 1;
	else
		return 0;
}

static int
partman_lvmlv_edit(menudesc *m, void *arg)
{
	((part_entry_t *)arg)[0].retvalue = m->cursel + 1;

	return partman_manage_edit(PMLV_MENU_END, partman_lvmlv_edit_menufmt,
		partman_lvmlv_set_value, partman_lvmlv_check, partman_lvmlv_new_init,
		NULL, ((part_entry_t *)arg)[m->cursel].dev_num, MAX_LVM_LV,
		sizeof lvms[0].lv[0], &(lvms[0].lv[0]));
}

static int
partman_lvm_commit(void)
{
	int i, ii, not_ok;
	for (i = 0; i < MAX_LVM_VG; i++) {
		if (! partman_lvm_check(i))
			continue;
		not_ok = 0;
		for (ii = 0; ii < MAX_LVM_PV && ! not_ok; ii++)
			if (lvms[i].pv[ii].pm != NULL) {
				not_ok += run_program(RUN_DISPLAY, "lvcreate pvcreate -y /dev/%s",
										(char*)lvms[i].pv[ii].pm_name);
				not_ok += run_program(RUN_DISPLAY, "lvcreate vgcreate %s /dev/%s",
										lvms[i].name, (char*)lvms[i].pv[ii].pm_name);
			}
		for (ii = 0; ii < MAX_LVM_LV && ! not_ok; ii++);

		lvms[i].enabled = 0;
	}

	return 0;
}

/*** Partman generic functions ***/

int 	// TODO: rewrite
partman_unconfigure(void)
{
 	run_program(RUN_SILENT | RUN_ERROR_OK, "cgdconfig -u %s", pm->diskdev);
	run_program(RUN_SILENT | RUN_ERROR_OK, "vnconfig -u %s", pm->diskdev);
	run_program(RUN_SILENT | RUN_ERROR_OK, "raidctl -u %s", pm->diskdev);
	run_program(RUN_SILENT | RUN_ERROR_OK, "umount /dev/%s*", pm->diskdev);
	return 0;
}

static void
partman_select(pm_devs_t *pm_devs_in)
{
	pm = pm_devs_in;
	if (logfp)
		(void)fprintf(logfp,"Partman device: %s\n", pm->diskdev);
	return;

}

static int
partman_mountall_sort(const void *a, const void *b)
{
	return strcmp(mnts[*(const int *)a].pi_mount,
		      mnts[*(const int *)b].pi_mount);
}

/* Mount all available partitions */
static int
partman_mountall(void)
{
	pm_devs_t *pm_i;
	int num_devs = 0;
	int i, ii, error, ok;
	char diskdev_with_root[SSTRSIZE];
	diskdev_with_root[0] = '\0';

	int mnts_order[MNTS_MAX]; // TODO: rewrite
	for (pm_i = pm_head->next, num_devs = 0; pm_i != NULL; pm_i = pm_i->next) {
		ok = 0;
		for (i = 0; i < MAXPARTITIONS; i++) {
			if (pm_i->bsdlabel[i].pi_flags & PIF_MOUNT &&
					pm_i->bsdlabel[i].mnt_opts != NULL) {
				mnts[num_devs].pm_part  = i;
				mnts[num_devs].diskdev  = pm_i->diskdev;
				mnts[num_devs].mnt_opts = pm_i->bsdlabel[i].mnt_opts;
				mnts[num_devs].fsname   = pm_i->bsdlabel[i].fsname;
				mnts[num_devs].pi_mount = pm_i->bsdlabel[i].pi_mount;
				if (strcmp(pm_i->bsdlabel[i].pi_mount, "/") == 0)
					strlcpy(diskdev_with_root, pm_i->bsdlabel[i].pi_mount, 
						sizeof diskdev_with_root);
				num_devs++;
				ok = 1;
			}
		}
		if (ok)
			md_pre_mount();
	}
	if (strlen(diskdev_with_root) == 0) {
		msg_display("No root partition defined, cannot continue\n"); // TODO: localize
		process_menu(MENU_ok, NULL);
		return -1;
	}
	for (i = 0; i < num_devs; i++)
		mnts_order[i] = i;
	qsort(mnts_order, num_devs, sizeof mnts_order[0], partman_mountall_sort);

	for (i = 0; i < num_devs; i++) {
		ii = mnts_order[i];
		make_target_dir(mnts[ii].pi_mount);
		error = target_mount(mnts[ii].mnt_opts, mnts[ii].diskdev, mnts[ii].pm_part, mnts[ii].pi_mount);
		if (error) {
			return error;
		}
	}
	/* Use disk with / as a default if the user has the sets on a local disk */
	strlcpy(localfs_dev, diskdev_with_root, sizeof localfs_dev);
	return 0;
}

static void
partman_mountmenu_menufmt(menudesc *m, int opt, void *arg)
{
	int num = ((part_entry_t *)arg)[opt].dev_num;

	if (pm->bsdlabel[num].pi_flags & PIF_MOUNT && pm->bsdlabel[num].mnt_opts != NULL)
		wprintw(m->mw, "%s%c at %s", pm->diskdev, 'a' + num, pm->bsdlabel[num].pi_mount);

	return;
}

static int
partman_mount(menudesc *m, void *arg)
{
	int num = ((part_entry_t *)arg)[m->cursel].dev_num;

	if (pm->bsdlabel[num].pi_flags & PIF_MOUNT && pm->bsdlabel[num].mnt_opts != NULL)
		return target_mount(pm->bsdlabel[num].mnt_opts, pm->diskdev, num,
					pm->bsdlabel[num].pi_mount);
	return -1;
}

int // TODO: rewrite
partman_mountmenu(void)
{
	int i, num_devs = 0;
	int menu_no;
	menu_ent menu_entries[MAXPARTITIONS];
	part_entry_t args[MAXPARTITIONS];

	for (i = 0; i < MAXPARTITIONS; i++) {
		if (pm->bsdlabel[i].pi_flags & PIF_MOUNT &&
				pm->bsdlabel[i].mnt_opts != NULL) {
			menu_entries[num_devs] = (menu_ent) {
				.opt_name = NULL,
				.opt_action = partman_mount,
				.opt_menu = OPT_NOMENU,
				.opt_flags = OPT_EXIT,
			};
			args[num_devs].dev_num = i;
			num_devs++;
		}
	}
	menu_no = new_menu(NULL, menu_entries, num_devs, 50, 5, num_devs+3, 0,
		MC_ALWAYS_SCROLL | MC_NOCLEAR, NULL, partman_mountmenu_menufmt,
		NULL, NULL, NULL);
	if (menu_no == -1)
		return -1;
	process_menu(menu_no, &args);
	free_menu(menu_no);
	return 0;
}

/* Safe erase of disk */
int
partman_shred(char *dev, int shredtype)
{
	int error = 0;
	switch(shredtype) {
		case SHRED_NONE:
			return 0;
		case SHRED_ZEROS:
			error += run_program(RUN_DISPLAY | RUN_PROGRESS,
				"dd of=/dev/r%sd if=/dev/zero bs=1m progress=100 msgfmt=human", dev);
			return error; 
		case SHRED_RANDOM:
			error += run_program(RUN_DISPLAY | RUN_PROGRESS,
				"dd of=/dev/r%sd if=/dev/urandom bs=1m progress=100 msgfmt=human", dev);
			return error;
		case SHRED_CRYPTO:
			error += run_program(RUN_DISPLAY | RUN_PROGRESS,
				"sh -c 'cgdconfig -s cgd0 /dev/%sd aes-cbc 128 < /dev/urandom'",
				dev);
			if (! error) {
				error += run_program(RUN_DISPLAY | RUN_PROGRESS,
					"dd of=/dev/rcgd0d if=/dev/urandom bs=1m progress=100 msgfmt=human");
				error += run_program(RUN_DISPLAY | RUN_PROGRESS,
					"cgdconfig -u cgd0");
			}
			return error;
	}
	return -1;
}

/* Write all changes to disk */
static int
partman_commit(menudesc *m, void *arg)
{
	int retcode;
	pm_devs_t *pm_i;
	((part_entry_t *)arg)[0].retvalue = m->cursel + 1;

	for (pm_i = pm_head->next; pm_i != NULL; pm_i = pm_i->next) {
		if (! pm_i->changed)
			continue;
		partman_select(pm_i);
		if (
				md_pre_disklabel() != 0  || /* Write partition table */
				write_disklabel() != 0   || /* Write slices table (disklabel) */
				md_post_disklabel() != 0 || /* Enable swap and check badblock */
				make_filesystems()          /* Create filesystems by newfs */
			) { /* If something fails... */
			if (logfp)
				fprintf(logfp, "Disk preparing error\n");
			return -1;
		} else
			pm_i->changed = 0;
		/* Write bootsector if needed */
		if (pm_i->bootable && md_post_newfs() != 0)
			return -1;
	}
	/* Call all functions that may create new devices */
	if ((retcode = partman_raid_commit()) != 0) {
		if (logfp)
			fprintf(logfp, "Raid configuring error #%d\n", retcode);
		return -1;
	}
	if ((retcode = partman_vnd_commit()) != 0) {
		if (logfp)
			fprintf(logfp, "VND configuring error #%d\n", retcode);
		return -1;
	}
	if ((retcode = partman_cgd_commit()) != 0) {
		if (logfp)
			fprintf(logfp, "CGD configuring error #%d\n", retcode);
		return -1;
	}
	if ((retcode = partman_lvm_commit()) != 0) {
		if (logfp)
			fprintf(logfp, "LVM configuring error #%d\n", retcode);
		return -1;
	}
	partman_upddevlist(m, arg);
	if (logfp)
		fflush (logfp);
    return 0;
}

/* Function for 'Enter'-menu */
static int
partman_submenu(menudesc *m, void *arg)
{
	pm_devs_t *pm_i = ((part_entry_t *)arg)[m->cursel].dev_pm;;
	int we_need_updlist = 0;
	((part_entry_t *)arg)[0].retvalue = m->cursel + 1;

	if (pm_i == NULL) 
		return -1;
	partman_select(pm_i);

	process_menu(MENU_pmentry, &we_need_updlist);
	if (we_need_updlist)
		partman_upddevlist(m, arg);

	return 0;
}

/* Functions that generate menu entries text */
static void
partman_menufmt(menudesc *m, int opt, void *arg)
{
	const char *dev_status = "";
	int part_num = ((part_entry_t *)arg)[opt].dev_num;
	pm_devs_t *pm_i = ((part_entry_t *)arg)[opt].dev_pm;

	switch (((part_entry_t *)arg)[opt].type) {
		case PM_DISK_T:
			if (! pm_i->changed)
				dev_status = "UNCHANGED";
			else if (pm_i->bootable)
				dev_status = "BOOT";
			else
				dev_status = "USED";
			wprintw(m->mw, "%-33s Name:%-20s %9s", pm_i->diskdev_descr, pm_i->bsddiskname, dev_status);
			break;
		case PM_PART_T:
			wprintw(m->mw, "   part %c: %-22s %-22s %11uM",
				'a' + part_num,
				(pm_i->bsdlabel[part_num].pi_flags & PIF_MOUNT) ?
					pm_i->bsdlabel[part_num].pi_mount :
					"",
				(pm_i->bsdlabel[part_num].lvmpv) ? 
					"LVM PV" :
					fstype_name(pm_i->bsdlabel[part_num].pi_fstype),
				pm_i->bsdlabel[part_num].pi_size / (MEG / pm_i->sectorsize));
			break;
		case PM_RAID_T:
			partman_raid_menufmt(m, opt, arg);
			break;
		case PM_VND_T:
			partman_vnd_menufmt(m, opt, arg);
			break;
		case PM_CGD_T:
			partman_cgd_menufmt(m, opt, arg);
			break;
		case PM_LVM_T:
			partman_lvm_menufmt(m, opt, arg);
			break;
		case PM_LVMLV_T:
			partman_lvmlv_menufmt(m, opt, arg);
			break;
	}
	return;
}

/* Submenu for RAID/LVM/CGD/VND */
static void
partman_upddevlist_adv(void *arg, menudesc *m, int *i,
	partman_upddevlist_adv_t *d)
{
	int ii;
	if (d->create_msg != NULL) {
		/* We want menu entrie for to create new device */
		((part_entry_t *)arg)[*i].dev_num = -1;
		m->opts[(*i)++] = (struct menu_ent) {
			.opt_name = d->create_msg,
			.opt_action = d->action,
		};
	}
	for (ii = 0; ii < d->max; ii++) {
		if (*(int*)((char*)d->entry_enabled + d->entry_size * ii) == 0)
			continue;
		m->opts[*i] = (struct menu_ent) {
			.opt_name = NULL,
			.opt_action = d->action,
		};
		((part_entry_t *)arg)[*i].dev_num = ii;
		((part_entry_t *)arg)[*i].type = d->pe_type;
		(*i)++;
		if (d->sub != NULL)
			partman_upddevlist_adv(arg, m, i, d->sub);
	}
	return;
}

/* Update partman main menu with devices list */
static int
partman_upddevlist(menudesc *m, void *arg)
{
	int i = 0, ii;
	pm_devs_t *pm_i;
	if (arg != NULL)
		((part_entry_t *)arg)[0].retvalue = m->cursel + 1;

	find_disks("partman");

	if (m != NULL && arg != NULL) {
		for (pm_i = pm_head->next, i = 0; pm_i != NULL;
				pm_i = pm_i->next, i++) {
			m->opts[i].opt_name = NULL;
			m->opts[i].opt_action = partman_submenu;
			((part_entry_t *)arg)[i].dev_pm = pm_i;
			((part_entry_t *)arg)[i].dev_num = -1;
			((part_entry_t *)arg)[i].type = PM_DISK_T;
			for (ii = 0; ii < MAXPARTITIONS; ii++) {
				if (
						(pm_i->bsdlabel[ii].pi_flags & PIF_MOUNT &&
							pm_i->bsdlabel[ii].pi_fstype != FS_UNUSED) ||
						pm_i->bsdlabel[ii].pi_fstype == FS_RAID ||
						pm_i->bsdlabel[ii].pi_fstype == FS_CGD ||
						(pm_i->bsdlabel[ii].pi_fstype == FS_BSDFFS &&
							pm_i->bsdlabel[ii].lvmpv)
					) {
					i++;
					m->opts[i].opt_name = NULL;
					m->opts[i].opt_action = partman_submenu;
					((part_entry_t *)arg)[i].dev_pm = pm_i;
					((part_entry_t *)arg)[i].dev_num = ii;
					((part_entry_t *)arg)[i].type = PM_PART_T;
				}
			}
		}
		partman_upddevlist_adv(arg, m, &i,
			&((partman_upddevlist_adv_t) {"Create cryptographic volume (CGD)", 
									partman_cgd_edit, PM_CGD_T, MAX_CGD,
									sizeof cgds[0], &(cgds[0].enabled), NULL}));
		partman_upddevlist_adv(arg, m, &i,
			&((partman_upddevlist_adv_t) {"Create virtual disk image (VND)",
									partman_vnd_edit, PM_VND_T, MAX_VND,
									sizeof vnds[0], &(vnds[0].enabled), NULL}));
		partman_upddevlist_adv(arg, m, &i,
			&((partman_upddevlist_adv_t) {"Create volume group (LVM VG) [WIP]",
									partman_lvm_edit, PM_LVM_T, MAX_LVM_VG,
									sizeof lvms[0], &(lvms[0].enabled), 
			&((partman_upddevlist_adv_t) {"      Create logical volume",
									partman_lvmlv_edit, PM_LVMLV_T, MAX_LVM_LV,
									sizeof lvms[0].lv[0], &(lvms[0].lv[0].size),
									NULL})}));
		partman_upddevlist_adv(arg, m, &i,
			&((partman_upddevlist_adv_t) {"Create software RAID",
									partman_raid_edit, PM_RAID_T, MAX_RAID, 
									sizeof raids[0], &(raids[0].enabled), NULL}));

		m->opts[i++] = (struct menu_ent) {
			.opt_name = "Update devices list",
			.opt_action = partman_upddevlist,
		};
		m->opts[i  ] = (struct menu_ent) {
			.opt_name = "Save changes",
			.opt_action = partman_commit,
		};
		for (ii = 0; ii <= i; ii++) {
			m->opts[ii].opt_menu = OPT_NOMENU;
			m->opts[ii].opt_flags = OPT_EXIT;
		}
	}
	return i;
}

/* Main partman function */
int
partman(void)
{
	int menu_no;
	int menu_num_entries;
	menu_ent menu_entries[MAX_DISKS+6];
	part_entry_t args[MAX_DISKS];

	do {
		clear();
		refresh();
		menu_num_entries = partman_upddevlist(&((menudesc) {.opts = menu_entries}), args);
		menu_no = new_menu("Partition manager. All disks, partitions, LVM, RAID displayed there.\nAt first add drive, then prepare partitions and go.",
			menu_entries, menu_num_entries+1, 1, 1, 0, 0,
			MC_ALWAYS_SCROLL | MC_NOBOX | MC_NOCLEAR,
			NULL, partman_menufmt, NULL, NULL, "Finish partitioning");
		if (menu_no == -1)
			args[0].retvalue = -1;
		else {
			args[0].retvalue = 0;
			process_menu(menu_no, &args);
			free_menu(menu_no);
		}

		// TODO: check_partitions()
		/* Check that fstab on target device is readable - if so then we can go */
		if (args[0].retvalue == 0) {
			unwind_mounts();
			if (partman_mountall() != 0 ||
				make_fstab() != 0) {
					msg_display("Do you want to try?");
					process_menu(MENU_yesno, NULL);
					args[0].retvalue = (yesno) ? 1:-1;
			} else {
				FILE *file_tmp = fopen(concat_paths(targetroot_mnt, "/etc/fstab"), "r");
				if (file_tmp == NULL)
					args[0].retvalue = -1;
				else
					fclose(file_tmp);
			}
		}
	} while (args[0].retvalue > 0);
	
	/* retvalue <0 - error, retvalue ==0 - user quits, retvalue >0 - all ok */
	return (args[0].retvalue >= 0)?0:-1;
}
