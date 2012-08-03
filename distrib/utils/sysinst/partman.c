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

#include <fcntl.h>
#include <libgen.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <util.h>

#include "defs.h"
#include "md.h"
#include "msg_defs.h"
#include "menu_defs.h"

#define MAX_ENTRIES 96
typedef struct {
	int retvalue;
	int dev_num;
	pm_devs_t *dev_pm;
	void *dev_ptr;
	char fullname[SSTRSIZE];
	enum {PM_DISK_T, PM_PART_T, PM_RAID_T, PM_VND_T, PM_CGD_T,
		PM_LVM_T, PM_LVMLV_T} type;
} part_entry_t;

#define MNTS_MAX 48
struct {
    const char *diskdev, *mnt_opts, *fsname;
    char *pi_mount;
    int pm_part;
} mnts[MNTS_MAX];

// TODO: replace all MAX_ defince with depeding on kernel settings vars
#define MAX_RAID 16 
#define MAX_IN_RAID 48
typedef struct raids_t {
	int enabled;
	int blocked;
	int node;
	char pm_name[MAX_IN_RAID][SSTRSIZE];
	int pm_part[MAX_IN_RAID];
	int pm_is_spare[MAX_IN_RAID];
	int numRow, numCol, numSpare;
	int sectPerSU, SUsPerParityUnit, SUsPerReconUnit, raid_level;
	uint total_size;
	pm_devs_t *pm[MAX_IN_RAID];
} raids_t;
raids_t raids[MAX_RAID];

#define MAX_VND 4
typedef struct vnds_t {
	int enabled;
	int blocked;
	int node;
	char filepath[STRSIZE];
	int size;
	int readonly;
	int is_exist;
	int manual_geom;
	int secsize, nsectors, ntracks, ncylinders;
	int pm_part;    /* Used only for */
	pm_devs_t *pm;  /* reffering device */
} vnds_t;
vnds_t vnds[MAX_VND];

#define MAX_CGD 4
typedef struct cgds_t {
	int enabled;
	int blocked;
	int node;
	pm_devs_t *pm;
	char pm_name[SSTRSIZE];
	int pm_part;
	const char *keygen_type;
	const char *verify_type;
	const char *enc_type;
	const char *iv_type;
	int key_size;
} cgds_t;
cgds_t cgds[MAX_CGD];

#define MAX_LVM_VG 16
#define MAX_LVM_PV 255
#define MAX_LVM_LV 255
typedef struct pv_t {
	pm_devs_t *pm;
	char pm_name[SSTRSIZE];
	int pm_part;
	int metadatasize;
	int metadatacopies;
	int labelsector;
	int setphysicalvolumesize;
} pv_t;
typedef struct lv_t {
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
} lv_t;
typedef struct lvms_t {
	int enabled;
	char name[SSTRSIZE];
	int maxlogicalvolumes;
	int maxphysicalvolumes;
	int physicalextentsize;
	uint total_size;
	pv_t pv[MAX_LVM_PV];
	lv_t lv[MAX_LVM_LV];
} lvms_t;
lvms_t lvms[MAX_LVM_VG];

typedef struct structinfo_t {
	int max;
	uint entry_size;
	void *entry_first;
	void *entry_enabled;
	void *entry_blocked;
	void *entry_node;
} structinfo_t;
structinfo_t raids_t_info, vnds_t_info, cgds_t_info, lvms_t_info;

typedef struct partman_upddevlist_adv_t {
	const char *create_msg;
	int (*action)(menudesc *, void *);
	int pe_type;
	structinfo_t *s;
	struct partman_upddevlist_adv_t *sub;
} partman_upddevlist_adv_t;

int changed; /* flag indicating that we have unsaved changes */
int manage_raid_spare_cur; // TODO: replace by true way

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
	PMLV_MENU_NAME, PMLV_MENU_SIZE, PMLV_MENU_READONLY, PMLV_MENU_CONTIGUOUS,
	PMLV_MENU_CHUNKSIZE, PMLV_MENU_EXTENTS, PMLV_MENU_MINOR, PMLV_MENU_MIRRORS,
	PMLV_MENU_REGIONSIZE, PMLV_MENU_PERSISTENT, PMLV_MENU_READAHEAD,
	PMLV_MENU_STRIPES, PMLV_MENU_STRIPESIZE, PMLV_MENU_ZERO, 
	PMLV_MENU_REMOVE, PMLV_MENU_END
};

part_entry_t partman_manage_getdev(int);
static int partman_raid_disk_add(menudesc *, void *);
static int partman_raid_disk_del(menudesc *, void *);
static int partman_cgd_disk_set(cgds_t *, part_entry_t *);
static int partman_mount(pm_devs_t *, int);
static int partman_upddevlist(menudesc *, void *);


/* Menu for RAID/VND/CGD/LVM entry edit */
static int
partman_manage_edit(int menu_entries_count, void (*menu_fmt)(menudesc *, int, void *),
	int (*action)(menudesc *, void *), int (*check_fun)(void *),
	void (*entry_init)(void *, void *),	void* entry_init_arg,
	void *dev_ptr, structinfo_t *s)
{
	int i, ok = 0;

	if (dev_ptr == NULL) {
		/* We should create new device */
		for (i = 0; i < s->max && !ok; i++)
			if (*(int*)((char*)s->entry_enabled + s->entry_size * i) == 0) {
				dev_ptr = (char*)s->entry_first + s->entry_size * i;
				entry_init(dev_ptr, entry_init_arg);
				ok = 1;
			}
		if (!ok) {
			msg_prompt_win("Limit for the devices count was reached!", -1, 18, 0, 0, NULL, NULL, 0);
			return -1;
		}
	} else {
		/* ... or edit existent */
		if (check_fun(dev_ptr) == 0) {
			if (logfp)
				fprintf(logfp, "partman_manage_edit: invalid drive\n");
				msg_prompt_win("Invalid device!", -1, 18, 0, 0, NULL, NULL, 0);
			return -2;
		}
	}

	menu_ent menu_entries[menu_entries_count];
	for (i = 0; i < menu_entries_count - 1; i++)
		menu_entries[i] = (menu_ent) {NULL, OPT_NOMENU, 0, action};
	menu_entries[i] = (menu_ent) {"REMOVE", OPT_NOMENU, OPT_EXIT, action};

	int menu_no = -1;
	menu_no = new_menu(NULL, menu_entries, menu_entries_count,
		-1, -1, 0, 40, MC_NOCLEAR | MC_SCROLL,
		NULL, menu_fmt, NULL, NULL, MSG_DONE);
	
	process_menu(menu_no, dev_ptr);
	free_menu(menu_no);

	return check_fun(dev_ptr);
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
				disk_entries[num_devs].dev_num = i;
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

static int
partman_manage_getfreenode(void *node, const char *d, structinfo_t *s)
{
	int i, ii, ok;
	char buf[SSTRSIZE];
	pm_devs_t *pm_i;

	*(int*)node = -1;
	for (i = 0; i < s->max; i++) {
		ok = 1;
		/* Check that node is not already reserved */
		for (ii = 0; ii < s->max; ii++)
			if (*(int*)((char*)s->entry_node + s->entry_size * ii) == i) {
				ok = 0;
				break;
			}
		if (! ok)
			continue;
		/* Check that node is not in the device list */
		snprintf(buf, SSTRSIZE, "%s%d", d, i);
		for (pm_i = pm_head->next; pm_i != NULL; pm_i = pm_i->next)
			if (! strcmp(pm_i->diskdev, buf)) {
				ok = 0;
				break;
			}
		if (ok) {
			*(int*)node = i;
			return i;
		}
	}
	msg_prompt_win("Cannot allocate device node!", -1, 18, 0, 0, NULL, NULL, 0);
	return -1;
}

/*** RAIDs ***/

static void
partman_raid_menufmt(menudesc *m, int opt, void *arg)
{
	int i, ok = 0;
	char buf[STRSIZE]; buf[0] = '\0';
	raids_t *dev_ptr = ((part_entry_t *)arg)[opt].dev_ptr;

	if (dev_ptr->enabled == 0)
		return;
	for (i = 0; i < MAX_IN_RAID; i++)
		if (dev_ptr->pm[i] != NULL) {
			strncat(buf, dev_ptr->pm_name[i], STRSIZE);
			strncat(buf, " ", STRSIZE);
			ok = 1;
		}
	if (ok)
		wprintw(m->mw, "   raid%d (level %1d) on %-34s %11uM", dev_ptr->node,
			dev_ptr->raid_level, buf, dev_ptr->total_size);
	else
		wprintw(m->mw, "   EMPTY RAID!");
	return;
}

static void
partman_raid_edit_menufmt(menudesc *m, int opt, void *arg)
{
	int i;
	char buf[STRSIZE]; buf[0] = '\0';

	switch (opt){
		case PMR_MENU_DEVS:
			for (i = 0; i < MAX_IN_RAID; i++)
				if ( ((raids_t*)arg)->pm[i] != NULL && ((raids_t*)arg)->pm_is_spare[i] == 0)
					snprintf(buf, STRSIZE, "%s %s", buf, ((raids_t*)arg)->pm_name[i]);
			wprintw(m->mw, "Disks: %33s", buf);
			break;
		case PMR_MENU_DEVSSPARE:
			for (i = 0; i < MAX_IN_RAID; i++)
				if (((raids_t*)arg)->pm[i] != NULL && ((raids_t*)arg)->pm_is_spare[i] != 0)
					snprintf(buf, STRSIZE, "%s %s", buf, ((raids_t*)arg)->pm_name[i]);
			wprintw(m->mw, "Spares: %32s", buf);
			break;
		case PMR_MENU_RAIDLEVEL:
			wprintw(m->mw, "RAID level:       %22d", ((raids_t*)arg)->raid_level);
			break;
		case PMR_MENU_NUMROW:
			wprintw(m->mw, "numRow:           %22d", ((raids_t*)arg)->numRow);
			break;
		case PMR_MENU_NUMCOL:
			wprintw(m->mw, "numCol:           %22d", ((raids_t*)arg)->numCol);
			break;
		case PMR_MENU_NUMSPARE:
			wprintw(m->mw, "numSpare:         %22d", ((raids_t*)arg)->numSpare);
			break;
		case PMR_MENU_SECTPERSU:
			wprintw(m->mw, "sectPerSU:        %22d", ((raids_t*)arg)->sectPerSU);
			break;
		case PMR_MENU_SUSPERPARITYUNIT:
			wprintw(m->mw, "SUsPerParityUnit: %22d", ((raids_t*)arg)->SUsPerParityUnit);
			break;
		case PMR_MENU_SUSPERRECONUNIT:
			wprintw(m->mw, "SUsPerReconUnit:  %22d", ((raids_t*)arg)->SUsPerReconUnit);
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
			process_menu(menu_disk_adddel, arg);
			return 0;
		case PMR_MENU_DEVSSPARE:
			manage_raid_spare_cur = 1;
			process_menu(menu_disk_adddel, arg);
			return 0;
		case PMR_MENU_RAIDLEVEL:
			process_menu(MENU_raidlevel, &retvalue);
			if (retvalue >= 0)
				((raids_t*)arg)->raid_level = retvalue;
			return 0;
		case PMR_MENU_NUMROW:
			msg_prompt_win("Multi-dimensional arrays are NOT supported!",
							-1, 18, 0, 0, NULL, NULL, 0);
			return 0;
			msg_to_show = "numRow?";
			out_var = &(((raids_t*)arg)->numRow);
			break;
		case PMR_MENU_NUMCOL:
			msg_to_show = "numCol?";
			out_var = &(((raids_t*)arg)->numCol);
			break;
		case PMR_MENU_NUMSPARE:
			msg_to_show = "numSpare?";
			out_var = &(((raids_t*)arg)->numSpare);
			break;
		case PMR_MENU_SECTPERSU:
			msg_to_show = "sectPerSU?";
			out_var = &(((raids_t*)arg)->sectPerSU);
			break;
		case PMR_MENU_SUSPERPARITYUNIT:
			msg_to_show = "SUsPerParityUnit?";
			out_var = &(((raids_t*)arg)->SUsPerParityUnit);
			break;
		case PMR_MENU_SUSPERRECONUNIT:
			msg_to_show = "SUsPerReconUnit?";
			out_var = &(((raids_t*)arg)->SUsPerReconUnit);
			break;
		case PMR_MENU_REMOVE:
			((raids_t*)arg)->enabled = 0;
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
partman_raid_new_init(void *dev_ptr, void *none)
{
	memset((raids_t*)dev_ptr, 0, sizeof *((raids_t*)dev_ptr));
	*((raids_t*)dev_ptr) = (struct raids_t) {
		.enabled = 1,
		.blocked = 0,
		.sectPerSU = 32,
		.SUsPerParityUnit = 1,
		.SUsPerReconUnit = 1,
	};
	return;
}

static int
partman_raid_check(void *dev_ptr)
{
	int i, dev_num = 0, min_size = 0, cur_size = 0;
	if (((raids_t*)dev_ptr)->blocked)
		return 0;
	for (i = 0; i < MAX_IN_RAID; i++)
		if (((raids_t*)dev_ptr)->pm[i] != NULL) {
			cur_size = ((raids_t*)dev_ptr)->pm[i]->bsdlabel[((raids_t*)dev_ptr)->pm_part[i]].pi_size /
						(MEG / ((raids_t*)dev_ptr)->pm[i]->sectorsize);
			if (cur_size < min_size || dev_num == 0)
				min_size = cur_size;
			if (!((raids_t*)dev_ptr)->pm_is_spare[i])
				dev_num++;
		}
	if (dev_num > 0) {
		switch (((raids_t*)dev_ptr)->raid_level) {
			case 0:
				((raids_t*)dev_ptr)->total_size = min_size * dev_num;
				break;
			case 1:
				((raids_t*)dev_ptr)->total_size = min_size;
				break;
			case 4:
			case 5:
				((raids_t*)dev_ptr)->total_size = min_size * (dev_num - 1);
				break;
		}
		partman_manage_getfreenode(&(((raids_t*)dev_ptr)->node), "raid", &raids_t_info);
		if (((raids_t*)dev_ptr)->node < 0)
			((raids_t*)dev_ptr)->enabled = 0;
	}
	else
		((raids_t*)dev_ptr)->enabled = 0;
	return ((raids_t*)dev_ptr)->enabled;
}

static int
partman_raid_disk_add(menudesc *m, void *arg)
{
	int i;
	part_entry_t disk_entrie = partman_manage_getdev(PM_RAID_T);
	if (disk_entrie.retvalue < 0)
		return disk_entrie.retvalue;

	for (i = 0; i < MAX_IN_RAID; i++)
		if (((raids_t*)arg)->pm[i] == NULL) {
			((raids_t*)arg)->pm[i] = disk_entrie.dev_pm;
			((raids_t*)arg)->pm_part[i] = disk_entrie.dev_num;
			((raids_t*)arg)->pm_is_spare[i] = manage_raid_spare_cur;
			strncpy(((raids_t*)arg)->pm_name[i], disk_entrie.fullname, SSTRSIZE);
			if (manage_raid_spare_cur)
				((raids_t*)arg)->numSpare++;
			else
				((raids_t*)arg)->numCol++;
			((raids_t*)arg)->numRow = 1;
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
		if (((raids_t*)arg)->pm[i] == NULL ||
			((raids_t*)arg)->pm_is_spare[i] != manage_raid_spare_cur)
			continue;
		menu_entries[num_devs] = (struct menu_ent) {
			.opt_name = ((raids_t*)arg)->pm_name[i],
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

	if (((raids_t*)arg)->pm_is_spare[pm_cur])
		((raids_t*)arg)->numSpare--;
	else
		((raids_t*)arg)->numCol--;
	((raids_t*)arg)->numRow = (((raids_t*)arg)->numCol)?1:0;
	((raids_t*)arg)->pm[pm_cur] = NULL;

	return 0;
}

static int
partman_raid_edit(menudesc *m, void *arg)
{
	((part_entry_t *)arg)[0].retvalue = m->cursel + 1;

	return partman_manage_edit(PMR_MENU_END, partman_raid_edit_menufmt,
		partman_raid_set_value,	partman_raid_check, partman_raid_new_init,
		NULL, ((part_entry_t *)arg)[m->cursel].dev_ptr, &raids_t_info);
}

static int
partman_raid_commit(void)
{
	int i, ii;
	FILE *f;
	char f_name[STRSIZE];

	for (i = 0; i < MAX_RAID; i++) {
		if (! partman_raid_check(&raids[i]))
			continue;

		/* Generating configure file for our raid */
		snprintf(f_name, SSTRSIZE, "/tmp/raid.%d.conf", raids[i].node);
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
			run_program(RUN_DISPLAY | RUN_PROGRESS, "raidctl -C %s raid%d",
							f_name, raids[i].node) == 0 &&
			run_program(RUN_DISPLAY | RUN_PROGRESS, "raidctl -I %d raid%d",
							rand(), raids[i].node) == 0 &&
			run_program(RUN_DISPLAY | RUN_PROGRESS, "raidctl -vi raid%d",
							raids[i].node) == 0 &&
			run_program(RUN_DISPLAY | RUN_PROGRESS, "raidctl -v -A yes raid%d",
							raids[i].node) == 0
			) {
			raids[i].blocked = 1; /* RAID creation done, remove it from list to 
									 prevent it's repeated reinitialization */
			for (ii = 0; ii < MAX_IN_RAID; ii++)
				if (raids[i].pm[ii] != NULL)
					raids[i].pm[ii]->blocked++;
		}
	}
	return 0;
}

/*** VND ***/

static void
partman_vnd_menufmt(menudesc *m, int opt, void *arg)
{
	vnds_t *dev_ptr = ((part_entry_t *)arg)[opt].dev_ptr;

	if (dev_ptr->enabled == 0)
		return;
	if (strlen(dev_ptr->filepath) < 1)
		wprintw(m->mw, "   PATH NOT DEFINED!");
	else if (dev_ptr->is_exist)
		wprintw(m->mw, "   vnd%1d on %-51s ASSIGN", dev_ptr->node, dev_ptr->filepath);
	else 
		wprintw(m->mw, "   vnd%1d on %-45s %11uM", dev_ptr->node, dev_ptr->filepath, dev_ptr->size);
	return;
}

static void
partman_vnd_edit_menufmt(menudesc *m, int opt, void *arg)
{
	vnds_t *dev_ptr = arg;
	char buf[SSTRSIZE];
	strcpy(buf, "-");

	switch (opt){
		case PMV_MENU_FILEPATH:
			wprintw(m->mw, "File path: %29s", dev_ptr->filepath);
			break;
		case PMV_MENU_EXIST:
			wprintw(m->mw, "Assign exist image: %20s", dev_ptr->is_exist?"yes":"no");
			break;
		case PMV_MENU_SIZE:
			if (!dev_ptr->is_exist)
				snprintf(buf, SSTRSIZE, "%d", dev_ptr->size);
			wprintw(m->mw, "Size (MB):        %22s", buf);
			break;
		case PMV_MENU_RO:
			wprintw(m->mw, "Read-only:        %22s", dev_ptr->readonly?"yes":"no");
			break;
		case PMV_MENU_MGEOM:
			if (!dev_ptr->is_exist)
				snprintf(buf, SSTRSIZE, "%s", dev_ptr->manual_geom?"yes":"no");
			wprintw(m->mw, "Set geometry by hand: %18s", buf);
			break;
		case PMV_MENU_SECSIZE:
			if (dev_ptr->manual_geom && !dev_ptr->is_exist)
				snprintf(buf, SSTRSIZE, "%d", dev_ptr->secsize);
			wprintw(m->mw, "Bytes per Sector:     %18s", buf);
			break;
		case PMV_MENU_NSECTORS:
			if (dev_ptr->manual_geom && !dev_ptr->is_exist)
				snprintf(buf, SSTRSIZE, "%d", dev_ptr->nsectors);
			wprintw(m->mw, "Sectors per Track:    %18s", buf);
			break;
		case PMV_MENU_NTRACKS:
			if (dev_ptr->manual_geom && !dev_ptr->is_exist)
				snprintf(buf, SSTRSIZE, "%d", dev_ptr->ntracks);
			wprintw(m->mw, "Tracks per Cylinder:  %18s", buf);
			break;
		case PMV_MENU_NCYLINDERS:
			if (dev_ptr->manual_geom && !dev_ptr->is_exist)
				snprintf(buf, SSTRSIZE, "%d", dev_ptr->ncylinders);
			wprintw(m->mw, "Cylinders:        %22s", buf);
			break;
	}
	return;
}

static int
partman_vnd_set_value(menudesc *m, void *arg)
{
	vnds_t *dev_ptr = arg;
	char buf[STRSIZE];
	const char *msg_to_show = NULL;
	int *out_var = NULL;
	
	switch (m->cursel) {
		case PMV_MENU_FILEPATH:
			msg_prompt_win("File path?", -1, 18, 0, 0, dev_ptr->filepath,
				dev_ptr->filepath, STRSIZE);
			if (dev_ptr->filepath[0] != '/') {
				strncpy(buf, dev_ptr->filepath, MOUNTLEN);
				snprintf(dev_ptr->filepath, MOUNTLEN, "/%s", buf);
			}
			if (dev_ptr->filepath[strlen(dev_ptr->filepath) - 1] == '/')
				dev_ptr->filepath[strlen(dev_ptr->filepath) - 1] = '\0';
			return 0;
		case PMV_MENU_EXIST:
			dev_ptr->is_exist = !dev_ptr->is_exist;
			return 0;
		case PMV_MENU_SIZE:
			if (dev_ptr->is_exist)
				return 0;
			msg_to_show = "Size (MB)";
			out_var = &(dev_ptr->size);
			break;
		case PMV_MENU_RO:
			dev_ptr->readonly = !dev_ptr->readonly;
			return 0;
		case PMV_MENU_MGEOM:
			if (dev_ptr->is_exist)
				return 0;
			dev_ptr->manual_geom = !dev_ptr->manual_geom;
			return 0;
		case PMV_MENU_SECSIZE:
			if (!dev_ptr->manual_geom || dev_ptr->is_exist)
				return 0;
			msg_to_show = "Bytes per Sector";
			out_var = &(dev_ptr->secsize);
			break;
		case PMV_MENU_NSECTORS:
			if (!dev_ptr->manual_geom || dev_ptr->is_exist)
				return 0;
			msg_to_show = "Sectors per Track";
			out_var = &(dev_ptr->nsectors);
			break;
		case PMV_MENU_NTRACKS:
			if (!dev_ptr->manual_geom || dev_ptr->is_exist)
				return 0;
			msg_to_show = "Tracks per Cylinder";
			out_var = &(dev_ptr->ntracks);
			break;
		case PMV_MENU_NCYLINDERS:
			if (!dev_ptr->manual_geom || dev_ptr->is_exist)
				return 0;
			msg_to_show = "Cylinders";
			out_var = &(dev_ptr->ncylinders);
			break;
		case PMV_MENU_REMOVE:
			dev_ptr->enabled = 0;
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
partman_vnd_new_init(void *dev_ptr, void *none)
{
	memset(((vnds_t*)dev_ptr), 0, sizeof *((vnds_t*)dev_ptr));
	*((vnds_t*)dev_ptr) = (struct vnds_t) {
		.enabled = 1,
		.blocked = 0,
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
partman_vnd_check(void *dev_ptr)
{
	if (((vnds_t*)dev_ptr)->blocked)
		return 0;
	if (strlen(((vnds_t*)dev_ptr)->filepath) < 1 ||
			((vnds_t*)dev_ptr)->size < 1)
		((vnds_t*)dev_ptr)->enabled = 0;
	else {
		partman_manage_getfreenode(&(((vnds_t*)dev_ptr)->node), "vnd", &vnds_t_info);
		if (((vnds_t*)dev_ptr)->node < 0)
			((vnds_t*)dev_ptr)->enabled = 0;
	}
	return ((vnds_t*)dev_ptr)->enabled;
}

static int
partman_vnd_edit(menudesc *m, void *arg)
{
	((part_entry_t *)arg)[0].retvalue = m->cursel + 1;

	return partman_manage_edit(PMV_MENU_END, partman_vnd_edit_menufmt,
		partman_vnd_set_value, partman_vnd_check, partman_vnd_new_init,
		NULL, ((part_entry_t *)arg)[m->cursel].dev_ptr, &vnds_t_info);
}

/* TODO: vnconfig always return 0? */
static int
partman_vnd_commit(void)
{
	int i, ii, error, part_suit;
	char r_o[3], buf[MOUNTLEN+3], resultpath[STRSIZE]; 
	pm_devs_t *pm_i, *pm_suit = NULL;

	for (i = 0; i < MAX_VND; i++) {
		error = 0;
		if (! partman_vnd_check(&vnds[i]))
			continue;

		/* Trying to assign target device */
		for (pm_i = pm_head->next; pm_i != NULL; pm_i = pm_i->next)
			for (ii = 0; ii < 6; ii++) {
				strcpy(buf, pm_i->bsdlabel[ii].pi_mount);
				if (buf[strlen(buf)-1] != '/')
					sprintf(buf,"%s/", buf);
				printf("%s\n",buf);
				if (strstr(vnds[i].filepath, buf) == vnds[i].filepath)
					if (part_suit < 0 || pm_suit == NULL ||
						strlen(buf) > strlen(pm_suit->bsdlabel[part_suit].pi_mount)) {
						part_suit = ii;
						pm_suit = pm_i;
					}
			}
		if (part_suit < 0 || pm_suit == NULL || 
			pm_suit->bsdlabel[part_suit].mnt_opts == NULL ||
			! (pm_suit->bsdlabel[part_suit].pi_flags & PIF_MOUNT))
			continue;
		
		/* Mounting assigned partition and try to get real file path*/
		if (partman_mount(pm_suit, part_suit) != 0)
			continue;
		snprintf(resultpath, STRSIZE, "%s/%s",
			pm_suit->bsdlabel[part_suit].mounted,
			&(vnds[i].filepath[strlen(pm_suit->bsdlabel[part_suit].pi_mount)]));

		strcpy(r_o, vnds[i].readonly?"-r":"");
		/* If this is a new image */
		if (!vnds[i].is_exist) {
			run_program(RUN_DISPLAY | RUN_PROGRESS, "mkdir -p %s ", dirname(resultpath));
			error += run_program(RUN_DISPLAY | RUN_PROGRESS,
						"dd if=/dev/zero of=%s bs=1m count=%d progress=100 msgfmt=human",
						resultpath, vnds[i].size);
		}
		if (error)
			continue;
		/* If this is a new image with manual geometry */
		if (!vnds[i].is_exist && vnds[i].manual_geom)
			error += run_program(RUN_DISPLAY | RUN_PROGRESS,
						"vnconfig %s vnd%d %s %d %d %d %d", r_o, vnds[i].node,
						resultpath, vnds[i].secsize, vnds[i].nsectors,
						vnds[i].ntracks, vnds[i].ncylinders);
		/* If this is a existent image or image without manual geometry */
		else
			error += run_program(RUN_DISPLAY | RUN_PROGRESS, "vnconfig %s vnd%d %s",
						r_o, vnds[i].node, resultpath);

		if (! error) {
			vnds[i].blocked = 1;
			vnds[i].pm_part = part_suit;
			vnds[i].pm = pm_suit;
			vnds[i].pm->blocked++;
		}
	}
	return 0;
}

/*** CGD ***/

static void
partman_cgd_menufmt(menudesc *m, int opt, void *arg)
{
	cgds_t *dev_ptr = ((part_entry_t *)arg)[opt].dev_ptr;
	char desc[STRSIZE];

	if (dev_ptr->enabled == 0)
		return;
	if (dev_ptr->pm == NULL)
		wprintw(m->mw, "   DISK NOT DEFINED!");
	else {
		snprintf(desc, STRSIZE, "(%s-%d) on %s", dev_ptr->enc_type,
			dev_ptr->key_size, dev_ptr->pm_name);
		wprintw(m->mw, "   cgd%1d %-48s %11uM", dev_ptr->node, desc,
			dev_ptr->pm->bsdlabel[dev_ptr->pm_part].pi_size /
							(MEG / dev_ptr->pm->sectorsize));
	}
	return;
}

static void
partman_cgd_edit_menufmt(menudesc *m, int opt, void *arg)
{
	cgds_t *dev_ptr = arg;
	switch (opt){
		case PMC_MENU_DEV:
			wprintw(m->mw, "Base device:    %24s", dev_ptr->pm_name);
			break;
		case PMC_MENU_ENCTYPE:
			wprintw(m->mw, "Encyption:      %24s", dev_ptr->enc_type);
			break;
		case PMC_MENU_KEYSIZE:
			wprintw(m->mw, "Key size:       %24d", dev_ptr->key_size);
			break;
		case PMC_MENU_IVTYPE:
			wprintw(m->mw, "IV algorithm:   %24s", dev_ptr->iv_type);
			break;
		case PMC_MENU_KEYGENTYPE:
			wprintw(m->mw, "Key generation: %24s", dev_ptr->keygen_type);
			break;
		case PMC_MENU_VERIFYTYPE:
			wprintw(m->mw, "Verification method:   %17s", dev_ptr->verify_type);
			break;
	}
	return;
}

static int
partman_cgd_set_value(menudesc *m, void *arg)
{
	char *retstring;
	cgds_t *dev_ptr = arg;

	switch (m->cursel) {
		case PMC_MENU_DEV:
			partman_cgd_disk_set(dev_ptr, NULL);
			return 0;
		case PMC_MENU_ENCTYPE:
			process_menu(MENU_cgd_enctype, &retstring);
			dev_ptr->enc_type = retstring;
			if (! strcmp(retstring, "blowfish-cbc"))
				dev_ptr->key_size = 128;
			if (! strcmp(retstring, "3des-cbc"))
				dev_ptr->key_size = 192;
			return 0;
		case PMC_MENU_KEYSIZE:
			if (! strcmp(dev_ptr->enc_type, "aes-cbc"))
				dev_ptr->key_size +=
					(dev_ptr->key_size < 256)? 64 : -128;
			if (! strcmp(dev_ptr->enc_type, "blowfish-cbc"))
				dev_ptr->key_size = 128;
			if (! strcmp(dev_ptr->enc_type, "3des-cbc"))
				dev_ptr->key_size = 192;
			return 0;
		case PMC_MENU_IVTYPE:
			process_menu(MENU_cgd_ivtype, &retstring);
			dev_ptr->iv_type = retstring;
			return 0;
		case PMC_MENU_KEYGENTYPE:
			process_menu(MENU_cgd_keygentype, &retstring);
			dev_ptr->keygen_type = retstring;
			return 0;
		case PMC_MENU_VERIFYTYPE:
			process_menu(MENU_cgd_verifytype, &retstring);
			dev_ptr->verify_type = retstring;
			return 0;
		case PMC_MENU_REMOVE:
			dev_ptr->enabled = 0;
			return 0;
	}
	return -1;
}

static void
partman_cgd_new_init(void *dev_ptr, void *assign_pm)
{
	part_entry_t disk_entrie;

	memset((cgds_t*)dev_ptr, 0, sizeof *((cgds_t*)dev_ptr));
	*((cgds_t*)dev_ptr) = (struct cgds_t) {
		.enabled = 1,
		.blocked = 0,
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
			disk_entrie.dev_num = PART_E;
			snprintf(disk_entrie.fullname, SSTRSIZE, "%s%c",
				pm->diskdev, 'a' + PART_E);
		partman_cgd_disk_set(dev_ptr, &disk_entrie);
	}
	return;
}

static int
partman_cgd_check(void *dev_ptr)
{
	if (((cgds_t*)dev_ptr)->blocked)
		return 0;
	if (((cgds_t*)dev_ptr)->pm == NULL)
		((cgds_t*)dev_ptr)->enabled = 0;
	else
		partman_manage_getfreenode(&(((cgds_t*)dev_ptr)->node), "cgd", &cgds_t_info);
		if (((cgds_t*)dev_ptr)->node < 0)
			((cgds_t*)dev_ptr)->enabled = 0;
	return ((cgds_t*)dev_ptr)->enabled;
}

static int
partman_cgd_disk_set(cgds_t *dev_ptr, part_entry_t *disk_entrie)
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
	dev_ptr->pm = disk_entrie->dev_pm;
	dev_ptr->pm_part = disk_entrie->dev_num;
	strncpy(dev_ptr->pm_name, disk_entrie->fullname, SSTRSIZE);

	if (alloc_disk_entrie)
		free(disk_entrie);
	return 0;
}

int
partman_cgd_edit_adddisk(void *dev_ptr, pm_devs_t *assign_pm)
{
	if (assign_pm != NULL)
		dev_ptr = NULL;
	return partman_manage_edit(PMC_MENU_END, partman_cgd_edit_menufmt,
		partman_cgd_set_value, partman_cgd_check, partman_cgd_new_init,
		assign_pm, dev_ptr, &cgds_t_info);
}

static int
partman_cgd_edit(menudesc *m, void *arg)
{
	((part_entry_t *)arg)[0].retvalue = m->cursel + 1;
	return partman_cgd_edit_adddisk(((part_entry_t *)arg)[m->cursel].dev_ptr, NULL);
}

static int
partman_cgd_commit(void)
{
	int i, error = 0;
	for (i = 0; i < MAX_CGD; i++) {
		if (! partman_cgd_check(&cgds[i]))
			continue;
		if (run_program(RUN_DISPLAY | RUN_PROGRESS,
			"cgdconfig -g -i %s -k %s -o /tmp/cgd.%d.conf %s %d",
			cgds[i].iv_type, cgds[i].keygen_type, cgds[i].node,
			cgds[i].enc_type, cgds[i].key_size) != 0) {
			error++;
			continue;
		}
		if (run_program(RUN_DISPLAY | RUN_PROGRESS,
			"cgdconfig -V re-enter cgd%d /dev/%s /tmp/cgd.%d.conf",
			cgds[i].node, cgds[i].pm_name, cgds[i].node) != 0) {
			error++;
			continue;
		}
		cgds[i].pm->blocked++;
		cgds[i].blocked = 1;
	}
	return error;
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
		if (((lvms_t*)arg)->pv[i].pm == NULL) {
			((lvms_t*)arg)->pv[i].pm = disk_entries.dev_pm;
			((lvms_t*)arg)->pv[i].pm_part = disk_entries.dev_num;
			strncpy(((lvms_t*)arg)->pv[i].pm_name, disk_entries.fullname, SSTRSIZE);
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
		if (((lvms_t*)arg)->pv[i].pm == NULL)
			continue;
		menu_entries[num_devs] = (struct menu_ent) {
			.opt_name = ((lvms_t*)arg)->pv[i].pm_name,
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

	((lvms_t*)arg)->pv[submenu_args[retvalue].dev_num].pm = NULL;

	return 0;
}

static void
partman_lvm_menufmt(menudesc *m, int opt, void *arg)
{
	int i, ok = 0;
	char buf[STRSIZE]; buf[0] = '\0';
	lvms_t *dev_ptr = ((part_entry_t *)arg)[opt].dev_ptr;

	if (dev_ptr->enabled == 0)
		return;
	snprintf(buf, STRSIZE, "VG '%s' on ", dev_ptr->name);
	for (i = 0; i < MAX_LVM_PV; i++)
		if (dev_ptr->pv[i].pm != NULL) {
			strncat(buf, dev_ptr->pv[i].pm_name, STRSIZE);
			strncat(buf, " ", STRSIZE);
			ok = 1;
		}
	if (ok)
		wprintw(m->mw, "   %-53s %11uM", buf,
			dev_ptr->total_size);
	else
		wprintw(m->mw, "   EMPTY VG!");
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
				if (((lvms_t*)arg)->pv[i].pm != NULL)
					snprintf(buf, STRSIZE, "%s %s", buf, ((lvms_t*)arg)->pv[i].pm_name);
			wprintw(m->mw, "PV's: %34s", buf);
			break;
		case PML_MENU_NAME:
			wprintw(m->mw, "Name: %34s", ((lvms_t*)arg)->name);
			break;
		case PML_MENU_MAXLOGICALVOLUMES:
			wprintw(m->mw, "MaxLogicalVolumes:  %20d", ((lvms_t*)arg)->maxlogicalvolumes);
			break;
		case PML_MENU_MAXPHYSICALVOLUMES:
			wprintw(m->mw, "MaxPhysicalVolumes: %20d", ((lvms_t*)arg)->maxphysicalvolumes);
			break;
		case PML_MENU_PHYSICALEXTENTSIZE:
			wprintw(m->mw, "PhysicalExtentSize: %18dMB", ((lvms_t*)arg)->physicalextentsize);
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
			process_menu(menu_disk_adddel, arg);
			return 0;
		case PML_MENU_NAME:
			msg_prompt_win("Name?", -1, 18, 0, 0, ((lvms_t*)arg)->name,
				((lvms_t*)arg)->name, SSTRSIZE);
			return 0;
		case PML_MENU_REMOVE:
			((lvms_t*)arg)->enabled = 0;
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
partman_lvm_new_init(void *dev_ptr, void* none)
{
	memset((lvms_t*)dev_ptr, 0, sizeof *((lvms_t*)dev_ptr));
	*((lvms_t*)dev_ptr) = (struct lvms_t) {
		.enabled = 1,
		.maxlogicalvolumes = MAX_LVM_PV,
		.maxphysicalvolumes = MAX_LVM_LV,
		.physicalextentsize = 4,
	};
	sprintf(((lvms_t*)dev_ptr)->name, "vg%.2d", rand()%100);
	return;
}

static int
partman_lvm_check(void *dev_ptr)
{
	int i, ok = 0;
	((lvms_t*)dev_ptr)->total_size = 0;

	for (i = 0; i < MAX_LVM_PV; i++)
		if (((lvms_t*)dev_ptr)->pv[i].pm != NULL) {
			ok = 1;
			((lvms_t*)dev_ptr)->total_size += ((lvms_t*)dev_ptr)->pv[i].pm->
						bsdlabel[((lvms_t*)dev_ptr)->pv[i].pm_part].pi_size /
						(MEG / ((lvms_t*)dev_ptr)->pv[i].pm->sectorsize);
		}
	if (! ok)
		((lvms_t*)dev_ptr)->enabled = 0;
	return ((lvms_t*)dev_ptr)->enabled;
}

static int
partman_lvm_edit(menudesc *m, void *arg)
{
	((part_entry_t *)arg)[0].retvalue = m->cursel + 1;

	return partman_manage_edit(PML_MENU_END, partman_lvm_edit_menufmt,
		partman_lvm_set_value, partman_lvm_check, partman_lvm_new_init,
		NULL, ((part_entry_t *)arg)[m->cursel].dev_ptr, &lvms_t_info);
}

static void
partman_lvmlv_menufmt(menudesc *m, int opt, void *arg)
{
	lv_t *dev_ptr = ((part_entry_t *)arg)[opt].dev_ptr;

	if (dev_ptr->size > 0)
		wprintw(m->mw, "      Logical volume %-35s %11uM", dev_ptr->name,
			dev_ptr->size);
	
	return;
}

static void
partman_lvmlv_edit_menufmt(menudesc *m, int opt, void *arg)
{
	switch (opt){
		case PMLV_MENU_NAME:
			wprintw(m->mw, "Name: %34s", ((lv_t*)arg)->name);
			break;
		case PMLV_MENU_SIZE:
			wprintw(m->mw, "Size: %34d", ((lv_t*)arg)->size);
			break;
		case PMLV_MENU_READONLY:
			wprintw(m->mw, "Read-only:   %27s", ((lv_t*)arg)->readonly?"yes":"no");
			break;
		case PMLV_MENU_CONTIGUOUS:
			wprintw(m->mw, "Contiguous:  %27s", ((lv_t*)arg)->contiguous?"yes":"no");
			break;
		case PMLV_MENU_CHUNKSIZE:
			wprintw(m->mw, "ChunkSize:   %27d", ((lv_t*)arg)->chunksize);
			break;
		case PMLV_MENU_EXTENTS:
			wprintw(m->mw, "LogicalExtentsNumber: %18s", ((lv_t*)arg)->extents);
			break;
		case PMLV_MENU_MINOR:
			wprintw(m->mw, "Minor number: %26d", ((lv_t*)arg)->minor);
			break;
		case PMLV_MENU_MIRRORS:
			wprintw(m->mw, "Mirrors:     %27d", ((lv_t*)arg)->mirrors);
			break;
		case PMLV_MENU_REGIONSIZE:
			wprintw(m->mw, "MirrorLogRegionSize:  %18d", ((lv_t*)arg)->regionsize);
			break;
		case PMLV_MENU_PERSISTENT:
			wprintw(m->mw, "Persistent:  %27s", ((lv_t*)arg)->persistent?"yes":"no");
			break;
		case PMLV_MENU_READAHEAD:
			wprintw(m->mw, "ReadAheadSectors:     %18d", ((lv_t*)arg)->readahead);
			break;
		case PMLV_MENU_STRIPES:
			wprintw(m->mw, "Stripes:     %27d", ((lv_t*)arg)->stripes);
			break;
		case PMLV_MENU_STRIPESIZE:
			wprintw(m->mw, "StripeSize:  %27d", ((lv_t*)arg)->stripesize);
			break;
		case PMLV_MENU_ZERO:
			wprintw(m->mw, "Zero: %34s", ((lv_t*)arg)->zero?"yes":"no");
			break;
	}
	return;
}

static int
partman_lvmlv_set_value(menudesc *m, void *arg)
{
	switch (m->cursel) {
		case PMLV_MENU_REMOVE:
			((lv_t*)arg)->size = 0;
			return 0;
			break;
	}
	return 0;
}

static void
partman_lvmlv_new_init(void *dev_ptr, void *none)
{
	memset((lv_t*)dev_ptr, 0, sizeof *((lv_t*)dev_ptr));
	*((lv_t*)dev_ptr) = (struct lv_t) {
		.size = 1024,
	};
	sprintf(((lvms_t*)dev_ptr)->name, "lvol%.2d", rand()%100);
	return;
}

static int
partman_lvmlv_check(void *dev_ptr)
{
	if (((lv_t*)dev_ptr)->size > 0 &&
		strlen(((lv_t*)dev_ptr)->name) > 0)
		return 1;
	else {
		((lv_t*)dev_ptr)->size = 0;
		return 0;
	}
}

static int
partman_lvmlv_edit(menudesc *m, void *arg)
{
	((part_entry_t *)arg)[0].retvalue = m->cursel + 1;

	return partman_manage_edit(PMLV_MENU_END, partman_lvmlv_edit_menufmt,
		partman_lvmlv_set_value, partman_lvmlv_check, partman_lvmlv_new_init,
		NULL, ((part_entry_t *)arg)[m->cursel].dev_ptr, 
		&((structinfo_t) { .max = MAX_LVM_LV, .entry_first = &lvms[0].lv[0],
		.entry_enabled = &(lvms[0].lv[0].size), .entry_size = sizeof lvms[0].lv[0], }));
}

static int
partman_lvm_commit(void)
{
	int i, ii, error;
	for (i = 0; i < MAX_LVM_VG; i++) {
		if (! partman_lvm_check(&lvms[i]))
			continue;
		error = 0;
		for (ii = 0; ii < MAX_LVM_PV && ! error; ii++)
			if (lvms[i].pv[ii].pm != NULL) {
				error += run_program(RUN_DISPLAY | RUN_PROGRESS,
										"lvcreate pvcreate -y /dev/%s",
										(char*)lvms[i].pv[ii].pm_name);
				error += run_program(RUN_DISPLAY | RUN_PROGRESS,
										"lvcreate vgcreate %s /dev/%s",
										lvms[i].name, (char*)lvms[i].pv[ii].pm_name);
			}
		for (ii = 0; ii < MAX_LVM_LV && ! error; ii++);

		lvms[i].enabled = 0;
	}

	return 0;
}

/*** Partman generic functions ***/

int
partman_getrefdev(pm_devs_t *pm_cur)
{
	int i, ii, dev_num, num_devs, num_devs_s;
	pm_cur->refdev = -1;
	if (! strncmp(pm_cur->diskdev, "cgd", 3)) {
		dev_num = pm_cur->diskdev[3] - '0';
		for (i = 0; i < MAX_CGD; i++)
			if (cgds[i].blocked && cgds[i].node == dev_num) {
				pm_cur->refdev = i;
				snprintf(pm_cur->diskdev_descr, STRSIZE, "%s (%s%c, %s-%d)",
					pm_cur->diskdev_descr, cgds[i].pm->diskdev,
					cgds[i].pm_part + 'a', cgds[i].enc_type, cgds[i].key_size);
				break;
			}
 	} else if (! strncmp(pm_cur->diskdev, "vnd", 3)) {
 		dev_num = pm_cur->diskdev[3] - '0';
 		for (i = 0; i < MAX_VND; i++)
			if (vnds[i].blocked && vnds[i].node == dev_num) {
				pm_cur->refdev = i;
				snprintf(pm_cur->diskdev_descr, STRSIZE, "%s (%s%c, %s)",
					pm_cur->diskdev_descr, vnds[i].pm->diskdev,
					vnds[i].pm_part + 'a', vnds[i].filepath);
				break;
			}
	} else if (! strncmp(pm_cur->diskdev, "raid", 4)) {
		dev_num = pm_cur->diskdev[4] - '0';
 		for (i = 0; i < MAX_RAID; i++)
			if (raids[i].blocked && raids[i].node == dev_num) {
				pm_cur->refdev = i;
				num_devs = 0; num_devs_s = 0;
				for (ii = 0; ii < MAX_IN_RAID; ii++)
					if (raids[i].pm[ii] != NULL) {
						if(! raids[i].pm_is_spare[ii])
							num_devs++;
						else
							num_devs_s++;
					}
				snprintf(pm_cur->diskdev_descr, STRSIZE,
					"%s (%d disks, %d spare, lvl %d)",
					pm_cur->diskdev_descr, num_devs, num_devs_s,
					raids[i].raid_level);
				break;
			}
	}
	return pm_cur->refdev;
}

static void
partman_select(pm_devs_t *pm_devs_in)
{
	pm = pm_devs_in;
	if (logfp)
		(void)fprintf(logfp,"Partman device: %s\n", pm->diskdev);
	return;

}

void
partman_rename(pm_devs_t *pm_cur)
{
	partman_select(pm_cur);
	msg_prompt_win(MSG_packname, -1, 18,	0, 0, pm_cur->bsddiskname,
		pm_cur->bsddiskname, sizeof pm_cur->bsddiskname);
	(void) savenewlabel(pm_cur->bsdlabel, MAXPARTITIONS);
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
					strlcpy(diskdev_with_root, pm_i->diskdev, sizeof diskdev_with_root);
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

/* Mount partition bypassing ordinary order */
static int
partman_mount(pm_devs_t *pm_cur, int part_num)
{
	int error = 0;
	char buf[MOUNTLEN];

	if (strlen(pm_cur->bsdlabel[part_num].mounted) > 0)
		return 0;

	snprintf(buf, MOUNTLEN, "/tmp/%s%c", pm_cur->diskdev, part_num + 'a');
	if (! dir_exists_p(buf))
		run_program(RUN_DISPLAY | RUN_PROGRESS, "/bin/mkdir -p %s", buf);
	if (pm_cur->bsdlabel[part_num].pi_flags & PIF_MOUNT &&
		pm_cur->bsdlabel[part_num].mnt_opts != NULL &&
		strlen(pm_cur->bsdlabel[part_num].mounted) < 1)
		error += run_program(RUN_DISPLAY | RUN_PROGRESS, "/sbin/mount %s /dev/%s%c %s",
				pm_cur->bsdlabel[part_num].mnt_opts,
				pm_cur->diskdev, part_num + 'a', buf);

	if (error)
		pm_cur->bsdlabel[part_num].mounted[0] = '\0';
	else {
		strncpy(pm_cur->bsdlabel[part_num].mounted, buf, MOUNTLEN);
		pm_cur->blocked++;
	}
	return error;
}

void
partman_umount(pm_devs_t *pm_cur, int part_num)
{
	if (run_program(RUN_DISPLAY | RUN_PROGRESS,
			"umount -f /dev/%s%c", pm_cur->diskdev, part_num + 'a') == 0)
		pm_cur->bsdlabel[part_num].mounted[0] = '\0';
	return;
}

int
partman_unconfigure(pm_devs_t *pm_cur)
{
	int i, error = 0;
	if (! strncmp(pm_cur->diskdev, "cgd", 3)) {
 		error = run_program(RUN_DISPLAY | RUN_PROGRESS, "cgdconfig -u %s", pm_cur->diskdev);
 		if (! error && pm->refdev >= 0) {
			cgds[pm_cur->refdev].pm->blocked--;
			cgds[pm_cur->refdev].blocked = 0;
 		}
 	} else if (! strncmp(pm_cur->diskdev, "vnd", 3)) {
		error = run_program(RUN_DISPLAY | RUN_PROGRESS, "vnconfig -u %s", pm_cur->diskdev);
 		if (! error && pm_cur->refdev >= 0) {
			vnds[pm_cur->refdev].pm->blocked--;
			vnds[pm_cur->refdev].blocked = 0;
 		}
	} else if (! strncmp(pm_cur->diskdev, "raid", 4)) {
		error = run_program(RUN_DISPLAY | RUN_PROGRESS, "raidctl -u %s", pm_cur->diskdev);
		if (! error && pm_cur->refdev >= 0) {
			raids[pm_cur->refdev].blocked = 0;
			for (i = 0; i < MAX_IN_RAID; i++)
				if (raids[pm_cur->refdev].pm[i] != NULL)
					raids[pm_cur->refdev].pm[i]->blocked--;
		}
	}
	else
		error = run_program(RUN_DISPLAY | RUN_PROGRESS, "eject -t disk /dev/%s", pm_cur->diskdev);
	return error;
}

/* unconfigure and unmount all devices */
void
partman_unconfigureall(void)
{
	int i;
	pm_devs_t *pm_i;
	for (pm_i = pm_head->next; pm_i != NULL; pm_i = pm_i->next) {
		for (i = 0; i < MAXPARTITIONS; i++)
			if (strlen(pm_i->bsdlabel[i].mounted) > 0)
				if (run_program (0, "unmount %s", pm_i->bsdlabel[i].mounted) == 0)
					pm_i->bsdlabel[i].mounted[0] = '\0';
		partman_unconfigure(pm_i);
	}
	return;
}

/* Safe erase of disk */
int
partman_shred(char *dev, char part, int shredtype)
{
	int error = 0;
	part += 'a';
	if (part < 'a' || part > 'a' + MAXPARTITIONS)
		part = 'd';

	switch(shredtype) {
		case SHRED_NONE:
			return 0;
		case SHRED_ZEROS:
			error += run_program(RUN_DISPLAY | RUN_PROGRESS,
				"dd of=/dev/r%s%c if=/dev/zero bs=1m progress=100 msgfmt=human",
				dev, part);
			return error; 
		case SHRED_RANDOM:
			error += run_program(RUN_DISPLAY | RUN_PROGRESS,
				"dd of=/dev/r%s%c if=/dev/urandom bs=1m progress=100 msgfmt=human",
				dev, part);
			return error;
		case SHRED_CRYPTO:
			error += run_program(RUN_DISPLAY | RUN_PROGRESS,
				"sh -c 'cgdconfig -s cgd0 /dev/%s%c aes-cbc 128 < /dev/urandom'",
				dev, part); // TODO: cgd0?!
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
	if (m != NULL && arg != NULL)
		((part_entry_t *)arg)[0].retvalue = m->cursel + 1;

	for (pm_i = pm_head->next; pm_i != NULL; pm_i = pm_i->next) {
		if (! pm_i->changed)
			continue;
		partman_select(pm_i);
		if (
				write_disklabel() != 0   || /* Write slices table (disklabel) */
				md_post_disklabel() != 0 || /* Enable swap and check badblock */
				make_filesystems()          /* Create filesystems by newfs */
			) {
			/* Oops, something fails... */
			if (logfp)
				fprintf(logfp, "Disk preparing error %s\n", pm_i->diskdev);
			continue;
		}	
		/* Write bootsector if needed */
		if (pm_i->bootable) {
			if (! strncmp("raid", pm_i->diskdev, 4))
				run_program(RUN_DISPLAY | RUN_PROGRESS, "raidctl -v -A root %s", pm_i->diskdev);
		 	if (check_partitions() == 0 || md_post_newfs() != 0) {
		 		if (logfp)
					fprintf(logfp, "Boot disk preparing error: %s\n", pm_i->diskdev);
				continue;
			}
		}
		pm_i->changed = 0;
	
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
	if (m != NULL && arg != NULL)
		partman_upddevlist(m, arg);
	if (logfp)
		fflush (logfp);
    return 0;
}

/* Is there some unsaved changes? */
static int
partman_needsave(void)
{
	pm_devs_t *pm_i;

	for (pm_i = pm_head->next; !changed && pm_i != NULL; pm_i = pm_i->next)
		if (pm_i->changed)
			changed = 1;
	if (changed) {
		/* Oops, we have unsaved changes */
		msg_display("Save changes before finishing?");
		process_menu(MENU_yesno, NULL);
		return (yesno);
	}
	return 0;
}

/* Last check before leaving partition manager */
static int
partman_lastcheck(void)
{
	int error = 0;
	FILE *file_tmp = fopen(concat_paths(targetroot_mnt, "/etc/fstab"), "r");
	if (file_tmp == NULL)
		error = 1;
	else
		fclose(file_tmp);
	return error;
}

/* Function for 'Enter'-menu */
static int
partman_disk_edit(menudesc *m, void *arg)
{
	pm_devs_t *pm_i = ((part_entry_t *)arg)[m->cursel].dev_pm;;
	((part_entry_t *)arg)[0].retvalue = m->cursel + 1;
	int part_num = -1;

	if (pm_i == NULL) 
		return -1;
	if (pm_i->blocked) {
		msg_display("Device is blocked. Do you want to force unblock it and continue?");
		process_menu(MENU_noyes, NULL);
		if (!yesno)
			return -2;
		pm_i->blocked = 0;
	}
	partman_select(pm_i);

	if (((part_entry_t *)arg)[m->cursel].type == PM_PART_T) {
		part_num = ((part_entry_t *)arg)[m->cursel].dev_num;	
		process_menu(MENU_pmpartentry, &part_num);
	} else
		process_menu(MENU_pmdiskentry, &part_num);

	return 0;
}

/* Functions that generate menu entries text */
static void
partman_menufmt(menudesc *m, int opt, void *arg)
{
	const char *dev_status = "";
	char buf[STRSIZE];
	int part_num = ((part_entry_t *)arg)[opt].dev_num;
	pm_devs_t *pm_i = ((part_entry_t *)arg)[opt].dev_pm;

	switch (((part_entry_t *)arg)[opt].type) {
		case PM_DISK_T:
			if (pm_i->blocked)
				dev_status = "BLOCKED";
			else if (! pm_i->changed)
				dev_status = "UNCHANGED";
			else if (pm_i->bootable)
				dev_status = "BOOT";
			else
				dev_status = "USED";
			wprintw(m->mw, "%-33s Name:%-20s %9s", pm_i->diskdev_descr, pm_i->bsddiskname, dev_status);
			break;
		case PM_PART_T:
			snprintf(buf, STRSIZE, "%s %s", pm_i->bsdlabel[part_num].pi_mount,
				(strlen(pm_i->bsdlabel[part_num].mounted) > 0) ? "(mounted)" : "");
			wprintw(m->mw, "   part %c: %-22s %-22s %11uM",
				'a' + part_num, buf,
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
		/* We want menu entrie for creating new device */
		((part_entry_t *)arg)[*i].dev_ptr = NULL;
		m->opts[(*i)++] = (struct menu_ent) {
			.opt_name = d->create_msg,
			.opt_action = d->action,
		};
	}
	for (ii = 0; ii < d->s->max; ii++) {
		if (*(int*)((char*)d->s->entry_enabled + d->s->entry_size * ii) == 0 ||
			*(int*)((char*)d->s->entry_blocked + d->s->entry_size * ii) != 0)
			continue;
		/* We have entry for saving */
		changed = 1;
		m->opts[*i] = (struct menu_ent) {
			.opt_name = NULL,
			.opt_action = d->action,
		};
		((part_entry_t *)arg)[*i].dev_ptr = (char*)d->s->entry_first + d->s->entry_size * ii;
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

	changed = 0;
	find_disks("partman");

	if (m != NULL && arg != NULL) {
		for (pm_i = pm_head->next, i = 0; pm_i != NULL;
				pm_i = pm_i->next, i++) {
			m->opts[i].opt_name = NULL;
			m->opts[i].opt_action = partman_disk_edit;
			((part_entry_t *)arg)[i].dev_pm = pm_i;
			((part_entry_t *)arg)[i].dev_num = -1;
			((part_entry_t *)arg)[i].type = PM_DISK_T;
			for (ii = 0; ii < MAXPARTITIONS; ii++) {
				if (
						(pm_i->bsdlabel[ii].pi_flags & PIF_MOUNT &&
							pm_i->bsdlabel[ii].pi_fstype != FS_UNUSED) ||
						pm_i->bsdlabel[ii].pi_fstype == FS_SWAP ||
						pm_i->bsdlabel[ii].pi_fstype == FS_RAID ||
						pm_i->bsdlabel[ii].pi_fstype == FS_CGD ||
						(pm_i->bsdlabel[ii].pi_fstype == FS_BSDFFS &&
							pm_i->bsdlabel[ii].lvmpv)
					) {
					i++;
					m->opts[i].opt_name = NULL;
					m->opts[i].opt_action = partman_disk_edit;
					((part_entry_t *)arg)[i].dev_pm = pm_i;
					((part_entry_t *)arg)[i].dev_num = ii;
					((part_entry_t *)arg)[i].type = PM_PART_T;
				}
			}
		}
		partman_upddevlist_adv(arg, m, &i,
			&((partman_upddevlist_adv_t) {"Create cryptographic volume (CGD)", 
								partman_cgd_edit, PM_CGD_T, &cgds_t_info, NULL}));
		partman_upddevlist_adv(arg, m, &i,
			&((partman_upddevlist_adv_t) {"Create virtual disk image (VND)",
								partman_vnd_edit, PM_VND_T, &vnds_t_info, NULL}));
		partman_upddevlist_adv(arg, m, &i,
			&((partman_upddevlist_adv_t) {"Create volume group (LVM VG) [WIP]",
								partman_lvm_edit, PM_LVM_T, &lvms_t_info,
			&((partman_upddevlist_adv_t) {"      Create logical volume",
								partman_lvmlv_edit, PM_LVMLV_T,
								&((structinfo_t){
									.max = MAX_LVM_LV, .entry_size = sizeof lvms[0].lv[0],
		 							.entry_first = &lvms[0].lv[0], .entry_enabled = &(lvms[0].lv[0].size),
		 						}),
								NULL})}));
		partman_upddevlist_adv(arg, m, &i,
			&((partman_upddevlist_adv_t) {"Create software RAID",
								partman_raid_edit, PM_RAID_T, &raids_t_info, NULL}));

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
	int menu_no, menu_num_entries;
	menu_ent menu_entries[MAX_ENTRIES+6];
	part_entry_t args[MAX_ENTRIES];

	raids_t_info = (structinfo_t) {
		.max = MAX_RAID, .entry_size = sizeof raids[0], .entry_first = &raids[0],
		.entry_enabled = &(raids[0].enabled), .entry_blocked = &(raids[0].blocked),
		.entry_node = &(raids[0].node), };
	vnds_t_info = (structinfo_t) {
		.max = MAX_VND, .entry_size = sizeof vnds[0], .entry_first = &vnds[0],
		.entry_enabled = &(vnds[0].enabled), .entry_blocked = &(vnds[0].blocked),
		.entry_node = &(vnds[0].node), };
	cgds_t_info = (structinfo_t) {
		.max = MAX_CGD, .entry_size = sizeof cgds[0], .entry_first = &cgds[0],
		.entry_enabled = &(cgds[0].enabled), .entry_blocked = &(cgds[0].blocked),
		.entry_node = &(cgds[0].node), };
	lvms_t_info = (structinfo_t) {
		.max = MAX_LVM_VG, .entry_size = sizeof lvms[0], .entry_first = &lvms[0],
		.entry_enabled = &(lvms[0].enabled), .entry_node = NULL, };

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

		if (args[0].retvalue == 0) {
			if (partman_needsave())
				partman_commit(NULL, NULL);
			if (partman_mountall() != 0 ||
				make_fstab() != 0 ||
				partman_lastcheck() != 0) {
					msg_display("Do you want to try?");
					process_menu(MENU_yesno, NULL);
					args[0].retvalue = (yesno) ? 1:-1;
			}
		}
	} while (args[0].retvalue > 0);
	
	/* retvalue <0 - error, retvalue ==0 - user quits, retvalue >0 - all ok */
	return (args[0].retvalue >= 0)?0:-1;
}
