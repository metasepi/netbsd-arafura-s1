/*	$NetBSD: partman.c,v 0.000 0000/00/00 00:00:00 uzixls Exp $ */

/*
 * Copyright 2012 Eugene Lozovoy
 * All rights reserved.
 *
 * Written by Philip A. Nelson for Piermont Information Systems Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of Piermont Information Systems Inc. may not be used to endorse
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
} part_entry_t;

#define MNTS_MAX 96
struct {
    const char *diskdev, *mnt_opts, *fsname;
    char *pi_mount;
    int pm_part;
} mnts[MNTS_MAX];

#define MAX_RAIDS 16 // TODO: replace by true way
#define MAX_IN_RAID 48
struct raids_t {
	int enabled;
	pm_devs_t *pm[MAX_IN_RAID];
	char pm_name[MAX_IN_RAID][SSTRSIZE];
	int pm_part[MAX_IN_RAID];
	int pm_is_spare[MAX_IN_RAID];
	int numRow, numCol, numSpare;
	int sectPerSU, SUsPerParityUnit, SUsPerReconUnit, RAID_level;
} raids[MAX_RAIDS];

#define MAX_VNDS 16
struct vnds_t {
	int enabled;
	char filepath[STRSIZE];
	int size;
	int readonly;
	int is_exist;
	int manual_geom;
	int secsize, nsectors, ntracks, ncylinders;
} vnds[MAX_VNDS];

#define MAX_CGDS 16
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
} cgds[MAX_CGDS];

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


static int partman_manage_menu(const char *, void (*)(menudesc *, int, void *),
	int (*)(menudesc *, void *), int, int, void *);
static int partman_manage_edit(int menu_entries_count, void (*)(menudesc *, int, void *),
	int (*)(menudesc *, void *), void (*)(void), int cursel, int max,
	int entry_size, void *entry_enabled);
static int partman_raid_edit(menudesc *, void *);
static void partman_raid_edit_menufmt(menudesc *, int, void *);
static int partman_raid_set_value(menudesc *, void *);
static void partman_raid_new_init(void);
static int partman_raid_disk_add(menudesc *, void *);
static int partman_raid_disk_del(menudesc *, void *);
static int partman_raid_set_value(menudesc *, void *);
static void partman_raid_manage_menufmt(menudesc *, int, void *);
static int partman_raid_manage(menudesc *, void *);
static int partman_raid_commit(void);
part_entry_t partman_getdev(int);
static void partman_select(pm_devs_t *);
static int partman_mountall_sort(const void *, const void *);
static int partman_mountall(void);
static int partman_upddevlist(menudesc *, void *);
static int partman_vnd_commit(void);
static int partman_vnd_manage(menudesc *, void *);
static int partman_commit(menudesc *, void *);
static int partman_submenu(menudesc *, void *);
static void partman_menufmt(menudesc *, int, void *);


static int
partman_manage_menu(const char *header, void (*menu_fmt)(menudesc *, int, void *),
	int (*action)(menudesc *, void *), int max, int entry_size, void *entry_enabled)
{
	menu_ent menu_entries[max+1];
	part_entry_t args[max];
	int i, num_devs;
	int menu_no;

	do {
		manage_curdev = -1;
		num_devs = 0;
		for (i = 0; i < max; i++) {
			if (*(int*)((char*)entry_enabled + entry_size * i) == 0)
				continue;

			menu_entries[num_devs] = (struct menu_ent) {
				.opt_name = NULL,
				.opt_action = action,
				.opt_menu = OPT_NOMENU,
				.opt_flags = OPT_EXIT,
			};
			args[num_devs].dev_num = i;
			num_devs++;
		}
		if (num_devs < max)
			menu_entries[num_devs] = (struct menu_ent) {
				.opt_name = "Create new",
				.opt_action = action,
				.opt_menu = OPT_NOMENU,
				.opt_flags = OPT_EXIT,
			};
		else
			menu_entries[num_devs] = (struct menu_ent) {
				.opt_name = "Entries count limit reached",
				.opt_action = NULL,
				.opt_menu = OPT_NOMENU,
				.opt_flags = 0,
			};

		menu_no = new_menu(header, menu_entries, num_devs + 1, -1, -1,
			(num_devs+1<3)?3:num_devs+2, 40, MC_SCROLL | MC_NOCLEAR, NULL,
			menu_fmt, NULL, NULL, NULL);
		if (menu_no == -1)
			return -1;
		args[0].retvalue = 0;
		process_menu(menu_no, &args);
		free_menu(menu_no);
	} while (args[0].retvalue > 0);

	return 0;
}

static int
partman_manage_edit(int menu_entries_count, void (*menu_fmt)(menudesc *, int, void *),
	int (*action)(menudesc *, void *), void (*entry_init)(void),
	int cursel, int max, int entry_size, void *entry_enabled)
{
	int i, retvalue = 0, ok = 0;

	for (i = -1, manage_curdev = -1; manage_curdev < max-1 && i < cursel;)
		if (*(int*)((char*)entry_enabled + entry_size * ++manage_curdev) != 0)
			i++;
	if (i < cursel) { /* Cannot find device, try to create new */
		for (i = 0; i < max && !ok; i++) {
			if (*(int*)((char*)entry_enabled + entry_size * i) != 1) {
				manage_curdev = i;
				entry_init();
				*(int*)((char*)entry_enabled + entry_size * i) = 1;
				ok = 1;
			}
		}
		if (!ok) {
			if (logfp)
				fprintf(logfp, "partman_manage_edit: invalid drive\n");
			return -1;
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
	
	process_menu(menu_no, &retvalue);
	free_menu(menu_no);

	return retvalue;
}

/*** RAIDs ***/

static int
partman_raid_edit(menudesc *m, void *arg)
{
	((part_entry_t *)arg)[0].retvalue = m->cursel + 1;

	return partman_manage_edit(PMR_MENU_END, partman_raid_edit_menufmt,
		partman_raid_set_value,	partman_raid_new_init, m->cursel, MAX_RAIDS,
		sizeof raids[0], &(raids[0].enabled));
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
			wprintw(m->mw, "RAID level:       %22d", raids[manage_curdev].RAID_level);
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
	const char *msg_to_show;
	int retvalue = -1;
	int *out_var;
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
				raids[manage_curdev].RAID_level = retvalue;
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
	snprintf(buf, SSTRSIZE, "%d", *out_var);
	msg_prompt_win(msg_to_show, -1, 18, 0, 0, buf, buf, SSTRSIZE);
	if (atoi(buf) >= 0)
		*out_var = atoi(buf);
	return 0;
}

static void
partman_raid_new_init(void)
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
partman_raid_disk_add(menudesc *m, void *arg)
{
	int i;
	part_entry_t disk_entries = partman_getdev(FS_RAID);
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

static void
partman_raid_manage_menufmt(menudesc *m, int opt, void *arg)
{
	int i, num, ok = 0;
	
	num = ((part_entry_t *)arg)[opt].dev_num;

	if (raids[num].enabled != 0) {
		wprintw(m->mw, "RAID%d - ", raids[num].RAID_level);
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

static int
partman_raid_manage(menudesc *m, void *arg)
{
	((part_entry_t *)arg)[0].retvalue = m->cursel + 1;
	partman_manage_menu("RAIDframe manager", partman_raid_manage_menufmt,
		partman_raid_edit, MAX_RAIDS, sizeof raids[0], &(raids[0].enabled));
	return 0;
}

static int
partman_raid_commit(void)
{
	int i, ii, raid_devnum;
	FILE *f;
	char f_name[STRSIZE];

	for (raid_devnum = 0, i = 0; raid_devnum < MAX_RAIDS && i < MAX_RAIDS;
			raid_devnum++, i++) {
		if (raids[i].enabled == 0)
			continue;

		/* Trying to detect free raid device */
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
						raids[i].RAID_level);

		scripting_fprintf(f, "\nSTART queue\nfifo 100\n\n");
		scripting_fprintf(NULL, "EOF\n");
		fclose (f);
		fflush(NULL);

		/* Raid initialization */
		if (
			run_program(RUN_DISPLAY | RUN_PROGRESS, "raidctl -C %s raid%d",
							f_name, raid_devnum) == 0 &&
			run_program(RUN_DISPLAY | RUN_PROGRESS, "raidctl -I %d raid%d",
							rand(), raid_devnum) == 0 &&
			run_program(RUN_DISPLAY | RUN_PROGRESS, "raidctl -vi raid%d",
							raid_devnum) == 0
			)
			raids[i].enabled = 0; /* RAID creation done, remove it from list to 
									 prevent it's repeated reinitialization */
	}
	return 0;
}

/*** VND ***/

static void
partman_vnd_edit_menufmt(menudesc *m, int opt, void *arg)
{
	char buf[SSTRSIZE];
	strcpy(buf, "-");

	switch (opt){
		case PMV_MENU_FILEPATH:
			wprintw(m->mw, "File path:        %22s", vnds[manage_curdev].filepath);
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
	const char *msg_to_show;
	int *out_var;
	
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
	snprintf(buf, SSTRSIZE, "%d", *out_var);
	msg_prompt_win(msg_to_show, -1, 18, 0, 0, buf, buf, SSTRSIZE);
	if (atoi(buf) >= 0)
		*out_var = atoi(buf);
	return 0;
}

static void
partman_vnd_new_init(void)
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
partman_vnd_edit(menudesc *m, void *arg)
{
	((part_entry_t *)arg)[0].retvalue = m->cursel + 1;

	return partman_manage_edit(PMV_MENU_END, partman_vnd_edit_menufmt,
		partman_vnd_set_value,	partman_vnd_new_init, m->cursel, MAX_RAIDS,
		sizeof vnds[0], &(vnds[0].enabled));
}

static void
partman_vnd_manage_menufmt(menudesc *m, int opt, void *arg)
{
	int num = ((part_entry_t *)arg)[opt].dev_num;

	if (vnds[num].enabled != 0) {
		wprintw(m->mw, "%s (%dMB)", vnds[num].filepath, vnds[num].size);
	}
	return;
}

static int
partman_vnd_manage(menudesc *m, void *arg)
{
	((part_entry_t *)arg)[0].retvalue = m->cursel + 1;
	partman_manage_menu("VND manager", partman_vnd_manage_menufmt,
		partman_vnd_edit, MAX_VNDS, sizeof vnds[0], &(vnds[0].enabled));
	return 0;
}

static int
partman_vnd_commit(void)
{
	int i, not_ok;
	char r_o[3];

	for (i = 0; i < MAX_VNDS; i++) {
		not_ok = 0;
		if (vnds[i].enabled == 0)
			continue;
		strcpy(r_o, vnds[i].readonly?"-r":"");
		/* If this is a new image */
		if (!vnds[i].is_exist)
			not_ok += run_program(RUN_DISPLAY,
									"dd if=/dev/zero of=%s bs=1m count=%d",
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

static int
partman_cgd_disk_set(void)
{
	part_entry_t disk_entries = partman_getdev(FS_CGD);
	if (disk_entries.retvalue < 0)
		return disk_entries.retvalue;

	cgds[manage_curdev].pm = disk_entries.dev_pm;
	cgds[manage_curdev].pm_part = disk_entries.dev_num;
	strncpy(cgds[manage_curdev].pm_name, disk_entries.fullname, SSTRSIZE);

	return 0;
}

static void
partman_cgd_new_init(void)
{
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
			partman_cgd_disk_set();
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

static int
partman_cgd_edit(menudesc *m, void *arg)
{
	((part_entry_t *)arg)[0].retvalue = m->cursel + 1;

	return partman_manage_edit(PMC_MENU_END, partman_cgd_edit_menufmt,
		partman_cgd_set_value, partman_cgd_new_init, m->cursel, MAX_CGDS,
		sizeof cgds[0], &(cgds[0].enabled));
}

static void
partman_cgd_manage_menufmt(menudesc *m, int opt, void *arg)
{
	int num = ((part_entry_t *)arg)[opt].dev_num;

	if (cgds[num].enabled != 0)
		wprintw(m->mw, "%s%c (%s-%d)", cgds[num].pm->diskdev, cgds[num].pm_part,
			cgds[num].enc_type, cgds[num].key_size);
	return;
}

static int
partman_cgd_manage(menudesc *m, void *arg)
{
	((part_entry_t *)arg)[0].retvalue = m->cursel + 1;
	partman_manage_menu("CGD manager", partman_cgd_manage_menufmt,
		partman_cgd_edit, MAX_CGDS, sizeof cgds[0], &(cgds[0].enabled));
	return 0;
}

static int
partman_cgd_commit(void)
{
	int i;
	for (i = 0; i < MAX_CGDS; i++) {
		if (! cgds[i].enabled)
			continue;
		if (run_program(RUN_DISPLAY, "cgdconfig -g -i %s -k %s -o /tmp/%s %s %d",
			cgds[i].iv_type, cgds[i].keygen_type, cgds[i].pm_name,
			cgds[i].enc_type, cgds[i].key_size) != 0)
			continue;
		if (run_program(RUN_DISPLAY, "cgdconfig -V re-enter cgd%d /dev/%s /tmp/%s",
			i, cgds[i].pm_name, cgds[i].pm_name) != 0)
			continue;
	}

	return 0;
}

/*** Partman generic functions ***/

part_entry_t
partman_getdev(int fstype)
{
	pm_devs_t *pm_devs_i;
	int dev_num = -1, num_devs;
	int i;
	int menu_no;
	menu_ent menu_entries[MAX_DISKS*MAXPARTITIONS];
	part_entry_t disk_entries[MAX_DISKS*MAXPARTITIONS];

	for (pm_devs_i = pm_devs->next, num_devs = 0; pm_devs_i != NULL;
			pm_devs_i = pm_devs_i->next)
		for (i = 0; i < MAXPARTITIONS; i++) {
			if (pm_devs_i->bsdlabel[i].pi_fstype == fstype) {
				disk_entries[num_devs].dev_pm = pm_devs_i;
				disk_entries[num_devs].dev_num = 'a' + i;
				snprintf(disk_entries[num_devs].fullname, SSTRSIZE, "%s%c",
						pm_devs_i->diskdev, 'a' + i);
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

static int
partman_mountall(void)
{
	pm_devs_t *pm_devs_i;
	int num_devs = 0;
	int i, ii, error, ok;
	char diskdev_with_root[SSTRSIZE];
	diskdev_with_root[0] = '\0';

	int mnts_order[MNTS_MAX]; // TODO: rewrite
	for (pm_devs_i = pm_devs->next, num_devs = 0; pm_devs_i != NULL;
			pm_devs_i = pm_devs_i->next) {
		ok = 0;
		for (i = 0; i < MAXPARTITIONS; i++) {
			if (pm_devs_i->bsdlabel[i].pi_flags & PIF_MOUNT &&
					pm_devs_i->bsdlabel[i].mnt_opts != NULL) {
				mnts[num_devs].pm_part = i;
				mnts[num_devs].diskdev = pm_devs_i->diskdev;
				mnts[num_devs].mnt_opts = pm_devs_i->bsdlabel[i].mnt_opts;
				mnts[num_devs].fsname = pm_devs_i->bsdlabel[i].fsname;
				mnts[num_devs].pi_mount = pm_devs_i->bsdlabel[i].pi_mount;
				if (strcmp(pm_devs_i->bsdlabel[i].pi_mount, "/") == 0)
					strlcpy(diskdev_with_root, pm_devs_i->bsdlabel[i].pi_mount, 
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
	int error;
	int num = ((part_entry_t *)arg)[m->cursel].dev_num;

	if (pm->bsdlabel[num].pi_flags & PIF_MOUNT && pm->bsdlabel[num].mnt_opts != NULL)
		error = target_mount(pm->bsdlabel[num].mnt_opts, pm->diskdev, num,
					pm->bsdlabel[num].pi_mount);
		if (error)
			return error;
	return 0;
}

int
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

static int
partman_upddevlist(menudesc *m, void *arg)
{
	int i;
	pm_devs_t *pm_devs_tmp;

	if (arg != NULL)
		((part_entry_t *)arg)[0].retvalue = m->cursel + 1;

	for (i = 0; find_disks(msg_string(MSG_install), i) > 0; i++) {
		
		for (pm_devs_tmp = pm_devs; pm_devs_tmp->next != NULL;
				pm_devs_tmp = pm_devs_tmp->next)
			if (strcmp(pm_devs_tmp->next->diskdev, pm->diskdev) == 0)
				return 0;
		pm_devs_tmp->next = pm_found;

		pm_found = malloc(sizeof (pm_devs_t));
		memset(pm_found, 0, sizeof *pm_found);
		pm_found->next = NULL;

		(void) savenewlabel(pm->oldlabel, getmaxpartitions());
	}
	return 0;
}

static int
partman_commit(menudesc *m, void *arg)
{
	int retcode;
	pm_devs_t *pm_devs_i;
	((part_entry_t *)arg)[0].retvalue = m->cursel + 1;

	for (pm_devs_i = pm_devs->next; pm_devs_i != NULL;
			pm_devs_i = pm_devs_i->next) {
		if (! pm_devs_i->changed)
			continue;
		partman_select(pm_devs_i);
		if (
				md_pre_disklabel() != 0  || /* Write partition table */
				write_disklabel() != 0   || /* Write slices table (disklabel) */
				md_post_disklabel() != 0 || /* Enable swap and check badblock */
				make_filesystems()          /* Create filesystems by newfs */
			) { /* If something fails... */
			if (logfp)
				fprintf(logfp, "Disk preparing error\n");
			return -1;
		}
		/* Write bootsector if needed */
		if (pm_devs_i->bootable && md_post_newfs() != 0)
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
	if (logfp)
		fflush (logfp);
    return 0;
}

static int
partman_submenu(menudesc *m, void *arg)
{
	pm_devs_t *pm_devs_i = ((part_entry_t *)arg)[m->cursel].dev_pm;;
	int retvalue;

	((part_entry_t *)arg)[0].retvalue = m->cursel + 1;

	if (pm_devs_i == NULL) 
		return -1;
	partman_select(pm_devs_i);

	process_menu(MENU_pmentry, &retvalue);
	return 0;
}

static void
partman_menufmt(menudesc *m, int opt, void *arg)
{
	int i;
	char dev_mounts[STRSIZE];
	dev_mounts[0] = '\0';
	pm_devs_t *pm_devs_i = ((part_entry_t *)arg)[opt].dev_pm;

	if (pm_devs_i == NULL) 
		return;

	for (i = 0; i < MAXPARTITIONS; i++) {
		if (pm_devs_i->bsdlabel[i].pi_mount != 0 &&
				pm_devs_i->bsdlabel[i].pi_fstype != FS_UNUSED) {
			strcat(dev_mounts, pm_devs_i->bsdlabel[i].pi_mount);
			strcat(dev_mounts, " ");
		}
	}
	wprintw(m->mw, "%-25s%1s %-20s %4s", pm_devs_i->diskdev_descr,
			pm_devs_i->changed? "*" : "", dev_mounts,
			pm_devs_i->bootable? "BOOT" : "");
	return;
}

// TODO: switch to langfiles
int
partman(void)
{
	int menu_no;
	int i, ii;
	pm_devs_t *pm_devs_i;
	menu_ent menu_entries[MAX_DISKS];
	part_entry_t args[MAX_DISKS];

	do {
		clear();
		refresh();
		partman_upddevlist(NULL, NULL);

		for (pm_devs_i = pm_devs->next, i = 0; pm_devs_i != NULL;
				pm_devs_i = pm_devs_i->next, i++) {
			menu_entries[i].opt_name = NULL;
			menu_entries[i].opt_action = partman_submenu;
			args[i].dev_pm = pm_devs_i;
		}

		menu_entries[i] = (struct menu_ent) {
			.opt_name = "Update devices list",
			.opt_action = partman_upddevlist,
		};
		menu_entries[++i] = (struct menu_ent) {
			.opt_name = "Manage cryptographic volumes (CGD)",
			.opt_action = partman_cgd_manage,
		};
		menu_entries[++i] = (struct menu_ent) {
			.opt_name = "Manage virtual disk images (VND)",
			.opt_action = partman_vnd_manage,
		};
		menu_entries[++i] = (struct menu_ent) {
			.opt_name = "Manage logical volumes (LVM) [WIP]",
			.opt_action = partman_vnd_manage,
		};
		menu_entries[++i] = (struct menu_ent) {
			.opt_name = "Manage software RAIDs",
			.opt_action = partman_raid_manage,
		};
		menu_entries[++i] = (struct menu_ent) {
			.opt_name = "Save changes",
			.opt_action = partman_commit,
		};

		for (ii = 0; ii <= i; ii++) {
			menu_entries[ii].opt_menu = OPT_NOMENU;
			menu_entries[ii].opt_flags = OPT_EXIT;
		}

		menu_no = new_menu("Partition manager. All disks, partitions, LVM, RAID displayed there.\nAt first add drive, then prepare partitions and go.",
			menu_entries, i+1, 1, 1, 0, 0, MC_ALWAYS_SCROLL | MC_NOBOX | MC_NOCLEAR,
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

