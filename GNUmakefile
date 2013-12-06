# Makefile for metasepi system.
ARCH       = i386
KERNCONF   = GENERIC_HS
CURDIR     = $(shell pwd)
TOOLDIR    = obj/tooldir
RELEASEDIR = obj/releasedir
SETSDIR    = ${RELEASEDIR}/${ARCH}/binary/sets
DESTDIR    = obj/destdir.${ARCH}
BOOTCDDIR  = obj/bootcd
NUMCPU     = $(shell cat /proc/cpuinfo | grep -c "^processor")
BUILDSH    = sh build.sh -U -u -N 0 -j ${NUMCPU}
NBMAKE     = ${CURDIR}/${TOOLDIR}/bin/nbmake-${ARCH} -j ${NUMCPU}
NBMAKEFS   = ${CURDIR}/${TOOLDIR}/bin/nbmakefs
NBGDB      = ${CURDIR}/${TOOLDIR}/bin/i486--netbsdelf-gdb
MINIIMGDIR = ${CURDIR}/distrib/${ARCH}/liveimage/miniimage
QEMUOPTS   = -m 1024 -soundhw ac97 -no-reboot -cdrom ${BOOTCDDIR}/cd.iso
MAKEFSOPTS = -t cd9660 -o 'bootimage=i386;bootxx_cd9660,no-emul-boot'
MP3FILE    = metasepi/sound/Epopsan-signal.mp3
LOGFILTER  = -e "===>" -e "^nbgmake"

HSBUILD = metasepi/sys/hsbuild
HSSRC   = metasepi/sys/hssrc
HSCODE  = $(wildcard $(HSSRC)/*.hs $(HSSRC)/*/*.hs $(HSSRC)/*/*/*.hs $(HSSRC)/*/*/*/*.hs)

### Build kernel
all: obj/build_tools.stamp ${HSBUILD}/hsmain.c
	${BUILDSH} -T ${TOOLDIR} -m ${ARCH} kernel=${KERNCONF}

${HSBUILD}/hsmain.c: ${HSCODE}
	ajhc -fffi -fcustomthread --include=hs_src --tdir=$(HSBUILD) -C -o $@ $(HSSRC)/Main.hs
	rm -f $(HSBUILD)/sys/queue.h # Use queue.h at NetBSD side

### Setup NetBSD environment
obj/build_tools.stamp:
	env MKCROSSGDB=yes ${BUILDSH} -T ${TOOLDIR} -m ${ARCH} tools | grep ${LOGFILTER}
	touch obj/build_tools.stamp

obj/build_dist.stamp: obj/build_tools.stamp
	${BUILDSH} -T ${TOOLDIR} -m ${ARCH} distribution | grep ${LOGFILTER}
	touch obj/build_dist.stamp

#obj/build_sets.stamp: obj/build_dist.stamp
#	${BUILDSH} -T ${TOOLDIR} -m ${ARCH} sets | grep ${LOGFILTER}
#	touch obj/build_sets.stamp

### Build QEMU image
bootcd: ${BOOTCDDIR}/cd/test.wav ${BOOTCDDIR}/bootxx_cd9660 ${BOOTCDDIR}/cd/boot \
	  ${BOOTCDDIR}/cd/boot.cfg ${BOOTCDDIR}/cd/miniroot.kmod all
	gzip -c sys/arch/${ARCH}/compile/obj/${KERNCONF}/netbsd > ${BOOTCDDIR}/cd/netbsd
	cd ${BOOTCDDIR} && ${NBMAKEFS} ${MAKEFSOPTS} cd.iso cd

${BOOTCDDIR}/cd/test.wav: ${MP3FILE}
	mkdir -p ${BOOTCDDIR}/cd
	rm -f $@
	ffmpeg -i ${MP3FILE} $@

${BOOTCDDIR}/bootxx_cd9660: obj/build_dist.stamp
	mkdir -p ${BOOTCDDIR}
	cp ${DESTDIR}/usr/mdec/bootxx_cd9660 $@

${BOOTCDDIR}/cd/boot: obj/build_dist.stamp
	mkdir -p ${BOOTCDDIR}
	cp ${DESTDIR}/usr/mdec/boot $@

${BOOTCDDIR}/cd/boot.cfg:
	mkdir -p ${BOOTCDDIR}/cd
	echo "timeout=0\nload=/miniroot.kmod" > $@

${BOOTCDDIR}/cd/miniroot.kmod: obj/build_dist.stamp obj/audioplay distrib/i386/ramdisks/ramdisk-audioplay/Makefile distrib/i386/ramdisks/ramdisk-audioplay/list
	mkdir -p ${BOOTCDDIR}/cd
	${NBMAKE} -C distrib/i386/ramdisks/ramdisk-audioplay
	${NBMAKE} -C distrib/i386/kmod-audioplay
	cp distrib/i386/kmod-audioplay/miniroot.kmod $@

obj/audioplay:
	${NBMAKE} -C usr.bin/audio clean
	${NBMAKE} -C usr.bin/audio LDSTATIC=-static
	cp usr.bin/audio/play/obj/audioplay $@

### Run QEMU image
qemu: bootcd
	env QEMU_AUDIO_DRV=alsa qemu-system-i386 ${QEMUOPTS}

qemucurses: bootcd
	env QEMU_AUDIO_DRV=alsa qemu-system-i386 ${QEMUOPTS} -curses

qemuvnc: bootcd
	@echo '####################################'
	@echo '### Run "gvncviewer localhost:0" ###'
	@echo '####################################'
	env QEMU_AUDIO_DRV=alsa qemu-system-i386 ${QEMUOPTS} -vnc :0

clean:
	rm -rf sys/arch/${ARCH}/compile/obj/${KERNCONF} ${HSBUILD} ${BOOTCDDIR} *~

#distclean: clean
#	rm -f obj/build_dist.stamp *~
#	env MKCROSSGDB=yes ${BUILDSH} -T ${TOOLDIR} -m ${ARCH} cleandir
#	${NBMAKE} -C distrib/i386/kmod-audioplay clean
#	${NBMAKE} -C distrib/i386/ramdisks/ramdisk-audioplay clean

.PHONY: setup bootcd clean distclean qemu qemucurses
