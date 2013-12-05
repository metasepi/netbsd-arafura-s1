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
BUILDSH    = sh build.sh -U -u -N 1 -j ${NUMCPU}
NBMAKE     = ${CURDIR}/${TOOLDIR}/bin/nbmake-${ARCH} -j ${NUMCPU}
NBMAKEFS   = ${CURDIR}/${TOOLDIR}/bin/nbmakefs
NBGDB      = ${CURDIR}/${TOOLDIR}/bin/i486--netbsdelf-gdb
MINIIMGDIR = ${CURDIR}/distrib/${ARCH}/liveimage/miniimage
QEMUOPTS   = -m 1024 -soundhw ac97 -cdrom ${BOOTCDDIR}/cd.iso
MAKEFSOPTS = -t cd9660 -o 'bootimage=i386;bootxx_cd9660,no-emul-boot'

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
	env MKCROSSGDB=yes ${BUILDSH} -T ${TOOLDIR} -m ${ARCH} tools
	touch obj/build_tools.stamp

obj/build_dist.stamp: obj/build_tools.stamp
	${BUILDSH} -T ${TOOLDIR} -m ${ARCH} distribution
	touch obj/build_dist.stamp

#obj/build_sets.stamp: obj/build_dist.stamp
#	${BUILDSH} -T ${TOOLDIR} -m ${ARCH} sets
#	touch obj/build_sets.stamp

### Setup Wav file
setup:
	@if [ "${WAV}" = "" ]; then 			\
		echo "Need to set WAV value.";		\
		false;						\
	fi
	@if [ ! -f ${WAV} ]; then 				\
		echo "Missing \"${WAV}\" file, aborting.";	\
		false; 						\
	fi
	rm -rf ${BOOTCDDIR}
	mkdir -p ${BOOTCDDIR}/cd
	cp ${WAV} ${BOOTCDDIR}/cd/test.wav

### Build QEMU image
bootcd: ${BOOTCDDIR}/cd/test.wav ${BOOTCDDIR}/bootxx_cd9660 ${BOOTCDDIR}/cd/boot \
	  ${BOOTCDDIR}/cd/boot.cfg ${BOOTCDDIR}/cd/miniroot.kmod all
	gzip -c sys/arch/${ARCH}/compile/obj/${KERNCONF}/netbsd > ${BOOTCDDIR}/cd/netbsd
	cd ${BOOTCDDIR} && ${NBMAKEFS} ${MAKEFSOPTS} cd.iso cd

${BOOTCDDIR}/bootxx_cd9660: obj/build_dist.stamp
	cp ${DESTDIR}/usr/mdec/bootxx_cd9660 $@

${BOOTCDDIR}/cd/boot: obj/build_dist.stamp
	cp ${DESTDIR}/usr/mdec/boot $@

${BOOTCDDIR}/cd/boot.cfg:
	echo "timeout=0\nload=/miniroot.kmod" > $@

${BOOTCDDIR}/cd/miniroot.kmod: obj/build_dist.stamp obj/audioplay
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

clean:
	rm -rf sys/arch/${ARCH}/compile/obj/${KERNCONF} ${HSBUILD}

distclean: clean
	env MKCROSSGDB=yes ${BUILDSH} -T ${TOOLDIR} -m ${ARCH} cleandir
	${NBMAKE} -C distrib/i386/kmod-audioplay clean
	${NBMAKE} -C distrib/i386/ramdisks/ramdisk-audioplay clean
	rm -f *~

.PHONY: setup bootcd clean distclean qemu qemucurses
