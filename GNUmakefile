# Makefile for metasepi system.
ARCH       = i386
KERNCONF   = GENERIC_HS
CURDIR     = $(shell pwd)
TOOLDIR    = obj/tooldir
RELEASEDIR = obj/releasedir
SETSDIR    = ${RELEASEDIR}/${ARCH}/binary/sets
DESTDIR    = obj/destdir.${ARCH}
MEDIACDDIR = obj/mediacd
NUMCPU     = $(shell cat /proc/cpuinfo | grep -c "^processor")
BUILDSH    = sh build.sh -U -u -N 1 -j ${NUMCPU}
NBMAKE     = ${CURDIR}/${TOOLDIR}/bin/nbmake-${ARCH} -j ${NUMCPU}
NBMAKEFS   = ${CURDIR}/${TOOLDIR}/bin/nbmakefs
NBCONFIG   = ${CURDIR}/${TOOLDIR}/bin/nbconfig
NBGCC      = ${CURDIR}/${TOOLDIR}/bin/i486--netbsdelf-gcc
NBGDB      = ${CURDIR}/${TOOLDIR}/bin/i486--netbsdelf-gdb
MINIIMGDIR = ${CURDIR}/distrib/${ARCH}/liveimage/miniimage
QEMUOPTS   = -m 1024 -soundhw ac97 -hdachs 390,16,63,lba -hda ${MINIIMGDIR}/${ARCH}-mini.img -cdrom ${MEDIACDDIR}/cd.iso

HSBUILD = metasepi/hsbuild
HSSRC   = metasepi/hssrc
HSCODE  = $(wildcard $(HSSRC)/*.hs $(HSSRC)/*/*.hs $(HSSRC)/*/*/*.hs $(HSSRC)/*/*/*/*.hs)

### Build kernel
all: sys/arch/${ARCH}/compile/${KERNCONF}/Makefile ${HSBUILD}/hsmain.c
	cd sys/arch/${ARCH}/compile/${KERNCONF} && ${NBMAKE} depend && ${NBMAKE}
	mkdir -p ${CURDIR}/obj/releasedir/${ARCH}/binary/sets
	cd sys/arch/${ARCH}/compile/${KERNCONF} && tar cfz ${CURDIR}/obj/releasedir/${ARCH}/binary/sets/kern-GENERIC.tgz ./netbsd

${HSBUILD}/hsmain.c: ${HSCODE}
	ajhc -fffi --include=hs_src --tdir=$(HSBUILD) -C -o $@ $(HSSRC)/Main.hs

sys/arch/${ARCH}/compile/${KERNCONF}/Makefile: sys/arch/${ARCH}/conf/${KERNCONF} obj/build_tools.stamp
	cd sys/arch/${ARCH}/conf && ${NBCONFIG} ${KERNCONF}

### Setup NetBSD environment
obj/build_tools.stamp:
	env MKCROSSGDB=yes ${BUILDSH} -T ${TOOLDIR} -m ${ARCH} tools
	touch obj/build_tools.stamp

obj/build_dist.stamp: obj/build_tools.stamp
	${BUILDSH} -T ${TOOLDIR} -m ${ARCH} distribution
	touch obj/build_dist.stamp

obj/build_sets.stamp: obj/build_dist.stamp
	${BUILDSH} -T ${TOOLDIR} -m ${ARCH} sets
	touch obj/build_sets.stamp

### Build QEMU image
miniimage: all obj/build_sets.stamp
	cd ${MINIIMGDIR} && ${NBMAKE} live_image

mediacd:
	@if [ "${WAVFILE}" = "" ]; then 			\
		echo "Need to set WAVFILE value.";		\
		false;						\
	fi
	@if [ ! -f ${WAVFILE} ]; then 				\
		echo "Missing \"${WAVFILE}\" file, aborting.";	\
		false; 						\
	fi
	rm -rf ${MEDIACDDIR}
	mkdir -p ${MEDIACDDIR}/cd
	cp ${WAVFILE} ${MEDIACDDIR}/cd/test.wav
	cd ${MEDIACDDIR} && ${NBMAKEFS} -t cd9660 cd.iso cd

### Run QEMU image
qemu:
	@if [ ! -f ${MINIIMGDIR}/${ARCH}-mini.img ]; then 				\
		echo "Missing \"${MINIIMGDIR}/${ARCH}-mini.img\" file, aborting.";	\
		false; 									\
	fi
	@if [ ! -f ${MEDIACDDIR}/cd.iso ]; then 					\
		echo "Missing \"${MEDIACDDIR}/cd.iso\" file, aborting.";		\
		false; 									\
	fi
	env QEMU_AUDIO_DRV=alsa qemu-system-i386 ${QEMUOPTS}

qemucurses:
	env QEMU_AUDIO_DRV=alsa qemu-system-i386 ${QEMUOPTS} -curses

clean:
	rm -rf sys/arch/${ARCH}/compile/${KERNCONF} ${HSBUILD}

distclean: clean
	${BUILDSH} -T ${TOOLDIR} -m ${ARCH} cleandir
	rm -f *~

.PHONY: clean miniimage qemu qemucurses qemugdb
