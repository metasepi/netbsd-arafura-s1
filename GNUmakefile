# Makefile for metasepi system.
ARCH = i386
CURDIR = $(shell pwd)
TOOLDIR = obj/tooldir
RELEASEDIR = obj/releasedir
SETSDIR = ${RELEASEDIR}/${ARCH}/binary/sets
DESTDIR = obj/destdir.${ARCH}
NUMCPU = $(shell cat /proc/cpuinfo | grep -c "^processor")
BUILDSH = sh build.sh -U -u -N 1 -j ${NUMCPU}
NBMAKE = ${CURDIR}/${TOOLDIR}/bin/nbmake-${ARCH} -j ${NUMCPU}
NBMAKEFS = ${CURDIR}/${TOOLDIR}/bin/nbmakefs
NBCONFIG =${CURDIR}/${TOOLDIR}/bin/nbconfig
NBGCC = ${CURDIR}/${TOOLDIR}/bin/i486--netbsdelf-gcc
NBGDB = ${CURDIR}/${TOOLDIR}/bin/i486--netbsdelf-gdb
MINIIMGDIR = ${CURDIR}/distrib/${ARCH}/liveimage/miniimage

all: sys/arch/${ARCH}/compile/GENERIC/Makefile
	cd sys/arch/${ARCH}/compile/GENERIC && ${NBMAKE}
	mkdir -p ${CURDIR}/obj/releasedir/${ARCH}/binary/sets
	cd sys/arch/${ARCH}/compile/GENERIC && tar cfz ${CURDIR}/obj/releasedir/${ARCH}/binary/sets/kern-GENERIC.tgz ./netbsd

sys/arch/${ARCH}/compile/GENERIC/Makefile: sys/arch/${ARCH}/conf/GENERIC obj/build_tools.stamp
	cd sys/arch/${ARCH}/conf && ${NBCONFIG} GENERIC

### Build and install NetBSD.
obj/build_tools.stamp:
	env MKCROSSGDB=yes ${BUILDSH} -T ${TOOLDIR} -m ${ARCH} tools
	touch obj/build_tools.stamp

obj/build_dist.stamp: obj/build_tools.stamp
	${BUILDSH} -T ${TOOLDIR} -m ${ARCH} distribution
	touch obj/build_dist.stamp

obj/build_sets.stamp: obj/build_dist.stamp
	${BUILDSH} -T ${TOOLDIR} -m ${ARCH} sets
	touch obj/build_sets.stamp

miniimage: all obj/build_sets.stamp
	cd ${MINIIMGDIR} && ${NBMAKE} live_image

qemu:
	env QEMU_AUDIO_DRV=alsa qemu-system-i386 -m 1024 -soundhw ac97 -hdachs 390,16,63,lba -hda ${MINIIMGDIR}/${ARCH}-mini.img

clean:
	${BUILDSH} -T ${TOOLDIR} -m ${ARCH} cleandir
	rm -f *~

.PHONY: clean miniimage qemu qemucurses qemugdb
