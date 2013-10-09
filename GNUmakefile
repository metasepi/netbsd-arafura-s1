# Makefile for metasepi system.
ARCH = i386
CURDIR = $(shell pwd)
TOOLDIR = obj/tooldir
RELEASEDIR = obj/releasedir
SETSDIR = ${RELEASEDIR}/${ARCH}/binary/sets
DESTDIR = obj/destdir.${ARCH}
NUMCPU = $(shell cat /proc/cpuinfo | grep -c "^processor")
BUILDSH = sh build.sh -U -N 1 -j ${NUMCPU}
NBMAKE = ${CURDIR}/${TOOLDIR}/bin/nbmake-${ARCH} -j ${NUMCPU}
NBMAKEFS = ${CURDIR}/${TOOLDIR}/bin/nbmakefs
NBCONFIG =${CURDIR}/${TOOLDIR}/bin/nbconfig
NBGCC = ${CURDIR}/${TOOLDIR}/bin/i486--netbsdelf-gcc
NBGDB = ${CURDIR}/${TOOLDIR}/bin/i486--netbsdelf-gdb
MINIIMG = ${CURDIR}/distrib/${ARCH}/liveimage/miniimage/${ARCH}-mini.img

all: sys/arch/${ARCH}/compile/GENERIC/Makefile
	cd sys/arch/${ARCH}/compile/GENERIC && ${NBMAKE}
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
	cd distrib/${ARCH}/liveimage/miniimage && ${NBMAKE} live_image

qemu:
	qemu-system-i386 -hda ${MINIIMG}

qemucurses:
	qemu-system-i386 -curses -hda ${MINIIMG}

clean:
	${BUILDSH} -T ${TOOLDIR} -m ${ARCH} cleandir
	rm -f *~

.PHONY: clean miniimage qemu qemucurses qemugdb
