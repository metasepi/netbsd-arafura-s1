# Makefile for metasepi system.
ARCH = i386
CURDIR = $(shell pwd)
TOOLDIR = obj/tooldir
NUMCPU = $(shell cat /proc/cpuinfo | grep -c "^processor")
BUILDSH = sh build.sh -U -N 1 -j ${NUMCPU}
NBMAKE = ${CURDIR}/${TOOLDIR}/bin/nbmake-${ARCH} -j ${NUMCPU}
NBMAKEFS = ${CURDIR}/${TOOLDIR}/bin/nbmakefs
NBCONFIG =${CURDIR}/${TOOLDIR}/bin/nbconfig
NBGCC = ${CURDIR}/${TOOLDIR}/bin/i486--netbsdelf-gcc
NBGDB = ${CURDIR}/${TOOLDIR}/bin/i486--netbsdelf-gdb

all: ${TOOLDIR}/build.stamp sys/arch/${ARCH}/compile/GENERIC/Makefile
	cd sys/arch/${ARCH}/stand/mbr/mbr             && ${NBMAKE}
	cd sys/arch/${ARCH}/stand/bootxx/bootxx_ffsv1 && ${NBMAKE}
	cd sys/arch/${ARCH}/stand/boot/biosboot       && ${NBMAKE}
	cd etc                                        && ${NBMAKE} MAKEDEV
	cd sys/arch/${ARCH}/compile/GENERIC           && ${NBMAKE}

sys/arch/${ARCH}/compile/GENERIC/Makefile: sys/arch/${ARCH}/conf/GENERIC
	cd sys/arch/${ARCH}/conf && ${NBCONFIG} GENERIC

### Build and install NetBSD tools.
${TOOLDIR}/build.stamp:
	env MKCROSSGDB=yes ${BUILDSH} -T ${TOOLDIR} -m ${ARCH} tools
	touch ${TOOLDIR}/build.stamp

### Run bootimage on qemu
bootimage/boot.img: all
	mkdir -p bootimage
	cd bootimage && rm -rf work
	cd bootimage && mkdir -p work
	cp sys/arch/${ARCH}/stand/boot/biosboot/boot bootimage/work/
	cp sys/arch/${ARCH}/compile/GENERIC/netbsd   bootimage/work/
	cd bootimage && mv boot_tmp.img boot.img

qemu: bootimage/boot.img
	qemu-system-i386 -hda bootimage/boot.img

qemucurses: bootimage/boot.img
	qemu-system-i386 -curses -hda bootimage/boot.img

qemugdb: bootimage/boot.img
	${NBGDB} -x metasepi-arafura/gdb.boot

clean:
	cd sys/arch/${ARCH}/stand/boot/biosboot && ${NBMAKE} clean
	rm -f *~
	rm -rf bootimage

.PHONY: clean qemu qemucurses qemugdb
