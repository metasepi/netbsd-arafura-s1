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
	cd sys/arch/${ARCH}/compile/GENERIC && ${NBMAKE}

sys/arch/${ARCH}/compile/GENERIC/Makefile: sys/arch/${ARCH}/conf/GENERIC
	cd sys/arch/${ARCH}/conf && ${NBCONFIG} GENERIC

### Build and install NetBSD tools.
${TOOLDIR}/build.stamp:
	env MKCROSSGDB=yes ${BUILDSH} -T ${TOOLDIR} -m ${ARCH} tools
	touch ${TOOLDIR}/build.stamp

live-image: ${TOOLDIR}/build.stamp
	${BUILDSH} -T ${TOOLDIR} -m ${ARCH} release
	${BUILDSH} -T ${TOOLDIR} -m ${ARCH} live-image

### Run bootimage on qemu
bootimage/boot.iso: all
	mkdir -p bootimage
	cd bootimage && rm -rf cdrom
	cd bootimage && mkdir -p cdrom
	cp sys/arch/${ARCH}/stand/boot/biosboot/boot bootimage/cdrom/
	cp sys/arch/${ARCH}/compile/GENERIC_TINY/netbsd bootimage/cdrom/
	gzip bootimage/cdrom/netbsd
	cp sys/arch/${ARCH}/stand/cdboot/bootxx_cd9660 bootimage/
	cd bootimage && qemu-img create boot_tmp.iso 16M
	cd bootimage && ${NBMAKEFS} -t cd9660 -o 'bootimage=${ARCH};bootxx_cd9660,no-emul-boot' boot_tmp.iso cdrom
	cd bootimage && mv boot_tmp.iso boot.iso

qemu: bootimage/boot.iso
	qemu-system-i386 -cdrom bootimage/boot.iso

qemucurses: bootimage/boot.iso
	qemu-system-i386 -curses -cdrom bootimage/boot.iso

qemugdb: bootimage/boot.iso
	${NBGDB} -x metasepi-arafura/gdb.boot

clean:
	cd sys/arch/${ARCH}/stand/cdboot && ${NBMAKE} clean
	cd sys/arch/${ARCH}/stand/boot/biosboot && ${NBMAKE} clean
	rm -f *~
	rm -rf bootimage

.PHONY: clean qemu qemucurses qemugdb live-image
