IMGBASE=	NetBSD-${DISTRIBVER}-i386-mini-wd0root	# gives ${IMGBASE}.img

BOOTDISK=	wd0			# for QEMU etc.
SETS= # xxx NEED xxx
IMAGEMB?=	64		# 64MB
SWAPMB?=	1		# 1MB

.include "${.CURDIR}/../../../common/bootimage/Makefile.bootimage"

all: ${IMGBASE}.img
