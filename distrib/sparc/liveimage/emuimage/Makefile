#	$NetBSD: Makefile,v 1.1.2.1 2012/08/17 20:50:51 martin Exp $

LIVEIMGBASE=	NetBSD-${DISTRIBVER}-sparc-live-sd0root	# gives ${IMGBASE}.img

.include <bsd.own.mk>

KERN_SET=		kern-GENERIC
BOOTDISK=		sd0
PRIMARY_BOOT=		bootxx
SECONDARY_BOOT=		boot
SECONDARY_BOOT_ARG=	/${SECONDARY_BOOT}

USE_SUNLABEL=		yes

.include "${.CURDIR}/../../../common/bootimage/Makefile.liveimage"
