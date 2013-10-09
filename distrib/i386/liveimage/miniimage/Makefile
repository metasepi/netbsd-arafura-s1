.include <bsd.own.mk>

LIVEIMGBASE=		i386-mini	# gives ${IMGBASE}.img

KERN_SET=		kern-GENERIC
BOOTDISK=		wd0		# for emulators
PRIMARY_BOOT=		bootxx_ffsv1
SECONDARY_BOOT=		boot
SECONDARY_BOOT_ARG=	# unnecessary
SETS=			base etc

USE_MBR=		yes

.include "${.CURDIR}/../../../common/bootimage/Makefile.liveimage"
