.include <bsd.own.mk>

LIVEIMGBASE=		i386-mini	# gives ${IMGBASE}.img

KERN_SET=		kern-GENERIC
BOOTDISK=		wd0		# for emulators
PRIMARY_BOOT=		bootxx_ffsv1
SECONDARY_BOOT=		boot
SECONDARY_BOOT_ARG=	# unnecessary
SETS=			base etc

IMAGEMB=		160
SWAPMB=			1
HEADS=			16
SECTORS=		63
MBRHEADS=		16
MBRSECTORS=		63
IMGMAKEFSOPTIONS=

USE_MBR=		yes
FSTAB_IN=		./fstab_cd.in

.include "${.CURDIR}/../../../common/bootimage/Makefile.liveimage"
