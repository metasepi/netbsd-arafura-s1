#	$NetBSD: Makefile,v 1.7 2011/08/11 10:52:12 uch Exp $
#

.include <bsd.own.mk>

LIB=	vfstest
SRCS=	fstest_ext2fs.c fstest_ffs.c fstest_lfs.c fstest_msdosfs.c	\
	fstest_nfs.c fstest_puffs.c fstest_rumpfs.c fstest_sysvbfs.c	\
	fstest_tmpfs.c fstest_v7fs.c

WARNS=	3

.PATH: ${.CURDIR}/nfsrpc
.include "nfsrpc/Makefile.inc"

LIBISPRIVATE=	# yup

# NFS client stuff
.PATH:	${NETBSDSRCDIR}/sbin/mount_nfs ${NETBSDSRCDIR}/sbin/mount
SRCS+=	getnfsargs.c pathadj.c fattr.c mount_nfs.c

CPPFLAGS+=	-I${NETBSDSRCDIR}/sbin/mount_nfs -I${NETBSDSRCDIR}/sbin/mount
CPPFLAGS+=	-DMOUNT_NOMAIN -DMOUNTNFS_RETRYRPC=1

.include "../../../libexec/lfs_cleanerd/Makefile.inc"
CPPFLAGS+=	-DUSE_RUMP -DLFS_CLEANER_AS_LIB

.include <bsd.lib.mk>
