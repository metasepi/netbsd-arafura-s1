#	$NetBSD: Makefile,v 1.3 2010/02/16 20:42:45 pooka Exp $
#

.PATH:	${.CURDIR}/../../../../netsmb

LIB=	rumpdev_netsmb

SRCS=	iconv.c smb_conn.c smb_crypt.c smb_dev.c smb_iod.c smb_rq.c	\
	smb_smb.c smb_subr.c smb_trantcp.c smb_usr.c subr_mchain.c

SRCS+=	component.c

CPPFLAGS+=	-I${RUMPTOP}/librump/rumpvfs
#CPPFLAGS+=	-DSMB_SOCKET_DEBUG -DSMB_IOD_DEBUG

.include <bsd.lib.mk>
.include <bsd.klinks.mk>
