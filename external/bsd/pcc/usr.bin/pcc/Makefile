#	$NetBSD: Makefile,v 1.8 2011/09/01 13:00:15 plunky Exp $

WARNS?=	2

.include <bsd.init.mk>

.PATH: ${PCC_DIST}/cc/cc

#
# we build PCC cc(1) as pcc(1) to avoid conflicts with GCC
#

PROG=	pcc
SRCS=	cc.c
MAN=	pcc.1

CPPFLAGS+=	-DTARGOS=\"${TARGOS}\"
CPPFLAGS+=	-DTARGMACH=\"${TARGMACH}\"
CPPFLAGS+=	-DLIBEXECDIR=\"/usr/libexec/\"
CPPFLAGS+=	-DINCLUDEDIR=\"/usr/include/\"
CPPFLAGS+=	-I${PCC_DIST}/mip
CPPFLAGS+=	-I${PCC_DIST}/os/${TARGOS}
CPPFLAGS+=	-I${PCC_DIST}/arch/${TARGMACH}

pcc.1:	cc.1
	${TOOL_SED} -e "s,Nm cc,Nm pcc,"	\
		    -e "s,Dt CC,Dt PCC,"	\
		    -e "s,Xr cpp,Xr pcpp,"	\
		    ${.ALLSRC} > ${.TARGET}

CLEANFILES+=	pcc.1

.include <bsd.prog.mk>
