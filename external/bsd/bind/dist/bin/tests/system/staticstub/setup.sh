#!/bin/sh
#
# Copyright (C) 2010, 2012  Internet Systems Consortium, Inc. ("ISC")
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND ISC DISCLAIMS ALL WARRANTIES WITH
# REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS.  IN NO EVENT SHALL ISC BE LIABLE FOR ANY SPECIAL, DIRECT,
# INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
# LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
# OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
# PERFORMANCE OF THIS SOFTWARE.

# Id: setup.sh,v 1.3 2010/12/17 00:57:38 marka Exp 

sed 's/SERVER_CONFIG_PLACEHOLDER/server-names { "ns.example.net"; };/' ns2/named.conf.in > ns2/named.conf

sed 's/EXAMPLE_ZONE_PLACEHOLDER/zone "example" { type master; file "example.db.signed"; };/' ns3/named.conf.in > ns3/named.conf

../../../tools/genrandom 400 random.data

cd ns3 && sh -e sign.sh
