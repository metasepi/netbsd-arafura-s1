# NetBSD arafura - season 1 [![Build Status](https://travis-ci.org/metasepi/netbsd-arafura-s1.png)](https://travis-ci.org/metasepi/netbsd-arafura-s1)

## How to build

Get your own Debian PC.

    $ uname -a
    Linux casper 3.11-2-amd64 #1 SMP Debian 3.11.8-1 (2013-11-13) x86_64 GNU/Linux

And git clone me.

    $ git clone https://github.com/metasepi/netbsd-arafura-s1.git
    $ cd netbsd-arafura-s1

Build QEMU image.

    $ make bootcd

## Run Metasepi kernel

Run QEMU.

    $ make qemu

You might see new QEMU window. Kick play.sh script, if boot ok.

    >> NetBSD/x86 BIOS Boot, Revision 5.9 (from NetBSD 6.1.1_PATCH)
    >> Memory: 639/1047544 k
    --snip--
    Created tmpfs /dev (1490944 byte, 2880 inodes)
    erase ^?, werase ^W, kill ^U, intr ^C
    This image contains utilities which may be needed
    to get you out of a pinch.
    # ./play.sh

Enjoy your wav sounds!

## License

### Under metasepi directory

Copyright (c) 2013 Metasepi term.

These files are free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

http://www.gnu.org/licenses/old-licenses/gpl-2.0.html

### Music located at metasepi/sound/Epopsan-signal.mp3

Copyright (c) 2011 Epopsan.

This work is licensed under a Creative Commons Attribution-ShareAlike 3.0 Unported License.

http://creativecommons.org/licenses/by-sa/3.0/

Following links to know about Epopsan and the music.

* http://www.jamendo.com/en/track/771128/signal
* http://www.jamendo.com/en/artist/368504/epopsan
* https://twitter.com/epopsan

### The others

```
/*-
 * Copyright (c) 2008 The NetBSD Foundation, Inc.
 * All rights reserved.
 *
 * This code is derived from software contributed to The NetBSD Foundation
 * by 
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE NETBSD FOUNDATION, INC. AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE FOUNDATION OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
```

http://netbsd.org/about/redistribution.html
