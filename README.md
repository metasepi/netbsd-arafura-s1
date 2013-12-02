# NetBSD arafura season 1 [![Build Status](https://travis-ci.org/metasepi/netbsd-arafura-s1.png)](https://travis-ci.org/metasepi/netbsd-arafura-s1)

## How to build

Get your own Debian PC.

    $ uname -a
    Linux casper 3.11-2-amd64 #1 SMP Debian 3.11.8-1 (2013-11-13) x86_64 GNU/Linux

And git clone me.

    $ git clone https://github.com/metasepi/netbsd-arafura-s1.git
    $ cd netbsd-arafura-s1

Setup your own sound file to play on Metasepi kernel.

    $ file ~/test.wav
    /home/USSR/test.wav: RIFF (little-endian) data, WAVE audio, Microsoft PCM, 16 bit, stereo 48000 Hz
    $ make setup WAV=~/test.wav

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
