---
title: Root device mounted successfully but /bin/systemd does not exist 
description: systemd(-sysvcompat) upgrade to 204-1 causes boot failure being unable to find /bin/systemd
tags: archlinux systemd grub
---

# Problem

I've been using archlinux without issue for well over a year but a recent system upgrade, via pacman, caused boot failure and dropped in to rootfs with messages similar to:

```bash
mount /dev/mapper/lvmgroup-archroot on real root
running cleanup hook [lvm2]
running cleanup hook [udev]
ERROR - root device mounted successfully but /bin/systemd does not exist
bailing out, you are on your own. Good Luck
```

# Solution

Remove `init` parameter in grub (`/boot/grub/grub.cfg`) which references systemd.

Before:

```bash
linux   /vmlinuz-linux root=/dev/mapper/lvmgroup-archroot ro cryptdevice=/dev/md0:lvmgroup resume=/dev/mapper/lvmgroup-swap init=/bin/systemd
```

After:

```bash
linux   /vmlinuz-linux root=/dev/mapper/lvmgroup-archroot ro cryptdevice=/dev/md0:lvmgroup resume=/dev/mapper/lvmgroup-swap
```

# Details

Logs are your friend with the pacman log `/var/log/pacman.log` giving the necessary details post system update:

```bash
[2013-05-12 22:45] [PACMAN] upgraded linux (3.8.11-1 -> 3.9.2-1)
[2013-05-12 22:45] [ALPM-SCRIPTLET] ==> The /bin/systemd symlink has been removed. Any references in your
[2013-05-12 22:45] [ALPM-SCRIPTLET]     bootloader (or elsewhere) must be updated to /usr/lib/systemd/systemd.
[2013-05-12 22:45] [PACMAN] upgraded systemd (203-2 -> 204-1)
[2013-05-12 22:45] [PACMAN] upgraded systemd-sysvcompat (203-2 -> 204-1)
```

The `GRUB_CMDLINE_LINUX_DEFAULT` variable in `/etc/default/grub` was updated to remove the `init` flag:

Before:

```bash
GRUB_CMDLINE_LINUX_DEFAULT="resume=/dev/mapper/lvmgroup-swap" init=/bin/systemd
```

After:

```bash
GRUB_CMDLINE_LINUX_DEFAULT="resume=/dev/mapper/lvmgroup-swap"
```

Regenerate grub (`/boot/grub/grub.cfg`):

```bash
sudo grub-mkconfig -o /boot/grub/grub.cfg
```

Reboot!

# Background

`init` switch was orginally included in grub (`/boot/grub/grub.cfg`) when systemd was preferred (over the former SysVinit [Archlinux Boot Process Wiki]), as orginally suggested in [Archlinux Grub2 Wiki] (but no longer referenced since the information is no longer current).

I have two hard drives `/dev/sda` and `/dev/sdb` configured for raid with the largest partition encrypted via **luks** and configured with **lvm** which itself is partitioned for root and home. This is much the same way as described by [Jason Ryan lvm luks raid].

The following gives a flavour of disk partitioning at the time of the original archlinux installation (gdisk):

```bash
1 50M BIOS boot partition --maps to-- (/dev/sd[ab]1) --no raid--
2 500M boot Linux Raid --maps to-- (/dev/sd[ab]2) --and to-- (/dev/md1)
3 REST lvm Linux Raid --maps to-- (/dev/sd[ab]3) --and to-- (/dev/md0)
```

`/dev/sd[ab]2` and `/dev/sd[ab]3` are both configured for raid:

```bash
mdadm --create /dev/md0 --level=1 --raid-devices=2 /dev/sd[ab]3
mdadm --create /dev/md1 --level=1 --raid-devices=2 /dev/sd[ab]2
```

but it is only `/dev/sd[ab]3` which is encrypted via **luks** and configured for **lvm** with partitions for root, swap and home.

The following summaries:

```bash
root /dev/mapper/lvmgroup-archroot
home /dev/mapper/lvmgroup-archhome
boot /dev/md1
```

or:

<pre class="terminal">
<span id="prompt">prompt</span> > df -h

Filesystem                     Size  Used Avail Use% Mounted on
/dev/mapper/lvmgroup-archroot   20G   17G  2.6G  87% /
dev                            1.5G     0  1.5G   0% /dev
run                            1.5G  9.1M  1.5G   1% /run
tmpfs                          1.5G  2.1M  1.5G   1% /dev/shm
tmpfs                          1.5G     0  1.5G   0% /sys/fs/cgroup
tmpfs                          1.5G  3.4M  1.5G   1% /tmp
/dev/md1                       484M   37M  423M   9% /boot
/dev/mapper/lvmgroup-archhome  558G  208G  323G  40% /home
</pre>

## Recovery Details

To make the necessary updates to grub (`/boot/grub/grub.cfg`) and be able to complete the boot process, get a recovery 'disk'/bootable ISO (on a USB), interrupt bios boot (F12 on my Dell), boot from external USB media until the recovery shell emerges.

De-crypt `/dev/sda3` with **luks** (enter password when prompted):

```bash
cryptsetup luksOpen /dev/sda3 lvm
```

Verify and activate the **lvm** partitions:

```bash
lvm lvscan
vgchange -ay lvmgroup
```

mount and chroot:

```bash
mount /dev/mapper/lvmgroup-archroot /mnt
mount /dev/mapper/lvmgroup-archhome /mnt/home
mount /dev/md1 /mnt/boot
chroot /mnt
```
Complete the updates described in the solution above, umount and reboot.

# Aside

If you have followed the above instruction but failed to install systemd-sysvcompat then you will get an error typically:

```bash
WARNING: Failed to connect to lvmetad: No such file or directory. Falling back to internal scanning
```
When I experienced this issue, no systemd services started and I was dropped into the (virtual) console.
Install systemd-sysvcompat as recommended to correct the error.

# Acknowledgements

The above information mirrors this post by [Gaijin Nippon] which I stumbled upon as I got half way through writing this contribution.

[Gaijin Nippon]: http://gaijin-nippon.blogspot.co.uk/2013/05/heed-this-warning-about-binsystemd.html
[Archlinux Grub2 Wiki]: https://wiki.archlinux.org/index.php/GRUB2
[Archlinux Boot Process Wiki]: https://wiki.archlinux.org/index.php/Arch_Boot_Process
[Jason Ryan lvm luks raid]: http://jasonwryan.com/blog/2012/02/11/lvm/
