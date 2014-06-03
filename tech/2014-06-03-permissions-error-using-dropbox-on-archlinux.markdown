---
title: Permissions Error with Dropbox client on Archlinux
description: archlinux dropbox client stopped working with permissions error but in reality disk space is full
tags: permissions error arch linux dropbox pacman cache disk full
---

# Problem

I have `dropbox` installed on `archlinux` as prescribed in the [Archlinux Wiki].
A week ago, the client (version 2.6.x) stopped working (failed to sync). Traditionally a new
update usually solves most issues but version 2.8.3 was released (installed via [AUR])
and brought no such relief.

# Solution

Create sufficient space on your harddrive!

# Details

Checking the `dropbox` service status via `systemd`

<pre class="terminal">
<span id="prompt">ian</span> systemctl status dropboxd.service 

dropboxd.service - Dropbox
   Loaded: loaded (/etc/systemd/system/dropboxd.service; enabled)
   Active: failed (Result: exit-code) since Tue 2014-06-03 08:25:36 BST; 5min ago
  Process: 1212 ExecStart=/usr/bin/dropboxd (code=exited, status=255)
 Main PID: 1212 (code=exited, status=255)

Jun 03 08:25:32 laptop systemd[1]: Starting Dropbox...
Jun 03 08:25:32 laptop systemd[1]: Started Dropbox.
Jun 03 08:25:36 laptop dropboxd[1212]: Couldn't start Dropbox.
Jun 03 08:25:36 laptop dropboxd[1212]: This is usually because of a permissions error. Storing your home folder on a network share can also cause an error.
Jun 03 08:25:36 laptop dropboxd[1212]: Get more help at https://www.dropbox.com/c/help/permissions_error
Jun 03 08:25:36 laptop dropboxd[1212]: Please contact Dropbox support with the following info for help:
Jun 03 08:25:36 laptop dropboxd[1212]: /tmp/dropbox_errorlUH3rW.txt
Jun 03 08:25:36 laptop systemd[1]: dropboxd.service: main process exited, code=exited, status=255/n/a
Jun 03 08:25:36 laptop systemd[1]: Unit dropboxd.service entered failed state.
</pre>

Note the `permissions error` and [Dropbox Help Wiki] reference. Locate the
`For our advanced users` section for correct permissions configuration.

I rechecked the permissions but `dropbox` has been working fine for me for some time now, so
I discounted this as the cause.

The error log referenced (`/tmp/dropbox_errorlUH3rW.txt`) in the above trace, nails the real cause

```bash
bn.BUILD_KEY: Dropbox
bn.VERSION: 2.8.3
bn.DROPBOXEXT_VERSION: failed
bn.is_frozen: True
pid: 3762
ppid: 3153
ppid exe: '/usr/bin/bash'
uid: 1000
user_info: pwd.struct_passwd(pw_name='ian', pw_passwd='x', pw_uid=1000, pw_gid=100, pw_gecos='', pw_dir='/home/ian', pw_shell='/bin/bash')
effective_user_info: pwd.struct_passwd(pw_name='ian', pw_passwd='x', pw_uid=1000, pw_gid=100, pw_gecos='', pw_dir='/home/ian', pw_shell='/bin/bash')
euid: 1000
gid: 100
egid: 100
group_info: grp.struct_group(gr_name='users', gr_passwd='x', gr_gid=100, gr_mem=[])
effective_group_info: grp.struct_group(gr_name='users', gr_passwd='x', gr_gid=100, gr_mem=[])
LD_LIBRARY_PATH: None
cwd: '/home/ian'
     real_path='/home/ian'
           	mode=040700	uid=1000	gid=100
     parent	mode=040755	uid=0	gid=0
HOME: u'/home/ian'
appdata: u'/home/ian/.dropbox'
         real_path=u'/home/ian/.dropbox'
               	mode=040700	uid=1000	gid=100
         parent	mode=040700	uid=1000	gid=100
dropbox_path: u'/home/ian/Dropbox'
              real_path=u'/home/ian/Dropbox'
                    	mode=040755	uid=1000	gid=100
              parent	mode=040700	uid=1000	gid=100
sys_executable: '/opt/dropbox/dropbox'
                real_path='/opt/dropbox/dropbox'
                      	mode=0100755	uid=0	gid=0
                parent	mode=040755	uid=0	gid=0
trace.__file__: '/opt/dropbox/library.zip/dropbox/boot_error.pyc'
                real_path='/opt/dropbox/dropbox/dropbox/boot_error.pyc'
                      	not found
                parent	not found
tempdir: '/tmp'
         real_path='/tmp'
               	mode=041777	uid=0	gid=0
         parent	mode=040755	uid=0	gid=0
Traceback (most recent call last):
  File "dropbox/client/main.py", line 2135, in main_startup
  File "dropbox/client/main.py", line 1157, in run
  File "dropbox/client/main.py", line 494, in startup_low
  File "dropbox/client/multiaccount/instance_database.py", line 1541, in get_info_by_instance_id
  File "dropbox/client/multiaccount/instance_database.py", line 1467, in get_master_row
  File "dropbox/sqlite3_helpers.py", line 575, in _runner
  File "dropbox/client/multiaccount/instance_database.py", line 665, in _create_row
  File "dropbox/sqlite3_helpers.py", line 105, in execute
OperationalError: database or disk is full
```
See the `Traceback` and reference to `full disk `

The issue for me is that although I have a `500G` drive, `/home` takes up `480G` of
this, leaving `20G` for the `archlinux` installation (in hindsight, more space allocation
would have been prudent)

A visualisation of space allocation can be seen via [Baobab]

![Baobab](/images/tech/baobab-dropbox-permissions-error.png)

Alternatively, using `du` gives:

<pre class="terminal">
<span id="prompt">ian</span> sudo du -h --max-depth=1 /

9.9M	/run
12K	/srv
20K	/tmp
0	/proc
416M	/opt
0	/dev
16K	/lost+found
0	/sys
4.0K	/media
8.0K	/mnt
76K	/root
13G	/usr
269G	/home
39M	/boot
5.6G	/var
15M	/etc
288G	/
</pre>


Further breakdown of `/usr`

<pre class="terminal">
<span id="prompt">ian</span> sudo du -h --max-depth=2 /usr | sort -rh

13G	/usr
5.7G	/usr/share
5.3G	/usr/lib
1.3G	/usr/bin
1.2G	/usr/share/doc
1.1G	/usr/share/texmf-dist
597M	/usr/share/netbeans
552M	/usr/lib/python2.7
502M	/usr/lib/ghc-7.6.3
429M	/usr/include
334M	/usr/share/intellijidea-ce
327M	/usr/lib/python3.4
323M	/usr/share/locale
317M	/usr/lib/libreoffice
239M	/usr/share/racket
225M	/usr/lib/go
222M	/usr/share/eclipse
221M	/usr/share/icons
208M	/usr/share/processing
161M	/usr/lib/mono
156M	/usr/share/gtk-doc
156M	/usr/lib/virtualbox
149M	/usr/lib/chromium
129M	/usr/lib/vtk-5.10
127M	/usr/lib/rustlib
126M	/usr/lib/modules
119M	/usr/lib/ocaml
118M	/usr/lib/erlang
115M	/usr/include/boost
95M	/usr/lib/jvm
93M	/usr/share/fonts
91M	/usr/share/man
87M	/usr/share/emacs
75M	/usr/lib/gcc
</pre>

and `/var'

<pre class="terminal">
<span id="prompt">ian</span> sudo du -h --max-depth=2 /var | sort -rh

5.6G	/var
3.2G	/var/cache/pacman
3.2G	/var/cache
1.7G	/var/lib
1.2G	/var/lib/mongodb
647M	/var/log
641M	/var/log/journal
232M	/var/lib/mlocate
79M	/var/lib/postgres
75M	/var/lib/texmf
71M	/var/lib/pacman
60M	/var/abs
31M	/var/lib/mysql

</pre>

Note that `pacman cache` is quite large but on most `pacman` and `yaourt`
([Yaourt]) updates I always purge the cache;

```bash
alias pclean='sudo pacman -Syuc'
alias yclean='yaourt -Sc'
```

To find out which are your larger programs using space, run `pacsysclean`

<pre class="terminal">
<span id="prompt">ian</span> pacsysclean

....
62.85 MiB	linux
63.64 MiB	firefox
64.36 MiB	gimp
64.55 MiB	linux-firmware
66.09 MiB	vagrant
67.84 MiB	llvm
73.20 MiB	jre7-openjdk-headless
73.38 MiB	jmeter
77.10 MiB	virtualbox
77.25 MiB	inkscape
80.21 MiB	qt4
82.82 MiB	calibre
83.01 MiB	gcc
83.93 MiB	python
84.46 MiB	mysql
90.98 MiB	emacs
94.51 MiB	maxima
104.41 MiB	erlang
115.13 MiB	webkitgtk
119.42 MiB	clang
126.27 MiB	rust
129.07 MiB	blender
129.55 MiB	boost
146.39 MiB	vtk
148.13 MiB	chromium
148.29 MiB	mono
176.29 MiB	qemu
177.32 MiB	ocaml
195.06 MiB	processing
217.57 MiB	eclipse
246.09 MiB	libreoffice-common
266.65 MiB	mongodb
267.18 MiB	texlive-core
277.57 MiB	go
330.52 MiB	intellij-idea-community-edition
344.41 MiB	libreoffice-sdk-doc
363.79 MiB	racket
395.64 MiB	scala-docs
486.36 MiB	texlive-fontsextra
536.66 MiB	ghc
578.93 MiB	netbeans

</pre>

Remove large and unwanted applications.

Upon reboot, the `dropbox` service starts and the computer needs to be
linked once again by following the link given and logging into your account

<pre class="terminal">
<span id="prompt">ian</span> systemctl status dropboxd.service
dropboxd.service - Dropbox
   Loaded: loaded (/etc/systemd/system/dropboxd.service; enabled)
   Active: active (running) since Tue 2014-06-03 12:17:02 BST; 46s ago
 Main PID: 2759 (dropbox)
   CGroup: /system.slice/dropboxd.service
           └─2759 /opt/dropbox/dropbox

Jun 03 12:17:35 laptop dropboxd[2759]: This computer isn't linked to any Dropbox account...
Jun 03 12:17:35 laptop dropboxd[2759]: Please visit https://www.dropbox.com/cli_link?host_id=96ab281c5f27c33b4cef1 to link this device.
Jun 03 12:17:41 laptop dropboxd[2759]: This computer isn't linked to any Dropbox account...
Jun 03 12:17:41 laptop dropboxd[2759]: Please visit https://www.dropbox.com/cli_link?host_id=96ab281c5f27c33b4cef1 to link this device.
</pre>

[AUR]:https://aur.archlinux.org/packages/dropbox/
[Archlinux Wiki]:https://wiki.archlinux.org/index.php/dropbox
[Dropbox Help Wiki]:https://www.dropbox.com/help/72/en
[Baobab]:https://wiki.gnome.org/Apps/Baobab
[Yaourt]:https://wiki.archlinux.org/index.php/yaourt
