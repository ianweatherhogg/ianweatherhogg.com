---
title: Permission Denied Error using Rsync over SSH to a VPS with Cron
description: cron usage introduces permission denied error with a working rsync script used to backup files over ssh
tags: permission denied public key keychain cron rsync vps ssh
---
# Problem

I have an `rsync` script, which when manually executed, successfully backs up my
important files to a [VPS] over `ssh`.

In an attempt to replicate this [post](http://jasonwryan.com/blog/2011/11/28/rsync/)
and use `cron`, with which to automate the script execution, I kept getting
`Permission Denied` errors.

# Solution

`cron` needs to be aware of `ssh-agent` environment variables as described by the
[serverfault] solution.

# Details
## Context

I have a simple Digital Ocean Backup (`dob`) script

```bash
/usr/bin/rsync --checksum -ave ssh /home/ian/Dropbox/org/ ian:Documents/org/
```
The `ian` in `ian:Documents/org` is an alias defined in `.ssh/config`

```
Host ian
    HostName ianweatherhogg.com
    IdentityFile ~/.ssh/id_rsa_laptop
    User ian
    Port 45678
```
The `ssh` keys are handled via [keychain], invoked on startup:

```bash
eval $(keychain --eval --quiet)
```
This is called from `.xinitrc`[^XI] (or use `.bash_profile`, `.bashrc` etc)

`keychain` drives `ssh-agent` (and `gpg-agent`) to which the private key is added,
with `ssh-add`:

```bash
ssh-add ~/.ssh/id_rsa_laptop
```
Manually executing the `dob` script, successfully backs everything up over `ssh`

## Cron

To automate the backup on a more regular basis I looked into using `cron`[^CR]
and set up `crontab`

```
EDITOR=nano crontab -e
```
and including

```
* */1 * * * ~/bin/dob 
```
which states that the `dob` script will be called every hour.

Exiting from `nano`, installed the new `crontab`

Notes

1. Including the contents of the `dob` script directly into the `crontab` as
a 'one liner' in lieu of reference the script, has the same net effect

```
* */1 * * * /usr/bin/rsync --checksum -ave ssh /home/ian/Dropbox/org/ ian:Documents/org/
```

2. Full pathnames have been included above, but I didn't have an issue with shortened ones:

      1. `rsync` instead of `/usr/bin/rsync`
	  2. `~/bin/dob` instead of `/home/ian/bin/dob`

3. I explicitly exported `nano`[^EX], since normally I have my editor set as
   `EDITOR=emacsclient -n`. This caused issues with me since the `crontab` would not be
   installed (probably since the `-n` flag causes `emacs` to return immediately - see
   `man` page for details). This frustrated me for a while since my normal means of
   file editing from the terminal failed to create a `crontab`

Start the `cron` system[^AR]

```bash
sudo systemctl start cronie
```
and investigate the status

<pre class="terminal">
<span id="prompt">ian</span> systemctl status cronie.service
systemctl status cronie.service
cronie.service - Periodic Command Scheduler
   Loaded: loaded (/usr/lib/systemd/system/cronie.service; disabled)
   Active: active (running) since Sat 2014-07-26 06:30:18 BST; 22min ago
 Main PID: 10429 (crond)
   CGroup: /system.slice/cronie.service
           └─10429 /usr/bin/crond -n

Jul 26 06:50:02 laptop CROND[11615]: (ian) CMDOUT (rsync: connection unexpectedly closed (0 bytes received so far) [sender])
Jul 26 06:51:01 laptop crond[11657]: pam_unix(crond:session): session opened for user ian by (uid=0)
Jul 26 06:51:01 laptop CROND[11658]: (ian) CMD (rsync --checksum -ave ssh $HOME/Dropbox/org/* ian:Documents/org/)
Jul 26 06:51:02 laptop CROND[11657]: [46B blob data]
Jul 26 06:51:02 laptop CROND[11657]: (ian) CMDOUT (rsync: connection unexpectedly closed (0 bytes received so far) [sender])
Jul 26 06:52:01 laptop crond[11700]: pam_unix(crond:session): session opened for user ian by (uid=0)
Jul 26 06:52:01 laptop CROND[11701]: (ian) CMD (rsync --checksum -ave ssh $HOME/Dropbox/org/* ian:Documents/org/)
Jul 26 06:52:02 laptop CROND[11700]: [46B blob data]
Jul 26 06:52:02 laptop CROND[11700]: (ian) CMDOUT (rsync: connection unexpectedly closed (0 bytes received so far) [sender])
Jul 26 06:52:02 laptop CROND[11700]: (ian) CMDOUT (rsync error: unexplained error (code 255) at io.c(226) [sender=3.1.1])
</pre>

Note the error message at the end of the trace.

The following script modification allows debug to file

```
* */1 * * * ~/bin/dob >/tmp/cron_output.log 2>&1
```
Giving

```
Permission denied (publickey).
rsync: connection unexpectedly closed (0 bytes received so far) [sender]
rsync error: unexplained error (code 255) at io.c(226) [sender=3.1.1]
```

The answer suggested by [serverfault] informs that `crontab` is unaware
of the `ssh-agent` variables and must be included explicitly 

```
* */1 * * * . ~/.keychain/`/bin/hostname`-sh && ~/bin/dob >/tmp/cron_output.log 2>&1
```
where `laptop` is my hostname.

Note:
<pre class="terminal">
<span id="prompt">ian</span> tree .keychain
.keychain/
├── laptop-csh
├── laptop-csh-gpg
├── laptop-fish
├── laptop-fish-gpg
├── laptop-sh
└── laptop-sh-gpg
</pre>

This finally got the backup script working

[^XI]: I use `i3` desktop/window manager, and the user session begins when the system calls `.xinitrc`
[^CR]: I referred to this [post](http://blog.davidsingleton.org/raspberry-pi-webcam-a-gentle-intro-to-crontab/) and this [post](https://www.digitalocean.com/community/tutorials/how-to-create-an-off-site-backup-of-your-site-with-rsync-on-centos-6) to get a grasp on the `cron` utility 
[^EX]: This tip I found whilst reading [this](https://www.digitalocean.com/community/tutorials/how-to-create-an-off-site-backup-of-your-site-with-rsync-on-centos-6)
[^AR]: I'm using Arch Linux and services are managed by `systemd`[archlinuxcron]

[VPS]:https://www.digitalocean.com/
[serverfault]:http://serverfault.com/questions/92683/execute-rsync-command-over-ssh-with-an-ssh-agent-via-crontab#answer-236437
[keychain]:https://wiki.archlinux.org/index.php/SSH_keys
[archlinuxcron]:https://wiki.archlinux.org/index.php/cron
