---
title: creating a hakyll static web site
description: creating a static hakyll web site
tags: haskell hakyll
---

# Problem

<pre class="terminal">
<span id="prompt">prompt</span> > vagrant up

Bringing machine 'nimbus' up with 'virtualbox' provider...
Bringing machine 'supervisor1' up with 'virtualbox' provider...
Bringing machine 'supervisor2' up with 'virtualbox' provider...
Bringing machine 'zookeeper1' up with 'virtualbox' provider...
[nimbus] Importing base box 'precise64'...
[nimbus] Matching MAC address for NAT networking...
[nimbus] Setting the name of the VM...
[nimbus] Clearing any previously set forwarded ports...
[nimbus] Clearing any previously set network interfaces...
There was an error while executing `VBoxManage`, a CLI used by Vagrant
for controlling VirtualBox. The command and stderr is shown below.

Command: ["hostonlyif", "create"]

Stderr: 0%...
Progress state: NS_ERROR_FAILURE
VBoxManage: error: Failed to create the host-only adapter
VBoxManage: error: VBoxNetAdpCtl: Error while adding new interface: failed to open /dev/vboxnetctl: No such file or directory
VBoxManage: error: Details: code NS_ERROR_FAILURE (0x80004005), component HostNetworkInterface, interface IHostNetworkInterface
VBoxManage: error: Context: "int handleCreate(HandlerArg*, int, int*)" at line 66 of file VBoxManageHostonly.cpp

</pre>

[Virtual Box Forum] suggests that happens on **guest network devices has Bridged selected** and continues to say **On the plus
side, it appears that NAT and Internal Network options work fine - though I haven't done much testing with Internal Network.
With NAT selected, the VM boots without errors and is able to connect to the internet and operate just fine; however, I am unable
to connect locally to the VM. Which is why Bridged or Host-only would be nice to have** (error in the context of Mac Maverick)

[Virtual Box Forum]: https://forums.virtualbox.org/viewtopic.php?f=8&t=56013


# Solution

As stated in the [Archlinux Wiki]
Failed to create the host-only network interface
To be able to create a Host-Only Network Adapter or a Bridged Network Adapter, the kernel modules vboxnetadp and vboxnetflt need to be loaded, you also need to make sure the net-tools package is installed. You can load these kernel modules manually with

```bash
sudo modprobe -a vboxdrv vboxnetadp vboxnetflt
```

[Archlinux Wiki]: https://wiki.archlinux.org/index.php/Virtualbox#Failed_to_create_the_host-only_network_interface


