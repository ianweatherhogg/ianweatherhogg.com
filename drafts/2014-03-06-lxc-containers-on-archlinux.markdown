---
title: creating a hakyll static web site
description: creating a static hakyll web site
tags: haskell hakyll
---

[Max Gonzih] blog

<pre class="terminal">
<span id="prompt">prompt</span> > cat /etc/netctl/lxcbridge

Description="LXC Bridge connection"
Interface=lxcbr0
Connection=bridge
BindsToInterfaces=()
IP=static
Address=192.168.100.1/24
FwdDelay=0

</pre>

Make sure you have dnsmasq installed.

1
pacman -S dnsmasq
Run interface.

1
sudo netctl start lxcbridge
And enable it for startup

1
sudo netctl enable lxcbridge
Add iptables rule:

1
sudo iptables -t nat -A POSTROUTING -o <you-main-ethernet-device-here> -j MASQUERADE
Save iptables (as superuser):

1
iptables-save > /etc/iptables/iptables.rules
Enable ip_forward:

1
sudo sysctl net.ipv4.ip_forward=1
Or to save forwarding persisent add following line:

/etc/sysctl.conf
1
net.ipv4.ip_forward=1
Create new Ubuntu container:

1
sudo lxc-create -n playtime -t ubuntu
It will create new root filesystem with new configuration. Lets now change conatiner’s configuration

It will create new root filesystem with new configuration. Lets now change conatiner’s configuration:

/var/lib/lxc/playtime/config
1
2
3
4
5
lxc.network.type = veth
lxc.network.flags = up
lxc.network.link = lxcbr0
lxc.network.hwaddr = 00:FF:AA:00:00:01
lxc.network.ipv4 = 192.168.100.10/24
Lets boot our conatiner. Better use screen or tmux for it.

1
sudo lxc-start -n playtime
Default user and password for ubuntu template is ubuntu.

Login to container and add default gateway configuration on network up:

/etc/network/if-up.d/routes

#! /bin/sh

route add default gw 192.168.100.1

exit 0
Reboot container with sudo reboot in it.

Now you can ssh to your container ssh ubuntu@192.168.100.10. You can also run container as daemon sudo lxc-start -n playtime -d. And you can shutdown container with sudo shutdown inside container.

[Max Gonzih]: http://blog.gonzih.me/blog/2013/04/16/fast-lxc-configuration-on-laptop-with-netctl-nat/
