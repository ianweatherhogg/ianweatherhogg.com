---
title: Vertx on archlinux is unable to download and install modules to /opt/vertx
description: /opt/vertx/ has insufficient write permissions to install downloaded modules
tags: archlinux vertx module installation environment variable 
---

# Problem

Using `vertx` on `archlinux` and following the [VertX Installation] helloworld instruction
ngives the following error:

```bash
vertx run server.js 
Downloading io.vertx~lang-rhino~2.0.0-final. Please wait... 
Downloading 100%
java.nio.file.AccessDeniedException: /opt/vertx/sys-mods 
java.nio.file.AccessDeniedException: /opt/vertx/sys-mods
```

# Solution

Install the downloaded modules locally by including the `VERTX_MODS` environment
variable in your `bash_profile`, typically:

```bash
export VERTX_MODS=$HOME/.vertx-modules
```
Open new shell (for environment variable to be within scope) and the helloworld
example should install the modules and run as per instruction.

# Details

This is my first initiation into `vertx` and opted for installation via `pacman`
(well `yaourt` which I prefer and find more accomodating for `AUR` packages such
as the [Vertx Package]) on `archlinux` over compiling from source and setting
the `PATH` manually thereafter:

```bash
yaourt vertx
```

As highlighted by the above error message, modules downloaded by `vertx` are installed
under `/opt/vertx/sys-mods`. This directory neither exists or has sufficient write
permissions (`root` naturally has but local `user` does not). This is further
affirmed by the [Vertx Build Script] which sets `755` permissions (no write for
`user`).

I thought about `chgrp/chmod` to force the issue but whilst perusing the codebase
noticed in the [Starter] java file, `displaySyntax` method a reference to
the `VERTX_MODS` environment variable which resolves the issue. Note also that
this usage information is generated at the command line by calling `vertx` with
no arguments.

Update: have just discovered reference and usage of `VERTS_MODS` in the
[Modules Manual] which I obviously failed to read/pay attention in the begining

[Vertx Installation]: http://vertx.io/install.html

[Vertx Package]: https://aur.archlinux.org/packages/vertx/

[Vertx Build Script]: https://aur.archlinux.org/packages/ve/vertx/PKGBUILD

[Starter]: https://github.com/eclipse/vert.x/blob/master/vertx-platform/src/main/java/org/vertx/java/platform/impl/cli/Starter.java

[Modules Manual]: http://vertx.io/mods_manual.html
