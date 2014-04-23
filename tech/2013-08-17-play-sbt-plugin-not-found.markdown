---
title: Unresolved dependencies for sbt-plugin 2.0
description: play framework sample projects fail to resolve play sbt plugin
tags: play sbt plugin scala
---

# Problem

I took a look at some of the samples included with the playframework source code.
Whilst building the [Play Twitter Stream Sample], dependency errors resulted.
`sbt` throws up the following unresolved dependency error trace whilst searching for the play sbt plugin:

```bash
[warn] ==== Typesafe repository: tried
[warn]   http://repo.typesafe.com/typesafe/releases/com/typesafe/play/sbt-plugin_2.10_0.13/2.0/sbt-plugin-2.0.pom
[warn] 	::::::::::::::::::::::::::::::::::::::::::::::
[warn] 	::          UNRESOLVED DEPENDENCIES         ::
[warn] 	::::::::::::::::::::::::::::::::::::::::::::::
[warn] 	:: com.typesafe.play#sbt-plugin;2.0: not found
[warn] 	::::::::::::::::::::::::::::::::::::::::::::::
[warn] 
[warn] 	Note: Some unresolved dependencies have extra attributes.  Check that these dependencies exist with the requested attributes.
[warn] 		com.typesafe.play:sbt-plugin:2.0 (sbtVersion=0.13, scalaVersion=2.10)
[warn] 
sbt.ResolveException: unresolved dependency: com.typesafe.play#sbt-plugin;2.0: not found
	at sbt.IvyActions$.sbt$IvyActions$$resolve(IvyActions.scala:213)
	at sbt.IvyActions$$anonfun$update$1.apply(IvyActions.scala:122)
```

# Solution
## Adjust for a known version which exists
Specify the exact milestone realease in the `plugins.sbt` file

Before:

``` scala
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % Option(System.getProperty("play.version")).getOrElse("2.0"))
```
After:

``` scala
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.2.0-M2")
```
## Alternatively, pass in the play version via command line options/system properties
Specify the play.version on the command line when invoking `sbt`:

```bash
sbt13RC4 -Dplay.version="2.2.0-M2"
```
# Details
## Plugin Version
The [Play Twitter Stream Sample], uses `sbt` version 0.13 (`sbt` being compiled with `scala` version 2.10)
The `play` `sbt` plugin which matches this versioning is searched for at the [Typesafe Releases Repo].
The most current match is versioned at "2.2.0-M2" (along with an older "2.0.6" version).

It would appear that `getOrElse("2.0")` doesn't 'glob' the "2.0.6", so I had to hard code the "2.2.0-M2" version.

An example of hard coding the milestone realease is demonstrated in the [Play Cake Pattern] project repo.

## System Properties via Command Line Args
System properties can be passed to `sbt` at the command line via the `-D` switch. Correctly setting the
`play.version` resolves the dependency issue.

I found it of benefit to go through some `sbt` scripts to get a better understanding of this, typically:

* [Play play script]
* [Play build script]
* [sbt extras script]
* system install of `sbt` found at `/bin/sbt` (system version currently "0.12.4" on my archlinux distro)

## sbtopts
A corollary to reading the above scripts is the discovery that command line arguments can be
included in a `.sbtopts` file. This is usually placed at the root of the project (system wide `/etc/sbt/sbtops` checked first).
The dependency issue can be resolved by including in the file:

```
-Dplay.version=2.2.0-M2
```
Note the absence of quotes around the version number

My system `sbt` script (version "0.12.4") checks for `sbtopts`, but the `sbt13RC4` is a simple bash
script calling the sbt launcher (version 0.13 release canditate 4) and does not check for `sbtopts`.
An example of such a script is demonstrated by this [Sbt Launcher Script].

As a consquence it is untested with `sbt13RC4` but I did verify it's functionality with
`sbt` version "0.12" (via an archived version of the play twitter example where
the desired `play` `sbt` plugin could once again not be found [Typesafe Releases Repo Sbt 0.12])

## SNAPSHOT not found

I also took a look at the [Play sbt-plugin examples] projects. These had similiar issues with being
unable to find SNAPSHOT versions for the `play` sbt-plugin:

``` bash
[info] Loading global plugins from /home/ian/.sbt/0.13/plugins
[info] Loading project definition from /home/ian/Code/scala/github/playframework/playframework/framework/src/sbt-plugin/src/sbt-test/play-sbt-plugin/distribution/project
[warn] 	module not found: com.typesafe.play#sbt-plugin;2.2-SNAPSHOT
[warn] ==== typesafe-ivy-releases: tried
[warn]   http://repo.typesafe.com/typesafe/ivy-releases/com.typesafe.play/sbt-plugin/scala_2.10/sbt_0.13/2.2-SNAPSHOT/ivys/ivy.xml
[warn] ==== sbt-plugin-releases: tried
[warn]   http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/com.typesafe.play/sbt-plugin/scala_2.10/sbt_0.13/2.2-SNAPSHOT/ivys/ivy.xml
[warn] ==== local: tried
[warn]   /home/ian/.ivy2/local/com.typesafe.play/sbt-plugin/scala_2.10/sbt_0.13/2.2-SNAPSHOT/ivys/ivy.xml
[warn] ==== public: tried
[warn]   http://repo1.maven.org/maven2/com/typesafe/play/sbt-plugin_2.10_0.13/2.2-SNAPSHOT/sbt-plugin-2.2-SNAPSHOT.pom
[warn] ==== Typesafe repository: tried
[warn]   http://repo.typesafe.com/typesafe/releases/com/typesafe/play/sbt-plugin_2.10_0.13/2.2-SNAPSHOT/sbt-plugin-2.2-SNAPSHOT.pom
[warn] ==== Sonatype snapshots: tried
[warn]   http://oss.sonatype.org/content/repositories/snapshots/com/typesafe/play/sbt-plugin_2.10_0.13/2.2-SNAPSHOT/sbt-plugin-2.2-SNAPSHOT.pom
[warn] 	::::::::::::::::::::::::::::::::::::::::::::::
[warn] 	::          UNRESOLVED DEPENDENCIES         ::
[warn] 	::::::::::::::::::::::::::::::::::::::::::::::
[warn] 	:: com.typesafe.play#sbt-plugin;2.2-SNAPSHOT: not found
[warn] 	::::::::::::::::::::::::::::::::::::::::::::::
[warn] 
[warn] 	Note: Some unresolved dependencies have extra attributes.  Check that these dependencies exist with the requested attributes.
[warn] 		com.typesafe.play:sbt-plugin:2.2-SNAPSHOT (sbtVersion=0.13, scalaVersion=2.10)
[warn] 
sbt.ResolveException: unresolved dependency: com.typesafe.play#sbt-plugin;2.2-SNAPSHOT: not found
	at sbt.IvyActions$.sbt$IvyActions$$resolve(IvyActions.scala:213)
	at sbt.IvyActions$$anonfun$update$1.apply(IvyActions.scala:122)
	at sbt.IvyActions$$anonfun$update$1.apply(IvyActions.scala:121)
```
Once again, `sbt` version 0.13 is being used (which is built against `scala` version 2.10)

My initial take would be that if I build and publish from source (`publish-local`) then the SNAPSHOT versions
would be included in my local ivy cache as described in the [Play build from source] documentation.

This turned out not to be the case, and infact the `publish-local` command, publishes to the `repository` directory at
the root of the playframework source code checkout.

This becomes evident on analyis of the [Play play script]. The key point is the `-D` switch, `sbt.boot.properties`, which points to
launcher file [Play sbt boot properties], whereupon the area of interest are the ivy settings, in particular, `ivy-home`:

```
[ivy]
  ivy-home: ${sbt.ivy.home-${play.home}/../repository}
```
Thus, if this parameter is fed in at the command line  `-D` switch, in one of the [Play sbt-plugin examples] projects,
the play sbt SNAPSHOT dependency should be picked up and resolve the issue. Typically:

``` bash
sbt13RC4 -Dsbt.ivy.home='/home/ian/Code/scala/github/playframework/playframework/repository'
```
Note, the switch is `sbt.ivy.home`. This [Stackoverflow sbt.boot.properties] question helped me out here.


[Typesafe Releases Repo]: http://repo.typesafe.com/typesafe/releases/com.typesafe.play/sbt-plugin/scala_2.10/sbt_0.13/

[Play Twitter Stream Sample]: https://github.com/playframework/playframework/tree/master/samples/workinprogress/twitterstream

[Stackoverflow sbt.boot.properties]:http://stackoverflow.com/questions/6780828/what-is-the-equivalent-of-sbt-boot-properties-in-sbt-0-10-x

[Play Framework]:https://github.com/playframework/playframework

[Play Cake Pattern]:https://github.com/kaeawc/play-cake-pattern/blob/master/project/plugins.sbt

[Play play script]:https://github.com/playframework/playframework/blob/master/play

[Play build script]:https://github.com/playframework/playframework/blob/master/framework/build

[sbt extras script]:https://github.com/paulp/sbt-extras/blob/master/sbt

[Typesafe Releases Repo Sbt 0.12]:http://repo.typesafe.com/typesafe/ivy-releases/play/sbt-plugin/scala_2.9.2/sbt_0.12/

[Play sbt boot properties]:https://github.com/playframework/playframework/blob/master/framework/sbt/sbt.boot.properties

[Play sbt-plugin examples]:https://github.com/playframework/playframework/tree/master/framework/src/sbt-plugin/src/sbt-test/play-sbt-plugin

[Play build from source]:http://www.playframework.com/documentation/2.1.x/BuildingFromSource

[Sbt Launcher Script]:http://www.scala-sbt.org/0.12.3/docs/Getting-Started/Setup.html#unix
