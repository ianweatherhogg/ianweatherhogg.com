---
title: creating scala project templates
description: using emacs macros and sbt plugins to create starting scala
project templates
tags: scala sbt emacs macros
---

# Problem

Since I do a lot of scala experimentation, I need to knock out a project
template really quickly so that I can generate the src/test code

# Solution

Normal [Workflow] is

```bash
$ touch build.sbt
$ mkdir -p src/{main,test}/scala
$ e build.sbt # fill in the basics (name, organization, version)
$ touch README.md && e README.md
$ sbt
```

Go and talk about macros being generated

# Details

## Creating Directory Structure from Scala Code

/home/ian/Code/scala/github/pchiusano/fpinscala/project/Master.scala -
real github reference required

```scala
def write(srcBaseDir: String, includesBaseDir: String, book: Book): Unit = {
  new File(srcBaseDir + "/exercises").mkdirs
  new File(srcBaseDir + "/answers").mkdirs
  new File(srcBaseDir + "/examples").mkdirs
  val bookRoot = { 
    val p = System.getProperty("bookRoot")
    if (p eq null) includesBaseDir + "/includes/"
    else p + "/includes/"
  }
  new File(bookRoot + "/examples").mkdirs
  new File(bookRoot + "/exercises").mkdirs
  new File(bookRoot + "/answers").mkdirs
  emitHints(includesBaseDir, book) 
  emitBook(srcBaseDir, includesBaseDir, book) 
}
```

this taken from shapeless

```scala
def gen(dir : File) = { 
  val tupler = dir / "shapeless" / "tupler.scala"
  IO.write(tupler, genTuplerInstances)
  
  val fntoproduct = dir / "shapeless" / "fntoproduct.scala"
  IO.write(fntoproduct, genFnToProductInstances)
  
  val fnfromproduct = dir / "shapeless" / "fnfromproduct.scala"
  IO.write(fnfromproduct, genFnFromProductInstances)
  
  val caseinst = dir / "shapeless" / "caseinst.scala"
  IO.write(caseinst, genCaseInst)

  val polyapply = dir / "shapeless" / "polyapply.scala"
  IO.write(polyapply, genPolyApply)

  val polyinst = dir / "shapeless" / "polyinst.scala"
  IO.write(polyinst, genPolyInst)

  val cases = dir / "shapeless" / "cases.scala"
  IO.write(cases, genCases)

  val polyntraits = dir / "shapeless" / "polyntraits.scala"
  IO.write(polyntraits, genPolyNTraits)

  val nats = dir / "shapeless" / "nats.scala"
  IO.write(nats, genNats)
  
  val tupletypeables = dir / "shapeless" / "tupletypeables.scala"
  IO.write(tupletypeables, genTupleTypeableInstances)

  val sizedbuilder = dir / "shapeless" / "sizedbuilder.scala"
  IO.write(sizedbuilder, genSizedBuilder)
  
  val hmapbuilder = dir / "shapeless" / "hmapbuilder.scala"
  IO.write(hmapbuilder, genHMapBuilder)
  
  Seq(
    tupler, fntoproduct, fnfromproduct, caseinst, polyapply,
    polyinst, cases, polyntraits, nats, tupletypeables, sizedbuilder,
    hmapbuilder
  )
}
```
/home/ian/Code/scala/github/milessabin/shapeless/project/Boilerplate.scala

## Eclipse Plugins

This is what has just prompted me to bang out something like this

/home/ian/Code/scala/github/markkolich/interviews/project/Build.scala

Realising that the eclipse sbt is generating the source and test directories

```scala
unmanagedSourceDirectories in Compile <<= baseDirectory(new File(_, "src/main/java"))(Seq(_)),
unmanagedSourceDirectories in Test <<= baseDirectory(new File(_, "src/test/java"))(Seq(_)),

unmanagedSourceDirectories in Compile in packageSrc <<= baseDirectory(new File(_, "src/main/java"))(Seq(_)),
// Override the SBT default "target" directory for compiled classes.
classDirectory in Compile <<= baseDirectory(new File(_, "target/classes")),
Seq(EclipseKeys.createSrc := EclipseCreateSrc.Default,
  // Make sure SBT also fetches/loads the "src" (source) JAR's for
  // all declared dependencies.
  EclipseKeys.withSource := true,
  // This is a Java project, only.
  EclipseKeys.projectFlavor := EclipseProjectFlavor.Java))	  
```

Note, that this is a java project under sbt
Curently giving this more consideration

[Workflow]:https://github.com/softprops/np/blob/master/README.md
