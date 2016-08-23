#Phylozzii

[![Build Status](https://travis-ci.com/izziiyt/phylozzii.svg?token=BJziRYquXYXWWkAa7YYz&branch=master)](https://travis-ci.com/izziiyt/phylozzii)

[Scaladoc](http://izziiyt.github.io/scaladoc/phylozzii/2.11)

This repository contains three bioinformatics softwares' source codes.

##Overview

###Fdur

This software is Scala version of [Fdur-algorithm](http://www.ncrna.org/software/fdur/) implementations. 
If your data is not so large, you should use the above c++ version implementation alternatively.
This software can run on single workstation also on [Spark](http://spark.apache.org/) clusters.

###Branco

Branco can assess evolutionary conservation using a phylogenetic tree, a probablistic evolution model and a multiple alignment.
Branco outputs expected branch length scores (EBLS) of a target nucleotide. EBLS is statistical expectaion of how long the nucleotide 
conserved without mutations in phylogenetic tree. 

###util

This utility software consists with some supportive functions. With these functions, 
you can execute deeper analisys or data reformatting to branco's outputs.

##Install & Build

###requirement

####jdk
This program is checked running on [oracle jdk](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html)1.8.0_45 or later.
Other jdks are not checked.

####sbt_0.13 or later
[sbt](http://www.scala-sbt.org/index.html) is a build tool for scala.
Installing takes you very long time because of resolving dependency.
These depended libraries are cached in your $HOME/.sbt or $HOME/.ivy2 by default and used next time.

###command

put later commands on your terminal

```bash
git clone https://github.com/izziiyt/phylozzii
cd phylozzii
sbt assembly
```

after that, jar files are build at these locations  

* phylozzii/branco/target/scala-2.11/branco.jar  
* phylozzii/fdur/target/scala-2.11/fdur.jar  
* phylozzii/util/target/scala-2.11/util.jar  

##Usage

see [wiki](https://github.com/izziiyt/phylozzii/wiki)

##LICENSE

Under the MIT License, see LICENSE.txt

##Reference

* [Hisanori Kiryu
  Sufficient statistics and expectation maximization algorithms in phylogenetic tree models
  Bioinformatics 27(17), 2346-2353 doi:10.1093/bioinformatics/btr420 (2011)](http://bioinformatics.oxfordjournals.org/content/27/17/2346.long)

##Contatct

Yuto Ichikawa : ichikaway{at}cb.k.u-tokyo.ac.jp  
Hisanori Kiryu : kiryu-h{at}k.u-tokyo.ac.jp
