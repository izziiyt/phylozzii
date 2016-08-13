#Phylozzii

[![Build Status](https://travis-ci.com/izziiyt/pbls.svg?token=BJziRYquXYXWWkAa7YYz)](https://travis-ci.com/izziiyt/pbls)

This repository contains three softwares' source codes.

###fdur

This software is Scala version of [fdur-algorithm](http://www.ncrna.org/software/fdur/) implementations. 
If your data is not so large, you should use the above c++ version implementation alternatively.
This software can run on single workstation also on [Spark](http://spark.apache.org/) clusters.

###branco

branco can assess evolutionary conservation using a phylogenetic tree, a phylogenetic model and a multiple alignment.
branco outputs expected branch length scores (EBLS) of a target nucleotide. EBLS is statistical expectaion of how long the nucleotide 
conserved without mutations in phylogenetic tree. 

###util

This utility software consists with some supportive functions. With these functions, 
you can execute deeper analisys or data reformatting to branco outputs.

##Install & Build

put later commands on your terminal

```
git clone https://github.com/izziiyt/phylozzii
cd phylozzii
chmod u+x sbt
./sbt +assembly
```

after that, jar file are build at these locations  

phylozzii/branco/target/scala-2.11/branco-[version].jar  
phylozzii/fdur/target/scala-2.11/fdur-[version].jar  
phylozzii/util/target/scala-2.11/util-[version].jar  

##Usage

see wiki


