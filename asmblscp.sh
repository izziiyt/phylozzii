#!/bin/bash
#/home/yuto/local/sbt/bin/sbt +assembly
scp util/target/scala-2.11/util.jar shirokane3:~/work/100wayz/util.jar
scp fdur/target/scala-2.11/fdur.jar shirokane3:~/work/100wayz/fdur.jar
scp branco/target/scala-2.11/branco.jar shirokane3:~/work/100wayz/branco.jar
