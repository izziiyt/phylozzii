#!/bin/bash
/home/yuto/local/sbt/bin/sbt assembly && scp core/target/scala-2.10/*.jar shirokane3:~/work/100wayz/phylozzii.jar

