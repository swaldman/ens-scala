name := "ens-scala"

organization := "com.mchange"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.11"

resolvers += ("releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")

resolvers += ("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")

resolvers += ("Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/")

libraryDependencies += "com.mchange" %% "consuela" % "0.0.3-SNAPSHOT"

ethPackageScalaStubs := "com.mchange.sc.v2.ens.contract"
