// only necessary while using a SNAPSHOT version of sbt-ethereum
resolvers += ("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")

addSbtPlugin("com.mchange" % "sbt-ethereum" % "0.1.7-SNAPSHOT")
addSbtPlugin("org.tpolecat" % "tut-plugin" % "0.6.2")
addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "1.3.1")
