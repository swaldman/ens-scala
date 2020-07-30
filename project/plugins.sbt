// only necessary while using a SNAPSHOT version of sbt-ethereum
resolvers += ("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")

addSbtPlugin("com.mchange" % "sbt-ethereum" % "0.4.7-SNAPSHOT" changing())
addSbtPlugin("org.tpolecat" % "tut-plugin" % "0.6.13")
addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "1.3.1")
