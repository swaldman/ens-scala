// only necessary while using a SNAPSHOT version of sbt-ethereum
resolvers += ("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")

addSbtPlugin("com.mchange" % "sbt-ethereum" % "0.0.2-SNAPSHOT")
addSbtPlugin("org.tpolecat" % "tut-plugin" % "0.5.2")

// note that we need 1.3.0-SNAPSHOT (built and published locally for now)
// in order to work with current versions of hugo!
addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "1.3.0-SNAPSHOT")
