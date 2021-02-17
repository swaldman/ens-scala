name := "ens-scala"

organization := "com.mchange"

version := "0.4.0"

scalaVersion := "2.12.13"

crossScalaVersions := Seq("2.10.7", "2.11.12", "2.12.13")

resolvers += ("releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")

resolvers += ("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")

// resolvers += ("Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/")

// This doesn't work... I'd like for the docs to also generate consuela docs, so
// that links to consuela artifacts work
//
// autoAPIMappings := true

ethcfgScalaStubsPackage := "com.mchange.sc.v2.ens.contract"

enablePlugins(ParadoxPlugin)

val updateSite = taskKey[Unit]("Updates the project website on tickle")

updateSite := {
  import scala.sys.process._

  val dummy1 = (Compile / paradox).value // force a build of the site

  val localDir1 = target.value / "paradox" / "site" / "main"

  val local1 = localDir1.listFiles.map( _.getPath ).mkString(" ")
  val remote1 = s"tickle.mchange.com:/home/web/public/www.mchange.com/projects/${name.value}-versions/${version.value}/"
  s"rsync -avz ${local1} ${remote1}"!

  val dummy2 = (Compile / doc).value // force scaladocs

  val localDir2 = target.value / "scala-2.12" / "api"
  val local2 = localDir2.listFiles.map( _.getPath ).mkString(" ")
  val remote2 = s"tickle.mchange.com:/home/web/public/www.mchange.com/projects/${name.value}-versions/${version.value}/apidocs"
  s"rsync -avz ${local2} ${remote2}"!
}

val nexus = "https://oss.sonatype.org/"
val nexusSnapshots = nexus + "content/repositories/snapshots"
val nexusReleases = nexus + "service/local/staging/deploy/maven2"

publishTo := version {
  (v: String) => {
    if (v.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at nexusSnapshots )
    else
      Some("releases"  at nexusReleases )
  }
}.value

pomExtra := {
    <url>https://github.com/swaldman/{name.value}</url>
    <licenses>
      <license>
        <name>GNU Lesser General Public License, Version 2.1</name>
        <url>http://www.gnu.org/licenses/lgpl-2.1.html</url>
        <distribution>repo</distribution>
      </license>
      <license>
        <name>Eclipse Public License, Version 1.0</name>
        <url>http://www.eclipse.org/org/documents/epl-v10.html</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:swaldman/{name.value}.git</url>
      <connection>scm:git:git@github.com:swaldman/{name.value}</connection>
    </scm>
    <developers>
      <developer>
        <id>swaldman</id>
        <name>Steve Waldman</name>
        <email>swaldman@mchange.com</email>
      </developer>
    </developers>
}




