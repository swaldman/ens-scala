name := "ens-scala"

organization := "com.mchange"

version := "0.0.3-SNAPSHOT"

scalaVersion := "2.12.6"

crossScalaVersions := Seq("2.10.7", "2.11.12", "2.12.6")

resolvers += ("releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")

resolvers += ("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")

resolvers += ("Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/")

// This doesn't work... I'd like for the docs to also generate consuela docs, so
// that links to consuela artifacts work
//
// autoAPIMappings := true

ethcfgScalaStubsPackage := "com.mchange.sc.v2.ens.contract"

// documentation stuff

enablePlugins(TutPlugin)
enablePlugins(PreprocessPlugin)
enablePlugins(SiteScaladocPlugin)
enablePlugins(HugoPlugin)

// we set up a pipeline
//
//   tut -> preprocess -> hugo -> done

val pipelineSourceDirectory = settingKey[File]("The directory from which the documentation pipeline begins.")
val pipelineTarget          = settingKey[File]("The directory in which the completed, processed documentation will be placed.")

pipelineSourceDirectory := ( sourceDirectory in Compile ).value / "site-pipeline"
pipelineTarget := (target in makeSite).value // same as where Scaladoc goes

tutSourceDirectory := pipelineSourceDirectory.value

// make sure tut happens before preprocessing then let the preprocessor take file from tuts output

preprocessVars in Preprocess := Map("VERSION" -> version.value)
mappings in Preprocess := { ( ( mappings in Preprocess ) dependsOn ( tut ) ).value }
sourceDirectory in Preprocess := tutTargetDirectory.value

// make sure preprocess happens before hugo then let the hugo take file from preprocessor output

baseURL in Hugo := uri(s"http://www.mchange.com/projects/${name.value}/")
mappings in Hugo := { ( ( mappings in Hugo ) dependsOn ( mappings in Preprocess ) ).value }
sourceDirectory in Hugo := (target in Preprocess).value
target in Hugo := target.value / "hugo"

makeSite := {
  val ensureHugoAndScaladoc = makeSite.value

  val hugoPublicSite = (target in Hugo).value / "public"

  IO.copyDirectory( hugoPublicSite, pipelineTarget.value, overwrite = true, preserveLastModified = false )

  ensureHugoAndScaladoc // evaluate to the same directory as it would have, which is now the merged directory
}

val updateSite = taskKey[Unit]("Updates the project website on tickle")

updateSite := {
  import scala.sys.process._

  val dummy = makeSite.value // force a build of the site

  val localDir = pipelineTarget.value

  val local = localDir.listFiles.map( _.getPath ).mkString(" ")
  val remote = s"tickle.mchange.com:/home/web/public/www.mchange.com/projects/${name.value}"
  s"rsync -avz ${local} ${remote}"!
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




