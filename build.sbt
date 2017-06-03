name := "ens-scala"

organization := "com.mchange"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.11"

resolvers += ("releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")

resolvers += ("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")

resolvers += ("Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/")

libraryDependencies += "com.mchange" %% "consuela" % "0.0.3-SNAPSHOT"
libraryDependencies += "com.mchange" %% "mchange-commons-scala" % "0.4.3-SNAPSHOT"

ethPackageScalaStubs := "com.mchange.sc.v2.ens.contract"

// documentation stuff

enablePlugins(TutPlugin)
enablePlugins(PreprocessPlugin)
enablePlugins(SiteScaladocPlugin)
enablePlugins(HugoPlugin)

// we set up a pipeline
//
//   tut -> preprocess -> hugo -> done

val pipelineSourceDirectory = settingKey[File]("The directory from which the documentation pipeline begins.")
val pipelineTargetDirectory = settingKey[File]("The directory in which the completed, processed documentation will be placed.")

pipelineSourceDirectory := ( sourceDirectory in Compile ).value / "site-pipeline"
pipelineTargetDirectory := target.value / "site" // same as where Scaladoc goes by default

tutSourceDirectory := pipelineSourceDirectory.value

// make sure tut happens before preprocessing then let the preprocessor take file from tuts output

preprocessVars in Preprocess := Map("VERSION" -> version.value)
mappings in Preprocess := { ( ( mappings in Preprocess ) dependsOn ( tut ) ).value }
sourceDirectory in Preprocess := tutTargetDirectory.value

// make sure preprocess happens before hugo then let the hugo take file from preprocessor output

baseURL in Hugo := uri("https://www.mchange.com/projects/ens-scala/")
mappings in Hugo := { ( ( mappings in Hugo ) dependsOn ( mappings in Preprocess ) ).value }
sourceDirectory in Hugo := (target in Preprocess).value
//targetDirectory in Hugo := pipelineTargetDirectory.value

val updateSite = taskKey[Unit]("Updates the project website on tickle")

updateSite := {
  val dummy = makeSite.value // force a build of the site

  val local = pipelineTargetDirectory.value
  val remote = "swaldman@tickle.mchange.com:/home/web/public/www.mchange.com/projects/ens-scala"
  s"scp -r ${local} ${remote}"!
}





