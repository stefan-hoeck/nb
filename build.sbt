import scala.language.postfixOps
import sbt._
import Keys._
import Dependencies._

val buildOrganization = "efa.nb"
val buildVersion      = "0.4.0"
val buildScalaVersion = sv
val netbeansRepo      = "Netbeans" at "http://bits.netbeans.org/maven2/"

val manifest          = SettingKey[File]("manifest", "Location of the Manifest.mf file")
val removeManifest    = TaskKey[Unit]("remove-manifest", "Removes manifest file")

val buildSettings = Seq (
  organization       := buildOrganization,
  version            := buildVersion,
  scalaVersion       := buildScalaVersion,
  resolvers          += netbeansRepo,
  publishTo          := Some(Resolver.file("file", 
    new File(Path.userHome.absolutePath+"/.m2/repository"))),
//  manifest           <<= classDirectory in Compile apply (_ / "META-INF/MANIFEST.MF"),
//  removeManifest     <<= manifest map (f ⇒ f.delete),
  fork := true,
  libraryDependencies in ThisBuild ++= deps,

  scalacOptions      ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-language:higherKinds"
  )
)

lazy val util = Project("efa-nb-root", file("."))
                  .settings(buildSettings)
                  .aggregate(nb, localDe)

lazy val nb = Project ("efa-nb", file("nb"))
               .settings(
                 buildSettings,
                 libraryDependencies ++= Seq(
                   nbUtil, nbLookup, nbDialogs, nbNodes, nbExplorer, nbModules,
                   nbOptions, nbFilesystems, nbLoaders, dire_core,
                   dire_swing, efa_core, efa_io
                 ))

  lazy val localDe = Project("efa-nb-localDe", file("localDe"))
                       .settings(buildSettings)
                       .dependsOn(nb)

// vim: set ts=2 sw=2 nowrap et:
