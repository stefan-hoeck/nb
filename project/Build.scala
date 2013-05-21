import sbt._
import Keys._

object BuildSettings {
  val sv = "2.10.1"
  val buildOrganization = "efa.nb"
  val buildVersion = "0.3.0-SNAPSHOT"
  val buildScalaVersion = sv
  val netbeansRepo = "Netbeans" at "http://bits.netbeans.org/maven2/"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    fork := true,
    resolvers += netbeansRepo,
    scalacOptions ++= Seq ("-deprecation", "-feature",
      "-language:postfixOps", "-language:implicitConversions",
      "-language:higherKinds")
  )
} 

object Dependencies {
  import BuildSettings.sv

  val utilV = "0.2.1-SNAPSHOT"
  val direV = "0.1.0-SNAPSHOT"
  val reactV = "0.2.1-SNAPSHOT"
  val nbV = "RELEASE71"
  val scalazV = "7.0.0-RC2"

  val nb = "org.netbeans.api"
  val util = "efa"
  val dire = "dire"
  val react = "efa.react"
  val scalaz = "org.scalaz"

  val scalaSwing = "org.scala-lang" % "scala-swing" % sv

  val efa_core = util %% "efa-core" % utilV changing

  val efa_io = util %% "efa-io" % utilV changing

  val dire_core = dire %% "dire-core" % direV changing

  val dire_swing = dire %% "dire-swing" % direV changing
 
  val nbAnnotations = nb % "org-netbeans-api-annotations-common" % nbV
  val nbUtil = nb % "org-openide-util" % nbV
  val nbLookup = nb % "org-openide-util-lookup" % nbV
  val nbExplorer = nb % "org-openide-explorer" % nbV
  val nbWindows = nb % "org-openide-windows" % nbV
  val nbNodes = nb % "org-openide-nodes" % nbV
  val nbFilesystems = nb % "org-openide-filesystems" % nbV
  val nbLoaders = nb % "org-openide-loaders" % nbV
  val nbModules = nb % "org-openide-modules" % nbV
  val nbAwt = nb % "org-openide-awt" % nbV
  val nbSettings = nb % "org-netbeans-modules-settings" % nbV
  val nbActions = nb % "org-openide-actions" % nbV
  val nbDialogs = nb % "org-openide-dialogs" % nbV
  val nbOutline = nb % "org-netbeans-swing-outline" % nbV
  val nbOptions = nb % "org-netbeans-modules-options-api" % nbV
  val nbAutoupdateUi = nb % "org-netbeans-modules-autoupdate-ui" % nbV
  val nbAutoupdateServices = nb % "org-netbeans-modules-autoupdate-services" % nbV
  val nbModulesOptions = nb % "org-netbeans-modules-options-api" % nbV

  val shapeless = "com.chuusai" %% "shapeless" % "1.2.3"
  val scalaz_core = scalaz %% "scalaz-core" % scalazV
  val scalaz_effect = scalaz %% "scalaz-effect" % scalazV

  val scalacheck = "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"

  val coolness = Seq(scalaz_core, scalaz_effect, shapeless, scalacheck)
}

object UtilBuild extends Build {
  import Dependencies._
  import BuildSettings._

  def addDeps (ds: ModuleID*) =
    BuildSettings.buildSettings ++ Seq(libraryDependencies ++= (coolness ++ ds))

  lazy val util = Project (
    "efa-nb-root",
    file("."),
    settings = buildSettings
  ) aggregate (nb, localDe)

  lazy val nb = Project (
    "efa-nb",
    file("nb"),
    settings = addDeps (
      nbUtil, nbLookup, nbDialogs, nbNodes, nbExplorer, nbModules,
      nbOptions, nbFilesystems, nbLoaders, scalaSwing, dire_core,
      dire_swing, efa_core, efa_io
    )
  )

  lazy val localDe = Project (
    "efa-nb-localDe",
    file("localDe"),
    settings = addDeps()
  ) dependsOn(nb)
}

// vim: set ts=2 sw=2 nowrap et:
