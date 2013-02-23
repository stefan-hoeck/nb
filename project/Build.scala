import sbt._
import Keys._

object BuildSettings {
  val sv = "2.10.0"
  val buildOrganization = "efa.nb"
  val buildVersion = "0.2.1-SNAPSHOT"
  val buildScalaVersion = sv
  val netbeansRepo = "Netbeans" at "http://bits.netbeans.org/maven2/"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    resolvers += netbeansRepo,
    scalacOptions ++= Seq ("-deprecation", "-feature",
      "-language:postfixOps", "-language:implicitConversions",
      "-language:higherKinds")
  )
} 

object Dependencies {
  import BuildSettings.sv

  val utilV = "0.2.1-SNAPSHOT"
  val reactV = "0.2.1-SNAPSHOT"
  val nbV = "RELEASE71"
  val orgNb = "org.netbeans.api"
  val util = "efa"
  val react = "efa.react"

  val scalaSwing = "org.scala-lang" % "scala-swing" % sv

  val efa_core = util %% "efa-core" % utilV changing

  val efa_io = util %% "efa-io" % utilV changing

  val react_core = react %% "react-core" % reactV changing

  val react_swing = react %% "react-swing" % reactV changing
 
  val nbAnnotations = orgNb % "org-netbeans-api-annotations-common" % nbV
  val nbUtil = orgNb % "org-openide-util" % nbV
  val nbLookup = orgNb % "org-openide-util-lookup" % nbV
  val nbExplorer = orgNb % "org-openide-explorer" % nbV
  val nbWindows = orgNb % "org-openide-windows" % nbV
  val nbNodes = orgNb % "org-openide-nodes" % nbV
  val nbFilesystems = orgNb % "org-openide-filesystems" % nbV
  val nbLoaders = orgNb % "org-openide-loaders" % nbV
  val nbModules = orgNb % "org-openide-modules" % nbV
  val nbAwt = orgNb % "org-openide-awt" % nbV
  val nbSettings = orgNb % "org-netbeans-modules-settings" % nbV
  val nbActions = orgNb % "org-openide-actions" % nbV
  val nbDialogs = orgNb % "org-openide-dialogs" % nbV
  val nbOutline = orgNb % "org-netbeans-swing-outline" % nbV
  val nbOptions = orgNb % "org-netbeans-modules-options-api" % nbV
  val nbAutoupdateUi = orgNb % "org-netbeans-modules-autoupdate-ui" % nbV
  val nbAutoupdateServices = orgNb % "org-netbeans-modules-autoupdate-services" % nbV
  val nbModulesOptions = orgNb % "org-netbeans-modules-options-api" % nbV

  val shapeless = "com.chuusai" %% "shapeless" % "1.2.3"
  val scalaz_core = "org.scalaz" %% "scalaz-core" % "7.0.0-M8"
  val scalaz_effect = "org.scalaz" %% "scalaz-effect" % "7.0.0-M8"
  val scalaz_scalacheck = "org.scalaz" %% "scalaz-scalacheck-binding" % "7.0.0-M8"

  val scalacheck = "org.scalacheck" %% "scalacheck" % "1.10.0"
  val scalacheckT = scalacheck % "test"

  val coolness = Seq(scalaz_core, scalaz_effect, shapeless)
}

object UtilBuild extends Build {
  import Dependencies._
  import BuildSettings._

  def addDeps (ds: Seq[ModuleID]) =
    BuildSettings.buildSettings ++ Seq (libraryDependencies ++= ds)

  lazy val util = Project (
    "efa-nb-root",
    file("."),
    settings = buildSettings
  ) aggregate (nb, localDe)

  lazy val nb = Project (
    "efa-nb",
    file("nb"),
    settings = addDeps (coolness ++
      Seq (nbUtil, nbLookup, nbDialogs, nbNodes, nbExplorer, nbModules,
           nbOptions, nbFilesystems, nbLoaders, scalaSwing, react_core,
           react_swing, efa_core, efa_io, scalacheckT
         )
       )
  )

  lazy val localDe = Project (
    "efa-localDe",
    file("localDe"),
    settings = addDeps(scalacheckT +: coolness)
  ) dependsOn(nb)
}

// vim: set ts=2 sw=2 nowrap et:
