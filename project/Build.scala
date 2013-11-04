import scala.language.postfixOps
import sbt._
import Keys._

object BuildSettings {
  val sv = "2.10.3"
  val buildOrganization = "efa.nb"
  val buildVersion = "0.3.0"
  val buildScalaVersion = sv
  val netbeansRepo = "Netbeans" at "http://bits.netbeans.org/maven2/"

  val manifest = SettingKey[File]("manifest", "Location of the Manifest.mf file")
  val removeManifest = TaskKey[Unit]("remove-manifest", "Removes manifest file")
 
  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    manifest <<= classDirectory in Compile apply (_ / "META-INF/MANIFEST.MF"),
    removeManifest <<= manifest map (f ⇒ f.delete),
    fork := true,
    resolvers += netbeansRepo,
    scalacOptions ++= Seq ("-deprecation", "-feature",
      "-language:postfixOps", "-language:implicitConversions",
      "-language:higherKinds")
  )
} 

object Dependencies {
  import BuildSettings.sv

  val utilV = "0.2.2"
  val direV = "0.1.0"
  val nbV = "RELEASE74"
  val scalazV = "7.0.4"

  val nb = "org.netbeans.api"
  val util = "efa"
  val dire = "dire"
  val scalaz = "org.scalaz"

  val efa_core = util %% "efa-core" % utilV
  val efa_io = util %% "efa-io" % utilV
  val dire_core = dire %% "dire-core" % direV
  val dire_swing = dire %% "dire-swing" % direV
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
      nbOptions, nbFilesystems, nbLoaders, dire_core,
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
