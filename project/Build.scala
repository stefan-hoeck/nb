import scala.language.postfixOps
import sbt._
import Keys._

object BuildSettings {
  val sv                = "2.11.8"
  val buildOrganization = "efa.nb"
  val buildVersion      = "0.3.5-SNAPSHOT"
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
    manifest           <<= classDirectory in Compile apply (_ / "META-INF/MANIFEST.MF"),
    removeManifest     <<= manifest map (f ⇒ f.delete),
    fork := true,

    scalacOptions      ++= Seq(
      "-unchecked",
      "-deprecation",
      "-feature",
      "-language:postfixOps",
      "-language:implicitConversions",
      "-language:higherKinds"
    )
  )
} 

object Dependencies {
  import BuildSettings.sv

  val direV                = "0.2.2-SNAPSHOT"
  val nbV                  = "RELEASE80"
  val scalacheckV          = "1.12.5"
  val scalazV              = "7.2.4"
  val shapelessV           = "2.2.5"
  val utilV                = "0.2.6-SNAPSHOT"

  val dire                 = "dire"
  val nb                   = "org.netbeans.api"
  val scalaz               = "org.scalaz"
  val util                 = "efa"

  val efa_core             = util %% "efa-core" % utilV
  val efa_io               = util %% "efa-io" % utilV
  val dire_core            = dire %% "dire-core" % direV
  val dire_swing           = dire %% "dire-swing" % direV
 
  val nbActions            = nb % "org-openide-actions" % nbV
  val nbAnnotations        = nb % "org-netbeans-api-annotations-common" % nbV
  val nbAutoupdateServices = nb % "org-netbeans-modules-autoupdate-services" % nbV
  val nbAutoupdateUi       = nb % "org-netbeans-modules-autoupdate-ui" % nbV
  val nbAwt                = nb % "org-openide-awt" % nbV
  val nbDialogs            = nb % "org-openide-dialogs" % nbV
  val nbExplorer           = nb % "org-openide-explorer" % nbV
  val nbFilesystems        = nb % "org-openide-filesystems" % nbV
  val nbLoaders            = nb % "org-openide-loaders" % nbV
  val nbLookup             = nb % "org-openide-util-lookup" % nbV
  val nbModules            = nb % "org-openide-modules" % nbV
  val nbModulesOptions     = nb % "org-netbeans-modules-options-api" % nbV
  val nbNodes              = nb % "org-openide-nodes" % nbV
  val nbOptions            = nb % "org-netbeans-modules-options-api" % nbV
  val nbOutline            = nb % "org-netbeans-swing-outline" % nbV
  val nbSettings           = nb % "org-netbeans-modules-settings" % nbV
  val nbUtil               = nb % "org-openide-util" % nbV
  val nbWindows            = nb % "org-openide-windows" % nbV

  val scalacheck           = "org.scalacheck" %% "scalacheck" % scalacheckV % "test"
  val scalaz_core          = scalaz %% "scalaz-core" % scalazV
  val scalaz_effect        = scalaz %% "scalaz-effect" % scalazV
  val shapeless            = "com.chuusai" %% "shapeless" % shapelessV

  val deps                 = Seq(scalaz_core, scalaz_effect, shapeless, scalacheck)
}

object UtilBuild extends Build {
  import Dependencies._
  import BuildSettings._

  def addDeps (ds: ModuleID*) =
    BuildSettings.buildSettings ++ Seq(libraryDependencies ++= (deps ++ ds))

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
