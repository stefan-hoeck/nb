import sbt._

object Dependencies {
  val sv                = "2.12.3"

  val direV                = "0.3.0"
  val nbV                  = "RELEASE80"
  val scalacheckV          = "1.12.6"
  val scalazV              = "7.2.15"
  val shapelessV           = "2.3.2"
  val utilV                = "0.3.0"

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

// vim: set ts=2 sw=2 nowrap et:
