import scala.language.postfixOps

lazy val commonSettings = Seq(
  scalacOptions ++= commonScalacOptions,
  scalaVersion := "2.12.9",
  organization := "org.ergoplatform",
  resolvers += Resolver.sonatypeRepo("public"),
  licenses := Seq("CC0" -> url("https://creativecommons.org/publicdomain/zero/1.0/legalcode")),
  homepage := Some(url("https://github.com/ergoplatform/ergo-contracts")),
  description := "Ergo contracts",
  pomExtra :=
      <developers>
        <developer>
          <id>greenhat</id>
          <name>Denys Zadorozhnyi</name>
          <url>https://github.com/greenhat/</url>
        </developer>
      </developers>,
  publishMavenStyle := true,
  publishTo := sonatypePublishToBundle.value,
  parallelExecution in Test := false,
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/ergoplatform/ergo-contracts"),
      "scm:git@github.com:ergoplatform/ergo-contracts.git"
    )
  ),
)

// prefix version with "-SNAPSHOT" for builds without a git tag
dynverSonatypeSnapshots in ThisBuild := true
// use "-" instead of default "+"
dynverSeparator in ThisBuild := "-"

lazy val allConfigDependency = "compile->compile;test->test"
val sigmaStateVersion = "3.2.1"
val ergoScalaCompilerVersion = "0.1.0"

lazy val dependencies = Seq(
  "org.ergoplatform" %% "ergo-scala-compiler" % ergoScalaCompilerVersion,
)

lazy val testingDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.0.8" % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.1" % Test,
  "org.scorexfoundation" %% "sigma-state" % sigmaStateVersion % allConfigDependency
)

lazy val rootProject = project
  .in(file("."))
  .withId("ergo-contracts")
  .settings(commonSettings)
  .settings(moduleName := "ergo-contracts")
  .settings(publish / skip := true)
  .aggregate(verifiedContracts, verifiedContractsTests, contracts)

lazy val verifiedContracts = project
  .in(file("verified-contracts"))
  .settings(moduleName := "verified-contracts")
  .enablePlugins(StainlessPlugin)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= dependencies ++ testingDependencies
  )
  .settings(
    scalacOptions ++= Seq(
      "-Xlog-free-terms",
    )
  )
  .settings(
    publishArtifact in(Compile, packageDoc) := false,
  )

lazy val verifiedContractsTests = project
  .in(file("verified-contracts-test"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= dependencies ++ testingDependencies 
  )
  .dependsOn(verifiedContracts)
  .settings(
    publishArtifact := false,
  )

lazy val contracts = project
  .in(file("contracts"))
  .settings(moduleName := "contracts")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= dependencies ++ testingDependencies
  )
  .settings(
    scalacOptions ++= Seq(
      "-Xlog-free-terms",
    )
  )

lazy val commonScalacOptions = List(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Yno-adapted-args",
  "-Ywarn-numeric-widen"
)


// PGP key for signing a release build published to sonatype
// signing is done by sbt-pgp plugin
// how to generate a key - https://central.sonatype.org/pages/working-with-pgp-signatures.html
// how to export a key and use it with Travis - https://docs.scala-lang.org/overviews/contributors/index.html#export-your-pgp-key-pair
// public key should be submitted to any public key server e.g. http://pgp.mit.edu/pks/add
// on macOS: gpg --armor --export [id] | pbcopy
pgpPassphrase := sys.env.get("PGP_PASSPHRASE").map(_.toArray)
usePgpKeyHex("8F2E12DE4EA7C643801166B5161DA7E917ECFC15")

lazy val credentialFile = Path.userHome / ".sbt" / ".ergo-sonatype-credentials"
credentials ++= (for {
  file <- if (credentialFile.exists) Some(credentialFile) else None
} yield Credentials(file)).toSeq

credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
