
addSbtPlugin("com.dwijnand" % "sbt-dynver" % "4.0.0")
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.10.0-RC1")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.4")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "2.0.1")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.3.0")

resolvers ++= Seq(
  Resolver.bintrayRepo("epfl-lara", "princess"),
  Resolver.bintrayIvyRepo("epfl-lara", "sbt-plugins"),
  ("uuverifiers" at "http://logicrunch.research.it.uu.se/maven").withAllowInsecureProtocol(true),
)

addSbtPlugin("ch.epfl.lara" % "sbt-stainless" % "0.6.1")