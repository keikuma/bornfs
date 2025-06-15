scalaVersion := "3.3.1"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies += "nz.ac.waikato.cms.weka" % "weka-dev" % "3.7.12"

libraryDependencies += "com.github.scopt" %% "scopt" % "4.1.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"

resolvers += Resolver.sonatypeRepo("public")
javacOptions ++= Seq("-source", "11", "-target", "11")

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", _*) => MergeStrategy.discard
  case _                        => MergeStrategy.first
}

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.19"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test
