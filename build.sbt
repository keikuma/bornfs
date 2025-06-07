ThisBuild / scalaVersion := "3.3.6"

ThisBuild / organization := "com.example"
ThisBuild / version      := "0.1.0-SNAPSHOT"

Compile / mainClass := Some("Main")

// テスト用に ScalaTest（Scala 3 用）を追加
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.16" % Test,
  "nz.ac.waikato.cms.weka" % "weka-dev" % "3.7.12",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0"
)

semanticdbEnabled := true

scalacOptions ++= Seq(
  "-unchecked",    // 型消去関連のチェック
  "-deprecation",  // 非推奨 API 使用時の警告
  "-feature"       // 言語機能使用時の警告
)

// libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4"

//  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
//  "org.scala-lang.modules" %% "scala-swing" % "1.0.1",

libraryDependencies += "com.github.scopt" %% "scopt" % "4.1.0"
resolvers += Resolver.sonatypeRepo("public")
javacOptions ++= Seq("-source", "11", "-target", "11")

assemblyMergeStrategy in assembly := {
 case PathList("META-INF", _*) => MergeStrategy.discard
 case _                        => MergeStrategy.first
}
// test
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.19"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
