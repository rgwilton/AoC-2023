val scala3Version = "3.3.1"


Global / excludeLintKeys += nativeImageVersion

lazy val root = project
  .in(file("."))
  .settings(
    nativeImageVersion := "22.3.0",
    nativeImageOptions += s"-H:ReflectionConfigurationFiles=${target.value / "native-image-configs" / "reflect-config.json"}",
    nativeImageOptions += s"-H:ConfigurationFileDirectories=${target.value / "native-image-configs" }",
    nativeImageOptions +="-H:+JNI",
  )
  .enablePlugins(NativeImagePlugin)
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "aoc-2023",
    version := "0.1.0",
    //scalacOptions := Seq("-Yexplicit-nulls"),
    scalaVersion := scala3Version,
    Compile / mainClass := Some("aoc.aoc"),
    libraryDependencies ++= Seq("com.novocode" % "junit-interface" % "0.11" % "test",
                                "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
                                "com.lihaoyi" %% "fastparse" % "3.0.2",
                                "com.lihaoyi" %% "requests" % "0.8.0")
  )
