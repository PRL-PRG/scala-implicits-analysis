package cz.cvut.fit.prl.scala.sbt

import sbt.Keys._
import sbt._
import sbt.plugins.JvmPlugin

object SemanticdbCommandPlugin extends AutoPlugin {
  override def requires: Plugins = JvmPlugin

  override def trigger: PluginTrigger = allRequirements

  val V: Map[(Int, Int), String] = Map(
    (2 -> 11) -> "2.11.12",
    (2 -> 12) -> "2.12.4"
  )

  def relevantProjects(state: State): Seq[(ProjectRef, String)] = {
    val extracted = Project.extract(state)
    for {
      p <- extracted.structure.allProjectRefs
      version <- scalaVersion.in(p).get(extracted.structure.data).toList
      partialVersion <- CrossVersion.partialVersion(version).toList
      fullVersion <- V.get(partialVersion).toList
    } yield p -> fullVersion
  }

  val compileAll = taskKey[Unit]("compile all projects in test+compile configs")

  override def globalSettings = List(
    aggregate.in(compileAll) := false,
    compileAll := Def.taskDyn {
      val refs = relevantProjects(state.value).map(_._1)
      println(refs.toList)
      val filter = ScopeFilter(
        projects = inProjects(refs: _*),
        configurations = inConfigurations(Compile, Test))
      compile.all(filter)
    }.value,
    commands += Command.command("semanticdb") { s =>
      val extracted = Project.extract(s)
      val toCompile = List.newBuilder[TaskKey[Analysis]]
      val refs = List.newBuilder[ProjectRef]
      val settings: Seq[Setting[_]] = for {
        (p, fullVersion) <- relevantProjects(s)
        setting <- List(
          scalaVersion.in(p) := fullVersion,
          scalacOptions.in(p) ++= Seq("-Yrangepos", "-P:semanticdb:denotations:all"),
          libraryDependencies.in(p) += compilerPlugin(
            "org.scalameta" % "semanticdb-scalac" % "3.7.4" cross CrossVersion.full)
        )
      } yield setting
      val installed = extracted.append(settings, s)
      "compileAll" ::
        installed
    }
  )
}