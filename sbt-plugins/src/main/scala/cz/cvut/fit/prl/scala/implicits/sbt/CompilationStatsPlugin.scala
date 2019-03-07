package cz.cvut.fit.prl.scala.implicits.sbt
import sbt.Keys.{commands, scalacOptions}
import sbt.plugins.JvmPlugin
import sbt.{AutoPlugin, Command, PluginTrigger, Plugins, Project, Setting}

object CompilationStatsPlugin extends AutoPlugin {
  override def requires: Plugins = JvmPlugin

  override def trigger: PluginTrigger = allRequirements

  override def globalSettings = List(
    commands += Command.command("compileWithStats") { s =>
      println("** Compiling with statistics: " + Config.StatisticsScalacOptions)
      val extracted = Project.extract(s)
      val settings: Seq[Setting[_]] = for {
        p <- extracted.structure.allProjectRefs
        setting <- List(
          scalacOptions.in(p) ++= Config.StatisticsScalacOptions
        )
      } yield setting
      val installed = extracted.append(settings, s)
      "compile" :: "test:compile" ::
        installed
    }
  )
}
