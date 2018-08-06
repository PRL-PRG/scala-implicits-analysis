scriptedLaunchOpts := {
  scriptedLaunchOpts.value ++
    //Seq("-Dplugin.version=" + version.value, "-Xdebug", "-Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=5005")
    Seq("-Dplugin.version=" + version.value)
}

scriptedBufferLog := false