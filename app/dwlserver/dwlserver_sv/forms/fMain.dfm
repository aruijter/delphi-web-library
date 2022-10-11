object DWL_Server: TDWL_Server
  AllowPause = False
  DisplayName = 'DWLServer'
  StartType = stManual
  AfterInstall = ServiceAfterInstall
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
