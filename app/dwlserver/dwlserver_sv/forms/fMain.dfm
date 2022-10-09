object DWL_Server: TDWL_Server
  AllowPause = False
  DisplayName = 'DWLServer, serving handlers residing in DLLs'
  StartType = stManual
  AfterInstall = ServiceAfterInstall
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
