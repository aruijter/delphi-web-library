unit DWLServer.Server;

interface

uses
  System.Threading, DWL.Params, DWL.HTTP.Server.Handler.Log, DWL.Logging,
  DWL.HTTP.Server, DWL.HTTP.Server.Handler.Mail;

type
  TdwlServerCore = class
  strict private
    FDLLBasePath: string;
    FIsStopping: boolean;
    FExecutionTask: ITask;
    FParams: IdwlParams;
    FMySQL_Profile: IdwlParams;
    FLogHandler: TdwlHTTPHandler_Log;
    procedure Execute;
    procedure InitDatabase;
    procedure LoadDLLHandlers(HTTPServer: TdwlHTTPServer);
    procedure LoadURIAliases(RestServer: TdwlHTTPServer);
    procedure DoLog(LogItem: PdwlLogItem);
  private
    function GetIsRunning: boolean;
  public
    property IsRunning: boolean read GetIsRunning;
    destructor Destroy; override;
    procedure Start(Params: IdwlParams);
    procedure Stop;
  end;

type
  TFeedConfigProc = procedure(Params: IdwlParams) of object;

implementation

uses
  System.SysUtils, System.Classes, DWL.ACME,
  System.Rtti, DWL.Params.Consts, DWL.Logging.Callback, System.StrUtils,
  DWL.MySQL, DWL.HTTP.Server.Types, DWL.HTTP.Server.Handler.DLL,
  System.Generics.Collections, Winapi.Windows, Winapi.ShLwApi, DWLServer.Consts,
  DWL.HTTP.Consts, DWL.Mail.Queue, IdAssignedNumbers, System.Math;

const
  SQL_CheckTable_Handlers =
    'CREATE TABLE IF NOT EXISTS dwl_handlers (id INT AUTO_INCREMENT, open_order SMALLINT, endpoint VARCHAR(50), handler_uri VARCHAR(255), `params` TEXT NULL, INDEX `primaryindex` (`id`))';
  SQL_CheckTable_UriAliases =
    'CREATE TABLE IF NOT EXISTS dwl_urialiases (id INT AUTO_INCREMENT, alias VARCHAR(255), uri VARCHAR(255), PRIMARY KEY (id))';
  SQL_Get_UriAliases =
    'SELECT alias, uri FROM dwl_urialiases';

{ TdwlServerCore }

destructor TdwlServerCore.Destroy;
begin
  Stop;
  inherited Destroy;
end;

procedure TdwlServerCore.DoLog(LogItem: PdwlLogItem);
begin
  if FLogHandler<>nil then
    FLogHandler.SubmitLog('', integer(LogItem.SeverityLevel), LogItem.Source, LogItem.Channel,
      LogItem.Topic, LogItem.Msg, LogItem.ContentType, LogItem.Content);
end;

procedure TdwlServerCore.Execute;
var
  HTTPServer: TdwlHTTPServer;
  ACMECLient: TdwlACMEClient;

  function CheckACME: boolean;
  begin
    if ACMEClient.Domain='' then
      Exit(false);
    try
      TdwlLogger.Log('ACME: checking certificate for '+ACMEClient.Domain, lsTrace);
      ACMECLient.CheckCertificate;
      ACMEClient.LogCertificateStatus;
      var GotANewCertificate := false;
      if (ACMEClient.CertificateStatus in [certstatNotFound, certstatExpired, certstatAboutToExpire]) then
      begin
        ACMEClient.GetOrRenewCertificate;
        GotANewCertificate := ACMEClient.CertificateStatus=certstatOk;
      end;
      if GotANewCertificate or not HTTPServer.Active then
      begin
        if ACMEClient.CertificateStatus in [certstatAboutToExpire, certstatOk] then
        begin
          if not HTTPServer.Active then
          begin
            // feed new certificate to server
            HTTPServer.AddHostName(ACMEClient.Domain, ACMECLient.CertFile, ACMECLient.RootCertFile, ACMECLient.KeyFile);
            TdwlLogger.Log('Activated ACME Certificate for '+ACMEClient.Domain, lsTrace);
          end
          else
          begin
            // we need a reboot to activate the new certificate
            var ConfigChangeProc: TValue;
            TdwlLogger.Log('Requesting server restart due to changed ACME Certificate for '+ACMEClient.Domain, lsTrace);
            if FParams.TryGetValue(Param_FeedConfigProc, ConfigChangeProc) then
              ConfigChangeProc.AsType<TFeedConfigProc>(false)(nil);
          end;
        end
        else
          TdwlLogger.Log('Switching to ACME Certificate failed', lsError);
      end
      else
    finally
      Result := ACMECLient.CertificateStatus in [certstatAboutToExpire, certstatOk];
    end;
  end;
const
  SleepDelay=250;
  DeLaysInADay=24*60*60*1000 div SleepDelay;
var
  DelayCount: cardinal;
begin
  TdwlLogger.Log('DWL Server bootstrap started', lsTrace);
  try
    FMySQL_Profile := New_Params;
    FParams.AssignTo(FMySQL_Profile, Params_SQLConnection);
    InitDatabase;
    FLogHandler := TdwlHTTPHandler_Log.Create(FParams); // init before activating DoLog!
    TdwlMailQueue.Configure(FParams, true);
    var CallBackLogDispatcher := EnableLogDispatchingToCallback(false, DoLog);
    ACMECLient := TdwlACMEClient.Create;
    try
      ACMECLient.Domain := FParams.StrValue(Param_ACMEDomain);
      ACMECLient.ProfileCountryCode := FParams.StrValue(Param_ACMECountry);
      ACMECLient.ProfileState := FParams.StrValue(Param_ACMEState);
      ACMECLient.ProfileCity := FParams.StrValue(Param_ACMECity);
      ACMEClient.CallBackPortNumber := FParams.IntValue(Param_ACMEPort, ParamDef_ACMEPort);
      HTTPServer := TdwlHTTPServer.Create;
      try
        var IP: string;
        if FParams.TryGetStrValue(Param_Binding_IP, IP) then
        begin
          TdwlLogger.Log('Bound to specific IP '+IP, lsTrace);
          ACMECLient.ChallengeIP := IP;
        end
        else
          IP := '';
        CheckACME;
        var Port: integer;
        if not FParams.TryGetIntValue(Param_Binding_Port, Port) then
        begin
          if HTTPServer.IsSecure then
            Port := IdPORT_https
          else
            Port := IdPORT_HTTP;
        end;
        var Binding := HTTPServer.Bindings.Add;
        if IP<>'' then
          Binding.IP := IP;
        Binding.Port := Port;
        TdwlLogger.Log('Bound to '+IfThen(Binding.IP='', '*', Binding.IP)+':'+Binding.Port.ToString);
        LoadURIAliases(HTTPServer);
        HTTPServer.Open;
        HTTPServer.RegisterHandler(EndpointURI_Log,  FLogHandler);
        HTTPServer.LogLevel := FParams.IntValue(Param_LogLevel, httplogLevelWarning);
        TdwlLogger.Log('Enabled Request logging (level '+HTTPServer.LogLevel.ToString+')', lsTrace);
        HTTPServer.RegisterHandler(EndpointURI_Mail,  TdwlHTTPHandler_Mail.Create(FParams));
        if not HTTPServer.IsSecure then
        begin
          HTTPServer.OnlyLocalConnections := FParams.BoolValue(Param_TestMode);
          if HTTPServer.OnlyLocalConnections then
            TdwlLogger.Log('SERVER IS NOT SECURE, only allowing local connections', lsWarning)
          else
            TdwlLogger.Log('SERVER IS NOT SECURE, please configure or review ACME parameters', lsWarning);
        end;
        TdwlLogger.Log('Opened HTTP Server', lsNotice);
        FDLLBasePath := FParams.StrValue('DLLBasePath', ExtractFileDir(ParamStr(0)));
        if HTTPServer.IsSecure or HTTPServer.OnlyLocalConnections then
          LoadDLLHandlers(HTTPServer)
        else
          TdwlLogger.Log('Skipped loading of handlers because server is not secure', lsWarning);
        TdwlLogger.Log('DWL Server bootstrap finished', lsTrace);
        while not FIsStopping do
        begin
          DelayCount := DeLaysInADay;
          while (DelayCount>0) and not FIsStopping do
          begin
            Sleep(SleepDelay);
            dec(DelayCount);
          end;
          if not FIsStopping then
            CheckACME;
        end;
        TdwlLogger.Log('Closing HTTP Server', lsNotice);
        FLogHandler := nil; // do not try to log when server goes down
        TdwlLogger.FinalizeDispatching;
      finally
        HTTPServer.Free;
      end;
    finally
      TdwlLogger.UnregisterDispatcher(CallBackLogDispatcher);
      ACMECLient.Free;
    end;
  except
    on E: Exception do
      TdwlLogger.Log('Exception occured: ' + E.Message, lsError);
  end;
end;

function TdwlServerCore.GetIsRunning: boolean;
begin
  Result := (FExecutionTask<>nil) and (FExecutionTask.Status=TTaskStatus.Running);
end;

procedure TdwlServerCore.InitDatabase;
begin
  FMySQL_Profile.WriteValue(Param_CreateDatabase, true);
  FMySQL_Profile.WriteValue(Param_TestConnection, true);
  var Session := New_MySQLSession(FMySQL_Profile);
  FMySQL_Profile.ClearKey(Param_CreateDatabase);
  FMySQL_Profile.ClearKey(Param_TestConnection);
  Session.CreateCommand(SQL_CheckTable_Handlers).Execute;
  Session.CreateCommand(SQL_CheckTable_UriAliases).Execute;
end;

procedure TdwlServerCore.LoadDLLHandlers(HTTPServer: TdwlHTTPServer);
const
  SQL_Get_Resthandlers =
    'SELECT endpoint, handler_uri, params FROM dwl_handlers ORDER BY open_order';
var
  DLLHandle: HModule;
  ProcessProc: TDLL_ProcessRequestProc;
  AuthorizeProc: TDLL_AuthorizeProc;
  Cmd: IdwlMySQlCommand;
  HandlerParams: IdwlParams;
  URI: string;
  FileName: string;
  EndPoint: string;
  L: integer;
  Handler: TdwlHTTPHandler_DLL;
begin
  FParams.WriteValue(Param_BaseURI, HTTPServer.BaseURI);
  var Issuer := FParams.StrValue(Param_Issuer);
  if Issuer='' then
  begin
    Issuer :=  FParams.StrValue(Param_BaseURI)+Default_EndpointURI_OAuth2;
    TdwlLogger.Log('No issuer configured, default applied: '+Issuer, lsNotice);
    FParams.WriteValue(Param_Issuer, Issuer);
  end;
  try
    Cmd := New_MySQLSession(FMySQL_Profile).CreateCommand(SQL_Get_Resthandlers);
    Cmd.Execute;
    while Cmd.Reader.Read do
    begin
      try
        EndPoint := Cmd.Reader.GetString(0);
        URI := Cmd.Reader.GetString(1);
        HandlerParams := New_Params;
        FParams.AssignTo(HandlerParams);
        HandlerParams.WriteNameValueText(Cmd.Reader.GetString(2 ,true));
        if not HandlerParams.BoolValue(Param_Enabled, true) then
          Continue;
        HandlerParams.WriteValue(Param_Endpoint, Endpoint);
        try
          if HTTPServer.UnRegisterHandler(EndPoint) then
            TdwlLogger.Log('Unregistered DLL Handler at endpoint '+Endpoint, lsTrace);
          if SameText(Copy(URI, 1, 17), 'file://localhost/') then
            FileName := FDLLBasePath+ReplaceStr(Copy(URI, 17, MaxInt), '/', '\')
          else
          begin
            L := MAX_PATH;
            SetLength(FileName, L);
            if PathCreateFromUrl(PChar(URI), PChar(FileName), @L, 0)<>S_OK then
              raise Exception.Create('Invalid URI');
            SetLength(FileName, L);
          end;
          if not FileExists(FileName) then
          begin
            TdwlLogger.Log('Missing DLL '+URI+' ('+FileName+') for endpoint '+Endpoint, lsError);
            Continue;
          end;
          DLLHandle := LoadLibrary(PChar(FileName));
          if DLLHandle=0 then
            raise Exception.Create('LoadLibrary failed');
          ProcessProc := GetProcAddress(DLLHandle, 'ProcessRequest');
          AuthorizeProc := GetProcAddress(DLLHandle, 'Authorize');
          if Assigned(ProcessProc) and Assigned(AuthorizeProc) then
          begin
            Handler := TdwlHTTPHandler_DLL.Create(DLLHandle, ProcessProc, AuthorizeProc, EndPoint, HandlerParams);
            HTTPServer.RegisterHandler(EndPoint, Handler);
            TdwlLogger.Log('Registered DLL Handler '+ExtractFileName(FileName)+' at endpoint '+Endpoint, lsTrace);
          end
          else
          begin
            FreeLibrary(DLLHandle);
            raise Exception.Create('No ProcessRequest or Authorize function found.');
          end;
        except
          on E: Exception do
            TdwlLogger.Log('Failed loading DLL '+URI+'('+FileName+') on endpoint '+Endpoint+': '+E.Message, lsError);
        end;
      except
        on E: Exception do
          TdwlLogger.Log('Error loading DLL handler at '+EndPoint+': '+E.Message, lsError);
      end;
    end;
  except
    on E: Exception do
      TdwlLogger.Log('Error loading DLL handlers: '+E.Message, lsError);
  end;
end;

procedure TdwlServerCore.LoadURIAliases(RestServer: TdwlHTTPServer);
begin
  try
    RestServer.ClearURIAliases;
    var Cmd := New_MySQLSession(FMySQL_Profile).CreateCommand(SQL_Get_UriAliases);
    Cmd.Execute;
    while Cmd.Reader.Read do
      RestServer.AddURIAlias(Cmd.Reader.GetString(0), Cmd.Reader.GetString(1));
  except
    on E: Exception do
      TdwlLogger.Log('Error loading URI Aliases: '+E.Message, lsError);
  end;
end;

procedure TdwlServerCore.Start(Params: IdwlParams);
begin
  if IsRunning then
    Exit;
  FParams := Params;
  FIsStopping := false;
  FExecutionTask := TTask.Create(procedure()
    begin
      Execute;
    end);
  FExecutionTask.Start;
end;

procedure TdwlServerCore.Stop;
begin
  FIsStopping := true;
  if FExecutionTask<>nil then
    FExecutionTask.Wait(5000);
end;

end.
