unit DWL.DisCo;

interface

uses
  DWL.HTTP.APIClient, DWL.Params, DWL.Classes, DWL.HTTP.Types;

type
  TdwlDiscoMsgFunc = procedure(const Msg: string; ProcessingFinished: boolean) of object;

  TdwlDisCo = class
  public
    class var
      FApiSession: TdwlAPISession;
    class destructor Destroy;
    class procedure Initialize(const Disco_BaseURL: string; Authorizer: IdwlAPIAuthorizer); virtual;
  end;

  TdwlDiscoClient = class(TdwlDisCo)
  strict private
    class var
      FAppName: string;
      FConfigParams: IdwlParams;
      FVersionParams: IdwlParams;
      FTempUpdateDir: string;
    class procedure EmptyTempUpdateDir;
    class procedure Init_Params;
    class function VersionParams: IdwlParams;
    class function TempUpdateDir: string;
    class procedure ProgressMsg(const Msg: string; ProcessingFinished: boolean=false);
    class function CheckDLL(const PackageName, FileVersion: string; WithoutAuthentication: boolean=false): boolean;
  protected
    class var
      FProgressBytesFunc: TdwlHTTPProgressEvent;
      FProgressMsgFunc: TdwlDiscoMsgFunc;
  public
    class constructor Create;
    class function AppName: string;
    class function CheckApplicationUpdate(ForceUpdate: boolean=false): boolean;
    class function CheckPreReleaseUpdate: TdwlResult;
    class function CheckPackageUpdate(const PackageName: string; const DestinationDir: string=''): boolean;
    class function ConfigParams: IdwlParams;
    class function CheckDLL_7Z: boolean;
    class function CheckDLL_MySQL: boolean;
    class function CheckDLL_OpenSSL3: boolean;
    class function CheckDLL_WebView2: boolean;
    class function GetReleaseInDir(const PackageName, DestinationDir: string; RequestPrerelease: boolean=false; WithoutAuthentication: boolean=false): boolean;
  end;

implementation

uses
  DWL.HTTP.Consts, DWL.IOUtils, System.SysUtils, System.JSON, System.IOUtils,
  Winapi.WinInet, System.Classes, DWL.Compression, DWL.DisCo.Consts,
  System.Math, DWL.OS, sevenzip, DWL.Params.Utils, DWL.Params.Consts,
  System.StrUtils, DWL.StrUtils, System.NetEncoding;


{ TdwlDisCo }

class procedure TdwlDisCo.Initialize(const Disco_BaseURL: string; Authorizer: IdwlAPIAuthorizer);
begin
  if FApiSession<>nil then
    FApiSession.Free;
  FApiSession := TdwlAPISession.Create(Disco_BaseURL, Authorizer);
end;

class destructor TdwlDisCo.Destroy;
begin
  FApiSession.Free;
  inherited;
end;

{ TdwlDiscoClient }

class function TdwlDiscoClient.AppName: string;
begin
  Result := FAppName;
end;

class function TdwlDiscoClient.CheckApplicationUpdate(ForceUpdate: boolean=false): boolean;
begin
  Result := false;
  if ConfigParams.ContainsKey('skipupdate') then
    Exit;
  var DoUpdate := ForceUpdate;
  if not DoUpdate then
  begin
    var ConfigVersion := TdwlFileVersionInfo.CreateFromString(VersionParams.StrValue(AppName));
    var FileVersion := TdwlFileVersionInfo.CreateFromFile;
    DoUpdate := (not ConfigVersion.IsEmpty) and (not FileVersion.IsEmpty) and (ConfigVersion>FileVersion);
  end;
  if not DoUpdate then
    Exit;
  Result := false;
  EmptyTempUpdateDir;
  if not GetReleaseInDir(AppName, TempUpdateDir) then
    Exit;
  var TempExeFn := TempUpdateDir+'\'+AppName+'.exe';
  if not FileExists(TempExeFn) then
    Exit;
  if not GetReleaseInDir('CoCon', TempUpdateDir) then
    Exit;
  var CoconFn := TempUpdateDir+'\CoCon.exe';
  if not FileExists(CoconFn) then
    Exit;
  var Parm := 'skipupdate';
  for var i := 1 to ParamCount do
    Parm := Parm+' '+ParamStr(i);
  TdwlOS.ExecuteFile(CoconFn, '"'+TempExeFn+'" "'+ParamStr(0)+'" '+Parm);
  Result := true;
end;

class function TdwlDiscoClient.CheckDLL(const PackageName, FileVersion: string; WithoutAuthentication: boolean=false): boolean;
begin
  var ExeDir := ExtractFileDir(ParamStr(0));
  var Fn := ExeDir+'\'+PackageName+'.dll';
  Result := FileExists(Fn) and (TdwlFileVersionInfo.CreateFromFile(Fn)>=TdwlFileVersionInfo.CreateFromString(FileVersion));
  if not Result then
    Result := GetReleaseInDir(PackageName, ExeDir, false, WithoutAuthentication);
end;

class function TdwlDiscoClient.CheckDLL_7Z: boolean;
begin
  {$IFDEF WIN64}
  Result := CheckDLL('7z64', '18.5.0', true);
  {$ELSE}
  Result := CheckDLL('7z', '18.5.0');
  {$ENDIF}
end;

class function TdwlDiscoClient.CheckDLL_MySQL: boolean;
begin
  {$IFDEF WIN64}
  Result := CheckDLL('libmysql64', '5.6.24');
  {$ELSE}
  Result := CheckDLL('libmysql', '5.6.24');
  {$ENDIF}
end;

class function TdwlDiscoClient.CheckDLL_OpenSSL3: boolean;
begin
  {$IFDEF WIN64}
  Result := CheckDLL('libcrypto-3-x64', '3.0.0', true);
  {$ELSE}
  Result := false;
  {$ENDIF}
end;

class function TdwlDiscoClient.CheckDLL_WebView2: boolean;
begin
  {$IFDEF WIN64}
  Result := CheckDLL('WebView2Loader', '1.0.664');
  {$ELSE}
  Result := false;
  {$ENDIF}
end;

class function TdwlDiscoClient.CheckPackageUpdate(const PackageName: string; const DestinationDir: string=''): boolean;
begin
  Result := false;
  var DestDir := DestinationDir;
  if DestDir='' then
    DestDir := ExtractFilePath(ParamStr(0))+PackageName;
  var FnVersion := DestDir+'\version.info';
  var CurrentIsOk := FileExists(FnVersion);
  if CurrentIsOk then // check current version
  begin
    var CurrentVersion := TdwlFileVersionInfo.CreateFromString(TFile.ReadAllText(FnVersion));
    var ConfigVersion := TdwlFileVersionInfo.CreateFromString(TdwlDiscoClient.VersionParams.StrValue(PackageName));
    CurrentIsOk := ConfigVersion.IsEmpty or CurrentVersion.IsEmpty or (CurrentVersion>=ConfigVersion);
  end;
  if CurrentIsOk then
    Exit;
  if DirectoryExists(DestDir) then
    TDirectory.Delete(DestDir, true);
  GetReleaseInDir(PackageName, DestDir);
  Result := true;
end;

class function TdwlDiscoClient.CheckPreReleaseUpdate: TdwlResult;
begin
  var Response := TdwlDisco.FApiSession.ExecuteJSONRequest('release', HTTP_METHOD_GET, 'packagename='+AppName);
  if (not Response.Success) or (Response.JSON_Data.GetValue<integer>('kind')<>discoreleasekindPreRelease) then
  begin
    Result.AddErrorMsg('No PreRelease available right now.');
    Exit;
  end;
  var ConfigVersion: TdwlFileVersionInfo;
  ConfigVersion.SetFromString(Response.JSON_Data.GetValue<string>('version'));
  ConfigVersion.IsPreRelease := true;
  var FileVersion := TdwlFileVersionInfo.CreateFromFile;
  if ConfigVersion.IsEmpty or FileVersion.IsEmpty or (FileVersion>=ConfigVersion) then
  begin
    Result.AddErrorMsg('PreRelease not needed: currrent version is already latest.');
    Exit;
  end;
  EmptyTempUpdateDir;
  if not GetReleaseInDir(AppName, TempUpdateDir, true) then
  begin
    Result.AddErrorMsg('Error while downloading PreRelease');
    Exit;
  end;
  var TempExeFn := TempUpdateDir+'\'+AppName+'.exe';
  if not FileExists(TempExeFn) then
  begin
    Result.AddErrorMsg('Error while downloading PreRelease');
    Exit;
  end;
  if not GetReleaseInDir('CoCon', TempUpdateDir) then
  begin
    Result.AddErrorMsg('Error while downloading PreRelease');
    Exit;
  end;
  var CoconFn := TempUpdateDir+'\CoCon.exe';
  if not FileExists(CoconFn) then
  begin
    Result.AddErrorMsg('Error while downloading PreRelease');
    Exit;
  end;
  var Parm := 'skipupdate';
  for var i := 1 to ParamCount do
    Parm := Parm+' '+ParamStr(i);
  TdwlOS.ExecuteFile(CoconFn, '"'+TempExeFn+'" "'+ParamStr(0)+'" '+Parm);
end;

class function TdwlDiscoClient.ConfigParams: IdwlParams;
begin
  if FConfigParams=nil then
    Init_Params;
  Result := FConfigParams;
end;

class constructor TdwlDiscoClient.Create;
begin
  inherited;
  FAppName := TdwlFile.ExtractBareName;
end;

class procedure TdwlDiscoClient.EmptyTempUpdateDir;
begin
  if not TDirectory.IsEmpty(TempUpdateDir) then
  begin
    TDirectory.Delete(TempUpdateDir, true);
    TDirectory.CreateDirectory(TempUpdateDir);
  end;
end;

class function TdwlDiscoClient.GetReleaseInDir(const PackageName, DestinationDir: string; RequestPrerelease: boolean=false; WithoutAuthentication: boolean=false): boolean;
begin
  Result := false;
  try
    ProgressMsg('Downloading '+PackageName);
    try
      var Response := FApiSession.ExecuteApiRequest(IfThen(WithoutAuthentication, 'download/supportpackage', 'download/package'), HTTP_METHOD_GET, 'packagename='+TNetEncoding.URL.Encode(PackageName)+'&kind='+
        IfThen(RequestPrerelease, discoreleasekindPreRelease, discoreleasekindRelease).ToString, true, WithoutAuthentication, FProgressBytesFunc);
      if (Response=nil) or (Response.StatusCode<>HTTP_STATUS_OK) then
        Exit;
      var FileName: string;
      var DispList := TStringList.Create;
      try
        DispList.Delimiter := ';';
        DispList.DelimitedText := Response.Header[HTTP_FIELD_CONTENT_DISPOSITION];
        FileName := DispList.Values['filename'];
      finally
        DispList.Free;
      end;
      if FileName='' then
        Exit;
      ForceDirectories(DestinationDir);
      if SameText(ExtractFileExt(FileName), '.7z') then
      begin
        CheckDLL_7Z;
        var ZipFn := TempUpdateDir+'\'+FileName;
        TFile.WriteAllBytes(ZipFn, Response.AsBytes);
        if not TdwlCompression.ExtractArchive(ZipFn, DestinationDir).Success then
          Exit;
        try
          TFile.Delete(ZipFn);
        except
        end;
      end
      else
        TFile.WriteAllBytes(DestinationDir+'\'+FileName, Response.AsBytes);
      Result := true;
    finally
      ProgressMsg('', true);
    end;
  except
  end;
end;

class procedure TdwlDiscoClient.Init_Params;

begin
  FConfigParams := New_Params;
  FVersionParams := New_Params;
  var CommandLineParams := New_Params;
  try
    TdwlParamsUtils.Import_CommandLine(CommandLineParams);
  except
  end;
  var Profile := CommandLineParams.StrValue(Param_Profile);
  {$IFDEF DEBUG}
  if Profile='' then
    Profile := 'localhost';
  {$ENDIF}
  var Response: IdwlAPIResponse := nil;
  try
    Response := FApiSession.ExecuteJSONRequest('phonehome', HTTP_METHOD_GET, 'appname='+AppName.ToLower+IfThen(Profile<>'', '&profile='+Profile));
  except
    Response := nil;
  end;
  var Data: TJSONObject := nil;
  var FreeData := false;
  var CacheFn: string;
  try
    // seen the naming convention 'Path' you would expect a trailing backslash, but it comes without
    // create a safe workaround for this:
    CacheFn := TPath.GetCachePath.TrimRight(['\'])+'\DWL\DisCo\PhoneHome';
    ForceDirectories(CacheFn);
    CacheFn := CacheFn+'\'+AppName+IfThen(Profile<>'', '_'+Profile)+'.json';
  except
    CacheFn := '';
  end;
  if (Response<>nil) and Response.Success then
  begin
    Data := Response.JSON_Data;
    try
      if CacheFn<>'' then
        TFile.WriteAllText(CacheFn, Data.ToJSON);
    except
      // Writing to cache never can lead to stopping the flow, resulting in a not updating application
    end;
  end
  else
  begin
    try
      if FileExists(CacheFn) then
      begin
        FreeData := true;
        Data := TJSONObject(TJSONValue.ParseJSONValue(TFile.ReadAllText(CacheFn)));
      end
    except
    end;
  end;
  if Data<>nil then
  begin
    try
      var ParseData := Data.GetValue<TJSONObject>('parameters');
      if ParseData<>nil then
        FConfigParams.WriteJSON(ParseData);
      ParseData := Data.GetValue<TJSONObject>('versions');
      if ParseData<>nil then
        FVersionParams.WriteJSON(ParseData);
    finally
      if FreeData then
        Data.Free;
    end;
  end;
  // Now add commandline params to configparams
  // Doing this at the end give the commandline the highest priority
  try
    CommandLineParams.AssignTo(FConfigParams);
  except
  end;
end;

class procedure TdwlDiscoClient.ProgressMsg(const Msg: string; ProcessingFinished: boolean=false);
begin
  if Assigned(FProgressMsgFunc) then
    FProgressMsgFunc(Msg, ProcessingFinished);
end;

class function TdwlDiscoClient.TempUpdateDir: string;
begin
  if FTempUpdateDir='' then
  begin
    FTempUpdateDir :=TPath.GetTempPath+'DisCo';
    TDirectory.CreateDirectory(FTempUpdateDir);
  end;
  Result := FTempUpdateDir;
end;

class function TdwlDiscoClient.VersionParams: IdwlParams;
begin
  if FVersionParams=nil then
    Init_Params;
  Result := FVersionParams;
end;

end.
