unit DWL.DisCo;

interface

uses
  DWL.HTTP.APIClient, DWL.Params;

type
  TdwlDisCo = class
  public
    class var
      FApiSession: TdwlAPISession;
    class destructor Destroy;
    class procedure Initialize(const Disco_BaseURL: string; Authorizer: IdwlAPIAuthorizer);
  end;

  TdwlDiscoClient = class(TdwlDisCo)
    class var
      FAppName: string;
      FConfigParams: IdwlParams;
      FVersionParams: IdwlParams;
      FTempUpdateDir: string;
      F7ZWasChecked: boolean;
    class procedure EmptyTempUpdateDir;
    class procedure Init_Params;
    class function VersionParams: IdwlParams;
    class function GetReleaseInDir(const PackageName, DestinationDir: string; RequestPrerelease: boolean=false): boolean;
    class function TempUpdateDir: string;
  public
    class constructor Create;
    class function AppName: string;
    class function CheckApplicationUpdate(ForceUpdate: boolean=false): boolean;
    class function ConfigParams: IdwlParams;
    class procedure CheckDLL_7Z;
  end;

implementation

uses
  DWL.HTTP.Consts, DWL.IOUtils, System.SysUtils, System.JSON, System.IOUtils,
  Winapi.WinInet, System.Classes, DWL.Compression, DWL.DisCo.Consts,
  System.Math, DWL.OS, sevenzip, DWL.Params.Utils;

{ TdwlDisCo }

class procedure TdwlDisCo.Initialize(const Disco_BaseURL: string; Authorizer: IdwlAPIAuthorizer);
begin
  if FApiSession=nil then
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

class procedure TdwlDiscoClient.CheckDLL_7Z;
const
  Version7z = '18.5';
begin
  if F7ZWasChecked then
    Exit;
  // let the JCL know which dll we want to use...
  var ExeDir := ExtractFileDir(ParamStr(0));
  var ZipDllFn := ExeDir+'\'+SevenZipLibraryName;
  if not (FileExists(ZipDllFn) and
    (TdwlFileVersionInfo.CreateFromFile(ZipDllFn)>=TdwlFileVersionInfo.CreateFromString(Version7Z))) then
    GetReleaseInDir(TdwlFile.ExtractBareName(ZipDllFn), ExeDir);
  F7ZWasChecked := true;
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

class function TdwlDiscoClient.GetReleaseInDir(const PackageName, DestinationDir: string; RequestPrerelease: boolean): boolean;
begin
  Result := false;
  try
    var Response := FApiSession.DoApiRequest('download/package', HTTP_COMMAND_GET, 'packagename='+PackageName+'&kind='+
      IfThen(RequestPrerelease, discoreleasekindPreRelease, discoreleasekindRelease).ToString);
    if (Response=nil) or (Response.StatusCode<>HTTP_STATUS_OK) then
      Exit;
    var FileName: string;
    var DispList := TStringList.Create;
    try
      DispList.Delimiter := ';';
      DispList.DelimitedText := Response.Header[HTTP_HEADER_CONTENT_DISPOSITION];
      FileName := DispList.Values['filename'];
    finally
      DispList.Free;
    end;
    if FileName='' then
      Exit;
    if SameText(Response.Header[HTTP_HEADER_CONTENT_TYPE], CONTENT_TYPE_7Z) then
    begin
      CheckDLL_7Z;
      var ZipFn := TempUpdateDir+'\'+FileName;
      TFile.WriteAllBytes(ZipFn, Response.AsBytes);
      if not TdwlCompression.ExtractArchive(ZipFn, DestinationDir).Success then
        Exit;
    end
    else
      TFile.WriteAllBytes(DestinationDir+'\'+FileName, Response.AsBytes);
    Result :=true;
  except
  end;
end;

class procedure TdwlDiscoClient.Init_Params;
begin
  FConfigParams := New_Params;
  FVersionParams := New_Params;
  var Response := FApiSession.DoApiRequest('phonehome', HTTP_COMMAND_GET, 'appname='+AppName.ToLower{$IFDEF DEBUGGING}+'&profile=localhost' {$ENDIF});
  if Response.StatusCode<>200 then
    raise Exception.Create('Error getting phonehome from API: '+Response.StatusCode.ToString);
  var JSON := TJSONObject(TJSONValue.ParseJSONValue(Response.AsString));
  try
    var Data := JSON.GetValue<TJSONObject>('data.parameters');
    if Data<>nil then
      FConfigParams.WriteJSON(Data);
    Data := JSON.GetValue<TJSONObject>('data.versions');
    if Data<>nil then
      FVersionParams.WriteJSON(Data);
  finally
    JSON.Free;
  end;
  TdwlParamsUtils.Import_CommandLine(FConfigParams);
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
