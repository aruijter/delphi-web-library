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
      F7ZWasChecked: boolean;
    class procedure EmptyTempUpdateDir;
    class procedure Init_Params;
    class function VersionParams: IdwlParams;
    class function TempUpdateDir: string;
    class procedure ProgressMsg(const Msg: string; ProcessingFinished: boolean=false);
  protected
    class var
      FProgressBytesFunc: TdwlHTTPProgressEvent;
      FProgressMsgFunc: TdwlDiscoMsgFunc;
  public
    class constructor Create;
    class function AppName: string;
    class function CheckApplicationUpdate(ForceUpdate: boolean=false): boolean;
    class function CheckPreReleaseUpdate: TdwlResult;
    class function ConfigParams: IdwlParams;
    class procedure CheckDLL_7Z;
    class function GetReleaseInDir(const PackageName, DestinationDir: string; RequestPrerelease: boolean=false): boolean;
  end;

implementation

uses
  DWL.HTTP.Consts, DWL.IOUtils, System.SysUtils, System.JSON, System.IOUtils,
  Winapi.WinInet, System.Classes, DWL.Compression, DWL.DisCo.Consts,
  System.Math, DWL.OS, sevenzip, DWL.Params.Utils, DWL.Params.Consts,
  System.StrUtils, DWL.StrUtils;


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

class function TdwlDiscoClient.CheckPreReleaseUpdate: TdwlResult;
begin
  var Response := TdwlDisco.FApiSession.ExecuteJSONRequest('release', HTTP_COMMAND_GET, 'packagename='+AppName);
  if (not Response.Success) or (Response.Data.GetValue<integer>('kind')<>discoreleasekindPreRelease) then
  begin
    Result.AddErrorMsg('No PreRelease available right now.');
    Exit;
  end;
  var ConfigVersion: TdwlFileVersionInfo;
  ConfigVersion.SetFromString(Response.Data.GetValue<string>('version'));
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

class function TdwlDiscoClient.GetReleaseInDir(const PackageName, DestinationDir: string; RequestPrerelease: boolean): boolean;
begin
  Result := false;
  try
    ProgressMsg('Downloading '+PackageName);
    try
      var Response := FApiSession.ExecuteApiRequest('download/package', HTTP_COMMAND_GET, 'packagename='+PackageName+'&kind='+
        IfThen(RequestPrerelease, discoreleasekindPreRelease, discoreleasekindRelease).ToString, true, false, FProgressBytesFunc);
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
      if SameText(ExtractFileExt(FileName), '.7z') then
      begin
        CheckDLL_7Z;
        var ZipFn := TempUpdateDir+'\'+FileName;
        TFile.WriteAllBytes(ZipFn, Response.AsBytes);
        if not TdwlCompression.ExtractArchive(ZipFn, DestinationDir).Success then
          Exit;
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
  TdwlParamsUtils.Import_CommandLine(CommandLineParams);
  var Profile := CommandLineParams.StrValue(Param_Profile);
  {$IFDEF DEBUG}
  if Profile='' then
    Profile := 'localhost';
  {$ENDIF}
  // seen the naming convention 'Path' you would expect a trailing backslash, but it comes withou
  // create a safe workaround for this:
  var CacheFn := TPath.GetPublicPath.TrimRight(['\'])+'\DisCo\PhoneHome';
  ForceDirectories(CacheFn);
  CacheFn := CacheFn+'\'+AppName+IfThen(Profile<>'', '_'+Profile)+'.json';
  var Response := FApiSession.ExecuteJSONRequest('phonehome', HTTP_COMMAND_GET, 'appname='+AppName.ToLower+IfThen(Profile<>'', '&profile='+Profile));
  var Data: TJSONObject;
  if Response.Success then
  begin
    Data := Response.Data;
    TFile.WriteAllText(CacheFn, Data.ToJSON);
  end
  else
  begin
    if FileExists(CacheFn) then
      Data := TJSONObject(TJSONValue.ParseJSONValue(TFile.ReadAllText(CacheFn)))
    else
     Data := nil;
  end;
  if Data<>nil then
  begin
    var ParseData := Data.GetValue<TJSONObject>('parameters');
    if ParseData<>nil then
      FConfigParams.WriteJSON(ParseData);
    ParseData := Data.GetValue<TJSONObject>('versions');
    if ParseData<>nil then
      FVersionParams.WriteJSON(ParseData);
  end;
  // Now add commandline params to configparams
  // Doing this at the end give the commandline the highest priority
  CommandLineParams.AssignTo(FConfigParams);
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
