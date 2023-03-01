unit DisCo.Handler;

interface

uses
  DWL.HTTP.Server.Handler.DLL.Classes, DWL.HTTP.Server.Types;

type
  THandler_DisCo = class(TdwlDLLHandling_OpenID)
  strict private
    class var FAdditionalParametersSQL: string;
    class function Get_phonehome(const State: PdwlHTTPHandlingState): boolean;
    class function Get_download_package(const State: PdwlHTTPHandlingState): boolean;
    class function Get_release(const State: PdwlHTTPHandlingState): boolean;
    class function Get_releases(const State: PdwlHTTPHandlingState): boolean;
    class function Post_profileip(const State: PdwlHTTPHandlingState): boolean;
    class function Post_upload_package(const State: PdwlHTTPHandlingState): boolean;
  public
    class procedure Configure(const Params: string); override;
  end;


implementation

uses
  DWL.HTTP.Consts, DWL.HTTP.Server.Utils, DWL.MySQL, DWL.Params.Consts,
  System.JSON, DWL.Resolver, System.StrUtils, System.SysUtils, Winapi.WinInet,
  DWL.HTTP.Server.Globals, DWL.DisCo.Consts, System.DateUtils;

const
  Param_Additional_parameters_SQL = 'additional_parameters_sql';

{ THandler_DisCo }

class procedure THandler_DisCo.Configure(const Params: string);
const
  SQL_CheckTable_AppPackages = 'CREATE TABLE IF NOT EXISTS `dwl_disco_apppackages` (id INT AUTO_INCREMENT, appname VARCHAR(50), packagename VARCHAR(50),	PRIMARY KEY (id), INDEX appnameIndex (appname))';
  SQL_CheckTable_Releases = 'CREATE TABLE IF NOT EXISTS dwl_disco_releases (id INT AUTO_INCREMENT, packagename VARCHAR(50), version VARCHAR(20), releasemoment DATETIME, kind TINYINT, data LONGBLOB, fileextension VARCHAR(10), PRIMARY KEY (id)'+',	INDEX packagenamereleasemomentIndex (packagename, releasemoment))';
  SQL_CheckTable_ProfileParameters = 'CREATE TABLE IF NOT EXISTS dwl_disco_profileparameters (id INT AUTO_INCREMENT, appname VARCHAR(50), profile VARCHAR(50),	`key` VARCHAR(50), value VARCHAR(100), PRIMARY KEY (id), INDEX `appnameprofileIndex` (appname, profile))';
  SQL_CheckTable_KnownIps = 'CREATE TABLE IF NOT EXISTS dwl_disco_known_ipaddresses'+' (id INT AUTO_INCREMENT, ipaddress VARCHAR(50), profile VARCHAR(50), lastseen DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP, PRIMARY KEY (id), INDEX `profileIndex` (profile), 	UNIQUE INDEX IpIndex (ipaddress))';
begin
  inherited Configure(Params);
  FConfigParams.WriteValue(Param_CreateDatabase, true);
  FConfigParams.WriteValue(Param_TestConnection, true);
  var Session := New_MySQLSession(FConfigParams);
  FConfigParams.ClearKey(Param_CreateDatabase);
  FConfigParams.ClearKey(Param_TestConnection);
  Session.CreateCommand(SQL_CheckTable_AppPackages).Execute;
  Session.CreateCommand(SQL_CheckTable_Releases).Execute;
  Session.CreateCommand(SQL_CheckTable_ProfileParameters).Execute;
  Session.CreateCommand(SQL_CheckTable_KnownIps).Execute;
  FAdditionalParametersSQL := FConfigParams.StrValue(Param_Additional_parameters_SQL);
  var AdminScope := FConfigParams.StrValue('scope_admin', 'disco_admin');
  RegisterHandling(dwlhttpGET, '/phonehome', Get_phonehome, []);
  RegisterHandling(dwlhttpGET, '/download/package', Get_download_package, []);
  RegisterHandling(dwlhttpGET, '/release', Get_release, []);
  RegisterHandling(dwlhttpGET, '/releases', Get_releases, [AdminScope]);
  RegisterHandling(dwlhttpPOST, '/profileip', Post_profileip, [AdminScope]);
  RegisterHandling(dwlhttpPOST, '/upload/package', Post_upload_package, [AdminScope]);
end;

class function THandler_DisCo.Get_phonehome(const State: PdwlHTTPHandlingState): boolean;
const
  SQL_Get_ProfileByIP = 'SELECT profile from dwl_disco_known_ipaddresses WHERE ipaddress=?';
  SQL_Get_EmptyProfileParameters = 'SELECT `key`, value FROM dwl_disco_profileparameters WHERE ((appname IS NULL) OR (appname="") OR (appname=?)) AND ((profile IS NULL) or (profile=""))';
  SQL_Get_ProfileParameters = 'SELECT `key`, value FROM dwl_disco_profileparameters WHERE ((appname IS NULL) OR (appname="") OR (appname=?)) AND (profile=?)';
  SQL_Get_AppVersion = 'SELECT version FROM dwl_disco_releases WHERE (packagename=?) AND (kind=0) ORDER BY ReleaseMoment DESC LIMIT 1';
  SQL_Get_PackageVersions = 'SELECT ap.packagename, (SELECT version FROM dwl_disco_releases r WHERE (r.packagename=ap.packagename) AND (r.kind=0) ORDER BY ReleaseMoment DESC LIMIT 1) FROM dwl_disco_apppackages ap WHERE (ap.appname=?)';

  procedure AddOrOverwritePair(JSON: TJSONObject; const Str, Val: string);
  begin
    var Pair := JSON.RemovePair(Str);
    if Pair<>nil then
      Pair.Free;
    JSON.AddPair(Str, Val);
  end;
begin
  Result := true;
  var AppName: string;
  if not TryGetRequestParamStr(State, 'appname', AppName, true) then
    Exit;
  var Profile: string;
  if not TryGetRequestParamStr(State, 'profile', Profile) then
  begin
    Profile := 'default';
    var RemoteIp: string;
    if TryGetRequestParamStr(State, 'remoteip', RemoteIP) then
    begin
      var Cmd := MySQLCommand(State, SQL_Get_ProfileByIP);
      Cmd.Parameters.SetTextDataBinding(0, RemoteIp);
      Cmd.Execute;
      if Cmd.Reader.Read then
        Profile := Cmd.Reader.GetString(0);
    end;
  end;

  // Put the parameters into Result Json
  var JSONParams := TJSONObject.Create;
  JSON_Data(State).AddPair('parameters', JSONParams);
  // First get profile='', these will be always added
  var Cmd := MySQLCommand(State, SQL_Get_EmptyProfileParameters);
  Cmd.Parameters.SetTextDataBinding(0, AppName);
  Cmd.Execute;
  while Cmd.Reader.Read do
    AddOrOverwritePair(JSONParams, Cmd.Reader.GetString(0), Cmd.Reader.GetString(1));
  // Now get specific profile, this will override the same values (as designed)
  Cmd := MySQLCommand(State, SQL_Get_ProfileParameters);
  Cmd.Parameters.SetTextDataBinding(0, AppName);
  Cmd.Parameters.SetTextDataBinding(1, Profile);
  Cmd.Execute(true);
  while Cmd.Reader.Read do
    AddOrOverwritePair(JSONParams, Cmd.Reader.GetString(0), Cmd.Reader.GetString(1));

  // Now Add additional parameters from external query
  if FAdditionalParametersSQL<>'' then
  begin
    var Par_SQL  := FAdditionalParametersSQL;
    Par_SQL := ReplaceStr(Par_SQL, '$(appname)', AppName);
    Par_SQL := ReplaceStr(Par_SQL, '$(profile)', Profile);
    Cmd := MySQLCommand(State, Par_SQL);
    Cmd.Execute;
    while Cmd.Reader.Read do
      AddOrOverwritePair(JSONParams, Cmd.Reader.GetString(0), Cmd.Reader.GetString(1));
  end;
  var JSONVersions := TJSONObject.Create;
  JSON_Data(State).AddPair('versions', JSONVersions);
  Cmd := MySQLCommand(State, SQL_Get_AppVersion);
  Cmd.Parameters.SetTextDataBinding(0, AppName);
  Cmd.Execute;
  if Cmd.Reader.Read then
    JSONVersions.AddPair(AppName, Cmd.Reader.GetString(0));
  Cmd := MySQLCommand(State, SQL_Get_PackageVersions);
  Cmd.Parameters.SetTextDataBinding(0, AppName);
  Cmd.Execute;
  while Cmd.Reader.Read do
    JSONVersions.AddPair(Cmd.Reader.GetString(0), Cmd.Reader.GetString(1));
  JSON_Set_Success(State);
end;

class function THandler_DisCo.Get_release(const State: PdwlHTTPHandlingState): boolean;
const
  SQl_GetRelease = 'SELECT version, kind FROM dwl_disco_releases WHERE (packagename=?) ORDER BY ReleaseMoment DESC LIMIT 1';
begin
  Result := true;
  var PackageName: string;
  if not TryGetRequestParamStr(State, 'packagename', PackageName, true) then
    Exit;
  var Cmd := MySQLCommand(State, SQl_GetRelease);
  Cmd.Parameters.SetTextDataBinding(0, PackageName);
  Cmd.Execute;
  if Cmd.Reader.Read then
  begin
    var JSON:= JSON_Data(State);
    JSON.AddPair('version', Cmd.Reader.GetString(0));
    JSON.AddPair('kind', TJSONNumber.Create(Cmd.Reader.GetInteger(1)));
    JSON_Set_Success(State);
  end
  else
    State.StatusCode := HTTP_STATUS_NO_CONTENT;
end;

class function THandler_DisCo.Get_releases(const State: PdwlHTTPHandlingState): boolean;
const
  SQL_GetReleases = 'SELECT id, packagename, version, releasemoment, kind, fileextension FROM dwl_disco_releases';
begin
  Result := true;
  var Cmd := MySQLCommand(State, SQL_GetReleases);
  Cmd.Execute;
  var JSON:= JSON_Data(State);
  var Releases := TJSONArray.Create;
  JSON.AddPair('releases', Releases);
  while Cmd.Reader.Read do
  begin
    var Release := TJSONObject.Create;
    Releases.Add(Release);
    Release.AddPair('id', Cmd.Reader.GetString(0, true));
    Release.AddPair('packagename', Cmd.Reader.GetString(1, true));
    Release.AddPair('version', Cmd.Reader.GetString(2, true));
    Release.AddPair('releasemoment', DateToISO8601(Cmd.Reader.GetDateTime(3, true)));
    Release.AddPair('kind', TJSONNumber.Create(Cmd.Reader.GetInteger(4, true)));
    Release.AddPair('fileextension', Cmd.Reader.GetString(5, true));
  end;
  JSON_Set_Success(State);
end;

class function THandler_DisCo.Get_download_package(const State: PdwlHTTPHandlingState): boolean;
const
  SQL_GetRelease = 'SELECT data, fileextension, packagename FROM dwl_disco_releases WHERE (packagename=?) AND (Kind=?) ORDER BY ReleaseMoment DESC LIMIT 1';
  SQL_GetReleaseById = 'SELECT data, fileextension, packagename FROM dwl_disco_releases WHERE (id=?)';
begin
  Result := true;
  var Id: integer;
  var Cmd: IdwlMySQLCommand;
  if TryGetRequestParamInt(State, 'id', Id) then
  begin
    Cmd := MySQLCommand(State, SQL_GetReleaseById);
    Cmd.Parameters.SetIntegerDataBinding(0, Id);
  end
  else
  begin
    var PackageName: string;
    if not TryGetRequestParamStr(State, 'packagename', PackageName, true) then
      Exit;
    var Kind: integer;
    if not TryGetRequestParamInt(State, 'kind', Kind) then
      Kind := discoreleasekindRelease;
    Cmd := MySQLCommand(State, SQl_GetRelease);
    Cmd.Parameters.SetTextDataBinding(0, PackageName);
    Cmd.Parameters.SetIntegerDataBinding(1, Kind);
  end;
  Cmd.Execute;
  if Cmd.Reader.Read then
  begin
    Cmd.Reader.GetBlobRef(0, procedure(const pBuffer: Pointer; const dwDataSize: cardinal; var MemoryOwnerShipTaken: boolean)
      begin
        if dwDataSize > 0 then
        begin
          var ContentBuffer: pointer := nil;
          var Size: Int64 := dwDataSize;
          serverProcs.ArrangeContentBufferProc(State, ContentBuffer, Size);
          Move(pBuffer^, ContentBuffer^, Size);
          State.SetContentType(CONTENT_TYPE_OCTET_STREAM);
          State.SetHeaderValue(HTTP_HEADER_CONTENT_DISPOSITION, 'filename='+Cmd.Reader.GetString(2, true)+'.'+Cmd.Reader.GetString(1));
        end
        else
          State.StatusCode := HTTP_STATUS_NO_CONTENT;
      end);
  end
  else
    State.StatusCode := HTTP_STATUS_NO_CONTENT;
end;

class function THandler_DisCo.Post_profileip(const State: PdwlHTTPHandlingState): boolean;
const
  SQL_Post_Profile = 'INSERT INTO dwl_disco_known_ipaddresses (ipaddress, profile, lastseen) VALUES (?,?,?) ON DUPLICATE KEY UPDATE profile=VALUES(profile), lastseen=VALUES(lastseen)';
begin
  Result := true;
  var Profile: string;
  if not TryGetJSONParam<string>(State, nil, 'profile', Profile, true) then
    Exit;
  var RemoteIP: string;
  if not TryGetRequestParamStr(State, 'remoteip', RemoteIP, true) then
    Exit;
 var Cmd := MySQLCommand(State, SQL_Post_Profile);
 Cmd.Parameters.SetTextDataBinding(0, RemoteIP);
 Cmd.Parameters.SetTextDataBinding(1, Profile);
 Cmd.Parameters.SetDateTimeDataBinding(2, Now);
 Cmd.Execute;
 JSON_Set_Success(State);
end;

class function THandler_DisCo.Post_upload_package(const State: PdwlHTTPHandlingState): boolean;
const
  SQL_Post_Release = 'INSERT INTO dwl_disco_releases (packagename, version, releasemoment, kind, fileextension, data) VALUES (?,?,?,?,?,?)';
begin
  Result := true;
  var PackageName: string;
  var Version: string;
  var Kind: string;
  var FileExtension: string;
  if not (TryGetHeaderValue(State, 'packagename', PackageName, true) and TryGetHeaderValue(State, 'version', Version, true) and
    TryGetHeaderValue(State, 'kind', Kind, true) and TryGetHeaderValue(State, 'fileextension', FileExtension, true)) then
    Exit;
  var Data: pointer;
  var DataSize: Int64;
  if not TryGetPayloadPtr(State, Data, DataSize) then
  begin
    JSON_AddError(State, 99, 'missing payload', HTTP_STATUS_BAD_REQUEST);
    Exit;
  end;
 var Cmd := MySQLCommand(State, SQL_Post_Release);
 Cmd.Parameters.SetTextDataBinding(0, PackageName.ToLower);
 Cmd.Parameters.SetTextDataBinding(1, Version);
 Cmd.Parameters.SetDateTimeDataBinding(2, Now);
 Cmd.Parameters.SetTextDataBinding(3, Kind);
 Cmd.Parameters.SetTextDataBinding(4, FileExtension);
 Cmd.Parameters.SetBinaryRefDataBinding(5, Data, DataSize);
 Cmd.Execute;
 JSON_Set_Success(State);
end;

end.
