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
    class function Post_profileip(const State: PdwlHTTPHandlingState): boolean;
  public
//    class function Authorize(const State: PdwlHTTPHandlingState): boolean; override;
    class procedure Configure(const Params: string); override;
  end;


implementation

uses
  DWL.HTTP.Consts, DWL.HTTP.Server.Utils, DWL.MySQL, DWL.Params.Consts,
  System.JSON, DWL.Resolver, System.StrUtils, System.SysUtils, Winapi.WinInet,
  DWL.HTTP.Server.Globals, DWL.DisCo.Consts;

const
  Param_Additional_parameters_SQL = 'additional_parameters_sql';

{ THandler_DisCo }

//class function THandler_DisCo.Authorize(const State: PdwlHTTPHandlingState): boolean;
//begin
//  Result := true;
//end;

class procedure THandler_DisCo.Configure(const Params: string);
const
  SQL_CheckTable_AppPackages = 'CREATE TABLE IF NOT EXISTS `dwl_disco_apppackages` (id INT AUTO_INCREMENT, appname VARCHAR(50), packagename VARCHAR(50),	PRIMARY KEY (id), INDEX appnameIndex (appname))';
  SQL_CheckTable_Releases = 'CREATE TABLE IF NOT EXISTS dwl_disco_releases (id INT AUTO_INCREMENT, packagename VARCHAR(50), version VARCHAR(20), build SMALLINT, releasemoment DATETIME, kind TINYINT, data LONGBLOB, mediatype VARCHAR(50), fileextension VARCHAR(10), PRIMARY KEY (id)'+',	INDEX packagenamereleasemomentIndex (packagename, releasemoment))';
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
  RegisterHandling(dwlhttpGET, '/phonehome', Get_phonehome, []);
  RegisterHandling(dwlhttpGET, '/download/package', Get_download_package, []);
  RegisterHandling(dwlhttpPOST, '/profileip', Post_profileip, []);
end;

class function THandler_DisCo.Get_phonehome(const State: PdwlHTTPHandlingState): boolean;
const
  SQL_Get_ProfileByIP = 'SELECT profile from dwl_disco_known_ipaddresses WHERE ipaddress=?';
  SQL_Get_ProfileParameters = 'SELECT `key`, value FROM dwl_disco_profileparameters WHERE ((appname IS NULL) OR (appname="") OR (appname=?)) AND ((profile IS NULL) or (profile="") OR (profile=?))';
  SQL_Get_AppVersion = 'SELECT version FROM dwl_disco_releases r WHERE (r.packagename=?) ORDER BY ReleaseMoment DESC LIMIT 1';
  SQL_Get_PackageVersions = 'SELECT ap.packagename, (SELECT version FROM dwl_disco_releases r WHERE (ap.packagename=r.packagename) ORDER BY ReleaseMoment DESC LIMIT 1) FROM dwl_disco_apppackages ap WHERE (ap.appname=?)';
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
  var Cmd := MySQLCommand(State, SQL_Get_ProfileParameters);
  Cmd.Parameters.SetTextDataBinding(0, AppName);
  Cmd.Parameters.SetTextDataBinding(1, Profile);
  Cmd.Execute;

  var JSONParams := TJSONObject.Create;
  JSON_Data(State).AddPair('parameters', JSONParams);
  while Cmd.Reader.Read do
    JSONParams.AddPair(Cmd.Reader.GetString(0), Cmd.Reader.GetString(1));
  if FAdditionalParametersSQL<>'' then
  begin
    var Par_SQL  := FAdditionalParametersSQL;
    Par_SQL := ReplaceStr(Par_SQL, '$(appname)', AppName);
    Par_SQL := ReplaceStr(Par_SQL, '$(profile)', Profile);
    Cmd := MySQLCommand(State, Par_SQL);
    Cmd.Execute;
    while Cmd.Reader.Read do
      JSONParams.AddPair(Cmd.Reader.GetString(0), Cmd.Reader.GetString(1));
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

class function THandler_DisCo.Get_download_package(const State: PdwlHTTPHandlingState): boolean;
const
  SQl_GetRelease = 'SELECT data, mediatype, fileextension FROM dwl_disco_releases WHERE (packagename=?) AND (Kind=?) ORDER BY ReleaseMoment DESC LIMIT 1';
begin
  Result := true;
  var PackageName: string;
  if not TryGetRequestParamStr(State, 'packagename', PackageName) then
  begin
    State.StatusCode := HTTP_STATUS_BAD_REQUEST;
    Exit;
  end;
  var Kind: integer;
  if not TryGetRequestParamInt(State, 'kind', Kind) then
    Kind := discoreleasekindRelease;
  var Cmd := MySQLCommand(State, SQl_GetRelease);
  Cmd.Parameters.SetTextDataBinding(0, PackageName);
  Cmd.Parameters.SetIntegerDataBinding(1, Kind);
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
          State.SetContentType(Cmd.Reader.GetString(1));
          State.SetHeaderValue(HTTP_HEADER_CONTENT_DISPOSITION, 'filename='+PackageName+'.'+Cmd.Reader.GetString(2));
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

end.
