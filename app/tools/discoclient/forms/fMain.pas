unit fMain;

interface

uses
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, System.Classes, Vcl.ExtCtrls,
  DWL.Params, DWL.HTTP.APIClient, DWL.HTTP.APIClient.UserPw, System.JSON,
  Vcl.AppEvnts, System.Generics.Collections, Winapi.Windows, Winapi.Messages,
  System.Actions, Vcl.ActnList, Vcl.Grids, Vcl.Dialogs;

type
  TRelease = record
    Id: integer;
    PackageName: string;
    Version: string;
    ReleaseMoment: TDateTime;
    Kind: byte;
    FileExtension: string;
  end;

  TReleases = class(TList<TRelease>)
  end;

  TMainForm = class(TForm)
    pnlMain: TPanel;
    pnlLeft: TPanel;
    pnlLeftTop: TPanel;
    lbPackages: TListBox;
    lblPackages: TLabel;
    pnlRight: TPanel;
    pnlRightTop: TPanel;
    lblPackageName: TLabel;
    pnlReleases: TPanel;
    Splitter1: TSplitter;
    ApplicationEvents: TApplicationEvents;
    Panel1: TPanel;
    pnlRelease: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    Button1: TButton;
    ActionList: TActionList;
    aiDownload: TAction;
    Button2: TButton;
    aiUpload: TAction;
    OpenDialog: TOpenDialog;
    lbReleases: TListBox;
    Panel3: TPanel;
    lblRelease: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblMoment: TLabel;
    Label5: TLabel;
    lblSoort: TLabel;
    lblExtension: TLabel;
    SaveDialog: TSaveDialog;
    procedure ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
    procedure lbReleasesClick(Sender: TObject);
    procedure lbPackagesClick(Sender: TObject);
    procedure aiDownloadExecute(Sender: TObject);
    procedure aiUploadExecute(Sender: TObject);
  strict private
    FHaveBeenIdle: boolean;
    FConfigParams: IdwlParams;
    FApiSession: TdwlAPISession;
    FPackages: TStringList;
    FCurrentReleases: TReleases;
    FCurrentRelease: TRelease;
    procedure ClearPackages;
    procedure GetUserNamePassword(var Username, Password, Token: string; Action: TdwlAPIUserNamePasswordAuthorizerCallBackAction; JSONResponse: TJSONValue);
    procedure LoadReleases;
    procedure UpdateShownPackage;
    procedure UpdateShownRelease;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.IOUtils, System.SysUtils, Winapi.WinInet,
  DWL.HTTP.Consts, System.DateUtils, System.Generics.Defaults, System.StrUtils,
  DWL.DisCo.Consts, DWL.IOUtils, DWL.Compression;

{$R *.dfm}

const
  ParamAuth_Endpoint='auth_endpoint';
  ParamDisco_Endpoint='disco_endpoint';
  paramUsername='username';
  paramPassword='password';

procedure TMainForm.AfterConstruction;
begin
  inherited AfterConstruction;
  FPackages := TStringlist.Create;
  FPackages.Sorted := true;
  FConfigParams := New_Params;
  FConfigParams.WriteJSON(TFile.ReadAllText(ChangeFileExt(ParamStr(0), '.config')));
  FApiSession := TdwlAPISession.Create(FConfigParams.StrValue(paramDisCo_Endpoint),
    New_UserPwAuthorizer(FConfigParams.StrValue(paramAuth_Endpoint), GetUsernamePassword));
end;

procedure TMainForm.aiDownloadExecute(Sender: TObject);
begin
  var Response := FApiSession.ExecuteApiRequest('download/package', HTTP_COMMAND_GET, 'id='+FCurrentRelease.ID.ToString, true, false);
  SaveDialog.FileName := FCurrentRelease.PackageName+' '+FCurrentRelease.Version+'.'+FCurrentRelease.FileExtension;
  if SaveDialog.Execute then
    TFile.WriteAllBytes(SaveDialog.FileName, Response.AsBytes);
end;

procedure TMainForm.aiUploadExecute(Sender: TObject);
begin
  if not OpenDialog.Execute then
    Exit;
  var NextVersion: TdwlFileVersionInfo;
  NextVersion.SetFromString(FCurrentReleases[0].Version);
  // increase version
  if NextVersion.Release=9 then
  begin
    NextVersion.Release := 0;
    if NextVersion.Minor=9 then
    begin
      NextVersion.Minor := 0;
      inc(NextVersion.Major);
    end
    else
      inc(NextVersion.Minor);
  end
  else
    inc(NextVersion.Release);
  var FileName := OpenDialog.FileName;
  if not SameText(TdwlFile.ExtractBareName(FileName), FCurrentRelease.PackageName) then
  begin
    ShowMessage('Expected filename '+FCurrentRelease.PackageName+'.exe or '+FCurrentRelease.PackageName+'.dll or '+FCurrentRelease.PackageName+'.7z');
    Exit;
  end;
  var FileExt := ExtractFileExt(FileName);
  var FileVersion: TdwlFileVersionInfo;
  if (FileExt='.exe') or (FileExt='.dll') then
    FileVersion.SetFromFile(FileName)
  else
  begin
    if FileExt<>'.7z' then
    begin
      ShowMessage('Unexpected extension '+FileExt);
      Exit;
    end;
    var TmpDir := ExtractFilePath(ParamStr(0))+'temp';
    if DirectoryExists(TmpDir) then
      TDirectory.Delete(TmpDir, true);
    ForceDirectories(TmpDir);
    if not TdwlCompression.ExtractArchive(FileName, TmpDir).Success then
    begin
      ShowMessage('Error decompressing '+FileName);
      Exit;
    end;
    var VersionFile := TmpDir+'\version.info';
    if not FileExists(VersionFile) then
    begin
      ShowMessage(VersionFile+'not found.');
      Exit;
    end;
    FileVersion.SetFromString(TFile.ReadAllText(VersionFile));
  end;
  if FileVersion<NextVersion then
  begin
    ShowMessage('This file has version '+FileVersion.GetAsString+'.'#13#10'At lease version '+NextVersion.GetAsString+' is needed for release.');
    Exit;
  end;
  if FileExt<>'.7z' then
  begin
    var Fn7z := ChangeFileExt(FileName, '.7z');
    var Res := TdwlCompression.ZipFile(Fn7z, FileName);
    if not Res.Success then
    begin
      ShowMessage('Error zipping '+Res.ErrorMsg);
      Exit;
    end;
    FileName := Fn7z;
  end;
  // SENDING RELEASE
  var Request := FApiSession.PrepareAPIRequest('upload/package', HTTP_COMMAND_POST);
  var Bytes := TFile.ReadAllBytes(FileName);
  Request.PostStream.Write(Bytes[0], Length(Bytes));
  Request.Header['packagename'] := FCurrentRelease.PackageName;
  Request.Header['version'] := FileVersion.GetAsString(true);
  if FileVersion.IsPreRelease then
    Request.Header['kind'] := discoreleasekindPreRelease.ToString
  else
    Request.Header['kind'] := discoreleasekindRelease.ToString;
  Request.Header['fileextension'] := '7z';
  var Response := Request.Execute;
  var IsOk: boolean;
  IsOk := Response.StatusCode=HTTP_STATUS_OK;
  if IsOk then
  begin
    var JSON := TJSONObject.ParseJSONValue(Response.AsString);
    try
      IsOk := JSON.GetValue<boolean>('success', false);
      if not IsOk then
        ShowMessage('Upload failed: '+Response.AsString)
      else
      begin
        ShowMessage('Success! Uploaded file version '+FileVersion.GetAsString(true));
        LoadReleases;
      end;
    finally
      JSON.Free;
    end;
  end
  else
    ShowMessage('Upload failed');
end;

procedure TMainForm.ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
begin
  if not FHaveBeenIdle then
  begin
    FHaveBeenIdle := true;
    LoadReleases;
  end;
end;

procedure TMainForm.BeforeDestruction;
begin
  ClearPackages;
  FApiSession.Free;
  FPackages.Free;
  inherited BeforeDestruction;
end;

procedure TMainForm.ClearPackages;
begin
  for var i := 0 to FPackages.Count-1 do
    FPackages.Objects[i].Free;
  FPackages.Clear;
end;

procedure TMainForm.GetUserNamePassword(var Username, Password, Token: string; Action: TdwlAPIUserNamePasswordAuthorizerCallBackAction; JSONResponse: TJSONValue);
begin
  if Action=acapwGetUserNamePassword then
  begin
    UserName := FConfigParams.StrValue(paramUsername);
    Password := FConfigParams.StrValue(paramPassword);
  end;
end;

procedure TMainForm.lbPackagesClick(Sender: TObject);
begin
  UpdateShownPackage;
end;

procedure TMainForm.lbReleasesClick(Sender: TObject);
begin
  UpdateShownRelease;
end;

procedure TMainForm.LoadReleases;
begin
  var CurrentPackagename := '';
  if FCurrentReleases<>nil then
    CurrentPackagename := FCurrentReleases[0].PackageName;
  ClearPackages;
  var Response := FApiSession.ExecuteJSONRequest('releases');
  if not Response.Success then
    Exit;
  var JSONReleases := Response.Data.GetValue<TJSONArray>('releases');
  for var JSONRelease in JSONReleases do
  begin
    var Release: TRelease;
    Release.Id := JSONRelease.GetValue<integer>('id');
    Release.PackageName := JSONRelease.GetValue<string>('packagename').ToLower;
    Release.Version := JSONRelease.GetValue<string>('version');
    Release.ReleaseMoment := ISo8601ToDate(JSONRelease.GetValue<string>('releasemoment'), false);
    Release.Kind := JSONRelease.GetValue<byte>('kind');
    Release.FileExtension := JSONRelease.GetValue<string>('fileextension');
    var Idx := FPackages.IndexOf(Release.PackageName);
    var Releases: TReleases;
    if Idx<0 then
    begin
      Releases := TReleases.Create;
      FPackages.AddObject(Release.PackageName, Releases);
    end
    else
      Releases := TReleases(FPackages.Objects[Idx]);
    Releases.Add(Release);
  end;
  lbPackages.Items.Assign(FPackages);
  for var i := 0 to FPackages.Count-1 do
  begin
    TReleases(FPackages.Objects[i]).Sort(TComparer<TRelease>.Construct(
      function(const Left, Right: TRelease): Integer
      begin
        if Left.ReleaseMoment<Right.ReleaseMoment then
          Result := 1
        else
        begin
          if Left.ReleaseMoment>Right.ReleaseMoment then
            Result := -1
          else
            Result := 0;
        end;
      end)
    );
  end;
  if CurrentPackagename<>'' then
  begin
    for var i := 0 to FPackages.Count-1 do
    begin
      if Sametext(CurrentPackagename, FPackages[i]) then
      begin
        lbPackages.ItemIndex := i;
        Break;
      end;
    end;
  end;
  UpdateShownPackage;
end;

procedure TMainForm.UpdateShownPackage;
begin
  if lbPackages.ItemIndex<0 then
  begin
    FCurrentReleases := nil;
    lblPackageName.Caption := '<no package>';
    pnlReleases.Visible := false;
  end
  else
  begin
    lbReleases.Clear;
    FCurrentReleases := TReleases(FPackages.Objects[lbPackages.ItemIndex]);
    lblPackageName.Caption := FCurrentReleases[0].PackageName;
    for var i := 0 to FCurrentReleases.Count-1 do
    begin
      var Release := FCurrentReleases[i];
      lbReleases.Items.Add(Release.Version+IfThen(Release.Kind=discoreleasekindPreRelease, ' (PreRelease)'));
    end;
    lbReleases.ItemIndex := 0;
    pnlReleases.Visible := true;
    UpdateShownRelease;
  end;
end;

procedure TMainForm.UpdateShownRelease;
begin
  FCurrentRelease := FCurrentReleases[lbReleases.ItemIndex];
  lblRelease.Caption := FCurrentRelease.Version;
  lblMoment.Caption := FormatDateTime('yyyy-mm-dd hh:nn', FCurrentRelease.ReleaseMoment);
  lblSoort.Caption := IfThen(FCurrentRelease.Kind=discoreleasekindRelease, 'Release', 'PreRelease');;
  lblExtension.Caption := FCurrentRelease.FileExtension;
end;

end.
//    2022-12-15 12:24
