unit fPublish;

interface

uses
  Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, System.Classes, Vcl.ExtCtrls,
  Vcl.ComCtrls, DWL.IOUtils, Vcl.AppEvnts, DWL.HTTP.APIClient.UserPw,
  System.JSON, DWL.Logging, ToolsAPI, DWL.HTTP.APIClient, Winapi.Windows;

type
  TPublishForm = class(TForm)
    pnlFinish: TPanel;
    Panel6: TPanel;
    btnRelease: TButton;
    btnCancel: TButton;
    ApplicationEvents1: TApplicationEvents;
    lbMessages: TListBox;
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure btnReleaseClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure lbMessagesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  strict private
    FCancelled: boolean;
    FHasError: boolean;
    FInitStarted: boolean;
    FBusy: boolean;
    FPackageName: string;
    FTargetName: string;
    FVersionOfFile: TdwlFileVersionInfo;
    FSignTool: string;
    FSignCmd: string;
    FZipToDir: string;
    FProject: IOTAProject;
    FApiSession: TdwlAPISession;
    procedure Initialize;
    procedure GoAndPublish;
    procedure AddMessage(SeverityLevel: TdwlLogSeverityLevel; const ErrorStr: string);
    procedure PutErrorInMemo(SeverityLevel: TdwlLogSeverityLevel; const ErrorStr: string);
    procedure GetUserNamePassword(var Username, Password, Token: string; Action: TdwlAPIUserNamePasswordAuthorizerCallBackAction; JSONResponse: TJSONValue);
  public
    destructor Destroy; override;
  end;

implementation

uses
  DWL.ToolsAPI, Vcl.Graphics, System.SysUtils, System.Threading,
  DWL.HTTP.Consts, Winapi.WinInet, DWL.DisCo.Consts,
  System.StrUtils, sevenzip, DWL.OS, System.IOUtils, JclCompression,
  DisCoIde.General, DisCoIde.Consts, System.DateUtils;

{$R *.dfm}

{ TPublishForm }

procedure TPublishForm.AddMessage(SeverityLevel: TdwlLogSeverityLevel; const ErrorStr: string);
begin
  FHasError := FHasError or (SeverityLevel=lsError);
  TThread.Queue(nil,
    procedure
    begin
      PutErrorInMemo(SeverityLevel, ErrorStr);
    end);
end;

procedure TPublishForm.ApplicationEvents1Idle(Sender: TObject;
  var Done: Boolean);
begin
  if FCancelled and not FBusy then
    ModalResult := mrCancel;
  if FInitStarted then
    Exit;
  FInitStarted := true;
  Initialize;
end;

procedure TPublishForm.btnCancelClick(Sender: TObject);
begin
  FCancelled := true;
end;

procedure TPublishForm.btnReleaseClick(Sender: TObject);
begin
  FHasError := false;
  lbMessages.Clear;
  btnRelease.Enabled := false;
  btnRelease.Caption := 'Close';
  btnRelease.OnClick := nil;
  btnRelease.ModalResult := mrOk;
  GoAndPublish;
end;

destructor TPublishForm.Destroy;
begin
  FApiSession.Free;
  inherited Destroy;
end;

procedure TPublishForm.GetUserNamePassword(var Username, Password, Token: string; Action: TdwlAPIUserNamePasswordAuthorizerCallBackAction; JSONResponse: TJSONValue);
begin
  if Action=acapwGetUserNamePassword then
  begin
    UserName := TDisCoIde_General.FConfigParams.StrValue(paramUsername);
    Password := TDisCoIde_General.FConfigParams.StrValue(paramPassword);
  end;
end;

procedure TPublishForm.GoAndPublish;
begin
  TTask.Run(procedure
    begin
      FBusy := true;
      try
        try
          // SIGN THE EXECUTABLE
          AddMessage(lsNotice, 'Signing...');
          var Params := FSignCmd;
          Params := StringReplace(Params, '$(filename)', FTargetName, [rfReplaceAll]);
          var Res := TdwlOS.ExecuteFileAndWait(FSignTool, Params);
          if not Res.Success then
          begin
            AddMessage(lsError, 'Error in executing Codesign: '+Res.ErrorMsg);
            Exit;
          end;
          if FCancelled then
            Exit;
          // ZIP THE EXECUTABLE
          AddMessage(lsNotice, 'Zipping...');
          TDirectory.CreateDirectory(FZipToDir);
          var ZipTo := FZipToDir+'\'+FPackagename+'.7z';
          var CompressArchive :=  TJcl7ZCompressArchive.Create(ZipTo);
          try
            CompressArchive.AddFile(ExtractFilename(FTargetName), FTargetName);
            CompressArchive.Compress;
          finally
            CompressArchive.Free;
          end;
          if FCancelled then
            Exit;
          // SENDING RELEASE
          AddMessage(lsNotice, 'Sending release to server...');
          var Request := FApiSession.New_APIRequest('upload/package', HTTP_METHOD_POST);
          var Bytes := TFile.ReadAllBytes(ZipTo);
          Request.HTTPRequest.PostStream.Write(Bytes[0], Length(Bytes));
          Request.HTTPRequest.Header['packagename'] := FPackagename;
          Request.HTTPRequest.Header['version'] := FVersionOfFile.GetAsString(true);
          if FVersionOfFile.IsPreRelease then
            Request.HTTPRequest.Header['kind'] := discoreleasekindPreRelease.ToString
          else
            Request.HTTPRequest.Header['kind'] := discoreleasekindRelease.ToString;
          Request.HTTPRequest.Header['fileextension'] := '7z';
          var Response := Request.Execute;
          var IsOk: boolean;
          IsOk := Response.HTTPResponse.StatusCode=HTTP_STATUS_OK;
          if IsOk then
          begin
            var JSON := TJSONObject.ParseJSONValue(Response.HTTPResponse.AsString);
            try
              IsOk := JSON.GetValue<boolean>('success', false);
              if not IsOk then
                AddMessage(lsError, 'Upload failed: '+Response.HTTPResponse.AsString);
            finally
              JSON.Free;
            end;
          end
          else
            AddMessage(lsError, 'Upload failed');
          if IsOk then
          begin
            // write new versioninfo
            var ProjPar := TStringList.Create;
            try
              ProjPar.Values[paramPublish_Version] := FVersionOfFile.GetAsString(true);
              ProjPar.Values[paramPublish_Time] := DateToISO8601(Now, false);
              ProjPar.SaveToFile(ChangeFileExt(FProject.FileName, '.params'));
            finally
              ProjPar.Free;
            end;
            AddMessage(lsNotice, 'Release  '+FPackageName+' '+FVersionOfFile.GetAsString(true, true));
            AddMessage(lsWarning, 'PUBLISHED SUCCESSFULLY !');
            FCancelled := false;
          end;
        except
          on E: Exception do
            AddMessage(lsError, E.Message);
        end;
      finally
        FBusy := false;
        if FHasError or FCancelled then
        begin
          if FHasError then
            AddMessage(lsError, 'Publishing ended with Errors')
          else
            AddMessage(lsError, 'Publishing was cancelled');
          TDirectory.Delete(FZipToDir, true);
        end;
      end;
      TThread.Queue(nil,
        procedure
        begin
          btnCancel.Visible := false;
          btnRelease.Enabled := true;
        end);
    end);
end;

procedure TPublishForm.Initialize;
begin
  TTask.Run(procedure
    begin
      try
        if FCancelled then
          Exit;
        FBusy := true;
        try
          // FETCHING PROJECT
          FProject := TdwlToolsAPI.ActiveProject;
          if FProject=nil then
          begin
            AddMessage(lsError, 'No Active Project found');
            Exit;
          end;
          // CHECKING CONFIGURATION
          if FProject.CurrentConfiguration <> 'Release' then
          begin
            AddMessage(lsError, 'Current configuration is not "Release"');
          end;
          // FETCHING TARGET
          FTargetName := FProject.ProjectOptions.TargetName;
          if not FileExists(FTargetName) then
          begin
            AddMessage(lsError, 'Target does not exist: '+FTargetname);
            Exit;
          end
          else
          begin
            FVersionOfFile.SetFromFile(FTargetName);
            AddMessage(lsNotice, 'Target is: '+FTargetName);
            AddMessage(lsNotice, 'Version is: '+FVersionOfFile.GetAsString(true, true));
          end;
          FPackageName := TdwlFile.ExtractBareName(FTargetName);
          //CHECKING  VERSIONINFO
          var VersionOnServer: TdwlFileVersionInfo;
          FApiSession := TdwlAPISession.Create(TDisCoIde_General.FConfigParams.StrValue(paramDisco_Endpoint),
            New_UserPwAuthorizer(TDisCoIde_General.FConfigParams.StrValue(paramAuth_Endpoint), GetUsernamePassword));
          AddMessage(lsNotice, 'Fetching release info from '+FApiSession.ApiBaseUrl+'release');
          var Response := FApiSession.ExecuteApiRequest('release', HTTP_METHOD_GET, 'packagename='+FPackageName);
          if Response.StatusCode<>HTTP_STATUS_OK then
          begin
            if Response.StatusCode=HTTP_STATUS_NO_CONTENT then
              AddMessage(lsWarning, 'NO release on server, NEW PACKAGE WILL BE CREATED!')
            else
              AddMessage(lsError, 'Error fetching current releaseinfo from server');
            VersionOnServer.Clear;
          end
          else
          begin
            var JSON := TJSONValue.ParseJSONValue(Response.AsString);
            try
              var Params := JSON.GetValue<TJSONObject>('data');
              VersionOnServer.SetFromString(Params.GetValue<string>('version'));
              if Params.GetValue<integer>('kind')=discoreleasekindPreRelease then
                VersionOnServer.IsPreRelease := true;
            finally
              JSON.Free;
            end;
            AddMessage(lsNotice, 'Received version '+VersionOnServer.GetAsString(true, true));
          end;
          if FVersionOfFile<=VersionOnServer then
            AddMessage(lsError, 'Version ('+FVersionOfFile.GetAsString(true)+') same or older server ('+VersionOnServer.GetAsString(true)+')')
          else
          begin
            AddMessage(lsNotice, 'Version ('+FVersionOfFile.GetAsString(true)+') newer than server ('+VersionOnServer.GetAsString(true)+')');
            if not FVersionOfFile.IsPreRelease then
              AddMessage(lsWarning, 'Be Aware: this is a production release!');
          end;
          // CHECKING ZIP REQUIREMENTS
          if SevenzipLibraryName='' then
            SevenzipLibraryName := '7z.dll';
          var ZipDLLFn := TDisCoIde_General.ModuleDirectory+'\'+ExtractFileName(SevenzipLibraryName);
          if not FileExists(ZipDLLFn) then
            AddMessage(lsError, 'No DLL, put 32-bit 7z.ddl at "'+ZipDLLFn + '"')
          else
            SevenzipLibraryName := ZipDLLFn;
          // build the zipfilename
          FZipToDir := 'C:\data\delphi\releases\'+FPackageName+'\';
          if FVersionOfFile.IsPreRelease then
            FZipToDir := FZipToDir+'PreReleases\';
          FZipToDir := FZipToDir+FVersionOfFile.GetAsString(FVersionOfFile.IsPreRelease);
          if DirectoryExists(FZipToDir) then
            AddMessage(lsError, 'Folder already exists: '+FZipToDir);
          // CHECKING SIGN TOOL
          FSignTool := TDisCoIde_General.FConfigParams.StrValue(paramSigntool);
          if not FileExists(FSignTool) then
            AddMessage(lsError, 'Wrong signtool: "'+FSignTool);
          FSignCmd := TDisCoIde_General.FConfigParams.StrValue(paramSigncmd);
          if FSignCmd='' then
            AddMessage(lsError, 'Missing SignCmd');
          TThread.Queue(nil,
            procedure
            begin
              if not FHasError then
                btnRelease.Enabled := true;
            end);
        except
          on E: Exception do
            AddMessage(lsError, E.Message);
        end;
      finally
        FBusy := false;
        AddMessage(lsNotice, 'Initialization done...');
        if not (FHasError or FCancelled) then
          AddMessage(lsNotice, 'Please publish if you want...');
      end;
    end);
end;

procedure TPublishForm.lbMessagesDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  var LBox := TListBox(Control);
  LBox.Canvas.FillRect(Rect);
  case TdwlLogSeverityLevel(LBox.Items.Objects[Index]) of
  lsError:
    begin
      LBox.Canvas.Font.Color := clRed;
      LBox.Canvas.Font.Style := [fsBold];
    end;
  lsWarning:
    begin
      LBox.Canvas.Font.Color := clBlue;
      LBox.Canvas.Font.Style := [fsBold];
    end;
  else
    begin
      LBox.Canvas.Font.Color := clBlack;
      LBox.Canvas.Font.Style := [];
    end;
  end;
  LBox.Canvas.TextOut(Rect.Left + 2, Rect.Top, lBox.Items[Index]);
end;

procedure TPublishForm.PutErrorInMemo(SeverityLevel: TdwlLogSeverityLevel; const ErrorStr: string);
begin
  lbMessages.Items.AddObject(ErrorStr, pointer(SeverityLevel));
end;

end.
