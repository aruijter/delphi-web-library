unit fPublish;

interface

uses
  Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, System.Classes, Vcl.ExtCtrls,
  Vcl.ComCtrls, DWL.IOUtils, Vcl.AppEvnts, DWL.HTTP.APIClient.UserPw,
  System.JSON;

type
  TPublishForm = class(TForm)
    pnlFinish: TPanel;
    Panel6: TPanel;
    btnRelease: TButton;
    btnCancel: TButton;
    reMsg: TRichEdit;
    ApplicationEvents1: TApplicationEvents;
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure btnReleaseClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
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
    procedure Initialize;
    procedure GoAndPublish;
    procedure AddMessage(IsError: boolean; const ErrorStr: string);
    procedure PutErrorInMemo(IsError: boolean; const ErrorStr: string);
    procedure GetUserNamePassword(var Username, Password, Token: string; Action: TdwlAPIUserNamePasswordAuthorizerCallBackAction; JSONResponse: TJSONValue);
  public

  end;

implementation

uses
  DWL.ToolsAPI, Vcl.Graphics, System.SysUtils, DWL.DisCo, System.Threading,
  DWL.HTTP.Consts, Winapi.WinInet, DWL.DisCo.Consts,
  System.StrUtils, sevenzip, DWL.OS, System.IOUtils, JclCompression,
  DisCoIde.General;

{$R *.dfm}

{ TPublishForm }

procedure TPublishForm.AddMessage(IsError: boolean; const ErrorStr: string);
begin
  FHasError := FHasError or IsError;
  TThread.Queue(nil,
    procedure
    begin
      PutErrorInMemo(IsError, ErrorStr);
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
  reMsg.Clear;
  btnRelease.Enabled := false;
  btnRelease.Caption := 'Close';
  btnRelease.OnClick := nil;
  btnRelease.ModalResult := mrOk;
  GoAndPublish;
end;

procedure TPublishForm.GetUserNamePassword(var Username, Password, Token: string; Action: TdwlAPIUserNamePasswordAuthorizerCallBackAction; JSONResponse: TJSONValue);
begin
  if Action=acapwGetUserNamePassword then
  begin
    UserName := TDisCoIde_General.FConfigParams.StrValue('username');
    Password := TDisCoIde_General.FConfigParams.StrValue('password');
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
          AddMessage(false, 'Signing...');
          var Params := FSignCmd;
          Params := StringReplace(Params, '$(filename)', FTargetName, [rfReplaceAll]);
          var Res := TdwlOS.ExecuteFileAndWait(FSignTool, Params);
          if not Res.Success then
          begin
            AddMessage(true, 'Error in executing Codesign: '+Res.ErrorMsg);
            Exit;
          end;
          if FCancelled then
            Exit;
          // ZIP THE EXECUTABLE
          AddMessage(false, 'Zipping...');
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
          AddMessage(false, 'Sending release to server...');
          var Request := TdwlDisCo.FApiSession.PrepareAPIRequest('upload/package', HTTP_COMMAND_POST);
          var Bytes := TFile.ReadAllBytes(ZipTo);
          Request.PostStream.Write(Bytes[0], Length(Bytes));
          Request.Header['packagename'] := FPackagename;
          Request.Header['version'] := FVersionOfFile.GetAsString;
          Request.Header['build'] := FVersionOfFile.Build.ToString;
          if FVersionOfFile.IsPreRelease then
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
                AddMessage(true, 'Upload failed: '+Response.AsString);
            finally
              JSON.Free;
            end;
          end
          else
            AddMessage(true, 'Upload failed');
          if IsOk then
          begin
            AddMessage(false, 'Release  '+FPackageName+' '+FVersionOfFile.GetAsString(true, true));
            AddMessage(false, 'PUBLISHED SUCCESSFULLY !');
            FCancelled := false;
          end;
        except
          on E: Exception do
            AddMessage(true, E.Message);
        end;
      finally
        FBusy := false;
        if FHasError or FCancelled then
        begin
          if FHasError then
            AddMessage(true, 'Publishing ended with Errors')
          else
            AddMessage(true, 'Publishing was cancelled');
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
  var Authenticator := New_UserPwAuthorizer(TDisCoIde_General.FConfigParams.StrValue('auth_endpoint'), GetUsernamePassword);
  TdwlDisco.Initialize(TDisCoIde_General.FConfigParams.StrValue('disco_endpoint'), Authenticator);
  TTask.Run(procedure
    begin
      try
        if FCancelled then
          Exit;
        FBusy := true;
        try
          // FETCHING PROJECT
          var Project := TdwlToolsAPI.ActiveProject;
          if Project=nil then
          begin
            AddMessage(true, 'No Active Project found');
            Exit;
          end;
          // CHECKING CONFIGURATION
          if Project.CurrentConfiguration <> 'Release' then
          begin
            AddMessage(true, 'Current configuration is not "Release"');
          end;
          // FETCHING TARGET
          FTargetName := Project.ProjectOptions.TargetName;
          if not FileExists(FTargetName) then
          begin
            AddMessage(true, 'Target does not exist: '+FTargetname);
            Exit;
          end
          else
          begin
            FVersionOfFile.SetFromFile(FTargetName);
            AddMessage(false, 'Target is: '+FTargetName + ' ('+FVersionOfFile.GetAsString(true, true)+')');
          end;
          FPackageName := TdwlFile.ExtractBareName(FTargetName);
          //CHECKING  VERSIONINFO
          var VersionOnServer: TdwlFileVersionInfo;
          AddMessage(false, 'Fetching release info from server...');
          var Response := TdwlDisco.FApiSession.DoApiRequest('release', HTTP_COMMAND_GET, 'packagename='+FPackageName);
          if (Response=nil) or (Response.StatusCode<>HTTP_STATUS_OK) then
          begin
            if (Response<>nil) and (Response.StatusCode=HTTP_STATUS_NO_CONTENT) then
              AddMessage(false, 'NO release on server, NEW PACKAGE WILL BE CREATED!')
            else
              AddMessage(true, 'Error fetching current releaseinfo from server');
            VersionOnServer.Clear;
          end
          else
          begin
            var JSON := TJSONValue.ParseJSONValue(Response.AsString);
            try
              var Params := JSON.GetValue<TJSONObject>('data');
              VersionOnServer.SetFromString(Params.GetValue<string>('version'));
              VersionOnServer.Build := Params.GetValue<integer>('build');
              if Params.GetValue<integer>('kind')=discoreleasekindPreRelease then
                VersionOnServer.IsPreRelease := true;
            finally
              JSON.Free;
            end;
            AddMessage(false, 'Received version '+VersionOnServer.GetAsString(true, true));
          end;
          if FVersionOfFile<=VersionOnServer then
            AddMessage(true, 'Version ('+FVersionOfFile.GetAsString(true)+') same or older server ('+VersionOnServer.GetAsString(true)+')')
          else
            AddMessage(false, 'Version ('+FVersionOfFile.GetAsString(true)+') newer than server ('+VersionOnServer.GetAsString(true)+')');
          // CHECKING ZIP REQUIREMENTS
          var ZipDLLFn := TDisCoIde_General.ModuleDirectory+'\'+ExtractFileName(SevenzipLibraryName);
          if not FileExists(ZipDLLFn) then
            AddMessage(true, 'Missing DLL, please place the 32-bit 7z DLL at "'+ZipDLLFn + '"')
          else
            SevenzipLibraryName := ZipDLLFn;
          // build the zipfilename
          FZipToDir := 'C:\data\delphi\releases\'+FPackageName+'\';
          if FVersionOfFile.IsPreRelease then
            FZipToDir := FZipToDir+'PreReleases\';
          FZipToDir := FZipToDir+FVersionOfFile.GetAsString(FVersionOfFile.IsPreRelease);
          if DirectoryExists(FZipToDir) then
            AddMessage(true, 'Folder already exists: '+FZipToDir);
          // CHECKING SIGN TOOL
          FSignTool := TDisCoIde_General.FConfigParams.StrValue('signtool');
          if not FileExists(FSignTool) then
            AddMessage(true, 'Wrong signtool: "'+FSignTool);
          FSignCmd := TDisCoIde_General.FConfigParams.StrValue('signcmd');
          if FSignCmd='' then
            AddMessage(true, 'Missing SignCmd');
          TThread.Queue(nil,
            procedure
            begin
              if not FHasError then
                btnRelease.Enabled := true;
            end);
        except
          on E: Exception do
            AddMessage(true, E.Message);
        end;
      finally
        FBusy := false;
        AddMessage(false, 'Initialization done...');
        if not (FHasError or FCancelled) then
          AddMessage(false, 'Please publish if you want...');
      end;
    end);
end;

procedure TPublishForm.PutErrorInMemo(IsError: boolean; const ErrorStr: string);
begin
  if IsError then
  begin
    reMsg.SelAttributes.Color := clRed;
    reMsg.SelAttributes.Style := [fsBold];
  end
  else
  begin
    reMsg.SelAttributes.Color := clBlack;
    reMsg.SelAttributes.Style := [];
  end;
  reMsg.Lines.Add(ErrorStr);
end;

end.
