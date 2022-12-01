unit DisCoIde.BuildProfiles;

interface

implementation

uses
  ToolsAPI, Vcl.Menus, System.Generics.Collections, DWL.ToolsAPI,
  DWL.IOUtils, System.SysUtils, System.Classes, DisCoIde.Consts, System.IOUtils,
  DWL.ConvUtils, System.DateUtils, DWL.Types, System.StrUtils, Vcl.Dialogs;

type
  TDisCoIde_BuildProfiles = class
  strict private
    class var FMenuItems: TObjectList<TMenuItem>;
    class procedure DoBuildDebug(Sender: TObject);
    class procedure DoBuildPreRelease(Sender: TObject);
    class procedure DoBuildRelease(Sender: TObject);
    class function GetSuitableproject: IOTAProject;
    class procedure OutputVersionInfo(Project: IOTAProject; IsARelease, IsAPreRelease: boolean);
  public
    class constructor Create;
    class destructor Destroy;
  end;

  { TDisCoIde_BuildProfiles }

class constructor TDisCoIde_BuildProfiles.Create;
begin
  inherited;
  FMenuItems := TObjectList<TMenuItem>.Create(true);
  var MainMenu := (BorlandIDEServices as INTAServices).MainMenu;
  var ProjectMenu := MainMenu.Items.Find('Project');
  if ProjectMenu <> nil then
  begin
    var
    MI := TMenuItem.Create(ProjectMenu);
    FMenuItems.Add(MI); // this will destroy the item when finalizing
    MI.Caption := 'Build - &Release';
    MI.Name := 'DisCoIde_MenuItem_Build_Release';
    MI.OnClick := DoBuildRelease;
    ProjectMenu.Insert(0, MI);

    MI := TMenuItem.Create(ProjectMenu);
    FMenuItems.Add(MI); // this will destroy the item when finalizing
    MI.Caption := 'Build - &PreRelease';
    MI.Name := 'DisCoIde_MenuItem_Build_PreRelease';
    MI.OnClick := DoBuildPreRelease;
    ProjectMenu.Insert(0, MI);

    MI := TMenuItem.Create(ProjectMenu);
    FMenuItems.Add(MI); // this will destroy the item when finalizing
    MI.Caption := 'Build - &Debug';
    MI.Name := 'DisCoIde_MenuItem_Build_Debug';
    MI.OnClick := DoBuildDebug;
    MI.ShortCut := TextToShortCut('Ctrl+D');
    ProjectMenu.Insert(0, MI);
    (BorlandIDEServices as IOTAKeyBoardServices)
      .AddKeyboardBinding(TdwlToolsAPI_KeyBinding_MenuItem.Create(MI));
  end;
end;

class destructor TDisCoIde_BuildProfiles.Destroy;
begin
  FMenuItems.Free;
  inherited;
end;

class procedure TDisCoIde_BuildProfiles.DoBuildDebug(Sender: TObject);
begin
  try
    var
    Project := GetSuitableproject;
    if Project = nil then
      Exit;
    Project.CurrentConfiguration := 'Debug';
    TdwlToolsAPI.SetProjectOption(Project, 'Optimization', 0);
    TdwlToolsAPI.SetProjectOption(Project, 'RangeChecks', 1);
    TdwlToolsAPI.SetProjectOption(Project, 'IOChecks', 1);
    TdwlToolsAPI.SetProjectOption(Project, 'OverflowChecks', 1);
    TdwlToolsAPI.SetProjectOption(Project, 'UnitDebugInfo', 2);
    TdwlToolsAPI.SetProjectOption(Project, 'LocalSymbols', 1);
    TdwlToolsAPI.SetProjectOption(Project, 'ReferenceInfo', 2);
    TdwlToolsAPI.SetProjectOption(Project, 'Assertions', 1);
    TdwlToolsAPI.SetProjectOption(Project, 'StackFrames', 0);
    TdwlToolsAPI.SetProjectOption(Project, 'DebugInfo', true);
    TdwlToolsAPI.SetProjectOption(Project, 'MapFile', 0);
    OutputVersionInfo(Project, false, false);
    Project.ProjectBuilder.BuildProject(cmOTABuild, true);
  except
    on E: Exception do
      MessageDlg(E.Message, TMsgDlgType.mtError, [mbOk], 0);
  end;
end;

class procedure TDisCoIde_BuildProfiles.DoBuildPreRelease(Sender: TObject);
begin
  try
    var
    Project := GetSuitableproject;
    if Project = nil then
      Exit;
    Project.CurrentConfiguration := 'Release';
    TdwlToolsAPI.SetProjectOption(Project, 'Optimization', 1);
    TdwlToolsAPI.SetProjectOption(Project, 'RangeChecks', 0);
    TdwlToolsAPI.SetProjectOption(Project, 'IOChecks', 0);
    TdwlToolsAPI.SetProjectOption(Project, 'OverflowChecks', 0);
    TdwlToolsAPI.SetProjectOption(Project, 'UnitDebugInfo', 0);
    TdwlToolsAPI.SetProjectOption(Project, 'LocalSymbols', 0);
    TdwlToolsAPI.SetProjectOption(Project, 'ReferenceInfo', 0);
    TdwlToolsAPI.SetProjectOption(Project, 'Assertions', 0);
    TdwlToolsAPI.SetProjectOption(Project, 'StackFrames', 0);
    TdwlToolsAPI.SetProjectOption(Project, 'DebugInfo', false);
    TdwlToolsAPI.SetProjectOption(Project, 'MapFile', 3);
    OutputVersionInfo(Project, false, true);
    Project.ProjectBuilder.BuildProject(cmOTABuild, true);
  except
    on E: Exception do
      MessageDlg(E.Message, TMsgDlgType.mtError, [mbOk], 0);
  end;
end;

class procedure TDisCoIde_BuildProfiles.DoBuildRelease(Sender: TObject);
begin
  try
    var
    Project := GetSuitableproject;
    if Project = nil then
      Exit;
    Project.CurrentConfiguration := 'Release';
    TdwlToolsAPI.SetProjectOption(Project, 'Optimization', 1);
    TdwlToolsAPI.SetProjectOption(Project, 'RangeChecks', 0);
    TdwlToolsAPI.SetProjectOption(Project, 'IOChecks', 0);
    TdwlToolsAPI.SetProjectOption(Project, 'OverflowChecks', 0);
    TdwlToolsAPI.SetProjectOption(Project, 'UnitDebugInfo', 0);
    TdwlToolsAPI.SetProjectOption(Project, 'LocalSymbols', 0);
    TdwlToolsAPI.SetProjectOption(Project, 'ReferenceInfo', 0);
    TdwlToolsAPI.SetProjectOption(Project, 'Assertions', 0);
    TdwlToolsAPI.SetProjectOption(Project, 'StackFrames', 0);
    TdwlToolsAPI.SetProjectOption(Project, 'DebugInfo', false);
    TdwlToolsAPI.SetProjectOption(Project, 'MapFile', 3);
    OutputVersionInfo(Project, true, false);
    Project.ProjectBuilder.BuildProject(cmOTABuild, true);
  except
    on E: Exception do
      MessageDlg(E.Message, TMsgDlgType.mtError, [mbOk], 0);
  end;
end;

class function TDisCoIde_BuildProfiles.GetSuitableproject: IOTAProject;
begin
  Result := TdwlToolsAPI.ActiveProject;
  if Result = nil then
    Exit;
  if SameText(TdwlFile.ExtractBareName(Result.FileName), 'DisCoIde') then
  // To prevent building myself, this will crash the IDE
    Result := nil;
end;

class procedure TDisCoIde_BuildProfiles.OutputVersionInfo(Project: IOTAProject; IsARelease, IsAPreRelease: boolean);
begin
  var VersionInfo: TdwlFileVersionInfo;
  var Fn_ProjPar := ChangeFileExt(Project.FileName, '.params');
  var Path_Proj := ExtractFilePath(Fn_ProjPar);
  var ProjPar := TStringList.Create;
  try
    if FileExists(Fn_ProjPar) then
      ProjPar.LoadFromFile(Fn_ProjPar);
    var PublishVersion := ProjPar.Values[paramPublish_Version];
    if PublishVersion='' then // backward compatibility
    begin
      VersionInfo.Major := StrToIntDef(ProjPar.Values[paramVersion_MajorVersion], 1);
      VersionInfo.Minor := StrToIntDef(ProjPar.Values[paramVersion_MinorVersion], 0);
      VersionInfo.Release := StrToIntDef(ProjPar.Values[paramVersion_Release], 0);
      VersionInfo.Build := StrToIntDef(ProjPar.Values[paramVersion_Build], 0);
    end
    else
      VersionInfo.SetFromString(PublishVersion);
  finally
    ProjPar.Free;
  end;
  if IsARelease then
  begin
    if VersionInfo.Release=9 then
    begin
      VersionInfo.Release := 0;
      if VersionInfo.Minor=9 then
      begin
        VersionInfo.Minor := 0;
        inc(VersionInfo.Major);
      end
      else
        inc(VersionInfo.Minor);
    end
    else
      inc(VersionInfo.Release);
  end;
  // always increase build number
  inc(VersionInfo.Build);
  // write versioninfo.rc for inclusion as a resource in project
  var Resource := TStringList.Create;
  try
    Resource.Add('1 VERSIONINFO');
    Resource.Add('FILEVERSION ' + VersionInfo.GetAsString(true, false, ',' ));
    Resource.Add('PRODUCTVERSION ' + VersionInfo.GetAsString(true, false, ',' ));
    if IsAPreRelease then
    begin
      Resource.Add('FILEFLAGSMASK VS_FFI_FILEFLAGSMASK');
      Resource.Add('FILEFLAGS VS_FF_PRERELEASE');
    end;
    if Project.CurrentPlatform <> 'Win64' then
      Resource.Add('FILEOS VOS__WINDOWS32');
    Resource.Add('FILETYPE VFT_APP');
    Resource.Add('BEGIN');
    Resource.Add('BLOCK "StringFileInfo"');
    Resource.Add('BEGIN');
    Resource.Add('BLOCK "040904E4"');
    Resource.Add('BEGIN');
    Resource.Add('VALUE "FileVersion", "' + VersionInfo.GetAsString(true) + '\000"');
    Resource.Add('VALUE "ProductVersion", "' + VersionInfo.GetAsString(true) + '\000"');
    Resource.Add('END');
    Resource.Add('END');
    Resource.Add('BLOCK "VarFileInfo"');
    Resource.Add('BEGIN');
    Resource.Add('VALUE "Translation", 0x0409 0x04E4');
    Resource.Add('END');
    Resource.Add('END');
    Resource.SaveToFile(Path_Proj + 'versioninfo.rc');
  finally
    Resource.Free;
  end;
  // write versioninfo.inc for usage where needed in pascal source
  System.IOUtils.TFile.WriteAllText(Path_Proj + 'versioninfo.inc',
    'const'#13#10 + '  ' + paramVersion_MajorVersion + ' = ' + VersionInfo.Major.ToString
    + ';'#13#10 + '  ' + paramVersion_MinorVersion + ' = ' + VersionInfo.Minor.ToString +
    ';'#13#10 + '  ' + paramVersion_Release + ' = ' + VersionInfo.Release.ToString +
    ';'#13#10 + '  ' + paramVersion_Build + ' = ' + VersionInfo.Build.ToString +
    ';'#13#10 + '  ' + paramVersion_Time + ' = ' + TdwlConvUtils.FloatToDotStr
    (TTimeZone.Local.ToUniversalTime(Now)) + ';'#13#10 + '  ' +
    paramVersion_Is_Prerelease + ' = ' + IfThen(IsAPreRelease, 'true',
    'false') + ';');
end;

end.
