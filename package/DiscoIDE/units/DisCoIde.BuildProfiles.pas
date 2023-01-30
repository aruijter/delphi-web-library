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
    class procedure OutputVersionInfo(Project: IOTAProject; IsADebug, IsAPreRelease, IsARelease: boolean);
    class procedure SetDefines(Project: IOTAProject; IsRelease: boolean);
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
    SetDefines(Project, false);
    OutputVersionInfo(Project, true, false, false);
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
    SetDefines(Project, true);
    OutputVersionInfo(Project, false, true, false);
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
    SetDefines(Project, true);
    OutputVersionInfo(Project, false, false, true);
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

class procedure TDisCoIde_BuildProfiles.OutputVersionInfo(Project: IOTAProject; IsADebug, IsAPreRelease, IsARelease: boolean);
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
  // Increase version number
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
  inc(VersionInfo.Build); // always increase build number
  // set versioninfo in project
  var ProjectOptions := Project.ProjectOptions as IOTAProjectOptionsConfigurations;
  if Assigned(ProjectOptions) then
  begin
    var cfg := ProjectOptions.ActiveConfiguration;
    if Assigned(cfg) then
    begin
      cfg.SetBoolean('VerInfo_IncludeVerInfo', true);
      cfg.SetInteger('VerInfo_MajorVer', VersionInfo.Major);
      cfg.SetInteger('VerInfo_MinorVer', VersionInfo.Minor);
      cfg.SetInteger('VerInfo_Release', VersionInfo.Release);
      cfg.SetInteger('VerInfo_Build', VersionInfo.Build);
      cfg.SetValue('VerInfo_Keys', 'CompanyName=;FileDescription=$(MSBuildProjectName);FileVersion='+
        VersionInfo.GetAsString(true)+';InternalName=;LegalCopyright=;LegalTrademarks=;OriginalFilename=;ProgramID=com.embarcadero.$(MSBuildProjectName);ProductName=$(MSBuildProjectName);ProductVersion='+
        VersionInfo.GetAsString(true)+';Comments=');
      cfg.SetBoolean('VerInfo_AutoGenVersion', false);
      cfg.SetBOolean('VerInfo_AutoIncVersion', false);
      cfg.SetBoolean('VerInfo_Debug', IsADebug);
      cfg.SetBoolean('VerInfo_PreRelease', IsAPreRelease);
    end;
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

class procedure TDisCoIde_BuildProfiles.SetDefines(Project: IOTAProject; IsRelease: boolean);
begin
  var Defines := TStringList.Create;
  try
    Defines.Delimiter := ';';
    Defines.DelimitedText := Project.ProjectOptions.Values['Defines'];
    var i := Defines.IndexOf('DEBUG');
    if (i<0) and (not IsRelease) then
      Defines.Add('DEBUG');
    if (i>=0) and IsRelease then
      Defines.Delete(i);
    i := Defines.IndexOf('RELEASE');
    if (i<0) and IsRelease then
      Defines.Add('RELEASE' );
    if (i>=0) and not IsRelease then
      Defines.Delete(i);
    Project.ProjectOptions.Values['Defines'] := Defines.DelimitedText;
  finally
    Defines.Free;
  end;
end;

end.
