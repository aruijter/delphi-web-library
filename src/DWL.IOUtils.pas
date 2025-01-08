unit DWL.IOUtils;

interface

uses
  System.IOUtils, System.Classes,Winapi.Windows;

type
  TdwlFile = record
    class function ExtractBareName(const Path: string=''): string; static;
    class procedure SetReadOnlyFlag(const FileName: string; ReadOnly: boolean); static;
  end;

  TdwlIOEnumOption = (ioRecurse, ioIncludeFiles, ioIncludeDirectories, ioIncludeHidden, ioRemoveReadOnly, ioOverwrite);
  TdwlIOEnumOptions = set of TdwlIOEnumOption;

  PDirEnum = ^TDirEnum;
  TDirEnum = record
  private
    hFind: THandle;
    FindData: TWin32FindData;
    Parent: PDirEnum;
    BaseDirectory: string;
    RelativeDirectory: string;
    class procedure Close(var DirEnum: PDirEnum); static;
    class function Open(const BaseDirectory, RelativeDirectory: string): PDirENum; static;
  public
    function Directory: string;
    function FullPathName: string;
    function IsDirectory: boolean;
    function IsReadOny: boolean;
    function Name: string;
    function RelativePathName: string;
  end;

  IdwlDirectoryEnumerator = interface
    function Current: PDirEnum;
    function MoveNext: boolean;
  end;

  TdwlDirectory = record
  strict private
    class var FApplication_TempDir: string;
  public
    class destructor Destroy;
    class function Application_TempDir: string; static;
    class procedure Copy(const SourceDirectory, DestinationDirectory: string; Options: TdwlIOEnumOptions=[ioIncludeFiles, ioIncludeDirectories, ioRecurse]); static;
    class procedure Delete(const Directory: string; Options: TdwlIOEnumOptions=[ioIncludeFiles, ioIncludeDirectories, ioRecurse]); static;
    class procedure Move(const FromDirectory, ToDirectory: string; Options: TdwlIOEnumOptions=[ioIncludeFiles, ioIncludeDirectories, ioRecurse]); static;
    class function Enumerator(const Directory: string; Options: TdwlIOEnumOptions=[ioIncludeFiles]; const FileMask: string='*.*'): IdwlDirectoryEnumerator; static;
  end;

  TdwlFileVersionInfo = record
  public
    Major: word;
    Minor: word;
    Release: word;
    Build: word;
    IsPreRelease: boolean;
    class function CreateFromFile(const FileName: string=''): TdwlFileVersionInfo; static;
    class function CreateFromString(const VersionString: string; Separator: char='.'): TdwlFileVersionInfo; static;

    class operator Equal(VersionInfoA, VersionInfoB: TdwlFileVersionInfo): boolean;
    class operator GreaterThan(VersionInfoA, VersionInfoB: TdwlFileVersionInfo): boolean;
    class operator GreaterThanOrEqual(VersionInfoA, VersionInfoB: TdwlFileVersionInfo): boolean;
    class operator LessThan(VersionInfoA, VersionInfoB: TdwlFileVersionInfo): boolean;
    class operator LessThanOrEqual(VersionInfoA, VersionInfoB: TdwlFileVersionInfo): boolean;
    class operator NotEqual(VersionInfoA, VersionInfoB: TdwlFileVersionInfo): boolean;

    procedure SetFromFile(const FileName: string);
    procedure SetFromString(const VersionString: string; Separator: char='.');
    procedure Clear;

    function IsEmpty: boolean;
    function GetAsString(IncludeBuild: boolean=false; IncludePrerelease: boolean=false; Separator: char='.'): string;
  end;

implementation

uses
  System.SysUtils, DWL.Resolver, System.Masks, System.StrUtils;

type
  TdwlDirectoryEnumerator = class(TInterfacedObject, IdwlDirectoryEnumerator)
  strict private
    FDirEnum: PDirEnum;
    FFileMask: string;
    FOptions: TdwlIOEnumOptions;
    function UnfilteredMoveNext: boolean;
  private
    function Current: PDirEnum;
    function MoveNext: boolean;
  public
    constructor Create(const Directory: string; Options: TdwlIOEnumOptions; const FileMask: string);
    destructor Destroy; override;
  end;

{ TdwlFile }

class function TdwlFile.ExtractBareName(const Path: string=''): string;
begin
  if Path='' then
    Result := ChangeFileExt(ExtractFileName(GetModuleName(HInstance)),'')
  else
    Result := ChangeFileExt(ExtractFileName(Path),'');
end;

class procedure TdwlFile.SetReadOnlyFlag(const FileName: string; ReadOnly: boolean);
begin
  var Att := GetFileAttributes(PChar(FileName));
  if ((Att and FILE_ATTRIBUTE_READONLY)>0) and (not ReadOnly) then
    SetFileAttributes(PChar(Filename), Att-FILE_ATTRIBUTE_READONLY)
  else
  begin
    if ((Att and FILE_ATTRIBUTE_READONLY)=0) and ReadOnly then
      SetFileAttributes(PChar(Filename), Att+FILE_ATTRIBUTE_READONLY)
  end;
end;

{ TdwlFileVersionInfo }

procedure TdwlFileVersionInfo.Clear;
begin
  Major := 0;
  Minor := 0;
  Release := 0;
  Build := 0;
  IsPreRelease := false;
end;

class function TdwlFileVersionInfo.CreateFromFile(const FileName: string=''): TdwlFileVersionInfo;
begin
  if FileName='' then
    Result.SetFromFile(ParamStr(0))
  else
    Result.SetFromFile(FileName);
end;

class function TdwlFileVersionInfo.CreateFromString(const VersionString: string; Separator: char='.'): TdwlFileVersionInfo;
begin
  Result.SetFromString(VersionString, Separator);
end;

class operator TdwlFileVersionInfo.Equal(VersionInfoA, VersionInfoB: TdwlFileVersionInfo): boolean;
begin
  Result :=
    (VersionInfoA.Major=VersionInfoB.Major) and
    (VersionInfoA.Minor=VersionInfoB.Minor) and
    (VersionInfoA.Release=VersionInfoB.Release) and
    ((VersionInfoA.Build=0) or (VersionInfoB.Build=0) or (VersionInfoA.Build=VersionInfoB.Build));
end;

function TdwlFileVersionInfo.IsEmpty: boolean;
begin
  Result := (Major=0) and (Minor=0) and (Release=0) and (not IsPrerelease);
end;

class operator TdwlFileVersionInfo.LessThan(VersionInfoA, VersionInfoB: TdwlFileVersionInfo): boolean;
begin
  Result := (VersionInfoA.Major<VersionInfoB.Major);
  if Result or (VersionInfoA.Major>VersionInfoB.Major) then
    Exit;
  Result := (VersionInfoA.Minor<VersionInfoB.Minor);
  if Result  or (VersionInfoA.Minor>VersionInfoB.Minor) then
    Exit;
  Result := (VersionInfoA.Release<VersionInfoB.Release);
  if Result or (VersionInfoA.Release>VersionInfoB.Release)  then
    Exit;
  if (VersionInfoA.Build=0) or (VersionInfoB.Build=0) then
    Exit;
  Result := VersionInfoA.Build<VersionInfoB.Build;
end;

class operator TdwlFileVersionInfo.LessThanOrEqual(VersionInfoA, VersionInfoB: TdwlFileVersionInfo): boolean;
begin
  Result := (VersionInfoA.Major<=VersionInfoB.Major);
  if (VersionInfoA.Major<>VersionInfoB.Major) then
    Exit;
  Result := (VersionInfoA.Minor<=VersionInfoB.Minor);
  if (VersionInfoA.Minor<>VersionInfoB.Minor) then
    Exit;
  Result := (VersionInfoA.Release<=VersionInfoB.Release);
  if (VersionInfoA.Release<>VersionInfoB.Release) then
    Exit;
  if (VersionInfoA.Build=0) or (VersionInfoB.Build=0) then
    Exit;
  Result := VersionInfoA.Build<=VersionInfoB.Build;
end;

class operator TdwlFileVersionInfo.NotEqual(VersionInfoA, VersionInfoB: TdwlFileVersionInfo): boolean;
begin
  Result :=
    (VersionInfoA.Major<>VersionInfoB.Major) or
    (VersionInfoA.Minor<>VersionInfoB.Minor) or
    (VersionInfoA.Release<>VersionInfoB.Release) or
    ((VersionInfoA.Build<>0) and (VersionInfoB.Build<>0) and (VersionInfoA.Build<>VersionInfoB.Build));
end;

function TdwlFileVersionInfo.GetAsString(IncludeBuild: boolean=false; IncludePrerelease: boolean=false; Separator: char='.'): string;
begin
  Result := Major.ToString+Separator+Minor.ToString+Separator+Release.ToString;
  if IncludeBuild then
    Result := Result+Separator+Build.ToString;
  if IncludePrerelease and IsPreRelease then
    Result := Result+Separator+'PreRelease';
end;

class operator TdwlFileVersionInfo.GreaterThan(VersionInfoA, VersionInfoB: TdwlFileVersionInfo): boolean;
begin
  Result := (VersionInfoA.Major>VersionInfoB.Major);
  if Result or (VersionInfoA.Major<VersionInfoB.Major) then
    Exit;
  Result := (VersionInfoA.Minor>VersionInfoB.Minor);
  if Result  or (VersionInfoA.Minor<VersionInfoB.Minor) then
    Exit;
  Result := (VersionInfoA.Release>VersionInfoB.Release);
  if Result or (VersionInfoA.Release<VersionInfoB.Release)  then
    Exit;
  if (VersionInfoA.Build=0) or (VersionInfoB.Build=0) then
    Exit;
  Result := VersionInfoA.Build>VersionInfoB.Build;
end;

class operator TdwlFileVersionInfo.GreaterThanOrEqual(VersionInfoA, VersionInfoB: TdwlFileVersionInfo): boolean;
begin
  Result := (VersionInfoA.Major>=VersionInfoB.Major);
  if (VersionInfoA.Major<>VersionInfoB.Major) then
    Exit;
  Result := (VersionInfoA.Minor>=VersionInfoB.Minor);
  if (VersionInfoA.Minor<>VersionInfoB.Minor) then
    Exit;
  Result := (VersionInfoA.Release>=VersionInfoB.Release);
  if (VersionInfoA.Release<>VersionInfoB.Release) then
    Exit;
  if (VersionInfoA.Build=0) or (VersionInfoB.Build=0) then
    Exit;
  Result := VersionInfoA.Build>=VersionInfoB.Build;
end;

procedure TdwlFileVersionInfo.SetFromFile(const FileName: string);
begin
  var Success := false;
  var Dummy: DWORD;
  var VerInfoSize := GetFileVersionInfoSize(PChar(FileName), Dummy);
  if VerInfoSize>0 then
  begin
    var VerInfo: pointer;
    GetMem(VerInfo, VerInfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), 0, VerInfoSize, VerInfo) then
      begin
        var VerValue: PVSFixedFileInfo;
        var VerValueSize: DWORD;
        if  VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize) then
        begin
          Success := true;
          Major := VerValue.dwFileVersionMS shr 16;
          Minor := VerValue.dwFileVersionMS and $FFFF;
          Release := VerValue.dwFileVersionLS shr 16;
          Build := VerValue.dwFileVersionLS and $FFFF;
          IsPreRelease := (VS_FF_PRERELEASE and VerValue.dwFileFlags)>0;
        end;
      end;
    finally
      FreeMem(VerInfo, VerInfoSize);
    end;
  end;
  if not Success then
    Clear;
end;

procedure TdwlFileVersionInfo.SetFromString(const VersionString: string; Separator: char='.');
begin
  Clear;
  if VersionString='' then
    Exit;
  var VersionStr := VersionString;
  var P := Pos(Separator, VersionStr);
  if P<2 then
    P := MaxInt;
  Major := StrToIntDef(Copy(VersionStr, 1, P-1), 0);
  if P=MaxInt then
    Exit;
  VersionStr := Copy(VersionStr, P+1, MaxInt);
  P := Pos(Separator, VersionStr);
  if P<2 then
    P := MaxInt;
  Minor := StrToIntDef(Copy(VersionStr, 1, P-1), 0);
  if P=MaxInt then
    Exit;
  VersionStr := Copy(VersionStr, P+1, MaxInt);

  P := Pos(Separator, VersionStr);
  if P<2 then
    P := MaxInt;
  Release := StrToIntDef(Copy(VersionStr, 1, P-1), 0);
  if P=MaxInt then
    Exit;
  Build := StrToIntDef(Copy(VersionStr, P+1, MaxInt), 0);
end;

{ TdwlDirectory }

class function TdwlDirectory.Application_TempDir: string;
begin
  if FApplication_TempDir='' then
  begin
    FApplication_TempDir := TPath.GetTempPath+TdwlFile.ExtractBareName(ParamStr(0));
    ForceDirectories(FApplication_TempDir);
  end;
  Result := FApplication_TempDir;
end;

class procedure TdwlDirectory.Copy(const SourceDirectory, DestinationDirectory: string; Options: TdwlIOEnumOptions=[ioIncludeFiles, ioIncludeDirectories, ioRecurse]);
begin
  var ENum := Enumerator(SourceDirectory, Options);
  while Enum.MoveNext do
  begin
    var Dest := DestinationDirectory+IfThen(ENum.Current.RelativeDirectory<>'', '\')+ENum.Current.RelativeDirectory;
    ForceDirectories(Dest);
    Dest := Dest+'\'+ENum.Current.Name;
    if ENum.Current.IsDirectory then
    begin
      if not DirectoryExists(Dest) then
        WinApi.Windows.CreateDirectory(PChar(Dest), nil)
    end
    else
    begin
      if not Winapi.Windows.CopyFile(PChar(ENum.Current.FullPathName), PChar(Dest), not (ioOverwrite in Options)) then
        raise Exception.Create('Error copying file to '+Dest+' : '+SysErrorMessage(GetLastError));
      if (ioRemoveReadOnly in Options) then
        TdwlFile.SetReadOnlyFlag(Dest, false);
    end;
  end;
end;

class procedure TdwlDirectory.Delete(const Directory: string; Options: TdwlIOEnumOptions=[ioIncludeFiles, ioIncludeDirectories, ioRecurse]);
begin
  if not DirectoryExists(Directory) then
    Exit;
  var ENum := Enumerator(Directory, Options);
  while ENum.MoveNext do
  begin
    if ioRemoveReadOnly in Options then
      TdwlFile.SetReadOnlyFlag(ENum.Current.FullPathName, false);
    if ENum.Current.IsDirectory then
    begin
      if not RemoveDirectory(PChar(ENum.Current.FullPathName)) then
        raise Exception.Create('Error deleting directory '+ENum.Current.FullPathName+' : '+SysErrorMessage(GetLastError));
    end
    else
    begin
      if not WinApi.Windows.DeleteFile(PChar(ENum.Current.FullPathName)) then
        raise Exception.Create('Error deleting file '+ENum.Current.FullPathName+' : '+SysErrorMessage(GetLastError));
    end;
  end;
  // Finally Remove the Dir itself
  if ioRemoveReadOnly in Options then
    TdwlFile.SetReadOnlyFlag(Directory, false);
  if not RemoveDirectory(PChar(Directory)) then
    raise Exception.Create('Error deleting directcory '+Directory+' : '+SysErrorMessage(GetLastError));
end;

class destructor TdwlDirectory.Destroy;
begin
  if FApplication_TempDir<>'' then
    Delete(FApplication_TempDir);
  inherited;
end;

class function TdwlDirectory.Enumerator(const Directory: string; Options: TdwlIOEnumOptions=[ioIncludeFiles]; const FileMask: string='*.*'): IdwlDirectoryEnumerator;
begin
  Result := TdwlDirectoryEnumerator.Create(Directory, Options, FileMask);
end;

class procedure TdwlDirectory.Move(const FromDirectory, ToDirectory: string; Options: TdwlIOEnumOptions);
begin
  var ENum := Enumerator(FromDirectory, Options);
  while ENum.MoveNext do
  begin
    var Dest := ToDirectory+IfThen(ENum.Current.RelativeDirectory<>'', '\')+ENum.Current.RelativeDirectory;
    ForceDirectories(Dest);
    Dest := Dest+'\'+ENum.Current.Name;
    if ENum.Current.IsDirectory then
    begin
      if not DirectoryExists(Dest) then
        WinApi.Windows.CreateDirectory(PChar(Dest), nil)
    end
    else
    begin
      if not Winapi.Windows.MoveFile(PChar(ENum.Current.FullPathName), PChar(Dest)) then
        raise Exception.Create('Error moving file to '+Dest+' : '+SysErrorMessage(GetLastError));
      if (ioRemoveReadOnly in Options) then
        TdwlFile.SetReadOnlyFlag(Dest, false);
    end;
  end;
  // All files are moved, now Remove directory, including all subdirectories
  Options := Options-[ioIncludeFiles]+[ioIncludeDirectories];
  Delete(FromDirectory, Options);
end;

{ TdwlDirectoryEnumerator }

function TdwlDirectoryEnumerator.Current: PDirEnum;
begin
  if FDirEnum.hFind=0 then
    Result := nil
  else
    Result := FDirEnum;
end;

constructor TdwlDirectoryEnumerator.Create(const Directory: string; Options: TdwlIOEnumOptions; const FileMask: string);
begin
  inherited Create;
  FOptions := Options;
  FFileMask := FileMask;
  FDirEnum := TDirEnum.Open(Directory, '');
end;

destructor TdwlDirectoryEnumerator.Destroy;
begin
  while FDirEnum<>nil do
    TDirEnum.Close(FDirEnum);
  inherited Destroy;
end;

function TdwlDirectoryEnumerator.UnfilteredMoveNext: boolean;
begin
if FDirEnum.hFind=0 then  // not initialized, initialize and find first file
  begin
    var hFind := FindFirstFile(PChar(FDirEnum.Directory+'\'+FFileMask), FDirEnum.FindData);
    if hFind=INVALID_HANDLE_VALUE  then
    begin
      var Err := GetLastError;
      if Err=ERROR_FILE_NOT_FOUND then // no results found, return
        Exit(false);
      raise Exception.Create('Error enumerating '+FDirEnum.Directory+' : '+SysErrorMessage(GetLastError));
    end;
    FDirEnum.hFind := hFind;
    Result := true;
  end
  else
  begin
    Result := FindNextFile(FDirEnum.hFind, FDirEnum.FindData);
    if not Result then
    begin
      var Err := GetLastError;
      if Err<>ERROR_NO_MORE_FILES then
        raise Exception.Create('Error enumerating '+FDirEnum.Directory+' : '+SysErrorMessage(GetLastError));
      // if there is a parent, recurse back and replace DirEnum with parent
      // the parent is still pointing at the dir and this dir will become the current
      // the parent dir has not been visited yet (we recursed first), so leave it there
      if FDirEnum.Parent<>nil then
      begin
        var Me := FDirEnum;
        FDirEnum := FDirEnum.Parent;
        TDirEnum.Close(Me);
        // and now exit here, otherwise we will recurse again into this dir.
        Exit(true);
      end;
    end;
  end;
  // Check if result is a directory
  if Result and FDirEnum.IsDirectory then
  begin
    if (FDirEnum.Name='.') or (FDirEnum.Name='..') then // skip these useless entries
      Result := UnfilteredMoveNext
    else
    begin
      // recurse into directory
      if (ioRecurse in FOptions) and ((ioIncludeHidden in FOptions) or (((FILE_ATTRIBUTE_HIDDEN+FILE_ATTRIBUTE_SYSTEM) and  FDirEnum.FindData.dwFileAttributes)=0))  then
      begin
        var Me := FDirEnum;
        FDirEnum := TDirEnum.Open(FDirEnum.BaseDirectory, FDirEnum.RelativeDirectory+IfThen(FDirEnum.RelativeDirectory<>'', '\')+FDirEnum.Name);
        FDirEnum.Parent := Me;
        // call InternalMoveNext iterative to first get results from child directory
        Result := UnfilteredMoveNext;
        if not Result then
        begin // this worked out to be an empty dir, so step back, don't known if it works in real life, because . and .. seems to be standard returns from a directory
          TDirEnum.Close(FDirEnum);
          FDirEnum := Me;
          Result := true;
        end;
      end;
    end;
  end;
end;

function TdwlDirectoryEnumerator.MoveNext: boolean;
begin
  Result := UnfilteredMoveNext;
  while Result do
  begin
    // do filtering
    if (((FILE_ATTRIBUTE_DIRECTORY and FDirEnum.FindData.dwFileAttributes)<>0) and (not (ioIncludeDirectories in FOptions))) or
      (((FILE_ATTRIBUTE_DIRECTORY and FDirEnum.FindData.dwFileAttributes)=0) and (not (ioIncludeFiles in FOptions))) or
      ((((FILE_ATTRIBUTE_HIDDEN+FILE_ATTRIBUTE_SYSTEM) and FDirEnum.FindData.dwFileAttributes)>0) and (not (ioIncludeHidden in FOptions))) then
      Result := UnfilteredMoveNext
    else
      Break;
  end;
end;

{ TDirEnum }

class procedure TDirEnum.Close(var DirEnum: PDirEnum);
begin
  var ParentEnum := DirEnum.Parent;
  if DirEnum.hFind<>0 then
    WinApi.Windows.FindClose(DirEnum.hFind);
  Dispose(DirENum);
  DirEnum := ParentEnum;
end;

function TDirEnum.Directory: string;
begin
  Result := BaseDirectory+IfThen(RelativeDirectory<>'', '\')+RelativeDirectory;
end;

function TDirEnum.FullPathName: string;
begin
  Result := Directory+'\'+Name;
end;

function TDirEnum.IsDirectory: boolean;
begin
  Result := (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)<>0;
end;

function TDirEnum.IsReadOny: boolean;
begin
  Result := (FindData.dwFileAttributes and FILE_ATTRIBUTE_READONLY)>0;
end;

function TDirEnum.Name: string;
begin
  Result := FindData.cFileName;
end;

class function TDirEnum.Open(const BaseDirectory, RelativeDirectory: string): PDirENum;
begin
  New(Result);
  Result.hFind := 0;
  Result. BaseDirectory := BaseDirectory;
  Result. RelativeDirectory := RelativeDirectory;
  Result.Parent := nil;
end;

function TDirEnum.RelativePathName: string;
begin
  Result := RelativeDirectory+IfThen(RelativeDirectory<>'', '\')+Name;
end;

end.



