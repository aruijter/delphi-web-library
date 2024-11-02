unit DWL.IOUtils;

interface

uses
  System.IOUtils, System.Classes;

type
  TdwlFile = record
    class function ExtractBareName(const Path: string=''): string; static;
  end;

  TdwlDirectoryEnumOption = (eoRecurse, eoIncludeFiles, eoIncludeDirectories);
  TdwlDirectoryEnumOptions = set of TdwlDirectoryEnumOption;

  IdwlDirectoryEnumerator = interface
    function CurrentDirectory: string;
    function CurrentBareName: string;
    function CurrentName: string;
    function CurrentFullPath: string;
    function CurrentIsDirectory: boolean;
    function CurrentRelativePath: string;
    function MoveNext: boolean;
  end;

  TdwlDirectory = record
  strict private
    class var FApplication_TempDir: string;
  public
    class function Application_TempDir: string; static;
    class function GetEnumerator(const Directory: string; Options: TdwlDirectoryEnumOptions=[eoIncludeFiles]; const FileMask: string='*.*'): IdwlDirectoryEnumerator; static;
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
  System.SysUtils, Winapi.Windows, DWL.Resolver, System.Masks, System.StrUtils;

type
  PDirEnum = ^TDirEnum;
  TDirEnum = record
    hFind: THandle;
    FindData: TWin32FindData;
    Parent: PDirEnum;
    BaseDirectory: string;
    RelativeDirectory: string;
    class procedure Close(var DirEnum: PDirEnum); static;
    class function Open(const BaseDirectory, RelativeDirectory: string): PDirENum; static;
    function IsDirectory: boolean;
    function Name: string;
    function Directory: string;
  end;

  TdwlDirectoryEnumerator = class(TInterfacedObject, IdwlDirectoryEnumerator)
  strict private
    FDirEnum: PDirEnum;
    FFileMask: string;
    FOptions: TdwlDirectoryEnumOptions;
    function UnfilteredMoveNext: boolean;
  private
    function CurrentDirectory: string;
    function CurrentBareName: string;
    function CurrentName: string;
    function CurrentFullPath: string;
    function CurrentIsDirectory: boolean;
    function CurrentRelativePath: string;
    function MoveNext: boolean;
  public
    constructor Create(const Directory: string; Options: TdwlDirectoryEnumOptions; const FileMask: string);
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
  var VerInfoSize := GetFileVersionInfoSize(PWideChar(FileName), Dummy);
  if VerInfoSize>0 then
  begin
    var VerInfo: pointer;
    GetMem(VerInfo, VerInfoSize);
    try
      if GetFileVersionInfo(PWideChar(FileName), 0, VerInfoSize, VerInfo) then
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

class function TdwlDirectory.GetEnumerator(const Directory: string; Options: TdwlDirectoryEnumOptions=[eoIncludeFiles]; const FileMask: string='*.*'): IdwlDirectoryEnumerator;
begin
  Result := TdwlDirectoryEnumerator.Create(Directory, Options, FileMask);
end;

{ TdwlDirectoryEnumerator }

function TdwlDirectoryEnumerator.CurrentBareName: string;
begin
  Result := TdwlFile.ExtractBareName(FDirEnum.Name)
end;

function TdwlDirectoryEnumerator.CurrentFullPath: string;
begin
  Result := FDirEnum.Directory+'\'+FDirEnum.Name;
end;

constructor TdwlDirectoryEnumerator.Create(const Directory: string; Options: TdwlDirectoryEnumOptions; const FileMask: string);
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

function TdwlDirectoryEnumerator.CurrentDirectory: string;
begin
  Result := FDirEnum.BaseDirectory+IfThen(FDirEnum.RelativeDirectory<>'', '\')+FDirEnum.RelativeDirectory;
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
      if eoRecurse in FOptions then
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

function TdwlDirectoryEnumerator.CurrentIsDirectory: boolean;
begin
  Result := FDirEnum.IsDirectory;
end;

function TdwlDirectoryEnumerator.MoveNext: boolean;
begin
  Result := UnfilteredMoveNext;
  // do filtering
  if Result then
  begin
    if ((FILE_ATTRIBUTE_DIRECTORY and FDirEnum.FindData.dwFileAttributes)<>0) and (not (eoIncludeDirectories in FOptions)) then
      Exit(MoveNext);
    if ((FILE_ATTRIBUTE_DIRECTORY and FDirEnum.FindData.dwFileAttributes)=0) and (not (eoIncludeFiles in FOptions)) then
      Exit(MoveNext);
  end;
end;

function TdwlDirectoryEnumerator.CurrentName: string;
begin
  Result := FDirEnum.Name;
end;

function TdwlDirectoryEnumerator.CurrentRelativePath: string;
begin
  Result := FDirEnum.RelativeDirectory+IfThen(FDirEnum.RelativeDirectory<>'', '\')+FDirEnum.Name;
end;

{ TDirEnum }

class procedure TDirEnum.Close(var DirEnum: PDirEnum);
begin
  var ParentEnum := DirEnum.Parent;
  if DirEnum.hFind<>0 then
    FindClose(DirEnum.hFind);
  Dispose(DirENum);
  DirEnum := ParentEnum;
end;

function TDirEnum.Directory: string;
begin
  Result := BaseDirectory+IfThen(RelativeDirectory<>'', '\')+RelativeDirectory;
end;

function TDirEnum.IsDirectory: boolean;
begin
  Result := (hFind<>0) and ((FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)<>0);
end;

function TDirEnum.Name: string;
begin
  if hFind=0 then
    Exit('');
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

end.



