unit DWL.IOUtils;

interface

uses
  System.IOUtils;

type
  TdwlFile = record
    class function ExtractBareName(const Path: string=''): string; static;
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
  System.SysUtils, Winapi.Windows;

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

end.
