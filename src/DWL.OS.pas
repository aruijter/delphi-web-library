unit DWL.OS;

interface

uses
  Winapi.Windows, DWL.Classes, DWL.Imports.WbemScripting_TLB;

type
  /// <summary>
  ///   a abstract class containing OS related utility functions
  /// </summary>
  TdwlOS = record
    class var
      FServices: ISWbemServices;
    /// <summary>
    ///   Executes a file within the windows environment. Basically a nice
    ///   wrapper around the Windows ShellExecute function
    /// </summary>
    /// <param name="FileName">
    ///   the name of the file to be executed
    /// </param>
    /// <param name="Params">
    ///   Commandline params to be added
    /// </param>
    /// <param name="DefaultDir">
    ///   the directory within the file will be executed
    /// </param>
    /// <param name="ShowCmd">
    ///   The showwindow command to be used
    /// </param>
    /// <returns>
    ///   indicating if the execution was successful
    /// </returns>
    class function ExecuteFile(const FileName: string; const Params: string=''; const DefaultDir: string=''; ShowCmd: integer=SW_SHOW): TdwlResult; static;
    /// <summary>
    ///   Executes a file within the windows environment and waits until it is
    ///   finished. Basically a nice wrapper around the Windows ShellExecute
    ///   function
    /// </summary>
    /// <param name="FileName">
    ///   the name of the file to be executed
    /// </param>
    /// <param name="Params">
    ///   Commandline params to be added
    /// </param>
    /// <param name="DefaultDir">
    ///   the directory within the file will be executed
    /// </param>
    /// <param name="ShowCmd">
    ///   The showwindow command to be used
    /// </param>
    /// <returns>
    ///   indicating if the execution was successful
    /// </returns>
    class function ExecuteFileAndWait(const FileName: string; const Params: string=''; const DefaultDir: string=''; ShowCmd: integer=SW_SHOW): TdwlResult; static;
    /// <summary>
    ///   Launches an URI within the windows environment
    /// </summary>
    /// <param name="URI">
    ///   URI to be executed
    /// </param>
    /// <returns>
    ///   indicating if the execution was successful
    /// </returns>
    class function ExecuteURI(const URI: string): TdwlResult; static;
    /// <summary>
    ///   Find the filename of an executable by providing the extension. The
    ///   windows OS is consulted for the result
    /// </summary>
    /// <param name="Extension">
    ///   The extension for which an executable filename is to be retrieved
    /// </param>
    /// <example>
    ///   FileName := TdwlOSUtils.FindExecutableByExtension('.doc');
    /// </example>
    class function FindExecutableByExtension(const Extension: string): string; static;
    /// <summary>
    ///   Find the filename of an executable by providing the ProgID. The
    ///   windows OS is consulted for the result
    /// </summary>
    /// <param name="ProgID">
    ///   The progID for which and executable finame is to be retrieved
    /// </param>
    /// <example>
    ///   FileName := TdwlOSUtils.FindExecutableByProgID('Word.Document.8');
    /// </example>
    class function FindExecutableByProgID(const ProgID: string): string; static;
    /// <summary>
    ///   get the full SID of the currently logged in user
    /// </summary>
    class function GetWindowsUserSid: string; static;
    /// <summary>
    ///   Query a value within the Windows WMI system
    /// </summary>
    /// <param name="Win32Class">
    ///   The WMI Class to use
    /// </param>
    /// <param name="Win32Property">
    ///   The WMI Property to use
    /// </param>
    class function WMI_Value(const Win32Class, Win32Property: string): string; static;
    class function NumberOfLogicalProcessors: cardinal; static;
  end;

implementation

uses
  System.SysUtils, Winapi.ShellAPI, Winapi.ShLwApi,
  Winapi.ActiveX;

{ TdwlOS }

class function TdwlOS.ExecuteFile(const FileName, Params, DefaultDir: string; ShowCmd: integer): TdwlResult;
  procedure CheckRes(Res:HINST);
  begin
    if Res<=32 then
      Result.AddErrorMsg('Failed to execute program: "'+FileName+' '+Params+'". Error Code ' +IntToStr(GetLastError));
  end;
begin
  var DefDir := DefaultDir;
  if DefDir='' then
    DefDir := ExtractFilePath(FileName);
  if DefDir='' then
    DefDir := ExtractFilePath(ParamStr(0));
  var Ext := ExtractFileExt(FileName).ToLower;
  if Ext='.exe' then
    CheckRes(ShellExecute(0, nil, PWideChar(FileName), PWideChar(Params), PWideChar(DefDir), ShowCmd))
  else
  begin
    // no exe, params not allowed
    if Params<>'' then
      Result.AddErrorMsg('Params are only effective when executing an executable');
    // evaluate if this is an office xml
    if SameText(Ext, '.xml')  then
    begin
      // the second line contains the type, we apporach this quite lazy ;-)
      var f: textfile;
      AssignFile(f, FileName);
      Reset(f);
      try
        Readln(f);
        var Line: string;
        Readln(f, Line);
        Line := Line.ToLower;
        var P := Pos('progid="', Line);
        if P>=0 then
        begin
          Line := Copy(Line, P+8);
          P := Pos('"', Line);
          if P>0 then
          begin
            Line := Copy(Line, 1, P-1);
            var Exe := FindExecutableByProgID(Line);
            if Exe<>'' then
            begin
              CheckRes(ShellExecute(0, nil, PWideChar(Exe), PWideChar('"'+FileName+'"'), PWideChar(DefaultDir), ShowCmd));
              Exit;
            end;
          end;
        end;
      finally
        CloseFile(f);
      end;
    end;
    var Exe := FindExecutableByExtension(Ext);
    if Exe<>'' then
      CheckRes(ShellExecute(0, nil, PWideChar(Exe), PWideChar('"'+FileName+'"'), PWideChar(DefaultDir), ShowCmd))
    else
      CheckRes(ShellExecute(0, nil, PWideChar(FileName), PWideChar(Params), PWideChar(DefaultDir), ShowCmd))
  end;
end;


class function TdwlOS.ExecuteFileAndWait(const FileName, Params, DefaultDir: string; ShowCmd: integer): TdwlResult;
  procedure CheckRes(Res:HINST);
  begin
    if Res<=32 then
      Result.AddErrorMsg('Failed to execute program: "'+FileName+' '+Params+'". Error Code ' +IntToStr(GetLastError));
  end;
begin
  var DefDir := DefaultDir;
  if DefDir='' then
    DefDir := ExtractFilePath(FileName);
  if DefDir='' then
    DefDir := ExtractFilePath(ParamStr(0));
  var Ext := ExtractFileExt(FileName).ToLower;
  if Ext<>'.exe' then
  begin
    Result.AddErrorMsg('ExecuteFileAndWait can only be called for executable files');
    Exit;
  end;
  var SI: TStartupInfo;
  var PI: TProcessInformation;
  FillChar(SI, SizeOf(TStartupInfo), 0);
  SI.cb := SizeOf(TStartupInfo);
  SI.dwFlags := STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
  SI.wShowWindow := ShowCmd;
  if not CreateProcess(nil, PWideChar(trim(FileName+' '+Params)), nil, nil, False,
    NORMAL_PRIORITY_CLASS, nil, PWideChar(DefDir), SI, PI) then
  begin
    Result.AddErrorMsg('Failed to execute program: "'+FileName+' '+Params+'". Error Code ' +IntToStr(GetLastError));
    Exit;
  end;
  var Proc := PI.hProcess;
  CloseHandle(PI.hThread);
  if WaitForSingleObject(Proc, Infinite)=WAIT_FAILED then
    Result.AddErrorMsg('Waiting for failed');
  CloseHandle(Proc);
end;

class function TdwlOS.ExecuteURI(const URI: string): TdwlResult;
begin
  if ShellExecute(0, nil, PWideChar(URI), '', '', SW_SHOW)< 32 then
    Result.AddErrorMsg('Failed to execute program: "'+URI+'". Error Code ' +IntToStr(GetLastError));
end;

class function TdwlOS.FindExecutableByExtension(const Extension: string): string;
begin
  SetLength(Result, MAX_PATH);
  var Ext := Extension;
  if Ext='' then
    Exit('');
  if Ext[1]<>'.' then
    Ext := '.'+Ext;
  var StrLen := MAX_PATH;
  var Res := AssocQueryString(0, ASSOCSTR_EXECUTABLE, PWideChar(Ext), 'open', PWideChar(Result), @StrLen);
  if Res = S_OK then
    SetLength(Result, StrLen - 1)
  else
    Result := '';
end;

class function TdwlOS.FindExecutableByProgID(const ProgID: string): string;
begin
  SetLength(Result, MAX_PATH);
  var StrLen := MAX_PATH;
  var Res := AssocQueryString(0, ASSOCSTR_EXECUTABLE, PWideChar(ProgID), 'open', PWideChar(Result), @StrLen);
  if Res = S_OK then
    SetLength(Result, StrLen - 1)
  else
    Result := '';
end;

class function TdwlOS.GetWindowsUserSid: string;
var
  UserNameSam: array[0..249] of char;
begin
  Result := '';
  var L: ULONG := 250;
  if GetUserNameEx(NameSamCompatible, UserNameSam, L)<>0 then
  begin
    var SidSize: ULONG := 100; // according to specs 68 bytes, so take some room for future
    var Sid: pointer;
    GetMem(Sid, SidSize);
    try
      var ReferencedDomainName: string;
      L := 250;
      Setlength(ReferencedDomainName, L);
      var SidUse: DWORD;
      if LookupAccountname(nil, UserNameSam, Sid, SidSize, PChar(ReferencedDomainName), L, SidUse) then
      begin
        var StringSid: PChar;
        if ConvertSidToStringSid(Sid, StringSid) then
        try
          SetLength(Result, Length(StringSid));
          StrCopy(PChar(Result), StringSid);
        finally
          LocalFree(StringSid);
        end;
      end;
    finally
      FreeMem(Sid);
    end;
  end;
end;

class function TdwlOS.NumberOfLogicalProcessors: cardinal;
begin
  var SystemInfo: _SYSTEM_INFO;
  GetSystemInfo(SystemInfo);
  Result := SystemInfo.dwNumberOfProcessors;
end;

class function TdwlOS.WMI_Value(const Win32Class, Win32Property: string): string;
begin
  if FServices=nil then
    FServices := CoSWbemLocator.Create.ConnectServer('', 'root\cimv2', '', '', '','', 0, nil);
  Result := '';
  try
    var ObjSet := FServices.ExecQuery('SELECT '+Win32Property+' FROM '+WIn32Class, 'WQL', wbemFlagReturnImmediately and wbemFlagForwardOnly , nil);
    var Enum := (ObjSet._NewEnum) as IEnumVariant;
    var TempObj:  OleVariant;
    var Value: cardinal;
    while Enum.Next(1, TempObj, Value) = S_OK do
    begin
      var SObject := IUnknown(TempObj) as ISWBemObject;
      try
        Result := trim(SObject.Properties_.Item(Win32Property, 0).Get_Value);
      except
        // maybe next try is better
      end;
   end;
  except
    // return the empty string
  end;
end;
end.
