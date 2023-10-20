unit uCocon;
{$WARN SYMBOL_PLATFORM OFF}
interface

procedure Install_New_Exe(const OldExe, ActualExe, NewExe: string);

implementation

uses
  Winapi.Windows, Winapi.ShellAPI, System.SysUtils, System.IOUtils,
  System.Classes;

const
  RETRY_MSECS = 1000;
  COPY_FILE_ATTEMPTS = 15;

procedure Install_New_Exe(const OldExe, ActualExe, NewExe: string);
var
  ErrorLog: TStringlist;
  Res: Cardinal;

  function DoDeleteFile(const Fn: string): boolean;
  begin
    Result := not FileExists(Fn);
    if Result then
      Exit;
    var T := GetTickCount64+RETRY_MSECS;
    Result := Winapi.Windows.DeleteFile(PChar(Fn));
    if not Result then
      ErrorLog.Add('Failed to delete ' + Fn + ' at first attempt');
    while (not Result) and (GetTickCount64<T) do
    begin
      sleep(250);
      Result := Winapi.Windows.DeleteFile(PChar(Fn));
      if not Result then
        ErrorLog.Add('DeleteFile error: ' + SysErrorMessage(GetLastError));
    end;
    if not Result then
      ErrorLog.AddStrings(ErrorLog);
  end;

  function DoCopyFile(const SourceFn, DestFn: string): boolean;
  var
    ErrCode: Cardinal;
    Attempts: integer;
  begin
    Result := Winapi.Windows.CopyFile(PChar(SourceFn), PChar(DestFn), false);
    if Result then
      Exit;
    if not Result then
      ErrorLog.Add('Failed to copy ' + SourceFn + ' at first attempt to ' + DestFn);

    ErrCode := GetLastError;
    if ErrCode <= 32 then
      ErrorLog.Add('CopyFile error: ' + SysErrorMessage(ErrCode));

    Attempts := 0;
    while (not Result) and (Attempts<COPY_FILE_ATTEMPTS) do
    begin
      Result := Winapi.Windows.CopyFile(PChar(SourceFn), PChar(DestFn), false);
      inc(Attempts);
      if not Result then
      begin
        Sleep(RETRY_MSECS);
        ErrorLog.Add('CopyFile error: ' + SysErrorMessage(GetLastError));
      end;
    end;
    if not Result then
      ErrorLog.Add('Failed to copy ' + SourceFn + ' to ' + DestFn + ' in '  + Attempts.ToString + ' attempts');
  end;

var
  Parm: string;
  i: integer;
begin
  ErrorLog := TStringList.Create;
  try
    var LogFileName := ExtractFilePath(ActualExe)+'CoCon_errors.log';
    var AllOk := true;
    try
      // cleanup and copy the Current exe to the old exe (for fallback scenario's)
      AllOk := DoDeleteFile(LogFileName) and DoDeleteFile(OldExe) and DoCopyFile(ActualExe, OldExe);
      // copy the new exe in place
      AllOk := AllOk and DoCopyFile(NewExe, ActualExe);
      // remove the old and the fallback
      AllOk := AllOk and DoDeleteFile(OldExe);
      AllOk := AllOk and DoDeleteFile(NewExe);
      if ParamCount>2 then
      begin
        Parm := ParamStr(3);
        for i := 4 to ParamCount do
          Parm := Parm+' '+ParamStr(i);
      end
      else
        Parm := '';
      Res := ShellExecute(0, nil, PChar(ActualExe), PChar(Parm), '', 5);
      if Res <= 32 then
      begin
        AllOk := false;
        ErrorLog.Add('Failed to execute ' + ActualExe + ' ' + Parm + ' error ' + SysErrorMessage(GetLastError));
      end
    finally
      if not AllOk then
      begin
        ErrorLog.Insert(0, '');
        ErrorLog.Insert(0, 'Timeout = ' + RETRY_MSECS.ToString);
        ErrorLog.Insert(0, 'Executed ' + CmdLine);
        TFile.WriteAllText(LogFileName, ErrorLog.Text);
      end;
    end;
  finally
    ErrorLog.Free;
  end;
end;

end.
