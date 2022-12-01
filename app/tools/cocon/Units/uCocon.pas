unit uCocon;

interface

procedure Install_New_Exe(const OldExe, ActualExe, NewExe: string);

implementation

uses
  Winapi.Windows, Winapi.ShellAPI, System.SysUtils, System.IOUtils,
  System.Classes;

const
  COPY_RETRY_MSECS = 1000;
  DEL_RETRY_MSECS = 1000;
  COPY_FILE_ATTEMPTS = 40;

procedure Install_New_Exe(const OldExe, ActualExe, NewExe: string);
var
  ErrorLog: TStringlist;
  ErrorLog0: TStringlist;
  Res: Cardinal;

  function DoDeleteFile(const Fn: string): boolean;
  var
    T: cardinal;
  begin
    Result := not FileExists(Fn);
    if Result then
      Exit;
    T := GetTickCount+DEL_RETRY_MSECS;
    Result := Winapi.Windows.DeleteFile(PChar(Fn));
    if not Result then
      ErrorLog0.Add('Failed to delete ' + Fn + ' at first attempt');
    while (not Result) and (GetTickCount<T) do
    begin
      sleep(250);
      Result := Winapi.Windows.DeleteFile(PChar(Fn));
      if not Result then
        ErrorLog0.Add('DeleteFile error: ' + SysErrorMessage(GetLastError));
    end;
    if not Result then
      ErrorLog.AddStrings(ErrorLog0);
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
      ErrorLog0.Add('Failed to copy ' + SourceFn + ' at first attempt to ' + DestFn);

    ErrCode := GetLastError;
    if ErrCode <= 32 then
      ErrorLog0.Add('CopyFile error: ' + SysErrorMessage(ErrCode));

    Attempts := 0;
    while (not Result) and (Attempts<COPY_FILE_ATTEMPTS) do
    begin
      Result := Winapi.Windows.CopyFile(PChar(SourceFn), PChar(DestFn), false);
      inc(Attempts);
      if not Result then
      begin
        Sleep(COPY_RETRY_MSECS);
        ErrorLog0.Add('CopyFile error: ' + SysErrorMessage(GetLastError));
      end;
    end;
    if not Result then
      ErrorLog.Add('Failed to copy ' + SourceFn + ' to ' + DestFn + ' in '  + Attempts.ToString + ' attempts');
  end;

var
  Parm: string;
  i: integer;
begin
  ErrorLog := TStringlist.Create;
  ErrorLog0 := TStringlist.Create;
  try
    // copy the Current exe to the old exe (for fallback scenario's)
    if not DoDeleteFile(OldExe) then
      ErrorLog.Add('Failed to delete ' + OldExe);
    if not DoCopyFile(ActualExe, OldExe) then
      ErrorLog.Add('Failed to copy ' + ActualExe + ' to ' + OldExe);

    // copy the new exe in place
    if DoCopyFile(NewExe, ActualExe) then
    begin
      DoDeleteFile(OldExe);
      DoDeleteFile(NewExe);
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
        ErrorLog.Add('Failed to execute ' + ActualExe + ' ' + Parm + ' error ' + SysErrorMessage(GetLastError));
    end
    else
      ErrorLog.Add('Failed to copy ' + NewExe + ' to ' + ActualExe);

    if ErrorLog.Count > 0 then
    begin
      ErrorLog.Insert(0, 'Timeout delete = ' + DEL_RETRY_MSECS.ToString + ' copy = ' + COPY_RETRY_MSECS.ToString);
      ErrorLog.Insert(0, 'Executed ' + CmdLine);
      TFile.WriteAllText(ChangeFileExt(ParamStr(0), '_errors.log'), ErrorLog.Text);
    end;
  finally
    ErrorLog.Free;
    ErrorLog0.Free;
  end;
end;

end.
