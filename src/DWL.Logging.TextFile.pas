unit DWL.Logging.TextFile;

interface

uses
  DWL.Logging;

function EnableLogDispatchingToTextFile(const LogFileName: string=''; const Identifier: string=''): TdwlLogDispatcher;

implementation

uses
  System.SysUtils, System.DateUtils, System.IOUtils, DWL.StrUtils;

type
  TdwlLogDispatcher_TextFile = class(TdwlLogDispatcher)
  strict private
    FFileName: string;
  protected
    procedure DispatchLog(LogItem: PdwlLogItem); override;
  public
    constructor Create(const identifier, Filename: string);
  end;

function EnableLogDispatchingToTextFile(const LogFileName: string=''; const Identifier: string=''): TdwlLogDispatcher;
begin
  var lFileName: string;
  if(LogFileName = '') then
  begin
    lFileName := ChangeFileExt(ParamStr(0), '.log');
    ForceDirectories(ExtractFilePath(lFileName));
  end
  else
    lFileName := LogFileName;
  Result := TdwlLogDispatcher_TextFile.Create(Identifier, lFileName);
  TdwlLogger.RegisterDispatcher(Result);
end;

{ TdwlLogDispatcher_TextFile }

constructor TdwlLogDispatcher_TextFile.Create(const identifier, Filename: string);
begin
  inherited Create(Identifier);
  FFileName := Filename;
end;

procedure TdwlLogDispatcher_TextFile.DispatchLog(LogItem: PdwlLogItem);
begin
  try
    var LogTxt := FormatDateTime('YYYY-MM-DD HH:MM:SS', UnixToDateTime(LogItem.TimeStamp, false))+' '+
      TdwlLogger.GetSeverityLevelAsString(LogItem.SeverityLevel)+' '+
      TdwlStrUtils.Sanatize(LogItem.Msg, [soRemoveLineBreaks])+#13#10;
    if not TFile.Exists(FFileName) then
    begin
      ForceDirectories(ExtractFilePath(FFileName));
      TFile.WriteAllText(FFileName, LogTxt, TEncoding.UTF8);
      if SameText(Copy(LogItem.ContentType, 1, 4), 'text') then
        TFile.WriteAllBytes(FFileName, LogItem.Content);
    end
    else
      TFile.AppendAllText(FFileName, LogTxt, TEncoding.UTF8);
  except
    //when an exception is thrown, do nothing. The application mustn't suffer from the logging not being properly initialized.
  end;
end;

end.

