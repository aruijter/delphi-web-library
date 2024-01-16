unit DWL.Params.Utils;

{$I DWL.inc}

interface

uses
  DWL.Params;

type
  TdwlParamsUtils = record
  public
    /// <summary>
    ///   Imports all parameters for the commandline into an existing
    ///   IdwlParams
    /// </summary>
    /// <param name="Params">
    ///   Params to fill with the resulting pairs
    /// </param>
    class procedure Import_CommandLine(const Params: IdwlParams; LogImportedKeys: boolean=false); static;
    /// <summary>
    ///   Imports all values from one section of an inifile into an existing
    ///   IdwlParams <br />
    /// </summary>
    /// <param name="Params">
    ///   Params to fill with the resulting pairs
    /// </param>
    /// <param name="IniFileName">
    ///   FileName of the Inifile to import from
    /// </param>
    /// <param name="Section">
    ///   Section in IniFile containing Name-value pairs to import
    /// </param>
    class procedure Import_IniFile_Section(const Params: IdwlParams; const IniFileName, Section: string; LogImportedKeys: boolean=false); static;
  end;

implementation

uses
  System.SysUtils, System.IniFiles, System.Classes, DWL.Logging;

{ TdwlParamsUtils }

class procedure TdwlParamsUtils.Import_CommandLine(const Params: IdwlParams; LogImportedKeys: boolean=false);
begin
  var ParamNo := 1;
  while ParamNo<=ParamCount do
  begin
    var Key := ParamStr(ParamNo);
    var Value := '';
    // if an equal sign is present then split key into key/value
    var P := pos('=', Key);
    if P>1 then
    begin
      Value := Copy(Key, P+1, MaxInt);
      Key := Copy(key, 1, P-1);
    end
    else
    begin
      // if a switch is signalled, read the next parameter as value
      if Key.StartsWith('-')  or Key.StartsWith('/') then
      begin
        inc(ParamNo);
        Value := ParamStr(ParamNo);
      end;
    end;
    Key := Key.TrimLeft(['-','/']);
    Params.WriteValue(Key, Value);
    if LogImportedKeys then
      TdwlLogger.Log('read kvp: '+Key+'='+Value+' from cmdline');
    inc(ParamNo);
  end;
end;

class procedure TdwlParamsUtils.Import_IniFile_Section(const Params: IdwlParams; const IniFileName, Section: string; LogImportedKeys: boolean=false);
begin
  var IniFile := TIniFile.Create(IniFileName);
  try
    var Values := TStringList.Create;
    try
      IniFile.ReadSectionValues(Section, Values);
      for var i := 0 to Values.Count-1 do
      begin
        var Key := Values.Names[i];
        var Value := Values.ValueFromIndex[i];
        Params.WriteValue(Key, Value);
        if LogImportedKeys then
          TdwlLogger.Log('read kvp: '+Key+'='+Value+' section:'+Section+' file:'+IniFileName);
      end;
    finally
      Values.Free;
    end;
  finally
    IniFile.Free;
  end;
end;

end.
