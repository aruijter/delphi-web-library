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
    class procedure Import_CommandLine(const Params: IdwlParams); static;
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
    class procedure Import_IniFile_Section(const Params: IdwlParams; const IniFileName, Section: string); static;
  end;

implementation

uses
  System.SysUtils, System.IniFiles, System.Classes;

{ TdwlParamsUtils }

class procedure TdwlParamsUtils.Import_CommandLine(const Params: IdwlParams);
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
    Params.WriteValue(Key.TrimLeft(['-','/']), Value);
    inc(ParamNo);
  end;
end;

class procedure TdwlParamsUtils.Import_IniFile_Section(const Params: IdwlParams; const IniFileName, Section: string);
begin
  var IniFile := TIniFile.Create(IniFileName);
  try
    var Values := TStringList.Create;
    try
      IniFile.ReadSectionValues(Section, Values);
      for var i := 0 to Values.Count-1 do
        Params.WriteValue(Values.Names[i], Values.ValueFromIndex[i]);
    finally
      Values.Free;
    end;
  finally
    IniFile.Free;
  end;
end;

end.
