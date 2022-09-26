unit DWL.SysUtils;

interface

/// <summary>
///   A string to float conversion function that always uses a decimal point
///   for fraction separation.
/// </summary>
/// <param name="TextToConvert">
///   Text to be converted
/// </param>
/// <param name="Value">
///   Converted double value
/// </param>
/// <returns>
///   boolean indicating of conversion was successful
/// </returns>
/// <remarks>
///   This is a very handy function when converting string to double in a
///   technical setting. fe. xml, json, locale independent configurations, etc
/// </remarks>
function TryDotStrToFloat(const TextToConvert: string; out Value: double): boolean;

implementation

uses
  System.SysUtils;

var
  _DotFormatSettings: TFormatSettings;

// AdR 2018-11-05
// there is a bug in TextToFloat that can raise an exception
// (See https://quality.embarcadero.com/browse/RSP-21532)
// so this is a workaround, and can be removed once it's repaired by Embarcadero...
// 2022-09-19 Even FOUR!! years later not fixed and still status 'InternalDev' in Quality Central
function TextToFloat(const S: string; var Value: Double; const AFormatSettings: TFormatSettings): Boolean;
var
  ExtValue: Extended;
begin
  Result := System.SysUtils.TextToFloat(PWideChar(S), ExtValue, fvExtended, AFormatSettings);
  if Result then
    Value := ExtValue;
end;
// end of workaround replacement

function TryDotStrToFloat(const TextToConvert: string; out Value: double): boolean;
begin
  try
    Result := DWL.SysUtils.TextToFloat(TextToConvert, Value, _DotFormatSettings);
  except
    Result := false;
  end;
end;

initialization
  _DotFormatSettings.DecimalSeparator := '.';

end.
