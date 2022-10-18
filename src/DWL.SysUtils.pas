unit DWL.SysUtils;

interface

uses
  System.SysUtils;

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
function BytestoLowerHex(Bytes: TBytes): string;
function LowerHexToBytes(const Hex: string): TBytes;

implementation

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

function BytestoLowerHex(Bytes: TBytes): string;
const
  Convert: array[0..15] of WideChar = '0123456789abcdef';
var
  I: integer;
  L: integer;
begin
  if Bytes=nil then
  begin
    Result := '';
    Exit;
  end;
  L := Length(Bytes);
  SetLength(Result, L*2);
  for I := 0 to L-1 do
  begin
    Result[I*2+1] := Convert[Bytes[I] shr 4];
    Result[I*2+2] := Convert[Bytes[I] and $F];
  end;
end;

function LowerHexToBytes(const Hex: string): TBytes;
const
  H2BConvert: array['0'..'f'] of SmallInt =
    ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15);
var
  I: integer;
  Count: integer;
begin
  Result := nil;
  Count := Length(Hex);
  if Count mod 2<>0 then
    Exit;
  SetLength(Result, Count div 2);
  for I := 0 to High(Result) do
    Result[I] :=
      (H2BConvert[Hex[I*2+1]] shl 4) or
       H2BConvert[Hex[I*2+2]];
end;

initialization
  _DotFormatSettings.DecimalSeparator := '.';

end.
