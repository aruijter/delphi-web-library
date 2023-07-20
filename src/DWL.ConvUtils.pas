unit DWL.ConvUtils;

interface

uses
  System.SysUtils;

type
  TdwlConvUtils = record
    class var
       FDotFormatSettings: TFormatSettings;
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
    class function TryDotStrToFloat(const TextToConvert: string; out Value: double): boolean; static;
    class function DotStrToFloatDef(const TextToConvert: string; Default: double=0): double; static;
    class function FloatToDotStr(Value: double): string; static;
    class function BytestoLowerHex(Bytes: TBytes): string; static;
    class function HexToBytes(const Hex: string): TBytes; static;
    class constructor Create;
  end;


implementation


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

class function TdwlConvUtils.TryDotStrToFloat(const TextToConvert: string; out Value: double): boolean;
begin
  try
    Result := DWL.ConvUtils.TextToFloat(TextToConvert, Value, FDotFormatSettings);
  except
    Result := false;
  end;
end;

class function TdwlConvUtils.BytestoLowerHex(Bytes: TBytes): string;
const
  Convert: array[0..15] of WideChar = '0123456789abcdef';
begin
  if Bytes=nil then
  begin
    Result := '';
    Exit;
  end;
  var L := Length(Bytes);
  SetLength(Result, L*2);
  for var i := 0 to L-1 do
  begin
    Result[i*2+1] := Convert[Bytes[i] shr 4];
    Result[i*2+2] := Convert[Bytes[i] and $F];
  end;
end;

class constructor TdwlConvUtils.Create;
begin
  inherited;
  FDotFormatSettings.DecimalSeparator := '.';
end;

class function TdwlConvUtils.DotStrToFloatDef(const TextToConvert: string;
  Default: double=0): double;
begin
  if not TryDotStrToFloat(TextToConvert, Result) then
    Result := Default;
end;

class function TdwlConvUtils.FloatToDotStr(Value: double): string;
begin
  Result := FloatToStr(Value, FDotFormatSettings);
end;

class function TdwlConvUtils.HexToBytes(const Hex: string): TBytes;
const
  H2BConvert: array['0'..'f'] of SmallInt =
    ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15);
begin
  Result := nil;
  var Count := Length(Hex);
  if Count mod 2<>0 then
    Exit;
  SetLength(Result, Count div 2);
  for var i := 0 to High(Result) do
    Result[i] :=
      (H2BConvert[Hex[i*2+1]] shl 4) or
       H2BConvert[Hex[i*2+2]];
end;

end.
