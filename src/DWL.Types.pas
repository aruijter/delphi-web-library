unit DWL.Types;

interface

uses
  System.Rtti, System.UITypes;

type
  /// <summary>
  ///   A record including class operators to represent a Unix Time.
  /// </summary>
  TUnixEpoch = record
  strict private
    var
      FEpoch: Int64;
  public
    class function Now: TUnixEpoch;  static;
    class operator Add(Epoch: TUnixEpoch; Amount: Int64): TUnixEpoch;
    class operator Equal(EpochA, EpochB: TUnixEpoch): boolean;
    class operator GreaterThan(EpochA, EpochB: TUnixEpoch): boolean;
    class operator GreaterThanOrEqual(EpochA, EpochB: TUnixEpoch): boolean;
    class operator Implicit(Epoch: TUnixEpoch): Int64;
    class operator Implicit(Epoch: TUnixEpoch): TValue;
    class operator Implicit(I: Int64): TUnixEpoch;
    class operator LessThan(EpochA, EpochB: TUnixEpoch): boolean;
    class operator LessThanOrEqual(EpochA, EpochB: TUnixEpoch): boolean;
    class operator NotEqual(EpochA, EpochB: TUnixEpoch): boolean;
    class operator Subtract(EpochA, EpochB: TUnixEpoch): integer;
    class operator Subtract(Epoch: TUnixEpoch; Amount: Int64): TUnixEpoch;
    /// <param name="ADateTime">
    ///   The dattime to be initialized with
    /// </param>
    /// <param name="IsUTC">
    ///   A boolean to indicate if it is not UTC time and that the given local
    ///   date time must be converted to UTC
    /// </param>
    constructor Create(ADateTime: TDateTime; IsUTC: boolean=true);
    /// <summary>
    ///   Set the value of the time using date and time components
    /// </summary>
    /// <param name="Year">
    ///   Julian Year
    /// </param>
    /// <param name="Month">
    ///   Julian Month
    /// </param>
    /// <param name="Day">
    ///   Julian Day
    /// </param>
    /// <param name="Hour">
    ///   Hour of the day
    /// </param>
    /// <param name="Minute">
    ///   Minutes within the hour
    /// </param>
    /// <param name="Second">
    ///   seconds within the minute
    /// </param>
    /// <param name="IsUTC">
    ///   A boolean to indicate if it is not UTC time and that the given
    ///   localdate time must be converted to UTC
    /// </param>
    procedure SetDateTime(Year, Month, Day: word; Hour: word=0; Minute: word=0; Second: word=0; IsUTC: boolean=true);
    /// <summary>
    ///   function to see if time is not assigned
    /// </summary>
    function IsEmpty: boolean;
    /// <summary>
    ///   Convert the Unix time to a string. Resulting string is always UTC
    /// </summary>
    /// <param name="Fmt">
    ///   The format to be used to create the resulting string
    /// </param>
    function ToString(Fmt: string='yyyy-mm-dd hh:nn:ss'): string;
    /// <summary>
    ///   the Julian year component of the Unix Time
    /// </summary>
    function Year: word;
  end;

  /// <summary>
  ///   A record to represent a UUID, including class operators
  /// </summary>
  /// <remarks>
  ///   This is NOT a GUID, although there are conversion functions available,
  ///   internally it is a UUID
  /// </remarks>
  PdwlUUID = ^TdwlUUID;
  TdwlUUID = record
    data: array[0..15] of byte;
    class operator Equal(const Left, Right: TdwlUUID): Boolean; inline;
    class operator NotEqual(const Left, Right: TdwlUUID): Boolean; inline;
    /// <summary>
    ///   a function returning an empty TdwlUUID
    /// </summary>
    class function Empty: TdwlUUID; static;
    /// <summary>
    ///   constructor to create a new uqnique generated UUID
    /// </summary>
    class function CreateNew: TdwlUUID; static;
    /// <summary>
    ///   constructor to create a UUID based on a UUID string representation
    /// </summary>
    class function CreateFromUUID(const UUID: string): TdwlUUID; static;
    /// <summary>
    ///   Set the value of the UUID using a windows guid representation in a
    ///   TGUID
    /// </summary>
    procedure SetFromGUID(const GUID: TGUID);
    /// <summary>
    ///   set the UUID based on a UUID string representation
    /// </summary>
    procedure SetFromUUID(const UUID: string);
    /// <summary>
    ///   convert the UUID to a TGUID windows style structure
    /// </summary>
    function AsGUID: TGUID;
    /// <summary>
    ///   Get the UUID represented as a string (without brackets)
    /// </summary>
    function AsString: string;
    /// <summary>
    ///   get the UUID formatted as a windows style GUID string including curly
    ///   brackets
    /// </summary>
    function AsGUIDStr: string;
    /// <summary>
    ///   Check is a UUID is assigned
    /// </summary>
    function IsEmpty: boolean;
  end;

  TdwlARGBColor = cardinal;
  TdwlABGRColor = cardinal;

  TdwlDelphiABGRColorRec = record
    constructor Create(AColor: TColor); overload;
    constructor Create(AColor: TdwlARGBColor); overload;
    constructor Create(const AHTMLColor: string); overload;
    function AsHTML: string;
    function AsARGBColor: TdwlARGBCOlor;
    case Cardinal of
      0:
        (Color: TdwlABGRColor);
      1:
        (R, G, B, A: System.Byte);
  end;

  TdwlRestOfTheWorldARGBColorRec = record
    constructor Create(AColor: TColor); overload;
    constructor Create(AColor: TdwlARGBColor); overload;
    constructor Create(const AHTMLColor: string); overload;
    function AsHTML: string;
    function AsABGRColor: TdwlABGRCOlor;
    case Cardinal of
      0:
        (Color: TdwlARGBColor);
      1:
        (B, G, R, A: System.Byte);
  end;

const
  EmptyEpoch: TUnixEpoch = (FEpoch: Low(Int64));
  MinEpoch: TUnixEpoch = (FEpoch: Low(Int64)+1);
  MaxEpoch: TUnixEpoch = (FEpoch: High(Int64));

implementation

uses
  System.DateUtils, System.SysUtils, Winapi.Windows, System.UIConsts;

{ TUnixEpoch }

class operator TUnixEpoch.Add(Epoch: TUnixEpoch; Amount: Int64): TUnixEpoch;
begin
  Result.FEpoch := Epoch.FEpoch+Amount;
end;

constructor TUnixEpoch.Create(ADateTime: TDateTime; IsUTC: boolean=true);
begin
  FEpoch := DateTimeToUnix(ADateTime, IsUTC);
end;

class operator TUnixEpoch.Equal(EpochA, EpochB: TUnixEpoch): boolean;
begin
  Result := EpochA.FEpoch=EpochB.FEpoch;
end;

class operator TUnixEpoch.GreaterThan(EpochA, EpochB: TUnixEpoch): boolean;
begin
  Result := EpochA.FEpoch>EpochB.FEpoch;
end;

class operator TUnixEpoch.GreaterThanOrEqual(EpochA, EpochB: TUnixEpoch): boolean;
begin
  Result := EpochA.FEpoch>=EpochB.FEpoch;
end;

class operator TUnixEpoch.Implicit(Epoch: TUnixEpoch): Int64;
begin
  Result := Epoch.FEpoch;
end;

class operator TUnixEpoch.Implicit(I: Int64): TUnixEpoch;
begin
  Result.FEpoch := I;
end;

class operator TUnixEpoch.Implicit(Epoch: TUnixEpoch): TValue;
begin
  Result := Epoch.FEpoch;
end;

function TUnixEpoch.IsEmpty: boolean;
begin
  Result := FEpoch=EmptyEpoch;
end;

class operator TUnixEpoch.LessThan(EpochA, EpochB: TUnixEpoch): boolean;
begin
  Result := EpochA.FEpoch<EpochB.FEpoch;
end;

class operator TUnixEpoch.LessThanOrEqual(EpochA, EpochB: TUnixEpoch): boolean;
begin
  Result := EpochA.FEpoch<=EpochB.FEpoch;
end;

class operator TUnixEpoch.NotEqual(EpochA, EpochB: TUnixEpoch): boolean;
begin
  Result := EpochA.FEpoch<>EpochB.FEpoch;
end;

class function TUnixEpoch.Now: TUnixEpoch;
begin
  var SystemTime: TSystemTime;
  GetSystemTime(SystemTime);  // SystemTime is in UTC
  Result := DateTimeToUnix(EncodeDateTime(SystemTime.wYear, SystemTime.wMonth, SystemTime.wDay, SystemTime.wHour, SystemTime.wMinute, SystemTime.wSecond, 0));
end;

class operator TUnixEpoch.Subtract(EpochA, EpochB: TUnixEpoch): integer;
begin
  Result := EpochA.FEpoch-EpochB.FEpoch;
end;

procedure TUnixEpoch.SetDateTime(Year, Month, Day: word; Hour: word=0; Minute: word=0; Second: word=0; IsUTC: boolean=true);
begin
  var DTime := EncodeDateTime(Year, Month, Day, Hour, Minute, Second, 0);
  FEpoch := DateTimeToUnix(DTime, IsUTC);
end;

class operator TUnixEpoch.Subtract(Epoch: TUnixEpoch; Amount: Int64): TUnixEpoch;
begin
  Result.FEpoch := Epoch.FEpoch-Amount;
end;

function TUnixEpoch.ToString(Fmt: string='yyyy-mm-dd hh:nn:ss'): string;
begin
  Result := FormatDateTime(Fmt, UnixToDateTime(FEpoch));
end;

function TUnixEpoch.Year: word;
begin
  Result := YearOf(UnixToDateTime(FEpoch));
end;

{ TdwlUUID }

{$WARN SYMBOL_PLATFORM OFF}
function UuidCreate(out guid: TdwlUUID): Longint; stdcall; external 'rpcrt4.dll' name 'UuidCreate' delayed;
{$WARN SYMBOL_PLATFORM ON}

function TdwlUUID.AsGUID: TGUID;
begin
  // see comment in SetFromGUID
  PByteArray(@Result)^[0] := data[3];
  PByteArray(@Result)^[1] := data[2];
  PByteArray(@Result)^[2] := data[1];
  PByteArray(@Result)^[3] := data[0];

  PByteArray(@Result)^[4] := data[5];
  PByteArray(@Result)^[5] := data[4];

  PByteArray(@Result)^[6] := data[7];
  PByteArray(@Result)^[7] := data[6];

  Move(data[8], PByteArray(@Result)[8], 8);
end;

function TdwlUUID.AsGUIDStr: string;
begin
  Result := GUIDToString(AsGUID);
end;

function TdwlUUID.AsString: string;

  procedure PutChars(var P: PByte; var C:PWideChar; Count: byte); inline;
  const
    HexChars: array[0..15] of WideChar = '0123456789abcdef';
  begin
    for var i: byte := 1 to Count do
    begin
      C^ := HexChars[P^ shr 4];
      inc(C);
      C^ := HexChars[P^ and $F];
      inc(C);
      Inc(P);
    end;
  end;

  procedure PutDash(var C:PWideChar); inline;
  begin
    C^ := '-';
    inc(C);
  end;
begin
  SetLength(Result, 36);
  var P := PByte(@Self);
  var C := PWideChar(Result);
  PutChars(P, C, 4);
  PutDash(C);
  PutChars(P, C, 2);
  PutDash(C);
  PutChars(P, C, 2);
  PutDash(C);
  PutChars(P, C, 2);
  PutDash(C);
  PutChars(P, C, 6);
end;

class function TdwlUUID.CreateFromUUID(const UUID: string): TdwlUUID;
begin
  Result.SetFromUUID(UUID);
end;

class function TdwlUUID.CreateNew: TdwlUUID;
begin
  var GUID: TGUID;
  if CreateGUID(GUID)<>S_OK then
    RaiseLastOSError;
  Result.SetFromGUID(GUID);
end;

class function TdwlUUID.Empty: TdwlUUID;
begin
  FillChar(Result, 16, 0);
end;

class operator TdwlUUID.Equal(const Left, Right: TdwlUUID): Boolean;
begin
  Result := (Left = Right);
end;

function TdwlUUID.IsEmpty: boolean;
begin
{$IFDEF CPU64BITS}
  var a := PInt64Array(@data);
  Result := (a^[0]=0) and (a^[1]=0);
{$ELSE !CPU64BITS}
  var a := PIntegerArray(@data);
  Result := (a^[0]=0) and (a^[1]=0) and (a^[2]=0) and (a^[3]=0);
{$ENDIF !CPU64BITS}
end;

class operator TdwlUUID.NotEqual(const Left, Right: TdwlUUID): Boolean;
begin
  Result := not (Left = Right);
end;

procedure TdwlUUID.SetFromGUID(const GUID: TGUID);
begin
  // our UUID is implemented as variant 1, f.e. compatible with MySQL, big-endian byte order
  // Microsoft guids are implemented as variant 2, this is a mixed-endian,
  // having the first three components stored in little-endian, the rest in big-endian
  //
  // see also: https://en.wikipedia.org/wiki/Universally_unique_identifier
  // This results in a quite inefficient Byte move that we need to do :-{

  data[0] := PByteArray(@GUID)^[3];
  data[1] := PByteArray(@GUID)^[2];
  data[2] := PByteArray(@GUID)^[1];
  data[3] := PByteArray(@GUID)^[0];

  data[4] := PByteArray(@GUID)^[5];
  data[5] := PByteArray(@GUID)^[4];

  data[6] := PByteArray(@GUID)^[7];
  data[7] := PByteArray(@GUID)^[6];

  Move(PByteArray(@GUID)^[8], data[8], 8);
end;

procedure TdwlUUID.SetFromUUID(const UUID: string);
const
  HexChars: array['0'..'f'] of byte =
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, {0-9}
     0, 0, 0, 0, 0, 0, 0, {:-@}
     10, 11, 12, 13, 14, 15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, {A-Z}
     0, 0, 0, 0, 0, 0, {[-`}
     10, 11, 12, 13, 14, 15); {a-f}
begin
  try
    if UUID.Length<>36 then
      raise Exception.Create('Wrong Length');
    var C1 := PWideChar(UUID);
    var C2 := C1;
    inc(C2);
    data[0] := (HexChars[C1^] shl 4)+HexChars[C2^];
    inc(C1, 2); inc(C2, 2);
    data[1] := (HexChars[C1^] shl 4)+HexChars[C2^];
    inc(C1, 2); inc(C2, 2);
    data[2] := (HexChars[C1^] shl 4)+HexChars[C2^];
    inc(C1, 2); inc(C2, 2);
    data[3] := (HexChars[C1^] shl 4)+HexChars[C2^];
    inc(C1, 3); inc(C2, 3);

    data[4] := (HexChars[C1^] shl 4)+HexChars[C2^];
    inc(C1, 2); inc(C2, 2);
    data[5] := (HexChars[C1^] shl 4)+HexChars[C2^];
    inc(C1, 3); inc(C2, 3);

    data[6] := (HexChars[C1^] shl 4)+HexChars[C2^];
    inc(C1, 2); inc(C2, 2);
    data[7] := (HexChars[C1^] shl 4)+HexChars[C2^];
    inc(C1, 3); inc(C2, 3);

    data[8] := (HexChars[C1^] shl 4)+HexChars[C2^];
    inc(C1, 2); inc(C2, 2);
    data[9] := (HexChars[C1^] shl 4)+HexChars[C2^];
    inc(C1, 3); inc(C2, 3);

    data[10] := (HexChars[C1^] shl 4)+HexChars[C2^];
    inc(C1, 2); inc(C2, 2);
    data[11] := (HexChars[C1^] shl 4)+HexChars[C2^];
    inc(C1, 2); inc(C2, 2);
    data[12] := (HexChars[C1^] shl 4)+HexChars[C2^];
    inc(C1, 2); inc(C2, 2);
    data[13] := (HexChars[C1^] shl 4)+HexChars[C2^];
    inc(C1, 2); inc(C2, 2);
    data[14] := (HexChars[C1^] shl 4)+HexChars[C2^];
    inc(C1, 2); inc(C2, 2);
    data[15] := (HexChars[C1^] shl 4)+HexChars[C2^];
  except
    Self := Empty;
  end;
end;

{ TdwlDelphiABGRColorRec }

constructor TdwlDelphiABGRColorRec.Create(AColor: TdwlARGBColor);
begin
  R := TdwlRestOfTheWorldARGBColorRec(AColor).R;
  G := TdwlRestOfTheWorldARGBColorRec(AColor).G;
  B := TdwlRestOfTheWorldARGBColorRec(AColor).B;
  A := TdwlRestOfTheWorldARGBColorRec(AColor).A;
end;

constructor TdwlDelphiABGRColorRec.Create(AColor: TColor);
begin
  Color := TColorRec.ColorToRGB(AColor);
end;

function TdwlDelphiABGRColorRec.AsARGBColor: TdwlARGBCOlor;
begin
  Result := TdwlRestOfTheWorldARGBColorRec.Create(Color).Color;
end;

function TdwlDelphiABGRColorRec.AsHTML: string;
begin
  Result := '#' + IntToHex(R, 2)+IntToHex(G, 2)+IntToHex(B, 2);
end;

constructor TdwlDelphiABGRColorRec.Create(const AHTMLColor: string);
begin
  Color := StringToColor('$' + Copy(AHTMLColor, 6, 2) + Copy(AHTMLColor, 4, 2) + Copy(AHTMLColor, 2, 2));
end;

{ TdwlRestOfTheWorldARGBColorRec }

constructor TdwlRestOfTheWorldARGBColorRec.Create(AColor: TColor);
begin
  AColor := TColorRec.ColorToRGB(AColor);
  R := TdwlDelphiABGRColorRec(AColor).R;
  G := TdwlDelphiABGRColorRec(AColor).G;
  B := TdwlDelphiABGRColorRec(AColor).B;
  A := TdwlDelphiABGRColorRec(AColor).A;
end;

constructor TdwlRestOfTheWorldARGBColorRec.Create(AColor: TdwlARGBColor);
begin
  Color := AColor;
end;

function TdwlRestOfTheWorldARGBColorRec.AsABGRColor: TdwlABGRCOlor;
begin
  Result := TdwlDelphiABGRColorRec.Create(Color).Color;
end;

function TdwlRestOfTheWorldARGBColorRec.AsHTML: string;
begin
  Result := '#' + IntToHex(R, 2)+IntToHex(G, 2)+IntToHex(B, 2);
end;

constructor TdwlRestOfTheWorldARGBColorRec.Create(const AHTMLColor: string);
begin
  Create(StringToColor('$' + Copy(AHTMLColor, 6, 2) + Copy(AHTMLColor, 4, 2) + Copy(AHTMLColor, 2, 2)));
end;



end.
