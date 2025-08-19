unit DWL.Types;

interface

uses
  System.Rtti, System.UITypes;

type
  TPeriodicity_Implementation=class;
  TPeriodicity = class of TPeriodicity_Implementation;

  /// <summary>
  ///   A record including class operators to represent a Unix Time.
  /// </summary>
  TUnixEpoch = record
  strict private
    FEpoch: Int64;
  public
    Epoch: Int64;
    class function Now(ReturnStartOfTheDay: boolean=false): TUnixEpoch;  static;
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
    ///   Convert the Unix time to a datetime. Resulting string is UTC,
    ///   unless explicitly called with AReturnUTC=false
    /// </summary>
    /// <param name="AReturnUTC">
    ///   If false result will be in local datetime
    /// </param>
    function ToDateTime(AReturnUTC: boolean=true): TDateTime;
    /// <summary>
    ///   the Julian year component of the Unix Time, This is UTC!
    /// </summary>
    function Year: word;
    procedure AddPeriod(APeriodicity: TPeriodicity; Amount: integer=1);
  end;

  TPeriodicity_Implementation = class abstract
  public
    class function AddPeriod(Epoch: TUnixEpoch; Amount: integer): TUnixEpoch; virtual; abstract;
    class function EndOfThePeriod(Epoch: TUnixEpoch): TUnixEpoch; virtual; abstract;
    class function StartOfThePeriod(Epoch: TUnixEpoch): TUnixEpoch; virtual; abstract;
  end;

  TPeriodicity_Daily = class abstract(TPeriodicity_Implementation)
  public
    class function AddPeriod(Epoch: TUnixEpoch; Amount: integer): TUnixEpoch; override;
    class function EndOfThePeriod(Epoch: TUnixEpoch): TUnixEpoch; override;
    class function StartOfThePeriod(Epoch: TUnixEpoch): TUnixEpoch; override;
  end;

  TPeriodicity_Dekadal = class abstract(TPeriodicity_Implementation)
  public
    class function AddPeriod(Epoch: TUnixEpoch; Amount: integer=1): TUnixEpoch; override;
    class function EndOfThePeriod(Epoch: TUnixEpoch): TUnixEpoch; override;
    class function StartOfThePeriod(Epoch: TUnixEpoch): TUnixEpoch; override;
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

  TdwlColor = record
    constructor CreateFromTColor(Color: TColor);
    constructor CreateFromHex(const Hex: string);
    function AsHex(IncludeAlpha: boolean=true; LeadChar: WideChar=#0): string;
    function AsTColor: TColor;
    function AsRGB: cardinal;
  case cardinal of
    0:
      (ARGB: cardinal);
    1:
     (B, G, R, A: byte);
  end;

  PdwlBounds = ^TdwlBounds;
  TdwlBounds = packed record
    XMin,
    XMax,
    YMin,
    YMax: double;
    function IsEmpty: boolean;
    function XExtent: double;
    function YExtent: double;
    procedure Update(BoundsToInclude: TdwlBounds);
  end;

  PdwlGridDataType = ^TdwlGridDataType;
  TdwlGridDataType = packed record // do not change this, is f.e. used in DDF version 4 Files
  const
    Sizes: array[0..129] of byte = (1, 1, 2, 2, 4, 4, 8, 8,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      4, 8);
    var
      DataType: word;
      NoDataValue: Int64;
      ValueScale: double;
      ValueOffset: double;
      DataTypeFlags: byte;
    class function Create(DataType: word; Flags: byte=0; NoDataValue: Int64=0; ValueScale: double=1; ValueOffset: double=0): TdwlGridDataType; static;
    function HighestValue: Int64;
    function Size: byte;
  end;

  TdwlGridDim = packed record // do not change this, is f.e. used in DDF version 4 Files
    WidthInPixels: word;
    HeightInPixels: word;
    LeftWorldX: double;
    TopWorldY: double;
    ScaleGridToWorld: double;
    function Grid2WorldX(X: double): double;
    function Grid2WorldY(Y: double): double;
    function World2GridX(X: double): double;
    function World2GridY(Y: double): double;
    function OuterWorldBounds: TdwlBounds;
    class function Create(AWidthInPixels, AHeightInPixels: word; ALeftWorldX, ATopWorldY, AScaleGridToWorld: double): TdwlGridDim; static;
  end;

  PPbyte = ^PByte;

const
  UnixEpoch_Empty: TUnixEpoch = (FEpoch: Low(Int64));
  UnixEpoch_Min: TUnixEpoch = (FEpoch: Low(Int64)+1);
  UnixEpoch_Max: TUnixEpoch = (FEpoch: High(Int64));

const
  // DataTypes
  dwlInt8 = 0;
  dwlUInt8 = 1;
  dwlInt16 = 2;
  dwlUInt16 = 3;
  dwlInt32 = 4;
  dwlUInt32 = 5;
  dwlInt64 = 6;
  dwlUInt64 = 7;
  dwlSingle = 128;
  dwlDouble = 129;

const
  flagNoDataValueUsed = 1;
  flagValueScalingUsed = 2;
  flagValueOffsetUsed = 4;

const
  Bounds_Empty: TdwlBounds=(XMin: 1e+100; XMax: -1e+100; YMin: 1e+100; YMax: -1e+100);

implementation

uses
  System.DateUtils, System.SysUtils, Winapi.Windows, System.UIConsts,
  System.Math;

{ TUnixEpoch }

class operator TUnixEpoch.Add(Epoch: TUnixEpoch; Amount: Int64): TUnixEpoch;
begin
  Result.FEpoch := Epoch.FEpoch+Amount;
end;

procedure TUnixEpoch.AddPeriod(APeriodicity: TPeriodicity; Amount: integer=1);
begin
  Self := APeriodicity.AddPeriod(Self, Amount);
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
  Result := FEpoch=UnixEpoch_Empty;
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

class function TUnixEpoch.Now(ReturnStartOfTheDay: boolean=false): TUnixEpoch;
begin
  var SystemTime: TSystemTime;
  GetSystemTime(SystemTime);  // SystemTime is in UTC
  if ReturnStartOfTheDay then
    Result := DateTimeToUnix(EncodeDateTime(SystemTime.wYear, SystemTime.wMonth, SystemTime.wDay, 0, 0, 0, 0))
  else
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

function TUnixEpoch.ToDateTime(AReturnUTC: boolean=true): TDateTime;
begin
  Result := UnixToDateTime(FEpoch, AReturnUTC);
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

{ TPeriodicity_Daily }

class function TPeriodicity_Daily.AddPeriod(Epoch: TUnixEpoch; Amount: integer): TUnixEpoch;
begin
  Result := Epoch+Amount*86400;
end;

class function TPeriodicity_Daily.EndOfThePeriod(Epoch: TUnixEpoch): TUnixEpoch;
begin
  Result := StartOfThePeriod(Epoch)+86399;
end;

class function TPeriodicity_Daily.StartOfThePeriod(Epoch: TUnixEpoch): TUnixEpoch;
begin
  // simply return the start of the day
  Result := TUnixEpoch.Create(trunc(Epoch.ToDateTime));
end;

{ TPeriodicity_Dekadal }

class function TPeriodicity_Dekadal.AddPeriod(Epoch: TUnixEpoch; Amount: integer=1): TUnixEpoch;
begin
  // First decode the Epoch to y,m,d and DekInYear
  var y, m, d: word;
  var Input := Epoch.ToDateTime;
  DecodeDate(Input, y, m, d);
  var DekInYear := (m-1)*3+Min(3,((d+9) div 10));
  // Keep the seconds offset from the start of the dekad
  var SecsOffset := round((Input-EncodeDate(y, m,  (DekInYear-(m-1)*3)*10-9))*SecsPerDay);
  // do the actual increase
  inc(DekInYear, Amount);
  // noermalize values
  while DekInYear<1 do
  begin
    Inc(DekInYear, 36);
    Dec(y);
  end;
  while DekInYear>36 do
  begin
    Dec(DekInYear, 36);
    Inc(y);
  end;
  m := (DekInYear+2) div 3;
  d := (DekInYear-(m-1)*3)*10-9;
  // assemble the result
  var Start := TUnixEpoch.Create(EncodeDate(y, m, d));
  Result := Start+SecsOffset;
  // not all dekads have the same length, maximize on the last second
  Result := Min(Result, EndOfThePeriod(Start));
end;

class function TPeriodicity_Dekadal.EndOfThePeriod(Epoch: TUnixEpoch): TUnixEpoch;
begin
  // First decode the Epoch to y,m,d and DekInYear
  var y, m, d: word;
  var Input := Epoch.ToDateTime;
  DecodeDate(Input, y, m, d);
  var DekInYear := (m-1)*3+Min(3,((d+9) div 10));
  // go to next dekad
  inc(DekInYear);
  // normalize
  if DekInYear=37 then
  begin
    inc(y);
    DekInYear:= 1;
  end;
  // assemble the Reusult
  m := (DekInYear+2) div 3;
  d := (DekInYear-(m-1)*3)*10-9;
  Result := TUnixEpoch.Create(EncodeDate(y, m, d));
  // and report back the second before the start of the next dekad
  Result := Result-1;
end;

class function TPeriodicity_Dekadal.StartOfThePeriod(Epoch: TUnixEpoch): TUnixEpoch;
begin
  // First decode the Epoch to y,m,d and DekOffSet
  var y, m, d: word;
  var Input := Epoch.ToDateTime;
  DecodeDate(Input, y, m, d);
  var DekOffsetInMonth := Min(3,((d+9) div 10))-1;
  // And Assemble the result
  Result := TUnixEpoch.Create(EncodeDate(y, m, 1));
  Result := Result++DekOffsetInMonth*864000;
end;

{ TdwlColor }

function TdwlColor.AsHex(IncludeAlpha: boolean=true; LeadChar: WideChar=#0): string;
begin
  if LeadChar=#0 then
    Result := ''
  else
    Result := LeadChar;
  if IncludeAlpha then
    Result := Result+IntToHex(ARGB, 8).ToLower
  else
    Result := Result+IntToHex(ARGB and $FFFFFF, 6).ToLower;
end;

function TdwlColor.AsRGB: cardinal;
begin
  Result := ARGB and $ffffff;
end;

function TdwlColor.AsTColor: TColor;
begin
  Result := B;
  Result := Result shl 8 + G;
  Result := Result shl 8 + R;
end;

constructor TdwlColor.CreateFromHex(const Hex: string);
begin
  var lHex := Hex;
  while (lHex<>'') and not CharInSet(lHex[1], ['0'..'9', 'A'..'F', 'a'..'f']) do
    lHex := Copy(lHex, 2, MaxInt);
  ARGB := StrToUInt('$'+lHex);
  if length(lHex)<7 then
    A := 255;
end;

constructor TdwlColor.CreateFromTColor(Color: TColor);
begin
  A :=  255;
  R := Color and $0000FF;
  G := (Color and $00FF00) shr 8;
  B := (Color and $FF0000) shr 16;
end;

{ TdwlBounds }

function TdwlBounds.IsEmpty: boolean;
begin
  Result := (XMin>XMax) or (YMin>YMax);
end;

procedure TdwlBounds.Update(BoundsToInclude: TdwlBounds);
begin
  if IsEmpty then
    Self := BoundsToInclude
  else
  begin
    if BoundsToInclude.XMin < XMin then
      XMin := BoundsToInclude.XMin;
    if BoundsToInclude.XMax > XMax then
      XMax := BoundsToInclude.XMax;
    if BoundsToInclude.YMin < YMin then
      YMin := BoundsToInclude.YMin;
    if BoundsToInclude.YMax > YMax then
      YMax := BoundsToInclude.YMax;
  end;
end;

function TdwlBounds.XExtent: double;
begin
  Result := XMax-XMin;
end;

function TdwlBounds.YExtent: double;
begin
  Result := YMax-YMin;
end;

{ TdwlGridDataType }

function TdwlGridDataType.Size: byte;
begin
  Result := Sizes[DataType];
end;

class function TdwlGridDataType.Create(DataType: word; Flags: byte=0; NoDataValue: Int64=0; ValueScale: double=1; ValueOffset: double=0): TdwlGridDataType;
begin
  Result.DataType := DataType;
  Result.NoDataValue := NoDataValue;
  Result.ValueScale := ValueScale;
  Result.ValueOffset := ValueOffset;
  Result.DataTypeFlags := Flags;
end;

function TdwlGridDataType.HighestValue: Int64;
begin
  case DataType of
    dwlInt8: Result := High(Int8);
    dwlUInt8: Result := High(UInt8);
    dwlInt16: Result := High(Int16);
    dwlUInt16: Result := High(UInt16);
    dwlInt32: Result := High(Int32);
    dwlUInt32: Result := High(UInt32);
  else
    Result := High(Int64);
  end;
end;

{ TdwlGridDim }

class function TdwlGridDim.Create(AWidthInPixels, AHeightInPixels: word; ALeftWorldX, ATopWorldY, AScaleGridToWorld: double): TdwlGridDim;
begin
  Result.WidthInPixels := AWidthInPixels;
  Result.HeightInPixels := AHeightInPixels;
  Result.LeftWorldX := ALeftWorldX;
  Result.TopWorldY := ATopWorldY;
  Result.ScaleGridToWorld := AScaleGridToWorld;
end;

function TdwlGridDim.Grid2WorldX(X: double): double;
begin
  Result := LeftWorldX+X*ScaleGridToWorld;
end;

function TdwlGridDim.Grid2WorldY(Y: double): double;
begin
  Result := TopWorldY-Y*ScaleGridToWorld;
end;

function TdwlGridDim.OuterWorldBounds: TdwlBounds;
begin
  Result.XMin := Grid2WorldX(-0.5);
  Result.XMax := Grid2WorldX(WidthInPixels-0.5);
  Result.YMin := Grid2WorldY(HeightInPixels-0.5);
  Result.YMax := Grid2WorldY(-0.5);
end;

function TdwlGridDim.World2GridX(X: double): double;
begin
  Result := (X-LeftWorldX)/ScaleGridToWorld;
end;

function TdwlGridDim.World2GridY(Y: double): double;
begin
  Result := (TopWorldY-Y)/ScaleGridToWorld;
end;

end.

