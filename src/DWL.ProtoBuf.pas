unit DWL.ProtoBuf;

interface

uses
  DWL.IO;

const
  wiretypeLengthDelimited = 2;

type
  IdwlProtoValue=interface;
  
  IdwlProtoValueEnumerator = interface
    function MoveNext: boolean;
    function Value: IdwlProtoValue;
  end;

  IdwlProtoValue = interface
    function AsDouble: double;
    function AsInteger: Int64;
    function ValueByFieldNumber(FieldNumber: integer): IdwlProtoValue;
    function FieldNumber: integer;
    function ValueEnumerator(FieldNumber: integer): IdwlProtoValueEnumerator;
  end;

  IdwlProtoBuf = interface
    function GetRootValue: IdwlProtoValue;
  end;

function New_ProtoBuf(CursoredIO: IdwlCursoredIO): IdwlProtoBuf;

implementation

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  TdwlProtoValue = class(TInterfacedObject, IdwlProtoValue)
  strict private
    FProtoBuf: IdwlProtoBuf;
    FBuf: PByte;
    FBufSize: cardinal;
    FWireType: byte;
    FFieldNumber: integer;
    function ReadVarInt(var Buf: PByte): Int64;
    procedure Initialize;
  private
    FValues: TList<IdwlProtoValue>;
    function AsDouble: double;
    function AsInteger: Int64;
    function ValueByFieldNumber(FieldNumber: integer): IdwlProtoValue;
    function FieldNumber: integer;
    function ValueEnumerator(FieldNumber: integer): IdwlProtoValueEnumerator;
  public
    constructor Create(ProtoBuf: IdwlProtoBuf; Buf: PByte; BufSize: cardinal; WireType: byte; FieldNumber: integer);
    destructor Destroy; override;
  end;

  TdwlProtoBuf = class(TInterfacedObject, IdwlProtoBuf)
  strict private
    FCursor: IdwlCursor_Read;
    FBuf: PByte;
  private
    function GetRootValue: IdwlProtoValue;
  public
    constructor Create(CursoredIO: IdwlCursoredIO);
  end;

  TdwlProtoValueEnumerator = class(TInterfacedObject, IdwlProtoValueEnumerator)
  strict private
    FProtoValue: IdwlProtoValue;
    FFieldNumber: integer;
    FCurrOffset: integer;
  private
    function MoveNext: boolean;
    function Value: IdwlProtoValue;
  public
    constructor Create(AProtoValue: IdwlProtoValue; FieldNumber: integer);
  end;

function New_ProtoBuf(CursoredIO: IdwlCursoredIO): IdwlProtoBuf;
begin
  Result := TdwlProtoBuf.Create(CursoredIO);
end;

{ TdwlProtoBuf }

constructor TdwlProtoBuf.Create(CursoredIO: IdwlCursoredIO);
begin
  inherited Create;
  FCursor := CursoredIO.GetReadCursor;
  FCursor.Seek(0);
  FBuf := FCursor.CursorPtr;
end;

function TdwlProtoBuf.GetRootValue: IdwlProtoValue;
begin
  Result := TdwlProtoValue.Create(Self, FBuf, FCursor.Size, wiretypeLengthDelimited, -1);
end;

{ TdwlProtoValue }

function TdwlProtoValue.AsDouble: double;
begin
  if FWireType<>1 then
    raise Exception.Create('Double wiretype error');
  Result := PDouble(FBuf)^;
end;

function TdwlProtoValue.AsInteger: Int64;
begin
  var B := FBuf;
  Result := ReadVarInt(B);
end;

constructor TdwlProtoValue.Create(ProtoBuf: IdwlProtoBuf; Buf: PByte; BufSize: cardinal; WireType: byte; FieldNumber: integer);
begin
  inherited Create;
  FProtoBuf := ProtoBuf;
  FBuf := Buf;
  FBufSize := BufSize;
  FWireType := WireType;
  FFieldNumber := FieldNumber;
end;

destructor TdwlProtoValue.Destroy;
begin
  FValues.Free;
  inherited Destroy;
end;

function TdwlProtoValue.FieldNumber: integer;
begin
  Result := FFieldNumber;
end;

procedure TdwlProtoValue.Initialize;
begin
  if FValues=nil then
    FValues := TList<IdwlProtoValue>.Create;
  var Eof := FBuf+FBufSize;
  var ReadBuf := FBuf;
  while ReadBuf<Eof do
  begin
    var Tag := ReadVarInt(ReadBuf);
    var WireType := Tag and %111;
    var FieldNumber: integer := Tag shr 3;
    case WireType of
    0:
      begin // varint
        FValues.Add(TdwlProtoValue.Create(FProtoBuf, ReadBuf, 0, WireType, FieldNumber));
        ReadVarInt(ReadBuf);
      end;
    1:
      begin // 64bit (fixed64)
        FValues.Add(TdwlProtoValue.Create(FProtoBuf, ReadBuf, 0, WireType, FIeldNumber));
        inc(ReadBuf, 8);
      end;
    2:
      begin  // length delimitedfield
        var Len := ReadVarInt(ReadBuf);
        FValues.Add(TdwlProtoValue.Create(FProtoBuf, ReadBuf, Len, WireType, FieldNumber));
        inc(ReadBuf, Len);
      end;
//    3:
//      begin  // start group: deprecated
//        inc(ReadBuf, 4);
//      end;
//    4:
//      begin  // end group: deprecated
//        inc(ReadBuf, 4);
//      end;
    5:
      begin // 32bit (fixed32)
        FValues.Add(TdwlProtoValue.Create(FProtoBuf, ReadBuf, 0, WireType, FieldNumber));
        inc(ReadBuf, 4);
      end;
    6:
      begin // 32bit (fixed32)
        FValues.Add(TdwlProtoValue.Create(FProtoBuf, ReadBuf, 0, WireType, FieldNumber));
        inc(ReadBuf, 4);
      end;
    else
      raise Exception.Create('Implement WireType '+WireType.ToString);
    end;
  end;
end;

function TdwlProtoValue.ReadVarInt(var Buf: PByte): Int64;
begin
  Result := 0;
  var B: byte;
  var ShiftAmount := -7;
  repeat
    B := PByte(Buf)^;
    inc(Buf);
    inc(ShiftAmount, 7);
    Result := Result+((B and %01111111) shl ShiftAmount);
  until (B and %10000000)=0;
end;

function TdwlProtoValue.ValueByFieldNumber(FieldNumber: integer): IdwlProtoValue;
begin
  Initialize;
  for var i := 0 to FValues.Count-1 do
    if FValues[i].FieldNumber=FieldNumber then
      Exit(FValues[i]);
  Result := nil;
end;

function TdwlProtoValue.ValueEnumerator(FieldNumber: integer): IdwlProtoValueEnumerator;
begin
  Initialize;
  Result := TdwlProtoValueEnumerator.Create(Self, FieldNumber);
end;

{ TdwlProtoValueEnumerator }

constructor TdwlProtoValueEnumerator.Create(AProtoValue: IdwlProtoValue; FieldNumber: integer);
begin
  inherited Create;
  FProtoValue := AProtoValue;
  FCurrOffset := -1;
  FFieldNumber := FieldNumber;
end;

function TdwlProtoValueEnumerator.MoveNext: boolean;
begin
  Result := false;
  inc(FCurrOffset);
  while FCurrOffset<TdwlProtoValue(FProtoValue).FValues.Count do
  begin
    var Value := TdwlProtoValue(FProtoValue).FValues[FCurrOffset];
    if (FFieldNumber=-1) or (fFieldNumber=Value.FieldNumber) then
      Exit(true);
    inc(FCurrOffset);
  end;
end;

function TdwlProtoValueEnumerator.Value: IdwlProtoValue;
begin
  if (FCurrOffset<0) or (FCurrOffset>=TdwlProtoValue(FProtoValue).FValues.Count) then
    Exit(nil);
  Result := TdwlProtoValue(FProtoValue).FValues[FCurrOffset];
end;

end.
