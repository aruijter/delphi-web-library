unit DWL.FlatBuf;

interface

uses
  DWL.IO;

type
  TflatValue = record
  private
    Buf: PByte;
    Offset: cardinal;
  public
    function AsPByte(var Size: cardinal): PByte;
    function AsString: string;
    function AsTable: TflatValue;
    function AsUInt8: byte;
    function AsUInt32: cardinal;
    function AsUInt64: UInt64;
    function GetTableValue(FieldNo: byte): TflatValue;
 end;

  IdwlFlatBuf = interface
    function GetRootValue: TflatValue;
  end;

function New_FlatBuf(CursoredIO: IdwlCursoredIO): IdwlFlatBuf;

implementation

uses
  System.SysUtils, Vcl.Dialogs, Winapi.Windows;

type
  TdwlFlatBuf = class(TInterfacedObject, IdwlFlatBuf)
  strict private
    FCursor: IdwlCursor_Read;
    FBuf: PByte;
  private
    function GetRootValue: TflatValue;
  public
    constructor Create(CursoredIO: IdwlCursoredIO);
  end;

function New_FlatBuf(CursoredIO: IdwlCursoredIO): IdwlFlatBuf;
begin
  Result := TdwlFlatBuf.Create(CursoredIO);
end;

{ TdwlFlatBuf }

constructor TdwlFlatBuf.Create(CursoredIO: IdwlCursoredIO);
begin
  inherited Create;
  FCursor := CursoredIO.GetReadCursor;
  FCursor.Seek(0);
  FBuf := FCursor.CursorPtr;
end;

function TdwlFlatBuf.GetRootValue: TflatValue;
begin
  if FCursor.Size=0 then
  begin
    Result.Buf := nil;
    Result.Offset := 0;
  end
  else
  begin
    Result.Buf := FBuf;
    Result.Offset := PCardinal(FBuf)^;
  end;
end;

{ TflatValue }

function TflatValue.AsPByte(var Size: cardinal): PByte;
begin
  Result := Buf+Offset;
  // jump to buffer definition
  inc(Result, PCardinal(Result)^);
  Size := PCardinal(Result)^;
  inc(Result, 4);
end;

function TflatValue.AsString: string;
begin
  var BufPtr := Buf+Offset;
  // jump to string definition
  inc(BufPtr, PCardinal(BufPtr)^);

  var StrLen := MultiByteToWideChar(CP_UTF8, 0, PAnsiChar(BufPtr+4), PCardinal(BufPtr)^, nil, 0);
  SetLength(Result, StrLen);
  MultiByteToWideChar(CP_UTF8, 0, PAnsiChar(BufPtr+4), PCardinal(BufPtr)^, PWideChar(Result), StrLen);
end;

function TflatValue.AsTable: TflatValue;
begin
  Result.Buf := Buf;
  Result.Offset := Offset+PCardinal(Buf+Offset)^;
end;

function TflatValue.AsUInt32: cardinal;
begin
  Result := PCardinal(Buf+Offset)^;
end;

function TflatValue.AsUInt64: UInt64;
begin
  Result := PUInt64(Buf+Offset)^;
end;

function TflatValue.AsUInt8: byte;
begin
  Result := PByte(Buf+Offset)^;
end;

function TflatValue.GetTableValue(FieldNo: byte): TflatValue;
begin
  Result.Buf := Buf;
  Result.Offset := Offset+PWord(Buf+Offset-PInteger(Buf+Offset)^+4+FieldNo*2)^;
end;

end.
