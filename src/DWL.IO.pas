unit DWL.IO;

interface

uses
  System.Classes, System.SysUtils, Winapi.Windows;

type
  TdwlFileOption = (pfoCreateIfNeeded, pfoCreateEmptyNew, pfoReadOnly);
  TdwlFileOptions = set of TdwlFileOption;

  IdwlCursor_Read = interface
    function CRC32: UInt32;
    function Eof: Boolean;
    procedure Read(var Buf; Count: UInt64);
    function ReadBoolean: boolean;
    function ReadBytes(const Size: UInt32): TBytes;
    function ReadDouble: double;
    function ReadInt8: shortint;
    function ReadInt64: Int64;
    function ReadString_LenByte: string;
    function ReadString_LenCardinal: string;
    function ReadUInt8: byte;
    function ReadUInt16: word;
    function ReadUInt32: cardinal;
    function ReadUInt64: UInt64;
    procedure Seek(Offset: Int64; Origin: TSeekOrigin);
    function GetSize: UInt64;
    function CursorOffset: UInt64;
    function CursorPtr: PByte;
  end;

  IdwlCursor_Write = interface(IdwlCursor_Read)
    procedure Fill(Value: byte; Count: UInt64);
    procedure Flush;
    procedure SetSize(NewSize: UInt64);
    procedure Write(const Buf; const Count: UInt64); overload;
    procedure WriteBoolean(Value: boolean);
    procedure WriteDouble(Value: double);
    procedure WriteInt8(Value: shortint);
    procedure WriteInt64(Value: Int64);
    procedure WriteString_LenByte(const S: string; KnownMultiByteLength: UInt8=0);
    procedure WriteString_LenCardinal(const S: string; KnownMultiByteLength: UInt32=0);
    procedure WriteUInt8(Value: byte);
    procedure WriteUInt16(Value: word);
    procedure WriteUInt32(Value: cardinal);
    procedure WriteUInt64(Value: UInt64);
  end;

  IdwlCursoredIO = interface
    function GetReadCursor: IdwlCursor_Read;
    function GetWriteCursor: IdwlCursor_Write;
  end;

function New_CursoredIO(const FileName: string; Options: TdwlFileOptions=[]; CodePage: UINT=CP_UTF8; GrowSize: cardinal=524288): IdwlCursoredIO;

implementation

uses
 System.SyncObjs, DWL.Crypt, System.Math, System.Generics.Collections;

type
  TdwlCursor = class;

  TdwlCursoredIO = class(TInterfacedObject, IdwlCursoredIO)
  strict private
    FCursors: TList<TdwlCursor>;
  private
    FCodePage: UINT;
    FMemory: PByte;
    FActuallyUsedMemorySize: ULARGE_INTEGER;
    FAllocatedMemorySize: ULARGE_INTEGER;
    FSynchronizer: TLightweightMREW;
    procedure RegisterCursor(Cursor: TdwlCursor);
    procedure UnRegisterCursor(Cursor: TdwlCursor);
    procedure AssureMemory(const RequestedSize: UInt64);
    function GetReadCursor: IdwlCursor_Read;
    function GetWriteCursor: IdwlCursor_Write;
  protected
    function ExtendMemory(const NeededFileSize: UInt64): Int64; virtual; abstract;
    procedure Flush; virtual;
  public
    constructor Create(CodePage: UINT);
    destructor Destroy; override;
  end;

  TdwlCursoredFile = class(TdwlCursoredIO)
  strict private
    FFileHandle: THandle;
    FGrowSize: cardinal;
    FIsReadOnly: boolean;
    FMapHandle: THandle;
    FOptions: TdwlFileOptions;
    procedure InternalClose;
    procedure InternalOpen(const FileName: string);
  protected
    function ExtendMemory(const NeededFileSize: UInt64): Int64; override;
    procedure Flush; override;
  public
    constructor Create(const FileName: string; Options: TdwlFileOptions; CodePage: UINT; GrowSize: cardinal);
    destructor Destroy; override;
  end;

  TdwlCursor = class(TInterfacedObject, IdwlCursor_Read, IdwlCursor_Write)
  strict private
    FWritable: boolean;
    procedure AssureMemory(const RequestedSize: UInt64);
  private
    FCursoredMemory: TdwlCursoredIO;
    FCursor: PByte;
    FCodePage: UINT;
    function CRC32: UInt32;
    function CursorOffset: UInt64;
    function CursorPtr: PByte;
    function Eof: Boolean;
    procedure Fill(Value: byte; Count: UInt64);
    procedure Flush;
    function GetSize: UInt64;
    procedure Read(var Buf; Count: UInt64);
    function ReadBoolean: boolean;
    function ReadBytes(const Size: UInt32): TBytes;
    function ReadDouble: double;
    function ReadInt8: shortint;
    function ReadInt64: Int64;
    function ReadString_LenByte: string;
    function ReadString_LenCardinal: string;
    function ReadUInt8: byte;
    function ReadUInt16: word;
    function ReadUInt32: cardinal;
    function ReadUInt64: UInt64;
    procedure Seek(Offset: Int64; Origin: TSeekOrigin);
    procedure SetSize(NewSize: UInt64);
    procedure Write(const Buf; const Count: UInt64);
    procedure WriteBoolean(Value: boolean);
    procedure WriteDouble(Value: double);
    procedure WriteInt8(Value: shortint);
    procedure WriteInt64(Value: Int64);
    procedure WriteString_LenByte(const S: string; KnownMultiByteLength: UInt8=0);
    procedure WriteString_LenCardinal(const S: string; KnownMultiByteLength: UInt32=0);
    procedure WriteUInt8(Value: byte);
    procedure WriteUInt16(Value: word);
    procedure WriteUInt32(Value: cardinal);
    procedure WriteUInt64(Value: UInt64);
    procedure ReAllocationOccured(Offset: Int64);
  public
    constructor Create(ACursoredMemory: TdwlCursoredIO; Writable: boolean);
    destructor Destroy; override;
  end;

function New_CursoredIO(const FileName: string; Options: TdwlFileOptions=[]; CodePage: UINT=CP_UTF8; GrowSize: cardinal=524288): IdwlCursoredIO;
begin
  Result := TdwlCursoredFile.Create(FileName, Options, CodePage, GrowSize);
end;

{ TdwlCursoredFile }

constructor TdwlCursoredFile.Create(const FileName: string; Options: TdwlFileOptions; CodePage: UINT; GrowSize: cardinal);
begin
  inherited Create(CodePage);
  FGrowSize := GrowSize;
  FOptions := Options;
  InternalOpen(FileName);
end;

destructor TdwlCursoredFile.Destroy;
begin
  InternalClose;
  inherited Destroy;
end;

function TdwlCursoredFile.ExtendMemory(const NeededFileSize: UInt64): Int64;
begin
  // resize the file
  var OldMemory := FMemory;
  FAllocatedMemorySize.QuadPart := ((FActuallyUsedMemorySize.QuadPart div FGrowSize)+1)*FGrowSize;
  UnMapViewOfFile(FMemory);
  CloseHandle(FMapHandle);
  FMapHandle := CreateFileMapping(FFileHandle, nil, PAGE_READWRITE, FAllocatedMemorySize.HighPart, FAllocatedMemorySize.LowPart, nil);
  if FMapHandle = 0 then
    raise Exception.Create('Failed to open file on resize'#13#10'Error $' + IntToHex(GetLastError, 2));
  FMemory := MapViewOfFile(FMapHandle, FILE_MAP_READ or FILE_MAP_WRITE, 0, 0, FAllocatedMemorySize.QuadPart);
  if FMemory = nil then
    raise Exception.Create('Failed to open file on resize'#13#10'Error $' + IntToHex(GetLastError, 2));
  Result := FMemory-OldMemory;
end;

procedure TdwlCursoredFile.Flush;
begin
  FlushViewOfFile(FMemory, 0);
end;

procedure TdwlCursoredFile.InternalClose;
begin
  UnMapViewOfFile(FMemory);
  CloseHandle(FMapHandle);
  if (not FIsReadOnly) and (FActuallyUsedMemorySize.QuadPart<>FAllocatedMemorySize.QuadPart) then
  begin
    SetFilePointerEx(FFileHandle, FActuallyUsedMemorySize.QuadPart, nil, FILE_BEGIN);
    SetEndOfFile(FFileHandle);
  end;
  CloseHandle(FFileHandle);
end;

procedure TdwlCursoredFile.InternalOpen(const FileName: string);
begin
  FIsReadOnly := pfoReadOnly in FOptions;
  var MakeNewFile := (not FileExists(FileName)) or (pfoCreateEmptyNew in FOptions);
  if MakeNewFile and ([pfoCreateIfNeeded, pfoCreateEmptyNew]*FOptions=[]) then
    raise Exception.Create('File does not exist: ' + Filename);
  System.SetLastError(0);
  if MakeNewFile then
    FFileHandle := CreateFile(PWideChar(FileName), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_WRITE, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0)
  else
  begin
    if FIsReadOnly then
      FFileHandle := CreateFile(PWideChar(FileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0)
    else
      FFileHandle := CreateFile(PWideChar(FileName), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  end;
  if FFileHandle=INVALID_HANDLE_VALUE then
    raise Exception.Create('Failed to open file: '+FileName + #13#10 + 'Error $' + IntToHex(GetLastError, 2));
  if MakeNewFile then
  begin
    FAllocatedMemorySize.QuadPart := FGrowSize;
    FActuallyUsedMemorySize.QuadPart := 0;
  end
  else
  begin
    FAllocatedMemorySize.LowPart := GetFileSize(FFileHandle, @FAllocatedMemorySize.HighPart);
    FActuallyUsedMemorySize := FAllocatedMemorySize;
  end;
  try
    if FIsReadOnly then
      FMapHandle := CreateFileMapping(FFileHandle, nil, PAGE_READONLY, FAllocatedMemorySize.HighPart, FAllocatedMemorySize.LowPart, nil)
    else
      FMapHandle := CreateFileMapping(FFileHandle, nil, PAGE_READWRITE, FAllocatedMemorySize.HighPart, FAllocatedMemorySize.LowPart, nil);
    if FMapHandle = 0 then
      raise Exception.Create('Failed to open file: '+FileName + #13#10 + 'Error $' + IntToHex(GetLastError, 2));
    try
      if FIsReadOnly then
        FMemory := MapViewOfFile(FMapHandle, FILE_MAP_READ, 0, 0, FAllocatedMemorySize.QuadPart)
      else
        FMemory := MapViewOfFile(FMapHandle, FILE_MAP_READ or FILE_MAP_WRITE, 0, 0, FAllocatedMemorySize.QuadPart);
      if FMemory = nil then
        raise Exception.Create('Failed to open file: '+FileName + #13#10 + 'Error $' + IntToHex(GetLastError, 2));
    except
      CloseHandle(FMapHandle);
      FMapHandle := 0;
    end;
  except
    CloseHandle(FFileHandle);
    FFileHandle := 0;
  end;
end;

{ TdwlCursor }

function TdwlCursor.GetSize: UInt64;
begin
  Result := FCursoredMemory.FActuallyUsedMemorySize.QuadPart;
end;

procedure TdwlCursor.AssureMemory(const RequestedSize: UInt64);
begin
  FCursoredMemory.AssureMemory(UInt64(FCursor-FCursoredMemory.FMemory)+RequestedSize)
end;

function TdwlCursor.CRC32: UInt32;
begin
  Result := TdwlCrypt.CRC32(FCursoredMemory.FMemory, FCursoredMemory.FActuallyUsedMemorySize.QuadPart);
end;

constructor TdwlCursor.Create(ACursoredMemory: TdwlCursoredIO; Writable: boolean);
begin
  inherited Create;
  FCursoredMemory := ACursoredMemory;
  FCursoredMemory.RegisterCursor(Self);
  FCursor := FCursoredMemory.FMemory;
  FCodePage := FCursoredMemory.FCodePage;
  // we keep the memorymapped file here as a object, not as an interface
  // to be able to use 'internal functions'
  // So do manual reference counting
  FCursoredMemory._AddRef;
  FWritable := Writable;
  if Writable then
    FCursoredMemory.FSynchronizer.BeginWrite
  else
    FCursoredMemory.FSynchronizer.BeginRead;
end;

function TdwlCursor.CursorOffset: UInt64;
begin
  Result := FCursor-FCursoredMemory.FMemory;
end;

function TdwlCursor.CursorPtr: PByte;
begin
  Result := FCursor;
end;

destructor TdwlCursor.Destroy;
begin
  FCursoredMemory.UnRegisterCursor(Self);
  if FWritable then
    FCursoredMemory.FSynchronizer.EndWrite
  else
    FCursoredMemory.FSynchronizer.EndRead;
  FCursoredMemory._Release;
  inherited Destroy;
end;

function TdwlCursor.Eof: Boolean;
begin
  Result := FCursor >= FCursoredMemory.FMemory+FCursoredMemory.FActuallyUsedMemorySize.QuadPart;
end;

procedure TdwlCursor.Fill(Value: byte; Count: UInt64);
begin
  AssureMemory(Count);
  FillMemory(FCursor, Count, Value);
end;

procedure TdwlCursor.Flush;
begin
  FCursoredMemory.Flush;
end;

procedure TdwlCursor.Read(var Buf; Count: UInt64);
begin
  Move(FCursor^, Buf, Count);
  inc(FCursor, Count);
end;

function TdwlCursor.ReadBoolean: boolean;
begin
  Result := ReadUInt8<>0;
end;

function TdwlCursor.ReadBytes(const Size: UInt32): TBytes;
begin
  SetLength(Result, Size);
  Read(Result[0], Size);
end;

function TdwlCursor.ReadDouble: double;
begin
  Result := PDouble(FCursor)^;
  inc(FCursor, SizeOf(double));
end;

function TdwlCursor.ReadInt64: Int64;
begin
  Result := PInt64(FCursor)^;
  inc(FCursor, SizeOf(Int64));
end;

function TdwlCursor.ReadInt8: shortint;
begin
  Result := PShortInt(FCursor)^;
  inc(FCursor, SizeOf(shortint));
end;

function TdwlCursor.ReadString_LenByte: string;
begin
  var ByteLen: integer := ReadUInt8;
  if ByteLen=0 then
    Exit('');
  var StrLen := MultiByteToWideChar(FCodePage, 0, PAnsiChar(FCursor), ByteLen, nil, 0);
  SetLength(Result, StrLen);
  MultiByteToWideChar(FCodePage, 0, PAnsiChar(FCursor), ByteLen, PWideChar(Result), StrLen);
  inc(FCursor, ByteLen);
end;

function TdwlCursor.ReadString_LenCardinal: string;
begin
  var ByteLen: integer := ReadUInt32;
  if ByteLen=0 then
    Exit('');
  var StrLen := MultiByteToWideChar(FCodePage, 0, PAnsiChar(FCursor), ByteLen, nil, 0);
  SetLength(Result, StrLen);
  MultiByteToWideChar(FCodePage, 0, PAnsiChar(FCursor), ByteLen, PWideChar(Result), StrLen);
  inc(FCursor, ByteLen);
end;

function TdwlCursor.ReadUInt16: word;
begin
  Result := PWord(FCursor)^;
  inc(FCursor, SizeOf(UInt16));
end;

function TdwlCursor.ReadUInt32: cardinal;
begin
  Result := Plongword(FCursor)^;
  inc(FCursor, SizeOf(UInt32));
end;

function TdwlCursor.ReadUInt64: UInt64;
begin
  Result := PUInt64(FCursor)^;
  inc(FCursor, SizeOf(UInt64));
end;

function TdwlCursor.ReadUInt8: byte;
begin
  Result := FCursor^;
  inc(FCursor, SizeOf(UInt8));
end;

procedure TdwlCursor.ReAllocationOccured(Offset: Int64);
begin
  inc(FCursor, Offset);
end;

procedure TdwlCursor.Seek(Offset: Int64; Origin: TSeekOrigin);
begin
  case Origin of
  soBeginning: FCursor := FCursoredMemory.FMemory + Offset;
  soCurrent: Inc(FCursor, Offset);
  soEnd: FCursor := FCursoredMemory.FMemory+FCursoredMemory.FActuallyUsedMemorySize.QuadPart-Offset;
  end;
end;

procedure TdwlCursor.SetSize(NewSize: UInt64);
begin
  AssureMemory(NewSize);
  FCursoredMemory.FActuallyUsedMemorySize.QuadPart := NewSize;
end;

procedure TdwlCursor.Write(const Buf; const Count: UInt64);
begin
  AssureMemory(Count);
  Move(Buf, FCursor^, Count);
  inc(FCursor, Count);
end;

procedure TdwlCursor.WriteBoolean(Value: boolean);
begin
  WriteUInt8(ord(Value));
end;

procedure TdwlCursor.WriteDouble(Value: double);
begin
  Write(Value, Sizeof(double));
end;

procedure TdwlCursor.WriteInt64(Value: Int64);
begin
  Write(Value, SizeOf(Int64));
end;

procedure TdwlCursor.WriteInt8(Value: shortint);
begin
  Write(Value, SizeOf(shortint));
end;

procedure TdwlCursor.WriteString_LenByte(const S: string; KnownMultiByteLength: UInt8=0);
begin
  var StrLen := S.Length;
  if KnownMultiByteLength=0 then
  begin
    var CalcLength := WideCharToMultiByte(FCodePage, 0, PWideChar(S), StrLen, nil, 0, nil, nil);
    if CalcLength>High(UInt8) then
      raise Exception.Create('Persisted Params: String too long to write with byte length:'+CalcLength.ToString);
    KnownMultiByteLength := CalcLength;
  end;
  WriteUInt8(KnownMultiByteLength);
  AssureMemory(KnownMultiByteLength);
  WideCharToMultiByte(FCodePage, 0, PWideChar(S), StrLen, PAnsiChar(FCursor), KnownMultiByteLength, nil, nil);
  inc(FCursor, KnownMultiByteLength);
end;

procedure TdwlCursor.WriteString_LenCardinal(const S: string; KnownMultiByteLength: UInt32=0);
begin
  var StrLen := S.Length;
  if KnownMultiByteLength=0 then
    KnownMultiByteLength := WideCharToMultiByte(FCodePage, 0, PWideChar(S), StrLen, nil, 0, nil, nil);
  WriteUInt32(KnownMultiByteLength);
  AssureMemory(KnownMultiByteLength);
  WideCharToMultiByte(FCodePage, 0, PWideChar(S), StrLen, PAnsiChar(FCursor), KnownMultiByteLength, nil, nil);
  inc(FCursor, KnownMultiByteLength);
end;

procedure TdwlCursor.WriteUInt16(Value: word);
begin
  Write(Value, Sizeof(UInt16));
end;

procedure TdwlCursor.WriteUInt32(Value: cardinal);
begin
  Write(Value, SizeOf(UInt32));
end;

procedure TdwlCursor.WriteUInt64(Value: UInt64);
begin
  Write(Value, SizeOf(UInt64));
end;

procedure TdwlCursor.WriteUInt8(Value: byte);
begin
  Write(Value, 1);
end;

{ TdwlCursoredIO }

procedure TdwlCursoredIO.AssureMemory(const RequestedSize: UInt64);
begin
  if (RequestedSize>FActuallyUsedMemorySize.QuadPart) then
  begin
    FActuallyUsedMemorySize.QuadPart := RequestedSize;
    if RequestedSize>FAllocatedMemorySize.QuadPart then
    begin
      var Offset := ExtendMemory(RequestedSize);
      for var Cursor in FCursors do
        Cursor.ReAllocationOccured(Offset);
    end;
  end;
end;

constructor TdwlCursoredIO.Create(CodePage: UINT);
begin
  inherited Create;
  FCursors := TList<TdwlCursor>.Create;
  FCodePage := CodePage;
end;

destructor TdwlCursoredIO.Destroy;
begin
  FCursors.Free;
  inherited Destroy;
end;

procedure TdwlCursoredIO.Flush;
begin
end;

function TdwlCursoredIO.GetReadCursor: IdwlCursor_Read;
begin
  Result := TdwlCursor.Create(Self, false);
end;

function TdwlCursoredIO.GetWriteCursor: IdwlCursor_Write;
begin
  Result := TdwlCursor.Create(Self, true);
end;

procedure TdwlCursoredIO.RegisterCursor(Cursor: TdwlCursor);
begin
  FCursors.Add(Cursor);
end;

procedure TdwlCursoredIO.UnRegisterCursor(Cursor: TdwlCursor);
begin
  FCursors.Remove(Cursor);
end;

end.
