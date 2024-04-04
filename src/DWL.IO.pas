unit DWL.IO;

interface

uses
  System.Classes, System.SysUtils, Winapi.Windows;

type
  TdwlFileOption = (pfoCreateIfNeeded, pfoCreateEmptyNew, pfoReadOnly);
  TdwlFileOptions = set of TdwlFileOption;

  IdwlCursor_Read = interface
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
    function CursorOffset: UInt64;
    function CursorPtr: PByte;
  end;

  IdwlCursor_Write = interface(IdwlCursor_Read)
    procedure Fill(Value: byte; Count: UInt64);
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

  IdwlFileCursor_Read = interface(IdwlCursor_Read)
    function CRC32: UInt32;
    function Filename: string;
    function FileSize: UInt64;
  end;

  IdwlFileCursor_Write = interface(IdwlCursor_Write)
    function Eof: boolean;
    function FileSize: UInt64;
    procedure SetFileSize(NewFileSize: UInt64);
    procedure Flush;
  end;

  IdwlFile = interface
    function GetReadCursor: IdwlFileCursor_Read;
    function GetWriteCursor: IdwlFileCursor_Write;
  end;

function New_File(const FileName: string; Options: TdwlFileOptions=[]; CodePage: UINT=CP_UTF8; GrowSize: cardinal=524288): IdwlFile;

implementation

uses
 System.SyncObjs, DWL.Crypt, System.Math, System.Generics.Collections;

type
  TdwlCursor = class;

  TdwlCursoredMemory = class(TInterfacedObject)
  strict private
    FCursors: TList<TdwlCursor>;
  private
    FCodePage: UINT;
    FMemory: PByte;
    FActuallyUsedMemorySize: ULARGE_INTEGER;
    procedure RegisterCursor(Cursor: TdwlCursor);
    procedure UnRegisterCursor(Cursor: TdwlCursor);
    procedure AssureMemory(const RequestedSize: UInt64);
  protected
    function ExtendMemory(const NeededFileSize: UInt64): Int64; virtual; abstract;
  public
    constructor Create(CodePage: UINT);
    destructor Destroy; override;
  end;

  TdwlFile = class(TdwlCursoredMemory, IdwlFile)
  strict private
    FFileHandle: THandle;
    FFileSize: ULARGE_INTEGER;
    FGrowSize: cardinal;
    FIsReadOnly: boolean;
    FMapHandle: THandle;
    FOptions: TdwlFileOptions;
    procedure InternalClose;
    procedure InternalOpen;
  private
    FFileName: string;
    FSynchronizer: TLightweightMREW;
    function GetReadCursor: IdwlFileCursor_Read;
    function GetWriteCursor: IdwlFileCursor_Write;
  protected
    function ExtendMemory(const NeededFileSize: UInt64): Int64; override;
    procedure Flush;
  public
    constructor Create(const FileName: string; Options: TdwlFileOptions; CodePage: UINT; GrowSize: cardinal);
    destructor Destroy; override;
  end;

  TdwlCursor = class(TInterfacedObject, IdwlCursor_Read, IdwlCursor_Write)
  strict private
    procedure AssureMemory(const RequestedSize: UInt64);
  private
    FCursoredMemory: TdwlCursoredMemory;
    FMemory: PByte;
    FCursor: PByte;
    FCodePage: UINT;
    function CursorOffset: UInt64;
    function CursorPtr: PByte;
    procedure Fill(Value: byte; Count: UInt64);
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
    constructor Create(ACursoredMemory: TdwlCursoredMemory);
    destructor Destroy; override;
  end;

  TdwlFileCursor = class(TdwlCursor, IdwlFileCursor_Read, IdwlFileCursor_Write)
  strict private
    FWritable: boolean;
  private
    FFile: TdwlFile;
    function Eof: Boolean;
    function CRC32: UInt32;
    function FileName: string;
    function FileSize: UInt64;
    procedure Flush;
    procedure SetFileSize(NewFileSize: UInt64);
  protected
  public
    constructor Create(AFile: TdwlFile; Writable: boolean);
    destructor Destroy; override;
  end;

function New_File(const FileName: string; Options: TdwlFileOptions=[]; CodePage: UINT=CP_UTF8; GrowSize: cardinal=524288): IdwlFile;
begin
  Result := TdwlFile.Create(FileName, Options, CodePage, GrowSize);
end;

{ TdwlFile }

constructor TdwlFile.Create(const FileName: string; Options: TdwlFileOptions; CodePage: UINT; GrowSize: cardinal);
begin
  inherited Create(CodePage);
  FGrowSize := GrowSize;
  FFilename := Filename;
  FOptions := Options;
  InternalOpen;
end;

destructor TdwlFile.Destroy;
begin
  InternalClose;
  inherited Destroy;
end;

function TdwlFile.ExtendMemory(const NeededFileSize: UInt64): Int64;
begin
  Result := 0;
  if NeededFileSize>FFileSize.QuadPart then
  begin // resize the file
    var OldMemory := FMemory;
    FFileSize.QuadPart := ((FActuallyUsedMemorySize.QuadPart div FGrowSize)+1)*FGrowSize;
    UnMapViewOfFile(FMemory);
    CloseHandle(FMapHandle);
    FMapHandle := CreateFileMapping(FFileHandle, nil, PAGE_READWRITE, FFileSize.HighPart, FFileSize.LowPart, nil);
    if FMapHandle = 0 then
      raise Exception.Create('Failed to open file on resize'#13#10'Error $' + IntToHex(GetLastError, 2));
    FMemory := MapViewOfFile(FMapHandle, FILE_MAP_READ or FILE_MAP_WRITE, 0, 0, FFileSize.QuadPart);
    if FMemory = nil then
      raise Exception.Create('Failed to open file on resize'#13#10'Error $' + IntToHex(GetLastError, 2));
    Result := FMemory-OldMemory;
  end;
end;

procedure TdwlFile.Flush;
begin
  FlushViewOfFile(FMemory, 0);
end;

function TdwlFile.GetReadCursor: IdwlFileCursor_Read;
begin
  Result := TdwlFileCursor.Create(Self, false);
end;

function TdwlFile.GetWriteCursor: IdwlFileCursor_Write;
begin
  Result := TdwlFileCursor.Create(Self, true);
end;

procedure TdwlFile.InternalClose;
begin
  UnMapViewOfFile(FMemory);
  CloseHandle(FMapHandle);
  if (not FIsReadOnly) and (FActuallyUsedMemorySize.QuadPart<>FFileSize.QuadPart) then
  begin
    SetFilePointerEx(FFileHandle, FActuallyUsedMemorySize.QuadPart, nil, FILE_BEGIN);
    SetEndOfFile(FFileHandle);
  end;
  CloseHandle(FFileHandle);
end;

procedure TdwlFile.InternalOpen;
begin
  FIsReadOnly := pfoReadOnly in FOptions;
  var MakeNewFile := (not FileExists(FFileName)) or (pfoCreateEmptyNew in FOptions);
  if MakeNewFile and ([pfoCreateIfNeeded, pfoCreateEmptyNew]*FOptions=[]) then
    raise Exception.Create('File does not exist: ' + FFilename);
  System.SetLastError(0);
  if MakeNewFile then
    FFileHandle := CreateFile(PWideChar(FFileName), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_WRITE, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0)
  else
  begin
    if FIsReadOnly then
      FFileHandle := CreateFile(PWideChar(FFileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0)
    else
      FFileHandle := CreateFile(PWideChar(FFileName), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  end;
  if FFileHandle=INVALID_HANDLE_VALUE then
    raise Exception.Create('Failed to open file: '+FFileName + #13#10 + 'Error $' + IntToHex(GetLastError, 2));
  if MakeNewFile then
  begin
    FFileSize.QuadPart := FGrowSize;
    FActuallyUsedMemorySize.QuadPart := 0;
  end
  else
  begin
    FFileSize.LowPart := GetFileSize(FFileHandle, @FFileSize.HighPart);
    FActuallyUsedMemorySize := FFileSize;
  end;
  try
    if FIsReadOnly then
      FMapHandle := CreateFileMapping(FFileHandle, nil, PAGE_READONLY, FFileSize.HighPart, FFileSize.LowPart, nil)
    else
      FMapHandle := CreateFileMapping(FFileHandle, nil, PAGE_READWRITE, FFileSize.HighPart, FFileSize.LowPart, nil);
    if FMapHandle = 0 then
      raise Exception.Create('Failed to open file: '+FFileName + #13#10 + 'Error $' + IntToHex(GetLastError, 2));
    try
      if FIsReadOnly then
        FMemory := MapViewOfFile(FMapHandle, FILE_MAP_READ, 0, 0, FFileSize.QuadPart)
      else
        FMemory := MapViewOfFile(FMapHandle, FILE_MAP_READ or FILE_MAP_WRITE, 0, 0, FFileSize.QuadPart);
      if FMemory = nil then
        raise Exception.Create('Failed to open file: '+FFileName + #13#10 + 'Error $' + IntToHex(GetLastError, 2));
    except
      CloseHandle(FMapHandle);
      FMapHandle := 0;
    end;
  except
    CloseHandle(FFileHandle);
    FFileHandle := 0;
  end;
end;

{ TdwlFileCursor }

function TdwlFileCursor.CRC32: UInt32;
begin
  Result := TdwlCrypt.CRC32(FMemory, FileSize);
end;

constructor TdwlFileCursor.Create(AFile: TdwlFile; Writable: boolean);
begin
  inherited Create(AFile);
  FFile := AFile;
  // we keep the memorymapped file here as a object, not as an interface
  // to be able to use 'internal functions'
  // So do manual reference counting
  FFile._AddRef;
  FWritable := Writable;
  if Writable then
    FFile.FSynchronizer.BeginWrite
  else
    FFile.FSynchronizer.BeginRead;
end;

destructor TdwlFileCursor.Destroy;
begin
  inherited Destroy;
  if FWritable then
    FFile.FSynchronizer.EndWrite
  else
    FFile.FSynchronizer.EndRead;
  FFile._Release;
end;

function TdwlFileCursor.Eof: Boolean;
begin
  Result := FCursor>=FMemory+FileSize;
end;

function TdwlFileCursor.FileName: string;
begin
  Result := FFile.FFileName;
end;

function TdwlFileCursor.FileSize: UInt64;
begin
  Result := FFile.FActuallyUsedMemorySize.QuadPart;
end;

procedure TdwlFileCursor.Flush;
begin
  FFile.Flush;
end;

procedure TdwlFileCursor.SetFileSize(NewFileSize: UInt64);
begin
  FFile.FActuallyUsedMemorySize.QuadPart := NewFileSize;
end;

{ TdwlCursor }

procedure TdwlCursor.AssureMemory(const RequestedSize: UInt64);
begin
  FCursoredMemory.AssureMemory(UInt64(FCursor-FMemory)+RequestedSize)
end;

constructor TdwlCursor.Create(ACursoredMemory: TdwlCursoredMemory);
begin
  inherited Create;
  FCursoredMemory := ACursoredMemory;
  FCursoredMemory.RegisterCursor(Self);
  FMemory := FCursoredMemory.FMemory;
  FCursor := FMemory;
  FCodePage := FCursoredMemory.FCodePage;
end;

function TdwlCursor.CursorOffset: UInt64;
begin
  Result := FCursor-FMemory;
end;

function TdwlCursor.CursorPtr: PByte;
begin
  Result := FCursor;
end;

destructor TdwlCursor.Destroy;
begin
  FCursoredMemory.UnRegisterCursor(Self);
  inherited Destroy;
end;

procedure TdwlCursor.Fill(Value: byte; Count: UInt64);
begin
  AssureMemory(Count);
  FillMemory(FCursor, Count, Value);
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
  inc(FMemory, Offset);
  inc(FCursor, Offset);
end;

procedure TdwlCursor.Seek(Offset: Int64; Origin: TSeekOrigin);
begin
  case Origin of
  soBeginning: FCursor := FMemory + Offset;
  soCurrent: Inc(FCursor, Offset);
  soEnd: FCursor := FMemory+FCursoredMemory.FActuallyUsedMemorySize.QuadPart-Offset;
  end;
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

{ TdwlCursoredMemory }

procedure TdwlCursoredMemory.AssureMemory(const RequestedSize: UInt64);
begin
  if (RequestedSize>FActuallyUsedMemorySize.QuadPart) then
  begin
    FActuallyUsedMemorySize.QuadPart := RequestedSize;
    var Offset := ExtendMemory(RequestedSize);
    if Offset<>0 then
    begin
       for var Cursor in FCursors do
        Cursor.ReAllocationOccured(Offset);
    end;
  end;
end;

constructor TdwlCursoredMemory.Create(CodePage: UINT);
begin
  inherited Create;
  FCursors := TList<TdwlCursor>.Create;
  FCodePage := CodePage;
end;

destructor TdwlCursoredMemory.Destroy;
begin
  FCursors.Free;
  inherited Destroy;
end;

procedure TdwlCursoredMemory.RegisterCursor(Cursor: TdwlCursor);
begin
  FCursors.Add(Cursor);
end;

procedure TdwlCursoredMemory.UnRegisterCursor(Cursor: TdwlCursor);
begin
  FCursors.Remove(Cursor);
end;

end.

