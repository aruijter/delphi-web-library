unit DWL.IO.DDF;

interface

uses
  DWL.IO, DWL.Types, DWL.GridData;

const
  // DataTypes
  ddfInt8 = 0;
  ddfUInt8 = 1;
  ddfInt16 = 2;
  ddfUInt16 = 3;
  ddfInt32 = 4;
  ddfUInt32 = 5;
  ddfInt64 = 6;
  ddfUInt64 = 7;
  ddfSingle = 128;
  ddfDouble = 129;

type
  PddfDataDefintion = ^TddfDataDefinition;
  TddfDataDefinition = packed record
    DataType: UInt16;
    Base10Component: Int8;
    NoDataValueUsed: Int8;
    NoDataValue: Int64;
    Reserved4: UInt32;
  end;

  IdwlDDFPage = interface
    function GetData(TileCol, TileRow: cardinal; Destination: PByte): boolean;
    procedure PutData(TileCol, TileRow: cardinal; Source: PByte);
    function DataDefinition: TddfDataDefinition;
    procedure SetMetaData(const Key, Value: string);
    function GetMetaData(const Key: string): string;
    function GetDim: TdwlGridDim;
    property MetaData[const Key: string]: string read GetMetaData write SetMetaData;
  end;

  IdwlDDF = interface
    function GetBounds: TdwlBounds;
    function GetMetaData(const Key: string): string;
    procedure SetBounds(const Bounds: TdwlBounds);
    procedure SetMetaData(const Key, Value: string);
    function AddPage(Bounds: TdwlBounds; TilePixelCountX, TilePixelCountY: cardinal; DataType: word=ddfDouble; WidthInTiles: cardinal=1; HeightInTiles: cardinal=1; MaxUsedAsNoDataValue: boolean=false; Base10Exponent: shortint=0): IdwlDDFPage;
    function Page(PageNumber: cardinal): IdwlDDFPage;
    property Bounds: TdwlBounds read GetBounds write SetBounds;
    property MetaData[const Key: string]: string read GetMetaData write SetMetaData;
  end;

function New_DDF(const FileName: string; Options: TdwlFileOptions=[]): IdwlDDF; overload;
function New_DDF(CursoredIO: IdwlCursoredIO): IdwlDDF; overload;

implementation

uses
  System.SysUtils, System.Generics.Collections, System.Classes,
  System.ZLib;

const
  FILEVERSION_3  = 3;
  FILEVERSION_4  = 4;
  DDF_SIGNATURE = $616c;

  // Compression
  COMPRESSION_NONE = 0;
  COMPRESSION_ZLIB = 8;

  //ContentType
  dbctHeader = 1;
  dbctPageHeader = 2;
  dbctArray = 3;
  dbctArrayWithDataStarts = 4;
  dbctMetaData = 5;
  dbctNoData = 254;
  dbctDeleted = 255;

  ddfcrdEPSG4326=4326;

const
  DataTypeByteSizes: array[0..129] of byte = (1, 1, 2, 2, 4, 4, 8, 8,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    4, 8);

type
  PDDFFileIdentifier3 = ^ TDDFFileIdentifier3;
  TDDFFileIdentifier3 = packed record
    Version: UInt8;
    Signature: UInt16;
    Reserved0: UInt8;
  end;

  PDDFFileIdentifier4 = ^ TDDFFileIdentifier4;
  TDDFFileIdentifier4 = packed record
    Version: UInt8;
    Signature: UInt16;
    FirstDataBlockOffset: UInt8;
    Reserved1: UInt32;
    Reserved2: UInt64;
  end;

  PDDFDataBlockHeader3 = ^TDDFDataBlockHeader3;
  TDDFDataBlockHeader3 = packed record
    Size: UInt32; {including this header}
    ContentType: UInt16;
    Compression: UInt8;
    Reserved1: UInt8;
    MasterID: UInt32;
    ClientID: UInt32;
    MetaDataOffSet: UInt32;
  end;

  PDDFDataBlockHeader4 = ^TDDFDataBlockHeader4;
  TDDFDataBlockHeader4 = packed record
    Size: UInt32; {including this header}
    ContentType: UInt16;  // PageHeader, Metadata, Array, ArrayWithDataStarts
    Compression: UInt8; // for now only used with array or array with datastarts
    Reserved1: UInt8;  // $1: Deleted
    MasterID: UInt32; // unused
    ClientID: UInt32; //unused
    MetaDataOffSet: UInt64;
    Reserved2: UInt32;
    Reserved3: UInt32;
  end;

  PDDFFileContentHeader3 = ^TDDFFileContentHeader3;
  TDDFFileContentHeader3 = packed record
    DataBlockHeader: TDDFDataBlockHeader3;
    FirstDataPageOffset: cardinal;
    PageCount: cardinal;
    CoordinateSystem: cardinal; // 4326 // EPGS coordinate system definition
    DefinedBounds: TdwlBounds; // of the pointdata bounds, not the outer side of the pixels
    ActualBounds: TdwlBounds; // of the pointdata bounds, not the outer side of the pixels
    Reserved1: cardinal;
  end;

  PDDFFileContentHeader4 = ^TDDFFileContentHeader4;
  TDDFFileContentHeader4 = packed record
    DataBlockHeader: TDDFDataBlockHeader3;
    FirstDataPageOffset: cardinal;
    PageCount: cardinal;
    CoordinateSystem: cardinal; // 4326 // EPGS coordinate system definition
    Bounds: TdwlBounds;
    Reserved1: cardinal;
  end;

  PDDFPageheader3 =^TDDFPageHeader3;
  TDDFPageHeader3 = packed record
    DataBlockHeader: TDDFDataBlockHeader3;
    DataType: word;
    Base10Exponent: shortint;
    MaxUsedAsNoDataValue: byte;
    FrameType: byte; // unused
    TileColCount: cardinal;
    TileRowCount: cardinal;
    WidthInTiles: cardinal;
    HeightinTiles: cardinal;
    MetaDataOffset: cardinal;
    NextDataPageOffset: cardinal;
    // followed by ItemOffsets (4 bytes each: cardinal)
  end;
  
  PDDFPageheader4 =^TDDFPageHeader4;
  TDDFPageHeader4 = packed record
    DataBlockHeader: TDDFDataBlockHeader3;

    DataDefinition: TddfDataDefinition;

    FrameType: byte; // unused

    TileColCount: cardinal;
    TileRowCount: cardinal;
    WidthInTiles: cardinal;
    HeightinTiles: cardinal;

    MetaDataOffset: cardinal;
    NextDataPageOffset: cardinal;

    // followed by ItemOffsets (8 bytes each: UInt64)
  end;


type
  TDDF3=class;
  TDDF4=class;

  TMetaDataObject = class(TInterfacedObject)
  strict private
    FMetaDataPairs: TDictionary<string, string>;
    FMetaDataChanged: boolean;
    procedure InitMetaData;
  private
    function GetMetaData(const Key: string): string;
    procedure SetMetaData(const Key, Value: string);
  strict protected
    function GetMetaDataReadCursor: IdwlCursor_Read; virtual; abstract;
  public
    destructor Destroy; override;
  end;

  TDDFPage3 = class(TMetaDataObject, IdwlDDFPage)
  strict private
    FDDF: IdwlDDF;
    FPageHeader: PDDFPageHeader3;
    function TileByteSize: cardinal;
    function PageItemOffset(TileCol, TileRow: cardinal): UInt32;
  private
    function GetData(TileCol, TileRow: cardinal; Destination: PByte): boolean;
    function GetDim: TdwlGridDim;
    function DataDefinition: TddfDataDefinition;
    procedure PutData(TileCol, TileRow: cardinal; Data: PByte);
  strict protected
    function GetMetaDataReadCursor: IdwlCursor_Read; override;
  public
    constructor Create(DDF: IdwlDDF; PageOffset: cardinal);
  end;

  TDDFPage4 = class(TMetaDataObject, IdwlDDFPage)
  strict private
    FDDF: TDDF4;
    FPageHeader: PDDFPageHeader4;
    function PageItemPtr(TileCol, TileRow: cardinal): PUInt64;
  strict protected
    function GetMetaDataReadCursor: IdwlCursor_Read; override;
  private
    function DataDefinition: TddfDataDefinition;
    function GetData(TileCol, TileRow: cardinal; Destination: PByte): boolean;
    function GetDim: TdwlGridDim;
    procedure PutData(TileCol, TileRow: cardinal; Data: PByte);
  public
    constructor Create(DDF: TDDF4; PageOffset: UInt64);
    destructor Destroy; override;
  end;

  TDDF3 = class(TMetaDataObject, IdwlDDF)
  strict private
  private
    const
      CompressionLevel: TCompressionLevel = clMax;
    var
      FCursor: IdwlCursor_Read;
      FHeader: PDDFFileContentHeader3;
    function AddPage(Bounds: TdwlBounds; TilePixelCountX, TilePixelCountY: cardinal; DataType: word=ddfDouble; WidthInTiles: cardinal=1; HeightInTiles: cardinal=1; MaxUsedAsNoDataValue: boolean=false; Base10Exponent: shortint=0): IdwlDDFPage;
    function GetBounds: TdwlBounds;
    function Page(PageNumber: cardinal): IdwlDDFPage;
    procedure RaiseWritingError;
    procedure SetBounds(const Bounds: TdwlBounds);
  strict protected
    function GetMetaDataReadCursor: IdwlCursor_Read; override;
  public
    constructor Create(CursoredIO: IdwlCursoredIO);
    destructor Destroy; override;
  end;

  TDDF4 = class(TMetaDataObject, IdwlDDF)
  strict private
    FHeader: PDDFFileContentHeader4;
    FPageOffsets: TList<UInt64>;
  private
    const
      CompressionLevel: TCompressionLevel = clMax;
    var
      FCursor: IdwlCursor_Write;
    function AddPage(Bounds: TdwlBounds; TilePixelCountX, TilePixelCountY: cardinal; DataType: word=ddfDouble; WidthInTiles: cardinal=1; HeightInTiles: cardinal=1; MaxUsedAsNoDataValue: boolean=false; Base10Exponent: shortint=0): IdwlDDFPage;
    function GetBounds: TdwlBounds;
    function GetNewDataBlock(const Size: cardinal; ContentType: word; Compression: byte): PDDFDataBlockHeader4;
    function Page(PageNumber: cardinal): IdwlDDFPage;
    procedure SetBounds(const Bounds: TdwlBounds);
  strict protected
    function GetMetaDataReadCursor: IdwlCursor_Read; override;
  public
    constructor Create(CursoredIO: IdwlCursoredIO);
    destructor Destroy; override;
  end;


function New_DDF(CursoredIO: IdwlCursoredIO): IdwlDDF;
begin
  var Cur := CursoredIO.GetReadCursor;
  var FileVersion := FILEVERSION_4;
  if not Cur.Eof then // new file is version 4
    FileVersion := Cur.ReadUInt8;
  Cur := nil;
  if FileVersion=FILEVERSION_4 then
    Result := TDDF4.Create(CursoredIO)
  else
  begin
    if FileVersion=FILEVERSION_3 then
      Result := TDDF3.Create(CursoredIO)
    else
      raise Exception.Create('File version '+FileVersion.ToString+' not supported');
  end;
end;

function New_DDF(const FileName: string; Options: TdwlFileOptions=[]): IdwlDDF;
begin
  Result := New_DDF(New_CursoredIO(FileName, Options));
end;


{ TDDF3 }

function TDDF3.AddPage(Bounds: TdwlBounds; TilePixelCountX, TilePixelCountY: cardinal; DataType: word=ddfDouble; WidthInTiles: cardinal=1; HeightInTiles: cardinal=1; MaxUsedAsNoDataValue: boolean=false; Base10Exponent: shortint=0): IdwlDDFPage;
begin
  RaiseWritingError;
end;

constructor TDDF3.Create(CursoredIO: IdwlCursoredIO);
begin
  inherited Create;
  FCursor := CursoredIO.GetReadCursor;
  if PDDFFileIdentifier3(FCursor.CursorPtr).Signature<>DDF_SIGNATURE then
    raise Exception.Create('Invalid signature');
  FHeader := PDDFFileContentHeader3(FCursor.CursorPtr+SizeOf(TDDFFileIdentifier3));
  if FHeader.CoordinateSystem<>ddfcrdEPSG4326 then
    raise Exception.Create('Unknown CoordinateSystem');
  if FHeader.PageCount<>1 then
    raise Exception.Create('Version 3 reading only supports single page files');
  FCursor.Seek(FHeader.FirstDataPageOffset);
  if (PDDFPageHeader3(FCursor.CursorPtr).WidthInTiles<>1) or
    (PDDFPageHeader3(FCursor.CursorPtr).WidthInTiles<>1) then
    raise Exception.Create('Version 3 reading only supports single tile files');
end;

destructor TDDF3.Destroy;
begin
  inherited Destroy;
end;

function TDDF3.GetBounds: TdwlBounds;
begin
  Result := FHeader.ActualBounds;
end;

function TDDF3.GetMetaDataReadCursor: IdwlCursor_Read;
begin
  if FHeader.DataBlockHeader.MetaDataOffSet=0 then
    Exit(nil);
  Result := FCursor;
  Result.Seek(FHeader.DataBlockHeader.MetaDataOffSet);
  Result.Seek(SizeOf(TDDFDataBlockHeader3), soCurrent);
end;

function TDDF3.Page(PageNumber: cardinal): IdwlDDFPage;
begin
  if PageNumber<>0 then
    raise Exception.Create('Version 3 reading only supports single page files');
  Result := TDDFPage3.Create(Self, FHeader.FirstDataPageOffset);
end;

procedure TDDF3.RaiseWritingError;
begin
  raise Exception.Create('DDF writing is disabled for legacy versions');
end;

procedure TDDF3.SetBounds(const Bounds: TdwlBounds);
begin
  RaiseWritingError;
end;

{ TDDFPage3 }

constructor TDDFPage3.Create(DDF: IdwlDDF; PageOffset: cardinal);
begin
  inherited Create;
  FDDF := DDF;
  TDDF3(FDDF).FCursor.Seek(PageOffset);
  FPageHeader := PDDFPageHeader3(TDDF3(FDDF).FCursor.CursorPtr);
end;

function TDDFPage3.DataDefinition: TddfDataDefinition;
begin
  Result.DataType := FPageHeader.DataType;
  Result.Base10Component := FPageHeader.Base10Exponent;
  Result.NoDataValueUsed := FPageHeader.MaxUsedAsNoDataValue;
  if Result.NoDataValueUsed<>0 then
  begin
    case Result.DataType of
    ddfInt8: Result.NoDataValue := High(Int8);
    ddfUInt8: Result.NoDataValue := High(UInt8);
    ddfInt16: Result.NoDataValue := High(Int16);
    ddfUInt16: Result.NoDataValue := High(UInt16);
    ddfInt32: Result.NoDataValue := High(Int32);
    ddfUInt32: Result.NoDataValue := High(UInt32);
    ddfInt64: Result.NoDataValue := High(Int64);
    end;
  end;
end;

function TDDFPage3.GetData(TileCol, TileRow: cardinal; Destination: PByte): boolean;
begin
  var Offset := PageItemOffset(TileCol, TileRow);
  if Offset=0 then
    Exit(false);
  TDDF3(FDDF).FCursor.Seek(Offset);
  var DataBlock := PDDFDataBlockHeader3(TDDF3(FDDF).FCursor.CursorPtr);
  if DataBlock.ContentType<>dbctArray then
    raise Exception.Create('In version 3 only Array datablocks are allowed');
  TDDF3(FDDF).FCursor.Seek(SizeOf(TDDFDataBlockHeader3), soCurrent);
  var PayloadSize := TileByteSize;
  case DataBlock.Compression of
  COMPRESSION_ZLIB:
    begin
      var DecompBuf: Pointer;
      var DecompSize: integer;
      ZDecompress(PByte(DataBlock)+SizeOf(TDDFDataBlockHeader3), DataBlock.Size-SizeOf(TDDFDataBlockHeader3), DecompBuf, DecompSize);
      try
        if DecompSize<>integer(PayloadSize) then
          raise Exception.Create('payload Size error while reading data');
        Move(DecompBuf^, Destination^, PayloadSize);
      finally
        FreeMem(DecompBuf);
      end;
    end;
  COMPRESSION_NONE:
    Move(TDDF3(FDDF).FCursor.CursorPtr^, Destination^, PayloadSize);
  else
    raise Exception.Create('Unknow Compression');
  end;
  Result := true;
end;

function TDDFPage3.GetDim: TdwlGridDim;
begin
  Result.WidthInPixels := FPageHeader.TileColCount;
  Result.HeightInPixels := FPageHeader.TileRowCount;
  Result. ScaleGridToWorld := TDDF3(FDDF).FHeader.DefinedBounds.XExtent / (FPageHeader.TileColCount-1);
  Result.LeftWorldX := TDDF3(FDDF).FHeader.DefinedBounds.XMin;
  Result.TopWorldY := TDDF3(FDDF).FHeader.DefinedBounds.YMax;
end;

function TDDFPage3.GetMetaDataReadCursor: IdwlCursor_Read;
begin
  Result := nil; 
  if FPageHeader.MetaDataOffset<>0 then
  begin
    Result := TDDF3(FDDF).FCursor;
    Result.Seek(FPageHeader.MetaDataOffset);
  end
  else
  begin
    if FPageHeader.DataBlockHeader.MetaDataOffSet<>0 then
    begin
      Result := TDDF3(FDDF).FCursor;
      Result.Seek(FPageHeader.DataBlockHeader.MetaDataOffSet);
      Result.Seek(SizeOf(TDDFDataBlockHeader3), soCurrent);
    end;
  end;
end;

function TDDFPage3.PageItemOffset(TileCol, TileRow: cardinal): UInt32;
begin
  Result := PUInt32(PByte(FPageHeader)+SizeOf(TDDFPageHeader3)+(TileRow*FPageHeader.TileColCount+TileCol)*SizeOf(UInt64))^;
end;

procedure TDDFPage3.PutData(TileCol, TileRow: cardinal; Data: PByte);
begin
  TDDF3(FDDF).RaiseWritingError;
end;

function TDDFPage3.TileByteSize: cardinal;
begin
  Result := DataTypeByteSizes[FPageHeader.DataType] * FPageHeader.TileColCount * FPageHeader.TileRowCount;
end;

{ TMetaDataObject }

destructor TMetaDataObject.Destroy;
begin
  FMetaDataPairs.Free;
  inherited Destroy;
end;

function TMetaDataObject.GetMetaData(const Key: string): string;
begin
  InitMetaData;
  if not FMetaDataPairs.TryGetValue(Key, Result) then
    Result := '';
end;

procedure TMetaDataObject.InitMetaData;
begin 
  if FMetaDataPairs<>nil then
    Exit;
  FMetaDataPairs := TDictionary<string, string>.Create;
  var Cursor := GetMetaDataReadCursor;
  if Cursor= nil then
    Exit;
  while true do
  begin
    var Str := Cursor.ReadString_LenByte;
    if Str='' then
      Break;
    FMetaDataPairs.Add(Str, Cursor.ReadString_LenWord);
  end;
end;

procedure TMetaDataObject.SetMetaData(const Key, Value: string);
begin
  InitMetaData;
  var CurrValue: string;
  if FMetaDataPairs.TryGetValue(Key, CurrValue) then
  begin
    if Value='' then
    begin
      FMetaDataPairs.Remove(Key);
      FMetaDataChanged := true;
    end
    else
    begin
      if Value<>CurrValue then
      begin
        FMetaDataPairs.AddOrSetValue(Key, Value);
        FMetaDataChanged := true;
      end;
    end;
  end
  else
  begin
    if Value<>'' then
    begin
      FMetaDataPairs.Add(Key, Value);
      FMetaDataChanged := true;
    end;
  end;
end;

{ TDDF4 }

function TDDF4.AddPage(Bounds: TdwlBounds; TilePixelCountX, TilePixelCountY: cardinal; DataType: word; WidthInTiles, HeightInTiles: cardinal; MaxUsedAsNoDataValue: boolean; Base10Exponent: shortint): IdwlDDFPage;
begin
  GetNewDataBlock(SizeOf(TDDFPageHeader3)+(WidthInTiles*HeightInTiles)*SizeOf(cardinal), dbctPageHeader, COMPRESSION_NONE);
  var PageHeader := PDDFPageheader3(FCursor.CursorPtr);

  PageHeader.DataType := word(DataType);
  PageHeader.Base10Exponent := Base10Exponent;
  PageHeader.MaxUsedAsNoDataValue := byte(MaxUsedAsNoDataValue);
  PageHeader.TileColCount := TilePixelCountX;
  PageHeader.TileRowCount := TilePixelCountY;
  PageHeader.WidthInTiles := WidthInTiles;
  PageHeader.HeightinTiles := HeightInTiles;
  Result := TDDFPage4.Create(Self, FCursor.CursorOffset);

  if FPageOffsets.Count=0 then
    FHeader.FirstDataPageOffset := FCursor.CursorOffset
  else
  begin
    var NewOffset := FCursor.CursorOffset;
    FCursor.Seek(FPageOffsets[FHeader.PageCount-1]);
    PDDFPageheader3(FCursor.CursorPtr).NextDataPageOffset := NewOffset;
  end;
  inc(FHeader.PageCount);
end;

constructor TDDF4.Create(CursoredIO: IdwlCursoredIO);
begin
  inherited Create;
  FPageOffsets := TList<UInt64>.Create;
  FCursor := CursoredIO.GetWriteCursor;
  // initialize header
  if FCursor.Size=0 {New File: initialize structure} then
  begin
    FCursor.SetSize(SizeOf(TDDFFileIdentifier4)+SizeOf(TDDFFileContentHeader4));
    var FileIdent := PDDFFileIdentifier3(FCursor.CursorPtr);
    FileIdent.Version := FILEVERSION_4;
    FileIdent.Signature := DDF_SIGNATURE;
    FileIdent.Reserved0 := 0;
    FHeader.DataBlockHeader.Size := SizeOf(TDDFFileContentHeader4);
    FHeader.DataBlockHeader.ContentType := dbctHeader;
    FHeader.DataBlockHeader.Compression := COMPRESSION_NONE;
    FHeader.DataBlockHeader.Reserved1 := 0;
    FHeader.DataBlockHeader.MasterID := 0;
    FHeader.DataBlockHeader.ClientID := 0;
    FHeader.DataBlockHeader.MetaDataOffSet := 0;
    FHeader.FirstDataPageOffset := 0;
    FHeader.PageCount := 0;
    FHeader.CoordinateSystem := ddfcrdEPSG4326;
    FHeader.Bounds := EmptyBounds;
    FHeader.Reserved1 := 0;
  end
  else
  begin
    var FileIdent := PDDFFileIdentifier3(FCursor.CursorPtr);
    if FileIdent.Version<>FILEVERSION_4 then
      raise Exception.Create('FileVersion '+FileIdent.Version.ToString+' is not supported');
    if FileIdent.Signature<>DDF_SIGNATURE then
      raise Exception.Create('Invalid signature');
    if FHeader.CoordinateSystem<>ddfcrdEPSG4326 then
      raise Exception.Create('Unknown CoordinateSystem');
  end;
  FHeader := PDDFFileContentHeader4(FCursor.CursorPtr+SizeOf(TDDFFileIdentifier4));
  FCursor.RegisterMemoryPointer(@FHeader);
  // initialize pages
  var PageOffset := FHeader.FirstDataPageOffset;
  for var PageNo := 1 to FHeader.PageCount do
  begin
    FPageOffsets.Add(PageOffset);
    FCursor.Seek(PageOffset);
    PageOffset := PDDFPageHeader3(FCursor.CursorPtr).NextDataPageOffset;
  end;
end;

destructor TDDF4.Destroy;
begin
  FPageOffsets.Free;
  inherited Destroy;
end;

function TDDF4.GetBounds: TdwlBounds;
begin
  Result := FHeader.Bounds;
end;

function TDDF4.GetMetaDataReadCursor: IdwlCursor_Read;
begin
  if FHeader.DataBlockHeader.MetaDataOffSet=0 then
    Exit(nil);
  Result := FCursor;
  Result.Seek(FHeader.DataBlockHeader.MetaDataOffSet);
end;

function TDDF4.GetNewDataBlock(const Size: cardinal; ContentType: word; Compression: byte): PDDFDataBlockHeader4;
begin
  FCursor.AllocateBlock(Size);
  Result := PDDFDataBlockHeader4(FCursor.CursorPtr);
  FillChar(Result^, SizeOf(TDDFDataBlockHeader4), 0);
  Result.ContentType := ContentType;
  Result.Compression := Compression;
  Result.Size := Size;
end;

function TDDF4.Page(PageNumber: cardinal): IdwlDDFPage;
begin
  Result := TDDFPage4.Create(Self, FPageOffsets[PageNumber]);
end;

procedure TDDF4.SetBounds(const Bounds: TdwlBounds);
begin
  FHeader.Bounds := Bounds;
end;

{ TDDFPage4 }

constructor TDDFPage4.Create(DDF: TDDF4; PageOffset: UInt64);
begin
  inherited Create;
  FPageHeader := PDDFPageHeader4(FDDF.FCursor.CursorPtr);
  FDDF.FCursor.RegisterMemoryPointer(@FPageHeader);
end;

function TDDFPage4.DataDefinition: TddfDataDefinition;
begin

end;

destructor TDDFPage4.Destroy;
begin
  FDDF.FCursor.UnRegisterMemoryPointer(@FPageHeader);
  // because we did a manual AddRef, Release it on destroy
  FDDF._Release;
  inherited Destroy;
end;

function TDDFPage4.GetData(TileCol, TileRow: cardinal; Destination: PByte): boolean;
begin
  Result := false;
end;

function TDDFPage4.GetDim: TdwlGridDim;
begin

end;

function TDDFPage4.GetMetaDataReadCursor: IdwlCursor_Read;
begin
  Result := nil; 
  if FPageHeader.MetaDataOffset<>0 then
  begin
    Result := FDDF.FCursor;
    Result.Seek(FPageHeader.MetaDataOffset);
  end
  else
  begin
    if FPageHeader.DataBlockHeader.MetaDataOffSet<>0 then
    begin
      Result := FDDF.FCursor;
      Result.Seek(FPageHeader.DataBlockHeader.MetaDataOffSet);
    end;
  end;
end;

function TDDFPage4.PageItemPtr(TileCol, TileRow: cardinal): PUInt64;
begin
  Result := PUInt64(PByte(FPageHeader)+SizeOf(TDDFPageHeader4)+(TileRow*FPageHeader.TileColCount*TileRow+TileCol)*SizeOf(UInt64));
end;

procedure TDDFPage4.PutData(TileCol, TileRow: cardinal; Data: PByte);
begin
  var DataSize :=  DataTypeByteSizes[FPageHeader.DataDefinition.DataType]*FPageHeader.TileColCount*FPageHeader.TileRowCount;
  var CompBuf: pointer;
  var CompSize: integer;
  ZCompress(Data, DataSize, CompBuf, CompSize, FDDF.CompressionLevel);
  try
    FDDF.GetNewDataBlock(SizeOf(TDDFDataBlockHeader3)+CompSize, dbctArray, COMPRESSION_ZLIB);
    // put offset of this new block in Pageheader
    PageItemPtr(TileCol, TileRow)^ := FDDF.FCursor.CursorOffset;
    // put compressed data in file
    FDDF.FCursor.Seek(SizeOf(TDDFDataBlockHeader3), soCurrent);
    Move(CompBuf^, FDDF.FCursor.CursorPtr^, CompSize);
  finally
    FreeMem(CompBuf);
  end;

end;

end.
