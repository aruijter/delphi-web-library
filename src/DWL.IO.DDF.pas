unit DWL.IO.DDF;

interface

uses
  DWL.IO, DWL.Types, DWL.GridData, DWL.Params;

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
    DataType: word;
    Base10Component: shortint;
    NoDataValueUsed: ByteBool;
    NoDataValue: Int64;
    Reserved4: cardinal;
    class function Create(ADataType: word; ABase10Component: shortint=0; ANoDataValueUsed: boolean=false; ANoDataValue: Int64=0): TddfDataDefinition; static;
  end;

  IdwlMetaDataObject = interface
    function MetaData: IdwlParams;
  end;

  IdwlDDFPage = interface(IdwlMetaDataObject)
    function GetData(TileCol, TileRow: cardinal; Destination: PByte): boolean;
    procedure PutGridData(TileCol, TileRow: cardinal; Dim: TdwlGridDim; SourceSize: cardinal; SourcePtr: PByte);
    function DataDefinition: TddfDataDefinition;
    function GetDim(TileCol, TileRow: cardinal): TdwlGridDim;
  end;

  IdwlDDF = interface(IdwlMetaDataObject)
    function AddPage(DataDefinition: TddfDataDefinition; OuterBounds: TdwlBounds; TilePixelCountX, TilePixelCountY: cardinal; WidthInTiles: cardinal=1; HeightInTiles: cardinal=1): IdwlDDFPage;
    function Page(PageNumber: cardinal): IdwlDDFPage;
  end;

function New_DDF(const FileName: string; Options: TdwlFileOptions=[]): IdwlDDF; overload;
function New_DDF(CursoredIO: IdwlCursoredIO): IdwlDDF; overload;

implementation

uses
  System.SysUtils, System.Generics.Collections, System.Classes,
  System.ZLib, System.Rtti, Winapi.Windows;

const
  FILEVERSION_3  = 3;
  FILEVERSION_4  = 4;
  DDF_SIGNATURE = $616c;

  // Compression
  COMPRESSION_NONE = 0;
  COMPRESSION_ZLIB = 8;

  //ContentType
  dbctFileContent3 = $0001;
  dbctPageHeader3 = $0002;
  dbctArray3 = $0003;
  dbctMetaData3 = $0005;
  dbctDeleted3 = $00ff;

  dbctFileContent4 = $0401;
  dbctPageHeader4 = $0402;
  dbctGridData = $0403;
  dbctMetaData4 = $0405;
  dbctDeleted4 = $04ff;

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

  PDDFDataBlockHeaderStart = ^TDDFDataBlockHeaderStart;
  TDDFDataBlockHeaderStart = packed record
    Size: UInt32; {including this header}
    ContentType: UInt16;
    Compression: UInt8;
    Reserved1: UInt8;
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
    TotalPageCount: cardinal;
    CoordinateSystem: cardinal; // 4326 // EPGS coordinate system definition
    DefinedBounds: TdwlBounds; // of the pointdata bounds, not the outer side of the pixels
    ActualBounds: TdwlBounds; // of the pointdata bounds, not the outer side of the pixels
    Reserved1: cardinal;
  end;

  PDDFFileContentHeader4 = ^TDDFFileContentHeader4;
  TDDFFileContentHeader4 = packed record
    DataBlockHeader: TDDFDataBlockHeader4;
    FirstDataPageOffset: UInt64;
    TotalPageCount: cardinal;
    CoordinateSystem: cardinal; // 4326 // EPGS coordinate system definition
    OuterBounds: TdwlBounds;
    LastUsedClientID: UInt32;
    Reserved5: UInt32;
    Reserved6: UInt32;
    Reserved7: UInt32;
  end;

  PDDFPageheader3 =^TDDFPageHeader3;
  TDDFPageHeader3 = packed record
    DataBlockHeader: TDDFDataBlockHeader3;
    DataType: word;
    Base10Exponent: shortint;
    MaxUsedAsNoDataValue: ByteBool;
    FrameType: byte; // unused
    TileWidthX: cardinal;
    TileHeightY: cardinal;
    WidthInTiles: cardinal;
    HeightinTiles: cardinal;
    MetaDataOffset: cardinal;
    NextDataPageOffset: cardinal;
    // followed by ItemOffsets (4 bytes each: cardinal)
  end;

  PDDFPageHeader4 =^TDDFPageHeader4;
  TDDFPageHeader4 = packed record
    DataBlockHeader: TDDFDataBlockHeader4;
    DataDefinition: TddfDataDefinition;
    TileWidthX: cardinal;
    TileHeightY: cardinal;
    WidthInTiles: cardinal;
    HeightinTiles: cardinal;
    NextDataPageOffset: UInt64;
    Reserved5: UInt32;
    Reserved6: UInt32;
    OuterBounds: TdwlBounds;
    // followed by ItemOffsets (8 bytes each: UInt64)
  end;

  PDDFGridDataHeader = ^TDDFGridDataHeader;
  TDDFGridDataHeader = packed record
    DataBlockHeader: TDDFDataBlockHeader4;
    Dim: TdwlGridDim;
    Reserved4: UInt32;
    // followed by the data
  end;

type
  TDDF3=class;
  TDDF4=class;

  TMetaDataObject = class(TInterfacedObject)
  strict private
    FMetaData: IdwlParams;
    FDDF: TDDF4;
    procedure FlushMetaData;
  private
    FMetaDataChanged: boolean;
    FMasterBlock: PDDFDataBlockHeaderStart;
    function MetaData: IdwlParams;
  strict protected
    procedure MetaDataChanged(Sender: IdwlParams; const Key: string; const Value: TValue);
  protected
    FCursor: IdwlCursor_Write;
  public
    constructor Create(Cursor: IdwlCursor_Write; MasterBlock: PDDFDataBlockHeaderStart; DDF: TDDF4);
    destructor Destroy; override;
  end;

  TDDFPage3 = class(TMetaDataObject, IdwlDDFPage)
  strict private
    FDDF: IdwlDDF;
    FPageHeader: PDDFPageHeader3;
    function DDF: TDDF3;
    function TileByteSize: cardinal;
  private
    function GetData(TileCol, TileRow: cardinal; Destination: PByte): boolean;
    function GetDim(TileCol, TileRow: cardinal): TdwlGridDim;
    function DataDefinition: TddfDataDefinition;
    procedure PutGridData(TileCol, TileRow: cardinal; Dim: TdwlGridDim; SourceSize: cardinal; SourcePtr: PByte);
  strict protected
  public
    constructor Create(ADDF: IdwlDDF);
  end;

  TDDFPage4 = class(TMetaDataObject, IdwlDDFPage)
  strict private
    FDDF: IdwlDDF;
    FPageHeader: PDDFPageHeader4;
    function PageItemPtr(TileCol, TileRow: cardinal): PUInt64;
    function DDF: TDDF4;
  strict protected
    function TileByteSize: cardinal;
  private
    function DataDefinition: TddfDataDefinition;
    function GetData(TileCol, TileRow: cardinal; Destination: PByte): boolean;
    function GetDim(TileCol, TileRow: cardinal): TdwlGridDim;
    procedure PutGridData(TileCol, TileRow: cardinal; Dim: TdwlGridDim; SourceSize: cardinal; SourcePtr: PByte);
  public
    constructor Create(ADDF: IdwlDDF);
    destructor Destroy; override;
  end;

  TDDF3 = class(TMetaDataObject, IdwlDDF)
  strict private
  private
    const
      CompressionLevel: TCompressionLevel = clMax;
    var
      FHeader: PDDFFileContentHeader3;
    function AddPage(DataDefinition: TddfDataDefinition; OuterBounds: TdwlBounds; TilePixelCountX, TilePixelCountY: cardinal; WidthInTiles: cardinal=1; HeightInTiles: cardinal=1): IdwlDDFPage;
    function Page(PageNumber: cardinal): IdwlDDFPage;
    procedure RaiseWritingError;
  public
    constructor Create(Cursor: IdwlCursor_Write);
  end;

  TDDF4 = class(TMetaDataObject, IdwlDDF)
  strict private
    FPageOffsets: TList<UInt64>;
  private
    const
      CompressionLevel: TCompressionLevel = clMax;
    var
      FHeader: PDDFFileContentHeader4;
    function AddPage(DataDefinition: TddfDataDefinition; OuterBounds: TdwlBounds; TilePixelCountX, TilePixelCountY: cardinal; WidthInTiles: cardinal=1; HeightInTiles: cardinal=1): IdwlDDFPage;
    procedure DeleteDataBlock;
    function GetNewDataBlock(const Size: cardinal; ContentType: word; Compression: byte; MasterID: cardinal): PDDFDataBlockHeader4;
    function Page(PageNumber: cardinal): IdwlDDFPage;
  public
    constructor Create(Cursor: IdwlCursor_Write);
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
    Result := TDDF4.Create(CursoredIO.GetWriteCursor)
  else
  begin
    if FileVersion=FILEVERSION_3 then
      Result := TDDF3.Create(CursoredIO.GetWriteCursor)
    else
      raise Exception.Create('File version '+FileVersion.ToString+' not supported');
  end;
end;

function New_DDF(const FileName: string; Options: TdwlFileOptions=[]): IdwlDDF;
begin
  Result := New_DDF(New_CursoredIO(FileName, Options));
end;


{ TDDF3 }

function TDDF3.AddPage(DataDefinition: TddfDataDefinition; OuterBounds: TdwlBounds; TilePixelCountX, TilePixelCountY: cardinal; WidthInTiles: cardinal=1; HeightInTiles: cardinal=1): IdwlDDFPage;
begin
  RaiseWritingError;
end;

constructor TDDF3.Create(Cursor: IdwlCursor_Write);
begin
  if PDDFFileIdentifier3(Cursor.CursorPtr).Signature<>DDF_SIGNATURE then
    raise Exception.Create('Invalid signature');
  FHeader := PDDFFileContentHeader3(Cursor.CursorPtr+SizeOf(TDDFFileIdentifier3));
  if FHeader.CoordinateSystem<>ddfcrdEPSG4326 then
    raise Exception.Create('Unknown CoordinateSystem');
  if FHeader.TotalPageCount<>1 then
    raise Exception.Create('Version 3 reading only supports single page files');
  Cursor.Seek(FHeader.FirstDataPageOffset);
  if (PDDFPageHeader3(Cursor.CursorPtr).WidthInTiles<>1) or
    (PDDFPageHeader3(Cursor.CursorPtr).WidthInTiles<>1) then
    raise Exception.Create('Version 3 reading only supports single tile files');
  inherited Create(Cursor, PDDFDataBlockHeaderStart(FHeader), nil);
end;

function TDDF3.Page(PageNumber: cardinal): IdwlDDFPage;
begin
  if PageNumber<>0 then
    raise Exception.Create('Version 3 reading only supports single page files');
  FCursor.Seek(FHeader.FirstDataPageOffset);
  Result := TDDFPage3.Create(Self);
end;

procedure TDDF3.RaiseWritingError;
begin
  raise Exception.Create('DDF writing is disabled for legacy versions');
end;

{ TDDFPage3 }

constructor TDDFPage3.Create(ADDF: IdwlDDF);
begin
  FDDF := ADDF;
  FPageHeader := PDDFPageHeader3(DDF.FCursor.CursorPtr);
  inherited Create(DDF.FCursor, PDDFDataBlockHeaderStart(FPageHeader), nil)
end;

function TDDFPage3.DataDefinition: TddfDataDefinition;
begin
  Result.DataType := FPageHeader.DataType;
  Result.Base10Component := FPageHeader.Base10Exponent;
  Result.NoDataValueUsed := FPageHeader.MaxUsedAsNoDataValue;
  if Result.NoDataValueUsed then
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

function TDDFPage3.DDF: TDDF3;
begin
  Result := TDDF3(FDDF);
end;

function TDDFPage3.GetData(TileCol, TileRow: cardinal; Destination: PByte): boolean;
begin
  FCursor.Seek(PByte(FPageHeader));
  FCursor.Seek(SizeOf(TDDFPageHeader3), soCurrent);
  FCursor.Seek(PCardinal(FCursor.CursorPtr)^);
  var DataBlock := PDDFDataBlockHeader3(FCursor.CursorPtr);
  if DataBlock.ContentType<>dbctArray3 then
    raise Exception.Create('In version 3 only Array datablocks are allowed');
  FCursor.Seek(SizeOf(TDDFDataBlockHeader3), soCurrent);
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
    Move(FCursor.CursorPtr^, Destination^, PayloadSize);
  else
    raise Exception.Create('Unknow Compression');
  end;
  Result := true;
end;

function TDDFPage3.GetDim(TileCol, TileRow: cardinal): TdwlGridDim;
begin
  Result.WidthInPixels := FPageHeader.TileWidthX;
  Result.HeightInPixels := FPageHeader.TileHeightY;
  Result. ScaleGridToWorld := DDF.FHeader.DefinedBounds.XExtent / (FPageHeader.TileWidthX-1);
  Result.LeftWorldX := DDF.FHeader.DefinedBounds.XMin;
  Result.TopWorldY := DDF.FHeader.DefinedBounds.YMax;
end;

procedure TDDFPage3.PutGridData(TileCol, TileRow: cardinal; Dim: TdwlGridDim; SourceSize: cardinal; SourcePtr: PByte);

begin
  DDF.RaiseWritingError;
end;

function TDDFPage3.TileByteSize: cardinal;
begin
  Result := DataTypeByteSizes[FPageHeader.DataType] * FPageHeader.TileWidthX * FPageHeader.TileHeightY;
end;

{ TDDF4 }

function TDDF4.AddPage(DataDefinition: TddfDataDefinition; OuterBounds: TdwlBounds; TilePixelCountX, TilePixelCountY: cardinal; WidthInTiles: cardinal=1; HeightInTiles: cardinal=1): IdwlDDFPage;
begin
  var GlobalBounds := FHeader.OuterBounds;
  GlobalBounds.Update(OuterBounds);
  FHeader.OuterBounds := GlobalBounds;
  GetNewDataBlock(SizeOf(TDDFPageHeader4)+(WidthInTiles*HeightInTiles)*SizeOf(UInt64), dbctPageHeader4, COMPRESSION_NONE, FHeader.DataBlockHeader.ClientID);
  var PageHeader := PDDFPageheader4(FCursor.CursorPtr);
  PageHeader.OuterBounds := OuterBounds;
  PageHeader.DataDefinition := DataDefinition;
  PageHeader.TileWidthX := TilePixelCountX;
  PageHeader.TileHeightY := TilePixelCountY;
  PageHeader.WidthInTiles := WidthInTiles;
  PageHeader.HeightinTiles := HeightInTiles;
  Result := TDDFPage4.Create(Self);

  FPageOffsets.Add(FCursor.CursorOffset);
  if FPageOffsets.Count=1 then
    FHeader.FirstDataPageOffset := FCursor.CursorOffset
  else
  begin
    var NewOffset := FCursor.CursorOffset;
    FCursor.Seek(FPageOffsets[FHeader.TotalPageCount-1]);
    PDDFPageheader4(FCursor.CursorPtr).NextDataPageOffset := NewOffset;
  end;
  inc(FHeader.TotalPageCount);
end;

constructor TDDF4.Create(Cursor: IdwlCursor_Write);
begin
  // we need to call some functions that use FCursor
  // before we do an inherited Create,
  // so init it also here..
  FCursor := Cursor;
  // initialize header
 if FCursor.Size=0 {New File: initialize structure} then
  begin
    FCursor.SetSize(SizeOf(TDDFFileIdentifier4));
    var FileIdent := PDDFFileIdentifier4(FCursor.CursorPtr);
    FileIdent.Version := FILEVERSION_4;
    FileIdent.Signature := DDF_SIGNATURE;
    FileIdent.FirstDataBlockOffset := SizeOf(TDDFFileIdentifier4);
    FileIdent.Reserved1 := 0;
    FileIdent.Reserved2 := 0;
    FHeader := PDDFFileContentHeader4(GetNewDataBlock(SizeOf(TDDFFileContentHeader4), dbctFileContent4, COMPRESSION_NONE, 0));
    FHeader.LastUsedClientID := FHeader.DataBlockHeader.ClientID;
    FHeader.CoordinateSystem := ddfcrdEPSG4326;
    FHeader.OuterBounds := EmptyBounds;
  end
  else
  begin
    var FileIdent := PDDFFileIdentifier4(FCursor.CursorPtr);
    if FileIdent.Version<>FILEVERSION_4 then
      raise Exception.Create('FileVersion '+FileIdent.Version.ToString+' is not supported');
    if FileIdent.Signature<>DDF_SIGNATURE then
      raise Exception.Create('Invalid signature');
    FCursor.Seek(FileIdent.FirstDataBlockOffset);
    FHeader := PDDFFileContentHeader4(FCursor.CursorPtr);
    if FHeader.CoordinateSystem<>ddfcrdEPSG4326 then
      raise Exception.Create('Unknown CoordinateSystem');
  end;
  FCursor.RegisterMemoryPointer(@FHeader);
  inherited Create(Cursor, PDDFDataBlockHeaderStart(FHeader), Self);
  // initialize pages
  FPageOffsets := TList<UInt64>.Create;
  var PageOffset := FHeader.FirstDataPageOffset;
  for var PageNo := 1 to FHeader.TotalPageCount do
  begin
    FPageOffsets.Add(PageOffset);
    FCursor.Seek(PageOffset);
    PageOffset := PDDFPageHeader4(FCursor.CursorPtr).NextDataPageOffset;
  end;
end;

destructor TDDF4.Destroy;
begin
  FCursor.UnRegisterMemoryPointer(@FHeader);
  FPageOffsets.Free;
  inherited Destroy;
end;

procedure TDDF4.DeleteDataBlock;
begin
  PDDFDataBlockHeader4(FCursor.CursorPtr).ContentType := dbctDeleted4;
  PDDFDataBlockHeader4(FCursor.CursorPtr).MasterID := 0;
end;

function TDDF4.GetNewDataBlock(const Size: cardinal; ContentType: word; Compression: byte; MasterID: cardinal): PDDFDataBlockHeader4;
begin
  FCursor.AllocateBlock((((Size-1) div 16)+1)*16);
  Result := PDDFDataBlockHeader4(FCursor.CursorPtr);
  FillChar(Result^, SizeOf(TDDFDataBlockHeader4), 0);
  Result.ContentType := ContentType;
  Result.Compression := Compression;
  Result.MasterID := MasterID;
  if FHeader=nil  then
    Result.ClientID := 1
  else
  begin
    inc(FHeader.LastUsedClientID);
    Result.ClientID := FHeader.LastUsedClientID
  end;
  Result.Size := Size;
end;

function TDDF4.Page(PageNumber: cardinal): IdwlDDFPage;
begin
  FCursor.Seek(FPageOffsets[PageNumber]);
  Result := TDDFPage4.Create(Self);
end;

{ TDDFPage4 }

constructor TDDFPage4.Create(ADDF: IdwlDDF);
begin
  FDDF := ADDF;
  FPageHeader := PDDFPageHeader4(DDF.FCursor.CursorPtr);
  DDF.FCursor.RegisterMemoryPointer(@FPageHeader);
  inherited Create(DDF.FCursor, PDDFDataBlockHeaderStart(FPageHeader), DDF);
end;

function TDDFPage4.DataDefinition: TddfDataDefinition;
begin
  Result := FPageHeader.DataDefinition;
end;

function TDDFPage4.DDF: TDDF4;
begin
  Result := TDDF4(FDDF);
end;

destructor TDDFPage4.Destroy;
begin
  FCursor.UnRegisterMemoryPointer(@FPageHeader);
  inherited Destroy;
end;

function TDDFPage4.GetData(TileCol, TileRow: cardinal; Destination: PByte): boolean;
begin
  FCursor.Seek(PageItemPtr(TileCol, TileRow)^);
  var GridDataHeader := PDDFGridDataHeader(FCursor.CursorPtr);
  if GridDataHeader.DataBlockHeader.ContentType<>dbctGridData then
    raise Exception.Create('Expected GridData Block');
  FCursor.Seek(SizeOf(TDDFGridDataHeader), soCurrent);
  var PayloadSize := TileByteSize;
  var SizeInBlock := GridDataHeader.DataBlockHeader.Size-SizeOf(TDDFGridDataHeader);
  case GridDataHeader.DataBlockHeader.Compression of
  COMPRESSION_ZLIB:
    begin
      var DecompBuf: Pointer;
      var DecompSize: integer;
      ZDecompress(PByte(FCursor.CursorPtr),  SizeInBlock, DecompBuf, DecompSize);
      try
        if DecompSize<>integer(PayloadSize) then
          raise Exception.Create('payload Size error while reading data');
        Move(DecompBuf^, Destination^, PayloadSize);
      finally
        FreeMem(DecompBuf);
      end;
    end;
  COMPRESSION_NONE:
    begin
      if SizeInBlock<>PayloadSize then
        raise Exception.Create('payload Size error while reading data');
      Move(FCursor.CursorPtr^, Destination^, PayloadSize);
    end
  else
    raise Exception.Create('Unknow Compression');
  end;
  Result := true;
end;

function TDDFPage4.GetDim(TileCol, TileRow: cardinal): TdwlGridDim;
begin
  FCursor.Seek(PageItemPtr(TileCol, TileRow)^);
  Result := PDDFGridDataHeader(FCursor.CursorPtr).Dim;
end;

function TDDFPage4.PageItemPtr(TileCol, TileRow: cardinal): PUInt64;
begin
  Result := PUInt64(PByte(FPageHeader)+SizeOf(TDDFPageHeader4)+(TileRow*FPageHeader.WidthInTiles+TileCol)*SizeOf(UInt64));
end;

procedure TDDFPage4.PutGridData(TileCol, TileRow: cardinal; Dim: TdwlGridDim; SourceSize: cardinal; SourcePtr: PByte);
begin
  if DataTypeByteSizes[FPageHeader.DataDefinition.DataType]*FPageHeader.TileWidthX*FPageHeader.TileHeightY<>SourceSize then
    raise Exception.Create('Inconstistent Source Data Size in PutGridData');
  var CompBuf: pointer;
  var CompSize: integer;
  ZCompress(SourcePtr, SourceSize, CompBuf, CompSize, DDF.CompressionLevel);
  try
    DDF.GetNewDataBlock(SizeOf(TDDFGridDataHeader)+CompSize, dbctGridData, COMPRESSION_ZLIB, FPageHeader.DataBlockHeader.ClientID);
    PDDFGridDataHeader(FCursor.CursorPtr).Dim := Dim;
    // put offset of this new block in Pageheader
    PageItemPtr(TileCol, TileRow)^ := FCursor.CursorOffset;
    // put compressed data in file
    FCursor.Seek(SizeOf(TDDFGridDataHeader), soCurrent);
    FCursor.Write(CompBuf^, CompSize);
  finally
    FreeMem(CompBuf);
  end;
end;

function TDDFPage4.TileByteSize: cardinal;
begin
  Result := DataTypeByteSizes[FPageHeader.DataDefinition.DataType] * FPageHeader.TileWidthX * FPageHeader.TileHeightY;
end;

{ TMetaDataObject }

constructor TMetaDataObject.Create(Cursor: IdwlCursor_Write; MasterBlock: PDDFDataBlockHeaderStart; DDF: TDDF4);
begin
  inherited Create;
  FDDF :=  DDF;
  FCursor := Cursor;
  FMasterBlock := MasterBlock;
  if FDDF<>nil then
    FCursor.RegisterMemoryPointer(@FMasterBLock);
end;

destructor TMetaDataObject.Destroy;
begin
  FlushMetaData;
  if FDDF<>nil then
    FCursor.UnRegisterMemoryPointer(@FMasterBlock);
  inherited Destroy;
end;

procedure TMetaDataObject.FlushMetaData;
var
  Buf: PAnsiChar;
  CurBufSize: cardinal;
  function EncodeStringForStream(const S: string): cardinal;
  begin
    Result := WideCharToMultiByte(FCursor.CodePage, 0, PWideChar(S), -1, Buf, CurBufSize, nil, nil);
    if Result<1 then
    begin
      if GetLastError=ERROR_INSUFFICIENT_BUFFER then
      begin
        CurBufSize := WideCharToMultiByte(FCursor.CodePage, 0, PWideChar(S), -1, nil, 0, nil, nil);
        FreeMem(Buf);
        GetMem(Buf, CurBufSize);
        Result := WideCharToMultiByte(FCursor.CodePage, 0, PWideChar(S), -1, Buf, CurBufSize, nil, nil){Subtract the trailing zero};
      end
      else
        Result := 0;
      if Result<1 then
        raise Exception.Create('Error encoding MultiByte string');
    end;
    dec(Result); // eliminate trailing 0 terminated character.
  end;
begin
  if not FMetaDataChanged then
    Exit;
  // clear current metadata
  if PDDFDataBlockHeader4(FMasterBlock).MetaDataOffSet<>0 then
  begin
    FCursor.Seek(PDDFDataBlockHeader4(FMasterBlock).MetaDataOffSet);
    FDDF.DeleteDataBlock;
    PDDFDataBlockHeader4(FMasterBlock).MetaDataOffSet := 0;
  end;
  CurBufSize := 512;
  GetMem(Buf, CurBufSize);
  var MemStream := TMemoryStream.Create;
  try
    var Enum := MetaData.GetEnumerator;
    while ENum.MoveNext do
    begin
      var Cnt := EncodeStringForStream(Enum.CurrentKey);
      MemStream.Write(Cnt, 1);
      MemStream.Write(Buf^, Cnt);
      Cnt := EncodeStringForStream(Enum.CurrentValue.AsString);
      MemStream.Write(Cnt, 2);
      MemStream.Write(Buf^, Cnt);
    end;
    if MemStream.Size=0 then // no metadata present
      Exit;
    // write trailing zero
    var Zero: byte := 0;
    MemStream.Write(Zero, 1);

    var CompBuf: pointer;
    var CompSize: integer;
    ZCompress(MemStream.Memory, MemStream.Size, CompBuf, CompSize, FDDF.CompressionLevel);
    try
      FDDF.GetNewDataBlock(SizeOf(TDDFDataBlockHeader4)+CompSize, dbctMetaData4, COMPRESSION_ZLIB, PDDFDataBlockHeader4(FMasterBlock).ClientID);
      PDDFDataBlockHeader4(FMasterBlock).MetaDataOffSet := FCursor.CursorOffset;
      FCursor.Seek(SizeOf(TDDFDataBlockHeader4), soCurrent);
      //write composed datablock into file
      FCursor.Write(CompBuf^, CompSize);
    finally
      FreeMem(CompBuf);
    end;
  finally
    MemStream.Free;
    FreeMem(Buf);
  end;
end;

function TMetaDataObject.MetaData: IdwlParams;
begin
  if FMetaData=nil then
  begin
    var ImportKeys: boolean;
    var MetaDataSize: cardinal := 0;
    var IsCompressed := false;
    FMetaData := New_Params;
    var IsVersion3 := FDDF=nil;
    if IsVersion3 then
    begin
      var Offset := PDDFDataBlockHeader3(FMasterBlock).MetaDataOffSet;
      ImportKeys := Offset<>0;
      if ImportKeys then
      begin
        FCursor.Seek(Offset);
        MetaDataSize := PDDFDataBlockHeader3(FCursor.CursorPtr).Size-SizeOf(TDDFDataBlockHeader3);
        IsCompressed := PDDFDataBlockHeader3(FCursor.CursorPtr).Compression=COMPRESSION_ZLIB;
        FCursor.Seek(SizeOf(TDDFDataBlockHeader3), soCurrent);
      end;
    end
    else
    begin
      var Offset := PDDFDataBlockHeader4(FMasterBlock).MetaDataOffSet;
      ImportKeys := Offset<>0;
      if ImportKeys then
      begin
        FCursor.Seek(Offset);
        MetaDataSize := PDDFDataBlockHeader4(FCursor.CursorPtr).Size-SizeOf(TDDFDataBlockHeader4);
        IsCompressed := PDDFDataBlockHeader4(FCursor.CursorPtr).Compression=COMPRESSION_ZLIB;
        FCursor.Seek(SizeOf(TDDFDataBlockHeader4), soCurrent);
      end;
    end;
    if ImportKeys then
    begin
      if IsCompressed then
      begin
        var DecompBuf: PByte;
        var DecompSize: integer;
        try
          ZDecompress(PByte(FCursor.CursorPtr), MetaDataSize, pointer(DecompBuf), DecompSize);
          var P := DecompBuf;
          var ByteLen := P^;
          while ByteLen>0 do
          begin
            inc(P);
            var StrLen := MultiByteToWideChar(FCursor.CodePage, 0, PAnsiChar(P), ByteLen, nil, 0);
            var Key: string;
            SetLength(Key, StrLen);
            MultiByteToWideChar(FCursor.CodePage, 0, PAnsiChar(P), ByteLen, PWideChar(Key), StrLen);
            inc(P, ByteLen);
            var WordLen := PWord(P)^;
            inc(P, 2);
            StrLen := MultiByteToWideChar(FCursor.CodePage, 0, PAnsiChar(P), WordLen, nil, 0);
            var Value: string;
            SetLength(Value, StrLen);
            MultiByteToWideChar(FCursor.CodePage, 0, PAnsiChar(P), WordLen, PWideChar(Value), StrLen);
            inc(P, WordLen);
            FMetaData.WriteValue(Key, Value);
            ByteLen := P^;
          end;
        finally
          FreeMem(DecompBuf);
        end;
      end
      else
      begin
        while true do
        begin
          var Key := FCursor.ReadString_LenByte;
          if Key='' then
            Break;
          FMetaData.WriteValue(Key, FCursor.ReadString_LenWord);
        end;
      end;
    end;
    if not IsVersion3 then
      FMetaData.EnableChangeTracking(MetaDataChanged);
  end;
  Result := FMetaData;
end;

procedure TMetaDataObject.MetaDataChanged(Sender: IdwlParams; const Key: string; const Value: TValue);
begin
  FMetaDataChanged := true;
end;

{ TddfDataDefinition }

class function TddfDataDefinition.Create(ADataType: word; ABase10Component: shortint; ANoDataValueUsed: boolean; ANoDataValue: Int64): TddfDataDefinition;
begin
  Result.DataType := ADataType;
  Result.Base10Component := ABase10Component;
  Result.NoDataValueUsed := ANoDataValueUsed;
  Result.NoDataValue := ANoDataValue;
  Result.Reserved4 := 0;
end;

end.

