unit DWL.IO.DDF;

interface

uses
  DWL.IO, DWL.Types;

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
  IdwlDDFPage = interface
    procedure PutData(TileCol, TileRow: cardinal; Data: PByte);

    procedure SetMetaData(const Key, Value: string);
    function GetMetaData(const Key: string): string;
    property MetaData[const Key: string]: string read GetMetaData write SetMetaData;
  end;

  IdwlDDF = interface
    procedure SetMetaData(const Key, Value: string);
    function GetBounds: TdwlBounds;
    function GetMetaData(const Key: string): string;
    procedure SetBounds(const Bounds: TdwlBounds);
    function AddPage(TilePixelCountX, TilePixelCountY: cardinal; DataType: word=ddfDouble; WidthInTiles: cardinal=1; HeightInTiles: cardinal=1; MaxUsedAsNoDataValue: boolean=false; Base10Exponent: shortint=0): IdwlDDFPage;
    function Page(PageNumber: cardinal): IdwlDDFPage;
    property Bounds: TdwlBounds read GetBounds write SetBounds;
    property MetaData[const Key: string]: string read GetMetaData write SetMetaData;
  end;

function New_DDF(CursoredIO: IdwlCursoredIO): IdwlDDF;

implementation

uses
  System.SysUtils, System.Generics.Collections, System.Classes,
  System.ZLib;

const
  CURRENT_FILEVERSION  = 3;
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
  PDDFDataBlockHeader = ^TDDFDataBlockHeader;
  TDDFDataBlockHeader = packed record
    Size: cardinal; {including this header}
    ContentType: word;  // PageHeader, Metadata, Array, ArrayWithDataStarts
    Compression: byte; // for now only used with array or array with datastarts
    Flags: byte;  // $1: Deleted
    MasterID: cardinal; // unused
    ClientID: cardinal; //unused
    MetaDataOffSet: cardinal;
  end;

  PDDFFileIdentifier = ^ TDDFFileIdentifier;
  TDDFFileIdentifier = packed record
    Version: byte;
    Signature: word;
    Reserved0: byte;
  end;

  PDDFHeader = ^TDDFHeader;
  TDDFHeader = packed record
    DataBlockHeader: TDDFDataBlockHeader;
    FirstDataPageOffset: cardinal;
    PageCount: cardinal;
    CoordinateSystem: cardinal; // 4326 // EPGS coordinate system definition
    DefinedBounds: TdwlBounds; // of the pointdata bounds, not the outer side of the pixels
    ActualBounds: TdwlBounds; // of the pointdata bounds, not the outer side of the pixels
    Reserved1: cardinal;
  end;

  PDDFPageheader =^TDDFPageHeader;
  TDDFPageHeader = packed record
    DataBlockHeader: TDDFDataBlockHeader;

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

    // followed by ItemOffsets
  end;

type
  TDDF=class;

  TDDFMetaDataDictionary = class
    FChanged: boolean;
    FPairs: TDictionary<string, string>;
  private
    procedure ReadMetaDataFromDataBlock(Cursor: IdwlCursor_Write; Block: PDDFDataBlockHeader);
    function GetMetaData(const Key: string): string;
    procedure SetMetaData(const Key, Value: string);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TDDFPage = class(TInterfacedObject, IdwlDDFPage)
  strict private
    FDDF: TDDF;
    FPageHeader: PDDFPageHeader;
    FMetaData: TDDFMetaDataDictionary;
    function PageItemPtr(TileCol, TileRow: cardinal): PCardinal;
    function MetaData: TDDFMetaDataDictionary;
  private
    procedure PutData(TileCol, TileRow: cardinal; Data: PByte);
    function GetMetaData(const Key: string): string;
    procedure SetMetaData(const Key, Value: string);
  public
    constructor Create(DDF: TDDF; PageOffset: cardinal);
    destructor Destroy; override;
  end;

  TDDF = class(TInterfacedObject, IdwlDDF)
  strict private
    FPageOffsets: TList<cardinal>;
    FHeader: PDDFHeader;
    FMetaData: TDDFMetaDataDictionary;
    procedure Initialize;
    function MetaData: TDDFMetaDataDictionary;
  private
    const
      CompressionLevel: TCompressionLevel = clMax;
    var
      FCursor: IdwlCursor_Write;
    function AddPage(TilePixelCountX, TilePixelCountY: cardinal; DataType: word=ddfDouble; WidthInTiles: cardinal=1; HeightInTiles: cardinal=1; MaxUsedAsNoDataValue: boolean=false; Base10Exponent: shortint=0): IdwlDDFPage;
    function GetBounds: TdwlBounds;
    function GetNewDataBlock(const Size: UInt64; ContentType: word; Compression: byte): PDDFDataBlockHeader;
    function Page(PageNumber: cardinal): IdwlDDFPage;
    function GetMetaData(const Key: string): string;
    procedure SetMetaData(const Key, Value: string);
    procedure SetBounds(const Bounds: TdwlBounds);
  public
    constructor Create(CursoredIO: IdwlCursoredIO);
    destructor Destroy; override;
  end;

function New_DDF(CursoredIO: IdwlCursoredIO): IdwlDDF;
begin
  Result := TDDF.Create(CursoredIO);
end;

{ TDDF }

function TDDF.AddPage(TilePixelCountX, TilePixelCountY: cardinal; DataType: word=ddfDouble; WidthInTiles: cardinal=1; HeightInTiles: cardinal=1; MaxUsedAsNoDataValue: boolean=false; Base10Exponent: shortint=0): IdwlDDFPage;
begin
  GetNewDataBlock(SizeOf(TDDFPageHeader)+(WidthInTiles*HeightInTiles)*SizeOf(cardinal), dbctPageHeader, COMPRESSION_NONE);
  var PageHeader := PDDFPageheader(FCursor.CursorPtr);

  PageHeader.DataType := word(DataType);
  PageHeader.Base10Exponent := Base10Exponent;
  PageHeader.MaxUsedAsNoDataValue := byte(MaxUsedAsNoDataValue);
  PageHeader.TileColCount := TilePixelCountX;
  PageHeader.TileRowCount := TilePixelCountY;
  PageHeader.WidthInTiles := WidthInTiles;
  PageHeader.HeightinTiles := HeightInTiles;
  Result := TDDFPage.Create(Self, FCursor.CursorOffset);

  if FPageOffsets.Count=0 then
    FHeader.FirstDataPageOffset := FCursor.CursorOffset
  else
  begin
    var NewOffset := FCursor.CursorOffset;
    FCursor.Seek(FPageOffsets[FHeader.PageCount-1]);
    PDDFPageheader(FCursor.CursorPtr).NextDataPageOffset := NewOffset;
  end;
  inc(FHeader.PageCount);
end;

constructor TDDF.Create(CursoredIO: IdwlCursoredIO);
begin
  inherited Create;
  FPageOffsets := TList<cardinal>.Create;
  FCursor := CursoredIO.GetWriteCursor;
  Initialize;
end;

destructor TDDF.Destroy;
begin
  FMetaData.Free;
  FPageOffsets.Free;
  inherited Destroy;
end;

function TDDF.GetBounds: TdwlBounds;
begin
  Result := FHeader.ActualBounds;
end;

function TDDF.GetMetaData(const Key: string): string;
begin
  Result := MetaData.GetMetaData(Key);
end;

function TDDF.GetNewDataBlock(const Size: UInt64; ContentType: word; Compression: byte): PDDFDataBlockHeader;
begin
  FCursor.AllocateBlock(Size);
  Result := PDDFDataBlockHeader(FCursor.CursorPtr);
  FillChar(Result^, SizeOf(TDDFDataBlockHeader), 0);
  Result.ContentType := ContentType;
  Result.Compression := Compression;
  Result.Size := Size;
end;

procedure TDDF.Initialize;
begin
  FCursor.Seek(0);
  FHeader := PDDFHeader(FCursor.CursorPtr+SizeOf(TDDFFileIdentifier));
  FCursor.RegisterMemoryPointer(@FHeader);
  // initialize header
  if FCursor.Size=0 {New File: initialize structure} then
  begin
    FCursor.SetSize(SizeOf(TDDFFileIdentifier)+SizeOf(TDDFHeader));
    var FileIdent := PDDFFileIdentifier(FCursor.CursorPtr);
    FileIdent.Version := CURRENT_FILEVERSION;
    FileIdent.Signature := DDF_SIGNATURE;
    FileIdent.Reserved0 := 0;
    FHeader.DataBlockHeader.Size := SizeOf(TDDFHeader);
    FHeader.DataBlockHeader.ContentType := dbctHeader;
    FHeader.DataBlockHeader.Compression := COMPRESSION_NONE;
    FHeader.DataBlockHeader.Flags := 0;
    FHeader.DataBlockHeader.MasterID := 0;
    FHeader.DataBlockHeader.ClientID := 0;
    FHeader.DataBlockHeader.MetaDataOffSet := 0;
    FHeader.FirstDataPageOffset := 0;
    FHeader.PageCount := 0;
    FHeader.CoordinateSystem := ddfcrdEPSG4326;
    FHeader.DefinedBounds := EmptyBounds;
    FHeader.ActualBounds := EmptyBounds;
    FHeader.Reserved1 := 0;
  end
  else
  begin
    var FileIdent := PDDFFileIdentifier(FCursor.CursorPtr);
    if FileIdent.Version<>CURRENT_FILEVERSION then
      raise Exception.Create('FileVersion '+FileIdent.Version.ToString+' is not supported');
    if FileIdent.Signature<>DDF_SIGNATURE then
      raise Exception.Create('Invalid signature');
    if FHeader.CoordinateSystem<>ddfcrdEPSG4326 then
      raise Exception.Create('Unknown CoordinateSystem');
  end;
  // initialize pages
  var PageOffset := FHeader.FirstDataPageOffset;
  for var PageNo := 1 to FHeader.PageCount do
  begin
    FPageOffsets.Add(PageOffset);
    FCursor.Seek(PageOffset);
    PageOffset := PDDFPageHeader(FCursor.CursorPtr).NextDataPageOffset;
  end;
end;

function TDDF.MetaData: TDDFMetaDataDictionary;
begin
  if FMetaData=nil then
  begin
    FMetaData := TDDFMetaDataDictionary.Create;
    FMetaData.ReadMetaDataFromDataBlock(FCursor, PDDFDataBlockHeader(@FHeader.DataBlockHeader));
  end;
  Result := FMetaData;
end;

function TDDF.Page(PageNumber: cardinal): IdwlDDFPage;
begin
  Result := TDDFPage.Create(Self, FPageOffsets[PageNumber]);
end;

procedure TDDF.SetBounds(const Bounds: TdwlBounds);
begin
  FHeader.ActualBounds := Bounds;
  FHeader.DefinedBounds := Bounds;
end;

procedure TDDF.SetMetaData(const Key, Value: string);
begin
  MetaData.SetMetaData(Key, Value);
end;

{ TDDFPage }

constructor TDDFPage.Create(DDF: TDDF; PageOffset: cardinal);
begin
  inherited Create;
  FDDF := DDF;
  // we keep the DDF here as a object, not as an interface
  // to be able to use 'internal functions'
  // So do manual reference counting
  FDDF._AddRef;
  FDDF.FCursor.Seek(PageOffset);
  FPageHeader := PDDFPageHeader(FDDF.FCursor.CursorPtr);
  FDDF.FCursor.RegisterMemoryPointer(@FPageHeader);
end;

destructor TDDFPage.Destroy;
begin
  FDDF.FCursor.UnRegisterMemoryPointer(@FPageHeader);
  // because we did a manual AddRef, Release it on destroy
  FDDF._Release;
  FMetaData.Free;
  inherited Destroy;
end;

function TDDFPage.GetMetaData(const Key: string): string;
begin
  Result := MetaData.GetMetaData(Key);
end;

function TDDFPage.MetaData: TDDFMetaDataDictionary;
begin
  if FMetaData=nil then
  begin
    FMetaData := TDDFMetaDataDictionary.Create;
    FMetadata.ReadMetaDataFromDataBlock(FDDF.FCursor, @(FPageHeader.DataBlockHeader));
  end;
  Result := FMetaData;
end;

function TDDFPage.PageItemPtr(TileCol, TileRow: cardinal): PCardinal;
begin
  Result := PCardinal(PByte(FPageHeader)+SizeOf(TDDFPageHeader)+(TileRow*FPageHeader.TileColCount*TileRow+TileCol)*SizeOf(cardinal));
end;

procedure TDDFPage.PutData(TileCol, TileRow: cardinal; Data: PByte);
begin
  var DataSize :=  DataTypeByteSizes[FPageHeader.DataType]*FPageHeader.TileColCount*FPageHeader.TileRowCount;
  var CompBuf: pointer;
  var CompSize: integer;
  ZCompress(Data, DataSize, CompBuf, CompSize, FDDF.CompressionLevel);
  try
    FDDF.GetNewDataBlock(SizeOf(TDDFDataBlockHeader)+CompSize, dbctArray, COMPRESSION_ZLIB);
    // put offset of this new block in Pageheader
    PageItemPtr(TileCol, TileRow)^ := FDDF.FCursor.CursorOffset;
    // put compressed data in file
    FDDF.FCursor.Seek(SizeOf(TDDFDataBlockHeader), soCurrent);
    Move(CompBuf^, FDDF.FCursor.CursorPtr^, CompSize);
  finally
    FreeMem(CompBuf);
  end;
end;

procedure TDDFPage.SetMetaData(const Key, Value: string);
begin
  MetaData.SetMetaData(key, Value);
end;

{ TDDFMetaDataDictionary }

constructor TDDFMetaDataDictionary.Create;
begin
  inherited Create;
  FPairs := TDictionary<string, string>.Create;
end;

destructor TDDFMetaDataDictionary.Destroy;
begin
  FPairs.Free;
  inherited Destroy;
end;

function TDDFMetaDataDictionary.GetMetaData(const Key: string): string;
begin
  if not FPairs.TryGetValue(Key, Result) then
    Result := '';
end;

procedure TDDFMetaDataDictionary.ReadMetaDataFromDataBlock(Cursor: IdwlCursor_Write; Block: PDDFDataBlockHeader);
begin
  FChanged := false;
  FPairs.Clear;
  if Block.MetaDataOffSet=0 then
    Exit;
  Cursor.Seek(Block.MetaDataOffSet);
  if PDDFDataBlockHeader(Cursor.CursorPtr).ContentType<>dbctMetaData then
    raise Exception.Create('Metadata block read error');
  if PDDFDataBlockHeader(Cursor.CursorPtr).Compression<>COMPRESSION_NONE then
    raise Exception.Create('Unknown compression while reading Metadata block');
  Cursor.Seek(SizeOf(TDDFDataBlockHeader), soCurrent);
  while true do
  begin
    var Str := Cursor.ReadString_LenByte;
    if Str='' then
      Break;
    FPairs.Add(Str, Cursor.ReadString_LenWord);
  end;
end;

procedure TDDFMetaDataDictionary.SetMetaData(const Key, Value: string);
begin
  var CurrValue: string;
  if FPairs.TryGetValue(Key, CurrValue) then
  begin
    if Value='' then
    begin
      FPairs.Remove(Key);
      FChanged := true;
    end
    else
    begin
      if Value<>CurrValue then
      begin
        FPairs.AddOrSetValue(Key, Value);
        FChanged := true;
      end;
    end;
  end
  else
  begin
    if Value<>'' then
    begin
      FPairs.Add(Key, Value);
      FChanged := true;
    end;
  end;
end;

end.
