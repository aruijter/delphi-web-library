unit DWL.IO.GeoTIFF;

interface

uses
  DWL.IO, DWL.GridData;

type
  IdwlGeoTIFF = interface
    function GetGridData: IdwlGridData;
  end;

function New_GeoTIFF(const FileName: string; Options: TdwlFileOptions=[]): IdwlGeoTIFF; overload;
function New_GeoTIFF(CursoredIO: IdwlCursoredIO): IdwlGeoTIFF; overload;

implementation

uses
  System.SysUtils, System.Generics.Collections, System.Rtti, DWL.Types,
  DWL.Math, System.Classes, System.Math, Winapi.Windows, System.ZLib, dsEasyXml;

const
  NaC: cardinal = $FFFFFFFF; // Not A Cardinal

  fillorderHiLo2LoHi = 1;
  fillorderLoLo2HiHi = 2;

  RasterPixelIsArea = 1;
  RasterPixelIsPoint = 2;

  COMPRESSION_NONE = 1;
  COMPRESSION_HUFFMANRUNLENGTH = 2;
  COMPRESSION_LZW = 5;
  COMPRESSION_ADOBE_DEFLATE = 8; // zlib
  COMPRESSION_DEFLATE = 32946; // gzip
  COMPRESSION_PACKBITS = 32773;

  PREDICTOR_NONE = 1;
  PREDICTOR_HORIZONTAL = 2;
  PREDICTOR_FLOATINGPOINT = 3;

  // SampleFormat
  sfUnsignedInteger = 1;
  sfSignedInteger = 2;
  sfFloatingPoint = 3;
  sfUndefined = 4;

  ftBYTE = 1;   // 8-bit unsigned integer
  ftASCII = 2;  // 8-bit byte that contains a 7-bit ASCII code; the last byte must be NUL (binary zero).
  ftSHORT = 3;  // 16-bit (2-byte) unsigned integer.
  ftLONG = 4;   // 32-bit (4-byte) unsigned integer.
  ftRATIONAL = 5; // Two LONGs: the first represents the numerator of a fraction; the second, the denominator.
  ftSBYTE = 6;  // An 8-bit signed (twos-complement) integer.
  ftUNDEFINED = 7; // An 8-bit byte that may contain anything, depending on the definition of the field.
  ftSSHORT = 8; // A 16-bit (2-byte) signed (twos-complement) integer.
  ftSLONG = 9; // A 32-bit (4-byte) signed (twos-complement) integer.
  ftSRATIONAL = 10; // Two SLONG’s: the first represents the numerator of a fraction, the second the denominator.
  ftFLOAT = 11; // Single precision (4-byte) IEEE format.
  ftDOUBLE = 12; // Double precision (8-byte) IEEE format.
  ftLONG8 = 16; // being unsigned 8byte integer
  ftSLONG8 = 17; // being signed 8byte integer
  ftIFD8 = 18; // being a new unsigned 8byte IFD offset.

  FieldTypeByteSizes: array[1..18] of byte = (1, 1, 2, 4, 8, 1, 1, 2, 4, 8, 4, 8, 0, 0, 0, 8, 8, 8);

const // LZW encoding and decoding support
  NoLZWCode = 4096;

type
  TUInt64Array = TArray<UInt64>;
  TLongWordArray = TArray<Longword>;

type
  TLzwDecoder = record
    class procedure DeCompress(const inBuffer: Pointer; inSize: Integer; const outBuffer: Pointer; outSize: Integer); static;
  end;

  TModelTiePoint = record
    I,J,K: double;
    X,Y,Z: double;
  end;

  TIFD = class
  private
    FMaxValueBytes: byte;
    FValues: TArray<TValue>;
    FFieldType: word;
    FCount: UInt64;
    FTag: word;
    function ReadValue(Cursor: IdwlCursor_Read): TValue;
    function ReadOffset(Cursor: IdwlCursor_Read): UInt64; virtual;
    procedure ReadValuesFromCursor(Cursor: IdwlCursor_Read);
    procedure ReadFromCursor(Cursor: IdwlCursor_Read); virtual;
    procedure ReadAsciiValues(Cursor: IdwlCursor_Read);
  public
    constructor Create;
  end;

  TBigIFD = class(TIFD)
    constructor Create;
    function ReadOffset(Cursor: IdwlCursor_Read): UInt64; override;
    procedure ReadFromCursor(Cursor: IdwlCursor_Read); override;
  end;

  TTagData = class
    FImageWidth: UInt64;   // tag 256
    FImageHeight: UInt64;  // tag 257 = ImageLength = no. of scanlines
    FRowsPerStrip: UInt64;     // The number of rows in each strip (except possibly the last strip.)
    FStripOffsetsCount: UInt64; // number of stripoffset (expect it to match ImageHeight div RowPerStrip (+1))
    FStripOffsets: TUInt64Array;    // For each strip, the byte offset of that strip
    FBitsPerSampleCount: word;
    FBitsPerSample: TArray<word>; // tag 258
    FCompression: word;  // tag 259
    FFillOrder: word;
    FSamplesPerPixel: word;
    FPlanarConfiguration: word;
    FStripByteCounts: TLongWordArray; // For each strip, the number of bytes in that strip after any compression.
    FStripByteCountsCount: UInt64;
    FPredictor: word;
    FTileHeight: UInt64;
    FTileWidth: UInt64;
    FTileOffsetsCount: UInt64;
    FTileOffsets: TUInt64Array;
    FTileByteCounts: TUInt64Array;
    FTileByteCountsCount: UInt64;
    FSampleFormat: word;
    FIsRGB: boolean;
    FTopLeftLon: double;     // tag 33922
    FTopLeftLat: double;
    FModelTiePoints: TArray<TModelTiePoint>;
    FScaleX: double;         // tag 33550
    FScaleY: double;
    FRasterType: integer;
    FIFDs: TObjectList<TIFD>;
    FNoDataValue: Int64;
    FNoDataValueUsed: boolean;
    FScaling: double;
    FOffset: double;
    function CreateEmptyGridData: IdwlGridData;
    procedure GetFieldTags(Cursor: IdwlCursor_Read);
    procedure ReadModelTiePoints(IFD: TIFD);
    procedure ReadModelPixelScaleTag(IFD: TIFD);
    procedure Initialize;
    function TileCountX: UInt64;
    function TileCountY: UInt64;
  public
    constructor Create(Cursor: IdwlCursor_Read);
    destructor Destroy; override;
  end;

  TGeoTiff = class(TInterfacedObject, IdwlGeoTIFF)
  strict private
    FCursor: IdwlCursor_Read;
    FTagData: TTagData;
    procedure InternalAdobeInflateStrip(GridCur: IdwlGridCursor_Write; Idx: integer; var StartRow: UInt64);
    procedure InternalAdobeInflateTile(GridCur: IdwlGridCursor_Write; Idx, TileRow, TileCol: integer);
    procedure InternalDoDecompressedStrip(GridCur: IdwlGridCursor_Write; Buf: Pointer; BufSize: integer; var NextRow: UInt64);
    procedure InternalDoDecompressedStripSampleFmt1(GridCur: IdwlGridCursor_Write; Buf: Pointer; BufSize: integer; RowsInBuf, RunHeight, RunWidth, StartCol, PadCols: UInt64; var StartRow: UInt64);
    procedure InternalDoDecompressedStripSampleFmt2(GridCur: IdwlGridCursor_Write; Buf: Pointer; BufSize: integer; RowsInBuf, RunHeight, RunWidth, StartCol, PadCols: UInt64; var StartRow: UInt64);
    procedure InternalDoDecompressedStripSampleFmt3(GridCur: IdwlGridCursor_Write; Buf: Pointer; BufSize: integer; RowsInBuf, RunHeight, RunWidth, StartCol, PadCols: UInt64; var StartRow: UInt64);
    procedure InternalDoDecompressedStripSampleFmtRGB(GridCur: IdwlGridCursor_Write; Buf: Pointer; BufSize: integer; RowsInBuf, RunHeight, RunWidth, StartCol,  PadBytes: UInt64; var StartRow: UInt64);
    procedure InternalDoDecompressedTile(GridCur: IdwlGridCursor_Write; Buf: Pointer; BufSize: integer; TileRowIdx, TileColIdx: UInt64);
    procedure InternalNoDecompressStrip(GridCur: IdwlGridCursor_Write; Idx: integer; var StartRow: UInt64);
    procedure InternalNoDecompressTile( GridCur: IdwlGridCursor_Write; Idx: integer; RowTileIdx, ColTileIdx: UInt64);
    procedure InternalPackbitsInflateStrip(GridCur: IdwlGridCursor_Write; Idx: integer; var StripRow: UInt64);
    procedure InternalPackbitsInflateTile(GridCur: IdwlGridCursor_Write; NTileCols, NTileRows: Integer; Idx, TileRow, TileCol: integer);
    procedure InternalLzwInflateStrip(GridCur: IdwlGridCursor_Write; Idx: integer; var StripRow: UInt64);
  private
    function GetGridData: IdwlGridData;
  public
    constructor Create(Cursor: IdwlCursor_Read);
    destructor Destroy; override;
  end;

function New_GeoTIFF(const FileName: string; Options: TdwlFileOptions=[]): IdwlGeoTIFF; overload;
begin
  Result := New_GeoTIFF(New_CursoredIO(FileName, Options));
end;

function New_GeoTIFF(CursoredIO: IdwlCursoredIO): IdwlGeoTIFF; overload;
begin
  Result := TGeoTiff.Create(CursoredIO.GetReadCursor);
end;

{ TTagData }

constructor TTagData.Create(Cursor: IdwlCursor_Read);
begin
  inherited Create;
  FIFDs := TObjectList<TIFD>.Create;
  Initialize;
  GetFieldTags(Cursor);
end;

function TTagData.CreateEmptyGridData: IdwlGridData;
begin
  var Dim: TdwlGridDim;
  Dim.WidthInPixels := FImageWidth;
  Dim.HeightInPixels := FImageHeight;
  Dim.ScaleGridToWorld := FScaleX;
  Dim.LeftWorldX := FTopLeftLon;
  Dim.TopWorldY := FTopLeftLat;
  if (FRasterType = RasterPixelIsArea) then
  begin
    Dim.LeftWorldX := Dim.LeftWorldX + (Dim.ScaleGridToWorld/2);
    Dim.TopWorldY := FTopLeftLat - (Dim.ScaleGridToWorld/2);
  end;
  var GridDataType: TdwlGridDataType;
  if FNoDataValueUsed  then
  begin
    GridDataType.DataTypeFlags := flagNoDataValueUsed;
    GridDataType.NoDataValue := FNoDataValue;
  end;
  GridDataType.DataType := dwlUInt32;
  if not FIsRGB then
  begin
    case FSampleFormat of
    sfUnsignedInteger:
      begin
        case FBitsPerSample[0] of
        1,2,4,8: GridDataType.DataType := dwlUInt8;
        16: GridDataType.DataType := dwlUInt16;
        32: GridDataType.DataType := dwlUInt32;
        end;
      end;
    sfSignedInteger:
      begin
        case FBitsPerSample[0] of
        1,2,4,8: GridDataType.DataType := dwlInt8;
        16: GridDataType.DataType := dwlInt16;
        32: GridDataType.DataType := dwlInt32;
        end;
      end;
    sfFloatingPoint:
      begin
        if FBitsPerSample[0]=64 then
          GridDataType.DataType := dwlDouble
        else
          GridDataType.DataType := dwlSingle;
      end;
    end;
  end;
  if not TdwlMathUtils.FuzzyEqual(FScaling, 1) then
  begin
    GridDataType.DataTypeFlags := GridDataType.DataTypeFlags or flagValueScalingUsed;
    GridDataType.ValueScale := FScaling;
  end;
  if not TdwlMathUtils.FuzzyEqual(FOffset, 0) then
  begin
    GridDataType.DataTypeFlags := GridDataType.DataTypeFlags or flagValueOffsetUsed;
    GridDataType.ValueOffset := FOffset;
  end;
  Result := New_GridData(Dim, GridDataType);
end;

destructor TTagData.Destroy;
begin
  FIFDs.Free;
  inherited Destroy;
end;

procedure TTagData.GetFieldTags(Cursor: IdwlCursor_Read);
begin
  Cursor.Seek(0);
  // Bytes 0-1: The byte order used within the file. Legal values are: “II” (4949.H) for little endian and “MM” (4D4D.H)
  var ByteOrder := Cursor.ReadUInt8;
  if (Cursor.ReadUInt8<>ByteOrder) then
    raise Exception.Create('First two bytes differ');
  if (ByteOrder <> $49) then
    raise Exception.Create('Big endian is not supported');
  var TiffVersion := Cursor.ReadUInt16;
  if not (TiffVersion in [42,43]) then
    raise Exception.Create('bytes 2-3 do not represent 42 or 43');
  var BigTiff := TiffVersion=43;
  var FirstIFDOffset: UInt64;
  if BigTiff then
  begin
    if Cursor.ReadUInt16<>8 then
      raise Exception.Create('Third word is not $8');
    if (Cursor.ReadUInt16<>0) then
      raise Exception.Create('Fourth word is not $0');
    FirstIFDOffset := Cursor.ReadUInt64;
  end
  else
    FirstIFDOffset := Cursor.ReadUInt32;
  Cursor.Seek(FirstIFDOffset);
  // An Image File Directory (IFD) consists of a 2-byte count of the number of directory entries ...
  var NumberofIFDs: UInt64;
  if BigTiff then
    NumberOfIFDs := Cursor.ReadUInt64
  else
    NumberOfIFDs := Cursor.ReadUInt16;
  // ... followed by a sequence of 12/20-byte field entries ...
  var IFDOffSet := Cursor.CursorOffset;
  for var IFD := 1 to NumberofIFDs do
  begin
    Cursor.Seek(IFDOffSet);
    var Item: TIFD;
    if BigTiff then
      Item := TBigIFD.Create
    else
      Item := TIFD.Create;
    Item.ReadFromCursor(Cursor);
    FIFDs.Add(Item);
    if BigTiff then
      inc(IFDOffSet, 20)
    else
      inc(IFDOffSet, 12);
  end;

  for var IFD in FIFDs do
  begin
    case IFD.FTag of
     256: FImageWidth := IFD.FValues[0].AsType<UInt64>;
     257: FImageHeight := IFD.FValues[0].AsType<UInt64>;
     258: begin
            var BPS := IFD.FValues[0].AsType<word>;
            if (BPS = 1) or (BPS = 2) or (BPS = 4) or (BPS = 8) or (BPS = 16) or (BPS = 32) or (BPS = 64) then
              FBitsPerSample[0] := BPS
            else
            begin
              FBitsPerSampleCount := 3;
              SetLEngth(FBitsPerSample, FBitsPerSampleCount);
              FBitsPerSample[0] := 8;
              FBitsPerSample[1] := 8;
              FBitsPerSample[2] := 8;
            end;
          end;
     259: FCompression := IFD.FValues[0].AsType<word>;
     266: begin
           FFillOrder := IFD.FValues[0].AsType<word>;
            if (FFillOrder <> fillOrderHiLo2LoHi) then
              raise Exception.Create('Unsupported FillOrder (tag 266)');
          end;
     273: begin
            FStripOffsetsCount := IFD.FCount;
            SetLength(FStripOffsets, FStripOffsetsCount);
            for var idx := 0 to FStripOffsetsCount-1 do
               FStripOffsets[idx] := IFD.FValues[idx].AsType<UInt64>;
          end;
     277: begin
            FSamplesPerPixel := IFD.FValues[0].AsType<word>;
            // SamplesPerPixel The number of components per pixel.
            // SamplesPerPixel is usually 1 for bilevel, grayscale, and palette-color images. SamplesPerPixel is usually 3 for RGB images. If this value is higher, ExtraSamples should give an indication of the meaning of the additional channels.
            // Seen in the wild: incorrect combination of PlanarConfiguration=2 and SamplesPerPixel=1, we ignore value for PlanarConfiguration
            // "The specification adds a warning that PlanarConfiguration=2 is not in widespread use and that Baseline TIFF readers are not required to support it.
            // If SamplesPerPixel is 1, PlanarConfiguration is irrelevant, and need not be included.
            if (FSamplesPerPixel > 1) and (FPlanarConfiguration = 2) then
              raise Exception.Create('The use of non-contiguous data is not supported');
          end;
     278: FRowsPerStrip := IFD.FValues[0].AsType<UInt64>;
     279: begin
            FStripByteCountsCount := IFD.FCount;
            SetLength(FStripByteCounts, FStripByteCountsCount);
            for var idx := 0 to FStripByteCountsCount-1 do
               FStripByteCounts[idx] := IFD.FValues[idx].AsType<cardinal>;
          end;
     284: begin
            FPlanarConfiguration := IFD.FValues[0].AsType<word>;
            if (FSamplesPerPixel > 1) and (FPlanarConfiguration <> 1) then
              raise Exception.Create('The use of non-contiguous data is not supported');
             // http://www.awaresystems.be/imaging/tiff/tifftags/planarconfiguration.html
             // PlanarConfiguration How the components of each pixel are stored.
             // 1 = Chunky format. The component values for each pixel are stored contiguously. For example, for RGB data, the data is stored as RGBRGBRGB
          end;
     317: if (IFD.FValues[0].AsType<word> <> 1) then
            FPredictor := IFD.FValues[0].AsType<word>;
     322: FTileWidth := IFD.FValues[0].AsType<UInt64>;
     323: FTileHeight := IFD.FValues[0].AsType<UInt64>;
     324: begin
            FTileOffsetsCount := IFD.FCount;
            SetLength(FTileOffsets, FTileOffsetsCount);
            for var idx := 0 to FTileOffsetsCount-1 do
              FTileOffsets[idx] := IFD.FValues[idx].AsType<UInt64>;
          end;
     325: begin
            FTileByteCountsCount := IFD.FCount;
            SetLength(FTileByteCounts, FTileByteCountsCount);
            for var idx := 0 to FTileByteCountsCount-1 do
               FTileByteCounts[idx] := IFD.FValues[idx].AsType<UInt64>;
          end;
     339: begin
            FSampleFormat := IFD.FValues[0].AsType<word>;
           // 1 = unsigned integer data
           // 2 = two's complement signed integer
           // 3 = RGB
           // SampleFormat = Specifies how to interpret each data sample in a pixel.
            if not (FSampleFormat in [sfUnsignedInteger, sfSignedInteger, sfFloatingPoint]) then
              raise Exception.Create('Undefined data sampling is not supported'); // images/logluv-3c-16b.tiff
          end;
     33550: ReadModelPixelScaleTag(IFD);
     33922: ReadModelTiePoints(IFD);
     42112: begin
              var Xml := TdsEasyXml.Create;
              try
                Xml.Options := Xml.Options - [sxoAutoCreate, sxoCasesensitive];
                Xml.LoadFromString(IFD.FValues[0].AsString);
                if SameText(Xml.Root.Name, 'GDALMetadata') then
                begin
                  for var elem := 0 to Xml.Root.ItemCount-1 do
                    if SameText(Xml.Root.Items[elem].Name, 'Item') then
                    begin
                      if SameText(Xml.Root.Items[elem].Properties.Value('role'), 'offset') then
                        FOffset := Xml.Root.Items[elem].FloatValue
                      else
                      if SameText(Xml.Root.Items[elem].Properties.Value('role'), 'scale') then
                        FScaling := Xml.Root.Items[elem].FloatValue;
                    end;
                end;
              finally
                Xml.Free;
              end;
             end;
      42113: FNoDataValueUsed := TryStrToInt64(IFD.FValues[0].AsString, FNoDataValue);
    end
  end;

  FIsRGB := (FBitsPerSampleCount=3) and (FBitsPerSample[0]=8) and (FBitsPerSample[1]=8) and (FBitsPerSample[2]=8);

  if High(FModelTiePoints) >=  0 then
  begin
    FTopLeftLon := FModelTiePoints[0].X;
    FTopLeftLat := FModelTiePoints[0].Y;
  end;

  if (FScaleX<>0) and (FScaleY<>0) and (High(FModelTiePoints) > 0) then
  begin
    var lMaxLat: double := -400;
    var lMaxLon: double := -400;
    var lMinLat: double := 400;
    var lMinLon: double := 400;
    for var t := 0 to High(FModelTiePoints) do
    begin
      if (FModelTiePoints[t].X > lMaxLon) then
        lMaxLon := FModelTiePoints[t].X;
      if (FModelTiePoints[t].X < lMinLon) then
        lMinLon := FModelTiePoints[t].X;
      if (FModelTiePoints[t].Y > lMaxLat) then
        lMaxLat := FModelTiePoints[t].Y;
      if (FModelTiePoints[t].Y < lMinLat) then
        lMinLat := FModelTiePoints[t].Y;
    end;
    if abs(lMaxLon - lMinLon) > 0.00000001 then
      FScaleX := abs(lMaxLon - lMinLon)/FImageWidth;
    if abs(lMaxLat - lMinLat) > 0.00000001 then
      FScaleY := abs(lMaxLat - lMinLat)/FImageHeight;
    FTopLeftLon := lMinLon;
    FTopLeftLat := lMaxLat;
  end;

  if FPredictor=PREDICTOR_FLOATINGPOINT then
    raise Exception.Create('Predictor FLoating point not implemented');
  if not TdwlMathUtils.FuzzyEqual(FScaleX, FScaleY) then
    raise Exception.Create('ScaleX<>ScaleY');
end;

procedure TTagData.Initialize;
begin
  FImageHeight := NaC;
  FImageWidth := NaC;
  FBitsPerSampleCount := 1;
  SetLength(FBitsPerSample, FBitsPerSampleCount);
  FBitsPerSample[0] := 0;
  FPlanarConfiguration := 1;
  FFillOrder := fillOrderHiLo2LoHi;
  FSampleFormat := 1;
  FRowsPerStrip := 1;
  FRasterType := RasterPixelIsArea;
  FScaling := 1;
end;

procedure TTagData.ReadModelPixelScaleTag(IFD: TIFD);
begin
  FScaleX := IFD.FValues[0].AsType<double>;
  FScaleY := IFD.FValues[1].AsType<double>;
end;

procedure TTagData.ReadModelTiePoints(IFD: TIFD);
begin
  var TiePointCount := IFD.FCount div 6;
  SetLength(FModelTiePoints, TiePointCount);
  for var i := 0 to TiePointCount-1 do
  begin
    FModelTiePoints[i].I := IFD.FValues[i*6].AsType<double>;
    FModelTiePoints[i].J := IFD.FValues[i*6+1].AsType<double>;
    FModelTiePoints[i].K := IFD.FValues[i*6+2].AsType<double>;
    FModelTiePoints[i].X := IFD.FValues[i*6+3].AsType<double>;
    FModelTiePoints[i].Y := IFD.FValues[i*6+4].AsType<double>;
    FModelTiePoints[i].Z := IFD.FValues[i*6+5].AsType<double>;
  end;
end;

function TTagData.TileCountX: UInt64;
begin
  if FTileWidth>0 then
    Result := 1+((FImageWidth-1) div FTileWidth)
  else
    Result := 1;
end;

function TTagData.TileCountY: UInt64;
begin
  if FTileHeight>0 then
    Result := 1+ (FImageHeight-1) div FTileHeight
  else
    Result := 1;
end;

{ TIFD }

constructor TIFD.Create;
begin
  FMaxValueBytes := 4;
end;

procedure TIFD.ReadAsciiValues(Cursor: IdwlCursor_Read);
begin
  var ValueIndex := 0;
  var Bytes2Go := FCount;
  while Bytes2Go>0 do
  begin
    var S: ansistring := PAnsiChar(Cursor.CursorPtr);
    dec(Bytes2Go, Length(S)+1);
    SetLength(FValues, ValueIndex+1);
    FValues[ValueIndex] := string(S);
    inc(ValueIndex);
  end;
end;

procedure TIFD.ReadFromCursor(Cursor: IdwlCursor_Read);
begin
  FTag := Cursor.ReadUInt16;
  FFieldType := Cursor.ReadUInt16;
  FCount := Cursor.ReadUInt32;
  ReadValuesFromCursor(Cursor);
end;

function TIFD.ReadOffset(Cursor: IdwlCursor_Read): UInt64;
begin
  Result := Cursor.ReadUInt32;
end;

function TIFD.ReadValue(Cursor: IdwlCursor_Read): TValue;
begin
  case FFieldType of
  ftBYTE: Result := Cursor.ReadUInt8;
  ftSHORT: Result := Cursor.ReadUInt16;
  ftLONG: Result := Cursor.ReadUInt32;
  ftRATIONAL: Result := Cursor.ReadUInt32/Cursor.ReadUInt32;
  ftSBYTE: Result := Cursor.ReadInt8;
  ftUNDEFINED: Result := Cursor.ReadUInt8;
  ftSSHORT: Result := Cursor.ReadInt16;
  ftSLONG: Result := Cursor.ReadInt32;
  ftSRATIONAL: Result := Cursor.ReadInt32/Cursor.ReadInt32;
  ftFLOAT: Result := Cursor.ReadSingle;
  ftDOUBLE: Result := Cursor.ReadDouble;
  ftLONG8: Result := Cursor.ReadUInt64;
  ftSLONG8: Result := Cursor.ReadInt64;
  ftIFD8: Result := Cursor.ReadUInt64;
  else
    Result := Cursor.ReadUInt8;
  end;
end;

procedure TIFD.ReadValuesFromCursor(Cursor: IdwlCursor_Read);
begin
  if FCount*FieldTypeByteSizes[FFieldType] > FMaxValueBytes then
    Cursor.Seek(ReadOffset(Cursor));
  if FFieldType=ftASCII then
    ReadAsciiValues(Cursor)
  else
  begin
    SetLength(FValues, FCount);
    for var i := 0 to FCount-1 do
      FValues[i] := ReadValue(Cursor);
  end;
end;

{ TBigIFD }

constructor TBigIFD.Create;
begin
  FMaxValueBytes := 8;
end;

procedure TBigIFD.ReadFromCursor(Cursor: IdwlCursor_Read);
begin
  FTag := Cursor.ReadUInt16;
  FFieldType := Cursor.ReadUInt16;
  FCount := Cursor.ReadUInt64;
  ReadValuesFromCursor(Cursor);
end;

function TBigIFD.ReadOffset(Cursor: IdwlCursor_Read): UInt64;
begin
  Result := Cursor.ReadUInt64;
end;

{ TGeoTiff }

constructor TGeoTiff.Create(Cursor: IdwlCursor_Read);
begin
  inherited Create;
  FCursor := Cursor;
  FTagData := TTagData.Create(Cursor);
end;

destructor TGeoTiff.Destroy;
begin
  FTagData.Free;
  inherited Destroy;
end;

                                                                 
function TGeoTiff.GetGridData: IdwlGridData;
begin
  Result := FTagData.CreateEmptyGridData;
  var GridCur := Result.GetWriteCursor;
  case FTagData.FCompression of
  COMPRESSION_NONE:
    begin
      var CurrentRow: UInt64 := 0;
      if (FTagData.FTileOffsetsCount > 0) then
      begin // t101tn t108tn t116tn t132tn t208tn t216tn t232tn t332tn t364tn
        var NTileCols := FTagData.TileCountX;
        var NTileRows := FTagData.TileCountY;
        Assert(((FTagData.FPlanarConfiguration = 1) and ((NTileCols * NTileRows) = integer(FTagData.FTileOffsetsCount))) or
          ((FTagData.FPlanarConfiguration = 2) and ((NTileCols * NTileRows * FTagData.FSamplesPerPixel) = integer(FTagData.FTileOffsetsCount))));
        var i := 0;
        for var r := 0 to NTileRows - 1 do
        begin
          for var c := 0 to NTileCols - 1 do
          begin
            try
              InternalNoDecompressTile(GridCur, i, r, c);
            except
            end;
            inc(i);
          end;
        end;
      end
      else
      if (FTagData.FStripOffsetsCount > 0) then
      begin // t101sn t108sn t116sn t132sn t208sn t216sn t232sn t332sn t364sn
        for var i := 0 to FTagData.FStripOffsetsCount-1 do
          InternalNoDecompressStrip(GridCur, i, CurrentRow); //
      end;
    end;
  COMPRESSION_HUFFMANRUNLENGTH:
    raise Exception.Create('Huffman Run Length Compression Not implemented');
  COMPRESSION_LZW:
    begin
      if (FTagData.FTileOffsetsCount > 0) then
      begin
        var DecompSize := FTagData.FTileHeight * FTagData.FTileWidth * GridCur.BytesPerElement;
        if (FTagData.FBitsPerSampleCount = 1) and (FTagData.FBitsPerSample[0]<=4) then
          DecompSize := 1+ (DecompSize div (8 div FTagData.FBitsPerSample[0]));
        var DecompBuf := AllocMem(DecompSize);
        try
          var Idx := 0;
          for var TileRowIdx := 0 to FTagData.TileCountY - 1 do
          begin
            for var TileColIdx := 0 to FTagData.TileCountX - 1 do
            begin
              FCursor.Seek(FTagData.FTileOffsets[Idx]);
              TLzwDecoder.Decompress(FCursor.CursorPtr, FTagData.FTileByteCounts[Idx], DecompBuf, DecompSize);
              InternalDoDecompressedTile(GridCur, DecompBuf, DecompSize, TileRowIdx, TileColIdx);
              inc(Idx);
            end;
          end;
        finally
          FreeMem(DecompBuf);
        end;
      end
      else
      if (FTagData.FStripOffsetsCount > 0) then
      begin
        var CurrentRow: UInt64 := 0;
        for var i := 0 to FTagData.FStripOffsetsCount-1 do
          InternalLzwInflateStrip(GridCur, i, CurrentRow);
      end;
    end;
  COMPRESSION_PACKBITS:
    begin
      var CurrentRow: UInt64 := 0;
      if (FTagData.FTileOffsetsCount > 0) then
      begin // t101tp t108tp t116tp t132tp t208tp t216tp t232tp t332tp t364tp
        var NTileCols := FTagData.TileCountX;
        var NTileRows := FTagData.TileCountY;
        Assert((NTileCols * NTileRows) = integer(FTagData.FTileOffsetsCount));
        var i := 0;
        for var r := 0 to NTileRows - 1 do
        begin
          for var c := 0 to NTileCols - 1 do
          begin
            try
              InternalPackbitsInflateTile(GridCur, NTileCols, NTileRows, i, r, c); // Sample: afm_ff_qc_effectiveT_22D_20021203_v03r01.tif
            except
            end;
            inc(i);
          end;
        end;
      end
      else
      if (FTagData.FStripOffsetsCount > 0) then
      begin // t101sp t108sp t116sp t132sp t208sp t216sp t232sp t332sp t364sp
        for var i := 0 to FTagData.FStripOffsetsCount-1 do
          InternalPackbitsInflateStrip(GridCur, i, CurrentRow); // erdas_spnad83.tiff
      end;
    end;
  COMPRESSION_ADOBE_DEFLATE, COMPRESSION_DEFLATE:
    begin
      var CurrentRow: UInt64 := 0;
      if FTagData.FTileOffsetsCount > 0 then
      begin // t101td t108td t116td t132td t208td t216td t232td t332td t364td
        var Idx := 0;
        for var TileRow := 0 to FTagData.TileCountY - 1 do
        begin
          for var TileCol := 0 to FTagData.TileCountX - 1 do
          begin
            InternalAdobeInflateTile(GridCur, Idx, TileRow, TileCol); // Sample: afm_mask_3s_00S_030E_v01r00.tif
            inc(Idx);
          end;
        end;
      end
      else
      if (FTagData.FStripOffsetsCount > 0) then
      begin // t101sd t108sd t116sd t132sd t208sd t216sd t232sd t332sd t364sd
        for var i := 0 to FTagData.FStripOffsetsCount-1 do
        try
          InternalAdobeInflateStrip(GridCur, i, CurrentRow); // Sample: afed_3s_15N_010E_20020715_v03r01.tif
        except
          on E: Exception do
          begin
            raise Exception.Create('Error: decompression failed at offset idx ' + i.ToString + ' row ' + CurrentRow.ToString +
              sLineBreak + E.Message);
          end;
        end;
      end
    end
    else
      raise Exception.Create(Format('Decompression %s not implemented', [FTagData.FCompression.ToString]));
  end;
end;

procedure TGeoTiff.InternalAdobeInflateStrip(GridCur: IdwlGridCursor_Write; Idx: integer; var StartRow: UInt64);
var
  DecompBuf: Pointer;
  DecompSize: Integer;
begin
  FCursor.Seek(FTagData.FStripOffsets[Idx]);
  ZDecompress(FCursor.CursorPtr, FTagData.FStripByteCounts[Idx], DecompBuf, DecompSize);
  FCursor.Seek(FTagData.FStripByteCounts[Idx], soCurrent);
  InternalDoDecompressedStrip(GridCur, DecompBuf, DecompSize, StartRow);
  FreeMem(DecompBuf);
end;

procedure TGeoTiff.InternalAdobeInflateTile(GridCur: IdwlGridCursor_Write; Idx, TileRow, TileCol: integer);
var
  DecompBuf: Pointer;
  DecompSize: Integer;
begin
  FCursor.Seek(fTagData.FTileOffsets[Idx]);
  System.ZLib.ZDecompress(FCursor.CursorPtr, FTagData.FTileByteCounts[Idx], DecompBuf, DecompSize);
  try
    InternalDoDecompressedTile(GridCur, DecompBuf, DecompSize, TileRow, TileCol);
  finally
    FreeMem(DecompBuf);
  end;
end;

procedure TGeoTiff.InternalDoDecompressedStrip(GridCur: IdwlGridCursor_Write; Buf: Pointer; BufSize: integer; var NextRow: UInt64);
begin
  if FTagData.FIsRGB then
    InternalDoDecompressedStripSampleFmtRGB(GridCur, Buf, BufSize, FTagData.FRowsPerStrip, FTagData.FImageHeight, FTagData.FImageWidth, 0, 0, NextRow)
  else
  case FTagData.FSampleFormat of
    sfUnsignedInteger: InternalDoDecompressedStripSampleFmt1(GridCur, Buf, BufSize, FTagData.FRowsPerStrip, FTagData.FImageHeight, FTagData.FImageWidth, 0, 0, NextRow);
    sfSignedInteger: InternalDoDecompressedStripSampleFmt2(GridCur, Buf, BufSize, FTagData.FRowsPerStrip, FTagData.FImageHeight, FTagData.FImageWidth, 0, 0, NextRow); // ETOPO1_Ice_g_geotiff.tif
    sfFloatingPoint: InternalDoDecompressedStripSampleFmt3(GridCur, Buf, BufSize, FTagData.FRowsPerStrip, FTagData.FImageHeight, FTagData.FImageWidth, 0, 0, NextRow); // Biomass_2015.tif
  else
  begin
    if FTagData.FBitsPerSampleCount = 3 then
      InternalDoDecompressedStripSampleFmt1(GridCur, Buf, BufSize, FTagData.FRowsPerStrip, FTagData.FImageHeight, FTagData.FImageWidth, 0, 0, NextRow); // Sample: Globe15_TIFF.tif
  end;
  end;
end;


procedure TGeoTiff.InternalDoDecompressedStripSampleFmt1(GridCur: IdwlGridCursor_Write; Buf: Pointer; BufSize: integer; RowsInBuf, RunHeight, RunWidth, StartCol, PadCols: UInt64; var StartRow: UInt64);
var
  RunByte, OverByte: PByte;
  Row: UInt64;
  Col: UInt64;
  RunBit: byte;
  bValue, bPredictor: byte;
  wValue, wPredictor: word;
  lwValue, lwPredictor: longword;
  RowWidthEff: UInt64;
  BPS: byte;
  Mask: byte;
begin
    // handles: t101* t102* t104* t108* t116* t132* (tn + sn + td +sd + tz + sz) = 36 cases

  Assert(FTagData.FSampleFormat = sfUnsignedInteger);
  Row := 0;
  RunByte := Buf;
  try
    OverByte := pointer(UInt64(Buf) + UInt64(BufSize));
    if (FTagData.FBitsPerSample[0] in [1, 2, 4]) then
    begin
      // sub byte handling
      BPS := FTagData.FBitsPerSample[0];
      Mask := $1;
      if BPS = 2 then
        Mask := $3
      else
      if BPS = 4 then
        Mask := $F;
      RunBit := 8 - BPS;
      RowWidthEff := RunWidth;
      if PadCols > 0 then
        RowWidthEff := RunWidth + PadCols;
      while (Row < RunHeight) do
      begin
        Col := 0;
        GridCur.Seek(StartCol, StartRow + Row);
        while (Col < RowWidthEff) do
        begin
          if Col < RunWidth then
          begin
            bValue := (RunByte^ shr RunBit) and Mask;
            // don't see how predictor could work here: unsigned,
            // not much room for negative values
            GridCur.WriteBareValue(bValue);
          end;

          // NextBit:
          if (RunBit = 0) then
          begin
            // Next Byte
            inc(RunByte);
            if (RunByte = OverByte) then
            begin
              inc(Row);
              Exit;
            end;
            RunBit := 8 - BPS;
          end
          else
            dec(RunBit, BPS);

          inc(Col);
        end;
        inc(Row);
      end;
    end
    else
    if (FTagData.FBitsPerSample[0] = 8) then
    begin
      while (Row < RunHeight) do
      begin
        Col := 0;
        bPredictor := 0;
        GridCur.Seek(StartCol, StartRow + Row);
        while (Col < RunWidth) do
        begin
          bValue := RunByte^;
          if FTagData.FPredictor = PREDICTOR_HORIZONTAL then
          begin
            bValue := bPredictor + bValue;
            bPredictor := bValue;
          end;

          GridCur.WriteBareValue(bValue);
          inc(RunByte);
          if (RunByte >= OverByte) then
          begin
            inc(Row);
            Exit;
          end;
          inc(Col);
        end;
        if PadCols > 0 then
          inc(RunByte, PadCols { * 1 });
        inc(Row);
      end;
    end
    else
    if (FTagData.FBitsPerSample[0] = 16) then
    begin
      while (Row < RunHeight) do
      begin
        Col := 0;
        wPredictor := 0;
        GridCur.Seek(StartCol, StartRow + Row);
        while (Col < RunWidth) do
        begin
          wValue := Word(RunByte^);
          if FTagData.FPredictor = PREDICTOR_HORIZONTAL then
          begin
            wValue := wPredictor + wValue;
            wPredictor := wValue;
          end;

          GridCur.WriteBareValue(wValue);
          inc(RunByte, 2);
          if (RunByte >= OverByte) then
          begin
            inc(Row);
            Exit;
          end;
          inc(Col);
        end;
        if PadCols > 0 then
          inc(RunByte, PadCols * 2);
        inc(Row);
      end;
    end
    else
    if (FTagData.FBitsPerSample[0] = 32) then
    begin
      while (Row < RunHeight) do
      begin
        Col := 0;
        lwPredictor := 0;
        GridCur.Seek(StartCol, StartRow + Row);
        while (Col < RunWidth) do
        begin
          lwValue := LongWord(RunByte^);
          if FTagData.FPredictor = PREDICTOR_HORIZONTAL then
          begin
            lwValue := lwPredictor + lwValue;
            lwPredictor := lwValue;
          end;
          GridCur.WriteBareValue(lwValue);
          inc(RunByte, 4);
          if (RunByte >= OverByte) then
          begin
            inc(Row);
            Exit;
          end;
          inc(Col);
        end;
        if PadCols > 0 then
          inc(RunByte, PadCols * 4);
        inc(Row);
      end;
    end
  finally
    StartRow := StartRow + Max(1, Row);
  end;
end;


procedure TGeoTiff.InternalDoDecompressedStripSampleFmt2(GridCur: IdwlGridCursor_Write; Buf: Pointer; BufSize: integer; RowsInBuf, RunHeight, RunWidth, StartCol, PadCols: UInt64; var StartRow: UInt64);
var
  RunShort, OverShort: PShortInt;
  RunSmall, OverSmall: PSmallInt;
  Row: UInt64;
  Col: UInt64;
  SmI: smallint;
  shValue, shPredictor: shortint;
  smPredictor: smallint;
  iValue, iPredictor: integer;
begin { TODO : Needs factoring out runsmall, use runbyte everywwhere }
    // handles: t208* t216* t232*  (tn + sn + td +sd) = 12 cases
  Assert(FTagData.FSampleFormat = sfSignedInteger);
  Row := 0;
  RunShort := Buf;
  RunSmall := Buf;
  try
    OverShort := pointer(UInt64(Buf) + UInt64(BufSize));
    OverSmall := pointer(UInt64(Buf) + UInt64(BufSize));
    while (Row < RunHeight) do
    begin
      Col := 0;
      smPredictor := 0;
      shPredictor := 0;
      iPredictor := 0;
      GridCur.Seek(StartCol, StartRow + Row);
      while (Col < RunWidth) do
      begin
        if FTagData.FBitsPerSample[0] = 8 then
        begin
          shValue := shortint(RunShort^);
          if FTagData.FPredictor = PREDICTOR_HORIZONTAL then
          begin
            shValue := shPredictor + shValue;
            shPredictor := shValue;
          end;

          GridCur.WriteBareValue(shValue);
          inc(RunShort);
          if (integer(RunShort) >= integer(OverShort)) then
          begin
            if Col >= (RunWidth-1) then
              inc(Row);
            Exit;
          end;
        end
        else
        if FTagData.FBitsPerSample[0] = 16 then // Sample: ETOPO1_Ice_g_geotiff.tif
        begin
          SmI := smallint(RunSmall^);
          if FTagData.FPredictor = PREDICTOR_HORIZONTAL then
          begin
            SmI := smPredictor + SmI;
            smPredictor := SmI
          end;
          GridCur.WriteBareValue(SmI);
          inc(RunSmall);
          if (Int64(RunSmall) >= Int64(OverSmall)) then
          begin
            if Col >= (RunWidth-1) then
              inc(Row);
            Exit;
          end;
        end
        else
        if FTagData.FBitsPerSample[0] = 32 then
        begin
          iValue := Integer(RunShort^);
          if FTagData.FPredictor = PREDICTOR_HORIZONTAL then
          begin
            iValue := iPredictor + iValue;
            iPredictor := iValue;
          end;
          GridCur.WriteBareValue(iValue);
          inc(RunShort, 4);
          if (integer(RunShort) >= integer(OverShort)) then
          begin
            if Col >= (RunWidth-1) then
              inc(Row);
            Exit;
          end;
        end;
        inc(Col);
      end;
      if PadCols > 0 then
      begin
        inc(RunShort, PadCols { is typed, not * SizeOf(ShortInt)} );
        inc(RunSmall, PadCols { is typed, not * SizeOf(SmallInt)} );
      end;
      inc(Row);
    end;
  finally
    StartRow := StartRow + Max(1, Row);
  end;
end;


procedure TGeoTiff.InternalDoDecompressedStripSampleFmt3(GridCur: IdwlGridCursor_Write; Buf: Pointer; BufSize: integer; RowsInBuf, RunHeight, RunWidth, StartCol, PadCols: UInt64; var StartRow: UInt64);
begin // t332* t364* (tn + sn + td +sd) = 8 cases
  var Row: UInt64 := 0;
  var RunByte: PByte := Buf;
  try
    var OverByte := PByte(Buf) + BufSize;
    begin
      while (Row < RunHeight) do
      begin
        var Col := 0;
        GridCur.Seek(StartCol, StartRow + Row);
        while (Col < RunWidth) do
        begin
          GridCur.WriteBareValue(RunByte^);
          inc(RunByte, GridCur.BytesPerElement);
          if (RunByte >= OverByte) then
            Exit;
          inc(Col);
        end;
        if PadCols > 0 then
          inc(RunByte, PadCols * SizeOf(double));
        inc(Row);
      end;
    end
  finally
    StartRow := StartRow + Max(1, Row);
  end;
end;

procedure TGeoTiff.InternalDoDecompressedStripSampleFmtRGB(GridCur: IdwlGridCursor_Write; Buf: Pointer; BufSize: integer; RowsInBuf, RunHeight, RunWidth, StartCol, PadBytes: UInt64; var StartRow: UInt64);
var
  RunByte, OverByte: PByte;
  Row: UInt64;
  Col: UInt64;
  V: longword;
  Triple: TRGBTRIPLE;
begin
  Row := 0;
  RunByte := Buf;
  try
    OverByte := pointer(UInt64(Buf) + UInt64(BufSize));
      while (Row < RunHeight) do
      begin
        Col := 0;
        GridCur.Seek(StartCol, StartRow + Row);
        while (Col < RunWidth) do
        begin
          Triple := PRGBTriple(RunByte)^;
          V := Triple.rgbtRed + (Triple.rgbtGreen shl 8) + (Triple.rgbtBlue shl 16);
          GridCur.WriteBareValue(V);
          inc(RunByte, 3);
          if (RunByte >= OverByte) then
          begin
            if Col >= (RunWidth-1) then
              inc(Row);
            Exit;
          end;
          inc(Col);
        end;
        if PadBytes > 0 then
          inc(RunByte, PadBytes);
        inc(Row);
      end;
  finally
    StartRow := StartRow + Max(1, Row);
  end;
end;

procedure TGeoTiff.InternalDoDecompressedTile(GridCur: IdwlGridCursor_Write; Buf: Pointer; BufSize: integer; TileRowIdx, TileColIdx: UInt64);
var
  NextRow: UInt64;
  StartCol: UInt64;
  RunWidth: UInt64;
  RunHeight: UInt64;
  PadCols: UInt64;
  EndCrit: UInt64;
  RowsInBuf: UInt64;
begin
  NextRow := TileRowIdx * FTagData.FTileHeight;
  StartCol := TileColIdx * FTagData.FTileWidth;
  EndCrit := Min(FTagData.FImageHeight, (TileRowIdx + 1) * FTagData.FTileHeight);
  repeat
    GridCur.Seek(StartCol, NextRow);

    RunWidth := FTagData.FTileWidth;
    if (StartCol + FTagData.FTileWidth) > FTagData.FImageWidth then
      RunWidth := FTagData.FImageWidth - StartCol;

    PadCols := 0;
    RunHeight := FTagData.FTileHeight;
    if (NextRow + FTagData.FTileHeight) > FTagData.FImageHeight then
      RunHeight := FTagData.FImageHeight - NextRow;
    if (TileColIdx+1) = FTagData.TileCountX then
    begin
      RunWidth := FTagData.FImageWidth - TileColIdx * FTagData.FTileWidth;
      PadCols := (FTagData.FTileWidth - RunWidth);
    end;
    if (TileRowIdx+1) = FTagData.TileCountY then
      RunHeight := FTagData.fImageHeight - TileRowIdx * FTagData.FTileHeight;

    RowsInBuf := Min(FTagData.FImageHeight, FTagData.FTileHeight);

    if FTagData.FIsRGB then
      InternalDoDecompressedStripSampleFmtRGB(GridCur, Buf, BufSize, RowsInBuf, RunHeight, RunWidth, StartCol, PadCols, NextRow)
    else
    begin
      case FTagData.FSampleFormat of
        sfUnsignedInteger: InternalDoDecompressedStripSampleFmt1(GridCur, Buf, BufSize, RowsInBuf, RunHeight, RunWidth, StartCol, PadCols, NextRow);
        sfSignedInteger: InternalDoDecompressedStripSampleFmt2(GridCur, Buf, BufSize, RowsInBuf, RunHeight, RunWidth, StartCol, PadCols, NextRow); //
        sfFloatingPoint: InternalDoDecompressedStripSampleFmt3(GridCur, Buf, BufSize, RowsInBuf, RunHeight, RunWidth, StartCol, PadCols, NextRow);
      else
        begin
          if FTagData.FBitsPerSampleCount = 3 then
            InternalDoDecompressedStripSampleFmt1(GridCur, Buf, BufSize, RowsInBuf, RunHeight, RunWidth, StartCol, PadCols, NextRow); // Sample:
        end;
      end;
    end;
    inc(NextRow);
  until (NextRow >= EndCrit);
end;


procedure TGeoTiff.InternalLzwInflateStrip(GridCur: IdwlGridCursor_Write; Idx: integer; var StripRow: UInt64);
var
  DecompBuf: Pointer;
  DecompSize: Integer;
begin
  if FTagData.FStripByteCounts[Idx] = 0 then
    Exit;
  FCursor.Seek(FTagData.FStripOffsets[Idx]);
  DecompSize := FTagData.FRowsPerStrip * FTagData.FImageWidth * GridCur.BytesPerElement;
  DecompBuf := AllocMem(DecompSize);
  TLzwDecoder.Decompress(FCursor.CursorPtr, FTagData.FStripByteCounts[Idx], DecompBuf, DecompSize);
  FCursor.Seek(fTagData.FStripByteCounts[Idx], soCurrent);
  // For a 6000 pixel bilevel row, 10 rowspersegment, we expect 6000/8*10 = 7500 bytes of clean data
  InternalDoDecompressedStrip(GridCur, DecompBuf, DecompSize, StripRow);
  FreeMem(DecompBuf);
end;

procedure TGeoTiff.InternalNoDecompressStrip(GridCur: IdwlGridCursor_Write; Idx: integer; var StartRow: UInt64);
var
  Buf: PByte;
  BufSize: integer;
begin
  FCursor.Seek(FTagData.FStripOffsets[Idx]);
  Buf := FCursor.CursorPtr;
  BufSize := FTagData.FStripByteCounts[Idx];
  InternalDoDecompressedStrip(GridCur, Buf, BufSize, StartRow);
end;

procedure TGeoTiff.InternalNoDecompressTile(GridCur: IdwlGridCursor_Write; Idx: integer; RowTileIdx, ColTileIdx: UInt64);
var
  Buf: PByte;
  BufSize: integer;
begin
  FCursor.Seek(FTagData.FTileOffsets[Idx]);
  Buf := FCursor.CursorPtr;
  BufSize := FTagData.FTileByteCounts[Idx];
  if BufSize > 0 then
    InternalDoDecompressedTile(GridCur, Buf, BufSize, RowTileIdx, ColTileIdx);
end;

procedure TGeoTiff.InternalPackbitsInflateStrip(GridCur: IdwlGridCursor_Write; Idx: integer; var StripRow: UInt64);
var
  OverByte: pointer; // 1 step beyond
  SubRun: integer;
  Run: ShortInt;
  RLEByte: byte;
  RLEWord: word;
  StripDecompressedCount: integer;
begin // handles: t101sp t108sp t116sp t132sp t208sp t216sp t232sp t332sp t364sp = 9 cases
  if FTagData.FStripByteCounts[Idx] = 0 then
    Exit;
  FCursor.Seek(FTagData.FStripOffsets[Idx]);

  StripDecompressedCount := integer(FTagData.FImageWidth * FTagData.FRowsPerStrip);

  if (FTagData.FBitsPerSample[0] = 8) and (FTagData.FSampleFormat = sfUnsignedInteger) then
  begin
    OverByte := pointer(UInt64(FCursor.CursorPtr) + FTagData.FStripByteCounts[Idx]);
    repeat
      Run := FCursor.ReadInt8;
      if (Run >= 0) then
      begin
        for SubRun := 1 to 1+Run do
        begin
          RLEByte := FCursor.ReadUInt8;
          GridCur.WriteBareValue(RLEByte);
          Dec(StripDecompressedCount);
          if (StripDecompressedCount = 0) then
            Break;
        end;
      end
      else
      if (Run < 0) then
      begin
        RLEByte := FCursor.ReadUInt8;
        for SubRun := 1 to 1-Run do
        begin
          GridCur.WriteBareValue(RLEByte);
          Dec(StripDecompressedCount);
          if (StripDecompressedCount = 0) then
            Break;
        end;
      end;
    until (integer(FCursor.CursorPtr) >= integer(Overbyte));
  end
  else
  if (FTagData.FBitsPerSample[0] = 16) and (FTagData.FSampleFormat = sfUnsignedInteger)  then
  begin
    OverByte := pointer(UInt64(FCursor.CursorPtr) + FTagData.FStripByteCounts[Idx]);
    repeat
      Run := FCursor.ReadInt8;
      if (Run >= 0) then
      begin
        for SubRun := 1 to 1+Run do
        begin
          RLEWord := FCursor.ReadUInt16;
          GridCur. WriteBareValue(RLEWord);
          Dec(StripDecompressedCount);
          if (StripDecompressedCount = 0) then
            Break;
        end;
      end
      else
      if (Run < 0) then
      begin
        RLEWord := FCursor.ReadUInt16;
        for SubRun := 1 to 1-Run do
        begin
          GridCur.WriteBareValue(RLEWord);
          Dec(StripDecompressedCount);
          if (StripDecompressedCount = 0) then
            Break;
        end;
      end;
    until (integer(FCursor.CursorPtr) >= integer(Overbyte));
  end;
  if (StripDecompressedCount  = 0) then
  begin
    Beep(0,0);
  end;
end;

procedure TGeoTiff.InternalPackbitsInflateTile(GridCur: IdwlGridCursor_Write; NTileCols, NTileRows: Integer; Idx, TileRow, TileCol: integer);
var
  Row0, Col0: integer;
  RowN, ColN: integer;
  Row, Col: integer;
  DecompBuf: PByte;
  DecompSize: Integer;
  Run: PByte;
  OverByte: PByte;  // 1 step beyond
  lr: Integer;
  n: smallint;
  ExpectedRowBytes: Cardinal;
  ReceivedRowBytes: cardinal;
  CopyUint8: UInt8;
  lTileWidth: UInt64;
begin // handles: t101tp t108tp t116tp t132tp t208tp t216tp t232tp t332tp t364tp = 9 cases
  if FTagData.FTileByteCounts[Idx] = 0 then
    Exit;
  DecompSize := FTagData.FTileHeight * FTagData.FTileWidth * GridCur.BytesPerElement;
  DecompBuf := AllocMem(DecompSize);
  OverByte := DecompBuf+DecompSize;

  Row0 := TileRow * integer(FTagData.FTileHeight);
  Col0 := TileCol * integer(FTagData.FTileWidth);
  RowN := Min(integer(FTagData.FImageHeight), Row0 + integer(FTagData.FTileHeight)) - 1;
  ColN := Min(integer(FTagData.FImageWidth), Col0 + integer(FTagData.FTileWidth)) - 1;

  lTileWidth := FTagData.FTileWidth;
  if (TileCol = (NTileCols-1)) and ((FTagData.FImageWidth mod FTagData.FTileWidth) > 0) then
    lTileWidth := FTagData.FTileWidth; //ImageWidth mod TagData.TileWidth;

  Run := DecompBuf;
  for Row := Row0 to RowN do
  begin
    for Col := Col0 to ColN do
    begin
      PSingle(Run)^ := -1000;
      inc(Run, GridCur.BytesPerElement);
      Assert(Run <= Overbyte);
    end;
  end;

  FCursor.Seek(FTagData.FTileOffsets[Idx]);

  try
    lr := Row0;
    while lr <= RowN do
    begin

      // (Un)Pack each row separately. Do not compress across row boundaries.
      Run := DecompBuf + cardinal(lr-Row0) * FTagData.FTileWidth * GridCur.BytesPerElement;

      // Loop until you get the number of unpacked bytes you are expecting:
      ExpectedRowBytes := lTileWidth * GridCur.BytesPerElement;
      ReceivedRowBytes := 0;

      while ReceivedRowBytes < ExpectedRowBytes do
      begin
        // Read the next source byte into n
        n := FCursor.ReadInt8;

        // If n is between 0 and 127 inclusive, copy the next n+1 bytes literally.
        if n>=0 then
        begin
          try
            n := n + 1;
            while (n>0) do
            begin
              CopyUint8 := FCursor.ReadUInt8;
              Run^ := CopyUInt8;
              dec(n);
              inc(Run);
              inc(ReceivedRowBytes);
              if Run > Overbyte then
              begin
                ReceivedRowBytes := ExpectedRowBytes;
                Break;
              end;

            end;
          except
            Beep(0,0);
          end;
        end
        // Else if n is between -127 and -1 inclusive, copy the next byte -n+1 times
        else
        if n>=-127 then
        begin
          try
            n := -n + 1;
            CopyUint8 := FCursor.ReadUInt8;
            while (n>0) do
            begin
              Run^ := CopyUInt8;
              dec(n);
              inc(Run);
              inc(ReceivedRowBytes);
              if Run > Overbyte then
              begin
                ReceivedRowBytes := ExpectedRowBytes;
                Break;
              end;
            end;
          except
            Beep(0,0);
          end;
        end
        // Else if n is -128, noop
        //else
        //  if n=-128 then
        //    Continue;
      end;

      inc(lr);
    end;
  except
    Beep(0,0);
  end;
end;

{ TLzwDecoder }

class procedure TLzwDecoder.DeCompress(const inBuffer: Pointer; inSize: Integer; const outBuffer: Pointer; outSize: Integer);
var
  I: Integer;
  Data,           // current data
  Bits,           // counter for bit management
  Code: Cardinal; // current code value
  SourcePtr: PByte;
  InCode: Cardinal; // Buffer for passed code

  CodeSize: Cardinal;
  CodeMask: Cardinal;
  FreeCode: Cardinal;
  OldCode: Cardinal;
  Prefix: array[0..4095] of Cardinal; // LZW prefix
  Suffix,                             // LZW suffix
  Stack: array [0..4095] of Byte;     // stack
  StackPointer: PByte;
  Target: PByte;
  FirstChar: Byte;  // Buffer for decoded byte
  ClearCode,
  EOICode: Word;

begin
  Target := OutBuffer;
  SourcePtr := Inbuffer;

  // initialize parameter
  ClearCode := 1 shl 8;
  EOICode := ClearCode + 1;
  FreeCode := ClearCode + 2;
  OldCode := NoLZWCode;
  CodeSize := 9;
  CodeMask := (1 shl CodeSize) - 1;

  // init code table
  for I := 0 to ClearCode - 1 do
  begin
    Prefix[I] := NoLZWCode;
    Suffix[I] := I;
  end;

  // initialize stack
  StackPointer := @Stack;
  FirstChar := 0;

  Data := 0;
  Bits := 0;
  while (InSize > 0) and (outSize > 0) do
  begin
    // read code from bit stream
    Inc(Data, NativeUInt(SourcePtr^) shl (24 - Bits));
    Inc(Bits, 8);
    while Bits >= CodeSize do
    begin
      // current code
      Code := (Data and ($FFFFFFFF - CodeMask)) shr (32 - CodeSize);
      // mask it
      Data := Data shl CodeSize;
      Dec(Bits, CodeSize);

      if Code = EOICode then Exit;

      // handling of clear codes
      if Code = ClearCode then
      begin
        // reset of all variables
        CodeSize := 9;
        CodeMask := (1 shl CodeSize) - 1;
        FreeCode := ClearCode + 2;
        OldCode := NoLZWCode;
        Continue;
      end;

      // check whether it is a valid, already registered code
      if Code > FreeCode then Break;

      // handling for the first LZW code: print and keep it
      if OldCode = NoLZWCode then
      begin
        FirstChar := Suffix[Code];
        Target^ := FirstChar;
        Inc(Target);
        Dec(outSize);
        OldCode := Code;
        Continue;
      end;

      // keep the passed LZW code
      InCode := Code;

      // the first LZW code is always smaller than FFirstCode
      if Code = FreeCode then
      begin
        StackPointer^ := FirstChar;
        Inc(StackPointer);
        Code := OldCode;
      end;

      // loop to put decoded bytes onto the stack
      while Code > ClearCode do
      begin
        StackPointer^ := Suffix[Code];
        Inc(StackPointer);
        Code := Prefix[Code];
      end;

      // place new code into code table
      FirstChar := Suffix[Code];
      StackPointer^ := FirstChar;
      Inc(StackPointer);
      Prefix[FreeCode] := OldCode;
      Suffix[FreeCode] := FirstChar;
      if FreeCode < 4096 then Inc(FreeCode);

      // increase code size if necessary
      if (FreeCode = CodeMask) and
         (CodeSize < 12) then
      begin
        Inc(CodeSize);
        CodeMask := (1 shl CodeSize) - 1;
      end;

      // put decoded bytes (from the stack) into the target Buffer
      OldCode := InCode;
      repeat
        Dec(StackPointer);
        Target^ := StackPointer^;
        Inc(Target);
        Dec(outSize);
      until NativeUInt(StackPointer) <= NativeUInt(@Stack);
    end;
    Inc(SourcePtr);
    Dec(InSize);
  end;
end;

end.
