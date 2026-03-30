unit DWL.GridData;

interface

uses
  DWL.Types, System.Classes;

type
  IdwlGridCursor = interface
    function Dim: TdwlGridDim;
    function BytesPerElement: byte;
    function GridDataType: TdwlGridDataType;
    procedure GetDataRef(var Data: PByte; var DataSize: UInt64);
    function GetValue(Left, Top: UInt64): double;
    function ReadValue: double;
    function CursorPtr: PByte;
    procedure Seek(ElementsOffset: Int64; Origin: TSeekOrigin=soBeginning); overload;
    procedure Seek(Left, Top: UInt64); overload;
  end;

  IdwlGridCursor_Write = interface(IdwlGridCursor)
    function EnforceDataType(GridDataType: TdwlGridDataType): IdwlGridCursor_Write;
    procedure Fill(Value: double); overload;
    procedure Fill(SubGridCur: IdwlGridCursor); overload;
    procedure SetBareValues(Left, Top: UInt64; const Value; const Count:UInt64);
    procedure SetDim(NewDim: TdwlGridDim);
    procedure SetValue(Left, Top: UInt64; Value: double);
    procedure WriteBareValue(const Value);
    procedure WriteValue(Value: double);
  end;

  IdwlGridData = interface
    function Dim: TdwlGridDim;
    function GetCursor: IdwlGridCursor;
    function GetWriteCursor: IdwlGridCursor_Write;
    function GridDataType: TdwlGridDataType;
  end;

function New_GridData(Dim: TdwlGridDim; GridDataType: TdwlGridDataType; PreparedDataBlock: PByte=nil): IdwlGridData;

implementation

uses
  System.SysUtils, System.TypInfo, System.Math, System.SyncObjs,
  DWL.Math;

type
  TdwlGridCursor = class(TInterfacedObject, IdwlGridCursor)
  strict private
    type
      TDouble_Write = reference to procedure(P: PByte; const Value: double);
      TDouble_Read = reference to procedure(P: PByte; var Value: double);
  private
    FDouble_Write: TDouble_Write;
    FDouble_Read: TDouble_Read;
    FGridData: IdwlGridData;
    FData: PByte;
    FDataSize: UInt64;
    FDim: TdwlGridDim;
    FGridDataType: TdwlGridDataType;
    FBytesperElement: byte;
    FCursor: PByte;
    FWritable: boolean;
    procedure InitializeDataVarsAndFunctions;
    function Dim: TdwlGridDim;
    function CursorPtr: PByte;
    function BytesPerElement: byte;
    function GridDataType: TdwlGridDataType;
    procedure GetDataRef(var Data: PByte; var DataSize: UInt64);
    function GetValue(Left, Top: UInt64): double;
    function ReadValue: double;
    procedure Seek(ElementsOffset: Int64; Origin: TSeekOrigin=soBeginning); overload;
    procedure Seek(Left, Top: UInt64); overload;
  public
    constructor Create(AGridData: IdwlGridData; Writable: boolean);
    destructor Destroy; override;
  end;

  TdwlGridCursor_Write = class(TdwlGridCursor, IdwlGridCursor_Write)
  private
    function EnforceDataType(GridDataType: TdwlGridDataType): IdwlGridCursor_Write;
    procedure Fill(Value: double); overload;
    procedure Fill(SubGridCur: IdwlGridCursor); overload;
    procedure SetBareValues(Left, Top: UInt64; const Value; const Count:UInt64);
    procedure SetDim(NewDim: TdwlGridDim);
    procedure SetValue(Left, Top: UInt64; Value: double);
    procedure WriteBareValue(const Value);
    procedure WriteValue(Value: double);
  end;

  TdwlGridData = class(TInterfacedObject, IdwlGridData)
  private
    FData: PByte;
    FDim: TdwlGridDim;
    FGridDataType: TdwlGridDataType;
    FSynchronizer: TLightweightMREW;
    function Dim: TdwlGridDim;
    function GetCursor: IdwlGridCursor;
    function GetWriteCursor: IdwlGridCursor_Write;
    function GridDataType: TdwlGridDataType;
  public
    constructor Create(ADim: TdwlGridDim; AGridDataType: TdwlGridDataType; PreparedDataBlock: PByte);
    destructor Destroy; override;
  end;

function New_GridData(Dim: TdwlGridDim; GridDataType: TdwlGridDataType; PreparedDataBlock: PByte=nil): IdwlGridData;
begin
  Result := TdwlGridData.Create(Dim, GridDataType, PreparedDataBlock);
end;

{ TdwlGridData }

constructor TdwlGridData.Create(ADim: TdwlGridDim; AGridDataType: TdwlGridDataType; PreparedDataBlock: PByte);
begin
  inherited Create;
  FGridDataType := AGridDataType;
  FDim := ADim;
  FData := PreparedDataBlock;
  if FData=nil then
  begin
    var RequestedSize: UInt64 := FDim.WidthInPixels*FDim.HeightInPixels*FGridDataType.Size;
    GetMem(FData, RequestedSize);
  end;
end;

destructor TdwlGridData.Destroy;
begin
  FreeMem(FData);
  inherited Destroy;
end;

function TdwlGridData.Dim: TdwlGridDim;
begin
  Result := FDim;
end;

function TdwlGridData.GetCursor: IdwlGridCursor;
begin
  Result := TdwlGridCursor.Create(Self, false);
end;

function TdwlGridData.GetWriteCursor: IdwlGridCursor_Write;
begin
  Result := TdwlGridCursor_Write.Create(Self, true);
end;

function TdwlGridData.GridDataType: TdwlGridDataType;
begin
  Result := FGridDataType;
end;

{ TdwlGridCursor }

function TdwlGridCursor.BytesPerElement: byte;
begin
  Result := FBytesperElement;
end;

constructor TdwlGridCursor.Create(AGridData: IdwlGridData; Writable: boolean);
begin
  inherited Create;
  FGridData := AGridData;
  FWritable := Writable;
  if FWritable then
    TdwlGridData(FGridData).FSynchronizer.BeginWrite
  else
    TdwlGridData(FGridData).FSynchronizer.BeginRead;
  InitializeDataVarsAndFunctions;
  Seek(0);
end;

function TdwlGridCursor.CursorPtr: PByte;
begin
  Result := FCursor;
end;

destructor TdwlGridCursor.Destroy;
begin
  if FWritable then
    TdwlGridData(FGridData).FSynchronizer.EndWrite
  else
    TdwlGridData(FGridData).FSynchronizer.EndRead;
  inherited Destroy;
end;

function TdwlGridCursor.Dim: TdwlGridDim;
begin
  Result := FDim;
end;

procedure TdwlGridCursor.GetDataRef(var Data: PByte; var DataSize: UInt64);
begin
  Data := FData;
  DataSize := FDataSize;
end;

function TdwlGridCursor.GetValue(Left, Top: UInt64): double;
begin
  var P := FData;
  inc(P, (Left+Top*FDim.WidthInPixels)*FBytesPerElement);
  FDouble_Read(P, Result);
end;

function TdwlGridCursor.GridDataType: TdwlGridDataType;
begin
  Result := FGridDataType;
end;

procedure TdwlGridCursor.InitializeDataVarsAndFunctions;
begin
  // Have these variables local for efficiency
  FData := TdwlGridData(FGridData).FData;
  FDim := TdwlGridData(FGridData).FDim;
  FGridDataType := TdwlGridData(FGridData).FGridDataType;
  FBytesperElement := FGridDataType.Size;
  FDataSize := FDim.WidthInPixels*FDim.HeightInPixels*FBytesperElement;

  if FGridDataType.DataType<=dwlUInt64 then
  begin
    if (FGridDataType.DataTypeFlags and flagNoDataValueUsed)>0 then
    begin
      if (FGridDataType.DataTypeFlags and flagValueScalingUsed)>0 then
      begin
        if (FGridDataType.DataTypeFlags and flagValueOffsetUsed)>0 then
        begin
          case FGridDataType.DataType of
          dwlInt8:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PShortInt(P)^=PShortInt(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PShortInt(P)^*FGridDataType.ValueScale+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PShortInt(P)^ := FGridDataType.NoDataValue else PShortInt(P)^:= Round((Value-FGridDataType.ValueOffset)/FGridDataType.ValueScale); end;
            end;
          dwlUInt8:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (P^=PByte(@FGridDataType.NoDataValue)^) then Value := NaN else Value := P^*FGridDataType.ValueScale+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then P^ := FGridDataType.NoDataValue else P^:= Round((Value-FGridDataType.ValueOffset)/FGridDataType.ValueScale); end;
            end;
          dwlInt16:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PSmallInt(P)^=PSmallInt(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PSmallInt(P)^*FGridDataType.ValueScale+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PSmallInt(P)^ := FGridDataType.NoDataValue else PSmallInt(P)^:= Round((Value-FGridDataType.ValueOffset)/FGridDataType.ValueScale); end;
            end;
          dwlUInt16:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PWord(P)^=PWord(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PWord(P)^*FGridDataType.ValueScale+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PWord(P)^ := FGridDataType.NoDataValue else PWord(P)^:= Round((Value-FGridDataType.ValueOffset)/FGridDataType.ValueScale); end;
            end;
          dwlInt32:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PInteger(P)^=PInteger(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PInteger(P)^*FGridDataType.ValueScale+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PInteger(P)^ := FGridDataType.NoDataValue else PInteger(P)^:= Round((Value-FGridDataType.ValueOffset)/FGridDataType.ValueScale); end;
            end;
          dwlUInt32:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PCardinal(P)^=PCardinal(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PCardinal(P)^*FGridDataType.ValueScale+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PCardinal(P)^ := FGridDataType.NoDataValue else PCardinal(P)^:= Round((Value-FGridDataType.ValueOffset)/FGridDataType.ValueScale); end;
            end;
          dwlInt64:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PInt64(P)^=PInt64(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PInt64(P)^*FGridDataType.ValueScale+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PInt64(P)^ := FGridDataType.NoDataValue else PInt64(P)^:= Round((Value-FGridDataType.ValueOffset)/FGridDataType.ValueScale); end;
            end;
          dwlUInt64:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PUInt64(P)^=PUInt64(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PUInt64(P)^*FGridDataType.ValueScale+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PUInt64(P)^ := FGridDataType.NoDataValue else PUInt64(P)^:= Round((Value-FGridDataType.ValueOffset)/FGridDataType.ValueScale); end;
            end;
          end;
        end
        else
        begin
          case FGridDataType.DataType of
          dwlInt8:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PShortInt(P)^=PShortInt(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PShortInt(P)^*FGridDataType.ValueScale; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PShortInt(P)^ := FGridDataType.NoDataValue else PShortInt(P)^:= Round(Value/FGridDataType.ValueScale); end;
            end;
          dwlUInt8:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (P^=PByte(@FGridDataType.NoDataValue)^) then Value := NaN else Value := P^*FGridDataType.ValueScale; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then P^ := FGridDataType.NoDataValue else P^:= Round(Value/FGridDataType.ValueScale); end;
            end;
          dwlInt16:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PSmallInt(P)^=PSmallInt(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PSmallInt(P)^*FGridDataType.ValueScale; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PSmallInt(P)^ := FGridDataType.NoDataValue else PSmallInt(P)^:= Round(Value/FGridDataType.ValueScale); end;
            end;
          dwlUInt16:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PWord(P)^=PWord(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PWord(P)^*FGridDataType.ValueScale; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PWord(P)^ := FGridDataType.NoDataValue else PWord(P)^:= Round(Value/FGridDataType.ValueScale); end;
            end;
          dwlInt32:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PInteger(P)^=PInteger(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PInteger(P)^*FGridDataType.ValueScale; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PInteger(P)^ := FGridDataType.NoDataValue else PInteger(P)^:= Round(Value/FGridDataType.ValueScale); end;
            end;
          dwlUInt32:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PCardinal(P)^=PCardinal(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PCardinal(P)^*FGridDataType.ValueScale; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PCardinal(P)^ := FGridDataType.NoDataValue else PCardinal(P)^:= Round(Value/FGridDataType.ValueScale); end;
            end;
          dwlInt64:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PInt64(P)^=PInt64(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PInt64(P)^*FGridDataType.ValueScale; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PInt64(P)^ := FGridDataType.NoDataValue else PInt64(P)^:= Round(Value/FGridDataType.ValueScale); end;
            end;
          dwlUInt64:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PUInt64(P)^=PUInt64(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PUInt64(P)^*FGridDataType.ValueScale; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PUInt64(P)^ := FGridDataType.NoDataValue else PUInt64(P)^:= Round(Value/FGridDataType.ValueScale); end;
            end;
          end;
        end;
      end
      else
      begin
        if (FGridDataType.DataTypeFlags and flagValueOffsetUsed)>0 then
        begin
          case FGridDataType.DataType of
          dwlInt8:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PShortInt(P)^=PShortInt(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PShortInt(P)^+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PShortInt(P)^ := FGridDataType.NoDataValue else PShortInt(P)^:= Round(Value-FGridDataType.ValueOffset)end;
            end;
          dwlUInt8:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (P^=PByte(@FGridDataType.NoDataValue)^) then Value := NaN else Value := P^+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then P^ := FGridDataType.NoDataValue else P^:= Round(Value-FGridDataType.ValueOffset); end;
            end;
          dwlInt16:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PSmallInt(P)^=PSmallInt(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PSmallInt(P)^+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PSmallInt(P)^ := FGridDataType.NoDataValue else PSmallInt(P)^:= Round(Value-FGridDataType.ValueOffset); end;
            end;
          dwlUInt16:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PWord(P)^=PWord(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PWord(P)^+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PWord(P)^ := FGridDataType.NoDataValue else PWord(P)^:= Round(Value-FGridDataType.ValueOffset); end;
            end;
          dwlInt32:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PInteger(P)^=PInteger(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PInteger(P)^+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PInteger(P)^ := FGridDataType.NoDataValue else PInteger(P)^:= Round(Value-FGridDataType.ValueOffset); end;
            end;
          dwlUInt32:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PCardinal(P)^=PCardinal(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PCardinal(P)^+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PCardinal(P)^ := FGridDataType.NoDataValue else PCardinal(P)^:= Round(Value-FGridDataType.ValueOffset); end;
            end;
          dwlInt64:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PInt64(P)^=PInt64(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PInt64(P)^+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PInt64(P)^ := FGridDataType.NoDataValue else PInt64(P)^:= Round(Value-FGridDataType.ValueOffset); end;
            end;
          dwlUInt64:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PUInt64(P)^=PUInt64(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PUInt64(P)^+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PUInt64(P)^ := FGridDataType.NoDataValue else PUInt64(P)^:= Round(Value-FGridDataType.ValueOffset); end;
            end;
          end;
        end
        else
        begin
          case FGridDataType.DataType of
          dwlInt8:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PShortInt(P)^=PShortInt(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PShortInt(P)^; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PShortInt(P)^ := FGridDataType.NoDataValue else PShortInt(P)^:= Round(Value); end;
            end;
          dwlUInt8:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (P^=PByte(@FGridDataType.NoDataValue)^) then Value := NaN else Value := P^; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then P^ := FGridDataType.NoDataValue else P^:= Round(Value); end;
            end;
          dwlInt16:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PSmallInt(P)^=PSmallInt(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PSmallInt(P)^; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PSmallInt(P)^ := FGridDataType.NoDataValue else PSmallInt(P)^:= Round(Value); end;
            end;
          dwlUInt16:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PWord(P)^=PWord(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PWord(P)^; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PWord(P)^ := FGridDataType.NoDataValue else PWord(P)^:= Round(Value); end;
            end;
          dwlInt32:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PInteger(P)^=PInteger(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PInteger(P)^; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PInteger(P)^ := FGridDataType.NoDataValue else PInteger(P)^:= Round(Value); end;
            end;
          dwlUInt32:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PCardinal(P)^=PCardinal(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PCardinal(P)^; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PCardinal(P)^ := FGridDataType.NoDataValue else PCardinal(P)^:= Round(Value); end;
            end;
          dwlInt64:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PInt64(P)^=PInt64(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PInt64(P)^; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PInt64(P)^ := FGridDataType.NoDataValue else PInt64(P)^:= Round(Value); end;
            end;
          dwlUInt64:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin if (PUInt64(P)^=PUInt64(@FGridDataType.NoDataValue)^) then Value := NaN else Value := PUInt64(P)^; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin if IsNan(Value) then PUInt64(P)^ := FGridDataType.NoDataValue else PUInt64(P)^:= Round(Value); end;
            end;
          end;
        end
      end;
    end
    else
    begin
      if (FGridDataType.DataTypeFlags and flagValueScalingUsed)>0 then
      begin
        if (FGridDataType.DataTypeFlags and flagValueOffsetUsed)>0 then
        begin
          case FGridDataType.DataType of
          dwlInt8:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PShortInt(P)^*FGridDataType.ValueScale+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PShortInt(P)^:= Round((Value-FGridDataType.ValueOffset)/FGridDataType.ValueScale); end;
            end;
          dwlUInt8:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := P^*FGridDataType.ValueScale+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin P^:= Round((Value-FGridDataType.ValueOffset)/FGridDataType.ValueScale); end;
            end;
          dwlInt16:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PSmallInt(P)^*FGridDataType.ValueScale+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PSmallInt(P)^:= Round((Value-FGridDataType.ValueOffset)/FGridDataType.ValueScale); end;
            end;
          dwlUInt16:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PWord(P)^*FGridDataType.ValueScale+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PWord(P)^:= Round((Value-FGridDataType.ValueOffset)/FGridDataType.ValueScale); end;
            end;
          dwlInt32:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PInteger(P)^*FGridDataType.ValueScale+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PINteger(P)^:= Round((Value-FGridDataType.ValueOffset)/FGridDataType.ValueScale); end;
            end;
          dwlUInt32:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PCardinal(P)^*FGridDataType.ValueScale+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PCardinal(P)^:= Round((Value-FGridDataType.ValueOffset)/FGridDataType.ValueScale); end;
            end;
          dwlInt64:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PInt64(P)^*FGridDataType.ValueScale+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PInt64(P)^:= Round((Value-FGridDataType.ValueOffset)/FGridDataType.ValueScale); end;
            end;
          dwlUInt64:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PUInt64(P)^*FGridDataType.ValueScale+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PUInt64(P)^:= Round((Value-FGridDataType.ValueOffset)/FGridDataType.ValueScale); end;
            end;
          end;
        end
        else
        begin
          case FGridDataType.DataType of
          dwlInt8:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PShortInt(P)^*FGridDataType.ValueScale; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PShortInt(P)^:= Round(Value/FGridDataType.ValueScale); end;
            end;
          dwlUInt8:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := P^*FGridDataType.ValueScale; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin P^:= Round(Value/FGridDataType.ValueScale); end;
            end;
          dwlInt16:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PSmallInt(P)^*FGridDataType.ValueScale; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PSmallInt(P)^:= Round(Value/FGridDataType.ValueScale); end;
            end;
          dwlUInt16:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PWord(P)^*FGridDataType.ValueScale; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PWord(P)^:= Round(Value/FGridDataType.ValueScale); end;
            end;
          dwlInt32:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PInteger(P)^*FGridDataType.ValueScale; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PINteger(P)^:= Round(Value/FGridDataType.ValueScale); end;
            end;
          dwlUInt32:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PCardinal(P)^*FGridDataType.ValueScale; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PCardinal(P)^:= Round(Value/FGridDataType.ValueScale); end;
            end;
          dwlInt64:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PInt64(P)^*FGridDataType.ValueScale; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PInt64(P)^:= Round(Value/FGridDataType.ValueScale); end;
            end;
          dwlUInt64:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PUInt64(P)^*FGridDataType.ValueScale; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PUInt64(P)^:= Round(Value/FGridDataType.ValueScale); end;
            end;
          end;
        end
      end
      else
      begin
        if (FGridDataType.DataTypeFlags and flagValueOffsetUsed)>0 then
        begin
          case FGridDataType.DataType of
          dwlInt8:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PShortInt(P)^+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PShortInt(P)^:= Round(Value-FGridDataType.ValueOffset); end;
            end;
          dwlUInt8:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := P^+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin P^:= Round(Value-FGridDataType.ValueOffset); end;
            end;
          dwlInt16:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PSmallInt(P)^+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PSmallInt(P)^:= Round(Value-FGridDataType.ValueOffset); end;
            end;
          dwlUInt16:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PWord(P)^+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PWord(P)^:= Round(Value-FGridDataType.ValueOffset); end;
            end;
          dwlInt32:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PInteger(P)^+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PINteger(P)^:= Round(Value-FGridDataType.ValueOffset); end;
            end;
          dwlUInt32:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PCardinal(P)^+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PCardinal(P)^:= Round(Value-FGridDataType.ValueOffset); end;
            end;
          dwlInt64:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PInt64(P)^+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PInt64(P)^:= Round(Value-FGridDataType.ValueOffset); end;
            end;
          dwlUInt64:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PUInt64(P)^+FGridDataType.ValueOffset; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PUInt64(P)^:= Round(Value-FGridDataType.ValueOffset); end;
            end;
          end;
        end
        else
        begin
          case FGridDataType.DataType of
          dwlInt8:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PShortInt(P)^; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PShortInt(P)^:= Round(Value); end;
            end;
          dwlUInt8:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := P^; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin P^:= Round(Value); end;
            end;
          dwlInt16:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PSmallInt(P)^; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PSmallInt(P)^:= Round(Value); end;
            end;
          dwlUInt16:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PWord(P)^; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PWord(P)^:= Round(Value); end;
            end;
          dwlInt32:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PInteger(P)^; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PINteger(P)^:= Round(Value); end;
            end;
          dwlUInt32:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PCardinal(P)^; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PCardinal(P)^:= Round(Value); end;
            end;
          dwlInt64:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PInt64(P)^; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PInt64(P)^:= Round(Value); end;
            end;
          dwlUInt64:
            begin
              FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PUInt64(P)^; end;
              FDouble_Write := procedure(P: PByte; const Value: double) begin PUInt64(P)^:= Round(Value); end;
            end;
          end;
        end
      end;
    end;
  end
  else
  begin
    case FGridDataType.DataType of
    dwlSingle:
      begin
        if (FGridDataType.DataTypeFlags and flagValueScalingUsed)>0 then
        begin
          if (FGridDataType.DataTypeFlags and flagValueOffsetUsed)>0 then
          begin
            FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PSingle(P)^*FGridDataType.ValueScale+FGridDataType.ValueOffset; end;
            FDouble_Write := procedure(P: PByte; const Value: double) begin PSingle(P)^:= (Value-FGridDataType.ValueOffset)/FGridDataType.ValueScale; end;
          end
          else
          begin
            FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PSingle(P)^*FGridDataType.ValueScale; end;
            FDouble_Write := procedure(P: PByte; const Value: double) begin PSingle(P)^:= Value/FGridDataType.ValueScale; end;
          end;
        end
        else
        begin
          if (FGridDataType.DataTypeFlags and flagValueOffsetUsed)>0 then
          begin
            FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PSingle(P)^+FGridDataType.ValueOffset; end;
            FDouble_Write := procedure(P: PByte; const Value: double) begin PSingle(P)^:= Value-FGridDataType.ValueOffset; end;
          end
          else
          begin
            FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PSingle(P)^; end;
            FDouble_Write := procedure(P: PByte; const Value: double) begin PSingle(P)^:= Value; end;
          end;
        end;
      end;
    dwlDouble:
      begin
        if (FGridDataType.DataTypeFlags and flagValueScalingUsed)>0 then
        begin
          if (FGridDataType.DataTypeFlags and flagValueOffsetUsed)>0 then
          begin
            FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PDouble(P)^*FGridDataType.ValueScale+FGridDataType.ValueOffset; end;
            FDouble_Write := procedure(P: PByte; const Value: double) begin PDouble(P)^:= (Value-FGridDataType.ValueOffset)/FGridDataType.ValueScale; end;
          end
          else
          begin
            FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PDouble(P)^*FGridDataType.ValueScale; end;
            FDouble_Write := procedure(P: PByte; const Value: double) begin PDouble(P)^:= Value/FGridDataType.ValueScale; end;
          end;
        end
        else
        begin
          if (FGridDataType.DataTypeFlags and flagValueOffsetUsed)>0 then
          begin
            FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PDouble(P)^+FGridDataType.ValueOffset; end;
            FDouble_Write := procedure(P: PByte; const Value: double) begin PDouble(P)^:= Value-FGridDataType.ValueOffset; end;
          end
          else
          begin
            FDouble_Read := procedure(P: PByte; var Value: double) begin Value := PDouble(P)^; end;
            FDouble_Write := procedure(P: PByte; const Value: double) begin PDouble(P)^:= Value; end;
          end;
        end
      end;
    end;
  end;
end;

function TdwlGridCursor.ReadValue: double;
begin
  FDouble_Read(FCursor, Result);
  inc(FCursor, FBytesPerElement);
end;

procedure TdwlGridCursor.Seek(ElementsOffset: Int64; Origin: TSeekOrigin);
begin
  case Origin of
    soBeginning: FCursor := FData+ElementsOffset*FBytesPerElement;
    soCurrent: FCursor := FCursor+ElementsOffset*FBytesPerElement;
    soEnd: FCursor := FData+FDataSize-ElementsOffset*FBytesPerElement;
  end;
end;

procedure TdwlGridCursor.Seek(Left, Top: UInt64);
begin
  Seek(Left+Top*FDim.WidthInPixels);
end;

{ TdwlGridCursor_Write }

function TdwlGridCursor_Write.EnforceDataType(GridDataType: TdwlGridDataType): IdwlGridCursor_Write;
begin
  if FGridDataType=GridDataType then
    Exit(Self);
  Result := New_GridData(FDim, GridDataType).GetWriteCursor;
  Seek(0);
  for var i := 1 to FDim.WidthInPixels*FDim.HeightInPixels do
    Result.WriteValue(ReadValue);
end;

procedure TdwlGridCursor_Write.Fill(Value: double);
begin
  FDouble_Write(FData, Value);
  var BytesDone: UInt64 := FBytesperElement;
  while (BytesDone shl 1)<=FDataSize do
  begin
    Move(FData^, (FData+BytesDone)^, BytesDone);
    BytesDone := BytesDone shl 1;
  end;
  Move(FData^, (PByte(FData)+BytesDone)^, FDataSize-BytesDone);
end;


procedure TdwlGridCursor_Write.Fill(SubGridCur: IdwlGridCursor);
begin
  var SubGridData: PByte;
  var SubGridDataSize: UInt64;
  SubGridCur.GetDataRef(SubGridData, SubGridDataSize);
  var SameDataType := GridDataType=SubGridCur.GridDataType;
  if SameDataType and (FDim=SubGridCur.Dim) then
  begin
    Move(SubGridData^, FData^, SubGridDataSize);
    Exit;
  end;
  if not TdwlMathUtils.FuzzyEqual(FDim.ScaleGridToWorld, SubGridCur.Dim.ScaleGridToWorld) then
    raise Exception.Create('Scale change not supported yet');

  var  OffsetX := round((SubGridCur.Dim.LeftWorldX-FDim.LeftWorldX)/FDim.ScaleGridToWorld);
  if (OffsetX < 0) and (-OffsetX >= SubGridCur.Dim.WidthInPixels) then
    Exit;
  if (OffsetX > 0) and (OffsetX >= FDim.WidthInPixels) then
    Exit;

  var XFrom := 0;
  if OffsetX < 0 then
    XFrom := Min(SubGridCur.Dim.WidthInPixels-1, UInt64(Max(0, -OffsetX)));

  var XTo: Int64 := SubGridCur.Dim.WidthInPixels-1;
  if (XTo + OffsetX) < 0 then // nothing to do
    Exit;
  if (XTo + OffsetX) > (FDim.WidthInPixels-1) then
    XTo := (Int64(FDim.WidthInPixels)-1 - OffsetX);

  var XAmount := Xto - XFrom + 1;

  var OffsetY := round((FDim.TopWorldY-SubGridCur.Dim.TopWorldY)/FDim.ScaleGridToWorld);

  if (OffsetY < 0) and (-OffsetY >= SubGridCur.Dim.HeightInPixels) then
    Exit;
  if (OffsetY > 0) and (OffsetY >= FDim.HeightInPixels) then
    Exit;

  for var Row: Int64 := Max(0, -OffsetY) to Min(Int64(SubGridCur.Dim.HeightInPixels), Int64(FDim.HeightInPixels)-OffsetY)-1 do
  begin
    SubGridCur.Seek(XFrom, Row);
    if SameDataType then
      SetBareValues(XFrom+OffsetX, Row+OffsetY, SubGridCur.CursorPtr^, XAmount)
    else
    begin
      Seek(XFrom+OffsetX, Row+OffsetY);
      for var Col := XFrom to XTo do
        WriteValue(SubGridCur.ReadValue);
    end;
  end;
end;

procedure TdwlGridCursor_Write.WriteBareValue(const Value);
begin
  Move(Value, FCursor^, FBytesperElement);
  inc(FCursor, FBytesperElement);
end;

procedure TdwlGridCursor_Write.SetBareValues(Left, Top: UInt64; const Value; const Count: UInt64);
begin
  var P := FData;
  inc(P, (Left+Top*FDim.WidthInPixels)*FBytesPerElement);
  Move(Value, P^, FBytesperElement*Count);
end;

procedure TdwlGridCursor_Write.SetDim(NewDim: TdwlGridDim);
begin
  if (FDim.WidthInPixels*FDim.HeightInPixels)<>(NewDim.WidthInPixels*NewDim.HeightInPixels) then
    raise Exception.Create('SetDim: ElementCount is not the same');
  FDim := NewDim;
  TdwlGridData(FGridData).FDim := NewDim;
end;

procedure TdwlGridCursor_Write.SetValue(Left, Top: UInt64; Value: double);
begin
  var P := FData;
  inc(P, (Left+Top*FDim.WidthInPixels)*FBytesPerElement);
  FDouble_Write(P, Value);
end;

procedure TdwlGridCursor_Write.WriteValue(Value: double);
begin
  FDouble_Write(FCursor, Value);
  inc(FCursor, FBytesPerElement);
end;

end.


