# Delinea Data Format version 3
This is the definition of the now legacy version 3 of the DDF format

## Header
The file always starts with the following header
|Datatype|Contents|Description|
|-|-|-
|UInt8|$03|FileVersion
|UInt16|$616C|Signature
|UInt8|$00|Reserved0

## Datablocks
Directly After the header, the file contains only datablocks. All datablocks are having the following header

|Datatype|Contents|Description|
|-|-|-
|UInt32||Total Size  of the datablock
|UInt16|$0001: File Content Header|Identification of the content of the datablock
||$0002: Page Header
||$0003: Data Array
||$0005: MetaData
||$00FF: Deleted
|UInt8|$00: None|Compression kind
||$08: ZLib
|UInt8|$00|Reserved1
|UInt32||MasterID, the MasterID of the block, blocks must be linked to other blocks
|UInt32||ClientID, the ClientID of the block. My own ID
|UInt32||MetadataOffset, offset of the datablock containing metadata for this datablock

### File Content Header
The first Datablock of a DDF file is always a File Content Header block and has the following layout

|Datatype|Contents|Description|
|-|-|-
|UInt32|$0000003A|Total Size  of the datablock
|UInt16|$0001|Identification File Content Header
|UInt8|$00|No Compression
|UInt8|$00|Reserved1
|UInt32|$00000000|MasterID
|UInt32|$00000001|ClientID
|UInt32||MetadataOffset
|UInt32||First Data Page Offset
|UInt32||Total Pagecount
|UInt32|$000010E6|Coordinate system, for now EPGS-4326
|bounds||The Defined Bounds of the content
|bounds||The Actual Bounds of the content
|UInt32|$00000000|Reserved2

### Page
|Datatype|Contents|Description|
|-|-|-
|UInt32||Total Size  of the datablock
|UInt16|$0002|Identification Page Header
|UInt8|$00|No Compression
|UInt8|$00|Reserved1
|UInt32|$00000001|MasterID
|UInt32||ClientID
|UInt32||MetadataOffset
|UInt16|$0000: Int8|DataType
||$0001: UInt8
||$0002: Int16
||$0003: UIInt16
||$0004: Int32
||$0005: UInt32
||$0006: Int64
||$0007: UInt64
||$0080: Single
||$0081: Double
|Int8||Base 10 Exponent
|Bool||HighUsedAsNoDataValue
|UInt8||Reserved2
|UInt32||TileColCount
|UInt32||TileRowCount
|UInt32||WidthInTiles
|UInt32||HeightInTiles
|UInt32||MetaDataOffset
|UInt32||NextDataPageOffset
|UInt32*N||TileDataBlockOffsets, row by row. N=TileColCount*TileRowCount.

### MetaData
|Datatype|Contents|Description|
|-|-|-
|UInt32||Total Size  of the datablock
|UInt16|$0005|Identification Metdata
|UInt8|$00|No Compression
|UInt8|$00|Reserved1
|UInt32||MasterID
|UInt32||ClientID
|UInt32||MetadataOffset
|Metadata*N|The Actual Metadata pairs terminated by an extra 0

## Datatypes
|Int8|bytecount|description
|-|-|-
|signed byte|1|-$7F : $7F

|UInt8|bytecount|description
|-|-|-
|unsigned byte|1|$00 : $FF

|UInt16|bytecount|description
|-|-|-
|unsigned word|2|$0000 : $FFFF

|UInt32|bytecount|description
|-|-|-
|unsigned integer|4|$00000000 : $FFFFFFFF

|Bool|bytecount|description
|-|-|-
|byte|1|Logical: $00: false, other values: true

|Double|bytecount|description
|-|-|-
|binary64|8|IEEE 754 double-precision binary floating-point

|Bounds|bytecount|description|
|-|-|-
|Double|8|XMin|
|Double|8|XMax|
|Double|8|YMin|
|Double|8|YMax|

|Metadata|bytecount|description|
|-|-|-
|UInt8|1|Key string length (=L1)
|Char*L1|L1|Key string
|UInt16|2|Value string length (=L2)
|Char*L2|L2|Value string