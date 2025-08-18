# DWL Distribution Format version 4
This is the definition of the DDF-format

## Header
The file always starts with the following header
||Datatype|Contents|Description|
|-|-|-|-
|0|UInt8|$03|FileVersion
|1|UInt16|$616C|Signature
|3|UInt8|$10|First Datablock offset
|4|UInt32|$00000000|Reserved1
|8|UInt64|$00000000|Reserved2

## Datablocks
Directly After the header, the file contains only datablocks. The datablocks are preferably aligned on a 16 byte pattern.
All datablocks are having the following header

|Datatype|Contents|Description|
|-|-|-|-
|0|UInt32||Total Size  of the datablock
|4|UInt16|$0401: File Content Header|Identification of the content of the datablock
|||$0402: Page Header
|||$0403: GridData
|||$0405: MetaData
|||$04FF: Deleted
|6|UInt8|$00: None|Compression kind of the data
|||$08: ZLib
|7|UInt8|$00|Reserved1
|8|UInt32||MasterID, the MasterID of the block, blocks must be linked to other blocks
|12|UInt32||ClientID, the ClientID of the block. My own ID
|16|UInt64||MetadataOffset, offset of the datablock containing metadata for this datablock
|24|UInt32|$00000000|Reserved2
|28|UInt32|$00000000|Reserved3

### File Content Header
The first Datablock of a DDF file is always a File Content Header block and has the following layout

||Datatype|Contents|Description|
|-|-|-|-
|0|UInt32|$0000003A|Total Size  of the datablock
|4|UInt16|$0401|Identification File Content Header
|6|UInt8|$00|No Compression
|7|UInt8|$00|Reserved1
|8|UInt32|$00000000|MasterID
|12|UInt32|$00000001|ClientID
|16|UInt64||MetadataOffset
|24|UInt32|$00000000|Reserved2
|28|UInt32|$00000000|Reserved3
|32|UInt64||First Data Page Offset
|40|UInt32||Total Pagecount
|44|UInt32|$000010E6|Coordinate system, for now EPGS-4326
|48|Bounds||The Outer Bounds of the content of all pages. Rectangular area including area's of all full pixels, not the gummiband around the datapoints
|80|UInt32|$00000000|Reserved4
|84|UInt32|$00000000|Reserved5
|88|UInt32|$00000000|Reserved6
|92|UInt32|$00000000|Reserved7

### Data Page
||Datatype|Contents|Description|
|-|-|-|-
|0|UInt32||Total Size  of the datablock
|4|UInt16|$0402|Identifiction Page Header
|6|UInt8|$00|No Compression
|7|UInt8|$00|Reserved1
|8|UInt32|$00000001|MasterID
|12|UInt32||ClientID
|16|UInt64||MetadataOffset
|24|UInt32|$00000000|Reserved2
|28|UInt32|$00000000|Reserved3
|32|UInt16|$0000: Int8|DataType
|||$0001: UInt8
|||$0002: Int16
|||$0003: UIInt16
|||$0004: Int32
|||$0005: UInt32
|||$0006: Int64
|||$0007: UInt64
|||$0080: Single
|||$0081: Double
|34|Int64||NoDataValue
|42|Double||ValueScale
|50|Double||ValueOffset
|58|UInt8|$01: NoDataValueUsed|DataTypeFlags
|||$02: ValueScalingUsed
|||$04: ValueOffsetUsed
|59|Int8||Reserved5
|60|Int32||Reserved6
|64|UInt32||TileColCount
|68|UInt32||TileRowCount
|72|UInt32||WidthInTiles
|76|UInt32||HeightInTiles
|80|UInt64||NextDataPageOffset
|88|UInt32|$00000000|Reserved7
|92|UInt32|$00000000|Reserved8
|96|bounds||The Outer Bounds of the page content. Rectangular area including area's of all full pixels, not the 'gummiband' around the datapoints
|128|UInt64*N||TileDataBlockOffsets, row by row. N=TileColCount*TileRowCount.

### MetaData
||Datatype|Contents|Description|
|-|-|-
|0|UInt32||Total Size  of the datablock
|4|UInt16|$0405|Identifiction Page Header
|6|UInt8|$05|ZLib Compression
|7|UInt8|$00|Reserved1
|8|UInt32||MasterID
|12|UInt32||ClientID
|16|UInt64||MetadataOffset
|24|UInt32|$00000000|Reserved2
|28|UInt32|$00000000|Reserved3
||Metadata*N|The Actual Metadata pairs terminated by one 0

## Datatypes
|Char|bytecount|description
|-|-|-
|unsigned byte|1|$00 : $FF, An UTF8 encoded character

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
|UInt16|2|Value string length (=L2), UTF8 encoded
|Char*L2|L2|Value string


