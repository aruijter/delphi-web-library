unit DWL.GridData;

interface

type
  TdwlGridDim = packed record
    WidthInPixels: word;
    HeightInPixels: word;
    LeftWorldX: double;
    TopWorldY: double;
    ScaleGridToWorld: double;
  end;

implementation

end.
