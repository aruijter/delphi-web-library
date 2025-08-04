unit DWL.GridData;

interface

type
  TdwlGridDim = packed record // do not change this, is f.e. used in DDF version 4 Files
    WidthInPixels: word;
    HeightInPixels: word;
    LeftWorldX: double;
    TopWorldY: double;
    ScaleGridToWorld: double;
  end;

implementation

end.
