unit DWL.HTTP.Types;

interface

type
  TdwlHTTPProgressEvent = procedure(ReceivedBytes, TotalBytes: cardinal; var CancelReceiving: boolean) of object;

implementation

end.
