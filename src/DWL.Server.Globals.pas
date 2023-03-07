/// <summary>
///   <para>
///     Globals to be used everywhere in the application or the DLL.
///   </para>
///   <para>
///     Main usage is to expose the callback functions used when handling a
///     request. Serverside they point to the callback functions of the
///     server and are transferred to the DLL Module during configuration time.
//      This makes it possible to share state manupilation code between modules
///   </para>
/// </summary>
unit DWL.Server.Globals;

interface

uses
  DWL.Server.Types;

var
  serverProcs: TdwlCallBackProcs;

implementation

end.
