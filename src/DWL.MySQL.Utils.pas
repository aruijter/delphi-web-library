unit DWL.MySQL.Utils;

interface

uses
  System.SysUtils, System.StrUtils;

type
  TdwlMySQLUtils = class abstract
    /// <summary>
    ///   backtick an identifier for use in a SQL statement
    /// </summary>
    /// <param name="Identifier">
    ///   Identifier to backtick
    /// </param>
    class function BackTickIdentifier(const Identifier: string): string;
  end;

implementation

{ TfdlMySQLUtils }

class function TdwlMySQLUtils.BackTickIdentifier(const Identifier: string): string;
begin
  Result := '`'+Identifier+'`';
end;

end.
