unit DWL.MySQL.Utils;

interface

uses
  System.SysUtils;

type
  TdwlMySQLUtils = record
    /// <summary>
    ///   backtick an identifier for use in a SQL statement
    /// </summary>
    /// <param name="Identifier">
    ///   Identifier to backtick
    /// </param>
    class function BackTickIdentifier(const Identifier: string): string; static;
  end;

implementation

{ TfdlMySQLUtils }

class function TdwlMySQLUtils.BackTickIdentifier(const Identifier: string): string;
begin
  Result := '`'+Identifier+'`';
end;

end.
