unit DWL.Mail.SASL;

interface

uses
  IdSASL;

type
  TIdSASLOAuth2 = class(TIdSASL)
  private
    FToken: string;
    FUser: string;
  public
    property Token: string read FToken write FToken;
    property User: string read FUser write FUser;
    class function ServiceName: TIdSASLServiceName; override;
    function StartAuthenticate(const AChallenge, AHost, AProtocolName: string): string; override;
  end;

implementation

{ TIdSASLOAuth2 }

class function TIdSASLOAuth2.ServiceName: TIdSASLServiceName;
begin
  Result := 'XOAUTH2';
end;

function TIdSASLOAuth2.StartAuthenticate(const AChallenge, AHost, AProtocolName: string): string;
begin
  Result := 'user=' + FUser + Chr($01) + 'auth=Bearer ' + FToken + Chr($01) + Chr($01);
end;

end.
