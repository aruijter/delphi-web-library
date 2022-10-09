unit DWL.HTTP.APIClient.OAuth2.LoginForm;

interface

uses
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.OleCtrls, SHDocVw, System.Classes,
  Vcl.ExtCtrls;

type
  TdwlOAuth2LoginForm = class(TForm)
    Panel1: TPanel;
    Browser: TWebBrowser;
    Panel2: TPanel;
    cxButton1: TButton;
    Panel3: TPanel;
    cxLabel1: TLabel;
    procedure BrowserBeforeNavigate2(ASender: TObject; const pDisp: IDispatch; const URL, Flags, TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool);
    procedure BrowserNavigateComplete2(ASender: TObject; const pDisp: IDispatch; const URL: OleVariant);
  strict private
    FCode: string;
    FRedirect_Uri: string;
    function CheckURL(const URL: string): boolean;
  public
    property Code: string read FCode write FCode;
    property Redirect_Uri: string read FRedirect_Uri write FRedirect_Uri;
  end;

implementation

uses
  IdURI, System.NetEncoding, System.SysUtils;

{$R *.dfm}

procedure TdwlOAuth2LoginForm.BrowserBeforeNavigate2(ASender: TObject; const pDisp: IDispatch; const URL, Flags, TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool);
begin
  if CheckURL(URL) then
    Cancel := true;
end;

procedure TdwlOAuth2LoginForm.BrowserNavigateComplete2(ASender: TObject;
  const pDisp: IDispatch; const URL: OleVariant);
begin
  CheckURL(URL);
end;

function TdwlOAuth2LoginForm.CheckURL(const URL: string): boolean;
begin
  Result := false;
  if URL.StartsWith(Redirect_Uri, true) then
  begin
    var Params := TStringList.Create;
    try
      Params.Delimiter := '&';
      Params.StrictDelimiter := true;
      var IUri := TIdURI.Create(URL);
      try
        Params.DelimitedText := IUri.Params;
      finally
        IUri.Free;
      end;
      Code := TNetEncoding.Url.Decode(Params.Values['code']);
    finally
      Params.Free;
    end;
    ModalResult := mrOk;
    Result := true;
  end;
end;

end.
