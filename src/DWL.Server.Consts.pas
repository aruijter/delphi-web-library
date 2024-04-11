unit DWL.Server.Consts;

interface

const
  Param_Section_Dwl_Db = 'section_dwl_db'; ParamDef_Section_Dwl_Db = 'dwl_db';

  Param_ACMEPort = 'ACMEport'; ParamDef_ACMEPort = 80;
  Param_ACMEDomain = 'ACMEDomain';
  Param_ACMECountry = 'ACMECountry';
  Param_ACMEState = 'ACMEState';
  Param_ACMECity = 'ACMECity';
  Param_ACME_Account_Key = 'acme_account_key';
  Param_Binding_IP = 'binding_ip';
  Param_Binding_Port = 'binding_port';
  Param_DLLBasePath = 'dllbasepath';
  Param_TestMode = 'testmode';
  Param_Hostnames = 'hostnames';
  Param_Issuer = 'issuer';
  Param_Endpoint= 'endpoint';
  Param_LogSecret = 'logsecret';
  Param_ServerBaseURL = 'serverbaseurl';

  EndpointURI_Log = '/log';
  EndpointURI_Mail = '/mail';
  EndpointURI_OAuth2 = '/oauth2';

  SpecialRequestParam_RemoteIP = 'remoteip';
  SpecialRequestParam_Context_Issuer = 'context_issuer';

  logdestinationServerConsole='serverconsole';

  serverservice_SendEMail = 1;
  serverservice_Log = 2;

implementation

end.
