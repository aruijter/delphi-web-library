DWLServer, an extensible versatile HTTP Server
==============================================
DWLServer is a Windows Service application with the option to serve HTTP requests by attaching DLL's to endpoints. 

Some characteristics:
* DWLServer functionality is arranged around a HTTP service that is always running
* DWLServer is a 64bit application with 64bit DLL's
* Full support for easy building a REST API is available
* Almost all configuration, logging, status, authentication data, etc  is stored in a MySQL database
* Bootstrapping is configured by minimal db connection info in an ini file or on the command line
* Authentication is done via an OpenID Connect provider, which of course also acts as OAuth2 server

## Deployment
For DWLServer to be able to function you need the following files to be placed in a directory of choice:
* DWLServer_sv.exe
* libeya32.dll (64bit version!)
* ssleay32.dll (64bit version!)
* libmysql64.dll
* libcrypto-3-x64.dll
* libssl-3-x64.dll

The needed DLL's can be found in the support directory of the DWL project on GitHub.

## Configuration
DWLServer configuration is fully based on parameters, handled as key/value pairs. 
It consists of a config store with pairs to hold the configuration parameters. During the addition of additional configuration pairs, existing keys will be overwritten with the new values.
* At first the command line parameters are processed and added to the store.
* Secondairy the ini file in the same directory with the same barename as the executable (in practice dwlserver_sv.ini) needs to have the section [dwl_db] All key-value pairs from this section are read into the config store.
* After the DB is available, the config pairs of the dwl_parameters table are added to the config store
* When a handler is loaded the params in the handler record are added to the config store and passed on to the DLL for internal configuration.

Requirement of this approach is that a minimal bootstrapping configuration for database access must be configured in the ini file (or if you really want from the commandline) (section can be configured, but is default *dwl_db*):
Identifier|Default|Description|
|-|-|-|
|host|127.0.0.1|The host name of the MySQL Server|
|port|3306|The port of the MySQL Server|
|username||The username used when connecting to the MySQL Server|
|password||The password used when connecting to the MySQL Server|
|db|dwl|The MySQL database where the information resides|

After running the application will check and if needed create the database and tables, so make sure the user has enough rights in this regard.

## MailQueue
The DWLServer main application also instantiates a Global MailQueue which will be used to deliver all email dropped via the logging handler or created by the log triggering sematics. The mailqueue needs to be configured to be able deliver mail.

The configuration parameter of the mailqueue is *mailqueue_domains* and contains the following JSON array

```json
[
  {
    "__comment__": "This is a regular SMTP profile", 
    "domain" : "emaildomain.example",
    "host" : "smtp.emaildomain.example",
    "port" : 465,
    "username" : "someone@emaildomain.example"
    "password" : "secret",
  },
  {
    "__comment__": "This is a Modern Auth SMTP profile", 
    "domain" : "emaildomain2.example",
    "host" : "smtp.emaildomain2.example",
    "port" : 587,
    "redirect_uri": "some://uri",
    "clientid": "<some-clientid>",
    "username" : "someone@emaildomain2.example",
    "refreshtoken" : "<some-token>",
  },
  {
    "__comment__": "...just another configured domain..."
  }
]
```

Please understand it does NOT act as a generic mail server. For the domain of every sender there needs to be a domain configuration to be able to send out the mail on behalf of that user.

## /log handler
The embedded handler on /log can be used to post logs to the generic logging system of the DWLServer. See the OpenAPI description on how to call the /log handler.

All logs with level lsNotice and higher will be saved in the dwl_log_messages table, all other debug messages are placed in the dwl_log_debug mysql table.

On the logs placed in the dwl_log-messages table triggers can be configured. These triggers will send out an email to a configured recipient as signalling of the fact that the log arrived. The triggering table structure is described later.

## ACME support
To be able to automatically fetch Server Certificates, ACME support is included. The table dwl_hostnames needs to contain the hostnames including Certificate properties that you want to use. The certificates will be requested on behalf of the identity described by the fields CountryCode, State and City. Request, renew, key generation etc is all taken care of within the HTTP Server. Currently only challenge method HTTP is supported.

If no ACME configuration is provided, the server won't function and loading of the handlers will be skipped. This is not a technical limitation but a consciously chosen approach. Security is important when it comes to DWLServer.

To be able to test without certificate, you can add the configurationpair *testmode=true*. While in testmode only requests from localhost will be handled.

## All configuration keys
The extensive list of applicable configuration keys:
|Key|Default|Description|
|-|-|-|
|section_dwl_db|dwl_db|The section from the ini file from which to read the configuration pairs (Practically placed in the commandline of the dwlserver_debug application)|
|host|127.0.0.1|The host name of the MySQL Server|
|port|3306|The port of the MySQL Server|
|username||The username used when connecting to the MySQL Server|
|password||The password used when connecting to the MySQL Server|
|db|dwl|The MySQL database where the information resides|
|mailqueue_domains|-|A JSON definition (described above that holds the maildelivery configuration|
|testmode|false|If no certificate is present and testmode is true, the server will handle unsecured requests only from localhost|
|binding_ip|*|The IP of the interface on which the HTTP Server will listen for incoming requests. If not configured The server will listen on all interfaces|
|binding_port|80/443|The portnumber on which the HTTP Server will listen for incoming request|
|dllbasepath|\<exeroot\>|The Base path where the DLLs to be loaded can be found (referred to by file://localhost)|
|acmeport|80|If needed in the case of port redirection, you can change the port on which the ACME client responds to HTTP challenge requests.|
|loglevel|4|The loglevel of the http requests; 3:failed, 4:warning, 6:all, 9:everything|
|logsecret||The secret that needs to be provided when calling the log handler (If left empty it will be auto generated in the first run)|
|issuer|https://\<hostname\>/oauth2|The global issuer used in JSON Web tokens (Refresh-, Access- and OpenID tokens). If not configured, a hostname specific default is used.|

## Configuration Table dwl_handlers
The table dwl_handlers has a record for each DLL that will be dynamically loaded. The fields must contain the following information:
|Fieldname|Description
|-|-|
|endpoint|The endpoint on which the handler will be registered. All calls that start with this url will be routed through this handler|
|handler_uri|The uri from which the DLL executable will be loaded. At this moment only uri's starting with file://localhost/ are supported. The uri that follows hereafter is the subpath to the DLL to load. The local path that is configured will point internally the actual DLL locations|
|params|A memofield with one key-value pair per line (separated by =) containing the configuration information for this handler. The configuration requirements of the handler are documented in the handler description. A special parameter evaluated on initialization is *enabled*. If set to false the handler will not be loaded. Another global parameter is *alloworigin* to define comma separated allowable origins (*=all)|

Please be aware that the often configured OAuth2 handler must be mounted at endpoint /oauth2. If you deviate from this, you also must configure a global issuer.

## Configuration Table dwl_hostnames
The table dwl_hostnames has a record for each hostname that the server will work with.  The fields of this record must contain the following information:
|Fieldname|Description
|-|-|
|hostname|The hostname the server will request a certificate for. Multiple hostnames (records) are possible because the server supports SNI|
|CountryCode|The countrycode that will be used when requesting the certificate.|
|State|The state that will be used when requesting the certificate.|
|City|The city that will be used when requesting the certificate.
|BindingIP|If NULL all bindings will be accessible throught this hostname, Otherwise access is limited to to specific given IP.|
|Cert|Leave empty when configuring. The system will put the Certificate in this field.|
|PrivateKey|Leave empty when configuring. The system will put the Private Key of the Certificate in this field.|

## Configuration Table dwl_urialiases
Uri aliasing is done as one of the first steps in dispatching a request. If the lead of a uri (without host information, but including leading slash) is one of the configured aliases the request will be internally handled using the rewritten uri.
The table dwl_urialiases  has a record for each uri redirection. The fields must contain the following information:
|Fieldname|Description
|-|-|
|alias|The lead of the uri that will be replaced|
|uri|The uri that will replace the alias within the full uri.|

## Configuration table dwl_log_triggers
There is also a configuration table dwl_log_triggers. In this table triggers can be defined which will push email messages to listeners using the trigger conditions.
|Fieldname|Default|Description
|-|-|-|
|Level_From|3 (lsNotice)|The minimum level of the messages to be pushed|
|Level_To|7 (lsFatal)|The maximum level of the messages to be pushed|
|Channel|\<all\>|The channel from which the messages are requested, this a RegEx expression, set to NULL (not empty string) to receive message from all channels|
|Topic|\<all\>|The topic from which the messages are requested, this a RegEx expression, set to NULL (not empty string) to receive messages from all topics|
|Parameters|-|Parameters that define the addressing of the push message. At this moment only E=Mail destinations can be configured.
|SuppressDuplicateSeconds|0|If non NULL it will suppress triggers for recurring identical messages for X seconds.
|SuppressEvaluateContent|false|If non NULL / nonzero the content will also be evaluated to conclude if it is a duplicate.

## Configuration table dwl_log_trigger_avoids
There is also a configuration table dwl_log_trigger_avoids. In this table messages can be defined for which triggering will be suppressed on first X occurences.
|Fieldname|Default|Description|
|-|-|-|
|Msg|-|The message for which triggering will be suppressed|
|MaxAvoidCount|255|The maximum amount of occurences of suppression. After MaxAvoidCount suppressions the message will execute the triggers|
|ClearSeconds|65536|The amount of seconds for which, after the last suppression, the avoidcount will be reset|

### dwl log trigger parameters
|Key|Description|
|-|-|
|email_to|The email address of the recipient|
|email_from|The email address of the sender|
|email_fromname|The name of the sender|
|email_subject|The subject topic, the macto's $(level) $(source) $(channel) $(topic) and $(msg) can be used in the subject and are hopefully self explanatory|