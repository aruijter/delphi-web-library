# Release Notes
All notable (including breaking) changes to this project will be documented in this file.

## Oct 13, 2023
A big improvement to DWL Server. Now reached the highest convenience possible for ACME challenges. ACME now uses the already open port 443!!
- Multiple bindings with their own IOHandler. Multiple ports can be opened, each with their own protocol (like HTTP and HTTPS)
- Added application layer protocol handling (RFC7301)
- Added TLS-ALPN-01 challnged type to ACME client. No longer need port 80 for ACME challenges (RFC8737). Yeah!

## Oct 3, 2023
- DwlServer: To improve on response times: Offloaded request logging from the core Request handling

## Sep 4, 2023
- Fix for thread event initialisation: this highly reduces thread load in certain circumstances and will fix a lot of 'unexpected' strange behaviour

## Aug 11, 2023
- Added socket timeout handling to DWL.Server.
- Improved DWLServer request timing when logging

## Jul 20, 2023
- DWL.Server DLL's: Configure is now a procedure, Error is not longer returned, this is because a dynamic string result in a DLL export is quite bad. Also made sure Exceptions within DLL are not transferred to the server. Errors must always be handled within DLL! This is a breaking change: BE AWARE TO CHANGE ALL CONFIGURE FUNCTIONS TO PROCEDURES IN THE HANDLER DPR'S. See f.e. HelloWorld.dpr for a preferred dpr layout.

## Jul 18, 2023
- DWL.HTTP.Client: Recoded to use WinHTTP instead of WinInet. WinInet is officially not supported serverside and incidentally reported problems, WinHTTP is the preferred API to use anyway

## Jul 6, 2023
- DWL.Server: improved error handling and request logging
- DWL.MySQL: improved connection handling

## Jun 27, 2023
- DWL.Params: GetAsNameValueText/WriteNameValueText now URL Encodes the values when (de)serializing. This is a breaking change, f.e. DWLServer and the DLLs must all be recompiled to adapt to the way the configuration is transferred using NameValueText.
- DWL.Server: added dwl_handler_parameters (and some improvements in handling of RequestParams)
- DWL.StrUtils: added some Char modification functions

## Jun 19, 2023
- OAuth2: Removed scope checking while granting

## May 25, 2023
- Corrected handling of (extreme) Unsigned values in MySQL
- Added DWL Server Log Avoids.

## May 23, 2023
- Changed MySQL tables to use Unsigned integers were applicable. dwl_log_requests.ProcessingTime is now max High(word). So for lengthy requests the table colum must be adapted to UNSIGNED to prevent an out of bounds.

## May 3, 2023
- Added the SuppressEvaluateContent column to table dwl_log_triggers. It is now able to use content in equality comparison. Migration: manually add new column to database table.

## April 20, 2023
- Restructured the Issuer used in JSON Web tokens. It's now based on hostname, unless a global issuer is provided
 
## March 29, 2023
- Renamed DisCoClient to DisCoAgent, client was a badly chosen name, it isn't a client at all, it's an agent to upload and view releases
- Repaired an updating error in the package list that appeared after uploading a release
  
## March 28, 2023
- DisCo: Added possibility to download support DLL's without authentication. This is needed because for authentication you simply need some dlls ;-)

## March 23, 2023
- Log request to a seperate table dwl_log_requests (and no longer via standard logging framework)
- Changed fieldtype of TimeStamp fields in logging tables. Although not breaking I advise to change the TimeStamp fieldtypes to TIMESTAMP to improve on handling of timezones.
- Another rename action to be RFC compliant: HTTP_COMMAND_* was renamed to HTTP_METHOD_*. Also some support functions/argument names are renamed to refer to Method instead of Command.
- Added Check if SNI matches Host from Header

## April 11, 2023
- Removed root certificate functionality, it was obsolete. Root certificates are not part of SSL cert chain.

## March 21, 2023
-  Added commandline functionality to DisCoClient
-  Improved DisCoClient interface. f.e. versions visible in list
-  Added csv export possiblity for packages (DisCoClient)

## March 20, 2023
- DWL Server trigger improvement. Channel and Topic filters are now regular expressions. In practice It should be backward compatible with plain string filter we used before. One warning: an empty string is an invalid RegEx, so make sure your MySQL filter field contains a valid RegEx expression or NULL, which means not filtering.
  
## March 9, 2023
- Added HTTP field support to DWLServer. Small breaking change is that the consts HTTP_HEADER_* are renamed to HTTP_FIELD_* to be RFC compliant
- ACME implementation now also uses DWL HTTP server for callback implementation and no longer depends on Indy.

## March 7, 2023
- Renamed namespace DWL.HTTP.Server.* to DWL.Server.*. as DWLServer is becoming a big thing in itself. Sorry this is a breaking change, good luck with changing your uses, but in this stage I thought that sanilty is more important than this little inconvenience.
- Rewrite of the core implementation of DWLServer. it is now based on our own HTTP server implementation and no longer depends on Indy. A big advantage of this approach is f.e. that the DWLServer now supports SNI and multiple certificates

## March 1, 2023
- Added DisCo client. A client of the DisCo framework with the ability to view, updload and download releases.

## February 26, 2023
- Added SSL IOHandling to the generic TCP implementation.
  
## February 25, 2023
- A generic TCP implementation, including a generic TCP server based on IO Completion ports.

## February 9, 2023
- Some fixes in the behavious of OAuth2.

## February 6, 2023
- Added OpenID ID_token and userinfo retrieval.
- Monitor the MailQueue and restart if it is stalled (indy sometimes freezes when communiation with a mailserver).

## February 1, 2023
- Added parameter testmode, this allows the server to be started in non-secure mode on listening for local connections.

## January 24, 2023
- Added the possiblity to suppress logs. This is a breaking change for current installations. Table structure changing is currently not supported, so the SuppressDuplicateSeconds field needs to be manually added to the dwl_log_triggers table.

## January 23, 2023
- Optimzations to mailing and triggering.
- Prevent logs to be created recursivly when communcation with non compliant mailservers.
- Trigger reloading (every minute), removing the need of a server restart when triggers are changed.


## December 29, 2022
- Log triggering became mature. Filtering options like Level From and Level to were added.
- The DisCo (Distribution and Configuration) framewwork was added to be able to auto update applications and packages and feed configuration while bootrapping applications via the so called phonehome functionality.

## December 8, 2022
- The library is deemed to be quite stable and is used for production purposes in multiple applications.

## September 18, 2022
- The initial version of this library.