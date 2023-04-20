# an OAuth2 / OpenID Connect Handler for DWL

OAuth2, the authorization and authentication handler for DWL implements the OAuth 2.0 industry-standard protocol for authorization as defined in [RFC 6789](https://datatracker.ietf.org/doc/html/rfc6749) and on top of that the [OpenID Connect Core 1.0](https://openid.net/specs/openid-connect-core-1_0.html) functionality for authentication.

This handler acts as the authorization server as defined in the roles of the OpenAuth RFC.
The idea is that any other handler within Argus can act as the resource server that serves out content on behalf of the resource owner.
A client can access the protected resource in the handler by first obtaining authorization from this authorization handler.

The final 'deliverable' of this handler is an issued access token to the client. The client can use this access token to access resources (in other handlers) on behalf of the resource owner.

## Protocol Flow
The OAuth2 protocol flow as defined is as follows:
```
+--------+                               +---------------+
|        |--(A)- Authorization Request ->|   Resource    |
|        |                               |     Owner     |
|        |<-(B)-- Authorization Grant ---|               |
|        |                               +---------------+
|        |
|        |                               +---------------+
|        |--(C)-- Authorization Grant -->| Authorization |
| Client |                               |     Server    |
|        |<-(D)----- Access Token -------|               |
|        |                               +---------------+
|        |
|        |                               +---------------+
|        |--(E)----- Access Token ------>|    Resource   |
|        |                               |     Server    |
|        |<-(F)--- Protected Resource ---|               |
+--------+                               +---------------+
```
The steps A,B,C and D are implemented in the handler, the steps E and F, the check of the access token is done in the resource providing handlers.

## OpenID Connect Provider Relaying
The first step in authorization is the authentication part. This handler has the possibility to let the identification part be done by any external OpenID Connect Identification Provider, for example Google Identity.
If external OpenID Connect Providers are configured, the authentication form not only asks for local credentials, but also shows the possiblity to use the external Identity provider. By using a additional redirect chain, the Identity confirmation is done by the external OpenID Connect provider.

## Configuration
The handler relies on a MySQL database for configuration and storing user identifities, access rights, etc. If needed the database structure will be initialized or upgraded by the handler.

### Params
The DLL exported function Configure needs to be fed with params. These params are a combination of the global DWL parameter definition, extended with parameters from the params field in the dwl_handlers table. This configuration sequence does not differ from the DWL configuration method for all DLL handlers.

The following configuration parameters need to be provided:

|Key Name|Default|Description|
|-|-|-|
|host|\<value from dwl\>|The host name of the MySQL Server|
|port|<value from dwl\>|The port of the MySQL Server|
|username|<value from dwl\>|The username used when connecting to the MySQL Server|
|password|<value from dwl\>|The password used when connecting to the MySQL Server|
|db|<value from adwl\>|The MySQL database where the information resides|
|usertable|dwl_oauth2_users|The tablename of the usertable to be used|
|field_md5|-|It configured, the handler will perform migration from a legacy md5 hash stored in this field. On login, when the user password is available, the salt and pwd will be updated in the database|
|migration_issuer||When retrieving tokens, the issuer in the JSON Web token is checked against the expected issuer. In case of migration an additional allowed issuer can be configured in this key.|

### Providers Table
The dwl_oauth2_providers database table may be filled with one or more external OpenID Connect Providers. These provider definitions will be used to offer the user the possibility to use and external Identity Provider. The records must be populated with the following information
|FieldName|Default|Description|
|-|-|-|
|id|-|An auto incremented unique id|
|techname|-|The internal identification used to refer to this provider|
|displayname|-|The external description used to describe this provider on the authentication page|
|imgdata|-|An embedded image definition to be put near the displayname on the authentication page. For example `data:image/gif;base64,R0lG....`|
|uri|-|The URI of the issuer. This URI is used to pad with  the /.well-known/openid-configuration to get the URI's of the needed endpoints
|client_id|-|the client_id to be used with this provider|
|client_secret|-|The optional client_secret to be used. A PKCE is always used when communicating with the provider. When configured a client_secret will be added where applicable|

### Clients Table
The dwl_oauth2_clients database table must be filled with one or more clients. A client_id is a requirement when communicating with this handler, the configured properties of the client are:

|FieldName|Default|Description|
|-|-|-|
|id|-|An auto incremented unique id|
|client_id|-|A character sequence uniquely defining this client|
|client_secret|-|The optional client_secret of this client. Used in code flow when no PKCE is applied|
|redirect_uri|-|An obligated redirect uri for check and redirect purposes|

## Endpoints
To implement the authorization and identity functionality following the OAuth 2.0 and OpenID Connect standards, the following endpoints are implemented:

### /.well-known/openid-configuration 
This endpoint implements the required OpenID Provider Configuration Response as described in [OpenID Connect Discovery 1.0](https://openid.net/specs/openid-connect-discovery-1_0.html).

### /authorize
This endpoint is the starting point of the OAuth 2.0 authorization flow. This endpoint serves the A and B steps of the OAuth authorization flow. At this moment only the code flow is implemented. PKCE (Proof key for Code Exchange) is supported and can be used in addition or as a replacement of the client_secret.  

As defined by the OpenID standard, an `openid` scopt must always be requested.This endpoint fully implements the other aspects of the OAuth2 standard including state and nonce handling.

When calling this handler a webpage is shown where the user can make itself known, either by providing local credentials or via a redirection flow of an external OpenID Connect provider. When successfully authenticated a grant is returned which can be exchanged for a refresh_token and access_token by calling `./token`

For further implementation details see the OAuth2 specification.

### /token
This endpoint implements the token retrieval after a successfull grant is obtained. Currently only grant_type=authorization_code is supported. 
A new access_token and refresh_token are generated and returned to the requester.

For further implementation details see the OAuth2 specification.

### /certs

### /oidc_return
This endpoint is an internal endpoint, not to be used externally. It's where the external OIDC Providers redirected to. After external authentication. The authorization based on the external identity is handled here.