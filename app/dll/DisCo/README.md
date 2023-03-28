# DisCo: distribution and configuration framework for DWL

DisCo is a distribution and configuration framework for DWL. Releases and configuration reside in MySQL tables. Releases can be up- and downloaded throught this API. Configurations can be distributied via a so called phonehome functionality.

The core functionality of DisCo is to deliver releases to clients to be able to download needed packages or to update themselves with a new version. 

## Configuration
The handler relies on a MySQL database for configuration and storing relesaes, etc. If needed the database structure will be initialized by the handler.

### Params
The DLL exported function Configure needs to be fed with params. These params are a combination of the global DWL parameter definition, extended with parameters from the params field in the dwl_handlers table. This configuration sequence does not differ from the DWL configuration method for all DLL handlers.

The following configuration parameters needs to be provided:

|Key Name|Default|Description|
|-|-|-|
|host|\<value from dwl\>|The host name of the MySQL Server|
|port|<value from dwl\>|The port of the MySQL Server|
|username|<value from dwl\>|The username used when connecting to the MySQL Server|
|password|<value from dwl\>|The password used when connecting to the MySQL Server|
|db|<value from dwl\>|The MySQL database where the information resides|
|scope_admin|disco_admin|The scope that will give access to the admin API endpoints of this handler.|
|additional_parameters_sql|\<empty\>|An optional SQL that will be executed to get additional key/value pairs to be added when assembling the phonehome reply. The $(appname) and $(profile) in this SQL|
|supportpackages|7z64,libcrypto-3-x64|A comma separeted text of packagenames that are allowed to be downloaded without authentication| 
statement will be resolved and the expected result of this query has two fields. First the key and secondly the value. These key/values from the result will be added to the phonehome reply.

### AppPackages Table
The dwl_disco_apppackages database table may be filled with one or more pakcages that can be downloaded by clients. The package with the same name as the application is always inclusive, this package must not be configured. Only additional packages are registered in this table. The table serves as the 'filter' of the versioninfo that is provided to the client when calling phonehome.

|FieldName|Default|Description|
|-|-|-|
|id|-|An auto incremented unique id|
|appname|-|The name of the appliation that needs this package to be reported when phoning home|
|packagename|-|The name of the package that is included|

### ProfileParameters Table
The dwl_disco_profileparameters table holds additional configuration parameters that will be included when the phonehome request is assembled. Appname and profile can be NULL which means that the field is not a filtering value for selecting the parameter to send. If multiple records with NULL and specific filter values for the same parameter key are found, the most specific value will be used. This creates the possibility or override a value that has been defined generic with NULL before.

|FieldName|Default|Description|
|-|-|-|
|id|-|An auto incremented unique id|
|appname|-|The appname that will receive this key/value pair. NULL: all appnames
|profile|-|The profile that will receive this key/value pair. NULL: all profiles|
|key|-|An obligated name of the key of the paramter|
|value|-|An obligated value of the parameter|

## Endpoints
The DisCo handler implements the following endpoints:

### GET /phonehome 
This endpoint implements the most used call from this framework. Endpoint meant to be used by every client to get the actual configuration and release information. The result is a JSON Object object with the following structure:
```json
{
  "versions": {
    "<packagname1>": "<version1>",
    "<packagname2>": "<version2>",
    ....
  }, 
  "parameters" {
    "<key1>": "<value1>",
    "<key2>": "<value2>",
    ....

  }
}
```
### GET /release
This endpoint implements retrieval of release information. The request parameter `packagename` needs to be provided with the request. The returned release info will always be the latest. The result is a JSON object with the following structure:
```json
{
  "version": "<versioninfo>",
  "kind": "<releasekind>"
}
```
### GET /releases
This endpoint implements retrieval of the information of all release. Admin rights are needed for this call. The result is a JSON Array with objects having the following structure:
```json
[
    {
        "id": "<string>",
        "packagenaem": "<string>",
        "version": "<string>",
        "releasemoment": "<ISO8601 datetime string>",
        "kind": "<integer>",
        "fileextension": "<string>"
    },
    ......
]
```

### GET /download_package
This endpoint implements retrieval of binary of a release. The request parameter `packagename` needs to be provided with the request. Optionals is to filter by the request parameter `kind`.
The result is the requested file of the release as the content. Content_Disposition of header will be filled.

### POST /upload_package
This endpoint implements upload of a release. The header values `packagename`, `version`, `kind` and `fileextension` needs to be provided with the request. The post body is the binary of the release.

### POST /profileip
This endpoint implements the ability to provide a combination of ip address and and profile for automatic derivation of client profile by ipaddress.

