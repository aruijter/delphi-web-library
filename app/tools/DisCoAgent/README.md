# DisCoAgent
A simple Windows agent for consulting the packages and releases of the DisCo framework. In this initial version, due to the current actual usage, the authentication is handled by a json file and currently only username/password authentication is supported. In a later stage this agent will become fully compliant with OAuth2

## Configuration
The configuration needs to be provided by the json file named DisCoAgent.config that needs to be placed in the same directory as the executable.

The config file contains a json object with the following pairs defined:
|Key|value|
|-|-|
|disco_endpoint|The endpoint of the disco API handler|
|auth_endpoint|The endpoint used for user authentication and authorization|
|username|The username used while authenticating|
|password|The plain password used while authenticating|

### Example
An example of the contents of the config file:
```json
{
  "disco_endpoint": "https://api.dis.co/disco", 
  "auth_endpoint" : "https://auth.dis.co/auth",
  "username" : "john.doe",
  "password" : "youwouldliketoknowisntit"
}
```

## Functionality
When the application is started the actual situation of the DisCo database is loaded.

### Interface
The interface of the application is quite simple. On the left hand side you will find a list with al the currently active packages.

When a package is selected, the releases are shown right to the list of packages. In the right hand side of the application, the details of the current selected release are shown.

### Download
In the release details pane you will find a download button. This button will download the current release into a location you're able to select. This way you can download the release for review or modification purposes.

### Upload
In the release details pane you will find an upload button. This button can be used to send a release to the server. The selected release can be in one of the following formats:
|Filename|Type of release|
|-|-|
|[packagename].exe|A release of an executable. The version info will be loaded from the version resource embedded in the executable|
|[packagename].dll|A release of a Dynamic Loadable Library. The version info will be loaded from the version resource embedded in the library file|
|[packagename].7z|A release of package. A packages consist of a 7z file with a possible directory structure that can pe released to the client as a package. This package will be completely extracted on the client with preservation of the directory structure. The version info will be loaded from the textfile version.info placed in the base directory of the package. The version.info file is a text file with the version info as string.|

When selecting a release for sending to server the packagename is deducted from the bare filename. If the file is of one of the supported extensions, the versioninfo of the upload will be checked against the latest known version.

If all is in order the release will be send to the server and immediatly be available for downloading/updating by all clients.

If a problem is found this will be reported back to the user in the form of a visual message.

## Commandline execution
The agent can also be executed with one commandline paramter. This parameter needs to be the full path of the package to be released.

When using this commandline parameter the application will be navigated to the package and the latest release and the upload functionality will be executed (of course without showing a dialog for filename selection). If a problem occurs the application will stay open with the reported problem message in view. If everything went well the application will close silently. This way the functionality can be used unblocking from a scripting environment.

