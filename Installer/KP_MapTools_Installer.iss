#define MyAppName "Knights Province Map Tools"
#define MyAppVersion "1.01"
#define MyAppPublisher "Thibmo"
#define MyAppURL "http://www.kp-wiki.org/"
#define MyAppExeName "MapExporter.exe"

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{7BDBEE9D-69CE-4C39-A793-FEB27F287A62}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={pf}\Knights Province
DefaultGroupName={#MyAppName}
AllowNoIcons=yes
OutputDir=C:\Users\Thimo\Desktop\KMR en KP\Delphi\Installer
OutputBaseFilename=KP_MapTools_Install
SetupIconFile=C:\Users\Thimo\Desktop\KMR en KP\Delphi\MapInstaller\MapInstaller_Icon.ico
Compression=lzma
SolidCompression=yes
MinVersion=0,5.01sp3
UninstallDisplayIcon={app}\MapExporter.exe
UninstallDisplayName=Knights Province MapTools

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
Source: "..\MapExporter.exe"; DestDir: "{app}"; Flags: ignoreversion unsetntfscompression
Source: "..\MapInstaller.exe"; DestDir: "{app}"; Flags: ignoreversion unsetntfscompression; Attribs: hidden
Source: "..\README.txt"; DestDir: "{app}"; Flags: ignoreversion isreadme
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\Map exporter"; Filename: "{app}\MapExporter.exe"; WorkingDir: "{app}"; IconFilename: "{app}\MapExporter.exe"; IconIndex: 0
Name: "{group}\Documentation\{cm:ProgramOnTheWeb,{#MyAppName}}"; Filename: "{#MyAppURL}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{group}\Documentation\README"; Filename: "{app}\README.txt"

[Registry]
Root: HKCR; SubKey: ".kpmap"; ValueType: string; ValueData: "MapInstaller.kpmap"; Flags: uninsdeletekey
Root: HKCR; SubKey: "MapInstaller.kpmap"; ValueType: string; ValueData: "Knights Province Map Installer File"; Flags: uninsdeletekey
Root: HKCR; SubKey: "MapInstaller.kpmap\Shell\Open\Command"; ValueType: string; ValueData: """{app}\MapInstaller.exe"" ""%1"""; Flags: uninsdeletekey
Root: HKCR; Subkey: "MapInstaller.kpmap\DefaultIcon"; ValueType: string; ValueData: "{app}\MapInstaller.exe,0"; Flags: uninsdeletevalue
