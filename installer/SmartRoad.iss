#ifndef SmartRoadVersion
  #error SmartRoadVersion must be supplied by the build
#endif

[Setup]
AppId={{C88544EB-20B9-4CE3-B76E-5A368F107077}
AppName=Smart Road for AutoCAD 2016
AppVersion={#SmartRoadVersion}
AppPublisher=Smart Road
DefaultDirName={userappdata}\Autodesk\ApplicationPlugins\SmartRoad.bundle
DefaultGroupName=Smart Road
DisableDirPage=yes
DisableProgramGroupPage=yes
UsePreviousAppDir=no
PrivilegesRequired=lowest
CloseApplications=no
UninstallFilesDir={localappdata}\SmartRoad\Uninstall
OutputDir=..\dist
OutputBaseFilename=SmartRoad-AutoCAD-2016-v{#SmartRoadVersion}-Setup
Compression=lzma2
SolidCompression=yes
WizardStyle=modern

[Files]
Source: "..\SmartRoad.bundle\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs

[Code]
function PrepareToInstall(var NeedsRestart: Boolean): String;
var
  Locator, Service, Processes: Variant;
begin
  Result := '';
  try
    Locator := CreateOleObject('WbemScripting.SWbemLocator');
    Service := Locator.ConnectServer('.', 'root\CIMV2');
    Processes := Service.ExecQuery('SELECT Name FROM Win32_Process WHERE Name = ''acad.exe''');
    if Processes.Count > 0 then
    begin
      Result := 'Close all AutoCAD windows, then run Smart Road Setup again.';
      Exit;
    end;
  except
    Log('Could not check for running AutoCAD processes.');
  end;

  if not WizardSilent and FileExists(ExpandConstant('{app}\PackageContents.xml')) then
    if MsgBox('Replace the Smart Road bundle already installed for this Windows user?', mbConfirmation, MB_YESNO) <> IDYES then
      Result := 'The existing Smart Road bundle was kept.';
end;
