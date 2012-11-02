;Paval Shalamitski
; *** Inno Setup version 5.5.0+ Belarusian (classical orthography) messages ***
; Translated from English in 2012 by Paval 'Klyok' Shalamitski (i.kliok@gmail.com)
;
; To download user-contributed translations of this file, go to:
;   http://www.jrsoftware.org/files/istrans/
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).

[LangOptions]
; The following three entries are very important. Be sure to read and
; understand the '[LangOptions] section' topic in the help file.
LanguageName=<0411><0435><043B><0430><0440><0443><0441><043A><0430><044F>
LanguageID=$0423
LanguageCodePage=1251
; If the language you are translating to requires special font faces or
; sizes, uncomment any of the following entries and change them accordingly.
;DialogFontName=
;DialogFontSize=8
;WelcomeFontName=Verdana
;WelcomeFontSize=12
;TitleFontName=Arial
;TitleFontSize=29
;CopyrightFontName=Arial
;CopyrightFontSize=8

[Messages]

; *** Application titles
SetupAppTitle=���������� �������
SetupWindowTitle=���������� � %1
UninstallAppTitle=�����������
UninstallAppFullTitle=����������� %1

; *** Misc. common
InformationTitle=�������
ConfirmTitle=�����������
ErrorTitle=�������

; *** SetupLdr messages
SetupLdrStartupMessage=����븢������ %1. ֳ ����������?
LdrCannotCreateTemp=��������� �������� ������ ����. ������� ����븢���� �������
LdrCannotExecTemp=��������� ���������� ���� � ������� ����븴�. ������� ����븢���� �������

; *** Startup error messages
LastErrorMessage=%1.%n%n������� %2: %3
SetupFileMissing=� ����븴�, ������ ����븢������ �������, �� ������ ���� �%1�. �������� ���� ��� �������� ���� ������ �������.
SetupFileCorrupt=�����, �� ��� ����븢������ �������, �����������. �������� ���� ������ �������.
SetupFileCorruptOrWrongVer=�����, �� ��� ����븢������ �������, ����������� �� �������������� � ����� ������ ������� ������������. �������� ���� ��� �������� ���� ������ �������.
InvalidParameter=� ������� ����� ������� ����� ���������:%n%n%1
SetupAlreadyRunning=������� ������������ ���� ������.
WindowsVersionNotSupported=������� �� ���������� ����� ����� �Windows�.
WindowsServicePackRequired=������� �������� ��������� ������� �%1� ���� %2 �� ��������.
NotOnThisPlatform=������� �� ����������� �� %1.
OnlyOnThisPlatform=������� ����� ���������� �� %1.
OnlyOnTheseArchitectures=������� ����� ���������� ����� �� ���� �Windows� ��� ��������� ����������� ���������:%n%n%1
MissingWOW64APIs=����� �Windows�, ���� ������ � ���, �� �������� ����븢���� � 64-����� �������. ��� �������� ����, ��������� ��������� ������� (Service Pack) %1.
WinVersionTooLowError=������� �������� �%1� ���� %2 �� ��������.
WinVersionTooHighError=������� ������ ���������� �� �%1� ���� %2 �� ��������.
AdminPrivilegesRequired=��� ���������� �������, ����� �������� �� ������ (�����������).
PowerUserPrivilegesRequired=��� ���������� �������, ����� �������� �� ������ (�����������) ��� �� ����� ������ ������������� ����������곻.
SetupAppRunningError=������� ������������ ������, ��� ����� ������ �%1�.%n%n������� ��� ���������� ������ �%1� � ��������� ������, ��� ����������. ��� �������, ��������� �����������.
UninstallAppRunningError=������� ������������� ������, ��� ����� ������ �%1�.%n%n������� ��� ���������� ������ �%1� � ��������� ������, ��� ����������. ��� �������, ��������� �����������.

; *** Misc. errors
ErrorCreatingDir=������� ������������ �� ���� �������� ����븴 �%1�
ErrorTooManyFilesInDir=��������� �������� ���� � ����븴� �%1�: ����븴 �������� ������ �����

; *** Setup common messages
ExitSetupTitle=������� � ������������
ExitSetupMessage=���� �� ������� ����븢����. ��� ������� �����, ������� �� ����������.%n%n��� ������� ������������ ����� ����� ����� ���������� ����� �����.%n%nֳ ������� �������?
AboutSetupMenuItem=&��� ������� �������������
AboutSetupTitle=��� ������� ������������
AboutSetupMessage=�%1� ���� %2%n%3%n%n������ ������ �%1�%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< ��&���
ButtonNext=��&����� >
ButtonInstall=&����������
ButtonOK=�����
ButtonCancel=���������
ButtonYes=&���
ButtonYesToAll=����� �&��
ButtonNo=&��
ButtonNoToAll=����� �&�
ButtonFinish=&��������
ButtonBrowse=&ǳ�����
ButtonWizardBrowse=�&�������
ButtonNewFolder=&�������� ����븴

; *** "Select Language" dialog messages
SelectLanguageTitle=������ ���� ������������
SelectLanguageLabel=������� ����, ���� ����� ���������� ������� ������������:

; *** Common wizard text
ClickNext=��� ����������,���������� ��������. ��� �������,���������� �����������.
BeveledLabel=
BrowseDialogTitle=ǳ����� ����븴
BrowseDialogLabel=������� ����븴 � ����� ����, ����� ��������� ������.
NewFolderName=���� ����븴

; *** "Welcome" wizard page
WelcomeLabel1=³���� � ���������� ����븢���� �[name]�
WelcomeLabel2=��������� ������� �� �������� �[name/ver]�.%n%n����� ���, �� ����븢����, �� ��� ������� ��� ����� �������.

; *** "Password" wizard page
WizardPassword=������
PasswordLabel1=��� ���������� �������, ����� ������ ������.
PasswordLabel3=�������� ������ � ��������� ��������, ��� ����������. �������� ������ � ����� ������ (������� ���������� ����� � �������� �����).
PasswordEditLabel=&������:
IncorrectPassword=������ ����������� ������. ����������� ���� ���.

; *** "License Agreement" wizard page
WizardLicense=˳�������� �����������
LicenseLabel=����� ���, �� ����������, ���������� ��������� ������ �������.
LicenseLabel3=���������� ��������� �����������. ��� ���������� �������, ����� ��������� � ������ ������ �����������.
LicenseAccepted=� &������ �����������
LicenseNotAccepted=� &�� ������ �����������

; *** "Information" wizard pages
WizardInfoBefore=�������
InfoBeforeLabel=����� ���, �� ����������, ���������� ��������� ������ �������.
InfoBeforeClickLabel=��� ����������, ��������� ��������, ��� ���������� ����븢����.
WizardInfoAfter=�������
InfoAfterLabel=����� ���, �� ����������, ���������� ��������� ������ �������.
InfoAfterClickLabel=��� ����������, ��������� ��������, ��� ���������� ����븢����.

; *** "User Information" wizard page
WizardUserInfo=������� ��� ������������
UserInfoDesc=�������� ������� ��� ����.
UserInfoName=&��� ������������:
UserInfoOrg=&��������:
UserInfoSerial=&������� �����:
UserInfoNameRequired=����� ��������� ���.

; *** "Select Destination Location" wizard page
WizardSelectDir=������� ����� ������������
SelectDirDesc=���� ����� ���������� �[name]�?
SelectDirLabel3=�[name]� ���������� � �������� ����븴.
SelectDirBrowseLabel=��� ����������, ��������� ��������. ��� ����� ������ ���� ����븴, ��������� �ǳ������.
DiskSpaceMBLabel=���������� ������� [mb] �� �������� �� �����.
CannotInstallToNetworkDrive=��������� ����븢���� �� ������� ����.
CannotInstallToUNCPath=��������� ����븢���� � ����븴 �� ����� UNC.
InvalidPath=����� ��������� ���� ���� � ������ �����, ���������:%n%nC:\�������%n%n��� ���� UNC � ���� ��������:%n%n\\��������\������
InvalidDrive=������� ������� �� ����븴 UNC �� �������, ��� �� �� ���� �������. ������� �����.
DiskSpaceWarningTitle=������ ������� �������� �� �����.
DiskSpaceWarning=��� ����븢����, ����� ������� %1 �� ������� ��������, ��� ������� ������� ��� ����� %2 ��.%n%nֳ ���������� �� ����?
DirNameTooLong=������� ���� ����� ����븴� �� ����.
InvalidDirName=ճ���� ����� ����븴�.
BadDirName32=����� ����븴� �� ���� ��������� ��������� ����:%n%n%1
DirExistsTitle=����븴 �����
DirExists=����븴:%n%n%1%n%n��� �����. ֳ ���������� � ��� �� ����?
DirDoesntExistTitle=����븴 �� �����.
DirDoesntExist=����븴:%n%n%1%n%n�� �����. ֳ �������� ���?

; *** "Select Components" wizard page
WizardSelectComponents=������� �������
SelectComponentsDesc=��� ������� ����� ����������?
SelectComponentsLabel2=������� �������, ��� ������ ����������; ���������� �������, ��� �� ������ ����븢����. ��� �������� ������, ��������� ��������.
FullInstallation=���������� ������
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=���������� ����� ����������
CustomInstallation=������, ��� ����븢����
NoUninstallWarningTitle=������� �����
NoUninstallWarning=��������, ��� �� �������� ��� ��� ��������� ���������:%n%n%1%n%n����� ��� ���������� ����� �������, ��� ���������� �����������.%n%nֳ ���������� �� ����?
ComponentSize1=%1 ��
ComponentSize2=%1 ��
ComponentsDiskSpaceMBLabel=������� �������� ������� [mb] �� �������� �� �����.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=������ ���������� �������
SelectTasksDesc=��� ����� ��������� ������?
SelectTasksLabel2=������� ���������� �������, ��� ����� ��������, ��� ����� ����븢����� �[name]�, � ����� ��������� ��������.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=������� ����븴 �������� ����
SelectStartMenuFolderDesc=��� ����� ������� ������� �� �������?
SelectStartMenuFolderLabel3=������� �� �������� ��������� � ��������� ����븴� �������� ����.
SelectStartMenuFolderBrowseLabel=��� ����������, ��������� ��������. ��� ����� ������ ���� ����븴, ��������� �ǳ������.
MustEnterGroupName=����� ��������� ����� ����븴�.
GroupNameTooLong=������� ���� ����� ����븴� �� ����.
InvalidGroupName=ճ���� ����� ����븴�.
BadGroupName=����� ����븴� �� ���� ��������� ��������� ����:%n%n%1
NoProgramGroupCheck2=&�� �������� ����븴 � ������� ����

; *** "Ready to Install" wizard page
WizardReady=����� �������� ����븢����
ReadyLabel1=��� ����� �������� ����븢���� �[name]� �� ��������.
ReadyLabel2a=��� ����������,���������� ������������. ��� ������� ������������ �� ������� ����� ������, ��������� ������.
ReadyLabel2b=��� ���������� ����븢����, ��������� ������������.
ReadyMemoUserInfo=������� ��� ������������:
ReadyMemoDir=������ ������������:
ReadyMemoType=������ ����븢����:
ReadyMemoComponents=������� �������:
ReadyMemoGroup=����� �������� ����:
ReadyMemoTasks=���������� �������:

; *** "Preparing to Install" wizard page
WizardPreparing=��������� ����븢����
PreparingDesc=�[name]� ��������� ����������� �� ��������.
PreviousInstallNotCompleted=���� �� ������� ����븢���� �� �����븢���� ���������� �������. ��� �������� ����븢����, ����� ������ ���������� ��������.%n%n��� �������� �����������, ����� ������� ������� ������������ �[name]�.
CannotContinue=������ ���������� ����븢����. ��� �������, ��������� �����������.
ApplicationsFound=��������� ������� ����������� ������, ��� ������� ������������ �������� �������. ����� �������� ������� ������������ ������� ����� �������.
ApplicationsFound2=��������� ������� ����������� ������, ��� ������� ������������ �������� �������. ����� �������� ������� ������������ ������� ����� �������. ������� ������������ ��������� ����� ���������� ����� �������,���� �������� ����븢����.
CloseApplications=&������� �������
DontCloseApplications=&�� �������� �������

; *** "Installing" wizard page
WizardInstalling=����븢������
InstallingLabel=��� �����, ���������, ������ �[name]� ����븢������ �� ��������.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=��������� ����븢���� �[name]� ��������� ���������
FinishedLabelNoIcons=������� ����븢���� �[name]� �� ��������.
FinishedLabel=������� ����븢���� �[name]� �� ��������. ������� ����� ���������� ����������� ������� (���������).
ClickFinish=��� ������� � ������������, ��������� ����������.
FinishedRestartLabel=��� �������� ����븢���� �[name]�, ����� ������ ���������� ��������. ֳ ���������� ������ �����?
FinishedRestartMessage=��� �������� ����븢���� �[name]�, ����� ������ ���������� ��������.%n%nֳ ���������� ������ �����?
ShowReadmeCheck=���, � ���� ���������� ���� � ���������� �������
YesRadio=&���, ���������� ������
NoRadio=&��, � ������� ������ �������
; used for example as 'Run MyProg.exe'
RunEntryExec=���������� %1
; used for example as 'View Readme.txt'
RunEntryShellExec=����������� %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=��� ����븢����, ����������� �������� ����
SelectDiskLabel2=������� ����� �%1� � ��������� ������.%n%n��� ����� �� ����� ����� ������ �� � ��� ����븴�, �� ����� ����, �������� ��������� ���� ��� ��������� �ǳ������.
PathLabel=&����:
FileNotInDir2=�� ����� �%2� �� ������ ���� �%1�. ������� ��������� ���� �� ������� ���� ����븴.
SelectDirectoryLabel=�������� �������� ����.

; *** Installation phase messages
SetupAborted=�� ������� ����븢���� �������.%n%n������� ���������� �� ���������� ����븢���� �����.
EntryAbortRetryIgnore=��� ������������ ���� ���, �������� ������������. ��� �� ���� ����������, ��������� ��� �������� (�� ����� ��� �����). ��� ������ ����븢����, ��������� ��������.

; *** Installation status messages
StatusClosingApplications=���������� ��������
StatusCreateDirs=���������� ����븴��
StatusExtractFiles=�������� ������
StatusCreateIcons=���������� ������곅
StatusCreateIniEntries=���������� ����� � ������ INI�
StatusCreateRegistryEntries=���������� ����� � ��������
StatusRegisterFiles=������� ����� ��� ����ࢅ
StatusSavingUninstall=���������� ������� �����븢���� �������
StatusRunProgram=���������� ����븢�����
StatusRestartingApplications=������� ����������� �����
StatusRollback=������� ����� ���������

; *** Misc. errors
ErrorInternal2=��������� ����: %1
ErrorFunctionFailedNoCode=�� ������� ������ %1
ErrorFunctionFailed=�� ������� ������ %1; ��� %2
ErrorFunctionFailedWithMessage=�� ������� ������ %1; ��� %2.%n%3
ErrorExecutingProgram=������ �������� ����:%n%1

; *** Registry errors
ErrorRegOpenKey=�� ������� ������� ���� �������:%n%1\%2
ErrorRegCreateKey=�� ������� �������� ���� �������:%n%1\%2
ErrorRegWriteKey=�� ������� ������� ���� �������:%n%1\%2

; *** INI errors
ErrorIniEntry=�� ������� �������� ���� � ����� INI �%1�.

; *** File copying errors
FileAbortRetryIgnore=��� ������������ ���� ���, �������� ������������. ��� ����������� ����, ��������� ��� �������� (�� ����� ��� �����). ��� ������ ����븢����, ��������� ��������.
FileAbortRetryIgnore2=��� ������������ ���� ���, �������� ������������. ��� �� ���� ����������, ��������� ��� ��������. ��� ������ ����븢����, ��������� ��������.
SourceIsCorrupted=������� ���� � ����������
SourceDoesntExist=������� ���� �%1� �� �����
ExistingFileReadOnly=���� ����� ����� ����� ������.%n%n��� �������� �������� �������� � ���� � ������������ �����, ��������� ������������. ��� ����������� ����,���������� ��� ��������. ��� ������ ����븢���� �������, ��������� ��������.
ErrorReadingExistingDest=�� ������� ��������� ���� ����:
FileExists=���� ��� �����.%n%nֳ ������� �������� ���?
ExistingFileNewer=���� ���� � ������� �� ���, ��� �������� ����������. ����� �������� ���� ����.%n%nֳ �������� ���� ����?
ErrorChangingAttr=�� ������� ������� ������������� ������ �����:
ErrorCreatingTemp=�� ������� �������� ���� � ����������� ����븴�:
ErrorReadingSource=�� ������� ��������� ������� ����:
ErrorCopying=�� ������� ��������� ����:
ErrorReplacingExistingFile=�� ������� ������� ���� ����:
ErrorRestartReplace=�� ������� ������ ���������� �������� ����:
ErrorRenamingTemp=�� ������� ���� ����� ����� ����� � ����������� ����븴�:
ErrorRegisterServer=��������� ������ ���� ��� DLL �� OCX: %1
ErrorRegSvr32Failed=�RegSvr32� ������� ������ � ����� %1
ErrorRegisterTypeLib=��������� ������ ���� ��� ������� ���������� ���: %1

; *** Post-installation errors
ErrorOpeningReadme=�� ������� ������� ���� � �������.
ErrorRestartingComputer=��������� ������ ���������� ��������. ������ ���� ����������.

; *** Uninstaller messages
UninstallNotFound=���� �%1� �� �����. ��������� �����������.
UninstallOpenError=���� �%1� ������ �������. ��������� �����������
UninstallUnsupportedVer=������� ������������� �� ���� ��������� ���������� ����������� ������������� �%1�. ��������� �����������
UninstallUnknownEntry=� ����������� ������������� ���ﳢ�� �������� ���� (%1)
ConfirmUninstall=ֳ ��������� ��, ��� ����� ������ ������� ������� �%1� � ��� ���������?
UninstallOnlyOnWin64=����������� ����� ����� �� 64-����� ���� �Windows�.
OnlyAdminCanUninstall=����������� ����� ����� ����������� � ������ �������.
UninstallStatusLabel=���������, ������ �%1� ���������� � ���������.
UninstalledAll=�%1� ������� � ���������.
UninstalledMost=�%1� ����������.%n%n������� ����� �� �������. �� ����� ������� ����������.
UninstalledAndNeedsRestart=��� �������� �����븢���� �%1�, ����� ������ ���������� ��������.%n%nֳ ���������� ������ �����?
UninstallDataCorrupted=���� �%1� ��� �����������. ��������� �����������

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=ֳ ������� ��������� ����?
ConfirmDeleteSharedFile2=������� ��������, ��� �������� ��������� ���� ����� �� ������������ ������ �������. ֳ ������� ���� ����?%n%n��� ������ �������� ����������� ��� �������������, � ��� �������, ����� ������� �� ������ ��������� �������� �����. ��� �� �� ����� ������������, ������� ���. ��� ������� ���� ���� � �������, ����� �� �����.
SharedFileNameLabel=����� �����:
SharedFileLocationLabel=�����:
WizardUninstalling=���� �������������
StatusUninstalling=�����븢������ �%1��

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=����븢������ �%1�
ShutdownBlockReasonUninstallingApp=�����븢������ �%1�.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=�%1� ���� %2
AdditionalIcons=���������� �����:
CreateDesktopIcon=�������� ������ �� &��������
CreateQuickLaunchIcon=�������� ������ �� ����� &������� �������
ProgramOnTheWeb=�%1� � ������
UninstallProgram=����������� �%1�
LaunchProgram=���������� �%1�
AssocFileExtension=&�������� �%1� � ������ ���������� �%2�
AssocingFileExtension=�%1� ����������� � ������ ���������� �%2��
AutoStartProgramGroupDescription=���������:
AutoStartProgram=��������� �%1� ���������
AddonHostProgramNotFound=� ������� ����븴� �� ������ ���� �%1�.%n%nֳ ���������� �� ����?
