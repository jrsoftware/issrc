; *** Inno Setup version 6.4.0+ Afrikaans messages ***
;
; Created by: Leon Odendaal
; E-mail:     leonrsa@gmail.com
;
; To download user-contributed translations of this file, go to:
;   http://www.jrsoftware.org/files/istrans/
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).

[LangOptions]
LanguageName=Afrikaans
LanguageID=$0436
LanguageCodePage=1252
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
SetupAppTitle=Installasie
SetupWindowTitle=Installasie - %1
UninstallAppTitle=Verwyder
UninstallAppFullTitle=Verwyder %1

; *** Misc. common
InformationTitle=Inligting
ConfirmTitle=Bevestig
ErrorTitle=Fout

; *** SetupLdr messages
SetupLdrStartupMessage=Hierdie program sal %1 installeer. Wil u voortgaan?
LdrCannotCreateTemp=Onmoontlik om 'n tydelike leer te skep. Installasie gestaak.
LdrCannotExecTemp=Onmoontlik om 'n uitvoerbare leer in die tydelike vouer te skep. Installasie gestaak.
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nFout %2: %3
SetupFileMissing=Die leer %1 word vermis in die installasiegids. Korrigeer die fout of verkry 'n nuwe weergawe van die program.
SetupFileCorrupt=Die installasie leers is korrup. Verkry 'n nuwe weergawe van die program.
SetupFileCorruptOrWrongVer=Die installasieleers is korrup, of onversoenbaar met hierdie weergawe van Installeerder. Korrigeer die problem of verkry 'n nuwe weergawe van die program.
InvalidParameter='n Ongeldige parameter is deurgegee op die opdraglyn:%n%n%1
SetupAlreadyRunning=Installasie reeds aktief.
WindowsVersionNotSupported=Hierdie program ondersteun nie die Windows-weergawe op u rekenaar nie.
WindowsServicePackRequired=Hierdie program benodig %1 Service Pack %2 of nuwer.
NotOnThisPlatform=Hierdie program sal nie uitvoer op %1 nie.
OnlyOnThisPlatform=Hierdie program moet uitgevoer word op %1.
OnlyOnTheseArchitectures=Hierdie program kan net installeer word op weergawes van Windows ontwerp vir die volgende verwerkerargitekture:%n%n%1
WinVersionTooLowError=Hierdie program vereis %1 weergawe %2 of nuwer.
WinVersionTooHighError=Hierdie program kan nie installeer word op %1 weergawe %2 of nuwer nie.
AdminPrivilegesRequired=U moet ingeteken wees as 'n administrateur om hierdie program te installeer.
PowerUserPrivilegesRequired=U moet aangeteken wees as 'n administrateur of as 'n lid van die Power Users groep om hierdie program te installeer.
SetupAppRunningError=Die installeerder het bespeur dat %1 op die oomblik loop.%n%nMaak asb. nou alle kopiee daarvan toe, en kliek dan Aanvaar om voort te gaan, of Kanselleer om die installasie te verlaat.
UninstallAppRunningError=Verwyder het bespeur dat %1 op die oomblik oop is.%n%nMaak asb. alle kopiee daarvan toe, en kliek dan op Aanvaar om voort te gaan, of Kanselleer om die verwyderaar te verlaat.

; *** Startup questions
PrivilegesRequiredOverrideTitle=Kies Installeerder Modus
PrivilegesRequiredOverrideInstruction=Kies Installeerder Modus
PrivilegesRequiredOverrideText1=%1 kan vir alle gebruikers installeer word (benodig administratiewe voorregte), of slegs vir jou.
PrivilegesRequiredOverrideText2=%1 kan slegs vir jou installeer word, of vir alle gebruikers (benodig administratiewe voorregte).
PrivilegesRequiredOverrideAllUsers=Installeer vir &alle gebruikers
PrivilegesRequiredOverrideAllUsersRecommended=Installeer vir &alle gebruikers (aanbeveel)
PrivilegesRequiredOverrideCurrentUser=Installeer slegs vir &my
PrivilegesRequiredOverrideCurrentUserRecommended=Installeer slegs vir &my (aanbeveel)

; *** Misc. errors
ErrorCreatingDir=Die installeerder kon nie die gids %1 skep nie
ErrorTooManyFilesInDir=Onmoontlik om 'n leer in die gids "%1" te skep omdat dit te veel leers bevat

; *** Setup common messages
ExitSetupTitle=Verlaat Installeerder
ExitSetupMessage=Installasie is nog nie voltooi nie. Indien u dit nou verlaat, sal die program nie geinstalleerd wees nie.%n%nU kan die Installeerder later weer uitvoer om die installasie te voltooi.%n%nVerlaat die Installeerder?
AboutSetupMenuItem=&Meer oor die Installeerder...
AboutSetupTitle=Meer oor die Installeerder
AboutSetupMessage=%1 weergawe %2%n%3%n%n%1 tuisblad: %n%4
AboutSetupNote=
TranslatorNote=Vertaling deur Leon Odendaal

; *** Buttons
ButtonBack=< &Terug
ButtonNext=&Volgende >
ButtonInstall=&Installeer
ButtonOK=Aanvaar
ButtonCancel=Kanselleer
ButtonYes=&Ja
ButtonYesToAll=Ja vir &Almal
ButtonNo=&Nee
ButtonNoToAll=N&ee vir Almal
ButtonFinish=&Voltooi
ButtonBrowse=&Rondblaai...
ButtonWizardBrowse=R&ondblaai...
ButtonNewFolder=&Skep Nuwe Vouer

; *** "Select Language" dialog messages
SelectLanguageTitle=Kies Installeerdertaal
SelectLanguageLabel=Kies die taal om te gebruik gedurende die installasie:

; *** Common wizard text
ClickNext=Kliek Volgende om voort te gaan, of Kanselleer om die installeerder te verlaat.
BeveledLabel=
BrowseDialogTitle=Blaai rond vir vouer
BrowseDialogLabel=Kies 'n vouer in die lys hieronder en kliek Aanvaar.
NewFolderName=Nuwe Vouer

; *** "Welcome" wizard page
WelcomeLabel1=Welkom by die Installasie-Assistent vir [name]
WelcomeLabel2=Hierdie program sal [name/ver] installeer op u rekenaar.%n%nDit word aanbeveel dat u alle ander programme toemaak voor dat u voortgaan.

; *** "Password" wizard page
WizardPassword=Wagwoord
PasswordLabel1=Hierdie installasie word deur 'n wagwoord beskerm.
PasswordLabel3=Verskaf asb die wagwoord, en kliek Volgende om voor te gaan. Wagwoorde is kassensitief.
PasswordEditLabel=&Wagwoord:
IncorrectPassword=Die wagwoord wat u ingesleutel het, is nie korrek nie. Probeer weer.

; *** "License Agreement" wizard page
WizardLicense=Lisensie-ooreenkoms
LicenseLabel=Lees asb die volgende belangrike inligting voordat u voortgaan.
LicenseLabel3=Lees asb die volgende lisensieooreenkoms. U moet die terme van hierdie ooreenkoms aanvaar voordat u voortgaan met die installasie.
LicenseAccepted=Ek &aanvaar die ooreenkoms.
LicenseNotAccepted=Ek aan&vaar nie die ooreenkoms nie.

; *** "Information" wizard pages
WizardInfoBefore=Inligting
InfoBeforeLabel=Lees asb die volgende belangrike inligting voordat u voortgaan.
InfoBeforeClickLabel=Wanneer u gereed is om voort te gaan met die Installasie, kliek Volgende.
WizardInfoAfter=Inligting
InfoAfterLabel=Lees asb die volgende belangrike inligting voordat u voortgaan.
InfoAfterClickLabel=Wanneer u gereed is om voort te gaan met die Installasie, kliek Volgende.

; *** "User Information" wizard page
WizardUserInfo=Gebruikerinligting
UserInfoDesc=Sleutel asb u inligting in.
UserInfoName=&Gebruikernaam:
UserInfoOrg=&Organisasie:
UserInfoSerial=&Registrasienommer:
UserInfoNameRequired=U moet 'n naam insleutel.

; *** "Select Destination Location" wizard page
WizardSelectDir=Kies bestemming
SelectDirDesc=Waar moet [name] installeer word?
SelectDirLabel3=Die installeerder sal [name] installeer in die volgende vouer.
SelectDirBrowseLabel=Om voort te gaan, kliek Volgende. Indien u 'n ander vouer wil kies, kliek Rondblaai.
DiskSpaceGBLabel=Ten minste [gb] GG oop hardeskyfspasie word benodig.
DiskSpaceMBLabel=Ten minste [mb] MG oop hardeskyfspasie word benodig.
CannotInstallToNetworkDrive=Installeerder kan nie op 'n netwerk-skyf installeer word nie.
CannotInstallToUNCPath=Installeerder kan nie na 'n UNC-roete installeer nie.
InvalidPath=U moet 'n volledige roete insleutel met 'n aandrywerletter; bv.:%n%nC:\APP%n%nof 'n UNC-pad in die vorm:%n%n\\server\share
InvalidDrive=Die aandrywer of UNC-netwerkgids wat u gekies het, bestaan nie of is nie toeganklik nie. Kies asb 'n ander een.
DiskSpaceWarningTitle=Onvoldoende skyfspasie
DiskSpaceWarning=Die installasie vereis ten minste %1 KG oop spasie, maar die gekose skyf het slegs %2 KG spasie beskikbaar.%n%nWil u voortgaan ten spyte daarvan?
DirNameTooLong=Die vouernaam of roete is te lank.
InvalidDirName=Die vouernaam is ongeldig.
BadDirName32=Vouername mag nie een van die volgende karakters bevat nie:%n%n%1
DirExistsTitle=Vouer bestaan
DirExists=Die vouer:%n%n%1%n%nbestaan alreeds. Wil u ten spyte daarvan steeds daarheen installeer?
DirDoesntExistTitle=Vouer bestaan nie
DirDoesntExist=Die vouer:%n%n%1%n%n bestaan nie. Wil u die vouer skep?

; *** "Select Components" wizard page
WizardSelectComponents=Kies komponente
SelectComponentsDesc=Watter komponente moet installeer word?
SelectComponentsLabel2=Kies die komponente wat u wil installeer; deselekteer die komponente wat u nie wil installeer nie. Kliek Volgende wanneer u gereed is om voort te gaan.
FullInstallation=Volledige installasie
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Kompakte installasie
CustomInstallation=Pasgemaakte installasie
NoUninstallWarningTitle=Komponente Bestaan
NoUninstallWarning=Die installeerder het bespeur dat die volgende komponente reeds op u rekenaar installeer is:%n%n%1%n%nDeur die komponente te deselekteer sal hulle nie verwyder nie.%n%nWil u ten spyte daarvan voortgaan?
ComponentSize1=%1 KG
ComponentSize2=%1 MG
ComponentsDiskSpaceGBLabel=Huidige keuse vereis ten minste [gb] GG skyfspasie.
ComponentsDiskSpaceMBLabel=Huidige keuse vereis ten minste [mb] MG skyfspasie.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Kies bykomende take
SelectTasksDesc=Watter bykomende take moet uitgevoer word?
SelectTasksLabel2=Kies die bykomende take wat u wil verwag die Installeerder moet uitvoer tydens die installasie van [name], en kliek dan Volgende.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Kies Begin-kieslysvouer
SelectStartMenuFolderDesc=Waar moet die Installeerder die program se kortpaaie plaas?
SelectStartMenuFolderLabel3=Die installeerder sal die program se kortpaaie in die volgende Begin-kieslysvouer plaas.
SelectStartMenuFolderBrowseLabel=Om voort te gaan, kliek Volgende. Indien u 'n ander vouer wil kies, kliek Rondblaai.
MustEnterGroupName=U moet 'n vouernaam insleutel.
GroupNameTooLong=Die vouernaam of roete is te lank.
InvalidGroupName=Die vouernaam is ongeldig.
BadGroupName=Die vouernaam mag nie enige van die volgende karakters bevat nie:%n%n%1
NoProgramGroupCheck2=&Moenie 'n Begin-kieslysvouer skep nie

; *** "Ready to Install" wizard page
WizardReady=Gereed om te Installeer
ReadyLabel1=Die installeerder is nou gereed om [name] te installeer op u rekenaar.
ReadyLabel2a=Kliek Installeer om voort te gaan met die installasie, of kliek Terug indien u enige keuses wil hersien of verander.
ReadyLabel2b=Kliek Installeer om voort te gaan met die installasie.
ReadyMemoUserInfo=Gebruikerinligting:
ReadyMemoDir=Bestemmingligging:
ReadyMemoType=Installasietipe:
ReadyMemoComponents=Geselekteerde komponente:
ReadyMemoGroup=Begin-kieslysvouer:
ReadyMemoTasks=Bykomende take:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel=Bykomende leers word afgelaai files...
ButtonStopDownload=&Staak aflaai
StopDownload=Is jy seker jy will die aflaai staak?
ErrorDownloadAborted=Aflaai laat vaar
ErrorDownloadFailed=Aflaai fout: %1 %2
ErrorDownloadSizeFailed=Groote van aflaai fout: %1 %2
ErrorFileHash1=Leersom fout: %1
ErrorFileHash2=Ongeldige leersom: %1 verwag, %2 gevind
ErrorProgress=Ongeldige vordering: %1 of %2
ErrorFileSize=Ongeldige leer grootte: %1 verwag, %2 gevind

; *** TExtractionWizardPage wizard page and Extract7ZipArchive
ExtractionLabel=Onttrek bykomende leers...
ButtonStopExtraction=&Staak onttrekking
StopExtraction=Is jy seker jy wil die onttrekking staak?
ErrorExtractionAborted=Ontrekking laat vaar
ErrorExtractionFailed=Onttrekking fout: %1


; *** "Preparing to Install" wizard page
WizardPreparing=Berei voor om te Installeer
PreparingDesc=Die installeerder is besig om voor te berei om [name] op u rekenaar te installeer.
PreviousInstallNotCompleted=Die installasie/verwydering van 'n vorige program is nie voltooi nie. U moet u rekenaar herbegin om daardie installasie te voltooi.%n%nNadat u die rekenaar herbegin het, kan u die installeerder weer uitvoer om die installasie van [name] te voltooi.
CannotContinue=Die installeerder kan nie voortgaan nie. Kliek asb. Kanselleer om dit te verlaat.
ApplicationsFound=Die volgende programme gebruik tans leers wat deur die installeerder opgedateer moet word. Dit word aanbeveel dat u die installeerder toelaat om die programme outomaties toe te maak.
ApplicationsFound2=Die volgende programme gebruik tans leers wat deur die installeerder opgedateer moet word. Dit word aanbeveel dat u die installeerder toelaat om hierdie programme outomaties toe te maak. Na afloop van die installasie, sal die installeerder probeer om die programme te herbegin.
CloseApplications=&Maak die programme outomaties toe
DontCloseApplications=M&oet nie die programme toemaak nie
ErrorCloseApplications=Die Installeerder kon nie al die programme outomaties sluit nie. Dit word aanbeveel dat u al die programme toemaak wat leers bevat wat opdateer moet word voor u aangaan.
PrepareToInstallNeedsRestart=Die Installeerder moet jou rekenaar herbegin. Begin die Installeerder daarna weer om die installasie  van [name] te voltooi .%n%nWil jy aangaan?

; *** "Installing" wizard page
WizardInstalling=Besig om te Installeer
InstallingLabel=Wag asb. terwyl [name] op u rekenaar installeer word.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Finalisering van die Installasie-Assistent van [name]
FinishedLabelNoIcons=Die installasie van [name] is voltooi.
FinishedLabel=Die installasie van [name] is voltooi. Die program kan uitgevoer word deur die geinstalleerde ikone te gebruik.
ClickFinish=Kliek Voltooi om die installeerder te verlaat.
FinishedRestartLabel=Om die installasie van [name] te voltooi, moet u rekenaar herbegin word. Wil u die rekenaar nou herbegin?
FinishedRestartMessage=Om die [name] installasie te voltooi, moet u rekenaar herbegin word.%n%nWil u die rekenaar nou herbegin?
ShowReadmeCheck=Ja, ek wil die README-leer sien
YesRadio=&Ja, herbegin die rekenaar nou
NoRadio=&Nee, ek sal die rekenaar later herbegin
; used for example as 'Run MyProg.exe'
RunEntryExec=Voer %1 uit
; used for example as 'View Readme.txt'
RunEntryShellExec=Bekyk %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Installeerder benodig volgende CD/DVD
SelectDiskLabel2=Plaas asb. skyf %1 in die aandrywer en kliek Aanvaar.%n%nIndien die leers in 'n ander vouer gevind kan word as die een hieronder, sleutel die korrekte roete in of kliek Rondblaai.
PathLabel=&Roete:
FileNotInDir2=Die leer "%1" kan nie gevind word in "%2" nie. Plaas asb. die korrekte skyf in die aandrywer of kies 'n ander vouer.
SelectDirectoryLabel=Spesifiseer asb. die ligging van die volgende skyf.

; *** Installation phase messages
SetupAborted=Die installasie is nie voltooi nie.%n%nKorrigeer asb. die probleem en voer die installeerder weer uit.
AbortRetryIgnoreSelectAction=Kies aksie
AbortRetryIgnoreRetry=&Probeer weer
AbortRetryIgnoreIgnore=&Ignoreer die fout en gaan aan
AbortRetryIgnoreCancel=Kanseleer die installasie

; *** Installation status messages
StatusClosingApplications=Maak programme toe...
StatusCreateDirs=Skep vouers...
StatusExtractFiles=Pak leers uit...
StatusCreateIcons=Skep kortpaaie...
StatusCreateIniEntries=Skep INI-inskrywings...
StatusCreateRegistryEntries=Skep van registerinskrywings...
StatusRegisterFiles=Registreer leers...
StatusSavingUninstall=Stoor verwyderingsinligting...
StatusRunProgram=Voltooi installasie...
StatusRestartingApplications=Herbegin programme...
StatusRollback=Rol veranderinge terug...

; *** Misc. errors
ErrorInternal2=Interne fout: %1
ErrorFunctionFailedNoCode=%1 gefaal
ErrorFunctionFailed=%1 gefaal; kode %2
ErrorFunctionFailedWithMessage=%1 gefaal; kode %2.%n%3
ErrorExecutingProgram=Onmoontlik om die volgende leer uit te voer:%n%1

; *** Registry errors
ErrorRegOpenKey=Fout terwyl registersleutel oopgemaak word:%n%1\%2
ErrorRegCreateKey=Fout terwyl registersleutel geskep word:%n%1\%2
ErrorRegWriteKey=Fout terwyl geskryf word na registersleutel:%n%1\%2

; *** INI errors
ErrorIniEntry=Fout terwyl INI-inskrywing in die leer "%1" gemaak word.

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=&Slaan hierdie leer oor (nie aanbeveel nie)
FileAbortRetryIgnoreIgnoreNotRecommended=&Ignoreer die fout en gaan aan (nie aanbeveel nie)
SourceIsCorrupted=Die bronleer is korrup
SourceDoesntExist=Die bronleer "%1" bestaan nie
ExistingFileReadOnly2=Die bestaande leer kon nie vervang word nie want dit is Alleenlik vir lees gemerk.
ExistingFileReadOnlyRetry=&Verwyder die Alleenlik-lees merker en probeer weer
ExistingFileReadOnlyKeepExisting=&Behou die bestaande leer
ErrorReadingExistingDest='n Fout het voorgekom terwyl die bestaande leer gelees is:
FileExistsSelectAction=Kies aksie
FileExists2=Die leer bestaan alreeds.
FileExistsOverwriteExisting=&Vervang die bestaande leer
FileExistsKeepExisting=&Behou die bestaande leer
FileExistsOverwriteOrKeepAll=&Doen dit vir al die volgende probleme
ExistingFileNewerSelectAction=Kies aksie
ExistingFileNewer2=Die bestaande leer is nuwer as die een wat die installeerder probeer installeer.
ExistingFileNewerOverwriteExisting=&Vervang die bestaande leer
ExistingFileNewerKeepExisting=&Behou die bestaande leer (aanbeveel)
ExistingFileNewerOverwriteOrKeepAll=&Doen dit vir al die volgende probleme
ErrorChangingAttr='n Fout het voorgekom terwyl die attribute van die bestaande leer verander is:
ErrorCreatingTemp='n Fout het voorgekom toe 'n leer in die bestaande gids geskep is:
ErrorReadingSource='n Fout het voorgekom terwyl die bronleer gelees is:
ErrorCopying='n Fout het voorgekom terwyl 'n leer gekopieer is:
ErrorReplacingExistingFile='n Fout het voorgekom toe die bestaande leer oorskryf is:
ErrorRestartReplace=HerbeginVervang gefaal:
ErrorRenamingTemp='n Fout het voorgekom terwyl 'n leer in die bestemmingsgids van naam verander is:
ErrorRegisterServer=Onmoontlik om die DLL/OCX te registreer: %1
ErrorRegSvr32Failed=RegSvr32 het gefaal met kode %1
ErrorRegisterTypeLib=Onmoontlik om die biblioteek tipe te registreer: %1

; *** Uninstall display name markings
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32-bis
UninstallDisplayNameMark64Bit=64-bis
UninstallDisplayNameMarkAllUsers=Alle gebruikers
UninstallDisplayNameMarkCurrentUser=Huidige gebruiker

; *** Post-installation errors
ErrorOpeningReadme='n Fout het voorgekom terwyl die README-leer oopgemaak is.
ErrorRestartingComputer=Die installeerder kon nie die rekenaar herbegin nie. Doen dit asb self.

; *** Uninstaller messages
UninstallNotFound=Leer "%1" bestaan nie. Kan nie verwyder nie.
UninstallOpenError=Leer "%1" kan nie oopgemaak word nie. Onmoontlik om te verwyder.
UninstallUnsupportedVer=Die verwyder staafleer "%1" se formaat word nie herken deur hierdie weergawe van die verwyderaar nie. Onmoontlik om te verwyder.
UninstallUnknownEntry='n Onbekende inskrywing (%1) is teegekom in die verwyder staafleer.
ConfirmUninstall=Is u seker dat u %1 en al die komponente daarvan heeltemal wil verwyder?
UninstallOnlyOnWin64=Hierdie installasie kan slegs verwyder word op 64-bis-Windows.
OnlyAdminCanUninstall=Hierdie installasie kan slegs verwyder word deur 'n gebruiker met administratiewe regte.
UninstallStatusLabel=Wag asb. terwyl %1 van u rekenaar verwyder word.
UninstalledAll=%1 is suksesvol verwyder vanaf u rekenaar.
UninstalledMost=%1 verwydering voltooi.%n%nSommige elemente kon nie verwyder word nie. Hierdie elemente kan handmatig verwyder word.
UninstalledAndNeedsRestart=Om die verwydering van %1 te voltooi, moet u rekenaar herbegin word.%n%nWil u nou herbegin?
UninstallDataCorrupted="%1" leer is korrup. Onmoontlik om te verwyder.

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Verwyder gedeelde leer?
ConfirmDeleteSharedFile2=Die stelsel dui aan dat die volgende gedeelde leers nie meer deur enige programme gebruik word nie. Moet die verwyderaar die gedeelde leer verwyder?%n%nIndien enige programme hierdie leer steeds gebruik en dit verwyder word, sal daardie programme nie meer reg funksioneer nie. Indien u onseker is, kies Nee. Indien die leer op u stelsel gelaat word, sal dit geen skade doen nie.
SharedFileNameLabel=Leernaam:
SharedFileLocationLabel=Ligging:
WizardUninstalling=Verwyderingstatus
StatusUninstalling=Verwyder %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Installeer %1.
ShutdownBlockReasonUninstallingApp=Verwyder %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 weergawe %2
AdditionalIcons=Bykomende ikone:
CreateDesktopIcon=Skep 'n &werksbladikoon
CreateQuickLaunchIcon=Skep 'n &Quick Launch ikoon
ProgramOnTheWeb=%1 op die Web
UninstallProgram=Verwyder %1
LaunchProgram=Voer %1 uit
AssocFileExtension=&Assosieer %1 met die %2 leeruitbreiding
AssocingFileExtension=Assosieer %1 met die %2 leeruitbreiding...
AutoStartProgramGroupDescription=Begin:
AutoStartProgram=Begin %1 outomaties
AddonHostProgramNotFound=%1 kon nie gevind word in die vouer wat u gekies het nie.%n%nWil u voortgaan ten spyte daarvan?