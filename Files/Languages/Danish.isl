; *** Inno Setup version 5.5.3+ Danish messages ***
;
; To download user-contributed translations of this file, go to:
;   http://www.jrsoftware.org/files/istrans/
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).
;
; ID: Danish.isl,v 5.5.3+ 2015/07/03 Thomas Vedel, thomas@veco.dk
; scootergrisen, 2015

[LangOptions]
; The following three entries are very important. Be sure to read and 
; understand the '[LangOptions] section' topic in the help file.
LanguageName=Dansk
LanguageID=$0406
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
SetupAppTitle=Installation
SetupWindowTitle=Installation - %1
UninstallAppTitle=Afinstall�r
UninstallAppFullTitle=%1 afinstallation

; *** Misc. common
InformationTitle=Information
ConfirmTitle=Bekr�ft
ErrorTitle=Fejl

; *** SetupLdr messages
SetupLdrStartupMessage=Dette vil installere %1. Vil du forts�tte?
LdrCannotCreateTemp=Kunne ikke oprette en midlertidig fil. Installationen afbrydes
LdrCannotExecTemp=Kunne ikke udf�re fil i den midlertidige mappe. Installationen afbrydes

; *** Startup error messages
LastErrorMessage=%1.%n%nFejl %2: %3
SetupFileMissing=Filen %1 mangler i installationsmappen. Ret venligst problemet eller f� en ny kopi af programmet.
SetupFileCorrupt=Installationsfilerne er beskadiget. F� venligst en ny kopi af installationsprogrammet.
SetupFileCorruptOrWrongVer=Installationsfilerne er beskadiget, eller ogs� er de ikke kompatible med denne version af installationsprogrammet. Ret venligst problemet eller f� en ny kopi af programmet.
InvalidParameter=En ugyldig parameter blev angivet p� kommandolinjen:%n%n%1
SetupAlreadyRunning=Installationsprogrammet k�rer allerede.
WindowsVersionNotSupported=Programmet underst�tter ikke den version af Windows som computeren k�rer.
WindowsServicePackRequired=Programmet kr�ver %1 Service Pack %2 eller senere.
NotOnThisPlatform=Programmet kan ikke k�re p� %1.
OnlyOnThisPlatform=Programmet kan kun k�re p� %1.
OnlyOnTheseArchitectures=Programmet kan kun installeres p� Windows-versioner som er designet til f�lgende processorarkitekturer:%n%n%1
MissingWOW64APIs=Den version af Windows du k�re indeholder ikke funktioner som er n�dvendige for at foretage en 64-bit installation. Install�r venligst Service Pack %1, for at rette problemet.
WinVersionTooLowError=Programmet kr�ver %1 version %2 eller senere.
WinVersionTooHighError=Programmet kan ikke installeres p� %1 version %2 eller senere.
AdminPrivilegesRequired=Du skal v�re logget p� som administrator n�r programmet installeres.
PowerUserPrivilegesRequired=Du skal v�re logget p� som administrator eller v�re medlem af gruppen Superbrugere, n�r programmet installeres.
SetupAppRunningError=Installationsprogrammet har registreret at %1 k�re.%n%nLuk venligst alle forekomster af det nu, klik p� OK for at forts�tte, eller Annuller for at afslutte.
UninstallAppRunningError=Afinstallationsprogrammet har registreret at %1 k�re.%n%nLuk venligst alle forekomster af det nu, klik p� OK for at forts�tte, eller Annuller for at afslutte.

; *** Misc. errors
ErrorCreatingDir=Installationsprogrammet kunne ikke oprette mappen "%1"
ErrorTooManyFilesInDir=Kunne ikke oprette en fil i mappen "%1" da mappen indeholder for mange filer

; *** Setup common messages
ExitSetupTitle=Afslut installationen
ExitSetupMessage=Installationen er ikke fuldf�rt. Programmet vil ikke blive installeret, hvis du afslutter nu.%n%nDu kan n�r som helst k�re installationsprogrammet igen for at fuldf�re installationen.%n%nAfslut installationen?
AboutSetupMenuItem=&Om installationsprogrammet...
AboutSetupTitle=Om installationsprogrammet
AboutSetupMessage=%1 version %2%n%3%n%n%1 hjemmeside:%n%4
AboutSetupNote=
TranslatorNote=Thomas Vedel%n%nscootergrisen

; *** Buttons
ButtonBack=< &Tilbage
ButtonNext=&N�ste >
ButtonInstall=&Install�r
ButtonOK=OK
ButtonCancel=Annuller
ButtonYes=&Ja
ButtonYesToAll=Ja til &alle
ButtonNo=&Nej
ButtonNoToAll=Nej t&il alle
ButtonFinish=&F�rdig
ButtonBrowse=&Gennemse...
ButtonWizardBrowse=G&ennemse...
ButtonNewFolder=&Opret ny mappe

; *** "Select Language" dialog messages
SelectLanguageTitle=V�lg installationssprog
SelectLanguageLabel=V�lg det sprog der skal bruges under installationen:

; *** Common wizard text
ClickNext=Klik p� N�ste, for at forts�tte, eller Annuller for at afslutte installationen.
BeveledLabel=
BrowseDialogTitle=V�lg mappe
BrowseDialogLabel=V�lg en mappe i nedenst�ende liste og klik p� OK.
NewFolderName=Ny mappe

; *** "Welcome" wizard page
WelcomeLabel1=Velkommen til installationsguiden for [name]
WelcomeLabel2=Dette vil installere [name/ver] p� computeren.%n%nDet anbefales at lukke alle de andre programmer, inden du forts�tter.

; *** "Password" wizard page
WizardPassword=Adgangskode
PasswordLabel1=Installationen er beskyttet med adgangskode.
PasswordLabel3=Indtast venligst adgangskoden og klik p� N�ste, for at forts�tte. Der skelnes mellem store og sm� bogstaver.
PasswordEditLabel=&Adgangskode:
IncorrectPassword=Den indtastede adgangskode er forkert. Pr�v venligst igen.

; *** "License Agreement" wizard page
WizardLicense=Licensaftale
LicenseLabel=L�s venligst f�lgende vigtige oplysninger, inden du forts�tter.
LicenseLabel3=L�s venligst licensaftalen. Du skal acceptere betingelserne i aftalen for at forts�tte installationen.
LicenseAccepted=Jeg &accepterer aftalen
LicenseNotAccepted=Jeg accepterer &ikke aftalen

; *** "Information" wizard pages
WizardInfoBefore=Information
InfoBeforeLabel=L�s venligst f�lgende information inden du forts�tter.
InfoBeforeClickLabel=Klik p� N�ste, n�r du er klar til at forts�tte installationen.
WizardInfoAfter=Information
InfoAfterLabel=L�s venligst f�lgende information inden du forts�tter.
InfoAfterClickLabel=Klik p� N�ste, n�r du er klar til at forts�tte installationen.

; *** "User Information" wizard page
WizardUserInfo=Brugerinformation
UserInfoDesc=Indtast venligst dine oplysninger.
UserInfoName=&Brugernavn:
UserInfoOrg=&Organisation:
UserInfoSerial=&Serienummer:
UserInfoNameRequired=Du skal indtaste et navn.

; *** "Select Destination Directory" wizard page
WizardSelectDir=V�lg installationsmappe
SelectDirDesc=Hvor skal [name] installeres?
SelectDirLabel3=Installationsprogrammet installerer [name] i f�lgende mappe.
SelectDirBrowseLabel=Klik p� N�ste, for at forts�tte. Klik p� Gennemse, hvis du vil v�lge en anden mappe.
DiskSpaceMBLabel=Der skal v�re mindst [mb] MB fri diskplads.
CannotInstallToNetworkDrive=Installationsprogrammet kan ikke installere p� et netv�rksdrev.
CannotInstallToUNCPath=Installationsprogrammet kan ikke installere p� en UNC-sti.
InvalidPath=Du skal indtaste en fuld sti med drevbogstav, for eksempel:%n%nC:\PROGRAM%n%neller en UNC-sti i formatet:%n%n\\server\share
InvalidDrive=Drevet eller UNC-stien du valgte findes ikke eller der er ikke adgang. V�lg venligst en anden.
DiskSpaceWarningTitle=Ikke nok diskplads
DiskSpaceWarning=Installationen kr�ver mindst %1 KB fri diskplads, men det valgte drev har kun %2 KB fri diskplads.%n%nVil du forts�tte alligevel?
DirNameTooLong=Mappenavnet eller stien er for langt.
InvalidDirName=Mappenavnet er ugyldigt.
BadDirName32=Mappenavne m� ikke indeholde f�lgende tegn:%n%n%1
DirExistsTitle=Mappen findes
DirExists=Mappen:%n%n%1%n%nfindes allerede. Vil du installere i mappen alligevel?
DirDoesntExistTitle=Mappen findes ikke
DirDoesntExist=Mappen:%n%n%1%n%nfindes ikke. Skal mappen oprettes?

; *** "Select Components" wizard page
WizardSelectComponents=V�lg komponenter
SelectComponentsDesc=Hvilke komponenter skal installeres?
SelectComponentsLabel2=V�lg de komponenter der skal installeres, og ryd de komponenter du ikke vil have installeret. Klik p� N�ste, n�r du er klar til at forts�tte.
FullInstallation=Fuld installation
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Kompakt installation
CustomInstallation=Tilpasset installation
NoUninstallWarningTitle=Komponenterne findes
NoUninstallWarning=Installationsprogrammet har registreret at f�lgende komponenter allerede er installeret p� computeren:%n%n%1%n%nKomponenterne bliver ikke afinstalleret hvis de frav�lges.%n%nVil du forts�tte alligevel?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=De nuv�rende valg kr�ver mindst [mb] MB fri diskplads.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=V�lg yderligere opgaver
SelectTasksDesc=Hvilke yderligere opgaver skal udf�res?
SelectTasksLabel2=V�lg de yderligere opgaver du vil have installationsprogrammet til at udf�rer under installationen af [name], og klik p� N�ste.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=V�lg mappe i menuen Start
SelectStartMenuFolderDesc=Hvor skal installationsprogrammet placere genveje til programmet?
SelectStartMenuFolderLabel3=Installationsprogrammet opretter genveje til programmet i f�lgende mappe i menuen Start.
SelectStartMenuFolderBrowseLabel=Klik p� N�ste, for at forts�tte. Klik p� Gennemse, hvis du vil v�lge en anden mappe.
MustEnterGroupName=Du skal indtaste et mappenavn.
GroupNameTooLong=Mappenavnet eller stien er for langt.
InvalidGroupName=Mappenavnet er ugyldigt.
BadGroupName=Mappenavnet m� ikke indeholde f�lgende tegn:%n%n%1
NoProgramGroupCheck2=&Opret ikke en mappe i menuen Start

; *** "Ready to Install" wizard page
WizardReady=Klar til at installere
ReadyLabel1=Installationsprogrammet er nu klar til at installere [name] p� computeren.
ReadyLabel2a=Klik p� Install�r, for at forts�tte med installationen, eller klik p� Tilbage, hvis du vil se eller �ndre indstillingerne.
ReadyLabel2b=Klik p� Install�r, for at forts�tte med installationen.
ReadyMemoUserInfo=Brugerinformation:
ReadyMemoDir=Installationsmappe:
ReadyMemoType=Installationstype:
ReadyMemoComponents=Valgte komponenter:
ReadyMemoGroup=Mappe i menuen Start:
ReadyMemoTasks=Yderligere opgaver:

; *** "Preparing to Install" wizard page
WizardPreparing=Klarg�ring af installationen
PreparingDesc=Installationsprogrammet g�r klar til at installere [name] p� din computer.
PreviousInstallNotCompleted=Installationen/fjernelsen af et foreg�ende program blev ikke fuldf�rt. Du skal genstarte computeren for at afslutte den foreg�ende installation.%n%N�r computeren er blevet genstartet skal du k�re installationsprogrammet igen for at fuldf�re installationen af [name].
CannotContinue=Installationsprogrammet kan ikke forts�tte. Klik venligst p� Fortryd for at afslutte.
ApplicationsFound=F�lgende programmer bruger filer som skal opdateres. Det anbefales at du giver installationsprogrammet tilladelse til automatisk at lukke programmerne.
ApplicationsFound2=F�lgende programmer bruger filer som skal opdateres. Det anbefales at du giver installationsprogrammet tilladelse til automatisk at lukke programmerne. Installationsprogrammet vil fors�ge at genstarte programmerne, n�r installationen er fuldf�rt.
CloseApplications=&Luk programmerne automatisk
DontCloseApplications=Luk &ikke programmerne
ErrorCloseApplications=Installationsprogrammet kunne ikke lukke alle programmerne automatisk. Det anbefales at du lukker alle programmer som bruger filer der skal opdateres af installationsprogrammet, inden du forts�tter.

; *** "Installing" wizard page
WizardInstalling=Installerer
InstallingLabel=Vent venligst mens installationsprogrammet installerer [name] p� computeren.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Fuldf�rer installationsguiden for [name]
FinishedLabelNoIcons=Installationsprogrammet har fuldf�rt installationen af [name] p� computeren.
FinishedLabel=Installationsprogrammet har fuldf�rt installationen af [name] p� computeren. Programmet kan startes ved at v�lge de ikoner der er blevet installeret.
ClickFinish=Klik p� F�rdig, for at afslutte installationsprogrammet.
FinishedRestartLabel=Computeren skal genstartes, for at fuldf�re installationen af [name]. Vil du genstarte nu?
FinishedRestartMessage=Computeren skal genstartes, for at fuldf�re installationen af [name].%n%nVil du genstarte nu?
ShowReadmeCheck=Ja, jeg vil gerne se README-filen
YesRadio=&Ja, genstart computeren nu
NoRadio=&Nej, jeg genstarter computeren senere
; used for example as 'Run MyProg.exe'
RunEntryExec=K�r %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Vis %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Installationsprogrammet skal bruge den n�ste disk
SelectDiskLabel2=Inds�t disk %1 og klik p� OK.%n%nHvis filerne findes i en anden mappe, end den herunder, s� indtast den korrekte sti eller klik p� Gennemse.
PathLabel=&Sti:
FileNotInDir2=Filen "%1" blev ikke fundet i "%2". Inds�t venligst den korrekte disk eller v�lg en anden mappe.
SelectDirectoryLabel=Angiv venligst placeringen af den n�ste disk.

; *** Installation phase messages
SetupAborted=Installationen blev ikke fuldf�rt.%n%nRet venligst problemet og k�r installationsprogrammet igen.
EntryAbortRetryIgnore=Klik p� Fors�g igen/Pr�v igen, for at fors�ge igen, Ignorer for at forts�tte alligevel, eller Afbryd for at annullere installationen.

; *** Installation status messages
StatusClosingApplications=Lukker programmer...
StatusCreateDirs=Opretter mapper...
StatusExtractFiles=Udpakker filer...
StatusCreateIcons=Opretter genveje...
StatusCreateIniEntries=Opretter INI-poster...
StatusCreateRegistryEntries=Opretter poster i registreringsdatabasen...
StatusRegisterFiles=Registrerer filer...
StatusSavingUninstall=Gemmer information om afinstallation...
StatusRunProgram=Fuldf�rer installation...
StatusRestartingApplications=Genstarter programmer...
StatusRollback=Gendanner �ndringer...

; *** Misc. errors
ErrorInternal2=Intern fejl: %1
ErrorFunctionFailedNoCode=%1 fejlede
ErrorFunctionFailed=%1 fejlede; kode %2
ErrorFunctionFailedWithMessage=%1 fejlede; kode %2.%n%3
ErrorExecutingProgram=Kan ikke udf�re filen:%n%1

; *** Registry errors
ErrorRegOpenKey=Fejl ved �bning af n�gle i registreringsdatabase:%n%1\%2
ErrorRegCreateKey=Fejl ved oprettelse af n�gle i registreringsdatabase:%n%1\%2
ErrorRegWriteKey=Fejl ved skrivning til n�gle i registreringsdatabase:%n%1\%2

; *** INI errors
ErrorIniEntry=Fejl ved oprettelse af INI-post i filen "%1".

; *** File copying errors
FileAbortRetryIgnore=Klik p� Fors�g igen/Pr�v igen, for at pr�ve igen, Ignorer for at springe filen over (anbefales ikke), eller Afbryd for at annullere installationen.
FileAbortRetryIgnore2=Klik p� Fors�g igen/Pr�v igen, for at pr�ve igen, Ignorer for at forts�tte alligevel (anbefales ikke), eller Afbryd for at annullere installationen.
SourceIsCorrupted=Kildefilen er beskadiget
SourceDoesntExist=Kildefilen "%1" findes ikke
ExistingFileReadOnly=Den eksisterende fil er skrivebeskyttet.%n%nKlik p� Fors�g igen/Pr�v igen, for at fjerne skrivebeskyttelsesattributten, og pr�ve igen, Ignorer for at springe filen over, eller Afbryd for at annullere installationen.
ErrorReadingExistingDest=Der opstod en fejl ved l�sning af den eksisterende fil:
FileExists=Filen findes allerede.%n%nVil du have installationsprogrammet til at overskrive den?
ExistingFileNewer=Den eksisterende fil er nyere end den installationsprogrammet fors�ger at installere. Det anbefales at du beholder den eksisterende fil.%n%nVil du beholde den eksisterende fil?
ErrorChangingAttr=Der opstod en fejl ved �ndring af attributter for den eksisterende fil:
ErrorCreatingTemp=Der opstod en fejl ved filoprettelse i installationsmappen:
ErrorReadingSource=Der opstod en fejl ved l�sning af kildefilen:
ErrorCopying=Der opstod en fejl ved filkopiering:
ErrorReplacingExistingFile=Der opstod en fejl ved fors�g p� at erstatte den eksisterende fil:
ErrorRestartReplace=RestartReplace mislykkedes:
ErrorRenamingTemp=Der opstod en fejl ved fors�g p� at omd�be en fil i installationsmappen:
ErrorRegisterServer=Kan ikke registrere DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 mislykkedes med afslutningskoden %1
ErrorRegisterTypeLib=Kan ikke registrere typebiblioteket: %1

; *** Post-installation errors
ErrorOpeningReadme=Der opstod en fejl ved fors�g p� at �bne README-filen.
ErrorRestartingComputer=Installationsprogrammet kunne ikke genstarte computeren. Genstart venligst computeren manuelt.

; *** Uninstaller messages
UninstallNotFound=Filen "%1" findes ikke. Kan ikke afinstallere.
UninstallOpenError=Filen "%1" kunne ikke �bnes. Kan ikke afinstallere
UninstallUnsupportedVer=Afinstallationslogfilen "%1" er i et format der ikke genkendes af denne version af afinstallationsprogrammet. Kan ikke afinstallere
UninstallUnknownEntry=Ukendt post (%1) i afinstallingslogfilen
ConfirmUninstall=Er du sikker p� at du vil afinstallere %1 og alle dens komponenter?
UninstallOnlyOnWin64=Installationen kan kun afinstalleres p� 64-bit Windows.
OnlyAdminCanUninstall=Programmet kan kun afinstalleres af en bruger med administratorrettigheder.
UninstallStatusLabel=Vent venligst mens %1 afinstalleres fra computeren.
UninstalledAll=%1 blev fjernet fra computeren.
UninstalledMost=%1 afinstallation fuldf�rt.%n%nNogle elementer kunne ikke fjernes. De kan fjernes manuelt.
UninstalledAndNeedsRestart=Computeren skal genstartes, for at fuldf�re afinstallation af %1.%n%nVil du genstarte nu?
UninstallDataCorrupted=Filen "%1" er beskadiget. Kan ikke afinstallere

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Fjern delt fil?
ConfirmDeleteSharedFile2=Systemet indikerer at f�lgende delte fil ikke l�ngere bruges af nogle programmer. Skal afinstallationsprogrammet fjerne den delte fil?%n%nHvis der er programmer som stadig bruger filen og den fjernes, vil disse programmer m�ske ikke fungere korrekt. V�lg Nej, hvis du er i tvivl. Der sker ikke noget ved at beholde filen p� systemet.
SharedFileNameLabel=Filnavn:
SharedFileLocationLabel=Placering:
WizardUninstalling=Status for afinstallation
StatusUninstalling=Afinstallerer %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Installerer %1.
ShutdownBlockReasonUninstallingApp=Afinstallerer %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 version %2
AdditionalIcons=Yderligere ikoner:
CreateDesktopIcon=Opret ikon p� &skrivebordet
CreateQuickLaunchIcon=Opret ikon i &Hurtig start
ProgramOnTheWeb=%1 p� nettet
UninstallProgram=Afinstall�r %1
LaunchProgram=Start %1
AssocFileExtension=&Tilknyt %1 med filtypen %2
AssocingFileExtension=Tilknyt %1 med filtypen %2...
AutoStartProgramGroupDescription=Start:
AutoStartProgram=Start automatisk %1
AddonHostProgramNotFound=%1 blev ikke fundet i den valgte mappe.%n%nVil du forts�tte alligevel?
