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
UninstallAppTitle=Afinstallér
UninstallAppFullTitle=%1 afinstallation

; *** Misc. common
InformationTitle=Information
ConfirmTitle=Bekræft
ErrorTitle=Fejl

; *** SetupLdr messages
SetupLdrStartupMessage=Dette vil installere %1. Vil du fortsætte?
LdrCannotCreateTemp=Kunne ikke oprette en midlertidig fil. Installationen afbrydes
LdrCannotExecTemp=Kunne ikke udføre fil i den midlertidige mappe. Installationen afbrydes

; *** Startup error messages
LastErrorMessage=%1.%n%nFejl %2: %3
SetupFileMissing=Filen %1 mangler i installationsmappen. Ret venligst problemet eller få en ny kopi af programmet.
SetupFileCorrupt=Installationsfilerne er beskadiget. Få venligst en ny kopi af installationsprogrammet.
SetupFileCorruptOrWrongVer=Installationsfilerne er beskadiget, eller også er de ikke kompatible med denne version af installationsprogrammet. Ret venligst problemet eller få en ny kopi af programmet.
InvalidParameter=En ugyldig parameter blev angivet på kommandolinjen:%n%n%1
SetupAlreadyRunning=Installationsprogrammet kører allerede.
WindowsVersionNotSupported=Programmet understøtter ikke den version af Windows som computeren kører.
WindowsServicePackRequired=Programmet kræver %1 Service Pack %2 eller senere.
NotOnThisPlatform=Programmet kan ikke køre på %1.
OnlyOnThisPlatform=Programmet kan kun køre på %1.
OnlyOnTheseArchitectures=Programmet kan kun installeres på Windows-versioner som er designet til følgende processorarkitekturer:%n%n%1
MissingWOW64APIs=Den version af Windows du køre indeholder ikke funktioner som er nødvendige for at foretage en 64-bit installation. Installér venligst Service Pack %1, for at rette problemet.
WinVersionTooLowError=Programmet kræver %1 version %2 eller senere.
WinVersionTooHighError=Programmet kan ikke installeres på %1 version %2 eller senere.
AdminPrivilegesRequired=Du skal være logget på som administrator når programmet installeres.
PowerUserPrivilegesRequired=Du skal være logget på som administrator eller være medlem af gruppen Superbrugere, når programmet installeres.
SetupAppRunningError=Installationsprogrammet har registreret at %1 køre.%n%nLuk venligst alle forekomster af det nu, klik på OK for at fortsætte, eller Annuller for at afslutte.
UninstallAppRunningError=Afinstallationsprogrammet har registreret at %1 køre.%n%nLuk venligst alle forekomster af det nu, klik på OK for at fortsætte, eller Annuller for at afslutte.

; *** Misc. errors
ErrorCreatingDir=Installationsprogrammet kunne ikke oprette mappen "%1"
ErrorTooManyFilesInDir=Kunne ikke oprette en fil i mappen "%1" da mappen indeholder for mange filer

; *** Setup common messages
ExitSetupTitle=Afslut installationen
ExitSetupMessage=Installationen er ikke fuldført. Programmet vil ikke blive installeret, hvis du afslutter nu.%n%nDu kan når som helst køre installationsprogrammet igen for at fuldføre installationen.%n%nAfslut installationen?
AboutSetupMenuItem=&Om installationsprogrammet...
AboutSetupTitle=Om installationsprogrammet
AboutSetupMessage=%1 version %2%n%3%n%n%1 hjemmeside:%n%4
AboutSetupNote=
TranslatorNote=Thomas Vedel%n%nscootergrisen

; *** Buttons
ButtonBack=< &Tilbage
ButtonNext=&Næste >
ButtonInstall=&Installér
ButtonOK=OK
ButtonCancel=Annuller
ButtonYes=&Ja
ButtonYesToAll=Ja til &alle
ButtonNo=&Nej
ButtonNoToAll=Nej t&il alle
ButtonFinish=&Færdig
ButtonBrowse=&Gennemse...
ButtonWizardBrowse=G&ennemse...
ButtonNewFolder=&Opret ny mappe

; *** "Select Language" dialog messages
SelectLanguageTitle=Vælg installationssprog
SelectLanguageLabel=Vælg det sprog der skal bruges under installationen:

; *** Common wizard text
ClickNext=Klik på Næste, for at fortsætte, eller Annuller for at afslutte installationen.
BeveledLabel=
BrowseDialogTitle=Vælg mappe
BrowseDialogLabel=Vælg en mappe i nedenstående liste og klik på OK.
NewFolderName=Ny mappe

; *** "Welcome" wizard page
WelcomeLabel1=Velkommen til installationsguiden for [name]
WelcomeLabel2=Dette vil installere [name/ver] på computeren.%n%nDet anbefales at lukke alle de andre programmer, inden du fortsætter.

; *** "Password" wizard page
WizardPassword=Adgangskode
PasswordLabel1=Installationen er beskyttet med adgangskode.
PasswordLabel3=Indtast venligst adgangskoden og klik på Næste, for at fortsætte. Der skelnes mellem store og små bogstaver.
PasswordEditLabel=&Adgangskode:
IncorrectPassword=Den indtastede adgangskode er forkert. Prøv venligst igen.

; *** "License Agreement" wizard page
WizardLicense=Licensaftale
LicenseLabel=Læs venligst følgende vigtige oplysninger, inden du fortsætter.
LicenseLabel3=Læs venligst licensaftalen. Du skal acceptere betingelserne i aftalen for at fortsætte installationen.
LicenseAccepted=Jeg &accepterer aftalen
LicenseNotAccepted=Jeg accepterer &ikke aftalen

; *** "Information" wizard pages
WizardInfoBefore=Information
InfoBeforeLabel=Læs venligst følgende information inden du fortsætter.
InfoBeforeClickLabel=Klik på Næste, når du er klar til at fortsætte installationen.
WizardInfoAfter=Information
InfoAfterLabel=Læs venligst følgende information inden du fortsætter.
InfoAfterClickLabel=Klik på Næste, når du er klar til at fortsætte installationen.

; *** "User Information" wizard page
WizardUserInfo=Brugerinformation
UserInfoDesc=Indtast venligst dine oplysninger.
UserInfoName=&Brugernavn:
UserInfoOrg=&Organisation:
UserInfoSerial=&Serienummer:
UserInfoNameRequired=Du skal indtaste et navn.

; *** "Select Destination Directory" wizard page
WizardSelectDir=Vælg installationsmappe
SelectDirDesc=Hvor skal [name] installeres?
SelectDirLabel3=Installationsprogrammet installerer [name] i følgende mappe.
SelectDirBrowseLabel=Klik på Næste, for at fortsætte. Klik på Gennemse, hvis du vil vælge en anden mappe.
DiskSpaceMBLabel=Der skal være mindst [mb] MB fri diskplads.
CannotInstallToNetworkDrive=Installationsprogrammet kan ikke installere på et netværksdrev.
CannotInstallToUNCPath=Installationsprogrammet kan ikke installere på en UNC-sti.
InvalidPath=Du skal indtaste en fuld sti med drevbogstav, for eksempel:%n%nC:\PROGRAM%n%neller en UNC-sti i formatet:%n%n\\server\share
InvalidDrive=Drevet eller UNC-stien du valgte findes ikke eller der er ikke adgang. Vælg venligst en anden.
DiskSpaceWarningTitle=Ikke nok diskplads
DiskSpaceWarning=Installationen kræver mindst %1 KB fri diskplads, men det valgte drev har kun %2 KB fri diskplads.%n%nVil du fortsætte alligevel?
DirNameTooLong=Mappenavnet eller stien er for langt.
InvalidDirName=Mappenavnet er ugyldigt.
BadDirName32=Mappenavne må ikke indeholde følgende tegn:%n%n%1
DirExistsTitle=Mappen findes
DirExists=Mappen:%n%n%1%n%nfindes allerede. Vil du installere i mappen alligevel?
DirDoesntExistTitle=Mappen findes ikke
DirDoesntExist=Mappen:%n%n%1%n%nfindes ikke. Skal mappen oprettes?

; *** "Select Components" wizard page
WizardSelectComponents=Vælg komponenter
SelectComponentsDesc=Hvilke komponenter skal installeres?
SelectComponentsLabel2=Vælg de komponenter der skal installeres, og ryd de komponenter du ikke vil have installeret. Klik på Næste, når du er klar til at fortsætte.
FullInstallation=Fuld installation
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Kompakt installation
CustomInstallation=Tilpasset installation
NoUninstallWarningTitle=Komponenterne findes
NoUninstallWarning=Installationsprogrammet har registreret at følgende komponenter allerede er installeret på computeren:%n%n%1%n%nKomponenterne bliver ikke afinstalleret hvis de fravælges.%n%nVil du fortsætte alligevel?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=De nuværende valg kræver mindst [mb] MB fri diskplads.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Vælg yderligere opgaver
SelectTasksDesc=Hvilke yderligere opgaver skal udføres?
SelectTasksLabel2=Vælg de yderligere opgaver du vil have installationsprogrammet til at udfører under installationen af [name], og klik på Næste.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Vælg mappe i menuen Start
SelectStartMenuFolderDesc=Hvor skal installationsprogrammet placere genveje til programmet?
SelectStartMenuFolderLabel3=Installationsprogrammet opretter genveje til programmet i følgende mappe i menuen Start.
SelectStartMenuFolderBrowseLabel=Klik på Næste, for at fortsætte. Klik på Gennemse, hvis du vil vælge en anden mappe.
MustEnterGroupName=Du skal indtaste et mappenavn.
GroupNameTooLong=Mappenavnet eller stien er for langt.
InvalidGroupName=Mappenavnet er ugyldigt.
BadGroupName=Mappenavnet må ikke indeholde følgende tegn:%n%n%1
NoProgramGroupCheck2=&Opret ikke en mappe i menuen Start

; *** "Ready to Install" wizard page
WizardReady=Klar til at installere
ReadyLabel1=Installationsprogrammet er nu klar til at installere [name] på computeren.
ReadyLabel2a=Klik på Installér, for at fortsætte med installationen, eller klik på Tilbage, hvis du vil se eller ændre indstillingerne.
ReadyLabel2b=Klik på Installér, for at fortsætte med installationen.
ReadyMemoUserInfo=Brugerinformation:
ReadyMemoDir=Installationsmappe:
ReadyMemoType=Installationstype:
ReadyMemoComponents=Valgte komponenter:
ReadyMemoGroup=Mappe i menuen Start:
ReadyMemoTasks=Yderligere opgaver:

; *** "Preparing to Install" wizard page
WizardPreparing=Klargøring af installationen
PreparingDesc=Installationsprogrammet gør klar til at installere [name] på din computer.
PreviousInstallNotCompleted=Installationen/fjernelsen af et foregående program blev ikke fuldført. Du skal genstarte computeren for at afslutte den foregående installation.%n%Når computeren er blevet genstartet skal du køre installationsprogrammet igen for at fuldføre installationen af [name].
CannotContinue=Installationsprogrammet kan ikke fortsætte. Klik venligst på Fortryd for at afslutte.
ApplicationsFound=Følgende programmer bruger filer som skal opdateres. Det anbefales at du giver installationsprogrammet tilladelse til automatisk at lukke programmerne.
ApplicationsFound2=Følgende programmer bruger filer som skal opdateres. Det anbefales at du giver installationsprogrammet tilladelse til automatisk at lukke programmerne. Installationsprogrammet vil forsøge at genstarte programmerne, når installationen er fuldført.
CloseApplications=&Luk programmerne automatisk
DontCloseApplications=Luk &ikke programmerne
ErrorCloseApplications=Installationsprogrammet kunne ikke lukke alle programmerne automatisk. Det anbefales at du lukker alle programmer som bruger filer der skal opdateres af installationsprogrammet, inden du fortsætter.

; *** "Installing" wizard page
WizardInstalling=Installerer
InstallingLabel=Vent venligst mens installationsprogrammet installerer [name] på computeren.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Fuldfører installationsguiden for [name]
FinishedLabelNoIcons=Installationsprogrammet har fuldført installationen af [name] på computeren.
FinishedLabel=Installationsprogrammet har fuldført installationen af [name] på computeren. Programmet kan startes ved at vælge de ikoner der er blevet installeret.
ClickFinish=Klik på Færdig, for at afslutte installationsprogrammet.
FinishedRestartLabel=Computeren skal genstartes, for at fuldføre installationen af [name]. Vil du genstarte nu?
FinishedRestartMessage=Computeren skal genstartes, for at fuldføre installationen af [name].%n%nVil du genstarte nu?
ShowReadmeCheck=Ja, jeg vil gerne se README-filen
YesRadio=&Ja, genstart computeren nu
NoRadio=&Nej, jeg genstarter computeren senere
; used for example as 'Run MyProg.exe'
RunEntryExec=Kør %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Vis %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Installationsprogrammet skal bruge den næste disk
SelectDiskLabel2=Indsæt disk %1 og klik på OK.%n%nHvis filerne findes i en anden mappe, end den herunder, så indtast den korrekte sti eller klik på Gennemse.
PathLabel=&Sti:
FileNotInDir2=Filen "%1" blev ikke fundet i "%2". Indsæt venligst den korrekte disk eller vælg en anden mappe.
SelectDirectoryLabel=Angiv venligst placeringen af den næste disk.

; *** Installation phase messages
SetupAborted=Installationen blev ikke fuldført.%n%nRet venligst problemet og kør installationsprogrammet igen.
EntryAbortRetryIgnore=Klik på Forsøg igen/Prøv igen, for at forsøge igen, Ignorer for at fortsætte alligevel, eller Afbryd for at annullere installationen.

; *** Installation status messages
StatusClosingApplications=Lukker programmer...
StatusCreateDirs=Opretter mapper...
StatusExtractFiles=Udpakker filer...
StatusCreateIcons=Opretter genveje...
StatusCreateIniEntries=Opretter INI-poster...
StatusCreateRegistryEntries=Opretter poster i registreringsdatabasen...
StatusRegisterFiles=Registrerer filer...
StatusSavingUninstall=Gemmer information om afinstallation...
StatusRunProgram=Fuldfører installation...
StatusRestartingApplications=Genstarter programmer...
StatusRollback=Gendanner ændringer...

; *** Misc. errors
ErrorInternal2=Intern fejl: %1
ErrorFunctionFailedNoCode=%1 fejlede
ErrorFunctionFailed=%1 fejlede; kode %2
ErrorFunctionFailedWithMessage=%1 fejlede; kode %2.%n%3
ErrorExecutingProgram=Kan ikke udføre filen:%n%1

; *** Registry errors
ErrorRegOpenKey=Fejl ved åbning af nøgle i registreringsdatabase:%n%1\%2
ErrorRegCreateKey=Fejl ved oprettelse af nøgle i registreringsdatabase:%n%1\%2
ErrorRegWriteKey=Fejl ved skrivning til nøgle i registreringsdatabase:%n%1\%2

; *** INI errors
ErrorIniEntry=Fejl ved oprettelse af INI-post i filen "%1".

; *** File copying errors
FileAbortRetryIgnore=Klik på Forsøg igen/Prøv igen, for at prøve igen, Ignorer for at springe filen over (anbefales ikke), eller Afbryd for at annullere installationen.
FileAbortRetryIgnore2=Klik på Forsøg igen/Prøv igen, for at prøve igen, Ignorer for at fortsætte alligevel (anbefales ikke), eller Afbryd for at annullere installationen.
SourceIsCorrupted=Kildefilen er beskadiget
SourceDoesntExist=Kildefilen "%1" findes ikke
ExistingFileReadOnly=Den eksisterende fil er skrivebeskyttet.%n%nKlik på Forsøg igen/Prøv igen, for at fjerne skrivebeskyttelsesattributten, og prøve igen, Ignorer for at springe filen over, eller Afbryd for at annullere installationen.
ErrorReadingExistingDest=Der opstod en fejl ved læsning af den eksisterende fil:
FileExists=Filen findes allerede.%n%nVil du have installationsprogrammet til at overskrive den?
ExistingFileNewer=Den eksisterende fil er nyere end den installationsprogrammet forsøger at installere. Det anbefales at du beholder den eksisterende fil.%n%nVil du beholde den eksisterende fil?
ErrorChangingAttr=Der opstod en fejl ved ændring af attributter for den eksisterende fil:
ErrorCreatingTemp=Der opstod en fejl ved filoprettelse i installationsmappen:
ErrorReadingSource=Der opstod en fejl ved læsning af kildefilen:
ErrorCopying=Der opstod en fejl ved filkopiering:
ErrorReplacingExistingFile=Der opstod en fejl ved forsøg på at erstatte den eksisterende fil:
ErrorRestartReplace=RestartReplace mislykkedes:
ErrorRenamingTemp=Der opstod en fejl ved forsøg på at omdøbe en fil i installationsmappen:
ErrorRegisterServer=Kan ikke registrere DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 mislykkedes med afslutningskoden %1
ErrorRegisterTypeLib=Kan ikke registrere typebiblioteket: %1

; *** Post-installation errors
ErrorOpeningReadme=Der opstod en fejl ved forsøg på at åbne README-filen.
ErrorRestartingComputer=Installationsprogrammet kunne ikke genstarte computeren. Genstart venligst computeren manuelt.

; *** Uninstaller messages
UninstallNotFound=Filen "%1" findes ikke. Kan ikke afinstallere.
UninstallOpenError=Filen "%1" kunne ikke åbnes. Kan ikke afinstallere
UninstallUnsupportedVer=Afinstallationslogfilen "%1" er i et format der ikke genkendes af denne version af afinstallationsprogrammet. Kan ikke afinstallere
UninstallUnknownEntry=Ukendt post (%1) i afinstallingslogfilen
ConfirmUninstall=Er du sikker på at du vil afinstallere %1 og alle dens komponenter?
UninstallOnlyOnWin64=Installationen kan kun afinstalleres på 64-bit Windows.
OnlyAdminCanUninstall=Programmet kan kun afinstalleres af en bruger med administratorrettigheder.
UninstallStatusLabel=Vent venligst mens %1 afinstalleres fra computeren.
UninstalledAll=%1 blev fjernet fra computeren.
UninstalledMost=%1 afinstallation fuldført.%n%nNogle elementer kunne ikke fjernes. De kan fjernes manuelt.
UninstalledAndNeedsRestart=Computeren skal genstartes, for at fuldføre afinstallation af %1.%n%nVil du genstarte nu?
UninstallDataCorrupted=Filen "%1" er beskadiget. Kan ikke afinstallere

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Fjern delt fil?
ConfirmDeleteSharedFile2=Systemet indikerer at følgende delte fil ikke længere bruges af nogle programmer. Skal afinstallationsprogrammet fjerne den delte fil?%n%nHvis der er programmer som stadig bruger filen og den fjernes, vil disse programmer måske ikke fungere korrekt. Vælg Nej, hvis du er i tvivl. Der sker ikke noget ved at beholde filen på systemet.
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
CreateDesktopIcon=Opret ikon på &skrivebordet
CreateQuickLaunchIcon=Opret ikon i &Hurtig start
ProgramOnTheWeb=%1 på nettet
UninstallProgram=Afinstallér %1
LaunchProgram=Start %1
AssocFileExtension=&Tilknyt %1 med filtypen %2
AssocingFileExtension=Tilknyt %1 med filtypen %2...
AutoStartProgramGroupDescription=Start:
AutoStartProgram=Start automatisk %1
AddonHostProgramNotFound=%1 blev ikke fundet i den valgte mappe.%n%nVil du fortsætte alligevel?
