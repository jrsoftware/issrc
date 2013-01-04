; *** Inno Setup version 5.5.3+ Corsican messages ***
;
; To download user-contributed translations of this file, go to :
;   http ://www.jrsoftware.org/files/istrans/
;
; Note : When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).
;
; Created and maintained by Patriccollu di Santa Maria Sichè
;
; E-mail: Patrick.Santa-Maria@LaPoste.Net
;
; Changes :
; January 3, 2013 : update to version 5.5.3+
; August 8, 2012 : update to version 5.5.0+
; September 17, 2011 : creation for version 5.1.11

[LangOptions]
; The following three entries are very important. Be sure to read and 
; understand the '[LangOptions] section' topic in the help file.
LanguageName=Corsu
LanguageID=$0483
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
SetupAppTitle=Installazione
SetupWindowTitle=Installazione di %1
UninstallAppTitle=Disinstallazione
UninstallAppFullTitle=Disinstallazione di %1

; *** Misc. common
InformationTitle=Infurmazione
ConfirmTitle=Cunfirmà
ErrorTitle=Errore

; *** SetupLdr messages
SetupLdrStartupMessage=Site prontu à installà %1 ?
LdrCannotCreateTemp=Ùn hè micca pussibule di creà unu schedariu timpurariu. Installazione cancellata
LdrCannotExecTemp=Ùn hè micca pussibule di eseguisce unu schedariu in lu cartulare timpurariu. Installazione cancellata

; *** Startup error messages
LastErrorMessage=%1.%n%nErrore %2 : %3
SetupFileMissing=U schedariu %1 ùn hè micca in u cartulare di l'installazione. Ci vole à curreghje stu prublemu o riceve una nova copia di u programmu.
SetupFileCorrupt=I schedarii di setup sò alterati. Ci vole à riceve una nova copia di u programmu.
SetupFileCorruptOrWrongVer=I schedarii di setup sò alterati, o sò incompatibili cù sta versione di u prugrammu d'installazione. Ci vole à curreghje stu prublemu o riceve una nova copia di u programmu.
InvalidParameter=Unu parametru micca validu hè statu passatu nantu à a linea di cumanda :%n%n%1
SetupAlreadyRunning=U prucessu d'installazione hè dighià in funzione.
WindowsVersionNotSupported=Stu prugrammu ùn supporta micca a versione di Windows installata nantu à st'urdinatore.
WindowsServicePackRequired=Stu prugrammu richiede %1 Service Pack %2 o più ricente.
NotOnThisPlatform=Stu prugrammu ùn funziona micca cù %1.
OnlyOnThisPlatform=Stu prugrammu deve funzionà cù %1.
OnlyOnTheseArchitectures=Stu prugrammu pò esse installatu solu nantu à i versioni di Windows chì funzionanu cù st'architetture di prucessore :%n%n%1
MissingWOW64APIs=A versione di Windows uduprata ùn hà micca i funzioni per fà una installazione 64-bit. Per curreghje stu prublemu, ci vole à installà u Service Pack %1.
WinVersionTooLowError=Stu programmu richiede a versione %2 o più recente di %1.
WinVersionTooHighError=Stu programmu ùn pò micca esse installatu nantu à a versione %2 o più recente di %1.
AdminPrivilegesRequired=Ci vole à esse amministratore per installà stu prugrammu.
PowerUserPrivilegesRequired=Ci vole à esse amministratore o membru di u gruppu di i "Power Users" per installà stu prugrammu.
SetupAppRunningError=%1 hè dighjà in esecuzione.%n%nChjode avà tutte l'istanze di stu prugrammu è sciglite OK per continuà, osinnò Cancellà per abbandunà l'installazione.
UninstallAppRunningError=%1 hè dighjà in esecuzione.%n%nChjode avà tutte l'istanze di stu prugrammu è sciglite OK per continuà, osinnò Cancellà per abbandunà a disinstallazione.

; *** Misc. errors
ErrorCreatingDir=Impussibule di creà u cartulare "%1"
ErrorTooManyFilesInDir=Impussibule di creà unu schedariu in u cartulare "%1" perchè ci sò dighjà troppu di schedarii

; *** Setup common messages
ExitSetupTitle=Esce di l'installazione
ExitSetupMessage=L'installazione ùn hè micca compia. S'è voi escite l'installazione avà, u prugrammu ùn serà micca installatu.%n%nCi vulerà à ripiglià l'installazione più tardi per compiella.%n%nEsce di l'installazione ?
AboutSetupMenuItem=&Apprupositu di l'Assistente d'Installazione...
AboutSetupTitle=Apprupositu di l'Assistente d'Installazione
AboutSetupMessage=%1 versione %2%n%3%n%nPagina d'accolta %1 :%n%4
AboutSetupNote=
TranslatorNote=Traduzzione corsa da Patriccollu di Santa Maria Sichè

; *** Buttons
ButtonBack=< &Antecedente
ButtonNext=&Seguente >
ButtonInstall=In&stallà
ButtonOK=OK
ButtonCancel=Cancellà
ButtonYes=&Iè
ButtonYesToAll=Iè per &tutti
ButtonNo=I&nnò
ButtonNoToAll=Inn&ò per tutti
ButtonFinish=&Piantà
ButtonBrowse=&Sfuglià...
ButtonWizardBrowse=S&fuglià...
ButtonNewFolder=&Creà novu cartulare

; *** "Select Language" dialog messages
SelectLanguageTitle=Lingua di l'Assistente d'Installazione
SelectLanguageLabel=Sceglie a lingua à aduprà durante l'installazione :

; *** Common wizard text
ClickNext=Sceglie Seguente per cuntinuà, o Cancellà per compie l'installazione.
BeveledLabel=
BrowseDialogTitle=Sfuglià i cartulari
BrowseDialogLabel=Sceglite unu cartulare nantu à a lista inghjò, eppò OK.
NewFolderName=Novu cartulare

; *** "Welcome" wizard page
WelcomeLabel1=Benvenutu in l'Assistente d'Installazione di [name]
WelcomeLabel2=St'Assistente hè prontu à installà [name/ver] nantu à u vostru urdinatore.%n%nHè ricummendatu di chjode tutti l'altri appiecazioni nanzu à cuntinuà.

; *** "Password" wizard page
WizardPassword=Parolla d'intrata
PasswordLabel1=St'installazione hè prutetta cù una parolla d'intrata.
PasswordLabel3=Entrite a parolla d'intrata, eppò sceglite Seguente per cuntinuà. Fate casu di rispettà e minuscule è maiuscule in e parolle d'intrata.
PasswordEditLabel=&Parolla d'intrata :
IncorrectPassword=A parolla d'intrata hè falsa. Pruvate un'altra volta.

; *** "License Agreement" wizard page
WizardLicense=Cuntrattu d'Utilizazione
LicenseLabel=L'infurmazioni chì seguitanu sò impurtentissimi. Ci vole à leghjelli nanzu à cuntinuà.
LicenseLabel3=Per piacè leghjite u Cuntrattu d'Utilizazione chì seguita. Ci vole à esse d'accunsentu cù tutti i vucabuli di stu cuntrattu per pudè cuntinuà l'installazione.
LicenseAccepted=Capiscu è sò d'&accunsentu cù tutti i vucabuli di u cuntrattu
LicenseNotAccepted=Ùn sò &micca d'accunsentu cù stu cuntrattu

; *** "Information" wizard pages
WizardInfoBefore=Infurmazione
InfoBeforeLabel=L'infurmazioni chì seguitanu sò impurtentissimi. Ci vole à leghjelli nanzu à cuntinuà.
InfoBeforeClickLabel=Quandu site prontu à cuntinuà, sceglite Seguente.
WizardInfoAfter=Infurmazione
InfoAfterLabel=L'infurmazioni chì seguitanu sò impurtentissimi. Ci vole à leghjelli nanzu à cuntinuà.
InfoAfterClickLabel=Quandu site prontu à cuntinuà, sceglite Seguente.

; *** "User Information" wizard page
WizardUserInfo=Infurmazione Utilizatore
UserInfoDesc=Entrite i vostri infurmazioni.
UserInfoName=&Nome di l'utilizatore :
UserInfoOrg=&Urganismu :
UserInfoSerial=Numeru di &seria :
UserInfoNameRequired=Ci vole à entre unu nome.

; *** "Select Destination Location" wizard page
WizardSelectDir=Selezziunà u cartulare d'installazione
SelectDirDesc=Induve ci vole à installà [name] ?
SelectDirLabel3=L'Assistente hà da installà [name] in stu cartulare.
SelectDirBrowseLabel=Per cuntinuà, sceglite Seguente. S'è voi preferite un'altru cartulare, sceglite Sfuglià.
DiskSpaceMBLabel=U prugrammu hà bisognu d'al menu [mb] Mo di spaziu liberu nantu à u dischettu duru.
CannotInstallToNetworkDrive=Ùn hè micca pussibule di fà l'installazione nantu à unu dischettu di a reta.
CannotInstallToUNCPath=Ùn hè micca pussibule d'impiegà unu caminu UNC. S'è voi vulete installà nantu à a reta, ci vole à cunnettà, primu, u lettore di reta.
InvalidPath=Ci vole à entre unu caminu cumplettu ; per indettu :%n%nC :\APP%n%è micca unu caminu UNC di a forma :%n%n\\server\share
InvalidDrive=L'unità o u caminu UNC selezziunatu ùn esiste o ùn hè micca dispunibule. Per piacà, sceglite un'altru.
DiskSpaceWarningTitle=Spaziu nantu à u dischettu duru ùn hè micca abbastanza
DiskSpaceWarning=L'Assistente richiede almenu %1 Ko di spaziu liberu per l'installazione, ma l'unità selezziunata hà solu %2 Ko dispunibule.%n%nVulete cuntinuà quantunque ?
DirNameTooLong=U nome di u cartulare o u caminu hè troppu longu.
InvalidDirName=U nome di u cartulare ùn hè micca leghjittimu.
BadDirName32=U nome di u cartulare ùn pò micca cuntene sti caratteri :%n%n%1
DirExistsTitle=Cartulare esistente
DirExists=U cartulare :%n%n%1%n%nesiste dighjà. Vulete installà in stu cartulare quantunque ?
DirDoesntExistTitle=Cartulare inesistente
DirDoesntExist=U cartulare :%n%n%1%n%nùn esiste micca. Vulete creà stu cartulare ?

; *** "Select Components" wizard page
WizardSelectComponents=Selezziunà i cumpunenti
SelectComponentsDesc=Chì cumpunenti vulete installà ?
SelectComponentsLabel2=Selezziunate i cumpunenti chì ci vole à installà ; deselezziunate quelli ch'ùn ci vole micca à installà. Quandu site prontu à cuntinuà, sceglite Seguente.
FullInstallation=Installazione cumpleta
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Installazione strinta
CustomInstallation=Installazione persunalizata
NoUninstallWarningTitle=Cumpunenti esistenti
NoUninstallWarning=L'Assistente hà trovu sti cumpunenti dighjà installati nantu à l'urdinatore :%n%n%1%n%nDeselezziunà sti cumpunenti ùn i disinstallerà micca.%n%nVulete cuntinuà ?
ComponentSize1=%1 Ko
ComponentSize2=%1 Mo
ComponentsDiskSpaceMBLabel=A selezzione currente richiede al menu [mb] Mo di spaziu liberu nantu à u dischettu duru.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Selezziunà trattamenti addizziunali
SelectTasksDesc=Chì trattamenti addizziunali vulete fà ?
SelectTasksLabel2=Selezziunate i trattamenti addizziunali chì l'Assistente hà da fà durante l'installazione di [name], eppò sceglite Seguente.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Selezzione di u cartulare di u Menu Démarrer (Start)
SelectStartMenuFolderDesc=Induve l'Assistente hà da mette l'accurtatoghji di u prugrammu ?
SelectStartMenuFolderLabel3=L'Assistente hà da mette l'accurtatoghji di u prugrammu in u Menu Démarrer (Start).
SelectStartMenuFolderBrowseLabel=Per cuntinuà, sceglite Seguente. S'è voi preferite un'altru cartulare,sceglite Sfuglià.
MustEnterGroupName=Ci vole à entre unu nome di cartulare.
GroupNameTooLong=U nome di u cartulare o u caminu hè troppu longu.
InvalidGroupName=U nome di u cartulare ùn hè micca leghjittimu.
BadGroupName=U nome di u cartulare ùn pò micca cuntene sti caratteri :%n%n%1
NoProgramGroupCheck2=Ù&n creà micca di cartulare in u Menu Démarrer (Start)

; *** "Ready to Install" wizard page
WizardReady=Prontu à Installà
ReadyLabel1=Avà l'Assistente hè prontu à principià l'installazione di [name] nantu à u vostru urdinatore.
ReadyLabel2a=Sceglite Installà per cuntinuà l'installazione, o sceglite Antecedente per rivede o cambià l'ozzioni.
ReadyLabel2b=Sceglite Installà per cuntinuà l'installazione.
ReadyMemoUserInfo=Infurmazione Utilizatore :
ReadyMemoDir=Cartulare d'installazione :
ReadyMemoType=Tipu d'installazione :
ReadyMemoComponents=Cumpunenti selezziunati :
ReadyMemoGroup=Cartulare di u Menu Démarrer (Start) :
ReadyMemoTasks=Trattamenti addizziunali :

; *** "Preparing to Install" wizard page
WizardPreparing=Preparazione di l'installazione
PreparingDesc=L'Assistente appronta l'installazione di [name] nantu à u vostru urdinatore.
PreviousInstallNotCompleted=L'installazione/suppressione precedente di u prugrammu ùn hè micca compia bè. Ci vulerà à dimarrà torna l'urdinatore per cumpie st'installazione.%n%nDopu ci vulerà à rilancià l'Assistente per cumpie l'installazione di [name].
CannotContinue=L'Assistente ùn pò micca cuntinuà. Sceglite Cancellà per esce.
ApplicationsFound=St'applicazioni impieganu schedarii chì devenu esse mudificati da l'installazione. Seria più faciule di permette à u prucessu di chjode autumaticamente st'applicazioni.
ApplicationsFound2=St'applicazioni impieganu schedarii chì devenu esse mudificati da l'installazione. Seria più faciule di permette à u prucessu di chjode autumaticamente st'applicazioni. S'è l'installazione si compie bè, u prucessu pruverà di rilancià l'applicazioni.
CloseApplications=Chjode &autumaticamente l'applicazioni
DontCloseApplications=&Ùn chjode micca l'applicazioni
ErrorCloseApplications=L'Assistente ùn hà micca pussutu chjode autumaticamente tutti l'applicazioni. Hè ricummandatu di chjode tutti l'applicazioni chì impieganu schedarii chì devenu esse mudificati da l'Assistente durante l'installazione nanzu à cuntinuà.

; *** "Installing" wizard page
WizardInstalling=Installazione in corsu
InstallingLabel=Aspittate per piacè mentre l'installazione di [name] nantu à u vostru urdinatore.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Cunclusione di l'installazione di [name]
FinishedLabelNoIcons=L'Assistente hà compiu l'installazione di [name] nantu à u vostru urdinatore.
FinishedLabel=L'Assistente hà compiu l'installazione di [name] nantu à u vostru urdinatore. L'applicazione pò esse lampata grazia à l'icone installate.
ClickFinish=Sceglite Piantà per compie l'Assistente.
FinishedRestartLabel=Per cumpie l'installazione di [name], l'Assistente hà da dimarrà torna u vostru urdinatore. Site d'accunsentu per fallu avà ?
FinishedRestartMessage=Per cumpie l'installazione di [name], l'Assistente hà da dimarrà torna u vostru urdinatore.%n%nSite d'accunsentu per fallu avà ?
ShowReadmeCheck=Iè, vogliu leghje u schedariu LISEZMOI o README
YesRadio=&Iè, dimarrà l'urdinatore avà
NoRadio=I&nnò, preferiscu dimarrà l'urdinatore più tardi
; used for example as 'Run MyProg.exe'
RunEntryExec=Eseguisce %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Fighjà %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=L'Assistente hà bisogniu di u dischettu seguente
SelectDiskLabel2=Mittite u dischettu %1 è sceglite OK.%n%nS'è i schedarii di stu dischettu si trovanu in un'altru cartulare chì quellu indicatu inghjò, intruducite u caminu currettu o sceglite Sfuglià.
PathLabel=&Caminu :
FileNotInDir2=U schedariu "%1" ùn si truva micca in "%2". Mittite u dischettu curretu o sceglite un'altru cartulare.
SelectDirectoryLabel=Ci vole à dì induve si truva u dischettu seguente.

; *** Installation phase messages
SetupAborted=L'installazione ùn hè micca compia.%n%nCi vole à curreghje u prublemu è eseguisce l'installazione un'altra volta.
EntryAbortRetryIgnore=Sceglite Réessayer per pruvà torna, Ignorer per cuntinuà quantunque, o Abandonner per cancellà l'installazione.

; *** Installation status messages
StatusClosingApplications=Chjudendu l'applicazioni…
StatusCreateDirs=Creazione di i cartulari...
StatusExtractFiles=Estrazzione di i schedarii...
StatusCreateIcons=Creazione di l'accurtatoghji...
StatusCreateIniEntries=Creazione di l'elementi INI...
StatusCreateRegistryEntries=Creazione di l'elementi di u registru...
StatusRegisterFiles=Arregistramentu di i schedarii...
StatusSavingUninstall=Cunservazione di l'informazioni di disinstallazione...
StatusRunProgram=Cumpiera di l'installazione...
StatusRestartingApplications=Relanciendu l'applicazioni...
StatusRollback=Annulazione di i mudificazioni...

; *** Misc. errors
ErrorInternal2=Errore internu : %1
ErrorFunctionFailedNoCode=Fiascu di %1
ErrorFunctionFailed=Fiascu di %1 ; codice %2
ErrorFunctionFailedWithMessage=Fiascu di %1 ; codice %2.%n%3
ErrorExecutingProgram=Impussibule di eseguisce u schedariu :%n%1

; *** Registry errors
ErrorRegOpenKey=Errore durente l'apertura di a chjave di registru :%n%1\%2
ErrorRegCreateKey=Errore durente a creazione di a chjave di registru :%n%1\%2
ErrorRegWriteKey=Errore durente a scrittura di a chjave di registru :%n%1\%2

; *** INI errors
ErrorIniEntry=Errore durente a creazione di l'elementu INI in u schedariu "%1".

; *** File copying errors
FileAbortRetryIgnore=Sceglite Réessayer per pruvà torna, Ignorer per saltà stu schedariu (scunsigliatu), o Abandonner per cancellà l'installazione.
FileAbortRetryIgnore2=Sceglite Réessayer per pruvà torna, Ignorer per cuntinuà quantunque (scunsigliatu), o Abandonner per cancellà l'installazione.
SourceIsCorrupted=U schedariu surghjente hè alteratu
SourceDoesntExist=U schedariu surghjente "%1" ùn esista micca
ExistingFileReadOnly=U schedariu esistente hà unu attributu di lettura-sola.%n%nSceglite Réessayer per caccià st'attributu è pruvà torna, Ignorer per saltà stu schedariu, o Abandonner per cancellà l'installazione.
ErrorReadingExistingDest=Un'errore hè affacatu durante a lettura di u schedariu esistente :
FileExists=U schedariu esiste dighjà.%n%nVulite chì l'Assistente u rimpiazza ?
ExistingFileNewer=U schedariu esistente hè più ricente chì quellu chì l'Assistente prova d'installà. Hè ricummendatu di cunservà u schedariu esistente.%n%nVulite cunservà u schedariu esistente ?
ErrorChangingAttr=Un'errore hè affacatu pruvendu di cambià l'attributi of u schedariu esistente :
ErrorCreatingTemp=Un'errore hè affacatu pruvendu di creà unu schedariu in u cartelaru d'installazione :
ErrorReadingSource=Un'errore hè affacatu pruvendu di leghje u schedariu surghjente :
ErrorCopying=Un'errore hè affacatu pruvendu di cupià unu schedariu :
ErrorReplacingExistingFile=Un'errore hè affacatu pruvendu di rimpiazzà u schedariu esistente :
ErrorRestartReplace=Fiascu di Ridimarrà/Rimpiazzà :
ErrorRenamingTemp=Un'errore hè affacatu pruvendu di cambià u nome di unu schedariu in u cartelaru d'installazione :
ErrorRegisterServer=Impussibule d'arregistrà a bibliuteca DLL/OCX : %1
ErrorRegSvr32Failed=Fiascu di RegSvr32 cù u codice di uscita %1
ErrorRegisterTypeLib=Impussibule d'arregistrà a bibliuteca di tipu : %1

; *** Post-installation errors
ErrorOpeningReadme=Un'errore hè affacatu pruvendu di leghje u schedariu LISEZMOI/README.
ErrorRestartingComputer=L'Assistente ùn pò micca ridimarrà l'urdinatore. Ci vole à fallu manualmente.

; *** Uninstaller messages
UninstallNotFound=U schedariu "%1" ùn esista micca. Impussibule di disinstallà.
UninstallOpenError=U schedariu "%1" ùn pò micca esse apertu. Impussibule di disinstallà
UninstallUnsupportedVer=U schedariu ghjurnale 'log' "%1" si trova in una forma micca ricunnisciuta da sta versione di l'Assistente di disinstallazione. Impussibule di disinstallà
UninstallUnknownEntry=Una infurmazione scunisciuta (%1) si trova in u schedariu ghjurnale 'log' di disinstallazione
ConfirmUninstall=Site sicuru di vulè scaccià tutalmente %1 è tutti i so cumpunenti ?
UninstallOnlyOnWin64=A disinstallazione di stu prugrammu si pò fà solu cù una versione  64-bit di Windows.
OnlyAdminCanUninstall=A disinstallazione di stu prugrammu si pò fà solu da unu utilisatore di u gruppu di l'Amministratori.
UninstallStatusLabel=Aspittate per piacè fin'à chì %1 hè scacciatu da u vostru urdinatore.
UninstalledAll=%1 hà statu scacciatu bè da u vostru urdinatore.
UninstalledMost=A disinstallazione di %1 hè compia.%n%nQualchì elementu ùn pò micca esse scacciatu. Ci vole à scacciallu manualmente.
UninstalledAndNeedsRestart=Per cumpie a disinstallazione di %1, ci vole à dimarrà torna u vostru urdinatore.%n%nSite d'accunsentu per fallu avà ?
UninstallDataCorrupted=U schedariu "%1" hè alteratu. Impussibule di disinstallà

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Scaccià i schedarii cundivisi ?
ConfirmDeleteSharedFile2=U sistemu indica chì i schedarii cundivisi ùn sò più aduprati d'alcunu programmu. Vulite scaccià i schedarii cundivisi ?%n%nS'è qualchì programmu sempre adupra sti schedarii è quessi sò scacciati, ùn puderà funziunà currettamente. Se ùn site micca sicuru, sceglite No. Lascià sti schedarii nantu à u sistemu ùn pò micca pruduce danni.
SharedFileNameLabel=Nome di u schedariu :
SharedFileLocationLabel=Lucalizazione :
WizardUninstalling=Statu di a disinstallazione
StatusUninstalling=Disinstallazione di %1 in corsu...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Installazione di %1.
ShutdownBlockReasonUninstallingApp=Disinstallazione di %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 versione %2
AdditionalIcons=Addizziunale icone :
CreateDesktopIcon=Creà una icona nantu à u &Scagnu (Bureau)
CreateQuickLaunchIcon=Creà una icona nantu à a barra di &Lanciu rapidu
ProgramOnTheWeb=%1 nantu à u Web
UninstallProgram=Desinstallà %1
LaunchProgram=Dimarrà %1
AssocFileExtension=&Assucià %1 cù l'estensione di u schedariu %2
AssocingFileExtension=Assuciazion di %1 cù l'estensione di u schedariu %2...
AutoStartProgramGroupDescription=Relancia autumatica :
AutoStartProgram=Relancià automaticamente %1
AddonHostProgramNotFound=Impussibule di truvà %1 in u cartulare selezziunatu.%n%nVulete cuntinuà l'installazione quantunque ?
