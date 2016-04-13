; *** Inno Setup version 5.5.3+ Corsican messages ***
;
; To download user-contributed translations of this file, go to:
;   http://www.jrsoftware.org/files/istrans/
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).

; Created and maintained by Patriccollu di Santa Maria è Sichè
;
; E-mail: Patrick.Santa-Maria[at]LaPoste.Net
;
; Changes:
; April 9, 2016 - Changes to current version 5.5.3+
; January 3, 2013 - Update to version 5.5.3+
; August 8, 2012 - Update to version 5.5.0+
; September 17, 2011 - Creation for version 5.1.11

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
SetupAppTitle=Assistente d'Installazione
SetupWindowTitle=Assistente d'Installazione - %1
UninstallAppTitle=Disinstallà
UninstallAppFullTitle=Disinstallazione di %1

; *** Misc. common
InformationTitle=Infurmazione
ConfirmTitle=Cunfirmà
ErrorTitle=Sbagliu

; *** SetupLdr messages
SetupLdrStartupMessage=St'Assistente hà da installà %1. Vulete cuntinuà ?
LdrCannotCreateTemp=Impussibule di creà un cartulare timpurariu. Assistente d'Installazione interrottu
LdrCannotExecTemp=Impussibule d'eseguisce u schedariu in u cartulare timpurariu. Assistente d'Installazione interrottu

; *** Startup error messages
LastErrorMessage=%1.%n%nSbagliu %2 : %3
SetupFileMissing=U schedariu %1 manca in u cartulare d'Installazione. Ci vole à currege u prublemu o ottene una nova copia di u prugramma.
SetupFileCorrupt=I schedarii d'installazione sò alterati. Ci vole à ottene una nova copia di u prugramma.
SetupFileCorruptOrWrongVer=I schedarii d'installazione sò alterati, o sò incumpatibule cù sta versione di l'Assistente. Ci vole à currege u prublemu o ottene una nova copia di u prugramma.
InvalidParameter=Un parametru micca accettevule hè statu passatu in a linea di cumanda :%n%n%1
SetupAlreadyRunning=L'assistente d'Installazione hè dighjà in corsu.
WindowsVersionNotSupported=Stu prugramma ùn pò micca funziunà cù a versione di Windows installata nant'à st'urdinatore.
WindowsServicePackRequired=Stu prugramma richiede %1 Service Pack %2 o più recente.
NotOnThisPlatform=Stu prugramma ùn funzionerà micca cù %1.
OnlyOnThisPlatform=Stu prugramma deve funzionà cù %1.
OnlyOnTheseArchitectures=Stu prugramma pò solu esse installatu nant'à e versioni di Windows fatte apposta per st'architetture di prucessore :%n%n%1
MissingWOW64APIs=A versione di Windows impiegata quì ùn cuntene micca a funzione richiesta da l'Assistente per fà un installazione 64-bit. Per currege stu prublemu, ci vole à installà Service Pack %1.
WinVersionTooLowError=Stu prugramma richiede %1 versione %2 o più recente.
WinVersionTooHighError=Stu prugramma ùn pò micca esse installatu nant'à %1 version %2 o più recente.
AdminPrivilegesRequired=Ci vole à esse cunnettu cum'è un amministratore quandu voi installate stu prugramma.
PowerUserPrivilegesRequired=Ci vole à esse cunnettu cum'è un amministratore o un membru di u gruppu Power Users quandu voi installate stu prugramma.
SetupAppRunningError=L'Assistente hà vistu chì %1 era dighjà in corsu.%n%nCi vole à chjode tutte e so finestre avà, po sceglie Vai per cuntinuà, o Abbandunà per compie.
UninstallAppRunningError=A disinstallazione hà vistu chì %1 era dighjà in corsu.%n%nCi vole à chjode tutte e so finestre avà, po sceglie Vai per cuntinuà, o Abbandunà per compie.

; *** Misc. errors
ErrorCreatingDir=L'Assistente ùn hà micca pussutu creà u cartulare "%1"
ErrorTooManyFilesInDir=Impussibule di creà un schedariu in u cartulare "%1" perchè ellu ne cuntene troppu

; *** Setup common messages
ExitSetupTitle=Compie l'Assistente
ExitSetupMessage=L'Assistente ùn hè micca compiu bè. S'è voi escite avà, u prugramma ùn serà micca installatu.%n%nPudete impiegà l'Assistente torna un altra volta per compie l'installazione.%n%nCompie l'Assistente ?
AboutSetupMenuItem=&Apprupositu di l'Assistente...
AboutSetupTitle=Apprupositu di l'Assistente
AboutSetupMessage=%1 versione %2%n%3%n%n%1 pagina d'accolta :%n%4
AboutSetupNote=
TranslatorNote=Traduzzione corsa da Patriccollu di Santa Maria è Sichè

; *** Buttons
ButtonBack=< &Precedente
ButtonNext=&Seguente >
ButtonInstall=&Installà
ButtonOK=Vai
ButtonCancel=Abbandunà
ButtonYes=&Iè
ButtonYesToAll=Iè per &Tutti
ButtonNo=I&nnò
ButtonNoToAll=Innò per T&utti
ButtonFinish=&Piantà
ButtonBrowse=&Sfuglià...
ButtonWizardBrowse=&Sfuglià...
ButtonNewFolder=&Creà Novu Cartulare

; *** "Select Language" dialog messages
SelectLanguageTitle=Definisce Lingua di l'Assistente
SelectLanguageLabel=Selezziunà a lingua à impiegà per l'installazione :

; *** Common wizard text
ClickNext=Sceglie Seguente per cuntinuà, o Abbandunà per compie l'Assistente.
BeveledLabel=
BrowseDialogTitle=Sfuglià u Cartulare
BrowseDialogLabel=Selezziunà un cartulare in a lista inghjò, po sceglie Vai.
NewFolderName=Novu Cartulare

; *** "Welcome" wizard page
WelcomeLabel1=Benvenuta in l'Assistente d'Installazione di [name]
WelcomeLabel2=Quessu installerà [name/ver] nant'à l'urdinatore.%n%nHè ricumandatu di chjode tutte l'altre appiecazioni nanzu di cuntinuà.

; *** "Password" wizard page
WizardPassword=Parolla d'intrata
PasswordLabel1=L'installazione hè prutetta da una parolla d'intrata.
PasswordLabel3=Ci vole à pruvede a parolla d'intrata, po sceglie Seguente per cuntinuà. Sfarenzià maiuscule è minuscule in e parolle d'intrata.
PasswordEditLabel=&Parolla d'intrata :
IncorrectPassword=A parolla d'intrata pruvista ùn hè micca curretta. Ci vole à pruvà torna.

; *** "License Agreement" wizard page
WizardLicense=Cuntrattu di Licenza
LicenseLabel=Ci vole à leghje l'infurmazione impurtante chì seguiteghja nanzu di cuntinuà.
LicenseLabel3=Ci vole à leghje u Cuntrattu di Licenza chì seguiteghja. Duvete accettà i termini di stu cuntrattu nanzu di cuntinuà l'installazione.
LicenseAccepted=Sò d'&accunsentu cù u cuntrattu
LicenseNotAccepted=Ùn sò &micca d'accunsentu cù u cuntrattu

; *** "Information" wizard pages
WizardInfoBefore=Infurmazione
InfoBeforeLabel=Ci vole à leghje l'infurmazione impurtante chì seguiteghja nanzu di cuntinuà.
InfoBeforeClickLabel=Quandu site prontu à cuntinuà cù l'Assistente, sciglite Seguente.
WizardInfoAfter=Infurmazione
InfoAfterLabel=Ci vole à leghje l'infurmazione impurtante chì seguiteghja nanzu di cuntinuà.
InfoAfterClickLabel=Quandu site prontu à cuntinuà cù l'Assistente, sciglite Seguente.

; *** "User Information" wizard page
WizardUserInfo=Infurmazioni di l'Utilizatore
UserInfoDesc=Ci vole à scrive e vostre infurmazioni.
UserInfoName=&Nome d'Utilizatore :
UserInfoOrg=&Urganismu :
UserInfoSerial=&Numeru di Seria :
UserInfoNameRequired=Ci vole à scrive un nome.

; *** "Select Destination Location" wizard page
WizardSelectDir=Selezziunà u Locu di Destinazione
SelectDirDesc=Induve [name] deve esse installatu ?
SelectDirLabel3=L'Assistente installerà [name] in stu cartulare.
SelectDirBrowseLabel=Per cuntinuà, sceglie Seguente. S'è voi preferisce selezziunà un altru cartulare, sciglite Sfuglià.
DiskSpaceMBLabel=Almenu [mb] Mo di spaziu liberu di discu hè richiestu.
CannotInstallToNetworkDrive=L'Assistente ùn pò micca installà nantu un discu di a reta.
CannotInstallToUNCPath=L'Assistente ùn pò micca installà in un passeghju UNC.
InvalidPath=Ci vole à scrive un passeghju cumplettu cù a lettera di u lettore ; per indettu :%n%nC:\APP%n%no un passeghju UNC in a forma :%n%n\\servu\spartu
InvalidDrive=U lettore o u passeghju UNC spartu ùn esiste micca o ùn hè micca accessibule. Ci vole à selezziunà un altru.
DiskSpaceWarningTitle=Spaziu Discu ùn Basta
DiskSpaceWarning=L'Assistente richiede almenu %1 Ko di spaziu liberu per installà, ma u lettore selezziunatu hà solu %2 Ko dispunibule.%n%nVulete cuntinuà quantunque ?
DirNameTooLong=U nome di cartulare o u passeghju hè troppu longu.
InvalidDirName=U nome di cartulare ùn hè micca accettevule.
BadDirName32=I nomi di cartulare ùn ponu micca cuntene sti caratteri :%n%n%1
DirExistsTitle=Cartulare Esistente
DirExists=U cartulare :%n%n%1%n%nesiste dighjà. Vulete installà in stu cartulare quantunque ?
DirDoesntExistTitle=Cartulare Inesistente
DirDoesntExist=U cartulare :%n%n%1%n%nùn esiste micca. Vulete chì stu cartulare sia creatu ?

; *** "Select Components" wizard page
WizardSelectComponents=Selezzione di Cumpunenti
SelectComponentsDesc=Chì cumpunenti devenu esse installati ?
SelectComponentsLabel2=Selezziunà i cumpunenti à installà ; deselezziunà quelli ch'ùn devenu micca esse installati. Sceglie Seguente quandu site prontu à cuntinuà.
FullInstallation=Installazione cumpleta
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Installazione cumpatta
CustomInstallation=Installazione persunalizata
NoUninstallWarningTitle=Cumpunenti Esistenti
NoUninstallWarning=L'Assistente hà vistu chì sti cumpunenti sò dighjà installati nant'à l'urdinatore :%n%n%1%n%nDeselezziunà sti cumpunenti ùn i disinstallerà micca.%n%nVulete cuntinuà quantunque ?
ComponentSize1=%1 Ko
ComponentSize2=%1 Mo
ComponentsDiskSpaceMBLabel=A selezzione currente richiede almenu [mb] Mo di spaziu liberu nant'à u discu.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Selezziunà Trattamenti Addizziunali
SelectTasksDesc=Chì trattamenti addizziunali vulete fà ?
SelectTasksLabel2=Selezziunà i trattamenti addizziunali chì l'Assistente deve fà durante l'installazione di [name], po sceglie Seguente.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Selezzione di u Cartulare di u "Menu Démarrer"
SelectStartMenuFolderDesc=Induve l'Assistente deve piazzà l'accurtatoghji di u prugramma ?
SelectStartMenuFolderLabel3=L'Assistente piazzerà l'accurtatoghji di u prugramma in stu cartulare di u "Menu Démarrer".
SelectStartMenuFolderBrowseLabel=Per cuntinuà, sceglie Seguente. S'è voi preferisce selezziunà un altru cartulare, sciglite Sfuglià.
MustEnterGroupName=Ci vole à scrive un nome di cartulare.
GroupNameTooLong=U nome di cartulare o u passeghju hè troppu longu.
InvalidGroupName=U nome di cartulare ùn hè micca accettevule.
BadGroupName=U nome di u cartulare ùn pò micca cuntene alcunu di sti caratteri :%n%n%1
NoProgramGroupCheck2=Ùn creà &micca di cartulare in u "Menu Démarrer"

; *** "Ready to Install" wizard page
WizardReady=Prontu à Installà
ReadyLabel1=Avà l'Assistente hè prontu à principià l'installazione di [name] nant'à l'urdinatore.
ReadyLabel2a=Sceglie Installà per cuntinuà l'installazione, o nant'à Precedente per rivede o cambià qualchì preferenza.
ReadyLabel2b=Sceglie Installà per cuntinuà l'installazione.
ReadyMemoUserInfo=Infurmazioni di l'utilizatore :
ReadyMemoDir=Cartulare d'installazione :
ReadyMemoType=Tipu d'installazione :
ReadyMemoComponents=Cumpunenti selezziunati :
ReadyMemoGroup=Cartulare di u "Menu Démarrer" :
ReadyMemoTasks=Trattamenti addizziunali :

; *** "Preparing to Install" wizard page
WizardPreparing=Preparazione di l'Installazione
PreparingDesc=L'Assistente appronta l'installazione di [name] nant'à l'urdinatore.
PreviousInstallNotCompleted=L'installazione/cacciatura di un prugramma precedente ùn hè micca compia bè. Ci vulerà à spenghje l'urdinatore è ridimarrallu per compie st'installazione.%n%nDopu, ci vulerà à rilancià l'Assistente per compie l'installazione di [name].
CannotContinue=L'Assistente ùn pò micca cuntinuà. Sceglie Abbandunà per esce.
ApplicationsFound=St'appiecazioni impieganu schedarii chì devenu esse mudificati da l'Assistente. Hè ricumandatu di permette à l'Assistente di chjode autumaticamente st'appiecazioni.
ApplicationsFound2=St'appiecazioni impieganu schedarii chì devenu esse mudificati da l'Assistente. Hè ricumandatu di permette à l'Assistente di chjode autumaticamente st'appiecazioni. S'è l'installazione si compie bè, l'Assistente pruverà di rilancià l'appiecazioni.
CloseApplications=Chjode &autumaticamente l'appiecazioni
DontCloseApplications=Ùn chjode &micca l'appiecazioni
ErrorCloseApplications=L'Assistente ùn hà micca pussutu chjode autumaticamente tutti l'appiecazioni. Nanzu di cuntinuà, hè ricumandatu di chjode tutti l'appiecazioni chì impieganu schedarii chì devenu esse mudificati da l'Assistente durante l'installazione.

; *** "Installing" wizard page
WizardInstalling=Installazione in corsu
InstallingLabel=Ci vole à aspettà durante l'installazione di [name] nant'à l'urdinatore.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Fine di l'installazione di [name]
FinishedLabelNoIcons=L'installazione di [name] nant'à l'urdinatore hè compia.
FinishedLabel=L'installazione di [name] nant'à l'urdinatore hè compia. L'appiecazione pò esse lamciata selezziunendu l'accurtatoghji installati.
ClickFinish=Sceglie Piantà per compie l'Assistente.
FinishedRestartLabel=Per compie l'installazione di [name], l'Assistente deve spenghje l'urdinatore è ridimarrallu. Vulete spenghje l'urdinatore è ridimarrallu avà ?
FinishedRestartMessage=Per compie l'installazione di [name], l'Assistente deve spenghje l'urdinatore è ridimarrallu.%n%nVulete spenghje l'urdinatore è ridimarrallu avà ?
ShowReadmeCheck=Iè, vogliu leghje u schedariu LISEZMOI o README
YesRadio=&Iè, spenghje l'urdinatore è ridimarrallu avà
NoRadio=I&nnò, preferiscu spenghje l'urdinatore è ridimarrallu dopu
; used for example as 'Run MyProg.exe'
RunEntryExec=Eseguisce %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Fighjà %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=L'Assistente hà Bisogniu di u Discu Seguente
SelectDiskLabel2=Mette u discu %1 è sceglie Vai.%n%nS'è i schedarii di stu discu si trovanu in un'altru cartulare chì quellu indicatu inghjò, scrive u passeghju currettu o sceglie Sfuglià.
PathLabel=&Passeghju :
FileNotInDir2=U schedariu "%1" ùn si truva micca in "%2". Mette u discu curretu o sceglie un'altru cartulare.
SelectDirectoryLabel=Ci vole à specificà induve si trova u discu seguente.

; *** Installation phase messages
SetupAborted=L'installazione ùn hè micca compia bè.%n%nCi vole à currege u prublema è eseguisce l'Assistente torna.
EntryAbortRetryIgnore=Sceglie Réessayer per pruvà torna, Ignorer per cuntinuà quantunque, o Abandonner per abbandunà l'installazione.

; *** Installation status messages
StatusClosingApplications=Chjusura di l'appiecazioni…
StatusCreateDirs=Creazione di i cartulari...
StatusExtractFiles=Estrazzione di i schedarii...
StatusCreateIcons=Creazione di l'accurtatoghji...
StatusCreateIniEntries=Creazione di l'elementi INI...
StatusCreateRegistryEntries=Creazione di l'elementi di u registru...
StatusRegisterFiles=Arregistramentu di i schedarii...
StatusSavingUninstall=Cunservazione di l'informazioni di disinstallazione...
StatusRunProgram=Cumpiera di l'installazione...
StatusRestartingApplications=Relanciu di l'appiecazioni...
StatusRollback=Annulazione di i mudificazioni...

; *** Misc. errors
ErrorInternal2=Sbagliu internu : %1
ErrorFunctionFailedNoCode=Fiascu di %1
ErrorFunctionFailed=Fiascu di %1 ; codice %2
ErrorFunctionFailedWithMessage=Fiascu di %1 ; codice %2.%n%3
ErrorExecutingProgram=Impussibule d'eseguisce u schedariu :%n%1

; *** Registry errors
ErrorRegOpenKey=Sbagliu durante l'apertura di a chjave di registru :%n%1\%2
ErrorRegCreateKey=Sbagliu durante a creazione di a chjave di registru :%n%1\%2
ErrorRegWriteKey=Sbagliu durante a scrittura di a chjave di registru :%n%1\%2

; *** INI errors
ErrorIniEntry=Sbagliu durante a creazione di l'elementu INI in u schedariu "%1".

; *** File copying errors
FileAbortRetryIgnore=Sceglie Réessayer per pruvà torna, Ignorer per ignurà stu schedariu (micca ricumandatu), o Abandonner per abbandunà l'installazione.
FileAbortRetryIgnore2=Sceglie Réessayer per pruvà torna, Ignorer per cuntinuà quantunque (micca ricumandatu), o Abandonner per abbandunà l'installazione.
SourceIsCorrupted=U schedariu d'urigine hè alteratu
SourceDoesntExist=U schedariu d'urigine "%1" ùn esiste micca
ExistingFileReadOnly=U schedariu esistente hà un attributu di lettura-sola.%n%nSceglie Réessayer per caccià st'attributu è pruvà torna, Ignorer per ignurà stu schedariu, o Abandonner per abbandunà l'installazione.
ErrorReadingExistingDest=Un sbagliu s'hè affaccatu pruvendu di leghje u schedariu esistente :
FileExists=U schedariu esiste dighjà.%n%nVulete chì l'Assistente u rimpiazzi ?
ExistingFileNewer=U schedariu esistente hè più recente chì quellu chì l'Assistente prova d'installà. Hè ricumandatu di cunservà u schedariu esistente.%n%nVulete cunservà u schedariu esistente ?
ErrorChangingAttr=Un sbagliu s'hè affaccatu pruvendu di cambià l'attributi di u schedariu esistente :
ErrorCreatingTemp=Un sbagliu s'hè affaccatu pruvendu di creà un schedariu in u cartulare di destinazione :
ErrorReadingSource=Un sbagliu s'hè affaccatu pruvendu di leghje u schedariu d'urigine :
ErrorCopying=Un sbagliu s'hè affaccatu pruvendu di cupià un schedariu :
ErrorReplacingExistingFile=Un sbagliu s'hè affaccatu pruvendu di rimpiazzà u schedariu esistente :
ErrorRestartReplace=Fiascu di Rimpiazzamentu di schedariu à u riavviu di l'urdinatore :
ErrorRenamingTemp=Un sbagliu s'hè affaccatu pruvendu di rinumà un schedariu in u cartulare di destinazione :
ErrorRegisterServer=Impussibule d'arregistrà a bibliuteca DLL/OCX : %1
ErrorRegSvr32Failed=Fiascu di RegSvr32 cù codice d'esciuta %1
ErrorRegisterTypeLib=Impussibule d'arregistrà a bibliuteca di tipu : %1

; *** Post-installation errors
ErrorOpeningReadme=Un sbagliu s'hè affaccatu pruvendu d'apre u schedariu LISEZMOI o README.
ErrorRestartingComputer=L'Assistente ùn hà micca pussutu ridimarrà l'urdinatore. Ci vole à fallu manualmente.

; *** Uninstaller messages
UninstallNotFound=U schedariu "%1" ùn esiste micca. Impussibule di disinstallà.
UninstallOpenError=U schedariu "%1" ùn pò micca esse apertu. Impussibule di disinstallà
UninstallUnsupportedVer=U ghjurnale di disinstallazione "%1" hè in una forma scunnisciuta da sta versione di l'Assistente di disinstallazione. Impussibule di disinstallà
UninstallUnknownEntry=Un elementu scunisciutu (%1) hè statu trovu in u ghjurnale di disinstallazione
ConfirmUninstall=Site sicuru di vulè caccià cumpletamente %1 è tutti i so cumpunenti ?
UninstallOnlyOnWin64=St'appiecazione pò esse disinstallata solu cù una versione 64-bit di Windows.
OnlyAdminCanUninstall=St'appiecazione pò esse disinstallata solu da un utilizatore di u gruppu d'Amministratori.
UninstallStatusLabel=Ci vole à aspettà chì %1 sia cacciatu di l'urdinatore.
UninstalledAll=%1 hè statu cacciatu bè da l'urdinatore.
UninstalledMost=A disinstallazione di %1 hè compia.%n%nQualchì elementu ùn pò micca esse cacciatu. Ci vole à cacciallu manualmente.
UninstalledAndNeedsRestart=Per compie a disinstallazione di %1, l'urdinatore deve esse spentu è ridimarratu.%n%nVulete spenghje l'urdinatore è ridimarrallu avà ?
UninstallDataCorrupted=U schedariu "%1" hè alteratu. Impussibule di disinstallà

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Caccià i Schedarii Sparti ?
ConfirmDeleteSharedFile2=U sistema indica chì u schedariu spartu ùn hè più impiegatu da nisunu prugramma. Vulete chì a disinstallazione cacci stu schedariu spartu ?%n%nS'è qualchì prugramma impiega sempre stu schedariu è ch'ellu hè cacciatu, quellu prugramma ùn puderà funziunà currettamente. S'è ùn site micca sicuru, sceglie Innò. Lascià stu schedariu nant'à u sistema ùn pò micca pruduce danni.
SharedFileNameLabel=Nome di schedariu :
SharedFileLocationLabel=Lucalizazione :
WizardUninstalling=Statu di Disinstallazione
StatusUninstalling=Disinstallazione in corsu di %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Installazione di %1.
ShutdownBlockReasonUninstallingApp=Disinstallazione di %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 versione %2
AdditionalIcons=Accurtatoghji addizziunali :
CreateDesktopIcon=Creà un accurtatoghju di &Scagnu
CreateQuickLaunchIcon=Creà un accurtatoghju nant'à a barra di &Lanciu Prontu
ProgramOnTheWeb=%1 nant'à u Web
UninstallProgram=Disinstallà %1
LaunchProgram=Lancià %1
AssocFileExtension=&Assucià %1 cù l'estensione di schedariu %2
AssocingFileExtension=Associu di %1 cù l'estensione di schedariu %2...
AutoStartProgramGroupDescription=Lanciu autumaticu :
AutoStartProgram=Lanciu autumaticu di %1
AddonHostProgramNotFound=Impussibule di truvà %1 in u cartulare selezziunatu.%n%nVulete cuntinuà l'installazione quantunque ?
