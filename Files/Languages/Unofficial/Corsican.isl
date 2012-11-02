; *** Inno Setup version 5.5.0+ Corsican messages ***
;
; To download user-contributed translations of this file, go to :
;   http ://www.jrsoftware.org/files/istrans/
;
; Note : When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).
;
; Created and maintained by Patriccollu di Santa Maria Sich�
;
; E-mail: Patrick.Santa-Maria@LaPoste.Net
;
; Changes :
; September 17, 2011 : creation
; August 8, 2012 : update to 5.5.0+

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
ConfirmTitle=Cunfirm�
ErrorTitle=Errore

; *** SetupLdr messages
SetupLdrStartupMessage=Site prontu � install� %1 ?
LdrCannotCreateTemp=�n h� micca pussibule di cre� unu schedariu timpurariu. Installazione cancellata
LdrCannotExecTemp=�n h� micca pussibule di eseguisce unu schedariu in lu cartulare timpurariu. Installazione cancellata

; *** Startup error messages
LastErrorMessage=%1.%n%nErrore %2 : %3
SetupFileMissing=U schedariu %1 �n h� micca in u cartulare di l'installazione. Ci vole � curreghje stu prublemu o riceve una nova copia di u programmu.
SetupFileCorrupt=I schedarii di setup s� alterati. Ci vole � riceve una nova copia di u programmu.
SetupFileCorruptOrWrongVer=I schedarii di setup s� alterati, o s� incompatibili c� sta versione di u prugrammu d'installazione. Ci vole � curreghje stu prublemu o riceve una nova copia di u programmu.
InvalidParameter=Unu parametru micca validu h� statu passatu nantu � a linea di cumanda :%n%n%1
SetupAlreadyRunning=U prucessu d'installazione h� dighi� in funzione.
WindowsVersionNotSupported=Stu prugrammu �n supporta micca a versione di Windows installata nantu � st'urdinatore.
WindowsServicePackRequired=Stu prugrammu richiede %1 Service Pack %2 o pi� ricente.
NotOnThisPlatform=Stu prugrammu �n funziona micca c� %1.
OnlyOnThisPlatform=Stu prugrammu deve funzion� c� %1.
OnlyOnTheseArchitectures=Stu prugrammu p� esse installatu solu nantu � i versioni di Windows ch� funzionanu c� st'architetture di prucessore :%n%n%1
MissingWOW64APIs=A versione di Windows uduprata �n h� micca i funzioni per f� una installazione 64-bit. Per curreghje stu prublemu, ci vole � install� u Service Pack %1.
WinVersionTooLowError=Stu programmu richiede a versione %2 o pi� recente di %1.
WinVersionTooHighError=Stu programmu �n p� micca esse installatu nantu � a versione %2 o pi� recente di %1.
AdminPrivilegesRequired=Ci vole � esse amministratore per install� stu prugrammu.
PowerUserPrivilegesRequired=Ci vole � esse amministratore o membru di u gruppu di i "Power Users" per install� stu prugrammu.
SetupAppRunningError=%1 h� dighj� in esecuzione.%n%nChjode av� tutte l'istanze di stu prugrammu � sciglite OK per continu�, osinn� Cancell� per abbandun� l'installazione.
UninstallAppRunningError=%1 h� dighj� in esecuzione.%n%nChjode av� tutte l'istanze di stu prugrammu � sciglite OK per continu�, osinn� Cancell� per abbandun� a disinstallazione.

; *** Misc. errors
ErrorCreatingDir=Impussibule di cre� u cartulare "%1"
ErrorTooManyFilesInDir=Impussibule di cre� unu schedariu in u cartulare "%1" perch� ci s� dighj� troppu di schedarii

; *** Setup common messages
ExitSetupTitle=Esce di l'installazione
ExitSetupMessage=L'installazione �n h� micca compia. S'� voi escite l'installazione av�, u prugrammu �n ser� micca installatu.%n%nCi vuler� � ripigli� l'installazione pi� tardi per compiella.%n%nEsce di l'installazione ?
AboutSetupMenuItem=&Apprupositu di l'Assistente d'Installazione...
AboutSetupTitle=Apprupositu di l'Assistente d'Installazione
AboutSetupMessage=%1 versione %2%n%3%n%nPagina d'accolta %1 :%n%4
AboutSetupNote=
TranslatorNote=Traduzzione corsa da Patriccollu di Santa Maria Sich�

; *** Buttons
ButtonBack=< &Antecedente
ButtonNext=&Seguente >
ButtonInstall=In&stall�
ButtonOK=OK
ButtonCancel=Cancell�
ButtonYes=&I�
ButtonYesToAll=I� per &tutti
ButtonNo=I&nn�
ButtonNoToAll=Inn&� per tutti
ButtonFinish=&Piant�
ButtonBrowse=&Sfugli�...
ButtonWizardBrowse=S&fugli�...
ButtonNewFolder=&Cre� novu cartulare

; *** "Select Language" dialog messages
SelectLanguageTitle=Lingua di l'Assistente d'Installazione
SelectLanguageLabel=Sceglie a lingua � adupr� durante l'installazione :

; *** Common wizard text
ClickNext=Sceglie Seguente per cuntinu�, o Cancell� per compie l'installazione.
BeveledLabel=
BrowseDialogTitle=Sfugli� i cartulari
BrowseDialogLabel=Sceglite unu cartulare nantu � a lista inghj�, epp� OK.
NewFolderName=Novu cartulare

; *** "Welcome" wizard page
WelcomeLabel1=Benvenutu in l'Assistente d'Installazione di [name]
WelcomeLabel2=St'Assistente h� prontu � install� [name/ver] nantu � u vostru urdinatore.%n%nH� ricummendatu di chjode tutti l'altri appiecazioni nanzu � cuntinu�.

; *** "Password" wizard page
WizardPassword=Parolla d'intesa
PasswordLabel1=St'installazione h� prutetta c� una parolla d'intesa.
PasswordLabel3=Entrite a parolla d'intesa, epp� sceglite Seguente per cuntinu�. Fate casu di rispett� e minuscule � maiuscule in e parolle d'intese.
PasswordEditLabel=&Parolla d'intesa :
IncorrectPassword=A parolla d'intesa h� falsa. Pruvate un'altra volta.

; *** "License Agreement" wizard page
WizardLicense=Cuntrattu d'Utilizazione
LicenseLabel=L'infurmazioni ch� seguitanu s� impurtentissimi. Ci vole � leghjelli nanzu � cuntinu�.
LicenseLabel3=Per piac� leghjite u Cuntrattu d'Utilizazione ch� seguita. Ci vole � esse d'accunsentu c� tutti i vucabuli di stu cuntrattu per pud� cuntinu� l'installazione.
LicenseAccepted=Capiscu � s� d'&accunsentu c� tutti i vucabuli di u cuntrattu
LicenseNotAccepted=�n s� &micca d'accunsentu c� stu cuntrattu

; *** "Information" wizard pages
WizardInfoBefore=Infurmazione
InfoBeforeLabel=L'infurmazioni ch� seguitanu s� impurtentissimi. Ci vole � leghjelli nanzu � cuntinu�.
InfoBeforeClickLabel=Quandu site prontu � cuntinu�, sceglite Seguente.
WizardInfoAfter=Infurmazione
InfoAfterLabel=L'infurmazioni ch� seguitanu s� impurtentissimi. Ci vole � leghjelli nanzu � cuntinu�.
InfoAfterClickLabel=Quandu site prontu � cuntinu�, sceglite Seguente.

; *** "User Information" wizard page
WizardUserInfo=Infurmazione Utilizatore
UserInfoDesc=Entrite i vostri infurmazioni.
UserInfoName=&Nome di l'utilizatore :
UserInfoOrg=&Urganismu :
UserInfoSerial=Numeru di &seria :
UserInfoNameRequired=Ci vole � entre unu nome.

; *** "Select Destination Location" wizard page
WizardSelectDir=Selezziun� u cartulare d'installazione
SelectDirDesc=Induve ci vole � install� [name] ?
SelectDirLabel3=L'Assistente h� da install� [name] in stu cartulare.
SelectDirBrowseLabel=Per cuntinu�, sceglite Seguente. S'� voi preferite un'altru cartulare, sceglite Sfugli�.
DiskSpaceMBLabel=U prugrammu h� bisognu d'al menu [mb] Mo di spaziu liberu nantu � u dischettu duru.
CannotInstallToNetworkDrive=�n h� micca pussibule di f� l'installazione nantu � unu dischettu di a reta.
CannotInstallToUNCPath=�n h� micca pussibule d'impieg� unu caminu UNC. S'� voi vulete install� nantu � a reta, ci vole � cunnett�, primu, u lettore di reta.
InvalidPath=Ci vole � entre unu caminu cumplettu ; per indettu :%n%nC :\APP%n%� micca unu caminu UNC di a forma :%n%n\\server\share
InvalidDrive=L'unit� o u caminu UNC selezziunatu �n esiste o �n h� micca dispunibule. Per piac�, sceglite un'altru.
DiskSpaceWarningTitle=Spaziu nantu � u dischettu duru �n h� micca abbastanza
DiskSpaceWarning=L'Assistente richiede almenu %1 Ko di spaziu liberu per l'installazione, ma l'unit� selezziunata h� solu %2 Ko dispunibule.%n%nVulete cuntinu� quantunque ?
DirNameTooLong=U nome di u cartulare o u caminu h� troppu longu.
InvalidDirName=U nome di u cartulare �n h� micca leghjittimu.
BadDirName32=U nome di u cartulare �n p� micca cuntene sti caratteri :%n%n%1
DirExistsTitle=Cartulare esistente
DirExists=U cartulare :%n%n%1%n%nesiste dighj�. Vulete install� in stu cartulare quantunque ?
DirDoesntExistTitle=Cartulare inesistente
DirDoesntExist=U cartulare :%n%n%1%n%n�n esiste micca. Vulete cre� stu cartulare ?

; *** "Select Components" wizard page
WizardSelectComponents=Selezziun� i cumpunenti
SelectComponentsDesc=Ch� cumpunenti vulete install� ?
SelectComponentsLabel2=Selezziunate i cumpunenti ch� ci vole � install� ; deselezziunate quelli ch'�n ci vole micca � install�. Quandu site prontu � cuntinu�, sceglite Seguente.
FullInstallation=Installazione cumpleta
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Installazione strinta
CustomInstallation=Installazione persunalizata
NoUninstallWarningTitle=Cumpunenti esistenti
NoUninstallWarning=L'Assistente h� trovu sti cumpunenti dighj� installati nantu � l'urdinatore :%n%n%1%n%nDeselezziun� sti cumpunenti �n i disinstaller� micca.%n%nVulete cuntinu� ?
ComponentSize1=%1 Ko
ComponentSize2=%1 Mo
ComponentsDiskSpaceMBLabel=A selezzione currente richiede al menu [mb] Mo di spaziu liberu nantu � u dischettu duru.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Selezziun� trattamenti addizziunali
SelectTasksDesc=Ch� trattamenti addizziunali vulete f� ?
SelectTasksLabel2=Selezziunate i trattamenti addizziunali ch� l'Assistente h� da f� durante l'installazione di [name], epp� sceglite Seguente.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Selezzione di u cartulare di u Menu D�marrer (Start)
SelectStartMenuFolderDesc=Induve l'Assistente h� da mette l'accurtatoghji di u prugrammu ?
SelectStartMenuFolderLabel3=L'Assistente h� da mette l'accurtatoghji di u prugrammu in u Menu D�marrer (Start).
SelectStartMenuFolderBrowseLabel=Per cuntinu�, sceglite Seguente. S'� voi preferite un'altru cartulare,sceglite Sfugli�.
MustEnterGroupName=Ci vole � entre unu nome di cartulare.
GroupNameTooLong=U nome di u cartulare o u caminu h� troppu longu.
InvalidGroupName=U nome di u cartulare �n h� micca leghjittimu.
BadGroupName=U nome di u cartulare �n p� micca cuntene sti caratteri :%n%n%1
NoProgramGroupCheck2=�&n cre� micca di cartulare in u Menu D�marrer (Start)

; *** "Ready to Install" wizard page
WizardReady=Prontu � Install�
ReadyLabel1=Av� l'Assistente h� prontu � principi� l'installazione di [name] nantu � u vostru urdinatore.
ReadyLabel2a=Sceglite Install� per cuntinu� l'installazione, o sceglite Antecedente per rivede o cambi� l'ozzioni.
ReadyLabel2b=Sceglite Install� per cuntinu� l'installazione.
ReadyMemoUserInfo=Infurmazione Utilizatore :
ReadyMemoDir=Cartulare d'installazione :
ReadyMemoType=Tipu d'installazione :
ReadyMemoComponents=Cumpunenti selezziunati :
ReadyMemoGroup=Cartulare di u Menu D�marrer (Start) :
ReadyMemoTasks=Trattamenti addizziunali :

; *** "Preparing to Install" wizard page
WizardPreparing=Preparazione di l'installazione
PreparingDesc=L'Assistente appronta l'installazione di [name] nantu � u vostru urdinatore.
PreviousInstallNotCompleted=L'installazione/suppressione precedente di u prugrammu �n h� micca compia b�. Ci vuler� � dimarr� torna l'urdinatore per cumpie st'installazione.%n%nDopu ci vuler� � rilanci� l'Assistente per cumpie l'installazione di [name].
CannotContinue=L'Assistente �n p� micca cuntinu�. Sceglite Cancell� per esce.
ApplicationsFound=St'applicazioni impieganu schedarii ch� devenu esse mudificati da l'installazione. Seria pi� faciule di permette � u prucessu di chjode autumaticamente st'applicazioni.
ApplicationsFound2=St'applicazioni impieganu schedarii ch� devenu esse mudificati da l'installazione. Seria pi� faciule di permette � u prucessu di chjode autumaticamente st'applicazioni. S'� l'installazione si compie b�, u prucessu pruver� di rilanci� l'applicazioni.
CloseApplications=Chjode &autumaticamente l'applicazioni
DontCloseApplications=&�n chjode micca l'applicazioni

; *** "Installing" wizard page
WizardInstalling=Installazione in corsu
InstallingLabel=Aspittate per piac� mentre l'installazione di [name] nantu � u vostru urdinatore.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Cunclusione di l'installazione di [name]
FinishedLabelNoIcons=L'Assistente h� compiu l'installazione di [name] nantu � u vostru urdinatore.
FinishedLabel=L'Assistente h� compiu l'installazione di [name] nantu � u vostru urdinatore. L'applicazione p� esse lampata grazia � l'icone installate.
ClickFinish=Sceglite Piant� per compie l'Assistente.
FinishedRestartLabel=Per cumpie l'installazione di [name], l'Assistente h� da dimarr� torna u vostru urdinatore. Site d'accunsentu per fallu av� ?
FinishedRestartMessage=Per cumpie l'installazione di [name], l'Assistente h� da dimarr� torna u vostru urdinatore.%n%nSite d'accunsentu per fallu av� ?
ShowReadmeCheck=I�, vogliu leghje u schedariu LISEZMOI o README
YesRadio=&I�, dimarr� l'urdinatore av�
NoRadio=I&nn�, preferiscu dimarr� l'urdinatore pi� tardi
; used for example as 'Run MyProg.exe'
RunEntryExec=Eseguisce %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Fighj� %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=L'Assistente h� bisogniu di u dischettu seguente
SelectDiskLabel2=Mittite u dischettu %1 � sceglite OK.%n%nS'� i schedarii di stu dischettu si trovanu in un'altru cartulare ch� quellu indicatu inghj�, intruducite u caminu currettu o sceglite Sfugli�.
PathLabel=&Caminu :
FileNotInDir2=U schedariu "%1" �n si truva micca in "%2". Mittite u dischettu curretu o sceglite un'altru cartulare.
SelectDirectoryLabel=Ci vole � d� induve si truva u dischettu seguente.

; *** Installation phase messages
SetupAborted=L'installazione �n h� micca compia.%n%nCi vole � curreghje u prublemu � eseguisce l'installazione un'altra volta.
EntryAbortRetryIgnore=Sceglite R�essayer per pruv� torna, Ignorer per cuntinu� quantunque, o Abandonner per cancell� l'installazione.

; *** Installation status messages
StatusClosingApplications=Chjudendu l'applicazioni�
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
FileAbortRetryIgnore=Sceglite R�essayer per pruv� torna, Ignorer per salt� stu schedariu (scunsigliatu), o Abandonner per cancell� l'installazione.
FileAbortRetryIgnore2=Sceglite R�essayer per pruv� torna, Ignorer per cuntinu� quantunque (scunsigliatu), o Abandonner per cancell� l'installazione.
SourceIsCorrupted=U schedariu surghjente h� alteratu
SourceDoesntExist=U schedariu surghjente "%1" �n esista micca
ExistingFileReadOnly=U schedariu esistente h� unu attributu di lettura-sola.%n%nSceglite R�essayer per cacci� st'attributu � pruv� torna, Ignorer per salt� stu schedariu, o Abandonner per cancell� l'installazione.
ErrorReadingExistingDest=Un'errore h� affacatu durante a lettura di u schedariu esistente :
FileExists=U schedariu esiste dighj�.%n%nVulite ch� l'Assistente u rimpiazza ?
ExistingFileNewer=U schedariu esistente h� pi� ricente ch� quellu ch� l'Assistente prova d'install�. H� ricummendatu di cunserv� u schedariu esistente.%n%nVulite cunserv� u schedariu esistente ?
ErrorChangingAttr=Un'errore h� affacatu pruvendu di cambi� l'attributi of u schedariu esistente :
ErrorCreatingTemp=Un'errore h� affacatu pruvendu di cre� unu schedariu in u cartelaru d'installazione :
ErrorReadingSource=Un'errore h� affacatu pruvendu di leghje u schedariu surghjente :
ErrorCopying=Un'errore h� affacatu pruvendu di cupi� unu schedariu :
ErrorReplacingExistingFile=Un'errore h� affacatu pruvendu di rimpiazz� u schedariu esistente :
ErrorRestartReplace=Fiascu di Ridimarr�/Rimpiazz� :
ErrorRenamingTemp=Un'errore h� affacatu pruvendu di cambi� u nome di unu schedariu in u cartelaru d'installazione :
ErrorRegisterServer=Impussibule d'arregistr� a bibliuteca DLL/OCX : %1
ErrorRegSvr32Failed=Fiascu di RegSvr32 c� u codice di uscita %1
ErrorRegisterTypeLib=Impussibule d'arregistr� a bibliuteca di tipu : %1

; *** Post-installation errors
ErrorOpeningReadme=Un'errore h� affacatu pruvendu di leghje u schedariu LISEZMOI/README.
ErrorRestartingComputer=L'Assistente �n p� micca ridimarr� l'urdinatore. Ci vole � fallu manualmente.

; *** Uninstaller messages
UninstallNotFound=U schedariu "%1" �n esista micca. Impussibule di disinstall�.
UninstallOpenError=U schedariu "%1" �n p� micca esse apertu. Impussibule di disinstall�
UninstallUnsupportedVer=U schedariu ghjurnale 'log' "%1" si trova in una forma micca ricunnisciuta da sta versione di l'Assistente di disinstallazione. Impussibule di disinstall�
UninstallUnknownEntry=Una infurmazione scunisciuta (%1) si trova in u schedariu ghjurnale 'log' di disinstallazione
ConfirmUninstall=Site sicuru di vul� scacci� tutalmente %1 � tutti i so cumpunenti ?
UninstallOnlyOnWin64=A disinstallazione di stu prugrammu si p� f� solu c� una versione  64-bit di Windows.
OnlyAdminCanUninstall=A disinstallazione di stu prugrammu si p� f� solu da unu utilisatore di u gruppu di l'Amministratori.
UninstallStatusLabel=Aspittate per piac� fin'� ch� %1 h� scacciatu da u vostru urdinatore.
UninstalledAll=%1 h� statu scacciatu b� da u vostru urdinatore.
UninstalledMost=A disinstallazione di %1 h� compia.%n%nQualch� elementu �n p� micca esse scacciatu. Ci vole � scacciallu manualmente.
UninstalledAndNeedsRestart=Per cumpie a disinstallazione di %1, ci vole � dimarr� torna u vostru urdinatore.%n%nSite d'accunsentu per fallu av� ?
UninstallDataCorrupted=U schedariu "%1" h� alteratu. Impussibule di disinstall�

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Scacci� i schedarii cundivisi ?
ConfirmDeleteSharedFile2=U sistemu indica ch� i schedarii cundivisi �n s� pi� aduprati d'alcunu programmu. Vulite scacci� i schedarii cundivisi ?%n%nS'� qualch� programmu sempre adupra sti schedarii � quessi s� scacciati, �n puder� funziun� currettamente. Se �n site micca sicuru, sceglite No. Lasci� sti schedarii nantu � u sistemu �n p� micca pruduce danni.
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
CreateDesktopIcon=Cre� una icona nantu � u &Scagnu (Bureau)
CreateQuickLaunchIcon=Cre� una icona nantu � a barra di &Lanciu rapidu
ProgramOnTheWeb=%1 nantu � u Web
UninstallProgram=Desinstall� %1
LaunchProgram=Dimarr� %1
AssocFileExtension=&Assuci� %1 c� l'estensione di u schedariu %2
AssocingFileExtension=Assuciazion di %1 c� l'estensione di u schedariu %2...
AutoStartProgramGroupDescription=Relancia autumatica :
AutoStartProgram=Relanci� automaticamente %1
AddonHostProgramNotFound=Impussibule di truv� %1 in u cartulare selezziunatu.%n%nVulete cuntinu� l'installazione quantunque ?
