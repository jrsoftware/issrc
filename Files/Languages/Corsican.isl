; *** Inno Setup version 6.0.0+ Corsican messages ***
;
; To download user-contributed translations of this file, go to:
;   http://www.jrsoftware.org/files/istrans/
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).

; Created and maintained by Patriccollu di Santa Maria � Sich�
;
; E-mail: Patrick.Santa-Maria[at]LaPoste.Net
;
; Changes:
; January 20, 2019 - Update to version 6.0.0+
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
SetupAppTitle=Assistente d�installazione
SetupWindowTitle=Assistente d�installazione - %1
UninstallAppTitle=Disinstall�
UninstallAppFullTitle=Disinstallazione di %1

; *** Misc. common
InformationTitle=Infurmazione
ConfirmTitle=Cunfirm�
ErrorTitle=Sbagliu

; *** SetupLdr messages
SetupLdrStartupMessage=St�assistente h� da install� %1. Vulete cuntinu� ?
LdrCannotCreateTemp=Impussibule di cre� un cartulare timpurariu. Assistente d�installazione interrottu
LdrCannotExecTemp=Impussibule d�eseguisce u schedariu in u cartulare timpurariu. Assistente d�installazione interrottu
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nSbagliu %2 : %3
SetupFileMissing=U schedariu %1 manca in u cartulare d�installazione. Ci vole � currege u penseru o ottene una nova copia di u prugramma.
SetupFileCorrupt=I schedarii d�installazione s� alterati. Ci vole � ottene una nova copia di u prugramma.
SetupFileCorruptOrWrongVer=I schedarii d�installazione s� alterati, o s� incumpatibule c� sta versione di l�assistente. Ci vole � currege u penseru o ottene una nova copia di u prugramma.
InvalidParameter=Un parametru micca accettevule h� statu passatu in a linea di cumanda :%n%n%1
SetupAlreadyRunning=L�assistente d�installazione h� dighj� in corsu.
WindowsVersionNotSupported=Stu prugramma �n p� micca funziun� c� a versione di Windows installata nant�� st�ordinatore.
WindowsServicePackRequired=Stu prugramma richiede %1 Service Pack %2 o pi� recente.
NotOnThisPlatform=Stu prugramma �n funzioner� micca c� %1.
OnlyOnThisPlatform=Stu prugramma deve funzion� c� %1.
OnlyOnTheseArchitectures=Stu prugramma p� solu esse installatu nant�� e versioni di Windows fatte apposta per st�architetture di prucessore :%n%n%1
WinVersionTooLowError=Stu prugramma richiede %1 versione %2 o pi� recente.
WinVersionTooHighError=Stu prugramma �n p� micca esse installatu nant�� %1 version %2 o pi� recente.
AdminPrivilegesRequired=Ci vole � esse cunnettu cum�� un amministratore quandu voi installate stu prugramma.
PowerUserPrivilegesRequired=Ci vole � esse cunnettu cum�� un amministratore o f� parte di u gruppu � Utilizatori c� putere � quandu voi installate stu prugramma.
SetupAppRunningError=L�assistente h� vistu ch� %1 era dighj� in corsu.%n%nCi vole � chjode tutte e so finestre av�, po sceglie Vai per cuntinu�, o Abbandun� per compie.
UninstallAppRunningError=A disinstallazione h� vistu ch� %1 era dighj� in corsu.%n%nCi vole � chjode tutte e so finestre av�, po sceglie Vai per cuntinu�, o Abbandun� per compie.

; *** Startup questions
PrivilegesRequiredOverrideTitle=Selezziun� u modu d�installazione di l�assistente
PrivilegesRequiredOverrideInstruction=Selezziun� u modu d�installazione
PrivilegesRequiredOverrideText1=%1 p� esse installatu per tutti l�utilizatore (richiede i diritti d�amministratore), o solu per voi.
PrivilegesRequiredOverrideText2=%1 p� esse installatu solu per voi, o per tutti l�utilizatore (richiede i diritti d�amministratore).
PrivilegesRequiredOverrideAllUsers=Installazione per &tutti l�utilizatori
PrivilegesRequiredOverrideAllUsersRecommended=Installazione per &tutti l�utilizatori (ricumandatu)
PrivilegesRequiredOverrideCurrentUser=Installazione solu per &m�
PrivilegesRequiredOverrideCurrentUserRecommended=Installazione solu per &m� (ricumandatu)

; *** Misc. errors
ErrorCreatingDir=L�assistente �n h� micca pussutu cre� u cartulare � %1 �
ErrorTooManyFilesInDir=Impussibule di cre� un schedariu in u cartulare � %1 � perch� ellu ne cuntene troppu

; *** Setup common messages
ExitSetupTitle=Compie l�assistente
ExitSetupMessage=L�assistente �n h� micca compiu b�. S�� voi escite av�, u prugramma �n ser� micca installatu.%n%nPudete impieg� l�assistente torna un altra volta per compie l�installazione.%n%nCompie l�assistente ?
AboutSetupMenuItem=&Apprupositu di l�assistente�
AboutSetupTitle=Apprupositu di l�assistente
AboutSetupMessage=%1 versione %2%n%3%n%n%1 pagina d�accolta :%n%4
AboutSetupNote=
TranslatorNote=Traduzzione corsa da Patriccollu di Santa Maria � Sich�

; *** Buttons
ButtonBack=< &Precedente
ButtonNext=&Seguente >
ButtonInstall=&Install�
ButtonOK=Vai
ButtonCancel=Abbandun�
ButtonYes=&I�
ButtonYesToAll=I� per &tutti
ButtonNo=I&nn�
ButtonNoToAll=Inn� per t&utti
ButtonFinish=&Piant�
ButtonBrowse=&Sfugli��
ButtonWizardBrowse=&Sfugli��
ButtonNewFolder=&Cre� un novu cartulare

; *** "Select Language" dialog messages
SelectLanguageTitle=Definisce a lingua di l�assistente
SelectLanguageLabel=Selezziun� a lingua � impieg� per l�installazione.

; *** Common wizard text
ClickNext=Sceglie Seguente per cuntinu�, o Abbandun� per compie l�assistente.
BeveledLabel=
BrowseDialogTitle=Sfugli� u cartulare
BrowseDialogLabel=Selezziun� un cartulare in a lista inghj�, po sceglie Vai.
NewFolderName=Novu cartulare

; *** "Welcome" wizard page
WelcomeLabel1=Benvenuta in l�assistente d�installazione di [name]
WelcomeLabel2=Quessu installer� [name/ver] nant�� l�ordinatore.%n%nH� ricumandatu di chjode tutte l�altre appiecazioni nanzu di cuntinu�.

; *** "Password" wizard page
WizardPassword=Parolla d�entrata
PasswordLabel1=L�installazione h� prutetta da una parolla d�entrata.
PasswordLabel3=Ci vole � pruvede a parolla d�entrata, po sceglie Seguente per cuntinu�. Sfarenzi� maiuscule � minuscule in e parolle d�entrata.
PasswordEditLabel=&Parolla d�entrata :
IncorrectPassword=A parolla d�entrata pruvista �n h� micca curretta. Ci vole � pruv� torna.

; *** "License Agreement" wizard page
WizardLicense=Cuntrattu di licenza
LicenseLabel=Ci vole � leghje l�infurmazione impurtante ch� seguiteghja nanzu di cuntinu�.
LicenseLabel3=Ci vole � leghje u cuntrattu di licenza ch� seguiteghja. Duvete accett� i termini di stu cuntrattu nanzu di cuntinu� l�installazione.
LicenseAccepted=S� d�&accunsentu c� u cuntrattu
LicenseNotAccepted=�n s� &micca d�accunsentu c� u cuntrattu

; *** "Information" wizard pages
WizardInfoBefore=Infurmazione
InfoBeforeLabel=Ci vole � leghje l�infurmazione impurtante ch� seguiteghja nanzu di cuntinu�.
InfoBeforeClickLabel=Quandu site prontu � cuntinu� c� l�assistente, sciglite Seguente.
WizardInfoAfter=Infurmazione
InfoAfterLabel=Ci vole � leghje l�infurmazione impurtante ch� seguiteghja nanzu di cuntinu�.
InfoAfterClickLabel=Quandu site prontu � cuntinu� c� l�assistente, sciglite Seguente.

; *** "User Information" wizard page
WizardUserInfo=Infurmazioni di l�utilizatore
UserInfoDesc=Ci vole � scrive e vostre infurmazioni.
UserInfoName=&Nome d�utilizatore :
UserInfoOrg=&Urganismu :
UserInfoSerial=&Numeru di Seria :
UserInfoNameRequired=Ci vole � scrive un nome.

; *** "Select Destination Location" wizard page
WizardSelectDir=Selezziun� u locu di destinazione
SelectDirDesc=Induve [name] deve esse installatu ?
SelectDirLabel3=L�assistente installer� [name] in stu cartulare.
SelectDirBrowseLabel=Per cuntinu�, sceglie Seguente. S�� voi preferisce selezziun� un altru cartulare, sciglite Sfugli�.
DiskSpaceMBLabel=H� richiestu almenu [mb] Mo di spaziu liberu di discu.
CannotInstallToNetworkDrive=L�assistente �n p� micca install� nant�� un discu di a reta.
CannotInstallToUNCPath=L�assistente �n p� micca install� in un passeghju UNC.
InvalidPath=Ci vole � scrive un passeghju cumplettu c� a lettera di u lettore ; per indettu :%n%nC:\APP%n%no un passeghju UNC in a forma :%n%n\\servu\spartu
InvalidDrive=U lettore o u passeghju UNC spartu �n esiste micca o �n h� micca accessibule. Ci vole � selezziun� un altru.
DiskSpaceWarningTitle=�n basta u spaziu discu
DiskSpaceWarning=L�assistente richiede almenu %1 Ko di spaziu liberu per install�, ma u lettore selezziunatu h� solu %2 Ko dispunibule.%n%nVulete cuntinu� quantunque ?
DirNameTooLong=U nome di cartulare o u passeghju h� troppu longu.
InvalidDirName=U nome di cartulare �n h� micca accettevule.
BadDirName32=I nomi di cartulare �n ponu micca cuntene sti caratteri :%n%n%1
DirExistsTitle=Cartulare esistente
DirExists=U cartulare :%n%n%1%n%nesiste dighj�. Vulete install� in stu cartulare quantunque ?
DirDoesntExistTitle=Cartulare inesistente
DirDoesntExist=U cartulare :%n%n%1%n%n�n esiste micca. Vulete ch� stu cartulare sia creatu ?

; *** "Select Components" wizard page
WizardSelectComponents=Selezzione di cumpunenti
SelectComponentsDesc=Ch� cumpunenti devenu esse installati ?
SelectComponentsLabel2=Selezziun� i cumpunenti � install� ; deselezziun� quelli ch��n devenu micca esse installati. Sceglie Seguente quandu site prontu � cuntinu�.
FullInstallation=Installazione sana
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Installazione cumpatta
CustomInstallation=Installazione persunalizata
NoUninstallWarningTitle=Cumpunenti esistenti
NoUninstallWarning=L�assistente h� vistu ch� sti cumpunenti s� dighj� installati nant�� l�ordinatore :%n%n%1%n%nDeselezziun� sti cumpunenti �n i disinstaller� micca.%n%nVulete cuntinu� quantunque ?
ComponentSize1=%1 Ko
ComponentSize2=%1 Mo
ComponentsDiskSpaceMBLabel=A selezzione attuale richiede almenu [mb] Mo di spaziu liberu nant�� u discu.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Selezziun� trattamenti addizziunali
SelectTasksDesc=Ch� trattamenti addizziunali vulete f� ?
SelectTasksLabel2=Selezziun� i trattamenti addizziunali ch� l�assistente deve f� durante l�installazione di [name], po sceglie Seguente.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Selezzione di u cartulare di u � Menu D�marrer �
SelectStartMenuFolderDesc=Induve l�assistente deve piazz� l�accurtatoghji di u prugramma ?
SelectStartMenuFolderLabel3=L�assistente piazzer� l�accurtatoghji di u prugramma in stu cartulare di u � Menu D�marrer �.
SelectStartMenuFolderBrowseLabel=Per cuntinu�, sceglie Seguente. S�� voi preferisce selezziun� un altru cartulare, sciglite Sfugli�.
MustEnterGroupName=Ci vole � scrive un nome di cartulare.
GroupNameTooLong=U nome di cartulare o u passeghju h� troppu longu.
InvalidGroupName=U nome di cartulare �n h� micca accettevule.
BadGroupName=U nome di u cartulare �n p� micca cuntene alcunu di sti caratteri :%n%n%1
NoProgramGroupCheck2=�n cre� &micca di cartulare in u � Menu D�marrer �

; *** "Ready to Install" wizard page
WizardReady=Prontu � Install�
ReadyLabel1=Av� l�assistente h� prontu � principi� l�installazione di [name] nant�� l�ordinatore.
ReadyLabel2a=Sceglie Install� per cuntinu� l�installazione, o nant�� Precedente per rivede o cambi� qualch� preferenza.
ReadyLabel2b=Sceglie Install� per cuntinu� l�installazione.
ReadyMemoUserInfo=Infurmazioni di l�utilizatore :
ReadyMemoDir=Cartulare d�installazione :
ReadyMemoType=Tipu d�installazione :
ReadyMemoComponents=Cumpunenti selezziunati :
ReadyMemoGroup=Cartulare di u � Menu D�marrer � :
ReadyMemoTasks=Trattamenti addizziunali :

; *** "Preparing to Install" wizard page
WizardPreparing=Preparazione di l�installazione
PreparingDesc=L�assistente appronta l�installazione di [name] nant�� l�ordinatore.
PreviousInstallNotCompleted=L�installazione/cacciatura di un prugramma precedente �n h� micca compia b�. Ci vuler� � spenghje l�ordinatore � ridimarrallu per compie st�installazione.%n%nDopu, ci vuler� � rilanci� l�assistente per compie l�installazione di [name].
CannotContinue=L�assistente �n p� micca cuntinu�. Sceglie Abbandun� per esce.
ApplicationsFound=St�appiecazioni impieganu schedarii ch� devenu esse mudificati da l�assistente. H� ricumandatu di permette � l�assistente di chjode autumaticamente st�appiecazioni.
ApplicationsFound2=St�appiecazioni impieganu schedarii ch� devenu esse mudificati da l�assistente. H� ricumandatu di permette � l�assistente di chjode autumaticamente st�appiecazioni. S�� l�installazione si compie b�, l�assistente pruver� di rilanci� l�appiecazioni.
CloseApplications=Chjode &autumaticamente l�appiecazioni
DontCloseApplications=�n chjode &micca l�appiecazioni
ErrorCloseApplications=L�assistente �n h� micca pussutu chjode autumaticamente tutti l�appiecazioni. Nanzu di cuntinu�, h� ricumandatu di chjode tutti l�appiecazioni ch� impieganu schedarii ch� devenu esse mudificati da l�assistente durante l�installazione.

; *** "Installing" wizard page
WizardInstalling=Installazione in corsu
InstallingLabel=Ci vole � aspett� durante l�installazione di [name] nant�� l�ordinatore.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Fine di l�installazione di [name]
FinishedLabelNoIcons=L�installazione di [name] nant�� l�ordinatore h� compia.
FinishedLabel=L�installazione di [name] nant�� l�ordinatore h� compia. L�appiecazione p� esse lamciata selezziunendu l�accurtatoghji installati.
ClickFinish=Sceglie Piant� per compie l�assistente.
FinishedRestartLabel=Per compie l�installazione di [name], l�assistente deve spenghje l�ordinatore � ridimarrallu. Vulete spenghje l�ordinatore � ridimarrallu av� ?
FinishedRestartMessage=Per compie l�installazione di [name], l�assistente deve spenghje l�ordinatore � ridimarrallu.%n%nVulete spenghje l�ordinatore � ridimarrallu av� ?
ShowReadmeCheck=I�, vogliu leghje u schedariu LISEZMOI o README
YesRadio=&I�, spenghje l�ordinatore � ridimarrallu av�
NoRadio=I&nn�, preferiscu spenghje l�ordinatore � ridimarrallu dopu
; used for example as 'Run MyProg.exe'
RunEntryExec=Eseguisce %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Fighj� %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=L�assistente h� bisogniu di u discu seguente
SelectDiskLabel2=Mette u discu %1 � sceglie Vai.%n%nS�� i schedarii di stu discu si trovanu in un�altru cartulare ch� quellu indicatu inghj�, scrive u passeghju currettu o sceglie Sfugli�.
PathLabel=&Passeghju :
FileNotInDir2=U schedariu � %1 � �n si truva micca in � %2 �. Mette u discu curretu o sceglie un�altru cartulare.
SelectDirectoryLabel=Ci vole � specific� induve si trova u discu seguente.

; *** Installation phase messages
SetupAborted=L�installazione �n h� micca compia b�.%n%nCi vole � currege u penseru � eseguisce l�assistente torna.
AbortRetryIgnoreSelectAction=Selezziunate un�azzione
AbortRetryIgnoreRetry=&Pruv� torna
AbortRetryIgnoreIgnore=&Ignur� u sbagliu � cuntinu�
AbortRetryIgnoreCancel=Abbandun� l�installazione

; *** Installation status messages
StatusClosingApplications=Chjusura di l�appiecazioni�
StatusCreateDirs=Creazione di i cartulari�
StatusExtractFiles=Estrazzione di i schedarii�
StatusCreateIcons=Creazione di l�accurtatoghji�
StatusCreateIniEntries=Creazione di l�elementi INI�
StatusCreateRegistryEntries=Creazione di l�elementi di u registru�
StatusRegisterFiles=Arregistramentu di i schedarii�
StatusSavingUninstall=Cunservazione di l�informazioni di disinstallazione�
StatusRunProgram=Cumpiera di l�installazione�
StatusRestartingApplications=Relanciu di l�appiecazioni�
StatusRollback=Annulazione di i mudificazioni�

; *** Misc. errors
ErrorInternal2=Sbagliu internu : %1
ErrorFunctionFailedNoCode=Fiascu di %1
ErrorFunctionFailed=Fiascu di %1 ; codice %2
ErrorFunctionFailedWithMessage=Fiascu di %1 ; codice %2.%n%3
ErrorExecutingProgram=Impussibule d�eseguisce u schedariu :%n%1

; *** Registry errors
ErrorRegOpenKey=Sbagliu durante l�apertura di a chjave di registru :%n%1\%2
ErrorRegCreateKey=Sbagliu durante a creazione di a chjave di registru :%n%1\%2
ErrorRegWriteKey=Sbagliu durante a scrittura di a chjave di registru :%n%1\%2

; *** INI errors
ErrorIniEntry=Sbagliu durante a creazione di l�elementu INI in u schedariu � %1 �.

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=Ignur� stu &schedariu (micca ricumandatu)
FileAbortRetryIgnoreIgnoreNotRecommended=&Ignur� u sbagliu � cuntinu� (micca ricumandatu)
SourceIsCorrupted=U schedariu d�urigine h� alteratu
SourceDoesntExist=U schedariu d�urigine � %1 � �n esiste micca
ExistingFileReadOnly2=U schedariu esistente h� un attributu di lettura-sola � �n p� micca esse rimpiazzatu.
ExistingFileReadOnlyRetry=&Cacci� l�attributu di lettura-sola � pruv� torna
ExistingFileReadOnlyKeepExisting=Cunserv� u schedariu &esistente
ErrorReadingExistingDest=Un sbagliu h� accadutu pruvendu di leghje u schedariu esistente :
FileExists=U schedariu esiste dighj�.%n%nVulete ch� l�assistente u rimpiazzi ?
ExistingFileNewer=U schedariu esistente h� pi� recente ch� quellu ch� l�assistente prova d�install�. H� ricumandatu di cunserv� u schedariu esistente.%n%nVulete cunserv� u schedariu esistente ?
ErrorChangingAttr=Un sbagliu h� accadutu pruvendu di cambi� l�attributi di u schedariu esistente :
ErrorCreatingTemp=Un sbagliu h� accadutu pruvendu di cre� un schedariu in u cartulare di destinazione :
ErrorReadingSource=Un sbagliu h� accadutu pruvendu di leghje u schedariu d�urigine :
ErrorCopying=Un sbagliu h� accadutu pruvendu di cupi� un schedariu :
ErrorReplacingExistingFile=Un sbagliu h� accadutu pruvendu di rimpiazz� u schedariu esistente :
ErrorRestartReplace=Fiascu di Rimpiazzamentu di schedariu � u riavviu di l�ordinatore :
ErrorRenamingTemp=Un sbagliu h� accadutu pruvendu di rinum� un schedariu in u cartulare di destinazione :
ErrorRegisterServer=Impussibule d�arregistr� a bibliuteca DLL/OCX : %1
ErrorRegSvr32Failed=Fiascu di RegSvr32 c� codice d�esciuta %1
ErrorRegisterTypeLib=Impussibule d�arregistr� a bibliuteca di tipu : %1

; *** Uninstall display name markings
UninstallDisplayNameMark=%1 (%2)
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32-bit
UninstallDisplayNameMark64Bit=64-bit
UninstallDisplayNameMarkAllUsers=Tutti l�utilizatori
UninstallDisplayNameMarkCurrentUser=L�utilizatore attuale

; *** Post-installation errors
ErrorOpeningReadme=Un sbagliu h� accadutu pruvendu d�apre u schedariu LISEZMOI o README.
ErrorRestartingComputer=L�assistente �n h� micca pussutu ridimarr� l�ordinatore. Ci vole � fallu manualmente.

; *** Uninstaller messages
UninstallNotFound=U schedariu � %1 � �n esiste micca. Impussibule di disinstall�.
UninstallOpenError=U schedariu� %1 � �n p� micca esse apertu. Impussibule di disinstall�
UninstallUnsupportedVer=U ghjurnale di disinstallazione � %1 � h� in una forma scunnisciuta da sta versione di l�assistente di disinstallazione. Impussibule di disinstall�
UninstallUnknownEntry=Un elementu scunisciutu (%1) h� statu trovu in u ghjurnale di disinstallazione
ConfirmUninstall=Site sicuru di vul� cacci� cumpletamente %1 � tutti i so cumpunenti ?
UninstallOnlyOnWin64=St�appiecazione p� esse disinstallata solu c� una versione 64-bit di Windows.
OnlyAdminCanUninstall=St�appiecazione p� esse disinstallata solu da un utilizatore di u gruppu d�amministratori.
UninstallStatusLabel=Ci vole � aspett� ch� %1 sia cacciatu di l�ordinatore.
UninstalledAll=%1 h� statu cacciatu b� da l�ordinatore.
UninstalledMost=A disinstallazione di %1 h� compia.%n%nQualch� elementu �n p� micca esse cacciatu. Ci vole � cacciallu manualmente.
UninstalledAndNeedsRestart=Per compie a disinstallazione di %1, l�ordinatore deve esse spentu � ridimarratu.%n%nVulete spenghje l�ordinatore � ridimarrallu av� ?
UninstallDataCorrupted=U schedariu � %1 � h� alteratu. Impussibule di disinstall�

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Cacci� i schedarii sparti ?
ConfirmDeleteSharedFile2=U sistema indica ch� u schedariu spartu �n h� pi� impiegatu da nisunu prugramma. Vulete ch� a disinstallazione cacci stu schedariu spartu ?%n%nS�� qualch� prugramma impiega sempre stu schedariu � ch�ellu h� cacciatu, quellu prugramma �n puder� funziun� currettamente. S�� �n site micca sicuru, sceglie Inn�. Lasci� stu schedariu nant�� u sistema �n p� micca pruduce danni.
SharedFileNameLabel=Nome di schedariu :
SharedFileLocationLabel=Lucalizazione :
WizardUninstalling=Statu di disinstallazione
StatusUninstalling=Disinstallazione di %1�

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Installazione di %1.
ShutdownBlockReasonUninstallingApp=Disinstallazione di %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 versione %2
AdditionalIcons=Accurtatoghji addizziunali :
CreateDesktopIcon=Cre� un accurtatoghju nant�� u &scagnu
CreateQuickLaunchIcon=Cre� un accurtatoghju nant�� a barra di &lanciu prontu
ProgramOnTheWeb=%1 nant�� u Web
UninstallProgram=Disinstall� %1
LaunchProgram=Lanci� %1
AssocFileExtension=&Assuci� %1 c� l�estensione di schedariu %2
AssocingFileExtension=Associu di %1 c� l�estensione di schedariu %2�
AutoStartProgramGroupDescription=Lanciu autumaticu :
AutoStartProgram=Lanciu autumaticu di %1
AddonHostProgramNotFound=Impussibule di truv� %1 in u cartulare selezziunatu.%n%nVulete cuntinu� l�installazione quantunque ?
