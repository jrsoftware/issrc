; *** Inno Setup version 5.5.3+ Romanian messages ***
; Translator : Alexandru Bogdan Munteanu (muntealb@gmail.com)
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
LanguageName=Rom<00E2>n<0103>
LanguageID=$0418
LanguageCodePage=0
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
SetupAppTitle=Instalare
SetupWindowTitle=Instalare - %1
UninstallAppTitle=Dezinstalare
UninstallAppFullTitle=Dezinstalare %1

; *** Misc. common
InformationTitle=Informații
ConfirmTitle=Confirmare
ErrorTitle=Eroare

; *** SetupLdr messages
SetupLdrStartupMessage=Va fi instalat programul %1. Vrei să continui?
LdrCannotCreateTemp=Nu pot crea o filă temporară. Instalare abandonată
LdrCannotExecTemp=Nu pot executa o filă din dosarul temporar. Instalare abandonată

; *** Startup error messages
LastErrorMessage=%1.%n%nEroarea %2: %3
SetupFileMissing=Fila %1 lipsește din dosarul de instalare. Corectează problema sau folosește o altă copie a programului.
SetupFileCorrupt=Filele de instalare sunt stricate (corupte). Folosește o altă copie a programului.
SetupFileCorruptOrWrongVer=Filele de instalare sunt stricate (corupte) sau sunt incompatibile cu această versiune a Instalatorului. Remediază problema sau folosește o altă copie a programului.
InvalidParameter=Un parametru invalid a fost trecut către linia de comandă:%n%n%1
SetupAlreadyRunning=Instalarea rulează deja.
WindowsVersionNotSupported=Acest program nu suportă versiunea de Windows care rulează pe calculatorul tău.
WindowsServicePackRequired=Acest program necesită %1 Service Pack %2 sau mai nou.
NotOnThisPlatform=Acest program nu va rula pe %1.
OnlyOnThisPlatform=Acest program trebuie să ruleze pe %1.
OnlyOnTheseArchitectures=Acest program poate fi instalat doar pe versiuni de Windows proiectate pentru următoarele arhitecturi de procesor:%n%n%1
MissingWOW64APIs=Versiunea de Windows pe care o rulezi nu include funcționalitatea cerută de Instalator pentru a realiza o instalare pe 64-biți. Pentru a corecta problema, va trebui să instalezi Service Pack %1.
WinVersionTooLowError=Acest program necesită %1 versiunea %2 sau mai nouă.
WinVersionTooHighError=Acest program nu poate fi instalat pe %1 versiunea %2 sau mai nouă.
AdminPrivilegesRequired=Trebuie să fii logat ca Administrator pentru instalarea acestui program.
PowerUserPrivilegesRequired=Trebuie să fii logat ca Administrator sau ca Membru al Grupului de Utilizatori Pricepuți ("Power Users") pentru a instala acest program.
SetupAppRunningError=Aplicația a detectat că %1 rulează în acest moment.%n%nÎnchide toate instanțele programului respectiv, apoi apasă OK pentru a continua sau Anulează pentru a abandona instalarea.
UninstallAppRunningError=Dezinstalatorul a detectat că %1 rulează în acest moment.%n%nÎnchide toate instanțele programului respectiv, apoi apasă OK pentru a continua sau Anulează pentru a abandona dezinstalarea.

; *** Misc. errors
ErrorCreatingDir=Aplicația nu a putut crea dosarul "%1"
ErrorTooManyFilesInDir=Nu pot crea o filă în dosarul "%1" din cauză că are deja prea multe file

; *** Setup common messages
ExitSetupTitle=Abandonarea instalării
ExitSetupMessage=Instalarea nu este terminată. Dacă o abandonezi acum, programul nu va fi instalat.%n%nPoți să rulezi aplicația din nou altă dată pentru a termina instalarea.%n%nAbandonezi instalarea?
AboutSetupMenuItem=&Despre Instalator...
AboutSetupTitle=Despre Instalator
AboutSetupMessage=%1 versiunea %2%n%3%n%n%1 sit:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< Îna&poi
ButtonNext=&Continuă >
ButtonInstall=&Instalează
ButtonOK=OK
ButtonCancel=Anulează
ButtonYes=&Da
ButtonYesToAll=Da la &Tot
ButtonNo=&Nu
ButtonNoToAll=N&u la Tot
ButtonFinish=&Finalizează
ButtonBrowse=&Explorează...
ButtonWizardBrowse=Explo&rează...
ButtonNewFolder=Creea&ză Dosar Nou

; *** "Select Language" dialog messages
SelectLanguageTitle=Selectarea Limbii Instalatorului
SelectLanguageLabel=Selectează limba folosită pentru instalare:

; *** Common wizard text
ClickNext=Apasă pe Continuă pentru a avansa cu instalarea sau pe Anulează pentru a o abandona.
BeveledLabel=
BrowseDialogTitle=Explorare după dosar
BrowseDialogLabel=Selectează un dosar din lista de mai jos, apoi apasă pe OK.
NewFolderName=Dosar Nou

; *** "Welcome" wizard page
WelcomeLabel1=Bun venit la Instalarea [name]
WelcomeLabel2=Programul [name/ver] va fi instalat pe calculator.%n%nEste recomandat să închizi toate celelalte aplicații înainte de a continua.

; *** "Password" wizard page
WizardPassword=Parolă
PasswordLabel1=Această instalare este protejată prin parolă.
PasswordLabel3=Completează parola, apoi apasă pe Continuă pentru a merge mai departe. Tipul literelor din parolă (Majuscule/minuscule) este luat în considerare.
PasswordEditLabel=&Parolă:
IncorrectPassword=Parola pe care ai introdus-o nu este corectă. Reîncearcă.

; *** "License Agreement" wizard page
WizardLicense=Acord de licențiere
LicenseLabel=Citește informațiile următoare înainte de a continua, sunt importante.
LicenseLabel3=Citește următorul Acord de licențiere. Trebuie să accepți termenii acestui acord înainte de a continua instalarea.
LicenseAccepted=&Accept licența
LicenseNotAccepted=&Nu accept licența

; *** "Information" wizard pages
WizardInfoBefore=Informații
InfoBeforeLabel=Citește informațiile următoare înainte de a continua, sunt importante.
InfoBeforeClickLabel=Cînd ești gata de a trece la Instalare, apasă pe Continuă.
WizardInfoAfter=Informații
InfoAfterLabel=Citește informațiile următoare înainte de a continua, sunt importante.
InfoAfterClickLabel=Cînd ești gata de a trece la Instalare, apasă pe Continuă.

; *** "User Information" wizard page
WizardUserInfo=Informații despre Utilizator
UserInfoDesc=Completează informațiile cerute.
UserInfoName=&Utilizator:
UserInfoOrg=&Organizație:
UserInfoSerial=Număr de &Serie:
UserInfoNameRequired=Trebuie să introduci un nume.

; *** "Select Destination Location" wizard page
WizardSelectDir=Selectarea locului de destinație
SelectDirDesc=Unde vrei să instalezi [name]?
SelectDirLabel3=Aplicația va pune [name] în dosarul specificat mai jos.
SelectDirBrowseLabel=Pentru a avansa cu instalarea, apasă pe Continuă. Dacă vrei să selectezi un alt dosar, apasă pe Explorează.
DiskSpaceMBLabel=Este necesar un spațiu liber de stocare de cel puțin [mb] MB.
CannotInstallToNetworkDrive=Aplicația nu poate realiza instalarea pe un dispozitiv de rețea.
CannotInstallToUNCPath=Aplicația nu poate realiza instalarea pe o cale în format UNC.
InvalidPath=Trebuie să introduci o cale completă, inclusiv litera dispozitivului; de exemplu:%n%nC:\APP%n%nsau o cale UNC de forma:%n%n\\server\share
InvalidDrive=Dispozitivul sau partajul UNC pe care l-ai selectat nu există sau nu este accesibil. Selectează altul.
DiskSpaceWarningTitle=Spațiu de Stocare Insuficient
DiskSpaceWarning=Instalarea necesită cel puțin %1 KB de spațiu de stocare liber, dar dispozitivul selectat are doar %2 KB liberi.%n%nVrei să continui oricum?
DirNameTooLong=Numele dosarului sau al căii este prea lung.
InvalidDirName=Numele dosarului nu este valid.
BadDirName32=Numele dosarelor nu pot include unul din următoarele caractere:%n%n%1
DirExistsTitle=Dosarul Există
DirExists=Dosarul:%n%n%1%n%nexistă deja. Vrei totuși să instalezi în acel dosar?
DirDoesntExistTitle=Dosarul Nu Există
DirDoesntExist=Dosarul:%n%n%1%n%nnu există. Vrei ca el să fie creat?

; *** "Select Components" wizard page
WizardSelectComponents=Selectarea Componentelor
SelectComponentsDesc=Care dintre componente trebuie instalate?
SelectComponentsLabel2=Selectează componentele de instalat; deselectează componentele care nu trebuie instalate. Apasă pe Continuă pentru a merge mai departe.
FullInstallation=Instalare Completă
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Instalare Compactă
CustomInstallation=Instalare Personalizată
NoUninstallWarningTitle=Componentele Există
NoUninstallWarning=Aplicația a detectat că următoarele componente sunt deja instalate pe calculator:%n%n%1%n%nDeselectarea lor nu le va dezinstala.%n%nVrei să continui oricum?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Selecția curentă necesită cel puțin [mb] MB spațiu de stocare.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Selectarea sarcinilor suplimentare
SelectTasksDesc=Ce sarcini suplimentare trebuie îndeplinite?
SelectTasksLabel2=Selectează sarcinile suplimentare care trebuie îndeplinite în timpul instalării [name], apoi apasă pe Continuă.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Selectarea dosarului din meniul de Start
SelectStartMenuFolderDesc=Unde trebuie să fie plasate scurtăturile programului?
SelectStartMenuFolderLabel3=Scurtăturile vor fi plasate în dosarul specificat mai jos al meniului de Start.
SelectStartMenuFolderBrowseLabel=Pentru a avansa cu instalarea, apasă pe Continuă. Dacă vrei să selectezi un alt dosar, apasă pe Explorează.
MustEnterGroupName=Trebuie să introduci numele dosarului.
GroupNameTooLong=Numele dosarului sau al căii este prea lung.
InvalidGroupName=Numele dosarului nu este valid.
BadGroupName=Numele dosarului nu poate include unul dintre caracterele următoarele:%n%n%1
NoProgramGroupCheck2=Nu crea un &dosar în Meniul de Start

; *** "Ready to Install" wizard page
WizardReady=Pregătit de instalare
ReadyLabel1=Aplicația e pregătită pentru instalarea [name] pe calculator.
ReadyLabel2a=Apasă pe Instalează pentru a continua cu instalarea sau apasă pe Înapoi dacă vrei să revezi sau să schimbi setările.
ReadyLabel2b=Apasă pe Instalează pentru a continua cu instalarea.
ReadyMemoUserInfo=Info Utilizator:
ReadyMemoDir=Loc de destinație:
ReadyMemoType=Tip de Instalare:
ReadyMemoComponents=Componente Selectate:
ReadyMemoGroup=Dosarul din meniul de Start:
ReadyMemoTasks=Sarcini suplimentare:

; *** "Preparing to Install" wizard page
WizardPreparing=Pregătire pentru Instalare
PreparingDesc=Aplicația pregătește instalarea [name] pe calculator.
PreviousInstallNotCompleted=Instalarea/dezinstalarea anterioară a unui program nu a fost terminată. Va trebui să repornești calculatorul pentru a termina operația precedentă.%n%nDupă repornirea calculatorului, rulează Aplicația din nou pentru a realiza instalarea [name].
CannotContinue=Instalarea nu poate continua. Apasă pe Anulează pentru a o închide.
ApplicationsFound=Aplicațiile următoare folosesc file care trebuie actualizate de către Instalator. Este recomandat să permiți Instalatorului să închidă automat aplicațiile respective.
ApplicationsFound2=Aplicațiile următoare folosesc file care trebuie actualizate de către Instalator. Este recomandat să permiți Instalatorului să închidă automat aplicațiile respective. După ce instalarea e terminată, Aplicația va încerca să repornească aplicațiile.
CloseApplications=Închide &automat aplicațiile
DontCloseApplications=Nu închi&de aplicațiile
ErrorCloseApplications=Aplicația nu a putut închide automat toate aplicațiile. Înainte de a continua, e recomandat să închizi manual toate aplicațiile care folosesc file ce trebuie actualizate de Instalator.

; *** "Installing" wizard page
WizardInstalling=Instalare în desfășurare
InstallingLabel=Așteaptă să se termine instalarea [name] pe calculator.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Finalizarea instalării [name]
FinishedLabelNoIcons=Instalarea [name] pe calculator a fost terminată.
FinishedLabel=Instalarea [name] pe calculator a fost terminată. Aplicația poate fi lansată prin apăsarea pe icoanele instalate.
ClickFinish=Apasă pe Finalizează pentru a părăsi aplicația.
FinishedRestartLabel=Pentru a termina instalarea [name], trebuie repornit calculatorul. Vrei să fie repornit acum?
FinishedRestartMessage=Pentru a termina instalarea [name], trebuie repornit calculatorul.%n%nVrei să fie repornit acum?
ShowReadmeCheck=Da, vreau să văd fila de informare (README)
YesRadio=&Da, repornește calculatorul acum
NoRadio=&Nu, voi reporni eu calculatorul mai tîrziu
; used for example as 'Run MyProg.exe'
RunEntryExec=Rulează %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Vezi %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Aplicația Necesită Discul Următor
SelectDiskLabel2=Introdu Discul %1 și apasă pe OK.%n%nDacă filele de pe acest disc pot fi găsite într-un alt dosar decît cel afișat mai jos, introdu calea corectă sau apasă pe Explorează.
PathLabel=&Cale:
FileNotInDir2=Fila "%1" nu poate fi găsită în "%2". Introdu discul corect sau selectează alt dosar.
SelectDirectoryLabel=Specifică locul discului următor.

; *** Installation phase messages
SetupAborted=Instalarea nu a fost terminată.%n%nCorectează problema și apoi rulează Instalarea din nou.
EntryAbortRetryIgnore=Apasă pe Reîncearcă pentru a încerca din nou, pe Ignoră pentru a continua oricum, sau pe Abandonează pentru a anula instalarea.

; *** Installation status messages
StatusClosingApplications=Închid aplicațiile...
StatusCreateDirs=Creez dosarele...
StatusExtractFiles=Extrag filele...
StatusCreateIcons=Creez scurtăturile...
StatusCreateIniEntries=Creez intrările INI...
StatusCreateRegistryEntries=Creez intrările în registru...
StatusRegisterFiles=Înregistrez filele...
StatusSavingUninstall=Salvez informațiile de dezinstalare...
StatusRunProgram=Finalizez instalarea...
StatusRestartingApplications=Repornesc aplicațiile...
StatusRollback=Reîntorc la starea inițială, prin anularea modificărilor făcute...

; *** Misc. errors
ErrorInternal2=Eroare Internă: %1
ErrorFunctionFailedNoCode=%1 a eșuat
ErrorFunctionFailed=%1 a eșuat; cod %2
ErrorFunctionFailedWithMessage=%1 a eșuat; cod %2.%n%3
ErrorExecutingProgram=Nu pot executa fila:%n%1

; *** Registry errors
ErrorRegOpenKey=Eroare la deschiderea cheii de registru:%n%1\%2
ErrorRegCreateKey=Eroare la crearea cheii de registru:%n%1\%2
ErrorRegWriteKey=Eroare la scrierea în cheia de registru:%n%1\%2

; *** INI errors
ErrorIniEntry=Eroare la crearea intrării INI în fișierul "%1".

; *** File copying errors
FileAbortRetryIgnore=Apasă pe Reîncearcă pentru a încerca din nou, pe Ignoră pentru a sări această filă (nerecomandat), sau pe Abandonează pentru a anula instalarea.
FileAbortRetryIgnore2=Apasă pe Reîncearcă pentru a încerca din nou, pe Ignoră pentru a continua oricum (nerecomandat), sau pe Abandonează pentru a anula instalarea.
SourceIsCorrupted=Fila sursă este stricată (coruptă)
SourceDoesntExist=Fila sursă "%1" nu există
ExistingFileReadOnly=Fila deja existentă este marcată doar-citire.%n%nApasă pe Reîncearcă pentru a înlătura atributul doar-citire și a încerca din nou, pe Ignoră pentru a sări această filă, sau pe Abandonează pentru a anula instalarea.
ErrorReadingExistingDest=A apărut o eroare în timpul citirii filei deja existente:
FileExists=Fila există deja.%n%Vrei ca ea să fie suprascrisă de Instalator?
ExistingFileNewer=Fila deja existentă este mai nouă decît cea care trebuie instalată. Este recomandat s-o păstrezi pe cea existentă.%n%nVrei să păstrezi fila deja existentă?
ErrorChangingAttr=A apărut o eroare în timpul schimbării atributelor filei deja existente:
ErrorCreatingTemp=A apărut o eroare în timpul creării filei în dosarul de destinație:
ErrorReadingSource=A apărut o eroare în timpul citirii filei sursă:
ErrorCopying=A apărut o eroare în timpul copierii filei:
ErrorReplacingExistingFile=A apărut o eroare în timpul înlocuirii filei deja existente:
ErrorRestartReplace=Repornirea/Înlocuirea a eșuat:
ErrorRenamingTemp=A apărut o eroare în timpul renumirii unei file din dosarul de destinație:
ErrorRegisterServer=Nu pot înregistra DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 a eșuat, avînd codul de ieșire %1
ErrorRegisterTypeLib=Nu pot înregistra biblioteca de tipuri: %1

; *** Post-installation errors
ErrorOpeningReadme=A apărut o eroare la deschiderea filei de informare (README).
ErrorRestartingComputer=Aplicația nu a putut reporni calculatorul. Va trebui să-l repornești manual.

; *** Uninstaller messages
UninstallNotFound=Fila "%1" nu există. Dezinstalarea nu poate fi făcută.
UninstallOpenError=Fila "%1" nu poate fi deschisă. Dezinstalarea nu poate fi făcută
UninstallUnsupportedVer=Fila "%1" ce conține jurnalul de dezinstalare este într-un format nerecunoscut de această versiune a dezinstalatorului. Dezinstalarea nu poate fi făcută
UninstallUnknownEntry=A fost întîlnită o intrare necunoscută (%1) în jurnalul de dezinstalare
ConfirmUninstall=Sigur vrei să înlături complet %1 și componentele sale?
UninstallOnlyOnWin64=Această instalare poate fi dezinstalată doar pe un sistem Windows 64-biți.
OnlyAdminCanUninstall=Această instalare poate fi dezinstalată doar de către un utilizator cu drepturi de Administrator.
UninstallStatusLabel=Așteaptă ca %1 să fie înlăturat de pe calculator.
UninstalledAll=%1 a fost înlăturat cu succes de pe calculator.
UninstalledMost=Dezinstalare completă a %1.%n%nAnumite elemente nu au putut fi înlăturate. Acestea pot fi înlăturate manual.
UninstalledAndNeedsRestart=Pentru a termina dezinstalarea %1, calculatorul trebuie repornit.%n%nVrei să fie repornit acum?
UninstallDataCorrupted=Fila "%1" este stricată (coruptă). Dezinstalarea nu poate fi făcută

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=ªterg Fila Partajată?
ConfirmDeleteSharedFile2=Sistemul indică faptul că fila partajată următoare pare să nu mai fie folosită de vreun alt program. Vrei ca Dezinstalatorul să șteargă această filă partajată?%n%nDacă totuși mai există programe care folosesc fila și ea este ștearsă, acele programe ar putea să funcționeze greșit. Dacă nu ești sigur, alege Nu. Lăsarea filei în sistem nu va produce nici o neplăcere.
SharedFileNameLabel=Nume Filă:
SharedFileLocationLabel=Loc:
WizardUninstalling=Starea Dezinstalării
StatusUninstalling=Dezinstalez %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Instalez %1.
ShutdownBlockReasonUninstallingApp=Dezinstalez %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 versiunea %2
AdditionalIcons=Icoane suplimentare:
CreateDesktopIcon=Creează o icoană pe &Desktop
CreateQuickLaunchIcon=Creează o icoană în Bara de &Lansare Rapidă ("Quick Launch")
ProgramOnTheWeb=%1 pe internet
UninstallProgram=Dezinstalează %1
LaunchProgram=Lansează %1
AssocFileExtension=&Asociază %1 cu extensia de file %2
AssocingFileExtension=Asociez %1 cu extensia de file %2...
AutoStartProgramGroupDescription=Pornire:
AutoStartProgram=Pornește automat %1
AddonHostProgramNotFound=%1 nu poate fi găsit în dosarul selectat.%n%nVrei să continui oricum?
