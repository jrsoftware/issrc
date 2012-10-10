; *** Inno Setup version 5.1.11+ Occitan messages ***
;
; To download user-contributed translations of this file, go to:
;   http://www.jrsoftware.org/is3rdparty.php
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).
;
; Translated by David Gimeno i Ayuso, info@sima.cat, 2007/03/18

[LangOptions]
LanguageName=Occit<00E0>n
LanguageID=$0482
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
SetupAppTitle=Installacion
SetupWindowTitle=Installacion - %1
UninstallAppTitle=Desinstallacion
UninstallAppFullTitle=Desinstallar %1

; *** Misc. common
InformationTitle=Informacion
ConfirmTitle=Confirmacion
ErrorTitle=Error

; *** SetupLdr messages
SetupLdrStartupMessage=Aguest programa installarà %1. Voletz continuar?
LdrCannotCreateTemp=Non s'a pogut crear un fichèr temporau. Installacion cancellada
LdrCannotExecTemp=Non s'a pogut executar eth fichèr ara carpeta temporau. Installacion cancellada

; *** Startup error messages
LastErrorMessage=%1.%n%nError %2: %3
SetupFileMissing=Eth fichèr %1 non se trapa ara carpeta d'installacion. Resolvatz eth problema o obtietz ua naua còpia deth programa.
SetupFileCorrupt=Es fichèrs d'installacion estan corrompudi. Obtietz ua naua còpia deth programa.
SetupFileCorruptOrWrongVer=Es fichèrs d'installacion estan espatladi, o son incompatibles damb aguesta version deth programa. Resolvatz eth problema o obtietz ua naua còpia deth programa.
NotOnThisPlatform=Aguest programa non foncionarà jos %1.
OnlyOnThisPlatform=Aguest programa sonque se pòt executar jos %1.
OnlyOnTheseArchitectures=Aguest programa sonque se pèt installar a versions de Windows dessenhades entàs següentes arquitectures de processador:%n%n%1
MissingWOW64APIs=Era version de Windows qu'auetz non includís ua foncionalitat requerida per Setup entà efectuar ua installacion de 64 bits. Entà corregir aguest problèma, installatz eth Service Pack %1.
WinVersionTooLowError=Aguest programa requerís %1 version %2 o posteriora.
WinVersionTooHighError=Aguest programa non se pòt installar jos %1 version %2 o posteriora.
AdminPrivilegesRequired=Cau qu'ajatz privilègis d'administrador entà poder installar aguest programa.
PowerUserPrivilegesRequired=Cau que acceditz coma administrador o coma membre deth grop Power Users en installar aguest programa.
SetupAppRunningError=Eth programa d'installacion a detectat que %1 s'execute actuaument.%n%nBarratz eth programa e premetz Següent entà continuar o Cancellar entà gésser.
UninstallAppRunningError=Eth programa de desinstallacion a detectat que %1 s'execute en aguest moment.%n%nBarratz eth programa e premetz Següent entà continuar o Cancellar entà gésser.

; *** Misc. errors
ErrorCreatingDir=Eth programa d'installacion non a pogut crear era carpeta "%1"
ErrorTooManyFilesInDir=Non s'a pogut crear un fichèr ara carpeta "%1" pr'amor que conten massa fichèrs

; *** Setup common messages
ExitSetupTitle=Gésser
ExitSetupMessage=Era installacion non s'a completat. Se gessetz ara, eth programa non serà installat.%n%nEnta completar-la poderatz tornar a executar eth programa d'installacion quan volgatz.%n%nVolgatz gésser-ne?
AboutSetupMenuItem=&Sus era installacion...
AboutSetupTitle=Sus era installacion
AboutSetupMessage=%1 version %2%n%3%n%nPagina web de %1:%n%4
AboutSetupNote=
TranslatorNote=Occitan translation maintained by David Gimeno i Ayuso (info@sima.cat)

; *** Buttons
ButtonBack=< &Tornar
ButtonNext=&Següent >
ButtonInstall=&Installar
ButtonOK=Corrècte
ButtonCancel=Cancellar
ButtonYes=Ò&c
ButtonYesToAll=Òc a &Tot
ButtonNo=&Non
ButtonNoToAll=N&on a tot
ButtonFinish=&Finalizar
ButtonBrowse=&Explorar...
ButtonWizardBrowse=E&xplorar...
ButtonNewFolder=&Crear ua carpeta naua

; *** "Select Language" dialog messages
SelectLanguageTitle=Seleccionatz idiòma
SelectLanguageLabel=Seleccionatz er idiòma d'installacion:

; *** Common wizard text
ClickNext=Premetz Següent entà continuar o Cancellar entà abandonar era installacion.
BeveledLabel=
BrowseDialogTitle=Explorar ua carpeta
BrowseDialogLabel=Seleccionatz ua carpeta dera lista següenta e clicatz Corrècte.
NewFolderName=Carpeta naua

; *** "Welcome" wizard page
WelcomeLabel1=Benvengut ar assistent d'installacion de [name]
WelcomeLabel2=Aguest programa installarà [name/ver] ath vòste ordinador.%n%nEi recomanable que abantes de continuar barratz toti es autes programes dubèrti, per tau d'evitar conflictes pendent eth procès d'installacion.

; *** "Password" wizard page
WizardPassword=Còdi d'accès
PasswordLabel1=Aguesta installacion està protegida damb un còdi d'accès.
PasswordLabel3=Indicatz eth còdi d'accès e premetz Següent entà continuar. Aguest còdi distinguís entre majuscules e minuscules.
PasswordEditLabel=&Còdi:
IncorrectPassword=Eth còdi introdusit non ei corrècte. Tornatz a intentar-ac.

; *** "License Agreement" wizard page
WizardLicense=Acceptacion dera licéncia d'emplec.
LicenseLabel=Cau que liegetz aguesta informacion abantes de continuar.
LicenseLabel3=Liegetz-vos er Acord de Licéncia següent. Cau que n'acceptatz es tèrmes abantes de continuar damb era installacion.
LicenseAccepted=&Accepti er acòrd
LicenseNotAccepted=&Non accepti er acòrd

; *** "Information" wizard pages
WizardInfoBefore=Informacion
InfoBeforeLabel=Liegetz-vos era informacion següenta abantes de continuar.
InfoBeforeClickLabel=Quan estetz preparat entà continuar, premetz Següent
WizardInfoAfter=Informacion
InfoAfterLabel=Liegetz-vos era informacion següenta abantes de continuar.
InfoAfterClickLabel=Quan estetz preparat entà continuar, premetz Següent

; *** "User Information" wizard page
WizardUserInfo=Informacion sus er usatgèr
UserInfoDesc=Entratz-i era vòsta informacion.
UserInfoName=&Nòm der usatgèr:
UserInfoOrg=&Organizacion
UserInfoSerial=&Numerò de sèrie:
UserInfoNameRequired=Cau que i entratz un nòm

; *** "Select Destination Directory" wizard page
WizardSelectDir=Escuelhetz Carpeta de Destinacion
SelectDirDesc=A on s'a d'installar [name]?
SelectDirLabel3=Escuelhetz era carpeta a on voletz installar [name].
SelectDirBrowseLabel=Premetz Següent entà continuar. Se en voletz seleccionar ua de diferenta, premetz Explorar.
DiskSpaceMBLabel=Aguest programa a de besonh un minim de [mb] MB d'espaci a disc.
ToUNCPathname=Era installacion non pòt installar eth programa en ua carpeta UNC. Se estatz en tot provar d'installar-lo en hilat, auratz d'assignar ua letra (D:, E:, etc...) ath disc de destinacion.
InvalidPath=Cau dar ua rota completa damb letra d'unitat, per exemple:%n%nC:\Aplicacion%n%non ben ua rota UNC en era forma:%n%n\\servidor\compartit
InvalidDrive=Eth disc o rota de hilat seleccionat non existís, escuelhetz-ne un aute.
DiskSpaceWarningTitle=Non i a pro espaci ath disc
DiskSpaceWarning=Eth programa d'installacion a de besonh coma minim %1 KB d'espaci liure, mès eth disc seleccionat sonque a %2 KB disponibles.%n%nTot e damb açò, desiratz continuar?
DirNameTooLong=Era rota o nòm dera carpeta ei massa long.
InvalidDirName=Eth nòm dera carpeta ei incorrècte.
BadDirName32=Un nòm de carpeta non pòt contier cap des caractèrs següents:%n%n%1
DirExistsTitle=Era carpeta existís
DirExists=Era carpeta:%n%n%1%n%nja existís. Volgatz installar igualament eth programa en aguesta carpeta?
DirDoesntExistTitle=Era Carpeta Non Existís
DirDoesntExist=Era carpeta:%n%n%1%n%nnon existís. Volgatz que sigue creada?

; *** "Select Program Group" wizard page
WizardSelectComponents=Escuelhetz Components
SelectComponentsDesc=Quini components cau installar?
SelectComponentsLabel2=Seleccionatz es components que voletz installar; eliminatz es components que non voletz installar. Premetz Següent entà continuar.
FullInstallation=Installacion completa
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Installacion compacta
CustomInstallation=Installacion personalizada
NoUninstallWarningTitle=Es components Existissen
NoUninstallWarning=Eth programa d'installacion a detectat qu'es components següents ja se trapen ath vòste ordinador:%n%n%1%n%nSe non estan seleccionadi non seran desinstalladi.%n%nVolgatz continuar igualament?
ComponentSize1=%1 Kb
ComponentSize2=%1 Mb
ComponentsDiskSpaceMBLabel=Aguesta seleccion requerís un minim de [mb] Mb d'espaci ath disc.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Escuelhetz prètzhèts addicionaus
SelectTasksDesc=Quini prètzhèts addicionaus cau executar?
SelectTasksLabel2=Escuelhetz es prètzhèts addicionaus que voletz que siguen executadi mentre s'installa [name], e dempús premetz Següent.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Escuelhetz era carpeta deth Menu Inici
SelectStartMenuFolderDesc=A on cau plaçar es ligams deth programa?
SelectStartMenuFolderLabel3=Escuelhetz era carpeta deth Menu Inici a on voletz qu'eth programa d'installacion cree es ligams.
SelectStartMenuFolderBrowseLabel=Premetz Següent entà continuar. S'en voletz seleccionar ua de diferenta, premetz Explorar.
MustEnterGroupName=Cau que i entratz un nòm de carpeta.
GroupNameTooLong=Era rota o nòm dera carpeta ei massa long.
InvalidGroupName=Eth nòm dera carpeta ei incorrècte.
BadGroupName=Eth nòm deth grop non pòt contier cap des caractèrs següents:%n%n%1
NoProgramGroupCheck2=&Non crear ua carpeta ath Menu Inici

; *** "Ready to Install" wizard page
WizardReady=Preparat entà installar
ReadyLabel1=Eth programa d'installacion està preparat entà iniciar era installacion de [name] ath vòste ordinador.
ReadyLabel2a=Premetz Installar entà continuar damb era installacion, o Tornar se voletz revisar o modificar es opcions d'installacion.
ReadyLabel2b=Premetz Installar entà continuar damb era installacion.
ReadyMemoUserInfo=Informacion der usatgèr:
ReadyMemoDir=Carpeta de destinacion:
ReadyMemoType=Tipe d'installacion:
ReadyMemoComponents=Components seleccionadi:
ReadyMemoGroup=Carpeta deth Menu Inici:
ReadyMemoTasks=Prètzhèts addicionaus:

; *** "Preparing to Install" wizard page
WizardPreparing=Se premanís era installacion
PreparingDesc=Se premanís era installacion de [name] ath vòste ordinador.
PreviousInstallNotCompleted=Era installacion o desinstallacion anteriora non s'a amiat a tèrme. Calerà que reiniciatz er ordinador entà finalizar aguesta installacion.%n%nDempús de reiniciar er ordinador, executatz aguest programa de nau entà completar era installacion de [name].
CannotContinue=Era installacion non pòt continuar. Premetz Cancellar entà gésser.

; *** "Installing" wizard page
WizardInstalling=S'installe
InstallingLabel=Esperatz-vos mentre s'installe [name] ath vòste ordinador.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Se complete er assistent d'installacion de [name]
FinishedLabelNoIcons=Eth programa a finalizat era installacion de [name] ath vòste ordinador.
FinishedLabel=Eth programa a finalizat era installacion de [name] ath vòste ordinador. Era aplicacion se pòt iniciar en tot seleccionar es icònes installades.
ClickFinish=Premetz Finalizar entà gésser dera installacion.
FinishedRestartLabel=Entà completar era installacion de [name] cau reiniciar er ordinador. Volgatz hèr-ac ara?
FinishedRestartMessage=Entà completar era installacion de [name] cau reiniciar er ordinador. Volgatz hèr-ac ara?
ShowReadmeCheck=Òc, voi visualizar eth fichèr LIEGETZ.TXT
YesRadio=Ò&c, reiniciar er ordinador ara
NoRadio=&Non, reiniciarè er ordinador mès tard
; used for example as 'Run MyProg.exe'
RunEntryExec=Executar %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Visualizar %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Eth programa d'installacion a de besonh eth disc següent
SelectDiskLabel2=Introdusitz eth disc %1 e premetz Continuar.%n%nSe es fichèrs d'aguest disc se pòden trapar en ua carpeta diferenta dera indicada tot seguit, entratz-ne era rota corrècta o ben premetz Explorar.
PathLabel=&Rota:
FileNotInDir2=Eth fichèr "%1" non s'a pogut trapar a "%2". Introdusitz eth disc corrècte o escuelhetz ua auta carpeta.
SelectDirectoryLabel=Indicatz a on se trapa eth disc següent.

; *** Installation phase messages
SetupAborted=Era installacion non s'a completat.%n%n%Resolvatz eth problema e executatz de nau eth programa d'installacion.
EntryAbortRetryIgnore=Premetz Reintentar entà intentar-ac de nau, Ignorar entà continuar igualament, o Cancellar entà abandonar era installacion.

; *** Installation status messages
StatusCreateDirs=Se creen carpetes...
StatusExtractFiles=S'extrèn fichèrs...
StatusCreateIcons=Se creen icònes de programa...
StatusCreateIniEntries=Se creen entrades ath fichèr INI...
StatusCreateRegistryEntries=Se creen entrades de registre...
StatusRegisterFiles=Se registren fichèrs...
StatusSavingUninstall=Se plegue informacion de desinstallacion...
StatusRunProgram=Se finalize era installacion...
StatusRollback=Se des·hèn es cambis...

; *** Misc. errors
ErrorInternal2=Error intern: %1
ErrorFunctionFailedNoCode=%1 a mancat
ErrorFunctionFailed=%1 a mancat; còdi %2
ErrorFunctionFailedWithMessage=%1 a mancat; còdi %2.%n%3
ErrorExecutingProgram=Non se pòt executar eth fichèr:%n%1

; *** Registry errors
ErrorRegOpenKey=Error en daurir era clau de registre:%n%1\%2
ErrorRegCreateKey=Error en crear era clau de registre:%n%1\%2
ErrorRegWriteKey=Error en escríuer ara clau de registre:%n%1\%2

; *** INI errors
ErrorIniEntry=Error en crear era entrada INI ath fichèr "%1".

; *** File copying errors
FileAbortRetryIgnore=Premetz Reintentar entà intentar-ac de nau, Ignorar entà sautar-se aguest fichèr (non recomanat), o Cancellar entà abandonar era installacion.
FileAbortRetryIgnore2=Premetz Reintentar entà intentar-ac de nau, Ignorar entà continuar igualament (non recomanat), o Cancellar entà abandonar era installacion.
SourceIsCorrupted=Eth fichèr d'origina està corromput
SourceDoesntExist=Eth fichèr d'origina "%1" non existís
ExistingFileReadOnly=Eth fichèr ei de sonque lectura.%n%nPremetz Reintentar entà trèir-li er atribut de sonque lectura e tornar-ac a intentar; Ométer entà sautar-se-lo (non recomanat), o Anullar entà abandonar era installacion.
ErrorReadingExistingDest=S'a produsit un error en liéger eth fichèr:
FileExists=Eth fichèr ja existís.%n%nVolgatz que sigue sus-escrit?
ExistingFileNewer=Eth fichèr existent ei mès nau qu'eth que s'intenta installar. Se recomana mantier eth fichèr existent.%n%nVolgatz mantier-lo?
ErrorChangingAttr=I a agut un error en cambiar es atributs deth fichèr:
ErrorCreatingTemp=I a agut un error en crear un fichèr ara carpeta de destinacion:
ErrorReadingSource=I a agut un error en liéger eth fichèr d'origina:
ErrorCopying=I a agut un error en copiar un fichèr:
ErrorReplacingExistingFile=I a agut un error en remplaçar eth fichèr existent:
ErrorRestartReplace=A mancat remplaçar:
ErrorRenamingTemp=I a agut un error en renomentar un fichèr ara carpeta de destinacion:
ErrorRegisterServer=Non s'a pogut registrar eth DLL/OCX: %1
ErrorRegSvr32Failed=A mancat RegSvr32 damb còdi de gessuda %1
ErrorRegisterTypeLib=Non s'a pogut registrar era bibliotèca de tipe: %1

; *** Post-installation errors
ErrorOpeningReadme=I a agut un error en daurir eth fichèr LIEGETZ.TXT.
ErrorRestartingComputer=Eth programa d'installacion non a pogut reiniciar er ordinador. Hetz-ac manualament.

; *** Uninstaller messages
UninstallNotFound=Eth fichèr "%1" non existís. Non se pòt desinstallar.
UninstallOpenError=Eth fichèr "%1" non se pòt daurir. Non se pòt desinstallar.
UninstallUnsupportedVer=Eth fichèr de desinstallacion "%1" està en un format non reconeishut entà aguesta version deth desinstallador. Non se pòt desinstallar
UninstallUnknownEntry=S'a trapat ua entrada desconeishuda (%1) ath fichèr de desinstallacion.
ConfirmUninstall=Estatz segur de voler eliminar completament %1 e toti es sòns components?
UninstallOnlyOnWin64=Aguest programa sonque se pòt desinstallar a Windows de 64 bits.
OnlyAdminCanUninstall=Aguest programa sonque se pòt desinstallar entà un usatgèr damb privilegis d'administrador.
UninstallStatusLabel=Esperatz-vos mentre s'elimine %1 deth vòste ordinador.
UninstalledAll=%1 a estat desinstallat correctament deth vòste ordinador.
UninstalledMost=Desinstallacion de %1 completada.%n%nQuauqui elements non s'an pogut eliminar. Pòden èster eliminadi manualament.
UninstalledAndNeedsRestart=Entà completar era installacion de %1, cau reiniciar eth vòste ordinador.%n%nVolgatz hèr-ac ara?
UninstallDataCorrupted=Eth fichèr "%1" està corromput. Non se pòt desinstallar.

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Eliminar fichèr compartit?
ConfirmDeleteSharedFile2=Eth sistema indica qu'eth fichèr compartit següent ja non ei emplegat per cap aute programa. Volgatz qu'era desinstallacion elimine aguest fichèr?%n%nSe quauque programa encara lo emplegue e ei eliminat, poderia non foncionar correctament. Se non n'estatz segur, escuelhetz Non. Deishar eth fichèr ath sistema non harà cap mau.
SharedFileNameLabel=Nòm deth fichèr:
SharedFileLocationLabel=Localizacion:
WizardUninstalling=Estat dera desinstallacion
StatusUninstalling=Se desinstalle %1...

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 version %2
AdditionalIcons=Icònes addicionaus:
CreateDesktopIcon=Crear ua icona ar &escriptòri
CreateQuickLaunchIcon=Crear ua icona d'execucion &rapida
ProgramOnTheWeb=%1 ath hilat
UninstallProgram=Desinstallar %1
LaunchProgram=Iniciar %1
AssocFileExtension=&Associar %1 damb era extension %2
AssocingFileExtension=S'assòcie %1 damb era extension %2...
