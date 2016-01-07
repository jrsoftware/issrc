; *** Inno Setup version 5.5.3+ Esperanto messages ***
;       
;       Author: Alexander Gritchin   (E-mail - alexgrimo@mail.ru)
;
;       Au`toro: Alexander Gritc`in  (E-mail - alexgrimo@mail.ru)
;       Versio del traduko -  15.06.08
; 
;  
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).

[LangOptions]
; The following three entries are very important. Be sure to read and 
; understand the '[LangOptions] section' topic in the help file.
LanguageName=Esperanto
LanguageID=$0
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
SetupAppTitle=Instalado
SetupWindowTitle=Instalado de - %1
UninstallAppTitle=Forigado
UninstallAppFullTitle=Forigado de %1 

; *** Misc. common
InformationTitle=Informacio
ConfirmTitle=Konfirmado
ErrorTitle=Eraro

; *** SetupLdr messages
SetupLdrStartupMessage=Nun estos instalado de %1. C`u vi volas kontinui?
LdrCannotCreateTemp=Nepoveble estas krei tempan dosieron. La Majstro estas s`topita
LdrCannotExecTemp=Nepoveble estas plenumi la dosieron en tempa dosierujo. La Majstro estas s`topita

; *** Startup error messages
LastErrorMessage=%1.%n%nEraro %2: %3
SetupFileMissing=La dosiero %1 estas preterpasita el instala dosierujo.Bonvolu korekti problemon au` ricevu novan kopion de programo.
SetupFileCorrupt=Instalaj dosieroj estas kriplitaj. Bonvolu ricevu novan kopion de programo.
SetupFileCorruptOrWrongVer=Instalaj dosieroj estas kriplitaj, au` ne komparablaj kun tia versio del Majstro. Bonvolu korekti problemon au` ricevu novan kopion de programo.
InvalidParameter=Malg`usta parametro estis en komandlinio:%n%n%1
SetupAlreadyRunning=La Majstro jam funkcias.
WindowsVersionNotSupported=C`i tia programo ne povas subteni la version de Vindoso en via komputilo.
WindowsServicePackRequired=Por c`i tia programo bezonas %1 Service Pack %2 au` pli olda.
NotOnThisPlatform=C`i tia programo ne funkcios en %1.
OnlyOnThisPlatform=C`i tia programo devas funkcii en %1.
OnlyOnTheseArchitectures=C`i tia programo nur povas esti instalita en version de Vindoso por sekvaj procesoraj arkitekturoj:%n%n%1
MissingWOW64APIs=La versio de Vindoso kian vi lanc`is, ne havas posedon bezonatan por ke Majstro plenumis 64-bit instaladon. Por korekti tian problemon bonvolu instali Service Pack %1.
WinVersionTooLowError=Por c`i tia programo bezonas %1 version %2 au` pli olda.
WinVersionTooHighError=C`i tia programo ne povas esti instalita en %1 versio %2 au` pli olda.
AdminPrivilegesRequired=Vi devas eniri kiel administranto kiam instalas c`i tian programon.
PowerUserPrivilegesRequired=Vi devas eniri kiel administranto au` kiel membro de grupo de Posedaj Uzantoj kiam instalas c`i tia programo.
SetupAppRunningError=La Majstro difinis ke %1 nun funkcias.%n%nBonvolu s`topi g`in, kaj poste kliku Jes por kontinui, au` S`topi por eliri.
UninstallAppRunningError=Forigados difinis ke %1 nun funkcias.%n%nBonvolu s`topi g`in, kaj poste kliku Jes por kontinui, au` S`topi por eliri.

; *** Misc. errors
ErrorCreatingDir=La Majstro ne povas krei dosierujon "%1"
ErrorTooManyFilesInDir=Estas nepoveble krei dosieron en dosierujo "%1" pro tio ke g`i havas tro multe da dosierojn

; *** Setup common messages
ExitSetupTitle=S`topo Majstron 
ExitSetupMessage=La instalado ne estas plena. Se vi eliros nun, la programo ne estos instalita.%n%nPor vi bezonas s`alti Majstron denove en alia tempo por plenumi instaladon.%n%nC`u fini la Majstron?
AboutSetupMenuItem=&Pri instalo...
AboutSetupTitle=Pri instalo
AboutSetupMessage=%1 version %2%n%3%n%n%1 hejma pag`o:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< &Reen
ButtonNext=&Antau`en >
ButtonInstall=&Instali
ButtonOK=Jes
ButtonCancel=S`topi
ButtonYes=&Jes
ButtonYesToAll=Jes por &c`iaj
ButtonNo=&Ne
ButtonNoToAll=Ne por c`iaj
ButtonFinish=&Fino
ButtonBrowse=&Elekto...
ButtonWizardBrowse=Elekto...
ButtonNewFolder=&Fari la novan dosierujon

; *** "Select Language" dialog messages
SelectLanguageTitle=Elektu la lingvon
SelectLanguageLabel=Elektu la lingvon por uzo dum instalado:

; *** Common wizard text
ClickNext=Kliku Antau`en por kontinui, au` S`topi por eliri Instaladon.
BeveledLabel=
BrowseDialogTitle=Elekto de dosierujo
BrowseDialogLabel=Elektu la dosierujon en listo malalte, kaj kliku Jes.
NewFolderName=Nova dosierujo

; *** "Welcome" wizard page
WelcomeLabel1=Bonvenon al Majstro de instalado de [name] 
WelcomeLabel2=Nun komencos instalado de [name/ver] en via komputilo.%n%nEstas rekomendite ke vi s`topu c`iajn viajn programojn antau` komenco.

; *** "Password" wizard page
WizardPassword=Pasvorto
PasswordLabel1=C`i tia instalado postulas pasvorton.
PasswordLabel3=Bonvolu tajpi pasvorton kaj poste kliku Antau`en por kontinui. La pasvortoj estas tajp sentemaj.
PasswordEditLabel=&Pasvorto:
IncorrectPassword=La pasvorto, kian vi tajpis estas malg`usta. Bonvolu provi denove.

; *** "License Agreement" wizard page
WizardLicense=Licenza konvenio
LicenseLabel=Bonvolu legi sekvan gravan informacion antau` komenci.
LicenseLabel3=Bonvolu legi sekvan Licenzan Konvenion. Vi devas akcepti dotaj`oj de tia konvenio antau` ke kontinui instaladon.
LicenseAccepted=Mi akceptas konvenion
LicenseNotAccepted=Mi ne akceptas konvenion

; *** "Information" wizard pages
WizardInfoBefore=Informacio
InfoBeforeLabel=Bonvolu legi sekvan gravan informacion antau` komenci.
InfoBeforeClickLabel=Kiam vi estas preta por kontinui per instalo, kliku Antau`en.
WizardInfoAfter=Informacio
InfoAfterLabel=Bonvolu legi sekvan gravan informacion antau` komenci.
InfoAfterClickLabel=Kiam vi estas preta por kontinui per instalo, kliku Antau`en.

; *** "User Information" wizard page
WizardUserInfo= Informacio pri uzanto
UserInfoDesc=Bonvolu skribi vian informacion.
UserInfoName=Nomo de uzanto:
UserInfoOrg=&Organizacio:
UserInfoSerial=&Seria Numero:
UserInfoNameRequired=Vi devas skribi nomon de uzanto.

; *** "Select Destination Location" wizard page
WizardSelectDir=Elektu Destinan Locon
SelectDirDesc=Kie devos [name] esti instalita?
SelectDirLabel3=La Majstro instalos [name] en sekvan dosierujon.
SelectDirBrowseLabel=Por kontinui, kliku Antau`en. Se vi volas elekti diversan dosierujon, kliku Elekto.
DiskSpaceMBLabel=Almenau`  [mb] MB de neta diska spaco bezonas.
CannotInstallToNetworkDrive=Majstro ne povas instali lokan diskon.
CannotInstallToUNCPath=Majstro ne povas instali lau`  UNC vojo.
InvalidPath=Vi devas skribi plenan vojon de diska litero; por ekzamplo:%n%nC:\APP%n%sed ne UNC vojo lau` formo:%n%n\\server\share
InvalidDrive=Disko au` UNC kian vi elektis ne ekzistas au` ne estas difinita. Bonvolu elekti denove.
DiskSpaceWarningTitle=Mankas Diskan Spacon
DiskSpaceWarning=Por instalo bezonas almenau` %1 KB de neta spaco por instalado, sed electita disko havas nur %2 KB.%n%nC`u vi volas kontinui per c`iokaze?
DirNameTooLong=La nomo de dosierujo au` vojo estas tro longa.
InvalidDirName=La nomo de dosierujo estas malg`usta.
BadDirName32=La nomoj de dosierujoj ne povas havi de sekvaj karakteroj:%n%n%1
DirExistsTitle=Dosierujo ekzistas
DirExists=La dosierujo:%n%n%1%n%njam ekzistas. C`u vi volas instali en g`i c`iokaze?
DirDoesntExistTitle=La dosierujo ne ekzistas
DirDoesntExist=La dosierujo:%n%n%1%n%nne ekzistas. C`u vi volas por ke tia dosierujo estos farita?

; *** "Select Components" wizard page
WizardSelectComponents=Elektu komponentoj
SelectComponentsDesc=Kiaj komponentoj devas esti instalitaj?
SelectComponentsLabel2=Elektu komponentoj kiaj vi volas instali; forigu la komponentojn kiaj vi ne volas instali. Kliku Antau`en kiam vi estas preta por kontinui.
FullInstallation=Tuta instalado
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Kompakta instalado
CustomInstallation=Kutima instalado
NoUninstallWarningTitle=Komponentoj ekzistas
NoUninstallWarning=La Majstro difinis ke sekvaj komponentoj jam estas instalitaj en via komputilo:%n%n%1%n%nNuligo de elekto de tiaj komponentoj ne forigos g`in.%n%nC`u vi volas kontinui c`iokaze?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Nuna elekto bezonas almenau` [mb] MB de diska spaco.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Elektu aldonaj taskoj
SelectTasksDesc=Kiaj aldonaj taskoj devos esti montrotaj?
SelectTasksLabel2=Elektu aldonaj taskoj kiaj bezonas por ke Majstro montros dum instalado [name], kaj poste kliku Antau`en.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Elektu dosierujon de starta menuo
SelectStartMenuFolderDesc=Kie Majstro devas krei tujklavon de programo?
SelectStartMenuFolderLabel3=La Majstro kreos tujklavojn de programo en sekva dosierujo de starta menuo.
SelectStartMenuFolderBrowseLabel=Por kontinui, kliku Antau`en. Se vi volas elekti alian dosierujon, kliku Elekto.
MustEnterGroupName=Vi devas skribi  la nomo de dosierujo.
GroupNameTooLong=La nomo de dosierujo au` vojo estas tro longa.
InvalidGroupName=La nomo de dosierujo estas malg`usta.
BadGroupName=La nomoj de dosierujoj ne povas havi de sekvaj karakteroj:%n%n%1
NoProgramGroupCheck2=Ne krei dosierujon de starta menuo

; *** "Ready to Install" wizard page
WizardReady=Preparado por Instalo
ReadyLabel1=Nun c`io estas preparita por komenci instaladon [name] en via komputilo.
ReadyLabel2a=Kliku Instali por kontinui instaladon, au`kliku Reen se vi volas rigardi au` s`ang`i ajnajn statojn.
ReadyLabel2b=Kliku Instali por kontinui instaladon.
ReadyMemoUserInfo=Informacio de uzanto:
ReadyMemoDir=Destina loko:
ReadyMemoType=Majstra tipo:
ReadyMemoComponents=Elektitaj komponentoj:
ReadyMemoGroup=La dosierujo de starta menuo:
ReadyMemoTasks=Aldonaj taskoj:

; *** "Preparing to Install" wizard page
WizardPreparing=Preparado por Instalo
PreparingDesc=Majstro estas preparata por instalo [name] en via komputilo.
PreviousInstallNotCompleted=Instalado/Forigo de antau`a programo ne estas plena. Por vi bezonas relanc`i vian komputilon por plenigi tian instaladon.%n%nPost relanc`o de via komputilo, s`altu Majstron denove por finigi instaladon de [name].
CannotContinue=La Majstro ne povas kontinui. Bonvolu kliki Fino por eliri.
ApplicationsFound=Sekvaj aplikaj`oj uzas dosierojn kiajn bezonas renovigi per Instalado. Estas rekomendite ke vi permesu al Majstro automate fermi tiajn aplikaj`ojn.
ApplicationsFound2=Sekvaj aplikaj`oj uzas dosierojn kiajn bezonas renovigi per Instalado. Estas rekomendite ke vi permesu al Majstro automate fermi tiajn aplikaj`ojn. Poste de instalado Majstro provos relanc`i aplikaj`ojn.
CloseApplications=&Automate fermi aplikaj`ojn
DontCloseApplications=Ne fermu aplikaj`ojn
ErrorCloseApplications=Majstro estis nepovebla au`tomate fermi c`iajn aplikaj`ojn. Estas rekomendite ke vi fermu c`iajn aplikaj`ojn, uzantaj dosierojn, kiaj estas bezonatajn por renovigo per la Majstro antau` kontinui. 

; *** "Installing" wizard page
WizardInstalling=Instalado
InstallingLabel=Bonvolu atenti dum Majstro instalas [name] en via komputilo.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=  Fino de instalado
FinishedLabelNoIcons=La Majstro finigis instaladon [name] en via komputilo.
FinishedLabel=La Majstro finigis instaladon [name] en via komputilo. La aplikaj`o povos esti lanc`ita  per elekto de instalaj ikonoj.
ClickFinish=  Kliku  Fino  por finigi instaladon.
FinishedRestartLabel=Por plenumigi instaladon de [name], Majstro devas relanc`i vian komputilon. C`u vi volas relanc`i nun?
FinishedRestartMessage=Por plenumigi instaladon de [name], Majstro devas relanc`i vian komputilon.%n%nC`u vi volas relanc`i nun?
ShowReadmeCheck=Jes, mi volas rigardi dosieron README 
YesRadio=&Jes, relanc`u komputilon nun
NoRadio=&Ne, mi volas relanc`i komputilon poste
; used for example as 'Run MyProg.exe'
RunEntryExec=S`altu %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Rigardi %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=La Majstro postulas sekvan diskon
SelectDiskLabel2=Bonvolu inserti Diskon %1 kaj kliku Jes.%n%nSe dosieroj en tia disko povos esti diversaj de prezentitaj malalte, enskribu korektan vojon au` kliku Elekto.
PathLabel=&Vojo:
FileNotInDir2=Dosieron "%1" estas nepoveble lokigi en "%2". Bonvolu inserti korectan diskon au` elektu alian dosierujon.
SelectDirectoryLabel=Bonvolu difini lokon de alia disko.

; *** Installation phase messages
SetupAborted=Instalado ne estis plena.%n%nBonvolu korekti problemon kaj lanc`u Majstron denove.
EntryAbortRetryIgnore=Kliku Reen por provi ankorau`, Ignori por fari c`iokaze, au` S`topi por finigi instaladon.

; *** Installation status messages
StatusClosingApplications=Fermado de aplikaj`oj...
StatusCreateDirs=Kreado de dosierujojn...
StatusExtractFiles=Ekstraktado de dosierojn...
StatusCreateIcons=Kreado de tujklavojn...
StatusCreateIniEntries=Kreado de INI dosierojn...
StatusCreateRegistryEntries=Kreado de registraj pointoj...
StatusRegisterFiles=Registrado de dosierojn...
StatusSavingUninstall=Konservas informacio por forigo...
StatusRunProgram=Finig`as instalado...
StatusRestartingApplications=Relanc`o de aplikaj`oj...
StatusRollback=Renovigo de s`ang`oj...

; *** Misc. errors
ErrorInternal2=Interna eraro: %1
ErrorFunctionFailedNoCode=%1 estas kripligita
ErrorFunctionFailed=%1 estas kripligita; kodnomo %2
ErrorFunctionFailedWithMessage=%1 estas kripligita; kodnomo %2.%n%3
ErrorExecutingProgram=Estas nepoveble plenumi dosieron:%n%1

; *** Registry errors
ErrorRegOpenKey=Eraro dum malfermo de registra s`losilo:%n%1\%2
ErrorRegCreateKey=Eraro dum kreado de registra s`losilo:%n%1\%2
ErrorRegWriteKey=Eraro dum skribado en registra s`losilo:%n%1\%2

; *** INI errors
ErrorIniEntry=Eraro dum kreado de INI pointo en dosiero "%1".

; *** File copying errors
FileAbortRetryIgnore=Kliku Reen por provi denove, Ignori por lasi tian dosieron (ne estas rekomendite), au` S`topi por finigi instaladon.
FileAbortRetryIgnore2=Kliku Reen por provi denove, Ignori por plenumi c`iokaze (ne estas rekomendite), au` S`topi por finigi instaladon.
SourceIsCorrupted=La fonta dosiero estas kripligita
SourceDoesntExist=La fonta dosiero "%1" ne ekzistas
ExistingFileReadOnly=Ekzista dosiero estas markita kiel nurlega.%n%nKliku Reen por forigi la nurlegan atributon kaj provu reen, Ignori por lasi tian dosieron, au` S`topi por fini instaladon.
ErrorReadingExistingDest=Eraro aperis dum legado de ekzista dosiero:
FileExists=La dosiero jam ekzistas.%n%nC`u vi volas ke Majstro reskribu g`in?
ExistingFileNewer=Ekzista dosiero estas pli nova ol Majstro provas instali. Estas rekomendita ke vi konservu ekzistan dosieron.%n%nC`u vi volas konservi ekzistan dosieron?
ErrorChangingAttr=Eraro aperis dum provo c`ang`i  atributoj de ekzista dosiero:
ErrorCreatingTemp=Eraro aperis dum kreado dosieron en destina dosierujo:
ErrorReadingSource=Eraro aperis dum legado de dosiero:
ErrorCopying=Eraro aperis dum kopiado de dosiero:
ErrorReplacingExistingFile=Eraro aperis dum relokig`o de ekzistan dosieron:
ErrorRestartReplace=Relanc`o/Relokig`o estas kripligita:
ErrorRenamingTemp=Eraro aperis dum renomig`o del dosiero en destina dosierujo:
ErrorRegisterServer=Estas nepoveble registri DLL/OC`: %1
ErrorRegSvr32Failed=RegSvr32estas kripligita kun elira codo %1
ErrorRegisterTypeLib=Estas nepoveble registri bibliotekon de tipo : %1

; *** Post-installation errors
ErrorOpeningReadme=Eraro aperis dum malfermado de README dosiero.
ErrorRestartingComputer=Majstro ne povis relanc`i komputilo. Bonvolu fari tion permane.

; *** Uninstaller messages
UninstallNotFound=Dosiero "%1" ne ekzistas. Estas nepoveble forigi.
UninstallOpenError=Dosieron "%1" nepoveble estas malfermi. Estas nepoveble forigi
UninstallUnsupportedVer=\Foriga protokolo "%1" estas en nekonata formato per c`i tia versio de forigprogramo. Estas nepoveble forigi
UninstallUnknownEntry=Ekzistas nekonata pointo (%1) en foriga protokolo
ConfirmUninstall=C`u vi reale volas tute forigi  %1 kaj c`iaj komponentoj de g`i?
UninstallOnlyOnWin64=C`i tian instaladon povos forigi nur en 64-bit Vindoso.
OnlyAdminCanUninstall=C`i tian instaladon povos forigi nur uzanto kun administrantaj rajtoj.
UninstallStatusLabel=Bonvolu atendi dum %1 forig`os de via komputilo.
UninstalledAll=%1 estis sukcese forigita de via komputilo.
UninstalledMost=Forigo de %1 estas plena.%n%nKelkaj elementoj ne estis forigitaj. G`in poveble estas forigi permane.
UninstalledAndNeedsRestart=Por plenumi forigadon de %1, via komputilo devas esti relanc`ita.%n%nC`u vi volas relanc`i nun?
UninstallDataCorrupted="%1" dosiero estas kriplita. Estas nepoveble forigi

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Forigi komune uzatan dosieron?
ConfirmDeleteSharedFile2=La sistemo indikas ke sekva komune uzata dosiero jam ne estas uzata per neniel aplikaj`oj. C`u vi volas forigi c`i tian dosieron?%n%nSe ajna programo jam uzas tian dosieron, dum forigo g`i povos malg`uste funkcii. Se vi ne estas certa elektu Ne. Restante en via sistemo la dosiero ne damag`os g`in.
SharedFileNameLabel=nomo de dosiero:
SharedFileLocationLabel=Loko:
WizardUninstalling=Stato de forigo
StatusUninstalling=Forigado %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Instalado %1.
ShutdownBlockReasonUninstallingApp=Forigado %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1   versio %2
AdditionalIcons=Aldonaj ikonoj:
CreateDesktopIcon=Krei  &Labortablan ikonon
CreateQuickLaunchIcon=Krei &Rapida lanc`o ikonon
ProgramOnTheWeb=%1 en Reto
UninstallProgram=Rorig`ado %1
LaunchProgram=Lanc`o %1
AssocFileExtension=&Asociigi %1 kun %2 dosieraj finaj`oj
AssocingFileExtension=Asociig`as %1 kun %2 dosiera finaj`o...
AutoStartProgramGroupDescription=Lanc`o:
AutoStartProgram=Automate s`alti %1
AddonHostProgramNotFound=%1 nepoveble estas loki en dosierujo kian vi elektis.%n%nC`u vi volas kontinui c`iokaze?
