; Inno Setup version 6.1.0+ Esperanto messages ***
;
; Aŭtoro:   Alexander Gritĉin
; Retpoŝto: alexgrimo@mail.ru
; Traduko:  15.06.2008
; 
; Aŭtoro:   Wolfgang Pohl
; Retpoŝto: software@interpohl.net)
; Traduko:  15.04.2021
;

; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).

[LangOptions]
; The following three entries are very important. Be sure to read and 
; understand the '[LangOptions] section' topic in the help file.
LanguageName=Esperanto
LanguageID=$1000
LanguageCodePage=28593
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
SetupLdrStartupMessage=Nun estos instalado de %1. Ĉu vi volas kontinui?
LdrCannotCreateTemp=Nepoveble estas krei tempan dosieron. La Majstro estas ŝtopita
LdrCannotExecTemp=Nepoveble estas plenumi la dosieron en tempa dosierujo. La Majstro estas ŝtopita
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nEraro %2: %3
SetupFileMissing=La dosiero %1 estas preterpasita el instala dosierujo.Bonvolu korekti problemon aŭ ricevu novan kopion de programo.
SetupFileCorrupt=Instalaj dosieroj estas kriplitaj. Bonvolu ricevu novan kopion de programo.
SetupFileCorruptOrWrongVer=Instalaj dosieroj estas kriplitaj, aŭ ne komparablaj kun tia versio del Majstro. Bonvolu korekti problemon aŭ ricevu novan kopion de programo.
InvalidParameter=Malĝusta parametro estis en komandlinio:%n%n%1
SetupAlreadyRunning=La Majstro jam funkcias.
WindowsVersionNotSupported=Ĉi tia programo ne povas subteni la version de Vindoso en via komputilo.
WindowsServicePackRequired=Por ĉi tia programo bezonas %1 Service Pack %2 aŭ pli olda.
NotOnThisPlatform=Ĉi tia programo ne funkcios en %1.
OnlyOnThisPlatform=Ĉi tia programo devas funkcii en %1.
OnlyOnTheseArchitectures=Ĉi tia programo nur povas esti instalita en version de Vindoso por sekvaj procesoraj arkitekturoj:%n%n%1
WinVersionTooLowError=Por ĉi tia programo bezonas %1 version %2 aŭ pli olda.
WinVersionTooHighError=Ĉi tia programo ne povas esti instalita en %1 versio %2 aŭ pli olda.
AdminPrivilegesRequired=Vi devas eniri kiel administranto kiam instalas ĉi tian programon.
PowerUserPrivilegesRequired=Vi devas eniri kiel administranto aŭ kiel membro de grupo de Posedaj Uzantoj kiam instalas ĉi tia programo.
SetupAppRunningError=La Majstro difinis ke %1 nun funkcias.%n%nBonvolu ŝtopi ĝin, kaj poste kliku Jes por kontinui, aŭ Ŝtopi por eliri.
UninstallAppRunningError=Forigados difinis ke %1 nun funkcias.%n%nBonvolu ŝtopi ĝin, kaj poste kliku Jes por kontinui, aŭ Ŝtopi por eliri.

; *** Startup questions
PrivilegesRequiredOverrideTitle=Elektu Instala Reĝimo
PrivilegesRequiredOverrideInstruction=Elektu instala reĝimo
PrivilegesRequiredOverrideText1=%1 povas esti instalita por ĉiuj uzantoj (postulas administrajn privilegiojn), aŭ nur por vi. 
PrivilegesRequiredOverrideText2=%1 povas esti instalita nur por vi aŭ por ĉiuj uzantoj (postulas administrajn privilegiojn). 
PrivilegesRequiredOverrideAllUsers=Instali por ĉiuj &uzantoj
PrivilegesRequiredOverrideAllUsersRecommended=Instali por ĉiuj &uzantoj (rekomendita) 
PrivilegesRequiredOverrideCurrentUser=Instali nur por &mi
PrivilegesRequiredOverrideCurrentUserRecommended=Instali nur por &mi (rekomendita) 

; *** Misc. errors
ErrorCreatingDir=La Majstro ne povas krei dosierujon "%1"
ErrorTooManyFilesInDir=Estas nepoveble krei dosieron en dosierujo "%1" pro tio ke ĝi havas tro multe da dosierojn

; *** Setup common messages
ExitSetupTitle=Ŝtopo Majstron
ExitSetupMessage=La instalado ne estas plena. Se vi eliros nun, la programo ne estos instalita.%n%nPor vi bezonas ŝalti Majstron denove en alia tempo por plenumi instaladon.%n%nĈu fini la Majstron?
AboutSetupMenuItem=&Pri instalo...
AboutSetupTitle=Pri instalo
AboutSetupMessage=%1 version %2%n%3%n%n%1 hejma paĝo:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< &Reen
ButtonNext=&Antaŭen >
ButtonInstall=&Instali
ButtonOK=Jes
ButtonCancel=Ŝtopi
ButtonYes=&Jes
ButtonYesToAll=Jes por &ĉiaj
ButtonNo=&Ne
ButtonNoToAll=Ne por ĉiaj
ButtonFinish=&Fino
ButtonBrowse=&Elekto...
ButtonWizardBrowse=Elekto...
ButtonNewFolder=&Fari la novan dosierujon

; *** "Select Language" dialog messages
SelectLanguageTitle=Elektu la lingvon
SelectLanguageLabel=Elektu la lingvon por uzo dum instalado:

; *** Common wizard text
ClickNext=Kliku Antaŭen por kontinui, aŭ Ŝtopi por eliri Instaladon.
BeveledLabel=
BrowseDialogTitle=Elekto de dosierujo
BrowseDialogLabel=Elektu la dosierujon en listo malalte, kaj kliku Jes.
NewFolderName=Nova dosierujo

; *** "Welcome" wizard page
WelcomeLabel1=Bonvenon al Majstro de instalado de [name]
WelcomeLabel2=Nun komencos instalado de [name/ver] en via komputilo.%n%nEstas rekomendite ke vi ŝtopu ĉiajn viajn programojn antaŭ komenco.

; *** "Password" wizard page
WizardPassword=Pasvorto
PasswordLabel1=Ĉi tia instalado postulas pasvorton.
PasswordLabel3=Bonvolu tajpi pasvorton kaj poste kliku Antaŭen por kontinui. La pasvortoj estas tajp sentemaj.
PasswordEditLabel=&Pasvorto:
IncorrectPassword=La pasvorto, kian vi tajpis estas malĝusta. Bonvolu provi denove.

; *** "License Agreement" wizard page
WizardLicense=Licenza konvenio
LicenseLabel=Bonvolu legi sekvan gravan informacion antaŭ komenci.
LicenseLabel3=Bonvolu legi sekvan Licenzan Konvenion. Vi devas akcepti dotaĵoj de tia konvenio antaŭ ke kontinui instaladon.
LicenseAccepted=Mi akceptas konvenion
LicenseNotAccepted=Mi ne akceptas konvenion

; *** "Information" wizard pages
WizardInfoBefore=Informacio
InfoBeforeLabel=Bonvolu legi sekvan gravan informacion antaŭ komenci.
InfoBeforeClickLabel=Kiam vi estas preta por kontinui per instalo, kliku Antaŭen.
WizardInfoAfter=Informacio
InfoAfterLabel=Bonvolu legi sekvan gravan informacion antaŭ komenci.
InfoAfterClickLabel=Kiam vi estas preta por kontinui per instalo, kliku Antaŭen.

; *** "User Information" wizard page
WizardUserInfo=Informacio pri uzanto
UserInfoDesc=Bonvolu skribi vian informacion.
UserInfoName=Nomo de uzanto:
UserInfoOrg=&Organizacio:
UserInfoSerial=&Seria Numero:
UserInfoNameRequired=Vi devas skribi nomon de uzanto.

; *** "Select Destination Location" wizard page
WizardSelectDir=Elektu Destinan Locon
SelectDirDesc=Kie devos [name] esti instalita?
SelectDirLabel3=La Majstro instalos [name] en sekvan dosierujon.
SelectDirBrowseLabel=Por kontinui, kliku Antaŭen. Se vi volas elekti diversan dosierujon, kliku Elekto.
DiskSpaceGBLabel=Almenaŭ [gb] GB de neta diska spaco bezonas.
DiskSpaceMBLabel=Almenaŭ [mb] MB de neta diska spaco bezonas.
CannotInstallToNetworkDrive=Majstro ne povas instali lokan diskon.
CannotInstallToUNCPath=Majstro ne povas instali laŭ UNC vojo.
InvalidPath=Vi devas skribi plenan vojon de diska litero; por ekzamplo:%n%nC:\APP%n%sed ne UNC vojo laŭ formo:%n%n\\server\share
InvalidDrive=Disko aŭ UNC kian vi elektis ne ekzistas aŭ ne estas difinita. Bonvolu elekti denove.
DiskSpaceWarningTitle=Mankas Diskan Spacon
DiskSpaceWarning=Por instalo bezonas almenaŭ %1 KB de neta spaco por instalado, sed electita disko havas nur %2 KB.%n%nĈu vi volas kontinui per ĉiokaze?
DirNameTooLong=La nomo de dosierujo aŭ vojo estas tro longa.
InvalidDirName=La nomo de dosierujo estas malĝusta.
BadDirName32=La nomoj de dosierujoj ne povas havi de sekvaj karakteroj:%n%n%1
DirExistsTitle=Dosierujo ekzistas
DirExists=La dosierujo:%n%n%1%n%njam ekzistas. Ĉu vi volas instali en ĝi ĉiokaze?
DirDoesntExistTitle=La dosierujo ne ekzistas
DirDoesntExist=La dosierujo:%n%n%1%n%nne ekzistas. Ĉu vi volas por ke tia dosierujo estos farita?

; *** "Select Components" wizard page
WizardSelectComponents=Elektu komponentoj
SelectComponentsDesc=Kiaj komponentoj devas esti instalitaj?
SelectComponentsLabel2=Elektu komponentoj kiaj vi volas instali; forigu la komponentojn kiaj vi ne volas instali. Kliku Antaŭen kiam vi estas preta por kontinui.
FullInstallation=Tuta instalado
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Kompakta instalado
CustomInstallation=Kutima instalado
NoUninstallWarningTitle=Komponentoj ekzistas
NoUninstallWarning=La Majstro difinis ke sekvaj komponentoj jam estas instalitaj en via komputilo:%n%n%1%n%nNuligo de elekto de tiaj komponentoj ne forigos ĝin.%n%nĈu vi volas kontinui ĉiokaze?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=Nuna elekto bezonas almenaŭ [gb] GB de diska spaco.
ComponentsDiskSpaceMBLabel=Nuna elekto bezonas almenaŭ [mb] MB de diska spaco.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Elektu aldonaj taskoj
SelectTasksDesc=Kiaj aldonaj taskoj devos esti montrotaj?
SelectTasksLabel2=Elektu aldonaj taskoj kiaj bezonas por ke Majstro montros dum instalado [name], kaj poste kliku Antaŭen.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Elektu dosierujon de starta menuo
SelectStartMenuFolderDesc=Kie Majstro devas krei tujklavon de programo?
SelectStartMenuFolderLabel3=La Majstro kreos tujklavojn de programo en sekva dosierujo de starta menuo.
SelectStartMenuFolderBrowseLabel=Por kontinui, kliku Antaŭen. Se vi volas elekti alian dosierujon, kliku Elekto.
MustEnterGroupName=Vi devas skribi la nomo de dosierujo.
GroupNameTooLong=La nomo de dosierujo aŭ vojo estas tro longa.
InvalidGroupName=La nomo de dosierujo estas malĝusta.
BadGroupName=La nomoj de dosierujoj ne povas havi de sekvaj karakteroj:%n%n%1
NoProgramGroupCheck2=Ne krei dosierujon de starta menuo

; *** "Ready to Install" wizard page
WizardReady=Preparado por Instalo
ReadyLabel1=Nun ĉio estas preparita por komenci instaladon [name] en via komputilo.
ReadyLabel2a=Kliku Instali por kontinui instaladon, aŭkliku Reen se vi volas rigardi aŭ ŝanĝi ajnajn statojn.
ReadyLabel2b=Kliku Instali por kontinui instaladon.
ReadyMemoUserInfo=Informacio de uzanto:
ReadyMemoDir=Destina loko:
ReadyMemoType=Majstra tipo:
ReadyMemoComponents=Elektitaj komponentoj:
ReadyMemoGroup=La dosierujo de starta menuo:
ReadyMemoTasks=Aldonaj taskoj:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel=Elŝuti pliajn dosierojn...
ButtonStopDownload=&Stopu elŝuti
StopDownload=Ĉu vi certas, ke vi volas stopi la elŝuton?
ErrorDownloadAborted=Elŝuto ĉesigita 
ErrorDownloadFailed=Elŝuto malsukcesis: %1 %2 
ErrorDownloadSizeFailed=Akiri grandecon malsukcesis: %1 %2 
ErrorFileHash1=Dosiero haŝkodo malsukcesis: %1
ErrorFileHash2=Nevalida dosiero haŝkodo: atendita %1, trovita %2
ErrorProgress=Nevalida progreso: %1 de %2 
ErrorFileSize=Nevalida dosiergrandeco: atendita %1, trovita %2

; *** "Preparing to Install" wizard page
WizardPreparing=Preparado por Instalo
PreparingDesc=Majstro estas preparata por instalo [name] en via komputilo.
PreviousInstallNotCompleted=Instalado/Forigo de antaŭa programo ne estas plena. Por vi bezonas relanĉi vian komputilon por plenigi tian instaladon.%n%nPost relanĉo de via komputilo, ŝaltu Majstron denove por finigi instaladon de [name].
CannotContinue=La Majstro ne povas kontinui. Bonvolu kliki Fino por eliri.
ApplicationsFound=Sekvaj aplikaĵoj uzas dosierojn kiajn bezonas renovigi per Instalado. Estas rekomendite ke vi permesu al Majstro automate fermi tiajn aplikaĵojn.
ApplicationsFound2=Sekvaj aplikaĵoj uzas dosierojn kiajn bezonas renovigi per Instalado. Estas rekomendite ke vi permesu al Majstro automate fermi tiajn aplikaĵojn. Poste de instalado Majstro provos relanĉi aplikaĵojn.
CloseApplications=&Automate fermi aplikaĵojn
DontCloseApplications=Ne fermu aplikaĵojn
ErrorCloseApplications=Majstro estis nepovebla aŭtomate fermi ĉiajn aplikaĵojn. Estas rekomendite ke vi fermu ĉiajn aplikaĵojn, uzantaj dosierojn, kiaj estas bezonatajn por renovigo per la Majstro antaŭ kontinui.
PrepareToInstallNeedsRestart=Instalado devas restartigi vian komputilon. Post restartigi vian komputilon, rulu denove Instaladon por kompletigi la instaladon de [name].%n%nĈu vi ŝatus rekomenci nun?

; *** "Installing" wizard page
WizardInstalling=Instalado
InstallingLabel=Bonvolu atenti dum Majstro instalas [name] en via komputilo.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Fino de instalado
FinishedLabelNoIcons=La Majstro finigis instaladon [name] en via komputilo.
FinishedLabel=La Majstro finigis instaladon [name] en via komputilo. La aplikaĵo povos esti lanĉita per elekto de instalaj ikonoj.
ClickFinish=Kliku Fino por finigi instaladon.
FinishedRestartLabel=Por plenumigi instaladon de [name], Majstro devas relanĉi vian komputilon. Ĉu vi volas relanĉi nun?
FinishedRestartMessage=Por plenumigi instaladon de [name], Majstro devas relanĉi vian komputilon.%n%nĈu vi volas relanĉi nun?
ShowReadmeCheck=Jes, mi volas rigardi dosieron README
YesRadio=&Jes, relanĉu komputilon nun
NoRadio=&Ne, mi volas relanĉi komputilon poste
; used for example as 'Run MyProg.exe'
RunEntryExec=Ŝaltu %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Rigardi %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=La Majstro postulas sekvan diskon
SelectDiskLabel2=Bonvolu inserti Diskon %1 kaj kliku Jes.%n%nSe dosieroj en tia disko povos esti diversaj de prezentitaj malalte, enskribu korektan vojon aŭ kliku Elekto.
PathLabel=&Vojo:
FileNotInDir2=Dosieron "%1" estas nepoveble lokigi en "%2". Bonvolu inserti korectan diskon aŭ elektu alian dosierujon.
SelectDirectoryLabel=Bonvolu difini lokon de alia disko.

; *** Installation phase messages
SetupAborted=Instalado ne estis plena.%n%nBonvolu korekti problemon kaj lanĉu Majstron denove.
AbortRetryIgnoreSelectAction=Elektu agon
AbortRetryIgnoreRetry=&Provu denove
AbortRetryIgnoreIgnore=&Ignori la eraron kaj daŭrigi 
AbortRetryIgnoreCancel=Nuligi instaladon

; *** Installation status messages
StatusClosingApplications=Fermado de aplikaĵoj...
StatusCreateDirs=Kreado de dosierujojn...
StatusExtractFiles=Ekstraktado de dosierojn...
StatusCreateIcons=Kreado de tujklavojn...
StatusCreateIniEntries=Kreado de INI dosierojn...
StatusCreateRegistryEntries=Kreado de registraj pointoj...
StatusRegisterFiles=Registrado de dosierojn...
StatusSavingUninstall=Konservas informacio por forigo...
StatusRunProgram=Finiĝas instalado...
StatusRestartingApplications=Relanĉo de aplikaĵoj...
StatusRollback=Renovigo de ŝanĝoj...

; *** Misc. errors
ErrorInternal2=Interna eraro: %1
ErrorFunctionFailedNoCode=%1 estas kripligita
ErrorFunctionFailed=%1 estas kripligita; kodnomo %2
ErrorFunctionFailedWithMessage=%1 estas kripligita; kodnomo %2.%n%3
ErrorExecutingProgram=Estas nepoveble plenumi dosieron:%n%1

; *** Registry errors
ErrorRegOpenKey=Eraro dum malfermo de registra ŝlosilo:%n%1\%2
ErrorRegCreateKey=Eraro dum kreado de registra ŝlosilo:%n%1\%2
ErrorRegWriteKey=Eraro dum skribado en registra ŝlosilo:%n%1\%2

; *** INI errors
ErrorIniEntry=Eraro dum kreado de INI pointo en dosiero "%1".

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=&Preterpasi ĉi tiun dosieron (ne rekomendita)
FileAbortRetryIgnoreIgnoreNotRecommended=&Ignori la eraron kaj daŭrigi (ne rekomendita)
SourceIsCorrupted=La fonta dosiero estas kripligita
SourceDoesntExist=La fonta dosiero "%1" ne ekzistas
ExistingFileReadOnly=Ekzistanta dosiero estas markita nurlega.%n%nAlklaku Reen por forigi la nurlegeblan atributon kaj provu denove, Ignori por transsalti la dosieron aŭ Ĉesigi por kompletigi la instaladon. 
ExistingFileReadOnly2=La ekzistanta dosiero ne povis esti anstataŭigita ĉar ĝi estas markita nurlega.
ExistingFileReadOnlyRetry=&Forigu la nurlegeblan atributon kaj reprovu
ExistingFileReadOnlyKeepExisting=&Konservu la ekzistantan dosieron
ErrorReadingExistingDest=Eraro aperis dum legado de ekzista dosiero:
FileExistsSelectAction=Elekti agon
FileExists2=La dosiero jam ekzistas.
FileExistsOverwriteExisting=&Anstataŭigu la ekzistantan dosieron
FileExistsKeepExisting=&Konservu la ekzistantan dosieron
FileExistsOverwriteOrKeepAll=&Faru ĉi tion por la venontaj konfliktoj
ExistingFileNewerSelectAction=Elekti agon
ExistingFileNewer2=La ekzistanta dosiero estas pli nova ol tiu instalilo provas instali.
ExistingFileNewerOverwriteExisting=&Anstataŭigu la ekzistantan dosieron
ExistingFileNewerKeepExisting=&Konservu la ekzistantan dosieron (recomendita)
ExistingFileNewerOverwriteOrKeepAll=&Faru ĉi tion por la venontaj konfliktoj
ErrorChangingAttr=Eraro aperis dum provo ĉanĝi atributoj de ekzista dosiero:
ErrorCreatingTemp=Eraro aperis dum kreado dosieron en destina dosierujo:
ErrorReadingSource=Eraro aperis dum legado de dosiero:
ErrorCopying=Eraro aperis dum kopiado de dosiero:
ErrorReplacingExistingFile=Eraro aperis dum relokiĝo de ekzistan dosieron:
ErrorRestartReplace=Relanĉo/Relokiĝo estas kripligita:
ErrorRenamingTemp=Eraro aperis dum renomiĝo del dosiero en destina dosierujo:
ErrorRegisterServer=Estas nepoveble registri DLL/OĈ: %1
ErrorRegSvr32Failed=RegSvr32estas kripligita kun elira codo %1
ErrorRegisterTypeLib=Estas nepoveble registri bibliotekon de tipo : %1

; *** Uninstall display name markings
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32-bita
UninstallDisplayNameMark64Bit=64-bita
UninstallDisplayNameMarkAllUsers=Ĉiuj uzantoj
UninstallDisplayNameMarkCurrentUser=Nuna uzanto

; *** Post-installation errors
ErrorOpeningReadme=Eraro aperis dum malfermado de README dosiero.
ErrorRestartingComputer=Majstro ne povis relanĉi komputilo. Bonvolu fari tion permane.

; *** Uninstaller messages
UninstallNotFound=Dosiero "%1" ne ekzistas. Estas nepoveble forigi.
UninstallOpenError=Dosieron "%1" nepoveble estas malfermi. Estas nepoveble forigi
UninstallUnsupportedVer=\Foriga protokolo "%1" estas en nekonata formato per ĉi tia versio de forigprogramo. Estas nepoveble forigi
UninstallUnknownEntry=Ekzistas nekonata pointo (%1) en foriga protokolo
ConfirmUninstall=Ĉu vi reale volas tute forigi %1 kaj ĉiaj komponentoj de ĝi?
UninstallOnlyOnWin64=Ĉi tian instaladon povos forigi nur en 64-bit Vindoso.
OnlyAdminCanUninstall=Ĉi tian instaladon povos forigi nur uzanto kun administrantaj rajtoj.
UninstallStatusLabel=Bonvolu atendi dum %1 foriĝos de via komputilo.
UninstalledAll=%1 estis sukcese forigita de via komputilo.
UninstalledMost=Forigo de %1 estas plena.%n%nKelkaj elementoj ne estis forigitaj. Ĝin poveble estas forigi permane.
UninstalledAndNeedsRestart=Por plenumi forigadon de %1, via komputilo devas esti relanĉita.%n%nĈu vi volas relanĉi nun?
UninstallDataCorrupted="%1" dosiero estas kriplita. Estas nepoveble forigi

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Forigi komune uzatan dosieron?
ConfirmDeleteSharedFile2=La sistemo indikas ke sekva komune uzata dosiero jam ne estas uzata per neniel aplikaĵoj. Ĉu vi volas forigi ĉi tian dosieron?%n%nSe ajna programo jam uzas tian dosieron, dum forigo ĝi povos malĝuste funkcii. Se vi ne estas certa elektu Ne. Restante en via sistemo la dosiero ne damaĝos ĝin.
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

NameAndVersion=%1 versio %2
AdditionalIcons=Aldonaj ikonoj:
CreateDesktopIcon=Krei &Labortablan ikonon
CreateQuickLaunchIcon=Krei &Rapida lanĉo ikonon
ProgramOnTheWeb=%1 en Reto
UninstallProgram=Roriĝado %1
LaunchProgram=Lanĉo %1
AssocFileExtension=&Asociigi %1 kun %2 dosieraj finaĵoj
AssocingFileExtension=Asociiĝas %1 kun %2 dosiera finaĵo...
AutoStartProgramGroupDescription=Lanĉo:
AutoStartProgram=Automate ŝalti %1
AddonHostProgramNotFound=%1 nepoveble estas loki en dosierujo kian vi elektis.%n%nĈu vi volas kontinui ĉiokaze?
