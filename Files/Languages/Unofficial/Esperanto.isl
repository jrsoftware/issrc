; *** Inno Setup version 6.5.0+ Esperanto messages ***
;
;
;       Aŭtoro: Alexander Gritĉin  (Retpoŝto - alexgrimo@mail.ru)
;
;       Nuna traduko      -       02.09.2026
;       Parte aldonita traduko per Wolfgang Pohl  (Retpoŝto: software@interpohl.net)
;       -  15.04.2021
;       La unua traduko    -     08.06.2015
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
LanguageCodePage=28593

; If the language you are translating to requires special font faces or
; sizes, uncomment any of the following entries and change them accordingly.
;DialogFontName=
;DialogFontSize=9
;DialogFontBaseScaleWidth=7
;DialogFontBaseScaleHeight=15
;WelcomeFontName=Segoe UI
;WelcomeFontSize=14

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
SetupLdrStartupMessage=Nun estos instalado de %1. Ĉu vi volas daŭrigi?
LdrCannotCreateTemp=Nepoveble estas krei tempan dosieron. La Majstro estas haltigita
LdrCannotExecTemp=Nepoveble estas plenumi la dosieron en tempa dosierujo. La Majstro estas haltigita
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nEraro %2: %3
SetupFileMissing=La dosiero %1 estas preterpasita el instala dosierujo. Bonvolu korekti problemon aŭ ricevu novan kopion de programo.
SetupFileCorrupt=Instalaj dosieroj estas kriplitaj. Bonvolu ricevi novan kopion de programo.
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

; 'Power Users group' is an outdated term but should still be translated, not dropped or modernized
PowerUserPrivilegesRequired=Vi devas eniri kiel administranto aŭ kiel membro de grupo de Posedaj Uzantoj kiam instalas ĉi tia programo.

; 'instance' may also be translated as 'copy'
SetupAppRunningError=La Majstro difinis ke %1 nun funkcias.%n%nBonvolu fermi ĉiujn okazojn de ĝi nun, kaj poste kliku Jes por daŭrigi, aŭ Haltigi por eliri.
UninstallAppRunningError=Forigado difinis ke %1 nun funkcias.%n%nBonvolu fermi ĉiujn okazojn de ĝi nun, kaj poste kliku Jes por daŭrigi, aŭ Haltigi por eliri.

; *** Startup questions
PrivilegesRequiredOverrideTitle=Elektu reĝimon por instalo
PrivilegesRequiredOverrideInstruction=Elektu instalan reĝimon
PrivilegesRequiredOverrideText1=%1 povas esti instalita por ĉiuj uzantoj (bezonas administrajn privilegiojn), aŭ nur por vi.
PrivilegesRequiredOverrideText2=%1 povas esti instalita nur por vi aŭ por ĉiuj uzantoj (bezonas administrajn privilegiojn).
PrivilegesRequiredOverrideAllUsers=Instali por ĉiuj &uzantoj
PrivilegesRequiredOverrideAllUsersRecommended=Instali por ĉiuj &uzantoj (estas rekomendite)
PrivilegesRequiredOverrideCurrentUser=Instali nur por &mi
PrivilegesRequiredOverrideCurrentUserRecommended=Instali nur por &mi (estas rekomendite)

; *** Misc. errors
ErrorCreatingDir=La Majstro ne povas krei dosierujon "%1"
ErrorTooManyFilesInDir=Estas nepoveble krei dosieron en dosierujo "%1" pro tio ke ĝi havas tro multe da dosierojn

; *** Setup common messages
ExitSetupTitle=Eliri de Majstro
ExitSetupMessage=La instalado ne estas plena. Se vi eliros nun, la programo ne estos instalita.%n%nPor vi bezonas ŝalti Majstron denove en alia tempo por plenumi instaladon.%n%nĈu fini la Majstron?
AboutSetupMenuItem=Pr&i instalo...
AboutSetupTitle=Pri instalo
AboutSetupMessage=%1 versio %2%n%3%n%n%1 hejma paĝo:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< &Reen
ButtonNext=&Antaŭen >
ButtonInstall=&Instali
ButtonOK=Jes
ButtonCancel=Haltigi
ButtonYes=&Jes
ButtonYesToAll=Jes por &ĉiuj
ButtonNo=&Ne
ButtonNoToAll=N&e por ĉiuj
ButtonFinish=&Fino
ButtonBrowse=&Elekto...
ButtonWizardBrowse=E&lekto...
ButtonNewFolder=&Fari la novan dosierujon

; *** "Select Language" dialog messages
SelectLanguageTitle=Elektu la lingvon
SelectLanguageLabel=Elektu la lingvon por uzo dum instalado.

; *** Common wizard text
ClickNext=Kliku Antaŭen por daŭrigi, aŭ Halti por eliri Instaladon.
BeveledLabel=
BrowseDialogTitle=Elekto de dosierujo
BrowseDialogLabel=Elektu la dosierujon en listo malalte, kaj kliku Jes.
NewFolderName=Nova dosierujo

; *** "Welcome" wizard page
WelcomeLabel1=Bonvenon al Majstro de instalado de [name]
WelcomeLabel2=Nun komencos instalado de [name/ver] en via komputilo.%n%nEstas rekomendite ke vi haltigu ĉiajn viajn programojn antaŭ daŭrigo.

; *** "Password" wizard page
WizardPassword=Pasvorto
PasswordLabel1=Ĉi tia instalado postulas pasvorton.
PasswordLabel3=Bonvolu tajpi pasvorton kaj poste kliku Antaŭen por daŭrigi. La pasvortoj estas tajp sentemaj.
PasswordEditLabel=&Pasvorto:
IncorrectPassword=La pasvorto, kian vi tajpis estas malĝusta. Bonvolu provi denove.

; *** "License Agreement" wizard page
WizardLicense=Licenza konvenio
LicenseLabel=Bonvolu legi sekvan gravan informacion antaŭ daŭrigo.
LicenseLabel3=Bonvolu legi sekvan Licenzan Konvenion. Vi devas akcepti dotaĵoj de tia konvenio antaŭ ke daŭrigi instaladon.
LicenseAccepted=Mi a&kceptas konvenion
LicenseNotAccepted=Mi &ne akceptas konvenion

; *** "Information" wizard pages
WizardInfoBefore=Informacio
InfoBeforeLabel=Bonvolu legi sekvan gravan informacion antaŭ daŭrigo.
InfoBeforeClickLabel=Kiam vi estas preta por daŭrigi per instalo, kliku Antaŭen.
WizardInfoAfter=Informacio
InfoAfterLabel=Bonvolu legi sekvan gravan informacion antaŭ komenci.
InfoAfterClickLabel=Kiam vi estas preta por daŭrigi per instalo, kliku Antaŭen.

; *** "User Information" wizard page
WizardUserInfo=Informacio pri uzanto
UserInfoDesc=Bonvolu skribi vian informacion.
UserInfoName=&Nomo de uzanto:
UserInfoOrg=&Organizacio:
UserInfoSerial=&Seria Numero:
UserInfoNameRequired=Vi devas skribi nomon de uzanto.

; *** "Select Destination Location" wizard page
WizardSelectDir=Elektu Destinan Lokon
SelectDirDesc=Kie devos [name] esti instalita?
SelectDirLabel3=La Majstro instalos [name] en sekvan dosierujon.
SelectDirBrowseLabel=Por daŭrigi, kliku Antaŭen. Se vi volas elekti diversan dosierujon, kliku Elekto.
DiskSpaceGBLabel=Almenaŭ [gb] GB de neta diska spaco bezonas.
DiskSpaceMBLabel=Almenaŭ [mb] MB de neta diska spaco bezonas.
CannotInstallToNetworkDrive=Majstro ne povas instali retan diskon.
CannotInstallToUNCPath=Majstro ne povas instali laŭ UNC vojo.
InvalidPath=Vi devas skribi plenan vojon de diska litero; por ekzemplo:%n%nC:\APP%n%naŭ UNC vojo laŭ formo:%n%n\\servilo\interŝanĝo
InvalidDrive=Disko aŭ UNC vojo kian vi elektis ne ekzistas aŭ ne estas difinita. Bonvolu elekti denove.
DiskSpaceWarningTitle=Mankas Diskan Spacon
DiskSpaceWarning=Por instalo bezonas almenaŭ %1 KB de neta spaco por instalado, sed elektita disko havas nur %2 KB.%n%nĈu vi volas daŭrigi ĉiokaze?
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
SelectComponentsLabel2=Elektu komponentojn kiujn vi volas instali; forigu la komponentojn kiujn vi ne volas instali. Kliku Antaŭen kiam vi estas preta por daŭrigi.

; don't translate 'Full' as 'Normal' or 'Default'
FullInstallation=Tuta instalado

; don't translate 'Compact' as 'Minimal' or 'Default'
CompactInstallation=Kompakta instalado
CustomInstallation=Kutima instalado
NoUninstallWarningTitle=Komponentoj ekzistas
NoUninstallWarning=La Majstro difinis ke sekvaj komponentoj jam estas instalitaj en via komputilo:%n%n%1%n%nDeselekto de tiuj komponentoj ne forigos ĝin.%n%nĈu vi volas daŭrigi ĉiokaze?
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
SelectStartMenuFolderBrowseLabel=Por daŭrigi, kliku Antaŭen. Se vi volas elekti alian dosierujon, kliku Elekto.
MustEnterGroupName=Vi devas skribi la nomo de dosierujo.
GroupNameTooLong=La nomo de dosierujo aŭ vojo estas tro longa.
InvalidGroupName=La nomo de dosierujo estas malĝusta.
BadGroupName=La nomo de dosierujo ne povas havi de sekvaj karakteroj:%n%n%1
NoProgramGroupCheck2=&Ne krei dosierujon de starta menuo

; *** "Ready to Install" wizard page
WizardReady=Preparado por Instalo
ReadyLabel1=Nun ĉio estas preparita por komenci instaladon [name] en via komputilo.
ReadyLabel2a=Kliku Instali por daŭrigi instaladon, aŭ kliku Reen se vi volas rigardi aŭ ŝanĝi ajnajn statojn.
ReadyLabel2b=Kliku Instali por daŭrigi instaladon.
ReadyMemoUserInfo=Informacio de uzanto:
ReadyMemoDir=Destina loko:
ReadyMemoType=Majstra tipo:
ReadyMemoComponents=Elektitaj komponentoj:
ReadyMemoGroup=La dosierujo de starta menuo:
ReadyMemoTasks=Aldonaj taskoj:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel2=Elŝuto de dosieroj...
ButtonStopDownload=H&altigi elŝuton
StopDownload=Ĉu vi reale volas haltigi elŝuton?
ErrorDownloadAborted=Elŝuto estas haltigita
ErrorDownloadFailed=Elŝuto malsukcesis: %1 %2
ErrorDownloadSizeFailed=Akiri grandecon malsukcesis: %1 %2
ErrorProgress=Progreso estas malĝusta: %1 de %2
ErrorFileSize=Dosiera grandeco estas malĝusta: atendita %1, trovita %2

; *** TExtractionWizardPage wizard page and ExtractArchive
ExtractingLabel=Eltiro de dosieroj...
ButtonStopExtraction=H&altigi eltiron
StopExtraction=Ĉu vi reale volas haltigi eltiron?
ErrorExtractionAborted=Eltiro estas haltigita
ErrorExtractionFailed=Eltiro malsukcesis: %1

; *** Archive extraction failure details
ArchiveIncorrectPassword=Pasvorto estas malĝusta
ArchiveIsCorrupted=Arkivo estas kriplita
ArchiveUnsupportedFormat=Arkiva formato ne estas subtenata

; *** "Preparing to Install" wizard page
WizardPreparing=Preparado por Instalo
PreparingDesc=Majstro estas preparata por instalo [name] en via komputilo.
PreviousInstallNotCompleted=Instalado/Forigo de antaŭa programo ne estas plena. Por vi bezonas relanĉi vian komputilon por plenigi tian instaladon.%n%nPost relanĉo de via komputilo, ŝaltu Majstron denove por finigi instaladon de [name].
CannotContinue=La Majstro ne povas daŭrigi. Bonvolu kliki Fino por eliri.
ApplicationsFound=Sekvaj aplikaĵoj uzas dosierojn kiajn bezonas renovigi per Instalado. Estas rekomendite ke vi permesu al Majstro aŭtomate fermi tiajn aplikaĵojn.
ApplicationsFound2=Sekvaj aplikaĵoj uzas dosierojn kiajn bezonas renovigi per Instalado. Estas rekomendite ke vi permesu al Majstro aŭtomate fermi tiajn aplikaĵojn. Poste de instalado Majstro provos relanĉi aplikaĵojn.
CloseApplications=&Aŭtomate fermi aplikaĵojn
DontCloseApplications=&Ne fermu aplikaĵojn
ErrorCloseApplications=Majstro estis nepovebla aŭtomate fermi ĉiajn aplikaĵojn. Estas rekomendite ke vi fermu ĉiajn aplikaĵojn, uzantaj dosierojn, kiuj estas bezonataj por renovigo per la Majstro antaŭ daŭrigo.
PrepareToInstallNeedsRestart=Majstro devas relanĉi vian komputilon. Post relanĉo, ŝaltu Majstron denove por plenumi instaladon de [name].%n%nĈu vi volas relanĉi nun?

; *** "Installing" wizard page
WizardInstalling=Instalado
InstallingLabel=Bonvolu atenti dum Majstro instalas [name] en via komputilo.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Fino de instalado [name]
FinishedLabelNoIcons=La Majstro finigis instaladon [name] en via komputilo.
FinishedLabel=La Majstro finigis instaladon [name] en via komputilo. La aplikaĵo povos esti lanĉita per elekto de instalaj tujklavoj.
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
SelectDiskLabel2=Bonvolu inserti Diskon %1 kaj kliku Jes.%n%nSe dosieroj en tia disko lokiĝas en dosierujo diversa de montrita malalte, enskribu korektan vojon aŭ kliku Elekto.
PathLabel=&Vojo:
FileNotInDir2=Dosieron "%1" estas nepoveble lokigi en "%2". Bonvolu inserti korektan diskon aŭ elektu alian dosierujon.
SelectDirectoryLabel=Bonvolu difini lokon de alia disko.

; *** Installation phase messages
SetupAborted=Instalado ne estis plena.%n%nBonvolu korekti problemon kaj lanĉu Majstron denove.
AbortRetryIgnoreSelectAction=Elektu agon
AbortRetryIgnoreRetry=P&rovi denove
AbortRetryIgnoreIgnore=&Ignori eraron kaj daŭrigi
AbortRetryIgnoreCancel=Haltigi instaladon
RetryCancelSelectAction=Elektu agon
RetryCancelRetry=P&rovi denove
RetryCancelCancel=Haltigi

; *** Installation status messages
StatusClosingApplications=Fermado de aplikaĵoj...
StatusCreateDirs=Kreado de dosierujoj...
StatusExtractFiles=Eltiro de dosieroj...
StatusDownloadFiles=Elŝuto de dosieroj...
StatusCreateIcons=Kreado de tujklavoj...
StatusCreateIniEntries=Kreado de INI dosieroj...
StatusCreateRegistryEntries=Kreado de registraj pointoj...
StatusRegisterFiles=Registrado de dosieroj...
StatusSavingUninstall=Konservas informacio por forigo...
StatusRunProgram=Finiĝas instalado...
StatusRestartingApplications=Relanĉo de aplikaĵoj...
StatusRollback=Malfari ŝanĝojn...

; *** Misc. errors
ErrorInternal2=Interna eraro: %1
ErrorFunctionFailedNoCode=%1 malsukcesis
ErrorFunctionFailed=%1 malsukcesis; kodnomo %2
ErrorFunctionFailedWithMessage=%1 malsukcesis; kodnomo %2.%n%3
ErrorExecutingProgram=Estas nepoveble plenumi dosieron:%n%1

; *** Registry errors
ErrorRegOpenKey=Eraro dum malfermo de registra ŝlosilo:%n%1\%2
ErrorRegCreateKey=Eraro dum kreado de registra ŝlosilo:%n%1\%2
ErrorRegWriteKey=Eraro dum skribado en registra ŝlosilo:%n%1\%2

; *** INI errors
ErrorIniEntry=Eraro dum kreado de INI pointo en dosiero "%1".

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=&Preterpasi ĉi tiun dosieron (ne estas rekomendite)
FileAbortRetryIgnoreIgnoreNotRecommended=&Ignori eraron kaj daŭrigi (ne estas rekomendite)
SourceIsCorrupted=La fonta dosiero estas kripligita
SourceDoesntExist=La fonta dosiero "%1" ne ekzistas
SourceVerificationFailed=Konfirmo de fonta dosiero malsukcesis: %1
VerificationSignatureDoesntExist=Subskribodosiero "%1" ne ekzistas
VerificationSignatureInvalid=Subskribodosiero "%1" estas malvalida
VerificationKeyNotFound=Subskribodosiero "%1" havas nekonatan ŝlosilon
VerificationFileNameIncorrect=La nomo de dosiero estas malĝusta
VerificationFileTagIncorrect=La etikedo de dosiero estas malĝusta
VerificationFileSizeIncorrect=La grandeco de dosiero estas malĝusta
VerificationFileHashIncorrect=La haŝo de dosiero estas malĝusta
ExistingFileReadOnly2=La ekzistantan dosieron nepoveble estas anstataŭigi pro tio ke ĝi estas markita nurlega.
ExistingFileReadOnlyRetry=&Forigi nurlegeblan atributon kaj provi denove
ExistingFileReadOnlyKeepExisting=&Konservi ekzistantan dosieron
ErrorReadingExistingDest=Eraro aperis dum legado de ekzistanta dosiero:
FileExistsSelectAction=Elekti agon
FileExists2=La dosiero jam ekzistas.
FileExistsOverwriteExisting=&Anstataŭigi ekzistantan dosieron
FileExistsKeepExisting=&Konservi ekzistantan dosieron
FileExistsOverwriteOrKeepAll=&Fari tion por la sekvaj konfliktoj
ExistingFileNewerSelectAction=Elekti agon
ExistingFileNewer2=Ekzistanta dosiero estas pli nova ol tiu, kiun Majstro provas instali.
ExistingFileNewerOverwriteExisting=&Anstataŭigi ekzistantan dosieron
ExistingFileNewerKeepExisting=&Konservi ekzistantan dosieron (estas rekomendite)
ExistingFileNewerOverwriteOrKeepAll=&Fari tion por la sekvaj konfliktoj
ErrorChangingAttr=Eraro aperis provante ŝanĝi atributojn de ekzistanta dosiero:
ErrorCreatingTemp=Eraro aperis provante krei dosieron en destina dosierujo:
ErrorReadingSource=Eraro aperis provante legi fontan dosieron:
ErrorCopying=Eraro aperis provante kopii dosieron:
ErrorDownloading=Eraro aperis provante elŝuti dosieron:
ErrorExtracting=Eraro aperis provante eltiri arkivon:
ErrorReplacingExistingFile=Eraro aperis provante anstataŭigi ekzistantan dosieron:

; 'RestartReplace' is an internal name, you may keep it as is
ErrorRestartReplace=Relanĉo/Anstataŭigo malsukcesis:
ErrorRenamingTemp=Eraro aperis provante renomi dosieron en destina dosierujo:
ErrorRegisterServer=Estas nepoveble registri DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 malsukcesis kun elira kodo %1
ErrorRegisterTypeLib=Estas nepoveble registri bibliotekon de tipo: %1

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
ErrorOpeningReadme=Eraro aperis provante malfermi README dosieron.
ErrorRestartingComputer=Majstro ne povis relanĉi komputilon. Bonvolu fari tion permane.

; *** Uninstaller messages
UninstallNotFound=Dosiero "%1" ne ekzistas. Estas nepoveble forigi.
UninstallOpenError=Dosieron "%1" nepoveble estas malfermi. Estas nepoveble forigi
UninstallUnsupportedVer=Foriga protokolo "%1" estas en nekonata formato per ĉi tia versio de forigprogramo. Estas nepoveble forigi
UninstallUnknownEntry=Ekzistas nekonata pointo (%1) en foriga protokolo
ConfirmUninstall=Ĉu vi reale volas tute forigi %1 kaj ĉiaj komponentoj de ĝi?
UninstallOnlyOnWin64=Ĉi tian instaladon povos forigi nur en 64-bita Vindoso.
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
AdditionalIcons=Aldonaj tujklavoj:
CreateDesktopIcon=Krei &Labortablan ikonon
CreateQuickLaunchIcon=Krei &Rapida lanĉo ikonon
ProgramOnTheWeb=%1 en Reto
UninstallProgram=Forigado %1
LaunchProgram=Lanĉo %1
AssocFileExtension=&Asociigi %1 kun %2 dosieraj finaĵoj
AssocingFileExtension=Asociiĝas %1 kun %2 dosiera finaĵo...
AutoStartProgramGroupDescription=Lanĉo:
AutoStartProgram=Automate ŝalti %1
AddonHostProgramNotFound=%1 nepoveble estas loki en dosierujo kian vi elektis.%n%nĈu vi volas daŭrigi ĉiokaze?
