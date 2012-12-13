;Agris Ausejs 
;******************************************************
; ***                                                ***
; *** Inno Setup version 5.5.1+ Latvian messages    ***
; ***                                                ***
; *** Original Author:                               ***
; ***                                                ***
; ***   Agris Ausejs (oby2005@gmail.com)             ***
; ***                                                ***
; ***  02/22/2008                                    ***
; ******************************************************
;
;
; To download user-contributed translations of this file, go to:
;   http://www.jrsoftware.org/is3rdparty.php
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).

[LangOptions]
; The following three entries are very important. Be sure to read and 
; understand the '[LangOptions] section' topic in the help file.
LanguageName=Latviski
LanguageID=$0426
LanguageCodePage=1257
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
SetupAppTitle=Uzstâdîðana
SetupWindowTitle=Uzstâdîðana - %1
UninstallAppTitle=Atinstalâcija
UninstallAppFullTitle=%1 Atinstalâcija

; *** Misc. common
InformationTitle=Informâcija
ConfirmTitle=Apstiprinât
ErrorTitle=Kïûda

; *** SetupLdr messages
SetupLdrStartupMessage=Tiks uzstâdîta programma %1. Vai vçlaties turpinât?
LdrCannotCreateTemp=Neiespçjami izveidot pagaidu datnes. Uzstâdîðana pârtraukta
LdrCannotExecTemp=Neiespçjami palaist datni no pagaidu mapes. Uzstâdîðana pârtraukta

; *** Startup error messages
LastErrorMessage=%1.%n%nKïûda %2: %3
SetupFileMissing=Datne %1 nav atrodama instalâcijas mapç. Lûdzu, izlabojiet kïûdu vai iegâdâjieties jaunu programmas kopiju.
SetupFileCorrupt=Uzstâdâmâs datnes ir sabojâtas. Lûdzu, iegâdâjieties jaunu programmas kopiju.
SetupFileCorruptOrWrongVer=Uzstâdâmâs datnes ir bojâtas vai nav savienojamas ar ðo Uzstâdîðanas programmu. Lûdzu, izlabojiet ðo kïûdu vai iegâdâjieties jaunu programmas kopiju.
InvalidParameter=Nederîgs parametrs tika pieòemts uz komandrindas:%n%n%1
SetupAlreadyRunning=Uzstâdîðana jau darbojas.
WindowsVersionNotSupported=Ðî programma neatbalsta Windows versiju datorâ darbojas.
WindowsServicePackRequired=Ðî programma pieprasa %1 servisa pakotnes %2 vai jaunâka.
NotOnThisPlatform=Ðo programmu nevar palaist uz %1.
OnlyOnThisPlatform=Ðî programma darbojas uz %1.
OnlyOnTheseArchitectures=Ðo programmu var uzstâdît tikai uz ðâdâm Windows versijâm:%n%n%1
MissingWOW64APIs=Paðlaik palaistâ Windows versija neatbalsta 64-bitu instalâciju. Lai izlabotu ðo kïûdu, uzinstalçjiet Service Pack %1.
WinVersionTooLowError=Ðî programma pieprasa %1 versiju %2 vai jaunâku.
WinVersionTooHighError=Ðo programmu nevar uzstâdît uz %1 versijas %2 vai jaunâkas.
AdminPrivilegesRequired=Jums ir jâbût adminstratoram, lai varçtu uzsâkt instalâciju.
PowerUserPrivilegesRequired=Jums ir jâbût administratoram vai pilnvarotam lietotâjam, lai uzstâdîtu ðo programmu.
SetupAppRunningError=Uzstâdîðana ir atklâjusi, ka %1 paðlaik darbojas.%n%nLûdzu, aizveriet visas programmas un spiediet "Ok" vai "Atcelt", lai izietu.
UninstallAppRunningError=Atinstalâcija ir atklâjusi ka %1 paðlaik darbojas.%n%nLûdzu, aizveriet visas programmas un spiediet "Ok", lai turpinâtu, vai "Atcelt", lai izietu.

; *** Misc. errors
ErrorCreatingDir=Uzstâdîðanâ ir neiespçjami izveidot mapi "%1"
ErrorTooManyFilesInDir=Neiespçjami izveidot datnes mapç "%1", jo tâ satur pârâk daudz datòu

; *** Setup common messages
ExitSetupTitle=Iziet no Uzstâdîðanas
ExitSetupMessage=Uzstâdîðana nav pabeigta. Ja Jûs tagad iziesiet, tad programma netiks uzinstalçta.%n%nJums bûs atkal jâpalaiþ Uzstâdîðana, lai pabeigtu programmas instalâciju.%n%nIziet no Uzstâdîðanas?
AboutSetupMenuItem=&Par Uzstâdîðanu...
AboutSetupTitle=Par Uzstâdîðanu
AboutSetupMessage=%1 versija %2%n%3%n%n%1 mâjas lapa:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< &Atpakaï
ButtonNext=&Tâlâk >
ButtonInstall=&Uzstâdît
ButtonOK=OK
ButtonCancel=Atcelt
ButtonYes=&Jâ
ButtonYesToAll=Jâ &Visam
ButtonNo=&Nç
ButtonNoToAll=Nç V&isam
ButtonFinish=&Pabeigt
ButtonBrowse=Pâ&rlûkot...
ButtonWizardBrowse=Pârlû&kot...
ButtonNewFolder=I&zveidot jaunu mapi

; *** "Select Language" dialog messages
SelectLanguageTitle=Izvçlieties Uzstâdîðanas valodu
SelectLanguageLabel=Izvçlieties valodu, kurâ notiks Uzstâdîðana:

; *** Common wizard text
ClickNext=Spiediet "Tâlâk", lai turpinâtu, vai "Atcelt", lai izietu no Uzstâdîðanas.
BeveledLabel=
BrowseDialogTitle=Pârlûkot mapi
BrowseDialogLabel=Izvçlieties mapi no saraksta, tad spiediet "Ok".
NewFolderName=Jauna mape

; *** "Welcome" wizard page
WelcomeLabel1=Laipni lûdzam [name] Uzstâdîðanâ
WelcomeLabel2=Ðis uzstâdîs [name/ver] uz Jûsu datora.%n%nVçlams aizvçrt visas programmas pirms turpinâðanas.

; *** "Password" wizard page
WizardPassword=Parole
PasswordLabel1=Ðî instalâcija ir aizsargâta ar paroli.
PasswordLabel3=Lûdzu, ievadiet paroli, tad spiediet "Tâlâk", lai turpinâtu. Parole ir reìistrjutîga.
PasswordEditLabel=&Parole:
IncorrectPassword=Parole, ko Jûs ievadîjât, ir nepareiza. Lûdzu, mçìiniet vçlreiz.

; *** "License Agreement" wizard page
WizardLicense=Lîgums
LicenseLabel=Lûdzu, izlasiet sekojoðo informâciju, pirms turpinât.
LicenseLabel3=Lûdzu, izlasiet Lîgumu. Jums ir jâapstiprina Lîgums, lai turpinâtu instalâciju.
LicenseAccepted=Es &piekrîtu lîgumam
LicenseNotAccepted=Es &nepiekrîtu lîgumam

; *** "Information" wizard pages
WizardInfoBefore=Informâcija
InfoBeforeLabel=Lûdzu, izlasiet ðo informâciju.
InfoBeforeClickLabel=Kad esat gatavs turpinât instalâciju, spiediet "Tâlâk".
WizardInfoAfter=Informâcija
InfoAfterLabel=Lûdzu izlasiet sekojoðo informâciju.
InfoAfterClickLabel=Kad esat gatavs turpinât instalâciju, spiediet "Tâlâk".

; *** "User Information" wizard page
WizardUserInfo=Lietotâja informâcija
UserInfoDesc=Lûdzu, ievadiet savu informâciju.
UserInfoName=&Lietotâja vârds:
UserInfoOrg=&Organizâcija:
UserInfoSerial=&Seriâlais numurs:
UserInfoNameRequired=Jums ir jâievada savs vârds.

; *** "Select Destination Location" wizard page
WizardSelectDir=Izvçlieties mapi, uz kuru tiks sûtîti dati
SelectDirDesc=Kur [name] tiks instalçts?
SelectDirLabel3=[name] datnes tiks instalçtas norâdîtajâ mapç.
SelectDirBrowseLabel=Lai turpinâtu, spiediet "Tâlâk". Ja vçlaties norâdît citu mapi, spiediet "Pârlûkot".
DiskSpaceMBLabel=Ir nepiecieðami brîvi [mb] MB uz cietâ diska.
CannotInstallToNetworkDrive=Iestatîðana nevar instalçt ar tîkla disku.
CannotInstallToUNCPath=Iestatîðana nevar uzstâdît uz UNC ceïu.
InvalidPath=Jums ir jânorâda pilna instalâcijas adrese, piemçrs:%n%nC:\APP%n%nvai  UNC adrese:%n%n\\server\share
InvalidDrive=Ierîce UNC, kuru Jûs izvçlçjâties, nepastâv vai arî nav pieejama. Lûdzu, izvçlieties citu.
DiskSpaceWarningTitle=Nepietiek vietas uz diska
DiskSpaceWarning=Instalâcijai ir nepiecieðami vismaz %1 KB brîvâs vietas uz diska, bet pieejami ir tikai %2 KB.%n%nVai vçlaties turpinât?
DirNameTooLong=Mapes nosaukums vai adrese ir pârâk gara.
InvalidDirName=Mapes nosaukums nav derîgs.
BadDirName32=Mapes nosaukumâ nedrîkst bût ðâdi simboli:%n%n%1
DirExistsTitle=Mape jau pastâv
DirExists=Mape:%n%n%1%n%njau pastâv. Vai vienalga vçlaties turpinât?
DirDoesntExistTitle=Mape nepastâv
DirDoesntExist=Mape:%n%n%1%n%ndoes nepastâv. Vai vçlaties izveidot mapi?

; *** "Select Components" wizard page
WizardSelectComponents=Izvçlieties sastâvdaïas
SelectComponentsDesc=Kurus komponentus vçlaties uzstâdît?
SelectComponentsLabel2=Izvçlieties komponentus, kurus vçlaties uzstâdît. Spiediet "Tâlâk", lai turpinâtu.
FullInstallation=Pilna Uzstâdîðana
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Kompakta Uzstâdîðana
CustomInstallation=Izveidot Uzstâdîðanu
NoUninstallWarningTitle=Komponenti jau pastâv
NoUninstallWarning=Uzstâdîðana ir atklâjusi ka ðâdi faili jau ir uzstâdîti:%n%n%1%n%nAtiestatiet ðos komponentus.%n%nVai vçlaties turpinât?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Paðlaik izvçlçtie komponenti aizòem [mb] MB uz cietâ diska.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Papilduzdevumu izvçlne
SelectTasksDesc=Kurus papilduzdevumus vajadzçtu veikt?
SelectTasksLabel2=Izvçlieties, kâdi papilduzdevumi tiks veikti [name] Uzstâdîðanas laikâ, tad spiediet "Tâlâk".

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Izvçlieties Start Menu mapi
SelectStartMenuFolderDesc=Kur Uzstâdîðanas programmai vajadzçtu likt îsinâjumikonas?
SelectStartMenuFolderLabel3=Uzstâdîðana izveidos îsinâjumikonas Start Menu mapç.
SelectStartMenuFolderBrowseLabel=Lai turpinâtu, spiediet "Tâlâk". Ja vçlaties norâdît citu mapi, spiediet "Pârlûkot".
MustEnterGroupName=Jums ir jânorâda mape.
GroupNameTooLong=Mapes nosaukums ir pârâk garð.
InvalidGroupName=Mape nav derîga.
BadGroupName=Mapes nosaukums satur kâdu no ðiem simboliem:%n%n%1
NoProgramGroupCheck2=&Neizveidot Start Menu mapi

; *** "Ready to Install" wizard page
WizardReady=Gatavs instalâcijai
ReadyLabel1=Uzstâdîðana ir gatava instalçt [name] uz Jûsu datora.
ReadyLabel2a=Spiediet "Uzstâdît", lai sâktu instalâciju, vai spiediet Atpakaï, lai izmainîtu parametrus.
ReadyLabel2b=Spiediet "Uzstâdît", lai sâktu instalâciju.
ReadyMemoUserInfo=Lietotâja informâcija:
ReadyMemoDir=Galamçríis:
ReadyMemoType=Uzstâdîðanas tips:
ReadyMemoComponents=Izvçlçtie komponenti:
ReadyMemoGroup=Start Menu mape:
ReadyMemoTasks=Papilduzdevumi:

; *** "Preparing to Install" wizard page
WizardPreparing=Gatavoties instalâcijai
PreparingDesc=Uzstâdîðana ir gatava instalçt [name] uz Jûsu datora.
PreviousInstallNotCompleted=Instalâcija/noòemðana iepriekðçjai programmai nav pabeigta. Jums ir jâpârstartç dators, lai pabeigtu instalâciju.%n%nPçc pârstartçðanas palaidiet uzstâdîðanu no jauna, lai pabeigtu uzstâdît [name].
CannotContinue=Uzstâdîðanu nevar turpinât. Lûdzu, spiediet "Atcelt", lai izietu.
ApplicationsFound=Ðâdas lietojumprogrammas izmanto failus, kas ir jâatjaunina ar Setup. Tas ir ieteicams, ka jûs ïaujat Setup automâtiski aizvçrt ðos pieteikumus.
ApplicationsFound2=Ðâdas lietojumprogrammas izmanto failus, kas ir jâatjaunina ar Setup. Tas ir ieteicams, ka jûs ïaujat Setup automâtiski aizvçrt ðos pieteikumus. Pçc uzstâdîðana ir pabeigta, Setup mçìinâs atsâkt pieteikumus.
CloseApplications=&Automâtiski aizvçrtu programmas
DontCloseApplications=&Nav aizvçrtu programmas

; *** "Installing" wizard page
WizardInstalling=Instalâcija
InstallingLabel=Lûdzu, uzgaidiet, kamçr [name] tiks uzstâdîts uz Jûsu datora.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Pabeigta [name] Uzstâdîðana
FinishedLabelNoIcons=Uzstâdîðana pabeigta.
FinishedLabel=Uzstâdîðana pabeigta. Programmu var palaist, uzklikðíinot uz izveidotajâm ikonâm.
ClickFinish=Spiediet "Pabeigt", lai aizvçrtu Uzstâdîðanu.
FinishedRestartLabel=Lai pabeigtu [name] uzstâdîðanu, nepiecieðams pârstartçt Jûsu datoru. Vai vçlaties to darît tagad?
FinishedRestartMessage=Lai pabeigtu [name] uzstâdîðanu, nepiecieðams pârstartçt Jûsu datoru.%n%nVai vçlaties to darît tagad?
ShowReadmeCheck=Jâ, vçlos apskatît README failu
YesRadio=&Jâ, pârstartçt datoru tagad
NoRadio=&Nç, datoru pârstartçðu vçlâk
; used for example as 'Run MyProg.exe'
RunEntryExec=Run %1
; used for example as 'View Readme.txt'
RunEntryShellExec=View %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Uzstâdîðanai ir nepiecieðams nâkamais disks
SelectDiskLabel2=Lûdzu, ielieciet %1 disku un spiediet "Ok".%n%nJa datne ir atrodama uz ðî paða diska kâdâ citâ mapç, norâdiet tâs atraðanâs vietu vai spiediet "Pârlûkot", lai to norâdîtu.
PathLabel=&Ceïð:
FileNotInDir2=Datne "%1" neatrodas "%2". Lûdzu, ielieciet pareizo disku vai norâdiet pareizo mapi.
SelectDirectoryLabel=Lûdzu, norâdiet nâkamâ diska atraðanâs vietu.

; *** Installation phase messages
SetupAborted=Uzstâdîðana netika pabeigta.%n%nLûdzu, izlabojiet kïûdu un palaidiet Uzstâdîðanu no jauna.
EntryAbortRetryIgnore=Spiediet "Atkârtot", lai mçìinâtu vçlreiz, vai "Ignorçt", lai turpinâtu, vai "Pârtraukt", lai beigtu instalâciju.

; *** Installation status messages
StatusClosingApplications=Noslçguma pieteikumi...
StatusCreateDirs=Mapju izveidoðana...
StatusExtractFiles=Datòu kopçðana...
StatusCreateIcons=Îsinâjumikonu izveidoðana...
StatusCreateIniEntries=Izveido INI ierakstu...
StatusCreateRegistryEntries=Izveido reìistra ierakstus...
StatusRegisterFiles=Reìistrç datnes...
StatusSavingUninstall=Saglabâ atinstalçðanas datus...
StatusRunProgram=Pabeidz instalâciju...
StatusRestartingApplications=Restartçðana pieteikumi...
StatusRollback=Izveido izmaiòas...

; *** Misc. errors
ErrorInternal2=Iekðçja kïûda: %1
ErrorFunctionFailedNoCode=%1 cieta neveiksmi
ErrorFunctionFailed=%1 cieta neveiksmi; kods %2
ErrorFunctionFailedWithMessage=%1 cieta neveiksmi; kods %2.%n%3
ErrorExecutingProgram=Neiespçjami palaist failu:%n%1

; *** Registry errors
ErrorRegOpenKey=Kïûda, atverot reìistra atslçgu:%n%1\%2
ErrorRegCreateKey=Kïûda, izveidojot reìistra atslçgu:%n%1\%2
ErrorRegWriteKey=Kïûda, rakstot reìistra atslçgu:%n%1\%2

; *** INI errors
ErrorIniEntry=Kïûda, izveidojot INI ieraksta datni "%1".

; *** File copying errors
FileAbortRetryIgnore=Spiediet "Atkârtot", lai mçìinâtu vçlreiz, "Ignorçt", lai izlaistu datni (nav ieteicams), vai "Pârtraukt", lai beigtu instalâciju.
FileAbortRetryIgnore2=Spiediet "Atkârtot", lai mçìinâtu vçlreiz, "Ignorçt", lai turpinâtu (nav ieteicams), vai "Pârtraukt", lai beigtu instalâciju.
SourceIsCorrupted=Datnes avots ir bojâts
SourceDoesntExist=Datnes avots "%1" nepastâv
ExistingFileReadOnly=Pastâvoðâ datne ir izveidota kâ read-only.%n%nSpiediet "Atkârtot", lai noòemtu read-only îpaðîbu un mçìinâtu vçlreiz, "Ignorçt", lai izlaistu datni, vai "Pârtraukt", lai beigtu instalâciju.
ErrorReadingExistingDest=Kïûda, nolasot pastâvoðo datni:
FileExists=Datne jau pastâv.%n%nVai vçlaties, lai Uzstâdîðana to pârraksta?
ExistingFileNewer=Pastâvoðâ datne ir jaunâka par to, kuru nepiecieðams uzstâdît. Vçlams atstât jau pastâvoðo datni.%n%nVai vçlaties to paturçt?
ErrorChangingAttr=Radusies kïûda, mçìinot nomainît datnes îpaðîbu:
ErrorCreatingTemp=Radusies kïûda, izveidojot datni galamçría mapç:
ErrorReadingSource=Radusies kïûda, nolasot datni:
ErrorCopying=Radusies kïûda, pârkopçjot datni:
ErrorReplacingExistingFile=Radusies kïûda, pârrakstot jau pastâvoðo datni:
ErrorRestartReplace=Atkârtota aizstâðana cietusi neveiksmi:
ErrorRenamingTemp=Radusies kïûda, nomainot nosaukumu datnei galamçría mapç:
ErrorRegisterServer=Neiespçjami reìistrçt DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 neizdevâs ar izejas kodu %1
ErrorRegisterTypeLib=Neiespçjami reìistrçt tipa bibliotçku: %1

; *** Post-installation errors
ErrorOpeningReadme=Radusies kïûda, atverot README datni.
ErrorRestartingComputer=Uzstâdîðana nevar pârstartçt datoru. Lûdzu, izdariet to manuâli.

; *** Uninstaller messages
UninstallNotFound=Datne "%1" nepastâv. Nevar atinstalçt.
UninstallOpenError=Datni "%1" nevar atvçrt. Nevar atinstalçt
UninstallUnsupportedVer=Atinstalçðanas datne "%1" nav atpazîstama ðai atinstalçðanas programmai. Nevar atinstalçt
UninstallUnknownEntry=Nezinâms ieraksts (%1) izveidoja sadursmi ar atinstalâciju
ConfirmUninstall=Vai esat pârliecinâts, ka vçlaties pilnîbâ noòemt %1 un visus tâ komponentus?
UninstallOnlyOnWin64=Ðo instalâciju var noòemt tikai ar 64-bitu Windows.
OnlyAdminCanUninstall=Atinstalâciju var veikt tikai lietotâjs ar Adminstratora privilçìijâm.
UninstallStatusLabel=Lûdzu uzgaidiet, kamçr %1 tiek noòemts no Jûsu datora.
UninstalledAll=%1 tika veiksmîgi noòemts no Jûsu datora.
UninstalledMost=%1 atinstalâcija pabeigta.%n%nDaþus elementus nevarçja noòemt. Tos var noòemt manuâli.
UninstalledAndNeedsRestart=Lai pabeigtu atinstalâciju %1, Jûsu dators jâpârstartç.%n%nVai vçlaties to darît tagad?
UninstallDataCorrupted="%1" datne ir bojâta. Nevar atinstalçt

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Noòemt kopîgâs datnes?
ConfirmDeleteSharedFile2=Sistçma ir secinâjusi, ka ðîs koplietoðanas datnes vairs netiks lietotas. Vai vçlaties tâs noòemt?%n%nJa kâda cita programma izmanto ðîs datnes, tad ðî programma var strâdât nekorekti. Ja neesat droðs, izvçlieties "Nç". Atstâjot ðîs datnes, Jûsu datoram netiks nodarîti nekâdi bojâjumi.
SharedFileNameLabel=Faila nosaukums:
SharedFileLocationLabel=Atraðanâs vieta:
WizardUninstalling=Atinstalçðanas Statuss
StatusUninstalling=Atinstalç %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Instalçðana %1.
ShutdownBlockReasonUninstallingApp=Atinstalç %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 versija %2
AdditionalIcons=Papildu ikonas:
CreateDesktopIcon=Izveidot &darbvisrmas ikonu
CreateQuickLaunchIcon=Izveidot &Quick Launch ikonu
ProgramOnTheWeb=%1 Internçtâ
UninstallProgram=Atinstalçt %1
LaunchProgram=Palaist %1
AssocFileExtension=&Apvienot %1 ar %2 faila paplaðinâjumu
AssocingFileExtension=Apvienoðana %1 ar %2 faila paplaðinâjumu...
AutoStartProgramGroupDescription=starta:
AutoStartProgram=Automâtiski sâkt %1
AddonHostProgramNotFound=%1 nevar atrasties mapç jûs izvçlçjâties.%n%nVai vçlaties turpinât?
