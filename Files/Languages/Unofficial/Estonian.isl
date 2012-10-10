; *** Inno Setup version 5.1.11+ Estonian messages ***

; Estonian translation by tudiludi
; E-mail: tudiludi.estonia@mail.ee
; Last modification date: 2010-06-07
; Tõlge baseerub rix'i tõlkele, mis on parandatud ja kohandatud uuemale versioonile

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
LanguageName=Estonian
LanguageID=$0425
LanguageCodePage=0
; If the language you are translating to requires special font faces or
; sizes, uncomment any of the following entries and change them accordingly.
;DialogFontName=MS Shell Dlg
;DialogFontSize=8
;WelcomeFontName=Verdana
;WelcomeFontSize=12
;TitleFontName=Arial
;TitleFontSize=29
;CopyrightFontName=Arial
;CopyrightFontSize=8

[Messages]

; *** Application titles
SetupAppTitle=Paigaldaja
SetupWindowTitle=%1'i paigaldamine
UninstallAppTitle=Eemaldamine
UninstallAppFullTitle=%1'i eemaldamine

; *** Misc. common
InformationTitle=Informatsioon
ConfirmTitle=Kinnitus
ErrorTitle=Viga

; *** SetupLdr messages
SetupLdrStartupMessage=Paigaldatakse %1. Kas soovid jätkata?
LdrCannotCreateTemp=Ei saanud luua ajutist faili. Paigaldamine katkestati
LdrCannotExecTemp=Ei suutnud käivitada faili ajutises kataloogis. Paigaldamine katkestati

; *** Startup error messages
LastErrorMessage=%1.%n%nViga %2: %3
SetupFileMissing=%1 puudub paigaldamise kaustast. Palun paranda see viga või hangi uus koopia programmist.
SetupFileCorrupt=Paigaldaja failid on rikutud. Palun hangi uus koopia programmist.
SetupFileCorruptOrWrongVer=Paigaldaja failid on rikutud või ei tööta selle paigaldaja versiooniga. Palun paranda see viga või hangi uus koopia programmist.
NotOnThisPlatform=See programm ei tööta %1's.
OnlyOnThisPlatform=See programm peab töötama %1's.
OnlyOnTheseArchitectures=Seda programmi saab paigaldada ainult Windowsi versioonidel, mis on mõeldud järgmistele protsessori arhitektuuridele:%n%n%1
MissingWOW64APIs=Sinu poolt kasutatava Windowsi versioon ei sisalda paigaldamise poolt vajatud funktsionaalsust 64-bitise paigaldamiseks. Selle probleemi parandamiseks paigalda Service Pack (Hoolduspakett) %1.
WinVersionTooLowError=See programm nõuab %1 versiooniga %2 või uuemat.
WinVersionTooHighError=Seda programmi ei saa paigaldada %1 versioon %2 või uuema puhul.
AdminPrivilegesRequired=Pead administraatorina sisse logima, et seda programmi paigaldada.
PowerUserPrivilegesRequired=Programmi paigaldamiseks pead olema sisse logitud kui administraator või "Power user".
SetupAppRunningError=Paigaldaja tuvastas, et %1 töötab hetkel.%n%nPalun sulge programm ning seejärel jätkamiseks vajuta OK, katkestamiseks Katkesta.
UninstallAppRunningError=Eemaldaja tuvastas, et %1 töötab hetkel.%n%nPalun sulge programm ning seejärel jätkamiseks vajuta OK, katkestamiseks Katkesta.

; *** Misc. errors
ErrorCreatingDir=Paigaldaja ei suutnud luua kataloogi "%1"
ErrorTooManyFilesInDir=Ei suutnud luua faili kataloogi "%1", kuna seal on juba liiga palju faile

; *** Setup common messages
ExitSetupTitle=Välju paigaldajast
ExitSetupMessage=Paigaldamine pole valmis. Kui praegu väljud, siis programmi ei paigaldata.%n%nPaigaldamise lõpetamiseks võid paigaldaja mõni teine kord uuesti käivitada.%n%nSoovid väljuda paigaldajast?
AboutSetupMenuItem=&Teave paigaldajast...
AboutSetupTitle=Teave paigaldajast
AboutSetupMessage=%1 versioon %2%n%3%n%n%1 koduleht:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< &Tagasi
ButtonNext=&Edasi >
ButtonInstall=&Paigalda
ButtonOK=OK
ButtonCancel=Katkesta
ButtonYes=&Jah
ButtonYesToAll=Kõikidele &Jah
ButtonNo=&Ei
ButtonNoToAll=Kõikidele E&i
ButtonFinish=&Valmis
ButtonBrowse=&Sirvi...
ButtonWizardBrowse=S&irvi...
ButtonNewFolder=&Loo uus kaust

; *** "Select Language" dialog messages
SelectLanguageTitle=Vali paigaldaja keel
SelectLanguageLabel=Vali keel, mida soovid kasutada paigaldamise käigus:

; *** Common wizard text
ClickNext=Jätkamiseks vajuta Edasi, paigaldajast väljumiseks vajuta Katkesta.
BeveledLabel=
BrowseDialogTitle=Sirvi kausta
BrowseDialogLabel=Vali nimekirjast kaust ja vajuta OK.
NewFolderName=Uus kaust

; *** "Welcome" wizard page
WelcomeLabel1=Tere tulemast [name]'i paigaldaja viisardisse
WelcomeLabel2=Sinu arvutisse paigaldatakse [name/ver].%n%nEnne jätkamist on soovitatav sulgeda kõik programmid.

; *** "Password" wizard page
WizardPassword=Parool
PasswordLabel1=See paigaldaja on kaitstud parooliga.
PasswordLabel3=Palun sisesta parool ja vajuta Edasi. Paroolid on tõstutundlikud.
PasswordEditLabel=&Parool:
IncorrectPassword=Sisestatud parool on vale. Palun proovi uuesti.

; *** "License Agreement" wizard page
WizardLicense=Litsentsileping
LicenseLabel=Palun loe see oluline informatsioon läbi enne, kui jätkad.
LicenseLabel3=Palun loe järgnevat litsentsilepingut. Sa pead nõustuma selle lepingu tingimustega enne, kui paigaldamine saab jätkuda.
LicenseAccepted=Ma &nõustun lepinguga
LicenseNotAccepted=Ma &ei nõustu lepinguga

; *** "Information" wizard pages
WizardInfoBefore=Informatsioon
InfoBeforeLabel=Palun loe see oluline informatsioon läbi enne, kui jätkad.
InfoBeforeClickLabel=Kui oled valmis jätkama paigaldamist, vajuta Edasi.
WizardInfoAfter=Informatsioon
InfoAfterLabel=Palun loe see oluline informatsioon läbi enne, kui jätkad.
InfoAfterClickLabel=Kui oled valmis jätkama paigaldamist, vajuta Edasi.

; *** "User Information" wizard page
WizardUserInfo=Informatsioon kasutaja kohta
UserInfoDesc=Palun sisesta oma andmed.
UserInfoName=&Kasutaja nimi:
UserInfoOrg=&Organisatsioon:
UserInfoSerial=&Seeria number:
UserInfoNameRequired=Pead sisestama nime.

; *** "Select Destination Location" wizard page
WizardSelectDir=Vali programmile kaust
SelectDirDesc=Kuhu [name] paigaldada?
SelectDirLabel3=Paigaldaja paigaldab [name]'i järgnevasse kausta.
SelectDirBrowseLabel=Jätkamiseks vajuta Edasi. Kui soovid valida teise kausta, vajuta Sirvi.
DiskSpaceMBLabel=Programm vajab vähemalt [mb] MB kõvakettaruumi.
ToUNCPathname=Paigaldaja ei saa paigaldada UNC kataloogi. Kui üritad programmi võrku paigaldada, pead kõigepealt kaardistama võrguketta.
InvalidPath=Pead sisestama täispika tee koos draivitähisega; näiteks:%n%nC:\APP%n%nvõi UNC kataloog kujul:%n%n\\server\share
InvalidDrive=Ketast või UNC ühiskasutust, mille valisid, ei eksisteeri või pole kättesaadav. Palun vali mõni teine.
DiskSpaceWarningTitle=Kõvakettal pole piisavalt ruumi
DiskSpaceWarning=Paigaldamiseks on vaja vähemalt %1 KB vaba ruumi, aga valitud kettal on vaba ainult %2 KB.%n%nKas soovid sellegipoolest jätkata?
DirNameTooLong=Kausta nimi või kaustatee on liiga pikk
InvalidDirName=Kausta nimi ei ole kehtiv.
BadDirName32=Kausta nimed ei tohi sisaldada järgmisi märke:%n%n%1
DirExistsTitle=Kaust on olemas
DirExists=Kaust:%n%n%1%n%non juba olemas. Kas soovid sellegipoolest sinna paigaldada?
DirDoesntExistTitle=Kausta ei eksisteeri
DirDoesntExist=Kaust:%n%n%1%n%nei eksisteeri. Kas soovid, et see kaust luuakse?

; *** "Select Components" wizard page
WizardSelectComponents=Vali komponendid
SelectComponentsDesc=Millised komponendid paigaldada?
SelectComponentsLabel2=Vali komponendid, mida paigaldada; puhasta komponendid, mida ei soovi paigaldada. Kui oled valmis jätkama, vajuta Edasi.
FullInstallation=Täielik paigaldamine
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Kompaktne paigaldamine
CustomInstallation=Kohandatud paigaldamine
NoUninstallWarningTitle=Komponendid eksisteerivad
NoUninstallWarning=Paigaldaja avastas, et järgmised komponendid on juba olemas sinu arvutis:%n%n%1%n%nNende mittevalimine ei eemalda neid.%n%nKas soovid sellegipoolest jätkata?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Praegune valik vajab vähemalt [mb] MB kettaruumi.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Vali täiendavad ülesanded
SelectTasksDesc=Milliseid täiendavad ülesanded täita?
SelectTasksLabel2=Vali milliseid täiendavaid ülesandeid [name]'i paigaldaja peab täitma ja vajuta Edasi.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Vali Start menüü kaust
SelectStartMenuFolderDesc=Kuhu luua programmi otseteed?
SelectStartMenuFolderLabel3=Paigaldaja loob programmi otseteed järgnevasse Start menüü kausta.
SelectStartMenuFolderBrowseLabel=Jätkamiseks vajuta Edasi. Kui soovid valida teise kausta, vajuta Sirvi.
MustEnterGroupName=Pead sisestama kausta nime.
GroupNameTooLong=Kausta nimi või kaustatee on liiga pikk.
InvalidGroupName=Kausta nimi ei kehti.
BadGroupName=Kausta nimi ei tohi sisaldada järgmisi märke:%n%n%1
NoProgramGroupCheck2=&Ära loo Start menüü kausta

; *** "Ready to Install" wizard page
WizardReady=Valmis paigaldama
ReadyLabel1=Paigaldaja on valmis paigaldama [name]'i sinu arvutisse.
ReadyLabel2a=Vajuta Paigalda, et jätkata paigaldamisega või vajuta Tagasi, et saada ülevaade valitutest või muuta seadeid.
ReadyLabel2b=Paigaldamise jätkamiseks vajuta Paigalda.
ReadyMemoUserInfo=Kasutaja informatsioon:
ReadyMemoDir=Sihtkaust:
ReadyMemoType=Paigaldamise tüüp:
ReadyMemoComponents=Valitud komponendid:
ReadyMemoGroup=Start menüü kaust:
ReadyMemoTasks=Lisaülesanded:

; *** "Preparing to Install" wizard page
WizardPreparing=Paigaldamiseks valmistumine
PreparingDesc=Paigaldaja valmistub paigaldama [name]'i sinu arvutisse.
PreviousInstallNotCompleted=Eelmise programmi paigaldamine/eemaldamine ei ole lõpetatud. Pead arvuti taaskäivitama, et lõpetada selle paigaldamine.%n%nPärast taaskäivitust käivitage [name]'i paigaldaja uuesti, et lõpetada paigaldamine.
CannotContinue=Paigaldaja ei saa jätkata. Väljumiseks vajuta Katkesta.

; *** "Installing" wizard page
WizardInstalling=Paigaldamine
InstallingLabel=Palun oota, kuni [name] paigaldatakse sinu arvutisse.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=[name]'i paigaldamise lõpetamine
FinishedLabelNoIcons=[name]'i paigaldamine on lõpetatud.
FinishedLabel=[name]'i paigaldamine on lõpetatud. Programmi saab käivitada paigaldatud ikoonide abil.
ClickFinish=Vajuta Valmis, et väljuda paigaldajast.
FinishedRestartLabel=[name]'i paigaldamise lõpetamiseks peab arvuti taaskäivituma. Kas soovid kohe taaskäivitada?
FinishedRestartMessage=[name]'i paigaldamise lõpetamiseks peab arvuti taaskäivituma.%n%nKas soovid kohe taaskäivitada?
ShowReadmeCheck=Jah, sooviksin näha Readme faili
YesRadio=&Jah, taaskäivita arvuti kohe
NoRadio=&Ei, taaskäivitan arvuti hiljem
; used for example as 'Run MyProg.exe'
RunEntryExec=Käivita %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Vaata %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Paigaldaja vajab järgmist diski
SelectDiskLabel2=Palun sisesta disk %1 ja vajuta OK.%n%nKui diskil olevad failid asuvad kuskil mujal, sisesta õige asukoht või vajuta Sirvi.
PathLabel=&Asukoht:
FileNotInDir2=Faili "%1" ei suudetud leida asukohas "%2". Palun sisesta õige disk või vali teine kaust.
SelectDirectoryLabel=Palun täpsusta järgmise diski asukoht.

; *** Installation phase messages
SetupAborted=Paigaldamist ei lõpetatud.%n%nPalun parandage viga ja käivitage paigaldaja uuesti.
EntryAbortRetryIgnore=Uuesti proovimiseks vajuta Retry, jätkamiseks Ignore või lõpetamiseks Katkesta.

; *** Installation status messages
StatusCreateDirs=Kaustade loomine...
StatusExtractFiles=Failide lahtipakkimine...
StatusCreateIcons=Otseteede loomine...
StatusCreateIniEntries=INI kirjete loomine...
StatusCreateRegistryEntries=Registri kirjete loomine...
StatusRegisterFiles=Failide registreerimine...
StatusSavingUninstall=Eemaldamise teabe salvestamine...
StatusRunProgram=Paigaldamise lõpetamine...
StatusRollback=Muudatuste tagasivõtmine...

; *** Misc. errors
ErrorInternal2=Sisemine viga: %1
ErrorFunctionFailedNoCode=%1 luhtus
ErrorFunctionFailed=%1 luhtus; code %2
ErrorFunctionFailedWithMessage=%1 luhtus; code %2.%n%3
ErrorExecutingProgram=Ei saanud käivitada faili:%n%1

; *** Registry errors
ErrorRegOpenKey=Ei saanud avada registri võtit:%n%1\%2
ErrorRegCreateKey=Ei saanud luua registri võtit:%n%1\%2
ErrorRegWriteKey=Ei saanud kirjutada registri võtit:%n%1\%2

; *** INI errors
ErrorIniEntry=Viga INI kirje loomisel faili "%1".

; *** File copying errors
FileAbortRetryIgnore=Uuesti proovimiseks vajuta Retry, faili vahelejätmiseks Ignore (mittesoovitatav) või paigaldamisest loobumiseks Katkesta.
FileAbortRetryIgnore2=Uuesti proovimiseks vajuta Retry, jätkamiseks Ignore (mittesoovitatav) või paigaldamisest loobumiseks Katkesta.
SourceIsCorrupted=Lähtefail on rikutud
SourceDoesntExist=Lähtefaili "%1" ei eksisteeri
ExistingFileReadOnly=Fail on märgitud kui kirjutuskaitstud.%n%nKirjutuskaitstuse mahavõtmiseks vajuta Retry ja proovi uuesti, faili vahelejätmiseks Ignore või paigaldamisest loobumiseks Katkesta.
ErrorReadingExistingDest=Faili lugemisel tekkis viga:
FileExists=Fail on juba olemas.%n%nKas soovid, et paigaldaja selle üle kirjutaks?
ExistingFileNewer=Olemasolev fail on uuem kui see, mida paigaldaja üritab paigaldada. Soovitatav on olemasolev fail alles hoida.%n%nKas soovid olemasoleva faili alles hoida?
ErrorChangingAttr=Faili atribuutide muutmisel ilmnes viga:
ErrorCreatingTemp=Faili loomisel sihtkataloogi ilmnes viga:
ErrorReadingSource=Lähtefaili lugemisel ilmnes viga:
ErrorCopying=Faili kopeerimisel ilmnes viga:
ErrorReplacingExistingFile=Olemasoleva faili asendamisel ilmnes viga:
ErrorRestartReplace=Faili asendamine peale taaskäivitust ei õnnestunud:
ErrorRenamingTemp=Faili nime muutmisel sihtkataloogis ilmnes viga:
ErrorRegisterServer=Ei saa registreerida DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 failed with exit code %1
ErrorRegisterTypeLib=Unable to register the type library: %1

; *** Post-installation errors
ErrorOpeningReadme=README faili avamisel ilmnes viga.
ErrorRestartingComputer=Paigaldaja ei suutnud arvutit taaskäivitada. Palun tehke seda käsitsi.

; *** Uninstaller messages
UninstallNotFound=Faili"%1" ei ole olemas. Ei saa eemaldada.
UninstallOpenError=Faili"%1" ei saa avada. Ei saa eemaldada.
UninstallUnsupportedVer=Eemaldamise logi faili "%1" formaat on tundmatu selle versiooni eemaldaja jaoks. Ei saa eemaldada
UninstallUnknownEntry=Tundmatu kirje (%1) leiti eemaldaja logist
ConfirmUninstall=Oled kindel, et soovid eemaldada %1 ja kõik tema komponendid?
UninstallOnlyOnWin64=Seda paigaldamist saab eemaldada ainult 64-bitises Windowsis.
OnlyAdminCanUninstall=Seda paigaldamist saab eemaldada ainult administraatoriõigustega kasutaja.
UninstallStatusLabel=Palun oota, kuni %1 eemaldatakse sinu arvutist.
UninstalledAll=%1 kustutati edukalt sinu arvutist.
UninstalledMost=%1'i eemaldamine õnnestus.%n%nMõned failid jäid kustutamata. Need võib käsitsi kustutada.
UninstalledAndNeedsRestart=%1'i eemaldamise lõpetamiseks peab arvuti taaskäivituma.%n%nKas soovid kohe taaskäivitada?
UninstallDataCorrupted="%1" fail on rikutud. Ei saa eemaldada

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Kas kustutan ühiskasutuses oleva faili?
ConfirmDeleteSharedFile2=Süsteem kinnitab, et ühiskasutuses olevat faili ei kasuta ükski teine programm. Kas soovid, et eemaldaja selle kustutaks?%n%nKui mõni programm seda siiski veel kasutab, siis ei pruugi ta enam korralikult töötada. Kui sa pole kindel, vali Ei. Faili allesjätmine ei tekita probleeme.
SharedFileNameLabel=Faili nimi:
SharedFileLocationLabel=Asukoht:
WizardUninstalling=Eemaldamise staatus
StatusUninstalling=%1'i eemaldamine...

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 versioon %2
AdditionalIcons=Täiendavad ikoonid:
CreateDesktopIcon=Loo &töölaua ikoon
CreateQuickLaunchIcon=Loo &kiirkäivituse ikoon
ProgramOnTheWeb=%1 veebis.
UninstallProgram=Eemalda %1
LaunchProgram=Käivita %1
AssocFileExtension=&Seosta %1 faili %2 laiendiga
AssocingFileExtension=Seostan %1 faili %2 laiendiga...
