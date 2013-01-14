; *** Inno Setup version 5.5.3+ Estonian messages ***
;
; Estonian translation by LiivaneLord
; E-mail: liivane.lord@mail.ee
; Last modification date: 2013-01-09
; Tõlge baseerub rix'i tõlkele, mida on parandatud ja kohandatud uuemale versioonile.
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
LanguageName=Eesti
LanguageID=$0425
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
SetupAppTitle=Paigalda
SetupWindowTitle=Paigalda %1
UninstallAppTitle=Eemalda
UninstallAppFullTitle=Eemalda %1

; *** Misc. common
InformationTitle=Informatsioon
ConfirmTitle=Kinnita
ErrorTitle=Viga

; *** SetupLdr messages
SetupLdrStartupMessage=Paigaldatakse %1. Kas soovid jätkata?
LdrCannotCreateTemp=Ei saanud luua ajutist faili. Paigaldamine katkestati
LdrCannotExecTemp=Ei saanud käivitada faili ajutises kataloogis. Paigaldamine katkestati

; *** Startup error messages
LastErrorMessage=%1.%n%nViga %2: %3
SetupFileMissing=%1 on paigaldamise kaustast kadunud. Palun paranda see viga või hangi programmi uus koopia.
SetupFileCorrupt=Paigaldaja failid on rikutud. Palun hangi programmi uus koopia.
SetupFileCorruptOrWrongVer=Paigaldaja failid on kas rikutud või ei tööta selle paigaldaja versiooniga. Palun paranda see viga või hangi programmi uus koopia.
InvalidParameter=Käsureale anti vale parameeter:%n%n%1
SetupAlreadyRunning=Paigaldaja alles töötab.
WindowsVersionNotSupported=Seda programmi ei saa selle Windowsi versiooniga kasutada, mis arvutis praegu töötab.
WindowsServicePackRequired=See programm vajab %1 Service Pack (Hoolduspakett) %2 või uuemat.
NotOnThisPlatform=See programm ei tööta %1'i platvormil.
OnlyOnThisPlatform=See programm peab töötama %1'i platvormil.
OnlyOnTheseArchitectures=Seda programmi saab paigaldada ainult neile Windowsi versioonidele, mis on välja töötatud järgmistele protsessori arhitektuuridele:%n%n%1
MissingWOW64APIs=Sinu arvutis töötaval Windowsil puuduvad 64-bitise paigaldamise jaoks vajalik funktsionaalsus. Selle probleemi parandamiseks paigalda palun Service Pack (Hoolduspakett) %1.
WinVersionTooLowError=See programm vajab %1 versiooniga %2 või uuemat.
WinVersionTooHighError=Seda programmi ei saa paigaldada %1 versiooniga %2 või uuema puhul.
AdminPrivilegesRequired=Selle programmi paigaldamiseks pead olema administraatorina sisse logitud.
PowerUserPrivilegesRequired=Selle programmi paigaldamiseks pead olema sisse logitud administraatorina või Power user liikmena.
SetupAppRunningError=Paigaldaja tuvastas, et %1 töötab hetkel.%n%nPalun sulge see programm ning seejärel jätkamiseks vajuta OK, katkestamiseks Katkesta.
UninstallAppRunningError=Eemaldaja tuvastas, et %1 töötab hetkel.%n%nPalun sulge see programm ning seejärel jätkamiseks vajuta OK, katkestamiseks Katkesta.

; *** Misc. errors
ErrorCreatingDir=Paigaldaja ei saanud luua kataloogi "%1"
ErrorTooManyFilesInDir=Ei saanud luua faili kataloogi "%1", kuna seal on juba liiga palju faile

; *** Setup common messages
ExitSetupTitle=Välju paigaldajast
ExitSetupMessage=Paigaldamine pole valmis. Kui praegu väljud, siis programmi ei paigaldata.%n%nPaigaldamise lõpetamiseks võid paigaldaja mõni teine kord uuesti käivitada.%n%nSoovid väljuda paigaldajast?
AboutSetupMenuItem=&Teave paigaldajast...
AboutSetupTitle=Teave paigaldajast
AboutSetupMessage=%1 versiooniga %2%n%3%n%n%1 koduleht:%n%4
AboutSetupNote=
TranslatorNote=Tõlkis LiivaneLord (liivane[dot]lord[at]mail[dot]ee)

; *** Buttons
ButtonBack=< &Tagasi
ButtonNext=&Edasi >
ButtonInstall=&Paigalda
ButtonOK=OK
ButtonCancel=Katkesta
ButtonYes=&Jah
ButtonYesToAll=Kõikidele J&ah
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
BrowseDialogLabel=Vali allolevast nimekirjast kaust ja vajuta OK.
NewFolderName=Uus kaust

; *** "Welcome" wizard page
WelcomeLabel1=Tere tulemast [name] paigaldaja viisardisse
WelcomeLabel2=Sinu arvutisse paigaldatakse [name/ver].%n%nEnne jätkamist on soovitatav sulgeda kõik muud programmid.

; *** "Password" wizard page
WizardPassword=Parool
PasswordLabel1=See paigaldaja on kaitstud parooliga.
PasswordLabel3=Palun sisesta parool ja vajuta Edasi. Paroolid on tõstutundlikud.
PasswordEditLabel=&Parool:
IncorrectPassword=Sisestatud parool on vale. Palun proovi uuesti.

; *** "License Agreement" wizard page
WizardLicense=Litsentsileping
LicenseLabel=Palun loe enne jätkamist see informatsioon läbi.
LicenseLabel3=Palun loe järgnevat litsentsilepingut. Paigaldamise jätkamiseks pead nõustuma selle lepingu tingimustega.
LicenseAccepted=Ma &nõustun lepinguga
LicenseNotAccepted=Ma &ei nõustu lepinguga

; *** "Information" wizard pages
WizardInfoBefore=Informatsioon
InfoBeforeLabel=Palun loe enne jätkamist see oluline informatsioon läbi.
InfoBeforeClickLabel=Kui oled valmis jätkama paigaldamist, vajuta Edasi.
WizardInfoAfter=Informatsioon
InfoAfterLabel=Palun loe enne jätkamist see oluline informatsioon läbi.
InfoAfterClickLabel=Kui oled valmis jätkama paigaldamist, vajuta Edasi.

; *** "User Information" wizard page
WizardUserInfo=Andmed kasutaja kohta
UserInfoDesc=Palun sisesta oma andmed.
UserInfoName=&Kasutaja nimi:
UserInfoOrg=&Organisatsioon:
UserInfoSerial=&Seerianumber:
UserInfoNameRequired=Pead sisestama nime.

; *** "Select Destination Location" wizard page
WizardSelectDir=Vali programmile kaust
SelectDirDesc=Kuhu [name] paigaldada?
SelectDirLabel3=Paigaldaja paigaldab [name]'i järgnevasse kausta.
SelectDirBrowseLabel=Jätkamiseks vajuta Edasi. Kui soovid valida muu kausta, vajuta Sirvi.
DiskSpaceMBLabel=Programm vajab vähemalt [mb] MB vaba ruumi.
CannotInstallToNetworkDrive=Programmi ei saa paigaldada võrgudraivile.
CannotInstallToUNCPath=Programmi ei saa paigaldada UNC kataloogi.
InvalidPath=Pead sisestama täispika draivitee koos draivitähisega; näiteks:%n%nC:\APP%n%nvõi UNC kataloog kujul:%n%n\\server\share
InvalidDrive=Sinu valitud draivi või UNC kataloogi ei eksisteeri või puudub sellele ligipääs. Palun vali mõni teine.
DiskSpaceWarningTitle=Pole piisavalt ruumi
DiskSpaceWarning=Paigaldamiseks on vaja vähemalt %1 KB vaba ruumi, aga valitud draivil on vaba ainult %2 KB.%n%nKas soovid sellegipoolest jätkata?
DirNameTooLong=Kausta nimi või kaustatee on liiga pikk
InvalidDirName=Kausta nimi on vale.
BadDirName32=Kausta nimed ei tohi sisaldada ühtegi järgnevatest sümbolitest:%n%n%1
DirExistsTitle=Kaust on olemas
DirExists=Kaust:%n%n%1%n%non juba olemas. Kas soovid sellegipoolest sinna paigaldada?
DirDoesntExistTitle=Kaust puudub
DirDoesntExist=Kaust:%n%n%1%n%npuudub. Kas soovid, et see kaust luuakse?

; *** "Select Components" wizard page
WizardSelectComponents=Vali komponendid
SelectComponentsDesc=Millised komponendid paigaldada?
SelectComponentsLabel2=Vali komponendid, mida paigaldada; eemalda märgid komponentidelt, mida ei soovi paigaldada. Kui oled valmis jätkama, vajuta Edasi.
FullInstallation=Täielik paigaldamine
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Kompaktne paigaldamine
CustomInstallation=Kohandatud paigaldamine
NoUninstallWarningTitle=Komponendid on juba olemas
NoUninstallWarning=Paigaldaja tuvastas, et järgnevad komponendid on sinu arvutis juba olemas:%n%n%1%n%nNende mittevalimine ei eemalda neid.%n%nKas soovid sellegipoolest jätkata?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Praegune valik vajab vähemalt [mb] MB vaba ruumi.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Vali täiendavad ülesanded
SelectTasksDesc=Milliseid täiendavaid ülesanded täita?
SelectTasksLabel2=Vali, milliseid täiendavaid ülesandeid [name] paigaldaja peab täitma ja vajuta Edasi.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Vali Start menüü kaust
SelectStartMenuFolderDesc=Kuhu luua programmi otseteed?
SelectStartMenuFolderLabel3=Paigaldaja loob programmi otseteed järgnevasse Start menüü kausta.
SelectStartMenuFolderBrowseLabel=Jätkamiseks vajuta Edasi. Kui soovid valida muu kausta, vajuta Sirvi.
MustEnterGroupName=Pead sisestama kausta nime.
GroupNameTooLong=Kausta nimi või kaustatee on liiga pikk.
InvalidGroupName=Kausta nimi on vale.
BadGroupName=Kausta nimi ei tohi sisaldada järgnevatest sümbolitest:%n%n%1
NoProgramGroupCheck2=&Ära loo Start menüü kausta

; *** "Ready to Install" wizard page
WizardReady=Valmis paigaldama
ReadyLabel1=Paigaldaja on valmis paigaldama [name]'i sinu arvutisse.
ReadyLabel2a=Paigaldamise jätkamiseks vajuta Paigalda või vajuta Tagasi, et näha või muuta seadeid.
ReadyLabel2b=Paigaldamise jätkamiseks vajuta Paigalda.
ReadyMemoUserInfo=Kasutaja andmed:
ReadyMemoDir=Sihtkaust:
ReadyMemoType=Paigalduse tüüp:
ReadyMemoComponents=Valitud komponendid:
ReadyMemoGroup=Start menüü kaust:
ReadyMemoTasks=Lisaülesanded:

; *** "Preparing to Install" wizard page
WizardPreparing=Paigaldamiseks valmistumine
PreparingDesc=Paigaldaja valmistub paigaldama [name]'i sinu arvutisse.
PreviousInstallNotCompleted=Eelmise programmi paigaldamine/eemaldamine ei ole lõpetatud. Paigaldamise lõpetamiseks pead arvuti taaskäivitama.%n%nPärast taaskäivitust käivitage [name]'i paigaldaja uuesti, et lõpetada paigaldamine.
CannotContinue=Paigaldaja ei saa jätkata. Väljumiseks vajuta palun Katkesta.
ApplicationsFound=Järgnevad rakendused kasutavad faile, mida paigaldaja peab uuendama. Soovitatav on lubada paigaldajal need rakendused automaatselt sulgeda.
ApplicationsFound2=Järgnevad rakendused kasutavad faile, mida paigaldaja peab uuendama. Soovitatav on lubada paigaldajal need rakendused automaatselt sulgeda. Pärasta paigaldamise lõpetamist üritab paigaldaja need rakendused taaskäivitada.
CloseApplications=&Sulge rakendused automaatselt
DontCloseApplications=Ära s&ulge rakendusi
ErrorCloseApplications=Paigaldaja ei saanud kõiki rakendusi automaatselt sulgeda. Enne jätkamist on soovitatav sul sulgeda kõik rakendused, mis kasutavad faile, mida paigaldaja peab uuendama.

; *** "Installing" wizard page
WizardInstalling=Paigaldamine
InstallingLabel=Palun oota, kuni [name] paigaldatakse sinu arvutisse.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=[name]'i paigaldamise lõpetamine
FinishedLabelNoIcons=[name]'i paigaldamine on lõpetatud.
FinishedLabel=[name]'i paigaldamine on lõpetatud. Programmi saab käivitada paigaldatud ikoonide abil.
ClickFinish=Paigaldajast väljumiseks vajuta Valmis.
FinishedRestartLabel=[name]'i paigaldamise lõpetamiseks peab arvuti taaskäivituma. Kas soovid kohe taaskäivitada?
FinishedRestartMessage=[name]'i paigaldamise lõpetamiseks peab arvuti taaskäivituma.%n%nKas soovid kohe taaskäivitada?
ShowReadmeCheck=Jah, sooviksin näha Readme (LoeMind) faili
YesRadio=&Jah, taaskäivita arvuti kohe
NoRadio=&Ei, taaskäivitan arvuti hiljem
; used for example as 'Run MyProg.exe'
RunEntryExec=Käivita %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Vaata %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Paigaldaja vajab järgmist diski
SelectDiskLabel2=Palun sisesta disk %1 ja vajuta OK.%n%nKui diskil olevad failid asuvad kuskil mujal, siis sisesta õige kaustatee või vajuta Sirvi.
PathLabel=&Asukoht:
FileNotInDir2=Fail "%1" ei asu kohas "%2". Palun sisesta õige disk või vali teine kaust.
SelectDirectoryLabel=Palun täpsusta järgmise diski asukoht.

; *** Installation phase messages
SetupAborted=Paigaldamist ei lõpetatud.%n%nPalun paranda viga ja käivita paigaldaja uuesti.
EntryAbortRetryIgnore=Uuesti proovimiseks vajuta Proovi uuesti, jätkamiseks Ignoreeri või lõpetamiseks Katkesta.

; *** Installation status messages
StatusClosingApplications=Rakenduste sulgemine...
StatusCreateDirs=Kaustade loomine...
StatusExtractFiles=Failide lahtipakkimine...
StatusCreateIcons=Otseteede loomine...
StatusCreateIniEntries=INI kirjete loomine...
StatusCreateRegistryEntries=Registri kirjete loomine...
StatusRegisterFiles=Failide registreerimine...
StatusSavingUninstall=Eemaldamise teabe salvestamine...
StatusRunProgram=Paigaldamise lõpetamine...
StatusRestartingApplications=Rakenduste taaskäivitamine...
StatusRollback=Muudatuste tagasivõtmine...

; *** Misc. errors
ErrorInternal2=Sisemine viga: %1
ErrorFunctionFailedNoCode=%1 luhtus
ErrorFunctionFailed=%1 luhtus; kood %2
ErrorFunctionFailedWithMessage=%1 luhtus; kood %2.%n%3
ErrorExecutingProgram=Ei saanud käivitada faili:%n%1

; *** Registry errors
ErrorRegOpenKey=Ei saanud avada registri võtit:%n%1\%2
ErrorRegCreateKey=Ei saanud luua registri võtit:%n%1\%2
ErrorRegWriteKey=Ei saanud kirjutada registri võtit:%n%1\%2

; *** INI errors
ErrorIniEntry=Viga INI kirje loomisel failis "%1".

; *** File copying errors
FileAbortRetryIgnore=Uuesti proovimiseks vajuta Proovi uuesti, faili vahelejätmiseks Ignoreeri (mittesoovitatav) või paigaldamisest loobumiseks Katkesta.
FileAbortRetryIgnore2=Uuesti proovimiseks vajuta Proovi uuesti, jätkamiseks Ignoreeri (mittesoovitatav) või paigaldamisest loobumiseks Katkesta.
SourceIsCorrupted=Lähtefail on rikutud
SourceDoesntExist=Lähtefaili "%1" ei eksisteeri
ExistingFileReadOnly=Fail on märgitud kui kirjutuskaitstud.%n%nKirjutuskaitstuse mahavõtmiseks vajuta Proovi uuesti ja proovi uuesti, faili vahelejätmiseks Ignoreeri või paigaldamisest loobumiseks Katkesta.
ErrorReadingExistingDest=Faili lugemisel ilmnes viga:
FileExists=Fail on juba olemas.%n%nKas soovid, et paigaldaja selle üle kirjutaks?
ExistingFileNewer=Olemasolev fail on uuem kui see, mida paigaldaja üritab paigaldada. Soovitatav on olemasolev fail alles jätta.%n%nKas soovid olemasoleva faili alles jätta?
ErrorChangingAttr=Faili atribuutide muutmisel ilmnes viga:
ErrorCreatingTemp=Faili loomisel sihtkataloogi ilmnes viga:
ErrorReadingSource=Lähtefaili lugemisel ilmnes viga:
ErrorCopying=Faili kopeerimisel ilmnes viga:
ErrorReplacingExistingFile=Olemasoleva faili asendamisel ilmnes viga:
ErrorRestartReplace=Faili asendamine peale taaskäivitust ei õnnestunud:
ErrorRenamingTemp=Faili nime muutmisel sihtkataloogis ilmnes viga:
ErrorRegisterServer=Ei saanud registreerida DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 luhtus koodiga %1
ErrorRegisterTypeLib=Unable to register the type library: %1

; *** Post-installation errors
ErrorOpeningReadme=README (LoeMind) faili avamisel ilmnes viga.
ErrorRestartingComputer=Paigaldaja ei suutnud arvutit taaskäivitada. Palun tee seda käsitsi.

; *** Uninstaller messages
UninstallNotFound=Faili "%1" ei ole olemas. Ei saa eemaldada.
UninstallOpenError=Faili "%1" ei saanud avada. Ei saa eemaldada.
UninstallUnsupportedVer=Eemaldamise logifaili "%1" formaat on tundmatu selle versiooni eemaldaja jaoks. Ei saa eemaldada
UninstallUnknownEntry=Eemaldaja logis on tundmatu kirje (%1)
ConfirmUninstall=Oled kindel, et soovid eemaldada %1'i ja kõik selle komponendid?
UninstallOnlyOnWin64=Seda paigaldamist saab eemaldada ainult 64-bitises Windowsis.
OnlyAdminCanUninstall=Seda paigaldamist saab eemaldada ainult administraatoriõigustega kasutaja.
UninstallStatusLabel=Palun oota, kuni %1 eemaldatakse sinu arvutist.
UninstalledAll=%1 eemaldati sinu arvutist edukalt.
UninstalledMost=%1'i eemaldamine õnnestus.%n%nMõned elemendid jäid alles. Need võib käsitsi kustutada.
UninstalledAndNeedsRestart=%1'i eemaldamise lõpetamiseks peab arvuti taaskäivituma.%n%nKas soovid kohe taaskäivitada?
UninstallDataCorrupted="%1" fail on rikutud. Ei saa eemaldada

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Kas kustutan ühiskasutuses oleva faili?
ConfirmDeleteSharedFile2=Süsteem kinnitab, et ühiskasutuses olevat faili ei kasuta ükski teine programm. Kas soovid, et eemaldaja selle ühiskasutuses oleva faili kustutaks?%n%nKui mõni programm seda siiski veel kasutab, siis ei pruugi see enam korralikult töötada. Kui sa pole kindel, vali Ei. Faili allesjätmine ei tekita probleeme.
SharedFileNameLabel=Faili nimi:
SharedFileLocationLabel=Asukoht:
WizardUninstalling=Eemaldamise staatus
StatusUninstalling=%1'i eemaldamine...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=%1'i paigaldamine.
ShutdownBlockReasonUninstallingApp=%1'i eemaldamine.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 versiooniga %2
AdditionalIcons=Täiendavad ikoonid:
CreateDesktopIcon=Loo &töölaua ikoon
CreateQuickLaunchIcon=Loo &kiirkäivituse ikoon
ProgramOnTheWeb=%1 veebis
UninstallProgram=%1 - eemalda
LaunchProgram=Käivita %1
AssocFileExtension=&Seosta %1 %2 faililaiendiga
AssocingFileExtension=Seostan %1 %2 faililaiendiga...
AutoStartProgramGroupDescription=Käivitus:
AutoStartProgram=Käivita %1 automaatselt
AddonHostProgramNotFound=%1 ei asu sinu valitud kaustas.%n%nKas soovid sellegipoolest jätkata?
