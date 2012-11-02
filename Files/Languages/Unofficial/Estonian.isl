; *** Inno Setup version 5.1.11+ Estonian messages ***

; Estonian translation by tudiludi
; E-mail: tudiludi.estonia@mail.ee
; Last modification date: 2010-06-07
; T�lge baseerub rix'i t�lkele, mis on parandatud ja kohandatud uuemale versioonile

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
SetupLdrStartupMessage=Paigaldatakse %1. Kas soovid j�tkata?
LdrCannotCreateTemp=Ei saanud luua ajutist faili. Paigaldamine katkestati
LdrCannotExecTemp=Ei suutnud k�ivitada faili ajutises kataloogis. Paigaldamine katkestati

; *** Startup error messages
LastErrorMessage=%1.%n%nViga %2: %3
SetupFileMissing=%1 puudub paigaldamise kaustast. Palun paranda see viga v�i hangi uus koopia programmist.
SetupFileCorrupt=Paigaldaja failid on rikutud. Palun hangi uus koopia programmist.
SetupFileCorruptOrWrongVer=Paigaldaja failid on rikutud v�i ei t��ta selle paigaldaja versiooniga. Palun paranda see viga v�i hangi uus koopia programmist.
NotOnThisPlatform=See programm ei t��ta %1's.
OnlyOnThisPlatform=See programm peab t��tama %1's.
OnlyOnTheseArchitectures=Seda programmi saab paigaldada ainult Windowsi versioonidel, mis on m�eldud j�rgmistele protsessori arhitektuuridele:%n%n%1
MissingWOW64APIs=Sinu poolt kasutatava Windowsi versioon ei sisalda paigaldamise poolt vajatud funktsionaalsust 64-bitise paigaldamiseks. Selle probleemi parandamiseks paigalda Service Pack (Hoolduspakett) %1.
WinVersionTooLowError=See programm n�uab %1 versiooniga %2 v�i uuemat.
WinVersionTooHighError=Seda programmi ei saa paigaldada %1 versioon %2 v�i uuema puhul.
AdminPrivilegesRequired=Pead administraatorina sisse logima, et seda programmi paigaldada.
PowerUserPrivilegesRequired=Programmi paigaldamiseks pead olema sisse logitud kui administraator v�i "Power user".
SetupAppRunningError=Paigaldaja tuvastas, et %1 t��tab hetkel.%n%nPalun sulge programm ning seej�rel j�tkamiseks vajuta OK, katkestamiseks Katkesta.
UninstallAppRunningError=Eemaldaja tuvastas, et %1 t��tab hetkel.%n%nPalun sulge programm ning seej�rel j�tkamiseks vajuta OK, katkestamiseks Katkesta.

; *** Misc. errors
ErrorCreatingDir=Paigaldaja ei suutnud luua kataloogi "%1"
ErrorTooManyFilesInDir=Ei suutnud luua faili kataloogi "%1", kuna seal on juba liiga palju faile

; *** Setup common messages
ExitSetupTitle=V�lju paigaldajast
ExitSetupMessage=Paigaldamine pole valmis. Kui praegu v�ljud, siis programmi ei paigaldata.%n%nPaigaldamise l�petamiseks v�id paigaldaja m�ni teine kord uuesti k�ivitada.%n%nSoovid v�ljuda paigaldajast?
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
ButtonYesToAll=K�ikidele &Jah
ButtonNo=&Ei
ButtonNoToAll=K�ikidele E&i
ButtonFinish=&Valmis
ButtonBrowse=&Sirvi...
ButtonWizardBrowse=S&irvi...
ButtonNewFolder=&Loo uus kaust

; *** "Select Language" dialog messages
SelectLanguageTitle=Vali paigaldaja keel
SelectLanguageLabel=Vali keel, mida soovid kasutada paigaldamise k�igus:

; *** Common wizard text
ClickNext=J�tkamiseks vajuta Edasi, paigaldajast v�ljumiseks vajuta Katkesta.
BeveledLabel=
BrowseDialogTitle=Sirvi kausta
BrowseDialogLabel=Vali nimekirjast kaust ja vajuta OK.
NewFolderName=Uus kaust

; *** "Welcome" wizard page
WelcomeLabel1=Tere tulemast [name]'i paigaldaja viisardisse
WelcomeLabel2=Sinu arvutisse paigaldatakse [name/ver].%n%nEnne j�tkamist on soovitatav sulgeda k�ik programmid.

; *** "Password" wizard page
WizardPassword=Parool
PasswordLabel1=See paigaldaja on kaitstud parooliga.
PasswordLabel3=Palun sisesta parool ja vajuta Edasi. Paroolid on t�stutundlikud.
PasswordEditLabel=&Parool:
IncorrectPassword=Sisestatud parool on vale. Palun proovi uuesti.

; *** "License Agreement" wizard page
WizardLicense=Litsentsileping
LicenseLabel=Palun loe see oluline informatsioon l�bi enne, kui j�tkad.
LicenseLabel3=Palun loe j�rgnevat litsentsilepingut. Sa pead n�ustuma selle lepingu tingimustega enne, kui paigaldamine saab j�tkuda.
LicenseAccepted=Ma &n�ustun lepinguga
LicenseNotAccepted=Ma &ei n�ustu lepinguga

; *** "Information" wizard pages
WizardInfoBefore=Informatsioon
InfoBeforeLabel=Palun loe see oluline informatsioon l�bi enne, kui j�tkad.
InfoBeforeClickLabel=Kui oled valmis j�tkama paigaldamist, vajuta Edasi.
WizardInfoAfter=Informatsioon
InfoAfterLabel=Palun loe see oluline informatsioon l�bi enne, kui j�tkad.
InfoAfterClickLabel=Kui oled valmis j�tkama paigaldamist, vajuta Edasi.

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
SelectDirLabel3=Paigaldaja paigaldab [name]'i j�rgnevasse kausta.
SelectDirBrowseLabel=J�tkamiseks vajuta Edasi. Kui soovid valida teise kausta, vajuta Sirvi.
DiskSpaceMBLabel=Programm vajab v�hemalt [mb] MB k�vakettaruumi.
ToUNCPathname=Paigaldaja ei saa paigaldada UNC kataloogi. Kui �ritad programmi v�rku paigaldada, pead k�igepealt kaardistama v�rguketta.
InvalidPath=Pead sisestama t�ispika tee koos draivit�hisega; n�iteks:%n%nC:\APP%n%nv�i UNC kataloog kujul:%n%n\\server\share
InvalidDrive=Ketast v�i UNC �hiskasutust, mille valisid, ei eksisteeri v�i pole k�ttesaadav. Palun vali m�ni teine.
DiskSpaceWarningTitle=K�vakettal pole piisavalt ruumi
DiskSpaceWarning=Paigaldamiseks on vaja v�hemalt %1 KB vaba ruumi, aga valitud kettal on vaba ainult %2 KB.%n%nKas soovid sellegipoolest j�tkata?
DirNameTooLong=Kausta nimi v�i kaustatee on liiga pikk
InvalidDirName=Kausta nimi ei ole kehtiv.
BadDirName32=Kausta nimed ei tohi sisaldada j�rgmisi m�rke:%n%n%1
DirExistsTitle=Kaust on olemas
DirExists=Kaust:%n%n%1%n%non juba olemas. Kas soovid sellegipoolest sinna paigaldada?
DirDoesntExistTitle=Kausta ei eksisteeri
DirDoesntExist=Kaust:%n%n%1%n%nei eksisteeri. Kas soovid, et see kaust luuakse?

; *** "Select Components" wizard page
WizardSelectComponents=Vali komponendid
SelectComponentsDesc=Millised komponendid paigaldada?
SelectComponentsLabel2=Vali komponendid, mida paigaldada; puhasta komponendid, mida ei soovi paigaldada. Kui oled valmis j�tkama, vajuta Edasi.
FullInstallation=T�ielik paigaldamine
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Kompaktne paigaldamine
CustomInstallation=Kohandatud paigaldamine
NoUninstallWarningTitle=Komponendid eksisteerivad
NoUninstallWarning=Paigaldaja avastas, et j�rgmised komponendid on juba olemas sinu arvutis:%n%n%1%n%nNende mittevalimine ei eemalda neid.%n%nKas soovid sellegipoolest j�tkata?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Praegune valik vajab v�hemalt [mb] MB kettaruumi.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Vali t�iendavad �lesanded
SelectTasksDesc=Milliseid t�iendavad �lesanded t�ita?
SelectTasksLabel2=Vali milliseid t�iendavaid �lesandeid [name]'i paigaldaja peab t�itma ja vajuta Edasi.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Vali Start men�� kaust
SelectStartMenuFolderDesc=Kuhu luua programmi otseteed?
SelectStartMenuFolderLabel3=Paigaldaja loob programmi otseteed j�rgnevasse Start men�� kausta.
SelectStartMenuFolderBrowseLabel=J�tkamiseks vajuta Edasi. Kui soovid valida teise kausta, vajuta Sirvi.
MustEnterGroupName=Pead sisestama kausta nime.
GroupNameTooLong=Kausta nimi v�i kaustatee on liiga pikk.
InvalidGroupName=Kausta nimi ei kehti.
BadGroupName=Kausta nimi ei tohi sisaldada j�rgmisi m�rke:%n%n%1
NoProgramGroupCheck2=&�ra loo Start men�� kausta

; *** "Ready to Install" wizard page
WizardReady=Valmis paigaldama
ReadyLabel1=Paigaldaja on valmis paigaldama [name]'i sinu arvutisse.
ReadyLabel2a=Vajuta Paigalda, et j�tkata paigaldamisega v�i vajuta Tagasi, et saada �levaade valitutest v�i muuta seadeid.
ReadyLabel2b=Paigaldamise j�tkamiseks vajuta Paigalda.
ReadyMemoUserInfo=Kasutaja informatsioon:
ReadyMemoDir=Sihtkaust:
ReadyMemoType=Paigaldamise t��p:
ReadyMemoComponents=Valitud komponendid:
ReadyMemoGroup=Start men�� kaust:
ReadyMemoTasks=Lisa�lesanded:

; *** "Preparing to Install" wizard page
WizardPreparing=Paigaldamiseks valmistumine
PreparingDesc=Paigaldaja valmistub paigaldama [name]'i sinu arvutisse.
PreviousInstallNotCompleted=Eelmise programmi paigaldamine/eemaldamine ei ole l�petatud. Pead arvuti taask�ivitama, et l�petada selle paigaldamine.%n%nP�rast taask�ivitust k�ivitage [name]'i paigaldaja uuesti, et l�petada paigaldamine.
CannotContinue=Paigaldaja ei saa j�tkata. V�ljumiseks vajuta Katkesta.

; *** "Installing" wizard page
WizardInstalling=Paigaldamine
InstallingLabel=Palun oota, kuni [name] paigaldatakse sinu arvutisse.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=[name]'i paigaldamise l�petamine
FinishedLabelNoIcons=[name]'i paigaldamine on l�petatud.
FinishedLabel=[name]'i paigaldamine on l�petatud. Programmi saab k�ivitada paigaldatud ikoonide abil.
ClickFinish=Vajuta Valmis, et v�ljuda paigaldajast.
FinishedRestartLabel=[name]'i paigaldamise l�petamiseks peab arvuti taask�ivituma. Kas soovid kohe taask�ivitada?
FinishedRestartMessage=[name]'i paigaldamise l�petamiseks peab arvuti taask�ivituma.%n%nKas soovid kohe taask�ivitada?
ShowReadmeCheck=Jah, sooviksin n�ha Readme faili
YesRadio=&Jah, taask�ivita arvuti kohe
NoRadio=&Ei, taask�ivitan arvuti hiljem
; used for example as 'Run MyProg.exe'
RunEntryExec=K�ivita %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Vaata %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Paigaldaja vajab j�rgmist diski
SelectDiskLabel2=Palun sisesta disk %1 ja vajuta OK.%n%nKui diskil olevad failid asuvad kuskil mujal, sisesta �ige asukoht v�i vajuta Sirvi.
PathLabel=&Asukoht:
FileNotInDir2=Faili "%1" ei suudetud leida asukohas "%2". Palun sisesta �ige disk v�i vali teine kaust.
SelectDirectoryLabel=Palun t�psusta j�rgmise diski asukoht.

; *** Installation phase messages
SetupAborted=Paigaldamist ei l�petatud.%n%nPalun parandage viga ja k�ivitage paigaldaja uuesti.
EntryAbortRetryIgnore=Uuesti proovimiseks vajuta Retry, j�tkamiseks Ignore v�i l�petamiseks Katkesta.

; *** Installation status messages
StatusCreateDirs=Kaustade loomine...
StatusExtractFiles=Failide lahtipakkimine...
StatusCreateIcons=Otseteede loomine...
StatusCreateIniEntries=INI kirjete loomine...
StatusCreateRegistryEntries=Registri kirjete loomine...
StatusRegisterFiles=Failide registreerimine...
StatusSavingUninstall=Eemaldamise teabe salvestamine...
StatusRunProgram=Paigaldamise l�petamine...
StatusRollback=Muudatuste tagasiv�tmine...

; *** Misc. errors
ErrorInternal2=Sisemine viga: %1
ErrorFunctionFailedNoCode=%1 luhtus
ErrorFunctionFailed=%1 luhtus; code %2
ErrorFunctionFailedWithMessage=%1 luhtus; code %2.%n%3
ErrorExecutingProgram=Ei saanud k�ivitada faili:%n%1

; *** Registry errors
ErrorRegOpenKey=Ei saanud avada registri v�tit:%n%1\%2
ErrorRegCreateKey=Ei saanud luua registri v�tit:%n%1\%2
ErrorRegWriteKey=Ei saanud kirjutada registri v�tit:%n%1\%2

; *** INI errors
ErrorIniEntry=Viga INI kirje loomisel faili "%1".

; *** File copying errors
FileAbortRetryIgnore=Uuesti proovimiseks vajuta Retry, faili vahelej�tmiseks Ignore (mittesoovitatav) v�i paigaldamisest loobumiseks Katkesta.
FileAbortRetryIgnore2=Uuesti proovimiseks vajuta Retry, j�tkamiseks Ignore (mittesoovitatav) v�i paigaldamisest loobumiseks Katkesta.
SourceIsCorrupted=L�htefail on rikutud
SourceDoesntExist=L�htefaili "%1" ei eksisteeri
ExistingFileReadOnly=Fail on m�rgitud kui kirjutuskaitstud.%n%nKirjutuskaitstuse mahav�tmiseks vajuta Retry ja proovi uuesti, faili vahelej�tmiseks Ignore v�i paigaldamisest loobumiseks Katkesta.
ErrorReadingExistingDest=Faili lugemisel tekkis viga:
FileExists=Fail on juba olemas.%n%nKas soovid, et paigaldaja selle �le kirjutaks?
ExistingFileNewer=Olemasolev fail on uuem kui see, mida paigaldaja �ritab paigaldada. Soovitatav on olemasolev fail alles hoida.%n%nKas soovid olemasoleva faili alles hoida?
ErrorChangingAttr=Faili atribuutide muutmisel ilmnes viga:
ErrorCreatingTemp=Faili loomisel sihtkataloogi ilmnes viga:
ErrorReadingSource=L�htefaili lugemisel ilmnes viga:
ErrorCopying=Faili kopeerimisel ilmnes viga:
ErrorReplacingExistingFile=Olemasoleva faili asendamisel ilmnes viga:
ErrorRestartReplace=Faili asendamine peale taask�ivitust ei �nnestunud:
ErrorRenamingTemp=Faili nime muutmisel sihtkataloogis ilmnes viga:
ErrorRegisterServer=Ei saa registreerida DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 failed with exit code %1
ErrorRegisterTypeLib=Unable to register the type library: %1

; *** Post-installation errors
ErrorOpeningReadme=README faili avamisel ilmnes viga.
ErrorRestartingComputer=Paigaldaja ei suutnud arvutit taask�ivitada. Palun tehke seda k�sitsi.

; *** Uninstaller messages
UninstallNotFound=Faili"%1" ei ole olemas. Ei saa eemaldada.
UninstallOpenError=Faili"%1" ei saa avada. Ei saa eemaldada.
UninstallUnsupportedVer=Eemaldamise logi faili "%1" formaat on tundmatu selle versiooni eemaldaja jaoks. Ei saa eemaldada
UninstallUnknownEntry=Tundmatu kirje (%1) leiti eemaldaja logist
ConfirmUninstall=Oled kindel, et soovid eemaldada %1 ja k�ik tema komponendid?
UninstallOnlyOnWin64=Seda paigaldamist saab eemaldada ainult 64-bitises Windowsis.
OnlyAdminCanUninstall=Seda paigaldamist saab eemaldada ainult administraatori�igustega kasutaja.
UninstallStatusLabel=Palun oota, kuni %1 eemaldatakse sinu arvutist.
UninstalledAll=%1 kustutati edukalt sinu arvutist.
UninstalledMost=%1'i eemaldamine �nnestus.%n%nM�ned failid j�id kustutamata. Need v�ib k�sitsi kustutada.
UninstalledAndNeedsRestart=%1'i eemaldamise l�petamiseks peab arvuti taask�ivituma.%n%nKas soovid kohe taask�ivitada?
UninstallDataCorrupted="%1" fail on rikutud. Ei saa eemaldada

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Kas kustutan �hiskasutuses oleva faili?
ConfirmDeleteSharedFile2=S�steem kinnitab, et �hiskasutuses olevat faili ei kasuta �kski teine programm. Kas soovid, et eemaldaja selle kustutaks?%n%nKui m�ni programm seda siiski veel kasutab, siis ei pruugi ta enam korralikult t��tada. Kui sa pole kindel, vali Ei. Faili allesj�tmine ei tekita probleeme.
SharedFileNameLabel=Faili nimi:
SharedFileLocationLabel=Asukoht:
WizardUninstalling=Eemaldamise staatus
StatusUninstalling=%1'i eemaldamine...

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 versioon %2
AdditionalIcons=T�iendavad ikoonid:
CreateDesktopIcon=Loo &t��laua ikoon
CreateQuickLaunchIcon=Loo &kiirk�ivituse ikoon
ProgramOnTheWeb=%1 veebis.
UninstallProgram=Eemalda %1
LaunchProgram=K�ivita %1
AssocFileExtension=&Seosta %1 faili %2 laiendiga
AssocingFileExtension=Seostan %1 faili %2 laiendiga...
