; *** Inno Setup version 6.4.0+ Basque messages ***
;
; Basque Translation: (EUS_Xabier Aramendi) (azpidatziak@gmail.com)
; Default.isl 6.4.0 translation: Marimar (bmarimar2@gmail.com) 
; Based in translation 5.5.3+ by Xabier Aramendi
;
; To download user-contributed translations of this file, go to:
;   https://jrsoftware.org/files/istrans/
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).

[LangOptions]
; The following three entries are very important. Be sure to read and 
; understand the '[LangOptions] section' topic in the help file.
LanguageName=Basque (Euskara)
LanguageID=$042D
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
SetupAppTitle=Ezarpena
SetupWindowTitle=Ezarpena - %1
UninstallAppTitle=Kentzea
UninstallAppFullTitle=Kentzea - %1

; *** Misc. common
InformationTitle=Argibideak
ConfirmTitle=Baieztatu
ErrorTitle=Akatsa

; *** SetupLdr messages
SetupLdrStartupMessage=Honek %1 ezarriko du. Jarraitzea nahi duzu?
LdrCannotCreateTemp=Ezinezkoa aldibaterako agiri bat sortzea. Ezarpena utzita
LdrCannotExecTemp=Ezinezkoa agiria exekutatzea aldibaterako zuzenbidean. Ezarpena utzita
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nAkatsa %2: %3
SetupFileMissing=%1 agiria ez dago ezarpen zuzenbidean. Mesedez zuzendu arazoa edo lortu programaren kopia berri bat.
SetupFileCorrupt=Ezarpen agiriak hondatuta daude. Mesedez lortu programaren kopia berri bat.
SetupFileCorruptOrWrongVer=Ezarpen agiriak hondatuta daude, edo bateraezinak dira Ezartzaile bertsio honekin. Mesedez zuzendu arazoa edo lortu programaren kopia berri bat.
InvalidParameter=Parametro baliogabe bat igaro da komando lerroan:%n%n%1
SetupAlreadyRunning=Ezarpena jadanik ekinean dago.
WindowsVersionNotSupported=Programa honek ez du zure ordenagailuan ekinean dagoen Windows bertsioa sostengatzen.
WindowsServicePackRequired=Programa honek %1 Service Pack %2 edo berriagoa behar du.
NotOnThisPlatform=Programa honek ez du ekingo hemen: %1.
OnlyOnThisPlatform=Programa hau hemen ekin behar da: %1.
OnlyOnTheseArchitectures=Programa hau hurrengo Windows arkitekturatarako diseinaturiko bertsioetan bakarrik ezarri daiteke:%n%n%1
WinVersionTooLowError=Programa honek %1 bertsioa %2 edo berriagoa behar du.
WinVersionTooHighError=Programa hau ezin da %1 bertsioa %2 edo berriagoan ezarria izan.
AdminPrivilegesRequired=Administrari bezala izena-emanda egon behar zara programa hau ezartzeko.
PowerUserPrivilegesRequired=Administrari bezala izena-emanda edo Boteredun Erabiltzaile taldeko kide bezala egon behar zara programa hau ezartzerakoan.
SetupAppRunningError=Ezartzaileak %1 ekinean dagoela atzeman du.%n%nMesedez itxi bere eskabide guztiak orain, orduan klikatu Ongi jarritzeko, edo Ezeztatu irtetzeko.
UninstallAppRunningError=Kentzaileak %1 ekinean dagoela atzeman du.%n%nMesedez itxi bere eskabide guztiak orain, orduan klikatu Ongi jarritzeko, edo Ezeztatu irtetzeko.

; *** Startup questions
PrivilegesRequiredOverrideTitle= Instalatzeko modua hautatzea
PrivilegesRequiredOverrideInstruction=Aukeratu instalatzeko modua
PrivilegesRequiredOverrideText1=%1 erabiltzaile guztientzat instalatu daiteke (administrazio-pribilegioak behar ditu), edo zuretzat bakarrik.
PrivilegesRequiredOverrideText2=%1 Zuretzat bakarrik instalatu daiteke, edo erabiltzaile guztientzat (administrazio-pribilegioak behar ditu).
PrivilegesRequiredOverrideAllUsers=Instalatu erabiltzaile guztientzat
PrivilegesRequiredOverrideAllUsersRecommended=Instalatu &erabiltzaile guztientzat (gomendatua)
PrivilegesRequiredOverrideCurrentUser=Instalatu &niretzako soilik
PrivilegesRequiredOverrideCurrentUserRecommended=Instalatu &niretzako soilik (gomendatua)

; *** Misc. errors
ErrorCreatingDir=Ezartzaileak ezin izan du zuzenbidea sortu "%1" 
ErrorTooManyFilesInDir=Ezinezkoa agiri bat sortzea "%1" zuzenbidean agiri gehiegi dituelako

; *** Setup common messages
ExitSetupTitle=Irten Ezartzailetik
ExitSetupMessage=Ezarpena ez dago osatuta. Orain irtetzen bazara, programa ez da ezarriko.%n%nEzartzailea berriro edonoiz abiatu dezakezu ezarpena osatzeko.%n%nIrten Ezartzailetik?
AboutSetupMenuItem=&Ezartzaileari buruz...
AboutSetupTitle=Ezartzaileari buruz
AboutSetupMessage=%1 bertsioa %2%n%3%n%n%1 webgunea:%n%4
AboutSetupNote=
TranslatorNote=TranslatorNote=Basque translation maintained by Marimar

; *** Buttons
ButtonBack=< &Atzera
ButtonNext=&Hurrengoa >
ButtonInstall=&Ezarri
ButtonOK=Ongi
ButtonCancel=Ezeztatu
ButtonYes=&Bai
ButtonYesToAll=Bai &Guztiari
ButtonNo=&Ez
ButtonNoToAll=E&z Guztiari
ButtonFinish=A&maitu
ButtonBrowse=&Bilatu...
ButtonWizardBrowse=B&ilatu...
ButtonNewFolder=Egi&n Agiritegi Berria

; *** "Select Language" dialog messages
SelectLanguageTitle=Hautatu Ezarpen Hizkuntza
SelectLanguageLabel=Hautatu ezarpenean zehar erabiltzeko hizkuntza:

; *** Common wizard text
ClickNext=Klikatu Hurrengoa jarraitzeko, edo Ezeztatu Ezartzailetik irtetzeko
BeveledLabel=
BrowseDialogTitle=Bilatu Agiritegia
BrowseDialogLabel=Hautatu agiritegi bat azpiko zerrendan, orduan klikatu Ongi
NewFolderName=Agiritegi Berria

; *** "Welcome" wizard page
WelcomeLabel1=Ongi etorria [name] Ezarpen Laguntzailera
WelcomeLabel2=Honek [name/ver] zure ordenagailuan ezarriko du.%n%nGomendagarria da beste aplikazio guztiak istea jarraitu aurretik.

; *** "Password" wizard page
WizardPassword=Sarhitza
PasswordLabel1=Ezarpen hau sarhitzez babestuta dago.
PasswordLabel3=Mesedez eman sarhitza, orduan klikatu Hurrengoa jarraitzeko. Sarhitzek hizki larri-xeheak bereizten dituzte.
PasswordEditLabel=&Sarhitza:
IncorrectPassword=Eman duzun sarhitza ez da zuzena. Mesedez saiatu berriro.

; *** "License Agreement" wizard page
WizardLicense=Baimen Ituna
LicenseLabel=Mesedez irakurri hurrengo argibide garrantzitsuak jarraitu aurretik.
LicenseLabel3=Mesedez irakurri hurrengo Baimen Ituna. Itun honen baldintzak onartu behar dituzu ezarpenarekin jarraitu aurretik.
LicenseAccepted=&Onartzen dut ituna
LicenseNotAccepted=&Ez dut onartzen ituna

; *** "Information" wizard pages
WizardInfoBefore=Argibideak
InfoBeforeLabel=Mesedez irakurri hurrengo argibide garrantzitsuak jarraitu aurretik.
InfoBeforeClickLabel=Ezarpenarekin jarraitzeko gertu zaudenean, klikatu Hurrengoa.
WizardInfoAfter=Argibideak
InfoAfterLabel=Mesedez irakurri hurrengo argibide garrantzitsuak jarraitu aurretik.
InfoAfterClickLabel=Ezarpenarekin jarraitzeko gertu zaudenean, klikatu Hurrengoa.

; *** "User Information" wizard page
WizardUserInfo=Erabailtzaile Argibideak
UserInfoDesc=Mesedez sartu zure argibideak
UserInfoName=&Erabiltzaile Izena:
UserInfoOrg=&Antolakundea:
UserInfoSerial=&Serie Zenbakia:
UserInfoNameRequired=Izen bat sartu behar duzu.

; *** "Select Destination Location" wizard page
WizardSelectDir=Hautatu Helmuga Kokalekua
SelectDirDesc=Non ezarri behar da [name]?
SelectDirLabel3=Ezartzaileak [name] hurrengo agiritegian ezarriko du.
SelectDirBrowseLabel=Jarraitzeko, klikatu Hurrengoa. Beste agiritegi bat hautatzea nahi baduzu, klikatu Bilatu.
DiskSpaceGBLabel=Gutxienez [gb ] GB-eko toki askea behar da diska gogorrean.
DiskSpaceMBLabel=Gutxienez [mb] MB-eko toki askea behar da diska gogorrean.
CannotInstallToNetworkDrive=Ezarpena ezin da sare gidagailu batean egin.
CannotInstallToUNCPath=Ezarpena ezin da UNC helburu batean egin.
InvalidPath=Helburu osoa gidagailu hizkiarekin sartu behar duzu; adibidez:%n%nC:\APP%n%nedo UNC helburu bat forma honetan:%n%n\\server\share
InvalidDrive=Hautatu duzun gidagailua edo UNC elkarbanaketa ez dago edo sarbidea ezinezkoa da. Mesedez hautatu beste bat.
DiskSpaceWarningTitle=Ez Dago Nahikoa Toki Diskan
DiskSpaceWarning=Ezarpenak gutxienez %1 KB-eko toki askea behar du ezartzeko, baina hautaturiko gidagailuak %2 KB bakarrik ditu eskuragarri.%n%nHorrela ere jarraitzea nahi duzu?
DirNameTooLong=Agiritegi izena edo helburua luzeegia da.
InvalidDirName=Agiritegi izena ez da baliozkoa.
BadDirName32=Agiritegi izenek ezin dute hurrengo hizkietako bat ere izan:%n%n%1
DirExistsTitle=Agiritegia Badago
DirExists=Agiritegia:%n%n%1%n%njadanik badago. Horrela ere agiritegi horretan ezartzea nahi duzu?
DirDoesntExistTitle=Agiritegia Ez Dago
DirDoesntExist=Agiritegia:%n%n%1%n%nez dago. Nahi duzu agiritegia sortzea?

; *** "Select Components" wizard page
WizardSelectComponents=Hautatu Osagaiak
SelectComponentsDesc=Zer osagai ezarri behar dira?
SelectComponentsLabel2=Hautatu ezartzea nahi dituzun osagaiak; garbitu ezartzea nahi ez dituzun osagaiak. Klikatu Hurrengoa jarraitzeko gertu zaudenean.
FullInstallation=Ezarpen osoa
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Ezarpen trinkoa
CustomInstallation=Norbere ezarpena
NoUninstallWarningTitle=Osagaiak Badaude
NoUninstallWarning=Ezartzaileak atzeman du hurrengo osagaiak jadanik zure ordenagailuan ezarrita daudela:%n%n%1%n%nOsagai hauek deshautatuz gero ez dira ezarriko.%n%nHorrela ere jarraitzea nahi duzu?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=Oraingo hautapenak gutxienez [gb ] GB-eko tokia behar du diskan.
ComponentsDiskSpaceMBLabel=Oraingo hautapenak gutxienez [mb] MB-eko tokia behar du diskan.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Hautatu Eginkizun Gehigarriak
SelectTasksDesc=Zer eginkizun gehigarri burutu behar dira?
SelectTasksLabel2=Hautatu Ezartzaileak  [name]-ren ezarpenean zehar burutzea nahi dituzun eginkizun gehigarriak, orduan klikatu Hurrengoa

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Hautatu Hasiera Menuko Agiritegia
SelectStartMenuFolderDesc=Non ezarri behar ditu Ezartzaileak programaren lasterbideak?
SelectStartMenuFolderLabel3=Ezartzaileak programaren lasterbideak hurrengo Hasiera Menuko agiritegian sortuko ditu.
SelectStartMenuFolderBrowseLabel=Jarraitzeko, klikatu Hurrengoa. Beste agiritegi bat hautatzea nahi baduzu, klikatu Bilatu.
MustEnterGroupName=Agiritegi izen bat sartu behar duzu.
GroupNameTooLong=Agiritegi izena edo helburua luzeegia da.
InvalidGroupName=Agiritegi izena ez da baliozkoa.
BadGroupName=Agiritegi izenak ezin du hurrengo hizkietako bat ere izan:%n%n%1
NoProgramGroupCheck2=&Ez sortu Hasiera Menuko agiritegia

; *** "Ready to Install" wizard page
WizardReady=Ezartzeko Gertu
ReadyLabel1=Ezartzailea orain gertu dago [name] zure ordenagailuan ezartzeko.
ReadyLabel2a=Klikatu Ezarri ezarpenarekin jarraitzeko, edo klikatu Atzera ezarpenen bat berrikustea edo aldatzea nahi baduzu.
ReadyLabel2b=Klikatu Ezarri ezarpenarekin jarraitzeko.
ReadyMemoUserInfo=Erabiltzaile argibideak:
ReadyMemoDir=Helmuga kokalekua:
ReadyMemoType=Ezarpen mota:
ReadyMemoComponents=Hautaturiko osagaiak:
ReadyMemoGroup=Hasiera Menuko agiritegia:
ReadyMemoTasks=Eginkizun gehigarriak:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel=Fitxategi gehigarriak deskargatzen...
ButtonStopDownload=&Gelditu deskarga
StopDownload=Ondoren, deskarga gelditu nahi duzu?
ErrorDownloadAborted=Deskarga ezeztatuta
ErrorDownloadFailed=Deskarga huts egin du: %1 %2
ErrorDownloadSizeFailed=Tamaina lortzea huts egin du: %1 %2
ErrorFileHash1=Fitxategiaren hash huts egin zuen: %1
ErrorFileHash2=Fitxategiaren hash ez da baliozkoa: espero %1, aurkitu %2
ErrorProgress=Aurrerapena ez da baliozkoa: %1 (e) tik %2 (e) ra
ErrorFileSize=Fitxategiaren tamaina ez da baliozkoa: espero %1, aurkitu %2

; *** TExtractionWizardPage wizard page and Extract7ZipArchive
ExtractionLabel=Fitxategi gehigarriak ateratzen...
ButtonStopExtraction=&Erauzketa gelditu
StopExtraction=Ziur zaude erauzketa gelditu nahi duzula?
ErrorExtractionAborted=Erauzketa ezeztatua
ErrorExtractionFailed=Erauzketak huts egin zuen: %1

; *** "Preparing to Install" wizard page
WizardPreparing=Ezartzeko Gertatzen
PreparingDesc=Ezartzailea [name] zure ordenagailuan ezartzeko gertatzen ari da.
PreviousInstallNotCompleted=Aurreko programaren ezartze/kentzea ez dago osatuta. Zure ordenagailua berrabiarazi behar duzu ezarpena osatzeko.%n%nZure ordenagailua berrabiarazi ondoren, ekin Ezartzailea berriro [name]-ren ezarpena osatzeko.
CannotContinue=Ezarpenak ezin du jarraitu. Mesedez klikatu Ezeztatu irtetzeko.
ApplicationsFound=Hurrengo aplikazioak Ezartzaileak eguneratu behar dituen agiriak erabiltzen ari dira. Gomendagarria da Ezartzaileari aplikazio hauek berezgaitasunez istea ahalbidetzea.
ApplicationsFound2=Hurrengo aplikazioak Ezartzaileak eguneratu behar dituen agiriak erabiltzen ari dira. Gomendagarria da Ezartzaileari aplikazio hauek berezgaitasunez istea ahalbidetzea. Ezarpena osatu ondoren, Ezartzailea aplikazioak berrabiarazten saiatuko da.
CloseApplications=&Berezgaitasunez itxi aplikazioak
DontCloseApplications=&Ez itxi aplikazioak
ErrorCloseApplications=Ezartzaileak ezin ditu berezgaitasunez aplikazio guztiak itxi. Gomendagarria da Ezartzaileak eguneratu behar dituen agiriak erabiltzen ari diren aplikazio guztiak istea jarraitu aurretik.
PrepareToInstallNeedsRestart=Instalazio-programak sistema berrabiarazi behar du. Berriro hasi ondoren, instalazio-programa exekutatu berriro, [name] -ren instalazioa osatzeko.%n%n Sistema berrabiarazi nahi duzu orain?

; *** "Installing" wizard page
WizardInstalling=Ezartzen
InstallingLabel=Mesedez itxaron Ezartzaileak [name] zure ordenagailuan ezartzen duen bitartean.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=[name] Ezarpen Laguntzailea osatzen
FinishedLabelNoIcons=Ezartzaileak amaitu du [name] zure ordenagailuan ezartzeaz.
FinishedLabel=Ezartzaileak amaitu du [name] zure ordenagailuan ezartzea. Aplikazioa ezarritako ikurren bidez abiarazi daiteke.
ClickFinish=Klikatu Amaitu Ezartzailetik irtetzeko.
FinishedRestartLabel=[name]-ren ezarpena osatzeko, Ezartzaileak zure ordenagailua berrabiarazi behar du. Orain berrabiaraztea nahi duzu?
FinishedRestartMessage=[name]-ren ezarpena osatzeko, Ezartzaileak zure ordenagailua berrabiarazi behar du.%n%nOrain berrabiaraztea nahi duzu?
ShowReadmeCheck=Bai, IRAKURRI agiria ikustea nahi dut
YesRadio=&Bai, berrabiarazi ordenagailua orain
NoRadio=&Ez, geroago berrabiaraziko dut ordenagailua
; used for example as 'Run MyProg.exe'
RunEntryExec=Ekin %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Ikusi %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Ezarpenak Hurrengo Diska Behar Du
SelectDiskLabel2=Mesedez txertatu %1 Diska eta klikatu Ongi.%n%nDiska honetako agiriak azpian erakutsitakoa ez den beste agiritegi batean aurkitu badaitezke, sartu helburu zuzena edo klikatu Bilatu.
PathLabel=&Helburua:
FileNotInDir2="%1" agiria ezin da hemen aurkitu: "%2". Mesedez txertatu diska zuzena edo hautatu beste agiritegi bat.
SelectDirectoryLabel=Mesedez adierazi hurrengo diskaren kokalekua.

; *** Installation phase messages
SetupAborted=Ezarpena ez da osatu.%n%nMesedez zuzendu arazoa eta ekin Ezartzailea berriro.
AbortRetryIgnoreSelectAction=Hautatu ekintza
AbortRetryIgnoreRetry=&Berrsaiatu
AbortRetryIgnoreIgnore=&Errorea alde batera utzi eta jarraitu
AbortRetryIgnoreCancel=Instalazioa bertan behera utzi

; *** Installation status messages
StatusClosingApplications=Aplikazioak isten...
StatusCreateDirs=Zuzenbideak sortzen...
StatusExtractFiles=Agiriak ateratzen...
StatusCreateIcons=Lasterbideak sortzen...
StatusCreateIniEntries=INI sarrerak sortzen...
StatusCreateRegistryEntries=Erregistro sarrerak sortzen...
StatusRegisterFiles=Agiriak erregistratzen...
StatusSavingUninstall=Kentze argibideak gordetzen...
StatusRunProgram=Ezarpena amaitzen...
StatusRestartingApplications=Aplikazioak berrabiarazten...
StatusRollback=Aldaketak leheneratzen...

; *** Misc. errors
ErrorInternal2=Barneko akatsa: %1
ErrorFunctionFailedNoCode=%1 hutsegitea
ErrorFunctionFailed=%1 hutsegitea; kodea %2
ErrorFunctionFailedWithMessage=%1 hutsegitea; kodea %2.%n%3
ErrorExecutingProgram=Ezinezkoa agiria exekutatzea:%n%1

; *** Registry errors
ErrorRegOpenKey=Akatsa erregistro giltza irekitzerakoan:%n%1\%2
ErrorRegCreateKey=Akatsa erregistro giltza sortzerakoan:%n%1\%2
ErrorRegWriteKey=Akatsa erregistro giltza idazterakoan:%n%1\%2

; *** INI errors
ErrorIniEntry=Akatsa INI sarrera "%1" agirian sortzerakoan.

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=&Omitir fitxategi hau (ez da gomendatua)
FileAbortRetryIgnoreIgnoreNotRecommended=&Errorea alde batera utzi eta jarraitu (ez da gomendatua)
SourceIsCorrupted=Iturburu agiria hondatuta dago.
SourceDoesntExist="%1" iturburu agiria ez dago
ExistingFileReadOnly2=Dagoen fitxategia ezin da ordeztu, bakarrik irakurtzeko markatuta dagoelako.
ExistingFileReadOnlyRetry=&Ezabatu eguzkitarako eta irakurketarako atributua
ExistingFileReadOnlyKeepExisting=&Dagoen fitxategia mantentzea
ErrorReadingExistingDest=Akats bat gertatu da dagoen agiria irakurtzen saiatzerakoan:
FileExistsSelectAction=Hautatu ekintza
FileExists2=Fitxategia existitzen da.
FileExistsOverwriteExisting=&Dagoen fitxategia gainidatzi
FileExistsKeepExisting=&Dagoen fitxategia mantentzea
FileExistsOverwriteOrKeepAll=&Gauza bera egin honako gatazka hauetarako
ExistingFileNewerSelectAction=Hautatu ekintza
ExistingFileNewer2=Dagoen fitxategia instalatu nahi dena baino berriagoa da.
ExistingFileNewerOverwriteExisting=&Dagoen fitxategia gainidatzi
ExistingFileNewerKeepExisting=&Dagoen fitxategia mantentzea (gomendatua)
ExistingFileNewerOverwriteOrKeepAll=&Gauza bera egin honako gatazka hauetarako
ErrorChangingAttr=Errorea gertatu zen fitxategiaren atributuak aldatzen saiatzean:
ErrorCreatingTemp=Errore bat gertatu zen helmugako karpetan fitxategi bat sortzen saiatzean:
ErrorReadingSource=Errore bat gertatu zen jatorrizko fitxategia irakurtzen saiatzean:
ErrorCopying=Errorea gertatu da fitxategia kopiatzen saiatzean:
ErrorReplacingExistingFile=Errore bat gertatu zen dagoen fitxategia ordezten saiatzean:
ErrorRestartReplace=Huts egin zuen ordezteko saiakeran:
ErrorRenamingTemp=Errore bat gertatu zen helmugako karpetan fitxategi bat berrizendatzean:
ErrorRegisterServer=Ezinezkoa da DLL/OCX erregistratzea: %1
ErrorRegSvr32Failed=RegSvr32 %1 irteera-kodearekin huts egin zuen
ErrorRegisterTypeLib=Ezinezkoa da tipoen liburu-denda erregistratzea: %1

; *** Uninstall display name markings
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 ( %2, %3)
UninstallDisplayNameMark32Bit=32-bit
UninstallDisplayNameMark64Bit=64-bit
UninstallDisplayNameMarkAllUsers=Erabiltzaile guztiak
UninstallDisplayNameMarkCurrentUser=Egungo erabiltzailea

; *** Post-installation errors
ErrorOpeningReadme=Akats bat gertatu da IRAKURRI agiria irekitzen saiatzerakoan.
ErrorRestartingComputer=Ezartzaileak ezin du ordenagailua berrabiarazi. Mesedez egin hau eskuz.

; *** Uninstaller messages
UninstallNotFound="%1" agiria ez dago. Ezinezkoa kentzea
UninstallOpenError="%1" agiria ezin da ireki. Ezinezkoa kentzea
UninstallUnsupportedVer="%1" kentze ohar agiria kentzaile bertsio honek ezagutzen ez duen heuskarri batean dago. Ezinezkoa kentzea.
UninstallUnknownEntry=Sarrera ezezagun bat (%1) aurkitu da kentze oharrean
ConfirmUninstall=Zihur zaude %1 eta bere osagai guztiak erabat kentzea nahi dituzula?
UninstallOnlyOnWin64=Ezarpen hau 64-biteko Windows-etik bakarrik kendu daiteke.
OnlyAdminCanUninstall=Ezarpen hau administrari pribilegioak dituen erabiltzaile batek bakarrik kendu dezake.
UninstallStatusLabel=Mesedez itxaron %1 zure ordenagailutik kentzen den bitartean.
UninstalledAll=%1 ongi kendu da zure ordenagailutik.
UninstalledMost=%1-ren kentzea osatuta.%n%nZenbait gai ezin izan dira kendu. Hauek eskuz kendu daitezke.
UninstalledAndNeedsRestart=%1-ren kentzea osatzeko, zure ordenagailua berrabiarazi behar duzu.%n%nOrain berrabiaraztea nahi duzu?
UninstallDataCorrupted="%1" agiria hondatuta da. Ezinezkoa kentzea

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Ezabatu Agiri Elkarbanatua?
ConfirmDeleteSharedFile2=Sistemaren arabera hurrengo agiri elkarbanatua ez du inongo programak erabiliko hemendik aurrera. Kentzaileak agiri hau ezabatu dezan nahi duzu?%n%nProgramaren bat agiri hau erabiltzen ari da oraindik eta ezabatzen baduzu, programa hori ez da egoki ibiliko. Zihur ez bazaude, hautatu Ez. Agiria sisteman uzteak ez du inongo kalterik eragingo.
SharedFileNameLabel=Agiri izena:
SharedFileLocationLabel=Kokalekua:
WizardUninstalling=Kentze Egoera
StatusUninstalling=Kentzen %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Ezartzen %1.
ShutdownBlockReasonUninstallingApp=Kentzen %1.
; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]
;Inno Setup Built-in Custom Messages
NameAndVersion=%1 %2 bertsioa
AdditionalIcons=Ikur gehigarriak:
CreateDesktopIcon=Sortu &mahaigain ikurra
CreateQuickLaunchIcon=Sortu &Abiarazpen Azkarreko ikurra
ProgramOnTheWeb=%1 Webean
UninstallProgram=Kendu %1
LaunchProgram=Abiarazi %1
AssocFileExtension=&Elkartu %1 programa %2 agiri luzapenarekin
AssocingFileExtension=%1 programa %2 agiri luzapenarekin elkartzen...
AutoStartProgramGroupDescription=Abirazpena:
AutoStartProgram=Berezgaitasunez abiarazi %1
AddonHostProgramNotFound=%1 ezin da aurkitu hautatu duzun agiritegian.%n%nHorrela ere jarraitzea nahi duzu?
