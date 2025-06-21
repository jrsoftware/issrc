; *** Inno Setup version 6.0.0+ Basque (Euskara) messages ***
;
; Maintained by Iker Ocio Zuazo (iker@ikerocio.com)
; Basque.isl version 1.0 (20250621)
; Default.isl version 6.0.0
;
; Based on Spanish.isl. Thanks to Jorge Andres Brugger and the original contributors.


[LangOptions]
; The following three entries are very important. Be sure to read and 
; understand the '[LangOptions] section' topic in the help file.
LanguageName=Euskara
LanguageID=$042D
LanguageCodePage=1252
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
SetupAppTitle=Instalatu
SetupWindowTitle=Instalatu - %1
UninstallAppTitle=Desinstalatu
UninstallAppFullTitle=Desinstalatu - %1

; *** Misc. common
InformationTitle=Informazioa
ConfirmTitle=Berretsi
ErrorTitle=Errorea

; *** SetupLdr messages
SetupLdrStartupMessage=Programa honek %1 instalatuko du. Jarraitu nahi duzu?
LdrCannotCreateTemp=Ezin da aldi baterako fitxategia sortu. Instalazioa eten da.
LdrCannotExecTemp=Ezin da aldi baterako karpetako fitxategia exekutatu. Instalazioa eten da.
HelpTextNote=Oharra:

; *** Startup error messages
LastErrorMessage=%1.%n%nErrorea %2: %3
SetupFileMissing=Ez da %1 fitxategia aurkitu instalazio karpetan. Mesedez, konpondu arazoa edo lortu programaren beste kopia bat.
SetupFileCorrupt=Instalazio-fitxategiak hondatuta daude. Mesedez, lortu programaren kopia berri bat.
SetupFileCorruptOrWrongVer=Instalazio-fitxategiak hondatuta daude edo ez dira bateragarriak instalatzailearen bertsio honekin. Mesedez, konpondu arazoa edo lortu programaren kopia berri bat.
InvalidParameter=Komando-lerroan parametro baliogabe bat erabili da:%n%n%1
SetupAlreadyRunning=Instalatzailea dagoeneko exekutatzen ari da.
WindowsVersionNotSupported=Programa hau ez da zure Windows bertsioarekin bateragarria.
WindowsServicePackRequired=Programa honek %1 Service Pack %2 edo berriagoa behar du.
NotOnThisPlatform=Programa hau ezin da %1 plataforman exekutatu.
OnlyOnThisPlatform=Programa hau %1 plataforman exekutatu behar da.
OnlyOnTheseArchitectures=Programa hau ondorengo prozesadore-arkitekturentzako Windows bertsioetan soilik instala daiteke:%n%n%1
WinVersionTooLowError=Programa honek %1 %2 bertsioa edo berriagoa behar du.
WinVersionTooHighError=Programa hau ezin da %1 %2 bertsioa edo berriagoan instalatu.
AdminPrivilegesRequired=Programa hau instalatzeko administratzaile gisa saioa hasi behar duzu.
PowerUserPrivilegesRequired=Programa hau instalatzeko administratzaile edo Erabiltzaile Aurreratuen taldeko kide izan behar duzu.
SetupAppRunningError=Instalatzaileak detektatu du %1 exekutatzen ari dela.%n%nMesedez, itxi orain eta egin klik Ados botoian jarraitzeko edo Utzi botoian irteteko.
UninstallAppRunningError=Desinstalatzaileak detektatu du %1 exekutatzen ari dela.%n%nMesedez, itxi orain eta egin klik Ados botoian jarraitzeko edo Utzi botoian irteteko.

; *** Startup questions
PrivilegesRequiredOverrideTitle=Instalazio Modua Hautatu
PrivilegesRequiredOverrideInstruction=Hautatu instalazio modua
PrivilegesRequiredOverrideText1=%1 erabil daiteke erabiltzaile guztientzat instalatzeko (administratzaile baimenak behar dira), edo zuretzat bakarrik.
PrivilegesRequiredOverrideText2=%1 instalatu daiteke zuretzat bakarrik, edo erabiltzaile guztientzat (administratzaile baimenak behar dira).
PrivilegesRequiredOverrideAllUsers=Instalatu &erabiltzaile guztientzat
PrivilegesRequiredOverrideAllUsersRecommended=Instalatu &erabiltzaile guztientzat (gomendatua)
PrivilegesRequiredOverrideCurrentUser=Instalatu &niretzat bakarrik
PrivilegesRequiredOverrideCurrentUserRecommended=Instalatu &niretzat bakarrik (gomendatua)

; *** Misc. errors
ErrorCreatingDir=Instalatzaileak ezin izan du "%1" karpeta sortu
ErrorTooManyFilesInDir=Ezin da fitxategirik sortu "%1" karpetan, fitxategi gehiegi daudelako

; *** Setup common messages
ExitSetupTitle=Irten Instalaziotik
ExitSetupMessage=Instalazioa oraindik ez da amaitu. Orain bertan behera uzten baduzu, programa ez da instalatuko.%n%nInstalatzailea beste une batean exekutatu dezakezu instalazioa osatzeko.%n%nInstalaziotik irten nahi duzu?
AboutSetupMenuItem=&Instalatzaileari buruz...
AboutSetupTitle=Instalatzaileari buruz
AboutSetupMessage=%1 %2 bertsioa%n%3%n%n%1 webgunea:%n%4
AboutSetupNote=
TranslatorNote=Itzulpena euskaraz: Iker Ocio Zuazo (ikerocio.com)

; *** Buttons
ButtonBack=< &Atzera
ButtonNext=&Hurrengoa >
ButtonInstall=&Instalatu
ButtonOK=Ados
ButtonCancel=Utzi
ButtonYes=&Bai
ButtonYesToAll=Bai &guztiak
ButtonNo=&Ez
ButtonNoToAll=E&z guztiak
ButtonFinish=&Amaitu
ButtonBrowse=&Arakatu...
ButtonWizardBrowse=&Arakatu...
ButtonNewFolder=Karpeta &berria sortu

; *** "Select Language" dialog messages
SelectLanguageTitle=Hautatu Instalazioaren Hizkuntza
SelectLanguageLabel=Hautatu instalazioan erabiliko den hizkuntza.

; *** Common wizard text
ClickNext=Egin klik Hurrengoa botoian jarraitzeko edo Utzi botoian instalazioa amaitzeko.
BeveledLabel=
BrowseDialogTitle=Karpeta bilatu
BrowseDialogLabel=Hautatu karpeta bat eta egin klik Ados botoian.
NewFolderName=Karpeta Berria

; *** "Welcome" wizard page
WelcomeLabel1=Ongi etorri [name] instalazioaren morroi honetara
WelcomeLabel2=Programa honek [name/ver] instalatuko du zure sisteman.%n%nGomendagarria da beste aplikazio guztiak ixtea jarraitu aurretik.

; *** "Password" wizard page
WizardPassword=Pasahitza
PasswordLabel1=Instalazio hau pasahitz batekin babestuta dago.
PasswordLabel3=Mesedez, sartu pasahitza eta egin klik Hurrengoa botoian jarraitzeko. Kontuan izan maiuskulak eta minuskulak bereizten direla.
PasswordEditLabel=&Pasahitza:
IncorrectPassword=Pasahitza ez da zuzena. Saiatu berriro, mesedez.

; *** "License Agreement" wizard page
WizardLicense=Lizentzia Hitzarmena
LicenseLabel=Garrantzitsua da hurrengo informazioa irakurtzea jarraitu aurretik.
LicenseLabel3=Mesedez, irakurri lizentzia-hitzarmen hau. Instalazioarekin jarraitu aurretik baldintzak onartu behar dituzu.
LicenseAccepted=Hitzarmena &onartzen dut
LicenseNotAccepted=Ez dut hitzarmena &onartzen

; *** "Information" wizard pages
WizardInfoBefore=Informazioa
InfoBeforeLabel=Garrantzitsua da jarraitu aurretik hurrengo informazioa irakurtzea.
InfoBeforeClickLabel=Instalazioarekin jarraitzeko prest zaudenean, egin klik Hurrengoa botoian.
WizardInfoAfter=Informazioa
InfoAfterLabel=Garrantzitsua da jarraitu aurretik hurrengo informazioa irakurtzea.
InfoAfterClickLabel=Jarraitzeko prest zaudenean, egin klik Hurrengoa botoian.

; *** "User Information" wizard page
WizardUserInfo=Erabiltzailearen Informazioa
UserInfoDesc=Mesedez, sartu zure datuak.
UserInfoName=Erabiltzailearen &izena:
UserInfoOrg=&Erakundea:
UserInfoSerial=&Serie zenbakia:
UserInfoNameRequired=Izena sartu behar duzu.

; *** "Select Destination Location" wizard page
WizardSelectDir=Hautatu Helmugako Karpeta
SelectDirDesc=Non instalatu behar da [name]?
SelectDirLabel3=Programa hurrengo karpetan instalatuko da: [name].
SelectDirBrowseLabel=Jarraitzeko, egin klik Hurrengoa botoian. Beste karpeta bat hautatzeko, egin klik Arakatu botoian.
DiskSpaceGBLabel=Gutxienez [gb] GB libre egon behar dira diskoan.
DiskSpaceMBLabel=Gutxienez [mb] MB-ko disko espazio librea behar da.
CannotInstallToNetworkDrive=Instalatzaileak ezin du sareko unitate batean instalatu.
CannotInstallToUNCPath=Instalatzaileak ezin du UNC bide batean instalatu.
InvalidPath=Bide osoa sartu behar duzu, unitatearen hizkiarekin; adibidez:%n%nC:\APP%n%nhau ez:%n%n\\zerbitzaria\partekatua
InvalidDrive=Hautatutako unitatea edo UNC bidea ez da existitzen edo ez dago eskuragarri. Hautatu beste bat, mesedez.
DiskSpaceWarningTitle=Disko Gune Nahikorik Ez
DiskSpaceWarning=Instalazioak gutxienez %1 KB-ko leku librea behar du, baina hautatutako unitateak %2 KB baino ez dauka.%n%nJarraitu nahi duzu hala ere?
DirNameTooLong=Karpeta izena edo bidea luzeegia da.
InvalidDirName=Karpeta izena baliogabea da.
BadDirName32=Karpeta izenek ezin dituzte karaktere hauek eduki:%n%n%1
DirExistsTitle=Karpeta Dagoeneko Existitzen da
DirExists=Karpeta hau:%n%n%1%n%ndagoeneko existitzen da. Instalatu nahi duzu hala ere karpeta honetan?
DirDoesntExistTitle=Karpeta Ez da Existitzen
DirDoesntExist=Karpeta hau:%n%n%1%n%nEz da existitzen. Sortu nahi duzu?

; *** "Select Components" wizard page
WizardSelectComponents=Hautatu Osagaiak
SelectComponentsDesc=Zein osagai instalatu behar dira?
SelectComponentsLabel2=Hautatu instalatu nahi dituzun osagaiak eta desmarkatu nahi ez dituzunak. Jarraitzeko, egin klik Hurrengoa botoian.
FullInstallation=Instalazio Osoa
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Instalazio Trinkoa
CustomInstallation=Instalazio Pertsonalizatua
NoUninstallWarningTitle=Osagaiak Aurkitu Dira
NoUninstallWarning=Instalatzaileak hurrengo osagaiak dagoeneko zure sisteman instalatuta daudela detektatu du:%n%n%1%n%nOsagai hauek desmarkatzeak ez ditu desinstalatuko.%n%nJarraitu nahi duzu hala ere?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=Aukeratutako osagaiek gutxienez [gb] GB disko-leku behar dute.
ComponentsDiskSpaceMBLabel=Hautatutako aukerek gutxienez [mb] MB-ko disko lekua behar dute.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Hautatu Beste Atazak
SelectTasksDesc=Zein ataza gehigarri egin behar dira?
SelectTasksLabel2=Hautatu [name] instalazioan burutu nahi dituzun ataza gehigarriak eta egin klik Hurrengoa botoian.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Hautatu Hasierako Menuaren Karpeta
SelectStartMenuFolderDesc=Non kokatu behar dira programaren lasterbideak?
SelectStartMenuFolderLabel3=Instalatzaileak programaren lasterbideak hurrengo Hasierako Menuaren karpetan sortuko ditu.
SelectStartMenuFolderBrowseLabel=Jarraitzeko, egin klik Hurrengoa botoian. Beste karpeta bat hautatzeko, egin klik Arakatu botoian.
MustEnterGroupName=Karpeta izen bat eman behar duzu.
GroupNameTooLong=Karpeta izena edo bidea luzeegia da.
InvalidGroupName=Karpeta izena baliogabea da.
BadGroupName=Karpeta izenak ezin ditu karaktere hauek eduki:%n%n%1
NoProgramGroupCheck2=&Ez sortu karpeta bat Hasierako Menuan

; *** "Ready to Install" wizard page
WizardReady=Prest Instalatzeko
ReadyLabel1=Orain [name] zure sisteman instalatzeko prest dago.
ReadyLabel2a=Jarraitzeko, egin klik Instalatu botoian edo egin klik Atzera botoian konfigurazioa berrikusi edo aldatzeko.
ReadyLabel2b=Jarraitzeko, egin klik Instalatu botoian.
ReadyMemoUserInfo=Erabiltzailearen informazioa:
ReadyMemoDir=Helmugako karpeta:
ReadyMemoType=Instalazio mota:
ReadyMemoComponents=Hautatutako osagaiak:
ReadyMemoGroup=Hasierako Menuaren karpeta:
ReadyMemoTasks=Ataza gehigarriak:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel=Fitxategi osagarriak deskargatzen...
ButtonStopDownload=&Gelditu deskarga
StopDownload=Ziur zaude deskarga gelditu nahi duzula?
ErrorDownloadAborted=Deskarga bertan behera utzi da
ErrorDownloadFailed=Deskargak huts egin du: %1 %2
ErrorDownloadSizeFailed=Tamaina eskuratzeak huts egin du: %1 %2
ErrorFileHash1=Fitxategiaren hash-ak huts egin du: %1
ErrorFileHash2=Fitxategiaren hash baliogabea: espero zen %1, aurkitua %2
ErrorProgress=Balio gabeko aurrerapena: %1 / %2
ErrorFileSize=Fitxategiaren tamaina baliogabea: espero zen %1, aurkitua %2

; *** TExtractionWizardPage wizard page and Extract7ZipArchive
ExtractionLabel=Fitxategi osagarriak erauzten...
ButtonStopExtraction=&Gelditu erauzketa
StopExtraction=Ziur zaude erauzketa gelditu nahi duzula?
ErrorExtractionAborted=Erauzketa bertan behera utzi da
ErrorExtractionFailed=Erauzketak huts egin du: %1

; *** "Preparing to Install" wizard page
WizardPreparing=Instalatzeko prestatzen
PreparingDesc=Instalatzailea zure sisteman [name] instalatzeko prestatzen ari da.
PreviousInstallNotCompleted=Aurreko programa baten instalazioa/desinstalazioa ez da amaitu. Sistema berrabiarazi behar duzu instalazio hori amaitzeko.%n%nSistema berrabiarazi ondoren, exekutatu berriz instalatzailea [name] instalatzeko.
CannotContinue=Instalatzaileak ezin du jarraitu. Mesedez, sakatu Utzi irteteko.
ApplicationsFound=Hurrengo aplikazioek instalatzaileak eguneratu behar dituen fitxategiak erabiltzen ari dira. Gomendagarria da instalatzaileari aplikazio hauek automatikoki ixtea uztea.
ApplicationsFound2=Hurrengo aplikazioek instalatzaileak eguneratu behar dituen fitxategiak erabiltzen ari dira. Gomendagarria da instalatzaileari aplikazio hauek automatikoki ixtea uztea. Instalazioa amaitutakoan, instalatzaileak aplikazioak berrabiaratzen saiatuko da.
CloseApplications=&Itxi aplikazioak automatikoki
DontCloseApplications=&Ez itxi aplikazioak
ErrorCloseApplications=Instalatzaileak ezin izan ditu aplikazio guztiak automatikoki itxi. Jarraitu aurretik, gomendagarria da instalatzaileak eguneratu behar dituen fitxategiak erabiltzen dituzten aplikazio guztiak eskuz ixtea.
PrepareToInstallNeedsRestart=Instalatzaileak sistema berrabiaraztea behar du. Behin sistema berrabiarazi ondoren, exekutatu berriro instalatzailea [name] instalazioa osatzeko.%n%nSistema orain berrabiarazi nahi duzu?

; *** "Installing" wizard page
WizardInstalling=Instalatzen
InstallingLabel=Itxaron mesedez, [name] zure sisteman instalatzen ari da.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=[name](r)en instalazioa amaitzen
FinishedLabelNoIcons=Programa [name] zure sisteman instalatu da.
FinishedLabel=Programa [name] zure sisteman instalatu da. Aplikazioa exekutatu dezakezu sortutako lasterbideak erabiliz.
ClickFinish=Egin klik Amaitu botoian instalatzailetik irteteko.
FinishedRestartLabel=[name](r)en instalazioa amaitzeko, zure sistema berrabiarazi behar da. Berrabiarazi nahi duzu orain?
FinishedRestartMessage=[name](r)en instalazioa amaitzeko, zure sistema berrabiarazi behar da.%n%nBerrabiarazi nahi duzu orain?
ShowReadmeCheck=Bai, README fitxategia ikusi nahi dut
YesRadio=&Bai, sistema orain berrabiarazi nahi dut
NoRadio=&Ez, sistema geroago berrabiaraziko dut
; used for example as 'Run MyProg.exe'
RunEntryExec=Exekutatu %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Ikusi %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Instalatzaileak Hurrengo Diskoa Behar Du
SelectDiskLabel2=Mesedez, sartu %1. Diskoa eta egin klik Ados botoian.%n%nFitxategiak behean adierazitako karpetan ez badaude, sartu ibilbide zuzena edo egin klik Arakatu botoian.
PathLabel=&Ibilbidea:
FileNotInDir2=Ezin izan da "%1" fitxategia aurkitu hemen: "%2". Mesedez, sartu disko zuzena edo hautatu beste karpeta bat.
SelectDirectoryLabel=Mesedez, adierazi hurrengo diskoaren kokapena.

; *** Installation phase messages
SetupAborted=Instalazioa ez da amaitu.%n%nMesedez, konpondu arazoa eta exekutatu berriz instalatzailea.
AbortRetryIgnoreSelectAction=Hautatu ekintza
AbortRetryIgnoreRetry=&Saiatu berriro
AbortRetryIgnoreIgnore=&Ez ikusi egin errorea eta jarraitu
AbortRetryIgnoreCancel=Utzi instalazioa

; *** Installation status messages
StatusClosingApplications=Aplikazioak ixten...
StatusCreateDirs=Karpetak sortzen...
StatusExtractFiles=Fitxategiak ateratzen...
StatusCreateIcons=Lasterbideak sortzen...
StatusCreateIniEntries=INI sarrerak sortzen...
StatusCreateRegistryEntries=Erregistro-sarrerak sortzen...
StatusRegisterFiles=Fitxategiak erregistratzen...
StatusSavingUninstall=Desinstalaziorako informazioa gordetzen...
StatusRunProgram=Instalazioa amaitzen...
StatusRestartingApplications=Aplikazioak berrabiarazten...
StatusRollback=Aldaketak desegiten...

; *** Misc. errors
ErrorInternal2=Barneko errorea: %1
ErrorFunctionFailedNoCode=%1 huts egin du
ErrorFunctionFailed=%1 huts egin du; kodea %2
ErrorFunctionFailedWithMessage=%1 huts egin du; kodea %2.%n%3
ErrorExecutingProgram=Ezin izan da fitxategia exekutatu:%n%1

; *** Registry errors
ErrorRegOpenKey=Errorea erregistro-gakoa irekitzean:%n%1\%2
ErrorRegCreateKey=Errorea erregistro-gakoa sortzean:%n%1\%2
ErrorRegWriteKey=Errorea erregistro-gakoan idaztean:%n%1\%2

; *** INI errors
ErrorIniEntry=Errorea INI sarrera sortzean fitxategian "%1".

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=&Fitxategi hau baztertu (ez gomendatua)
FileAbortRetryIgnoreIgnoreNotRecommended=&Errorea alde batera utzi eta jarraitu (ez gomendatua)
SourceIsCorrupted=Iturburuko fitxategia hondatuta dago
SourceDoesntExist=Iturburuko fitxategia "%1" ez da existitzen
ExistingFileReadOnly2=Dagoen fitxategia ezin da ordezkatu soilik-irakurri gisa markatuta dagoelako.
ExistingFileReadOnlyRetry=&Kendu soilik-irakurri atributua eta saiatu berriro
ExistingFileReadOnlyKeepExisting=&Mantendu dagoen fitxategia
ErrorReadingExistingDest=Errorea gertatu da jomugako fitxategia irakurtzen saiatzean:
FileExistsSelectAction=Aukeratu ekintza
FileExists2=Fitxategia jada existitzen da.
FileExistsOverwriteExisting=&Gainidatzi dagoen fitxategia
FileExistsKeepExisting=&Mantendu dagoen fitxategia
FileExistsOverwriteOrKeepAll=&Errepikatu hurrengo gatazketarako ere
ExistingFileNewerSelectAction=Aukeratu ekintza
ExistingFileNewer2=Dagoen fitxategia instalatu nahi dena baino berriagoa da.
ExistingFileNewerOverwriteExisting=&Gainidatzi dagoen fitxategia
ExistingFileNewerKeepExisting=&Mantendu dagoen fitxategia (gomendatua)
ExistingFileNewerOverwriteOrKeepAll=&Errepikatu hurrengo gatazketarako ere
ErrorChangingAttr=Errorea gertatu da fitxategiaren atributuak aldatzean:
ErrorCreatingTemp=Errorea gertatu da jomugako karpetan fitxategi bat sortzean:
ErrorReadingSource=Errorea gertatu da iturburuko fitxategia irakurtzean:
ErrorCopying=Errorea gertatu da fitxategia kopiatzean:
ErrorReplacingExistingFile=Errorea gertatu da dagoen fitxategia ordezkatzean:
ErrorRestartReplace=Huts egin du ordezkapen berriro saioan:
ErrorRenamingTemp=Errorea gertatu da jomugako karpetan fitxategia berrizendatzean:
ErrorRegisterServer=Ezin izan da DLL/OCX erregistratu: %1
ErrorRegSvr32Failed=RegSvr32-k huts egin du irteera-kodearekin %1
ErrorRegisterTypeLib=Ezin izan da mota-liburutegia erregistratu: %1

; *** Desinstalazioaren bistaratze-izenaren markak
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32-bit
UninstallDisplayNameMark64Bit=64-bit
UninstallDisplayNameMarkAllUsers=Erabiltzaile guztiak
UninstallDisplayNameMarkCurrentUser=Uneko erabiltzailea

; *** Post-installation errors
ErrorOpeningReadme=Errorea gertatu da README fitxategia irekitzean.
ErrorRestartingComputer=Instalatzaileak ezin izan du ordenagailua berrabiarazi. Mesedez, egin ezazu eskuz.

; *** Uninstaller messages
UninstallNotFound=" %1 " fitxategia ez da existitzen. Ezin da desinstalatu.
UninstallOpenError=" %1 " fitxategia ezin izan da ireki. Ezin da desinstalatu.
UninstallUnsupportedVer=“%1” desinstalazio-erregistroaren formatua ez da bertsio honekin bateragarria. Ezin da desinstalatu.
UninstallUnknownEntry=Desinstalazioaren erregistroan sarrera ezezaguna aurkitu da (%1)
ConfirmUninstall=Ziur zaude %1 eta bere osagai guztiak erabat desinstalatu nahi dituzula?
UninstallOnlyOnWin64=Programa hau soilik 64 biteko Windows sistemetan desinstala daiteke.
OnlyAdminCanUninstall=Programa hau administratzaile baimenak dituen erabiltzaile batek soilik desinstala dezake.
UninstallStatusLabel=Itxaron mesedez %1 zure sistematik desinstalatzen den bitartean.
UninstalledAll=%1 zure sistematik arrakastaz desinstalatu da.
UninstalledMost=%1 desinstalazioa burutu da.%n%nElementu batzuk ezin izan dira ezabatu, baina eskuz ezaba ditzakezu nahi izanez gero.
UninstalledAndNeedsRestart=%1 desinstalazioa amaitzeko, zure ordenagailua berrabiarazi behar da.%n%nBerrabiarazi orain?
UninstallDataCorrupted="%1" fitxategia hondatuta dago. Ezin da desinstalatu.

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Fitxategi partekatua ezabatu?
ConfirmDeleteSharedFile2=Sistema honek adierazten du hurrengo fitxategi partekatua ez dela beste inongo programak erabiltzen. Ezabatu nahi duzu fitxategi hau?%n%nFitxategia ezabatzen baduzu eta beste programa batzuek erabiltzen badute, programa horiek ez dute behar bezala funtzionatuko. Ziur ez bazaude, aukeratu Ez. Fitxategia zure sisteman uzteak ez du kalterik eragingo.
SharedFileNameLabel=Fitxategia:
SharedFileLocationLabel=Kokalekua:
WizardUninstalling=Desinstalazioaren egoera
StatusUninstalling=%1 desinstalatzen...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=%1 instalatzen.
ShutdownBlockReasonUninstallingApp=%1 desinstalatzen.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 %2 bertsioa
AdditionalIcons=Ikono osagarriak:
CreateDesktopIcon=Sortu lasterbide bat &mahaigainean
CreateQuickLaunchIcon=Sortu lasterbide bat &Abiarazte Azkarrean
ProgramOnTheWeb=%1 webgunean
UninstallProgram=%1 desinstalatu
LaunchProgram=%1 exekutatu
AssocFileExtension=&Elkartu %1 fitxategi luzapenarekin %2
AssocingFileExtension=%1 elkartzen %2 fitxategi luzapenarekin...
AutoStartProgramGroupDescription=Abiaraztea:
AutoStartProgram=%1 automatikoki abiarazi
AddonHostProgramNotFound=%1 ezin izan da aurkitu hautatutako karpetan.%n%nJarraitu nahi duzu hala ere?
