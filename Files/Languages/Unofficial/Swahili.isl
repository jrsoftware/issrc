; *** Inno Setup version 6.5.0+ Swahili messages ***
;
; Translator: Stefan Kassler (mail@safkas.de)
;
; To download user-contributed translations of this file, go to:
;   https://jrsoftware.org/files/istrans/
;
; Terms used:
;   Setup = Kisakinishi, Uninstall (the program) = Kiondoaji, uninstall = ondoa,
;   installation = usakinishaji, uninstallation = uondoaji, Setup Wizard = Msaidizi wa Usakinishaji,
;   program/application = programu, component = kijenzi (vijenzi), task = kazi,
;   shortcut = njia ya mkato, Start Menu = Menyu ya Anza, folder = folda, directory = saraka,
;   path = njia, location = mahali, drive = diski, registry = sajili, archive = faili iliyobanwa,
;   OK = Sawa, Cancel = Ghairi, Yes = Ndiyo, No = Hapana, Next = Endelea, Back = Rudi,
;   Install = Sakinisha, Browse = Vinjari, Finish = Maliza

[LangOptions]
LanguageName=Kiswahili
LanguageID=$0441
LanguageCodePage=0

[Messages]

; *** Application titles
SetupAppTitle=Kisakinishi
SetupWindowTitle=Kisakinishi - %1
UninstallAppTitle=Kiondoaji
UninstallAppFullTitle=Kiondoaji cha %1

; *** Misc. common
InformationTitle=Taarifa
ConfirmTitle=Thibitisha
ErrorTitle=Hitilafu

; *** SetupLdr messages
SetupLdrStartupMessage=Hii itasakinisha %1. Je, unataka kuendelea?
LdrCannotCreateTemp=Imeshindwa kuunda faili ya muda. Usakinishaji umekatizwa
LdrCannotExecTemp=Imeshindwa kuendesha faili kwenye saraka ya muda. Usakinishaji umekatizwa
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nHitilafu %2: %3
SetupFileMissing=Faili %1 haipo kwenye saraka ya usakinishaji. Tafadhali rekebisha tatizo au upate nakala mpya ya programu.
SetupFileCorrupt=Faili za kisakinishi zimeharibika. Tafadhali pata nakala mpya ya programu.
SetupFileCorruptOrWrongVer=Faili za kisakinishi zimeharibika, au haziendani na toleo hili la Kisakinishi. Tafadhali rekebisha tatizo au upate nakala mpya ya programu.
InvalidParameter=Kigezo batili kimepitishwa kwenye mstari wa amri:%n%n%1
SetupAlreadyRunning=Kisakinishi tayari kinaendeshwa.
WindowsVersionNotSupported=Programu hii haiauni toleo la Windows linaloendeshwa na kompyuta yako.
WindowsServicePackRequired=Programu hii inahitaji %1 Service Pack %2 au jipya zaidi.
NotOnThisPlatform=Programu hii haitaendeshwa kwenye %1.
OnlyOnThisPlatform=Programu hii lazima iendeshwe kwenye %1.
OnlyOnTheseArchitectures=Programu hii inaweza kusakinishwa tu kwenye matoleo ya Windows yaliyoundwa kwa ajili ya miundo ifuatayo ya prosesa:%n%n%1
WinVersionTooLowError=Programu hii inahitaji %1 toleo %2 au jipya zaidi.
WinVersionTooHighError=Programu hii haiwezi kusakinishwa kwenye %1 toleo %2 au jipya zaidi.
AdminPrivilegesRequired=Lazima uwe umeingia kama msimamizi unaposakinisha programu hii.
PowerUserPrivilegesRequired=Lazima uwe umeingia kama msimamizi au kama mwanachama wa kikundi cha Watumiaji Wenye Mamlaka unaposakinisha programu hii.
SetupAppRunningError=Kisakinishi kimegundua kuwa %1 inaendeshwa kwa sasa.%n%nTafadhali funga nakala zake zote sasa, kisha ubofye Sawa ili kuendelea, au Ghairi ili kuondoka.
UninstallAppRunningError=Kiondoaji kimegundua kuwa %1 inaendeshwa kwa sasa.%n%nTafadhali funga nakala zake zote sasa, kisha ubofye Sawa ili kuendelea, au Ghairi ili kuondoka.

; *** Startup questions
PrivilegesRequiredOverrideTitle=Chagua Hali ya Usakinishaji
PrivilegesRequiredOverrideInstruction=Chagua hali ya usakinishaji
PrivilegesRequiredOverrideText1=%1 inaweza kusakinishwa kwa watumiaji wote (inahitaji haki za msimamizi), au kwa ajili yako tu.
PrivilegesRequiredOverrideText2=%1 inaweza kusakinishwa kwa ajili yako tu, au kwa watumiaji wote (inahitaji haki za msimamizi).
PrivilegesRequiredOverrideAllUsers=Sakinisha kwa &watumiaji wote
PrivilegesRequiredOverrideAllUsersRecommended=Sakinisha kwa &watumiaji wote (inapendekezwa)
PrivilegesRequiredOverrideCurrentUser=Sakinisha kwa ajili &yangu tu
PrivilegesRequiredOverrideCurrentUserRecommended=Sakinisha kwa ajili &yangu tu (inapendekezwa)

; *** Misc. errors
ErrorCreatingDir=Kisakinishi kimeshindwa kuunda saraka "%1"
ErrorTooManyFilesInDir=Imeshindwa kuunda faili kwenye saraka "%1" kwa sababu ina faili nyingi mno

; *** Setup common messages
ExitSetupTitle=Ondoka kwenye Kisakinishi
ExitSetupMessage=Usakinishaji haujakamilika. Ukiondoka sasa, programu haitasakinishwa.%n%nUnaweza kuendesha Kisakinishi tena wakati mwingine ili kukamilisha usakinishaji.%n%nUondoke kwenye Kisakinishi?
AboutSetupMenuItem=&Kuhusu Kisakinishi...
AboutSetupTitle=Kuhusu Kisakinishi
AboutSetupMessage=%1 %2%n%3%n%nTovuti ya %1:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< &Rudi
ButtonNext=&Endelea >
ButtonInstall=&Sakinisha
ButtonOK=Sawa
ButtonCancel=Ghairi
ButtonYes=&Ndiyo
ButtonYesToAll=Ndiyo kwa &zote
ButtonNo=&Hapana
ButtonNoToAll=Hapana kwa z&ote
ButtonFinish=&Maliza
ButtonBrowse=&Vinjari...
ButtonWizardBrowse=&Vinjari...
ButtonNewFolder=&Unda folda mpya

; *** "Select Language" dialog messages
SelectLanguageTitle=Chagua Lugha ya Kisakinishi
SelectLanguageLabel=Chagua lugha ya kutumia wakati wa usakinishaji.

; *** Common wizard text
ClickNext=Bofya Endelea ili kuendelea, au Ghairi ili kuondoka kwenye Kisakinishi.
BeveledLabel=
BrowseDialogTitle=Vinjari Folda
BrowseDialogLabel=Chagua folda kwenye orodha iliyo hapa chini, kisha ubofye Sawa.
NewFolderName=Folda Mpya

; *** "Welcome" wizard page
WelcomeLabel1=Karibu kwenye Msaidizi wa Usakinishaji wa [name]
WelcomeLabel2=Hii itasakinisha [name/ver] kwenye kompyuta yako.%n%nInapendekezwa ufunge programu nyingine zote kabla ya kuendelea.

; *** "Password" wizard page
WizardPassword=Nenosiri
PasswordLabel1=Usakinishaji huu unalindwa kwa nenosiri.
PasswordLabel3=Tafadhali weka nenosiri, kisha ubofye Endelea ili kuendelea. Manenosiri hutofautisha herufi kubwa na ndogo.
PasswordEditLabel=&Nenosiri:
IncorrectPassword=Nenosiri uliloweka si sahihi. Tafadhali jaribu tena.

; *** "License Agreement" wizard page
WizardLicense=Makubaliano ya Leseni
LicenseLabel=Tafadhali soma taarifa muhimu ifuatayo kabla ya kuendelea.
LicenseLabel3=Tafadhali soma Makubaliano ya Leseni yafuatayo. Lazima ukubali masharti ya makubaliano haya kabla ya kuendelea na usakinishaji.
LicenseAccepted=&Ninakubali makubaliano
LicenseNotAccepted=&Sikubali makubaliano

; *** "Information" wizard pages
WizardInfoBefore=Taarifa
InfoBeforeLabel=Tafadhali soma taarifa muhimu ifuatayo kabla ya kuendelea.
InfoBeforeClickLabel=Ukiwa tayari kuendelea na usakinishaji, bofya Endelea.
WizardInfoAfter=Taarifa
InfoAfterLabel=Tafadhali soma taarifa muhimu ifuatayo kabla ya kuendelea.
InfoAfterClickLabel=Ukiwa tayari kuendelea na usakinishaji, bofya Endelea.

; *** "User Information" wizard page
WizardUserInfo=Taarifa za Mtumiaji
UserInfoDesc=Tafadhali weka taarifa zako.
UserInfoName=&Jina la mtumiaji:
UserInfoOrg=&Shirika:
UserInfoSerial=&Nambari ya mfululizo:
UserInfoNameRequired=Lazima uweke jina.

; *** "Select Destination Location" wizard page
WizardSelectDir=Chagua Mahali pa Kusakinisha
SelectDirDesc=[name] isakinishwe wapi?
SelectDirLabel3=Kisakinishi kitasakinisha [name] kwenye folda ifuatayo.
SelectDirBrowseLabel=Ili kuendelea, bofya Endelea. Ukitaka kuchagua folda nyingine, bofya Vinjari.
DiskSpaceGBLabel=Nafasi tupu ya angalau GB [gb] kwenye diski inahitajika.
DiskSpaceMBLabel=Nafasi tupu ya angalau MB [mb] kwenye diski inahitajika.
CannotInstallToNetworkDrive=Kisakinishi hakiwezi kusakinisha kwenye diski ya mtandao.
CannotInstallToUNCPath=Kisakinishi hakiwezi kusakinisha kwenye njia ya UNC.
InvalidPath=Lazima uweke njia kamili yenye herufi ya diski; kwa mfano:%n%nC:\App%n%nau njia ya UNC katika muundo:%n%n\\server\share
InvalidDrive=Diski au folda ya UNC inayoshirikiwa uliyochagua haipo au haifikiki. Tafadhali chagua nyingine.
DiskSpaceWarningTitle=Nafasi ya Diski Haitoshi
DiskSpaceWarning=Kisakinishi kinahitaji angalau KB %1 za nafasi tupu ili kusakinisha, lakini diski iliyochaguliwa ina KB %2 tu.%n%nJe, unataka kuendelea hata hivyo?
DirNameTooLong=Jina la folda au njia ni refu mno.
InvalidDirName=Jina la folda si halali.
BadDirName32=Majina ya folda hayawezi kuwa na herufi yoyote kati ya zifuatazo:%n%n%1
DirExistsTitle=Folda Ipo
DirExists=Folda:%n%n%1%n%ntayari ipo. Je, ungependa kusakinisha kwenye folda hiyo hata hivyo?
DirDoesntExistTitle=Folda Haipo
DirDoesntExist=Folda:%n%n%1%n%nhaipo. Je, ungependa folda hiyo iundwe?

; *** "Select Components" wizard page
WizardSelectComponents=Chagua Vijenzi
SelectComponentsDesc=Vijenzi vipi visakinishwe?
SelectComponentsLabel2=Chagua vijenzi unavyotaka kusakinisha; ondoa alama kwenye vijenzi usivyotaka kusakinisha. Bofya Endelea ukiwa tayari kuendelea.
FullInstallation=Usakinishaji kamili
CompactInstallation=Usakinishaji uliobanwa
CustomInstallation=Usakinishaji maalum
NoUninstallWarningTitle=Vijenzi Vipo
NoUninstallWarning=Kisakinishi kimegundua kuwa vijenzi vifuatavyo tayari vimesakinishwa kwenye kompyuta yako:%n%n%1%n%nKuacha kuchagua vijenzi hivi hakutaviondoa.%n%nJe, ungependa kuendelea hata hivyo?
ComponentSize1=KB %1
ComponentSize2=MB %1
ComponentsDiskSpaceGBLabel=Uteuzi wa sasa unahitaji angalau GB [gb] za nafasi ya diski.
ComponentsDiskSpaceMBLabel=Uteuzi wa sasa unahitaji angalau MB [mb] za nafasi ya diski.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Chagua Kazi za Ziada
SelectTasksDesc=Kazi zipi za ziada zifanywe?
SelectTasksLabel2=Chagua kazi za ziada ambazo ungependa Kisakinishi kifanye kinaposakinisha [name], kisha ubofye Endelea.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Chagua Folda ya Menyu ya Anza
SelectStartMenuFolderDesc=Kisakinishi kiweke wapi njia za mkato za programu?
SelectStartMenuFolderLabel3=Kisakinishi kitaunda njia za mkato za programu kwenye folda ifuatayo ya Menyu ya Anza.
SelectStartMenuFolderBrowseLabel=Ili kuendelea, bofya Endelea. Ukitaka kuchagua folda nyingine, bofya Vinjari.
MustEnterGroupName=Lazima uweke jina la folda.
GroupNameTooLong=Jina la folda au njia ni refu mno.
InvalidGroupName=Jina la folda si halali.
BadGroupName=Jina la folda haliwezi kuwa na herufi yoyote kati ya zifuatazo:%n%n%1
NoProgramGroupCheck2=&Usiunde folda ya Menyu ya Anza

; *** "Ready to Install" wizard page
WizardReady=Tayari Kusakinisha
ReadyLabel1=Kisakinishi sasa kiko tayari kuanza kusakinisha [name] kwenye kompyuta yako.
ReadyLabel2a=Bofya Sakinisha ili kuendelea na usakinishaji, au bofya Rudi ikiwa unataka kukagua au kubadilisha mipangilio yoyote.
ReadyLabel2b=Bofya Sakinisha ili kuendelea na usakinishaji.
ReadyMemoUserInfo=Taarifa za mtumiaji:
ReadyMemoDir=Mahali pa kusakinisha:
ReadyMemoType=Aina ya usakinishaji:
ReadyMemoComponents=Vijenzi vilivyochaguliwa:
ReadyMemoGroup=Folda ya Menyu ya Anza:
ReadyMemoTasks=Kazi za ziada:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel2=Inapakua faili...
ButtonStopDownload=&Simamisha upakuaji
StopDownload=Je, una uhakika unataka kusimamisha upakuaji?
ErrorDownloadAborted=Upakuaji umekatizwa
ErrorDownloadFailed=Upakuaji umeshindwa: %1 %2
ErrorDownloadSizeFailed=Kupata ukubwa kumeshindwa: %1 %2
ErrorProgress=Maendeleo batili: %1 kati ya %2
ErrorFileSize=Ukubwa wa faili si sahihi: ulitarajiwa %1, umepatikana %2

; *** TExtractionWizardPage wizard page and ExtractArchive
ExtractingLabel=Inatoa faili...
ButtonStopExtraction=&Simamisha utoaji
StopExtraction=Je, una uhakika unataka kusimamisha utoaji?
ErrorExtractionAborted=Utoaji umekatizwa
ErrorExtractionFailed=Utoaji umeshindwa: %1

; *** Archive extraction failure details
ArchiveIncorrectPassword=Nenosiri si sahihi
ArchiveIsCorrupted=Faili iliyobanwa imeharibika
ArchiveUnsupportedFormat=Muundo wa faili iliyobanwa hauauniwi

; *** "Preparing to Install" wizard page
WizardPreparing=Inajiandaa Kusakinisha
PreparingDesc=Kisakinishi kinajiandaa kusakinisha [name] kwenye kompyuta yako.
PreviousInstallNotCompleted=Usakinishaji/uondoaji wa programu ya awali haukukamilika. Utahitaji kuanzisha upya kompyuta yako ili kukamilisha usakinishaji huo.%n%nBaada ya kuanzisha upya kompyuta yako, endesha Kisakinishi tena ili kukamilisha usakinishaji wa [name].
CannotContinue=Kisakinishi hakiwezi kuendelea. Tafadhali bofya Ghairi ili kuondoka.
ApplicationsFound=Programu zifuatazo zinatumia faili ambazo Kisakinishi kinahitaji kusasisha. Inapendekezwa uruhusu Kisakinishi kufunga programu hizi kiotomatiki.
ApplicationsFound2=Programu zifuatazo zinatumia faili ambazo Kisakinishi kinahitaji kusasisha. Inapendekezwa uruhusu Kisakinishi kufunga programu hizi kiotomatiki. Baada ya usakinishaji kukamilika, Kisakinishi kitajaribu kuanzisha upya programu hizo.
CloseApplications=&Funga programu kiotomatiki
DontCloseApplications=&Usifunge programu
ErrorCloseApplications=Kisakinishi kimeshindwa kufunga programu zote kiotomatiki. Inapendekezwa ufunge programu zote zinazotumia faili ambazo Kisakinishi kinahitaji kusasisha kabla ya kuendelea.
PrepareToInstallNeedsRestart=Lazima Kisakinishi kianzishe upya kompyuta yako. Baada ya kuanzisha upya kompyuta yako, endesha Kisakinishi tena ili kukamilisha usakinishaji wa [name].%n%nJe, ungependa kuanzisha upya sasa?

; *** "Installing" wizard page
WizardInstalling=Inasakinisha
InstallingLabel=Tafadhali subiri wakati Kisakinishi kinasakinisha [name] kwenye kompyuta yako.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Kukamilisha Msaidizi wa Usakinishaji wa [name]
FinishedLabelNoIcons=Kisakinishi kimemaliza kusakinisha [name] kwenye kompyuta yako.
FinishedLabel=Kisakinishi kimemaliza kusakinisha [name] kwenye kompyuta yako. Programu inaweza kufunguliwa kwa kuchagua njia za mkato zilizosakinishwa.
ClickFinish=Bofya Maliza ili kuondoka kwenye Kisakinishi.
FinishedRestartLabel=Ili kukamilisha usakinishaji wa [name], lazima Kisakinishi kianzishe upya kompyuta yako. Je, ungependa kuanzisha upya sasa?
FinishedRestartMessage=Ili kukamilisha usakinishaji wa [name], lazima Kisakinishi kianzishe upya kompyuta yako.%n%nJe, ungependa kuanzisha upya sasa?
ShowReadmeCheck=Ndiyo, ningependa kuona faili ya README
YesRadio=&Ndiyo, anzisha upya kompyuta sasa
NoRadio=&Hapana, nitaanzisha upya kompyuta baadaye
; used for example as 'Run MyProg.exe'
RunEntryExec=Endesha %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Tazama %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Kisakinishi Kinahitaji Diski Inayofuata
SelectDiskLabel2=Tafadhali ingiza Diski %1 kisha ubofye Sawa.%n%nIkiwa faili zilizo kwenye diski hii zinapatikana kwenye folda nyingine isipokuwa ile iliyoonyeshwa hapa chini, weka njia sahihi au bofya Vinjari.
PathLabel=&Njia:
FileNotInDir2=Faili "%1" haikupatikana katika "%2". Tafadhali ingiza diski sahihi au uchague folda nyingine.
SelectDirectoryLabel=Tafadhali bainisha mahali pa diski inayofuata.

; *** Installation phase messages
SetupAborted=Usakinishaji haukukamilika.%n%nTafadhali rekebisha tatizo kisha uendeshe Kisakinishi tena.
AbortRetryIgnoreSelectAction=Chagua kitendo
AbortRetryIgnoreRetry=&Jaribu tena
AbortRetryIgnoreIgnore=&Puuza hitilafu na uendelee
AbortRetryIgnoreCancel=Ghairi usakinishaji
RetryCancelSelectAction=Chagua kitendo
RetryCancelRetry=&Jaribu tena
RetryCancelCancel=Ghairi

; *** Installation status messages
StatusClosingApplications=Inafunga programu...
StatusCreateDirs=Inaunda saraka...
StatusExtractFiles=Inatoa faili...
StatusDownloadFiles=Inapakua faili...
StatusCreateIcons=Inaunda njia za mkato...
StatusCreateIniEntries=Inaunda maingizo ya INI...
StatusCreateRegistryEntries=Inaunda maingizo ya sajili...
StatusRegisterFiles=Inasajili faili...
StatusSavingUninstall=Inahifadhi taarifa za uondoaji...
StatusRunProgram=Inakamilisha usakinishaji...
StatusRestartingApplications=Inaanzisha upya programu...
StatusRollback=Inarejesha mabadiliko...

; *** Misc. errors
ErrorInternal2=Hitilafu ya ndani: %1
ErrorFunctionFailedNoCode=%1 imeshindwa
ErrorFunctionFailed=%1 imeshindwa; msimbo %2
ErrorFunctionFailedWithMessage=%1 imeshindwa; msimbo %2.%n%3
ErrorExecutingProgram=Imeshindwa kuendesha faili:%n%1

; *** Registry errors
ErrorRegOpenKey=Hitilafu wakati wa kufungua ufunguo wa sajili:%n%1\%2
ErrorRegCreateKey=Hitilafu wakati wa kuunda ufunguo wa sajili:%n%1\%2
ErrorRegWriteKey=Hitilafu wakati wa kuandika kwenye ufunguo wa sajili:%n%1\%2

; *** INI errors
ErrorIniEntry=Hitilafu wakati wa kuunda ingizo la INI kwenye faili "%1".

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=&Ruka faili hii (haipendekezwi)
FileAbortRetryIgnoreIgnoreNotRecommended=&Puuza hitilafu na uendelee (haipendekezwi)
SourceIsCorrupted=Faili chanzo imeharibika
SourceDoesntExist=Faili chanzo "%1" haipo
SourceVerificationFailed=Uthibitishaji wa faili chanzo umeshindwa: %1
VerificationSignatureDoesntExist=Faili ya sahihi "%1" haipo
VerificationSignatureInvalid=Faili ya sahihi "%1" si halali
VerificationKeyNotFound=Faili ya sahihi "%1" inatumia ufunguo usiojulikana
VerificationFileNameIncorrect=Jina la faili si sahihi
VerificationFileTagIncorrect=Lebo ya faili si sahihi
VerificationFileSizeIncorrect=Ukubwa wa faili si sahihi
VerificationFileHashIncorrect=Hashi ya faili si sahihi
ExistingFileReadOnly2=Faili iliyopo haikuweza kubadilishwa kwa sababu imewekwa alama ya kusoma tu.
ExistingFileReadOnlyRetry=&Ondoa sifa ya kusoma tu kisha ujaribu tena
ExistingFileReadOnlyKeepExisting=&Bakiza faili iliyopo
ErrorReadingExistingDest=Hitilafu imetokea wakati wa kujaribu kusoma faili iliyopo:
FileExistsSelectAction=Chagua kitendo
FileExists2=Faili tayari ipo.
FileExistsOverwriteExisting=&Andika juu ya faili iliyopo
FileExistsKeepExisting=&Bakiza faili iliyopo
FileExistsOverwriteOrKeepAll=&Fanya hivi kwa migongano inayofuata
ExistingFileNewerSelectAction=Chagua kitendo
ExistingFileNewer2=Faili iliyopo ni mpya zaidi kuliko ile ambayo Kisakinishi kinajaribu kusakinisha.
ExistingFileNewerOverwriteExisting=&Andika juu ya faili iliyopo
ExistingFileNewerKeepExisting=&Bakiza faili iliyopo (inapendekezwa)
ExistingFileNewerOverwriteOrKeepAll=&Fanya hivi kwa migongano inayofuata
ErrorChangingAttr=Hitilafu imetokea wakati wa kujaribu kubadilisha sifa za faili iliyopo:
ErrorCreatingTemp=Hitilafu imetokea wakati wa kujaribu kuunda faili kwenye saraka lengwa:
ErrorReadingSource=Hitilafu imetokea wakati wa kujaribu kusoma faili chanzo:
ErrorCopying=Hitilafu imetokea wakati wa kujaribu kunakili faili:
ErrorDownloading=Hitilafu imetokea wakati wa kujaribu kupakua faili:
ErrorExtracting=Hitilafu imetokea wakati wa kujaribu kutoa faili iliyobanwa:
ErrorReplacingExistingFile=Hitilafu imetokea wakati wa kujaribu kubadilisha faili iliyopo:
ErrorRestartReplace=RestartReplace imeshindwa:
ErrorRenamingTemp=Hitilafu imetokea wakati wa kujaribu kubadilisha jina la faili kwenye saraka lengwa:
ErrorRegisterServer=Imeshindwa kusajili DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 imeshindwa kwa msimbo wa kutoka %1
ErrorRegisterTypeLib=Imeshindwa kusajili maktaba ya aina: %1

; *** Uninstall display name markings
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=biti 32
UninstallDisplayNameMark64Bit=biti 64
UninstallDisplayNameMarkAllUsers=Watumiaji wote
UninstallDisplayNameMarkCurrentUser=Mtumiaji wa sasa

; *** Post-installation errors
ErrorOpeningReadme=Hitilafu imetokea wakati wa kujaribu kufungua faili ya README.
ErrorRestartingComputer=Kisakinishi kimeshindwa kuanzisha upya kompyuta. Tafadhali fanya hivyo wewe mwenyewe.

; *** Uninstaller messages
UninstallNotFound=Faili "%1" haipo. Haiwezi kuondoa.
UninstallOpenError=Faili "%1" haikuweza kufunguliwa. Haiwezi kuondoa
UninstallUnsupportedVer=Faili ya kumbukumbu ya uondoaji "%1" iko katika muundo usiotambuliwa na toleo hili la kiondoaji. Haiwezi kuondoa
UninstallUnknownEntry=Ingizo lisilojulikana (%1) limepatikana kwenye kumbukumbu ya uondoaji
ConfirmUninstall=Je, una uhakika unataka kuondoa kabisa %1 pamoja na vijenzi vyake vyote?
UninstallOnlyOnWin64=Usakinishaji huu unaweza kuondolewa tu kwenye Windows ya biti 64.
OnlyAdminCanUninstall=Usakinishaji huu unaweza kuondolewa tu na mtumiaji mwenye haki za msimamizi.
UninstallStatusLabel=Tafadhali subiri wakati %1 inaondolewa kwenye kompyuta yako.
UninstalledAll=%1 imeondolewa kwenye kompyuta yako kwa mafanikio.
UninstalledMost=Uondoaji wa %1 umekamilika.%n%nBaadhi ya vipengele havikuweza kuondolewa. Unaweza kuviondoa wewe mwenyewe.
UninstalledAndNeedsRestart=Ili kukamilisha uondoaji wa %1, lazima kompyuta yako ianzishwe upya.%n%nJe, ungependa kuanzisha upya sasa?
UninstallDataCorrupted=Faili "%1" imeharibika. Haiwezi kuondoa

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Uondoe Faili Inayoshirikiwa?
ConfirmDeleteSharedFile2=Mfumo unaonyesha kuwa faili ifuatayo inayoshirikiwa haitumiwi tena na programu yoyote. Je, ungependa Kiondoaji kiondoe faili hii inayoshirikiwa?%n%nIkiwa programu yoyote bado inatumia faili hii na ikaondolewa, programu hizo huenda zisifanye kazi ipasavyo. Ikiwa huna uhakika, chagua Hapana. Kuiacha faili hii kwenye mfumo wako hakutasababisha madhara yoyote.
SharedFileNameLabel=Jina la faili:
SharedFileLocationLabel=Mahali:
WizardUninstalling=Hali ya Uondoaji
StatusUninstalling=Inaondoa %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Inasakinisha %1.
ShutdownBlockReasonUninstallingApp=Inaondoa %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 toleo %2
AdditionalIcons=Njia za mkato za ziada:
CreateDesktopIcon=Unda njia ya mkato kwenye eneo-&kazi
CreateQuickLaunchIcon=Unda njia ya mkato ya &Uzinduzi wa Haraka
ProgramOnTheWeb=%1 kwenye Wavuti
UninstallProgram=Ondoa %1
LaunchProgram=Zindua %1
AssocFileExtension=&Husisha %1 na kiendelezi cha faili %2
AssocingFileExtension=Inahusisha %1 na kiendelezi cha faili %2...
AutoStartProgramGroupDescription=Kuanzisha:
AutoStartProgram=Anzisha %1 kiotomatiki
AddonHostProgramNotFound=%1 haikupatikana kwenye folda uliyochagua.%n%nJe, unataka kuendelea hata hivyo?
