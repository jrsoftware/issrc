; *** Inno Setup version 6.5.0+ Serbian (Cyrillic) messages ***
;
; To download user-contributed translations of this file, go to:
;   https://jrsoftware.org/files/istrans/
;
; Maintained by Davor Nikolić (support@trackworktime.com).
; Based on previous translations of Rancher (theranchcowboy@gmail.com)
;
; Note: When translating this text:
; - All translations should back-translate to the same meaning.
; - Do not add to or change the meaning of messages to suit your personal taste.
; - Do not add or remove sentences, or add or omit information within sentences.
; - Do not remove these words: all, only, automatically, now, later, may, must, and not.
; - Do not add periods (.) or colons (:) or ellipses (...) to the end of messages that didn't have them already.
;   Exception: for languages with their own period character, such as Japanese and Chinese, it was added
;   to the end of messages as needed.
; - Do not remove periods or colons or ellipses or question marks from the end of messages.
;   Exception: Thai and Lao remove trailing periods and question marks.
; - Do not replace periods with colons. Replacing '...' with '…' is allowed.
; - Do not add or remove number placeholders (%1, %2, etc.). Changing the order is allowed.
; - Do not add or remove named placeholders ([name], [name/ver], etc.). Do not replace one with another.
; - Do not add or remove line breaks (%n).
; - Do not add accelerators (&) or create collisions. Remove an accelerator only if it cannot be moved to another letter.
; - Do not add new custom messages to the [CustomMessages] section.
; - Do not translate comments like these.
; - Keep AboutSetupNote empty.
; Before you start, decide how you will translate each of these recurring terms:
; - Setup, Uninstall (the program), uninstall (the verb), Cannot uninstall,
;   program, application, component, task,
;   shortcut, Start Menu, folder, directory, path, location, drive,
;   existing file, source file, registry, INI entries, README,
;   computer, Windows, version, administrator, all users, current user,
;   Downloading files, Extracting files, aborted, corrupted, close (applications), restart,
;   Select action, try again, anyway, at least,
;   and the button captions OK, Cancel, Yes, No, Next, Back, Install, Browse, Finish.
; - When in doubt, use Microsoft Terminology Search from:
;   https://msit.powerbi.com/view?r=eyJrIjoiODJmYjU4Y2YtM2M0ZC00YzYxLWE1YTktNzFjYmYxNTAxNjQ0IiwidCI6IjcyZjk4OGJmLTg2ZjEtNDFhZi05MWFiLTJkN2NkMDExZGI0NyIsImMiOjV9
; Removing this section from your translation is allowed, but do not disregard it.

[LangOptions]
; The following three entries are very important. Be sure to read and 
; understand the '[LangOptions] section' topic in the help file.
LanguageName=Српски
LanguageID=$0C1A
; LanguageCodePage should always be set if possible, even if this file is Unicode
; For English it's set to zero anyway because English only uses ASCII characters
LanguageCodePage=1251
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
SetupAppTitle=Инсталација
SetupWindowTitle=Инсталација - %1
UninstallAppTitle=Деинсталација
UninstallAppFullTitle=Деинсталација програма %1

; *** Misc. common
InformationTitle=Информације
ConfirmTitle=Потврда
ErrorTitle=Грешка

; *** SetupLdr messages
SetupLdrStartupMessage=Програм %1 ће бити инсталиран. Желите ли да наставите?
LdrCannotCreateTemp=Није могуће направити привремену датотеку. Инсталација је прекинута
LdrCannotExecTemp=Није могуће покренути датотеку у привременој фасцикли. Инсталација је прекинута
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nГрешка %2: %3
SetupFileMissing=Датотека %1 недостаје у инсталационој фасцикли. Исправите проблем или набавите нови примерак програма.
SetupFileCorrupt=Инсталационе датотеке су оштећене. Набавите нови примерак програма.
SetupFileCorruptOrWrongVer=Инсталационе датотеке су оштећене или нису компатибилне с овом верзијом инсталације. Исправите проблем или набавите нови примерак програма.
InvalidParameter=Неисправан параметар је прослеђен у командној линији:%n%n%1
SetupAlreadyRunning=Инсталација је већ покренута.
WindowsVersionNotSupported=Програм не подржава верзију Windows-а коју користите.
WindowsServicePackRequired=Програм захтева %1 сервисни пакет %2 или новији.
NotOnThisPlatform=Програм неће радити на %1.
OnlyOnThisPlatform=Програм мора да се покрене на %1.
OnlyOnTheseArchitectures=Програм се може инсталирати само на верзијама Windows-а намењеним за следеће архитектуре процесора:%n%n%1
WinVersionTooLowError=Програм захтева %1 верзију %2 или новију.
WinVersionTooHighError=Програм није могуће инсталирати на %1 верзију %2 или новију.
AdminPrivilegesRequired=Морате бити пријављени као администратор да бисте инсталирали програм.
; 'Power Users group' is an outdated term but should still be translated, not dropped or modernized
PowerUserPrivilegesRequired=Морате бити пријављени као администратор или као члан групе „Power Users“ да бисте инсталирали овај програм.
; 'instance' may also be translated as 'copy'
SetupAppRunningError=Инсталациони програм је утврдио да је програм %1 тренутно покренут.%n%nЗатворите га и кликните на дугме „У реду“ да наставите или „Откажи“ да напустите инсталацију.
UninstallAppRunningError=Деинсталациони програм је утврдио да је програм %1 тренутно покренут.%n%nЗатворите га и кликните на дугме „У реду“ да наставите или „Откажи“ да напустите деинсталацију.

; *** Startup questions
PrivilegesRequiredOverrideTitle=Одаберите начин инсталације програма
PrivilegesRequiredOverrideInstruction=Одаберите начин инсталације
PrivilegesRequiredOverrideText1=%1 може да се инсталира за све кориснике (захтева администраторске привилегије) или само за вас.
PrivilegesRequiredOverrideText2=%1 може да се инсталира само за вас или за све кориснике (захтева администраторске привилегије).
PrivilegesRequiredOverrideAllUsers=Инсталирај за &све кориснике
PrivilegesRequiredOverrideAllUsersRecommended=Инсталирај за &све кориснике (препоручено)
PrivilegesRequiredOverrideCurrentUser=Инсталирај само за &мене
PrivilegesRequiredOverrideCurrentUserRecommended=Инсталирај само за &мене (препоручено)

; *** Misc. errors
ErrorCreatingDir=Није могуће направити фасциклу „%1“
ErrorTooManyFilesInDir=Није могуће направити датотеку у фасцикли „%1“ јер садржи превише датотека

; *** Setup common messages
ExitSetupTitle=Напуштање инсталације
ExitSetupMessage=Инсталација није завршена. Ако сада изађете, програм неће бити инсталиран.%n%nИнсталацију можете поново покренути и довршити неком другом приликом.%n%nЖелите ли да изађете из инсталације?
AboutSetupMenuItem=&О инсталационом програму...
AboutSetupTitle=Подаци о инсталационом програму
AboutSetupMessage=%1 %2%n%3%n%nПочетна страница програма %1:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< &Назад
ButtonNext=&Даље >
ButtonInstall=&Инсталирај
ButtonOK=У реду
ButtonCancel=Откажи
ButtonYes=&Да
ButtonYesToAll=Д&а за све
ButtonNo=&Не
ButtonNoToAll=Н&е за све
ButtonFinish=&Заврши
ButtonBrowse=&Изабери...
ButtonWizardBrowse=И&забери...
ButtonNewFolder=&Направи нову фасциклу

; *** "Select Language" dialog messages
SelectLanguageTitle=Одабир језика инсталације
SelectLanguageLabel=Изаберите језик који ће се користити током инсталације.

; *** Common wizard text
ClickNext=Кликните на „Даље“ да наставите или „Откажи“ да напустите инсталацију.
BeveledLabel=
BrowseDialogTitle=Одабир фасцикле
BrowseDialogLabel=Изаберите фасциклу са списка испод, а затим кликните на „У реду“.
NewFolderName=Нова фасцикла

; *** "Welcome" wizard page
WelcomeLabel1=Добро дошли у чаробњак за инсталацију програма [name]
WelcomeLabel2=Програм [name/ver] ће бити инсталиран на рачунар.%n%nПре него што наставите, препоручујемо вам да затворите све друге програме.

; *** "Password" wizard page
WizardPassword=Лозинка
PasswordLabel1=Инсталација је заштићена лозинком.
PasswordLabel3=Унесите лозинку и кликните на „Даље“ да наставите. Лозинка је осетљива на мала и велика слова.
PasswordEditLabel=&Лозинка:
IncorrectPassword=Наведена лозинка није исправна. Покушајте поново.

; *** "License Agreement" wizard page
WizardLicense=Уговор о лиценци
LicenseLabel=Прочитајте следеће важне информације пре него што наставите.
LicenseLabel3=Прочитајте Уговор о лиценци који се налази испод. Морате прихватити услове овог уговора пре него што наставите са инсталацијом.
LicenseAccepted=&Прихватам уговор
LicenseNotAccepted=Н&е прихватам уговор

; *** "Information" wizard pages
WizardInfoBefore=Информације
InfoBeforeLabel=Прочитајте следеће важне информације пре него што наставите.
InfoBeforeClickLabel=Када будете спремни да наставите инсталацију, кликните на „Даље“.
WizardInfoAfter=Информације
InfoAfterLabel=Прочитајте следеће важне информације пре него што наставите.
InfoAfterClickLabel=Када будете спремни да наставите инсталацију, кликните на „Даље“.

; *** "User Information" wizard page
WizardUserInfo=Кориснички подаци
UserInfoDesc=Унесите своје податке.
UserInfoName=&Корисничко име:
UserInfoOrg=&Организација:
UserInfoSerial=&Серијски број:
UserInfoNameRequired=Морате навести име.

; *** "Select Destination Location" wizard page
WizardSelectDir=Одабир одредишне фасцикле
SelectDirDesc=Где треба инсталирати [name]?
SelectDirLabel3=Програм [name] ће бити инсталиран у следећу фасциклу.
SelectDirBrowseLabel=Кликните на „Даље“ да наставите. Ако желите да изаберете другу фасциклу, кликните на „Изабери...“.
DiskSpaceGBLabel=Потребно је најмање [gb] GB слободног простора на диску.
DiskSpaceMBLabel=Потребно је најмање [mb] MB слободног простора на диску.
CannotInstallToNetworkDrive=Није могуће инсталирати програм на мрежни диск.
CannotInstallToUNCPath=Није могуће инсталирати програм на UNC путању.
InvalidPath=Морате навести пуну путању са словом диска; нпр.:%n%nC:\APP%n%nили UNC путању у облику:%n%n\\server\share
InvalidDrive=Изабрани диск или UNC дељени ресурс не постоји или није доступан. Изаберите други.
DiskSpaceWarningTitle=Недовољно простора на диску
DiskSpaceWarning=За инсталацију је потребно најмање %1 KB слободног простора, а изабрани диск на располагању има само %2 KB.%n%nЖелите ли ипак да наставите?
DirNameTooLong=Назив фасцикле или путања је предугачка.
InvalidDirName=Назив фасцикле није исправан.
BadDirName32=Назив фасцикле не сме садржати ниједан од следећих знакова:%n%n%1
DirExistsTitle=Фасцикла већ постоји
DirExists=Фасцикла:%n%n%1%n%nвећ постоји. Желите ли ипак да инсталирате програм у њу?
DirDoesntExistTitle=Фасцикла не постоји
DirDoesntExist=Фасцикла:%n%n%1%n%nне постоји. Желите ли да се направи?

; *** "Select Components" wizard page
WizardSelectComponents=Одабир компонената
SelectComponentsDesc=Које компоненте треба инсталирати?
SelectComponentsLabel2=Изаберите компоненте које желите да инсталирате, а поништите избор оних које не желите. Кликните на „Даље“ када будете спремни да наставите.
; don't translate 'Full' as 'Normal' or 'Default'
FullInstallation=Пуна инсталација
; don't translate 'Compact' as 'Minimal' or 'Default'
CompactInstallation=Компактна инсталација
CustomInstallation=Прилагођена инсталација
NoUninstallWarningTitle=Компоненте већ постоје
NoUninstallWarning=Инсталациони програм је открио да су следеће компоненте већ инсталиране на рачунару:%n%n%1%n%nПоништавање избора ових компоненти их неће уклонити.%n%nЖелите ли ипак да наставите?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=Изабране ставке захтевају најмање [gb] GB простора на диску.
ComponentsDiskSpaceMBLabel=Изабране ставке захтевају најмање [mb] MB простора на диску.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Одабир додатних задатака
SelectTasksDesc=Које додатне задатке треба извршити?
SelectTasksLabel2=Изаберите додатне задатке које желите да се изврше при инсталирању програма [name] и кликните на „Даље“.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Одабир фасцикле у менију „Старт“
SelectStartMenuFolderDesc=Где треба поставити пречице програма?
SelectStartMenuFolderLabel3=Пречице програма ће бити постављене у следећу фасциклу у менију „Старт“.
SelectStartMenuFolderBrowseLabel=Кликните на „Даље“ да наставите. Ако желите да изаберете другу фасциклу, кликните на „Изабери...“.
MustEnterGroupName=Морате навести назив фасцикле.
GroupNameTooLong=Назив фасцикле или путања је предугачка.
InvalidGroupName=Назив фасцикле није исправан.
BadGroupName=Назив фасцикле не сме садржати ниједан од следећих знакова:%n%n%1
NoProgramGroupCheck2=Н&е прави фасциклу у менију „Старт“

; *** "Ready to Install" wizard page
WizardReady=Инсталација је спремна
ReadyLabel1=Програм [name] је сада спреман за инсталацију на рачунар.
ReadyLabel2a=Кликните на „Инсталирај“ да наставите са инсталацијом или на „Назад“ ако желите да прегледате или промените било које поставке.
ReadyLabel2b=Кликните на „Инсталирај“ да наставите са инсталацијом.
ReadyMemoUserInfo=Кориснички подаци:
ReadyMemoDir=Одредишна фасцикла:
ReadyMemoType=Врста инсталације:
ReadyMemoComponents=Изабране компоненте:
ReadyMemoGroup=Фасцикла у менију „Старт“:
ReadyMemoTasks=Додатни задаци:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel2=Преузимање датотека...
ButtonStopDownload=&Заустави преузимање
StopDownload=Да ли сте сигурни да желите да зауставите преузимање?
ErrorDownloadAborted=Преузимање је прекинуто
ErrorDownloadFailed=Преузимање није успело: %1 %2
ErrorDownloadSizeFailed=Није могуће утврдити величину: %1 %2
ErrorProgress=Неисправна вредност напретка: %1 од %2
ErrorFileSize=Неисправна величина датотеке: очекивано %1, пронађено %2

; *** TExtractionWizardPage wizard page and ExtractArchive
ExtractingLabel=Распакивање датотека...
ButtonStopExtraction=&Заустави распакивање
StopExtraction=Да ли сте сигурни да желите да зауставите распакивање?
ErrorExtractionAborted=Распакивање је прекинуто
ErrorExtractionFailed=Распакивање није успело: %1

; *** Archive extraction failure details
ArchiveIncorrectPassword=Лозинка је нетачна
ArchiveIsCorrupted=Архива је оштећена
ArchiveUnsupportedFormat=Формат архиве није подржан

; *** "Preparing to Install" wizard page
WizardPreparing=Припрема за инсталацију
PreparingDesc=Припрема се инсталација програма [name] на рачунар.
PreviousInstallNotCompleted=Инсталација или деинсталација претходног програма није завршена. Потребно је да поново покренете рачунар да би се та инсталација завршила.%n%nНакон поновног покретања рачунара, поново покрените инсталацију да бисте довршили инсталирање програма [name].
CannotContinue=Није могуће наставити инсталацију. Кликните на „Откажи“ да изађете.
ApplicationsFound=Следећи програми користе датотеке које треба да ажурира инсталациони програм. Препоручујемо вам да дозволите инсталационом програму да аутоматски затвори ове програме.
ApplicationsFound2=Следећи програми користе датотеке које треба да ажурира инсталациони програм. Препоручујемо вам да дозволите инсталационом програму да аутоматски затвори ове програме. Након што се инсталација заврши, инсталациони програм ће покушати да поново покрене програме.
CloseApplications=&Аутоматски затвори програме
DontCloseApplications=Не &затварај програме
ErrorCloseApplications=Инсталациони програм није могао аутоматски да затвори све програме. Пре него што наставите, препоручујемо вам да затворите све програме који користе датотеке које треба да ажурира инсталациони програм.
PrepareToInstallNeedsRestart=Потребно је поново покренути рачунар. Након поновног покретања рачунара, поново покрените инсталацију да бисте довршили инсталирање програма [name].%n%nЖелите ли да сада поново покренете рачунар?

; *** "Installing" wizard page
WizardInstalling=Инсталирање
InstallingLabel=Сачекајте да се [name] инсталира на рачунар.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Завршетак чаробњака за инсталацију програма [name]
FinishedLabelNoIcons=Инсталирање програма [name] на рачунар је завршено.
FinishedLabel=Инсталирање програма [name] на рачунар је завршено. Можете га покренути преко постављених пречица.
ClickFinish=Кликните на „Заврши“ да изађете из инсталације.
FinishedRestartLabel=Да би се завршила инсталација програма [name], потребно је поново покренути рачунар. Желите ли да га сада поново покренете?
FinishedRestartMessage=Да би се завршила инсталација програма [name], потребно је поново покренути рачунар.%n%nЖелите ли да га сада поново покренете?
ShowReadmeCheck=Да, желим да погледам README датотеку
YesRadio=&Да, сада поново покрени рачунар
NoRadio=Не, &касније ћу поново покренути рачунар
; used for example as 'Run MyProg.exe'
RunEntryExec=Покрени %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Погледај %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Инсталационом програму је потребан следећи диск
SelectDiskLabel2=Убаците диск %1 и кликните на „У реду“.%n%nАко се датотеке на овом диску могу пронаћи у фасцикли која није приказана испод, унесите исправну путању или кликните на „Изабери...“.
PathLabel=Пу&тања:
FileNotInDir2=Датотека „%1“ се не налази у „%2“. Убаците одговарајући диск или изаберите другу фасциклу.
SelectDirectoryLabel=Изаберите путању до следећег диска.

; *** Installation phase messages
SetupAborted=Инсталација није завршена.%n%nИсправите проблем и покрените је поново.
AbortRetryIgnoreSelectAction=Одаберите радњу
AbortRetryIgnoreRetry=&Покушајте поново
AbortRetryIgnoreIgnore=&Занемарите грешку и наставите
AbortRetryIgnoreCancel=Откажите инсталацију
RetryCancelSelectAction=Одаберите радњу
RetryCancelRetry=&Покушајте поново
RetryCancelCancel=Откажи

; *** Installation status messages
StatusClosingApplications=Затварање програма...
StatusCreateDirs=Прављење фасцикли...
StatusExtractFiles=Распакивање датотека...
StatusDownloadFiles=Преузимање датотека...
StatusCreateIcons=Постављање пречица...
StatusCreateIniEntries=Постављање INI уноса...
StatusCreateRegistryEntries=Постављање уноса у регистар...
StatusRegisterFiles=Регистровање датотека...
StatusSavingUninstall=Чување података о деинсталацији...
StatusRunProgram=Завршавање инсталације...
StatusRestartingApplications=Поновно покретање програма...
StatusRollback=Поништавање измена...

; *** Misc. errors
ErrorInternal2=Унутрашња грешка: %1
ErrorFunctionFailedNoCode=%1: неуспешно
ErrorFunctionFailed=%1: неуспешно; код %2
ErrorFunctionFailedWithMessage=%1: неуспешно; код %2.%n%3
ErrorExecutingProgram=Није могуће покренути датотеку:%n%1

; *** Registry errors
ErrorRegOpenKey=Грешка при отварању кључа у регистру:%n%1\%2
ErrorRegCreateKey=Грешка при прављењу кључа у регистру:%n%1\%2
ErrorRegWriteKey=Грешка при уписивању у кључ регистра:%n%1\%2

; *** INI errors
ErrorIniEntry=Грешка при прављењу INI уноса у датотеци „%1“.

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=Пре&скочите ову датотеку (не препоручује се)
FileAbortRetryIgnoreIgnoreNotRecommended=&Занемарите грешку и наставите (не препоручује се)
SourceIsCorrupted=Изворна датотека је оштећена
SourceDoesntExist=Изворна датотека „%1“ не постоји
SourceVerificationFailed=Верификација изворне датотеке није успела: %1
VerificationSignatureDoesntExist=Датотека потписа „%1“ не постоји
VerificationSignatureInvalid=Датотека потписа „%1“ је неважећа
VerificationKeyNotFound=Датотека потписа „%1“ користи непознат кључ
VerificationFileNameIncorrect=Назив датотеке није тачан
VerificationFileTagIncorrect=Ознака датотеке није тачна
VerificationFileSizeIncorrect=Величина датотеке није тачна
VerificationFileHashIncorrect=Хеш датотеке није тачан
ExistingFileReadOnly2=Постојећа датотека не може да се замени јер је означена као „само за читање“.
ExistingFileReadOnlyRetry=&Уклоните атрибут само за читање и покушајте поново
ExistingFileReadOnlyKeepExisting=&Задржите постојећу датотеку
ErrorReadingExistingDest=Дошло је до грешке при читању постојеће датотеке:
FileExistsSelectAction=Одаберите радњу
FileExists2=Датотека већ постоји.
FileExistsOverwriteExisting=&Замените постојећу датотеку
FileExistsKeepExisting=Задр&жите постојећу датотеку
FileExistsOverwriteOrKeepAll=&Урадите ово и за наредне конфликте
ExistingFileNewerSelectAction=Одаберите радњу
ExistingFileNewer2=Постојећа датотека је новија од оне коју инсталациони програм покушава да инсталира.
ExistingFileNewerOverwriteExisting=&Замените постојећу датотеку
ExistingFileNewerKeepExisting=Задр&жите постојећу датотеку (препоручено)
ExistingFileNewerOverwriteOrKeepAll=&Урадите ово и за наредне конфликте
ErrorChangingAttr=Дошло је до грешке при измени атрибута постојеће датотеке:
ErrorCreatingTemp=Дошло је до грешке при прављењу датотеке у одредишној фасцикли:
ErrorReadingSource=Дошло је до грешке при читању изворне датотеке:
ErrorCopying=Дошло је до грешке при копирању датотеке:
ErrorDownloading=Дошло је до грешке при преузимању датотеке:
ErrorExtracting=Дошло је до грешке при распакивању архиве:
ErrorReplacingExistingFile=Дошло је до грешке при замени постојеће датотеке:
; 'RestartReplace' is an internal name, you may keep it as is
ErrorRestartReplace=Није могуће заменити:
ErrorRenamingTemp=Дошло је до грешке при преименовању датотеке у одредишној фасцикли:
ErrorRegisterServer=Није могуће регистровати DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 није успео са излазним кодом %1
ErrorRegisterTypeLib=Није могуће регистровати библиотеку типова: %1

; *** Uninstall display name markings
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32-битни
UninstallDisplayNameMark64Bit=64-битни
UninstallDisplayNameMarkAllUsers=Сви корисници
UninstallDisplayNameMarkCurrentUser=Тренутни корисник

; *** Post-installation errors
ErrorOpeningReadme=Дошло је до грешке при отварању README датотеке.
ErrorRestartingComputer=Инсталациони програм није могао поново да покрене рачунар. Урадите то ручно.

; *** Uninstaller messages
UninstallNotFound=Датотека „%1“ не постоји. Деинсталација није могућа.
UninstallOpenError=Датотека „%1“ не може да се отвори. Деинсталација није могућа
UninstallUnsupportedVer=Лог датотека деинсталације „%1“ је у формату који ова верзија деинсталационог програма не препознаје. Деинсталација није могућа
UninstallUnknownEntry=Непознат унос (%1) се појавио у лог датотеци деинсталације
ConfirmUninstall=Да ли сте сигурни да желите у потпуности да деинсталирате програм %1 и све његове компоненте?
UninstallOnlyOnWin64=Ова инсталација може да се деинсталира само на 64-битном Windows-у.
OnlyAdminCanUninstall=Ову инсталацију може да деинсталира само корисник са администраторским привилегијама.
UninstallStatusLabel=Сачекајте да се %1 деинсталира са рачунара.
UninstalledAll=Програм %1 је успешно деинсталиран са рачунара.
UninstalledMost=Програм %1 је деинсталиран.%n%nНеки елементи нису могли бити уклоњени. Можете их уклонити ручно.
UninstalledAndNeedsRestart=Да би се завршила деинсталација програма %1, потребно је поново покренути рачунар.%n%nЖелите ли да сада поново покренете рачунар?
UninstallDataCorrupted=Датотека „%1“ је оштећена. Деинсталација није могућа

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Уклонити дељену датотеку?
ConfirmDeleteSharedFile2=Систем пријављује да следећу дељену датотеку више не користи ниједан програм. Желите ли да је деинсталациони програм уклони?%n%nАко неки програми и даље користе ову датотеку, а она буде уклоњена, ти програми можда неће исправно радити. Ако нисте сигурни, изаберите „Не“. Остављање датотеке на систему неће проузроковати никакву штету.
SharedFileNameLabel=Назив датотеке:
SharedFileLocationLabel=Локација:
WizardUninstalling=Стање деинсталације
StatusUninstalling=Деинсталирање програма %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Инсталирање програма %1.
ShutdownBlockReasonUninstallingApp=Деинсталирање програма %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 верзија %2
AdditionalIcons=Додатне пречице:
CreateDesktopIcon=Постави пречицу на &радну површину
CreateQuickLaunchIcon=П&остави пречицу на траку за брзо покретање
ProgramOnTheWeb=%1 на интернету
UninstallProgram=Деинсталирај %1
LaunchProgram=Покрени %1
AssocFileExtension=&Повежи %1 са екстензијом датотеке %2
AssocingFileExtension=Повезивање %1 са екстензијом датотеке %2...
AutoStartProgramGroupDescription=Покретање:
AutoStartProgram=Аутоматски покрени %1
AddonHostProgramNotFound=%1 се не налази у наведеној фасцикли.%n%nЖелите ли ипак да наставите?
