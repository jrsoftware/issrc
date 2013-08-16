; *** Inno Setup version 5.5.3+ Tatar messages ***
;
; Translate by: Irek Khaziev
; E-Mail: khazirek@mail.ru
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).

[LangOptions]
LanguageName=<0422><0430><0442><0430><0440>
LanguageID=$0444
LanguageCodePage=65001

[Messages]

; *** Application titles
SetupAppTitle=Урнаштыру
SetupWindowTitle=Урнаштыру — %1
UninstallAppTitle=Бетерү
UninstallAppFullTitle=Бетерү — %1

; *** Misc. common
InformationTitle=Мәгълүмат
ConfirmTitle=Раслау
ErrorTitle=Хата

; *** SetupLdr messages
SetupLdrStartupMessage=Программа санакка %1 урнаштырачак, дәвам итәргәме?
LdrCannotCreateTemp=Вакытлы файлны ясап булмый. Урнаштыру туктатылды
LdrCannotExecTemp=Вакытлы каталогта файлны башкарып булмады. Урнаштыру туктатылды

; *** Startup error messages
LastErrorMessage=%1.%n%nХата %2: %3
SetupFileMissing=%1 файлы урнаштыру папкасында юк. Зинһар, проблеманы юк итегез яки программаның яңа юрамасын алыгыз.
SetupFileCorrupt=Урнаштыру файллары бозык. Зинһар, программаның яңа күчермәсен алыгыз.
SetupFileCorruptOrWrongVer=Урнаштыру файллары бозык яки бу урнаштыру программасы белән туры килми. Зинһар, проблеманы юк итегез яки программаның яңа юрамасын алыгыз.
InvalidParameter=Боерык юлында мөмкин булмаган шарт:%n%n%1
SetupAlreadyRunning=Урнаштыру программасы инде кабызылган.
WindowsVersionNotSupported=Программа әлеге санакта урнаштырылган Windows юрамасын куллана алмый.
WindowsServicePackRequired=Программа %1 Service Pack %2 яки югарырак таләп итә.
NotOnThisPlatform=Әлеге программада %1 эшләмәячәк.
OnlyOnThisPlatform=Әлеге программаны %1 гына кабызып була.
OnlyOnTheseArchitectures=Программаны урнаштыру Windows юрамаларының түбәндәге процессор корылмалары өчен генә мөмкин:%n%n%1
MissingWOW64APIs=Эшләгән Windows юрамасында 64-битлы урнаштыру башкару өчен мөмкинлек юк. Проблеманы юк итү өчен яңарту төргәген урнаштырырга кирәк (Service Pack) %1.
WinVersionTooLowError=Программа %1, %2 юрамасы яки югарырак таләп итә.
WinVersionTooHighError=Программа %1, %2 юрамасы яки югарырак белән урнаштырыла алмый.
AdminPrivilegesRequired=Программаны урнаштыру өчен системага Администратор булып керегез.
PowerUserPrivilegesRequired=Программаны урнаштыру өчен системага Администратор яки «Тәҗрибәле кулланучы» (Power Users) булып керергә кирәк).
SetupAppRunningError=Кабызылган %1 нөсхәсе табылды.%n%nЗинһар, барлык кушымта нөсхәләрен ябыгыз, аннары дәвам итү өчен «Ярар» төймәсенә, яки «Баш тарту» төймәсенә чыгу өчен басыгыз.
UninstallAppRunningError=Бетерүче кабызылган %1 нөсхәсен тапты.%n%nЗинһар, барлык кушымта нөсхәләрен ябыгыз, аннары дәвам итү өчен «Ярар» төймәсенә яки «Баш тарту» төймәсенә чыгу өчен басыгыз.

; *** Misc. errors
ErrorCreatingDir="%1" папкасын ясап булмый
ErrorTooManyFilesInDir="%1" каталогында файлны ясап булмый, чөнки анда бик күп файл

; *** Setup common messages
ExitSetupTitle=Урнаштыру программасыннан чыгу
ExitSetupMessage=Урнаштыру тәмамланмады. Чыксагыз, программа урнаштырылмаячак.%n%nУрнаштыруны урнаштыру программасын соңрак кабызып тәмамлый аласыз.%n%nУрнаштыру программасыннан чыгаргамы?
AboutSetupMenuItem=&Программа турында...
AboutSetupTitle=Программа турында
AboutSetupMessage=%1, юрама %2%n%3%n%nСәхифәсе %1:%n%4
AboutSetupNote=
TranslatorNote=Tatar translation by Irek Khaziev, <khazirek@mail.ru>

; *** Buttons
ButtonBack=< &Кире
ButtonNext=&Алга >
ButtonInstall=&Урнаштырырга
ButtonOK=Ярар
ButtonCancel=Баш тарту
ButtonYes=Ә&йе
ButtonYesToAll=Ә&йе, барысы өчен
ButtonNo=&Юк
ButtonNoToAll=&Юк, барысы өчен
ButtonFinish=&Тәмамларга
ButtonBrowse=&Күзәтү...
ButtonWizardBrowse=&Күзәтү...
ButtonNewFolder=&Яңа папка

; *** "Select Language" dialog messages
SelectLanguageTitle=Урнаштыру телен сайлагыз
SelectLanguageLabel=Урнаштыру барышында файдаланучы телне сайлагыз:

; *** Common wizard text
ClickNext=«Алга» дәвам итү, яки урнаштыру программасыннан чыгу өчен «Баш тарту» төймәсенә басыгыз.
BeveledLabel=
BrowseDialogTitle=Папкаларны күзәтү
BrowseDialogLabel=Исемлектән папканы сайлап, «Ярар» төймәсенә басыгыз.
NewFolderName=Яңа папка

; *** "Welcome" wizard page
WelcomeLabel1=[name] урнаштыру остасына хуш килдегез
WelcomeLabel2=Программа санакка [name/ver] урнаштырачак.%n%nДәвам итү алдыннан барлык башка эшли торган кушымталарны ябарга кирәк.

; *** "Password" wizard page
WizardPassword=Серсүз
PasswordLabel1=Программа серсүз белән сакланган.
PasswordLabel3=Зинһар, серсүзне кертегез, аннары «Алга» басыгыз. Серсүзләрне теркәүне саклап кертегез.
PasswordEditLabel=&Серсүз:
IncorrectPassword=Керткән серсүз дөрес түгел. Зинһар, кабатлап карагыз.

; *** "License Agreement" wizard page
WizardLicense=Хокук килешүе
LicenseLabel=Зинһар, дәвам итү алдыннан түбәндәге мөһим мәгълүматны укып чыгыгыз.
LicenseLabel3=Зинһар, Хокук Килешүен укып чыгыгыз. Дәвам итү алдыннан аның шартларын кабул итегез.
LicenseAccepted=&Килешү шартларын кабул итәм
LicenseNotAccepted=&Килешү шартларын кабул итмим

; *** "Information" wizard pages
WizardInfoBefore=Мәгълүмат
InfoBeforeLabel=Зинһар, дәвам итү алдыннан түбәндәге мөһим мәгълүматны укып чыгыгыз.
InfoBeforeClickLabel=Урнаштыруны дәвам итәргә әзер булгач, «Алга» басыгыз.
WizardInfoAfter=Мәгълүмат
InfoAfterLabel=Зинһар, дәвам итү алдыннан түбәндәге мөһим мәгълүматны укып чыгыгыз.
InfoAfterClickLabel=Урнаштыруны дәвам итәргә әзер булгач, «Алга» басыгыз.

; *** "User Information" wizard page
WizardUserInfo=Кулланучы турында
UserInfoDesc=Зинһар, үзегез турында мәгълүмат кертегез.
UserInfoName=&Кулланучы исеме һәм фамилиясе:
UserInfoOrg=&Оешма:
UserInfoSerial=&Серия саны:
UserInfoNameRequired=Сез исем кертергә тиеш.

; *** "Select Destination Location" wizard page
WizardSelectDir=Урнаштыру папкасын сайлау
SelectDirDesc=[name] кайсы папкага урнаштырырга телисез?
SelectDirLabel3=Программа [name] түбәндәге папкага урнаштырылачак.
SelectDirBrowseLabel=Дәвам итү өчен «Алга» басыгыз. Башка папканы сайларга теләсәгез «Күзәтү» сайлагыз.
DiskSpaceMBLabel=Кимендә [mb] Мб тәлинкәдә буш урын кирәк.
CannotInstallToNetworkDrive=Челтәр тәлинкәсенә урнаштыруны башкарып булмый.
CannotInstallToUNCPath=UNC-юл буенча папкага урнаштырып булмый.
InvalidPath=Тәлинкә хәрефе белән тулы юлны күрсәтегез мәсәлән:%n%nC:\APP%n%nяки формада UNC:%n%n\\сервер_исеме\чыганак_исеме
InvalidDrive=Сайлаган тәлинкә яки челтәр юлы юк яки тыелган. Зинһар, башканы сайлагыз.
DiskSpaceWarningTitle=Тәлинкәдә урын җитми
DiskSpaceWarning=Урнаштыру өчен %1 Кб буш урын кирәк, ә сайлаган тәлинкәдә %2 Кб кына бар.%n%nШуңа карамастан урнаштыруны дәвам итәргәме?
DirNameTooLong=Папка исеме яки аңа юл яраган озынлыкны арттыра.
InvalidDirName=Күрсәтелгән папка исеме тыелган.
BadDirName32=Папка исемендә түбәндәге билгеләр була алмый: %n%n%1
DirExistsTitle=Папка бар
DirExists=Папка%n%n%1%n%бар. Барыбер шул папкага урнаштырыргамы?
DirDoesntExistTitle=Папка юк
DirDoesntExist=Папка%n%n%1%n%nюк. Аны ясаргамы?

; *** "Select Components" wizard page
WizardSelectComponents=Кисәкләрне сайлау
SelectComponentsDesc=Нинди кисәкләрне урнаштырырга?
SelectComponentsLabel2=Урнаштырырга теләгән кисәкләрне сайлагыз, урнаштырырга кирәк булмаган кисәкләрдән тамганы алыгыз. Дәвам итәргә әзер булгач, «Алга» басыгыз.
FullInstallation=Тулы урнаштыру
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Тыгыз урнаштыру
CustomInstallation=Сайлаулы урнаштыру
NoUninstallWarningTitle=Урнаштырылган кисәкләр
NoUninstallWarning=Урнаштыру программасы санакта түбәндәге урнаштырылган кисәкләрне тапты:%n%n%1%n%nКисәкләрне сайламау, аларны бетермәячәк.%n%nДәвам итәргәме?
ComponentSize1=%1 Кб
ComponentSize2=%1 Мб
ComponentsDiskSpaceMBLabel=Хәзерге сайлау тәлинкәдә [mb] Мб таләп итә

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Өстәмә йомышларны сайлагыз
SelectTasksDesc=Нинди өстәмә йомышлар башкарырга кирәк?
SelectTasksLabel2=[name] урнаштыруы барышында эшләнергә тиеш өстәмә йомышларны сайлагыз, шуннан соң «Алга» басыгыз:

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=«Башлау» менюсында папканы сайлагыз
SelectStartMenuFolderDesc=Урнаштыру программасы ярлыкларны кайда сакларга тиеш?
SelectStartMenuFolderLabel3=Кушымта «Башлау» менюсының түбәндәге папкасында ярлыклар ясаячак.
SelectStartMenuFolderBrowseLabel=Дәвам итү өчен «Алга» басыгыз. Башка папканы сайларга теләсәгез «Күзәтү» сайлагыз.
MustEnterGroupName=Папка исемен кертегез.
GroupNameTooLong=Төркем папкасы исеме яки аңа юл рөхсәт ителгән озынлыкны арттыра.
InvalidGroupName=Күрсәтелгән папка исеме тыелган.
BadGroupName=Папкалар исемендә түбәндәге билгеләр була алмый:%n%n%1
NoProgramGroupCheck2=«&Башлау» менюсында папка ясамаска

; *** "Ready to Install" wizard page
WizardReady=Барысы урнаштыруга әзер
ReadyLabel1=Урнаштыру программасы санакка [name] урнаштырырга әзер.
ReadyLabel2a=«Урнаштырырга» дәвам итү, яки урнаштыру рәвешләрен карарга яки үзгәртергә теләсәгез «Артка» басыгыз.
ReadyLabel2b=Дәвам итү өчен «Урнаштырырга» басыгыз.
ReadyMemoUserInfo=Кулланучы турында:
ReadyMemoDir=Урнаштыру папкасы:
ReadyMemoType=Урнаштыру төре:
ReadyMemoComponents=Сайланган кисәкләр:
ReadyMemoGroup=«Башлау» менюсында папка:
ReadyMemoTasks=Өстәмә йомышлар:

; *** "Preparing to Install" wizard page
WizardPreparing=Урнаштыруга әзерләнү
PreparingDesc=Урнаштыру программасы санакка [name] урнаштыруына әзерләнә.
PreviousInstallNotCompleted=Алдагы программа урнаштыруы яки бетерүе тәмамланмады. Урнаштыруны тәмамлау өчен санакны киредән кабызырга кирәк.%n%nШуннан соң Урнаштыру программасын [name] урнаштыруын тәмамлау өчен яңадан кабызыгыз.
CannotContinue=Урнаштыруны дәвам итеп булмый. Программадан чыгу өчен «Баш тарту» төймәсенә басыгыз.
ApplicationsFound=Түбәндәге кушымталар урнаштыру программасы яңартырга тиеш булган файлларны куллана. Урнаштыру программасына әлеге кушымталарны үзе ябарга рөхсәт бирергә кирәк.
ApplicationsFound2=Түбәндәге кушымталар урнаштыру программасы яңартырга тиеш булган файлларны куллана. Урнаштыру программасына әлеге кушымталарны үзе ябарга рөхсәт бирергә кирәк. Урнаштыру тәмамлангач, урнаштыру программасы яңадан аларны кабызачак.
CloseApplications=&Автоматик рәвештә әлеге кушымталарны ябарга
DontCloseApplications=Ә&леге кушымталарны ябмаска
ErrorCloseApplications=Урнаштыру программасы барлык кушымталарны үзе яба алмады. Яңартылачак файлларны кулланган барлык кушымталарны урнаштыру алдыннан ябарга кирәк.

; *** "Installing" wizard page
WizardInstalling=Урнаштыру...
InstallingLabel=Зинһар, [name] санакка урнаштырылганын көтегез.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=[name] урнаштыру остасын тәмамлау
FinishedLabelNoIcons=[name] программасы санакка урнаштырылды.
FinishedLabel=[name] программасы санакка урнаштырылды. Кушымтаны туры килүче билгечек аша кабызып була.
ClickFinish=Урнаштыру программасыннан чыгу өчен «Тәмамларга» төймәсенә басыгыз.
FinishedRestartLabel=[name] урнаштыруын тәмамлау өчен санакны киредән кабызырга кирәк. Хәзер аны эшләргәме?
FinishedRestartMessage=[name] урнаштыруын тәмамлау өчен санакны киредән кабызырга кирәк.%n%nХәзер аны эшләргәме?
ShowReadmeCheck=Мин README файлын карарга телим
YesRadio=Ә&йе, санакны хәзер киредән кабызырга
NoRadio=&Юк, кабат кабызуны соңрак башкарам
; used for example as 'Run MyProg.exe'
RunEntryExec=%1 кабызырга
; used for example as 'View Readme.txt'
RunEntryShellExec=%1 карарга

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Түбәндәге тәлинкәне кертергә кирәк
SelectDiskLabel2=Зинһар, %1 тәлинкәсен кертегез һәм «Ярар» төймәсенә басыгыз.%n%nБу тәлинкә файллары, түбәндә күрсәтелгән папкаларда табылмаса, дөрес юлны кертегез яки «Күзәтү» төймәсенә басыгыз.
PathLabel=&Юл:
FileNotInDir2="%1" файлы "%2" табылмады. Зинһар, дөрес тәлинкәне кертегез яки башка папканы сайлагыз.
SelectDirectoryLabel=Зинһар, түбәндәге тәлинкәгә юлны күрсәтегез.

; *** Installation phase messages
SetupAborted=Урнаштыру тәмамланмады.%n%nЗинһар, проблеманы юк итеп, урнаштыруны яңадан кабызыгыз.
EntryAbortRetryIgnore=«Кабатлау» төймәсенә кабатлау өчен, «Үткәрергә» файлны үткәрү өчен (тәкъдим ителми) яки «Баш тарту» төймәсенә урнаштырудан баш тарту өчен басыгыз.

; *** Installation status messages
StatusClosingApplications=Кушымталарны ябу...
StatusCreateDirs=Папкаларны ясау...
StatusExtractFiles=Файлларны чишү...
StatusCreateIcons=Программа ярлыкларын ясау...
StatusCreateIniEntries=INI-файлларны ясау...
StatusCreateRegistryEntries=Исемлек язмаларын ясау...
StatusRegisterFiles=Файлларны теркәү
StatusSavingUninstall=Бетерү өчен мәгълүматны саклау...
StatusRunProgram=Урнаштыруны тәмамлау...
StatusRestartingApplications=Кушымталарны киредән кабызу...
StatusRollback=Үзгәртүләрне артка чигү...

; *** Misc. errors
ErrorInternal2=Эчке хата: %1
ErrorFunctionFailedNoCode=%1: тоткарлык
ErrorFunctionFailed=%1: тоткарлык; коды %2
ErrorFunctionFailedWithMessage=1: тоткарлык; коды %2.%n%3
ErrorExecutingProgram=Файлны башкарып булмый:%n%1

; *** Registry errors
ErrorRegOpenKey=Исемлек ачкычын ачуда хата:%n%1\%2
ErrorRegCreateKey=Исемлек ачкычын ясауда хата:%n%1\%2
ErrorRegWriteKey=Исемлек ачкычына яздыруда хата:%n%1\%2

; *** INI errors
ErrorIniEntry=INI-файлында "%1" язма ясауда хата.

; *** File copying errors
FileAbortRetryIgnore=«Кабатлау» төймәсенә кабатлау өчен, «Үткәрергә» файлны үткәрү өчен (тәкъдим ителми) яки «Баш тарту» төймәсенә чыгу өчен басыгыз.
FileAbortRetryIgnore2=«Кабатлау» төймәсенә кабатлау өчен, «Үткәрергә» хатаны исәпләмәү өчен (тәкъдим ителми) яки «Баш тарту» төймәсенә чыгу өчен басыгыз.
SourceIsCorrupted=Башлангыч файл бозык
SourceDoesntExist=Башлангыч "%1" файлы юк
ExistingFileReadOnly=Булган файл «уку өчен генә» дип билгеләнгән.%n%n«Кабатлау»төймәсенә «уку өчен генә» аергычны бетерү өчен басыгыз, «Үткәрергә» - файлны үткәрү өчен яки «Баш тарту» төймәсенә чыгу өчен басыгыз.
ErrorReadingExistingDest=Булган файлны укуда хата килеп чыкты:
FileExists=Файл инде бар.%n%nАны кабат яздырыргамы?
ExistingFileNewer=Булган файл урнаштырыла торган файлдан яңарак. Булган файлны сакларга кирәк.%n%nБулган файлны сакларгамы?
ErrorChangingAttr=Булган файлның аергычларын үзгәртүдә хата килеп чыкты:
ErrorCreatingTemp=Билгеләнгән папкада файлны ясауда хата килеп чыкты:
ErrorReadingSource=Башлангыч файлны укуда хата килеп чыкты:
ErrorCopying=Файл күчермәсен ясауда хата килеп чыкты:
ErrorReplacingExistingFile=Булган файлны алмаштыруда хата килеп чыкты:
ErrorRestartReplace=RestartReplace хатасы:
ErrorRenamingTemp=Башкару папкасында файлның исемен алмаштыруда хата килеп чыкты:
ErrorRegisterServer=DLL/OCX теркәп булмый: %1
ErrorRegSvr32Failed=RegSvr32 башкаруда хата, кире кайту коды %1
ErrorRegisterTypeLib=Төрләр китапханәсен йөкләп булмый (Type Library): %1

; *** Post-installation errors
ErrorOpeningReadme=README файлны ачуда хата килеп чыкты.
ErrorRestartingComputer=Урнаштыру программасы санакны киредән кабыза алмады. Зинһар, аны үзегез башкарыгыз.

; *** Uninstaller messages
UninstallNotFound="%1" файлы юк, бетерү мөмкин түгел.
UninstallOpenError="%1" файлын ачып булмый. Бетерү мөмкин түгел
UninstallUnsupportedVer="%1" бетерү өчен беркетмә файлы бу бетерү программа юрамасы белән билгеләнмәде. Бетерү мөмкин түгел
UninstallUnknownEntry=Бетерү файлының беркетмәсендә билгесез (%1) ноктасы килеп чыкты
ConfirmUninstall=%1 һәм аның барлык кисәкләрен бетерергәме?
UninstallOnlyOnWin64=Программаны 64-битлы Windows мохитында генә бетереп була.
OnlyAdminCanUninstall=Программа администратор өстенлекләре булган кулланучы белән генә бетерелә ала.
UninstallStatusLabel=Зинһар, көтеп торыгыз %1 санактан бетерелүен.
UninstalledAll=Программа %1 санактан тулысынча бетерелде.
UninstalledMost=%1 бетерүе тәмамланды.%n%nКайбер кисәкләрне бетереп булмады. Аларны үзегез бетерә аласыз.
UninstalledAndNeedsRestart=%1 бетерүен тәмамлау өчен санакны киредән кабызырга кирәк.%n%nХәзер башкарыргамы?
UninstallDataCorrupted="%1" файлы бозык. Бетерү мөмкин түгел.

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Бергә файдаланучы файлны бетерергәме?
ConfirmDeleteSharedFile2=Система киләсе бергә файдаланучы файл башка бернинди кушымта белән дә кулланмаганын күрсәтә. Файлны бетерергәме?%n%nБерәр программа һаман әлеге файлны файдаланса, һәм ул бетерелсә, алар дөрес эшли алмаячак. Шикләнсәгез, «Юк» төймәсен сайлагыз. Калдырылган файл системага зарар китермәячәк.
SharedFileNameLabel=Файл исеме:
SharedFileLocationLabel=Урнашуы:
WizardUninstalling=Бетерү халәте
StatusUninstalling=%1 бетерүе...


; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Урнаштыру %1.
ShutdownBlockReasonUninstallingApp=Бетерү %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1, %2 юрамасы
AdditionalIcons=Өстәмә билгечекләр:
CreateDesktopIcon=&Эш өстәлендә билгечек ясарга
CreateQuickLaunchIcon=&Тиз кабызу аслыгында билгечек ясарга
ProgramOnTheWeb=%1 сәхифәсе
UninstallProgram=%1 бетерүе
LaunchProgram=%1 кабызырга
AssocFileExtension=%2 &киңәйтүле файллар белән %1 ялгаргамы?
AssocingFileExtension=%1 %2 файллары белән ялгау...
AutoStartProgramGroupDescription=Үзе кабызу:
AutoStartProgram=Үзе кабызу %1
AddonHostProgramNotFound=%1 күрсәткән папкада табылмады.%n%nБарыбер дәвам итәргәме?
