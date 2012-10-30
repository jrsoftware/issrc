;Paval Shalamitski
; *** Inno Setup version 5.5.0+ Belarusian (classical orthography) messages ***
; Translated from English in 2012 by Paval 'Klyok' Shalamitski (i.kliok@gmail.com)
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
LanguageName=<0411><0435><043B><0430><0440><0443><0441><043A><0430><044F>
LanguageID=$0423
LanguageCodePage=1251
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
SetupAppTitle=Усталяваць праґраму
SetupWindowTitle=Усталяваць — %1
UninstallAppTitle=Высталяваць
UninstallAppFullTitle=Высталяваць %1

; *** Misc. common
InformationTitle=Зьвесткі
ConfirmTitle=Пацьвердзіць
ErrorTitle=Памылка

; *** SetupLdr messages
SetupLdrStartupMessage=Усталёўваецца %1. Ці працягваць?
LdrCannotCreateTemp=Немагчыма стварыць часовы файл. Спыняем усталёўваць праґраму
LdrCannotExecTemp=Немагчыма запусьціць файл у часовым каталёґу. Спыняем усталёўваць праґраму

; *** Startup error messages
LastErrorMessage=%1.%n%nПамылка %2: %3
SetupFileMissing=У каталёґу, адкуль усталёўваецца праґрама, не знайшлі файл «%1». Выправіце гэта або знайдзіце новы асобнік праґрамы.
SetupFileCorrupt=Файлы, зь якіх усталёўваецца праґрама, пашкоджаныя. Знайдзіце новы асобнік праґрамы.
SetupFileCorruptOrWrongVer=Файлы, зь якіх усталёўваецца праґрама, пашкоджаныя ці несумяшчальныя з гэтаю вэрсіяй праґрамы ўсталяваньня. Выправіце гэта або знайдзіце новы асобнік праґрамы.
InvalidParameter=У загадны радок перадалі хібны парамэтар:%n%n%1
SetupAlreadyRunning=Праґрама ўсталяваньня яшчэ працуе.
WindowsVersionNotSupported=Праґрама не падтрымвае вашую вэрсію «Windows».
WindowsServicePackRequired=Праґрама патрабуе дапаможны пакунак «%1» вэрсіі %2 ці навейшае.
NotOnThisPlatform=Праґрама не запускаецца на %1.
OnlyOnThisPlatform=Праґрама мусіць запускацца на %1.
OnlyOnTheseArchitectures=Праґраму можна ўсталяваць толькі на вэрсіі «Windows» для наступнае архітэктуры працэсару:%n%n%1
MissingWOW64APIs=Вэрсія «Windows», якая працуе ў вас, не дазваляе ўсталёўваць ў 64-бітныя праґрамы. Каб выправіць гэта, усталюйце дапаможны пакунак (Service Pack) %1.
WinVersionTooLowError=Праґрама патрабуе «%1» вэрсіі %2 ці навейшае.
WinVersionTooHighError=Праґраму нельга ўсталяваць на «%1» вэрсіі %2 ці навейшае.
AdminPrivilegesRequired=Каб усталяваць праґраму, трэба ўвайсьці як спраўнік (адміністратар).
PowerUserPrivilegesRequired=Каб усталяваць праґраму, трэба ўвайсьці як спраўнік (адміністратар) або як чалец суполкі «Дасьведчаныя карыстальнікі».
SetupAppRunningError=Праґрама ўсталяваньня выявіла, што зараз працуе «%1».%n%nЗачыніце ўсе запушчаныя асобнікі «%1» і націсьніце «Добра», каб працягваць. Каб выйсьці, націсьніце «Скасаваць».
UninstallAppRunningError=Праґрама высталяваньня выявіла, што зараз працуе «%1».%n%nЗачыніце ўсе запушчаныя асобнікі «%1» і націсьніце «Добра», каб працягваць. Каб выйсьці, націсьніце «Скасаваць».

; *** Misc. errors
ErrorCreatingDir=Праґрама ўсталяваньня ня можа стварыць каталёґ «%1»
ErrorTooManyFilesInDir=Немагчыма стварыць файл у каталёґу «%1»: каталёґ зьмяшчае зашмат файлаў

; *** Setup common messages
ExitSetupTitle=Выйсьці з усталяваньня
ExitSetupMessage=Яшчэ ня скончылі ўсталёўваць. Калі выйсьці зараз, праґрама не ўсталюецца.%n%nАле праґраму ўсталяваньня можна будзе зноўку запусьціць іншым разам.%n%nЦі сапраўды выйсьці?
AboutSetupMenuItem=&Пра праґраму ўсталяваньня…
AboutSetupTitle=Пра праґраму ўсталяваньня
AboutSetupMessage=«%1» вэрсіі %2%n%3%n%nХатняя бачына «%1»%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< На&зад
ButtonNext=На&перад >
ButtonInstall=&Усталяваць
ButtonOK=Добра
ButtonCancel=Скасаваць
ButtonYes=&Так
ButtonYesToAll=Заўжды т&ак
ButtonNo=&Не
ButtonNoToAll=Заўжды н&е
ButtonFinish=&Скончыць
ButtonBrowse=&Зірнуць
ButtonWizardBrowse=З&ірнуць…
ButtonNewFolder=&Стварыць каталёґ

; *** "Select Language" dialog messages
SelectLanguageTitle=Абраць мову ўсталяваньня
SelectLanguageLabel=Абярыце мову, якою будзе карыстацца праґрама ўсталяваньня:

; *** Common wizard text
ClickNext=Каб працягваць, націсьніце «Наперад». Каб выйсьці, націсьніце «Скасаваць».
BeveledLabel=
BrowseDialogTitle=Зірнуць каталёґ
BrowseDialogLabel=Абярыце каталёґ у сьпісе ўнізе, потым націсьніце «Добра».
NewFolderName=Новы каталёґ

; *** "Welcome" wizard page
WelcomeLabel1=Вітаем у Дапаможніку ўсталёўваць «[name]»
WelcomeLabel2=Дапаможнік усталюе на кампутар «[name/ver]».%n%nПерад тым, як усталёўваць, мы раім зачыніць усе іншыя праґрамы.

; *** "Password" wizard page
WizardPassword=Пароль
PasswordLabel1=Каб усталяваць праґраму, трэба ведаць пароль.
PasswordLabel3=Пазначце пароль і націсьніце «Наперад», каб працягваць. Набірайце пароль з улікам памеру (праґрама адрозьнівае вялікія і маленькія літары).
PasswordEditLabel=&Пароль:
IncorrectPassword=Набралі неадпаведны пароль. Паспрабуйце яшчэ раз.

; *** "License Agreement" wizard page
WizardLicense=Ліцэнзійнае пагадненьне
LicenseLabel=Перад тым, як працягваць, прачытайце наступныя важныя зьвесткі.
LicenseLabel3=Прачытайце ліцэнзійнае пагадненьне. Каб усталяваць праґраму, трэба пагадзіцца з умовамі гэтага пагадненьня.
LicenseAccepted=Я &прымаю пагадненьне
LicenseNotAccepted=Я &не прымаю пагадненьне

; *** "Information" wizard pages
WizardInfoBefore=Зьвесткі
InfoBeforeLabel=Перад тым, як працягваць, прачытайце наступныя важныя зьвесткі.
InfoBeforeClickLabel=Калі прачытаеце, націсьніце «Наперад», каб працягваць усталёўваць.
WizardInfoAfter=Зьвесткі
InfoAfterLabel=Перад тым, як працягваць, прачытайце наступныя важныя зьвесткі.
InfoAfterClickLabel=Калі прачытаеце, націсьніце «Наперад», каб працягваць усталёўваць.

; *** "User Information" wizard page
WizardUserInfo=Зьвесткі пра карыстальніка
UserInfoDesc=Пазначце зьвесткі пра сябе.
UserInfoName=&Імя карыстальніка:
UserInfoOrg=&Установа:
UserInfoSerial=&Сэрыйны нумар:
UserInfoNameRequired=Трэба пазначыць імя.

; *** "Select Destination Location" wizard page
WizardSelectDir=Абярыце месца прызначэньня
SelectDirDesc=Куды трэба ўсталяваць «[name]»?
SelectDirLabel3=«[name]» усталюецца ў наступны каталёґ.
SelectDirBrowseLabel=Каб працягваць, націсьніце «Наперад». Калі трэба абраць іншы каталёґ, націсьніце «Зірнуць».
DiskSpaceMBLabel=Спатрэбіцца прынамсі [mb] МБ прасторы на дыску.
CannotInstallToNetworkDrive=Немагчыма ўсталёўваць на сеткавы дыск.
CannotInstallToUNCPath=Немагчыма ўсталёўваць у каталёґ па шляху UNC.
InvalidPath=Трэба пазначыць поўны шлях з літарай дыску, напрыклад:%n%nC:\Праґрама%n%nабо шлях UNC ў такім выглядзе:%n%n\\паслужнік\крыніца
InvalidDrive=Абраныя прылада ці каталёґ UNC не існуюць, або да іх няма доступу. Абярыце іншае.
DiskSpaceWarningTitle=Нестае вольнае прасторы на дыску.
DiskSpaceWarning=Каб усталёўваць, трэба прынамсі %1 КБ вольнае прасторы, але абраная прылада мае толькі %2 КБ.%n%nЦі працягваць усё роўна?
DirNameTooLong=Занадта доўгія назва каталёґа ці шлях.
InvalidDirName=Хібная назва каталёґа.
BadDirName32=Назва каталёґа ня можа зьмяшчаць наступныя знакі:%n%n%1
DirExistsTitle=Каталёґ існуе
DirExists=Каталёґ:%n%n%1%n%nужо існуе. Ці ўсталяваць у яго ўсё роўна?
DirDoesntExistTitle=Каталёґ не існуе.
DirDoesntExist=Каталёґ:%n%n%1%n%nне існуе. Ці стварыць яго?

; *** "Select Components" wizard page
WizardSelectComponents=Абярыце складнікі
SelectComponentsDesc=Якія складнікі трэба ўсталяваць?
SelectComponentsLabel2=Абярыце складнікі, якія хочаце ўсталяваць; выкрасьліце складнікі, якія ня хочаце ўсталёўваць. Калі скончыце абіраць, націсьніце «Наперад».
FullInstallation=Усталяваць цалкам
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Усталяваць толькі неабходнае
CustomInstallation=Абраць, што ўсталёўваць
NoUninstallWarningTitle=Складнік існуе
NoUninstallWarning=Выявілася, што на кампутар ужо былі ўсталявалі наступнае:%n%n%1%n%nНават калі выкрасьліць гэтыя складнікі, яны застануцца ўсталяванымі.%n%nЦі працягваць усё роўна?
ComponentSize1=%1 КБ
ComponentSize2=%1 МБ
ComponentsDiskSpaceMBLabel=Абранае патрабуе прынамсі [mb] МБ прасторы на дыску.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Абраць дадатковыя заданьні
SelectTasksDesc=Што трэба дадаткова зрабіць?
SelectTasksLabel2=Абярыце дадатковыя заданьні, якія трэба выканаць, калі будзе ўсталёўвацца «[name]», а потым націсьніце «Наперад».

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Абярыце каталёґ галоўнага мэню
SelectStartMenuFolderDesc=Дзе трэба пакінуць выклічнікі да праґрамы?
SelectStartMenuFolderLabel3=Выклічнікі да праграмы створацца ў наступным каталёґу галоўнага мэню.
SelectStartMenuFolderBrowseLabel=Каб працягваць, націсьніце «Наперад». Калі трэба абраць іншы каталёґ, націсьніце «Зірнуць».
MustEnterGroupName=Трэба пазначыць назву каталёґа.
GroupNameTooLong=Занадта доўгія назва каталёґа ці шлях.
InvalidGroupName=Хібная назва каталёґа.
BadGroupName=Назва каталёґа ня можа зьмяшчаць наступныя знакі:%n%n%1
NoProgramGroupCheck2=&Не ствараць каталёґ у галоўным мэню

; *** "Ready to Install" wizard page
WizardReady=Можна пачынаць усталёўваць
ReadyLabel1=Ужо можна пачынаць усталёўваць «[name]» на кампутар.
ReadyLabel2a=Каб усталяваць, націсьніце «Ўсталяваць». Калі жадаеце перагледзець ці зьмяніць нейкія налады, націсьніце «Назад».
ReadyLabel2b=Каб працягваць усталёўваць, націсьніце «Ўсталяваць».
ReadyMemoUserInfo=Зьвесткі пра карыстальніка:
ReadyMemoDir=Мейсца прызначэньня:
ReadyMemoType=Спосаб усталёўваць:
ReadyMemoComponents=Абраныя складнікі:
ReadyMemoGroup=Тэчка галоўнага мэню:
ReadyMemoTasks=Дадатковыя заданьні:

; *** "Preparing to Install" wizard page
WizardPreparing=Рыхтуемся ўсталёўваць
PreparingDesc=«[name]» рыхтуецца ўсталявацца на кампутар.
PreviousInstallNotCompleted=Яшчэ ня скончылі ўсталёўваць ці высталёўваць папярэднюю праґраму. Каб скончыць усталёўваць, трэба нанова запусьціць кампутар.%n%nКалі кампутар запусьціцца, зноўку адчыніце праґраму ўсталяваньня «[name]».
CannotContinue=Нельга працягваць усталёўваць. Каб выйсьці, націсьніце «Скасаваць».
ApplicationsFound=Наступныя праґрамы карыстаюцца файламі, якія праґрама ўсталяваньня патрабуе абнавіць. Раіцца дазволіць праґраме ўсталяваньня зачыніць гэтыя праґрамы.
ApplicationsFound2=Наступныя праґрамы карыстаюцца файламі, якія праґрама ўсталяваньня патрабуе абнавіць. Раіцца дазволіць праґраме ўсталяваньня зачыніць гэтыя праґрамы. Праґрама ўсталяваньня паспрабуе зноўку запусьціць гэтыя праґрамы, калі скончыць усталёўваць.
CloseApplications=&Зачыніць праґрамы
DontCloseApplications=&Не зачыняць праґрамы

; *** "Installing" wizard page
WizardInstalling=Усталёўваецца
InstallingLabel=Калі ласка, пачакайце, пакуль «[name]» усталёўваецца на кампутар.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Дапаможнік усталёўваць «[name]» заканчвае працаваць
FinishedLabelNoIcons=Скончылі ўсталёўваць «[name]» на кампутар.
FinishedLabel=Скончылі ўсталёўваць «[name]» на кампутар. Праґраму можна запусьціць усталяванымі значкамі (выклічнікамі).
ClickFinish=Каб выйсьці з усталяваньня, націсьніце «Скончыць».
FinishedRestartLabel=Каб скончыць усталёўваць «[name]», трэба нанова запусьціць кампутар. Ці запусьціць нанова зараз?
FinishedRestartMessage=Каб скончыць усталёўваць «[name]», трэба нанова запусьціць кампутар.%n%nЦі запусьціць нанова зараз?
ShowReadmeCheck=Так, я хачу паглядзець файл з дадатковымі заўвагамі
YesRadio=&Так, запусьціць нанова
NoRadio=&Не, я запушчу нанова пазьней
; used for example as 'Run MyProg.exe'
RunEntryExec=Запусьціць %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Прагледзець %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Каб усталёўваць, патрабуецца наступны дыск
SelectDiskLabel2=Усуньце дыску «%1» і націсьніце «Добра».%n%nКалі файлы на дыску трэба шукаць ня ў тым каталёґу, які бачна ўнізе, пазначце адпаведны шлях або націсьніце «Зірнуць».
PathLabel=&Шлях:
FileNotInDir2=Па шляху «%2» не знайшлі файл «%1». Усуньце адпаведны дыск ці абярыце іншы каталёґ.
SelectDirectoryLabel=Пазначце наступны дыск.

; *** Installation phase messages
SetupAborted=Ня скончылі ўсталёўваць праґраму.%n%nВыпраўце неабходнае ды запусьціце ўсталёўваць зноўку.
EntryAbortRetryIgnore=Каб паспрабаваць яшчэ раз, націсьне «Спрабаваць». Каб усё роўна працягваць, націсьніце «Не зважаць» (ня раіцца так рабіць). Каб спыніць усталёўваць, націсьніце «Спыніць».

; *** Installation status messages
StatusClosingApplications=Зачыняюцца праґрамы…
StatusCreateDirs=Ствараюцца каталёґі…
StatusExtractFiles=Вымаюцца файлы…
StatusCreateIcons=Ствараюцца выклічнікі…
StatusCreateIniEntries=Ствараюцца запісы ў файлах INI…
StatusCreateRegistryEntries=Ствараюцца запісу ў рэестры…
StatusRegisterFiles=Робяцца запісы для файлаў…
StatusSavingUninstall=Захоўваюцца зьвесткі высталёўваць праґраму…
StatusRunProgram=Заканчваем усталёўваць…
StatusRestartingApplications=Праґрамы запускаюцца зноўку…
StatusRollback=Вяртаем назад зьмененае…

; *** Misc. errors
ErrorInternal2=Унутраная хіба: %1
ErrorFunctionFailedNoCode=Не ўдалося зрабіць %1
ErrorFunctionFailed=Не ўдалося зрабіць %1; код %2
ErrorFunctionFailedWithMessage=Не ўдалося зрабіць %1; код %2.%n%3
ErrorExecutingProgram=Нельга выканаць файл:%n%1

; *** Registry errors
ErrorRegOpenKey=Не ўдаецца адчыніць ключ рэестру:%n%1\%2
ErrorRegCreateKey=Не ўдаецца стварыць ключ рэестру:%n%1\%2
ErrorRegWriteKey=Не ўдаецца запісаць ключ рэестру:%n%1\%2

; *** INI errors
ErrorIniEntry=Не ўдаецца стварыць запіс у файле INI «%1».

; *** File copying errors
FileAbortRetryIgnore=Каб паспрабаваць яшчэ раз, націсьне «Спрабаваць». Каб прапусьціць файл, націсьніце «Не зважаць» (ня раіцца так рабіць). Каб спыніць усталёўваць, націсьніце «Спыніць».
FileAbortRetryIgnore2=Каб паспрабаваць яшчэ раз, націсьне «Спрабаваць». Каб усё роўна працягваць, націсьніце «Не зважаць». Каб спыніць усталёўваць, націсьніце «Спыніць».
SourceIsCorrupted=Зыходны файл — пашкоджаны
SourceDoesntExist=Зыходны файл «%1» не існуе
ExistingFileReadOnly=Існы файлы можна толькі чытаць.%n%nКаб прыбраць забарону запісваць у файл і паспрабаваць зноўку, націсьніце «Спрабаваць». Каб прапусьціць файл, націсьніце «Не зважаць». Каб спыніць усталёўваць праґраму, націсьніце «Спыніць».
ErrorReadingExistingDest=Не ўдалося прачытаць існы файл:
FileExists=Файл ужо існуе.%n%nЦі запісаць паўзьверх яго?
ExistingFileNewer=Існы файл — навейшы за той, што спрабуем усталяваць. Раіцца захаваць існы файл.%n%nЦі захаваць існы файл?
ErrorChangingAttr=Не ўдалося зьмяніць уласьцівасьці існага файла:
ErrorCreatingTemp=Не ўдалося стварыць файл у прызначаным каталёґу:
ErrorReadingSource=Не ўдалося прачытаць зыходны файл:
ErrorCopying=Не ўдалося перапісаць файл:
ErrorReplacingExistingFile=Не ўдалося замяніць існы файл:
ErrorRestartReplace=Не ўдалося нанова запусьціць замяняць файл:
ErrorRenamingTemp=Не ўдалося даць новую назву файлу ў прызначаным каталёґу:
ErrorRegisterServer=Немагчыма зрабіць запіс для DLL ці OCX: %1
ErrorRegSvr32Failed=«RegSvr32» няўдала выйшла з кодам %1
ErrorRegisterTypeLib=Немагчыма зрабіць запіс для бібліятэк наступнага віду: %1

; *** Post-installation errors
ErrorOpeningReadme=Не ўдалося адчыніць файл з заўвагамі.
ErrorRestartingComputer=Немагчыма нанова запусьціць кампутар. Зрабіце гэта самастойна.

; *** Uninstaller messages
UninstallNotFound=Файл «%1» не існуе. Немагчыма высталяваць.
UninstallOpenError=Файл «%1» нельга адчыніць. Немагчыма высталяваць
UninstallUnsupportedVer=Праґрама высталяваньня ня можа зразумець зьмесьціва справаздачы высталяваньня «%1». Немагчыма высталяваць
UninstallUnknownEntry=У справаздачы высталяваньня трапіўся невядомы запіс (%1)
ConfirmUninstall=Ці ўпэўненыя вы, што трэба цалком сьцерці праґраму «%1» з усімі складнікамі?
UninstallOnlyOnWin64=Высталяваць можна толькі на 64-бітнае вэрсіі «Windows».
OnlyAdminCanUninstall=Высталяваць можна толькі карыстальнік з правамі спраўніка.
UninstallStatusLabel=Пачакайце, пакуль «%1» сьціраецца з кампутара.
UninstalledAll=«%1» прыбралі з кампутара.
UninstalledMost=«%1» высталявалі.%n%nНекалькі файлаў не прыбралі. Іх можна сьцерці самастойна.
UninstalledAndNeedsRestart=Каб скончыць высталёўваць «%1», трэба нанова запусьціць кампутар.%n%nЦі запусьціць нанова зараз?
UninstallDataCorrupted=Файл «%1» мае пашкоджаньні. Немагчыма высталяваць

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Ці сьцерці абагулены файл?
ConfirmDeleteSharedFile2=Сыстэма паказвае, што наступны абагулены файл болей не выкарыстоўвае аніякая праґрама. Ці сьцерці гэты файл?%n%nКалі пэўныя праграмы працягваюць яго выкарыстоўваць, а яго сьцерці, гэтыя праґрамы ня будуць працаваць належным чынам. Калі вы ня маеце ўпэўненасьці, абярыце «Не». Калі пакінуць гэны файл у сыстэме, шкоды ня будзе.
SharedFileNameLabel=Назва файла:
SharedFileLocationLabel=Месца:
WizardUninstalling=Стан высталяваньня
StatusUninstalling=Высталёўваецца «%1»…

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Усталёўваецца «%1»
ShutdownBlockReasonUninstallingApp=Высталёўваецца «%1».

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=«%1» вэрсіі %2
AdditionalIcons=Дадатковыя значкі:
CreateDesktopIcon=Стварыць значак на &стальніцы
CreateQuickLaunchIcon=Стварыць значак на паліцы &хуткага запуску
ProgramOnTheWeb=«%1» у сеціве
UninstallProgram=Высталяваць «%1»
LaunchProgram=Запусьціць «%1»
AssocFileExtension=&Зьвязаць «%1» з файламі пашырэньня «%2»
AssocingFileExtension=«%1» зьвязваецца з файламі пашырэньня «%2»…
AutoStartProgramGroupDescription=Запускаць:
AutoStartProgram=Запускаць «%1» самарушна
AddonHostProgramNotFound=У абраным каталёґу не знайшлі файл «%1».%n%nЦі працягваць усё роўна?
