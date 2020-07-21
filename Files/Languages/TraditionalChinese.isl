; *** Inno Setup version 5.1.11+ Traditional Chinese messages ***

;

; To download user-contributed translations of this file, go to:

; http://www.jrsoftware.org/is3rdparty.php

;

; Note: When translating this text, do not add periods (.) to the end of

; messages that didn't have them already, because on those messages Inno

; Setup adds the periods automatically (appending a period would result in

; two periods being displayed).

[LangOptions]

; The following three entries are very important. Be sure to read and

; understand the '[LangOptions] section' topic in the help file.

LanguageName=Chinese

LanguageID=$0409

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

SetupAppTitle=安裝

SetupWindowTitle=安裝 - %1

UninstallAppTitle=反安裝

UninstallAppFullTitle=%1 反安裝

; *** Misc. common

InformationTitle=安裝信息

ConfirmTitle=提示

ErrorTitle=錯誤

; *** SetupLdr messages

SetupLdrStartupMessage=現在將安裝 %1。是否繼續?

LdrCannotCreateTemp=不能創建臨時文件。安裝中止！

LdrCannotExecTemp=不能在臨時目錄中解壓文件，安裝過程中止！

; *** Startup error messages

LastErrorMessage=%1.%n%n 錯誤 %2: %3

SetupFileMissing=在安裝目錄中找不到文件 %1 。請更正該問題或者獲得壹個新的文件。

SetupFileCorrupt=安裝文件被占用。請獲得壹個新的安裝文件。

SetupFileCorruptOrWrongVer=安裝文件被占用, 或者安裝文件的版本不對。請更正該問題或者獲得壹個新文件。

NotOnThisPlatform=該程序不能在 %1 下運行。

OnlyOnThisPlatform=該程序必須在 %1 下運行。

OnlyOnTheseArchitectures=該程序只能在以下WINDOWS版本下運行:%n%n%1

MissingWOW64APIs=該版本需要64位的安裝程序。要更正該問題，請安裝 Service Pack %1。

WinVersionTooLowError=該軟件需要 %1 版本號 %2 或者更高。

WinVersionTooHighError=該軟件不能在 %1 版本號 %2 或者更高版本中安裝。

AdminPrivilegesRequired=您在安裝該程序時必須是管理員權限。

PowerUserPrivilegesRequired=您在安裝程序是必須是管理員權限或者高權限用戶權限。

SetupAppRunningError=安裝程序檢測到 %1 正在運行。%n%n請關閉它的所有實例。然後單擊“確認”繼續安裝，或者單擊“放棄”退出。UninstallAppRunningError=反安裝程序檢測到 %1 正在運行。%n%n請關閉它的所有實例。然後單擊“確認”繼續安裝，或者單擊“放棄”退出。

UninstallAppRunningError=Uninstall has detected that %1 is currently running.%n%nPlease close all instances of it now, then click OK to continue, or Cancel to exit.

; *** Misc. errors

ErrorCreatingDir=安裝程序不能創建目錄 "%1"

ErrorTooManyFilesInDir=不能在目錄"%1" 下創建文件。因為該目錄下有太多的文件了。

; *** Setup common messages

ExitSetupTitle=退出安裝

ExitSetupMessage=安裝沒有完成。 如果您現在退出，程序將不被安裝。%n%n您可以下次在完成安裝任務。%n%n退出安裝?

AboutSetupMenuItem=關於安裝程序[&A]...

AboutSetupTitle=關於安裝程序

AboutSetupMessage=%1 版本 %2%n%3%n%n%1 主頁:%n%4

AboutSetupNote=

TranslatorNote=

; *** Buttons

ButtonBack=< 回退[&B]

ButtonNext=下壹步[&N] >

ButtonInstall=安裝[&I]

ButtonOK=確認

ButtonCancel=放棄

ButtonYes=市[&Y]

ButtonYesToAll=全部確認[&A]

ButtonNo=否[&N]

ButtonNoToAll=全部否認[&o]

ButtonFinish=完成[&F]

ButtonBrowse=瀏覽[&B]...

ButtonWizardBrowse=瀏覽[&r]...

ButtonNewFolder=新建文件夾[&M]

; *** "Select Language" dialog messages

SelectLanguageTitle=選擇安裝語言種類

SelectLanguageLabel=選擇該語言作為安裝語言:

; *** Common wizard text

ClickNext=單擊“下壹步”繼續，或者單擊“放棄”退出本安裝程序。

BeveledLabel=

BrowseDialogTitle=瀏覽文件夾

BrowseDialogLabel=在下面的列表中選擇壹個目錄, 然後單擊“確認”繼續。

NewFolderName=新文件夾

; *** "Welcome" wizard page

WelcomeLabel1=歡迎進入 [name] 安裝向導

WelcomeLabel2=將在您的電腦上安裝 [name/ver] 。%n%n建議在繼續安裝之前退出其它程序的運行。

; *** "Password" wizard page

WizardPassword=密碼

PasswordLabel1=本安裝程序受密碼保護。

PasswordLabel3=請輸入密碼，密碼區分大小寫。然後單擊“下壹步”繼續。

PasswordEditLabel=密碼[&P]:

IncorrectPassword=輸入的密碼不正確，請重新輸入。

; *** "License Agreement" wizard page

WizardLicense=用戶許可協議

LicenseLabel=請在繼續之前仔細閱讀用戶許可協議。

LicenseLabel3=請仔細閱讀下面的用戶許可協議。在繼續安裝之前，您必須接受該協議。

LicenseAccepted=我接受協議[&a]。

LicenseNotAccepted=我不接受協議[&d]

; *** "Information" wizard pages

WizardInfoBefore=提示

InfoBeforeLabel=在繼續安裝之前請仔細閱讀以下重要的信息。

InfoBeforeClickLabel=當您準備好繼續安裝時，單擊“繼續”。

WizardInfoAfter=信息

InfoAfterLabel=在繼續安裝之前請仔細閱讀以下重要的信息。

InfoAfterClickLabel=當您準備好繼續安裝時，單擊“繼續”。

; *** "User Information" wizard page

WizardUserInfo=用戶信息

UserInfoDesc=請輸入您的信息。

UserInfoName=用戶名[&U]:

UserInfoOrg=組織[&O]:

UserInfoSerial=序列號[&S]:

UserInfoNameRequired=您必須輸入壹個名稱。

; *** "Select Destination Location" wizard page

WizardSelectDir=選擇目標位置

SelectDirDesc=您將把[name]安裝在哪裏?

SelectDirLabel3=安裝程序將把[name]安裝在下面的的文件夾中。

SelectDirBrowseLabel=為了繼續安裝, 請單擊“下壹步”。如果您想選擇壹個不同的目錄，請單擊“瀏覽”。

DiskSpaceMBLabel=為了安裝本軟件，至少需要 [mb] MB 的空閑磁盤空間。

ToUNCPathname=安裝程序不能安裝到壹個 UNC 目錄名稱。如果您試圖網上安裝本程序，請映射網絡驅動器。

InvalidPath=您比如輸入壹個含盤符的路徑名稱。比如:%n%nC:\APP%n%n 或者壹個網絡路徑，例如:%n%n\\server\share

InvalidDrive=您選擇的磁盤或者網絡路徑不存在或者不能訪問。請重新選擇其它的磁盤或者網絡路徑。

DiskSpaceWarningTitle=磁盤空間不足。

DiskSpaceWarning=安裝程序至少需要 %1 KB 空閑磁盤空間來安裝本軟件。但是選擇的磁盤上只有 %2 KB 空間可用。%n%n 您確認繼續？

DirNameTooLong=該文件夾的名稱太長。

InvalidDirName=該文件夾的名字太長。

BadDirName32=文件夾名稱中不能包括以下的任何字符:%n%n%1

DirExistsTitle=文件夾已經存在

DirExists=文件夾:%n%n%1%n%n已經存在。您想繼續安裝?

DirDoesntExistTitle=文件夾不存在

DirDoesntExist=文件夾:%n%n%1%n%n不存在。您想創建該文件夾?

; *** "Select Components" wizard page

WizardSelectComponents=選擇部件

SelectComponentsDesc=哪些部件需要安裝?

SelectComponentsLabel2=選擇您想要安裝胡部件; 清除您不想安裝的部件。當您準備好後，請單擊“下壹步” 。

FullInstallation=完全安裝

; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)

CompactInstallation=精簡安裝

CustomInstallation=自定義安裝

NoUninstallWarningTitle=部件存在

NoUninstallWarning=安裝程序檢測到以下部件已經在您的電腦中被安裝了:%n%n%1%n%n不選擇它們將不在您的電腦中安裝它們。%n%n您想就這樣繼續嗎?

ComponentSize1=%1 KB

ComponentSize2=%1 MB

ComponentsDiskSpaceMBLabel=當前選擇項至少需要 [mb] MB 的空閑磁盤空間才能安裝。

; *** "Select Additional Tasks" wizard page

WizardSelectTasks=選擇壹個附加任務

SelectTasksDesc=您想選擇哪個附加任務?

SelectTasksLabel2=選擇在安裝[name]時執行的附加任務, 然後單擊“下壹步”。

; *** "Select Start Menu Folder" wizard page

WizardSelectProgramGroup=選擇開始菜單文件夾

SelectStartMenuFolderDesc=您想在哪裏放置軟件的快捷方式?

SelectStartMenuFolderLabel3=安裝程序將在下面的文件夾中創建軟件的快捷方式。

SelectStartMenuFolderBrowseLabel=為了繼續，單擊“下壹步”。如果您想另外選擇壹個文件夾，單擊“瀏覽”。

MustEnterGroupName=您必須輸入壹個文件夾名稱。

GroupNameTooLong=文件夾名稱或者路徑名稱太長。

InvalidGroupName=文件夾名稱非法。

BadGroupName=文件夾名字中不能包含下面的字符:%n%n%1

NoProgramGroupCheck2=不創建開始菜單中的文件夾[&D]

; *** "Ready to Install" wizard page

WizardReady=準備開始安裝

ReadyLabel1=安裝程序開始在您的電腦中安裝[name].

ReadyLabel2a=單擊“安裝”開始安裝本軟件,或者單擊“回退”修改安裝設置。

ReadyLabel2b=單擊“安裝”開始安裝本軟件。

ReadyMemoUserInfo=用戶信息:

ReadyMemoDir=安裝目標位置:

ReadyMemoType=安裝種類:

ReadyMemoComponents=選中的部件:

ReadyMemoGroup=開始菜單文件夾:

ReadyMemoTasks=附加任務:

; *** "Preparing to Install" wizard page

WizardPreparing=準備安裝

PreparingDesc=安裝程序準備在您的電腦中安裝[name]。

PreviousInstallNotCompleted=安裝/反安裝壹個以前的程序沒有完成。您需要重新啟動您的電腦來完成安裝工作。%n%n當重新啟動您的電腦後，請運行安裝程序來完成安裝[name]。

CannotContinue=安裝程序不能繼續執行。請單擊“放棄”退出。

; *** "Installing" wizard page

WizardInstalling=安裝中

InstallingLabel=安裝程序正在安裝[name],請等待。

; *** "Setup Completed" wizard page

FinishedHeadingLabel=[name]安裝完成

FinishedLabelNoIcons=安裝程序已經在您的電腦中安裝了[name]。

FinishedLabel=安裝程序已經在您的電腦中安裝了[name]。要執行本軟件，請單擊安裝好的本軟件圖標 。

ClickFinish=單擊“完成”退出本安裝程序.

FinishedRestartLabel=為了完成[name]的安裝, 安裝程序必須重新啟動您的電腦。您想現在就重新啟動?

FinishedRestartMessage=為了完成[name]的安裝, 安裝程序必須重新啟動您的電腦。%n%您想現在就重新啟動?

ShowReadmeCheck=是的,我想查看 README 文件

YesRadio=是的，我想重新啟動計算機[&Y]

NoRadio=不，我將稍後重新啟動計算機[&N]

; used for example as 'Run MyProg.exe'

RunEntryExec=運行 %1

; used for example as 'View Readme.txt'

RunEntryShellExec=查看 %1

; *** "Setup Needs the Next Disk" stuff

ChangeDiskTitle=安裝程序需要下壹個安裝盤

SelectDiskLabel2=請插入安裝盤 %1 並且單擊“確認”。%n%n如果該盤不是下面顯示的盤，請輸入正確的路徑或者單擊“瀏覽”。

PathLabel=目錄[&P]:

FileNotInDir2=在 "%2" 中沒有發現文件。請插入正確的磁盤或者選擇其它的文件夾。

SelectDirectoryLabel=請輸入下壹個磁盤中的正確位置。

; *** Installation phase messages

SetupAborted=安裝沒有完成。%n%n請修復錯誤並重新安裝。

EntryAbortRetryIgnore=單擊“重試”重新嘗試，單擊“忽略”將繼續安裝，或者單擊“放棄”退出安裝。

; *** Installation status messages

StatusCreateDirs=創建目錄中...

StatusExtractFiles=解壓文件中...

StatusCreateIcons=創建快捷方式中...

StatusCreateIniEntries=創建INI單元中...

StatusCreateRegistryEntries=創建註冊表內容中...

StatusRegisterFiles=註冊文件中...

StatusSavingUninstall=保存反安裝信息中...

StatusRunProgram=正在完成安裝...

StatusRollback=恢復原來修改的內容中...

; *** Misc. errors

ErrorInternal2=內部錯誤: %1

ErrorFunctionFailedNoCode=%1 失敗

ErrorFunctionFailed=%1 失敗; 代碼 %2

ErrorFunctionFailedWithMessage=%1 錯誤; 代碼 %2.%n%3

ErrorExecutingProgram=不能執行文件:%n%1

; *** Registry errors

ErrorRegOpenKey=在打開註冊表鍵時發生錯誤:%n%1\%2

ErrorRegCreateKey=在創建註冊表鍵時發生錯誤:%n%1\%2

ErrorRegWriteKey=在些註冊表鍵時發生錯誤:%n%1\%2

; *** INI errors

ErrorIniEntry=在創建INI文件時發生錯誤 "%1".

; *** File copying errors

FileAbortRetryIgnore=單擊“重試”再試壹次，單擊“忽略”忽略該文件，或者單擊“放棄”退出安裝程序。

FileAbortRetryIgnore2=單擊“重試”再試壹次，單擊“忽略”忽略該文件繼續安裝，或者單擊“放棄”退出安裝程序。

SourceIsCorrupted=源文件被使用

SourceDoesntExist=源文件 "%1" 不存在

ExistingFileReadOnly=該已經存在的文件是只讀屬性。%n%n單擊“重試”刪除只讀屬性並重新嘗試，單擊“忽略”忽略該文件，或者單擊“放棄”退出安裝程序。

ErrorReadingExistingDest=當試圖讀取壹個已經存在的文件時發生了錯誤:

FileExists=該文件已經存在。%n%n您想覆蓋它嗎?

ExistingFileNewer=已經存在的文件比安裝程序試圖安裝的文件要新。建議您保留該文件。%n%n您想保留已經存在的文件嗎?

ErrorChangingAttr=當試圖改變壹個存在的文件的屬性時發生了錯誤:

ErrorCreatingTemp=當試圖在目標目錄中創建壹個文件時發生了錯誤:

ErrorReadingSource=當試圖讀取壹個文件時發生了錯誤:

ErrorCopying=當試圖復制壹個文件時發生了錯誤:

ErrorReplacingExistingFile=當試圖覆蓋已經存在的文件時發生錯誤:

ErrorRestartReplace=重新啟動置換失敗:

ErrorRenamingTemp=當在目標目錄中重命名文件時發生錯誤:

ErrorRegisterServer=不能註冊 DLL/OCX: %1

ErrorRegSvr32Failed=RegSvr32 執行失敗, 錯誤碼: %1

ErrorRegisterTypeLib=不能註冊以下類型庫: %1

; *** Post-installation errors

ErrorOpeningReadme=當打開 README 文件時發生錯誤。

ErrorRestartingComputer=安裝程序不能重新啟動電腦。請手動啟動。

; *** Uninstaller messages

UninstallNotFound=文件 "%1" 不存在。不能反安裝。

UninstallOpenError=文件 "%1" 不能打開。不能反安裝。

UninstallUnsupportedVer=反安裝記錄文件 "%1" 不是基於本安裝程序的版本。不能完成軟件的刪除工作。

UninstallUnknownEntry=壹個未知的實體 (%1) 存在於反安裝記錄文件中。

ConfirmUninstall=您確定完全刪除 %1 和所有基於它上面的部件嗎?

UninstallOnlyOnWin64=該安裝程序只能在64位的Windows上執行反安裝工作。

OnlyAdminCanUninstall=該安裝程序只能在您具有管理員權限時才能執行反安裝工作。

UninstallStatusLabel= %1 正在被刪除，請稍等。

UninstalledAll=%1 被成功地從您的電腦中刪除。

UninstalledMost=%1 刪除完成。%n%n有某些部件不能被刪除。您可能需要手動刪除它們。

UninstalledAndNeedsRestart=為了完成 %1 的刪除工作, 您必須重新啟動電腦。%n%n您想現在就重新啟動嗎?

UninstallDataCorrupted=文件"%1" 被占用。不能完成刪除工作。

; *** Uninstallation phase messages

ConfirmDeleteSharedFileTitle=刪除共享程序文件?

ConfirmDeleteSharedFile2=系統指出以下的共享程序文件將不再被使用。您是否想刪除這些共享文件?%n%n如果這些文件刪除後，其他程序仍然要使用它，可能其它程序的功能將受影響。如果您不能肯定，請選擇“否”。讓這些文件保留在系統中不會對系統造成損害。

SharedFileNameLabel=文件名:

SharedFileLocationLabel=位置:

WizardUninstalling=反安裝

StatusUninstalling=反安裝 %1 中...

; The custom messages below aren't used by Setup itself, but if you make

; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 版本 %2

AdditionalIcons=附加圖標:

CreateDesktopIcon=創建桌面圖標[&d]

CreateQuickLaunchIcon=創建快速啟動圖標[&Q]

ProgramOnTheWeb=%1 on the Web

UninstallProgram=反安裝 %1

LaunchProgram=執行 %1

AssocFileExtension=用文件擴展名 %2 匹配[&A] %1?

AssocingFileExtension=正在用文件擴展名 %2 匹配[&A] %1 中...
