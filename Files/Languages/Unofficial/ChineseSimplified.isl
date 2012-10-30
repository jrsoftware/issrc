; *** Inno Setup version 5.5.0+ Chinese (Simplified) messages ***
;   by Mack Zhang (hua_wuxin@21cn.com)
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
LanguageName=<4E2D><6587><FF08><7B80><4F53><FF09>
LanguageID=$0804
LanguageCodePage=936
; If the language you are translating to requires special font faces or
; sizes, uncomment any of the following entries and change them accordingly.
DialogFontName=宋体
DialogFontSize=9
;WelcomeFontName=Verdana
;WelcomeFontSize=12
;TitleFontName=Arial
;TitleFontSize=29
;CopyrightFontName=Arial
;CopyrightFontSize=8

[Messages]

; *** Application titles
SetupAppTitle=安装向导
SetupWindowTitle=安装向导 - %1
UninstallAppTitle=卸载向导
UninstallAppFullTitle=%1 卸载向导

; *** Misc. common
InformationTitle=信息
ConfirmTitle=确认
ErrorTitle=错误

; *** SetupLdr messages
SetupLdrStartupMessage=安装向导将在你的电脑上安装 %1。你确定要继续吗？
LdrCannotCreateTemp=无法创建临时文件。安装中止
LdrCannotExecTemp=无法运行临时文件夹中的文件。安装中止

; *** Startup error messages
LastErrorMessage=%1.%n%n错误 %2：%3
SetupFileMissing=安装文件夹缺少文件 %1。请纠正此问题或者索取软件的新版本。
SetupFileCorrupt=安装文件已损坏。请索取软件的新版本。
SetupFileCorruptOrWrongVer=安装文件已损坏，或者与本安装向导的版本不兼容。请纠正此问题或者索取软件的新版本。
InvalidParameter=一个错误的参数被传递到了命令行：%n%n%1
SetupAlreadyRunning=安装已经运行。
WindowsVersionNotSupported=本程序不支持您的电脑上运行的 Windows 版本。
WindowsServicePackRequired=本程序需要 %1 Service Pack %2 或更新的版本。
NotOnThisPlatform=本程序不能在 %1 上运行。
OnlyOnThisPlatform=本程序必须在 %1 上运行。
OnlyOnTheseArchitectures=本程序只能安装在为下列处理器架构设计的 Windows 版本中：%n%n%1
MissingWOW64APIs=当前的 Windows 版本没有包含执行 64 位安装向导所需的函数。若要纠正此问题，请安装 Service Pack %1。
WinVersionTooLowError=本程序需要 %1 v%2 或更高版本。
WinVersionTooHighError=本程序不能安装在 %1 v%2 或更高版本上。
AdminPrivilegesRequired=安装本程序时你必须以管理员身份登录。
PowerUserPrivilegesRequired=安装本程序时你必须以管理员或 Power Users 组成员的身份登录。
SetupAppRunningError=安装向导发现 %1 正在运行。%n%n请关闭其所有实例，然后单击“确定”继续，或单击“取消”退出。
UninstallAppRunningError=卸载向导发现 %1 正在运行。%n%n请关闭其所有实例，然后单击“确定”继续，或单击“取消”退出。

; *** Misc. errors
ErrorCreatingDir=安装向导无法创建文件夹“%1”
ErrorTooManyFilesInDir=无法在文件夹“%1”中创建文件，因为它包含了太多文件

; *** Setup common messages
ExitSetupTitle=退出安装
ExitSetupMessage=安装尚未完成。如果你现在退出，软件将不会安装。%n%n你可以在其它时间重新运行安装向导来完成安装。%n%n现在退出安装吗？
AboutSetupMenuItem=关于安装向导(&A)…
AboutSetupTitle=关于安装向导
AboutSetupMessage=%1 版本 %2%n%3%n%n%1 主页：%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< 上一步(&B)
ButtonNext=下一步(&N) >
ButtonInstall=安装(&I)
ButtonOK=确定
ButtonCancel=取消
ButtonYes=是(&Y)
ButtonYesToAll=全是(&A)
ButtonNo=否(&N)
ButtonNoToAll=全否(&O)
ButtonFinish=完成(&F)
ButtonBrowse=浏览(&B)…
ButtonWizardBrowse=浏览(&R)…
ButtonNewFolder=创建文件夹(&M)

; *** "Select Language" dialog messages
SelectLanguageTitle=选择安装语言
SelectLanguageLabel=选择安装期间要使用的语言：

; *** Common wizard text
ClickNext=单击“下一步”继续，或单击“取消”退出安装。
BeveledLabel=
BrowseDialogTitle=浏览文件夹
BrowseDialogLabel=选择一个文件夹，然后单击“确定”。
NewFolderName=新建文件夹

; *** "Welcome" wizard page
WelcomeLabel1=欢迎使用 [name] 安装向导
WelcomeLabel2=安装向导将在你的电脑上安装 [name/ver]。%n%n建议你在继续之前关闭所有其它应用程序。

; *** "Password" wizard page
WizardPassword=密码
PasswordLabel1=本安装有密码保护。
PasswordLabel3=请输入密码，然后单击“下一步”进入下一步。密码区分大小写。
PasswordEditLabel=密码(&P)：
IncorrectPassword=你输入的密码不正确。请重试。

; *** "License Agreement" wizard page
WizardLicense=许可协议
LicenseLabel=请在继续之前阅读以下重要信息。
LicenseLabel3=请阅读以下许可协议。在继续安装之前，你必须接受此协议的条款。
LicenseAccepted=我接受协议(&A)
LicenseNotAccepted=我不接受协议(&D)

; *** "Information" wizard pages
WizardInfoBefore=信息
InfoBeforeLabel=请在继续之前阅读以下重要信息。
InfoBeforeClickLabel=当你准备好继续安装后，请单击“下一步”。
WizardInfoAfter=信息
InfoAfterLabel=请在继续之前阅读以下重要信息。
InfoAfterClickLabel=当你准备好继续安装后，请单击“下一步”。

; *** "User Information" wizard page
WizardUserInfo=用户信息
UserInfoDesc=请输入你的信息。
UserInfoName=用户名(&U)：
UserInfoOrg=组织(&O)：
UserInfoSerial=序列号(&S)：
UserInfoNameRequired=必须输入用户名。

; *** "Select Destination Location" wizard page
WizardSelectDir=选择目标位置
SelectDirDesc=将 [name] 安装到哪里？
SelectDirLabel3=安装向导将把 [name] 安装到以下文件夹中。
SelectDirBrowseLabel=若要继续，单击“下一步”。如果你要选择不同的文件夹，请单击“浏览”。
DiskSpaceMBLabel=至少需要 [mb] MB 的空闲磁盘空间。
CannotInstallToNetworkDrive=无法安装到网络驱动器。
CannotInstallToUNCPath=无法安装到 UNC 路径。
InvalidPath=你必须输入带有盘符的完整路径。例如：%n%nC:\APP%n%n或者 UNC 路径格式：%n%n\\server\share
InvalidDrive=你选择的驱动器或 UNC 共享不存在或不可访问。请重新选择。
DiskSpaceWarningTitle=没有足够的磁盘空间
DiskSpaceWarning=安装需要至少 %1 KB 的剩余空间，但是所选驱动器只有 %2 KB 可用。%n%n你仍然要继续吗？
DirNameTooLong=文件夹名称或路径太长。
InvalidDirName=文件夹名称无效。
BadDirName32=文件夹名称不能包含下列字符：%n%n%1
DirExistsTitle=文件夹已存在
DirExists=文件夹：%n%n%1%n%n已存在。你确定要安装到该文件夹吗？
DirDoesntExistTitle=文件夹不存在
DirDoesntExist=文件夹：%n%n%1%n%n不存在。创建该文件夹吗？

; *** "Select Components" wizard page
WizardSelectComponents=选择组件
SelectComponentsDesc=要安装哪些组件？
SelectComponentsLabel2=请选择你要安装的组件，清除你不想安装的组件。准备好后点击“下一步”。
FullInstallation=完整安装
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=简洁安装
CustomInstallation=定制安装
NoUninstallWarningTitle=组件已存在
NoUninstallWarning=安装向导发现下列组件已经安装：%n%n%1%n%n取消选定不会卸载这些组件。%n%n继续安装吗？
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=当前的选择至少需要 [mb] MB 磁盘空间。

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=选择附加任务
SelectTasksDesc=要执行哪些附加任务？
SelectTasksLabel2=请选择在安装 [name] 期间安装向导要执行的附加任务，然后点击“下一步”。

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=选择开始菜单文件夹
SelectStartMenuFolderDesc=把程序快捷方式放到哪里？
SelectStartMenuFolderLabel3=安装向导将在以下开始菜单文件夹中创建程序快捷方式。
SelectStartMenuFolderBrowseLabel=点击“下一步”进入下一步。如果你要选择不同的文件夹，请点击“浏览”。
MustEnterGroupName=你必须输入文件夹名称
GroupNameTooLong=文件夹名称或路径太长
InvalidGroupName=文件夹名称无效
BadGroupName=文件夹名称不能包含下列字符：%n%n%1
NoProgramGroupCheck2=不创建开始菜单文件夹(&D)

; *** "Ready to Install" wizard page
WizardReady=准备安装
ReadyLabel1=安装向导现在准备开始安装 [name]。
ReadyLabel2a=点击“安装”继续安装，如果你想要查看或者更改设置请点击“上一步”。
ReadyLabel2b=点击“安装”继续安装。
ReadyMemoUserInfo=用户信息：
ReadyMemoDir=目标位置：
ReadyMemoType=安装类型：
ReadyMemoComponents=所选组件：
ReadyMemoGroup=开始菜单文件夹：
ReadyMemoTasks=附加任务：

; *** "Preparing to Install" wizard page
WizardPreparing=正在准备安装
PreparingDesc=安装向导正在准备安装 [name]。
PreviousInstallNotCompleted=先前程序的安装/卸载尚未完成。你需要重启电脑来完成安装。%n%n电脑重启之后，请重新运行安装向导来完成 [name] 的安装。
CannotContinue=安装不能继续。请点击“取消”退出。
ApplicationsFound=安装向导需要更新被下列应用程序占用的文件。建议你允许自动关闭这些应用程序。
ApplicationsFound2=安装向导需要更新被下列应用程序占用的文件。建议你允许自动关闭这些应用程序。安装完成后，安装向导会尝试重新启动这些应用程序。
CloseApplications=自动关闭应用程序(&A)
DontCloseApplications=不自动关闭应用程序(&D)

; *** "Installing" wizard page
WizardInstalling=正在安装
InstallingLabel=正在你的计算机中安装 [name]，请稍等…

; *** "Setup Completed" wizard page
FinishedHeadingLabel=完成 [name] 安装
FinishedLabelNoIcons=安装向导已完成 [name] 的安装。
FinishedLabel=安装向导已完成 [name] 的安装。可以通过选择已安装的图标来运行应用程序。
ClickFinish=点击“完成”退出安装。
FinishedRestartLabel=为了完成 [name] 的安装，安装向导必须重启电脑。你要立即重启吗？
FinishedRestartMessage=为了完成 [name] 的安装，安装向导必须重启电脑。%n%n你要立即重启吗？
ShowReadmeCheck=是，我要查看自述文件
YesRadio=是，立即重启电脑(&Y)
NoRadio=否，稍后重启电脑(&N)
; used for example as 'Run MyProg.exe'
RunEntryExec=运行 %1
; used for example as 'View Readme.txt'
RunEntryShellExec=查看 %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=安装向导需要下一个磁盘
SelectDiskLabel2=请插入磁盘 %1 并点击“确定”。%n%n如果在除了下面显示的文件夹以外的文件夹中找不到该磁盘上的文件，就请输入正确的路径或点击“浏览”。
PathLabel=路径(&P)：
FileNotInDir2=文件“%1”不在“%2”中。请插入正确的磁盘或选择其他文件夹。
SelectDirectoryLabel=请指定下一个磁盘的位置。

; *** Installation phase messages
SetupAborted=安装尚未完成。%n%n请纠正问题并重新运行安装向导。
EntryAbortRetryIgnore=点击“重试”重新尝试，点击“忽略”继续安装，或点击“中止”取消安装。

; *** Installation status messages
StatusClosingApplications=正在关闭应用程序…
StatusCreateDirs=正在创建文件夹…
StatusExtractFiles=正在提取文件…
StatusCreateIcons=正在创建快捷方式…
StatusCreateIniEntries=正在创建 INI 项目…
StatusCreateRegistryEntries=正在创建注册表项目…
StatusRegisterFiles=正在注册文件…
StatusSavingUninstall=正在保存卸载信息…
StatusRunProgram=正在完成安装…
StatusRestartingApplications=正在重启应用程序…
StatusRollback=正在回滚更改…

; *** Misc. errors
ErrorInternal2=内部错误：%1
ErrorFunctionFailedNoCode=%1 失败
ErrorFunctionFailed=%1 失败。代码 %2
ErrorFunctionFailedWithMessage=%1 失败。代码 %2。%n%3
ErrorExecutingProgram=无法执行文件：%n%1

; *** Registry errors
ErrorRegOpenKey=打开注册表键时出错：%n%1\%2
ErrorRegCreateKey=创建注册表键时出错：%n%1\%2
ErrorRegWriteKey=写入注册表键时出错：%n%1\%2

; *** INI errors
ErrorIniEntry=在文件“%1”中创建 INI 项目时出错。

; *** File copying errors
FileAbortRetryIgnore=点击“重试”重新尝试，点击“忽略”跳过此文件（不推荐），或点击“中止”取消安装。
FileAbortRetryIgnore2=点击“重试”重新尝试，点击“忽略”继续安装（不推荐），或点击“中止”取消安装。
SourceIsCorrupted=源文件已损坏
SourceDoesntExist=源文件“%1”不存在
ExistingFileReadOnly=现有文件为只读。%n%n点击“重试”移除只读属性并重试，点击“忽略”跳过此文件，或点击“中止”取消安装。
ErrorReadingExistingDest=读取现有文件时发生错误：
FileExists=文件已存在。%n%n你要覆盖它吗？
ExistingFileNewer=现有文件比安装向导要安装的还新。建议你保留现有文件。%n%n保留现有文件吗？
ErrorChangingAttr=更改现有文件的属性时发生错误：
ErrorCreatingTemp=在目标文件夹中创建文件时发生错误：
ErrorReadingSource=读取源文件时发生错误：
ErrorCopying=复制文件时发生错误：
ErrorReplacingExistingFile=替换现有文件时发生错误：
ErrorRestartReplace=重启后替换失败：
ErrorRenamingTemp=重命名目标文件夹中的文件时发生错误：
ErrorRegisterServer=无法注册 DLL/OCX：%1
ErrorRegSvr32Failed=RegSvr32 失败。返回值：%1
ErrorRegisterTypeLib=无法注册类型库：%1

; *** Post-installation errors
ErrorOpeningReadme=打开自述文件时发生错误。
ErrorRestartingComputer=安装向导无法重启电脑。请手动重启。

; *** Uninstaller messages
UninstallNotFound=文件“%1”不存在。不能卸载。
UninstallOpenError=文件“%1”不能打开。不能卸载
UninstallUnsupportedVer=卸载日志文件“%1”的格式不能被此版本的卸载向导识别。不能卸载
UninstallUnknownEntry=卸载日志中遇到一个未知的项目 (%1)
ConfirmUninstall=你是否确定要完全删除 %1 及其所有组件？
UninstallOnlyOnWin64=此安装只能在 64 位 Windows 上卸载。
OnlyAdminCanUninstall=此安装只能由具备管理员权限的用户卸载。
UninstallStatusLabel=正在删除 %1，请稍等…
UninstalledAll=%1 已成功删除。
UninstalledMost=%1 卸载完成。%n%n某些项目不能删除，可以手动删除。
UninstalledAndNeedsRestart=若要完成 %1 的卸载，必须重启电脑。%n%n你要立即重启吗？
UninstallDataCorrupted=文件“%1”已损坏。不能卸载

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=删除共享文件吗？
ConfirmDeleteSharedFile2=下列共享文件不再被任何程序使用。你要删除该共享文件吗？%n%n如果还有程序使用该文件而它已被删除，这些程序可能无法正常运行。如果你不确定，就请选择“否”。留下该文件不会对系统造成任何危害。
SharedFileNameLabel=文件名：
SharedFileLocationLabel=位置：
WizardUninstalling=卸载状态
StatusUninstalling=正在卸载 %1…

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=正在安装 %1。
ShutdownBlockReasonUninstallingApp=正在卸载 %1。

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 版本 %2
AdditionalIcons=附加图标：
CreateDesktopIcon=创建桌面图标(&D)
CreateQuickLaunchIcon=创建快速启动栏图标(&Q)
ProgramOnTheWeb=%1 网站
UninstallProgram=卸载 %1
LaunchProgram=运行 %1
AssocFileExtension=将 %1 与 %2 文件扩展名关联(&A)
AssocingFileExtension=正在将 %1 与 %2 文件扩展名关联…
AutoStartProgramGroupDescription=启动：
AutoStartProgram=自动启动 %1
AddonHostProgramNotFound=在你选择的文件夹中找不到 %1。%n%n是否仍然继续？
