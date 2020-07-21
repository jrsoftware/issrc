; *** Inno Setup version 5.1.11+ Simplified Chinese messages ***

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

LanguageName=Simplified Chinese

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

SetupAppTitle=安装

SetupWindowTitle=安装 - %1

UninstallAppTitle=反安装

UninstallAppFullTitle=%1 反安装

; *** Misc. common

InformationTitle=安装信息

ConfirmTitle=提示

ErrorTitle=错误

; *** SetupLdr messages

SetupLdrStartupMessage=现在将安装 %1。是否继续?

LdrCannotCreateTemp=不能创建临时文件。安装中止！

LdrCannotExecTemp=不能在临时目录中解压文件，安装过程中止！

; *** Startup error messages

LastErrorMessage=%1.%n%n 错误 %2: %3

SetupFileMissing=在安装目录中找不到文件 %1 。请更正该问题或者获得一个新的文件。

SetupFileCorrupt=安装文件被占用。请获得一个新的安装文件。

SetupFileCorruptOrWrongVer=安装文件被占用, 或者安装文件的版本不对。请更正该问题或者获得一个新文件。

NotOnThisPlatform=该程序不能在 %1 下运行。

OnlyOnThisPlatform=该程序必须在 %1 下运行。

OnlyOnTheseArchitectures=该程序只能在以下WINDOWS版本下运行:%n%n%1

MissingWOW64APIs=该版本需要64位的安装程序。要更正该问题，请安装 Service Pack %1。

WinVersionTooLowError=该软件需要 %1 版本号 %2 或者更高。

WinVersionTooHighError=该软件不能在 %1 版本号 %2 或者更高版本中安装。

AdminPrivilegesRequired=您在安装该程序时必须是管理员权限。

PowerUserPrivilegesRequired=您在安装程序是必须是管理员权限或者高权限用户权限。

SetupAppRunningError=安装程序检测到 %1 正在运行。%n%n请关闭它的所有实例。然后单击“确认”继续安装，或者单击“放弃”退出。UninstallAppRunningError=反安装程序检测到 %1 正在运行。%n%n请关闭它的所有实例。然后单击“确认”继续安装，或者单击“放弃”退出。

UninstallAppRunningError=Uninstall has detected that %1 is currently running.%n%nPlease close all instances of it now, then click OK to continue, or Cancel to exit.

; *** Misc. errors

ErrorCreatingDir=安装程序不能创建目录 "%1"

ErrorTooManyFilesInDir=不能在目录"%1" 下创建文件。因为该目录下有太多的文件了。

; *** Setup common messages

ExitSetupTitle=退出安装

ExitSetupMessage=安装没有完成。 如果您现在退出，程序将不被安装。%n%n您可以下次在完成安装任务。%n%n退出安装?

AboutSetupMenuItem=关于安装程序[&A]...

AboutSetupTitle=关于安装程序

AboutSetupMessage=%1 版本 %2%n%3%n%n%1 主页:%n%4

AboutSetupNote=

TranslatorNote=

; *** Buttons

ButtonBack=< 回退[&B]

ButtonNext=下一步[&N] >

ButtonInstall=安装[&I]

ButtonOK=确认

ButtonCancel=放弃

ButtonYes=市[&Y]

ButtonYesToAll=全部确认[&A]

ButtonNo=否[&N]

ButtonNoToAll=全部否认[&o]

ButtonFinish=完成[&F]

ButtonBrowse=浏览[&B]...

ButtonWizardBrowse=浏览[&r]...

ButtonNewFolder=新建文件夹[&M]

; *** "Select Language" dialog messages

SelectLanguageTitle=选择安装语言种类

SelectLanguageLabel=选择该语言作为安装语言:

; *** Common wizard text

ClickNext=单击“下一步”继续，或者单击“放弃”退出本安装程序。

BeveledLabel=

BrowseDialogTitle=浏览文件夹

BrowseDialogLabel=在下面的列表中选择一个目录, 然后单击“确认”继续。

NewFolderName=新文件夹

; *** "Welcome" wizard page

WelcomeLabel1=欢迎进入 [name] 安装向导

WelcomeLabel2=将在您的电脑上安装 [name/ver] 。%n%n建议在继续安装之前退出其它程序的运行。

; *** "Password" wizard page

WizardPassword=密码

PasswordLabel1=本安装程序受密码保护。

PasswordLabel3=请输入密码，密码区分大小写。然后单击“下一步”继续。

PasswordEditLabel=密码[&P]:

IncorrectPassword=输入的密码不正确，请重新输入。

; *** "License Agreement" wizard page

WizardLicense=用户许可协议

LicenseLabel=请在继续之前仔细阅读用户许可协议。

LicenseLabel3=请仔细阅读下面的用户许可协议。在继续安装之前，您必须接受该协议。

LicenseAccepted=我接受协议[&a]。

LicenseNotAccepted=我不接受协议[&d]

; *** "Information" wizard pages

WizardInfoBefore=提示

InfoBeforeLabel=在继续安装之前请仔细阅读以下重要的信息。

InfoBeforeClickLabel=当您准备好继续安装时，单击“继续”。

WizardInfoAfter=信息

InfoAfterLabel=在继续安装之前请仔细阅读以下重要的信息。

InfoAfterClickLabel=当您准备好继续安装时，单击“继续”。

; *** "User Information" wizard page

WizardUserInfo=用户信息

UserInfoDesc=请输入您的信息。

UserInfoName=用户名[&U]:

UserInfoOrg=组织[&O]:

UserInfoSerial=序列号[&S]:

UserInfoNameRequired=您必须输入一个名称。

; *** "Select Destination Location" wizard page

WizardSelectDir=选择目标位置

SelectDirDesc=您将把[name]安装在哪里?

SelectDirLabel3=安装程序将把[name]安装在下面的的文件夹中。

SelectDirBrowseLabel=为了继续安装, 请单击“下一步”。如果您想选择一个不同的目录，请单击“浏览”。

DiskSpaceMBLabel=为了安装本软件，至少需要 [mb] MB 的空闲磁盘空间。

ToUNCPathname=安装程序不能安装到一个 UNC 目录名称。如果您试图网上安装本程序，请映射网络驱动器。

InvalidPath=您比如输入一个含盘符的路径名称。比如:%n%nC:\APP%n%n 或者一个网络路径，例如:%n%n\\server\share

InvalidDrive=您选择的磁盘或者网络路径不存在或者不能访问。请重新选择其它的磁盘或者网络路径。

DiskSpaceWarningTitle=磁盘空间不足。

DiskSpaceWarning=安装程序至少需要 %1 KB 空闲磁盘空间来安装本软件。但是选择的磁盘上只有 %2 KB 空间可用。%n%n 您确认继续？

DirNameTooLong=该文件夹的名称太长。

InvalidDirName=该文件夹的名字太长。

BadDirName32=文件夹名称中不能包括以下的任何字符:%n%n%1

DirExistsTitle=文件夹已经存在

DirExists=文件夹:%n%n%1%n%n已经存在。您想继续安装?

DirDoesntExistTitle=文件夹不存在

DirDoesntExist=文件夹:%n%n%1%n%n不存在。您想创建该文件夹?

; *** "Select Components" wizard page

WizardSelectComponents=选择部件

SelectComponentsDesc=哪些部件需要安装?

SelectComponentsLabel2=选择您想要安装胡部件; 清除您不想安装的部件。当您准备好后，请单击“下一步” 。

FullInstallation=完全安装

; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)

CompactInstallation=精简安装

CustomInstallation=自定义安装

NoUninstallWarningTitle=部件存在

NoUninstallWarning=安装程序检测到以下部件已经在您的电脑中被安装了:%n%n%1%n%n不选择它们将不在您的电脑中安装它们。%n%n您想就这样继续吗?

ComponentSize1=%1 KB

ComponentSize2=%1 MB

ComponentsDiskSpaceMBLabel=当前选择项至少需要 [mb] MB 的空闲磁盘空间才能安装。

; *** "Select Additional Tasks" wizard page

WizardSelectTasks=选择一个附加任务

SelectTasksDesc=您想选择哪个附加任务?

SelectTasksLabel2=选择在安装[name]时执行的附加任务, 然后单击“下一步”。

; *** "Select Start Menu Folder" wizard page

WizardSelectProgramGroup=选择开始菜单文件夹

SelectStartMenuFolderDesc=您想在哪里放置软件的快捷方式?

SelectStartMenuFolderLabel3=安装程序将在下面的文件夹中创建软件的快捷方式。

SelectStartMenuFolderBrowseLabel=为了继续，单击“下一步”。如果您想另外选择一个文件夹，单击“浏览”。

MustEnterGroupName=您必须输入一个文件夹名称。

GroupNameTooLong=文件夹名称或者路径名称太长。

InvalidGroupName=文件夹名称非法。

BadGroupName=文件夹名字中不能包含下面的字符:%n%n%1

NoProgramGroupCheck2=不创建开始菜单中的文件夹[&D]

; *** "Ready to Install" wizard page

WizardReady=准备开始安装

ReadyLabel1=安装程序开始在您的电脑中安装[name].

ReadyLabel2a=单击“安装”开始安装本软件,或者单击“回退”修改安装设置。

ReadyLabel2b=单击“安装”开始安装本软件。

ReadyMemoUserInfo=用户信息:

ReadyMemoDir=安装目标位置:

ReadyMemoType=安装种类:

ReadyMemoComponents=选中的部件:

ReadyMemoGroup=开始菜单文件夹:

ReadyMemoTasks=附加任务:

; *** "Preparing to Install" wizard page

WizardPreparing=准备安装

PreparingDesc=安装程序准备在您的电脑中安装[name]。

PreviousInstallNotCompleted=安装/反安装一个以前的程序没有完成。您需要重新启动您的电脑来完成安装工作。%n%n当重新启动您的电脑后，请运行安装程序来完成安装[name]。

CannotContinue=安装程序不能继续执行。请单击“放弃”退出。

; *** "Installing" wizard page

WizardInstalling=安装中

InstallingLabel=安装程序正在安装[name],请等待。

; *** "Setup Completed" wizard page

FinishedHeadingLabel=[name]安装完成

FinishedLabelNoIcons=安装程序已经在您的电脑中安装了[name]。

FinishedLabel=安装程序已经在您的电脑中安装了[name]。要执行本软件，请单击安装好的本软件图标 。

ClickFinish=单击“完成”退出本安装程序.

FinishedRestartLabel=为了完成[name]的安装, 安装程序必须重新启动您的电脑。您想现在就重新启动?

FinishedRestartMessage=为了完成[name]的安装, 安装程序必须重新启动您的电脑。%n%您想现在就重新启动?

ShowReadmeCheck=是的,我想查看 README 文件

YesRadio=是的，我想重新启动计算机[&Y]

NoRadio=不，我将稍后重新启动计算机[&N]

; used for example as 'Run MyProg.exe'

RunEntryExec=运行 %1

; used for example as 'View Readme.txt'

RunEntryShellExec=查看 %1

; *** "Setup Needs the Next Disk" stuff

ChangeDiskTitle=安装程序需要下一个安装盘

SelectDiskLabel2=请插入安装盘 %1 并且单击“确认”。%n%n如果该盘不是下面显示的盘，请输入正确的路径或者单击“浏览”。

PathLabel=目录[&P]:

FileNotInDir2=在 "%2" 中没有发现文件。请插入正确的磁盘或者选择其它的文件夹。

SelectDirectoryLabel=请输入下一个磁盘中的正确位置。

; *** Installation phase messages

SetupAborted=安装没有完成。%n%n请修复错误并重新安装。

EntryAbortRetryIgnore=单击“重试”重新尝试，单击“忽略”将继续安装，或者单击“放弃”退出安装。

; *** Installation status messages

StatusCreateDirs=创建目录中...

StatusExtractFiles=解压文件中...

StatusCreateIcons=创建快捷方式中...

StatusCreateIniEntries=创建INI单元中...

StatusCreateRegistryEntries=创建注册表内容中...

StatusRegisterFiles=注册文件中...

StatusSavingUninstall=保存反安装信息中...

StatusRunProgram=正在完成安装...

StatusRollback=恢复原来修改的内容中...

; *** Misc. errors

ErrorInternal2=内部错误: %1

ErrorFunctionFailedNoCode=%1 失败

ErrorFunctionFailed=%1 失败; 代码 %2

ErrorFunctionFailedWithMessage=%1 错误; 代码 %2.%n%3

ErrorExecutingProgram=不能执行文件:%n%1

; *** Registry errors

ErrorRegOpenKey=在打开注册表键时发生错误:%n%1\%2

ErrorRegCreateKey=在创建注册表键时发生错误:%n%1\%2

ErrorRegWriteKey=在些注册表键时发生错误:%n%1\%2

; *** INI errors

ErrorIniEntry=在创建INI文件时发生错误 "%1".

; *** File copying errors

FileAbortRetryIgnore=单击“重试”再试一次，单击“忽略”忽略该文件，或者单击“放弃”退出安装程序。

FileAbortRetryIgnore2=单击“重试”再试一次，单击“忽略”忽略该文件继续安装，或者单击“放弃”退出安装程序。

SourceIsCorrupted=源文件被使用

SourceDoesntExist=源文件 "%1" 不存在

ExistingFileReadOnly=该已经存在的文件是只读属性。%n%n单击“重试”删除只读属性并重新尝试，单击“忽略”忽略该文件，或者单击“放弃”退出安装程序。

ErrorReadingExistingDest=当试图读取一个已经存在的文件时发生了错误:

FileExists=该文件已经存在。%n%n您想覆盖它吗?

ExistingFileNewer=已经存在的文件比安装程序试图安装的文件要新。建议您保留该文件。%n%n您想保留已经存在的文件吗?

ErrorChangingAttr=当试图改变一个存在的文件的属性时发生了错误:

ErrorCreatingTemp=当试图在目标目录中创建一个文件时发生了错误:

ErrorReadingSource=当试图读取一个文件时发生了错误:

ErrorCopying=当试图复制一个文件时发生了错误:

ErrorReplacingExistingFile=当试图覆盖已经存在的文件时发生错误:

ErrorRestartReplace=重新启动置换失败:

ErrorRenamingTemp=当在目标目录中重命名文件时发生错误:

ErrorRegisterServer=不能注册 DLL/OCX: %1

ErrorRegSvr32Failed=RegSvr32 执行失败, 错误码: %1

ErrorRegisterTypeLib=不能注册以下类型库: %1

; *** Post-installation errors

ErrorOpeningReadme=当打开 README 文件时发生错误。

ErrorRestartingComputer=安装程序不能重新启动电脑。请手动启动。

; *** Uninstaller messages

UninstallNotFound=文件 "%1" 不存在。不能反安装。

UninstallOpenError=文件 "%1" 不能打开。不能反安装。

UninstallUnsupportedVer=反安装记录文件 "%1" 不是基于本安装程序的版本。不能完成软件的删除工作。

UninstallUnknownEntry=一个未知的实体 (%1) 存在于反安装记录文件中。

ConfirmUninstall=您确定完全删除 %1 和所有基于它上面的部件吗?

UninstallOnlyOnWin64=该安装程序只能在64位的Windows上执行反安装工作。

OnlyAdminCanUninstall=该安装程序只能在您具有管理员权限时才能执行反安装工作。

UninstallStatusLabel= %1 正在被删除，请稍等。

UninstalledAll=%1 被成功地从您的电脑中删除。

UninstalledMost=%1 删除完成。%n%n有某些部件不能被删除。您可能需要手动删除它们。

UninstalledAndNeedsRestart=为了完成 %1 的删除工作, 您必须重新启动电脑。%n%n您想现在就重新启动吗?

UninstallDataCorrupted=文件"%1" 被占用。不能完成删除工作。

; *** Uninstallation phase messages

ConfirmDeleteSharedFileTitle=删除共享程序文件?

ConfirmDeleteSharedFile2=系统指出以下的共享程序文件将不再被使用。您是否想删除这些共享文件?%n%n如果这些文件删除后，其他程序仍然要使用它，可能其它程序的功能将受影响。如果您不能肯定，请选择“否”。让这些文件保留在系统中不会对系统造成损害。

SharedFileNameLabel=文件名:

SharedFileLocationLabel=位置:

WizardUninstalling=反安装

StatusUninstalling=反安装 %1 中...

; The custom messages below aren't used by Setup itself, but if you make

; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 版本 %2

AdditionalIcons=附加图标:

CreateDesktopIcon=创建桌面图标[&d]

CreateQuickLaunchIcon=创建快速启动图标[&Q]

ProgramOnTheWeb=%1 on the Web

UninstallProgram=反安装 %1

LaunchProgram=执行 %1

AssocFileExtension=用文件扩展名 %2 匹配[&A] %1?

AssocingFileExtension=正在用文件扩展名 %2 匹配[&A] %1 中...
