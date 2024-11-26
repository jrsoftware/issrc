; *** Inno Setup version 6.1.0+ French messages ***
;
; To download user-contributed translations of this file, go to:
;   https://jrsoftware.org/files/istrans/
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).

; Maintained by abolfazl saeedifar (abolfazl.saeedifar80@gmail.com)

[LangOptions]
; The following three entries are very important. Be sure to read and 
; understand the '[LangOptions] section' topic in the help file.
LanguageName=Persian
LanguageID=$0429
LanguageCodePage=1256
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
SetupAppTitle=نصب‌کننده
SetupWindowTitle=نصبکنندۀ - %1
UninstallAppTitle=حذف نصب
UninstallAppFullTitle=حذفِ %1

; *** Misc. common
InformationTitle=اطلاعیه
ConfirmTitle=تأیید
ErrorTitle=خطا

; *** SetupLdr messages
SetupLdrStartupMessage=%1 نصب خواهد شد. ادامه می‌دهید?
LdrCannotCreateTemp=امکان ایجاد فایل موقت وجود ندارد. نصب انجام نشد
LdrCannotExecTemp=امکان اجرای فایل در پوشۀ موقت وجود ندارد. نصب برنامه متوقف شد
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nخطا %2: %3
SetupFileMissing=فایل %1 در پوشۀ نصب یافت نشد. لطفاً مشکل را برطرف کرده، یا نسخۀ جدیدی از برنامه را تهیه کنید.
SetupFileCorrupt=فایل‌های نصب برنامه خراب هستند. لطفاً نسخۀ جدیدی از برنامه را تهیه کنید.
SetupFileCorruptOrWrongVer=فایل‌های نصب برنامه خراب، یا با این نسخه از نصب‌کننده نا‌سازگار هستند. لطفاً مشکل را برطرف کرده، یا نسخۀ جدیدی از برنامه را تهیه کنید.
InvalidParameter=پارامتری که در خط فرمان وارد شده است، نا‌معتبر است. جزئیات خطا:%n%n%1
SetupAlreadyRunning=نصب برنامه در حال انجام است.
WindowsVersionNotSupported=این برنامه از نسخۀ ویندوزِ فعلی پشتیبانی نمی‌کند.
WindowsServicePackRequired=این برنامه نیازمند %1 سرویس پک %2 یا جدیدتر می‌باشد.
NotOnThisPlatform=این برنامه روی %1 قابل اجرا نیست.
OnlyOnThisPlatform=این برنامه صرفاً روی %1 قابل اجرا می‌باشد.
OnlyOnTheseArchitectures=نصب این برنامه صرفاً بر روی نسخه‌های ویندوز سازگار با معماری‌های پردازنده‌ی %n%n%1 امکان‌پذیر است
WinVersionTooLowError=این برنامه برای اجرا به %1 نسخۀ %2 یا نسخه‌های بالاتر نیاز دارد.
WinVersionTooHighError=نصب این برنامه بر روی %1 نسخۀ %2 یا جدیدتر امکان‌پذیر نیست.
AdminPrivilegesRequired=برای نصب این برنامه، ورود با حساب کاربری دارای مجوز‌های مدیریتی ضروری است.
PowerUserPrivilegesRequired=برای نصب این برنامه، ورود با حساب کاربری دارای مجوز‌های مدیریتی، یا عضویت در گروه کاربران قدرتمند ضروری است.
SetupAppRunningError=به نظر میرسد که %1 در حال اجرا می‌باشد.%n%nپیش از ادامه، لطفاً تمامی موارد در حال اجرای آن را بسته و سپس بر روی تأیید کلیک کنید. در صورت تمایل به انصراف، دکمه‌ی لغو را انتخاب کنید.
UninstallAppRunningError=به نظر میرسد که %1 در حال اجرا می‌باشد.%n%nپیش از ادامه، لطفاً تمامی موارد در حال اجرای آن را بسته و سپس بر روی تأیید کلیک کنید. در صورت تمایل به انصراف، دکمه‌ی لغو را انتخاب کنید.

; *** Startup questions
PrivilegesRequiredOverrideTitle=انتخاب حالت نصب
PrivilegesRequiredOverrideInstruction=حالت نصب را انتخاب کنید
PrivilegesRequiredOverrideText1=نصب %1 برای همۀ کاربران، یا فقط برای شما امکان‌پذیر است. (نیازمند امتیازات مدیریتی).
PrivilegesRequiredOverrideText2=نصب %1 فقط برای شما، یا برای همۀ کاربران امکان‌پذیر است. (نیازمند امتیازات مدیریتی).
PrivilegesRequiredOverrideAllUsers=نصب برای &همۀ کاربران
PrivilegesRequiredOverrideAllUsersRecommended=نصب برای &همۀ کاربران، (حالت توصیه‌شده)
PrivilegesRequiredOverrideCurrentUser=نصب فقط برای &خودم
PrivilegesRequiredOverrideCurrentUserRecommended=نصب فقط برای &خودم، (حالت توصیه‌شده)

; *** Misc. errors
ErrorCreatingDir=نصب‌کننده قادر به ایجاد پوشۀ "%1" نیست
ErrorTooManyFilesInDir=فضای کافی برای ایجاد فایل جدید در پوشۀ "%1" وجود ندارد

; *** Setup common messages
ExitSetupTitle=خروج از نصب‌کننده
ExitSetupMessage=فرایند نصب کامل نشده است. اگر از این صفحه خارج شوید، برنامه نصب نخواهد شد.%n%nبرای ادامه‌ی نصب، می‌توانید در زمان دیگری مجدداً نصب‌کننده را اجرا کنید.%n%nاز نصب‌کننده خارج می‌شوید?
AboutSetupMenuItem=&دربارۀ نصب‌کننده...
AboutSetupTitle=دربارۀ نصب‌کننده
AboutSetupMessage=%1 نسخۀ %2%n%3%n%n%1 وبسایت:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< &بازگشت
ButtonNext=&بعدی >
ButtonInstall=&نصب
ButtonOK=تأیید
ButtonCancel=انصراف
ButtonYes=&بله
ButtonYesToAll=بله برای &همۀ موارد
ButtonNo=&خیر
ButtonNoToAll=خ&یر برای همۀ موارد
ButtonFinish=&پایان
ButtonBrowse=&انتخاب پوشه...
ButtonWizardBrowse=ا&نتخاب پوشه...
ButtonNewFolder=&ساختن پوشۀ جدید

; *** "Select Language" dialog messages
SelectLanguageTitle=انتخاب زبان نصب‌کننده
SelectLanguageLabel=لطفاً زبان مورد استفاده در هنگام نصب را انتخاب کنید.

; *** Common wizard text
ClickNext=برای ادامۀ نصب، روی بعدی، و برای خروج از نصب‌کننده، روی لغو کلیک کنید.
BeveledLabel=
BrowseDialogTitle=انتخاب پوشه
BrowseDialogLabel=یک پوشه از لیست زیر انتخاب کنید و سپس روی تأیید کلیک کنید.
NewFolderName=پوشۀ جدید

; *** "Welcome" wizard page
WelcomeLabel1=به نصب‌کنندۀ [name] خوش‌آمدید. ترجمه شده توسط ابوالفضل سعیدیفر
WelcomeLabel2=این نصب‌کننده، برنامۀ [name/ver] را روی کامپیوتر شما نصب می‌کند.%n%nپیشنهاد می‌کنیم همۀ برنامه‌های در‌حال اجرا را ببندید.

; *** "Password" wizard page
WizardPassword=ورود رمز عبور
PasswordLabel1=این نصب‌کننده دارای رمز عبور است.
PasswordLabel3=لطفاً رمز عبور را وارد کرده و سپس برای ادامه، روی بعدی کلیک کنید. رمز عبور به حروف کوچک و بزرگ حساس است.
PasswordEditLabel=&رمز عبور:
IncorrectPassword=رمز عبور وارد شده صحیح نیست. لطفاً دوباره امتحان کنید.

; *** "License Agreement" wizard page
WizardLicense=توافقنامه
LicenseLabel=لطفاً قبل از نصب، مطالبِ مندرج در توافقنامۀ زیر را مطالعه کنید.
LicenseLabel3=لطفاً قبل از نصب، مفاد توافق‌نامه را مطالعه کنید. برای نصب، باید این توافقنامه را پذیرفته باشید.
LicenseAccepted=شرایط فوق مورد &تأیید این جانب می‌باشد
LicenseNotAccepted=شرایط فوق مورد &تأیید این جانب نمی‌باشد

; *** "Information" wizard pages
WizardInfoBefore=اطلاعات
InfoBeforeLabel=لطفاً قبل از ادامه، نکات زیر را با دقت مطالعه کنید.
InfoBeforeClickLabel=برای ادامۀ مراحل نصب، روی بعدی کلیک کنید.
WizardInfoAfter=اطلاعات
InfoAfterLabel=لطفاً قبل از ادامه، نکات زیر را با دقت مطالعه کنید.
InfoAfterClickLabel=برای ادامۀ مراحل نصب، روی بعدی کلیک کنید.

; *** "User Information" wizard page
WizardUserInfo=اطلاعات کاربر
UserInfoDesc=لطفاً اطلاعات خود را وارد کنید.
UserInfoName=&نام کاربر:
UserInfoOrg=&سازمان:
UserInfoSerial=&شماره سریال:
UserInfoNameRequired=شما باید یک نام وارد کنید.

; *** "Select Destination Location" wizard page
WizardSelectDir=تعیین مسیر نصب برنامه
SelectDirDesc=[name] در چه پوشه‌ای باید نصب شود?
SelectDirLabel3=[name] در پوشۀ زیر نصب خواهد شد.
SelectDirBrowseLabel=برای ادامه، روی بعدی کلیک کنید. برای نصب در پوشۀ دیگر روی دکمۀ انتخاب پوشه کلیک کنید.
DiskSpaceGBLabel=حداقل به [gb] گیگابایت فضای خالی نیاز است.
DiskSpaceMBLabel=حداقل به [mb] مگابایت فضای خالی نیاز است.
CannotInstallToNetworkDrive=نصب برنامه در درایو شبکه امکان‌پذیر نیست.
CannotInstallToUNCPath=نصب برنامه در مسیر UNC امکان‌پذیر نیست.
InvalidPath=مسیر را به طور کامل و با ذکر درایو مربوطه وارد کنید; مثلاً:%n%nC:\APP%n%nیا یک آدرس شبکه به شکل:%n%n\\server\share
InvalidDrive=درایو یا اشتراک شبکه‌ای که انتخاب کرده‌اید وجود ندارد یا قابل دسترسی نیست. لطفاً درایو یا اشتراک دیگری را انتخاب کنید.
DiskSpaceWarningTitle=فضای دیسک کافی نیست
DiskSpaceWarning=برای نصب برنامه، حداقل به %1 کیلوبایت فضای خالی نیاز است، در حالی که درایو انتخابی شما فقط %2 کیلوبایت فضای خالی دارد.%n%nبا این وجود، مایلید نصب را ادامه دهید?
DirNameTooLong=نام یا مسیر پوشه خیلی طولانی است.
InvalidDirName=نام پوشه معتبر نیست.
BadDirName32=برای نام‌گذاری پوشه‌ها، از این کاراکتر‌ها استفاده نکنید:%n%n%1
DirExistsTitle=پوشه تکراری است
DirExists=پوشه‌ای با نام:%n%n%1%n%nوجود دارد. مایلید نصب را در همین پوشه انجام دهید?
DirDoesntExistTitle=پوشه‌ای با این نام یافت نشد
DirDoesntExist=پوشه‌ای با نام:%n%n%1%n%nوجود ندارد. مایلید این پوشه را ایجاد کنید?

; *** "Select Components" wizard page
WizardSelectComponents=انتخاب اجزا
SelectComponentsDesc=چه بخش‌هایی از نرم‌افزار باید نصب شوند?
SelectComponentsLabel2=بخش‌هایی که می‌خواهید نصب شوند را علامت‌دار، و بخش‌هایی که نمی‌خواهید نصب شوند را بدون‌علامت کنید. برای ادامه، روی بعدی کلیک کنید.
FullInstallation=نصب به‌صورت کامل
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=نصب به‌صورت فشرده
CustomInstallation=نصب به‌صورت سفارشی
NoUninstallWarningTitle=اجزایی که برای نصب انتخاب کردید موجود هستند
NoUninstallWarning=به نظر میرسد که اجزای زیر قبلاً روی کامپیوتر شما نصب شده‌اند:%n%n%1%n%nبا لغو انتخاب این اجزا، آنها حذف نخواهند شد.%n%nبا این وجود، مایلید نصب را ادامه دهید?
ComponentSize1=%1 کیلوبایت
ComponentSize2=%1 مگابایت
ComponentsDiskSpaceGBLabel=انتخاب فعلی حداقل به [gb] گیگابایت فضای دیسک نیاز دارد.
ComponentsDiskSpaceMBLabel=انتخاب فعلی حداقل به [mb] مگابایت فضای دیسک نیاز دارد.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=انتخاب کار‌های بیشتر
SelectTasksDesc=چه کار‌های دیگری باید انجام شود?
SelectTasksLabel2=کار‌های دیگری که می‌خواهید در حین نصب [name] توسط نصب‌کننده انجام شود را انتخاب کرده، و سپس روی بعدی کلیک کنید.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=انتخاب پوشۀ منوی استارت
SelectStartMenuFolderDesc=نصب‌کننده میانبر‌های برنامه را کجا باید قرار دهد?
SelectStartMenuFolderLabel3=میانبر‌ها در پوشۀ منوی استارت زیر ایجاد خواهند شد.
SelectStartMenuFolderBrowseLabel=برای ادامه، روی بعدی کلیک کنید. اگر می‌خواهید پوشۀ دیگری را انتخاب کنید، روی انتخاب پوشه کلیک کنید.
MustEnterGroupName=باید یک نام برای پوشه وارد کنید.
GroupNameTooLong=نام یا مسیر پوشه خیلی طولانی است.
InvalidGroupName=نام پوشه معتبر نیست.
BadGroupName=برای نام‌گذاری پوشه، از این کاراکتر‌ها استفاده نکنید:%n%n%1
NoProgramGroupCheck2=&هیچ پوشه‌ای در منوی استارت ایجاد نشود

; *** "Ready to Install" wizard page
WizardReady=آماده برای نصب
ReadyLabel1=فرایند نصب [name] بر روی کامپیوتر شما آغاز خواهد شد.
ReadyLabel2a=برای آغاز نصب، روی دکمۀ نصب کلیک کنید, برای تغییر تنظیمات قبلی، روی دکمۀ بازگشت کلیک کنید.
ReadyLabel2b=برای ادامه، روی دکمۀ نصب کلیک کنید.
ReadyMemoUserInfo=اطلاعات کاربر:
ReadyMemoDir=محل ذخیره‌سازی:
ReadyMemoType=حالت نصب:
ReadyMemoComponents=اجزای انتخاب‌شده:
ReadyMemoGroup=پوشۀ منوی استارت:
ReadyMemoTasks=کار‌های بیشتر:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel=در حال دانلود فایل‌های تکمیلی...
ButtonStopDownload=&توقف دانلود
StopDownload=آیا مطمئنید که می‌خواهید دانلود را متوقف کنید?
ErrorDownloadAborted=دانلود لغو شد
ErrorDownloadFailed=دانلود انجام نشد: %1 %2
ErrorDownloadSizeFailed=خطا در دریافت حجم: %1 %2
ErrorFileHash1=خطای تطابق هش فایل: %1
ErrorFileHash2=هش فایل نا‌معتبر: مقدار مورد انتظار %1, مقدار یافت‌شده %2
ErrorProgress=وضعیت پیشرفت نا‌معتبر: %1 از %2
ErrorFileSize=حجم فایل نا‌معتبر است: مقدار مورد انتظار %1, مقدار یافت‌شده %2

; *** "Preparing to Install" wizard page
WizardPreparing=در حال آماده‌سازی برای نصب
PreparingDesc=نصب [name] در حال آماده‌سازی است.
PreviousInstallNotCompleted=نصب یا حذف برنامۀ قبلی به‌طور کامل انجام نشده است. جهت اتمام مراحل نصب، باید کامپیوتر خود را مجدداً راه‌اندازی کنید.%n%nپس از راه‌اندازی مجدد کامپیوتر، برای تکمیل نصب [name]، نصب‌کننده را دوباره اجرا کنید.
CannotContinue=عملیات نصب با مشکل مواجه شده است. برای لغو نصب و خروج، بر روی لغو کلیک کنید.
ApplicationsFound=برنامه‌های زیر در حال استفاده از فایل‌هایی هستند که باید توسط نصب‌کننده به‌روزرسانی شوند. پیشنهاد می‌کنیم که به نصب‌کننده اجازه دهید تا این برنامه‌ها را به‌طور خودکار ببندد.
ApplicationsFound2=برنامه‌های زیر در حال استفاده از فایل‌هایی هستند که باید توسط نصب‌کننده به‌روزرسانی شوند. پیشنهاد می‌کنیم که به نصب‌کننده اجازه دهید تا این برنامه‌ها را به‌طور خودکار ببندد. پس از اتمام نصب، نصب‌کننده به‌طور خودکار برنامه‌ها را دوباره اجرا خواهد کرد.
CloseApplications=&بستن خودکار برنامه‌ها
DontCloseApplications=&عدم بستن برنامه‌ها
ErrorCloseApplications=نصب‌کننده نتوانست برنامه‌های باز را ببندد. پیشنهاد می‌کنیم قبل از ادامۀ نصب، تمام برنامه‌هایی که از فایل‌های در‌حال به‌روزرسانی توسط نصب‌کننده استفاده می‌کنند را ببندید.
PrepareToInstallNeedsRestart=برای ادامۀ روند نصب، لازم است کامپیوتر شما مجدداً راه‌اندازی شود. پس از راه‌اندازی مجدد کامپیوتر، برای تکمیل نصب [name]، دوباره نصب‌کننده را اجرا کنید.%n%nمایلید هم‌اکنون کامپیوتر خود را مجدداً راه‌اندازی کنید?

; *** "Installing" wizard page
WizardInstalling=در حال نصب
InstallingLabel=لطفاً منتظر بمانید تا نصب‌کننده [name] را روی کامپیوتر شما نصب کند.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=نصبِ [name] به پایان رسید
FinishedLabelNoIcons=[name] با موفقیت روی کامپیوتر شما نصب شد.
FinishedLabel=[name] با موفقیت روی کامپیوتر شما نصب شد. برای اجرای برنامه، می‌توانید از میانبرهای نصب‌شده استفاده کنید.
ClickFinish=برای بستن این پنجره، روی پایان کلیک کنید.
FinishedRestartLabel=برای تکمیل فرایند نصب [name], کامپیوتر شما باید مجدداً راه‌اندازی شود. آیا هم‌اکنون مایل به راه‌اندازی مجدد هستید?
FinishedRestartMessage=برای تکمیل فرایند نصب [name], کامپیوتر شما باید مجدداً راه‌اندازی شود.%n%nآیا هم‌اکنون مایل به راه‌اندازی مجدد هستید?
ShowReadmeCheck=بله، مایلم فایل README را بخوانم
YesRadio=&بله، مایلم هم‌اکنون کامپیوتر را مجدداً راه‌اندازی کنم
NoRadio=&خیر، ترجیح میدهم کامپیوتر را بعدا مجدداً راه‌اندازی کنم
; used for example as 'Run MyProg.exe'
RunEntryExec=اجرای %1
; used for example as 'View Readme.txt'
RunEntryShellExec=دیدنِ %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=نصب‌کننده به دیسک بعدی نیاز دارد
SelectDiskLabel2=لطفاً دیسک %1 را وارد کرده و بر روی تأیید کلیک کنید.%n%nاگر فایل‌های این دیسک در پوشه‌ای غیر از پوشۀ زیر یافت می‌شوند، مسیر صحیح را وارد کرده، یا بر روی انتخاب پوشه کلیک کنید.
PathLabel=&مسیر:
FileNotInDir2=فایل "%1" در "%2" پیدا نشد. لطفاً دیسک صحیح را وارد کرده، یا پوشۀ دیگری را انتخاب کنید.
SelectDirectoryLabel=لطفاً محل قرارگیری دیسک بعدی را مشخص کنید.

; *** Installation phase messages
SetupAborted=برنامه نصب نشد.%n%nلطفاً مشکل را برطرف کرده و سپس دوباره مراحل نصب را انجام دهید.
AbortRetryIgnoreSelectAction=یک گزینه را از میان گزینه‌های موجود انتخاب نمایید
AbortRetryIgnoreRetry=&تلاش مجدد
AbortRetryIgnoreIgnore=&نادیده‌گرفتن خطا و ادامه
AbortRetryIgnoreCancel=انصراف از نصب

; *** Installation status messages
StatusClosingApplications=در حال بستن برنامه‌ها...
StatusCreateDirs=در حال ایجاد پوشه‌ها...
StatusExtractFiles=در حال استخراج فایل‌ها...
StatusCreateIcons=در حال ایجاد میانبر‌ها...
StatusCreateIniEntries=در حال ایجاد ورودی‌های INI...
StatusCreateRegistryEntries=در حال ایجاد کلید‌های رجیستری...
StatusRegisterFiles=در حال ثبت فایل‌ها...
StatusSavingUninstall=در حال ذخیره‌سازی اطلاعات حذف نصب...
StatusRunProgram=در حال اتمام نصب...
StatusRestartingApplications=در حال راه‌اندازی مجدد برنامه‌ها...
StatusRollback=در حال بازگردانی تغییرات...

; *** Misc. errors
ErrorInternal2=خطای داخلی: %1
ErrorFunctionFailedNoCode=%1 انجام نشد
ErrorFunctionFailed=%1 انجام نشد; کد %2
ErrorFunctionFailedWithMessage=%1 انجام نشد; کد %2.%n%3
ErrorExecutingProgram=فایل:%n%1 قابل اجرا نیست

; *** Registry errors
ErrorRegOpenKey=خطا در باز کردن کلید رجیستری:%n%1\%2
ErrorRegCreateKey=خطا در ایجاد کلید رجیستری:%n%1\%2
ErrorRegWriteKey=خطا در نوشتن در کلید رجیستری:%n%1\%2

; *** INI errors
ErrorIniEntry=خطا در ایجاد ورودی INI در فایل "%1".

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=&چشم‌پوشی از این فایل، (پیشنهاد نمی‌شود)
FileAbortRetryIgnoreIgnoreNotRecommended=&چشم‌پوشی از خطا و ادامۀ فرایند، (پیشنهاد نمی‌شود)
SourceIsCorrupted=فایل مبدأ خراب شده است
SourceDoesntExist=فایل مبدأ "%1" وجود ندارد
ExistingFileReadOnly2=به دلیل قفل بودن فایل فعلی بر روی حالت فقط خواندنی، جایگزینی آن امکان‌پذیر نیست.
ExistingFileReadOnlyRetry=&حذف‌کردن حالت فقط خواندنی از فایل و تلاش مجدد
ExistingFileReadOnlyKeepExisting=&نگه‌داشتن فایل فعلی
ErrorReadingExistingDest=خطایی در هنگام خواندن فایل فعلی رخ داده است:
FileExistsSelectAction=یک گزینه را از میان گزینه‌های موجود انتخاب نمایید
FileExists2=این فایل تکراری است.
FileExistsOverwriteExisting=&جایگزین‌کردن فایل فعلی
FileExistsKeepExisting=&نگه‌داشتن فایل فعلی
FileExistsOverwriteOrKeepAll=&انجام همین کار برای موارد بعدی
ExistingFileNewerSelectAction=یک گزینه را از میان گزینه‌های موجود انتخاب نمایید
ExistingFileNewer2=فایل فعلی جدید‌تر از فایلی است که نصب‌کننده قصد جایگزینی آن را دارد.
ExistingFileNewerOverwriteExisting=&جایگزین‌کردن فایل فعلی
ExistingFileNewerKeepExisting=&نگه‌داشتن فایل فعلی، (حالت توصیه‌شده)
ExistingFileNewerOverwriteOrKeepAll=&انجام همین کار برای موارد بعدی
ErrorChangingAttr=تغییر ویژگی‌های فایل فعلی با خطا مواجه شد:
ErrorCreatingTemp=امکان ایجاد فایل در پوشۀ مقصد وجود ندارد:
ErrorReadingSource=خواندن فایل مبدأ با خطا مواجه شد:
ErrorCopying=کپی‌کردن فایل با خطا مواجه شد:
ErrorReplacingExistingFile=جایگزینی فایل با خطا مواجه شد:
ErrorRestartReplace=امکان جایگزینی فایل در زمان راه‌اندازی مجدد وجود ندارد:
ErrorRenamingTemp=تغییر نام فایل در پوشۀ مقصد با خطا مواجه شد:
ErrorRegisterServer=امکان ثبت DLL/OCX وجود ندارد: %1
ErrorRegSvr32Failed=RegSvr32 با کد خروج %1 با خطا مواجه شد
ErrorRegisterTypeLib=امکان ثبت نوع کتابخانه وجود ندارد: %1

; *** Uninstall display name markings
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32-بیتی
UninstallDisplayNameMark64Bit=64-بیتی
UninstallDisplayNameMarkAllUsers=همۀ کاربران
UninstallDisplayNameMarkCurrentUser=کاربرِ فعلی

; *** Post-installation errors
ErrorOpeningReadme=باز‌کردن فایل README با خطا مواجه شد.
ErrorRestartingComputer=نصب‌کننده نتوانست کامپیوتر را مجدداً راه‌اندازی کند. لطفاً این کار را به‌صورت دستی انجام دهید.

; *** Uninstaller messages
UninstallNotFound=فایل "%1" وجود ندارد. حذف برنامه امکان‌پذیر نیست.
UninstallOpenError=فایل "%1" باز نشد. حذف برنامه امکان‌پذیر نیست
UninstallUnsupportedVer=فرمت فایل گزارش حذف نصب "%1" توسط این نسخه از حذف‌کننده شناسایی نشد. حذف برنامه امکان‌پذیر نیست
UninstallUnknownEntry=در فایل گزارش حذف نصب، با ورودی ناشناخته‌ای (%1) مواجه شدیم
ConfirmUninstall=آیا مطمئنید که می‌خواهید %1 و تمام اجزای آن را به‌طور کامل حذف کنید?
UninstallOnlyOnWin64=این برنامه فقط در ویندوز‌های 64-بیتی قابل حذف است.
OnlyAdminCanUninstall=حذف این برنامه فقط توسط کاربری که  مجوز‌های مدیریتی دارد امکان‌پذیر است.
UninstallStatusLabel=در حال حذف %1 , لطفاً صبر کنید.
UninstalledAll=%1 با موفقیت حذف شد.
UninstalledMost=%1 با موفقیت حذف شد.%n%nبرخی از عناصر به‌طور کامل حذف نشدند. می‌توانید آنها را به‌صورت دستی حذف کنید.
UninstalledAndNeedsRestart=برای تکمیل فرایند حذف نصب %1, کامپیوتر شما باید مجدداً راه‌اندازی شود.%n%nمایلید هم‌اکنون کامپیوتر خود را مجدداً راه‌اندازی کنید?
UninstallDataCorrupted=فایل "%1" خراب است. حذف برنامه امکان‌پذیر نیست

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=میخواهید فایل اشتراکی را حذف کنید?
ConfirmDeleteSharedFile2=سیستم نشان می‌دهد که فایل اشتراکی زیر دیگر توسط هیچ برنامه‌ای استفاده نمی‌شود:%n%nآیا می‌خواهید این فایل اشتراکی را حذف کنید? اگر در حین حذف این فایل، برنامه‌ای از آن استفاده کند، ممکن است آن برنامه به درستی کار نکند. اگر مطمئن نیستید، گزینۀ خیر را انتخاب کنید. باقی ماندن این فایل در سیستم شما هیچ مشکلی به وجود نمی‌آوَرَد.
SharedFileNameLabel=نام فایل:
SharedFileLocationLabel=محل:
WizardUninstalling=وضعیت حذف نصب
StatusUninstalling=در حال حذف %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=در حال نصب %1.
ShutdownBlockReasonUninstallingApp=در حال حذف %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 نسخۀ %2
AdditionalIcons=میانبر‌های بیشتر:
CreateDesktopIcon=ایجاد میانبر &دسکتاپ
CreateQuickLaunchIcon=ایجاد میانبر در &نوار دسترسی سریع
ProgramOnTheWeb=%1 در اینترنت
UninstallProgram=حذف نصب %1
LaunchProgram=اجرای %1
AssocFileExtension=&تنظیمِ %1 به‌عنوانِ برنامۀ پیشفرض برای باز‌کردنِ فایل‌های %2
AssocingFileExtension=در‌حالِ تنظیمِ %1 به‌عنوانِ برنامۀ پیشفرض برای باز‌کردنِ فایل‌های %2...
AutoStartProgramGroupDescription=راه‌اندازی:
AutoStartProgram=اجرای خودکار %1
AddonHostProgramNotFound=فایل %1 در پوشه‌ای که انتخاب کرده‌اید پیدا نشد.%n%nبا این وجود، مایلید ادامه دهید?
