; *** Inno Setup version 6.4.0+ arabic messages ***
; Arabic translation  Translated by nacer baaziz (nacerstile@gmail.com)
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
LanguageName=العربية
LanguageID=$0401
LanguageCodePage=0
RightToLeft=yes
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
SetupAppTitle=تثبيت
SetupWindowTitle=تثبيت - %1
UninstallAppTitle=إزالة التثبيت
UninstallAppFullTitle=إزالة تثبيت %1

; *** Misc. common
InformationTitle=معلومات
ConfirmTitle=تأكيد
ErrorTitle=خطأ

; *** SetupLdr messages
SetupLdrStartupMessage=هذا المعالج سيقوم بتثبيت %1. هل تريد المتابعة?
LdrCannotCreateTemp=تعذر إنشاء الملفات المؤقتة, تم فشل معالج التثبيت.
LdrCannotExecTemp=تعذر تشغيل الملفات من المجلد المؤقت. فشل معالج التثبيت.
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%n خطأ %2: %3
SetupFileMissing=الملف %1 مفقود من دليل التثبيت. الرجاء تصحيح المشكلة أو الحصول على نسخة جديدة من البرنامج.
SetupFileCorrupt=ملفات الإعداد تالفة. الرجاء الحصول على نسخة جديدة من البرنامج.
SetupFileCorruptOrWrongVer=ملفات الإعداد تالفة أو غير متوافقة مع هذا الإصدار من برنامج الإعداد. الرجاء تصحيح المشكلة أو الحصول على نسخة جديدة من البرنامج.
InvalidParameter=تم تمرير أوامر غير صالحة إلى سطر الأوامر : %n%n%1
SetupAlreadyRunning=برنامج الإعداد قيد التشغيل بالفعل.
WindowsVersionNotSupported=لا يدعم هذا البرنامج إصدار Windows الذي يعمل به الكمبيوتر.
WindowsServicePackRequired=هذا البرنامج يتطلب %1 حزمة الخدمة %2 أو أعلى.
NotOnThisPlatform=لن يتم تشغيل هذا البرنامج على %1.
OnlyOnThisPlatform=يجب تشغيل هذا البرنامج على %1.
OnlyOnTheseArchitectures=يمكن تثبيت هذا البرنامج فقط على إصدارات Windows المصممة لهندسة المعالج التالية : %n%n%1
WinVersionTooLowError=هذا البرنامج يتطلب %1 الإصدار %2 أو أعلى.
WinVersionTooHighError=لا يمكن تثبيت هذا البرنامج على %1 الإصدار %2 أو أعلى.
AdminPrivilegesRequired=يجب أن يتم تسجيل دخولك كمسؤول عند تثبيت هذا البرنامج.
PowerUserPrivilegesRequired=يجب أن يتم تسجيل دخولك كمسؤول أو أن تكون عضو في مجموعة المستخدمين المميزين عند تثبيت هذا البرنامج.
SetupAppRunningError=لقد كشف معالج الإعداد أن  %1 يعمل بالفعل. %n%n يرجى إغلاق كل أجزائه الآن , ثم إضغط حسنا للمتابعة أو إلغاء الأمر للخروج.
UninstallAppRunningError=كشف معالج إلغاء التثبيت بأن %1 يعمل بالفعل.%n%n يرجى إغلاق كل أجزائه الآن , ثم إضغط حسنا للمتابعة أو إلغاء الأمر للخروج.

; *** Startup questions
PrivilegesRequiredOverrideTitle=تحديد وضع التثبيت
PrivilegesRequiredOverrideInstruction=تحديد وضع التثبيت
PrivilegesRequiredOverrideText1=يمكن ل %1 أن يُثَبَّت عند جميع المستخدمين (يتطلب إمتيازات المسؤول), أو في حسابك  فقط.
PrivilegesRequiredOverrideText2=يمكنك تثبيت %1  في حسابك فقط ، أو عند جميع المستخدمين (يتطلب إمتيازات المسؤول).
PrivilegesRequiredOverrideAllUsers=التثبيت ل&كافة المستخدمين
PrivilegesRequiredOverrideAllUsersRecommended=تثبيت ل&كافة المستخدمين (مستحسن)
PrivilegesRequiredOverrideCurrentUser=تثبيت في حسابي &فقط
PrivilegesRequiredOverrideCurrentUserRecommended=تثبيت في حسابي &فقط (مستحسن)

; *** Misc. errors
ErrorCreatingDir=تعذر على برنامج الإعداد إنشاء المجلد "%1"
ErrorTooManyFilesInDir=تعذر إنشاء ملف في المجلد  "%1" لأنه يحتوي على ملفات كثيرة جداً

; *** Setup common messages
ExitSetupTitle=الخروج من معالج التثبيت
ExitSetupMessage=لم يكتمل الإعداد. إذا قمت بالخروج الآن، لن يتم تثبيت البرنامج.%n%n يمكنك تشغيل برنامج الإعداد مرة أخرى في وقت آخر لإكمال التثبيت.%n%n إنهاء الإعداد؟
AboutSetupMenuItem=&حول الإعداد...
AboutSetupTitle=حول برنامج الإعداد
AboutSetupMessage=%1 الإصدار %2%n%3%n%n%1 صفحة الأنترنت:%n%4
AboutSetupNote=
TranslatorNote=تم ترجمة المعالج إلى اللغة العربية بواسطة ناصر بعزيز

; *** Buttons
ButtonBack=< ال&سابق
ButtonNext=ال&تالي >
ButtonInstall=&تثبيت
ButtonOK=&حسنا
ButtonCancel=إل&غاء الأمر
ButtonYes=&نعم
ButtonYesToAll=نعم لل&كل
ButtonNo=&لا
ButtonNoToAll=لا &للكل
ButtonFinish=إ&نهاء
ButtonBrowse=اس&تعراض...
ButtonWizardBrowse=اس&تعراض...
ButtonNewFolder=إنشاء مجلد &جديد

; *** "Select Language" dialog messages
SelectLanguageTitle=إختر لغة معالج التثبيت
SelectLanguageLabel=حدد اللغة التي يجب استخدامها أثناء التثبيت.

; *** Common wizard text
ClickNext=اضغط على  التالي للمتابعة، أو إلغاء الأمر لإنهاء عملية التثبيت.
BeveledLabel=
BrowseDialogTitle=تصفح لاختيار مجلد
BrowseDialogLabel=حدد مجلدًا في القائمة أدناه، ثم انقر فوق حسنا.
NewFolderName=مجلد جديد

; *** "Welcome" wizard page
WelcomeLabel1=مرحبا بكم في معالج تثبيت [name]
WelcomeLabel2=هذا المعالج سيقوم بتثبيت  [name/ver] على جهازك. %n%nمن المستحسن أن تقوم بإغلاق كافة التطبيقات الأخرى قبل المتابعة.

; *** "Password" wizard page
WizardPassword=كلمة المرور
PasswordLabel1=هذا التثبيت محمي بكلمة مرور.
PasswordLabel3=الرجاء تقديم كلمة المرور، ثم اضغط على التالي للمتابعة. كلمات المرور حساسة لحالة الأحرف.
PasswordEditLabel=&كلمة المرور:
IncorrectPassword=كلمة المرور التي أدخلتها غير صحيحة. يرجى إعادة المحاولة.

; *** "License Agreement" wizard page
WizardLicense=اتفاقية الترخيص
LicenseLabel=يرجى قراءة المعلومات الهامة التالية قبل المتابعة.
LicenseLabel3=الرجاء قراءة اتفاقية الترخيص التالية. يجب قبول شروط هذه الاتفاقية قبل متابعة التثبيت.
LicenseAccepted=قرأت وأ&وافق على كل ما ذكر في هذه الإتفاقية
LicenseNotAccepted=أنا &لا أوافق على هذه الإتفاقية

; *** "Information" wizard pages
WizardInfoBefore=معلومات
InfoBeforeLabel=يرجى قراءة المعلومات الهامة التالية قبل المتابعة.
InfoBeforeClickLabel=عندما تكون جاهزًا للاستمرار في عملية التثبيت اضغط على  التالي.
WizardInfoAfter=معلومات
InfoAfterLabel=يرجى قراءة المعلومات الهامة التالية قبل المتابعة.
InfoAfterClickLabel=عندما تكون جاهزًا للاستمرار في عملية  التثبيت اضغط على  التالي.

; *** "User Information" wizard page
WizardUserInfo=معلومات المستخدم
UserInfoDesc=يرجى إدخال معلوماتك.
UserInfoName=إسم ال&مستخدم :
UserInfoOrg=المن&ظمة:
UserInfoSerial=&الرقم التسلسلي:
UserInfoNameRequired=يجب إدخال إسم.

; *** "Select Destination Location" wizard page
WizardSelectDir=تحديد موقع الوِجْهة
SelectDirDesc=أين يجب تثبيت [name]؟
SelectDirLabel3=سيقوم معالج التثبيت بتثبيت  [name] في المجلد التالي.
SelectDirBrowseLabel=للمتابعة، انقر فوق التالي. إذا كنت ترغب في تحديد مجلد آخر، انقر فوق استعراض.
DiskSpaceGBLabel=تحتاج على الأقل [gb] GB من المساحة لتثبيت البرنامج. 
DiskSpaceMBLabel=تحتاج على الأقل [mb] MB من المساحة لتثبيت البرنامج.
CannotInstallToNetworkDrive=يتعذر على برنامج الإعداد التثبيت على محرك أقراص من خلال الشبكة.
CannotInstallToUNCPath=يتعذر على برنامج الإعداد تثبيت مسار UNC.
InvalidPath=يجب إدخال مسار كامل مع حرف محرك الأقراص; على سبيل المثال: %n%nC:\APP%n%أو مسار UNC في النموذج:%n%n\\server\share
InvalidDrive=محرك الأقراص أو مشاركة UNC التي حددتها غير موجود أو غير قابل للوصول. الرجاء تحديد آخر.
DiskSpaceWarningTitle=مساحة القرص غير كافية
DiskSpaceWarning=Sيتطلب الإعداد على الأقل %1 KB من المساحة الفارغة للتثبيت، ولكن محرك الأقراص المحدد فيه فقط %2 KB متوفرة.%n%nهل تريد المتابعة على أية حال؟
DirNameTooLong=اسم المجلد أو المسار طويل جداً.
InvalidDirName=اسم المجلد غير صالح.
BadDirName32=لا يمكن لأسماء المجلدات تضمين أي من الأحرف التالية:%n%n%1
DirExistsTitle=المجلد موجود بالفعل
DirExists=المجلد:%n%n%1%n%n موجود بالفعل. هل ترغب في التثبيت على هذا المجلد على أي حال؟
DirDoesntExistTitle=المجلد غير موجود
DirDoesntExist=المجلد:%n%n%1%n%nغير موجود. هل تريد إنشاء المجلد؟

; *** "Select Components" wizard page
WizardSelectComponents=تحديد المكونات
SelectComponentsDesc=ما هي المكونات التي يجب تثبيتها؟
SelectComponentsLabel2=حدد المكونات التي تريد تثبيتها ،  وقم بتعطيل المكونات التي لا تريد تثبيتها. انقر فوق "التالي" عندما تكون مستعدًا للمتابعة.
FullInstallation=تثبيت كامل
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=تثبيت محدود
CustomInstallation=تثبيت مخصص
NoUninstallWarningTitle=مكونات موجودة
NoUninstallWarning=اكتشف برنامج الإعداد أن المكونات التالية مثبتة بالفعل على جهاز الكمبيوتر الخاص بك: %n%n%1%n%nلن يؤدي إلغاء تحديد هذه المكونات إلى إزالة تثبيتها.%n%nهل ترغب في الاستمرار على أي حال?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=الاختيار الحالي يتطلب على الأقل [gb] GB من مساحة القرص.
ComponentsDiskSpaceMBLabel=الاختيار الحالي يتطلب على الأقل [mb] MB من مساحة القرص.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=حدد المهام الإضافية
SelectTasksDesc=ما المهام الإضافية التي ينبغي تنفيذها؟
SelectTasksLabel2=حدد المهام الإضافية التي ترغب في أن يقوم الإعداد بتنفيذها أثناء تثبيت [name], ثم إضغط على التالي.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=حدد مجلد قائمة ابدأ
SelectStartMenuFolderDesc=أين يجب أن يضع الإعداد اختصارات البرنامج؟
SelectStartMenuFolderLabel3=سيقوم برنامج الإعداد بإنشاء اختصارات البرنامج في مجلد قائمة ابدأ التالية.
SelectStartMenuFolderBrowseLabel=للمتابعة، اضغط على  التالي. إذا كنت ترغب في تحديد مجلد آخر، انقر فوق استعراض.
MustEnterGroupName=يجب إدخال اسم مجلد.
GroupNameTooLong=اسم المجلد أو المسار طويل جداً.
InvalidGroupName=اسم المجلد غير صالح.
BadGroupName=لا يمكن أن يتضمن اسم المجلد أي من الأحرف التالية:%n%n%1
NoProgramGroupCheck2=&عدم إنشاء مجلد في قائمة ابدأ

; *** "Ready to Install" wizard page
WizardReady=جاهز للتثبيت
ReadyLabel1=الإعداد جاهز الآن لبدء تثبيت [name] على جهازك.
ReadyLabel2a=اضغط على  "تثبيت" لمتابعة التثبيت، أو على "السابق" إذا كنت ترغب في مراجعة أو تغيير أية إعدادات.
ReadyLabel2b=اضغط على  تثبيت للاستمرار في  عملية التثبيت.
ReadyMemoUserInfo=معلومات المستخدم:
ReadyMemoDir=مسار الوِجْهة:
ReadyMemoType=نوع الإعداد:
ReadyMemoComponents=المكونات المحددة:
ReadyMemoGroup=مجلد قائمة ابدأ:
ReadyMemoTasks=مهام إضافية:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel=تحميل الملفات الإضافية...
ButtonStopDownload=إي&قاف التحميل
StopDownload=هل أنت متأكد من أنك ترغب في إيقاف التحميل؟
ErrorDownloadAborted=تم إلغاء التحميل
ErrorDownloadFailed=فشل تحميل: %1 %2
ErrorDownloadSizeFailed=خطأ في قراءة الحجم: %1 %2
ErrorFileHash1=خطأ في قراءة الهاش الخاص بالملف: %1
ErrorFileHash2=خطأ في هاش الملف: كان من المتوقع أن يكن : %1, بينما تم إيجاد : %2
ErrorProgress=خطأ في الحصول على نسبة التقدم: %1 من %2
ErrorFileSize=خطأ في حجم الملف: المتوقع هو : %1, الحجم الذي وجدناه هو : %2

; *** TExtractionWizardPage wizard page and Extract7ZipArchive
ExtractionLabel=جاري فك ضغط الملفات الإضافية...
ButtonStopExtraction=إي&قاف فك الضغط
StopExtraction=هل حقا ترغب في إيقاف فك الضغط?
ErrorExtractionAborted=تم إيقاف عملية فك الضغط
ErrorExtractionFailed=فشل فك الضغط : %1

; *** "Preparing to Install" wizard page
WizardPreparing=التحضير للتثبيت
PreparingDesc=الإعداد يستعد لتثبيت [name] على جهازك.
PreviousInstallNotCompleted=لم يكتمل التثبيت / إزالة البرنامج السابق. ستحتاج إلى إعادة تشغيل الكمبيوتر لإكمال هذا التثبيت.%n%nبعد إعادة تشغيل جهاز الكمبيوتر الخاص بك ، شغّل برنامج الإعداد مرة أخرى لإكمال تثبيت [name].
CannotContinue=لا يمكن لبرنامج الإعداد المتابعة. يرجى الضغط على  "إلغاء" للخروج.
ApplicationsFound=تستخدم التطبيقات التالية الملفات التي تحتاج إلى تحديث بواسطة برنامج الإعداد. يوصى بالسماح لبرنامج الإعداد بإغلاق هذه التطبيقات تلقائيًا.
ApplicationsFound2=تستخدم التطبيقات التالية الملفات التي تحتاج إلى تحديث بواسطة برنامج الإعداد. يوصى بالسماح لبرنامج الإعداد بإغلاق هذه التطبيقات تلقائيًا. بعد اكتمال التثبيت ، سيحاول برنامج الإعداد إعادة تشغيل التطبيقات.
CloseApplications=أغلق التطبيقات &تلقائيًا
DontCloseApplications=&لا تغلق التطبيقات
ErrorCloseApplications=لم يتمكن الإعداد من إغلاق جميع التطبيقات تلقائيًا. يوصى بإغلاق جميع التطبيقات التي تستخدم الملفات التي تحتاج إلى تحديث بواسطة برنامج الإعداد قبل المتابعة.
PrepareToInstallNeedsRestart=برنامج الإعداد يجب أن يقوم بإعادة تشغيل الجهاز. بعد إعادة تشغيل جهازك, قم بتشغيل برنامج الإعداد مرة أخرى لإكمال تثبيت [name].%n%nهل ترغب في إعادة التشغيل الآن?

; *** "Installing" wizard page
WizardInstalling=جاري التثبيت
InstallingLabel=يرجى الانتظار ريثما  يقوم برنامج الإعداد بتثبي [name] على جهازك.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=إنهاء معالج تثبيت [name]
FinishedLabelNoIcons=إكتمل معالج التثبيت من تثبيت [name] على جهازك.
FinishedLabel=اكتمل معالج التثبيت من تثبيت [name] على جهازك. قد يتم تشغيل التطبيق عن طريق تحديد الاختصارات المثبتة.
ClickFinish=إضغط على إنهاء للخروج من معالج التثبيت
FinishedRestartLabel=لاستكمال تثبيت [name], يجب على برنامج الإعداد إعادة تشغيل جهاز الكمبيوتر الخاص بك. هل ترغب في إعادة التشغيل الآن؟
FinishedRestartMessage=لاستكمال تثبيت [name], يجب على برنامج الإعداد إعادة تشغيل جهاز الكمبيوتر الخاص بك.%n%nهل ترغب في إعادة التشغيل الآن؟
ShowReadmeCheck=نعم ، أرغب في عرض ملف إقرأني
YesRadio=&نعم أعد تشغيل الكومبيوتر الآن
NoRadio=&لا ، سأعيد تشغيل الكمبيوتر لاحقًا
; used for example as 'Run MyProg.exe'
RunEntryExec=تشغيل %1
; used for example as 'View Readme.txt'
RunEntryShellExec=عرض %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=يحتاج برنامج الإعداد إلى القرص التالي
SelectDiskLabel2=الرجاء إدراج القرص %1 وانقر فوق حسنا.%n%nإذا كان يمكن العثور على الملفات الموجودة على هذا القرص في مجلد غير الذي يظهر أدناه، أدخل المسار الصحيح أو انقر فوق استعراض.
PathLabel=&مسار :
FileNotInDir2=لم نتمكن من العثور على الملف "%1" في "%2". الرجاء إدراج القرص الصحيح أو تحديد مجلد آخر.
SelectDirectoryLabel=الرجاء تحديد موقع القرص التالي.

; *** Installation phase messages
SetupAborted=لم يتم إكمال التثبيت. %n%nالرجاء تصحيح المشكلة وتشغيل الإعداد مرة أخرى.
AbortRetryIgnoreSelectAction=حدد إجراء
AbortRetryIgnoreRetry=&حاول مجددا
AbortRetryIgnoreIgnore=&تجاهل الخطأ والمتابعة
AbortRetryIgnoreCancel=إلغاء التثبيت

; *** Installation status messages
StatusClosingApplications=جاري إغلاق التطبيقات...
StatusCreateDirs=جاري إنشاء المجلدات...
StatusExtractFiles=يتم استخراج الملفات...
StatusCreateIcons=يجري إنشاء الإختصارات...
StatusCreateIniEntries=يتم إنشاء مدخلات INI...
StatusCreateRegistryEntries=يجري الآن إنشاء مفاتيح السجل...
StatusRegisterFiles=يتم الآن تسجيل الملفات...
StatusSavingUninstall=جاري تسجيل معلومات إزالة التثبيت...
StatusRunProgram=جاري الإنتهاء من التثبيت...
StatusRestartingApplications=يتم إعادة تشغيل التطبيقات...
StatusRollback=جاري التراجع عن التغييرات...

; *** Misc. errors
ErrorInternal2=خطأ داخلي: %1
ErrorFunctionFailedNoCode=فشل %1
ErrorFunctionFailed=فشل %1; رقم الخطء %2
ErrorFunctionFailedWithMessage=فشل %1; رقم الخطء %2.%n%3
ErrorExecutingProgram=الإعداد غير قابل على تشغيل الملف:%n%1

; *** Registry errors
ErrorRegOpenKey=خطأ في فتح مفتاح التسجيل:%n%1\%2
ErrorRegCreateKey=خطأ في إنشاء مفتاح التسجيل:%n%1\%2
ErrorRegWriteKey=خطأ في الكتابة على مفتاح التسجيل:%n%1\%2

; *** INI errors
ErrorIniEntry=حدث خطأ في إنشاء إدخال INI في الملف "%1".

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=&تخطي هذا الملف (غير مستحسن)
FileAbortRetryIgnoreIgnoreNotRecommended=&تجاهل الخطأ والمتابعة (غير مستحسن)
SourceIsCorrupted=الملف المصدر تالف
SourceDoesntExist=الملف "%1"غير موجود
ExistingFileReadOnly2=تعذر استبدال الملف الموجود لأنه تم تحديده للقراءة فقط.
ExistingFileReadOnlyRetry=&أزل وضع القراءة فقط عن الملفات ثم حاول مرة أخرى
ExistingFileReadOnlyKeepExisting=&إحتفظ بالملفات الموجودة
ErrorReadingExistingDest=حدث خطأ أثناء محاولة قراءة الملف الموجود:
FileExistsSelectAction=اختر إجراء
FileExists2=الملف موجود بالفعل.
FileExistsOverwriteExisting=&استبدال الملف الموجود
FileExistsKeepExisting=ا&بقاء الملف الموجود
FileExistsOverwriteOrKeepAll=ا&فعل هذا للنزاعات القادمة
ExistingFileNewerSelectAction=اختر إجراء
ExistingFileNewer2=الملف الموجود أحدث من الملف الذي سيقوم معالج الإعداد بتثبيته.
ExistingFileNewerOverwriteExisting=&&استبدال الملف الموجود
ExistingFileNewerKeepExisting=ال&ابقاء على الملف الموجود (مستحسن)
ExistingFileNewerOverwriteOrKeepAll=ا&فعل هذا مع النزاعات القادمة
ErrorChangingAttr=حدث خطأ أثناء محاولة تغيير سمات الملف الموجود:
ErrorCreatingTemp=حدث خطأ أثناء محاولة إنشاء ملف في الدليل الوجهة:
ErrorReadingSource=حدث خطأ أثناء محاولة قراءة ملف مصدر:
ErrorCopying=حدث خطأ أثناء محاولة نسخ ملف:
ErrorReplacingExistingFile=حدث خطأ أثناء محاولة استبدال الملف الموجود:
ErrorRestartReplace=فشل إعادة تشغيل "استبدال":
ErrorRenamingTemp=حدث خطأ أثناء محاولة إعادة تسمية ملف في الدليل الوجهة:
ErrorRegisterServer=تعذر تسجيل ملفات DLL/OCX: %1
ErrorRegSvr32Failed=فشل RegSvr32 مع رمز الخروج %1
ErrorRegisterTypeLib=الإعداد غير قادر على تسجيل مكتبة النوع: %1

; *** Uninstall display name markings
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32-bit
UninstallDisplayNameMark64Bit=64-bit
UninstallDisplayNameMarkAllUsers=كافة المستخدمين
UninstallDisplayNameMarkCurrentUser=المستخدم الحالي

; *** Post-installation errors
ErrorOpeningReadme=حدث خطأ أثناء محاولة فتح ملف إقرأني.
ErrorRestartingComputer=لم يتمكن برنامج الإعداد من إعادة تشغيل الكمبيوتر. الرجاء القيام بذلك يدوياً.

; *** Uninstaller messages
UninstallNotFound=الملف "%1" غير موجود. لا يمكن إزالة التثبيت.
UninstallOpenError=تعذر فتح "%1". لا يمكن إزالة التثبيت.
UninstallUnsupportedVer=ملف سجل الإزالة "%1" في تنسيق غير معروف من قبل هذا الإصدار من برنامج إلغاء التثبيت. لا يمكن إزالة التثبيت
UninstallUnknownEntry=إدخال غير معروف (%1) تمت مصادفته في سجل إلغاء التثبيت
ConfirmUninstall=هل أنت متأكد من أنك تريد إزالة %1 تماما, وجميع مكوناته?
UninstallOnlyOnWin64=يمكن إلغاء تثبيت هذا التثبيت على Windows 64-بت فقط.
OnlyAdminCanUninstall=يمكن إلغاء تثبيت هذا البرنامج فقط من قبل مستخدم له امتيازات إدارية.
UninstallStatusLabel=يرجى الإنتظار ريثما يتم إزالة تثبيت %1 من جهازك.
UninstalledAll=تم إزالة %1 تماما من جهازك بنجاح.
UninstalledMost=اكتمل إزالة %1.%n%nتعذر إزالة بعض العناصر. يمكن إزالتها يدوياً.
UninstalledAndNeedsRestart=لإكمال إلغاء تثبيت %1, يجب إعادة تشغيل الكمبيوتر.%n%nهل تريد إعادة تشغيل الآن؟
UninstallDataCorrupted=الملف "%1" تالف. لا يمكن إزالة التثبيت

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=إزالة ملف مشترك؟
ConfirmDeleteSharedFile2=يشير النظام إلى أن الملف المشترك التالي لم يعد في الاستخدام من قبل أي برامج. هل ترغب في أن يقوم برنامج الإعداد  بإزالة هذا الملف المشترك?%n%nإذا كانت أية برامج لا تزال تستخدم هذا الملف وتتم إزالته، قد لا تعمل هذه البرامج بشكل صحيح. إذا كنت غير متأكد، اختر لا. ترك الملف على النظام الخاص بك لن يسبب أي ضرر.
SharedFileNameLabel=اسم الملف:
SharedFileLocationLabel=الموقع :
WizardUninstalling=حالة إزالة التثبيت
StatusUninstalling=جاري إزالة تثبيت %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=جاري تثبيت %1.
ShutdownBlockReasonUninstallingApp=جاري إزالة تثبيت %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 الإصدار %2
AdditionalIcons=اختصارات إضافية:
CreateDesktopIcon=إنشاء اختصار في سطح المكتب
CreateQuickLaunchIcon=إنشاء اختصار "التشغيل السريع"
ProgramOnTheWeb=%1 على الأنترنت
UninstallProgram=إزالة تثبيت %1
LaunchProgram=تشغيل %1
AssocFileExtension=ربط %1 مع صيغة ملف %2
AssocingFileExtension=جاري ربط %1 مع صيغة ملف %2
AutoStartProgramGroupDescription=بدأ التشغيل:
AutoStartProgram=تشغيل %1 تلقائيا
AddonHostProgramNotFound= تعذر العثور على %1 في الموقع الذي إخترته.%n%nهل تريد المتابعة على أية حال؟
