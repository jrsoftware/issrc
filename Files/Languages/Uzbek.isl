; *** Inno Setup version 6.5.0+ Uzbek messages ***
;
; Translated from English by Husanboy Qodirov, husanboy.me@gmail.com
; Based in part on the earlier Uzbek translation by Shamsiddinov Zafar
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
;   shortcut, Start menu, folder, directory, path, location, drive,
;   existing file, source file, registry, INI entries, README,
;   computer, Windows, version, administrator, all users, current user,
;   Downloading files, Extracting files, aborted, corrupted, close (applications), restart,
;   Select action, try again, anyway, at least,
;   and the button captions OK, Cancel, Yes, No, Next, Back, Install, Browse, Finish.
; - When in doubt, use Microsoft Terminology Search from:
;   https://msit.powerbi.com/view?r=eyJrIjoiODJmYjU4Y2YtM2M0ZC00YzYxLWE1YTktNzFjYmYxNTAxNjQ0IiwidCI6IjcyZjk4OGJmLTg2ZjEtNDFhZi05MWFiLTJkN2NkMDExZGI0NyIsImMiOjV9
; - Also use Microsoft's Localization Style Guide for your language, from:
;   https://learn.microsoft.com/en-us/globalization/reference/microsoft-style-guides
; Removing this section from your translation is allowed, but do not disregard it.

[LangOptions]
; The following three entries are very important. Be sure to read and
; understand the '[LangOptions] section' topic in the help file.
LanguageName=Oʻzbekcha
LanguageID=$0443
; LanguageCodePage should always be set if possible, even if this file is Unicode
; For English it's set to zero anyway because English only uses ASCII characters
LanguageCodePage=1254
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
SetupAppTitle=Oʻrnatish
SetupWindowTitle=Oʻrnatish - %1
UninstallAppTitle=Oʻchirish
UninstallAppFullTitle=%1 - Oʻchirish

; *** Misc. common
InformationTitle=Maʼlumot
ConfirmTitle=Tasdiqlash
ErrorTitle=Xato

; *** SetupLdr messages
SetupLdrStartupMessage=%1 oʻrnatiladi. Davom ettirmoqchimisiz?
LdrCannotCreateTemp=Vaqtinchalik fayl yaratib boʻlmadi. Oʻrnatish toʻxtatildi
LdrCannotExecTemp=Vaqtinchalik katalogdagi faylni ishga tushirib boʻlmadi. Oʻrnatish toʻxtatildi
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nXato %2: %3
SetupFileMissing=Oʻrnatish katalogida %1 fayli yoʻq. Iltimos, muammoni tuzating yoki dasturning yangi nusxasini oling.
SetupFileCorrupt=Oʻrnatish fayllari shikastlangan. Iltimos, dasturning yangi nusxasini oling.
SetupFileCorruptOrWrongVer=Oʻrnatish fayllari shikastlangan yoki Oʻrnatish dasturining ushbu versiyasiga mos emas. Iltimos, muammoni tuzating yoki dasturning yangi nusxasini oling.
InvalidParameter=Buyruq satrida notoʻgʻri parametr berildi:%n%n%1
SetupAlreadyRunning=Oʻrnatish dasturi allaqachon ishlamoqda.
WindowsVersionNotSupported=Bu dastur kompyuteringizda ishlayotgan Windows versiyasini qoʻllab-quvvatlamaydi.
WindowsServicePackRequired=Bu dastur uchun %1 Service Pack %2 yoki undan keyingisi talab qilinadi.
NotOnThisPlatform=Bu dastur %1 tizimida ishlamaydi.
OnlyOnThisPlatform=Bu dastur %1 tizimida ishga tushirilishi kerak.
OnlyOnTheseArchitectures=Bu dasturni faqat quyidagi protsessor arxitekturalari uchun moʻljallangan Windows versiyalariga oʻrnatish mumkin:%n%n%1
WinVersionTooLowError=Bu dastur uchun %1 %2 versiyasi yoki undan keyingisi talab qilinadi.
WinVersionTooHighError=Bu dasturni %1 %2 versiyasi yoki undan keyingisiga oʻrnatib boʻlmaydi.
AdminPrivilegesRequired=Bu dasturni oʻrnatish uchun tizimga administrator sifatida kirgan boʻlishingiz kerak.
; 'Power Users group' is an outdated term but should still be translated, not dropped or modernized
PowerUserPrivilegesRequired=Bu dasturni oʻrnatish uchun tizimga administrator yoki Tajribali foydalanuvchilar (Power Users) guruhi aʼzosi sifatida kirgan boʻlishingiz kerak.
; 'instance' may also be translated as 'copy'
SetupAppRunningError=Oʻrnatish dasturi %1 hozir ishlayotganini aniqladi.%n%nIltimos, uning barcha nusxalarini hozir yoping, soʻng davom etish uchun OK, chiqish uchun esa Bekor qilish tugmasini bosing.
UninstallAppRunningError=Oʻchirish dasturi %1 hozir ishlayotganini aniqladi.%n%nIltimos, uning barcha nusxalarini hozir yoping, soʻng davom etish uchun OK, chiqish uchun esa Bekor qilish tugmasini bosing.

; *** Startup questions
PrivilegesRequiredOverrideTitle=Oʻrnatish rejimini tanlash
PrivilegesRequiredOverrideInstruction=Oʻrnatish rejimini tanlang
PrivilegesRequiredOverrideText1=%1 barcha foydalanuvchilar uchun (administrator huquqlari talab qilinadi) yoki faqat siz uchun oʻrnatilishi mumkin.
PrivilegesRequiredOverrideText2=%1 faqat siz uchun yoki barcha foydalanuvchilar uchun (administrator huquqlari talab qilinadi) oʻrnatilishi mumkin.
PrivilegesRequiredOverrideAllUsers=&Barcha foydalanuvchilar uchun oʻrnatish
PrivilegesRequiredOverrideAllUsersRecommended=&Barcha foydalanuvchilar uchun oʻrnatish (tavsiya etiladi)
PrivilegesRequiredOverrideCurrentUser=Faqat &men uchun oʻrnatish
PrivilegesRequiredOverrideCurrentUserRecommended=Faqat &men uchun oʻrnatish (tavsiya etiladi)

; *** Misc. errors
ErrorCreatingDir=Oʻrnatish dasturi "%1" katalogini yarata olmadi
ErrorTooManyFilesInDir="%1" katalogida fayl yaratib boʻlmadi, chunki unda juda koʻp fayl bor

; *** Setup common messages
ExitSetupTitle=Oʻrnatishdan chiqish
ExitSetupMessage=Oʻrnatish tugallanmadi. Hozir chiqsangiz, dastur oʻrnatilmaydi.%n%nOʻrnatishni tugallash uchun Oʻrnatish dasturini boshqa vaqtda yana ishga tushirishingiz mumkin.%n%nOʻrnatishdan chiqilsinmi?
AboutSetupMenuItem=Oʻrnatish &haqida...
AboutSetupTitle=Oʻrnatish haqida
AboutSetupMessage=%1 %2%n%3%n%n%1 bosh sahifasi:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< &Orqaga
ButtonNext=&Keyingi >
ButtonInstall=Oʻ&rnatish
ButtonOK=OK
ButtonCancel=Bekor qilish
ButtonYes=&Ha
ButtonYesToAll=&Barchasiga ha
ButtonNo=&Yoʻq
ButtonNoToAll=Barchasiga y&oʻq
ButtonFinish=&Tugatish
ButtonBrowse=&Koʻrib chiqish...
ButtonWizardBrowse=Koʻrib &chiqish...
ButtonNewFolder=&Yangi jild yaratish

; *** "Select Language" dialog messages
SelectLanguageTitle=Oʻrnatish tilini tanlash
SelectLanguageLabel=Oʻrnatish vaqtida foydalaniladigan tilni tanlang.

; *** Common wizard text
ClickNext=Davom etish uchun Keyingi, Oʻrnatishdan chiqish uchun esa Bekor qilish tugmasini bosing.
BeveledLabel=
BrowseDialogTitle=Jildni tanlash
BrowseDialogLabel=Quyidagi roʻyxatdan jildni tanlang, soʻng OK tugmasini bosing.
NewFolderName=Yangi jild

; *** "Welcome" wizard page
WelcomeLabel1=[name] oʻrnatish ustasiga xush kelibsiz
WelcomeLabel2=Kompyuteringizga [name/ver] oʻrnatiladi.%n%nDavom etishdan oldin boshqa barcha ilovalarni yopish tavsiya etiladi.

; *** "Password" wizard page
WizardPassword=Parol
PasswordLabel1=Bu oʻrnatish parol bilan himoyalangan.
PasswordLabel3=Iltimos, parolni kiriting, soʻng davom etish uchun Keyingi tugmasini bosing. Parollarda katta-kichik harflar farqlanadi.
PasswordEditLabel=&Parol:
IncorrectPassword=Siz kiritgan parol notoʻgʻri. Iltimos, qaytadan urinib koʻring.

; *** "License Agreement" wizard page
WizardLicense=Litsenziya shartnomasi
LicenseLabel=Iltimos, davom etishdan oldin quyidagi muhim maʼlumotni oʻqib chiqing.
LicenseLabel3=Iltimos, quyidagi Litsenziya shartnomasini oʻqib chiqing. Oʻrnatishni davom ettirishdan oldin ushbu shartnoma shartlarini qabul qilishingiz kerak.
LicenseAccepted=Shartnomani &qabul qilaman
LicenseNotAccepted=Shartnomani qabul q&ilmayman

; *** "Information" wizard pages
WizardInfoBefore=Maʼlumot
InfoBeforeLabel=Iltimos, davom etishdan oldin quyidagi muhim maʼlumotni oʻqib chiqing.
InfoBeforeClickLabel=Oʻrnatishni davom ettirishga tayyor boʻlganingizda Keyingi tugmasini bosing.
WizardInfoAfter=Maʼlumot
InfoAfterLabel=Iltimos, davom etishdan oldin quyidagi muhim maʼlumotni oʻqib chiqing.
InfoAfterClickLabel=Oʻrnatishni davom ettirishga tayyor boʻlganingizda Keyingi tugmasini bosing.

; *** "User Information" wizard page
WizardUserInfo=Foydalanuvchi maʼlumotlari
UserInfoDesc=Iltimos, maʼlumotlaringizni kiriting.
UserInfoName=&Foydalanuvchi nomi:
UserInfoOrg=&Tashkilot:
UserInfoSerial=&Seriya raqami:
UserInfoNameRequired=Ism kiritishingiz kerak.

; *** "Select Destination Location" wizard page
WizardSelectDir=Oʻrnatish joyini tanlash
SelectDirDesc=[name] qayerga oʻrnatilsin?
SelectDirLabel3=Oʻrnatish dasturi [name] dasturini quyidagi jildga oʻrnatadi.
SelectDirBrowseLabel=Davom etish uchun Keyingi tugmasini bosing. Boshqa jildni tanlamoqchi boʻlsangiz, Koʻrib chiqish tugmasini bosing.
DiskSpaceGBLabel=Kamida [gb] GB boʻsh disk joyi talab qilinadi.
DiskSpaceMBLabel=Kamida [mb] MB boʻsh disk joyi talab qilinadi.
CannotInstallToNetworkDrive=Oʻrnatish dasturi tarmoq diskiga oʻrnata olmaydi.
CannotInstallToUNCPath=Oʻrnatish dasturi UNC yoʻliga oʻrnata olmaydi.
InvalidPath=Disk harfi bilan toʻliq yoʻlni kiritishingiz kerak; masalan:%n%nC:\App%n%nyoki quyidagi koʻrinishdagi UNC yoʻlini:%n%n\\server\share
InvalidDrive=Siz tanlagan disk yoki UNC umumiy resursi mavjud emas yoki unga kirib boʻlmaydi. Iltimos, boshqasini tanlang.
DiskSpaceWarningTitle=Diskda joy yetarli emas
DiskSpaceWarning=Oʻrnatish uchun kamida %1 KB boʻsh joy talab qilinadi, lekin tanlangan diskda faqat %2 KB boʻsh joy bor.%n%nBaribir davom ettirmoqchimisiz?
DirNameTooLong=Jild nomi yoki yoʻli juda uzun.
InvalidDirName=Jild nomi yaroqsiz.
BadDirName32=Jild nomlarida quyidagi belgilarning birortasi boʻlishi mumkin emas:%n%n%1
DirExistsTitle=Jild mavjud
DirExists=Quyidagi jild:%n%n%1%n%nallaqachon mavjud. Baribir shu jildga oʻrnatmoqchimisiz?
DirDoesntExistTitle=Jild mavjud emas
DirDoesntExist=Quyidagi jild:%n%n%1%n%nmavjud emas. Jild yaratilsinmi?

; *** "Select Components" wizard page
WizardSelectComponents=Komponentlarni tanlash
SelectComponentsDesc=Qaysi komponentlar oʻrnatilsin?
SelectComponentsLabel2=Oʻrnatmoqchi boʻlgan komponentlarni belgilang; oʻrnatishni istamagan komponentlardan belgini olib tashlang. Davom etishga tayyor boʻlganingizda Keyingi tugmasini bosing.
; don't translate 'Full' as 'Normal' or 'Default'
FullInstallation=Toʻliq oʻrnatish
; don't translate 'Compact' as 'Minimal' or 'Default'
CompactInstallation=Ixcham oʻrnatish
CustomInstallation=Maxsus oʻrnatish
NoUninstallWarningTitle=Komponentlar mavjud
NoUninstallWarning=Oʻrnatish dasturi quyidagi komponentlar kompyuteringizda allaqachon oʻrnatilganini aniqladi:%n%n%1%n%nBu komponentlardan belgini olib tashlash ularni oʻchirmaydi.%n%nBaribir davom ettirmoqchimisiz?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=Joriy tanlov uchun kamida [gb] GB disk joyi talab qilinadi.
ComponentsDiskSpaceMBLabel=Joriy tanlov uchun kamida [mb] MB disk joyi talab qilinadi.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Qoʻshimcha vazifalarni tanlash
SelectTasksDesc=Qaysi qoʻshimcha vazifalar bajarilsin?
SelectTasksLabel2=[name] oʻrnatilayotganda Oʻrnatish dasturi bajarishini istagan qoʻshimcha vazifalarni tanlang, soʻng Keyingi tugmasini bosing.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Start menyusi jildini tanlash
SelectStartMenuFolderDesc=Oʻrnatish dasturi dastur yorliqlarini qayerga joylashtirsin?
SelectStartMenuFolderLabel3=Oʻrnatish dasturi dastur yorliqlarini Start menyusidagi quyidagi jildda yaratadi.
SelectStartMenuFolderBrowseLabel=Davom etish uchun Keyingi tugmasini bosing. Boshqa jildni tanlamoqchi boʻlsangiz, Koʻrib chiqish tugmasini bosing.
MustEnterGroupName=Jild nomini kiritishingiz kerak.
GroupNameTooLong=Jild nomi yoki yoʻli juda uzun.
InvalidGroupName=Jild nomi yaroqsiz.
BadGroupName=Jild nomida quyidagi belgilarning birortasi boʻlishi mumkin emas:%n%n%1
NoProgramGroupCheck2=Start menyusida jild &yaratilmasin

; *** "Ready to Install" wizard page
WizardReady=Oʻrnatishga tayyor
ReadyLabel1=Oʻrnatish dasturi endi kompyuteringizga [name] dasturini oʻrnatishni boshlashga tayyor.
ReadyLabel2a=Oʻrnatishni davom ettirish uchun Oʻrnatish tugmasini bosing yoki biror sozlamani koʻrib chiqish yoki oʻzgartirish uchun Orqaga tugmasini bosing.
ReadyLabel2b=Oʻrnatishni davom ettirish uchun Oʻrnatish tugmasini bosing.
ReadyMemoUserInfo=Foydalanuvchi maʼlumotlari:
ReadyMemoDir=Oʻrnatish joyi:
ReadyMemoType=Oʻrnatish turi:
ReadyMemoComponents=Tanlangan komponentlar:
ReadyMemoGroup=Start menyusi jildi:
ReadyMemoTasks=Qoʻshimcha vazifalar:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel2=Fayllar yuklab olinmoqda...
ButtonStopDownload=Yuklab olishni &toʻxtatish
StopDownload=Yuklab olishni haqiqatan ham toʻxtatmoqchimisiz?
ErrorDownloadAborted=Yuklab olish toʻxtatildi
ErrorDownloadFailed=Yuklab olib boʻlmadi: %1 %2
ErrorDownloadSizeFailed=Hajmni aniqlab boʻlmadi: %1 %2
ErrorProgress=Yaroqsiz jarayon: %1 / %2
ErrorFileSize=Yaroqsiz fayl hajmi: %1 kutilgan edi, %2 aniqlandi

; *** TExtractionWizardPage wizard page and ExtractArchive
ExtractingLabel=Fayllar chiqarib olinmoqda...
ButtonStopExtraction=Chiqarib olishni &toʻxtatish
StopExtraction=Chiqarib olishni haqiqatan ham toʻxtatmoqchimisiz?
ErrorExtractionAborted=Chiqarib olish toʻxtatildi
ErrorExtractionFailed=Chiqarib olib boʻlmadi: %1

; *** Archive extraction failure details
ArchiveIncorrectPassword=Parol notoʻgʻri
ArchiveIsCorrupted=Arxiv shikastlangan
ArchiveUnsupportedFormat=Arxiv formati qoʻllab-quvvatlanmaydi

; *** "Preparing to Install" wizard page
WizardPreparing=Oʻrnatishga tayyorlanish
PreparingDesc=Oʻrnatish dasturi kompyuteringizga [name] dasturini oʻrnatishga tayyorlanmoqda.
PreviousInstallNotCompleted=Avvalgi dasturni oʻrnatish/oʻchirish tugallanmagan. Uni tugallash uchun kompyuteringizni qayta ishga tushirishingiz kerak boʻladi.%n%nKompyuteringiz qayta ishga tushganidan soʻng, [name] oʻrnatilishini tugallash uchun Oʻrnatish dasturini yana ishga tushiring.
CannotContinue=Oʻrnatish dasturi davom eta olmaydi. Iltimos, chiqish uchun Bekor qilish tugmasini bosing.
ApplicationsFound=Quyidagi ilovalar Oʻrnatish dasturi yangilashi kerak boʻlgan fayllardan foydalanmoqda. Oʻrnatish dasturiga bu ilovalarni avtomatik yopishga ruxsat berish tavsiya etiladi.
ApplicationsFound2=Quyidagi ilovalar Oʻrnatish dasturi yangilashi kerak boʻlgan fayllardan foydalanmoqda. Oʻrnatish dasturiga bu ilovalarni avtomatik yopishga ruxsat berish tavsiya etiladi. Oʻrnatish tugallangach, Oʻrnatish dasturi ilovalarni qayta ishga tushirishga harakat qiladi.
CloseApplications=Ilovalarni &avtomatik yopish
DontCloseApplications=Ilovalarni &yopmaslik
ErrorCloseApplications=Oʻrnatish dasturi barcha ilovalarni avtomatik yopa olmadi. Davom etishdan oldin Oʻrnatish dasturi yangilashi kerak boʻlgan fayllardan foydalanayotgan barcha ilovalarni yopish tavsiya etiladi.
PrepareToInstallNeedsRestart=Oʻrnatish dasturi kompyuteringizni qayta ishga tushirishi kerak. Kompyuteringiz qayta ishga tushganidan soʻng, [name] oʻrnatilishini tugallash uchun Oʻrnatish dasturini yana ishga tushiring.%n%nHozir qayta ishga tushirilsinmi?

; *** "Installing" wizard page
WizardInstalling=Oʻrnatilmoqda
InstallingLabel=Iltimos, Oʻrnatish dasturi kompyuteringizga [name] dasturini oʻrnatguncha kuting.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=[name] oʻrnatish ustasi yakunlanmoqda
FinishedLabelNoIcons=Oʻrnatish dasturi kompyuteringizga [name] dasturini oʻrnatishni tugatdi.
FinishedLabel=Oʻrnatish dasturi kompyuteringizga [name] dasturini oʻrnatishni tugatdi. Ilovani oʻrnatilgan yorliqlar orqali ishga tushirish mumkin.
ClickFinish=Oʻrnatishdan chiqish uchun Tugatish tugmasini bosing.
FinishedRestartLabel=[name] oʻrnatilishini tugallash uchun Oʻrnatish dasturi kompyuteringizni qayta ishga tushirishi kerak. Hozir qayta ishga tushirilsinmi?
FinishedRestartMessage=[name] oʻrnatilishini tugallash uchun Oʻrnatish dasturi kompyuteringizni qayta ishga tushirishi kerak.%n%nHozir qayta ishga tushirilsinmi?
ShowReadmeCheck=Ha, README faylini koʻrmoqchiman
YesRadio=&Ha, kompyuter hozir qayta ishga tushirilsin
NoRadio=&Yoʻq, kompyuterni keyinroq qayta ishga tushiraman
; used for example as 'Run MyProg.exe'
RunEntryExec=Ishga tushirish: %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Koʻrish: %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Oʻrnatish dasturiga keyingi disk kerak
SelectDiskLabel2=Iltimos, %1-diskni joylang va OK tugmasini bosing.%n%nAgar bu diskdagi fayllar quyida koʻrsatilgan jilddan boshqa jildda boʻlsa, toʻgʻri yoʻlni kiriting yoki Koʻrib chiqish tugmasini bosing.
PathLabel=&Yoʻl:
FileNotInDir2="%1" fayli "%2" ichidan topilmadi. Iltimos, toʻgʻri diskni joylang yoki boshqa jildni tanlang.
SelectDirectoryLabel=Iltimos, keyingi disk joylashuvini koʻrsating.

; *** Installation phase messages
SetupAborted=Oʻrnatish tugallanmadi.%n%nIltimos, muammoni tuzating va Oʻrnatish dasturini qayta ishga tushiring.
AbortRetryIgnoreSelectAction=Amalni tanlang
AbortRetryIgnoreRetry=&Qayta urinish
AbortRetryIgnoreIgnore=&Xatoga eʼtibor bermasdan davom etish
AbortRetryIgnoreCancel=Oʻrnatishni bekor qilish
RetryCancelSelectAction=Amalni tanlang
RetryCancelRetry=&Qayta urinish
RetryCancelCancel=Bekor qilish

; *** Installation status messages
StatusClosingApplications=Ilovalar yopilmoqda...
StatusCreateDirs=Kataloglar yaratilmoqda...
StatusExtractFiles=Fayllar chiqarib olinmoqda...
StatusDownloadFiles=Fayllar yuklab olinmoqda...
StatusCreateIcons=Yorliqlar yaratilmoqda...
StatusCreateIniEntries=INI yozuvlari yaratilmoqda...
StatusCreateRegistryEntries=Reyestr yozuvlari yaratilmoqda...
StatusRegisterFiles=Fayllar roʻyxatdan oʻtkazilmoqda...
StatusSavingUninstall=Oʻchirish maʼlumotlari saqlanmoqda...
StatusRunProgram=Oʻrnatish yakunlanmoqda...
StatusRestartingApplications=Ilovalar qayta ishga tushirilmoqda...
StatusRollback=Oʻzgarishlar bekor qilinmoqda...

; *** Misc. errors
ErrorInternal2=Ichki xato: %1
ErrorFunctionFailedNoCode=%1 bajarilmadi
ErrorFunctionFailed=%1 bajarilmadi; kod %2
ErrorFunctionFailedWithMessage=%1 bajarilmadi; kod %2.%n%3
ErrorExecutingProgram=Faylni ishga tushirib boʻlmadi:%n%1

; *** Registry errors
ErrorRegOpenKey=Reyestr kalitini ochishda xato:%n%1\%2
ErrorRegCreateKey=Reyestr kalitini yaratishda xato:%n%1\%2
ErrorRegWriteKey=Reyestr kalitiga yozishda xato:%n%1\%2

; *** INI errors
ErrorIniEntry="%1" faylida INI yozuvini yaratishda xato.

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=Bu faylni &oʻtkazib yuborish (tavsiya etilmaydi)
FileAbortRetryIgnoreIgnoreNotRecommended=&Xatoga eʼtibor bermasdan davom etish (tavsiya etilmaydi)
SourceIsCorrupted=Manba fayl shikastlangan
SourceDoesntExist="%1" manba fayli mavjud emas
SourceVerificationFailed=Manba faylni tekshirib boʻlmadi: %1
VerificationSignatureDoesntExist="%1" imzo fayli mavjud emas
VerificationSignatureInvalid="%1" imzo fayli yaroqsiz
VerificationKeyNotFound="%1" imzo fayli nomaʼlum kalitdan foydalanadi
VerificationFileNameIncorrect=Fayl nomi notoʻgʻri
VerificationFileTagIncorrect=Fayl tegi notoʻgʻri
VerificationFileSizeIncorrect=Fayl hajmi notoʻgʻri
VerificationFileHashIncorrect=Fayl xeshi notoʻgʻri
ExistingFileReadOnly2=Mavjud faylni almashtirib boʻlmadi, chunki u faqat oʻqish uchun deb belgilangan.
ExistingFileReadOnlyRetry=&Faqat oʻqish uchun atributini olib tashlab, qayta urinish
ExistingFileReadOnlyKeepExisting=&Mavjud faylni saqlab qolish
ErrorReadingExistingDest=Mavjud faylni oʻqishda xato yuz berdi:
FileExistsSelectAction=Amalni tanlang
FileExists2=Fayl allaqachon mavjud.
FileExistsOverwriteExisting=Mavjud fayl &ustidan yozish
FileExistsKeepExisting=&Mavjud faylni saqlab qolish
FileExistsOverwriteOrKeepAll=&Keyingi ziddiyatlarda ham shunday qilish
ExistingFileNewerSelectAction=Amalni tanlang
ExistingFileNewer2=Mavjud fayl Oʻrnatish dasturi oʻrnatmoqchi boʻlgan fayldan yangiroq.
ExistingFileNewerOverwriteExisting=Mavjud fayl &ustidan yozish
ExistingFileNewerKeepExisting=&Mavjud faylni saqlab qolish (tavsiya etiladi)
ExistingFileNewerOverwriteOrKeepAll=&Keyingi ziddiyatlarda ham shunday qilish
ErrorChangingAttr=Mavjud faylning atributlarini oʻzgartirishda xato yuz berdi:
ErrorCreatingTemp=Maqsad katalogda fayl yaratishda xato yuz berdi:
ErrorReadingSource=Manba faylni oʻqishda xato yuz berdi:
ErrorCopying=Faylni nusxalashda xato yuz berdi:
ErrorDownloading=Faylni yuklab olishda xato yuz berdi:
ErrorExtracting=Arxivni chiqarib olishda xato yuz berdi:
ErrorReplacingExistingFile=Mavjud faylni almashtirishda xato yuz berdi:
; 'RestartReplace' is an internal name, you may keep it as is
ErrorRestartReplace=RestartReplace bajarilmadi:
ErrorRenamingTemp=Maqsad katalogdagi fayl nomini oʻzgartirishda xato yuz berdi:
ErrorRegisterServer=DLL/OCX roʻyxatdan oʻtkazilmadi: %1
ErrorRegSvr32Failed=RegSvr32 %1 chiqish kodi bilan bajarilmadi
ErrorRegisterTypeLib=Turlar kutubxonasini roʻyxatdan oʻtkazib boʻlmadi: %1

; *** Uninstall display name markings
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32 bitli
UninstallDisplayNameMark64Bit=64 bitli
UninstallDisplayNameMarkAllUsers=Barcha foydalanuvchilar
UninstallDisplayNameMarkCurrentUser=Joriy foydalanuvchi

; *** Post-installation errors
ErrorOpeningReadme=README faylini ochishda xato yuz berdi.
ErrorRestartingComputer=Oʻrnatish dasturi kompyuterni qayta ishga tushira olmadi. Iltimos, buni qoʻlda bajaring.

; *** Uninstaller messages
UninstallNotFound="%1" fayli mavjud emas. Oʻchirib boʻlmaydi.
UninstallOpenError="%1" faylini ochib boʻlmadi. Oʻchirib boʻlmaydi
UninstallUnsupportedVer="%1" oʻchirish jurnali fayli Oʻchirish dasturining ushbu versiyasi tanimaydigan formatda. Oʻchirib boʻlmaydi
UninstallUnknownEntry=Oʻchirish jurnalida nomaʼlum yozuv (%1) uchradi
ConfirmUninstall=%1 va uning barcha komponentlarini haqiqatan ham butunlay oʻchirib tashlamoqchimisiz?
UninstallOnlyOnWin64=Bu oʻrnatilgan dasturni faqat 64 bitli Windowsda oʻchirish mumkin.
OnlyAdminCanUninstall=Bu oʻrnatilgan dasturni faqat administrator huquqlariga ega foydalanuvchi oʻchira oladi.
UninstallStatusLabel=Iltimos, %1 kompyuteringizdan oʻchirilguncha kuting.
UninstalledAll=%1 kompyuteringizdan muvaffaqiyatli oʻchirildi.
UninstalledMost=%1 dasturini oʻchirish yakunlandi.%n%nAyrim elementlarni oʻchirib boʻlmadi. Ularni qoʻlda oʻchirish mumkin.
UninstalledAndNeedsRestart=%1 dasturini oʻchirishni tugallash uchun kompyuteringiz qayta ishga tushirilishi kerak.%n%nHozir qayta ishga tushirilsinmi?
UninstallDataCorrupted="%1" fayli shikastlangan. Oʻchirib boʻlmaydi

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Umumiy fayl oʻchirilsinmi?
ConfirmDeleteSharedFile2=Tizim maʼlumotiga koʻra, quyidagi umumiy fayldan endi hech qaysi dastur foydalanmayapti. Oʻchirish dasturi bu umumiy faylni oʻchirsinmi?%n%nAgar biror dastur hali ham bu fayldan foydalanayotgan boʻlsa va fayl oʻchirilsa, u dasturlar toʻgʻri ishlamasligi mumkin. Ishonchingiz komil boʻlmasa, Yoʻq tugmasini tanlang. Faylni tizimingizda qoldirish hech qanday zarar keltirmaydi.
SharedFileNameLabel=Fayl nomi:
SharedFileLocationLabel=Joylashuvi:
WizardUninstalling=Oʻchirish holati
StatusUninstalling=%1 oʻchirilmoqda...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=%1 oʻrnatilmoqda.
ShutdownBlockReasonUninstallingApp=%1 oʻchirilmoqda.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 %2-versiya
AdditionalIcons=Qoʻshimcha yorliqlar:
CreateDesktopIcon=&Ish stolida yorliq yaratish
CreateQuickLaunchIcon=&Tezkor ishga tushirish panelida yorliq yaratish
ProgramOnTheWeb=%1 Internetda
UninstallProgram=%1 dasturini oʻchirish
LaunchProgram=%1 dasturini ishga tushirish
AssocFileExtension=%1 dasturini %2 fayl kengaytmasi bilan &bogʻlash
AssocingFileExtension=%1 dasturi %2 fayl kengaytmasi bilan bogʻlanmoqda...
AutoStartProgramGroupDescription=Avtomatik ishga tushirish:
AutoStartProgram=%1 dasturini avtomatik ishga tushirish
AddonHostProgramNotFound=%1 siz tanlagan jilddan topilmadi.%n%nBaribir davom ettirmoqchimisiz?
