; *** Inno Setup version 6.1.0+ Georgian ***
; Translated by Saba Khmaladze (skhmaladze@uglt.org)
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
LanguageName=Georgian
LanguageID=$0437
LanguageCodePage=0
; If the language you are translating to requires special font faces or
; sizes, uncomment any of the following entries and change them accordingly.
DialogFontName=Sylfaen
;DialogFontSize=8
WelcomeFontName=Sylfaen
;WelcomeFontSize=12
TitleFontName=Sylfaen
;TitleFontSize=29
CopyrightFontName=Sylfaen
;CopyrightFontSize=8

[Messages]

; *** Application titles
SetupAppTitle=ინსტალაცია
SetupWindowTitle=ინსტალდება - %1
UninstallAppTitle=წაშლა
UninstallAppFullTitle=იშლება %1

; *** Misc. common
InformationTitle=ინფორმაცია
ConfirmTitle=დაზუსტება
ErrorTitle=შეცდომა

; *** SetupLdr messages
SetupLdrStartupMessage=თქვენთან დაინსტალდება %1. გსურთ გაგრძელება?
LdrCannotCreateTemp=დროებითი ფაილი ვერ შეიქმნა. ინსტალაცია შეწყდა
LdrCannotExecTemp=დროებით საქაღალდეში ფაილი ვერ გაეშვა. ინსტალაცია შეწყდა
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nშეცდომა %2: %3
SetupFileMissing=საინსტალაციო საქაღალდეში არ მოიძებნა ფაილი %1. გაასწორეთ პრობლემა ან გადმოწერეთ პროგრამის ახალი ვერსია.
SetupFileCorrupt=საინსტალაციო ფაილები დაზიანებულია. გაასწორეთ პრობლემა ან გადმოწერეთ პროგრამის ახალი ვერსია.
SetupFileCorruptOrWrongVer=საინსტალაციო ფაილები დაზიანებული ან არათავსებადია ამ ვერსიასთან. გაასწორეთ პრობლემა ან გადმოწერეთ პროგრამის ახალი ვერსია.
InvalidParameter=არასწორი პარამეტრი გადაეცა ბრძანებათა ველს:%n%n%1
SetupAlreadyRunning=ინსტალაცია უკვე მიმდინარეობს.
WindowsVersionNotSupported=ეს პროგრამა Windows-ის ამ ვერსიაზე ვერ გაეშვება.
WindowsServicePackRequired=ამ პროგრამას სჭირდება %1 Service Pack %2 ან უფრო ახალი.
NotOnThisPlatform=ეს პროგრამა არ გაეშვება სისტემაზე %1.
OnlyOnThisPlatform=ეს პროგრამა უნდა გაეშვას სისტემაზე %1.
OnlyOnTheseArchitectures=ეს პროგრამა დაინსტალდება მხოლოდ Windows-ის შემდეგ არქიტექტურაზე:%n%n%1
WinVersionTooLowError=ამ პროგრამას სჭირდება %1 ვერსია %2 ან უფრო ახალი.
WinVersionTooHighError=ეს პროგრამა ვერ დაინსტალდება %1 ვერსია %2-ზე ან უფრო ახალზე.
AdminPrivilegesRequired=ამ პროგრამის დასაინსტალებლად საჭიროა ადმინისტრატორის ანგარიში.
PowerUserPrivilegesRequired=ამ პროგრამის დასაინსტალებლად საჭიროა ადმინისტრატორის ან მძლავრი იუზერის (Power User) ანგარიში.
SetupAppRunningError=საინსტალაციომ დაადგინა რომ გაშვებულია %1.%n%nგთხოვთ დახურეთ გაშვებული პროცესები, გასაგრძელებლად დააჭირეთ ღილაკს კარგი ან გამოსასვლელად ღილაკს გაუქმება.
UninstallAppRunningError=ამომშლელმა დაადგინა რომ გაშვებულია %1.%n%nგთხოვთ დახურეთ გაშვებული პროცესები, გასაგრძელებლად დააჭირეთ ღილაკს კარგი ან გამოსასვლელად ღილაკს გაუქმება.

; *** Startup questions
PrivilegesRequiredOverrideTitle=აირჩიეთ ინსტალაციის რეჟიმი
PrivilegesRequiredOverrideInstruction=აირჩიეთ ინსტალაციის რეჟიმი
PrivilegesRequiredOverrideText1=%1 შეიძლება დაინსტალდეს ყველასთვის (საჭიროა ადმინისტრატორის უფლება) ან მხოლოდ თქვენთვის.
PrivilegesRequiredOverrideText2=%1 შეიძლება დაინსტალდეს მხოლოდ თქვენთვის ან ყველასთვის (საჭიროა ადმინისტრატორის უფლება).
PrivilegesRequiredOverrideAllUsers=დაინსტალება ყველა მომხმარებლისტვის
PrivilegesRequiredOverrideAllUsersRecommended=დაინსტალება ყველა მომხმარებლისთვის (რეკომენდებულია)
PrivilegesRequiredOverrideCurrentUser=დაინსტალება მხოლოდ ჩემთვის
PrivilegesRequiredOverrideCurrentUserRecommended=დაინსტალება მხოლოდ ჩემთვის (რეკომენდებულია)

; *** Misc. errors
ErrorCreatingDir=საინსტალაციომ ვერ შექმნა საქაღალდე "%1"
ErrorTooManyFilesInDir=საქაღალდეში "%1" ვერ შეიქმნა ფაილი, რადგან ის შეიცავს ძალიან ბევრ ფაილს

; *** Setup common messages
ExitSetupTitle=საინსტალაციოს გათიშვა
ExitSetupMessage=ინსტალაცია არ დასრულებულა, გათიშვის შემთხვევაში პროცესი გაუქმდება.%n%nინსტალაციის დასასრულებლად მოგიწევთ საინსტალაციოს თავიდან გაშვება.%n%nგსურთ გათიშვა?
AboutSetupMenuItem=&ინსტალაციის შეწყვეტა...
AboutSetupTitle=ინსტალაციის შეწყვეტა
AboutSetupMessage=%1 ვერსია %2%n%3%n%n%1 ვებ-გვერდი:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< &უკან
ButtonNext=&შემდეგი >
ButtonInstall=&ინსტალაცია
ButtonOK=კარგი
ButtonCancel=გაუქმება
ButtonYes=&კი
ButtonYesToAll=კი ყველასთვის
ButtonNo=&არა
ButtonNoToAll=არა ყველასთვის
ButtonFinish=&დასრულება
ButtonBrowse=&მითითება...
ButtonWizardBrowse=მ&ითითება...
ButtonNewFolder=&ახალი საქაღალდე

; *** "Select Language" dialog messages
SelectLanguageTitle=ინსტალაციის ენის არჩევა
SelectLanguageLabel=მიუთითეთ ენა, რომელზეც გაეშვება საინსტალაციო.

; *** Common wizard text
ClickNext=გასაგრძელებლად დააჭირეთ შემდეგს, გასაუქმებლად გაუქმებას.
BeveledLabel=
BrowseDialogTitle=საქაღალდის მითითება
BrowseDialogLabel=აირჩიეთ საქაღალდე და დააჭირეთ ღილაკს კარგი.
NewFolderName=ახალი საქაღალდე

; *** "Welcome" wizard page
WelcomeLabel1=მოგესალმებით [name]-(ი)ს საინსტალაციოში
WelcomeLabel2=საინსტალაციო დააინსტალებს [name/ver] კომპიუტერში.%n%nგაგრძელებამდე რეკომენდებულია დახუროთ გახსნილი პროგრამები.

; *** "Password" wizard page
WizardPassword=პაროლი
PasswordLabel1=საინსტალაციო დაცულია პაროლით.
PasswordLabel3=შეიყვანეთ პაროლი და დააჭირეთ გაგრძელებას.
PasswordEditLabel=&პაროლი:
IncorrectPassword=შეყვანილი პაროლი არასწორია.

; *** "License Agreement" wizard page
WizardLicense=სალიცენზიო შეთანხმება
LicenseLabel=გაგრძელებამდე ყურადღებით გაეცანით ქვემოთ მოცემულ ინფორმაციას.
LicenseLabel3=ყურადღებით წაიკითხეთ სალიცენზიო შეთანხმება. გაგრძელებისთვის თქვენ უნდა დაეთანხმოთ მას.
LicenseAccepted=ვეთანხმები სალიცენზიო შეთანხმებას
LicenseNotAccepted=არ ვეთანხმები სალიცენზიო შეთანხმებას

; *** "Information" wizard pages
WizardInfoBefore=ინფორმაცია
InfoBeforeLabel=გაგრძელებამდე გთხოვთ წაიკითხოთ მნიშვნელოვანი ინფორმაცია.
InfoBeforeClickLabel=როცა მზად იქნები დააჭირე შემდეგს.
WizardInfoAfter=ინფორმაცია
InfoAfterLabel=გაგრძელებამდე გთხოვთ წაიკითხოთ მნიშვნელოვანი ინფორმაცია.
InfoAfterClickLabel=როცა მზად იქნები დააჭირე შემდეგს.

; *** "User Information" wizard page
WizardUserInfo=ინფორმაცია მომხმარებელზე
UserInfoDesc=შეიყვანეთ ინფორმაცია თქვენზე.
UserInfoName=&სახელი:
UserInfoOrg=&ორგანიზაცია:
UserInfoSerial=სერიული &ნომერი:
UserInfoNameRequired=უნდა შეიყვანოთ სახელი.

; *** "Select Destination Location" wizard page
WizardSelectDir=მიუთითეთ საინსტალაციო საქაღალდე
SelectDirDesc=სად დაინსტალდეს [name]?
SelectDirLabel3=საინსტალაციო დააინსტალებს [name]-(ი)ს მოცემულ საქაღალდეში.
SelectDirBrowseLabel=გასაგრძელებლად დააჭირეთ გაგრძელებას ან თუ გსურთ სხვა საქაღალდის მითითება - მითითებას.
DiskSpaceGBLabel=საჭიროა მინიმუმ [gb] გბ სივრცე.
DiskSpaceMBLabel=საჭიროა მინიმუმ [mb] მბ სივრცე.
CannotInstallToNetworkDrive=ვერ დაინსტალდება ქსელურ მისამართზე.
CannotInstallToUNCPath=ვერ დაინსტალდება UNC მისამართზე.
InvalidPath=უნდა შეიყვანეთ სრული მისამართი, დისკის სახელის ჩათვლით; მაგალითად:%n%nC:\APP%n%nან UNC მისამართი ფორმატში:%n%n\\server\share
InvalidDrive=დისკი ან UNC მისამართი არ არსებობს ან მიუწვდომელია. მიუთითეთ სხვა.
DiskSpaceWarningTitle=არასაკმარისი სივრცე დისკზე
DiskSpaceWarning=დასაინსტალებლად საჭიროა მინიმუმ %1 კბ სივრცე, მაგრამ ხელმისაწვდომია მხოლოდ %2 კბ.%n%nგსურთ გაგრძელება?
DirNameTooLong=საქაღალდის დასახელება ძალიან გრძელია.
InvalidDirName=საქაღალდის დასახელება არასწორია.
BadDirName32=საქაღალდის სახელში არ უნდა იყოს სიმბოლოები:%n%n%1
DirExistsTitle=საქაღალდე არსებობს
DirExists=საქაღალდე:%n%n%1%n%nუკვე არსებობს. გსურთ მაგ საქაღალდეში დაინსტალება?
DirDoesntExistTitle=საქაღალდე არ არსებობს
DirDoesntExist=საქაღალდე:%n%n%1%n%nარ არსებობს. გსურთ შექმნა?

; *** "Select Components" wizard page
WizardSelectComponents=აირჩიეთ კომპონენტები
SelectComponentsDesc=რომელი კომპონენტები დაინსტალდეს?
SelectComponentsLabel2=აირჩიეთ რომელი კომპონენტის დაინსტალებაც გსურთ; არ მონიშნოთ ის კომპონენტი რომლის დაინსტალებაც არ გსურთ. როცა მზად იქნებით დააჭირეთ გაგრძელებას.
FullInstallation=სრული ინსტალაცია
CompactInstallation=კომპაქტური ინსტალაცია
CustomInstallation=არჩევითი ინსტალაცია
NoUninstallWarningTitle=კომპონენტები არსებობს
NoUninstallWarning=საინსტალაციომ დაადგინა რომ ზოგიერთი კომპონენტი უკვე დაინსტალებულია:%n%n%1%n%nმათი არ მონიშვნა არ ნიშნავს რომ ისინი წაიშლება.%n%nგსურთ გაგრძელებას?
ComponentSize1=%1 კბ
ComponentSize2=%1 მბ
ComponentsDiskSpaceGBLabel=საჭიროა მინიმუმ [gb] გბ სივრცე.
ComponentsDiskSpaceMBLabel=საჭიროა მინიმუმ [mb] მბ სივრცე.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=მიუთითეთ დამატებითი დავალებები
SelectTasksDesc=რა დამატებითი დავალება შესრულდეს?
SelectTasksLabel2=აირჩიეთ თუ რომელიმე დამატებითი ფუნქციის შესრულება გსურთ [name]-(ი)ს ინსტალაციისას და დააჭირეთ შემდეგს.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=აირჩიეთ გაშვების მენიუს საქაღალდე
SelectStartMenuFolderDesc=სად დაინსტალდეს პროგრამის ხატულები?
SelectStartMenuFolderLabel3=საინსტალაციო პროგრამის ხატულებს გაშვების მენიუსთვის დააინსტალებს ქვემოთ მოცემულ საქაღალდეში.
SelectStartMenuFolderBrowseLabel=გასაგრძელებლად დააჭირეთ შემდეგს ან მიუთითეთ სხვა საქაღალდე.
MustEnterGroupName=ჩაწერეთ საქაღალდის სახელი.
GroupNameTooLong=საქაღალდის სახელი ან მისამართი ძალიან გრძელია.
InvalidGroupName=საქაღალდის სახელი არასწორია.
BadGroupName=სახელში არ უნდა იყოს შემდეგი სიმბოლოები:%n%n%1
NoProgramGroupCheck2=&არ შეიქმნას საქაღალდე გაშვების მენიუში

; *** "Ready to Install" wizard page
WizardReady=მზადაა დასაინსტალებლად
ReadyLabel1=საინსტალაციო მზადაა დააინსტალოს [name] თქვენს კომპიუტერში.
ReadyLabel2a=დასაინსტალებლად დააჭირეთ ინსტალაციას ან დაბრუნდით უკან და გადახედეთ პარამეტრებს.
ReadyLabel2b=დასაინსტალებლად დააჭირეთ ინსტალაციას.
ReadyMemoUserInfo=ინფორმაცია მომხმარებელზე:
ReadyMemoDir=ინფორმაცია საქაღალდეზე:
ReadyMemoType=ინსტალაციის სახეობა:
ReadyMemoComponents=არჩეული კომპონენტები:
ReadyMemoGroup=გაშვების მენიუს საქაღალდე:
ReadyMemoTasks=დამატებითი დავალებები:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel=იწერება დამატებითი ფაილები...
ButtonStopDownload=&გადმოწერის შეწყვეტა
StopDownload=ნამდვილად გსურთ გადმოწერის შეწყვეტა?
ErrorDownloadAborted=გადმოწერა შეწყდა
ErrorDownloadFailed=არ გადმოიწერა: %1 %2
ErrorDownloadSizeFailed=ზომის მონაცემები ვერ მივიღეთ: %1 %2
ErrorFileHash1=ფაილის ჰეში არ ემთხვევა: %1
ErrorFileHash2=ფაილის ჰეში არასწორია: ველოდებოდით %1, მივიღეთ %2
ErrorProgress=არასწორი პროცესი: %1 of %2
ErrorFileSize=ფაილის არასწორი ზომა: ველოდებოდთ %1, მივიღეთ %2

; *** "Preparing to Install" wizard page
WizardPreparing=მზადდება დასაინსტალებლად
PreparingDesc=საინსტალაციო ემზადება რომ [name] დააინსტალოს კომპიუტერში.
PreviousInstallNotCompleted=წინა პროგრამის ინსტალაცია/წაშლა არ მოხერხდა. საჭიროა კომპიუტერის გადატვირთვა.%n%nკომპიუტერის გადატვირთვის შემდეგ ხელახლა გაუშვით [name]-(ი)ს საინსტალაციო.
CannotContinue=ინსტალაცია არ გაგრძელდა. გასაუქმებლად დააჭირეთ გაუქმებას.
ApplicationsFound=მოცემული პროგრამები იყენებენ ფაილებს რომელიც საინსტალაციომ უნდა განახლოს. რეკომენდებულია უფლება მისცეთ საინსტალაციოს გათიშოს ეს პროგრამები.
ApplicationsFound2=ოცემული პროგრამები იყენებენ ფაილებს რომელიც საინსტალაციომ უნდა განახლოს. რეკომენდებულია უფლება მისცეთ საინსტალაციოს გათიშოს ეს პროგრამები. ინსტალაციის დასრულების შემდეგ საინსტალაციო შეეცდება ხელახლა ჩართოს ეს პროგრამები.
CloseApplications=&აპლიკაციების ავტომატურად გათიშვა
DontCloseApplications=&არ გაითიშოს აპლიკაციები
ErrorCloseApplications=საინსტალაციომ ავტომატურად ვერ გათიშა ყველა აპლიკაცია. რეკომენდებულია რომ გათიშოთ ყველა აპლიკაცია.
PrepareToInstallNeedsRestart=საინსტალაციომ უნდა გადატვირთოს კომპიუტერი. კომპიუტერის გადატვირთვის შემდეგ ხელახლა გაუშვით საინსტალაციო რათა გაგრძელდეს [name]-(ი)ს ინსტალაცია.%n%nგსურთ ახლა გადატვირთვა?

; *** "Installing" wizard page
WizardInstalling=ინსტალდება
InstallingLabel=მოითმინეთ სანამ საინსტალაციო დააინსტალებს [name]-(ი)ს კომპიუტერში.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=სრულდება [name]-(ი)ს ინსტალაცია
FinishedLabelNoIcons=საინსტალაციომ დაასრულა [name]-(ი)ს ინსტალაცია კომპიუტერში.
FinishedLabel=საინსტალაციომ დაასრულა [name]-(ი)ს ინსტალაცია კომპიუტერში. პროგრამა გაეშვება შესაბამისი ხატულას მონიშვნის შემთხვევაში.
ClickFinish=საინსტალაციოს გასათიშად დააჭირეთ დასრულებას.
FinishedRestartLabel=[name]-(ი)ს ინსტალაციის დასრულებისთვის საჭიროა კომპიუტეირს გადატვირთვა. გსურთ ახლა გადატვირთვა?
FinishedRestartMessage=[name]-(ი)ს ინსტალაციის დასრულებისთვის საჭიროა კომპიუტეირს გადატვირთვა.%n%nგსურთ ახლა გადატვირთვა?
ShowReadmeCheck=README ფაილის ჩვენება
YesRadio=&კი, გადაიტვირთოს კომპიუტერი
NoRadio=&არა, მოგვიანებით გადავტვირთავ კომპიუტერს
; used for example as 'Run MyProg.exe'
RunEntryExec=გაეშვას %1
; used for example as 'View Readme.txt'
RunEntryShellExec=%1-(ი)ს ნახვა

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=ინსტალაციისთვის საჭიროა შემდეგი დისკი
SelectDiskLabel2=ჩადეთ დისკი %1 და დააჭირეთ ღილაკს კარგი.%n%nთუ ფაილები არაა ქვემოთ მოცემულ მისამართზე დააჭირეთ მითითებას და მიუთითეთ სწორი მისამართი.
PathLabel=&მისამართი:
FileNotInDir2=ფაილი "%1" არ მოიძებნა "%2" მისამართზე. ჩადეთ სწორი დისკი და ან აირჩიეთ სხვა საქაღალდე.
SelectDirectoryLabel=მიუთითეთ შემდეგი დისკის მდებარეობა.

; *** Installation phase messages
SetupAborted=ინსტალაცია არ დასრულდა.%n%nგაასწორეთ პრობლემა და გაუშვით ინსტალაცია ხელახლა.
AbortRetryIgnoreSelectAction=აირჩიეთ მოქმედება
AbortRetryIgnoreRetry=&ხელახლა ცდა
AbortRetryIgnoreIgnore=&შეცდომის უგულებელყოფა და გაგრძელება
AbortRetryIgnoreCancel=ინსტალაციის გაუქმება

; *** Installation status messages
StatusClosingApplications=ითიშება პროგრამები...
StatusCreateDirs=მიმდინარეობს საქაღალდეების შექმნა...
StatusExtractFiles=მიმდინარეობს ფაილების ამოარქივება...
StatusCreateIcons=მიმდინარეობს ხატულების შექმნა...
StatusCreateIniEntries=იქმნება INI ჩანაწერები...
StatusCreateRegistryEntries=იქმნება რეესტრის ჩანაწერები...
StatusRegisterFiles=მიმდინარეობს ფაილების რეგისტრაცია...
StatusSavingUninstall=ინახება წასაშლელი ინფორმაცია...
StatusRunProgram=სრულდება ინსტალაცია...
StatusRestartingApplications=იტვირთება პროგრამები...
StatusRollback=უქმდება ცვლილებები...

; *** Misc. errors
ErrorInternal2=შიდა შეცდომა: %1
ErrorFunctionFailedNoCode=%1 არ შესრულდა
ErrorFunctionFailed=%1 არ შესრულდა; კოდი %2
ErrorFunctionFailedWithMessage=%1 არ შესრულდა; კოდი %2.%n%3
ErrorExecutingProgram=არ შესრულდა file:%n%1

; *** Registry errors
ErrorRegOpenKey=შეცდომა რეესტრის გასაღების გახსნისას:%n%1\%2
ErrorRegCreateKey=შეცდომა რეესტრის გასაღების შექმნისას:%n%1\%2
ErrorRegWriteKey=შეცდომა რეესტრის გასაღების ჩაწერისას:%n%1\%2

; *** INI errors
ErrorIniEntry=შეცდომა INI ჩანაწერის შექმნისას ფაილში "%1".

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=&ფაილის გამოტოვება (არაა რეკომენდებული)
FileAbortRetryIgnoreIgnoreNotRecommended=&შეცდომის უგულებელყოფა და გამოტოვება (არაა რეკომენდებული)
SourceIsCorrupted=საწყისი ფაილი დაზიანებულია
SourceDoesntExist=საწყისი ფაილი "%1" არსებობს
ExistingFileReadOnly2=არსებული ფაილი არ ჩანაცვლდა, იმიტომ რომ ის არის მხოლოდ წაკითხვადი.
ExistingFileReadOnlyRetry=&მხოლოდ წაკითხვადის მოხსნა და ხელახლა ცდა
ExistingFileReadOnlyKeepExisting=&არსებული ფაილის დატოვება
ErrorReadingExistingDest=შეცდომა არსებული ფაილის წაკითხვისას:
FileExistsSelectAction=აირჩიეთ მოქმედება
FileExists2=ფაილი უკვე არსებობს.
FileExistsOverwriteExisting=&არსებულ ფაილზე გადაწერა
FileExistsKeepExisting=ა&რსებული ფაილის დატოვება
FileExistsOverwriteOrKeepAll=&მოქმედების გამეორება შემდეგი კონფლიქტის დროს
ExistingFileNewerSelectAction=აირჩიეთ მოქმედება
ExistingFileNewer2=არსებული ფაილი უფრო ახალია ვიდრე საინსტალაციოში მოცემული.
ExistingFileNewerOverwriteExisting=&არსებულ ფაილზე გადაწერა
ExistingFileNewerKeepExisting=ა&რსებული ფაილის დატოვება (რეკომენდებულია)
ExistingFileNewerOverwriteOrKeepAll=&მოქმედების გამეორება შემდეგი კონფლიქტის დროს
ErrorChangingAttr=შეცდომა არსებულ ფაილზე ატრიბუტის შეცვლისას:
ErrorCreatingTemp=შეცდომა საქაღალდეში ფაილის შექმნისას:
ErrorReadingSource=შეცდომა საწყისი ფაილის წაკითხვისას:
ErrorCopying=შეცდომა ფაილის კოპირებისას:
ErrorReplacingExistingFile=შეცდომა არსებულ ფაილზ გადაწერისას:
ErrorRestartReplace=გადაწერა არ მოხერხდა:
ErrorRenamingTemp=შეცდომა ფაილის სახელის შეცვლისას:
ErrorRegisterServer=არ დარეგისტრდა DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 არ გაეშვა, კოდი: %1
ErrorRegisterTypeLib=არ დარეგისტრდა ბიბლიოთეკა: %1

; *** Uninstall display name markings
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32-bit
UninstallDisplayNameMark64Bit=64-bit
UninstallDisplayNameMarkAllUsers=ყველა მომხმარებელი
UninstallDisplayNameMarkCurrentUser=ახლანდელი მომხმარებელი

; *** Post-installation errors
ErrorOpeningReadme=შეცდომა Readme ფაილის გახსნისას.
ErrorRestartingComputer=საინსტალაციომ ვერ გადატვირთა კომპიუტერი. გადატვირთეთ ხელით.

; *** Uninstaller messages
UninstallNotFound=ფაილი "%1" არ არსებობს. არ წაიშალა.
UninstallOpenError=ფაილი "%1" არ არსებობს. არ წაიშალა
UninstallUnsupportedVer=წასაშლელი ჟურნალის ფაილი "%1" ისეთ ფორმატშია რომ ამ ვერსიის წამშლელი ვერ აღიქვამს. არ წაიშალა
UninstallUnknownEntry=უცნობი ჩანაწერი (%1)
ConfirmUninstall=ნამდვილად გსურთ სრულად წაშალოთ %1 და ყველა მისი კომპონენტი?
UninstallOnlyOnWin64=ეს ინსტალაცია შეიძლება წაიშალოს მხოლოდ 64 ბიტიან Windows-ში.
OnlyAdminCanUninstall=ეს ინსტალაცია შეიძლება წაიშალოს მხოლოდ ადმინისტრატორის უფლებებით.
UninstallStatusLabel=მოითმინეთ სანამ %1 წაიშლება კომპიუტერიდან.
UninstalledAll=%1 წაიშალა კომპიუტერიდან.
UninstalledMost=%1-(ი)ს წაშლა დასრულდა.%n%nზოგიერთი ელემენტი არ წაიშალა და წაშალეთ ხელით.
UninstalledAndNeedsRestart=იმისთვის რომ %1 წაიშალოს საჭიროა კომპიუტერის გადატვირთვა.%n%nგსურთ ახლა გადატვირთვა?
UninstallDataCorrupted="%1" ფაილი დაზიანებულია. არ წაიშალა

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=წაიშალოს გაზიარებული ფაილი?
ConfirmDeleteSharedFile2=სისტემამ აღმოაჩინა რომ მოცემულ გაზიარებულ ფაილს არ იყენებს არც ერთი პროგრამა. გსურთ წამშლელმა წაშალოს გაზიარებული ფაილი?%n%nთუ რომელიმე პროგრამა ისევ იყენებს ამ ფაილს და მას წაშლით, ეს პროგრამები ვეღარ იმუშავებენ ნორმალურად. დარწმუნებული თუ არ ხართ დააჭირეთ არას. ფაილის დატოვება არაფერს გააფუჭებს.
SharedFileNameLabel=ფაილის სახელი:
SharedFileLocationLabel=მდებარეობა:
WizardUninstalling=წაშლის სტატუსი
StatusUninstalling=იშლება %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=ინსტალდება %1.
ShutdownBlockReasonUninstallingApp=იშლება %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 ვერსია %2
AdditionalIcons=დამატებითი ხატულები:
CreateDesktopIcon=ხატულას შექმნა სამუშაო მაგიდაზე
CreateQuickLaunchIcon=სწრაფი გაშვების ხატულას შექმნა
ProgramOnTheWeb=%1 ვებ-გვერდზე
UninstallProgram=წაიშალოს %1
LaunchProgram=გაეშვას %1
AssocFileExtension=&ასოცირდეს %1 %2 ფაილის გაფართოებასთან?
AssocingFileExtension=%1 ასოცირდება %2 ფაილების გაფართოებასთან...
AutoStartProgramGroupDescription=გაშვება:
AutoStartProgram=ავტომატურად გაშვება %1
AddonHostProgramNotFound=%1 არ მოიძებნა მითითებულ საქაღალდეში.%n%nგსურთ გაგრძელება?