; *** Inno Setup version 6.5.0+ Thai messages ***
;
; To download user-contributed translations of this file, go to:
;   https://jrsoftware.org/files/istrans/
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).
;
; Translated by linesofcodes@dailitation.xyz (Satakun Utama)

[LangOptions]
; The following three entries are very important. Be sure to read and 
; understand the '[LangOptions] section' topic in the help file.
LanguageName=ไทย
LanguageID=$041E
LanguageCodePage=0
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
SetupAppTitle=การติดตั้ง
SetupWindowTitle=การติดตั้ง - %1
UninstallAppTitle=การถอนการติดตั้ง
UninstallAppFullTitle=การถอนการติดตั้ง %1

; *** Misc. common
InformationTitle=ข้อมูล
ConfirmTitle=ยืนยัน
ErrorTitle=ข้อผิดพลาด

; *** SetupLdr messages
SetupLdrStartupMessage=กระบวนการนี้จะทำการติดตั้ง %1 คุณต้องการดำเนินการต่อหรือไม่
LdrCannotCreateTemp=ไม่สามารถสร้างไฟล์ข้อมูลชั่วคราวได้ การติดตั้งถูกยกเลิก
LdrCannotExecTemp=ไม่สามารถเปิดใช้งานไฟล์ในโฟลเดอร์ชั่วคราวได้ การติดตั้งถูกยกเลิก
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nข้อผิดพลาด %2: %3
SetupFileMissing=ไม่พบไฟล์ %1 ในโฟลเดอร์การติดตั้ง โปรดแก้ไขปัญหาหรือรับสำเนาใหม่ของโปรแกรม
SetupFileCorrupt=ไฟล์การติดตั้งเสียหาย โปรดรับสำเนาใหม่ของโปรแกรม
SetupFileCorruptOrWrongVer=ไฟล์การติดตั้งเสียหายหรือไม่เข้ากันกับโปรแกรมติดตั้งเวอร์ชั่นนี้ โปรดแก้ไขปัญหาหรือรับสำเนาใหม่ของโปรแกรม
InvalidParameter=มีการส่งพารามิเตอร์ที่ไม่ถูกต้องบนบรรทัดคำสั่ง:%n%n%1
SetupAlreadyRunning=โปรแกรมติดตั้งทำงานอยู่แล้ว
WindowsVersionNotSupported=โปรแกรมนี้ไม่รองรับเวอร์ชั่นของ Windows ที่คอมพิวเตอร์ของคุณใช้งานอยู่
WindowsServicePackRequired=โปรแกรมนี้จำเป็นต้องใช้ %1 Service Pack %2 ขึ้นไป
NotOnThisPlatform=โปรแกรมนี้จะไม่ทำงานบน %1.
OnlyOnThisPlatform=โปรแกรมนี้จะต้องทำงานบน %1.
OnlyOnTheseArchitectures=โปรแกรมนี้สามารถติดตั้งได้บน Windows ที่ถูกออกแบบมาสำหรับสถาปัตยกรรมหน่วยประมวลผลต่อไปนี้เท่านั้น:%n%n%1
WinVersionTooLowError=โปรแกรมนี้ต้องใช้ %1 เวอร์ชั่น %2 ขึ้นไป
WinVersionTooHighError=โปรแกรมนี้ไม่สามารถติดตั้งบน %1 เวอร์ชั่น %2 ขึ้นไปได้
AdminPrivilegesRequired=คุณจำเป็นต้องล็อกอินเป็นผู้ดูแลระบบในการติดตั้งโปรแกรมนี้
PowerUserPrivilegesRequired=คุณต้องล็อกอินเป็นผู้ดูแลระบบหรือเป็นสมาชิกของกลุ่ม Power Users ในการติดตั้งโปรแกรมนี้
SetupAppRunningError=ตัวติดตั้งได้พบว่า %1 กำลังถูกใช้งานอยู่%n%nโปรดปิดทุกโปรเซสที่เกี่ยวข้องตอนนี้แล้วคลิกตกลงเพื่อดำเนินการต่อ และกดยกเลิกเพื่อออก
UninstallAppRunningError=ตัวถอนการติดตั้งไปพบว่า %1 กำลังถูกใช้งานอยู่%n%nโปรดปิดทุกโปรเซสที่เกี่ยวข้องตอนนี้แล้วคลิกตกลงเพื่อดำเนินการต่อ และกดยกเลิกเพื่อออก

; *** Startup questions
PrivilegesRequiredOverrideTitle=เลือกโหมดการติดตั้ง
PrivilegesRequiredOverrideInstruction=เลือกโหมดการติดตั้ง
PrivilegesRequiredOverrideText1=%1 สามารถถูกติดตั้งให้ผู้ใช้ทั้งหมดได้ (จำเป็นต้องมีสิทธิ์ของผู้ดูแลระบบ) หรือให้แค่คุณเท่านั้น
PrivilegesRequiredOverrideText2=%1 สามารถถูกติดตั้งให้คุณเท่านั้นได้หรือให้ผู้ใช้ทั้งหมด (จำเป็นต้องมีสิทธิ์ของผู้ดูแลระบบ)
PrivilegesRequiredOverrideAllUsers=ติดตั้งสำหรับผู้ใช้&ทั้งหมด
PrivilegesRequiredOverrideAllUsersRecommended=ติดตั้งสำหรับผู้ใช้&ทั้งหมด (แนะนำ)
PrivilegesRequiredOverrideCurrentUser=ติดตั้งให้&ฉันเท่านั้น
PrivilegesRequiredOverrideCurrentUserRecommended=ติดตั้งให้&ฉันเท่านั้น (แนะนำ)

; *** Misc. errors
ErrorCreatingDir=ตัวติดตั้งไม่สามารถสร้างโฟลเดอร์ "%1" ได้
ErrorTooManyFilesInDir=ไม่สามารถสร้างไฟล์ในโฟลเดอร์ "%1" ได้เพราะมันมีหลายไฟล์เกินไป

; *** Setup common messages
ExitSetupTitle=ออกจากตัวติดตั้ง
ExitSetupMessage=การติดตั้งยังไม่เสร็จสมบูรณ์ หากคุณออกตอนนี้ โปรแกรมจะไม่ถูกติดตั้ง%n%nคุณสามารถเรียกใช้ตัวติดตั้งอีกครั้งในภายหลังได้เพื่อทำการติดตั้งให้เสร็จสมบูรณ์%n%nออกจากตัวติดตั้งหรือไม่
AboutSetupMenuItem=&เกี่ยวกับตัวติดตั้ง...
AboutSetupTitle=เกี่ยวกับตัวติดตั้ง
AboutSetupMessage=%1 เวอร์ชั่น %2%n%3%n%n%1 หน้าหลัก:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< &กลับ
ButtonNext=&ถัดไป >
ButtonInstall=&ติดตั้ง
ButtonOK=ตกลง
ButtonCancel=ยกเลิก
ButtonYes=&ใช่
ButtonYesToAll=ใช่สำหรับ&ทั้งหมด
ButtonNo=&ไม่
ButtonNoToAll=ไ&ม่สำหรับทั้งหมด
ButtonFinish=&เสร็จสิ้น
ButtonBrowse=&เรียกดู...
ButtonWizardBrowse=เ&รียกดู...
ButtonNewFolder=&สร้างโฟลเดอร์ใหม่

; *** "Select Language" dialog messages
SelectLanguageTitle=เลือกภาษาตัวติดตั้ง
SelectLanguageLabel=เลือกภาษาที่จะใช้ในระหว่างการติดตั้ง

; *** Common wizard text
ClickNext=กดถัดไปเพื่อดำเนินการต่อหรือยกเลิกเพื่อออกจากตัวติดตั้ง
BeveledLabel=
BrowseDialogTitle=เรียกดูโฟลเดอร์
BrowseDialogLabel=เลือกโฟลเดอร์จากรายการด้านล่าง แล้วกดตกลง
NewFolderName=โฟลเดอร์ใหม่

; *** "Welcome" wizard page
WelcomeLabel1=ยินดีต้อนรับสู่ตัวติดตั้ง [name]
WelcomeLabel2=กระบวนการนี้จะติดตั้ง [name/ver] ลงบนคอมพิวเตอร์ของคุณ%n%nเราแนะนำให้คุณปิดโปรแกรมอื่น ๆ ก่อนดำเนินการต่อ

; *** "Password" wizard page
WizardPassword=รหัสผ่าน
PasswordLabel1=ตัวติดตั้งนี้ถูกป้องกันด้วยรหัสผ่าน
PasswordLabel3=โปรดใส่รหัสผ่านแล้วจึงกดถัดไปเพื่อดำเนินการต่อ และโปรดคำนึงถึงตัวพิมพ์เล็ก/ใหญ่ในรหัสผ่าน
PasswordEditLabel=&รหัสผ่าน:
IncorrectPassword=รหัสผ่านที่คุณใส่ไม่ถูกต้อง โปรดลองอีกครั้ง

; *** "License Agreement" wizard page
WizardLicense=ข้อตกลงการอนุญาต
LicenseLabel=โปรดอ่านข้อมูลที่สำคัญดังต่อไปนี้ก่อนดำเนินการต่อ
LicenseLabel3=โปรดอ่านข้อตกลงดังต่อไปนี้ คุณต้องตกลงกับข้อตกลงนี้ก่อนดำเนินการต่อไปในการติดตั้งซอฟท์แวร์
LicenseAccepted=ฉัน&ยอมรับข้อตกลง
LicenseNotAccepted=ฉัน&ไม่ยอมรับข้อตกลง

; *** "Information" wizard pages
WizardInfoBefore=ข้อมูล
InfoBeforeLabel=โปรดอ่านข้อมูลที่สำคัญดังต่อไปนี้ก่อนดำเนินการต่อ
InfoBeforeClickLabel=เมื่อคุณพร้อมที่จะดำเนินการต่อในการติดตั้ง คลิกถัดไป
WizardInfoAfter=ข้อมูล
InfoAfterLabel=โปรดอ่านข้อมูลที่สำคัญดังต่อไปนี้ก่อนดำเนินการต่อ
InfoAfterClickLabel=เมื่อคุณพร้อมที่จะดำเนินการต่อในการติดตั้ง คลิกถัดไป

; *** "User Information" wizard page
WizardUserInfo=ข้อมูลผู้ใช้
UserInfoDesc=โปรดใส่ข้อมูลของคุณ
UserInfoName=&ชื่อผู้ใช้:
UserInfoOrg=&องค์กร:
UserInfoSerial=&หมายเลขซีเรียล:
UserInfoNameRequired=คุณจำเป็นต้องใส่ชื่อ

; *** "Select Destination Location" wizard page
WizardSelectDir=เลือกสถานที่ติดตั้ง
SelectDirDesc=[name] ควรถูกติดตั้งที่ใด
SelectDirLabel3=ตัวติดตั้งจะติดตั้ง [name] ไปยังโฟลเดอร์ต่อไปนี้
SelectDirBrowseLabel=เพื่อดำเนินการต่อ กดถัดไป หากคุณต้องการเลือกโฟลเดอร์อื่น คลิกเรียกดู
DiskSpaceGBLabel=จำเป็นต้องใช้พื้นที่อย่างน้อย [gb] GB
DiskSpaceMBLabel=จำเป็นต้องใช้พื้นที่อย่างน้อย [mb] MB
CannotInstallToNetworkDrive=ตัวติดตั้งไม่สามารถติดตั้งไปยังไดรฟ์เครือข่ายได้
CannotInstallToUNCPath=ตัวติดตั้งไม่สามารถติดตั้งไปยังพาธ UNC ได้
InvalidPath=คุณต้องใส่พาธเต็มที่มีอักษรไดรฟ์ ตัวอย่างเช่น:%n%nC:\APP%n%nหรือพาธ UNC ในรูปแบบ:%n%n\\server\share
InvalidDrive=ไดรฟ์หรือแชร์ UNC ที่คุณเลือกไม่มีอยู่หรือไม่สามารถเข้าถึงได้ โปรดเลือกไดรฟ์อื่น
DiskSpaceWarningTitle=พื้นที่ไม่เพียงพอ
DiskSpaceWarning=ตัวติดตั้งต้องใช้พื้นที่อย่างน้อย %1 KB ในการติดตั้ง แต่ไดรฟ์ที่เลือกมีพื้นที่เหลือเพียงแค่ %2 KB%n%nคุณต้องการดำเนินการต่ออยู่หรือไม่
DirNameTooLong=ชื่อโฟลเดอร์หรือพาธยาวเกินไป
InvalidDirName=ชื่อโฟลเดอร์ไม่ถูกต้อง
BadDirName32=ชื่อโฟลเดอร์ไม่สามารถมีตัวอักษรต่อไปนี้ได้:%n%n%1
DirExistsTitle=มีโฟลเดอร์อยู่แล้ว
DirExists=โฟลเดอร์:%n%n%1%n%nมีอยู่แล้ว คุณต้องการที่จะติดตั้งไปยังโฟลเดอร์นั้นอยู่หรือไม่
DirDoesntExistTitle=ไม่มีโฟลเดอร์
DirDoesntExist=โฟลเดอร์:%n%n%1%n%nไม่มีอยู่ คุณต้องการให้โฟลเดอร์ถูกสร้างหรือไม่

; *** "Select Components" wizard page
WizardSelectComponents=เลือกส่วนประกอบ
SelectComponentsDesc=ส่วนประกอบใดบ้างที่จะถูกติดตั้ง
SelectComponentsLabel2=เลือกส่วนประกอบที่คุณต้องการติดตั้ง ล้างส่วนประกอบที่คุณไม่ต้องการติดตั้ง กดถัดไปเมื่อคุณพร้อมที่จะดำเนินการต่อ
FullInstallation=การติดตั้งแบบเต็ม
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=การติดตั้งแบบกะทัดรัด
CustomInstallation=การติดตั้งแบบกำหนดเอง
NoUninstallWarningTitle=มีส่วนประกอบอยู่แล้ว
NoUninstallWarning=ตัวติดตั้งได้พบว่าส่วนประกอบดังต่อไปนี้ติดตั้งอยู่บนคอมพิวเตอร์ของคุณอยู่แล้ว:%n%n%1%n%nการที่ไม่เลือกส่วนประกอบเหล่านี้จะไม่ส่งผลให้ส่วนประกอบถูกถอนการติดตั้ง%n%nคุณต้องการดำเนินการต่ออยู่หรือไม่
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=การเลือกปัจจุบันต้องใช้พื้นที่อย่างน้อย [gb] GB
ComponentsDiskSpaceMBLabel=การเลือกปัจจุบันต้องใช้พื้นที่อย่างน้อย [mb] MB

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=เลือกงานเพิ่มเติม
SelectTasksDesc=งานเพิ่มเติมใดบ้างที่ควรกระทำ
SelectTasksLabel2=เลือกงานเพิ่มเติมที่คุณต้องการให้ตัวติดตั้งกระทำในระหว่างการติดตั้ง [name] แล้วจึงกดถัดไป

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=เลือกโฟลเดอร์สตาร์ทเมนู
SelectStartMenuFolderDesc=ตัวติดตั้งควรวางทางลัดโปรแกรมไว้ที่ใด
SelectStartMenuFolderLabel3=ตัวติดตั้งจะสร้างทางลัดโปรแกรมไปยังโฟลเดอร์สตาร์ทเมนูดังต่อไปนี้
SelectStartMenuFolderBrowseLabel=ในการดำเนินการต่อ คลิกถัดไป หากคุณต้องการเลือกโฟลเดอร์อื่น คลิกเรียกดู
MustEnterGroupName=คุณต้องใส่ชื่อโฟลเดอร์
GroupNameTooLong=ชื่อโฟลเดอร์หรือพาธยาวเกินไป
InvalidGroupName=ชื่อโฟลเดอร์ไม่ถูกต้อง
BadGroupName=ชื่อโฟลเดอร์ไม่สามารถมีตัวอักษรดังต่อไปนี้ได้:%n%n%1
NoProgramGroupCheck2=&ไม่ต้องสร้างโฟลเดอร์สตาร์ทเมนู

; *** "Ready to Install" wizard page
WizardReady=พร้อมติดตั้ง
ReadyLabel1=ตัวติดตั้งพร้อมแล้วที่จะเริ่มทำการติดตั้ง [name] บนคอมพิวเตอร์ของคุณ
ReadyLabel2a=คลิกติดตั้งเพื่อดำเนินการต่อในการติดตั้ง หรือคลิกย้อนกลับหากคุณต้องการทบทวนหรือเปลี่ยนการตั้งค่าใด ๆ
ReadyLabel2b=คลิกติดตั้งเพื่อดำเนินการต่อในการติดตั้ง
ReadyMemoUserInfo=ข้อมูลผู้ใช้:
ReadyMemoDir=สถานที่ปลายทาง:
ReadyMemoType=ประเภทการติดตั้ง:
ReadyMemoComponents=ส่วนประกอบที่เลือก:
ReadyMemoGroup=โฟลเดอร์สตาร์ทเมนู:
ReadyMemoTasks=งานเพิ่มเติม:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel2=กำลังดาวน์โหลดไฟล์...
ButtonStopDownload=&หยุดดาวน์โหลด
StopDownload=คุณแน่ใจหรือไม่ที่จะหยุดการดาวน์โหลด
ErrorDownloadAborted=การดาวน์โหลดถูกยกเลิก
ErrorDownloadFailed=การดาวน์โหลดล้มเหลว: %1 %2
ErrorDownloadSizeFailed=การรับขนาดไฟล์ล้มเหลว: %1 %2
ErrorProgress=ความคืบหน้าไม่ถูกต้อง: %1 จาก %2
ErrorFileSize=ขนาดไฟล์ไม่ถูกต้อง: ควรจะเป็น %1 พบ %2

; *** TExtractionWizardPage wizard page and ExtractArchive
ExtractingLabel=กำลังแตกไฟล์...
ButtonStopExtraction=&หยุดการแตกไฟล์
StopExtraction=คุณแน่ใจหรือไม่ที่จะหยุดการแตกไฟล์
ErrorExtractionAborted=หยุดการแตกไฟล์แล้ว
ErrorExtractionFailed=การแตกไฟล์ล้มเหลว: %1

; *** Archive extraction failure details
ArchiveIncorrectPassword=รหัสผ่านไม่ถูกต้อง
ArchiveIsCorrupted=ไฟล์เก็บถาวรเสียหาย
ArchiveUnsupportedFormat=รูปแบบของไฟล์เก็บถาวรไม่ถูกต้อง

; *** "Preparing to Install" wizard page
WizardPreparing=เตรียมการติดตั้ง
PreparingDesc=ตัวติดตั้งกำลังเตรียมการที่จะติดตั้ง [name] บนคอมพิวเตอร์ของคุณ
PreviousInstallNotCompleted=การติดตั้ง/ถอนการติดตั้งของโปรแกรมก่อนหน้าไม่เสร็จสมบูรณ์ คุณจะต้องรีสตาร์ทคอมพิวเตอร์ของคุณในการติดตั้งให้เสร็จสมบูรณ์%n%nหลังจากรีสตาร์ทคอมพิวเตอร์ของคุณแล้ว เปิดตัวติดตั้งอีกครั้งเพื่อติดตั้ง [name] ให้เสร็จสิ้น
CannotContinue=ตัวติดตั้งไม่สามารถดำเนินการต่อได้ โปรดคลิกยกเลิกเพื่อออก
ApplicationsFound=แอพลิเคชั่นดังต่อไปนี้กำลังใช้ไฟล์ที่จำเป็นต้องถูกอัพเดทโดยตัวติดตั้ง ขอแนะนำให้คุณอนุญาตตัวติดตั้งในการปิดแอพลิเคชั่นเหล่านี้โดยอัติโนมัติ
ApplicationsFound2=แอพลิเคชั่นดังต่อไปนี้กำลังใช้ไฟล์ที่จำเป็นต้องถูกอัพเดทโดยตัวติดตั้ง ขอแนะนำให้คุณอนุญาตตัวติดตั้งในการปิดแอพลิเคชั่นเหล่านี้โดยอัติโนมัติ หลังจากการติดตั้งเสร็จสิ้น ตัวติดตั้งจะพยายามรีสตาร์ทแอพลิเคชั่นเหล่านั้น
CloseApplications=&ปิดแอพลิเคชั่นโดยอัติโนมัติ
DontCloseApplications=&ไม่ต้องปิดแอพลิเคชั่น
ErrorCloseApplications=ตัวติดตั้งไม่สามารถปิดแอพลิเคชั่นทั้งหมดได้ ขอแนะนำให้คุณปิดแอพลิเคชั่นทั้งหมดที่ใช้ไฟล์ที่จำเป็นต้องถูกอัพเดทโดยตัวติดตั้งก่อนดำเนินการต่อ
PrepareToInstallNeedsRestart=ตัวติดตั้งจำเป็นจะต้องรีสตาร์ทคอมพิวเตอร์ของคุณ หลังจากรีสตาร์ทคอมพิวเตอร์ของคุณแล้ว เปิดตัวติดตั้งอีกครั้งเพื่อติดตั้ง [name] ให้เสร็จสิ้น%n%nคุณอยากรีสตาร์ทตอนนี้หรือไม่

; *** "Installing" wizard page
WizardInstalling=กำลังติดตั้ง
InstallingLabel=โปรดรอในระหว่างที่ตัวติดตั้งติดตั้ง [name] ลงบนคอมพิวเตอร์ของคุณ

; *** "Setup Completed" wizard page
FinishedHeadingLabel=การเสร็จสิ้นของตัวติดตั้ง [name]
FinishedLabelNoIcons=ตัวติดตั้งเสร็จสิ้นในการติดตั้ง [name] ลงบนคอมพิวเตอร์ของคุณแล้ว
FinishedLabel=ตัวติดตั้งติดตั้ง [name] ลงบนคอมพิวเตอร์ของคุณแล้ว แอพลิเคชั่นสามารถถูกเปิดได้โดยการเลือกทางลัดที่ถูกติดตั้ง
ClickFinish=คลิกเสร็จสิ้นเพื่อออกจากตัวติดตั้ง
FinishedRestartLabel=ในการติดตั้ง [name] ให้เสร็จสิ้น ตัวติดตั้งจะต้องรีสตาร์ทคอมพิวเตอร์ของคุณ คุณต้องการรีสตาร์ทตอนนี้เลยหรือไม่
FinishedRestartMessage=ในการติดตั้ง [name] ให้เสร็จสิ้น ตัวติดตั้งจะต้องรีสตาร์ทคอมพิวเตอร์ของคุณ%n%nคุณต้องการรีสตาร์ทตอนนี้เลยหรือไม่
ShowReadmeCheck=ใช่ ฉันอยากจะดูไฟล์ README
YesRadio=&ใช่ รีสตาร์ทคอมพิวเตอร์ตอนนี้
NoRadio=&ไม่ ฉันจะรีสตาร์ทคอมพิวเตอร์ในภายหลัง
; used for example as 'Run MyProg.exe'
RunEntryExec=เปิด %1
; used for example as 'View Readme.txt'
RunEntryShellExec=ดู %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=ตัวติดตั้งต้องการดิสก์ถัดไป
SelectDiskLabel2=โปรดใส่ดิสก์ %1 และคลิกตกลง%n%nถ้าไฟล์บนดิสก์นี้สามารถพบได้ในโฟลเดอร์อื่นนอกจากที่แสดงด้านล่าง กรอกพาธที่ถูกต้องหรือคลิกเรียกดู
PathLabel=&พาธ:
FileNotInDir2=ไม่พบไฟล์ "%1" ใน "%2" โปรดใส่ดิสก์ที่ถูกต้องหรือเลือกโฟลเดอร์อื่น
SelectDirectoryLabel=โปรดระบุสถานที่ของดิสก์ถัดไป

; *** Installation phase messages
SetupAborted=การติดตั้งไม่เสร็จสิ้น%n%nโปรดแก้ไขปัญหาและเปิดตัวติดตั้งอีกครั้ง
AbortRetryIgnoreSelectAction=เลือกการดำเนินการ
AbortRetryIgnoreRetry=&ลองอีกครั้ง
AbortRetryIgnoreIgnore=&ไม่สนใจข้อผิดพลาดและดำเนินการต่อ
AbortRetryIgnoreCancel=ยกเลิกการติดตั้ง
RetryCancelSelectAction=เลือกการกระทำ
RetryCancelRetry=&ลองอีกครั้ง
RetryCancelCancel=ยกเลิก

; *** Installation status messages
StatusClosingApplications=กำลังปิดแอพลิเคชั่น...
StatusCreateDirs=กำลังสร้างโฟลเดอร์...
StatusExtractFiles=กำลังแตกไฟล์...
StatusDownloadFiles=กำลังดาวน์โหลดไฟล์...
StatusCreateIcons=กำลังสร้างทางลัด...
StatusCreateIniEntries=กำลังสร้างรายการ INI...
StatusCreateRegistryEntries=กำลังสร้างรายการรีจิสทรี...
StatusRegisterFiles=กำลังลงทะเบียนไฟล์...
StatusSavingUninstall=กำลังบันทึกข้อมูลการถอนการติดตั้ง...
StatusRunProgram=กำลังทำการติดตั้งให้เสร็จสิ้น...
StatusRestartingApplications=กำลังรีสตาร์ทแอพลิเคชั่น...
StatusRollback=กำลังย้อนการเปลี่ยนแปลง...

; *** Misc. errors
ErrorInternal2=ข้อผิดพลาดภายใน: %1
ErrorFunctionFailedNoCode=%1 ล้มเหลว
ErrorFunctionFailed=%1 ล้มเหลว; โค้ด %2
ErrorFunctionFailedWithMessage=%1 ล้มเหลว; โค้ด %2.%n%3
ErrorExecutingProgram=ไม่สามารถเปิดไฟล์ได้:%n%1

; *** Registry errors
ErrorRegOpenKey=ข้อผิดพลาดในการเปิดคีย์รีจิสทรี:%n%1\%2
ErrorRegCreateKey=ข้อผิดพลาดในการสร้างคีย์รีจิสทรี:%n%1\%2
ErrorRegWriteKey=ข้อผิดพลาดในการเขียนไปยังรีจิสทรีคีย์:%n%1\%2

; *** INI errors
ErrorIniEntry=ข้อผิดพลาดในการสร้างรายการ INI ในไฟล์ "%1"

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=&ข้ามไฟล์นี้ (ไม่แนะนำ)
FileAbortRetryIgnoreIgnoreNotRecommended=&ไม่สนใจข้อผิดพลาดและดำเนินการต่อ (ไม่แนะนำ)
SourceIsCorrupted=ไฟล์ต้นฉบับเสียหาย
SourceDoesntExist=ไม่มีไฟล์ต้นฉบับ "%1"
SourceVerificationFailed=การยืนยันไฟล์ต้นฉบับล้มเหลว: %1
VerificationSignatureDoesntExist=ไฟล์ลายเซ็น "%1" ไม่มีอยู่
VerificationSignatureInvalid=ไฟล์ลายเซ็น "%1" ไม่ถูกต้อง
VerificationKeyNotFound=ไฟล์ลายเซ็น "%1" ใช้คีย์ที่ไม่รู้จัก
VerificationFileNameIncorrect=ชื่อไฟล์ไม่ถูกต้อง
VerificationFileTagIncorrect=แท็กของไฟล์ไม่ถูกต้อง
VerificationFileSizeIncorrect=ขนาดของไฟล์ไม่ถูกต้อง
VerificationFileHashIncorrect=แฮชของไฟล์ไม่ถูกต้อง
ExistingFileReadOnly2=ไฟล์ที่มีอยู่ไม่สามารถถูกแทนที่ได้เนื่องจากถูกทำเครื่องหมายว่าเป็นไฟล์อ่านอย่างเดียว
ExistingFileReadOnlyRetry=&ลบเครื่องหมายอ่านอย่างเดียวและลองใหม่
ExistingFileReadOnlyKeepExisting=&เก็บไฟล์ที่มีอยู่ไว้
ErrorReadingExistingDest=เกิดข้อผิดพลาดในการอ่านไฟล์ที่มีอยู่:
FileExistsSelectAction=เลือกการกระทำ
FileExists2=ไฟล์มีอยู่แล้ว
FileExistsOverwriteExisting=&เขียนทับไฟล์ที่มีอยู่
FileExistsKeepExisting=&เก็บไฟล์ที่มีอยู่ไว้
FileExistsOverwriteOrKeepAll=&ทำสิ่งนี้สำหรับการขัดแย้งครั้งต่อไป
ExistingFileNewerSelectAction=เลือกการกระทำ
ExistingFileNewer2=ไฟล์ที่มีอยู่ใหม่กว่าไฟล์ที่ตัวติดตั้งกำลังพยายามจะติดตั้ง
ExistingFileNewerOverwriteExisting=&เขียนทับไฟล์ที่มีอยู่
ExistingFileNewerKeepExisting=&เก็บไฟล์ที่มีอยู่ (แนะนำ)
ExistingFileNewerOverwriteOrKeepAll=&ทำสิ่งนี้สำหรับการขัดแย้งครั้งต่อไป
ErrorChangingAttr=เกิดข้อผิดพลาดในการเปลี่ยนแปลงคุณสมบัติของไฟล์ที่มีอยู่:
ErrorCreatingTemp=เกิดข้อผิดพลาดในระหว่างการสร้างไฟล์ในโฟลเดอร์ที่หมาย:
ErrorReadingSource=เกิดข้อผิดพลาดในระหว่างการอ่านไฟล์ต้นฉบับ:
ErrorCopying=เกิดข้อผิดพลาดในระหว่างการคัดลอกไฟล์:
ErrorDownloading=เกิดข้อผิดพลาดในระหว่างการดาวน์โหลดไฟล์:
ErrorExtracting=เกิดข้อผิดพลาดในระหว่างการแตกไฟล์:
ErrorReplacingExistingFile=เกิดข้อผิดพลาดในระหว่างการแทนที่ไฟล์ที่มีอยู่:
ErrorRestartReplace=RestartReplace ล้มเหลว:
ErrorRenamingTemp=เกิดข้อผิดพลาดในระหว่างการเปลี่ยนชื่อไฟล์ในโฟลเดอร์ที่หมาย:
ErrorRegisterServer=ไม่สามารถลงทะเบียน DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 ล้มเหลวด้วยโค้ด %1
ErrorRegisterTypeLib=ไม่สามารถลงทะเบียนไลบรารีไทป์ได้: %1

; *** Uninstall display name markings
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32-bit
UninstallDisplayNameMark64Bit=64-bit
UninstallDisplayNameMarkAllUsers=ผู้ใช้ทั้งหมด
UninstallDisplayNameMarkCurrentUser=ผู้ใช้ปัจจุบัน

; *** Post-installation errors
ErrorOpeningReadme=เกิดข้อผิดพลาดในระหว่างการเปิดไฟล์ README
ErrorRestartingComputer=ตัวติดตั้งไม่สามารถรีสตาร์ทคอมพิวเตอร์ได้ โปรดทำการรีสตาร์ทด้วยตนเอง

; *** Uninstaller messages
UninstallNotFound=ไม่มีไฟล์ "%1" ไม่สามารถถอนการติดตั้งได้
UninstallOpenError=ไม่สามารถเปิดไฟล์ "%1" ไม่สามารถถอนการติดตั้งได้
UninstallUnsupportedVer=ไฟล์บันทึกการถอนการติดตั้ง "%1" อยู่ในรูปแบบที่ตัวถอนการติดตั้งเวอร์ชั่นนี้ไม่รู้จัก ไม่สามารถถอนการติดตั้งได้
UninstallUnknownEntry=รายการที่ไม่รู้จัก (%1) ถูกพบในไฟล์บันทึกการถอนการติดตั้ง
ConfirmUninstall=คุณแน่ใจหรือไม่ที่จะลบ %1 และส่วนประกอบทั้งหมดอย่างสมบูรณ์
UninstallOnlyOnWin64=การติดตั้งนี้สามารถถูกถอนการติดตั้งได้บน 64-bit Windows เท่านั้น
OnlyAdminCanUninstall=การติดตั้งนี้สามารถถูกถอนการติดตั้งได้โดยผู้ใช้ที่มีสิทธิ์ในการดูแลระบบเท่านั้น
UninstallStatusLabel=โปรดรอในระหว่าง %1 ถูกลบจากคอมพิวเตอร์ของคุณ
UninstalledAll=%1 ถูกลบออกจากคอมพิวเตอร์ของคุณด้วยความสำเร็จ
UninstalledMost=%1 ถูกถอนการติดตั้งแล้ว%n%nบางองค์ประกอบไม่สามารถถูกลบออกได้ องค์ประกอบเหล่านี้สามารถถูกลบออกได้ด้วยตนเอง
UninstalledAndNeedsRestart=ในการถอนการติดตั้ง %1 ให้เสร็จสิ้น คอมพิวเตอร์ของคุณจะต้องถูกรีสตาร์ท%n%nคุณอยากรีสตาร์ทตอนนี้หรือไม่
UninstallDataCorrupted=ไฟล์ "%1" เสียหาย ไม่สามารถถอนการติดตั้งได้

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=ลบไฟล์ที่แชร์หรือไม่
ConfirmDeleteSharedFile2=ระบบแจ้งว่าไฟล์ที่แชร์ต่อไปนี้ไม่ได้ใช้งานอีกต่อไปโดยโปรแกรมใด ๆ คุณต้องการให้ตัวถอนการติดตั้งลบไฟล์ที่แชร์นี้หรือไม่%n%nถ้าโปรแกรมใด ๆ ยังใช้ไฟล์นี้อยู่และไฟล์ถูกลบ โปรแกรมเหล่านั้นอาจไม่ทำงานตามปกติ หากคุณไม่แน่ใจ เลือกไม่ การทิ้งไฟล์ไว้บนระบบของคุณจะไม่ทำให้เกิดอันตรายใด ๆ
SharedFileNameLabel=ชื่อไฟล์:
SharedFileLocationLabel=สถานที่:
WizardUninstalling=สถานะการถอนการติดตั้ง
StatusUninstalling=กำลังถอนการติดตั้ง %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=กำลังติดตั้ง %1
ShutdownBlockReasonUninstallingApp=กำลังถอนการติดตั้ง %1

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 เวอร์ชั่น %2
AdditionalIcons=ทางลัดเพิ่มเติม:
CreateDesktopIcon=สร้างทางลัด&เดสก์ท็อป
CreateQuickLaunchIcon=สร้างทางลัด &Quick Launch
ProgramOnTheWeb=%1 บนเว็บ
UninstallProgram=ถอนการติดตั้ง %1
LaunchProgram=เปิด %1
AssocFileExtension=&เชื่อมโยง %1 กับนามสกุลไฟล์ %2
AssocingFileExtension=กำลังเชื่อมโยง %1 กับนามสกุลไฟล์ %2...
AutoStartProgramGroupDescription=สตาร์ทอัพ:
AutoStartProgram=เริ่ม %1 โดยอัตโนมัติ
AddonHostProgramNotFound=%1 ไม่สามารถพบได้ในโฟลเดอร์ที่คุณเลือก%n%nคุณต้องการดำเนินการต่ออยู่หรือไม่
