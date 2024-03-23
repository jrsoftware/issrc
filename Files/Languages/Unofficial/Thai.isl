; Nathawat Chalapinyo
; Nathawat-LTSC@outlook.co.th
; *** Inno Setup version 6.1.0+ Thai messages ***
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
LanguageName=Thai
LanguageID=$041E
LanguageCodePage=874
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
SetupAppTitle=การติดตั้ง
SetupWindowTitle=การติดตั้ง - %1
UninstallAppTitle=ถอนการติดตั้ง
UninstallAppFullTitle=%1 ถอนการติดตั้ง

; *** Misc. common
InformationTitle=คำอธิบาย
ConfirmTitle=ยืนยัน
ErrorTitle=ผิดพลาด

; *** SetupLdr messages
SetupLdrStartupMessage=กระบวนการนี้จะเริ่มการติดตั้ง %1. คุณต้องการติดตั้งหรือไม่?
LdrCannotCreateTemp=ไม่สามารถ สร้างโฟลเดอร์ข้อมูลชั่วคราวได้ การติดตั้งถูกยกเลิก
LdrCannotExecTemp=ไม่สามารถ ใช้ไฟล์ใน ที่เก็บข้อมูลชั่วคราวได้ การติดตั้งถูกยกเลิก
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nข้อผิดพลาด %2: %3
SetupFileMissing=ไฟล์ %1 ในโฟลเดอร์ที่ติดตั้งไม่สมบูรณ์ โปรดตรวจสอบไฟล์การติดตั้งแล้วลองอีกครั้ง
SetupFileCorrupt=ไฟล์การติดตั้งเสียหาย โปรดตรวจสอบไฟล์การติดตั้งแล้วลองอีกครั้ง
SetupFileCorruptOrWrongVer=เวอร์ชั่นไฟล์การติดตั้งไม่ถูกต้องหรือเข้ากันไม่ได้ โปรดตรวจสอบไฟล์การติดตั้งแล้วลองอีกครั้ง
InvalidParameter=พารามิเตอร์ผิดพลาดที่ชุดคำสั่ง:%n%n%1
SetupAlreadyRunning=การติดตั้งกำลังทำงานอยู่
WindowsVersionNotSupported=โปรแกรมไม่รองรับเวอร์ชั่นของวินโดวน์ที่ใช้อยู่
WindowsServicePackRequired=โปรแกรมต้องการ %1 Service Pack %2 หรือใหม่กว่า.
NotOnThisPlatform=โปรแกรมนี้ ไม่ทำงานบน %1.
OnlyOnThisPlatform=โปรแกรมนี้ ต้องทำงานบน %1.
OnlyOnTheseArchitectures=โปรแกรมนี้สามารถติดตั้งบน วินโดวน์รุ่นที่ออกแบบมาสำหรับสถาปัตยกรรมต่อไปนี้:%n%n%1
WinVersionTooLowError=โปรแกรมนี้ต้องการ %1 เวอร์ชั่น %2 หรือใหม่กว่า
WinVersionTooHighError=โปรแกรมนี้ไม่สามารถติดตั้งบน %1 เวอร์ชั่น %2 หรือใหม่กว่า
AdminPrivilegesRequired=คุณต้องใช้ผู้ใช้ของผู้ดูแลระบบ เช่น administrator เมื่อติดตั้งโปรแกรมนี้
PowerUserPrivilegesRequired=คุณต้องเข้าสู่ระบบ ด้วยผู้ใช้ผู้ดูแลระบบ เช่น administrator หรือกลุ่มผู้ใช้งาน Power Users เมื่อติดตั้งโปรแกรมนี้
SetupAppRunningError=โปรแกรมติดตั้งพบว่า %1 กำลังทำงานอยู่%n%nโปรดปิดโปรแกรม และกด ตกลง เพื่อดำเนินการต่อ หรือ ยกเลิก เพื่อจบการทำงาน
UninstallAppRunningError=โปรแกรมถอนการติดตั้งพบว่า %1 กำลังทำงานอยู่%n%nโปรดปิดโปรแกรม และกด ตกลง เพื่อดำเนินการต่อ หรือ ยกเลิก เพื่อจบการทำงาน

; *** Startup questions
PrivilegesRequiredOverrideTitle=เลือกโหมดการติดตั้ง
PrivilegesRequiredOverrideInstruction=เลือกโหมดการติดตั้ง
PrivilegesRequiredOverrideText1=%1 สามารถติดตั้งสำหรับผู้ใช้ทุกคน (ต้องการสิทธิ์ ระดับผู้ดูแลระบบ), หรือเฉพาะคุณเท่านั้น
PrivilegesRequiredOverrideText2=%1 สามารถติดตั้งสำหรับคุณเท่านั้น หรือสำหรับผู้ใช้ทุกคน (ต้องการสิทธิ์ ระดับผู้ดูแลระบบ)
PrivilegesRequiredOverrideAllUsers=ติดตั้งสำหรับ &ผู้ใช้ทั้งหมด
PrivilegesRequiredOverrideAllUsersRecommended=ติดตั้งสำหรับ &ผู้ใช้ทั้งหมด (แนะนำ)
PrivilegesRequiredOverrideCurrentUser=ติดตั้งสำหรับ &ฉันเท่านั้น
PrivilegesRequiredOverrideCurrentUserRecommended=ติดตั้งสำหรับ &ฉันเท่านั้น (แนะนำ)

; *** Misc. errors
ErrorCreatingDir=โปรแกรมติดตั้งไม่สามารถสร้างโฟลเดอร์ "%1" ได้
ErrorTooManyFilesInDir=ไม่สามารถสร้างไฟล์ในโฟลเดอร์ "%1" เพราะมีไฟล์จำนวนมากเกินไป

; *** Setup common messages
ExitSetupTitle=ออกจาก การติดตั้ง
ExitSetupMessage=การติดตั้งยังไม่สมบูรณ์ ถ้าคุณจบการทำงานในเวลานี้ โปรแกรมจะไม่ถูกติดตั้ง%n%nคุณอาจต้องทำการติดตั้งโปรแกรมใหม่อีกครั้ง เพื่อให้การติดตั้งสมบูรณ์%n%nคุณต้องการจบการติดตั้ง?
AboutSetupMenuItem=&เกี่ยวกับ การติดตั้ง...
AboutSetupTitle=เกี่ยวกับ การติดตั้ง
AboutSetupMessage=%1 เวอร์ชั่น %2%n%3%n%n%1 โฮมเพจ:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< &ย้อนกลับ
ButtonNext=&ทำต่อ >
ButtonInstall=&ติดตั้ง
ButtonOK=ตกลง
ButtonCancel=ยกเลิก
ButtonYes=&ใช่
ButtonYesToAll=ใช่สำหรับ &ทั้งหมด
ButtonNo=&ไม่
ButtonNoToAll=ไม่สำหรับ &ทั้งหมด
ButtonFinish=&เสร็จ
ButtonBrowse=&เรียกดู...
ButtonWizardBrowse=&เรียกดู...
ButtonNewFolder=&สร้างโฟลเดอร์ใหม่

; *** "Select Language" dialog messages
SelectLanguageTitle=เลือกภาษาที่ต้องการ
SelectLanguageLabel=เลือกภาษา ที่ต้องการใช้ระหว่างการติดตั้ง

; *** Common wizard text
ClickNext=กด ทำต่อ เพื่อดำเนินการต่อ หรือ กด ยกเลิก เพื่อจบการทำงาน
BeveledLabel=
BrowseDialogTitle=เลือกโฟลเดอร์
BrowseDialogLabel=เลือกโฟลเดอร์ในรายการด้างล่างจากนั้น ให้กด ตกลง
NewFolderName=โฟลเดอร์ใหม่

; *** "Welcome" wizard page
WelcomeLabel1=ยินดีต้อนรับสู่ [name] โปรแกรมติดตั้ง
WelcomeLabel2=สิ่งนี้จะติดตั้ง [name/ver] บนคอมพิวเตอร์ของคุณ%n%nขอแนะนำให้คุณปิดแอปพลิเคชันอื่นๆ ทั้งหมดก่อนดำเนินการต่อ

; *** "Password" wizard page
WizardPassword=รหัสผ่าน
PasswordLabel1=การติดตั้งนี้ ถูกป้องกันด้วยรหัสผ่าน
PasswordLabel3=กรุณาใส่รหัสผ่านที่ใช้ติดตั้งโปรแกรม จากนั้น ให้กด ทำต่อ เพื่อทำงาน (รหัสผ่านต้องตรงตามตัวพิมพ์เล็กและใหญ่)
PasswordEditLabel=&รหัสผ่าน:
IncorrectPassword=รหัสผ่านที่คุณใส่ไม่ถูกต้อง กรุณาลองอีกครั้ง

; *** "License Agreement" wizard page
WizardLicense=ข้อตกลง
LicenseLabel=โปรดอ่านข้อมูลสำคัญต่อไปนี้ ก่อนดำเนินการต่อ
LicenseLabel3=โปรดอ่านข้อตกลงต่อไปนี้ คุณต้องยอมรับเงื่อนไขของข้อตกลงนี้ก่อนที่จะดำเนินการติดตั้งต่อไป
LicenseAccepted=ฉัน &ยอมรับ ข้อตกลง
LicenseNotAccepted=ฉัน &ไม่ยอมรับ ข้อตกลง

; *** "Information" wizard pages
WizardInfoBefore=คำอธิบาย
InfoBeforeLabel=โปรดอ่านข้อมูลสำคัญ ก่อนดำเนินการต่อ
InfoBeforeClickLabel=เมื่อคุณพร้อมดำเนินการติดตั้งต่อ ให้กด ทำต่อ
WizardInfoAfter=คำอธิบาย
InfoAfterLabel=โปรดอ่านข้อมูลสำคัญ ก่อนดำเนินการต่อ
InfoAfterClickLabel=เมื่อคุณพร้อมดำเนินการติดตั้งต่อ ให้กด ทำต่อ

; *** "User Information" wizard page
WizardUserInfo=ข้อมูล ผู้ใช้
UserInfoDesc=โปรด กรอกข้อมูลของคุณ
UserInfoName=&ชื่อผู้ใช้:
UserInfoOrg=&หน่วยงาน:
UserInfoSerial=&หมายเลขซีเรียล:
UserInfoNameRequired=คุณต้องใส่ชื่อ

; *** "Select Destination Location" wizard page
WizardSelectDir=เลือกตำแหน่งปลายทาง
SelectDirDesc=ควรติดตั้ง [name] ที่ไหน?
SelectDirLabel3=โปรแกรมติดตั้งจะติดตั้ง [name] ลงในโฟลเดอร์ต่อไปนี้
SelectDirBrowseLabel=หากต้องการดำเนินการต่อ ให้กด ถัดไป หากคุณต้องการเลือกโฟลเดอร์อื่น ให้กด เรียกดู
DiskSpaceGBLabel=ต้องการพื้นที่ว่างในดิสก์อย่างน้อย [gb] GB
DiskSpaceMBLabel=ต้องการพื้นที่ว่างในดิสก์อย่างน้อย [mb] MB
CannotInstallToNetworkDrive=โปรแกรมติดตั้งไม่สามารถติดตั้งไปยังไดร์ฟเครือข่ายได้
CannotInstallToUNCPath=โปรแกรมติดตั้งไม่สามารถติดตั้งไปยังตำแหน่ง UNC ได้
InvalidPath=คุณต้องป้อนเส้นทางแบบเต็มด้วยอักษรระบุไดรฟ์ ตัวอย่างเช่น:%n%nC:\APP%n%nหรือเส้นทาง UNC แบบ:%n%n\\server\share
InvalidDrive=ไดร์ฟหรือการแชร์ที่คุณเลือกไม่มีอยู่ หรือไม่สามารถเข้าถึงได้ โปรดเลือกรายการอื่น
DiskSpaceWarningTitle=พื้นที่ดิสก์ไม่เพียงพอ
DiskSpaceWarning=การติดตั้งต้องการอย่างน้อย %1 KB ในการติดตั้ง แต่ไดร์ฟที่เลือกมี %2 KB เท่านั้น%n%nคุณต้องการดำเนินการต่อหรือไม่?
DirNameTooLong=ชื่อโฟลเดอร์หรือเส้นทางยาวเกินไป
InvalidDirName=ชื่อโฟลเดอร์ไม่ถูกต้อง
BadDirName32=ชื่อโฟลเดอร์ต้องไม่มีอักขระใดๆ ต่อไปนี้:%n%n%1
DirExistsTitle=มีโฟลเดอร์อยู่แล้ว
DirExists=โฟลเดอร์:%n%n%1%n%nมีอยู่แล้ว คุณต้องการติดตั้งลงในโฟลเดอร์นี้เลยหรือไม่?
DirDoesntExistTitle=ไม่พบโฟลเดอร์
DirDoesntExist=โฟลเดอร์:%n%n%1%n%nไม่มีอยู่ในขณะนี้ คุณต้องการสร้างโฟลเดอร์นี้เลยหรือไม่?

; *** "Select Components" wizard page
WizardSelectComponents=เลือกส่วนประกอบ
SelectComponentsDesc=ส่วนประกอบไหนที่คุณต้องการติดตั้ง?
SelectComponentsLabel2=ทำเครื่องหมายเลือกส่วนประกอบที่คุณต้องการติดตั้ง; ลบเครื่องหมายส่วนประกอบที่คุณไม่ต้องการ กดทำต่อ เมื่อคุณเลือกเสร็จแล้ว
FullInstallation=ติดตั้งทุกอย่าง
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=ติดตั้งน้อยที่สุด
CustomInstallation=กำหนดส่วนประกอบเอง
NoUninstallWarningTitle=ส่วนประกอบเดิมมีอยู่แล้ว
NoUninstallWarning=โปรแกรมติดตั้งตรวจพบว่าส่วนประกอบต่อไปนี้ได้รับการติดตั้งบนคอมพิวเตอร์ของคุณแล้ว:%n%n%1%n%nการยกเลิกการเลือกคอมโพเนนต์เหล่านี้จะไม่เป็นการถอนการติดตั้ง%n%nคุณต้องการดำเนินการต่อหรือไม่?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=การเลือกปัจจุบันต้องการพื้นที่ดิสก์อย่างน้อย [gb] GB
ComponentsDiskSpaceMBLabel=การเลือกปัจจุบันต้องการพื้นที่ดิสก์อย่างน้อย [mb] MB

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=เลือกการทำงานเพิ่มเติม
SelectTasksDesc=กำหนดการทำงานเพิ่มเติม
SelectTasksLabel2=เลือกการทำงานเพิ่มเติม เพื่อการติดตั้งโปรแกรม [name] และ กด ทำต่อ

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=เลือกโฟลเดอร์เมนูเริ่ม
SelectStartMenuFolderDesc=โปรแกรมติดตั้ง ควรวางทางลัดของโปรแกรมไว้ที่ไหน?
SelectStartMenuFolderLabel3=โปรแกรมติดตั้ง จะสร้างทางลัดของโปรแกรมในโฟลเดอร์ Start Menu ต่อไปนี้
SelectStartMenuFolderBrowseLabel=เพื่อดำเนินการต่อ กด ต่อไป หากคุณต้องการเลือกโฟลเดอร์อื่น ให้กด เรียกดู
MustEnterGroupName=คุณต้องใส่ชื่อโฟลเดอร์
GroupNameTooLong=ชื่อโฟลเดอร์หรือเส้นทางยาวเกินไป
InvalidGroupName=ชื่อโฟลเดอร์ไม่ถูกต้อง
BadGroupName=ชื่อโฟลเดอร์ต้องไม่มีอักขระใดๆ ต่อไปนี้:%n%n%1
NoProgramGroupCheck2=&ไม่ต้องสร้างโฟลเดอร์เมนูเริ่ม

; *** "Ready to Install" wizard page
WizardReady=พร้อมที่จะติดตั้ง
ReadyLabel1=โปรแกรมติดตั้งพร้อมที่จะเริ่มติดตั้ง [name] บนคอมพิวเตอร์ของคุณแล้ว
ReadyLabel2a=กด ติดตั้ง เพื่อดำเนินการติดตั้งต่อ หรือกด ย้อนกลับ หากคุณต้องการตรวจสอบหรือเปลี่ยนแปลงการตั้งค่าใดๆ
ReadyLabel2b=กด ติดตั้ง เพื่อดำเนินการติดตั้งต่อ
ReadyMemoUserInfo=ข้อมูลผู้ใช้:
ReadyMemoDir=ตำแหน่งปลายทาง:
ReadyMemoType=ประเภทการติดตั้ง:
ReadyMemoComponents=ส่วนประกอบที่เลือก:
ReadyMemoGroup=โฟลเดอร์เมนูเริ่ม:
ReadyMemoTasks=งานเพิ่มเติม:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel=กำลังดาวน์โหลดไฟล์เพิ่มเติม...
ButtonStopDownload=&หยุดการดาวน์โหลด
StopDownload=คุณแน่ใจหรือไม่ว่าต้องการหยุดการดาวน์โหลด?
ErrorDownloadAborted=การดาวน์โหลดถูกยกเลิก
ErrorDownloadFailed=การดาวน์โหลดล้มเหลว: %1 %2
ErrorDownloadSizeFailed=การรับขนาดล้มเหลว: %1 %2
ErrorFileHash1=แฮชไฟล์ล้มเหลว: %1
ErrorFileHash2=แฮชไฟล์ไม่ถูกต้อง: คาดว่าเป็น %1, พบ %2
ErrorProgress=ความคืบหน้าไม่ถูกต้อง: %1 จาก %2
ErrorFileSize=ขนาดไฟล์ไม่ถูกต้อง: คาดไว้ %1, พบ %2

; *** "Preparing to Install" wizard page
WizardPreparing=กำลังเตรียมการติดตั้ง
PreparingDesc=โปรแกรมติดตั้ง กำลังเตรียมการติดตั้ง [name] บนคอมพิวเตอร์ของคุณ
PreviousInstallNotCompleted=การติดตั้ง/การลบโปรแกรมก่อนหน้านี้ไม่เสร็จสมบูรณ์ คุณจะต้องรีสตาร์ทคอมพิวเตอร์เพื่อทำการติดตั้งให้เสร็จสมบูรณ์ %n%nหลังจากรีสตาร์ทคอมพิวเตอร์แล้ว ให้เรียกใช้โปรแกรมติดตั้งอีกครั้งเพื่อทำการติดตั้ง [name] ให้เสร็จสมบูรณ์
CannotContinue=การติดตั้งไม่สามารถดำเนินการต่อได้ โปรดกดยกเลิก เพื่อออก
ApplicationsFound=แอปพลิเคชันต่อไปนี้กำลังใช้ไฟล์ที่ต้องอัปเดตโดยโปรแกรมติดตั้ง ขอแนะนำให้คุณอนุญาตให้โปรแกรมติดตั้งปิดแอปพลิเคชันเหล่านี้โดยอัตโนมัติ
ApplicationsFound2=แอปพลิเคชันต่อไปนี้กำลังใช้ไฟล์ที่ต้องอัปเดตโดยโปรแกรมติดตั้ง ขอแนะนำให้คุณอนุญาตให้โปรแกรมติดตั้งปิดแอปพลิเคชันเหล่านี้โดยอัตโนมัติ หลังจากการติดตั้งเสร็จสิ้น โปรแกรมติดตั้งจะพยายามรีสตาร์ทแอปพลิเคชัน
CloseApplications=&ปิดแอปพลิเคชันโดยอัตโนมัติ
DontCloseApplications=&อย่าปิดแอปพลิเคชัน
ErrorCloseApplications=โปรแกรมติดตั้งไม่สามารถปิดแอปพลิเคชันทั้งหมดโดยอัตโนมัติ ขอแนะนำให้คุณปิดแอปพลิเคชันทั้งหมดโดยใช้ไฟล์ที่ต้องอัปเดตโดยโปรแกรมติดตั้งก่อนดำเนินการต่อ
PrepareToInstallNeedsRestart=การติดตั้งต้องรีสตาร์ทเครื่องคอมพิวเตอร์ของคุณ หลังจากรีสตาร์ทคอมพิวเตอร์แล้ว ให้เรียกใช้โปรแกรมติดตั้งอีกครั้งเพื่อทำการติดตั้ง [name] ให้เสร็จสมบูรณ์%n%nคุณต้องการเริ่มระบบใหม่เดี๋ยวนี้หรือไม่

; *** "Installing" wizard page
WizardInstalling=กำลังติดตั้ง
InstallingLabel=โปรดรอสักครู่ในขณะที่โปรแกรมติดตั้งติดตั้ง [name] บนคอมพิวเตอร์ของคุณ

; *** "Setup Completed" wizard page
FinishedHeadingLabel=การติดตั้ง [name] เสร็จสมบูรณ์
FinishedLabelNoIcons=การติดตั้งโปรแกรม [name] บนคอมพิวเตอร์ เสร็จสมบูรณ์
FinishedLabel=การติดตั้งโปรแกรม [name] บนคอมพิวเตอร์เสร็จสมบูรณ์ แอปพลิเคชันอาจเปิดใช้งานโดยเลือกทางลัดที่ติดตั้งไว้
ClickFinish=กด เสร็จ เพื่อจบการติดตั้ง
FinishedRestartLabel=เพื่อให้การติดตั้ง [name] เสร็จสมบูรณ์ โปรแกรมติดตั้งต้องเริ่มต้นระบบเครื่องคอมพิวเตอร์ของคุณใหม่ คุณต้องการเริ่มต้นใหม่ตอนนี้หรือไม่?
FinishedRestartMessage=เพื่อให้การติดตั้ง [name] เสร็จสิ้น โปรแกรมติดตั้งจะต้องเริ่มต้นะบบเครื่องคอมพิวเตอร์ของคุณใหม่ %n%nคุณต้องการเริ่มระบบใหม่เดี๋ยวนี้หรือไม่?
ShowReadmeCheck=ใช่ ฉันต้องการดูไฟล์ README
YesRadio=&ใช่ เริ่มต้นระบบคอมพิวเตอร์ใหม่ทันที
NoRadio=&ไม่ ฉันจะเริ่มต้นระบบคอมพิวเตอร์ในภายหลัง
; used for example as 'Run MyProg.exe'
RunEntryExec=เรียกใช้ %1
; used for example as 'View Readme.txt'
RunEntryShellExec=ดู %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=โปรแกรมติดตั้งต้องการดิสก์ถัดไป
SelectDiskLabel2=โปรดใส่ดิสก์ %1 และ กดตกลง%n%nหากพบไฟล์ในดิสก์นี้ในโฟลเดอร์อื่นนอกเหนือจากที่แสดงด้านล่าง ให้ป้อนเส้นทางที่ถูกต้องหรือกดเรียกดู
PathLabel=&เส้นทาง:
FileNotInDir2=ไม่พบไฟล์ "%1" ใน "%2" โปรดใส่ดิสก์ที่ถูกต้องหรือเลือกโฟลเดอร์อื่น
SelectDirectoryLabel=โปรดระบุตำแหน่งของดิสก์ถัดไป

; *** Installation phase messages
SetupAborted=การติดตั้งไม่เสร็จสมบูรณ์%n%nโปรดแก้ไขปัญหาและเรียกใช้การติดตั้งอีกครั้ง
AbortRetryIgnoreSelectAction=เลือกการดำเนินการ
AbortRetryIgnoreRetry=&ลองอีกครั้ง
AbortRetryIgnoreIgnore=&ละเว้นข้อผิดพลาดและดำเนินการต่อ
AbortRetryIgnoreCancel=ยกเลิกการติดตั้ง

; *** Installation status messages
StatusClosingApplications=กำลังปิดแอปพลิเคชัน...
StatusCreateDirs=กำลังสร้างไดเร็กทอรี...
StatusExtractFiles=กำลังแตกไฟล์...
StatusCreateIcons=กำลังสร้างทางลัด...
StatusCreateIniEntries=กำลังสร้างรายการ INI...
StatusCreateRegistryEntries=กำลังสร้างรายการรีจิสตรี...
StatusRegisterFiles=กำลังลงทะเบียนไฟล์...
StatusSavingUninstall=กำลังบันทึกข้อมูลการถอนการติดตั้ง...
StatusRunProgram=กำลังเสร็จสิ้นการติดตั้ง...
StatusRestartingApplications=กำลังเริ่มแอปพลิเคชันใหม่...
StatusRollback=กำลังย้อนกลับการเปลี่ยนแปลง...

; *** Misc. errors
ErrorInternal2=ข้อผิดพลาดภายใน: %1
ErrorFunctionFailedNoCode=%1 ล้มเหลว
ErrorFunctionFailed=%1 ล้มเหลว; รหัส %2
ErrorFunctionFailedWithMessage=%1 ล้มเหลว; รหัส %2.%n%3
ErrorExecutingProgram=ไม่สามารถเรียกใช้ไฟล์:%n%1

; *** Registry errors
ErrorRegOpenKey=ข้อผิดพลาดในการเปิดคีย์รีจิสทรี:%n%1\%2
ErrorRegCreateKey=เกิดข้อผิดพลาดในการสร้างคีย์รีจิสทรี:%n%1\%2
ErrorRegWriteKey=ข้อผิดพลาดในการเขียนคีย์รีจิสทรี:%n%1\%2

; *** INI errors
ErrorIniEntry=เกิดข้อผิดพลาดในการสร้างรายการ INI ในไฟล์ "%1".

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=&ข้ามไฟล์นี้ (ไม่แนะนำ)
FileAbortRetryIgnoreIgnoreNotRecommended=&ละเว้นข้อผิดพลาดและดำเนินการต่อ (ไม่แนะนำ)
SourceIsCorrupted=ไฟล์ต้นฉบับเสียหาย
SourceDoesntExist=ไม่มีไฟล์ต้นฉบับ "%1"
ExistingFileReadOnly2=ไม่สามารถแทนที่ไฟล์ที่มีอยู่ได้ เนื่องจากไฟล์ถูกทำเครื่องหมายเป็นแบบอ่านอย่างเดียว
ExistingFileReadOnlyRetry=&ลบแอตทริบิวต์แบบอ่านอย่างเดียวแล้วลองอีกครั้ง
ExistingFileReadOnlyKeepExisting=&เก็บไฟล์ที่มีอยู่
ErrorReadingExistingDest=เกิดข้อผิดพลาดขณะพยายามอ่านไฟล์ที่มีอยู่:
FileExistsSelectAction=เลือกการกระทำ
FileExists2=มีไฟล์อยู่แล้ว
FileExistsOverwriteExisting=&เขียนทับไฟล์ที่มีอยู่
FileExistsKeepExisting=&เก็บไฟล์ที่มีอยู่
FileExistsOverwriteOrKeepAll=&ทำเช่นนี้สำหรับความขัดแย้งครั้งต่อไป
ExistingFileNewerSelectAction=เลือกการดำเนินการ
ExistingFileNewer2=ไฟล์ที่มีอยู่ใหม่กว่าไฟล์ติดตั้งที่พยายามติดตั้ง
ExistingFileNewerOverwriteExisting=&เขียนทับไฟล์ที่มีอยู่
ExistingFileNewerKeepExisting=&เก็บไฟล์ที่มีอยู่ (แนะนำ)
ExistingFileNewerOverwriteOrKeepAll=&ทำเช่นนี้สำหรับความขัดแย้งครั้งต่อไป
ErrorChangingAttr=เกิดข้อผิดพลาดขณะพยายามเปลี่ยนแอตทริบิวต์ของไฟล์ที่มีอยู่:
ErrorCreatingTemp=เกิดข้อผิดพลาดขณะพยายามสร้างไฟล์ในไดเรกทอรีปลายทาง:
ErrorReadingSource=เกิดข้อผิดพลาดขณะพยายามอ่านไฟล์ต้นฉบับ:
ErrorCopying=เกิดข้อผิดพลาดขณะพยายามคัดลอกไฟล์:
ErrorReplacingExistingFile=เกิดข้อผิดพลาดขณะพยายามแทนที่ไฟล์ที่มีอยู่:
ErrorRestartReplace=เริ่มต้นใหม่แทนที่ ล้มเหลว:
ErrorRenamingTemp=เกิดข้อผิดพลาดขณะพยายามเปลี่ยนชื่อไฟล์ในไดเรกทอรีปลายทาง:
ErrorRegisterServer=ไม่สามารถลงทะเบียน DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 ล้มเหลวด้วยรหัสออก %1
ErrorRegisterTypeLib=ไม่สามารถลงทะเบียนไลบรารีประเภท: %1

; *** Uninstall display name markings
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32-บิต
UninstallDisplayNameMark64Bit=64-บิต
UninstallDisplayNameMarkAllUsers=ผู้ใช้ทั้งหมด
UninstallDisplayNameMarkCurrentUser=ผู้ใช้ปัจจุบัน

; *** Post-installation errors
ErrorOpeningReadme=เกิดข้อผิดพลาดขณะพยายามเปิดไฟล์ README
ErrorRestartingComputer=โปรแกรมติดตั้งไม่สามารถเริ่มต้นคอมพิวเตอร์ใหม่ได้ โปรดดำเนินการด้วยตนเอง

; *** Uninstaller messages
UninstallNotFound=ไม่มีไฟล์ "%1" อยู่ ไม่สามารถถอนการติดตั้ง
UninstallOpenError=ไม่สามารถเปิดไฟล์ "%1" ได้ ไม่สามารถถอนการติดตั้ง
UninstallUnsupportedVer=ไฟล์บันทึกการถอนการติดตั้ง "%1" อยู่ในรูปแบบที่โปรแกรมถอนการติดตั้งเวอร์ชันนี้ไม่รู้จัก จึงไม่สามารถถอนการติดตั้งได้
UninstallUnknownEntry=พบรายการที่ไม่รู้จัก (%1) ในบันทึกการถอนการติดตั้ง
ConfirmUninstall=คุณแน่ใจหรือไม่ว่าต้องการลบ %1 และส่วนประกอบทั้งหมดออกทั้งหมด?
UninstallOnlyOnWin64=โปรแกรมถอนการติดตั้งนี้สามารถถอนการติดตั้งได้บน Windows 64 บิตเท่านั้น
OnlyAdminCanUninstall=โปรแกรมถอนการติดตั้งนี้สามารถถอนการติดตั้งได้โดยผู้ใช้ที่มีสิทธิ์ของผู้ดูแลระบบเท่านั้น
UninstallStatusLabel=โปรดรอในขณะที่ %1 ถูกลบออกจากคอมพิวเตอร์ของคุณ
UninstalledAll=%1 ถูกลบออกจากคอมพิวเตอร์ของคุณเรียบร้อยแล้ว
UninstalledMost=%1 ถอนการติดตั้งเสร็จสมบูรณ์ %n%nองค์ประกอบบางอย่างไม่สามารถลบออกได้ สิ่งเหล่านี้สามารถลบออกได้ด้วยตนเอง
UninstalledAndNeedsRestart=ในการถอนการติดตั้ง %1 ให้เสร็จสมบูรณ์ คอมพิวเตอร์ของคุณจะต้อเริ่มต้นระบบใหม่ %n%nคุณต้องการเริ่มต้นระบบใหม่เดี๋ยวนี้หรือไม่?
UninstallDataCorrupted="%1" เสียหาย ไม่สามารถถอนการติดตั้ง

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=ลบไฟล์ที่ใช้ร่วมกัน?
ConfirmDeleteSharedFile2=ระบบระบุว่าไฟล์ที่ใช้ร่วมกันต่อไปนี้ไม่มีการใช้งานโดยโปรแกรมใดๆ อีกต่อไป คุณต้องการถอนการติดตั้งเพื่อลบไฟล์ที่ใช้ร่วมกันนี้หรือไม่%n%nหากโปรแกรมใดยังคงใช้ไฟล์นี้อยู่และถูกลบออก โปรแกรมเหล่านั้นอาจทำงานไม่ถูกต้อง หากคุณไม่แน่ใจ ให้เลือก ไม่ การทิ้งไฟล์ไว้ในระบบของคุณจะไม่ก่อให้เกิดอันตรายใดๆ
SharedFileNameLabel=ชื่อไฟล์:
SharedFileLocationLabel=ตำแหน่งที่ตั้ง:
WizardUninstalling=สถานะ ถอนการติดตั้ง
StatusUninstalling=กำลังถอนการติดตั้ง %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=กำลังติดตั้ง %1.
ShutdownBlockReasonUninstallingApp=ถอนการติดตั้ง %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 เวอร์ชัน %2
AdditionalIcons=ทางลัดเพิ่มเติม:
CreateDesktopIcon=สร้างทางลัด &เดสก์ท็อป
CreateQuickLaunchIcon=สร้างทางลัด &เปิดใช้ด่วน
ProgramOnTheWeb=%1 บนเว็บ
UninstallProgram=ถอนการติดตั้ง %1
LaunchProgram=เปิดใช้ %1
AssocFileExtension=&เชื่อมโยง %1 กับนามสกุลไฟล์ %2
AssocingFileExtension=กำลังเชื่อมโยง %1 กับนามสกุลไฟล์ %2...
AutoStartProgramGroupDescription=การเริ่มต้น:
AutoStartProgram=เริ่ม %1 โดยอัตโนมัติ
AddonHostProgramNotFound=ไม่พบ %1 ในโฟลเดอร์ที่คุณเลือก%n%nคุณต้องการดำเนินการต่อหรือไม่?
