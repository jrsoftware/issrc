; *** Inno Setup version 6.5.0+ Vietnamese messages ***
;
; Vietnamese translation by memecoder (memecoder17@gmail.com)
; Last modification date: 2023-10-26
;
[LangOptions]
LanguageName=Tiếng Việt
LanguageID=$042A
LanguageCodePage=1258
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
SetupAppTitle=Cài đặt
SetupWindowTitle=Cài đặt - %1
UninstallAppTitle=Gỡ cài đặt
UninstallAppFullTitle=Gỡ cài đặt %1

; *** Misc. common
InformationTitle=Thông tin
ConfirmTitle=Xác nhận
ErrorTitle=Lỗi

; *** SetupLdr messages
SetupLdrStartupMessage=Chương trình sẽ tiến hành cài đặt %1. Bạn có muốn tiếp tục không?
LdrCannotCreateTemp=Không thể tạo tệp tạm thời. Đã hủy quá trình cài đặt
LdrCannotExecTemp=Không thể mở tệp trong thư mục tạm. Đã hủy quá trình cài đặt
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nLỗi %2: %3
SetupFileMissing=Không tìm thấy tệp %1 trong thư mục cài đặt. Vui lòng kiểm tra lại hoặc tải một bản cài đặt mới.
SetupFileCorrupt=Các tệp cài đặt đã bị hỏng. Vui lòng tải lại một bản cài đặt mới.
SetupFileCorruptOrWrongVer=Các tệp cài đặt đã bị hỏng hoặc không tương thích với phiên bản hiện tại. Vui lòng tải lại một bản cài đặt mới.
InvalidParameter=Lệnh truyền vào không hợp lệ:%n%n%1
SetupAlreadyRunning=Chương trình cài đặt này đang được chạy rồi.
WindowsVersionNotSupported=Phần mềm này không hỗ trợ phiên bản Windows mà máy tính của bạn đang sử dụng.
WindowsServicePackRequired=Phần mềm này yêu cầu %1 Service Pack %2 hoặc mới hơn.
NotOnThisPlatform=Phần mềm này sẽ không hoạt động trên %1.
OnlyOnThisPlatform=Phần mềm này chỉ có thể hoạt động trên %1.
OnlyOnTheseArchitectures=Phần mềm này chỉ có thể cài đặt trên các phiên bản Windows hỗ trợ cấu trúc vi xử lý sau:%n%n%1
WinVersionTooLowError=Phần mềm này yêu cầu %1 phiên bản %2 hoặc mới hơn.
WinVersionTooHighError=Không thể cài phần mềm này trên %1 phiên bản %2 hoặc mới hơn.
AdminPrivilegesRequired=Bạn cần đăng nhập bằng tài khoản Quản trị viên (Administrator) để cài đặt phần mềm này.
PowerUserPrivilegesRequired=Bạn cần đăng nhập bằng tài khoản Quản trị viên (Admin) hoặc thuộc nhóm Power Users để cài đặt phần mềm này.
SetupAppRunningError=Hệ thống phát hiện %1 hiện đang chạy.%n%nVui lòng đóng tất cả các cửa sổ của ứng dụng đó lại, sau đó nhấn OK để tiếp tục, hoặc nhấn Hủy để thoát.
UninstallAppRunningError=Hệ thống phát hiện %1 hiện đang chạy.%n%nVui lòng đóng tất cả các cửa sổ của ứng dụng đó lại, sau đó nhấn OK để tiếp tục, hoặc nhấn Hủy để thoát.

; *** Startup questions
PrivilegesRequiredOverrideTitle=Chọn Chế độ Cài đặt
PrivilegesRequiredOverrideInstruction=Bạn muốn cài đặt theo chế độ nào?
PrivilegesRequiredOverrideText1=Bạn có thể cài đặt %1 cho tất cả mọi người dùng chung máy tính này (yêu cầu quyền Admin), hoặc chỉ cài cho riêng bạn.
PrivilegesRequiredOverrideText2=Bạn có thể cài đặt %1 cho riêng bạn, hoặc cài cho tất cả mọi người dùng chung máy tính này (yêu cầu quyền Admin).
PrivilegesRequiredOverrideAllUsers=Cài đặt cho &tất cả người dùng
PrivilegesRequiredOverrideAllUsersRecommended=Cài cho &tất cả người dùng (Khuyên dùng)
PrivilegesRequiredOverrideCurrentUser=Chỉ cài cho &riêng tôi
PrivilegesRequiredOverrideCurrentUserRecommended=Chỉ cài cho &riêng tôi (Khuyên dùng)

; *** Misc. errors
ErrorCreatingDir=Không thể tạo thư mục "%1"
ErrorTooManyFilesInDir=Không thể tạo tệp trong thư mục "%1" vì bên trong đã có quá nhiều tệp

; *** Setup common messages
ExitSetupTitle=Thoát Cài đặt
ExitSetupMessage=Quá trình cài đặt chưa hoàn tất. Nếu bạn thoát bây giờ, phần mềm sẽ không được cài vào máy.%n%nBạn có thể mở lại trình cài đặt này vào lúc khác.%n%nBạn có chắc chắn muốn thoát không?
AboutSetupMenuItem=&Thông tin về trình cài đặt...
AboutSetupTitle=Thông tin về trình cài đặt
AboutSetupMessage=%1 phiên bản %2%n%3%n%nTrang chủ %1:%n%4
AboutSetupNote=
TranslatorNote=Vietnamese translation by memecoder

; *** Buttons
ButtonBack=< &Quay lại
ButtonNext=&Tiếp theo >
ButtonInstall=&Cài đặt
ButtonOK=OK
ButtonCancel=Hủy
ButtonYes=&Có
ButtonYesToAll=Có với &Tất cả
ButtonNo=&Không
ButtonNoToAll=Khôn&g với Tất cả
ButtonFinish=&Hoàn tất
ButtonBrowse=&Duyệt...
ButtonWizardBrowse=D&uyệt...
ButtonNewFolder=&Tạo thư mục mới

; *** "Select Language" dialog messages
SelectLanguageTitle=Chọn ngôn ngữ cài đặt
SelectLanguageLabel=Vui lòng chọn ngôn ngữ bạn muốn sử dụng trong quá trình cài đặt.

; *** Common wizard text
ClickNext=Nhấn 'Tiếp theo' để tiếp tục, hoặc 'Hủy' để thoát.
BeveledLabel=
BrowseDialogTitle=Chọn thư mục
BrowseDialogLabel=Vui lòng chọn một thư mục trong danh sách bên dưới, sau đó nhấn OK.
NewFolderName=Thư mục mới

; *** "Welcome" wizard page
WelcomeLabel1=Chào mừng bạn đến với Cài đặt [name]
WelcomeLabel2=Chương trình sẽ cài đặt [name/ver] lên máy tính của bạn.%n%nBạn nên đóng tất cả các ứng dụng khác trước khi tiếp tục để đảm bảo quá trình cài đặt diễn ra suôn sẻ.

; *** "Password" wizard page
WizardPassword=Mật khẩu
PasswordLabel1=Bộ cài đặt này được bảo vệ bằng mật khẩu.
PasswordLabel3=Vui lòng nhập mật khẩu, sau đó nhấn 'Tiếp theo'. Lưu ý: Mật khẩu có phân biệt chữ hoa và chữ thường.
PasswordEditLabel=&Mật khẩu:
IncorrectPassword=Mật khẩu bạn nhập chưa đúng. Vui lòng thử lại nhé.

; *** "License Agreement" wizard page
WizardLicense=Thỏa thuận Sử dụng
LicenseLabel=Vui lòng đọc kỹ các thông tin quan trọng dưới đây trước khi tiếp tục.
LicenseLabel3=Vui lòng đọc Thỏa thuận Sử dụng sau đây. Bạn cần đồng ý với các điều khoản này để tiếp tục cài đặt.
LicenseAccepted=Tôi &đồng ý với các điều khoản trên
LicenseNotAccepted=Tôi &không đồng ý

; *** "Information" wizard pages
WizardInfoBefore=Thông tin
InfoBeforeLabel=Vui lòng đọc kỹ các thông tin quan trọng dưới đây trước khi tiếp tục.
InfoBeforeClickLabel=Khi bạn đã sẵn sàng tiếp tục, hãy nhấn 'Tiếp theo'.
WizardInfoAfter=Thông tin
InfoAfterLabel=Vui lòng đọc kỹ các thông tin quan trọng dưới đây trước khi tiếp tục.
InfoAfterClickLabel=Khi bạn đã sẵn sàng tiếp tục, hãy nhấn 'Tiếp theo'.

; *** "User Information" wizard page
WizardUserInfo=Thông tin người dùng
UserInfoDesc=Vui lòng điền thông tin của bạn.
UserInfoName=&Tên người dùng:
UserInfoOrg=&Tổ chức / Công ty:
UserInfoSerial=&Mã bản quyền (Serial Number):
UserInfoNameRequired=Bạn không được để trống tên.

; *** "Select Destination Location" wizard page
WizardSelectDir=Chọn vị trí cài đặt
SelectDirDesc=Bạn muốn cài đặt [name] ở đâu?
SelectDirLabel3=Chương trình sẽ được cài đặt vào thư mục dưới đây.
SelectDirBrowseLabel=Nhấn 'Tiếp theo' để tiếp tục. Nếu bạn muốn chọn một thư mục khác, hãy nhấn 'Duyệt'.
DiskSpaceGBLabel=Cần ít nhất [gb] GB dung lượng trống trên ổ đĩa.
DiskSpaceMBLabel=Cần ít nhất [mb] MB dung lượng trống trên ổ đĩa.
CannotInstallToNetworkDrive=Không thể cài đặt phần mềm lên ổ đĩa mạng.
CannotInstallToUNCPath=Không thể cài đặt phần mềm vào đường dẫn mạng (UNC).
InvalidPath=Bạn cần nhập đường dẫn đầy đủ có chứa tên ổ đĩa, ví dụ:%n%nC:\APP%n%nhoặc một đường dẫn mạng có dạng:%n%n\\may-chu\thu-muc
InvalidDrive=Ổ đĩa hoặc đường dẫn mạng bạn chọn không tồn tại hoặc không thể truy cập. Vui lòng chọn một vị trí khác.
DiskSpaceWarningTitle=Không đủ dung lượng trống
DiskSpaceWarning=Cần ít nhất %1 KB dung lượng trống để cài đặt, nhưng ổ đĩa bạn chọn hiện chỉ còn %2 KB.%n%nBạn có vẫn muốn tiếp tục không?
DirNameTooLong=Tên thư mục hoặc đường dẫn quá dài.
InvalidDirName=Tên thư mục không hợp lệ.
BadDirName32=Tên thư mục không được chứa bất kỳ ký tự nào sau đây:%n%n%1
DirExistsTitle=Thư mục đã tồn tại
DirExists=Thư mục:%n%n%1%n%nđã tồn tại. Bạn có vẫn muốn cài đè vào thư mục này không?
DirDoesntExistTitle=Thư mục không tồn tại
DirDoesntExist=Thư mục:%n%n%1%n%nhiện chưa có sẵn. Bạn có muốn tạo mới thư mục này không?

; *** "Select Components" wizard page
WizardSelectComponents=Chọn thành phần cài đặt
SelectComponentsDesc=Bạn muốn cài đặt những thành phần nào?
SelectComponentsLabel2=Đánh dấu vào các thành phần bạn muốn cài, và bỏ dấu tick ở những phần bạn không cần. Nhấn 'Tiếp theo' khi bạn đã sẵn sàng.
FullInstallation=Cài đặt đầy đủ
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Cài đặt thu gọn
CustomInstallation=Cài đặt tùy chọn
NoUninstallWarningTitle=Thành phần đã tồn tại
NoUninstallWarning=Hệ thống phát hiện các thành phần sau đã có sẵn trên máy tính của bạn:%n%n%1%n%nViệc bỏ chọn các thành phần này sẽ KHÔNG gỡ cài đặt chúng.%n%nBạn có vẫn muốn tiếp tục không?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=Các mục bạn chọn yêu cầu ít nhất [gb] GB dung lượng trống.
ComponentsDiskSpaceMBLabel=Các mục bạn chọn yêu cầu ít nhất [mb] MB dung lượng trống.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Các thiết lập bổ sung
SelectTasksDesc=Bạn muốn thực hiện thêm các tùy chọn nào?
SelectTasksLabel2=Chọn các thiết lập bổ sung mà bạn muốn thực hiện khi cài đặt [name], sau đó nhấn 'Tiếp theo'.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Chọn thư mục trên Start Menu
SelectStartMenuFolderDesc=Bạn muốn đặt biểu tượng của ứng dụng ở đâu?
SelectStartMenuFolderLabel3=Chương trình sẽ tạo các biểu tượng ứng dụng trong thư mục Start Menu dưới đây.
SelectStartMenuFolderBrowseLabel=Nhấn 'Tiếp theo' để tiếp tục. Nếu bạn muốn chọn thư mục khác, hãy nhấn 'Duyệt'.
MustEnterGroupName=Bạn cần nhập một tên cho thư mục.
GroupNameTooLong=Tên thư mục hoặc đường dẫn quá dài.
InvalidGroupName=Tên thư mục không hợp lệ.
BadGroupName=Tên thư mục không được chứa bất kỳ ký tự nào sau đây:%n%n%1
NoProgramGroupCheck2=&Không tạo thư mục trên Start Menu

; *** "Ready to Install" wizard page
WizardReady=Sẵn sàng Cài đặt
ReadyLabel1=Mọi thứ đã sẵn sàng. Chương trình sẽ bắt đầu cài đặt [name] lên máy tính của bạn.
ReadyLabel2a=Nhấn 'Cài đặt' để bắt đầu, hoặc nhấn 'Quay lại' nếu bạn muốn xem lại hoặc thay đổi các thiết lập.
ReadyLabel2b=Nhấn 'Cài đặt' để bắt đầu quá trình.
ReadyMemoUserInfo=Thông tin người dùng:
ReadyMemoDir=Vị trí cài đặt:
ReadyMemoType=Kiểu cài đặt:
ReadyMemoComponents=Các thành phần đã chọn:
ReadyMemoGroup=Thư mục Start Menu:
ReadyMemoTasks=Thiết lập bổ sung:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel2=Đang tải tệp xuống...
ButtonStopDownload=&Dừng tải xuống
StopDownload=Bạn có chắc chắn muốn dừng việc tải xuống không?
ErrorDownloadAborted=Quá trình tải xuống đã bị hủy
ErrorDownloadFailed=Tải xuống thất bại: %1 %2
ErrorDownloadSizeFailed=Không thể lấy thông tin dung lượng tệp: %1 %2
ErrorProgress=Tiến trình tải bị lỗi: %1 / %2
ErrorFileSize=Dung lượng tệp bị lỗi: mong đợi %1, nhưng thực tế là %2

; *** TExtractionWizardPage wizard page and ExtractArchive
ExtractingLabel=Đang giải nén tệp...
ButtonStopExtraction=&Dừng giải nén
StopExtraction=Bạn có chắc chắn muốn dừng việc giải nén không?
ErrorExtractionAborted=Quá trình giải nén đã bị hủy
ErrorExtractionFailed=Giải nén thất bại: %1

; *** Archive extraction failure details
ArchiveIncorrectPassword=Mật khẩu giải nén không đúng
ArchiveIsCorrupted=Tệp nén đã bị hỏng
ArchiveUnsupportedFormat=Định dạng tệp nén này không được hỗ trợ

; *** "Preparing to Install" wizard page
WizardPreparing=Đang chuẩn bị Cài đặt
PreparingDesc=Hệ thống đang chuẩn bị cài đặt [name] lên máy tính của bạn.
PreviousInstallNotCompleted=Quá trình cài đặt/gỡ cài đặt của một chương trình trước đó chưa hoàn tất. Bạn cần khởi động lại máy tính để hoàn tất quá trình đó.%n%nSau khi máy tính khởi động lại, hãy chạy lại bản cài đặt này để tiếp tục cài đặt [name].
CannotContinue=Không thể tiếp tục cài đặt. Vui lòng nhấn 'Hủy' để thoát.
ApplicationsFound=Các ứng dụng sau đang sử dụng những tệp tin cần được cập nhật. Bạn nên cho phép tự động đóng các ứng dụng này lại để tiếp tục cài đặt.
ApplicationsFound2=Các ứng dụng sau đang sử dụng những tệp tin cần được cập nhật. Bạn nên cho phép tự động đóng các ứng dụng này lại. Sau khi cài đặt xong, hệ thống sẽ cố gắng mở lại chúng cho bạn.
CloseApplications=&Tự động đóng các ứng dụng trên
DontCloseApplications=&Không đóng các ứng dụng
ErrorCloseApplications=Hệ thống không thể tự động đóng tất cả các ứng dụng. Tốt nhất là bạn nên tự đóng các ứng dụng đang dùng chung tệp tin với bản cài đặt này trước khi tiếp tục.
PrepareToInstallNeedsRestart=Cần khởi động lại máy tính. Sau khi máy tính khởi động xong, hãy chạy lại tệp này để hoàn tất cài đặt [name].%n%nBạn có muốn khởi động lại ngay bây giờ không?

; *** "Installing" wizard page
WizardInstalling=Đang cài đặt...
InstallingLabel=Vui lòng đợi trong giây lát, [name] đang được cài đặt vào máy tính của bạn.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Đã hoàn tất quá trình cài đặt [name]
FinishedLabelNoIcons=Quá trình cài đặt [name] đã thành công tốt đẹp.
FinishedLabel=Quá trình cài đặt [name] đã thành công tốt đẹp. Bạn có thể mở ứng dụng bằng các biểu tượng vừa được tạo.
ClickFinish=Nhấn 'Hoàn tất' để thoát khỏi chương trình cài đặt.
FinishedRestartLabel=Để việc cài đặt [name] hoàn tất 100%, máy tính của bạn cần được khởi động lại. Bạn có muốn khởi động lại ngay bây giờ không?
FinishedRestartMessage=Để việc cài đặt [name] hoàn tất 100%, máy tính của bạn cần được khởi động lại.%n%nBạn có muốn khởi động lại ngay bây giờ không?
ShowReadmeCheck=Có, tôi muốn xem tệp hướng dẫn (README)
YesRadio=&Có, khởi động lại máy tính ngay bây giờ
NoRadio=&Không, tôi sẽ tự khởi động lại sau
; used for example as 'Run MyProg.exe'
RunEntryExec=Mở %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Xem tệp %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Cần đưa đĩa tiếp theo vào
SelectDiskLabel2=Vui lòng đưa Đĩa số %1 vào và nhấn OK.%n%nNếu các tệp tin của đĩa này nằm ở một thư mục khác với đường dẫn bên dưới, hãy nhập lại đường dẫn cho đúng hoặc nhấn 'Duyệt'.
PathLabel=&Đường dẫn:
FileNotInDir2=Không tìm thấy tệp "%1" trong "%2". Vui lòng đưa đúng đĩa vào hoặc chọn thư mục khác.
SelectDirectoryLabel=Vui lòng chỉ định vị trí chứa đĩa tiếp theo.

; *** Installation phase messages
SetupAborted=Quá trình cài đặt chưa được hoàn tất.%n%nVui lòng khắc phục sự cố và chạy lại cài đặt sau.
AbortRetryIgnoreSelectAction=Chọn hành động
AbortRetryIgnoreRetry=&Thử lại
AbortRetryIgnoreIgnore=&Bỏ qua lỗi này và tiếp tục
AbortRetryIgnoreCancel=Hủy quá trình cài đặt
RetryCancelSelectAction=Chọn hành động
RetryCancelRetry=&Thử lại
RetryCancelCancel=Hủy

; *** Installation status messages
StatusClosingApplications=Đang đóng các ứng dụng...
StatusCreateDirs=Đang tạo các thư mục...
StatusExtractFiles=Đang giải nén tệp...
StatusDownloadFiles=Đang tải tệp xuống...
StatusCreateIcons=Đang tạo biểu tượng ứng dụng...
StatusCreateIniEntries=Đang tạo các mục cấu hình INI...
StatusCreateRegistryEntries=Đang thiết lập Registry...
StatusRegisterFiles=Đang đăng ký hệ thống tệp...
StatusSavingUninstall=Đang lưu thông tin để gỡ cài đặt sau này...
StatusRunProgram=Đang hoàn thành những bước cuối cùng...
StatusRestartingApplications=Đang khởi động lại ứng dụng...
StatusRollback=Đang khôi phục lại các thay đổi...

; *** Misc. errors
ErrorInternal2=Lỗi hệ thống nội bộ: %1
ErrorFunctionFailedNoCode=%1 đã thất bại
ErrorFunctionFailed=%1 đã thất bại; mã lỗi %2
ErrorFunctionFailedWithMessage=%1 đã thất bại; mã lỗi %2.%n%3
ErrorExecutingProgram=Không thể chạy tệp:%n%1

; *** Registry errors
ErrorRegOpenKey=Gặp lỗi khi mở khóa registry:%n%1\%2
ErrorRegCreateKey=Gặp lỗi khi tạo khóa registry:%n%1\%2
ErrorRegWriteKey=Gặp lỗi khi ghi dữ liệu vào khóa registry:%n%1\%2

; *** INI errors
ErrorIniEntry=Gặp lỗi khi tạo mục INI trong tệp "%1".

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=&Bỏ qua tệp này (Không khuyên dùng)
FileAbortRetryIgnoreIgnoreNotRecommended=&Bỏ qua lỗi này và tiếp tục (Không khuyên dùng)
SourceIsCorrupted=Tệp nguồn đã bị hỏng
SourceDoesntExist=Tệp nguồn "%1" không tồn tại
SourceVerificationFailed=Không thể xác minh tệp nguồn: %1
VerificationSignatureDoesntExist=Không tìm thấy tệp chữ ký (signature) "%1"
VerificationSignatureInvalid=Tệp chữ ký "%1" không hợp lệ
VerificationKeyNotFound=Tệp chữ ký "%1" sử dụng một khóa lạ không xác định được
VerificationFileNameIncorrect=Tên tệp không đúng
VerificationFileTagIncorrect=Thẻ (tag) của tệp không đúng
VerificationFileSizeIncorrect=Dung lượng tệp không đúng
VerificationFileHashIncorrect=Mã băm (hash) của tệp không đúng
ExistingFileReadOnly2=Không thể ghi đè lên tệp hiện tại vì tệp đó đang ở chế độ Chỉ đọc (Read-only).
ExistingFileReadOnlyRetry=&Xóa thuộc tính Chỉ đọc và thử lại
ExistingFileReadOnlyKeepExisting=&Giữ nguyên tệp hiện tại
ErrorReadingExistingDest=Đã xảy ra lỗi khi cố gắng đọc tệp hiện có:
FileExistsSelectAction=Chọn hành động
FileExists2=Tệp này đã tồn tại.
FileExistsOverwriteExisting=&Ghi đè lên tệp hiện có
FileExistsKeepExisting=&Giữ nguyên tệp hiện tại
FileExistsOverwriteOrKeepAll=&Áp dụng hành động này cho các xung đột tiếp theo
ExistingFileNewerSelectAction=Chọn hành động
ExistingFileNewer2=Tệp hiện có trên máy tính của bạn còn mới hơn cả tệp đang chuẩn bị được cài đặt.
ExistingFileNewerOverwriteExisting=&Ghi đè lên tệp hiện có
ExistingFileNewerKeepExisting=&Giữ nguyên tệp hiện tại (Khuyên dùng)
ExistingFileNewerOverwriteOrKeepAll=&Áp dụng hành động này cho các xung đột tiếp theo
ErrorChangingAttr=Đã xảy ra lỗi khi cố gắng thay đổi thuộc tính của tệp:
ErrorCreatingTemp=Đã xảy ra lỗi khi cố gắng tạo một tệp trong thư mục đích:
ErrorReadingSource=Đã xảy ra lỗi khi cố gắng đọc tệp nguồn:
ErrorCopying=Đã xảy ra lỗi khi cố gắng sao chép một tệp:
ErrorDownloading=Đã xảy ra lỗi trong quá trình tải tệp xuống:
ErrorExtracting=Đã xảy ra lỗi trong quá trình giải nén:
ErrorReplacingExistingFile=Đã xảy ra lỗi khi cố gắng thay thế tệp hiện tại:
ErrorRestartReplace=Hành động khởi động lại và thay thế (RestartReplace) thất bại:
ErrorRenamingTemp=Đã xảy ra lỗi khi cố gắng đổi tên một tệp trong thư mục cài đặt:
ErrorRegisterServer=Không thể đăng ký tệp DLL/OCX: %1
ErrorRegSvr32Failed=Lệnh RegSvr32 đã thất bại với mã thoát: %1
ErrorRegisterTypeLib=Không thể đăng ký thư viện định dạng (type library): %1

; *** Uninstall display name markings
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32-bit
UninstallDisplayNameMark64Bit=64-bit
UninstallDisplayNameMarkAllUsers=Tất cả người dùng
UninstallDisplayNameMarkCurrentUser=Người dùng hiện tại

; *** Post-installation errors
ErrorOpeningReadme=Đã xảy ra lỗi khi cố gắng mở tệp hướng dẫn (README).
ErrorRestartingComputer=Hệ thống không thể tự động khởi động lại máy tính. Vui lòng tự khởi động lại bằng tay nhé.

; *** Uninstaller messages
UninstallNotFound=Tệp "%1" không tồn tại. Không thể thực hiện gỡ cài đặt.
UninstallOpenError=Không thể mở tệp "%1". Không thể thực hiện gỡ cài đặt.
UninstallUnsupportedVer=Tệp nhật ký gỡ cài đặt "%1" có định dạng không tương thích với phiên bản gỡ cài đặt này. Không thể gỡ.
UninstallUnknownEntry=Phát hiện một mục lạ (%1) trong nhật ký gỡ cài đặt.
ConfirmUninstall=Bạn có chắc chắn muốn gỡ bỏ hoàn toàn %1 và tất cả các thành phần đi kèm không?
UninstallOnlyOnWin64=Phần mềm này chỉ có thể được gỡ cài đặt trên nền tảng Windows 64-bit.
OnlyAdminCanUninstall=Chỉ người dùng có quyền Quản trị viên (Admin) mới có thể gỡ bỏ ứng dụng này.
UninstallStatusLabel=Vui lòng đợi trong giây lát, %1 đang được gỡ bỏ khỏi máy tính của bạn.
UninstalledAll=%1 đã được gỡ bỏ hoàn toàn khỏi máy tính của bạn.
UninstalledMost=Quá trình gỡ bỏ %1 đã hoàn tất.%n%nTuy nhiên có một số thành phần không thể tự động xóa được. Bạn có thể tự tìm và xóa chúng bằng tay.
UninstalledAndNeedsRestart=Để hoàn tất việc gỡ bỏ %1, máy tính của bạn cần được khởi động lại.%n%nBạn có muốn khởi động lại ngay bây giờ không?
UninstallDataCorrupted=Tệp "%1" đã bị hỏng. Không thể thực hiện gỡ cài đặt.

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Xóa các tệp dùng chung?
ConfirmDeleteSharedFile2=Hệ thống phát hiện tệp dùng chung sau đây dường như không còn được ứng dụng nào sử dụng nữa. Bạn có muốn trình gỡ cài đặt xóa luôn tệp này không?%n%nLưu ý: Nếu vẫn còn ứng dụng khác sử dụng tệp này mà bạn lại xóa nó đi, các ứng dụng đó có thể sẽ bị lỗi. Nếu bạn không chắc chắn, hãy chọn 'Không'. Việc giữ lại tệp này trên máy tính hoàn toàn không gây hại gì cả.
SharedFileNameLabel=Tên tệp:
SharedFileLocationLabel=Vị trí:
WizardUninstalling=Trạng thái gỡ cài đặt
StatusUninstalling=Đang gỡ cài đặt %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Đang trong quá trình cài đặt %1.
ShutdownBlockReasonUninstallingApp=Đang trong quá trình gỡ cài đặt %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 phiên bản %2
AdditionalIcons=Các biểu tượng tùy chọn thêm:
CreateDesktopIcon=Tạo biểu tượng trên &Màn hình chính (Desktop)
CreateQuickLaunchIcon=Tạo biểu tượng khởi động nhanh (&Quick Launch)
ProgramOnTheWeb=Trang chủ %1
UninstallProgram=Gỡ cài đặt %1
LaunchProgram=Mở %1
AssocFileExtension=&Sử dụng %1 làm phần mềm mặc định để mở các tệp %2
AssocingFileExtension=Đang thiết lập để %1 mở các tệp %2...
AutoStartProgramGroupDescription=Khởi động cùng máy tính:
AutoStartProgram=Tự động chạy %1 khi mở máy
AddonHostProgramNotFound=Không tìm thấy %1 trong thư mục bạn vừa chọn.%n%nBạn có vẫn muốn tiếp tục không?