; *** Inno Setup version 6.5.0+ Vietnamese messages ***
;
; Vietnamese translation by memecoder (memecoder17@gmail.com)
; Reviewed and updated for Inno Setup 6.5.0+ by BlackSpirits
; Last updated: 11 July 2026
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
LdrCannotExecTemp=Không thể thực thi tệp trong thư mục tạm thời. Đã hủy quá trình cài đặt
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nLỗi %2: %3
SetupFileMissing=Tệp %1 không có trong thư mục cài đặt. Vui lòng khắc phục sự cố hoặc tải một bản sao mới của chương trình.
SetupFileCorrupt=Các tệp cài đặt đã bị hỏng. Vui lòng tải một bản sao mới của chương trình.
SetupFileCorruptOrWrongVer=Các tệp cài đặt đã bị hỏng hoặc không tương thích với phiên bản trình cài đặt này. Vui lòng khắc phục sự cố hoặc tải một bản sao mới của chương trình.
InvalidParameter=Một tham số không hợp lệ đã được truyền qua dòng lệnh:%n%n%1
SetupAlreadyRunning=Trình cài đặt đang chạy sẵn.
WindowsVersionNotSupported=Phần mềm này không hỗ trợ phiên bản Windows mà máy tính của bạn đang sử dụng.
WindowsServicePackRequired=Phần mềm này yêu cầu %1 Service Pack %2 hoặc mới hơn.
NotOnThisPlatform=Phần mềm này sẽ không hoạt động trên %1.
OnlyOnThisPlatform=Phần mềm này chỉ có thể hoạt động trên %1.
OnlyOnTheseArchitectures=Phần mềm này chỉ có thể được cài đặt trên các phiên bản Windows được thiết kế cho những kiến trúc bộ xử lý sau:%n%n%1
WinVersionTooLowError=Phần mềm này yêu cầu %1 phiên bản %2 hoặc mới hơn.
WinVersionTooHighError=Không thể cài phần mềm này trên %1 phiên bản %2 hoặc mới hơn.
AdminPrivilegesRequired=Bạn phải đăng nhập với tư cách quản trị viên khi cài đặt phần mềm này.
PowerUserPrivilegesRequired=Bạn phải đăng nhập với tư cách quản trị viên hoặc thành viên nhóm Power Users khi cài đặt phần mềm này.
SetupAppRunningError=Trình cài đặt phát hiện %1 hiện đang chạy.%n%nVui lòng đóng tất cả các phiên bản đang chạy của ứng dụng ngay bây giờ, sau đó nhấn OK để tiếp tục hoặc Hủy để thoát.
UninstallAppRunningError=Trình gỡ cài đặt phát hiện %1 hiện đang chạy.%n%nVui lòng đóng tất cả các phiên bản đang chạy của ứng dụng ngay bây giờ, sau đó nhấn OK để tiếp tục hoặc Hủy để thoát.

; *** Startup questions
PrivilegesRequiredOverrideTitle=Chọn chế độ cài đặt
PrivilegesRequiredOverrideInstruction=Chọn chế độ cài đặt
PrivilegesRequiredOverrideText1=Có thể cài đặt %1 cho tất cả người dùng (yêu cầu quyền quản trị) hoặc chỉ cho bạn.
PrivilegesRequiredOverrideText2=Có thể cài đặt %1 chỉ cho bạn hoặc cho tất cả người dùng (yêu cầu quyền quản trị).
PrivilegesRequiredOverrideAllUsers=Cài đặt cho &tất cả người dùng
PrivilegesRequiredOverrideAllUsersRecommended=Cài đặt cho &tất cả người dùng (khuyến nghị)
PrivilegesRequiredOverrideCurrentUser=Chỉ cài cho &riêng tôi
PrivilegesRequiredOverrideCurrentUserRecommended=Chỉ cài đặt cho &tôi (khuyến nghị)

; *** Misc. errors
ErrorCreatingDir=Không thể tạo thư mục "%1"
ErrorTooManyFilesInDir=Không thể tạo tệp trong thư mục "%1" vì bên trong đã có quá nhiều tệp

; *** Setup common messages
ExitSetupTitle=Thoát trình cài đặt
ExitSetupMessage=Quá trình cài đặt chưa hoàn tất. Nếu thoát bây giờ, phần mềm sẽ không được cài đặt.%n%nBạn có thể chạy lại trình cài đặt vào lúc khác để hoàn tất quá trình cài đặt.%n%nThoát trình cài đặt?
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
ButtonNoToAll=Không với tất &cả
ButtonFinish=&Hoàn tất
ButtonBrowse=&Duyệt...
ButtonWizardBrowse=D&uyệt...
ButtonNewFolder=&Tạo thư mục mới

; *** "Select Language" dialog messages
SelectLanguageTitle=Chọn ngôn ngữ cài đặt
SelectLanguageLabel=Vui lòng chọn ngôn ngữ bạn muốn sử dụng trong quá trình cài đặt.

; *** Common wizard text
ClickNext=Nhấn “Tiếp theo” để tiếp tục hoặc “Hủy” để thoát trình cài đặt.
BeveledLabel=
BrowseDialogTitle=Chọn thư mục
BrowseDialogLabel=Vui lòng chọn một thư mục trong danh sách bên dưới, sau đó nhấn OK.
NewFolderName=Thư mục mới

; *** "Welcome" wizard page
WelcomeLabel1=Chào mừng bạn đến với Trình hướng dẫn cài đặt [name]
WelcomeLabel2=Chương trình sẽ cài đặt [name/ver] lên máy tính của bạn.%n%nBạn nên đóng tất cả các ứng dụng khác trước khi tiếp tục để đảm bảo quá trình cài đặt diễn ra suôn sẻ.

; *** "Password" wizard page
WizardPassword=Mật khẩu
PasswordLabel1=Bộ cài đặt này được bảo vệ bằng mật khẩu.
PasswordLabel3=Vui lòng nhập mật khẩu, sau đó nhấn “Tiếp theo”. Lưu ý: Mật khẩu có phân biệt chữ hoa và chữ thường.
PasswordEditLabel=&Mật khẩu:
IncorrectPassword=Mật khẩu bạn nhập không đúng. Vui lòng thử lại.

; *** "License Agreement" wizard page
WizardLicense=Thỏa thuận cấp phép
LicenseLabel=Vui lòng đọc kỹ các thông tin quan trọng dưới đây trước khi tiếp tục.
LicenseLabel3=Vui lòng đọc Thỏa thuận cấp phép sau đây. Bạn phải chấp nhận các điều khoản của thỏa thuận này trước khi tiếp tục cài đặt.
LicenseAccepted=Tôi &chấp nhận thỏa thuận
LicenseNotAccepted=Tôi &không chấp nhận thỏa thuận

; *** "Information" wizard pages
WizardInfoBefore=Thông tin
InfoBeforeLabel=Vui lòng đọc kỹ các thông tin quan trọng dưới đây trước khi tiếp tục.
InfoBeforeClickLabel=Khi bạn đã sẵn sàng tiếp tục, hãy nhấn “Tiếp theo”.
WizardInfoAfter=Thông tin
InfoAfterLabel=Vui lòng đọc kỹ các thông tin quan trọng dưới đây trước khi tiếp tục.
InfoAfterClickLabel=Khi bạn đã sẵn sàng tiếp tục, hãy nhấn “Tiếp theo”.

; *** "User Information" wizard page
WizardUserInfo=Thông tin người dùng
UserInfoDesc=Vui lòng điền thông tin của bạn.
UserInfoName=&Tên người dùng:
UserInfoOrg=&Tổ chức:
UserInfoSerial=&Số sê-ri:
UserInfoNameRequired=Bạn không được để trống tên.

; *** "Select Destination Location" wizard page
WizardSelectDir=Chọn vị trí cài đặt
SelectDirDesc=Bạn muốn cài đặt [name] ở đâu?
SelectDirLabel3=Trình cài đặt sẽ cài đặt [name] vào thư mục sau.
SelectDirBrowseLabel=Nhấn “Tiếp theo” để tiếp tục. Nếu bạn muốn chọn một thư mục khác, hãy nhấn “Duyệt”.
DiskSpaceGBLabel=Cần ít nhất [gb] GB dung lượng trống trên ổ đĩa.
DiskSpaceMBLabel=Cần ít nhất [mb] MB dung lượng trống trên ổ đĩa.
CannotInstallToNetworkDrive=Không thể cài đặt phần mềm lên ổ đĩa mạng.
CannotInstallToUNCPath=Không thể cài đặt phần mềm vào đường dẫn mạng (UNC).
InvalidPath=Bạn phải nhập đường dẫn đầy đủ kèm ký tự ổ đĩa; ví dụ:%n%nC:\APP%n%nhoặc đường dẫn UNC có dạng:%n%n\\máy-chủ\thư-mục
InvalidDrive=Ổ đĩa hoặc đường dẫn mạng bạn chọn không tồn tại hoặc không thể truy cập. Vui lòng chọn một vị trí khác.
DiskSpaceWarningTitle=Không đủ dung lượng trống
DiskSpaceWarning=Cần ít nhất %1 KB dung lượng trống để cài đặt, nhưng ổ đĩa bạn chọn hiện chỉ còn %2 KB.%n%nBạn có vẫn muốn tiếp tục không?
DirNameTooLong=Tên thư mục hoặc đường dẫn quá dài.
InvalidDirName=Tên thư mục không hợp lệ.
BadDirName32=Tên thư mục không được chứa bất kỳ ký tự nào sau đây:%n%n%1
DirExistsTitle=Thư mục đã tồn tại
DirExists=Thư mục:%n%n%1%n%nđã tồn tại. Bạn có muốn tiếp tục cài đặt vào thư mục đó không?
DirDoesntExistTitle=Thư mục không tồn tại
DirDoesntExist=Thư mục:%n%n%1%n%nhiện chưa có sẵn. Bạn có muốn tạo mới thư mục này không?

; *** "Select Components" wizard page
WizardSelectComponents=Chọn thành phần cài đặt
SelectComponentsDesc=Bạn muốn cài đặt những thành phần nào?
SelectComponentsLabel2=Chọn các thành phần bạn muốn cài đặt và bỏ chọn những thành phần không muốn cài đặt. Nhấn “Tiếp theo” khi bạn đã sẵn sàng tiếp tục.
FullInstallation=Cài đặt đầy đủ
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Cài đặt thu gọn
CustomInstallation=Cài đặt tùy chọn
NoUninstallWarningTitle=Thành phần đã tồn tại
NoUninstallWarning=Trình cài đặt phát hiện các thành phần sau đã được cài đặt trên máy tính:%n%n%1%n%nViệc bỏ chọn các thành phần này sẽ không gỡ cài đặt chúng.%n%nBạn có vẫn muốn tiếp tục không?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=Các mục bạn chọn yêu cầu ít nhất [gb] GB dung lượng trống.
ComponentsDiskSpaceMBLabel=Các mục bạn chọn yêu cầu ít nhất [mb] MB dung lượng trống.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Chọn tác vụ bổ sung
SelectTasksDesc=Bạn muốn thực hiện những tác vụ bổ sung nào?
SelectTasksLabel2=Chọn các tác vụ bổ sung mà bạn muốn trình cài đặt thực hiện trong khi cài đặt [name], sau đó nhấn “Tiếp theo”.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Chọn thư mục trên Start Menu
SelectStartMenuFolderDesc=Bạn muốn trình cài đặt đặt các lối tắt của chương trình ở đâu?
SelectStartMenuFolderLabel3=Trình cài đặt sẽ tạo các lối tắt của chương trình trong thư mục Start Menu sau.
SelectStartMenuFolderBrowseLabel=Nhấn “Tiếp theo” để tiếp tục. Nếu bạn muốn chọn thư mục khác, hãy nhấn “Duyệt”.
MustEnterGroupName=Bạn cần nhập một tên cho thư mục.
GroupNameTooLong=Tên thư mục hoặc đường dẫn quá dài.
InvalidGroupName=Tên thư mục không hợp lệ.
BadGroupName=Tên thư mục không được chứa bất kỳ ký tự nào sau đây:%n%n%1
NoProgramGroupCheck2=&Không tạo thư mục trên Start Menu

; *** "Ready to Install" wizard page
WizardReady=Sẵn sàng cài đặt
ReadyLabel1=Trình cài đặt hiện đã sẵn sàng bắt đầu cài đặt [name] trên máy tính.
ReadyLabel2a=Nhấn “Cài đặt” để bắt đầu, hoặc nhấn “Quay lại” nếu bạn muốn xem lại hoặc thay đổi các thiết lập.
ReadyLabel2b=Nhấn “Cài đặt” để tiếp tục cài đặt.
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
ErrorProgress=Tiến trình không hợp lệ: %1 trên %2
ErrorFileSize=Kích thước tệp không hợp lệ: dự kiến %1, tìm thấy %2

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
WizardPreparing=Đang chuẩn bị cài đặt
PreparingDesc=Hệ thống đang chuẩn bị cài đặt [name] lên máy tính của bạn.
PreviousInstallNotCompleted=Quá trình cài đặt/gỡ cài đặt của một chương trình trước đó chưa hoàn tất. Bạn cần khởi động lại máy tính để hoàn tất quá trình đó.%n%nSau khi máy tính khởi động lại, hãy chạy lại bản cài đặt này để tiếp tục cài đặt [name].
CannotContinue=Không thể tiếp tục cài đặt. Vui lòng nhấn “Hủy” để thoát.
ApplicationsFound=Các ứng dụng sau đang sử dụng những tệp cần được trình cài đặt cập nhật. Bạn nên cho phép trình cài đặt tự động đóng các ứng dụng này.
ApplicationsFound2=Các ứng dụng sau đang sử dụng những tệp cần được trình cài đặt cập nhật. Bạn nên cho phép trình cài đặt tự động đóng các ứng dụng này. Sau khi cài đặt hoàn tất, trình cài đặt sẽ cố gắng khởi động lại các ứng dụng.
CloseApplications=&Tự động đóng các ứng dụng trên
DontCloseApplications=&Không đóng các ứng dụng
ErrorCloseApplications=Trình cài đặt không thể tự động đóng tất cả ứng dụng. Bạn nên đóng tất cả ứng dụng đang sử dụng những tệp cần được trình cài đặt cập nhật trước khi tiếp tục.
PrepareToInstallNeedsRestart=Cần khởi động lại máy tính. Sau khi máy tính khởi động xong, hãy chạy lại tệp này để hoàn tất cài đặt [name].%n%nBạn có muốn khởi động lại ngay bây giờ không?

; *** "Installing" wizard page
WizardInstalling=Đang cài đặt
InstallingLabel=Vui lòng đợi trong giây lát, [name] đang được cài đặt vào máy tính của bạn.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Đã hoàn tất quá trình cài đặt [name]
FinishedLabelNoIcons=Trình cài đặt đã hoàn tất việc cài đặt [name] trên máy tính.
FinishedLabel=Trình cài đặt đã hoàn tất việc cài đặt [name] trên máy tính. Có thể khởi chạy ứng dụng bằng cách chọn các lối tắt đã cài đặt.
ClickFinish=Nhấn “Hoàn tất” để thoát khỏi chương trình cài đặt.
FinishedRestartLabel=Để hoàn tất việc cài đặt [name], trình cài đặt phải khởi động lại máy tính. Bạn có muốn khởi động lại ngay bây giờ không?
FinishedRestartMessage=Để hoàn tất việc cài đặt [name], trình cài đặt phải khởi động lại máy tính.%n%nBạn có muốn khởi động lại ngay bây giờ không?
ShowReadmeCheck=Có, tôi muốn xem tệp hướng dẫn (README)
YesRadio=&Có, khởi động lại máy tính ngay bây giờ
NoRadio=&Không, tôi sẽ tự khởi động lại sau
; used for example as 'Run MyProg.exe'
RunEntryExec=Mở %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Xem tệp %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Cần đưa đĩa tiếp theo vào
SelectDiskLabel2=Vui lòng đưa Đĩa số %1 vào và nhấn OK.%n%nNếu các tệp của đĩa này nằm ở một thư mục khác với đường dẫn bên dưới, hãy nhập lại đường dẫn cho đúng hoặc nhấn “Duyệt”.
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
StatusCreateIcons=Đang tạo lối tắt...
StatusCreateIniEntries=Đang tạo các mục cấu hình INI...
StatusCreateRegistryEntries=Đang thiết lập Registry...
StatusRegisterFiles=Đang đăng ký tệp...
StatusSavingUninstall=Đang lưu thông tin để gỡ cài đặt sau này...
StatusRunProgram=Đang hoàn thành những bước cuối cùng...
StatusRestartingApplications=Đang khởi động lại ứng dụng...
StatusRollback=Đang khôi phục lại các thay đổi...

; *** Misc. errors
ErrorInternal2=Lỗi nội bộ: %1
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
FileAbortRetryIgnoreSkipNotRecommended=&Bỏ qua tệp này (không khuyến nghị)
FileAbortRetryIgnoreIgnoreNotRecommended=&Bỏ qua lỗi này và tiếp tục (không khuyến nghị)
SourceIsCorrupted=Tệp nguồn đã bị hỏng
SourceDoesntExist=Tệp nguồn "%1" không tồn tại
SourceVerificationFailed=Không thể xác minh tệp nguồn: %1
VerificationSignatureDoesntExist=Không tìm thấy tệp chữ ký (signature) "%1"
VerificationSignatureInvalid=Tệp chữ ký "%1" không hợp lệ
VerificationKeyNotFound=Tệp chữ ký "%1" sử dụng khóa không xác định
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
ExistingFileNewer2=Tệp hiện có mới hơn tệp mà trình cài đặt đang cố cài đặt.
ExistingFileNewerOverwriteExisting=&Ghi đè lên tệp hiện có
ExistingFileNewerKeepExisting=&Giữ nguyên tệp hiện tại (khuyến nghị)
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
ErrorRegisterTypeLib=Không thể đăng ký thư viện kiểu: %1

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
ErrorRestartingComputer=Trình cài đặt không thể khởi động lại máy tính. Vui lòng thực hiện thủ công.

; *** Uninstaller messages
UninstallNotFound=Tệp "%1" không tồn tại. Không thể thực hiện gỡ cài đặt.
UninstallOpenError=Không thể mở tệp "%1". Không thể thực hiện gỡ cài đặt
UninstallUnsupportedVer=Tệp nhật ký gỡ cài đặt "%1" có định dạng mà phiên bản trình gỡ cài đặt này không nhận dạng được. Không thể gỡ cài đặt
UninstallUnknownEntry=Phát hiện một mục lạ (%1) trong nhật ký gỡ cài đặt
ConfirmUninstall=Bạn có chắc chắn muốn gỡ bỏ hoàn toàn %1 và tất cả các thành phần đi kèm không?
UninstallOnlyOnWin64=Phần mềm này chỉ có thể được gỡ cài đặt trên nền tảng Windows 64-bit.
OnlyAdminCanUninstall=Chỉ người dùng có quyền quản trị viên mới có thể gỡ bỏ ứng dụng này.
UninstallStatusLabel=Vui lòng đợi trong giây lát, %1 đang được gỡ bỏ khỏi máy tính của bạn.
UninstalledAll=%1 đã được gỡ bỏ hoàn toàn khỏi máy tính của bạn.
UninstalledMost=Đã hoàn tất gỡ cài đặt %1.%n%nKhông thể xóa một số thành phần. Bạn có thể xóa chúng theo cách thủ công.
UninstalledAndNeedsRestart=Để hoàn tất việc gỡ bỏ %1, máy tính của bạn cần được khởi động lại.%n%nBạn có muốn khởi động lại ngay bây giờ không?
UninstallDataCorrupted=Tệp "%1" đã bị hỏng. Không thể thực hiện gỡ cài đặt

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Xóa tệp dùng chung?
ConfirmDeleteSharedFile2=Hệ thống phát hiện tệp dùng chung sau đây dường như không còn được ứng dụng nào sử dụng nữa. Bạn có muốn trình gỡ cài đặt xóa luôn tệp này không?%n%nLưu ý: Nếu vẫn còn ứng dụng khác sử dụng tệp này mà bạn lại xóa nó đi, các ứng dụng đó có thể sẽ bị lỗi. Nếu bạn không chắc chắn, hãy chọn “Không”. Việc giữ lại tệp này trên máy tính hoàn toàn không gây hại gì cả.
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
AdditionalIcons=Lối tắt bổ sung:
CreateDesktopIcon=Tạo lối tắt trên &màn hình nền
CreateQuickLaunchIcon=Tạo biểu tượng khởi động nhanh (&Quick Launch)
ProgramOnTheWeb=Trang chủ %1
UninstallProgram=Gỡ cài đặt %1
LaunchProgram=Mở %1
AssocFileExtension=&Liên kết %1 với phần mở rộng tệp %2
AssocingFileExtension=Đang liên kết %1 với phần mở rộng tệp %2...
AutoStartProgramGroupDescription=Khởi động:
AutoStartProgram=Tự động khởi động %1
AddonHostProgramNotFound=Không tìm thấy %1 trong thư mục bạn vừa chọn.%n%nBạn có vẫn muốn tiếp tục không?
