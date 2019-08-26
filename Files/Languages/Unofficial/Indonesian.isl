; *** Inno Setup version 6.0.0+ Indonesian messages ***
;
; Untuk mendownload terjemahan kontribusi-pengguna dari file ini, buka:
;   http://www.jrsoftware.org/files/istrans/
;
; Alih bahasa oleh: Resen (resen.zhu@gmail.com)
;
; Catatan: Ketika menerjemahkan teks ini, jangan menambahkan titik (.) di akhir
; pesan, karena di akhir pesan Inno Setup menambahkan titik tersebut
; secara otomatis (menambahkan titik dapat menampilkan dua titik.)

[LangOptions]
; Tiga entri berikut ini sangat penting. Pastikan untuk membaca dan
; memahami topik 'bagian [LangOptions]' di berkas bantuan.
LanguageName=Bahasa Indonesia
LanguageID=$0421
LanguageCodePage=0
; Jika bahasa yang Anda terjemahkan membutuhkan jenis atau ukuran font
; khusus, hilangkan komentar salah satu dari entri berikut dan ubah sesuai kebutuhan.
;DialogFontName=
;DialogFontSize=8
;WelcomeFontName=Verdana
;WelcomeFontSize=12
;TitleFontName=Arial
;TitleFontSize=29
;CopyrightFontName=Arial
;CopyrightFontSize=8

[Messages]

; *** Judul aplikasi
SetupAppTitle=Instalatur
SetupWindowTitle=Instalatur - %1
UninstallAppTitle=Uninstal
UninstallAppFullTitle=Uninstal %1

; *** Umum lain-lain
InformationTitle=Informasi
ConfirmTitle=Konfirmasi
ErrorTitle=Kesalahan

; *** Pesan SetupLdr
SetupLdrStartupMessage=Program ini akan menginstal %1. Apakah Anda ingin melanjutkan?
LdrCannotCreateTemp=Tidak dapat membuat berkas sementara. Instalasi dibatalkan
LdrCannotExecTemp=Tidak dapat mengeksekusi berkas dalam folder sementara. Instalasi dibatalkan
HelpTextNote=

; *** Pesan kesalahan startup
LastErrorMessage=%1.%n%nKesalahan %2: %3
SetupFileMissing=Berkas %1 hilang dari direktori instalasi. Silakan perbaiki masalah atau dapatkan salinan baru dari program ini.
SetupFileCorrupt=Berkas instalatur rusak. Silakan dapatkan salin baru dari program ini.
SetupFileCorruptOrWrongVer=Berkas instalatur rusak, atau tidak cocok dengan versi Instalatur ini. Silakan perbaiki masalah atau dapatkan salinan baru dari program ini.
InvalidParameter=Parameter tidak valid pada baris perintah:%n%n%1
SetupAlreadyRunning=Instalatur sedang berjalan.
WindowsVersionNotSupported=Program ini tidak mendukung versi Windows yang dijalankan oleh komputer Anda.
WindowsServicePackRequired=Program ini membutuhkan %1 Service Pack %2 atau yang terbaru.
NotOnThisPlatform=Program ini tidak dapat berjalan di %1.
OnlyOnThisPlatform=Program ini harus dijalankan di %1.
OnlyOnTheseArchitectures=Program ini hanya dapat diinstal di versi Windows yang dirancang untuk arsitektur prosesor berikut:%n%n%1
WinVersionTooLowError=Program ini membutuhkan %1 versi %2 atau yang terbaru.
WinVersionTooHighError=Program ini tidak dapat diinstal di %1 versi %2 atau yang terbaru.
AdminPrivilegesRequired=Anda harus masuk sebagai administrator ketika menginstal program ini.
PowerUserPrivilegesRequired=Anda harus masuk sebagai administrator atau sebagai anggota dari grup Power Users ketika menginstal program ini.
SetupAppRunningError=Instalatur mendeteksi %1 sedang berjalan.%n%nSilakan tutup semuanya sekarang, lalu klik OK untuk melanjutkan, atau Batal untuk keluar.
UninstallAppRunningError=Uninstal mendeteksi %1 sedang berjalan.%n%nSilakan tutup semuanya sekarang, lalu klik OK untuk melanjutkan, atau Batal untuk keluar.

; *** Pertanyaan startup
PrivilegesRequiredOverrideTitle=Pilih Mode Instalasi Instalatur
PrivilegesRequiredOverrideInstruction=Pilih mode instalasi
PrivilegesRequiredOverrideText1=%1 dapat diinstal untuk semua pengguna (membutuhkan hak administratif), atau hanya untuk Anda.
PrivilegesRequiredOverrideText2=%1 dapat diinstal hanya untuk Anda, atau untuk semua pengguna (membutuhkan hak administratif).
PrivilegesRequiredOverrideAllUsers=Instal untuk &semua pengguna
PrivilegesRequiredOverrideAllUsersRecommended=Instal untuk &semua pengguna (direkomendasikan)
PrivilegesRequiredOverrideCurrentUser=Instal untuk &saya saja
PrivilegesRequiredOverrideCurrentUserRecommended=Instal untuk &saya saja (direkomendasikan)

; *** Kesalahan lain-lain
ErrorCreatingDir=Instalatur tidak dapat membuat direktori "%1"
ErrorTooManyFilesInDir=Tidak dapat membuat berkas di direktori "%1" karena memuat terlalu banyak berkas

; *** Pesan umum instalatur
ExitSetupTitle=Keluar dari Instalatur
ExitSetupMessage=Instalasi tidak selesai. Bila Anda keluar sekarang, program tidak akan terinstal.%n%nAnda dapat menjalankan Instalatur kembali di lain waktu untuk menyelesaikan instalasi tersebut.%n%nKeluar dari Instalatur?
AboutSetupMenuItem=&Tentang Instalatur...
AboutSetupTitle=Tentang Instalatur
AboutSetupMessage=%1 versi %2%n%3%n%n%1 halaman awal:%n%4
AboutSetupNote=
TranslatorNote=

; *** Tombol
ButtonBack=< &Kembali
ButtonNext=&Lanjut >
ButtonInstall=&Instal
ButtonOK=OK
ButtonCancel=&Batal
ButtonYes=&Ya
ButtonYesToAll=Ya untuk &Semua
ButtonNo=&Tidak
ButtonNoToAll=T&idak untuk Semua
ButtonFinish=&Selesai
ButtonBrowse=&Jelajahi...
ButtonWizardBrowse=J&elajahi...
ButtonNewFolder=&Buat folder baru

; *** Pesan dialog "Pilih Bahasa"
SelectLanguageTitle=Pilih Bahasa Instalatur
SelectLanguageLabel=Pilih bahasa yang akan digunakan selama instalasi.

; *** Teks panduan umum
ClickNext=Klik Lanjut untuk melanjutkan, atau Batal untuk keluar dari Instalatur.
BeveledLabel=
BrowseDialogTitle=Jelajahi Folder
BrowseDialogLabel=Pilih folder dalam daftar di bawah ini, lalu klik OK.
NewFolderName=Folder Baru

; *** Halaman panduan "Selamat Datang"
WelcomeLabel1=Selamat datang di Panduan Instalatur [name]
WelcomeLabel2=Program ini akan menginstal [name/ver] di komputer Anda.%n%nDisarankan untuk menutup semua aplikasi yang sedang berjalan sebelum melanjutkan.

; *** Halaman panduan "Kata Sandi"
WizardPassword=Kata Sandi
PasswordLabel1=Instalasi ini dilindungi kata sandi.
PasswordLabel3=Silakan masukkan kata sandi, lalu klik Lanjut untuk melanjutkan. Kata sandi bersifat case-sensitive.
PasswordEditLabel=&Kata Sandi:
IncorrectPassword=Kata sandi yang Anda masukkan salah. Silakan coba lagi.

; *** Halaman panduan "Perjanjian Lisensi"
WizardLicense=Perjanjian Lisensi
LicenseLabel=Harap cermati informasi penting berikut ini sebelum melanjutkan.
LicenseLabel3=Harap cermati Perjanjian Lisensi berikut ini. Anda harus menyetujui ketentuan perjanjian ini sebelum melanjutkan dengan instalasi.
LicenseAccepted=Saya &setuju
LicenseNotAccepted=Saya &tidak setuju

; *** Halaman panduan "Informasi"
WizardInfoBefore=Informasi
InfoBeforeLabel=Harap cermati informasi penting berikut ini sebelum melanjutkan.
InfoBeforeClickLabel=Bila Anda siap untuk melanjutkan dengan Instalatur, klik Lanjut.
WizardInfoAfter=Informasi
InfoAfterLabel=Harap cermati informasi penting berikut ini sebelum melanjutkan.
InfoAfterClickLabel=Bila Anda siap untuk melanjutkan dengan Instalatur, klik Lanjut.

; *** Halaman panduan "Informasi Pengguna"
WizardUserInfo=Informasi pengguna
UserInfoDesc=Silakan masukkan informasi Anda.
UserInfoName=&Nama pengguna:
UserInfoOrg=&Organisasi:
UserInfoSerial=&Nomor Serial:
UserInfoNameRequired=Anda harus memasukkan nama.

; *** Halaman panduan "Pilih Lokasi Tujuan"
WizardSelectDir=Pilih Lokasi Tujuan
SelectDirDesc=Di manakah [name] akan diinstal?
SelectDirLabel3=Instalatur akan menginstal [name] ke dalam folder berikut.
SelectDirBrowseLabel=Untuk melanjutkan, klik Lanjut. Bila Anda ingin memilih folder lain, klik Jelajahi.
DiskSpaceMBLabel=Dibutuhkan setidaknya [mb] MB ruang disk kosong.
CannotInstallToNetworkDrive=Instalatur tidak dapat menginstal ke drive jaringan.
CannotInstallToUNCPath=Instalatur tidak dapat menginstal ke lokasi UNC.
InvalidPath=Anda harus memasukkan lokasi lengkap dengan huruf drive; contoh:%n%nC:\APP%n%natau lokasi UNC dalam bentuk:%n%n\\server\share
InvalidDrive=Drive atau UNC yang Anda pilih tidak ada atau tidak dapat diakses. Silakan pilih yang lain.
DiskSpaceWarningTitle=Ruang Disk Tidak Mencukupi
DiskSpaceWarning=Instalatur membutuhkan setidaknya %1 KB ruang kosong untuk menginstal, tapi pada drive yang dipilih hanya tersedia sebesar %2 KB.%n%nApakah Anda ingin melanjutkan?
DirNameTooLong=Nama folder terlalu panjang.
InvalidDirName=Nama folder tidak valid.
BadDirName32=Nama folder tidak dapat diisi dengan karakter berikut:%n%n%1
DirExistsTitle=Folder Sudah Ada
DirExists=Folder:%n%n%1%n%nsudah ada. Apakah Anda ingin menginstal di folder tersebut?
DirDoesntExistTitle=Folder Tidak Ada
DirDoesntExist=Folder:%n%n%1%n%ntidak ada. Apakah Anda ingin membuat folder tersebut?

; *** Halaman panduan "Pilih Komponen"
WizardSelectComponents=Pilih Komponen
SelectComponentsDesc=Komponen manakah yang akan diinstal?
SelectComponentsLabel2=Pilih komponen yang Anda ingin instal; hapus komponen yang Anda tidak ingin instal. Klik Lanjut bila Anda siap untuk melanjutkan.
FullInstallation=Instalasi penuh
; Jika memungkinkan, jangan menerjemahkan 'Compact' sebagai 'Minimal' (maksud saya 'Minimal' dalam bahasa Anda)
CompactInstallation=Instalasi padat
CustomInstallation=Instalasi lainnya
NoUninstallWarningTitle=Komponen Sudah Ada
NoUninstallWarning=Instalatur mendeteksi komponen berikut telah terinstal di komputer Anda:%n%n%1%n%nTidak memilih komponen ini tidak akan menghapus instalannya.%n%nApakah Anda ingin melanjutkan?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Pilihan saat ini membutuhkan setidaknya [mb] MB ruang disk.

; *** Halaman panduan "Pilih Perintah Tambahan"
WizardSelectTasks=Pilih Perintah Tambahan
SelectTasksDesc=Perintah tambahan manakah yang akan dijalankan?
SelectTasksLabel2=Pilih perintah tambahan yang Anda ingin Instalatur jalankan ketika menginstal [name], lalu klik Lanjut.

; *** Halaman panduan "Pilih Folder Start Menu"
WizardSelectProgramGroup=Pilih Folder Start Menu
SelectStartMenuFolderDesc=Di manakah Instalatur harus menempatkan shortcut program?
SelectStartMenuFolderLabel3=Instalatur akan membuat shortcut program di folder Start Menu berikut.
SelectStartMenuFolderBrowseLabel=Untuk melanjutkan, klik Lanjut. Bila Anda ingin memilih folder lain, klik Jelajahi.
MustEnterGroupName=Anda harus memasukkan nama folder.
GroupNameTooLong=Nama folder atau lokasi terlalu panjang.
InvalidGroupName=Nama folder tidak valid.
BadGroupName=Nama folder tidak dapat diisi dengan karakter berikut:%n%n%1
NoProgramGroupCheck2=&Jangan buat folder Start Menu

; *** Halaman panduan "Siap untuk Diinstal"
WizardReady=Siap untuk Diinstal
ReadyLabel1=Instalatur sekarang siap untuk mulai menginstal [name] di komputer Anda.
ReadyLabel2a=Klik Instal untuk melanjutkan instalasi, atau klik Kembali bila Anda ingin melihat ulang atau mengubah pengaturan.
ReadyLabel2b=Klik Instal untuk melanjutkan instalasi.
ReadyMemoUserInfo=Informasi pengguna:
ReadyMemoDir=Lokasi tujuan:
ReadyMemoType=Tipe instalasi:
ReadyMemoComponents=Komponen yang dipilih:
ReadyMemoGroup=Folder Start Menu:
ReadyMemoTasks=Perintah tambahan:

; *** Halaman panduan "Bersiap untuk Menginstal"
WizardPreparing=Bersiap untuk Menginstal
PreparingDesc=Instalatur sedang bersiap untuk menginstal [name] di komputer Anda.
PreviousInstallNotCompleted=Instalasi/penghapusan program sebelumnya tidak selesai. Anda harus me-restart komputer Anda untuk menyelesaikan instalasi tersebut.%n%nSetelah me-restart komputer Anda, jalankan kembali Instalatur untuk menyelesaikan instalasi [name].
CannotContinue=Instalatur tidak dapat lanjut. Silakan klik Batal untuk keluar.
ApplicationsFound=Aplikasi berikut sedang menggunakan berkas yang akan diperbarui oleh Instalatur. Disarankan agar Anda mengizinkan Instalatur untuk menutup aplikasi ini secara otomatis.
ApplicationsFound2=Aplikasi berikut sedang menggunakan berkas yang akan diperbarui oleh Instalatur. Disarankan agar Anda mengizinkan Instalatur untuk menutup aplikasi ini secara otomatis. Setelah instalasi selesai, Instalatur akan mencoba untuk me-restart aplikasi.
CloseApplications=&Otomatis tutup aplikasi.
DontCloseApplications=&Jangan tutup aplikasi.
ErrorCloseApplications=Instalatur tidak dapat menutup semua aplikasi secara otomatis. Disarankan agar Anda menutup semua aplikasi yang menggunakan berkas yang akan diperbarui oleh Instalatur sebelum melanjutkan.

; *** Halaman panduan "Menginstal"
WizardInstalling=Menginstal
InstallingLabel=Silakan tunggu sementara Instalatur menginstal [name] di komputer Anda.

; *** Halaman panduan "Instalasi Selesai"
FinishedHeadingLabel=Menyelesaikan Panduan Instalatur [name]
FinishedLabelNoIcons=Instalatur telah berhasil menginstal [name] di komputer Anda.
FinishedLabel=Instalatur telah berhasil menginstal [name] di komputer Anda. Aplikasi tersebut dapat dijalankan dengan memilih shortcut yang sudah diinstal.
ClickFinish=Klik Selesai untuk keluar dari Instalatur.
FinishedRestartLabel=Untuk menyelesaikan instalasi [name], Instalatur harus me-restart komputer Anda. Apakah Anda ingin me-restart sekarang?
FinishedRestartMessage=Untuk menyelesaikan instalasi [name], Instalatur harus me-restart komputer Anda.%n%nApakah Anda ingin me-restart sekarang?
ShowReadmeCheck=Ya, saya ingin melihat berkas README
YesRadio=&Ya, restart komputer sekarang
NoRadio=&Tidak, saya akan me-restart komputer pada lain waktu
; digunakan misalnya sebagai 'Jalankan MyProg.exe'
RunEntryExec=Jalankan %1
; digunakan misalnya sebagai 'Lihat Readme.txt'
RunEntryShellExec=Lihat %1

; *** Hal "Instalasi Membutuhkan Disk Selanjutnya"
ChangeDiskTitle=Instalatur Membutuhkan Disk Selanjutnya
SelectDiskLabel2=Silakan masukkan disk %1 dan klik OK.%n%nBila berkas dalam disk ini dapat ditemukan di folder selain yang ditampilkan di bawah, masukkan lokasi yang benar atau klik Jelajahi.
PathLabel=&Lokasi:
FileNotInDir2=Berkas "%1" tidak dapat ditemukan di "%2". Silakan masukkan disk yang benar atau pilih folder lain.
SelectDirectoryLabel=Silakan tentukan lokasi disk berikutnya.

; *** Pesan fase instalasi
SetupAborted=Instalasi tidak selesai.%n%Silakan perbaiki kesalahan dan jalankan Instalatur kembali.
AbortRetryIgnoreSelectAction=Pilih aksi
AbortRetryIgnoreRetry=&Coba lagi
AbortRetryIgnoreIgnore=&Abaikan kesalahan dan lanjutkan
AbortRetryIgnoreCancel=Batalkan instalasi

; *** Pesan status Instalasi
StatusClosingApplications=Menutup aplikasi...
StatusCreateDirs=Membuat direktori...
StatusExtractFiles=Mengekstrak berkas...
StatusCreateIcons=Membuat shortcut...
StatusCreateIniEntries=Membuat entri INI...
StatusCreateRegistryEntries=Membuat entri registry...
StatusRegisterFiles=Mendaftarkan berkas...
StatusSavingUninstall=Menyimpan informasi uninstal...
StatusRunProgram=Menyelesaikan instalasi...
StatusRestartingApplications=Me-restart aplikasi...
StatusRollback=Membatalkan perubahan...

; *** Kesalahan lain-lain
ErrorInternal2=Kesalahan internal: %1
ErrorFunctionFailedNoCode=%1 gagal
ErrorFunctionFailed=%1 gagal; kode %2
ErrorFunctionFailedWithMessage=%1 gagal; kode %2.%n%3
ErrorExecutingProgram=Tidak dapat mengeksekusi berkas:%n%1

; *** Kesalahan registry
ErrorRegOpenKey=Gagal membuka registry key:%n%1\%2
ErrorRegCreateKey=Gagal membuat registry key:%n%1\%2
ErrorRegWriteKey=Gagal menulis registry key:%n%1\%2

; *** Kesalahan INI
ErrorIniEntry=Gagal membuat entri INI pada berkas "%1".

; *** Kesalahan penyalinan berkas
FileAbortRetryIgnoreSkipNotRecommended=&Lewati berkas ini (tidak disarankan)
FileAbortRetryIgnoreIgnoreNotRecommended=&Abaikan kesalahan dan lanjutkan (tidak disarankan)
SourceIsCorrupted=Berkas sumber rusak
SourceDoesntExist=Berkas sumber "%1" tidak ada
ExistingFileReadOnly2=Berkas yang ada tidak dapat diganti karena ditandai read-only.
ExistingFileReadOnlyRetry=&Hapus atribut read-only dan coba lagi
ExistingFileReadOnlyKeepExisting=&Pertahankan berkas yang ada
ErrorReadingExistingDest=Terjadi kesalahan ketika mencoba untuk membaca berkas:
FileExists=Berkas sudah ada.%n%nApakah Anda ingin menimpa berkas tersebut?
ExistingFileNewer=Berkas yang ada lebih baru daripada yang coba diinstal oleh Instalatur. Disarankan agar Anda mempertahankan berkas yang ada.%n%nApakah Anda ingin mempertahankan berkas tersebut?
ErrorChangingAttr=Terjadi kesalahan ketika mencoba untuk mengubah atribut berkas:
ErrorCreatingTemp=Terjadi kesalahan ketika mencoba untuk membuat berkas di lokasi tujuan:
ErrorReadingSource=Terjadi kesalahan ketika mencoba untuk membaca berkas sumber:
ErrorCopying=Terjadi kesalahan ketika mencoba untuk menyalin berkas:
ErrorReplacingExistingFile=Terjadi kesalahan ketika mencoba untuk menimpa berkas:
ErrorRestartReplace=RestartReplace gagal:
ErrorRenamingTemp=Terjadi kesalahan ketika mencoba untuk merubah nama berkas di lokasi tujuan:
ErrorRegisterServer=Tidak dapat mendaftarkan berkas DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 gagal dengan kode %1
ErrorRegisterTypeLib=Tidak dapat mendaftarkan type library: %1

; *** Tanda nama tampilan uninstal
; digunakan misalnya sebagai 'Program Saya (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; digunakan misalnya sebagai 'Program Saya (32-bit, Semua pengguna)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32-bit
UninstallDisplayNameMark64Bit=64-bit
UninstallDisplayNameMarkAllUsers=Semua pengguna
UninstallDisplayNameMarkCurrentUser=Pengguna saat ini

; *** Kesalahan pasca instalasi
ErrorOpeningReadme=Terjadi kesalahan ketika mencoba untuk membuka berkas README.
ErrorRestartingComputer=Instalatur tidak dapat me-restart komputer. Silakan lakukan ini secara manual.

; *** Pesan uninstaler
UninstallNotFound=Berkas "%1" tidak ada. Tidak dapat uninstal.
UninstallOpenError=Berkas "%1" tidak dapat dibuka. Tidak dapat uninstal
UninstallUnsupportedVer=Berkas log uninstal "%1" dalam format yang tidak dikenali oleh versi uninstaler ini. Tidak dapat uninstal
UninstallUnknownEntry=Entri tidak diketahui (%1) ditemukan pada log uninstal
ConfirmUninstall=Apakah Anda yakin ingin menghapus %1 beserta semua komponennya?
UninstallOnlyOnWin64=Instalasi ini hanya dapat diuninstal di Windows versi 64-bit.
OnlyAdminCanUninstall=Instalasi ini hanya bisa diuninstal oleh pengguna dengan hak administratif.
UninstallStatusLabel=Silakan tunggu sementara %1 dihapus dari komputer Anda.
UninstalledAll=%1 berhasil dihapus dari komputer Anda.
UninstalledMost=Uninstal %1 selesai.%n%nBeberapa berkas tidak dapat dihapus. Berkas tersebut dapat dihapus secara manual.
UninstalledAndNeedsRestart=Untuk menyelesaikan proses uninstal %1, komputer Anda harus di-restart. %n%nApakah Anda ingin me-restart sekarang?
UninstallDataCorrupted=Berkas "%1" rusak. Tidak dapat uninstal

; *** Pesan fase uninstal
ConfirmDeleteSharedFileTitle=Hapus Berkas Bersama?
ConfirmDeleteSharedFile2=Sistem menunjukkan berkas bersama berikut tidak lagi digunakan oleh program apapun. Apakah Anda ingin meng-uninstal berkas bersama?%n%nBila ada program lain yang masih menggunakan berkas ini dan berkas ini dihapus, program tersebut mungkin tidak dapat berfungsi dengan baik. Bila Anda tidak yakin, pilih Tidak. Membiarkan berkas pada sistem Anda tidak akan menyebabkan kerusakan apapun.
SharedFileNameLabel=Nama Berkas:
SharedFileLocationLabel=Lokasi:
WizardUninstalling=Status Uninstal
StatusUninstalling=Meng-uninstal %1...

; *** Alasan blok shutdown
ShutdownBlockReasonInstallingApp=Menginstal %1
ShutdownBlockReasonUninstallingApp=Meng-uninstal %1

; Pesan lainnya di bawah ini tidak digunakan oleh Instalatur sendiri, tetapi bila Anda
; menggunakannya di skrip Anda, Anda mungkin ingin menerjemahkannya.

[CustomMessages]

NameAndVersion=%1 versi %2
AdditionalIcons=Shortcut tambahan:
CreateDesktopIcon=Buat sebuah shortcut di &Desktop
CreateQuickLaunchIcon=Buat sebuah shortcut di &Quick Launch
ProgramOnTheWeb=%1 di Web
UninstallProgram=Uninstal %1
LaunchProgram=Jalankan %1
AssocFileExtension=&Asosiasikan %1 dengan ekstensi berkas %2
AssocingFileExtension=Mengasosiasikan %1 dengan ekstensi berkas %2...
AutoStartProgramGroupDescription=Startup:
AutoStartProgram=Otomatis menjalankan %1
AddonHostProgramNotFound=%1 tidak dapat ditemukan di folder yang Anda pilih.%n%nApakah Anda ingin melanjutkan?
