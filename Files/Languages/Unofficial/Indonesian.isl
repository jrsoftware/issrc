; *** Inno Setup versi 5.5.3+ Bahasa Indonesia ***
;
; Untuk mengunduh terjemahan kontribusi-pemakai dari berkas ini, buka:
;   http://www.jrsoftware.org/files/istrans/
;
; Alih bahasa oleh: Muchammad Ulil Wafa (mozaik.tm@gmail.com)
;   http://muchammadulilwafa.blogspot.com/ 
;	http://mozaik-tkj.blogspot.com/
;	http://smadaver.com/profile/?u=84302
;
; Catatan: Ketika menterjemahkan teks ini, jangan masukkan titik (.) di akhir
; pesan yang memang tidak memiliki titik, karena Inno Setup memasukkan titik
; secara otomatis (menambahkan titik dapat menyebabkan tampilnya dua titik)

[LangOptions]
; Tiga baris di bawah ini sangat penting. Pastikan membaca dan mengerti 
; topik bagian '[LangOptions]' pada berkas bantuan.
LanguageName=Indonesian
LanguageID=$0421
LanguageCodePage=0
; Jika bahasa yang anda terjemahkan membutuhkan huruf atau ukuran khusus,
; hapus tanda titik koma pada entri di bawah dan ubah sesuai kebutuhan.
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
SetupAppTitle=Instalasi
SetupWindowTitle=Instalasi - %1
UninstallAppTitle=Penghapusan
UninstallAppFullTitle=Penghapusan %1

; *** Jenis jenis umum
InformationTitle=Informasi
ConfirmTitle=Konfirmasi
ErrorTitle=Kesalahan

; *** Pesan SetupLdr
SetupLdrStartupMessage=Program ini akan memasang %1. Lanjutkan?
LdrCannotCreateTemp=Tidak dapat membuat berkas sementara. Instalasi dibatalkan
LdrCannotExecTemp=Tidak dapat mengeksekusi berkas di direktori sementara. Instalasi dibatalkan
;
; *** Pesan eror saat Startup
LastErrorMessage=%1.%n%nError %2: %3
SetupFileMissing=Berkas %1 hilang dari direktori instalasi. Harap selesaikan masalah ini atau dapatkan salinan instalasi yang baru.
SetupFileCorrupt=Berkas instalasi telah rusak. Harap dapatkan salinan instalasi yang baru.
SetupFileCorruptOrWrongVer=Berkas instalasi telah rusak, atau tidak cocok dengan versi instalasi ini. Harap selesaikan masalah atau dapatkan salinan instalasi yang baru.
InvalidParameter=Parameter tidak sah ditemukan di perintah:%n%n%1
SetupAlreadyRunning=Instalasi sudah berjalan.
WindowsVersionNotSupported=Versi program ini tidak mendukung versi Windows yang anda gunakan.
WindowsServicePackRequired=Program ini memebutuhkan %1 Service Pack %2 atau yang terbaru.
NotOnThisPlatform=Program ini tidak berjalan di %1.
OnlyOnThisPlatform=Program ini harus dijalankan di %1.
OnlyOnTheseArchitectures=Program ini hanya bisa dipasang di Windows yang mendukung arsitektur prosesor:%n%n1
MissingWOW64APIs=Versi Windows yang anda gunakan tidak memiliki fungsi yang yang diperlukan untuk instalasi 64-bit. Untuk memperbaiki masalah ini, harap pasang Service Pack %1.
WinVersionTooLowError=Program ini membutuhkan %1 versi %2 atau yang terbaru.
WinVersionTooHighError=Program ini tidak dapat dipasang pada %1 versi %2 atau yang terbaru.
AdminPrivilegesRequired=Anda harus masuk sebagai Administrator ketika memasang program ini.
PowerUserPrivilegesRequired=Anda harus masuk sebagai Administrator atau sebagai salah satu anggota grup Super Users ketika memasang program ini.
SetupAppRunningError=Instalasi mendeteksi bahwa %1 sedang berjalan.%n%nHarap tutup semua aplikasi ini sekarang, kemudian klik OK untuk melanjutkan, atau Cancel untuk keluar.
UninstallAppRunningError=Instalasi mendeteksi bahwa %1 sedang berjalan.%n%nHarap tutup semua aplikasi ini sekarang, kemudian klik OK untuk melanjutkan, atau Cancel untuk keluar.

; *** Beberapa pesan kesalahan
ErrorCreatingDir=Instalasi tidak dapat membuat direktori "%1"
ErrorTooManyFilesInDir=Tidak dapat membuat berkas di direktori "%1" karena berisi terlalu banyak berkas

; *** Pesan instalasi biasa
ExitSetupTitle=Keluar Instalasi
ExitSetupMessage=Instalasi tidak lengkap. Jika anda keluar sekarang, program ini tidak akan terpasang.%n%nAnda dapat memasang kembali di lain waktu untuk melengkapi instalasi.%n%nKeluar dari Instalasi?
AboutSetupMenuItem=&Tentang Instalasi...
AboutSetupTitle=Tentang Instalasi
AboutSetupMessage=%1 versi %2%n%3%n%nHalaman web %1:%n%4
AboutSetupNote=
TranslatorNote=

; *** Tombol
ButtonBack=< &Kembali
ButtonNext=&Lanjut >
ButtonInstall=&Pasang
ButtonOK=OK
ButtonCancel=Batal
ButtonYes=&Ya
ButtonYesToAll=Ya semua
ButtonNo=&Tidak
ButtonNoToAll=&Tidak semua
ButtonFinish=&Selesai
ButtonBrowse=&Jelajahi...
ButtonWizardBrowse=J&elajahi...
ButtonNewFolder=&Buat Folder Baru

; *** Pesan dialog "Pilih Bahasa"
SelectLanguageTitle=Pilih Bahasa Instalasi
SelectLanguageLabel=Pilih bahasa yang digunakan selama instalasi:

; *** Teks wizard biasa
ClickNext=Klik Lanjut untuk melanjutkan, atau klik Batal untuk keluar dari Instalasi.
BeveledLabel=
BrowseDialogTitle=Jelajahi Folder
BrowseDialogLabel=Pilih sebuah folder di bawah, lalu klik OK.
NewFolderName=Folder Baru

; *** Halaman "Selamat Datang"
WelcomeLabel1=Selamat datang di Instalasi [name]
WelcomeLabel2=Program ini akan memasang [name/ver] di komputer anda.%n%nDisarankan untuk menutup semua aplikasi yang berjalan sebelum melanjutkan.

; *** Halaman "Kata Sandi" 
WizardPassword=Kata Sandi
PasswordLabel1=Instalasi ini dilindungi kata sandi.
PasswordLabel3=Harap masukkan kata sandi, klik Lanjut untuk melanjutkan. Kata sandi bersifat sensitif.
PasswordEditLabel=&Kata Sandi:
IncorrectPassword=Kata sandi yang anda masukkan salah. Silahkan coba lagi.

; *** "License Agreement" wizard page
WizardLicense=Persetujuan Lisensi
LicenseLabel=Harap baca informasi penting berikut sebelum melanjutkan.
LicenseLabel3=Harap baca Persetujuan Lisensi berikut. Anda harus setuju dengan persetujuan lisensi ini sebelum melanjutkan instalasi.
LicenseAccepted=&Ya, saya setuju
LicenseNotAccepted=&Tidak, saya tidak setuju

; *** Halaman "Informasi"
WizardInfoBefore=Informasi
InfoBeforeLabel=Harap baca informasi penting berikut sebelum melanjutkan.
InfoBeforeClickLabel=Jika anda siap melanjutkan, klik Lanjut.
WizardInfoAfter=Informasi
InfoAfterLabel=Harap baca informasi penting berikut sebelum melanjutkan.
InfoAfterClickLabel=Jika anda siap melanjutkan, klik Lanjut.

; *** Halaman "Informasi Pengguna"
WizardUserInfo=Infornasi pengguna
UserInfoDesc=Masukkan informasi anda.
UserInfoName=&Nama Pengguna:
UserInfoOrg=&Organisasi:
UserInfoSerial=&Nomor Seri:
UserInfoNameRequired=Anda harus memasukkan nama.

; *** Halaman "Pilih Lokasi Instalasi"
WizardSelectDir=Pilih Lokasi Instalasi
SelectDirDesc=Dimana [name] akan dipasang?
SelectDirLabel3=Program akan memasang [name] di folder yang dipilih.
SelectDirBrowseLabel=Untuk melanjutkan, klik Lanjut. Jika anda ingin memilik folder lain, klik Jelajahi.
DiskSpaceMBLabel=Dibutuhkan setidaknya [mb] MB ruang bebas.
CannotInstallToNetworkDrive=Tidak dapat memasang di drive jaringan.
CannotInstallToUNCPath=Tidak dapat memasang di UNC path.
InvalidPath=Anda harus memasukkan alamat beserta lokasi drive; contoh:%n%nC:\APP%n%maupun sebuah lokasi UNC:%n%n\\server\share
InvalidDrive=Drive atau UNC yang anda pilih tidak ada atau tidak dapat diakses. Harap pilih yang lain.
DiskSpaceWarningTitle=Ruang Bebas Tidak Cukup
DiskSpaceWarning=Program membutuhkan setidaknya %1 KB untuk instalasi, tetapi drive yang anda pilih hanya memiliki %2 yang tersedia.%n%nApakah anda tetap ingin melanjutkan?
DirNameTooLong=Nama folder atau lokasi terlalu panjang.
InvalidDirName=Nama folder tidak sah.
BadDirName32=Nama folder tidak boleh berisi karakter berikut:%n%n%1
DirExistsTitle=Folder Sudah Ada
DirExists=Folder:%n%n%1%n%nsudah ada. Apakah anda tetap ingin melanjutkan?
DirDoesntExistTitle=Folder Tidak Ada
DirDoesntExist=Folder:%n%n%1%n%ntidak ada. Apakah anda ingin membuatnya?

; *** Halaman "Pilih Komponen"
WizardSelectComponents=Pilih Komponen
SelectComponentsDesc=Komponen mana saja yang akan dipasang?
SelectComponentsLabel2=Pilih komponen yang ingin anda pasang; bersihkan komponen yang tidak ingin anda pasang. Klik Lanjut jika anda sudah siap melanjutkan.
FullInstallation=Instalasi Penuh
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Instalasi Padat
CustomInstallation=Instalasi Kustom
NoUninstallWarningTitle=Komponen Sudah Ada
NoUninstallWarning=Instalasi mendeteksi komponen berikut telah terpasang di komputer anda:%n%n%1%n%nMembatalkan pilihan komponen ini tidak akan menghapusnya.%n%nTetap lanjutkan?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Pilihan ini membutuhkan setidaknya [mb] MB ruang bebas.

; *** Halaman "Pilih Perintah Tambahan"
WizardSelectTasks=Pilih Perintah Tambahan
SelectTasksDesc=Perintah tambahan mana saja yang harus dilakukan?
SelectTasksLabel2=Pilih perintah tambahan yang anda ingin lakukan saat memasang [name], lalu klik Lanjut.

; *** Halaman "Pilih Folder Start Menu"
WizardSelectProgramGroup=Pilih Folder Start Menu
SelectStartMenuFolderDesc=Dimana Instalasi harus meletakkan jalan pintas program?
SelectStartMenuFolderLabel3=Instalasi akan membuat jalan pintas program di folder Start Menu berikut.
SelectStartMenuFolderBrowseLabel=Untuk melanjutkan, klik Lanjut. Jika anda ingin memilih folder lain, klik Jelajahi.
MustEnterGroupName=Anda harus memasukkan nama folder.
GroupNameTooLong=nama folder atau lokasi terlalu panjang.
InvalidGroupName=Nama folder tidak sah.
BadGroupName=Nama folder tidak boleh berisi karakter berikut:%n%n%1
NoProgramGroupCheck2=&Jangan buat folder Start Menu

; *** Halaman "Siap Memasang"
WizardReady=Siap Memasang
ReadyLabel1=Sekarang Instalasi siap memasang [name] di komputer anda.
ReadyLabel2a=Klik Pasang untuk melanjutkan instalasi, atau klik Kembali jika anda ingin melihat atau mengubah pilihan.
ReadyLabel2b=Klik Pasang untuk melanjutkan instalasi.
ReadyMemoUserInfo=Informasi Pengguna:
ReadyMemoDir=Lokasi instalasi:
ReadyMemoType=Tipe instalasi:
ReadyMemoComponents=Komponen terpilih:
ReadyMemoGroup=Folder Start Menu:
ReadyMemoTasks=Perintah tambahan:

; *** Halaman "Bersiap Memasang"
WizardPreparing=Bersiap Memasang
PreparingDesc=Sedang bersiap untuk memasang [name] di komputer anda.
PreviousInstallNotCompleted=Instalasi/penghapusan sebelumnya tidak lengkap. Anda mungkin harus memulai ulang komputer anda untuk melengkapi instalasi.%n%nSetelah memulai ulang komputer anda, jalankan Instalasi kembali untuk melengkapi instalasi [name]
CannotContinue=Tidak dapat melanjutkan. Klik Batal untuk keluar.
ApplicationsFound=Aplikasi berikut sedang menggunakan berkas yang harus diperbarui. Direkomendasikan untuk membiarkan Instalasi untuk menutup aplikasi ini secara otomatis. 
ApplicationsFound2=Aplikasi berikut sedang menggunakan berkas yang harus diperbarui. Direkomendasikan untuk membiarkan Instalasi untuk menutup aplikasi ini secara otomatis. Setelah pemasangan selesai, Instalasi akan berusaha menjalankan kembali aplikasi tersebut.
CloseApplications=&Otomatis tutup semua aplikasi
DontCloseApplications=&Jangan tutup aplikasi tersebut
ErrorCloseApplications=Instalasi tidak dapat menutup aplikasi secara otomatis. Anda disarankan untuk menutup aplikasi yang sedang menggunakan berkas yang perlu diperbarui oleh Instalasi sebelum melanjutkan.

; *** Halaman "Memasang"
WizardInstalling=Memasang
InstallingLabel=Harap tunggu sementara Instalasi memasang [name] di komputer anda.

; *** Halaman "Instalasi Lengkap"
FinishedHeadingLabel=Melengkapi instalasi [name]
FinishedLabelNoIcons=Berhasil memasang [name] di komputer anda.
FinishedLabel=Instalasi berhasil memasang [name] di komputer anda. Aplikasi tersebut dapat dijalankan dengan memilih jalan pintas yang terpasang.
ClickFinish=Klik Selesai untuk keluar.
FinishedRestartLabel=Untuk melengkapi pemasangan [name], Instalasi harus memulai ulang komputer anda. Lakukan sekarang?
FinishedRestartMessage=Untuk melengkapi pemasangan [name], Instalasi harus memulai ulang komputer anda.%n%nLakukan sekarang?
ShowReadmeCheck=Ya, baca berkas README sekarang
YesRadio=&Ya, mulai ulang komputer sekarang
NoRadio=&Tidak, saya akan memulai ulang nanti
; Contoh penggunaan: 'Run MyProg.exe'
RunEntryExec=Jalankan %1
; Contoh penggunaan: 'View Readme.txt'
RunEntryShellExec=Lihat %1

; *** Isian "Instalasi Membutuhkan Diska Lanjutan"
ChangeDiskTitle=Instalasi Membuthkan Diska Lanjutan
SelectDiskLabel2=Harap masukan Diska %1 dan klik OK.%n%nJika berkas dalam diska ini dapat ditemukan di folder lain yang ditampilkan di bawah, masukkan lokasi yang benar atau klik Jelajahi.
PathLabel=&Lokasi:
FileNotInDir2=Berkas "%1" tidak dapat ditemukan di "%2". Harap masukkan diska yang benar atau pilih folder lain.
SelectDirectoryLabel=Harap masukkan lokasi dari diska lanjutan.

; *** Pesan proses instalasi
SetupAborted=Instalasi tidak lengkap.%n%nHarap perbaiki masalah dan jalankan Instalasi kembali.
EntryAbortRetryIgnore=Klik Retry untuk mencoba lagi, Ignore untuk mengabaikan, atau Abort untuk membatalkan instalasi.

; *** Pesan status instalasi
StatusClosingApplications=Menutup aplikasi...
StatusCreateDirs=Membuat direktori...
StatusExtractFiles=Mengekstrak berkas...
StatusCreateIcons=Membuat jalan pintas...
StatusCreateIniEntries=Membuat entri INI...
StatusCreateRegistryEntries=Membuat entri registry...
StatusRegisterFiles=Meregistrasi berkas...
StatusSavingUninstall=Menyimpan informasi penghapusan...
StatusRunProgram=Mengakhiri instalasi...
StatusRestartingApplications=Menjalankan ulang aplikasi...
StatusRollback=Membatalkan perubahan...

; *** Macam macam kesalahan
ErrorInternal2=Kesalahan internal: %1
ErrorFunctionFailedNoCode=%1 failedgagal
ErrorFunctionFailed=%1 gagal; kode %2
ErrorFunctionFailedWithMessage=%1 gagal; kode %2.%n%3
ErrorExecutingProgram=Tidak dapat mengeksekusi berkas:%n%1

; *** Kesalahan Registry
ErrorRegOpenKey=Gagal menbuka kunci registry:%n%1\%2
ErrorRegCreateKey=Gagal membuat kunci registry:%n%1\%2
ErrorRegWriteKey=Gagal menulis isi registry:%n%1\%2

; *** Kesalahan INI
ErrorIniEntry=Gagal membuat entri INI pada berkas "%1".

; *** Kesalahan salin berkas
FileAbortRetryIgnore=Klik Retry untuk mencoba lagi, Ignore untuk mengabaikan berkas (tidak disarankan), atau Abort untuk membatalkan instalasi.
FileAbortRetryIgnore2=Klik Retry untuk mencoba lagi, Ignore untuk mengabaikan berkas (tidak disarankan), atau Abort untuk membatalkan instalasi.
SourceIsCorrupted=Berkas asal telah rusak
SourceDoesntExist=Berkas asal "%1" tidak ada
ExistingFileReadOnly=Berkas yang sudah ada diatur menjadi hanya-baca.%n%nClick Retry untuk menghapus atribut hanya-baca dan mencoba lagi, Ignore untuk mengabaikan berkas, atau Abort untuk membatalkan instalasi.
ErrorReadingExistingDest=Kesalahan terjadi saat mencoba membaca berkas:
FileExists=Berkas sudah ada.%n%nApakah anda ingin Instalasi menimpanya?
ExistingFileNewer=Berkas yang sudah ada lebih baru dari yang Instalasi coba pasang. Disarankan untuk membiarkan berkas tersebut.%n%nApa anda ingin membiar berkas tersebut?
ErrorChangingAttr=Kesalahan terjadi saat mencoba mengubah atribut berkas:
ErrorCreatingTemp=Kesalahan terjadi saat mencoba membuat berkas di lokasi instalasi:
ErrorReadingSource=Kesalahan terjadi saat mencoba membaca berkas sumber:
ErrorCopying=Kesalahan terjadi saat mencoba menyalin berkas:
ErrorReplacingExistingFile=Kesalahan terjadi saat mencoba menimpa berkas:
ErrorRestartReplace=RestartReplace gagal:
ErrorRenamingTemp=Kesalahan terjadi saat mencoba mengubah nama berkas di lokasi instalasi:
ErrorRegisterServer=Tidak dapat meregistrasi DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 gagal dengan kode %1
ErrorRegisterTypeLib=Tidak dapat meregistrasi berkas: %1

; *** Kesalahan setelah instalasi
ErrorOpeningReadme=Kesalahan terjadi saat mencoba membuka berkas README.
ErrorRestartingComputer=Program instalasi tidak dapat memulai ulang komputer. Harap lakukan secara manual.

; *** Pesan Penghapusan
UninstallNotFound=Berkas "%1" tidak ada. Tidak dapat menghapus.
UninstallOpenError=Berkas "%1" tidak dapat dibuka. Tidak dapat menghapus.
UninstallUnsupportedVer=The uninstall log file "%1" is in a format not recognized by this version of the uninstaller. Cannot uninstall
UninstallUnknownEntry=Entri tidak dikenal (%1) ditemukan di catatan penghapusan
ConfirmUninstall=Apakah anda yakin ingin menghapus seluruh %1 dan semua komponennya?
UninstallOnlyOnWin64=Instalasi ini hanya bisa dihapus di Windows 64-bit.
OnlyAdminCanUninstall=Instalasi ini hanya bisa dihapus oleh pengguna dengan ijin administrator.
UninstallStatusLabel=Mohon tunggu sementara %1 dihapus dari komputer anda.
UninstalledAll=%1 berhasil dihapus dari komputer anda.
UninstalledMost=Penghapusan %1 telah selesai.%n%nBeberapa berkas tidak dapat dihapus. Anda dapat menghapusnya secara manual.
UninstalledAndNeedsRestart=Untuk melengkapi penghapusan %1, komputer anda harus dimulai ulang.%n%nMulai ulang sekarang?
UninstallDataCorrupted=Berkas "%1" telah rusak. Tidak dapat memasang

; *** Pesan proses penghapusan
ConfirmDeleteSharedFileTitle=Hapus berbagi berkas?
ConfirmDeleteSharedFile2=Sistem mendeteksi bahwa berkas yang dibagikan tidak digunakan oleh program manapun. Apakah anda ingin Penghapus untuk menghapus berkas yang dibagikan ini?%n%nJika ada aplikasi yang masih memerlukan berkas ini dan berkas ini dihapus, aplikasi tersebut mungkin tidak bisa bekerja dengan semestinya. Jika anda tidak yakin, pilih No. Meninggalkan berkas ini di komputer anda tidak membahayakan sistem anda.
SharedFileNameLabel=Nama berkas:
SharedFileLocationLabel=Lokasi:
WizardUninstalling=Status penghapusan
StatusUninstalling=Menghapus %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Memasang %1.
ShutdownBlockReasonUninstallingApp=Menghapus %1.

; Pesan kustom di bawah ini tidak  digunakan oleh Instalasi itu sendiri,
; tetapi jika anda membuatnya diperlukan di skrip anda, anda perlu menterjemahkannya

[CustomMessages]

NameAndVersion=%1 versi %2
AdditionalIcons=Jalan pintas tambahan:
CreateDesktopIcon=Buat jalan pintas di &Desktop
CreateQuickLaunchIcon=Buat jalan pintah di &Quick Launch
ProgramOnTheWeb=%1 di Web
UninstallProgram=Hapus %1
LaunchProgram=Jalankan %1
AssocFileExtension=&Hubungkan %1 dengan berkas berekstensi %2
AssocingFileExtension=Menghubungkan %1 dengan ekstensi berkas %2...
AutoStartProgramGroupDescription=Startup:
AutoStartProgram=Jalankan %1 secara otomatis
AddonHostProgramNotFound=%1 tidak dapat diletakkan di folder yang anda pilih.%n%nApakah anda tetap ingin melanjutkan?
