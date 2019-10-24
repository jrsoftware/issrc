; *** Inno Setup version 6.0.3+ Indonesian messages ***
;
; Untuk mengunduh berkas terjemahan hasil konstribusi pengguna, kunjungi: 
;   http://www.jrsoftware.org/files/istrans/
;
; Alih bahasa oleh: MozaikTM (mozaik.tm@gmail.com)
;
; Catatan: Saat menerjemahkan pesan ini, jangan masukkan titik (.) pada
; akhir pesan tanpa titik, karena Inno Setup menambahkan titik pada pesan tersebut
; secara otomatis (menambahkan sebuah titik akan memunculkan dua titik).

[LangOptions]
; Tiga baris berikut sangat penting. Pastikan untuk membaca dan 
; memahami topik 'bagian [LangOption]' dalam berkas bantuan.
LanguageName=Bahasa Indonesia
LanguageID=$0421
LanguageCodePage=0
; Bila target bahasa Anda memerlukan fon atau ukuran khusus,
; hapus tanda komentar (;) dari salah satu atau beberapa baris berikut dan ubah seperlunya.
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
SetupAppTitle=Pemasang
SetupWindowTitle=Pemasangan %1
UninstallAppTitle=Pelepas
UninstallAppFullTitle=Pelepasan %1

; *** Misc. common
InformationTitle=Informasi
ConfirmTitle=Konfirmasi
ErrorTitle=Ada Masalah

; *** Pesan untuk SetupLdr
SetupLdrStartupMessage=Kami akan memasang %1. Lanjutkan?
LdrCannotCreateTemp=Tidak dapat membuat berkas sementara. Pemasangan dibatalkan
LdrCannotExecTemp=Tidak dapat mengeksekusi berkas di dalam direktori sementara. Pemasangan dibatalkan
HelpTextNote=

; *** Pesan kesalahan saat memuat Pemasang
LastErrorMessage=%1.%n%nKesalahan %2: %3
SetupFileMissing=Berkas %1 hilang dari lokasi pemasangan. Silakan selesaikan masalah atau dapatkan salinan baru dari pemasang ini.
SetupFileCorrupt=Berkas Pemasang telah rusak. Silakan dapatkan salinan baru dari pemasang ini.
SetupFileCorruptOrWrongVer=Berkas-berkas pemasang telah rusak, atau tidak cocok dengan versi pemasang ini. Silakan selesaikan masalah atau dapatkan salinan baru dari berkas ini.
InvalidParameter=Ada parameter tidak sah pada baris perintah:%n%n%1
SetupAlreadyRunning=Pemasang sudah berjalan.
WindowsVersionNotSupported=Program ini tidak mendukung Windows yang terpasang pada komputer ini.
WindowsServicePackRequired=Program ini memerlukan %1 Service Pack %2 atau yang terbaru.
NotOnThisPlatform=Program ini tidak akan berjalan pada %1.
OnlyOnThisPlatform=Program ini harus dijalankan pada %1.
OnlyOnTheseArchitectures=Program ini hanya dapat dipasang pada versi Windows yang didesain untuk arsitektur prosesor berikut:%n%n%1
WinVersionTooLowError=Program ini memerlukan %1 versi %2 atau yang terbaru.
WinVersionTooHighError=Program ini tidak dapat dipasang pada %1 versi %2 atau yang terbaru.
AdminPrivilegesRequired=Anda wajib masuk sebagai seorang administrator saat memasang program ini.
PowerUserPrivilegesRequired=Anda wajib masuk sebagai seorang administrator atau pengguna dari grup Power Users saat memasang program ini.
SetupAppRunningError=Pemasang mendeteksi bahwa %1 sedang berjalan.%n%nSilakan tutup semua program terkait, kemudian klik OK untuk lanjut, atau Batal untuk keluar.
UninstallAppRunningError=Pelepas mendeteksi bahwa %1 sedang berjalan.%n%nSilakan tutup semua program terkait, kemudian klik OK untuk lanjut, atau Batal untuk keluar.

; *** Pertanyaan saat memuat Pemasang
PrivilegesRequiredOverrideTitle=Pilih Mode Pemasang
PrivilegesRequiredOverrideInstruction=Pilih mode pemasangan
PrivilegesRequiredOverrideText1=%1 bisa dipasang untuk semua pengguna (perlu izin administratif), atau hanya untuk Anda.
PrivilegesRequiredOverrideText2=%1 bisa dipasang hanya untuk Anda, atau untuk semua pengguna (perlu izin administratif).
PrivilegesRequiredOverrideAllUsers=Pasang untuk &semua pengguna
PrivilegesRequiredOverrideAllUsersRecommended=Pasang untuk &semua pengguna (disarankan)
PrivilegesRequiredOverrideCurrentUser=Pasang hanya untuk saya
PrivilegesRequiredOverrideCurrentUserRecommended=Pasang hanya untuk saya (disarankan)

; *** Macam-macam galat
ErrorCreatingDir=Pemasang tidak dapat membuat direktori "%1"
ErrorTooManyFilesInDir=Tidak dapat membuat berkas dalam direktori "%1" karena berisi terlalu banyak berkas.

; *** Pesan umum pada Pemasamg
ExitSetupTitle=Tutup Pemasang
ExitSetupMessage=Pemasangan tidak lengkap. Bila Anda keluar sekarang, program tidak akan terpasang.%n%nAnda dapat menjalankan kembali Pemasang ini lain kali untuk melengkapi pemasangan.%n%nTutup Pemasang?
AboutSetupMenuItem=&Tentang Pemasang ....
AboutSetupTitle=Tentang Pemasang
AboutSetupMessage=%1 versi %2%n%3%n%n%1 laman muka:%n%4
AboutSetupNote=
TranslatorNote=Bila Anda menemukan typo (kesalahan pengetikan), terjemahan yang salah atau kurang tepat, atau Anda ingin mendapatkan terjemahan untuk versi lawas, silakan kirimkan surel (email) ke mozaik(dot)tm(at)gmail(dot)com

; *** Tombol-tombol
ButtonBack=< &Sebelumnya
ButtonNext=&Berikutnya >
ButtonInstall=&Pasang
ButtonOK=OK
ButtonCancel=Batal
ButtonYes=&Iya
ButtonYesToAll=Iya &semuanya
ButtonNo=&Tidak
ButtonNoToAll=&Tidak semuanya
ButtonFinish=&Selesai
ButtonBrowse=&Jelajahi ....
ButtonWizardBrowse=J&elajahi ....
ButtonNewFolder=&Buat Map Baru

; *** Halaman "Pilih Bahasa"
SelectLanguageTitle=Pilih Bahasa Pemasang
SelectLanguageLabel=Pilih bahasa untuk digunakan selama pemasangan.

; *** Pesan umum pada Pemasang
ClickNext=Klik Berikutnya untuk melanjutkan, atau Batal untuk menutup Pemasang.
BeveledLabel=
BrowseDialogTitle=Pilih Map
BrowseDialogLabel=Pilih satu map dalam daftar di bawah, kemudian klik OK.
NewFolderName=Map Baru

; *** Halaman "Selamat Datang"
WelcomeLabel1=Selamat datang di Asisten Pemasangan [name]
WelcomeLabel2=Kami akan memasang [name/ver] pada komputer Anda.%n%nAnda disarankan untuk menutup semua aplikasi sebelum melanjutkan.

; *** Halaman "Kata Sandi"
WizardPassword=Kata Sandi
PasswordLabel1=Pemasang ini dilindungi kata sandi.
PasswordLabel3=Silakan masukkan kata sandi, lalu klik Berikutnya untuk melanjutkan. Kata sandi bersifat sensitif kapitalisasi.
PasswordEditLabel=&Kata Sandi:
IncorrectPassword=Kata sandi yang Anda masukkan salah. Silakan coba lagi.

; *** Halaman "Kesepakatan Lisensi"
WizardLicense=Kesepakatan Lisensi
LicenseLabel=Silakan baca informasi penting berikut sebelum melanjutkan.
LicenseLabel3=Silakan baca Kesepakatan Lisensi berikut. Anda wajib menyetujui syarat-syarat kesepakatan ini sebelum melanjutkan pemasangan.
LicenseAccepted=Saya &setuju dengan kesepakatan ini
LicenseNotAccepted=Saya &tidak setuju dengan kesepakatan ini

; *** Halaman "Informasi"
WizardInfoBefore=Informasi
InfoBeforeLabel=Silakan baca informasi penting berikut sebelum melanjutkan.
InfoBeforeClickLabel=Bila Anda sudah siap melanjutkan pemasangan, klik Berikutnya.
WizardInfoAfter=Informasi
InfoAfterLabel=Silakan baca informasi penting berikut sebelum melanjutkan.
InfoAfterClickLabel=Bila Anda sudah siap melanjutkan pemasangan, klik Berikutnya.

; *** Halaman "Informasi Pengguna"
WizardUserInfo=Informasi Pengguna
UserInfoDesc=Silakan masukkan informasi Anda.
UserInfoName=&Nama Pengguna:
UserInfoOrg=&Organisasi:
UserInfoSerial=Nomor Seri:
UserInfoNameRequired=Anda wajib memasukkan nama.

; *** Halaman "Pilih Lokasi Pemasangan"
WizardSelectDir=Pilih Lokasi Pemasangan
SelectDirDesc=Di manakah [name] sebaiknya dipasang?
SelectDirLabel3=Kami akan memasang [name] di dalam map berikut.
SelectDirBrowseLabel=Klik Berikutnya untuk melanjutkan. Bila Anda ingin memilih map lain, klik Jelajahi.
DiskSpaceGBLabel=Diperlukan sedikitnya [gb] GB ruang kosong.
DiskSpaceMBLabel=Diperlukan sedikitnya [mb] MB ruang kosong.
CannotInstallToNetworkDrive=Kami tidak dapat memasang pada kandar jaringan.
CannotInstallToUNCPath=Kami tidak dapat memasang pada lokasi UNC.
InvalidPath=Anda wajib memasukkan lokasi map lengkap dengan nama kandar; misalnya:%n%nC:\APP%n%natau sebuah alamat UNC dengan format:%n%n\\server\share
InvalidDrive=Kandar atau alamat UNC yang Anda pilih tidak ada atau tidak dapat diakses. Silakan pilih yang lain.
DiskSpaceWarningTitle=Ruang Kosong Tidak Mencukupi
DiskSpaceWarning=Pemasang memerlukan sedikitnya %1 KB ruang kosong, tetapi kandar terpilih hanya memiliki %2 KB tersedia.%n%nTetap lanjutkan?
DirNameTooLong=Alamat atau nama map terlalu panjang.
InvalidDirName=Nama map ini tidak sah.
BadDirName32=Nama map dilarang berisi karakter berikut:%n%n%1
DirExistsTitle=Map Sudah Ada
DirExists=Map:%n%n%1%n%nsudah ada. Tetap pasang di map tersebut?
DirDoesntExistTitle=Map Belum Ada
DirDoesntExist=Map:%n%n%1%n%nbelum ada. Buat map tersebut?

; *** Halaman "Pilih Komponen"
WizardSelectComponents=Pilih Komponen
SelectComponentsDesc=Komponen mana sajakah yang sebaiknya dipasang?
SelectComponentsLabel2=Pilih komponen-komponen yang Anda ingin pasang; hapus centang pada komponen yang Anda tidak ingin pasang. Klik Berikutnya bila Anda siap melanjutkan.
FullInstallation=Pasang secara penuh
; kalau bisa, jangan terjemahkan "Padat" (Compact) menjadi "Minimal". Maksudnya, "Minimal" dalam bahasa Anda
CompactInstallation=Pemasangan Padat
CustomInstallation=Suka-suka saya
NoUninstallWarningTitle=Komponen Sudah Ada
NoUninstallWarning=Kami mendeteksi bahwa komponen-komponen berikut sudah terpasang pada komputer Anda:%n%n%1%n%nKomponen-komponen tersebut tidak akan dihapus walau Anda batal memilihnya.%n%nTetap lanjutkan?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=Pilihan Anda saat ini memerlukan sedikitnya [gb] GB ruang kosong.
ComponentsDiskSpaceMBLabel=Pilihan Anda saat ini memerlukan sedikitnya [mb] MB ruang kosong.

; *** Halaman "Pilih Tugas Tambahan"
WizardSelectTasks=Pilih Tugas Tambahan
SelectTasksDesc=Tugas tambahan mana sajakah yang Anda ingin jalankan?
SelectTasksLabel2=Pilih tugas tambahan yang Anda ingin agar kami jalankan saat memasang [name], lalu klik Berikutnya.

; *** Halaman "Pilih Map Menu Start"
WizardSelectProgramGroup=Pilih Map Menu Start
SelectStartMenuFolderDesc=Di manakah sebaiknya kami menempatkan pintasan program?
SelectStartMenuFolderLabel3=Kami akan membuat pintasan program di dalam map Menu Start berikut.
SelectStartMenuFolderBrowseLabel=Klik Berikutnya untuk melanjutkan. Bila Anda ingin memilih map lain, klik Jelajahi.
MustEnterGroupName=Anda wajib memasukkan nama map.
GroupNameTooLong=Alamat atau nama map terlalu panjang.
InvalidGroupName=Nama map tidak sah.
BadGroupName=Nama map dilarang berisi karakter berikut:%n%n%1
NoProgramGroupCheck2=&Jangan buat map Menu Start

; *** Halaman "Siap Memasang"
WizardReady=Siap Memasang
ReadyLabel1=Kami telah siap untuk mulai memasang [name] pada komputer Anda.
ReadyLabel2a=Klik Pasang untuk melanjutkan dengan pengaturan yang Anda pilih, atau klik Sebelumnya bila Anda ingin melihat ulang atau mengubah pengaturan.
ReadyLabel2b=Klik Pasang untuk melanjutkan dengan pengaturan yang Anda pilih
ReadyMemoUserInfo=Informasi pengguna:
ReadyMemoDir=Lokasi pemasangan:
ReadyMemoType=Jenis pemasangan:
ReadyMemoComponents=Komponen terpilih:
ReadyMemoGroup=Map Menu Start:
ReadyMemoTasks=Tugas Tambahan:

; *** Halaman "Bersiap Memasang"
WizardPreparing=Bersiap Memasang
PreparingDesc=Kami sedang bersiap memasang [name] pada komputer Anda.
PreviousInstallNotCompleted=Pemasangan/pelepasan dari program sebelumnya tidaklah lengkap. Anda perlu memulai ulang komputer untuk melengkapi pemasangan tersebut.%n%nSeusai memulai ulang komputer, jalankan Pemasang ini lagi untuk melengkapi pemasangan [name].
CannotContinue=Kami tidak dapat melanjutkan. Silakan klik Batal untuk keluar.
ApplicationsFound=Aplikasi-aplikasi berikut sedang memakai berkas-berkas yang perlu diperbarui oleh kami. Disarankan agar Anda mengizinkan kami untuk menutup aplikasi-aplikasi tersebut secara otomatis.
ApplicationsFound2=Aplikasi-aplikasi berikut sedang memakai berkas-berkas yang perlu diperbaru oleh kami. Disarankan agar Anda mengizinkan kami untuk menutup aplikasi-aplikasi tersebut secara otomatis. Seusai memasang, kami akan berusaha menjalankan ulang aplikasi-aplikasi tersebut.
CloseApplications=&Otomatis tutup aplikasi-aplikasi tersebut
DontCloseApplications=&Jangan tutup aplikasi-aplikasi tersebut
ErrorCloseApplications=Kami tidak dapat menutup semua aplikasi tersebut secara otomatis. Disarankan agar Anda menutup semua aplikasi yang memakai berkas-berkas yang perlu kami perbarui sebelum melanjutkan.
PrepareToInstallNeedsRestart=Kami harus memulai ulang komputer Anda. Seusai memulai ulang, jalankan kembali Pemasang ini untuk melengkapi pemasangan [name].%n%nMulai ulang sekarang?

; *** Halaman "Memasang"
WizardInstalling=Memasang
InstallingLabel=Silakan tunggu sementara kami memasang [name] pada komputer Anda.

; *** Halaman "Pemasangan Lengkap"
FinishedHeadingLabel=Menyelesaikan Asisten Pemasangan [name]
FinishedLabelNoIcons=Kami telah selesai memasang [name] pada komputer Anda.
FinishedLabel=Kami telah selesai memasang [name] pada komputer Anda. Program tersebut dapat dijalankan dengan memilih pintasan yang terpasang.
ClickFinish=Klik Selesai untuk mengakhiri pemasangan.
FinishedRestartLabel=Agar pemasangan [name] lengkap, kami harus memulai ulang komputer Anda. Mulai ulang sekarang?
FinishedRestartMessage=Agar pemasangan [name] lengkap, kami harus memulai ulang komputer Anda.%n%nMulai ulang sekarang?
ShowReadmeCheck=Ya, saya mau membaca berkas README
YesRadio=&Ya, mulai ulang sekarang
NoRadio=&Tidak, saya akan memulai ulang nanti
; contoh: 'Jalankan MyProg.exe'
RunEntryExec=Jalankan %1
; contoh: 'Lihat Readme.txt'
RunEntryShellExec=Lihat %1

; *** Pesan yang berkaitan dengan "Setup Needs the Next Disk"
ChangeDiskTitle=Kami Memerlukan Kandar Lanjutan
SelectDiskLabel2=Silakan masukkan Kandar %1 dan klik OK.%n%nBila berkas-berkas pada kandar ini dapat ditemukan selain pada map berikut, masukkan alamat yang tepat atau klik Jelajahi.
PathLabel=&Alamat:
FileNotInDir2=Berkas "%1" tidak dapat ditemukan di dalam "%2". Silakan masukkan kandar yang tepat atau pilih map lain.
SelectDirectoryLabel=Silakan tunjukkan lokasi kandar lanjutan.

; *** Pesan untuk fase pemasangan
SetupAborted=Pemasangan tidak lengkap.%n%nSilakan selesaikan masalah dan jalankan Pemasang ini kembali.
AbortRetryIgnoreSelectAction=Pilih tindakan
AbortRetryIgnoreRetry=&Coba lagi
AbortRetryIgnoreIgnore=&Abaikan masalah dan lanjutkan
AbortRetryIgnoreCancel=Batalkan pemasangan

; *** Pesan untuk status pemasangan
StatusClosingApplications=Menutup aplikasi ....
StatusCreateDirs=Membuat direktori ....
StatusExtractFiles=Mengekstrak berkas ....
StatusCreateIcons=Membuat pintasan ....
StatusCreateIniEntries=Membuat isi berkas INI ...
StatusCreateRegistryEntries=Membuat daftar registri ....
StatusRegisterFiles=Mendaftarkan berkas ....
StatusSavingUninstall=Menyimpan informasi pelepasan ....
StatusRunProgram=Mengakhiri pemasangan ....
StatusRestartingApplications=Memulai ulang aplikasi ....
StatusRollback=Membatalkan perubahan ....

; *** Masalah secara umum
ErrorInternal2=Masalah internal: %1
ErrorFunctionFailedNoCode=%1 gagal
ErrorFunctionFailed=%1 gagal; kode %2
ErrorFunctionFailedWithMessage=%1 gagal; kode %2.%n%3
ErrorExecutingProgram=Tidak dapat mengeksekusi berkas:%n%1

; *** Masalah pada Registry
ErrorRegOpenKey=Masalah saat membuka kunci registri:%n%1\%2
ErrorRegCreateKey=Masalah saat membuat kunci registri:%n%1\%2
ErrorRegWriteKey=Masalah saat menulis pada kunci registri:%n%1\%2

; *** Masalah pada INI
ErrorIniEntry=Terjadi masalah saat membuat entri INI dalam berkas "%1".

; *** Masalah saat menyalin berkas
FileAbortRetryIgnoreSkipNotRecommended=&Lewati berkas ini (tidak disarankan)
FileAbortRetryIgnoreIgnoreNotRecommended=&Abaikan masalah dan lanjutkan (tidak disarankan)
SourceIsCorrupted=Berkas sumber telah rusak
SourceDoesntExist=Berkas sumber "%1" tidak ada
ExistingFileReadOnly2=Berkas yang telah ada tidak bisa diganti karena ditandai hanya-baca.
ExistingFileReadOnlyRetry=&Hapus atribut hanya-baca dan coba lagi
ExistingFileReadOnlyKeepExisting=&Pertahankan berkas yang sudah ada
ErrorReadingExistingDest=Terjadi masalah saat mencoba membaca berkas yang sudah ada:
FileExists=Berkas sudah ada.%n%nTimpa berkas tersebut?
ExistingFileNewer=Berkas yang sudah ada lebih baru dibanding dengan yang akan kami pasang. Disarankan agar Anda mempertahankan berkas tersebut.%n%nPertahankan berkas tersebut?
ErrorChangingAttr=Terjadi masalah saat mencoba mengubah atribut berkas yang sudah ada:
ErrorCreatingTemp=Terjadi masalah saat mencoba membuat berkas di dalam direktori pemasangan:
ErrorReadingSource=Terjadi masalah saat mencoba membaca berkas sumber:
ErrorCopying=Terjadi masalah saat mencoba menyalin berkas:
ErrorReplacingExistingFile=Terjadi masalah saat mencoba menimpa berkas yang sudah ada:
ErrorRestartReplace=Fungsi RestartReplace gagal:
ErrorRenamingTemp=Terjadi masalah saat mencoba mengubah nama berkas dalam direktori pemasangan:
ErrorRegisterServer=Tidak dapat mendaftarkan berkas DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 gagal dengan kode akhir %1
ErrorRegisterTypeLib=Tidak dapat mendaftarkan pustaka: %1

; *** Penandaan tampilan nama saat melepas
; contoh 'Program saya (32-bita)'
UninstallDisplayNameMark=%1 (%2)
; contoh 'Program saya (32-bita, Semua pengguna)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32-bita
UninstallDisplayNameMark64Bit=64-bita
UninstallDisplayNameMarkAllUsers=Semua pengguna
UninstallDisplayNameMarkCurrentUser=Pengguna saat ini

; *** Masalah pasca-pemasangan
ErrorOpeningReadme=Terjadi masalah saat mencoba membuka berkas README.
ErrorRestartingComputer=Kami tidak dapat memulai ulang komputer. Silakan lakukan secara manual.

; *** Pesan untuk Pelepas
UninstallNotFound=Berkas "%1" tidak ada. Tidak bisa melepas.
UninstallOpenError=Berkas "%1" tidak bisa dibuka. Tidak bisa melepas
UninstallUnsupportedVer=Berkas catatan pelepas "%1" tertulis dalam format yang tak dikenali oleh pelepas versi ini. Tidak bisa melepas.
UninstallUnknownEntry=Entri tak dikenal (%1) ditemukan dalam catatan pelepas
ConfirmUninstall=Apakah Anda yakin hendak menghapus %1 beserta semua komponennya?
UninstallOnlyOnWin64=Instalasi ini hanya dapat dilepas pada Windows 64-bita.
OnlyAdminCanUninstall=Instalasi ini hanya dapat dilepas oleh pengguna dengan izin administratif.
UninstallStatusLabel=Silakan tunggu sementara %1 dihapus dari komputer Anda.
UninstalledAll=%1 berhasil dilepas dari komputer Anda.
UninstalledMost=Selesai melepas %1.%n%nBeberapa elemen tidak dapat dihapus. Anda dapat menghapusnya secara manual.
UninstalledAndNeedsRestart=Untuk melengkapi pelepasan %1, komputer Anda harus dimulai ulang.%n%nMulai ulang sekarang?
UninstallDataCorrupted=Berkas "%1" telah rusak. Tidak bisa melepas

; *** Pesan untuk fase pelepasan
ConfirmDeleteSharedFileTitle=Hapus Berkas Bersama?
ConfirmDeleteSharedFile2=Sistem mengindikasi bahwa berkas-berkas bersama berikut tidak lagi dipakai oleh program apa pun. Apakah Anda ingin kami menghapus berkas-berkas tersebut?%n%nJika berkas-berkas tersebut dihapus dan masih ada program yang memakainya, program tersebut mungkin akan berjalan di luar semestinya. Bila Anda tidak yakin, pilih Tidak. Membiarkan berkas tersebut pada komputer Anda tidak akan menimbulkan masalah.
SharedFileNameLabel=Nama berkas:
SharedFileLocationLabel=Lokasi:
WizardUninstalling=Status Pelepasan
StatusUninstalling=Melepas %1...

; *** Blok alasan Shutdown
ShutdownBlockReasonInstallingApp=Memasang %1.
ShutdownBlockReasonUninstallingApp=Melepas %1.

; Pesan khusus berikut tidak digunakan oleh Pemasang itu sendiri, 
; namun bila Anda memakainya di dalam skrip Anda, maka terjemahkan.

[CustomMessages]
NameAndVersion=%1 versi %2
AdditionalIcons=Pintasan tambahan:
CreateDesktopIcon=Buat pintasan di &Desktop
CreateQuickLaunchIcon=Buat pintasan di &Quick Launch
ProgramOnTheWeb=%1 di web
UninstallProgram=Lepas %1
LaunchProgram=Jalankan %1
AssocFileExtension=&Asosiasikan %1 dengan ekstensi berkas %2
AssocingFileExtension=Mengasosiasikan %1 dengan ekstensi berkas %2 ....
AutoStartProgramGroupDescription=Startup:
AutoStartProgram=Jalankan %1 secara otomatis
AddonHostProgramNotFound=%1 tidak dapat ditemukan di dalam map yang Anda pilih.%n%nTetap lanjutkan?
