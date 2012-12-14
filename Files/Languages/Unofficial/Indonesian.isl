; *** Inno Setup version 5.1.11+ Indonesian messages ***
;
; To download user-contributed translations of this file, go to:
;   http://www.jrsoftware.org/is3rdparty.php
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).
;
; Alih bahasa oleh:
;   Jaimy Azle (jazle@sat.net.id) - http://delphi.log.web.id
; Inno Setup Indonesian Translation home page:
;   http://delphi.log.web.id/comptools/index.html#tools
;
; Update: Translation is based on Indonesian goverment regulation
;         translation guidance No. 02/2002
; http://vlsm.org/etc/baku-0.txt
;

[LangOptions]
; The following three entries are very important. Be sure to read and
; understand the '[LangOptions] section' topic in the help file.
LanguageName=Indonesia
LanguageID=$0421
LanguageCodePage=0
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
SetupAppTitle=Instalasi
SetupWindowTitle=Instalasi - %1
UninstallAppTitle=Deinstalasi
UninstallAppFullTitle=Deinstalasi %1

; *** Misc. common
InformationTitle=Informasi
ConfirmTitle=Konfirmasi
ErrorTitle=Galat

; *** SetupLdr messages
SetupLdrStartupMessage=Program ini akan menginstal aplikasi %1. Anda akan melanjutkannya?
LdrCannotCreateTemp=Gagal membuat berkas temporer. Instalasi dibatalkan
LdrCannotExecTemp=Gagal mengeksekusi berkas dalam pelipat temporer. Instalasi dibatalkan

; *** Startup error messages
LastErrorMessage=%1.%n%nGalat %2: %3
SetupFileMissing=Berkas %1 tidak ditemukan dalam pelipat instalasi. Mohon periksa kembali atau ganti dengan kopi program instalasi yang lebih baru.
SetupFileCorrupt=Berkas instalasi rusak. Mohon ganti dengan kopi program instalasi yang lebih baru.
SetupFileCorruptOrWrongVer=Berkas instalasi rusak, atau tidak kompatibel dengan versi program instalasi ini. Mohon periksa kembali atau ganti dengan kopi program instalasi yang lebih baru.
NotOnThisPlatform=Program ini tidak bisa berjalan pada %1.
OnlyOnThisPlatform=Program ini harus dijalankan pada %1.
OnlyOnTheseArchitectures=Aplikasi ini hanya bisa diinstal pada versi Windows yang didisain untuk arsitektur prosesor berikut:%n%n%1
MissingWOW64APIs=Versi Windows yang anda jalankan tidak memiliki fungsionalitas yang dibutuhkan oleh program instalasi untuk melakukan instalasi 64-bit. Untuk memperbaikinya, silahkan instal Service Pack %1.
WinVersionTooLowError=Program ini membutuhkan %1 versi %2 atau yang lebih baru.
WinVersionTooHighError=Program ini tidak dapat diinstal pada %1 versi %2 atau yang lebih baru.
AdminPrivilegesRequired=Anda harus log masuk sebagai administrator saat menginstal program ini.
PowerUserPrivilegesRequired=Anda harus log masuk sebagai administrator atau dalam grup Power Users saat menginstal program ini.
SetupAppRunningError=Program instalasi mendeteksi adanya aplikasi %1 yang masih berjalan.%n%nMohon tutup program tersebut, kemudian klik OK untuk melanjutkan, atau Batal untuk keluar.
UninstallAppRunningError=Program deinstalasi mendeteksi adanya aplikasi %1 yang masih berjalan.%n%nMohon tutup program tersebut, kemudian klik OK untuk melanjutkan, atau Batal untuk keluar.

; *** Misc. errors
ErrorCreatingDir=Gagal membuat pelipat "%1"
ErrorTooManyFilesInDir=Gagal membuat berkas di pelipat "%1" karena sudah terlalu banyak berkas di dalamnya

; *** Setup common messages
ExitSetupTitle=Keluar dari program instalasi
ExitSetupMessage=Instalasi belum selesai. Jika anda keluar sekarang, program tidak akan diinstal.%n%nAnda bisa menjalankan program ini di lain waktu untuk menyelesaikannya.%n%nKeluar dari program instalasi?
AboutSetupMenuItem=&Tentang Instalasi...
AboutSetupTitle=Tentang Instalasi
AboutSetupMessage=%1 versi %2%n%3%n%n%1 laman:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< &Balik
ButtonNext=&Lanjut >
ButtonInstall=&Instal
ButtonOK=OK
ButtonCancel=Batal
ButtonYes=&Ya
ButtonYesToAll=Ya &semua
ButtonNo=&Tidak
ButtonNoToAll=T&idak semua
ButtonFinish=&Selesai
ButtonBrowse=&Rambah...
ButtonWizardBrowse=Ra&mbah...
ButtonNewFolder=&Buat Pelipat Baru

; *** "Select Language" dialog messages
SelectLanguageTitle=Pilihan Bahasa
SelectLanguageLabel=Pilih bahasa yang hendak digunakan selama proses instalasi:

; *** Common wizard text
ClickNext=Klik Lanjut untuk meneruskan, atau Batal untuk keluar dari Instalasi.
BeveledLabel=
BrowseDialogTitle=Rambah Untuk Pelipat
BrowseDialogLabel=Pilih pelipat dalam daftar berikut, kemudian klik OK.
NewFolderName=Pelipat Baru

; *** "Welcome" wizard page
WelcomeLabel1=Selamat Datang di Program Instalasi [name]
WelcomeLabel2=Program ini akan menginstal [name/ver] pada komputer anda.%n%nDisarankan untuk menutup terlebih dulu semua program yang berjalan sebelum anda melanjutkan.

; *** "Password" wizard page
WizardPassword=Sandi lewat
PasswordLabel1=Instalasi ini diproteksi dengan sandi lewat.
PasswordLabel3=Silahkan masukkan sandi lewat, kemudian klik Lanjut untuk melanjutkan. sandi lewat bersifat karakter sensitif.
PasswordEditLabel=&Sandi lewat:
IncorrectPassword=Sandi lewat yang anda masukkan tidak valid. Mohon coba kembali.

; *** "License Agreement" wizard page
WizardLicense=Kesepakatan atas Lisensi
LicenseLabel=Mohon dibaca terlebih dulu informasi penting berikut sebelum melanjutkan.
LicenseLabel3=Mohon dibaca terlebih dulu informasi kesepakatan atas lisensi berikut. Anda harus menyetujui poin-poin di dalamnya sebelum melanjutkan instalasi.
LicenseAccepted=Saya &setuju dengan kesepakatan tersebut
LicenseNotAccepted=Saya &tidak setuju dengan kesepakatan tersebut

; *** "Information" wizard pages
WizardInfoBefore=Informasi
InfoBeforeLabel=Mohon dibaca terlebih dulu informasi penting berikut sebelum melanjutkan.
InfoBeforeClickLabel=Jika anda sudah siap untuk melanjutkan instalasi, klik Lanjut.
WizardInfoAfter=Informasi
InfoAfterLabel=Mohon dibaca terlebih dulu  informasi penting berikut sebelum melanjutkan.
InfoAfterClickLabel=Jika anda sudah siap untuk melanjutkan, klik Lanjut.

; *** "User Information" wizard page
WizardUserInfo=Informasi Pengguna
UserInfoDesc=Silahkan masukkan data diri anda.
UserInfoName=&Nama Pengguna:
UserInfoOrg=&Organisasi:
UserInfoSerial=Nomor &Seri:
UserInfoNameRequired=Anda harus memasukkan nama.

; *** "Select Destination Directory" wizard page
WizardSelectDir=Pilih Pelipat Tujuan
SelectDirDesc=Dimana program [name] akan anda instal?
SelectDirLabel3=Program [name] akan diinstall pada pelipat berikut.
SelectDirBrowseLabel=Untuk melanjutkan, klik Lanjut. Jika hendak memilih pelipat lainnya, klik Rambah.
DiskSpaceMBLabel=Program membutuhkan minimal [mb] MB ruang pada disk.
ToUNCPathname=Instalasi tidak dapat dilakukan pada nama path UNC. Untuk menginstal melalui network, silahkan map network drive-nya terlebih dulu.
InvalidPath=Anda harus memasukkan path secara lengkap dengan drivenya; contoh:%n%nC:\APP%n%natau UNC path dalam format:%n%n\\server\share
InvalidDrive=Drive atau path UNC yang anda pilih tidak ada atau tidak dapat diakses. Silahkan pilih yang lainnya.
DiskSpaceWarningTitle=Ruang disk tidak mencukupi
DiskSpaceWarning=Instalasi membutuhkan paling tidak %1 KB ruang disk untuk melakukan proses, drive yang terpilih hanya memliki %2 KB.%n%nAkan dilanjutkan saja?
DirNameTooLong=Nama pelipat atau path tersebut terlalu panjang.
InvalidDirName=Nama pelipat tersebut tidak valid.
BadDirName32=Nama direktori tidak boleh menyertakan karakter berikut:%n%n%1
DirExistsTitle=Direktori tersebut sudah ada
DirExists=Direktori:%n%n%1%n%nsudah ada. Akan melanjutkan instalasi pada direktori tersebut?
DirDoesntExistTitle=Direktori Tidak Valid
DirDoesntExist=Direktori:%n%n%1%n%ntidak valid. Apakah direktori tersebut akan dibuat ulang?

; *** "Select Components" wizard page
WizardSelectComponents=Pilih Komponen
SelectComponentsDesc=Komponen apa saja yang akan anda install?
SelectComponentsLabel2=Pilih komponen yang ingin anda install; hapus komponen yang tidak ingin anda install. Klik Lanjut untuk melanjutkan proses.
FullInstallation=Instalasi Penuh
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Instalasi Ramping
CustomInstallation=Kustomisasi Instalasi
NoUninstallWarningTitle=Komponent Sudah Ada
NoUninstallWarning=Instalasi mendeteksi komponen berikut sudah terinstall pada komputer:%n%n%1%n%nTidak memilih komponen-komponen tersebut tidak akan menghapus keberadaannya.%n%nAnda akan melanjutkan?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Pilihan tersebut membutuhkan paling tidak [mb] MB ruang disk.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Pilih Task Tambahan
SelectTasksDesc=Task mana saja yang akan di jalankan?
SelectTasksLabel2=Pilih task tambahan yang ingin dijalankan dalam prosedur instalasi selama menginstalasikan [name], kemudian klik Lanjut.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Pilih Pelipat Start Menu
SelectStartMenuFolderDesc=Di pelipat mana shortcut program akan diletakkan?
SelectStartMenuFolderLabel3=Setup akan membuat shortcut program pada pelipat Start Menu berikut.
SelectStartMenuFolderBrowseLabel=Untuk melanjutkan, klik Next. Jika ingin memilih pelipat lainnya, klik Rambah.
MustEnterGroupName=Anda harus memasukkan nama pelipatnya.
GroupNameTooLong=Nama pelipat atau path tersebut terlalu panjang.
InvalidGroupName=Nama pelipat tersebut tidak valid.
BadGroupName=Nama direktori tidak boleh menyertakan karakter berikut:%n%n%1
NoProgramGroupCheck2=&Jangan buat pelipat Start Menu

; *** "Ready to Install" wizard page
WizardReady=Siap Memulai Instalasi
ReadyLabel1=Proses instalasi aplikasi [name] siap dimulai.
ReadyLabel2a=Klik Instal untuk memulai proses, atau klik Kembali jika ingin mereview atau mengubah setting yang ada.
ReadyLabel2b=Klik Instal untuk memulai proses instalasi.
ReadyMemoUserInfo=Informasi User:
ReadyMemoDir=Direktori Tujuan:
ReadyMemoType=Tipe Instalasi:
ReadyMemoComponents=Komponen Terpilih:
ReadyMemoGroup=Pelipat Start Menu:
ReadyMemoTasks=Task Tambahan:

; *** "Preparing to Install" wizard page
WizardPreparing=Mempersiapkan Proses Instalasi
PreparingDesc=Setup sedang mempersiapkan instalasi [name] pada komputer anda.
PreviousInstallNotCompleted=Instalasi/Deinstalasi program sebelumnya belum selesai. Anda akan memerlukan restart komputer anda untuk memastikan instalasi/deinstalasi selesai.%n%nSetelah merestart komputer, jalankan instalasi lagi untuk menyelesaikan proses instalasi aplikasi [name].
CannotContinue=Instalasi tidak dapat dilanjutkan. Silahkan tekan Batal untuk keluar.

; *** "Installing" wizard page
WizardInstalling=Sedang Menginstal
InstallingLabel=Mohon tunggu selama proses instalasi [name] pada komputer anda.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Finalisasi Instalasi Aplikasi [name]
FinishedLabelNoIcons=Proses instalasi aplikasi [name] telah selesai.
FinishedLabel=Setup telah selesai menginstalkan [name] pada komputer anda. Aplikasi tersebut bisa dijalankan dengan memilih icon yang telah terinstall.
ClickFinish=Klik Selesai untuk keluar dari program instalasi.
FinishedRestartLabel=Untuk menyelesaikan proses instalasi [name], Program harus merestart komputer anda. Komputer akan direstart sekarang?
FinishedRestartMessage=Untuk menyelesaikan proses instalasi [name], Program harus merestart komputer anda.%n%nKomputer akan direstart sekarang?
ShowReadmeCheck=Ya, Saya ingin membaca berkas README
YesRadio=&Ya, Restart komputer sekarang juga
NoRadio=&Tidak, saya akan restart komputer sendiri
; used for example as 'Run MyProg.exe'
RunEntryExec=Jalankan %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Tilik %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Program Membutuhkan Disk Berikutnya
SelectDiskLabel2=Mohon masukkan Disk %1 dan klik OK.%n%nJika berkas pada disk ditemukan pada pelipat berbeda dari yang ditampilkan, Masukkan path yang benar atau klik Rambah.
PathLabel=&Path:
FileNotInDir2=Berkas "%1" tidak ditemukan di "%2". Mohon masukkan disk yang benar atau pilih pelipat yang lain.
SelectDirectoryLabel=Spesifikasikan lokasi disk berikutnya.

; *** Installation phase messages
SetupAborted=Instalasi belum selesai.%n%nMohon koreksi kesalahan yang ada dan jalankan program instalasi ini lagi.
EntryAbortRetryIgnore=Klik Coba Lagi untuk mencobanya kembali, Indahkan untuk mengindahkannya, atau Batal untuk membatalkan instalasi.

; *** Installation status messages
StatusCreateDirs=Membuat pelipat...
StatusExtractFiles=Mengekstrak berkas...
StatusCreateIcons=Membuat shortcut program...
StatusCreateIniEntries=Memasukkan entri pada berkas INI...
StatusCreateRegistryEntries=Memasukkan entri pada registry...
StatusRegisterFiles=Meregistrasikan berkas...
StatusSavingUninstall=Menyimpan informasi untuk deinstalasi...
StatusRunProgram=Finalisasi instalasi...
StatusRollback=Mengembalikan perubahan yang dilakukan...

; *** Misc. errors
ErrorInternal2=Kesalahan internal: %1
ErrorFunctionFailedNoCode=%1 gagal
ErrorFunctionFailed=%1 gagal; kode %2
ErrorFunctionFailedWithMessage=%1 gagal; kode %2.%n%3
ErrorExecutingProgram=Gagal mengeksekusi berkas:%n%1

; *** Registry errors
ErrorRegOpenKey=Gagal membuka registry key:%n%1\%2
ErrorRegCreateKey=Gagal membuat registry key:%n%1\%2
ErrorRegWriteKey=Gagal menulis to registry key:%n%1\%2

; *** INI errors
ErrorIniEntry=Gagal membuat entry INI pada berkas "%1".

; *** File copying errors
FileAbortRetryIgnore=Klik Coba Lagi untuk mencoba lagi, Indahkan untuk mengindahkan berkas ini (tidak direkomendasikan), atau Batal untuk membatalkan instalasi.
FileAbortRetryIgnore2=Klik Coba Lagi untuk mencoba lagi, Indahkan untuk mengindahkan berkas ini (tidak direkomendasikan), atau Batal untuk membatalkan instalasi.
SourceIsCorrupted=Berkas asal telah rusak
SourceDoesntExist=Berkas asal "%1" tidak valid
ExistingFileReadOnly=Berkas yang ada tercatat sebagai read-only.%n%nKlik Coba Lagi untuk membuang catatan atribut read-only dan coba lagi, Indahkan untuk mengindahkan berkas ini, atau Batal untuk membatalkan instalasi.
ErrorReadingExistingDest=Kesalahan muncul saat mencoba membaca berkas berikut:
FileExists=Berkas sudah ada.%n%nApakah berkas tersebut akan ditimpa saja?
ExistingFileNewer=Berkas yang sudah ada lebih baru dari yang hendak diinstal. Direkomendasikan untuk membiarkan berkas tersebut.%n%nApakah akan membiarkan berkas tersebut?
ErrorChangingAttr=Kesalahan muncul saat mencoba merubah atribut berkas berikut:
ErrorCreatingTemp=Kesalahan muncul saat mencoba membuat berkas pada direktori tujuan:
ErrorReadingSource=Kesalahan muncul saat mencoba membaca berkas asal:
ErrorCopying=Kesalahan muncul saat mencoba meng-copy berkas:
ErrorReplacingExistingFile=Kesalahan muncul saat mencoba me-replace berkas:
ErrorRestartReplace=RestartReplace gagal:
ErrorRenamingTemp=Kesalahan muncul saat mencoba me-rename berkas pada direktori tujuan:
ErrorRegisterServer=Gagal meregister berkas DLL/OCX: %1
ErrorRegisterServerMissingExport=DllRegisterServer export tidak ditemukan
ErrorRegisterTypeLib=Gagal meregister type library: %1

; *** Post-installation errors
ErrorOpeningReadme=Kesalahan muncul saat mencoba membuka berkas README.
ErrorRestartingComputer=Setup tidak bisa meresart komputer anda. Silahkan lakukan secara manual.

; *** Uninstaller messages
UninstallNotFound=Berkas "%1" tidak ditemukan. Tidak dapat melanjutkan proses deinstalasi
UninstallOpenError=Berkas "%1" tidak bisa dibuka. Tidak dapat melanjutkan proses deinstalasi
UninstallUnsupportedVer=Arsip log berkas "%1" tidak dalam format yang dikenali oleh versi uninstaller ini. Deinstalasi tidak dapat dilanjutkan
UninstallUnknownEntry=Entri tidak dikenal (%1) ditemukan pada arsip log deinstalasi
ConfirmUninstall=Yakinkah anda untuk menghapus %1 beserta seluruh komponen yang terkait?
UninstallOnlyOnWin64=Instalasi ini hanya bisa melakukan deinstalasi pada Windows 64-bit.
OnlyAdminCanUninstall=Prosesi deinstalasi hanya bisa dilakukan oleh user yang memiliki kewenangan administratif.
UninstallStatusLabel=Mohon tunggu selama aplikasi %1 sedang dihapus dari komputer anda.
UninstalledAll=Aplikasi %1 berhasil dihapus dari komputer anda.
UninstalledMost=Deinstalasi %1 selesai.%n%nBeberapa elemen tidak berhasil dihapus dan harus anda lakukan secara manual.
UninstalledAndNeedsRestart=Untuk menyelesaikan deinstalasi aplikasi %1, komputer anda harus direstart.%n%nKomputer akan direstart sekarang?
UninstallDataCorrupted=Berkas "%1" telah rusak. Tidak dapat melanjutkan proses deinstalasi

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Hapus Shared File?
ConfirmDeleteSharedFile2=Sistem mengindikasikan bahwa shared berkas berikut tidak digunakan oleh program apapun. Apakah anda akan menghapus shared berkas ini?%n%nJika ternyata ada program yang masih membutuhkan berkas tersebut dan ternyata terhapus, Program tersebut tidak akan berjalan sebagaimana mestinya. Jika anda ragu, Pilih Tidak. Membiarkan berkas pada sistem anda tidak akan menyebabkan kerusakan apapun.
SharedFileNameLabel=Nama berkas:
SharedFileLocationLabel=Lokasi:
WizardUninstalling=Status Deinstalasi
StatusUninstalling=Deinstalasi %1...

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 versi %2
AdditionalIcons=Tambahan ikon:
CreateDesktopIcon=Buat ikon di &desktop
CreateQuickLaunchIcon=Buat ikon &Quick Launch
ProgramOnTheWeb=%1 di Web
UninstallProgram=Deinstal %1
LaunchProgram=Jalankan %1
AssocFileExtension=&Asosiasikan %1 dengan ekstensi berkas %2
AssocingFileExtension=Mengasosiasikan %1 dengan ekstensi berkas %2 ...
