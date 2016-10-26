; *** Inno Setup version 5.5.3+ Turkish messages ***
; Language	"Turkce" Turkish Translate by "Ceviren"	Kaya Zeren kayazeren@gmail.com
; To download user-contributed translations of this file, go to:
;   http://www.jrsoftware.org/files/istrans/
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).

[LangOptions]
; The following three entries are very important. Be sure to read and 
; understand the '[LangOptions] section' topic in the help file.
LanguageName=T<00FC>rk<00E7>e
LanguageID=$041f
LanguageCodePage=1254
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
SetupAppTitle=Kurulum
SetupWindowTitle=%1 - Kurulumu
UninstallAppTitle=Kaldırma
UninstallAppFullTitle=%1 Kaldırma

; *** Misc. common
InformationTitle=Bilgi
ConfirmTitle=Onay
ErrorTitle=Hata

; *** SetupLdr messages
SetupLdrStartupMessage=%1 yazılımı kurulacak. Devam etmek istiyor musunuz?
LdrCannotCreateTemp=Geçici bir dosya oluşturulamadı. Kurulum iptal edildi
LdrCannotExecTemp=Geçici klasördeki dosya çalıştırılamadığından kurulum iptal edildi

; *** Startup error messages
LastErrorMessage=%1.%n%nHata %2: %3
SetupFileMissing=Kurulum klasöründeki %1 dosyası eksik. Lütfen sorunu çözün ya da yazılımın yeni bir kopyasıyla yeniden deneyin.
SetupFileCorrupt=Kurulum dosyaları bozuk. Lütfen yazılımın yeni bir kopyasıyla yeniden deneyin.
SetupFileCorruptOrWrongVer=Kurulum dosyaları bozulmuş ya da bu kurulum sürümü ile uyumlu değil. Lütfen sorunu çözün ya da yazılımın yeni bir kopyasıyla yeniden deneyin.
InvalidParameter=Komut satırından geçersiz bir parametre gönderildi:%n%n%1
SetupAlreadyRunning=Kurulum zaten çalışıyor.
WindowsVersionNotSupported=Bu yazılım, bilgisayarınızda yüklü olan Windows sürümü ile uyumlu değil.
WindowsServicePackRequired=Bu yazılım, %1 Service Pack %2 ve üzerindeki sürümlerle çalışır.
NotOnThisPlatform=Bu yazılım, %1 üzerinde çalışmaz.
OnlyOnThisPlatform=Bu yazılım, %1 üzerinde çalıştırılmalıdır.
OnlyOnTheseArchitectures=Bu yazılım, yalnız şu işlemci mimarileri için tasarlanmış Windows sürümleriyle çalışır:%n%n%1
MissingWOW64APIs=Kullandığınız Windows sürümü 64-bit kurulumu için gerekli işlevlere sahip değil. Bu sorunu çözmek için lütfen Hizmet Paketi %1 yükleyin.
WinVersionTooLowError=Bu yazılım için %1 sürüm %2 ya da üzeri gereklidir.
WinVersionTooHighError=Bu yazılım, '%1' sürüm '%2' ya da üzerine kurulamaz.
AdminPrivilegesRequired=Bu yazılımı kurmak için Yönetici olarak oturum açmış olmanız gerekir.
PowerUserPrivilegesRequired=Bu yazılımı kurarken, Yönetici ya da Güçlü Kullanıcılar grubunun bir üyesi olarak oturum açmış olmanız gerekir.
SetupAppRunningError=Kurulum, %1 yazılımının çalışmakta olduğunu algıladı.%n%nLütfen yazılımın çalışan tüm kopyalarını kapatıp, devam etmek için Tamam ya da kurulumdan çıkmak için İptal düğmesine tıklayın.
UninstallAppRunningError=Kaldırma, %1 yazılımının çalışmakta olduğunu algıladı.%n%nLütfen yazılımın çalışan tüm kopyalarını kapatıp, devam etmek için Tamam ya da  kurulumdan çıkmak için İptal düğmesine tıklayın.

; *** Misc. errors
ErrorCreatingDir=Kurulum "%1" klasörünü oluşturamadı.
ErrorTooManyFilesInDir="%1" klasörü içinde çok sayıda dosya olduğundan bir dosya oluşturulamadı

; *** Setup common messages
ExitSetupTitle=Kurulumdan Çıkın
ExitSetupMessage=Kurulum tamamlanmadı. Şimdi çıkarsanız, yazılım yüklenmeyecek.%n%nYüklemeyi tamamlamak için istediğiniz zaman kurulum programını yeniden çalıştırabilirsiniz.%n%nKurulumdan çıkılsın mı?
AboutSetupMenuItem=Kurulum H&akkında...
AboutSetupTitle=Kurulum Hakkında
AboutSetupMessage=%1 %2 sürümü%n%3%n%n%1 anasayfa:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< G&eri
ButtonNext=İ&leri >
ButtonInstall=&Kurun
ButtonOK=Tamam
ButtonCancel=İptal
ButtonYes=E&vet
ButtonYesToAll=&Tümüne Evet
ButtonNo=&Hayır
ButtonNoToAll=Tümüne Ha&yır
ButtonFinish=&Bitti
ButtonBrowse=&Gözatın...
ButtonWizardBrowse=Göza&tın...
ButtonNewFolder=Ye&ni Klasör Oluşturun

; *** "Select Language" dialog messages
SelectLanguageTitle=Kurulum Dilini Seçin
SelectLanguageLabel=Kurulum süresince kullanılacak dili seçin:

; *** Common wizard text
ClickNext=Devam etmek için İleri, çıkmak için İptal düğmesine basın.
BeveledLabel=
BrowseDialogTitle=Klasöre Gözatın
BrowseDialogLabel=Aşağıdaki listeden bir klasör seçip, Tamam düğmesine tıklayın.
NewFolderName=Yeni Klasör

; *** "Welcome" wizard page
WelcomeLabel1=[name] Kurulum Yardımcısına Hoşgeldiniz.
WelcomeLabel2=Bilgisayarınıza [name/ver] yazılımı kurulacak.%n%nDevam etmeden önce çalışan diğer tüm programları kapatmanız önerilir.

; *** "Password" wizard page
WizardPassword=Parola
PasswordLabel1=Bu kurulum parola korumalıdır.
PasswordLabel3=Lütfen parolayı yazın ve devam etmek için İleri düğmesine tıklayın. Parolalar büyük küçük harflere duyarlıdır.
PasswordEditLabel=&Parola:
IncorrectPassword=Yazdığınız parola doğru değil. Lütfen yeniden deneyin.

; *** "License Agreement" wizard page
WizardLicense=Lisans Anlaşması
LicenseLabel=Lütfen devam etmeden önce aşağıdaki önemli bilgileri okuyun.
LicenseLabel3=Lütfen Aşağıdaki Lisans Anlaşmasını okuyun. Kuruluma devam edebilmek için bu anlaşmayı kabul etmelisiniz.
LicenseAccepted=Anlaşmayı kabul &ediyorum.
LicenseNotAccepted=Anlaşmayı kabul et&miyorum.

; *** "Information" wizard pages
WizardInfoBefore=Bilgiler
InfoBeforeLabel=Lütfen devam etmeden önce aşağıdaki önemli bilgileri okuyun.
InfoBeforeClickLabel=Kuruluma devam etmeye hazır olduğunuzda İleri düğmesine tıklayın.
WizardInfoAfter=Bilgiler
InfoAfterLabel=Lütfen devam etmeden önce aşağıdaki önemli bilgileri okuyun.
InfoAfterClickLabel=Kuruluma devam etmeye hazır olduğunuzda İleri düğmesine tıklayın.

; *** "User Information" wizard page
WizardUserInfo=Kullanıcı Bilgileri
UserInfoDesc=Lütfen bilgilerinizi yazın.
UserInfoName=K&ullanıcı Adı:
UserInfoOrg=Ku&rum:
UserInfoSerial=&Seri Numarası:
UserInfoNameRequired=Bir ad yazmalısınız.

; *** "Select Destination Directory" wizard page
WizardSelectDir=Hedef Klasörü Seçin
SelectDirDesc=[name] nereye kurulsun?
SelectDirLabel3=[name] yazılımı şu klasöre kurulacak.
SelectDirBrowseLabel=Devam etmek icin İleri düğmesine tıklayın. Farklı bir klasör seçmek için Gözatın düğmesine tıklayın.
DiskSpaceMBLabel=En az [mb] MB disk alanı gereklidir.
CannotInstallToNetworkDrive=Yazılım bir ağ sürücüsü üzerine kurulamaz.
CannotInstallToUNCPath=Yazılım bir UNC yolu üzerine (\\yol gibi) kurulamaz.
InvalidPath=Sürücü adı ile tam yolu yazmalısınız; örneğin: %n%nC:\APP%n%n ya da şu biçimde bir UNC yolu:%n%n\\sunucu\paylaşım
InvalidDrive=Sürücü ya da UNC paylaşımı yok ya da erişilemiyor. Lütfen başka bir tane seçin.
DiskSpaceWarningTitle=Yeterli Disk Alanı Yok
DiskSpaceWarning=Kurulum için %1 KB boş alan gerekli, ancak seçilmiş sürücüde yalnız %2 KB boş alan var.%n%nGene de devam etmek istiyor musunuz?
DirNameTooLong=Klasör adı ya da yol çok uzun.
InvalidDirName=Klasör adı geçersiz.
BadDirName32=Klasör adlarında şu karakterler bulunamaz:%n%n%1
DirExistsTitle=Klasör Zaten Var
DirExists=Klasör:%n%n%1%n%nzaten var. Kurulum için bu klasörü kullanmak ister misiniz?
DirDoesntExistTitle=Klasör Bulunamadı
DirDoesntExist=Klasör:%n%n%1%n%nbulunamadı.Klasörün oluşturmasını ister misiniz?

; *** "Select Components" wizard page
WizardSelectComponents=Bileşenleri Seçin
SelectComponentsDesc=Hangi bileşenler kurulacak?
SelectComponentsLabel2=Kurmak istediğiniz bileşenleri seçin; kurmak istemediğiniz bileşenlerin işaretini kaldırın. Devam etmeye hazır olduğunuzda İleri düğmesine tıklayın.
FullInstallation=Tam Kurulum
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Normal kurulum
CustomInstallation=Özel kurulum
NoUninstallWarningTitle=Varolan Bileşenler
NoUninstallWarning=Kur şu bileşenlerin bilgisayarınıza zaten kurulmuş olduğunu algıladı:%n%n%1%n%n Bu bileşenlerin işaretlerinin kaldırılması bileşenleri kaldırmaz.%n%nGene de devam etmek istiyor musunuz?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Seçili bileşenler için diskte en az [mb] MB bos alan gerekli.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Ek İşlemleri Seçin
SelectTasksDesc=Başka hangi ek işlemler yapılsın?
SelectTasksLabel2=[name] kurulurken yapılmasını istediğiniz ek işleri seçin ve İleri düğmesine tıklayın.

; *** "Başlat Menüsü Dizini Seç" sihirbaz sayfası
WizardSelectProgramGroup=Başlat Menüsü Klasörünü Seçin
SelectStartMenuFolderDesc=Yazılımın kısayolları nereye kurulsun?
SelectStartMenuFolderLabel3=Kur yazılım kısayollarını aşağıdaki Başlat Menüsü klasöründe oluşturacak.
SelectStartMenuFolderBrowseLabel=Devam etmek için İleri düğmesine tıklayın. Farklı bir klasör seçmek için Gözatın düğmesine tıklayın.
MustEnterGroupName=Bir klasör adı yazmalısınız.
GroupNameTooLong=Klasör adı ya da yol çok uzun.
InvalidGroupName=Klasör adı geçersiz.
BadGroupName=Klasör adında şu karakterler bulunamaz:%n%n%1
NoProgramGroupCheck2=Başlat Menüsü klasörü &oluşturulmasın

; *** "Ready to Install" wizard page
WizardReady=Kurulmaya Hazır
ReadyLabel1=[name] bilgisayarınıza kurulmaya hazır.
ReadyLabel2a=Kuruluma devam etmek için İleri düğmesine, ayarları gözden geçirip değiştirmek için Geri düğmesine tıklayın.
ReadyLabel2b=Kuruluma devam etmek için İleri düğmesine tıklayın.
ReadyMemoUserInfo=Kullanıcı bilgileri:
ReadyMemoDir=Hedef konumu:
ReadyMemoType=Kurulum tipi:
ReadyMemoComponents=Seçilmiş bileşenler:
ReadyMemoGroup=Başlat Menüsü klasörü:
ReadyMemoTasks=Ek işlemler:

; *** "Kurulmaya Hazır" sihirbaz sayfası
WizardPreparing=Kuruluma Hazırlanılıyor
PreparingDesc=[name] bilgisayarınıza kurulmaya hazırlanıyor.
PreviousInstallNotCompleted=Önceki yazılım kurulumu ya da kaldırılması tamamlanmamış. Bu kurulumun tamamlanması için bilgisayarınızı yeniden başlatmalısınız.%n%nBilgisayarınızı yeniden başlattıktan sonra işlemi tamamlamak için [name] kurulumunu yeniden çalıştırın.
CannotContinue=Kuruluma devam edilemiyor. Çıkmak için İptal düğmesine tıklayın.
ApplicationsFound=Şu uygulamalar, kurulum tarafından güncellenmesi gereken dosyaları kullanıyor. Kurulumun bu uygulamaları kendiliğinden kapatmasına izin vermeniz önerilir.
ApplicationsFound2=Şu uygulamalar, kurulum tarafından güncellenmesi gereken dosyaları kullanıyor. Kurulumun bu uygulamaları kendiliğinden kapatmasına izin vermeniz önerilir. Tamamlandıktan sonra kurulum, uygulamaları yeniden başlatmayı deneyecek.
CloseApplications=&Uygulamalar kapatılsın
DontCloseApplications=Uygulamalar &kapatılmasın
ErrorCloseApplications=Kurulum, uygulamaları kapatamadı. Kurulum tarafından güncellenmesi gereken dosyaları kullanan uygulamaları el ile kapatmanız önerilir.

; *** "Kuruluyor" sihirbaz
WizardInstalling=Kuruluyor
InstallingLabel=Lütfen [name] bilgisayarınıza kurulurken bekleyin.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=[name] kurulum yardımcısı tamamlanıyor
FinishedLabelNoIcons=Bilgisayarınıza [name] kurulumu tamamlandı.
FinishedLabel=Bilgisayarınıza [name] kurulumu tamamlandı. Simgeleri yüklemeyi seçtiyseniz, uygulamayı simgelere tıklayarak başlatabilirsiniz.
ClickFinish=Kurulumdan çıkmak için Bitti düğmesine tıklayın.
FinishedRestartLabel=[name] kurulumunun tamamlanması için, bilgisayarınız yeniden başlatılmalı. Şimdi yeniden başlatmak ister misiniz?
FinishedRestartMessage=[name] kurulumunun tamamlanması için, bilgisayarınız yeniden başlatılmalı.%n%nŞimdi yeniden başlatmak ister misiniz?
ShowReadmeCheck=Evet README dosyasına bakmak istiyorum
YesRadio=&Evet, bilgisayar şimdi yeniden başlatılsın
NoRadio=&Hayır, bilgisayarı daha sonra yeniden başlatacağım
; used for example as 'Run MyProg.exe'
RunEntryExec=%1 çalıştırılsın
; used for example as 'View Readme.txt'
RunEntryShellExec=%1 görüntülensin

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Kurulum için Sıradaki Disk Gerekli
SelectDiskLabel2=Lütfen %1. diski takıp Tamam düğmesine tıklayın.%n%nDiskteki dosyalar aşağıdakinden farklı bir klasörde bulunuyorsa, doğru yolu yazın ya da Gözatın düğmesine tıklayarak doğru klasörü seçin.
PathLabel=&Yol:
FileNotInDir2="%1" dosyası "%2" içinde yok. Lütfen doğru diski takın ya da başka bir klasör seçin.
SelectDirectoryLabel=Lütfen sonraki diskin konumunu belirtin.

; *** Installation phase messages
SetupAborted=Kurulum tamamlanamadı.%n%nLütfen sorunu düzelterek kurulumu yeniden çalıştırın.
EntryAbortRetryIgnore=Yeniden denemek için Yeniden Deneyin düğmesine, devam etmek için Yoksayın düğmesine, kurulumu iptal etmek için Vazgeçin düğmesine tıklayın.

; *** Installation status messages
StatusClosingApplications=Uygulamalar kapatılıyor...
StatusCreateDirs=Klasörler oluşturuluyor...
StatusExtractFiles=Dosyalar ayıklanıyor...
StatusCreateIcons=Kısayollar oluşturuluyor...
StatusCreateIniEntries=INI kayıtları oluşturuluyor...
StatusCreateRegistryEntries=Kayıt Defteri kayıtları oluşturuluyor...
StatusRegisterFiles=Dosyalar kaydediliyor...
StatusSavingUninstall=Kaldırma bilgileri kaydediliyor...
StatusRunProgram=Kurulum tamamlanıyor...
StatusRestartingApplications=Uygulamalar yeniden başlatılıyor...
StatusRollback=Değişiklikler geri alınıyor...

; *** Misc. errors
ErrorInternal2=İç hata: %1
ErrorFunctionFailedNoCode=%1 tamamlanamadı.
ErrorFunctionFailed=%1 tamamlanamadı; kod %2
ErrorFunctionFailedWithMessage=%1 tamamlanamadı; kod %2.%n%3
ErrorExecutingProgram=Şu dosya yürütülemedi:%n%1

; *** Registry errors
ErrorRegOpenKey=Kayıt defteri anahtarı açılırken bir hata oluştu:%n%1%2
ErrorRegCreateKey=Kayıt defteri anahtarı oluşturulurken bir hata oluştu:%n%1%2
ErrorRegWriteKey=Kayıt defteri anahtarı yazılırken bir hata oluştu:%n%1%2

; *** INI errors
ErrorIniEntry="%1" dosyasına INI kaydı eklenirken bir hata oluştu.

; *** File copying errors
FileAbortRetryIgnore=Yeniden denemek için Yeniden Deneyin düğmesine, bu dosyayı atlamak için (önerilmez) Yoksayın düğmesine, kurulumu iptal etmek için Vazgeçin düğmesine tıklayın.
FileAbortRetryIgnore2=Yeniden denemek için Yeniden Deneyin düğmesine, devam etmek için (önerilmez) Yoksayın düğmesine, kurulumu iptal etmek için Vazgeçin düğmesine tıklayın.
SourceIsCorrupted=Kaynak dosya bozuk
SourceDoesntExist="%1" kaynak dosyası bulunamadı
ExistingFileReadOnly=Varolan dosya salt okunabilir olarak işaretlenmiş.%n%nSalt okunur özniteliğini kaldırıp yeniden denemek için Yeniden Deneyin düğmesine, bu dosyayı atlamak için Yoksayın düğmesine, kurulumunu iptal etmek için Vazgeçin düğmesine tıklayın.
ErrorReadingExistingDest=Varolan dosya okunmaya çalışılırken bir hata oluştu.
FileExists=Dosya zaten var.%n%nKurulum bu dosyanın üzerine yazsın mı?
ExistingFileNewer=Varolan dosya, kurulum tarafından yazılmaya çalışılandan daha yeni.Varolan dosyayı korumanız önerilir %n%nVarolan dosya korunsun mu?
ErrorChangingAttr=Varolan dosyanın öznitelikleri değiştirilirken bir hata oluştu:
ErrorCreatingTemp=Hedef klasörde dosya oluşturulurken bir hata oluştu:
ErrorReadingSource=Kaynak dosya okunurken bir hata oluştu:
ErrorCopying=Dosya kopyalanırken bir hata oluştu:
ErrorReplacingExistingFile=Varolan dosya değiştirilirken bir hata oluştu.
ErrorRestartReplace=Yeniden başlatmada değiştirme tamamlanamadı:
ErrorRenamingTemp=Hedef klasördeki dosyanın adı değiştirilirken bir hata oluştu:
ErrorRegisterServer=DLL/OCX kayıt edilemedi: %1
ErrorRegSvr32Failed=RegSvr32 şu kod ile işlemi tamamlayamadı: %1
ErrorRegisterTypeLib=Tip kitaplığı kaydedilemedi: %1

; *** Post-installation errors
ErrorOpeningReadme=README dosyası açılırken bir hata oluştu.
ErrorRestartingComputer=Kurulum bilgisayarınızı yeniden başlatamıyor. Lütfen bilgisayarınızı yeniden başlatın.

; *** Uninstaller messages
UninstallNotFound="%1" dosyası bulunamadı. Yazılım kaldırılamıyor.
UninstallOpenError="%1" dosyası açılamadı. Yazılım kaldırılamıyor.
UninstallUnsupportedVer="%1" kaldırma günlük dosyasının biçimi, bu kaldırıcı sürümü tarafından anlaşılamadı. Yazılım kaldırılamıyor.
UninstallUnknownEntry=Kaldırma günlüğünde bilinmeyen bir kayıt (%1) bulundu.
ConfirmUninstall=%1 yazılımını tüm bileşenleri ile birlikte tamamen kaldırmak istediğinize emin misiniz?
UninstallOnlyOnWin64=Bu kurulum yalnız 64-bit Windows üzerinden kaldırılabilir.
OnlyAdminCanUninstall=Bu kurulum yalnız yönetici haklarına sahip bir kullanıcı tarafından kaldırılabilir.
UninstallStatusLabel=Lütfen %1 yazılımı bilgisayarınızdan kaldırılırken bekleyin.
UninstalledAll=%1 yazılımı bilgisayarınızdan kaldırıldı.
UninstalledMost=%1 yazılımı kaldırıldı.%n%nBazı bileşenler kaldırılamadı. Bunları el ile silebilirsiniz.
UninstalledAndNeedsRestart=%1 kaldırma işlemini tamamlamak için bilgisayarınız yeniden başlatılmalı.%n%nŞimdi yeniden başlatmak ister misiniz?
UninstallDataCorrupted="%1" dosyası bozulmuş. Kaldırılamıyor.

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Paylaşılan Dosya Silinsin mi?
ConfirmDeleteSharedFile2=Sisteme göre, paylaşılan şu dosya başka bir program tarafından kullanılmıyor ve kaldırılabilir. Bu paylaşılmış dosyayı silmek ister misiniz?%n%nBaşka herhangi bir yazılım bu dosyayı halen kullanıyor ise, sildiğinizde diğer yazılım düzgün çalışmayabilir. Emin değilseniz Hayır düğmesine tıklayın. Dosyayı sisteminizde bırakmanın bir zararı olmaz.
SharedFileNameLabel=Dosya adı:
SharedFileLocationLabel=Konum:
WizardUninstalling=Kaldırma Durumu
StatusUninstalling=%1 kaldırılıyor...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=%1 kuruluyor.
ShutdownBlockReasonUninstallingApp=%1 kaldırılıyor.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 %2 sürümü
AdditionalIcons=Ek simgeler:
CreateDesktopIcon=Masaüstü simg&esi oluşturulsun
CreateQuickLaunchIcon=Hızlı Başlat simgesi &oluşturulsun
ProgramOnTheWeb=%1 Web Sitesi
UninstallProgram=%1 Yazılımını Kaldırın
LaunchProgram=%1 Yazılımı Çalıştırılsın
AssocFileExtension=%1 y&azılımı ile %2 dosya uzantısı ilişkilendirilsin
AssocingFileExtension=%1 y&azılımı ile %2 dosya uzantısı ilişkilendiriliyor...
AutoStartProgramGroupDescription=Başlangıç:
AutoStartProgram=%1 kendiliğinden başlatılsın
AddonHostProgramNotFound=%1 seçtiğiniz klasörde bulunamadı.%n%nYine de devam etmek istiyor musunuz?
