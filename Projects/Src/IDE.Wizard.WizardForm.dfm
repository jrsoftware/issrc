object WizardForm: TWizardForm
  Left = 228
  Top = 106
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = '*'
  ClientHeight = 430
  ClientWidth = 594
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    594
    430)
  TextHeight = 13
  object Bevel: TBevel
    Left = 0
    Top = 386
    Width = 602
    Height = 1
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsTopLine
  end
  object RequiredLabel1: TNewStaticText
    Left = 8
    Top = 404
    Width = 21
    Height = 14
    Anchors = [akLeft, akBottom]
    Caption = 'bold'
    Enabled = False
    TabOrder = 4
  end
  object RequiredLabel2: TNewStaticText
    Left = 33
    Top = 404
    Width = 51
    Height = 14
    Anchors = [akLeft, akBottom]
    Caption = ' = required'
    Enabled = False
    TabOrder = 5
  end
  object OuterNotebook: TNewNotebook
    Left = 0
    Top = 0
    Width = 595
    Height = 386
    ActivePage = MainPage
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    object WelcomePage: TNewNotebookPage
      DesignSize = (
        595
        386)
      object WelcomeImage: TImage
        Left = 0
        Top = 0
        Width = 202
        Height = 386
        Anchors = [akLeft, akTop, akBottom]
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000CA0000
          01820806000000A1195C1D000000B66558496649492A000800000005001A0105
          00010000004A0000001B01050001000000520000002801030001000000020000
          0031010200100000005A00000069870400010000006A00000000000000600000
          000100000060000000010000005061696E742E4E455420352E312E3900030000
          900700040000003032333001A00300010000000100000005A004000100000094
          0000000000000002000100020004000000523938000200070004000000303130
          30000000004CCFC048258FD7A0000012374944415478DAEDDD6D8C1D5519C0F1
          99B9BBDBD7DD02DDF25244142815358210440D356AC104AAA64294228A1A4581
          A888F2F20510453FA0083131BC6A022A5A4DA8542C7EA04452F8202168C52802
          A226D6AE4A51D82EB6DBEE9D719E81BB7B77775E9EB967E6CEDC7BFEBFA48176
          B7BB7777EFBF73CF993367DC7FEE9E6C3A0052B9840264231440815000054201
          1408055020144081500005420114080550201440815000054201140805502014
          4081500005420114080550201440815000054201140805502014408150000542
          0114080550201440815000054201140805502014408150000542011408055020
          1440815000054201140805502014408150000542011408055020144081500005
          4201140805502014408150000542011408055020144081500005420114080550
          2014408150000542011408055020144081500005420114080550201440815000
          0542011408055020144081500005420114080550201440815000054201140805
          5020144081500005420114080550201440815000054201140805502014408150
          0005420114080550201440815000054201140805502014408150000542011408
          055020144081500005420114080550201440A12F4271773DE578BFB8D273F6ED
          76FC33BF1104AF3A29A8FA31A1BFF445288D4D9FF1BC9DDBA3FFF7475739CD0D
          3FF4AB7E4CE82FFD11CA9DEB3D6F626CFAF7FB3FFB28A1A0507D11CAE0774EF1
          DA7F4F28281AA1000A840228F47C2832E335B0F17C4241A97A3F941D8FBB03F7
          5EECB6FF19A1A068840228F4652853EB6FE6A4230AD5F3A1788FDEEE361EFB1E
          A1A054840228100AA0D0F3A1341EB8D6F59EFA25A1A054BD1F4ADB82C8164241
          D1CA0B6572B7E33EB9C575470E75FCA3DE59DA93B68A50BCBF3CE43ACF3DE3F8
          276C089C05C3657D1AD44869A1B8DB7FE40E3CF2EDE82591BF7A5DD03CFD9A52
          9EB8B1A19C7A49109CF0E1523E9FF7F08D6EE3773F79E5EB3A23FCBAAEE5C865
          81D242917F751BF75F393D76F05FFB8EA079DA3585FF0BDCB87DADE7ED9B98F5
          67CD933F19F8A77CBAD8277078846C6CB9DCF576FE76FA6BD27C9E679EDFEB4C
          4C4E15FA50F2D833E5BB3B5E9874962F1E70D61E73205177A8D431CADC817674
          51D599DFF09D9195857D8EB90B2245D1A1B4AEA06CBFE6457B34B978F3B3DEF6
          B1970AFB7A4DAC5ABED0B9EB83C7B26AA103A50FE6BD5F5CE636FEF6F04C2C43
          4B1DFFAC9BFD607475211FBFEC50E4C8E86EBDCE6D3F6AF9CB8F719A67DDEA6B
          8E8EEFF9DEEFBD89FDF5F987FC5D472D0BBEFE9E23EBF3807A44F9B35EF29265
          D3859EF7FC9FA7FF288A65CD1782E0B8F719FFC0CA0CC57BF40EB7F1D877674D
          3DFB838B9DE6C77FAE8AE4776313EE459BFFE266BE63979D79EC81C155EF3E82
          5872E8CEF4F0F84EA7F1E3F33C6FFFFF66FD71F3E44F854FE80B3AFE81C5ADF3
          7AF9E31A8622716FFDAAEBFD75DBBC48FCB36F551F0DAFDFB6C3DBFCC7FF14FC
          CD2C06B1E4D3B5F328D1EBFC7B2E9C178BC98C5819A1448F73EB759EB7EB9979
          6F6B9E797DA09DEA7E62EC25F7C2CDCFD6EE68D28E58F4BA7AC2D17DF23E77E0
          C1AFCD7BF24483FC0FDCA27A3933EBE32585F2A60F05FE3BBE94FB0910371E69
          C933E52C0FE8BE3FFDC79BD857DDB879C78B93CEA63F3C9FF97EC4A2D3F533F3
          716BB384C4E29F7675AE41FEDC29E8E98FB5F28470B07D5BAE67A9BB7DA33BF0
          C84DB147803CE74BE4031CB868C01B6C789A772FCDAFFFBEDB39EF274FA9DE97
          58B255B284256E7D968806F93936B04B8C2E4F28321ED97653F878B6C4479233
          BA650B07DC85035EE52FB9F2842288255D656BBDA2C17DDB4C58BBA9B557A966
          C48C43914986FBAF881D8F441F27C734B0181E6AB88BC35FDDFD4EC6CB1B8A20
          9664D52D8A8C99366ED73CFE9CC05FF3C5D41F9A492832BEF1EEBF22763C127D
          8C9C335C721491A34925DFCB1849A11CB76291F3E4737B12FF1EB1C4AB74F570
          D24C584BD6B297B92733A7FF5E462869E39196A90DDF574732D870DD83160DD6
          26129114CAE7DFBE3273A04F2CF355BECC3E69E6AA256D462C6E4164F4775242
          693CF0D5C4F1488BF6A59F90E1880CDE3DB7569DA4867249F8EBF25FFE955872
          A83C94E841244C1BB7242D7BC9158ABCD4FBD94589E391963CE760E4012F5F3C
          E835AA1FBBCF93158A2016BD5A8422BC6DDF721B4FFC343596E0B4AB679DF04B
          0A45B46F5914BDC4DB74B197341E99FE1C3997CD1F548369E0249A5004B1E8D4
          26149134E6683775EAA54120174C39F1EBBC5A5AA1641DAD5AF2CE70C9075C34
          18BEE07AE525973C9081C6CCEB2FF9DF2A8F34DA5004B164AB55285933612DAD
          652F59A168C623D1C71B3EC4696EB83BF7CA803C642C13B633FD5886C223914C
          2487FF75CB082A4F288258D2D52B1421B1DCF9FEC499B0167FE59B83F68BA8E6
          BD7D749593351E89DE2FE73470191684871F39FF3254E0CBB8BCA108624956BF
          509CEC69E322E559E858B6C5E14BB9E105C59C8BE92414412CF16A194AF4C094
          630B1379A681BB458E2E230B075CD3E9E64E4311C4325F6D43891E5CDB061545
          ABF3C610459CE537094510CB6CB50E45242DA034D17CCD9AC07FEF0DB5FE219B
          2EAE340D4510CB8CDA8722D2CE97E495771AB82A92C88A25431D8FEE8B084510
          CBCBBA1E8AFB8FC7677E3339E1BACF3D3DF3B65D4F47B35E2D69B35A459099B3
          E9DF84E104A3C74EFF361839CC71460E9B7E7BF4B62EC7657254292A14412C1D
          8622B352E1937CE6F73B7E33FDC374C7C71C67F7CE9977DEF58C9B7546BCD7C8
          2A016774D5CC136378E5CB61BD225821512D9D89ECF0933AFA3C266395224311
          B6C7921A4A344DBBED26D719FFA7DBBEA7153AE72F9523D5A1B2BA2073AB5993
          55C94587226C8E253594C69DEB3D0229871C959A9F7E30FD9A99F0D7C14B3B1B
          A794118AB03596F4506E7B57574EFAD92AEB5E93750C45D8184B6A28D1AE240F
          DFE87ABBFF55F5E3EC2BB2B6CC7FCB0599273BEBF6D2AB9D6DB1E41BCC8FEF94
          C1FACC0F2E1CC3B8AD41BDDCE661D7D3336FDB3DE6F47B60F2847786DB06F1A3
          C7CEBA1A73D6207EE1D220EF7AB23A0DE6E3D8144B5737C073F64ECCCC8EB54D
          13BB61806158334F88E79E76BAF5924F16453A2B66A685C3277E10B46F221EBE
          2D186A7BF277F1064575991E4E634B2CBD71C271E34732AF4CD48A06D11FBBB7
          274E38CAD5939DAEF9EA5628C286586A1F4AD24E2B26CABCB15151461634DC45
          839D6F7DD4CD5044BFC752EB50CA5C415CE65DB94CC90AE2030C7775E97628A2
          9F63A96D2832A619D8787EA917A4D771997D51BBBA54118AE8D758EA198AECE0
          B8F1A3999B41982AFAA646A6643AF88002AE4511558522FA3196FA85A2DC5648
          5601675E5B3FB83873F6AC0E837B19B8CB958D45EE595C6528A2DF62A95D288D
          4D17BA59AB86A3EB494EFF723078C769A92FCDF65FB0D56F6CFB56E6F52C9DDE
          76222F7910AD161AE16143369590F148195B1E551D8AE8A7586A158A66D794F6
          2B13D3766111D35B1629AE948CB66F5DF74DF50F4D9EEFB225916CAE32D0F644
          9779AAF65D55C2300A792995571D4211FD124B6D42D13C99E70EBEB32EE89AB5
          09DE93F7B9DEB61BDDB4976279A68DEB721F9424750945F4432CB50825E98640
          2D32D608C2975A7397A5A78512DD94F433BF9AB5E850B3BB4B9E9930D3938265
          AA5328A2D763A93C94ACED4EA30584EBBE193B33951A4AD246DD32A3B6E5F2D4
          4DF6A6D6DFACBE9951AF6ED25D855E8EA5DA506486EBAEF5C991645CDF9EB605
          6BEAAD1FC2CFEB3DF095E4BF9B73DAB86EF74611750C45F46A2CD5DE4828651A
          381AB4CB0D4B5366A2D296B7686E2694B6C38B5C89D83CF707EA99B025430D77
          694DEEB625EA1A8AE8C558AABB355DCA0C97F6D60BA9A128F7ED4A5B26134D1B
          6FF8A1FAFE8DA6EBB38A54E75044AFC552492869B778C835984E0925D77D4EE4
          36755B2E8B9D11CBBB80B22EB782A87B28A29762E9FE764549F79AEF60B3ECB4
          A3419E50A28F25930A32C88FB9D82CEFCD85EA306DDC0BA1885E89A5ABA124DD
          864E06EDFEE9D7E45E7395765BBBBCA144526E3BD16BB7AB4B0AE5EC372C77CE
          7EE368658F2BCEAD8F8E39DBFE369EF8F635470E07D79FF1DA4A63E9EA158E71
          D3C0D1A07BDD0D1D2D1F490DA5D35DEAA3FBCEC72F7BC97303D4AA67C23AB97D
          769D1D7FE812E796F547ABC78B45EB4E2809335CA61B65A78592E75C489CB8F1
          4FB48072C30F7C6744F7D2458E2A45DEF3248F7E0B459C7BFC68F0B9B7ADACE4
          C8D29550E22EE52DE4C2A930C0A48591A6A144DF9C9831509E059455DE56BB1F
          43597BF4B2E0BAD38FECCF50E64E03272D47E954D2C2C8224289BE4131CB5E64
          CFE2E659B7AA3EF68A25D52C71E9C750CE58754070F5DA57F75F2873173A9671
          1BB8A450B23697CB2566D98B76DAB8AA975FFD18CA274E3A38B8E0E443FB2B94
          B9E387B26EB790B49B65A1A10819676DB96CD6DA32CD84415567EC77BC38E9DC
          9332ED5A67F2D8E3A68CFB3294F6C1B066394AA79216466685222B7F83F05B9E
          F7BBDEBEEC45331951B7A52DBD20E968D897A1B4A6596533B9DCE73372880B25
          BA1DF6C77E9E1A8A0CB487C327F00B7BA77C3FE7A38BB69A7DF621C77FF3B999
          BB3FD669594BAFB02B942E890D45B120B2B525901F1E56FEBB67CA9FCA5B8B52
          9553C4BD8A504A10B70258134AFB4B228925FC36047B0BAEC564377A9B114A09
          624F0CE60CA565625F337829FC55D4632BF2BEF136219412C485D27CD387025F
          260F52240DB2F7EC6F06E393E6B1C8071EADE81C4AAF239412C486A258109936
          C8DEDFF4A3718BC94F84417CE708A504711B536842C91A644B2CE191A5A3413E
          9198219432BE8098859145842264903FBE772A085F89A9AF45192612638452C6
          1710138A669D579E69DB7D4D3F0807FAE151263E18F9E48BC3F18E0CDE199398
          239432BE80985DEF35A174B25831EC258AA635D46FED0A39D4286ECF60104A69
          E62E8CD4847208E7376A8B504A322F14C5958884525F845292B9A168560E134A
          7D114A491A77BD7FD6EE2959A15479E521B2114A49E62E8C2494DE462825690F
          25BA40ECDCBB09A587114A59C6773AEEF858F4C40F460E0BB27649A97A2B21A4
          23949AE0AAC37A23949AA8CBFEC08847283520E7D0572C616AB8CE08A506646C
          52E46DAA513C4231F09B7F4CB8271EBED4E89BC420BE37108A818B373FEBAD7B
          DD81C1BAD50775F48D2292DE4128062494ED632F45F7CBB8F4D495C192A186EE
          0B745E5E02CF2C57EF201403AD50C4D241CF597BCC3267CD6B4682130F1F96A3
          C5AC6F9E0C41C22C5C99D95AC435223D87500CB487D2EEEE73563B6F3DA2F81D
          28511D42314028F620140384620F42314028F620140384620F42314028F62014
          0384620F42314028F620140384620F42314028F620140384620F42314028F620
          140384620F42314028F620140384620F42314028F620140384620F42314028F6
          20140384620F42314028F620140384620F42314028F620140384620F42314028
          F620140384620F42314028F620140384620F42314028F620140384620F423140
          28F620140384620F42314028F620140384620F42314028F620140384620F4231
          4028F620140384620F42314028F620140384620F42314028F620140384620F42
          314028F620140384620F42314028F620140384620F42314028F620140384620F
          42314028F620140384620F42314028F620140384620F42314028F62014038462
          0F42314028F620140384620F42314028F620140384620F42314028F620140384
          620F42314028F620140384620F42314028F620140384620F42314028F6201403
          84620F42314028F620140384620F42314028F620140384620F42314028F62014
          0384620F42314028F620140384620F42314028F620140384620F42314028F620
          140384620F42314028F620140384620F42314028F620140384620F42314028F6
          20140384620F42314028F620140384620F42319014CAEB0F5EE48C2C18A8FAE1
          A140E39353CE1FFFBD67DE9F138A425228B007A128100A0845815040280A8402
          42512014100A5073840228100AA040288002A1000A840228100AA040288002A1
          000A840228100AA040288002A1000A840228100AA040288002A1000A84022810
          0AA040288002A1000A840228100AA040288002A1000A840228100AA040288002
          A1000A840228100AA040288002A1000A840228100AA040288002A1000A840228
          100AA040288002A1000A840228100AA040288002A1000A840228100AA0402880
          02A1000A840228100AA040288002A1000A840228100AA040288002A1000A8402
          28100AA040288002A1000A840228100AA040288002A1000A840228100AA04028
          8002A1000A840228100AA040288002A1000A840228100AA040288002A1000A84
          0228100AA040288002A1000A840228100AA040288002A1000A840228100AA040
          288002A1000A840228100AA040288002A1000A840228100AA040288002A1000A
          840228100AA0F07FFBB9CEC25571384E0000000049454E44AE426082}
        Stretch = True
      end
      object WelcomeImageDark: TImage
        Left = 33
        Top = 12
        Width = 202
        Height = 386
        Anchors = [akLeft, akTop, akBottom]
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000CA0000
          0182080300000096C7AC2F0000010E504C544531485354544CD47E34E0833260
          5749BD7737ED8730C87B368F6740A66F3C3C4C508263436C5C478E6641836443
          A66E3D9A6B3FB2733ABD76382D586D2C5D762C627D296E902B6683484F4E2389
          BC20A0E0238FC5B2723AC97A35219AD72583B0775F45E18231315F76267EAA2D
          80AA325F759B6B3E32485238718D3777973281A9484C4E4E4E4E436373444C4F
          404A4F3C4A512990C43849513181A93649524B4C4D494C4D3E4B513F4A4F464C
          4F4A4D4ED57F33434B4F3A4A514C4D4E454C4F3448525F584A6B5B48765F4538
          49523B49503F4A503A4A524362722D89B62584B4248BBE28779F2581AF257EAA
          2773994858603282AA2A6887314E5B229AD62D7FA833647E374F5B414B502C8B
          BB74DF6C73000000B66558496649492A000800000005001A010500010000004A
          0000001B01050001000000520000002801030001000000020000003101020010
          0000005A00000069870400010000006A00000000000000600000000100000060
          000000010000005061696E742E4E455420352E312E3900030000900700040000
          003032333001A00300010000000100000005A004000100000094000000000000
          000200010002000400000052393800020007000400000030313030000000004C
          CFC048258FD7A0000008C54944415478DAEDDBDB6F1B451406F099D84EEC8D69
          D2A66D52D2B401155C09444B48282D04C29FCB03485C1F7801898B5452717903
          A4AA017A21B4497A49D2DC1C337B99DDD9F5EECE783DB64FA2EF93A071EC64FD
          F3EE9C3D33BBE1ECD8840FFA0D8002CA11092814030AC5804231A0500C281403
          0AC5804231A0500C2814030AC5804231A0500C2814030AC5804231A0500C2814
          030AC5804231A0500C2814030AC5804231A0500C2814030AC5804231A0500C28
          14030AC5804231A0500C2814030AC5804231A0500C2814030AC5804231A0500C
          2814030AC5804231A0500C2814030AC58042319D53B8C8C1A0DFB61D4A8533BE
          3BE8B76D873254666C6FD06FDB0E659881024AEF28BC726C28E5215088520E29
          9E233BA6540F4101A587945AF3D8502AFC88511CCE370B50EABCB94D8D726287
          D59E6453AA4F539F1BDF66A5E7D4287571121CDD4DFB844B25F1BFA19D94679C
          EAA6F2CC84E6D01513B8D5BE50BCD1CD4FFEDBFE84DB4DA6523817139968A79C
          D38F427EAF2F1476FE3FC69A872D634AFDB9D85D936BE18EBC6050192A7FF585
          E24C88FDDF9C5931A4BCE0CE91A75743C9CB3BCC20B5DBFDA0B0A99ABBFF479E
          C5BFEBB5606D146764CB95AC443BF1925925B369C939A2F9AC6B4914B2540A1F
          F60CC351FD7E65CB70FB162D798373F6BEF78A92FA01FB94D93FD5D779C32456
          A11B5C57BF1EDBB7E46ED2EBB718DF53067FDD9BAAB4F695578DFA0F94334AA3
          AC2B5F7C2DFCD29A257F9B5EC3C59ACA129EAF53284EC527A8BA370EB59B8D28
          D62C9A8F6FC63F8DBDB8924599DAF077995286D995A67EB30AC5964543F14AB2
          FBAF3CB6139472CB1B26B1E275D5E08CE25326576D5AB407F5AC7F4A965D8C7B
          E68C28A392B41F49E64C26FE3E65CC7960D1A26D2FFC92151632AF9B94943139
          D2A30390CD73938632A094462C5AF49D925F926517A3509CA6DC15CA69A63166
          746E941466D16230F57A7525B0D43625C55D3DE243A5E0056A63FF9AD95C2EA4
          58B4986CD91F1F22952DBF0573297267C58B57639CB3E78EF8B54FF37F6544B1
          6731A1C832E6763192120E13B6F04BC621352F8ADD70F93015A550AC598C8E07
          6752CE2CEA419BC5C38AA596E1D42C1E3C6BFFA64AB165313CB467336749C39B
          DA9FBED6DE5BC628962C862B2ED1C8484429C3D959E4EBB9143B16D3C5A313E9
          5329C35589B6062041B162315E07ABA5355667EF1AFEF4076BF1C7498A0D8BF9
          925EA5FDA56A19CECFD2230DC5822597F2A1787E59F42EE2455983BB2E4EFDE2
          E8795B14DE9B79AEC46E69A7746F8953F81263B744232236F1A454E0B735C7DC
          06479C6B4481FE26F64C62B4A450BAB6480A3FF9B85CF0D3C8CEC1F89EDC9B89
          7E398DD2AD455286EC43DC3465B16854F4942E2D92F292DD95C23072671851BA
          B3484AFDCA720F200BF75682AF4C0EB02E2DEAB09F1215D3D96317EE888A5414
          B6F093D8C37F8B61220E2EB5393318F6DD5AF296F4C486C40872975BCE671F7E
          D377DD19BEF86FC77BFF59D117E3AE2DE6A7C891F60EB839627C8ADC8A7738D9
          94E216634A356D6D2BE372527BDE7B1C7F9C43296C31A564B4C6D5A7463FBDB8
          91DC6C0EA5A8C590C22B194F1835F9EDAB30B994821633CAD4C3AC3626ED6A52
          3273ED53817C4A318BD98438ACAB723D914DCB9AA61DFA4BEB29A34C43296431
          A29C929DD4D975596FCB2DD992C42F5AA869CC30CEEFA56E4147296231A184AB
          2BE24009565C444312CE2B479531BD542E891DB82D9807B9F34B2DA580C58012
          BE677788CB0998E84366E5155EA524EBAFAC98523AB7E8B75C0FBAA7E987EE61
          1650A6EF30651D4629634B87EBDADF6846E9D8A2A5C8F5D4851FBDA11F50FCF5
          EFA94A5005941B45E61D138B09A5538B8EE2ECFA9B93D3F860D13558CA774EF9
          0FD5926C7279C58CD2A14577A92828C3251E94A9A07F09AF15050B3107D5A88C
          BDA35FE433A47466D15082E2155D750828D1492FE868D4DBF4930D57714A4796
          7C4A703D4219D60145B9A2529EF106BFDA59EAAF4C98523AB1E46ED4FFC4630B
          DCC14E50EF41E0379613DF32BDD86D40E9C092B74DFFDADDE4DD56DBF7E2B753
          04172DD4AB78BA32E653C62FE8298CFFE6FF7BF5ABE214BF0CB72AB1BE24A0C4
          D7EF1DBF8BD9372F637C8D759C37BF2C4AF18B57B2A90D28C93B0EBD21D43C13
          DD3F76FD19CB4B110A9BFBA220C59B00B74DAD9C83548A3F8694CE5273C9BB10
          E5ADCF8B51DC32ECF72AF10CA75398D7C5D4A321F27EEE68294499FFAC10C5ED
          2153AFCDF99494CFDCEB62A2929C7F8415A22C7C5A84E20E89F44B0EFE3266DA
          E1E3EC73A51EE49FF32FFDD111E2F2CDE214318CC35E251EBF9F9494A507EA7B
          125D4C58264CDA17E3F83BB118C569B5326E7EF4280B3F048FE676CF7CAB3C59
          6FEEC863D2A07BE90F253B1E25EC2617376AAD5BA9AFD354630A14AF150E29E2
          30AAEDFF9AF2B246A583DF39208A77325429A263F9BAFD65D74CEF031D3425BC
          0DD41FDCE3DF275FD53861F5DEFC1E52C26E3218DCAD834471B53AE87B45F156
          2D428A1CDCAD5A6CF05B96F488E2F5936D1456AB7E17BEA471C6B2A49794B005
          534AEE0BBB3FFB908B1BD6FF86A537146F4D3FA42CA8EFDA1976D876D3E6F9A4
          B714AF9F0C29AFF7E07DF79712CE178F3E256C8C8F36E5DDE58862747B345D8A
          DB4F1E1FCAE43FC78332F5889D962B2B464BDD74296AAC4E15074B31BC6DFD08
          509277441E618AFE0FA188524E27F741BF06BD7DCAB9EB9F0C48629FC26B573F
          0E1F352EDE37FF517A14768373FE91F8727EE2E4ED7EFE617D0F282213FADB73
          4001051450400105145040010514504001051450400105145040010514504001
          0514504001051450400105145040010514504001051450400105145040010514
          504001051450400105145040010514504001E5A853262F0F80F2FB6A2F28030C
          28A080D26F0AF5804231A0500C2814030AC5804231A0500C2814030AC5804231
          A0500C2814030AC5804231A0500C2814030AC5804231A0500C2814030AC58042
          31A0500C2814030AC5804231A0500C2814030AC5804231A0500C2814030AC580
          4231A0500C2814030AC5804231A0500C2814030AC5804231A0500C2814030AC5
          804231A0500C2814030AC5804231FF0304B1B2B0D4EF29500000000049454E44
          AE426082}
        Stretch = True
        Visible = False
      end
      object WelcomeLabel1: TNewStaticText
        Left = 214
        Top = 16
        Width = 362
        Height = 53
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'Welcome to the [name]'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        ShowAccelChar = False
        TabOrder = 0
        WordWrap = True
      end
      object WelcomeLabel2: TNewStaticText
        Left = 214
        Top = 76
        Width = 362
        Height = 57
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'This wizard will guide you through the process of creating a new' +
          ' Inno Setup script file. The results will be used to generate a ' +
          'new script file which can be compiled directly or saved on disk ' +
          'for later use.'
        ShowAccelChar = False
        TabOrder = 1
        WordWrap = True
      end
      object WelcomeLabel3: TNewStaticText
        Left = 214
        Top = 140
        Width = 362
        Height = 77
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'Not all features of Inno Setup are covered by this wizard. See t' +
          'he documentation for details on creating Inno Setup script files' +
          '.'#13#13'Click Next to continue, or Cancel to exit this wizard.'
        TabOrder = 2
        WordWrap = True
      end
      object EmptyCheck: TCheckBox
        Left = 214
        Top = 360
        Width = 288
        Height = 17
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Create a new &empty script file'
        TabOrder = 3
      end
    end
    object MainPage: TNewNotebookPage
      DesignSize = (
        595
        386)
      object Bevel1: TBevel
        Left = 0
        Top = 58
        Width = 499
        Height = 1
        Shape = bsTopLine
        Visible = False
      end
      object InnerNotebook: TNewNotebook
        Left = 4
        Top = 64
        Width = 584
        Height = 317
        ActivePage = AppFilesPage
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
        object AppInfoPage: TNewNotebookPage
          DesignSize = (
            584
            317)
          object AppNameLabel: TNewStaticText
            Left = 36
            Top = 8
            Width = 512
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'Application na&me:'
            FocusControl = AppNameEdit
            TabOrder = 0
            WordWrap = True
          end
          object AppVersionLabel: TNewStaticText
            Left = 36
            Top = 56
            Width = 512
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'Application &version:'
            FocusControl = AppVersionEdit
            TabOrder = 2
            WordWrap = True
          end
          object AppPublisherLabel: TNewStaticText
            Left = 36
            Top = 104
            Width = 512
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'Application &publisher:'
            FocusControl = AppPublisherEdit
            TabOrder = 4
            WordWrap = True
          end
          object AppURLLabel: TNewStaticText
            Left = 36
            Top = 152
            Width = 512
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'Application &website:'
            FocusControl = AppURLEdit
            TabOrder = 6
            WordWrap = True
          end
          object AppNameEdit: TEdit
            Left = 36
            Top = 28
            Width = 408
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
          end
          object AppVersionEdit: TEdit
            Left = 36
            Top = 76
            Width = 408
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 3
          end
          object AppPublisherEdit: TEdit
            Left = 36
            Top = 124
            Width = 408
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 5
          end
          object AppURLEdit: TEdit
            Left = 36
            Top = 172
            Width = 408
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 7
          end
        end
        object AppDirPage: TNewNotebookPage
          DesignSize = (
            584
            317)
          object AppDirNameLabel: TNewStaticText
            Left = 36
            Top = 84
            Width = 512
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'Application folder na&me:'
            FocusControl = AppDirNameEdit
            TabOrder = 3
            WordWrap = True
          end
          object AppRootDirLabel: TNewStaticText
            Left = 36
            Top = 8
            Width = 512
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'Application &destination base folder:'
            FocusControl = AppRootDirComboBox
            TabOrder = 0
            WordWrap = True
          end
          object OtherLabel: TNewStaticText
            Left = 36
            Top = 156
            Width = 512
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'Other:'
            FocusControl = AppDirNameEdit
            TabOrder = 6
            WordWrap = True
          end
          object AppRootDirComboBox: TComboBox
            Left = 36
            Top = 28
            Width = 408
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            DropDownCount = 12
            TabOrder = 1
            OnChange = AppRootDirComboBoxChange
          end
          object AppRootDirEdit: TEdit
            Left = 36
            Top = 56
            Width = 408
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
          end
          object AppDirNameEdit: TEdit
            Left = 36
            Top = 104
            Width = 408
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 4
          end
          object NotDisableDirPageCheck: TCheckBox
            Left = 36
            Top = 132
            Width = 524
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Allow user to &change the application folder'
            TabOrder = 5
          end
          object NotCreateAppDirCheck: TCheckBox
            Left = 36
            Top = 176
            Width = 356
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'The application doe&sn'#39't need a folder'
            TabOrder = 7
            OnClick = NotCreateAppDirCheckClick
          end
        end
        object AppFilesPage: TNewNotebookPage
          DesignSize = (
            584
            317)
          object AppFilesLabel: TNewStaticText
            Left = 36
            Top = 100
            Width = 512
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'Other application &files:'
            FocusControl = AppFilesListBox
            TabOrder = 5
            WordWrap = True
          end
          object AppExeLabel: TNewStaticText
            Left = 36
            Top = 8
            Width = 512
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'Application &main executable file:'
            FocusControl = AppExeEdit
            TabOrder = 0
            WordWrap = True
          end
          object AppFilesListBox: TDropListBox
            Left = 36
            Top = 120
            Width = 408
            Height = 189
            Anchors = [akLeft, akTop, akRight, akBottom]
            ItemHeight = 13
            TabOrder = 6
          end
          object AppFilesAddButton: TButton
            Left = 459
            Top = 119
            Width = 89
            Height = 23
            Anchors = [akTop, akRight]
            Caption = '&Add file(s)...'
            TabOrder = 7
          end
          object AppFilesEditButton: TButton
            Left = 459
            Top = 203
            Width = 89
            Height = 23
            Anchors = [akTop, akRight]
            Caption = '&Parameters...'
            TabOrder = 10
          end
          object AppFilesRemoveButton: TButton
            Left = 459
            Top = 231
            Width = 89
            Height = 23
            Anchors = [akTop, akRight]
            Caption = 'Remo&ve'
            TabOrder = 11
          end
          object AppExeEdit: TEdit
            Left = 36
            Top = 28
            Width = 408
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
          end
          object AppExeRunCheck: TCheckBox
            Left = 36
            Top = 56
            Width = 512
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Allow &user to start the application after Setup has finished'
            TabOrder = 3
          end
          object AppExeButton: TButton
            Left = 459
            Top = 27
            Width = 89
            Height = 23
            Anchors = [akTop, akRight]
            Caption = 'B&rowse...'
            TabOrder = 2
            OnClick = AppExeButtonClick
          end
          object AppFilesAddDirButton: TButton
            Left = 459
            Top = 147
            Width = 89
            Height = 23
            Anchors = [akTop, akRight]
            Caption = 'Add fol&der...'
            TabOrder = 8
          end
          object NoAppExeCheck: TCheckBox
            Left = 36
            Top = 76
            Width = 512
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'The application doe&sn'#39't have a main executable file'
            TabOrder = 4
            OnClick = NoAppExeCheckClick
          end
          object AppFilesAddDownloadButton: TButton
            Left = 459
            Top = 175
            Width = 89
            Height = 23
            Anchors = [akTop, akRight]
            Caption = 'Dow&nload...'
            TabOrder = 9
          end
        end
        object AppAssocPage: TNewNotebookPage
          DesignSize = (
            584
            317)
          object AppAssocNameEdit: TEdit
            Left = 36
            Top = 49
            Width = 408
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
          end
          object AppAssocNameLabel: TNewStaticText
            Left = 36
            Top = 28
            Width = 512
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'Application file type &name:'
            FocusControl = AppAssocNameEdit
            TabOrder = 1
            WordWrap = True
          end
          object CreateAssocCheck: TCheckBox
            Left = 36
            Top = 8
            Width = 524
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Associate a file type to the main executable'
            TabOrder = 0
            OnClick = CreateAssocCheckClick
          end
          object AppAssocExtLabel: TNewStaticText
            Left = 36
            Top = 76
            Width = 512
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'Application file type &extension:'
            FocusControl = AppAssocExtEdit
            TabOrder = 3
            WordWrap = True
          end
          object AppAssocExtEdit: TEdit
            Left = 36
            Top = 96
            Width = 408
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 4
          end
        end
        object AppIconsPage: TNewNotebookPage
          DesignSize = (
            584
            317)
          object AppGroupNameLabel: TNewStaticText
            Left = 36
            Top = 28
            Width = 512
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'Application &Start Menu folder name:'
            FocusControl = AppGroupNameEdit
            TabOrder = 1
            WordWrap = True
          end
          object AppExeIconsLabel: TNewStaticText
            Left = 36
            Top = 160
            Width = 512
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'Other shortcuts to the main executable:'
            FocusControl = AppDirNameEdit
            TabOrder = 7
            WordWrap = True
          end
          object AppGroupNameEdit: TEdit
            Left = 36
            Top = 48
            Width = 408
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
          end
          object NotDisableProgramGroupPageCheck: TCheckBox
            Left = 36
            Top = 76
            Width = 524
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Allow user to &change the Start Menu folder name'
            TabOrder = 3
            OnClick = NotDisableProgramGroupPageCheckClick
          end
          object AllowNoIconsCheck: TCheckBox
            Left = 36
            Top = 96
            Width = 524
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Allow user to &disable Start Menu folder creation'
            TabOrder = 4
          end
          object DesktopIconCheck: TCheckBox
            Left = 36
            Top = 180
            Width = 356
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Allow user to create a deskto&p shortcut'
            TabOrder = 8
            OnClick = NotCreateAppDirCheckClick
          end
          object CreateUninstallIconCheck: TCheckBox
            Left = 36
            Top = 136
            Width = 400
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Create an &Uninstall shortcut in the Start Menu folder'
            TabOrder = 6
          end
          object CreateURLIconCheck: TCheckBox
            Left = 36
            Top = 116
            Width = 400
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Create an &Internet shortcut in the Start Menu folder'
            TabOrder = 5
          end
          object UseAutoProgramsCheck: TCheckBox
            Left = 36
            Top = 8
            Width = 524
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 
              '&Create a shortcut to the main executable in the Start Menu Prog' +
              'rams folder'
            TabOrder = 0
            OnClick = UseAutoProgramsCheckClick
          end
        end
        object AppDocsPage: TNewNotebookPage
          DesignSize = (
            584
            317)
          object AppLicenseFileLabel: TNewStaticText
            Left = 36
            Top = 8
            Width = 512
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '&License file:'
            FocusControl = AppLicenseFileEdit
            TabOrder = 0
            WordWrap = True
          end
          object AppInfoBeforeFileLabel: TNewStaticText
            Left = 36
            Top = 56
            Width = 512
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '&Information file shown before installation:'
            FocusControl = AppInfoBeforeFileEdit
            TabOrder = 3
            WordWrap = True
          end
          object AppInfoAfterFileLabel: TNewStaticText
            Left = 36
            Top = 104
            Width = 512
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'Information file shown &after installation:'
            FocusControl = AppInfoAfterFileEdit
            TabOrder = 6
            WordWrap = True
          end
          object AppLicenseFileEdit: TEdit
            Left = 36
            Top = 28
            Width = 408
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
          end
          object AppLicenseFileButton: TButton
            Left = 459
            Top = 27
            Width = 89
            Height = 23
            Anchors = [akTop, akRight]
            Caption = 'B&rowse...'
            TabOrder = 2
            OnClick = FileButtonClick
          end
          object AppInfoBeforeFileEdit: TEdit
            Left = 36
            Top = 76
            Width = 408
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 4
          end
          object AppInfoBeforeFileButton: TButton
            Left = 459
            Top = 75
            Width = 89
            Height = 23
            Anchors = [akTop, akRight]
            Caption = 'Br&owse...'
            TabOrder = 5
            OnClick = FileButtonClick
          end
          object AppInfoAfterFileEdit: TEdit
            Left = 36
            Top = 124
            Width = 408
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 7
          end
          object AppInfoAfterFileButton: TButton
            Left = 459
            Top = 123
            Width = 89
            Height = 23
            Anchors = [akTop, akRight]
            Caption = 'Bro&wse...'
            TabOrder = 8
            OnClick = FileButtonClick
          end
        end
        object PrivilegesRequiredPage: TNewNotebookPage
          DesignSize = (
            584
            317)
          object PrivilegesRequiredLabel: TNewStaticText
            Left = 36
            Top = 8
            Width = 512
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'Install mode:'
            FocusControl = AppLicenseFileEdit
            TabOrder = 0
            WordWrap = True
          end
          object PrivilegesRequiredAdminRadioButton: TRadioButton
            Left = 36
            Top = 28
            Width = 512
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Administrative install mode (install for all users)'
            TabOrder = 1
          end
          object PrivilegesRequiredLowestRadioButton: TRadioButton
            Left = 36
            Top = 48
            Width = 512
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Non administrative install mode (install for current user only)'
            TabOrder = 2
          end
          object PrivilegesRequiredOverridesAllowedCommandLineCheckbox: TCheckBox
            Left = 36
            Top = 68
            Width = 512
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Allow user to &override the install mode via the command line'
            TabOrder = 3
          end
          object PrivilegesRequiredOverridesAllowedDialogCheckbox: TCheckBox
            Left = 36
            Top = 88
            Width = 512
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Ask the user to &choose the install mode at startup'
            TabOrder = 4
            OnClick = PrivilegesRequiredOverridesAllowedDialogCheckboxClick
          end
        end
        object AppRegistryPage: TNewNotebookPage
          DesignSize = (
            584
            317)
          object AppRegistryMinVerDocBitBtn: TBitmapButton
            Left = 457
            Top = 159
            Width = 20
            Height = 20
            Anchors = [akTop, akRight]
            Caption = 'Help'
            TabOrder = 9
          end
          object AppRegistryFileLabel: TNewStaticText
            Left = 36
            Top = 8
            Width = 512
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '&Windows registry file (.reg) to import:'
            FocusControl = AppRegistryFileEdit
            TabOrder = 0
          end
          object AppRegistryFileEdit: TEdit
            Left = 36
            Top = 28
            Width = 408
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
          end
          object AppRegistryFileButton: TButton
            Left = 459
            Top = 27
            Width = 89
            Height = 23
            Anchors = [akTop, akRight]
            Caption = 'B&rowse...'
            TabOrder = 2
          end
          object AppRegistrySettingsLabel: TNewStaticText
            Left = 36
            Top = 60
            Width = 156
            Height = 14
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Settings (for all keys and values):'
            TabOrder = 3
          end
          object AppRegistryUninsDeleteKeyCheck: TCheckBox
            Left = 44
            Top = 100
            Width = 225
            Height = 17
            Caption = 'Also delete keys which are not empty'
            TabOrder = 5
          end
          object AppRegistryUninsDeleteKeyIfEmptyCheck: TCheckBox
            Left = 36
            Top = 80
            Width = 225
            Height = 17
            Caption = 'Delete keys which are empty on uninstall'
            Checked = True
            State = cbChecked
            TabOrder = 4
          end
          object AppRegistryUninsDeleteValueCheck: TCheckBox
            Left = 36
            Top = 130
            Width = 225
            Height = 17
            Caption = 'Delete values on uninstall'
            Checked = True
            State = cbChecked
            TabOrder = 6
          end
          object AppRegistryMinVerCheck: TCheckBox
            Left = 36
            Top = 160
            Width = 245
            Height = 17
            Caption = 'Create only if Windows'#39' version is at least:'
            TabOrder = 7
          end
          object AppRegistryMinVerEdit: TEdit
            Left = 287
            Top = 159
            Width = 158
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 8
          end
        end
        object LanguagesPage: TNewNotebookPage
          DesignSize = (
            584
            317)
          object LanguagesLabel: TNewStaticText
            Left = 36
            Top = 8
            Width = 512
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '&Languages:'
            FocusControl = LanguagesList
            TabOrder = 0
            WordWrap = True
          end
          object LanguagesList: TNewCheckListBox
            Left = 36
            Top = 28
            Width = 408
            Height = 281
            Anchors = [akLeft, akTop, akRight, akBottom]
            Offset = 2
            TabOrder = 1
          end
          object AllLanguagesButton: TButton
            Left = 459
            Top = 27
            Width = 89
            Height = 23
            Anchors = [akTop, akRight]
            Caption = '&Select all'
            TabOrder = 2
            OnClick = AllLanguagesButtonClick
          end
          object NoLanguagesButton: TButton
            Left = 459
            Top = 55
            Width = 89
            Height = 23
            Anchors = [akTop, akRight]
            Caption = '&Deselect all'
            TabOrder = 3
            OnClick = NoLanguagesButtonClick
          end
        end
        object CompilerPage: TNewNotebookPage
          DesignSize = (
            584
            317)
          object OutputDirLabel: TNewStaticText
            Left = 36
            Top = 8
            Width = 512
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'Custom compiler output fol&der:'
            FocusControl = OutputDirEdit
            TabOrder = 0
            WordWrap = True
          end
          object OutputDirEdit: TEdit
            Left = 36
            Top = 28
            Width = 408
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
          end
          object OutputBaseFileNameLabel: TNewStaticText
            Left = 36
            Top = 56
            Width = 512
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'Compiler output base &file name:'
            FocusControl = OutputBaseFileNameEdit
            TabOrder = 3
            WordWrap = True
          end
          object OutputBaseFileNameEdit: TEdit
            Left = 36
            Top = 76
            Width = 408
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 4
          end
          object SetupIconFileLabel: TNewStaticText
            Left = 36
            Top = 104
            Width = 512
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'Custom Setup &icon file:'
            FocusControl = SetupIconFileEdit
            TabOrder = 5
            WordWrap = True
          end
          object SetupIconFileEdit: TEdit
            Left = 36
            Top = 124
            Width = 408
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 6
          end
          object PasswordLabel: TNewStaticText
            Left = 36
            Top = 152
            Width = 512
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'Setup &password:'
            FocusControl = PasswordEdit
            TabOrder = 8
            WordWrap = True
          end
          object PasswordEdit: TEdit
            Left = 36
            Top = 172
            Width = 408
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 9
            OnChange = PasswordEditChange
          end
          object SetupIconFileButton: TButton
            Left = 459
            Top = 123
            Width = 89
            Height = 23
            Anchors = [akTop, akRight]
            Caption = 'Bro&wse...'
            TabOrder = 7
            OnClick = FileButtonClick
          end
          object EncryptionCheck: TCheckBox
            Left = 36
            Top = 200
            Width = 524
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Use the password to &encrypt the application files'
            TabOrder = 10
            OnClick = NotDisableProgramGroupPageCheckClick
          end
          object OutputDirButton: TButton
            Left = 459
            Top = 27
            Width = 89
            Height = 23
            Anchors = [akTop, akRight]
            Caption = 'B&rowse...'
            TabOrder = 2
            OnClick = OutputDirButtonClick
          end
        end
        object ISPPPage: TNewNotebookPage
          DesignSize = (
            584
            317)
          object ISPPLabel: TLabel
            Left = 36
            Top = 8
            Width = 524
            Height = 81
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '*'
            WordWrap = True
          end
          object ISPPCheck: TCheckBox
            Left = 36
            Top = 90
            Width = 524
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '*'
            TabOrder = 0
            OnClick = NotDisableProgramGroupPageCheckClick
          end
        end
      end
      object PnlMain: TPanel
        Left = 0
        Top = 0
        Width = 596
        Height = 58
        Anchors = [akLeft, akTop, akRight]
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 1
        DesignSize = (
          596
          58)
        object InnerImage: TImage
          Left = 537
          Top = 1
          Width = 55
          Height = 55
          Anchors = [akTop, akRight]
          Picture.Data = {
            0954506E67496D61676589504E470D0A1A0A0000000D49484452000000370000
            00370806000000A8DBD246000000017352474200AECE1CE90000000467414D41
            0000B18F0BFC6105000000097048597300000EC300000EC301C76FA864000000
            1874455874536F667477617265005061696E742E4E455420352E312E381B69EA
            A8000000B66558496649492A000800000005001A010500010000004A0000001B
            010500010000005200000028010300010000000200000031010200100000005A
            00000069870400010000006A0000000000000060000000010000006000000001
            0000005061696E742E4E455420352E312E380003000090070004000000303233
            3001A00300010000000100000005A00400010000009400000000000000020001
            000200040000005239380002000700040000003031303000000000AB80211353
            9E3828000007654944415478DAED99094C944714C7DF7C7BB00B0BCB7DA3EB81
            8A58AC82E27D1595D6466C4B15AD68C0461BC4AB8A45AD152B5AEF8A89A66A6B
            D17AB4464DEB5153E315B546D378342516D36A520E0FF00695E5F8A6FF597775
            31C821A0EE8649DECE9BF9E69BF77EF3DECECC6619D97161AFDA8126B8970517
            DFC59D560E0BECC4189BC9391F69989F450F4AE557CDD1307033FA7B53EA40BF
            21503F05DC5B3609D7CA436DAA4BCB39E5DE2B7BD23F3FDA8F927A7B7D84C825
            01AEA7359C5AC128D05565D28B8D32151497BF7E7002ECF4B476425542CAD37E
            BB466B4F14124763C9D0001AD7CD7322D404C04558E0BC744AFA3EAE197535E8
            C4ACAAF3C5EE6523FFEA58BD756EAEE472BA77EC07921FDD7F69704A44E720EC
            4FE7C4CF6F3F7B8B527EB94A1B4735A7E810FDE7181607B80EBD32B249AD9468
            5B7C73F275513B3062AB38B1337BA4DE990B95630D183316F3106A12B5098673
            6B7337D05A579A97CDAF6F486E50C02AE182F42A3A9B120267A8331CDA076726
            A07BEF89CB4526906E06DD72B818031F83D79C2CA0C4484FD2AA241F8CDF8129
            73C94117FF67C7397CC195C085FFDD2D99550B3F56C0C6CCD2FC4B009C08C0A2
            C6831325B9B727CD8BF617036240B90329948A087E6D7A89B16F510D86434124
            A220B108446C37FA7248A18E721C30B5A42C3476A08B562D164685F10C3567E6
            D059F48C535729E3F77C6E7E2E166CA65100AE6F9808B2EA1E64BC1F4823C3DD
            45234522B644A40F9C980C5FB6E3711F78E98DBE78B4BF41FB3A9EF550B78DBA
            E11CB32884291D4EA2CFDD02521D5C42B80FCB3C5720BA57E0718A31EF6FBABE
            6152BD01AB3D0AC4EEB72BB18548430486ADC7E071F0EC2074105308641D641A
            A4088EF502689666E852D2850EEA80BEF6103701080980C442BCADE75F2DE020
            FFCE88A0F4A3B99479F686E85E06FCCF8C7997A8BE295A094E1CD0BECE4AD239
            28C845A3206FEC809E90CE818E26562CF601C800EBD54755068941D781729953
            897F2479B5082549EB4A4CE342CCD195146ECD48E1DEDC932954229A6D9F8DDC
            95942E4C467A03906F3A5720E65C0A0BA9F54DD14A70850BC3C4CD23C0BCDA6E
            56626937C7F30ECFC089A5FD03720772DB5C5BEBB749A939E7F44EDA1D4D6874
            02DA1BAB82137302902F3892CB369F37A5280029B53E295A09EEE6A2B09DE832
            5801B9D08BDD3F8DD6909CCB4BB53DC7EF75EC33311AED5F2D832C6979196969
            29E290F8F2700E0150341703723676D1174AD1CA915B14F6C482E96C222EA1E5
            4A9CB9A32980C7A17FC23391BB89463C1A7978E30E1791E2F4C8B47748B8ADA8
            34A4F46A458E513349E5D77E0D5E4D7A5EE42CFD224501C8375F28149D8B21B3
            4B0078A38EE760253827B5443EE6EF9C5E23E1D6A1A2FEAD9D282EDC438CEC8A
            5DE5088689FB5509C419720BBE78C2A71D90B8F20ACE1F45265350C7BEC4B47A
            622A2D83A84852BA61B5C663EC3CBCA3A809CE92A2698773D8D60B85A2FB2B3C
            9C63CCCFAE538A569B7206DC54F68F6F898D451D049BA7D1E5082323E0682AF4
            7E303A04FD8990582ECBE988F45CA741B348131E371C7D6922EA18A383EE64F6
            BBCAA3A02A38A18B044A3B94435B2E14886C5D28E62FADC32EFA5C3837AD447B
            C6B7A6763E1A710E9C109182B9A1A82FC2FE2ED4EFA1FD26EA2C4C924EE22710
            F144A56FE866E7D85592A4F3FA911E6FFFCF2D557DE79E2D82CA0C482640CEE7
            9A22B82E89B8F141DDE1D40AA2ADF106EA1BECAC909824403C30E907A80BC4F7
            412149DFD1E38BB3A142E63918848958023E56C299A1EA90E813BAE8394E92C6
            F938C674AAE9109FD233E0E9A5D3FA026A95A2484F76FB619988A62945AFAE1E
            4BA580AC335C4B71719EDA9698C47025223FC83859E625B3F65FA5416D5C28AA
            9D8BB8494C834DBDE6EDB42205C964BCB08BCAAF5DEC0F77D6CAC486E806A55E
            D1868F10D1DD551DDCEA53F9A6EDCB1CA71AF5C7D94A527EC6987AC12901178F
            BCC87C881F7509DB73E9C83F45B465B4810687E8E7C2D417B0A3D6276CE34ADF
            F6B17251E1EE078797C9A5D98782F11B2650D365F451A7A819BD30DD716AA062
            4963D8955E3872E266B2758CC194F0A515324DFF399FB20B8C8F0DE0BE392AC2
            631254B1F379BA7EB20F3790C02C18CCE2C6E24463D6FE871577F3481316434A
            EFE00CF44F7A5EE4EAAA5BD2B85E91ABAE885FE2137B7B8FC69B69D8E25BBB25
            1F24C9C93D0FF6FD61FF1C86A4432F843E1C7AB265976C4838A852DEAAF88687
            9B3BD097A6F4F779176A1AA9B411EE938FA870508BAD4C5FD7B9EA5A2C69D9F0
            916312A9FDDBD0C850252DEFABC01D93FA491E2D36B97EBCD385498A5B585C45
            75ABFE5A478EA91DC9907E8C7CA422C568CDD98A61018FC82B2C8A542DBA7F88
            C73F3576D41A3572163807A57460F81B9E6B9322FD8A7D9CD5E210171B8BBEA6
            55B789C8093F27F7F0A7C9DDFD9E18AF8D63360187224FE929E0FC6BFB6A8395
            464F4BBB8E9C5DC351535ADA68E4EC1A8E9AD2D2462367D770D49496361A39BB
            86A3A6B4B4D1C8D9351C35A5A58D46CEAEE1A8292D6D3472C2CFC820678A0CD4
            3DFD47C6EC4155FFD03CD16B7A5E0BFD4C6E119DCEB9DFB86959DB771AB134CE
            7F05B6549AE06CB5D835DCFF41CF90A123E252D40000000049454E44AE426082}
          Stretch = True
        end
        object InnerImageDark: TImage
          Left = 513
          Top = 3
          Width = 55
          Height = 55
          Anchors = [akTop, akRight]
          Picture.Data = {
            0954506E67496D61676589504E470D0A1A0A0000000D49484452000000370000
            00370806000000A8DBD246000000017352474200AECE1CE90000000467414D41
            0000B18F0BFC6105000000097048597300000EC300000EC301C76FA864000000
            1874455874536F667477617265005061696E742E4E455420352E312E381B69EA
            A8000000B66558496649492A000800000005001A010500010000004A0000001B
            010500010000005200000028010300010000000200000031010200100000005A
            00000069870400010000006A0000000000000060000000010000006000000001
            0000005061696E742E4E455420352E312E380003000090070004000000303233
            3001A00300010000000100000005A00400010000009400000000000000020001
            000200040000005239380002000700040000003031303000000000AB80211353
            9E3828000007714944415478DAED5909709345147EFBA7092D49539AA6698142
            6329D2324C2B82CA288728481505E59016880E650667003944A08895E2002A2D
            2ACE88E089CA218C1CA252611C7404471D0566B0220EA0E58A4DDA94A3E99134
            F9D76FD3A4A6D813522499EEE465DFDBDD7FF77DFBBD7FF765C228840BFBBF1D
            E80077A3C099EED0D1AB8F26F4678C2DE29C6719971751A553FEBF710406DCB3
            C30D9433B2EB68A8CF00DCFD4109AE578CCA533B5D9CCE5DAEAD6F5F9ED19566
            0E899D02E66602DC3DFEE0540A46095D941EDDEE90C96A77DD7CE004B01FE7A7
            08350CE2CADBF737AD3B584A1CC62B63BAD3F441FA5950A701DC401FB8584D18
            7D90D993EE346AC4ACCAA3D5B1B599A70635BD32E7FFAA6E375D29DC4072E5A5
            1B062E0CECEC870B0B38F1A35B0FDB68E167667A7F722265A4463D8F619900D7
            6FF0DA13A40A93688B2991E2B5AA4E8CD8EB9CD84FBB23466C5C193DDB88E79F
            C43CC08256C6BCB8B8FF7216D81B9C678AB8B5E08980026C145C8F28251D5E98
            0A67E87638F405167F0ACD9F1F3C5DE10132C8A829808B63E163EF370F5929FB
            2E3D4528A5388CDF8E29CF55BA98A9E0D260BE2FE9C9950EB57E492BFC588335
            1639CFFCC6ADF95349AEBADC7EE044993D444FCB32BA89016381723B6232070C
            BEE67988B177518D82433D3CE125B181606C27DACED6CA3462DB99E89AAF6DB1
            232597436C8C12E3196ACEBCD4F9744BEA83549292C1BDFD05F5000B4C016190
            35D7B1765C02650DD00963A144EC15C0D80007E6C097ADE81E0A2F0D6833C15E
            0FBB047D77FF62EB6C79E7B43ED5C5A54368D3F98034074E7FEA5B664BBE576C
            D31A742F7414FF4A8108D166AF0271FAEDC8BE45842188616F63F07478B61F3A
            10532A6403643EA402FE0E8673456B7F8FA1639735FDD0D617122D0042BA4326
            400CFEF35BFA3E4496940C4ADB3597CC69E3A8ACD730D19C8FB9168341B2E64F
            41885E090C387141C7478691A69382B4E10A32E004D4436E4FE8ECC18ACD2E84
            DCE7BFFBA86A2163D154E89239FD6051919D7726BB4BA24A4805C45AA3244BB5
            52EF2626D8EC733573E9BBE63271C40020078362CED5909CEB0DD106E04A57A6
            89CCA3BB77B7A3FDC46727A2BFDF55E02A60FE0CB90829F7D6FE7A79B58B8E6C
            FC33E6E2CFE59A69B0DF6F0C9C674E7CCC69E399ADD75011A2ABF18EE7388B8B
            AE39441B802B5B95F6299A8C7E80B4746DF9A7C31F24E7F2EA3DE7A33EDF7D3E
            3A03F65EDFA0FAB0DC39C7CF2389CCE908D1A4A1C27A19FBF01CAE09AA3B45DB
            16A20D995B9556A770CF89888A4BB0BA10673A9802F074B43F75157365304C30
            CEE3898B5C30C6A95A4CED86E190195DA852D296621D9DADEAF4261E9DD92473
            BE39A19BD3C773CF3BC8B91720DEC136866803706A954471DE772E2A5C42D6A1
            A4E1C96ACA1C102346DE8953E5008689FCAA061209B1C1173D16DF0EC974B939
            5F7F22924A65ADE77D73B81973C89212AF62341C9E814996E119454BE07C217A
            217D222B4F1A2C42F425742DF530D886106D36E48CC854BE9C91848345D5036B
            FE88A6CEF0601268CD818EB39B8F467B36640297E515603A7713183A50A27D1C
            6D7982758CD140577BFD6EF42A68149C27A591D885F409644B120731ADC457AE
            535C13F9A6565DF44D828B8E9068CF8C644A890B17F7C041C114D61B83FA38D6
            DF81FA31D8B7A12EC2242B48FC04229E5D6C577DF4C61F06E99253F109D51DFF
            4D9646DFB9FF7888B04E9F2800920720E7B922442DABB38857DBDB0E4EA520DA
            6C32D2B0DE910A894902480C261D8FDA2A63431592F41ED525CE46B7CCCF6210
            2662D3F0F52AFAC7FC54A63EF8F15F3A758DACF80E63FAB77489C79FF8AA3E14
            99D7A7863AE300C75C2A8D687E095F4B4BF21E2111A66D06972412E7797D4454
            2087A4AE90E9B2CC6B967C69A6076ED5D28814ADC824E6C3C7A8B74FEA2AC465
            3FCC60A75B348EE1F0795DAD9B8FDE5AACFBF3DB52AD60774773E0C05CC38CBA
            059DD765DDD2DFD7092E0CE04CD8B68D55F851376DEB393A70B282364D35D2A8
            D4A85C2CF502D651BDF86B577EA64A35A18BD2BD735262B93C20BAAAB742A284
            7DE6C86FB69DD58958FA8E02547C618C75A592E563AE0D9CC84C363F61F4C486
            D32DD382DD17E884D5E1E97B03F9E6E481314F435D863DD42F3EDA8DCA9CCA22
            AC58141126670F8AA9AC8A0DAFA543A59164AE56AD85234F37C55C5B755F185F
            1773CD15F14B7CD610C3543C99E7922979C19104A4588AF358BF1BD63F82212B
            A097427F1CFA6CDF291948705025F3B287030F2E77643CCD1D1EF730D4BC1A37
            1B38EF708212F7991576545BE76A6BF18565E099435AA44AEC4B59290A2A188A
            1C13F79CB92AECC3DC63DDB478DB6DD85C4573BB7E5333C7C2D5D4F3AD22D2BB
            6C8A472E7DE1EE63F99E8E59887EBB1C3E11EFDEB6F666AD5D99F381936457A1
            AEF88775863FF6DB953557C4252E52AAA896763D2898137EC6FD5E4886E37BEB
            176F8D6341010E458E17E02037BAB47B58863473210D8E3AC23248990B6970D4
            119641CA5C4883A38EB00C52E6421A1C758465903217D2E0A8232C8394B99006
            471D6119A4CC093FD5A527495D76AAFE5F188F034DFE4353A7B7D4DF1ADDAE4F
            2648FB86656B9F69C7D23EFF150453E90017AC25A4C1FD0312C399A1223ED74E
            0000000049454E44AE426082}
          Stretch = True
          Visible = False
        end
        object PageNameLabel: TNewStaticText
          Left = 24
          Top = 10
          Width = 504
          Height = 13
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = '*'
          TabOrder = 0
        end
        object PageDescriptionLabel: TNewStaticText
          Left = 40
          Top = 26
          Width = 488
          Height = 29
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = '*'
          ShowAccelChar = False
          TabOrder = 1
          WordWrap = True
        end
      end
    end
    object FinishedPage: TNewNotebookPage
      DesignSize = (
        595
        386)
      object FinishedImage: TImage
        Left = 0
        Top = 0
        Width = 202
        Height = 386
        Anchors = [akLeft, akTop, akBottom]
        Stretch = True
      end
      object FinishedLabel: TNewStaticText
        Left = 214
        Top = 16
        Width = 362
        Height = 121
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'You have successfully completed the [name].'#13#13'To close this wizar' +
          'd and generate the new script file, click Finish.'
        ShowAccelChar = False
        TabOrder = 0
        WordWrap = True
      end
    end
  end
  object BackButton: TButton
    Left = 347
    Top = 398
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = '&Back'
    TabOrder = 0
    OnClick = BackButtonClick
  end
  object NextButton: TButton
    Left = 422
    Top = 398
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = '*'
    Default = True
    TabOrder = 1
    OnClick = NextButtonClick
  end
  object CancelButton: TButton
    Left = 507
    Top = 398
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
