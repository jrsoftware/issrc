object WizardForm: TWizardForm
  Left = 228
  Top = 106
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = '*'
  ClientHeight = 358
  ClientWidth = 495
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    495
    358)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel: TBevel
    Left = 0
    Top = 314
    Width = 503
    Height = 1
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsTopLine
  end
  object RequiredLabel1: TNewStaticText
    Left = 8
    Top = 332
    Width = 21
    Height = 14
    Anchors = [akLeft, akBottom]
    Caption = 'bold'
    Enabled = False
    TabOrder = 4
  end
  object RequiredLabel2: TNewStaticText
    Left = 33
    Top = 332
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
    Width = 496
    Height = 314
    ActivePage = MainPage
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBtnFace
    ParentColor = False
    TabOrder = 3
    object WelcomePage: TNewNotebookPage
      Color = clWindow
      ParentColor = False
      DesignSize = (
        496
        314)
      object WelcomeImage: TImage
        Left = 0
        Top = 0
        Width = 164
        Height = 314
        Anchors = [akLeft, akTop, akBottom]
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000A40000
          013A080600000098AEF9C6000000017352474200AECE1CE90000000467414D41
          0000B18F0BFC6105000000097048597300000EC300000EC301C76FA864000013
          5B4944415478DAEDDD099C54D595C7F1736EEFAC0D8271810664D14162DC7064
          51B60644518C4693CC602699C499E0249A8C89445010449044C76532F8517134
          C6B84602B844655351A2514654D42838A2E2CA22A0C8FE4EEE7DAFEDEEEAAA6E
          BAFADDEA3A55FDFF7E62BAABBBAAFA55F58FFB96BAF59A3FFE7CD73E02508211
          2468822041150409AA2048500541822A0812544190A00A8204551024A8822041
          150409AA2048500541822A0812544190A00A8204551024A8822041150409AA20
          48500541822A0812544190A00A8204551024A8822041150409AA204850054182
          2A0812544190A00A8204551024A8822041150409AA2048500541822A08125441
          90A00A8204551024A8822041150409AA2048500541822A0812544190A00A8204
          551024A8822041150409AA2048500541822A0812544190A00A8204551024A882
          2041150409AA2048500541822A0812544190A00A8204551024A8822041150409
          AA2048500541822A0812544190A00A8204551024A8822041150409AA20485005
          41822A0812544190A00A8204551024A8822041150409AA2048500541822A0812
          544190A00A8204551024A8822041150409AA204850257783FC6203F1829F192A
          2C2239FDBA805A75C8F6128107B91BE4BA156C16FE9CDDA7C1D9370774E8D1D9
          5E22F0207783FCDBA36C9EB8220A72DC8D42DD4E946C2F12C497BB41BE7C3F9B
          A7AE89821C7B8DD0612723C83C90BB413E3F97CDF3B744418E9929D4BB1241E6
          819C0D92975FCFFCD2DD5190A3A6091D310641E6819C0D92164D67F3C6C35190
          2326091D792682CC03B91BE42313D9BCBD2C0A72E82F858E3A0741E6819C0D92
          E74D605EBF320A72F08542C78E479079207783BCE73CC31BDE0C3F0F064C10EA
          FF030499079A3FC81D9F11BDF32C538F4142654D7F75857F77A6E1AD1F869FCB
          09FF2A72E28F9B1CA46CDF44FCCED34C3D87C65A2688AFF9837CFD21368BAF64
          39B02FC929D3032AAF68DA82DF526978E7B6F07339F63C91C13F6D5A901BDE22
          7E62AAE14D6F535079B950DFD331D26651564648F71A347FFA0651AB8EEE1862
          40871E9BDE7D8890F9ED0043124417BF71AEC8905FA41792BBED9AA56C96CC60
          DAF325C981FF4032EEFA20D508397DC9BBFCD89B9FB1DF27C22EAEBDC722C374
          D1A02E7256BF4EF88740D9DA86FCEC3DE23F9E6FD8C62925ED48865C9CDE71C4
          5D5F90B979B8F9EAA2F4FBA6C8F04B1B7F7B1B343F7733D34B7731EFDD4D6223
          946FDD1A5087E4D17AF7BE807EF4C73566EDA61D7E9F03B68B2B51E345854453
          86779311BD3AB4F828B3B753B37E25F1C39718DEFDB95D0A43C13F9E6FF794CF
          132A2CDEFF6DB77D44E68E71D541EEEB3D4678CCB4C6FD32EDF6223D399BCDDB
          4F863548715B92B1B385BA1C9FF2F6AB3FDECE17CC5FCB7B83CCB6525268E897
          277791538FE8D8A2A3CCEE5EF6EB0FDBEDC9E9D130610A480E1F6D47BAC94205
          450DDF6EC31A32F7FC73CD08D97B94C89819FBFF456EB123F3A3930C6F5C43E1
          2AD36A68BB719F8D70C2FC3566F5C75F66FEB9B0CF424981A129232A6458CFF2
          161B65D60FFBB85527BF703B576F0F1E7A9CC8A8A9426D0FAAFF46767435F326
          5407191C364468EC6F1AFE25AE5DC6BCF46AE69D9F55FD60FB0FE0F87F111990
          7AEF5CEC6ABDA490B95D7181E76DC7489F6B57D6792228FC37627F265D32A4AB
          8C39BC658E94590F92F6EE225E349D79CDA2EA5FBC74E84172DAEC803A764F7D
          1BBBBA358F5C5273FD6E03C5EE90A4FE0506F6E1ADBA8FCD737398ECF662F597
          7B8F141A39C56E2294A4BC5991216E5F9299189DFA82744AEDEA7B4A65850C3D
          ACE58D94D90FD27151FEE93F0C7FF44ACDD74ACBEDEAF4B2D4D3CA6AAFEA2DE9
          6247D5B36E4ABEDEAECF8997CEAE8ABDE6DB72D0D749CE9A13D417A31D14EDC8
          683216A35337483732EEDA5BB38CA5F6F2C4A15D65749F963552EA08D2D9BA9E
          78FE8586EDC76A8565149CFC9F42479E61778B6BF5F1D2DD6C965F5F13E4C147
          919C333748BCBF0F889F9866235F95F06569DF85E4CC1B03B21F53B18313D991
          31B335527290334675A3E94BDEB37BF5B5A2B4C3F454BB4D39A4058D947A8274
          ECCE0ACFFBB1613BB2D52CA1898E330EFC49F51E383F770BF35FE7D60479E011
          24DFB9B36A23D4FEEEDE7F81CDE35398766C4EB8FBF010D3593705D4B977CA1F
          EF2A2CB73166BC464A0EF2AD8B8FA3856F6CA6498FAF4B88B2ACC88E94432A64
          549F967148485790CE5B8B99175F111E1FACB598247D468A0CFB95DDD36843FC
          D4B5CC2FDF571364C7C348C6DF1B05F9F2FDCC2B6E62DEB33DE16EC5AE9EA5D2
          6E33DAFB49F944D8FFDAD9188B0C358B54413AF7BDB291AE5C9A3C525E61B729
          4FEE91FF23A5BE209D97EE65F3CC751C8E76B58423E198AB037EDE8E907F7BB4
          661CB3ABDFE03BBF0BF8D9FF615E3D2F797C73C739075F2474CC771BFC85BA1B
          BA2D033742B2FDACE673F791A3EF85D7612EA8FA5E53D517A4B3C08E9493EB8C
          94A576A4BC7468858CEC9DDF23A5CE2025B03B23B3985F5B90F42B17F7DAB70D
          8C3F5B57F3C552BB2AEED44BF88355D5878F126ED3F70C11378997FD0D7F6EC1
          8A6CA5ADECFF1536E16E1B0AD21D727223E58C65EF27AEBEED0F9A36B29B0CEE
          D13E6FA3D419A4B36707F1639399DF7926D6165DD07D90907BCF4D5159C616D5
          45D9AA90D35ACE8682745C946EA4BCEC897713474ABBF73D797845DEBECCA837
          48C74DC478E0DF0C6F79B74937970EDD49BE7573D01C53CADA141B2E2D687C94
          FB0B325C7E1BE53D76A49C5967A474DB9457DA917250F7FC1B297507E96C7E87
          78C185863FFF24AD9B49DBAF918CBB2120BBC3D31C5C89E5A505A6B187D21B13
          64F8386C94F35FDF4C972F7A3769EF7BD2B0FC1B29B317E4EE2FC303D7B46B1B
          D14EFB71E7160E2FEFD84AE1840BF7B5F0E35637679179C796B4EE5ECADA1375
          3E5CA8D47E2C6E1B6D6716B7B1BF497BB9C45DB67BACA5F66349BBE87271ABD8
          0FA9CCAEB65B17356E57A7B141868FC54679F7CB76A47CF27DDA5367A49C31AA
          9B0CEC963F23A5BF20BFD840B4710D8733C2DD2B242E2417D5AEADE174311B1E
          871F776C0D23E4606FB61F7B023185519C2E58176E69BBF0101395D8CB365C29
          AD0AD9ADFE3BF5166AD339E93EDCE8D8C18E928DF979E904192E9F8D729E1D29
          A7D891B2769465C54C970DEB96371332FC0579CF786336BC95EDC7D32C82CE7D
          88BE7B5790EA7B9DCA3213A4E3A2FC831D2967D96DCA3DB5A6C3B5B2ABEF19A3
          7BC88915ED723E4A6F41F21FC61BDED43282944E7D48FEA9F9830C7FB68DF2C1
          D736D1D445EF25465935520ECDF1913283ABEC2DD1AA7A670EAEB2C35575D52A
          BB34BA2CA5E5595D65272CAB8DF2F7AB36D0EC27D7274459566CE8AA51DD737A
          A4D4BF53E302DEB8D6EED46C4EEBEEA5ACA30DA79744DB846DA3ED40F7310777
          6A523E3E1BE503AB37D1B4C575474A4397BB091939FA32A3FEC33E5BD647877D
          6ACF026A847056CFB81B032AEF92D6ED9A2A53877D1A7C8C36CA3B5FDA40BF7E
          2A71A46C5D5C40578DEE262774CDBD915277906E949C37C1988D4DDB360D773E
          BE7953108E8419968903E38DE1A2BCFF553B522E798FF6268C9405346544D79C
          9B90A137C81433C99B427A8F14696066B80F9978E930ADC768A3BCE3FF3EA5DF
          3CFD415294B34EE92EFDBBB4CD99287506E9DEA6FAF4755553CCEACCF86977A8
          8D7527F1979B12BFDEFA00BB57514ABCED83BA0F91E4E86F8B9CF4F3C449BE71
          9F38CADCE48AA63D65D1848CE94BDF4F88D2ADBEA75656C8E01C7999516790AF
          2D60B3F46A5B52E2A2C9013D494EFB75C0CB6633BFFFD784BAA4EB0922C3260A
          3F724978168AC4475940C1F05F091D392EADE967E1C714D3CFECE7E1F73235FD
          ACA95C94B7DB91F29A3A23A5DD9CA099A7F490E37360A4D417E43BCFB0796C32
          BBD93EB51693A4D73091E193C4BD04C88F5ECABC76496290BD46889C3A4B68E7
          36E2A533EDF797258EAE4565149C7295508FC1F54ED0756F5D68CA68D7149908
          327C1E6C94F786937CEB8C942505E1DB21B48F94BA82DCBC8EF8C17F0FCF6851
          B38405D199294EBAA87A3B300C6EF5FCC4208F3C339AF3E8B8EDCFE537D8EBFC
          2961940DCF50E1FE62433DEF66CCF65B187C7151FEEFCA4FE9DAE57546CA1263
          B7297BC87187EA1D29F504F9F927C40B2E32BCF9FF6BBE56584AC1A09F0A1D75
          76C2E45A7EF6B7CC2BEF4C0CF2B8EF890CFA49ADB7160644AF3CC8FCEC8DCC36
          D0EA2FBBB73BB859406DBF967231B2F5262F9F41868FB36A42C655CBDCDE77CD
          D75D945754EA9D90A123C87D7BDC09A898D7BF50F33E19F786AC119385ECAA3A
          E9FA2FDCC1E62F73129A09065C20D4FFFBC9D75DBBD46E8FCEE2F015A3AFEEBB
          4BFFE87DDCF59C21A3A480B96D33BF0DD67790E1E3B451CE7DF113BAEE990FEA
          4419EE7D0776A4CCE4436C92EC07696324BB93625E5F581363FB8AE844019D7A
          A6BE8D1DF9CC93B313831C3A311A4953D9F836F123130D6F7DAFE6FA7DCF10B2
          3B41F545E90EE3B46AE4AB2E4DD11C4186CFA59B90B16A433875ADEE4839ADB2
          BB0CE8A6EBE079F6837CF14EE6E76E620EA2C59083BF41326A5A40ED0FA9FF36
          6F3ECEE6F1CB13831C7DA5D0E1A3EB7F72B77E189D07F2A397A39FE3CE25E446
          D5E3CEABF7366E942C49E360773A9A2BC8F0B1DA286FAD1A29F7D51929678FE9
          111C73489B8CFDEC746537C8354BA23D6AB7BDE70ECDD83D651A79F9FE0F62AF
          FB0B9B8517250679C60D42DD0734FCAFDD6D4B2EBA928DDB43773B3BEEDD886E
          CFBBF7887A6F579EA13DEFA453A964899B797EE9B00AA95432F33C7B417EF42A
          F143179B7056903BACD3FF07E2FE6BD42B2AF6B6E6811F2664129C735B40077F
          7DFFB7757BE02FDC1E9DE08AC4EE7997939CFE5F011DD42FE5D5DDC1EFF625FE
          57DD5A8274ECA6092D3EFFA820FE3DC5979D20DDF91D1F38DFD0F64FEDCE4B1B
          0A5F4549E754CA9BD791B9EBDCC420C7DF5FFFC9A95279FD21E6E5D731BB2971
          AD0FB441DF1A50BB83535ED54D9A28F49CA4A620DDC953574C38A6650629EEC0
          F5C3BF30E6C355767D511EAD32BBF64F6F75B17D1399DBC62406F9C33F07E45E
          3E4C873BE58ADB64D8B18582438E26197B4DC0A5ED92AE96EEC489C6D0136474
          6AE9161B64F549EF3B1F4E327ABA1DD57AA47D17B2773715CC39C9D4BC12C3B4
          EF82E501A738FB6EADB3DCA5E6DED5F8F894F04F8C04EE542B7DC7265DDDCD71
          2C4B73F2847675FF41ACB8E0E8161A64D59F0591EE03855B756CF2DD1817E457
          07BCED766760834C7A70141DE4DEB67B5FD0D01999E5CBCDC4EB56D4FBA74A32
          3142661B82F4CCDC769ADD06DD105D68DDD9AEB21F497A42DDAE4847BBFDE762
          7451EE6DE2539EA93DED6C4290BE17FCAE6F1BB6AB5B47EC6A5FC6DF97F484D6
          7E8F8B1B20BFD81DC8AE7D92D6F6AA0BD10599EDC7EB1B82F4BDE0F7FFC8F0C7
          D11977E5A0A348CE9D9BF484A63A64B37D4F203BF6363E4A775A67777AE76C3F
          5EDF10A4EF055FF8330EB7FBC89D637C80C8B81B9222ABEFD4CC3BED28E946CB
          FDFD8C36B6E8D23CDB99F90A82F4EDCF93D8AC591CFD35D8DE95D119CEEA6868
          9284DB9E74A3A5FD5FD2EDDC6ABA75517E8E8C5F4190BE177CD9D5CCAF462727
          0DFA9D2D347C6252588D795BAA0BD3AEC1DDCBBD617D857635EFB618F3B6C42A
          08D2F782AF98C3FCE21DD12AFBF8EF8B0CBC2029C8A6BCF9AAA54090BEADFC3D
          9B67FF3B1A21DD24DE14B376F2F180B62F08D2B7D5F3D92C9D1905E9DE6BD3EF
          CCA42033397D2CD72148DFDE7E8ACDA313A3204F9D2DD4734852906E0FBB1841
          A68420B3201F5F61F10541363353F52A0D86C7D41064337307B4DB64F03D31B9
          0E41C6F4E917BBE9C0368DF8E3EED4BCEFAFCE550832A6EFDDF7A6995A5911F4
          3CA0E1BF37E32654B42DF13FC33BDF20C89806CE5965CACB0AE9EC7E9D6444AF
          72E9DABE846A6F21BA008B0BA3E38E6871FF10644C2EC8DA9733F9B6D1960041
          C68420FD4290312148BF10644C08D22F04191382F40B41C68420FD4290312148
          BF10644C08D22F04191382F40B41C68420FD4290312148BF10644C08D22F0419
          1382F40B41C68420FD4290312148BF10644C08D22F04191382F40B41C68420FD
          4290312148BF10644C08D22F04191382F40B41C68420FD4290312148BF10644C
          08D22F04191382F40B41C68420FD4290312148BF10644C08D22F04191382F40B
          41C68420FD4290312148BF10644C08D22F04191382F40B41C68420FD42903121
          48BF10644C08D22F04191382F40B41C68420FD4290312148BF10644C08D22F04
          191382F40B41C68420FD4290312148BF10644C08D22F04191382F40B41C68420
          FD4290312148BF10644C08D22F04191382F40B41C68420FD4290312148BF1064
          4C08D22F04191382F40B41C68420FD4290312148BF10644C08D22F04191382F4
          0B41C68420FD429031D50D12FC4290694290998520D38420330B41A609416616
          8204480141822A0812544190A00A8204551024A8822041150409AA2048500541
          822A0812544190A00A8204551024A8822041150409AA2048500541822A081254
          4190A00A8204551024A8822041150409AA2048500541822A0812544190A00A82
          04551024A8822041150409AA2048500541822A0812544190A00A8204551024A8
          822041150409AA2048500541822A0812544190A00A8204551024A88220411504
          09AA2048500541822A0812544190A00A8204551024A8822041150409AA204850
          0541822A0812544190A00A8204551024A8822041150409AA2048500541822A08
          12544190A00A8204551024A8822041150409AA2048500541822A0812544190A0
          0A8204551024A8822041150409AA2048500541822A0812544190A00A82045510
          24A8822041150409AA2048500541822A0812544190A00A820455FE0EFF72D649
          7188BC220000000049454E44AE426082}
        Stretch = True
      end
      object WelcomeLabel1: TNewStaticText
        Left = 176
        Top = 16
        Width = 301
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
        Left = 176
        Top = 76
        Width = 301
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
        Left = 176
        Top = 140
        Width = 301
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
        Left = 176
        Top = 288
        Width = 189
        Height = 17
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Create a new &empty script file'
        TabOrder = 3
      end
    end
    object MainPage: TNewNotebookPage
      DesignSize = (
        496
        314)
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
        Width = 485
        Height = 245
        ActivePage = AppInfoPage
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
        object AppInfoPage: TNewNotebookPage
          DesignSize = (
            485
            245)
          object AppNameLabel: TNewStaticText
            Left = 36
            Top = 8
            Width = 413
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
            Width = 413
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
            Width = 413
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
            Width = 413
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
            Width = 309
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
          end
          object AppVersionEdit: TEdit
            Left = 36
            Top = 76
            Width = 309
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 3
          end
          object AppPublisherEdit: TEdit
            Left = 36
            Top = 124
            Width = 309
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 5
          end
          object AppURLEdit: TEdit
            Left = 36
            Top = 172
            Width = 309
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 7
          end
        end
        object AppDirPage: TNewNotebookPage
          DesignSize = (
            485
            245)
          object AppDirNameLabel: TNewStaticText
            Left = 36
            Top = 84
            Width = 413
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
            Width = 413
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
            Width = 413
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
            Width = 309
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
            Width = 309
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
          end
          object AppDirNameEdit: TEdit
            Left = 36
            Top = 104
            Width = 309
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 4
          end
          object NotDisableDirPageCheck: TCheckBox
            Left = 36
            Top = 132
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Allow user to &change the application folder'
            TabOrder = 5
          end
          object NotCreateAppDirCheck: TCheckBox
            Left = 36
            Top = 176
            Width = 257
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'The application doe&sn'#39't need a folder'
            TabOrder = 7
            OnClick = NotCreateAppDirCheckClick
          end
        end
        object AppFilesPage: TNewNotebookPage
          DesignSize = (
            485
            245)
          object AppFilesLabel: TNewStaticText
            Left = 36
            Top = 100
            Width = 413
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
            Width = 413
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
            Width = 309
            Height = 117
            Anchors = [akLeft, akTop, akRight, akBottom]
            ItemHeight = 13
            TabOrder = 6
          end
          object AppFilesAddButton: TButton
            Left = 360
            Top = 119
            Width = 89
            Height = 23
            Anchors = [akTop, akRight]
            Caption = '&Add file(s)...'
            TabOrder = 7
          end
          object AppFilesEditButton: TButton
            Left = 360
            Top = 175
            Width = 89
            Height = 23
            Anchors = [akTop, akRight]
            Caption = '&Parameters...'
            TabOrder = 9
          end
          object AppFilesRemoveButton: TButton
            Left = 360
            Top = 203
            Width = 89
            Height = 23
            Anchors = [akTop, akRight]
            Caption = 'Remo&ve'
            TabOrder = 10
          end
          object AppExeEdit: TEdit
            Left = 36
            Top = 28
            Width = 309
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
          end
          object AppExeRunCheck: TCheckBox
            Left = 36
            Top = 56
            Width = 413
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Allow &user to start the application after Setup has finished'
            TabOrder = 3
          end
          object AppExeButton: TButton
            Left = 360
            Top = 27
            Width = 89
            Height = 23
            Anchors = [akTop, akRight]
            Caption = 'B&rowse...'
            TabOrder = 2
            OnClick = AppExeButtonClick
          end
          object AppFilesAddDirButton: TButton
            Left = 360
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
            Width = 413
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'The application doe&sn'#39't have a main executable file'
            TabOrder = 4
            OnClick = NoAppExeCheckClick
          end
        end
        object AppAssocPage: TNewNotebookPage
          DesignSize = (
            485
            245)
          object AppAssocNameEdit: TEdit
            Left = 36
            Top = 49
            Width = 309
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
          end
          object AppAssocNameLabel: TNewStaticText
            Left = 36
            Top = 28
            Width = 413
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
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Associate a file type to the main executable'
            TabOrder = 0
            OnClick = CreateAssocCheckClick
          end
          object AppAssocExtLabel: TNewStaticText
            Left = 36
            Top = 76
            Width = 413
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
            Width = 309
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 4
          end
        end
        object AppIconsPage: TNewNotebookPage
          DesignSize = (
            485
            245)
          object AppGroupNameLabel: TNewStaticText
            Left = 36
            Top = 28
            Width = 413
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
            Width = 413
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
            Width = 309
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
          end
          object NotDisableProgramGroupPageCheck: TCheckBox
            Left = 36
            Top = 76
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Allow user to &change the Start Menu folder name'
            TabOrder = 3
            OnClick = NotDisableProgramGroupPageCheckClick
          end
          object AllowNoIconsCheck: TCheckBox
            Left = 36
            Top = 96
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Allow user to &disable Start Menu folder creation'
            TabOrder = 4
          end
          object DesktopIconCheck: TCheckBox
            Left = 36
            Top = 180
            Width = 257
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Allow user to create a deskto&p shortcut'
            TabOrder = 8
            OnClick = NotCreateAppDirCheckClick
          end
          object CreateUninstallIconCheck: TCheckBox
            Left = 36
            Top = 136
            Width = 301
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Create an &Uninstall shortcut in the Start Menu folder'
            TabOrder = 6
          end
          object CreateURLIconCheck: TCheckBox
            Left = 36
            Top = 116
            Width = 301
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Create an &Internet shortcut in the Start Menu folder'
            TabOrder = 5
          end
          object UseAutoProgramsCheck: TCheckBox
            Left = 36
            Top = 8
            Width = 425
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
            485
            245)
          object AppLicenseFileLabel: TNewStaticText
            Left = 36
            Top = 8
            Width = 413
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
            Width = 413
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
            Width = 413
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
            Width = 309
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
          end
          object AppLicenseFileButton: TButton
            Left = 360
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
            Width = 309
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 4
          end
          object AppInfoBeforeFileButton: TButton
            Left = 360
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
            Width = 309
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 7
          end
          object AppInfoAfterFileButton: TButton
            Left = 360
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
            485
            245)
          object PrivilegesRequiredLabel: TNewStaticText
            Left = 36
            Top = 8
            Width = 413
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
            Width = 413
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Administrative install mode (install for all users)'
            TabOrder = 1
          end
          object PrivilegesRequiredLowestRadioButton: TRadioButton
            Left = 36
            Top = 48
            Width = 413
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Non administrative install mode (install for current user only)'
            TabOrder = 2
          end
          object PrivilegesRequiredOverridesAllowedCommandLineCheckbox: TCheckBox
            Left = 36
            Top = 68
            Width = 413
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Allow user to &override the install mode via the command line'
            TabOrder = 3
          end
          object PrivilegesRequiredOverridesAllowedDialogCheckbox: TCheckBox
            Left = 36
            Top = 88
            Width = 413
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Ask the user to &choose the install mode at startup'
            TabOrder = 4
            OnClick = PrivilegesRequiredOverridesAllowedDialogCheckboxClick
          end
        end
        object AppRegistryPage: TNewNotebookPage
          DesignSize = (
            485
            245)
          object AppRegistryMinVerDocImage: TImage
            Left = 360
            Top = 161
            Width = 16
            Height = 16
            Anchors = [akTop, akRight]
            AutoSize = True
            Transparent = True
          end
          object AppRegistryFileLabel: TNewStaticText
            Left = 36
            Top = 8
            Width = 413
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '&Windows registry file (.reg) to import:'
            FocusControl = AppRegistryFileEdit
            TabOrder = 0
            WordWrap = True
          end
          object AppRegistryFileEdit: TEdit
            Left = 36
            Top = 28
            Width = 309
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
          end
          object AppRegistryFileButton: TButton
            Left = 360
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
            Width = 392
            Height = 230
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
            WordWrap = True
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
            Width = 59
            Height = 21
            Anchors = [akTop, akLeft, akRight]
            TabOrder = 8
          end
        end
        object LanguagesPage: TNewNotebookPage
          DesignSize = (
            485
            245)
          object LanguagesLabel: TNewStaticText
            Left = 36
            Top = 8
            Width = 413
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
            Width = 309
            Height = 209
            Anchors = [akLeft, akTop, akRight, akBottom]
            Offset = 2
            TabOrder = 1
          end
          object AllLanguagesButton: TButton
            Left = 360
            Top = 27
            Width = 89
            Height = 23
            Anchors = [akTop, akRight]
            Caption = '&Select all'
            TabOrder = 2
            OnClick = AllLanguagesButtonClick
          end
          object NoLanguagesButton: TButton
            Left = 360
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
            485
            245)
          object OutputDirLabel: TNewStaticText
            Left = 36
            Top = 8
            Width = 413
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
            Width = 309
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
          end
          object OutputBaseFileNameLabel: TNewStaticText
            Left = 36
            Top = 56
            Width = 413
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
            Width = 309
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 4
          end
          object SetupIconFileLabel: TNewStaticText
            Left = 36
            Top = 104
            Width = 413
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
            Width = 309
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 6
          end
          object PasswordLabel: TNewStaticText
            Left = 36
            Top = 152
            Width = 413
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
            Width = 309
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 9
            OnChange = PasswordEditChange
          end
          object SetupIconFileButton: TButton
            Left = 360
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
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Use the password to &encrypt the application files'
            TabOrder = 10
            OnClick = NotDisableProgramGroupPageCheckClick
          end
          object OutputDirButton: TButton
            Left = 360
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
            485
            245)
          object ISPPLabel: TLabel
            Left = 36
            Top = 8
            Width = 425
            Height = 81
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '*'
            WordWrap = True
          end
          object ISPPCheck: TCheckBox
            Left = 36
            Top = 90
            Width = 425
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
        Width = 497
        Height = 58
        Anchors = [akLeft, akTop, akRight]
        BevelOuter = bvNone
        Color = clWindow
        TabOrder = 1
        DesignSize = (
          497
          58)
        object InnerImage: TImage
          Left = 438
          Top = 1
          Width = 55
          Height = 55
          Anchors = [akTop, akRight]
          Picture.Data = {
            0954506E67496D61676589504E470D0A1A0A0000000D49484452000000370000
            003708030000009F052274000000017352474200AECE1CE90000000467414D41
            0000B18F0BFC6105000001D7504C5445FFFFFFFAE7D6F1AE76F8DAC1EFA464F7
            D3B4EEA05EFEFDFBF6CBA7EE9D58FEFCFAEFA364F4C39AED9952FEFBF9F2B684
            F7D0AFE4F0F890C4E391C4E4A4CEE8FEFEFEFAE6D5EC9043F8DAC0F3BB8DED94
            4CFDF3ECEFA160F1B0799CCAE62289C8228AC886BEE1F8D8BEEA8733F5C8A2F2
            B47FEC9145FAE5D4EC9246EEA05FFEFBF8FAFCFCFDFDFDFCFCFC8FC3E3228AC9
            86BFE1F5C79FEB852FF0AC72EB8F40F7D2B3EA8937EC944AFEFEFD82BBDF238A
            C987BFE1EA842FEFA669EE9E5AEB8A38EA8630EB8B3BFBEADB8FC2E2FBFCFC81
            BBDF88C0E1EB8732F9DCC32289C9F7D2B2F5C9A4F4C196F3B988F0A86BEB8B39
            F2B480F3BD8FF5C59DF6CDABF8D6B9F9DEC8FDF7F180BADF89C0E2EB8D3BEA89
            35ED9951EFA15F7FBADE8AC0E2FDF6F0FBEDE1FAE5D3F2B683EB8E3FFBE9DAFC
            F1E8FEFAF78AC1E2FAE4D2EB8B3AF1AD757DB9DE8BC1E2FCF0E7EC9144EB8C3B
            F9DDC6F8DBC2F1AD73F4C0947DB8DE8DC2E2FDF9F5EE9B56EC954BFBEADCF4BE
            91EA85317CB8DEFEFDFCF0AA6FFDF4ECF2B37EFCF1E7FAE2CE8EC2E23493CDF1
            AF78F3BB8CFDF8F4F9E0CAED974FFCEFE490C3E3238AC8F6D0AFF4C399EA8632
            FBEDE0F9DFC9F6CAA6EB8D3EFCF0E5F7D1B1FCFDFE64ADD9D8EAF5F8FBFC90C3
            E259A7D658A7D6976A4558000000097048597300000EC300000EC301C76FA864
            0000023B4944415478DAED955F48145118C5BFB3195B52B622B377B6720343B6
            C8AC5516F62522AD207A0BEAD5D7A82C7A11A4D422B08528A296A27A3302699F
            9308AD087A8B5628F641D6226A4B19849642CBFE8CDF9D3F8BE9CCB8CE4B44FB
            71B9C3CCE577CFB9E75EEE80FC15FE2A07FCF6C5ADC04F5FDC4ACC2E930B7E97
            FD2ACC186FD5D36572C1AA699D1F6BF085FB1AE0732DA083F85B009A975E1898
            208AE023D106E09B16B506A007DE7AAEAF01C85323C6280620B74DCAE9922A86
            50D03C388A63FCC756BC48001F0AC9AF54E2D603CFBD38252458698CE51E9342
            C21A0AE7D943EC91D73E54CF3673900A1EDA1309436FAD8AAA5F59CD81836861
            A5D1F89346E6EA1FECA88751F7156170A4D6BD8F0E3A70ADB9E6864F7F1C94E8
            C85E0C4F28AAF459432422CF760F38FB4CCA1847592F5ED808DC36BE35993EB9
            5723E3DB6FB9ADEFE8CBA9FDAF62D96446AE4521231A8B2335966D493B739D2C
            78279EC0D33D97CD48AC7D3038889D9923979CB82EC62EB6AEDEF2BA0DD9214E
            C43E47727DB244F2EE7CD01EEFC19BCDE7E9DCF0BE3E0A75D4F5CA282DBD75A4
            68B21D4666577A111739712F47D43F74B09B28057429AAC9756084DAED967BB7
            900BF6A7641A57709A13D1AEE294DC838515CE2FE2ACBA864E4E7F92A768B27C
            CE6F81A21B97684F4962924A7996C7A9C51927834BFA4CE3B84CD29CBE7CBD1B
            C7F8A0F8D0BB099C15B47CBD036D033AF9E0369DB94E1EE5EAD3AF5EC567C567
            C5E77FE4F3D0205FF1E6EF47D1EC67A99DBCE0E6939628B7FBBAFCFA57B83987
            B69D473A0117C30000000049454E44AE426082}
          Stretch = True
        end
        object PageNameLabel: TNewStaticText
          Left = 24
          Top = 10
          Width = 405
          Height = 13
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = '*'
          TabOrder = 0
        end
        object PageDescriptionLabel: TNewStaticText
          Left = 40
          Top = 26
          Width = 389
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
      Color = clWindow
      ParentColor = False
      DesignSize = (
        496
        314)
      object FinishedImage: TImage
        Left = 0
        Top = 0
        Width = 164
        Height = 314
        Anchors = [akLeft, akTop, akBottom]
        Stretch = True
      end
      object FinishedLabel: TNewStaticText
        Left = 176
        Top = 16
        Width = 301
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
    Left = 248
    Top = 326
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = '&Back'
    TabOrder = 0
    OnClick = BackButtonClick
  end
  object NextButton: TButton
    Left = 323
    Top = 326
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = '*'
    Default = True
    TabOrder = 1
    OnClick = NextButtonClick
  end
  object CancelButton: TButton
    Left = 408
    Top = 326
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
