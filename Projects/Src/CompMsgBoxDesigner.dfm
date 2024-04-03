object MsgBoxDesignerForm: TMsgBoxDesignerForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'MsgBox/TaskDialogMsgBox Call Designer'
  ClientHeight = 380
  ClientWidth = 548
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    548
    380)
  PixelsPerInch = 96
  TextHeight = 13
  object TaskInstructionLabel: TNewStaticText
    Left = 18
    Top = 14
    Width = 50
    Height = 14
    Caption = 'Instruction'
    TabOrder = 11
  end
  object TaskInstructionText: TEdit
    Left = 89
    Top = 10
    Width = 449
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object TaskMessageLabel: TNewStaticText
    Left = 18
    Top = 40
    Width = 49
    Height = 14
    Caption = 'Message'
    TabOrder = 12
  end
  object TaskMessageText: TEdit
    Left = 89
    Top = 36
    Width = 449
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object Button1Label: TNewStaticText
    Left = 18
    Top = 66
    Width = 62
    Height = 14
    Caption = 'Text Button1'
    TabOrder = 13
  end
  object Button1Text: TEdit
    Left = 89
    Top = 62
    Width = 159
    Height = 21
    TabOrder = 3
  end
  object Button2Label: TNewStaticText
    Left = 258
    Top = 67
    Width = 62
    Height = 14
    Caption = 'Text Button2'
    TabOrder = 14
  end
  object Button2Text: TEdit
    Left = 334
    Top = 62
    Width = 204
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 532
    Height = 81
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Message '
    TabOrder = 0
    DesignSize = (
      532
      81)
    object MSGText: TMemo
      Left = 7
      Top = 20
      Width = 519
      Height = 53
      Anchors = [akLeft, akTop, akRight, akBottom]
      ScrollBars = ssVertical
      TabOrder = 0
      OnKeyPress = MSGTextKeyPress
    end
  end
  object GroupBox6: TGroupBox
    Left = 8
    Top = 93
    Width = 240
    Height = 60
    Caption = ' Type'
    TabOrder = 5
    object cb_Suppressible: TCheckBox
      Left = 14
      Top = 24
      Width = 86
      Height = 17
      Caption = 'Suppressible'
      TabOrder = 0
      OnClick = cb_SuppressibleClick
    end
    object cb_MsgBox: TRadioButton
      Left = 115
      Top = 13
      Width = 115
      Height = 17
      Caption = 'MsgBox'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = cb_MsgBoxClick
    end
    object cb_TaskDialogMsgBox: TRadioButton
      Left = 115
      Top = 36
      Width = 115
      Height = 17
      Caption = 'TaskDialogMsgBox'
      TabOrder = 2
      TabStop = True
      OnClick = cb_TaskDialogMsgBoxClick
    end
  end
  object GroupBox5: TGroupBox
    Left = 258
    Top = 93
    Width = 282
    Height = 60
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Flags '
    TabOrder = 6
    object NewStaticText1: TNewStaticText
      Left = 14
      Top = 26
      Width = 68
      Height = 14
      Caption = 'Default button'
      TabOrder = 3
    end
    object NewEdit1: TEdit
      Left = 97
      Top = 22
      Width = 21
      Height = 21
      Alignment = taRightJustify
      NumbersOnly = True
      ReadOnly = True
      TabOrder = 0
      Text = '1'
    end
    object UpDown1: TUpDown
      Left = 118
      Top = 22
      Width = 16
      Height = 21
      Associate = NewEdit1
      Min = 1
      Max = 3
      Position = 1
      TabOrder = 1
      OnChanging = UpDown1Changing
    end
    object cb_MB_SETFOREGROUND: TCheckBox
      Left = 140
      Top = 24
      Width = 135
      Height = 17
      Caption = 'MB_SETFOREGROUND'
      TabOrder = 2
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 157
    Width = 160
    Height = 174
    Anchors = [akLeft, akTop, akBottom]
    Caption = ' Icon '
    TabOrder = 7
    DesignSize = (
      160
      174)
    object IMGmbInformation: TImage
      Left = 10
      Top = 23
      Width = 32
      Height = 32
      AutoSize = True
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000200000
        00200806000000737A7AF4000003A54944415478DAC5974D4F134118C767BBED
        F62DC105696340494B4810D0848331DCA0A8C1C4C4183D889E163F01C40F60FC
        00A67E02DA937A32440FC6042DE8CD48E2453829B5221A0A02625FB6A55D9F67
        B6BBDD6D77BB03419D64E86CBBECEFBFCFFF99676638F29F1BC77A63E0E25D11
        3EAE411F851E813ED670CB02F434F445E873F9F9073B4722A0068E23BCEA6B17
        2B6DDDA41A0C131813C5E5511F52291157719BB87259C2EFADC17807E173D067
        9C84700EF069F8B8B7DF1E15CBE1B344F104D41F14E35D8AF91AC65CF937F164
        3F12F76E1AE1F741C4C30309D0DE5A118252E9E408A904C275AA03DCF8050F11
        11BEBF0341B9A45D34381B780A423C2CF78E43988543C1B521DAE3CD2C1097BC
        F3012E638D22AC0424002E39C1F9DC06F51C5B35108228859AE09A48AE5AD644
        2441C094AD00F41CC21E2FF65D767C73F4B8F448A2DF08B713A4DC396409D741
        1009DF9779B463C698139C018EA15F957B2F882C9E5B0AB0811345BDE00B59E2
        FDBA881644352B8C021290ED52A97B84C9736A417EA366419854FCA19670AD09
        3FDE13F7AFB46E05677CFB42FF559175AAF17983007FB89603ADE1EA14CD117F
        FA851E054D8004899728F64D30C1F18F67132D50F349B885160C3AC2B5812FF3
        0A13720A042435018972F88C049D79AA3509383EC804A7FFBBB5423C3F97A90D
        9A80941C1D1F5393CF194E1F8249F8D84280039CDA87C9F8EDCD020888690294
        C2C00DBDB6B314191A8146010C706C5817FC9F9F1110C0E902F24393CC70DD02
        A3808E0126B816A5C0A7A75602D8E06A122ED7054CCE36E4406B3865360A289C
        BE6EB0A0359C5AB0652380014E2D587D6E12909223B17A123AC0A905560218E0
        D8F8C226F1AE9B9330510EC1340C0D31C1F51C7872A72EA06390094EA3B7BD82
        DD340DA1108989627482098E0FA4116812E00CC7E65B834254DA351522B514F7
        5D81521C7484EB1698040C30C1B97D28C59997E652ACD9B07F2C2295BACE3BC2
        6D0538C071206497887B2F635E8C8C51907B6222CBC262127073D65C076CE07C
        1193EFADF5725C13310D16C48B914B0D53B2B9C2412DB7166003C7A9E75B7B0D
        16E4AD3724462BAA5E51927BC6541136E5952FE072BC49C7557FA7BA1F680187
        37C7C46BBD253358910211C3F2A9518BE2641830786E80B36D4A0D22E2608754
        3A71AEF6760787A3E7C2C612863D4958B7E50D42D483495B44448F2D774B3653
        0D8B0D64FBE10E2656D1207834F38A6225D8453D87315138B7FA1008B34BDE25
        2E58E7F9DC3A86FB688E661642FEFDE1F46FB73FA626C73F4C6061F500000000
        49454E44AE426082}
    end
    object IMGmbConfirmation: TImage
      Left = 10
      Top = 60
      Width = 32
      Height = 32
      AutoSize = True
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000200000
        00200806000000737A7AF4000004DF4944415478DAC5576D6C5355187E6FEF6E
        BF966C774A5BB28DD18D05990CB798612689FB0205E38246493031C68E9825FE
        6341C5F8430331F84170FB4316FDD3256A42FC4118981817B0ED16FDE120D990
        0F11EBC61873B485756EEB6D6FD7D6F79CBB76F7B6F7B67704F524273DF7F4E4
        3CCF799FE7BCE71C06FEE7C2E81D68DD7588C79F97B0B66275626DCB1AE2C53A
        89D587F54CE4FC89F04321B002DC4BC093E6323E515201C9623B601B52064E9A
        242182213A0786A520B00BD3D80E13F033587B0A11610A801FC49F0F97CBAAF9
        B87D1BA438ABF4474A3E2AA5FCC636135F042E78158AE62709F81124D1B72602
        E955A78CC52EB1B2191256FB2A6A017079078B1131FEF50B125A1AD08A06A301
        EEC11037C66A3A30CC464D708310062629D2FEA499CF48221F4BE4314D79C110
        0B8FE1677B360935026E04776981E36A800B5C811A6B045AEAAB60E3FA32DA7F
        F98F19181C0B80E86884A48957448849C6D324069040972601A23986BD375ABB
        47159C186D3B7B038E773F0FCF346CCA91EED6DD39D87FF41B188D6F4612A58A
        889148986F9D270BE8917B82918113DA13B19A9DBC96E6ECD25D38DAC2C1FBAF
        3F07F38B020C5FF6C3AFFE19E8DC510F4F6C2AA76348FF96037D306BEF90C921
        4DC20A4130DDF61109AAD352C809B8D1ED2EB1A2595373762940096C43B0EEBE
        EF2064AC84146E4776E10EB89A4AE18B77F6D391EFF60FC2E7A32C9AD796014F
        17E3EC4528FA7B322305235FBDF0D85E3EDF56632301D82C8C833F5A0262F953
        8ABF8D8131183DD649233132EE879D9F0C43FC913AA54629C94396C9EF335148
        1370A1F1DCD1DADD9AE092040160C31339E0A470A1AB70E1BD16EA0D4AE0631F
        C41F7D3C671C6998A72E104376218181340177DC5EEFC29A779F1323A96D3569
        522FFCF665376C7494C1B1AF86E0034F0C12165BCE384AF6DE75E0EE5FA332A4
        097862D51D6D92F9D4C1B3938C7C52CC78D0F3B4193E7BEB45DA53F7C671B861
        6D96C8668153298919EF0C7B91407B9A404AA87B45B6BA3580DFBF09071AB98C
        014F9E1E819E6F6F4AE15701A791C4BC60F9F32C2001264320B2F5D5358373A1
        6BD0F5A43503FEF5D04578B37F1862E53B34C1A5760AACFED36A04F483933CBF
        6F43084E1D7129C045C776652455C02966360161CBCB2A065307A7DB6E76147E
        3FF91A35DDB99FAFC0BE135E10D737155C79468289730A029E98B37DD58405C0
        4931DFF682307898B6F7BCDD0F438BF5BA564E0A2B84C034A334A13B6EC36D68
        DBAA0B3C9B80A5F3238856EDD2054EFAB9B9EBA42AB6212622DE1DADDEAD0B9C
        4CE8B8F713CC9E3A2411D8FB29442B5B758153F2D39888C4794522925271ED0B
        988A8B0B824B93F832FD494CDFA2A3491738B38CA978EA07652A4ECBB05CEA74
        D1345B009C1463709CDCFD240278F48AB68682E0A4610C5E82A28529E561248F
        42ACAA9DA7A7581E70C9033E10CE1E5E95A0A2A520381B25E61B513F8E57481C
        44097AA3CE67B3B6A4129C4C4A245025A0014EB69E79FA479420A27E21914B81
        572A57ACAA6D2597E7824B128CE18936BF2AC1BA86BCE0B87262BCFC57329914
        1E24D118DBD0AA79FA29BF410FB8BE4BA98C442FCAE122D98D1EAB0F004E3437
        062E91B00F80DE6B791611E96152E2E4C9ED46F5B6A4B1D548B241B73FD8C344
        2D1A409E66269E4F149743D2B28E5EBD534C9134098699F8C180E73CBB3443C2
        FD709E662A44FEFBC7E9BF5DFE01E32FFB3F42F099B90000000049454E44AE42
        6082}
    end
    object IMGmbError: TImage
      Left = 10
      Top = 97
      Width = 32
      Height = 32
      AutoSize = True
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000200000
        00200806000000737A7AF40000033A4944415478DAD597DD4E135110C7678556
        935E5883B12636E982893591160D91822602811B4D84F5014CE0D23B7D02F109
        F4CE4B487C001645A2494D8BC634944F294DFC82564193E2572536428BA933BB
        3DBBDDEE6E778950E2C92673F69493FF6FE6CCF967E1609F07F7DF023CB80103
        18864BAF83D7EFC348CD00509CC7900A0679E97D71314DA11121D2B5021876B9
        0E0D08423B40B108E2D814E4729B230830B8E70028DE8521D2D9D90CDE130DD2
        DADADA57987C9EA469374244F71A20E2F1B8BB7A7B5A34EBE167AF2093C94611
        A07BCF0058F6BDBD2DE039E6D6FC86E212C44EABB0538094D77B94EFBC7446FF
        23F6C2E48B241EC7B7340234EE3A00BB76427F08B00175E234B011417C18A7A9
        ED6B690B00C5A9DEA9D37EAFBBB5F5A4BCB16E53235EFC2343CDCE2DC3EB379F
        B2205FCBEC6E010C399DF5B7FBFB42805101E02E4CC9E22FDB14807C7E1BC61E
        C529DE4180A17F0660D90703BC3B10F0A91B0FFC06EE625C074015492C7D84C5
        A50FB6AA600740329D2B975B95EC4944AA402540E938F2856D9878326FCB9C38
        0B719EB2EF68F74353D371455C39827280ED839ABD2BA90CC4A6DE025858B415
        C028662F50E7978B5B0314A5471C9FA12A8808706DC7003AD32913AF0E208BD3
        C8ACFF84702441535373AA06A05A6E85B839802ACE06012088A9451B0230D3A1
        C63BE27619974E07E0D489D3F891FD05134F17686A684E6600296C3ABE2374CA
        B83C740BEAB7B40005A7D11F4A4F2CFE0E56D2EB8616AD0340F19B18EE0A7D6D
        7ACB2D894B1B2D01D4E3C8E5B6407C3C43D35B0871CF1440351D9F3BD0EC3315
        B706D0F7422289E6945CD5995325806CB957DB54D33110AF0E5034EC857CA100
        63E37364521A8BE6CAC4790CF386D9EB6E01F5405EBB2201188BB3C544729555
        E11C33A77200F93B0FCFDE4A9C1ECE5109E0A82ACE8238314B3DA1583457123F
        4BD94B96DBE8B114973622807A04E7A198775A8AD3C0DB00B1E9F750AAC20203
        88A0DBA1E9046D89DB03D08BB397703409992F1B9239718AE5F690E51EB625CE
        00CA17B500E6E214501CC293F25734018C62F68292BD0D71CBF52AE26C100082
        880410F17A1BBA3A427E703AEA6A224EDF0BB1E96558FBFC3D4A0002AE8D42ED
        075DC7C17DFFEFF82F44FFE05EBAD2EA730000000049454E44AE426082}
    end
    object IMGmbCriticalError: TImage
      Left = 10
      Top = 135
      Width = 32
      Height = 32
      AutoSize = True
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000200000
        00200806000000737A7AF40000022B4944415478DACD97312C035118C7BFD75E
        A5AAD14B6B301818DA49824D24A235582C62B5B4BB049B8930D990D8AF8B552C
        16835624624362626030185468AA15DA9EEF6BEE5D9E7375774D79BEE4E5AECD
        DDFBFFEEFB7FEFBB770C2407737BE16E22A6E26106C7048E7E1C49CB25791C77
        388E71ECCF5D179EDB0260086F9278B7C2D4DE0E06D100033C878071F7870E50
        ACEAF084270FEF3A9D93F83E8E252710E620BE8887D5BEA04F4D847CD0E97397
        AD4A1DE0BA5C87FBB73A89AF21C4962700FED49D7E961E0AFB201670EDD49728
        60462E4B75A8D4F46CB36C7C9BD910CF618A8747237E6851DB0CB2E7ECA546B6
        5CE0CF9415C20E4043F1743BC46D20B20890690A409E63DA37C7D5F6898B1027
        CF35B26349AC0953C648FD2D3EB92A7ACEC211086D68F0B6BD02B59B2B5762FE
        F8200417D6A1BC9C01BDF462FE4F358199200B06B815228086D5DE283A31BA76
        F6401919C3898AF03A3FEB0841E2740F0B7743F5FCB4718F185494B83A4C2B98
        F8F4935145B52E357142270837D7D2123D7AAA9A59E000692C3C8DBC6F75622F
        A0540B58901904C872000D1B4D3A1E6ADE697E12F0224E71834D0A1B55C3060E
        90C3E24B3A351C3B215E276EC5298C62CC23408A03E85331C5D5D2B342340AC9
        8338052DC9C3421510809900D33D8AB3BA0D048517711E078FFF0D40B605D28B
        50FA3294DE88E4B6626E83B49791980569AF630342DE8644B442DA964CB042DE
        A6548090B32DB780C8F930B1CB06C8F834B301F9FB8FD3DF8E4F1B53543FEFB0
        D5E80000000049454E44AE426082}
    end
    object rb_mbInformation: TRadioButton
      Left = 53
      Top = 31
      Width = 99
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'mbInformation'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rb_mbConfirmation: TRadioButton
      Left = 53
      Top = 68
      Width = 99
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'mbConfirmation'
      TabOrder = 1
      TabStop = True
    end
    object rb_mbError: TRadioButton
      Left = 53
      Top = 105
      Width = 99
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'mbError'
      TabOrder = 2
      TabStop = True
    end
    object rb_mbCriticalError: TRadioButton
      Left = 53
      Top = 142
      Width = 99
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'mbCriticalError'
      TabOrder = 3
      TabStop = True
    end
  end
  object GroupBox3: TGroupBox
    Left = 179
    Top = 157
    Width = 169
    Height = 174
    Anchors = [akLeft, akTop, akBottom]
    Caption = ' Buttons '
    TabOrder = 8
    DesignSize = (
      169
      174)
    object rbMB_OK: TRadioButton
      Left = 14
      Top = 23
      Width = 147
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'MB_OK'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rbMB_OKClick
    end
    object rbMB_OKCANCEL: TRadioButton
      Left = 14
      Top = 46
      Width = 147
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'MB_OKCANCEL'
      TabOrder = 1
      TabStop = True
      OnClick = rbMB_OKCANCELClick
    end
    object rbMB_YESNO: TRadioButton
      Left = 14
      Top = 70
      Width = 147
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'MB_YESNO'
      TabOrder = 2
      TabStop = True
      OnClick = rbMB_YESNOClick
    end
    object rbMB_YESNOCANCEL: TRadioButton
      Left = 14
      Top = 94
      Width = 147
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'MB_YESNOCANCEL'
      TabOrder = 3
      TabStop = True
      OnClick = rbMB_YESNOCANCELClick
    end
    object rbMB_RETRYCANCEL: TRadioButton
      Left = 14
      Top = 118
      Width = 147
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'MB_RETRYCANCEL'
      TabOrder = 4
      TabStop = True
      OnClick = rbMB_RETRYCANCELClick
    end
    object rbMB_ABORTRETRYIGNORE: TRadioButton
      Left = 14
      Top = 142
      Width = 147
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'MB_ABORTRETRYIGNORE'
      TabOrder = 5
      TabStop = True
      OnClick = rbMB_ABORTRETRYIGNOREClick
    end
  end
  object GroupBox4: TGroupBox
    Left = 359
    Top = 157
    Width = 181
    Height = 174
    Anchors = [akLeft, akTop, akBottom]
    Caption = ' Return values '
    TabOrder = 9
    object cb_IDOK: TCheckBox
      Left = 16
      Top = 23
      Width = 80
      Height = 17
      Caption = 'IDOK'
      TabOrder = 0
    end
    object cb_IDCANCEL: TCheckBox
      Left = 16
      Top = 42
      Width = 80
      Height = 17
      Caption = 'IDCANCEL'
      TabOrder = 1
    end
    object cb_IDYES: TCheckBox
      Left = 16
      Top = 62
      Width = 80
      Height = 17
      Caption = 'IDYES'
      TabOrder = 2
    end
    object cb_IDNO: TCheckBox
      Left = 16
      Top = 82
      Width = 80
      Height = 17
      Caption = 'IDNO'
      TabOrder = 3
    end
    object cb_IDABORT: TCheckBox
      Left = 16
      Top = 102
      Width = 80
      Height = 17
      Caption = 'IDABORT'
      TabOrder = 4
    end
    object cb_IDRETRY: TCheckBox
      Left = 16
      Top = 122
      Width = 80
      Height = 17
      Caption = 'IDRETRY'
      TabOrder = 5
    end
    object cb_IDIGNORE: TCheckBox
      Left = 16
      Top = 142
      Width = 80
      Height = 17
      Caption = 'IDIGNORE'
      TabOrder = 6
    end
    object rb_IDOK: TCheckBox
      Left = 100
      Top = 23
      Width = 16
      Height = 17
      TabOrder = 7
      Visible = False
      OnClick = rb_IDOKClick
    end
    object rb_IDCANCEL: TCheckBox
      Left = 100
      Top = 42
      Width = 16
      Height = 17
      TabOrder = 8
      Visible = False
      OnClick = rb_IDCANCELClick
    end
    object rb_IDYES: TCheckBox
      Left = 100
      Top = 62
      Width = 16
      Height = 17
      TabOrder = 9
      Visible = False
      OnClick = rb_IDYESClick
    end
    object rb_IDNO: TCheckBox
      Left = 100
      Top = 82
      Width = 16
      Height = 17
      TabOrder = 10
      Visible = False
      OnClick = rb_IDNOClick
    end
    object rb_IDABORT: TCheckBox
      Left = 100
      Top = 102
      Width = 16
      Height = 17
      TabOrder = 11
      Visible = False
      OnClick = rb_IDABORTClick
    end
    object rb_IDRETRY: TCheckBox
      Left = 100
      Top = 122
      Width = 16
      Height = 17
      TabOrder = 12
      Visible = False
      OnClick = rb_IDRETRYClick
    end
    object rb_IDIGNORE: TCheckBox
      Left = 100
      Top = 142
      Width = 16
      Height = 17
      TabOrder = 13
      Visible = False
      OnClick = rb_IDIGNOREClick
    end
    object cb_DefIDOK: TRadioButton
      Left = 147
      Top = 23
      Width = 17
      Height = 17
      TabOrder = 14
      TabStop = True
      Visible = False
    end
    object cb_DefIDCANCEL: TRadioButton
      Left = 147
      Top = 42
      Width = 17
      Height = 17
      TabOrder = 15
      TabStop = True
      Visible = False
    end
    object cb_DefIDYES: TRadioButton
      Left = 147
      Top = 62
      Width = 17
      Height = 17
      TabOrder = 16
      TabStop = True
      Visible = False
    end
    object cb_DefIDNO: TRadioButton
      Left = 147
      Top = 82
      Width = 17
      Height = 17
      TabOrder = 17
      TabStop = True
      Visible = False
    end
    object cb_DefIDABORT: TRadioButton
      Left = 147
      Top = 102
      Width = 17
      Height = 17
      TabOrder = 18
      TabStop = True
      Visible = False
    end
    object cb_DefIDRETRY: TRadioButton
      Left = 147
      Top = 122
      Width = 17
      Height = 17
      TabOrder = 19
      TabStop = True
      Visible = False
    end
    object cb_DefIDIGNORE: TRadioButton
      Left = 147
      Top = 142
      Width = 17
      Height = 17
      TabOrder = 20
      TabStop = True
      Visible = False
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 338
    Width = 548
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 10
    DesignSize = (
      548
      42)
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 548
      Height = 3
      Align = alTop
      Shape = bsBottomLine
      ExplicitLeft = 168
      ExplicitWidth = 50
    end
    object MBDButtonPreview: TButton
      Left = 10
      Top = 11
      Width = 80
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Preview'
      TabOrder = 0
      OnClick = MBDButtonPreviewClick
    end
    object MBDButtonOK: TButton
      Left = 379
      Top = 11
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Insert'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
    object MBDButtonCancel: TButton
      Left = 463
      Top = 11
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
  end
end
