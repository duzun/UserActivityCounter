object Form1: TForm1
  Left = 1091
  Top = 179
  Width = 320
  Height = 250
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'User Activity Counter'
  Color = clRed
  Constraints.MaxHeight = 250
  Constraints.MaxWidth = 320
  Constraints.MinHeight = 250
  Constraints.MinWidth = 320
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001004040040000000000680A00001600000028000000400000008000
    0000010004000000000000080000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000333333333333310000000000000000000000000000
    0000000000000000333333333333333333333330000000000000000000000000
    0000000000000023333333333333333333333333300000000000000000000000
    0000000000000333333333333333333333333333333100000000000000000000
    0000000000033333333333333333333333333333333330000000000000000000
    0000000002333333333333033333333333333333333333000000000000000000
    0000000033333333333333033333333333333333333333300000000000000000
    0000000333333333333322033333333333333333333333111000000000000000
    0000003333333333332222213110011333333333333331199100000000000000
    0000033333333333222220000000000000033333333331991911000000000000
    0000233333333322222000000000000000000133333301119191000000000000
    0002333333333222200000000000000000000001333011111999100000000000
    0023333233322220000000000077700000000000030111111199900000000000
    0333333323222200000000777777777710000000001111111119910000000000
    0233333332222000000077778777777777000000000111111111111000000000
    2333333322220000000778888888888777770000000011111111111000000000
    3333333222200000077888888888888888777000000001111111119100000000
    2333333222000000778888888888888888877700000000111111111100000002
    2223332220000007888888888888888887887770000000011111111910000022
    2222232220000007888888778888888888888877000000011111111110000022
    2222222200000078888888888888888888888877700000001111111191000022
    2222222000000788888778888888888888888887700000001111111111000222
    2222222000000788888888888888888888888888730000000111111111000222
    2222222000007888888888888888888888888888770000000111111111000222
    2222220000000887888888888888888888888888770000000011111111100222
    2222220000007887888888888876788888888888870000000011111111100222
    22222200000078878FFF8888F706078888888888877000000011111111112222
    2222220000008888FFFFF88F807F768888888888877000000011111111102222
    2222220000008888F88FFFFF807F768888888888877000000011111111102222
    2222200000008887FFFFF8FFF704068888888888877000000011111111102222
    2222200000007F87FFFFFFFFFF87707888888888870000000001111111102222
    2222200000007F87FF8FFFFFFFFF870788888888870000000078888877802222
    22222000000078F88FFFFFFFFFFFF87078888888870000000078888888872222
    22222000000007F87FFFFFFFFFFFFF8648888888870000000078888888802222
    220000000000078F88FF8FFFFFFFFFF867888888700000000078888888800222
    222222000000007FF78F8FFFFFFFFFFF86788888700000000088888888770266
    2222220000000078F878FFF8FF888F88F8888887700000000088888888700262
    2222220000000007FF878FF8FF8788888F888877000000000788888888700222
    22222220000000007FF8788FFFF8FF8888888777700000000788788888700222
    2222222000000000078FF888888F888888887777770000000887446788000222
    22222220000000000078FFF88888888888877877777000007886444444000022
    2222222200000000000078FFF888888888778F877700000088744444C4000022
    2622222220000000000007788888888777007FF7770000078874444440000002
    2222222220000000000000077787778700000788700000788744444440000002
    22222222220000000000000007F8777000000077000000888644444440000000
    2222222222200000000000007787777000000000000007887444444440000000
    2222222222220000000000078887777700000000000078874444444000000000
    022222222222200000000007FF88887700000000000788744444444000000000
    022222222222220000000007FF88777700000000007887444444440000000000
    0022222222222220000000078888777770000000788874444444400000000000
    0002222222222222000000077777777730000007888744444446400000000000
    0000222222222222060000000000000000000788887644444464000000000000
    0000022222222220666660000000000000778888776664444644000000000000
    0000002222222226666666688777777778888877666666446400000000000000
    0000000222222206666666688888888888887766666666444000000000000000
    0000000022222266666666688888888877766666666666600000000000000000
    0000000002222666666666466777776666666666666666400000000000000000
    0000000000020666666666466666666666666666666640000000000000000000
    0000000000000666666666666666666666666666666400000000000000000000
    0000000000000066666666666666666666666666640000000000000000000000
    0000000000000000666664666666666666666660000000000000000000000000
    0000000000000000000664666666666666664000000000000000000000000000
    000000000000000000000000464444440000000000000000000000000000FFFF
    FE00007FFFFFFFFFF000000FFFFFFFFFC0000003FFFFFFFF00000000FFFFFFFC
    000000003FFFFFF8000000001FFFFFF00000000007FFFFC00000000003FFFF80
    0000000001FFFF000000000000FFFE00001FF000007FFE0000FFFF00003FFC00
    03FFFFC0003FF8000FC007F0001FF8001F0001F8000FF0003C00007C000FE000
    F800003E0007E000F000001F0007C001E000000F8003C003C0000007C003C007
    C0000007E001800780000003E001800F80000003F001800F00000001F000001F
    00000001F800001F00000001F800001E00000001F800003E00000000F800003E
    00000000FC00003E00000000FC00003E00000000FC00003E00000001FC00003F
    00000001FC00003F00000001FC00003F00000001FC00003F80000003FC00003F
    80000003F800001FC0000003F800001FC0000007F800001FE0000003F800800F
    F0000001F000800FF8000001F0018007FC000001E001C007FF000001E001C003
    FF800303C003C001FFF81F878003E001FFE007FF0007E000FFE007FE0007F000
    7FE007FC000FF8001FE007F8000FF8000FE007F0001FFC0003E007C0003FFE00
    00FFFF00007FFF00001FF800007FFF800000000000FFFFC00000000001FFFFE0
    0000000007FFFFF0000000000FFFFFF8000000001FFFFFFE000000007FFFFFFF
    00000000FFFFFFFFC0000003FFFFFFFFF800001FFFFFFFFFFF0000FFFFFF}
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnClick = FormClick
  OnClose = FormClose
  OnCreate = FormCreate
  OnDblClick = HideToTrayExecute
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 120
  TextHeight = 13
  object LBusy: TLabel
    Left = 65
    Top = 93
    Width = 51
    Height = 24
    Caption = 'LBusy'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object LIdle: TLabel
    Left = 65
    Top = 125
    Width = 40
    Height = 24
    Caption = 'LIdle'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 8
    Top = 101
    Width = 29
    Height = 13
    Caption = 'Busy: '
  end
  object Label2: TLabel
    Left = 8
    Top = 133
    Width = 23
    Height = 13
    Caption = 'Idle: '
  end
  object LITO: TLabel
    Left = 120
    Top = 165
    Width = 32
    Height = 20
    Caption = '        '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object LPresent: TLabel
    Left = 65
    Top = 29
    Width = 74
    Height = 24
    Caption = 'LPresent'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object LAbsent: TLabel
    Left = 65
    Top = 61
    Width = 69
    Height = 24
    Caption = 'LAbsent'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 8
    Top = 37
    Width = 42
    Height = 13
    Caption = 'Present: '
  end
  object Label6: TLabel
    Left = 8
    Top = 69
    Width = 39
    Height = 13
    Caption = 'Absent: '
  end
  object Label3: TLabel
    Left = 179
    Top = 7
    Width = 20
    Height = 13
    Caption = 'Last'
  end
  object Label4: TLabel
    Left = 65
    Top = 7
    Width = 24
    Height = 13
    Caption = 'Total'
  end
  object LBusyL: TLabel
    Left = 179
    Top = 93
    Width = 61
    Height = 24
    Caption = 'LBusyL'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object LIdleL: TLabel
    Left = 179
    Top = 125
    Width = 50
    Height = 24
    Caption = 'LIdleL'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lPresentL: TLabel
    Left = 179
    Top = 29
    Width = 84
    Height = 24
    Caption = 'LPresentL'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object LAbsentL: TLabel
    Left = 179
    Top = 61
    Width = 79
    Height = 24
    Caption = 'LAbsentL'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object EITO: TEdit
    Left = 8
    Top = 7
    Width = 49
    Height = 21
    TabOrder = 0
    Text = '300'
    Visible = False
    OnChange = EITOChange
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 186
    Width = 302
    Height = 19
    Panels = <
      item
        Width = 50
      end
      item
        Text = 'Present time'
        Width = 59
      end
      item
        Text = 'Absent time'
        Width = 59
      end
      item
        Text = 'duzun.icasa.md/programming/pascal/delphi.html'
        Width = 50
      end>
    ParentShowHint = False
    ShowHint = True
    OnMouseDown = StatusBar1MouseDown
    OnMouseMove = StatusBar1MouseMove
  end
  object Timer1: TTimer
    Interval = 1
    OnTimer = Timer1Timer
    Left = 216
    Top = 65533
  end
  object ActionList1: TActionList
    Left = 248
    Top = 65533
    object ShowInfo: TAction
      Caption = 'ShowInfo'
      OnExecute = ShowInfoExecute
    end
    object OnStateChange: TAction
      Caption = 'OnStateChange'
      OnExecute = OnStateChangeExecute
    end
    object OnPresentChange: TAction
      Caption = 'OnPresentChange'
      OnExecute = OnPresentChangeExecute
    end
    object OnAbsentChange: TAction
      Caption = 'OnAbsentChange'
      OnExecute = OnAbsentChangeExecute
    end
    object OnBusyChange: TAction
      Caption = 'OnBusyChange'
      OnExecute = OnBusyChangeExecute
    end
    object OnIdleChange: TAction
      Caption = 'OnIdleChange'
      OnExecute = OnIdleChangeExecute
    end
    object SetIdle: TAction
      Caption = 'Set Idle'
      OnExecute = SetIdleExecute
    end
    object HideToTray: TAction
      Caption = 'HideToTray'
      OnExecute = HideToTrayExecute
    end
    object ShowFromTray: TAction
      Caption = 'ShowFromTray'
      OnExecute = ShowFromTrayExecute
    end
  end
end
