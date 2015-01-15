object Form1: TForm1
  Left = 566
  Top = 100
  Width = 291
  Height = 216
  Caption = 'User Activity Counter'
  Color = clRed
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object LAt: TLabel
    Left = 96
    Top = 8
    Width = 27
    Height = 24
    Caption = 'LAt'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object LIt: TLabel
    Left = 96
    Top = 40
    Width = 18
    Height = 24
    Caption = 'LIt'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 78
    Height = 13
    Caption = 'Active (current): '
  end
  object Label2: TLabel
    Left = 8
    Top = 48
    Width = 86
    Height = 13
    Caption = 'Inactive (current): '
  end
  object LITO: TLabel
    Left = 120
    Top = 144
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
  object LAt_: TLabel
    Left = 96
    Top = 72
    Width = 27
    Height = 24
    Caption = 'LAt'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object LIt_: TLabel
    Left = 96
    Top = 104
    Width = 18
    Height = 24
    Caption = 'LIt'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 8
    Top = 80
    Width = 36
    Height = 13
    Caption = 'Active: '
  end
  object Label6: TLabel
    Left = 8
    Top = 112
    Width = 44
    Height = 13
    Caption = 'Inactive: '
  end
  object Label7: TLabel
    Left = 176
    Top = 136
    Width = 25
    Height = 16
    Caption = 'sec.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 208
    Top = 32
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label4: TLabel
    Left = 208
    Top = 96
    Width = 32
    Height = 13
    Caption = 'Label4'
  end
  object Label8: TLabel
    Left = 8
    Top = 144
    Width = 64
    Height = 13
    Caption = 'Idle Timeout: '
  end
  object EITO: TEdit
    Left = 96
    Top = 136
    Width = 73
    Height = 21
    TabOrder = 0
    Text = '0'
    OnChange = EITOChange
  end
  object Timer1: TTimer
    Interval = 1
    OnTimer = Timer1Timer
    Left = 248
  end
  object ActionList1: TActionList
    Left = 216
    object ShowInfo: TAction
      Caption = 'ShowInfo'
      OnExecute = ShowInfoExecute
    end
    object StateChange_: TAction
      Caption = 'StateChange_'
    end
  end
end
