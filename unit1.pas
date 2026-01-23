unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, uLzFlowmotion;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    ColorDialog1: TColorDialog;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    rbanimspeed: TRadioButton;
    rbpagesize: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    SpinEdit1: TSpinEdit;
    Timer1: TTimer;
    procedure Button10Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FFlow: TFlowmotion;
  public
    procedure SelectedImageDblClick(Sender: TObject; ImageItem:
        TImageItem; Index: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }



procedure TForm1.Button10Click(Sender: TObject);
begin
  if Colordialog1.Execute then FFlow.HotTrackColor := Colordialog1.color;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FFlow.Clear(True);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  FFlow.ImageEntryStyle := iesRandom;
  FFlow.EntryPoint := TPoint.Create(0, 0);
  FFlow.AddImage(Extractfilepath(Application.ExeName) + inttostr(random(11)+1) + '.jpg');
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  FFlow.ImageEntryStyle := iesFromPoint;
  FFlow.EntryPoint := TPoint.Create(Panel3.Left, Panel3.Top);
  FFlow.AddImage(Extractfilepath(Application.ExeName) + inttostr(random(11)+1) + '.jpg');
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  if Opendialog1.Execute then  begin
    FFlow.ImageEntryStyle := iesFromPoint;
    FFlow.EntryPoint := TPoint.Create(Panel3.Left, Panel3.Top);
    FFlow.AddImageAsync(Opendialog1.FileName, 'Caption', 'Path');
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  i: Integer;
  Files, Caps, Paths, Hints: TStringList;
begin
  FFlow.ImageEntryStyle := iesRandom;

  Files := TStringList.Create;
  Caps  := TStringList.Create;
  Paths := TStringList.Create;
  Hints := TStringList.Create;

  try
    for i := 1 to 12 do
    begin
      Files.Add(ExtractFilePath(Application.ExeName) + IntToStr(i) + '.jpg');
      Caps.Add('Image ' + IntToStr(i));
      Paths.Add('Folder ' + IntToStr(i));
      Hints.Add('Hint for image ' + IntToStr(i));
    end;

    FFlow.AddImagesAsync(Files, Caps, Paths, Hints);

  finally
    Files.Free;
    Caps.Free;
    Paths.Free;
    Hints.Free;
  end;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  if Opendialog1.Execute then FFlow.SetBackgroundpicture(Opendialog1.FileName);
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  FFlow.Clear(true, true, Panel2.BoundsRect, Panel2.BoundsRect, iesFromPoint);
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  if Colordialog1.Execute then FFlow.glowcolor := Colordialog1.color;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  FFlow.ShowCaptions := CheckBox1.Checked;
end;

procedure TForm1.CheckBox2Change(Sender: TObject);
begin
  FFlow.CaptionOnHoverOnly := Checkbox2.Checked;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(FFlow) then begin
    FFLow.Clear(true);
    FFlow.Free;
  end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if Assigned(FFlow) then FFLow.MaxZoomsize := trunc(Width /2);
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  if RbAnimspeed.Checked then FFlow.Animationspeed := Spinedit1.Value
   else if rbPagesize.Checked then FFLow.Pagesize := Spinedit1.Value;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  i: Integer;
  Files, Caps, Paths, Hints: TStringList;
begin
  Timer1.Enabled := False;
  Caption := 'Lazarus Flowmotion Test';

  // 1. Create the component dynamically
  FFlow := TFlowmotion.Create(Self);
  FFlow.Parent := Self;
  FFlow.Align := alClient;
  FFlow.DoubleBuffered := True;
  FFlow.ShowCaptions := True;
  FFLow.CaptionOnHoverOnly := True;
  FFLow.Animationspeed := 3;
  FFLow.MaxZoomsize := trunc(Width /2);
  FFlow.PageSize := 200;
  FFlow.Spacing := 10;

  FFlow.OnSelectedImageDblClick := @SelectedImageDblClick;

  FFLow.SetBackgroundpicture(Extractfilepath(Application.ExeName) + 'back.jpg');
  FFlow.Show;

  // 2. Prepare lists for loading
  Files := TStringList.Create;
  Caps  := TStringList.Create;
  Paths := TStringList.Create;
  Hints := TStringList.Create;

  try
    for i := 1 to 12 do
    begin
      Files.Add(ExtractFilePath(Application.ExeName) + IntToStr(i) + '.jpg');
      Caps.Add('Image ' + IntToStr(i));
      Paths.Add('Folder ' + IntToStr(i));
      Hints.Add('Hint for image ' + IntToStr(i));
    end;

    // 3. Load them
    FFlow.AddImages(Files, Caps, Paths, Hints);

  finally
    Files.Free;
    Caps.Free;
    Paths.Free;
    Hints.Free;
  end;
end;


procedure TForm1.SelectedImageDblClick(Sender: TObject;
    ImageItem: TImageItem; Index: Integer);
begin
  //selected dblclicked
  ShowMessage('Dblclicked selected');
end;


end.
