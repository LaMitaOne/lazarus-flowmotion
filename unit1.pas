unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  // This assumes you saved the Flowmotion component as "ulzflowmotion.pas"
  ulzflowmotion;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FFlow: TFlowmotion;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
  Files, Caps, Paths, Hints: TStringList;
begin
  Caption := 'Lazarus Flowmotion Test';

  // 1. Create the component dynamically
  FFlow := TFlowmotion.Create(Self);
  FFlow.Parent := Self;
  FFlow.Align := alClient;
  FFlow.DoubleBuffered := True;
  FFlow.ShowCaptions := True;
  FFLow.MaxZoomsize := trunc(Width /2);
  FFlow.PageSize := 50;
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

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(FFlow) then
    FFlow.Free;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if Assigned(FFlow) then FFLow.MaxZoomsize := trunc(Width /2);
end;


finalization
  // Cleanup dummy files if you want (optional)
  // for i := 1 to 24 do DeleteFile(ExtractFilePath(Application.ExeName) + IntToStr(i) + '.bmp');

end.
