unit isMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TASources, mrumanager, Forms,
  Controls, Graphics, Dialogs, Menus, ActnList, ExtCtrls, ComCtrls, ExtDlgs;

type

  TScanDirection = (sdHoriz, sdVert);

  { TMainForm }

  TMainForm = class(TForm)
    AcFileOpen: TAction;
    AcFileQuit: TAction;
    AcRed: TAction;
    AcGreen: TAction;
    AcBlue: TAction;
    AcHue: TAction;
    AcLightness: TAction;
    AcSaturation: TAction;
    AcCrosshair: TAction;
    ActionList1: TActionList;
    Bevel1: TBevel;
    ChartVert: TChart;
    ChartVertLineSeries: TLineSeries;
    ChartHoriz: TChart;
    ChartHorizLineSeries: TLineSeries;
    Image: TImage;
    ImageList: TImageList;
    LabelChartSource: TListChartSource;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    muOpen: TMenuItem;
    muRecentlyUsed: TMenuItem;
    MenuItem4: TMenuItem;
    mnuQiot: TMenuItem;
    MRUMenuManager: TMRUMenuManager;
    OpenPictureDialog: TOpenPictureDialog;
    PaintBox1: TPaintBox;
    RecentFilesPopup: TPopupMenu;
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    TrackBarHoriz: TTrackBar;
    TrackbarVert: TTrackBar;
    procedure AcCrosshairExecute(Sender: TObject);
    procedure AcFileOpenExecute(Sender: TObject);
    procedure AcFileQuitExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ImagePaint(Sender: TObject);
    procedure ImageResize(Sender: TObject);
    procedure MRUMenuManagerRecentFile(Sender: TObject; const AFileName: String);
    procedure PaintBox1Paint(Sender: TObject);
    procedure ColorComponentChanged(Sender: TObject);
    procedure StatusBarDrawPanel(AStatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure TrackBarHorizChange(Sender: TObject);
    procedure TrackbarVertChange(Sender: TObject);
  private
    { private declarations }
    FMouseColor: TColor;
    procedure CalcScan(APosition: Integer; ADirection: TScanDirection);
    procedure LoadFile(const AFileName: String);
    function XToBmp(X, WBmp, HBmp: Integer): Integer;
    function YToBmp(Y, WBmp, HBmp: Integer): Integer;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LCLIntf, GraphUtil, fpimage, intfgraphics;


{ TMainForm }

procedure TMainForm.AcFileOpenExecute(Sender: TObject);
begin
  with OpenPictureDialog do begin
    if FileName <> '' then
      InitialDir := ExtractfileDir(FileName);
    if Execute then
      LoadFile(FileName);
  end;
end;

procedure TMainForm.AcCrosshairExecute(Sender: TObject);
begin
  Image.Invalidate;
end;

procedure TMainForm.AcFileQuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MRUMenuManager.IniFileName := ChangeFileExt(Application.ExeName, '.ini');
  FMouseColor := clNone;
  Statusbar.Panels[0].Width := Statusbar.Height;
//  paintbox1.Parent := Statusbar;
//  paintbox1.Top := 0;
end;

procedure TMainForm.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
begin
  LoadFile(FileNames[0]);
end;

procedure TMainForm.CalcScan(APosition:Integer; ADirection:TScanDirection);

  function GetColorComponent(AColor: TFPColor): Byte;
  var
    H, L, S: Byte;
  begin
    Result := 0;
    if AcRed.Checked then
      Result := AColor.Red shr 8
    else
    if AcGreen.Checked then
      Result := AColor.Green shr 8
    else
    if AcBlue.Checked then
      Result := AColor.Blue shr 8
    else begin
      ColorToHLS(FPColorToTColor(AColor), H, L, S);
      if AcHue.Checked  then
        Result := H
      else
      if AcSaturation.Checked then
        Result := S
      else
      if AcLightness.checked then
        Result := L;
    end;
  end;

var
  fpclr: TFPColor;
  i, x, y: Integer;
  p: TPicture;
  val: integer;
  intfimg: TLazIntfImage;
begin
  intfimg := TLazIntfImage.Create(0, 0);
  try
    intfimg.LoadFromBitmap(Image.Picture.Bitmap.Handle, 0);
    case ADirection of
      sdHoriz:
        begin
          y := YToBmp(APosition, intfImg.Width, intfImg.Height);
          ChartHorizLineSeries.BeginUpdate;
          try
            ChartHorizLineSeries.Clear;
            for i := 0 to Image.Width - 1 do begin
              x := XToBmp(i, intfimg.Width, intfImg.Height);
              if (y < 0) or (y >= intfImg.Height) or (x < 0) or (x >= intfImg.Width) then
                fpclr := colBlack
              else
                fpclr := intfimg.Colors[x, y];
              val := GetColorComponent(fpclr);
              ChartHorizLineseries.AddXY(x, val);
            end;
          finally
            ChartHorizLineSeries.EndUpdate;
          end;
        end;
      sdVert:
        begin
          x := XToBmp(APosition, intfImg.Width, intfImg.Height);
          ChartVertLineSeries.BeginUpdate;
          try
            ChartVertLineSeries.Clear;
            for i := 0 to Image.Height - 1 do begin
              y := YToBmp(i, intfImg.Width, intfImg.Height);
              if (x < 0) or (x >= intfImg.Width) or (y < 0) or (y >= intfImg.Height) then
                fpclr := colBlack
              else
                fpclr := intfImg.Colors[x, y];
              val := GetColorComponent(fpclr);
              ChartVertLineSeries.AddXY(y, val);
            end;
          finally
            ChartVertLineSeries.EndUpdate;
          end;
        end;
    end;
  finally
    intfimg.Free;
  end;
end;

procedure TMainForm.ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  c: TColor;
  h,l,s: byte;
  w: Integer;
begin
  if (Image.Picture.Bitmap <> nil)
    and (Image.Picture.Width > 0)
    and (Image.Picture.Height > 0)
  then begin
    X := XToBmp(X, Image.Picture.Width, Image.Picture.Height);
    Y := YToBmp(Y, Image.Picture.Width, Image.Picture.Height);
    if (X >= 0) and (X < Image.Picture.Width) and (Y >= 0) and (Y < Image.Picture.Height) then
    begin
      c := Image.Picture.Bitmap.Canvas.Pixels[X, Y];
      ColorToHLS(c, h,l,s);
      Statusbar.Panels[1].Text := Format('R=%d G=%d B=%d', [Red(c), Green(c), Blue(c)]);
      Statusbar.Panels[2].Text := Format('H=%d L=%d S=%d', [h,l,s]);
      FMouseColor := c;
//      Paintbox1.Show;
    end else
    begin
      Statusbar.Panels[0].Text := '';
      Statusbar.Panels[1].Text := '';
      FMouseColor := clNone;
    end;
  end;
  Statusbar.Invalidate;
  //Paintbox1.Invalidate;
end;

procedure TMainForm.ImagePaint(Sender: TObject);
begin
  inherited;
  if (Image.Picture = nil) or (Image.Picture.Bitmap = nil) then
    exit;
  if not AcCrosshair.Checked then
    exit;
  Image.Canvas.Pen.Color := clWhite;
  Image.Canvas.Pen.Mode := pmXOR;
  Image.Canvas.Line(0, TrackbarHoriz.Position, Image.Width, TrackbarHoriz.Position);
  Image.Canvas.Line(TrackbarVert.Position, 0, TrackbarVert.Position, Image.Height);
end;

procedure TMainForm.ImageResize(Sender: TObject);
begin
  TrackbarHoriz.Max := Image.Height - 1;
  TrackbarVert.Max := Image.Width - 1;
end;

procedure TMainForm.MRUMenuManagerRecentFile(Sender: TObject;
  const AFileName: String);
begin
  LoadFile(AFileName);
end;

procedure TMainForm.PaintBox1Paint(Sender: TObject);
begin
  with Paintbox1 do begin
    if FMouseColor <> clNone then
      Canvas.Brush.Color := FMouseColor
    else
      Canvas.Brush.Color := Parent.Color;
    Canvas.FillRect(0, 0, Width-1, Height-1);
  end;
end;

procedure TMainForm.LoadFile(const AFileName: String);
var
  S: TStream;
begin
  S := TFileStream.Create(AFileName, fmOpenRead + fmShareDenyNone);
  try
    Image.Picture.LoadFromStream(S);
    TrackbarVert.Max := Image.Width - 1;
    TrackBarHoriz.Max := Image.Height - 1;
    TrackbarVert.Position := Image.Width div 2;
    TrackbarHoriz.Position := Image.Height div 2;
    CalcScan(TrackBarHoriz.Position, sdHoriz);
    CalcScan(TrackbarVert.Position, sdVert);
    Caption := Format('ImageScan - "%s"', [AFileName]);
    MRUMenuManager.AddToRecent(AFileName);
  finally
    S.Free;
  end;
end;

procedure TMainForm.ColorComponentChanged(Sender: TObject);
begin
  CalcScan(TrackBarHoriz.Position, sdHoriz);
  CalcScan(TrackbarVert.Position, sdVert);
  Image.Invalidate;
end;

procedure TMainForm.StatusBarDrawPanel(AStatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  R: TRect;
begin
  if FMouseColor = clNone then
    AStatusbar.Canvas.Brush.Color := AStatusbar.Color
  else
    AStatusbar.Canvas.Brush.Color := FMouseColor;
  R := Rect;
  InflateRect(R, -2, -2);
  R.Right := R.Left + R.Bottom - R.Top;
  AStatusbar.Canvas.FillRect(R);
end;

procedure TMainForm.TrackbarVertChange(Sender: TObject);
begin
  CalcScan(TrackbarVert.Position, sdVert);
  Image.Invalidate;
end;

procedure TMainForm.TrackBarHorizChange(Sender: TObject);
begin
  CalcScan(TrackBarHoriz.Position, sdHoriz);
  Image.Invalidate;
end;

function TMainForm.XToBmp(X, WBmp, HBmp: Integer): Integer;
var
  w, a: Integer;
begin
  if HBmp * Image.Width > WBmp * Image.Height then begin
    w := round(Image.Height * WBmp / HBmp);
    a := (Image.Width - w) div 2;
    X := X - a;
  end;
  Result := Round(X * HBmp / Image.Height);
end;

function TMainForm.YToBmp(Y, WBmp, HBmp: Integer): Integer;
var
  h, a: Integer;
begin
  if WBmp * Image.Height > HBmp * Image.Width then begin
    h := round(Image.Width * HBmp / WBmp);
    a := (Image.Height - h) div 2;
    Y := Y - a;
  end;
  Result := round(Y * WBmp / Image.Width)
end;


end.

