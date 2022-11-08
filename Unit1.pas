unit Unit1;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.UIConsts,
  System.JSON,
  System.Math,
  System.Math.Vectors,

  FMX.Forms,
  FMX.Graphics,
  FMX.Memo,
  FMX.Memo.Types,
  FMX.Objects,
  FMX.ExtCtrls,
  FMX.Dialogs,
  FMX.ScrollBox,
  FMX.Edit,
  FMX.Colors,
  FMX.StdCtrls,
  FMX.ListBox,
  FMX.EditBox,
  FMX.SpinBox,
  FMX.Types,
  FMX.Controls,
  FMX.Controls.Presentation,

  game_functions;

type TTileType = (
  TTT_Emptytile = 0,
  TTT_Floor,
  TTT_Wall,
  TTT_CornerNE,
  TTT_CornerSE,
  TTT_CornerSW,
  TTT_CornerNW,
  TTT_Window,
  TTT_Airlock
  );
TTileTypeHelper = record helper for TTileType
  function ToChar : char;
  function ToString : string;
  class function IndexToChar(index:integer): char; static;
  class function CharToTile(input: char): TTileType; static;
end;

type TTile = class
  x,y: integer;
  hull: TTileType;
  component: string;
  walkable: boolean;
end;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    SpinBox_room_size_x: TSpinBox;
    SpinBox_room_size_y: TSpinBox;
    ColorPicker1: TColorPicker;
    Button1: TButton;
    GroupBox2: TGroupBox;
    ComboBox_tiles: TComboBox;
    CheckBox_walkable: TCheckBox;
    Label_last_tile: TLabel;
    ComboBox2: TComboBox;
    Edit_tile_color: TEdit;
    Button_save: TButton;
    Button_load: TButton;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    Rectangle1: TRectangle;
    PlotGrid1: TPlotGrid;
    Label1: TLabel;
    Memo_shipcode: TMemo;
    Image_ship_tiles: TImage;
    procedure ColorPicker1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ComboBox_tilesChange(Sender: TObject);
    procedure PlotGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FormCreate(Sender: TObject);
    procedure PlotGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure PlotGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Button_saveClick(Sender: TObject);
    procedure Button_loadClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Init_blueprint;
    procedure Paint_tile(tile_point: TPoint);
    function Is_same_tiletype_as_selected(tile_point: TPoint): boolean;
    function Mouse_to_tile_point(X,Y:single): TPoint;
    procedure Redraw_ship_tiles;
  end;

var
  Form1: TForm1;
  fDown: TPointF;
  painting_tiles: boolean;
  tiles: array of array of char;
  tilecount_x, tilecount_y: integer;
  previous_shipCode: string;

implementation

{$R *.fmx}

procedure TForm1.Redraw_ship_tiles;
var buffer: TBitmap;
    size: integer;

  procedure Draw_tile(tile_point: TPoint; tile: TTileType);

    procedure Draw_rect;
    var tile_color: TAlphaColor;
    begin
      var rect:= TRect.Create(
        tile_point.X*size,
        tile_point.Y*size,
        (tile_point.X+1)*size,
        (tile_point.Y+1)*size);

      case tile of
        TTT_Floor:    tile_color:= TAlphaColorRec.LightGray;
        TTT_Window:   tile_color:= TAlphaColorRec.Cyan;
        TTT_Airlock:  tile_color:= TAlphaColorRec.Gray;
      else tile_color:= ColorPicker1.Color;
      end;

      var FloorBrush:= TBrush.Create(TBrushKind.Solid, tile_color);
      try
        buffer.Canvas.DrawRect(rect,1);
        buffer.Canvas.FillRect(rect,1,FloorBrush);
      finally
        FloorBrush.Free;
      end;
    end;

    procedure Draw_triangle(rotation:integer);
    var points: array[0..7] of TPoint;
    begin
      points[0]:= TPoint.Create( tile_point.X*size,   tile_point.Y*size);
      points[1]:= TPoint.Create((tile_point.X+1)*size,tile_point.Y*size);
      points[2]:= TPoint.Create((tile_point.X+1)*size,(tile_point.Y+1)*size);
      points[3]:= TPoint.Create( tile_point.X*size,   (tile_point.Y+1)*size);
      points[4]:= points[0];
      points[5]:= points[1];
      points[6]:= points[2];
      points[7]:= points[3];

      var poly: TPolygon;
      setLength(poly,3);
      for var i := rotation to rotation+2 do
        poly[i-rotation]:= points[i];

      buffer.Canvas.DrawPolygon(poly,1);
      buffer.Canvas.FillPolygon(poly,1);
    end;

  begin
    case tile of
      TTT_Floor,TTT_Wall,TTT_Window,TTT_Airlock: Draw_rect;
      TTT_CornerNE: Draw_triangle(0);
      TTT_CornerSE: Draw_triangle(1);
      TTT_CornerSW: Draw_triangle(2);
      TTT_CornerNW: Draw_triangle(3);
    end;
  end;

  procedure Draw_tiles_to_buffer;
  begin
    size:= round(form1.PlotGrid1.Frequency);

    buffer.Canvas.BeginScene;
    buffer.Canvas.Clear(TAlphaColorRec.Null);

    var FillBrush:= TBrush.Create(TBrushKind.Solid, ColorPicker1.Color);
    buffer.Canvas.Fill:= FillBrush;
    buffer.Canvas.Stroke.Thickness:= 3;

    try
      for var x:= 0 to tilecount_x-1 do
      for var y:= 0 to tilecount_y-1 do
        begin
          var tile:= TTileType.CharToTile( tiles[x,y] );
          if tile=TTileType.TTT_Emptytile then continue;
          Draw_tile(TPoint.Create(x,y), tile);
        end;

    finally
      buffer.Canvas.EndScene;
      FillBrush.Free;
    end;
  end;

begin
  buffer:= TBitmap.Create(round(PlotGrid1.Width),round(PlotGrid1.Height));
  try
    Draw_tiles_to_buffer;
    Image_ship_tiles.Bitmap.Assign(buffer);
  finally
    buffer.Free;
  end;
end;

function Recalculate_shipCode: string;
begin
  result:= '';
  for var y:= 0 to tilecount_y-1 do
  for var x:= 0 to tilecount_x-1 do
    result:= result + tiles[x,y];
end;

function Update_shipCode: string;
begin
  result:= Recalculate_shipCode;
  form1.Memo_shipcode.Text:= result;
end;

function TForm1.Is_same_tiletype_as_selected(tile_point: TPoint): boolean;
begin
  result:= false;
  if tile_point.X>tilecount_x then exit;
  if tile_point.Y>tilecount_y then exit;

  var existing_tile:= tiles[tile_point.x,tile_point.y];
  result:= existing_tile=ComboBox_tiles.itemIndex.ToString;
end;

function TForm1.Mouse_to_tile_point(X,Y:single): TPoint;
begin
  var freq:= round(PlotGrid1.Frequency);
  var XX:= round(X) div freq;
  var YY:= round(Y) div freq;
  result:= TPoint.Create(XX,YY);
end;

procedure TForm1.PlotGrid1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  painting_tiles:= button=TMouseButton.mbLeft;
end;

procedure TForm1.Paint_tile(tile_point: TPoint);
begin
  if not painting_tiles then exit;

  if tile_point.X>tilecount_x then exit;
  if tile_point.Y>tilecount_y then exit;

  if Is_same_tiletype_as_selected(tile_point) then
    begin

    end
  else
    begin
      var selected_tile_type:= TTileType.IndexToChar(ComboBox_tiles.ItemIndex);
      tiles[tile_point.x,tile_point.y]:= selected_tile_type;
    end;

  var newCode:= Update_shipCode;
  var no_changes:= previous_shipCode = newCode;
  if no_changes then exit;
  previous_shipCode:= newCode;

  Redraw_ship_tiles;
end;

procedure TForm1.PlotGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if not painting_tiles then exit;

  var tile_point:= Mouse_to_tile_point(x,y);
  Paint_tile(tile_point);

  Label_last_tile.Text:=
    'Coords: X='+tile_point.x.ToString+', Y='+tile_point.y.ToString;
end;

procedure TForm1.PlotGrid1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if button<>TMouseButton.mbLeft then exit;
  var tile_point:= Mouse_to_tile_point(x,y);
  Paint_tile(tile_point);
  painting_tiles:= false;
end;

procedure TForm1.Init_blueprint;
begin
  tilecount_x:= round(SpinBox_room_size_x.Value);
  tilecount_y:= round(SpinBox_room_size_y.Value);
  SetLength(tiles,tilecount_x,tilecount_y);

  PlotGrid1.Frequency:= PlotGrid1.Height / tilecount_y;
  PlotGrid1.Marks:= 5;

  for var x:= 0 to tilecount_x-1 do
  for var y:= 0 to tilecount_y-1 do
    tiles[x,y]:= TTileType.TTT_Emptytile.ToChar;

  Update_shipCode;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  painting_tiles:= false;
  Init_blueprint;

  ComboBox_tiles.Clear;
  for var tile:= low(TTileType) to high(TTileType) do
    ComboBox_tiles.Items.Add( tile.ToString );
  ComboBox_tiles.ItemIndex:= 2;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  (*
  Button1.Enabled:= false;
  SpinBox_room_size_x.Enabled:= false;
  SpinBox_room_size_y.Enabled:= false;
  *)

  Init_blueprint;
end;

procedure TForm1.Button_loadClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Memo_shipcode.Text:= FileToString(OpenDialog1.FileName);
end;

procedure TForm1.Button_saveClick(Sender: TObject);
begin
  var filename:= 'New ship '+
    SpinBox_room_size_x.Value.ToString+'x'+SpinBox_room_size_y.Value.ToString;
  SaveDialog1.FileName:= filename;

  if SaveDialog1.Execute then
    StringToFile(Memo_shipcode.Text, SaveDialog1.FileName);
end;

procedure TForm1.ColorPicker1Click(Sender: TObject);
begin
  var color := ColorPicker1.color;
  Edit_tile_color.Text := AlphaColorToString(color);
end;

procedure TForm1.ComboBox_tilesChange(Sender: TObject);
begin
end;

{ TTileTypeHelper }

class function TTileTypeHelper.CharToTile(input: char): TTileType;
begin
  var str:= string(input);
  result:= TTileType(str.ToInteger);
end;

class function TTileTypeHelper.IndexToChar(index:integer): char;
begin
  var str:= inttohex(index,1);
  result:= str[1];
end;

function TTileTypeHelper.ToChar: char;
begin
  var hex:= ord(self);
  var str:= hex.ToString;
  result:= str[1];
end;

function TTileTypeHelper.ToString: string;
begin
  case self of
    TTT_Emptytile:  result:= 'Emptytile';
    TTT_Floor:      result:= 'Floor';
    TTT_Wall:       result:= 'Wall';
    TTT_CornerNE:   result:= 'CornerNE';
    TTT_CornerSE:   result:= 'CornerSE';
    TTT_CornerSW:   result:= 'CornerSW';
    TTT_CornerNW:   result:= 'CornerNW';
    TTT_Window:     result:= 'Window';
    TTT_Airlock:    result:= 'Airlock';
  end;
end;

end.
