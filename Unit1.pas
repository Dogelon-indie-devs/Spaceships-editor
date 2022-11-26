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
  FMX.Layouts,
  FMX.Controls,
  FMX.Controls.Presentation,

  game_functions,
  ship_graphics;

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

type
  TForm1 = class(TForm)
    Panel_controls: TPanel;
    GroupBox1: TGroupBox;
    SpinBox_room_size_x: TSpinBox;
    SpinBox_room_size_y: TSpinBox;
    Button_generate: TButton;
    GroupBox2: TGroupBox;
    ComboBox_tiles: TComboBox;
    Label_last_tile: TLabel;
    ComboBox2: TComboBox;
    Edit_tile_color: TEdit;
    Button_save: TButton;
    Button_load: TButton;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    Rectangle_background: TRectangle;
    Memo_shipcode: TMemo;
    Image_ship_tiles: TImage;
    Edit_author: TEdit;
    GroupBox3: TGroupBox;
    Edit_ship_class_name: TEdit;
    ColorPanel1: TColorPanel;
    CheckBox_derelict: TCheckBox;
    CheckBox_outside_view: TCheckBox;
    Button_optimize: TButton;
    SaveDialog2: TSaveDialog;
    Layout1: TLayout;
    Image_grid: TImage;
    Button_test: TButton;
    procedure Button_generateClick(Sender: TObject);
    procedure ComboBox_tilesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button_saveClick(Sender: TObject);
    procedure Button_loadClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ColorPanel1Change(Sender: TObject);
    procedure CheckBox_derelictChange(Sender: TObject);
    procedure CheckBox_outside_viewChange(Sender: TObject);
    procedure Button_optimizeClick(Sender: TObject);
    procedure Image_gridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Image_gridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure Image_gridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Button_testClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Clear_tiles;
    procedure Redraw_grid;
    procedure Paint_tile(tile_point: TPoint);
    function Is_same_tiletype_as_selected(tile_point: TPoint): boolean;
    function Mouse_to_tile_point(X,Y:single): TPoint;
    procedure Redraw_ship_tiles;
    procedure Recalculate_sizing;
    procedure Import_layout_as_json(json:string);
    procedure Optimize_grid_size;
  end;

var
  Form1: TForm1;
  fDown: TPointF;
  painting_tiles,outside_view,derelict: boolean;
  tiles: array of array of char;
  tilecount_x, tilecount_y: integer;
  previous_shipCode: string;
  BlockSize: integer;

implementation

{$R *.fmx}

function Simulate_random_damage: integer;
const
  damage_stages = 21;
  minimum_damage= 1;
  step = 5;
begin
  // returns damage range 5 - 100
  result:= (random(damage_stages-minimum_damage)+minimum_damage) * step;
end;

function Shade_of_black_according_to_damage(damage:integer): TAlphaColor;
begin
  var inputColor:= TAlphaColorRec.Black;
  // inverse to damage, doubled to use range 10 - 200 alpha value
  TAlphaColorRec(inputColor).A:= 256 - (damage*2);
  result:= inputColor;
end;

function Randomize_color_alpha(inputColor: TAlphaColor): TAlphaColor;
begin
  TAlphaColorRec(inputColor).A:= random(256);
  result:= inputColor;
end;

procedure Move_design_left(steps:integer);
begin
  for var y:= 0 to tilecount_y-1 do
  for var x:= steps to tilecount_x-1 do
    begin
      tiles[x-steps,y]:= tiles[x,y];
      tiles[x,y]:= '0';
    end;
end;

procedure Move_design_up(steps:integer);
begin
  for var x:= 0 to tilecount_x-1 do
  for var y:= steps to tilecount_y-1 do
    begin
      tiles[x,y-steps]:= tiles[x,y];
      tiles[x,y]:= '0';
    end;
end;

procedure TForm1.Optimize_grid_size;
var empty_lines: integer;

  procedure Check_up;
  begin
    empty_lines:= 0;
    for var y:= 0 to tilecount_y-1 do
      begin
        var line:= '';
        for var x:= 0 to tilecount_x-1 do
          begin
            var tile:= tiles[x,y];
            if tile='0' then
              continue
            else
              line:= line + tile;
          end;

        if line.IsEmpty then
          inc(empty_lines)
        else
          break;
      end;

    if empty_lines>1 then
      begin
        const keep_one_line_empty = 1;
        var change:= empty_lines - keep_one_line_empty;
        Move_design_up(change);
        tilecount_y:= tilecount_y - change;
      end;
  end;

  procedure Check_down;
  begin
    empty_lines:= 0;
    for var y:= tilecount_y-1 downto 0 do
      begin
        var line:= '';
        for var x:= 0 to tilecount_x-1 do
          begin
            var tile:= tiles[x,y];
            if tile='0' then
              continue
            else
              line:= line + tile;
          end;

        if line.IsEmpty then
          inc(empty_lines)
        else
          break;
      end;

    if empty_lines>1 then
      begin
        const keep_one_line_empty = 1;
        var change:= empty_lines - keep_one_line_empty;
        tilecount_y:= tilecount_y - change;
      end;
  end;

  procedure Check_left;
  begin
    empty_lines:= 0;
    for var x:= 0 to tilecount_x-1 do
      begin
        var line:= '';
        for var y:= 0 to tilecount_y-1 do
          begin
            var tile:= tiles[x,y];
            if tile='0' then
              continue
            else
              line:= line + tile;
          end;

        if line.IsEmpty then
          inc(empty_lines)
        else
          break;
      end;

    if empty_lines>1 then
      begin
        const keep_one_line_empty = 1;
        var change:= empty_lines - keep_one_line_empty;
        Move_design_left(change);
        tilecount_x:= tilecount_x - change;
      end;
  end;

  procedure Check_right;
  begin
    empty_lines:= 0;
    for var x:= tilecount_x-1 downto 0 do
      begin
        var line:= '';
        for var y:= 0 to tilecount_y-1 do
          begin
            var tile:= tiles[x,y];
            if tile='0' then
              continue
            else
              line:= line + tile;
          end;

        if line.IsEmpty then
          inc(empty_lines)
        else
          break;
      end;

    if empty_lines>1 then
      begin
        const keep_one_line_empty = 1;
        var change:= empty_lines - keep_one_line_empty;
        tilecount_x:= tilecount_x - change;
      end;
  end;

begin
  Check_up;
  Check_down;
  Check_left;
  Check_right;

  var changed:= (SpinBox_room_size_x.Value <> tilecount_x) OR (SpinBox_room_size_y.Value <> tilecount_y);
  if not changed then
    begin
      ShowMessage('Nothing to optimize');
      exit;
    end;

  SpinBox_room_size_x.Value:= tilecount_x;
  SpinBox_room_size_y.Value:= tilecount_y;
  SetLength(tiles,tilecount_x,tilecount_y);

  Recalculate_sizing;
  Redraw_grid;
  Redraw_ship_tiles;

  ShowMessage('Design optimized, extra rows/columns removed');
  // unfinished
end;

procedure TForm1.Button_optimizeClick(Sender: TObject);
begin
  Optimize_grid_size;
end;

procedure TForm1.Redraw_ship_tiles;
var buffer: TBitmap;
    FillBrush: TBrush;

  procedure Draw_tile(tile_point: TPoint; tile: TTileType);

    procedure Draw_rect;
    var tile_color: TAlphaColor;
    begin
      var rect:= TRect.Create(
        tile_point.X*BlockSize,
        tile_point.Y*BlockSize,
        (tile_point.X+1)*BlockSize,
        (tile_point.Y+1)*BlockSize);

      case tile of
        TTT_Floor:    tile_color:= TAlphaColorRec.LightGray;
        TTT_Window:   tile_color:= TAlphaColorRec.Cyan;
        TTT_Airlock:  tile_color:= TAlphaColorRec.Gray;
      else tile_color:= ColorPanel1.Color;
      end;

      if outside_view then
      case tile of
        TTT_Floor,TTT_Airlock: tile_color:= ColorPanel1.Color;
      end;

      var FloorBrush:= TBrush.Create(TBrushKind.Solid, tile_color);
      try
        buffer.Canvas.DrawRect(rect,1);
        buffer.Canvas.FillRect(rect,1,FloorBrush);

        if derelict then
          begin
            var damage:= Simulate_random_damage;
            if damage>0 then
              begin
                FloorBrush.Color:= Shade_of_black_according_to_damage(damage);
                buffer.Canvas.FillRect(rect,1,FloorBrush);
              end;
          end;

      finally
        FloorBrush.Free;
      end;
    end;

    procedure Draw_triangle(rotation:integer);
    var points: array[0..7] of TPoint;
    begin
      points[0]:= TPoint.Create( tile_point.X*BlockSize,    tile_point.Y*BlockSize);
      points[1]:= TPoint.Create((tile_point.X+1)*BlockSize, tile_point.Y*BlockSize);
      points[2]:= TPoint.Create((tile_point.X+1)*BlockSize,(tile_point.Y+1)*BlockSize);
      points[3]:= TPoint.Create( tile_point.X*BlockSize,   (tile_point.Y+1)*BlockSize);
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

      if derelict then
        begin
          var damage:= Simulate_random_damage;
          if damage>0 then
            begin
              buffer.Canvas.Fill.Color:= Shade_of_black_according_to_damage(damage);
              buffer.Canvas.FillPolygon(poly,1);
            end;
        end;
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
    buffer.Canvas.BeginScene;
    buffer.Canvas.Clear(TAlphaColorRec.Null);

    FillBrush:= TBrush.Create(TBrushKind.Solid, ColorPanel1.Color);
    buffer.Canvas.Fill:= FillBrush;
    buffer.Canvas.Stroke.Thickness:= 3;

    try
      for var x:= 0 to tilecount_x-1 do
      for var y:= 0 to tilecount_y-1 do
        begin
          buffer.Canvas.Fill.Color:= ColorPanel1.Color;
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
  buffer:= TBitmap.Create(round(Layout1.Width),round(Layout1.Height));
  try
    Draw_tiles_to_buffer;
    Image_ship_tiles.Bitmap.Assign(buffer);
    //buffer.SaveToFile('test.bmp');
  finally
    buffer.Free;
  end;
end;

function Tiles_to_string:string; forward;

function Update_shipCode: string;
begin
  result:= Tiles_to_string;
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
  var XX:= round(X) div BlockSize;
  var YY:= round(Y) div BlockSize;
  result:= TPoint.Create(XX,YY);
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

procedure TForm1.Clear_tiles;
begin
  SetLength(tiles,tilecount_x,tilecount_y);

  for var x:= 0 to tilecount_x-1 do
  for var y:= 0 to tilecount_y-1 do
    tiles[x,y]:= TTileType.TTT_Emptytile.ToChar;

  Update_shipCode;
end;

procedure TForm1.Redraw_grid;
var buffer: TBitmap;

  procedure Draw_grid_to_buffer;
  begin
    with buffer.Canvas do
      begin
        BeginScene;
        Clear(TAlphaColorRec.Null);
        Stroke.Color:= TAlphaColorRec.White;
        var opacity:= 0.5;

        for var x:= 1 to tilecount_x-1 do
          DrawLine(TPointF.Create(X*BlockSize,0),TPointF.Create(X*BlockSize,Height),opacity);

        for var y:= 1 to tilecount_y-1 do
          DrawLine(TPointF.Create(0,Y*BlockSize),TPointF.Create(Width,Y*BlockSize),opacity);

        EndScene;
      end;
  end;

begin
  buffer:= TBitmap.Create(round(Layout1.Width),round(Layout1.Height));
  try
    Draw_grid_to_buffer;
    Image_grid.Bitmap.Assign(buffer);
  finally
    buffer.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  painting_tiles:= false;
  derelict:= false;
  Recalculate_sizing;
  Clear_tiles;
  Redraw_grid;

  ComboBox_tiles.Clear;
  for var tile:= low(TTileType) to high(TTileType) do
    ComboBox_tiles.Items.Add( tile.ToString );
  ComboBox_tiles.ItemIndex:= 2;
end;

procedure TForm1.Recalculate_sizing;
begin
  tilecount_x:= round(SpinBox_room_size_x.Value);
  tilecount_y:= round(SpinBox_room_size_y.Value);

  BlockSize:= round(Layout1.Height / tilecount_y);
  Layout1.Width:= BlockSize * tilecount_x;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Recalculate_sizing;
  Redraw_grid;
  Redraw_ship_tiles;
end;

procedure TForm1.Button_generateClick(Sender: TObject);
begin
  Recalculate_sizing;
  Clear_tiles;
  Redraw_grid;
end;

procedure String_to_tiles(input:string);
begin
  try
    Form1.Recalculate_sizing;
    Form1.Clear_tiles;
    Form1.Redraw_grid;
    var index:= 1;
    for var x := 0 to tilecount_x-1 do
    for var y := 0 to tilecount_y-1 do
      begin
        tiles[x,y]:= input[index];
        inc(index);
      end;

    form1.Memo_shipcode.Text:= input;

  except
    ShowMessage('Error loading the layout');
  end;
end;

procedure TForm1.Image_gridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  painting_tiles:= button=TMouseButton.mbLeft;
end;

procedure TForm1.Image_gridMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if not painting_tiles then exit;

  var tile_point:= Mouse_to_tile_point(x,y);
  Paint_tile(tile_point);

  Label_last_tile.Text:=
    'Coords: X='+tile_point.x.ToString+', Y='+tile_point.y.ToString;
end;

procedure TForm1.Image_gridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if button<>TMouseButton.mbLeft then exit;
  var tile_point:= Mouse_to_tile_point(x,y);
  Paint_tile(tile_point);
  painting_tiles:= false;
end;

procedure TForm1.Import_layout_as_json(json:string);
var shipCode: string;
begin
  var JSONObject := TJSONObject.ParseJSONValue(json);
  try
    Edit_ship_class_name.Text:= JSONObject.GetValue<String>('ship_class_name');
    Edit_author.Text:= JSONObject.GetValue<String>('author');

    try
      shipCode:= JSONObject.GetValue<String>('shipCode');
    except
      ShowMessage('Old shipCode format? Trying legacy mode');
      SpinBox_room_size_x.Value:= JSONObject.GetValue<Integer>('tilecount_x');
      SpinBox_room_size_y.Value:= JSONObject.GetValue<Integer>('tilecount_y');
      ColorPanel1.color:= StringToAlphaColor( JSONObject.GetValue<String>('hull_color') );
      String_to_tiles( JSONObject.GetValue<String>('layout') );
      Redraw_ship_tiles;
      exit;
    end;

    // mask: FF= X, FF=Y, FFFFFF=color, remaining characters are shipcode
    Insert(',',shipCode,11);
    Insert(',',shipCode,5);
    Insert(',',shipCode,3);

    var list:= TStringList.Create;
    try
      list.DelimitedText:= shipCode;
      SpinBox_room_size_x.Value:= StrToInt( '$' +list[0] );
      SpinBox_room_size_y.Value:= StrToInt( '$' +list[1] );
      ColorPanel1.color:= StringToAlphaColor( '#FF'+list[2] );
      String_to_tiles(list[3]);
      Redraw_ship_tiles;

    finally
      list.Free;
    end;

  finally
    JSONObject.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  derelict:= not derelict;
  Redraw_ship_tiles;
end;

procedure TForm1.Button_loadClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Import_layout_as_json( FileToString(OpenDialog1.FileName) );
end;

function Tiles_to_string: string;
begin
  result:= '';
  try
    // mask: FF= X, FF=Y, FFFFFF=color, remaining characters are shipcode
    var hex_x:= IntToHex(tilecount_x,2);
    var hex_y:= IntToHex(tilecount_y,2);
    var color:= AlphaColorToString(form1.ColorPanel1.color);
    color:= chop(color,'#FF');

    var shipCode: string;
    for var x := 0 to tilecount_x-1 do
    for var y := 0 to tilecount_y-1 do
      shipCode:= shipCode + tiles[x,y];

    result:=
      hex_x +
      hex_y +
      color +
      shipCode;

  except
    ShowMessage('Error saving the layout');
  end;
end;

function Export_layout_as_json: string;
begin
  var JSONObject := TJSONObject.Create;
  try
    try
      JSONObject.AddPair('ship_class_name',Form1.Edit_ship_class_name.Text);
      JSONObject.AddPair('author',form1.Edit_author.Text);
      JSONObject.AddPair('shipCode',Tiles_to_string);

    except
      ShowMessage('Error saving the data into json');
    end;

  finally
    result:= JSONObject.ToString;
    JSONObject.Free;
  end;
end;

procedure TForm1.Button_saveClick(Sender: TObject);
begin
  if form1.Edit_author.Text.IsEmpty then
    begin
      ShowMessage('Please add your name as author');
      exit;
    end;
  if form1.Edit_ship_class_name.Text.IsEmpty then
    begin
      ShowMessage('Please add ship class name for the design');
      exit;
    end;

  var filename:= Form1.Edit_ship_class_name.Text +' '+
    SpinBox_room_size_x.Value.ToString+'x'+SpinBox_room_size_y.Value.ToString;
  SaveDialog1.FileName:= filename;
  SaveDialog2.FileName:= filename;

  var json:= Export_layout_as_json;

  if SaveDialog1.Execute then
    StringToFile(json, SaveDialog1.FileName);

  if SaveDialog2.Execute then
    Image_ship_tiles.MakeScreenshot.SaveToFile(SaveDialog2.FileName);
end;

procedure TForm1.Button_testClick(Sender: TObject);
begin
  Tiles_to_string;
end;

procedure TForm1.CheckBox_derelictChange(Sender: TObject);
begin
  derelict:= CheckBox_derelict.IsChecked;
  Redraw_ship_tiles;
end;

procedure TForm1.CheckBox_outside_viewChange(Sender: TObject);
begin
  outside_view:= CheckBox_outside_view.IsChecked;
  Redraw_ship_tiles;
end;

procedure TForm1.ColorPanel1Change(Sender: TObject);
begin
  var color := ColorPanel1.color;
  Edit_tile_color.Text := AlphaColorToString(color);
  Redraw_ship_tiles;
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
