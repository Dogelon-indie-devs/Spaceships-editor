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

  game_classes,
  game_functions,
  ship_graphics;

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
    procedure Image_gridMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
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
  design: ShipDesignRec;
  painting_tiles: boolean;
  tiles: array of array of char;
  previous_shipCode: string;
  BlockSize: integer;

implementation

{$R *.fmx}

function Randomize_color_alpha(inputColor: TAlphaColor): TAlphaColor;
begin
  TAlphaColorRec(inputColor).A:= random(256);
  result:= inputColor;
end;

procedure Move_design_left(steps:integer);
begin
  for var y:= 0 to design.tiles_y-1 do
  for var x:= steps to design.tiles_x-1 do
    begin
      tiles[x-steps,y]:= tiles[x,y];
      tiles[x,y]:= '0';
    end;
end;

procedure Move_design_up(steps:integer);
begin
  for var x:= 0 to design.tiles_x-1 do
  for var y:= steps to design.tiles_y-1 do
    begin
      tiles[x,y-steps]:= tiles[x,y];
      tiles[x,y]:= '0';
    end;
end;

function Tiles_to_string:string; forward;

function Update_shipCode: string;
begin
  result:= Tiles_to_string;
  form1.Memo_shipcode.Text:= result;
end;

procedure TForm1.Optimize_grid_size;
var empty_lines: integer;

  procedure Check_up;
  begin
    empty_lines:= 0;
    for var y:= 0 to design.tiles_y-1 do
      begin
        var line:= '';
        for var x:= 0 to design.tiles_x-1 do
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
        design.tiles_y:= design.tiles_y - change;
      end;
  end;

  procedure Check_down;
  begin
    empty_lines:= 0;
    for var y:= design.tiles_y-1 downto 0 do
      begin
        var line:= '';
        for var x:= 0 to design.tiles_x-1 do
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
        design.tiles_y:= design.tiles_y - change;
      end;
  end;

  procedure Check_left;
  begin
    empty_lines:= 0;
    for var x:= 0 to design.tiles_x-1 do
      begin
        var line:= '';
        for var y:= 0 to design.tiles_y-1 do
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
        design.tiles_x:= design.tiles_x - change;
      end;
  end;

  procedure Check_right;
  begin
    empty_lines:= 0;
    for var x:= design.tiles_x-1 downto 0 do
      begin
        var line:= '';
        for var y:= 0 to design.tiles_y-1 do
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
        design.tiles_x:= design.tiles_x - change;
      end;
  end;

begin
  Check_up;
  Check_down;
  Check_left;
  Check_right;

  var changed:= (SpinBox_room_size_x.Value <> design.tiles_x) OR (SpinBox_room_size_y.Value <> design.tiles_y);
  if not changed then
    begin
      ShowMessage('Nothing to optimize');
      exit;
    end;

  SpinBox_room_size_x.Value:= design.tiles_x;
  SpinBox_room_size_y.Value:= design.tiles_y;
  SetLength(tiles,design.tiles_x,design.tiles_y);

  Recalculate_sizing;
  Redraw_grid;
  Redraw_ship_tiles;
  Update_shipCode;

  ShowMessage('Design optimized, extra rows/columns removed');
end;

procedure TForm1.Button_optimizeClick(Sender: TObject);
begin
  Optimize_grid_size;
end;

procedure TForm1.Redraw_ship_tiles;
begin
  var buffer:= Draw_ship_design_on_bitmap(design);
  try
    Image_ship_tiles.Bitmap.Assign(buffer);
    //buffer.SaveToFile('test.bmp');
  finally
    buffer.Free;
  end;
end;

function TForm1.Is_same_tiletype_as_selected(tile_point: TPoint): boolean;
begin
  result:= false;
  if tile_point.X>design.tiles_x then exit;
  if tile_point.Y>design.tiles_y then exit;

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

  if tile_point.X>design.tiles_x then exit;
  if tile_point.Y>design.tiles_y then exit;

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
  SetLength(tiles,design.tiles_x,design.tiles_y);

  for var x:= 0 to design.tiles_x-1 do
  for var y:= 0 to design.tiles_y-1 do
    tiles[x,y]:= TTileType.TTT_Emptytile.ToChar;

  Update_shipCode;
end;

procedure TForm1.Redraw_grid;
begin
  var buffer:= Draw_blueprint_grid_on_bitmap(design);
  try
    Image_grid.Bitmap.Assign(buffer);
  finally
    buffer.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  painting_tiles:= false;
  design.derelict:= false;
  design.color:= TAlphaColorRec.Darkorange;
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
  design.tiles_x:= round(SpinBox_room_size_x.Value);
  design.tiles_y:= round(SpinBox_room_size_y.Value);

  BlockSize:= round(Layout1.Height / design.tiles_y);
  Layout1.Width:= BlockSize * design.tiles_x;

  design.bitmap_width := round(Layout1.Width);
  design.bitmap_height:= round(Layout1.Height);
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
    for var x := 0 to design.tiles_x-1 do
    for var y := 0 to design.tiles_y-1 do
      begin
        tiles[x,y]:= input[index];
        inc(index);
      end;

    design.layout:= input;
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

procedure TForm1.Image_gridMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  var index:= ComboBox_tiles.ItemIndex;
  var selected_corner:= (index > 2) AND (index < 7);
  if not selected_corner then exit;

  if WheelDelta>0 then
    begin
      ComboBox_tiles.ItemIndex:= ComboBox_tiles.ItemIndex +1;
      if ComboBox_tiles.ItemIndex > 6 then
        ComboBox_tiles.ItemIndex:= 3;
    end
  else
    begin
      ComboBox_tiles.ItemIndex:= ComboBox_tiles.ItemIndex -1;
      if ComboBox_tiles.ItemIndex < 3 then
        ComboBox_tiles.ItemIndex:= 6;
    end;
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
      design:= Decompile_shipcode_into_design(shipCode);
    except
      ShowMessage('Old shipCode format? Trying legacy mode');
      design.tiles_x:= JSONObject.GetValue<Integer>('tilecount_x');
      design.tiles_y:= JSONObject.GetValue<Integer>('tilecount_y');
      design.color:=   StringToAlphaColor( JSONObject.GetValue<String>('hull_color') );
      design.layout:=  JSONObject.GetValue<String>('layout');
    end;

    SpinBox_room_size_x.Value:= design.tiles_x;
    SpinBox_room_size_y.Value:= design.tiles_y;
    String_to_tiles( design.layout );
    ColorPanel1.color:= design.color;

  finally
    JSONObject.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  design.derelict:= not design.derelict;
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
    var hex_x:= IntToHex(design.tiles_x,2);
    var hex_y:= IntToHex(design.tiles_y,2);
    var color:= IntToHex(design.color);
    if pos('#',color)=0 then
      color:= '#'+color;
    color:= chop(color,'#FF');

    design.layout:= '';
    for var x := 0 to design.tiles_x-1 do
    for var y := 0 to design.tiles_y-1 do
      design.layout:= design.layout + tiles[x,y];

    result:=
      hex_x +
      hex_y +
      color +
      design.layout;

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
  design.derelict:= CheckBox_derelict.IsChecked;
  Redraw_ship_tiles;
end;

procedure TForm1.CheckBox_outside_viewChange(Sender: TObject);
begin
  design.outside_view:= CheckBox_outside_view.IsChecked;
  Redraw_ship_tiles;
end;

procedure TForm1.ColorPanel1Change(Sender: TObject);
begin
  design.color := ColorPanel1.color;
  Edit_tile_color.Text := AlphaColorToString(design.color);
  Redraw_ship_tiles;
end;

end.
