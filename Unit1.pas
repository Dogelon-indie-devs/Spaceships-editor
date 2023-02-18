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
  ship_design,
  ship_defaults,
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
    CheckBox_interior_view: TCheckBox;
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
    procedure CheckBox_interior_viewChange(Sender: TObject);
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
    procedure FormDestroy(Sender: TObject);
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
  design: TShipDesign;
  painting_tiles: boolean;
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
  for var y:= 0 to design.tileCount_Y-1 do
  for var x:= steps to design.tileCount_X-1 do
    begin
      Design.tiles[x-steps,y]:= Design.tiles[x,y];
      Design.tiles[x,y]:= '0';
    end;
end;

procedure Move_design_up(steps:integer);
begin
  for var x:= 0 to design.tileCount_X-1 do
  for var y:= steps to design.tileCount_Y-1 do
    begin
      Design.tiles[x,y-steps]:= Design.tiles[x,y];
      Design.tiles[x,y]:= '0';
    end;
end;

//function Tiles_to_string:string; forward;

function Update_shipCode: string;
begin
  result:= Design.Recompile_into_ShipCode; //Tiles_to_string;
  form1.Memo_shipcode.Text:= result;
end;

procedure TForm1.Optimize_grid_size;
var empty_lines: integer;

  procedure Check_up;
  begin
    empty_lines:= 0;
    for var y:= 0 to design.tileCount_Y-1 do
      begin
        var line:= '';
        for var x:= 0 to design.tileCount_X-1 do
          begin
            var tile:= Design.tiles[x,y];
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
        design.tileCount_Y:= design.tileCount_Y - change;
      end;
  end;

  procedure Check_down;
  begin
    empty_lines:= 0;
    for var y:= design.tileCount_Y-1 downto 0 do
      begin
        var line:= '';
        for var x:= 0 to design.tileCount_X-1 do
          begin
            var tile:= Design.tiles[x,y];
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
        design.tileCount_Y:= design.tileCount_X - change;
      end;
  end;

  procedure Check_left;
  begin
    empty_lines:= 0;
    for var x:= 0 to design.tileCount_X-1 do
      begin
        var line:= '';
        for var y:= 0 to design.tileCount_Y-1 do
          begin
            var tile:= Design.tiles[x,y];
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
        design.tileCount_X:= design.tileCount_X - change;
      end;
  end;

  procedure Check_right;
  begin
    empty_lines:= 0;
    for var x:= design.tileCount_X-1 downto 0 do
      begin
        var line:= '';
        for var y:= 0 to design.tileCount_Y-1 do
          begin
            var tile:= Design.tiles[x,y];
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
        design.tileCount_X:= design.tileCount_X - change;
      end;
  end;

begin
  Check_up;
  Check_down;
  Check_left;
  Check_right;

  var changed:= (SpinBox_room_size_x.Value <> design.tileCount_X) OR (SpinBox_room_size_y.Value <> design.tileCount_X);
  if not changed then
    begin
      ShowMessage('Nothing to optimize');
      exit;
    end;

  SpinBox_room_size_x.Value:= design.tileCount_X;
  SpinBox_room_size_y.Value:= design.tileCount_Y;
  SetLength(Design.tiles,design.tileCount_X,design.tileCount_Y);

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
  if tile_point.X>design.tileCount_X then exit;
  if tile_point.Y>design.tileCount_Y then exit;

  var existing_tile:= Design.tiles[tile_point.x,tile_point.y];
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

  if tile_point.X>design.tileCount_X then exit;
  if tile_point.Y>design.tileCount_Y then exit;

  if Is_same_tiletype_as_selected(tile_point) then
    begin

    end
  else
    begin
      var selected_tile_type:= TTileType.IndexToChar(ComboBox_tiles.ItemIndex);
      Design.tiles[tile_point.x,tile_point.y]:= selected_tile_type;
    end;

  var newCode:= Update_shipCode;
  var no_changes:= previous_shipCode = newCode;
  if no_changes then exit;
  previous_shipCode:= newCode;

  Redraw_ship_tiles;
end;

procedure TForm1.Clear_tiles;
begin
  SetLength(Design.tiles,design.tileCount_X,design.tileCount_Y);

  for var x:= 0 to design.tileCount_X-1 do
  for var y:= 0 to design.tileCount_Y-1 do
    Design.tiles[x,y]:= TTileType.ttEmptytile.ToChar;

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
  design := TShipDesign.Create('0A0AFF8C0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000');
  design.derelict:= false;
  design.Set_BaseColor(TAlphaColorRec.Darkorange);

  Recalculate_sizing;
  Clear_tiles;
  Redraw_grid;

  ComboBox_tiles.Clear;
  for var tile:= low(TTileType) to high(TTileType) do
    ComboBox_tiles.Items.Add( tile.ToString );
  ComboBox_tiles.ItemIndex:= 2;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(design) then
    design.Free;
end;

procedure TForm1.Recalculate_sizing;
begin
  design.tileCount_X:= round(SpinBox_room_size_x.Value);
  design.tileCount_Y:= round(SpinBox_room_size_y.Value);

  BlockSize:= round(Layout1.Height / design.tileCount_Y);
  Layout1.Width:= BlockSize * design.tileCount_X;

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
  design.Clear_arrays;
  Redraw_grid;
  Redraw_ship_tiles;
  Self.Resize;
end;

procedure String_to_tiles(input:string);
begin
  try
    var expected_shipcode_length:= design.tileCount_X * design.tileCount_Y;
    var actual_length:= input.Length;

    if actual_length <> expected_shipcode_length then
      begin
        if actual_length = expected_shipcode_length -1 then
          begin
            // old shipcode format without weapon
            input:= '0'+input;
          end
        else
          Raise Exception.Create(
            'Expected shipcode length: '+expected_shipcode_length.ToString+
            ', received: '+actual_length.ToString
            );
      end;

    Form1.Recalculate_sizing;
    Form1.Clear_tiles;
    Form1.Redraw_grid;
    var index:= 1;
    for var x := 0 to design.tileCount_X-1 do
    for var y := 0 to design.tileCount_Y-1 do
      begin
        Design.tiles[x,y]:= input[index];
        inc(index);
      end;

    //design.shipcode:= input;
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
      if Assigned(design) then
        begin
          design.Free;
          design:= TShipDesign.Create(shipCode); //Decompile_shipcode_into_design(shipCode);
        end;
    except
      ShowMessage('Old shipCode format? Trying legacy mode');
      design.tileCount_X:= JSONObject.GetValue<Integer>('tilecount_x');
      design.tileCount_Y:= JSONObject.GetValue<Integer>('tilecount_y');
      design.Set_BaseColor(StringToAlphaColor( JSONObject.GetValue<String>('hull_color') ));
      //design.shipcode:=  JSONObject.GetValue<String>('layout');
    end;

    SpinBox_room_size_x.Value:= design.tileCount_X;
    SpinBox_room_size_y.Value:= design.tileCount_Y;
    String_to_tiles( design.Recompile_into_ShipCode );

    ColorPanel1.Color := design.BaseColor;

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

{function Tiles_to_string: string;
begin
  result:= '';
  try
    // mask: FF= X, FF=Y, FFFFFF=color, remaining characters are shipcode
    var hex_x:= IntToHex(design.tileCount_X,2);
    var hex_y:= IntToHex(design.tileCount_Y,2);
    var color:= IntToHex(design.PaletteColors[0]);
    if pos('#',color)=0 then
      color:= '#'+color;
    color:= chop(color,'#FF');
    var weapon:= '0';

    design.shipcode:= '';
    for var x := 0 to design.tileCount_X-1 do
    for var y := 0 to design.tileCount_Y-1 do
      design.shipcode:= design.shipcode + tiles[x,y];

    result:=
      hex_x +
      hex_y +
      color +
      weapon +
      design.shipcode;

  except
    ShowMessage('Error saving the layout');
  end;
end;}

function Export_layout_as_json: string;
begin
  var JSONObject := TJSONObject.Create;
  try
    try
      JSONObject.AddPair('ship_class_name',Form1.Edit_ship_class_name.Text);
      JSONObject.AddPair('author',form1.Edit_author.Text);
      JSONObject.AddPair('shipCode', Design.Recompile_into_ShipCode);

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
  Design.Recompile_into_ShipCode;
end;

procedure TForm1.CheckBox_derelictChange(Sender: TObject);
begin
  design.derelict:= CheckBox_derelict.IsChecked;
  Redraw_ship_tiles;
end;

procedure TForm1.CheckBox_interior_viewChange(Sender: TObject);
begin
  design.interior_view:= CheckBox_interior_view.IsChecked;
  Redraw_ship_tiles;
end;

procedure TForm1.ColorPanel1Change(Sender: TObject);
begin
  design.Set_BaseColor(ColorPanel1.color);
  Edit_tile_color.Text := AlphaColorToString(design.BaseColor);
  Redraw_ship_tiles;
end;

end.
