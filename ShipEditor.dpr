program ShipEditor;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  game_classes in '..\spaceships-shared\game_classes.pas',
  game_functions in '..\spaceships-shared\game_functions.pas',
  ship_graphics in '..\spaceships-shared\ship_graphics.pas',
  game_consts in '..\spaceships-shared\game_consts.pas',
  ship_defaults in '..\spaceships-shared\ship_defaults.pas',
  game_ships in '..\spaceships-shared\game_ships.pas',
  ship_design in '..\spaceships-shared\ship_design.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
