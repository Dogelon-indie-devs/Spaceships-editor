program ShipEditor;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  game_functions in '..\spaceships-shared\game_functions.pas',
  game_classes in '..\spaceships-shared\game_classes.pas',
  game_consts in '..\spaceships-shared\game_consts.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
