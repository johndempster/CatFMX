program Cat;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  CatMain in 'CatMain.pas' {MainFrm},
  CatModel in 'CatModel.pas' {Model: TDataModule},
  ModalBox in 'ModalBox.pas' {ModalBoxFrm}
  {$IFDEF MACOS}
  ,FMX.Platform.Mac in 'FMX.Platform.Mac.pas'
  {$ENDIF} ;
  

{$R *.res}

begin
  {$IFDEF MACOS}
  FMX.Types.GlobalUseMetal := true ;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainFrm, MainFrm);
  Application.CreateForm(TModel, Model);
  Application.CreateForm(TModalBoxFrm, ModalBoxFrm);
  Application.Run;
end.



