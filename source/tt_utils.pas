unit TT_utils;


{ A collection of small and unrelated utilites that are for use by TomBoyTools
  only, tomboy-ng has them already.

  Possible conside moving some of thes into tb_utils if it seeems to make sense.

}

{$mode ObjFPC}{$H+}

interface
uses
    Classes, SysUtils, Main;

procedure UpdateStatusBar(St : String);

implementation

uses LCLProc, Forms, laz2_DOM, laz2_XMLRead, LazFileUtils, tb_utils;

procedure UpdateStatusBar(St : String);
begin
  if assigned(FormMain) then begin
        FormMain.StatusBar1.SimpleText:= St;
        Application.ProcessMessages;
  end;
end;

end.

