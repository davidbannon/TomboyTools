program TomboyTools;

{   Copyright (C) 2017-2020 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    ------------------
}

{$mode objfpc}{$H+}

uses
        {$IFDEF UNIX}{$IFDEF UseCThreads}
        cthreads,
        {$ENDIF}{$ENDIF}
        Interfaces, // this includes the LCL widgetset
        Forms, main, cmdline, export_notes, import_notes,
		note2po, notenormal, tt_utils, exporthtml
        { you can add units after this };

{$R *.res}

begin
        if Finished() then exit;
        RequireDerivedFormResource:=True;
		Application.Scaled:=True;
        Application.Initialize;
		Application.CreateForm(TFormMain, FormMain);
        Application.Run;
end.

