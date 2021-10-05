unit main;

{$mode objfpc}{$H+} {$assertions on}

{   Copyright (C) 2017-2020 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    ------------------ }

{   HISTORY
    2021/08/27 Bug where ProcessNotebooks forgot to check the HTML button


}

interface

uses
        Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
		ExtCtrls, Buttons, StdCtrls, CheckLst, LCLProc;

type

		{ TFormMain }

        TFormMain = class(TForm)
            ButtonImSelectAll: TButton;
            ButtonImUnselectAll: TButton;
            ButtonSelectAll: TButton;
            ButtonUnselectAll: TButton;
            CheckFollowLinks: TCheckBox;
            CheckRetainFileName: TCheckBox;
				CheckListBox1: TCheckListBox;
                CheckListImportFiles: TCheckListBox;
                GroupBox1: TGroupBox;
                GroupBox2: TGroupBox;
                GroupBox3: TGroupBox;
                GroupBox4: TGroupBox;
                GroupBox5: TGroupBox;
                LabelVersion: TLabel;
				Label3: TLabel;
				Label5: TLabel;
				Label6: TLabel;
                Label8: TLabel;
                LabelImportDestination: TLabel;
                LabelErrorMessage: TLabel;
                LabelImportSource: TLabel;
				LabelSourcePrompt: TLabel;
				LabelSource: TLabel;
				LabelDestinationPrompt: TLabel;
				LabelDestination: TLabel;
				PageControl1: TPageControl;
                PanelLower: TPanel;
				PanelTop: TPanel;
                RadioExMan: TRadioButton;
                RadioImMarkDown: TRadioButton;
                RadioImText: TRadioButton;
                RadioExMarkDown: TRadioButton;
                RadioExText: TRadioButton;
                RadioExHtml: TRadioButton;
                RadioExNotes: TRadioButton;
                RadioExNotebook: TRadioButton;
                RadioExDir: TRadioButton;
                RadioFileNameTitle: TRadioButton;
                RadioFileNameID: TRadioButton;
                RadioTitleFirstLine: TRadioButton;
                RadioTitleFilename: TRadioButton;
				SelectDirectoryDialog1: TSelectDirectoryDialog;
                SpeedImportDestination: TSpeedButton;
                SpeedExit: TSpeedButton;
                SpeedImportSourceDir: TSpeedButton;
                SpeedProceed: TSpeedButton;
				SpeedSetDestination: TSpeedButton;
				StatusBar1: TStatusBar;
				TabExport: TTabSheet;
				TabImport: TTabSheet;
                procedure ButtonImSelectAllClick(Sender: TObject);
                procedure ButtonImUnselectAllClick(Sender: TObject);
                procedure ButtonSelectAllClick(Sender: TObject);
                procedure ButtonUnselectAllClick(Sender: TObject);
                procedure CheckListBox1Click(Sender: TObject);
                procedure CheckListImportFilesClick(Sender: TObject);
                procedure CheckListImportFilesClickCheck(Sender: TObject);
                procedure FormCreate(Sender: TObject);
                procedure FormDestroy(Sender: TObject);
                procedure FormShow(Sender: TObject);
                procedure RadioExDirChange(Sender: TObject);
                procedure RadioExManChange(Sender: TObject);
                procedure RadioExMarkDownChange(Sender: TObject);
                procedure RadioExNotebookChange(Sender: TObject);
                procedure RadioExNotesChange(Sender: TObject);
                procedure RadioImMarkDownChange(Sender: TObject);
                procedure RadioImTextChange(Sender: TObject);
                procedure SpeedImportDestinationClick(Sender: TObject);
				procedure SpeedExitClick(Sender: TObject);

				procedure SpeedProceedClick(Sender: TObject);
    			procedure SpeedSetDestinationClick(Sender: TObject);
				//procedure SpeedSetSourceClick(Sender: TObject);
                procedure SpeedImportSourceDirClick(Sender: TObject);
                procedure TabExportShow(Sender: TObject);
                procedure TabImportShow(Sender: TObject);
        private
            Ready : boolean;    // To prevent events that happen during setup being acted on
                    // lists we will use to store found note names. They are regional
                    // so the CheckListBox can store ID data in its Object
            RSLFiles, RegImFiles : TStringList;
            procedure DisplayImportPossibilities();
            function GetExportNotes(): integer;

            procedure ImportProceed();
            function NumberChecked(CLB: TCheckListBox): integer;

            procedure ProcessNotebooks;
                                // Called to iterate over the notes listed in the CheckListBox, exporting
                                // all that are checked.
            procedure ProcessNotes;
				//procedure SetUpSource(Mode: integer);
				procedure SetUpNoteBook();
				function GetNoteBooks(): integer;
				//function ExportReadyToGo(): boolean;
				//function SetExportSource(SDir: string): boolean;
                function TestImportDestination(): boolean;
        public
            DefaultNotesDir : string;
        end;


var
        FormMain: TFormMain;

implementation

uses cmdline, FileUtil, LazFileUtils, export_notes, import_notes, tb_utils, tt_utils;

const Version_string  = {$I %TTools_VER};

{$R *.lfm}

{ TFormMain }


// ========================= S H A R E D    M E T H O D S  ====================


procedure TFormMain.FormCreate(Sender: TObject);
begin
    Ready := False;
    left := (screen.Width div 2) - (width div 2);
    Top := (screen.height div 2) - (height div 2);
    LabelVersion.Caption := Version_string;
    LabelErrorMessage.Caption := '';
    RSLFiles := nil;
    RegImFiles := Nil;
    DefaultNotesDir := GetDefaultNoteDir();                           // Thats tomboy-ng
    PageControl1.TabIndex := 0;
    // Some initial defaults
    {$ifdef WINDOWS}
    LabelDestination.Caption := GetEnvironmentVariable('HOMEPATH');   // DoDo : Test This !!!!!
    {$else}
    LabelDestination.Caption := GetEnvironmentVariable('HOME');
    {$endif}
    LabelImportSource.Caption := LabelDestination.Caption;
    LabelSource.Caption := DefaultNotesDir;
    LabelImportDestination.Caption := DefaultNotesDir;
    RadioExNotes.Checked := True;
    // Maybe user put something on command line ? Override.
    if Application.HasOption('s', 'source') then begin
        LabelImportSource.Caption := Application.GetOptionValue('s', 'source');
        LabelSource.Caption := Application.GetOptionValue('s', 'source');
        RadioExDir.Checked := True;
        GroupBox3.Enabled := False;
        SpeedImportSourceDir.Enabled := False;
    end;
    if Application.HasOption('d', 'destination') then begin
        LabelDestination.Caption := Application.GetOptionValue('d', 'destination');
        LabelImportDestination.Caption := Application.GetOptionValue('d', 'destination');
        SpeedSetDestination.Enabled := False;
        SpeedImportDestination.Enabled := False;
    end;
    GetExportNotes();
    DisplayImportPossibilities();
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
    if assigned(RSLFiles) then RSLFiles.Free;
    if assigned(RegImFiles) then RegImFiles.free;
end;

procedure TFormMain.SpeedExitClick(Sender: TObject);
begin
    close;
end;


procedure TFormMain.SetUpNoteBook() ;
begin
    CheckListBox1.Enabled := true;
    if GetNoteBooks() = 0 then
        showmessage('That dir has no notes in notebooks');
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
    Ready := True;
end;


// ==================  E X P O R T I N G    M E T H O D S ======================

procedure TFormMain.RadioExDirChange(Sender: TObject);
begin
    if not Ready then exit;
    if RadioExDir.Checked then begin;
        if SelectDirectoryDialog1.Execute then begin
            LabelSource.Caption := TrimFilename(SelectDirectoryDialog1.FileName + PathDelim);
            if GetExportNotes() = 0 then
                showmessage('That dir has no notes');
	    end;
    end;
end;

procedure TFormMain.RadioExManChange(Sender: TObject);
begin

end;



                        // NOTE : called for change to all four radio buttons
procedure TFormMain.RadioExMarkDownChange(Sender: TObject);
begin
    CheckFollowLinks.enabled := RadioExHTML.Checked;
    GroupBox2.Enabled := (TRadioButton(sender).Name <> 'RadioExHtml')
                            or (TRadioButton(sender).Name <> 'RadioExMan');
end;

procedure TFormMain.RadioExNotebookChange(Sender: TObject);
begin
    if not Ready then exit;
    LabelSource.Caption := DefaultNotesDir;
    if RadioExNotebook.Checked then
        if GetNoteBooks() = 0 then
            showmessage('That dir has no notes in notebooks');
end;

procedure TFormMain.RadioExNotesChange(Sender: TObject);
begin
    if not Ready then exit;
    LabelSource.Caption := DefaultNotesDir;
    if RadioExNotes.Checked then
        if GetExportNotes() =0 then
            showmessage('That dir has no notes');
end;



procedure TFormMain.ProcessNotes;
var
    Exporter : TExportNote;
    Index : integer;
begin
    LabelErrorMessage.Caption := '';
    Exporter := TExportNote.Create;
    try
        Exporter.DestDir := appendPathDelim(LabelDestination.Caption);
        Exporter.NoteDir := appendPathDelim(LabelSource.caption);
        Exporter.FileNameIsTitle := RadioFileNameTitle.checked;
        Exporter.FollowLinks := CheckFollowLinks.Checked;
        Exporter.OutFormat := '';
        if RadioExMarkDown.Checked then Exporter.OutFormat := 'md';
        if RadioExText.Checked then Exporter.OutFormat := 'text';
        if RadioExHTML.Checked then Exporter.OutFormat := 'html';
        if RadioEXMan.checked then Exporter.OutFormat := 'man';
        if  Exporter.OutFormat = '' then begin
            showmessage('Error, out format not set');
            exit;
        end;
        Index := CheckListBox1.Count;
        while Index > 0 do begin
            dec(Index);
            //debugln('Exporting ' + CheckListBox1.Items[Index]);
            if CheckListBox1.Checked[Index] then
                Exporter.ExportOneFile(String(CheckListBox1.Items.Objects[Index]));
            if Exporter.ErrorMessage <> '' then
                LabelErrorMessage.Caption := Exporter.ErrorMessage;
        end;
    finally
        Exporter.Free;

	end;
end;


procedure TFormMain.ProcessNotebooks;
var
    Exporter : TExportNote;
    Index : integer = 0;
    //UsingChecked : boolean = false;
    NotesProcessed : integer = 0;
begin
    while Index < CheckListBox1.Items.Count do begin
        if CheckListBox1.Checked[Index] then begin
            //UsingChecked := True;
            debugln('Checked [' + CheckListBox1.Items[Index] + ']');
            break;
        end;
        inc(Index);
    end;
    Exporter := TExportNote.Create;
    try
        Exporter.DestDir := AppendPathDelim(LabelDestination.Caption);
        Exporter.NoteDir := AppendPathDelim(LabelSource.caption);
        Exporter.FollowLinks := False;
        Exporter.OutFormat := '';
        if RadioExMarkDown.Checked then Exporter.OutFormat := 'md';
        if RadioExText.Checked then Exporter.OutFormat := 'text';
        if RadioExHTML.Checked then Exporter.OutFormat := 'html';
        if  Exporter.OutFormat = '' then begin
            showmessage('Error, out format not set');
            exit;
        end;
        Exporter.FileNameIsTitle := RadioFileNameTitle.Checked;
        DebugLn('checked notebooks');
        Index := 0;
        while Index < CheckListBox1.Items.Count do begin
            if CheckListBox1.Checked[Index] then begin
                //debugln('Checked [' + CheckListBox1.Items[Index] + ']');
                Exporter.Notebook := CheckListBox1.Items[Index];
                Exporter.Execute();
                if Exporter.ErrorMessage <> '' then begin
                    debugln(Exporter.ErrorMessage);
                    showmessage(Exporter.ErrorMessage);
                    LabelErrorMessage.Caption := Exporter.ErrorMessage;
                end else begin
                    StatusBar1.SimpleText:=  CheckListBox1.items[Index] + ' '
                            + inttostr(Exporter.NotesProcessed) + ' notes processed.';
                    NotesProcessed := NotesProcessed + Exporter.NotesProcessed;
                end;
                Application.ProcessMessages;
            end;
            inc(Index);
        end;
        StatusBar1.SimpleText:=  'Total of '
                + inttostr(NotesProcessed) + ' notes processed.';
	finally
        Exporter.Free;
	end;
end;

procedure TFormMain.CheckListBox1Click(Sender: TObject);
begin
    //debugln('Clicked '+ string(CheckListBox1.items.objects[CheckListBox1.ItemIndex]));
    CheckListBox1.ItemIndex := -1;
    //ExportReadyToGo();
end;

procedure TFormMain.ButtonSelectAllClick(Sender: TObject);
var
    Index : integer = 0;
begin
    Index := CheckListBox1.Count;
    while Index > 0 do begin
        dec(index);
        CheckListBox1.Checked[Index] := True;
    end;
end;

procedure TFormMain.ButtonUnselectAllClick(Sender: TObject);
var
    Index : integer = 0;
begin
    Index := CheckListBox1.Count;
    while Index > 0 do begin
        dec(index);
        CheckListBox1.Checked[Index] := False;
    end;
end;

procedure TFormMain.CheckListImportFilesClick(Sender: TObject);
begin
    CheckListImportFiles.ItemIndex := -1;
end;


function TFormMain.GetExportNotes() : integer;
var
    // SLFiles : TStringList;
    St : string;
begin
    Result := 0;
    CheckListBox1.Clear;
    if LabelSource.Caption = '' then exit(0);
    // ToDo : if capion ends with .note do something else !
    if directoryExists(LabelSource.Caption) then begin
        if assigned(RSLFiles) then RSLFiles.Free;
	    RSLFiles := FindAllFiles(LabelSource.Caption, '*.note', false);   // Note : stores FFN !
        try
            for St in RSLFiles do begin
                CheckListBox1.AddItem(GetTitleFromFFN(St, False), Tobject(St));
            end;
            Result := checklistbox1.items.count;
            StatusBar1.SimpleText:= inttostr(CheckListBox1.Count) + ' notes found';
            if Result = 0 then StatusBar1.SimpleText:= 'No notes found';
		finally
          //SLFiles.free;
          CheckListBox1.enabled := CheckListBox1.Count > 0;
		end;
    end else debugln('Not Found ' + LabelSource.Caption);
end;

                            // Loads all the notebooks found in current source dir into the TListBox
function TFormMain.GetNoteBooks() : integer;
var
    SLFiles, SLContent, SLBooks : TStringList;
    ST : string;
    Index, NBStart, NBEnd : integer;
    Buff : string;
begin
    Result := 0;
    CheckListBox1.Clear;
    if LabelSource.Caption = '' then exit(0);
    if directoryExists(LabelSource.Caption) then begin
	    SLFiles := FindAllFiles(LabelSource.Caption, '*.note', false);
        SLBooks := TStringList.Create;
        SLBooks.Sorted := True;
        SLBooks.Duplicates:= dupIgnore;
        try
            for St in SLFiles do begin
                SLContent := TStringList.create;        // ToDo : reuse this list
                SLContent.LoadFromFile(St);
                Index := FindInStringList(SLContent, '<tag>system:notebook:');
                if Index > -1 then begin
                    Buff := SLContent.Strings[Index];
                    NBStart := pos('<tag>system:notebook:', Buff) +21;
                    NBEnd   := pos('</tag>', Buff);
                    SLBooks.Add(RestoreBadXMLChar((copy(Buff, NBStart, NBEnd - NBStart))));
                    inc(Result);
				end;
                SLContent.free;
			end;
            CheckListBox1.Items := SLBooks;
		finally
          SLBooks.Free;
          SLFiles.free;
		end;
	end;
    CheckListBox1.enabled := CheckListBox1.Count > 0;
    StatusBar1.SimpleText:= inttostr(CheckListBox1.Count) + ' notebooks found';
    if CheckListBox1.Count = 0 then StatusBar1.SimpleText:= 'No notebooks found';
end;

procedure TFormMain.SpeedSetDestinationClick(Sender: TObject);
begin
    if SelectDirectoryDialog1.Execute then
            LabelDestination.Caption := TrimFilename(SelectDirectoryDialog1.FileName + PathDelim);
    if not DirectoryIsWritable(LabelDestination.Caption) then
    //    ExportReadyToGo()
    //else
        Showmessage('Cannot write to that dir' + #10 + LabelDestination.Caption);
end;

(*
procedure TFormMain.SpeedSetSourceClick(Sender: TObject);
begin
    if SelectDirectoryDialog1.Execute then begin
        LabelSource.Caption := TrimFilename(SelectDirectoryDialog1.FileName + PathDelim);
        if  ComboExportMode.ItemIndex = cbBook then
            SetUpNoteBook();
	end;
    ExportReadyToGo();
end;    *)

procedure TFormMain.TabExportShow(Sender: TObject);
begin
    StatusBar1.SimpleText:= '';
    LabelErrorMessage.Caption := '';
    //SetUpSource(cbNG);   // will try NG first, then TB, then fall back to manual
    //ExportReadyToGo();
end;





function TFormMain.NumberChecked(CLB : TCheckListBox) : integer;
var
    Index : integer = 0;
begin
    result := 0;
    while Index < CLB.Count do begin
        if CLB.Checked[Index] then
            inc(Result);
        inc(Index);
    end;
end;

procedure TFormMain.SpeedProceedClick(Sender: TObject);
{var
    Exporter : TExportNote;  }
begin
    case PageControl1.ActivePage.TabIndex of
        0 : begin                                          // Thats Export
	              StatusBar1.SimpleText:= 'processing notes, please wait ....';
	              LabelErrorMessage.Caption := '';
	              Application.ProcessMessages;
                  if RadioExNotebook.checked then ProcessNoteBooks
                  else ProcessNotes;                        // Just process the ones in CheckListBox
                  StatusBar1.SimpleText:= 'processed notes';
	              {case comboExportMode.itemIndex of
	                  cbDirectory : ProcessDirectory;
	  		          cbBook      : ProcessNotebooks;
	  	          end;   }
	        end;
        1 : ImportProceed();
        //2 : DoNextCloud();             // not being used
	end;

end;


// -------------------------- I M P O R T ------------------------------------



procedure TFormMain.CheckListImportFilesClickCheck(Sender: TObject);
begin
    //ImportReadyToGo();
end;



procedure TFormMain.ImportProceed();
var
    NameList : TStringList;
    Import : TImportNotes;
    Index : integer = 0;
begin
    if NumberChecked(CheckListImportFiles) > 0 then begin
        try
            NameList := TStringList.Create;
            Import :=  TImportNotes.Create;
            while Index < CheckListImportFiles.Count do begin
                if CheckListImportFiles.Checked[Index] then
                    NameList.Add(string(CheckListImportFiles.Items.Objects[Index]));
                inc(Index);
            end;
            Import.ImportNames := NameList;
            Import.DestinationDir := LabelImportDestination.Caption;
            Import.FirstLineIsTitle := RadioTitleFirstLine.Checked;
            if RadioImMarkDown.checked then Import.Mode := 'markdown';
            if RadioImText.checked then Import.Mode := 'plaintext';
            StatusBar1.SimpleText:= ' ' + inttostr(Import.Execute) + ' files imported';
            LabelErrorMessage.Caption := Import.ErrorMsg;
        finally
            freeandnil(Import);
            freeandnil(NameList);
        end;
    end else showmessage('No files selected');
end;

procedure TFormMain.DisplayImportPossibilities();
var
    St : string;
begin
    CheckListImportFiles.Clear;
    if RadioImMarkDown.checked then begin
        if assigned(RegImFiles) then RegImFiles.free;
        RegImFiles := FindAllFiles(LabelImportSource.Caption, '*.md', false)     // Freed in Distroy
    end else if RadioImText.checked then
        RegImFiles := FindAllFiles(LabelImportSource.Caption, '*.txt;*.text', false);
    for St in RegImFiles do
                CheckListImportFiles.AddItem(ExtractFileName(St), Tobject(St));
    StatusBar1.SimpleText:= ' ' + inttostr(CheckListImportFiles.Count) + ' files found';
end;

function TFormMain.TestImportDestination() : boolean;
begin
    Label3.ShowHint := False;
    SpeedImportDestination.ShowHint := False;
    LabelImportDestination.ShowHint := False;
    if DirectoryIsWritable(LabelImportDestination.Caption) then begin
        if DefaultNotesDir = LabelImportDestination.Caption then begin
            LabelImportDestination.Hint := 'This is tomboy-ng Repository';
            SpeedImportDestination.Hint := 'This is tomboy-ng Repository';
            Label3.Hint := 'This is tomboy-ng Repository';
            Label3.ShowHint := True;
            SpeedImportDestination.ShowHint := True;
            LabelImportDestination.ShowHint := True;
        end;
    end else begin
        Showmessage('Cannot write to that dir' + #10 + LabelImportDestination.Caption);
        LabelImportDestination.Caption := '' ;
        LabelImportDestination.Hint := 'This MUST be set';
        SpeedImportDestination.Hint := 'This MUST be set';
        Label3.Hint := 'This MUST be set';
        Label3.ShowHint := true;
        SpeedImportDestination.ShowHint := true;
        LabelImportDestination.ShowHint := true;
    end;
    result := LabelImportDestination.Caption <> '';
end;

procedure TFormMain.SpeedImportDestinationClick(Sender: TObject);
begin
    if SelectDirectoryDialog1.Execute then begin
        LabelImportDestination.Caption := TrimFilename(SelectDirectoryDialog1.FileName + PathDelim);
        LabelImportSource.ShowHint := True;
        TestImportDestination();
	end;
end;

procedure TFormMain.SpeedImportSourceDirClick(Sender: TObject);
begin
    if SelectDirectoryDialog1.Execute then begin
        LabelImportSource.Caption := TrimFilename(SelectDirectoryDialog1.FileName + PathDelim);
//        LabelImportSource.Hint := LabelImportSource.Caption;
//        LabelImportSource.ShowHint := True;
        DisplayImportPossibilities();
	end;
end;

procedure TFormMain.RadioImMarkDownChange(Sender: TObject);
begin
    if RadioImMarkDown.Checked then
        DisplayImportPossibilities();
end;

procedure TFormMain.RadioImTextChange(Sender: TObject);
begin
    if RadioImText.Checked then
        DisplayImportPossibilities();
end;

procedure TFormMain.ButtonImSelectAllClick(Sender: TObject);
var
    Index : integer = 0;
begin
    Index := CheckListImportFiles.Count;
    while Index > 0 do begin
        dec(index);
        CheckListImportFiles.Checked[Index] := True;
    end;
end;

procedure TFormMain.ButtonImUnselectAllClick(Sender: TObject);
var
    Index : integer = 0;
begin
    Index := CheckListImportFiles.Count;
    while Index > 0 do begin
        dec(index);
        CheckListImportFiles.Checked[Index] := False;
    end;
end;

procedure TFormMain.TabImportShow(Sender: TObject);
begin
    StatusBar1.SimpleText:= '';
    LabelErrorMessage.Caption := '';
    TestImportDestination();
end;


end.

