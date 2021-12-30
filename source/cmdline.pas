unit cmdline;
{   Copyright (C) 2017-2020 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    ------------------ }

{
    The function 'Finished' is called before any forms are created, if it decides
    that the GUI is not needed, it will manage everything.
    Finished() will return True if
        * there has been a command line error
        * help is on command line
        * there was enough commandline to tell us what to do and it was done
    Finished() will return False if
        * There were no command line entries other than source and destination


-----------------------------------------------------------------

History
    July 2021  Newer CLI Mode, allow -s and -d and still proceed to GUI
    2021/12/30 Add ability to make man page from CLI, tidy help screens



    Note Title and File Names
    -------------------------
    The default, no relevent option, is -
    Import - Txt or Md file name becomes Title, Underscore to space.
             Set FirstLineIsTitle to True
    Export - Title becomes filename, add 'txt' or 'md', get rid of char unsuited
             to a filename, spaces and bad char all become underscore.
             Title is inserted as first line of new file, if md, marked as such.
             Set FileNameIsTitle to True


    If -t title-line is set, then we do something different -
    Import - Note title is the first line of the input file. In md, thats a title usually
             Set FirstLineIsTitle to False
    Export - File name is retained, probably an ID ?  But not always. Title is
             used as first line of the file, if md, its marked as a title.
             Set FileNameIsTitle to False

    Cleanup - no more than one consecuitive space in title.
            - When importing, if the first line of note content is same as title
              then remove the duplicate.

}



    { License - see tomboy-ng license information }

{$mode objfpc}{$H+}

interface

uses
        Classes, SysUtils;

type TToolMode = (
    ActNote2md, ActNote2txt, ActMd2note, ActTxt2note, ActGUI, ActExit,
    ActNote2Man,    // make a man page from note based on special template
    ActNote2POT,    // convert note to a pot file for translation
    ActJustDir      // we have just source and or destination directories, go to gui
    );

                // call this from project file, if true we don't need GUI
                // It is, in a way, the MAIN function, it decides are we GUI or
                // console app.
function Finished() : boolean;

function GetDefaultNoteDir(OldTB : boolean = false) : string;

implementation

uses LCLProc, Forms, LazFileUtils, FileUtil, export_notes, import_notes, note2po;

procedure ShowHelp();
begin
    debugln('');
    DebugLn('TomboyTools, a small set of tools for tomboy-ng (and Tomboy)');
    debugln('');
    debugln('With no options or with -s and/or -d preset will start the GUI');
    debugln('');
    debugln('Default source for export or destination for import is tomboy-ng notes dir.');
    debugln('Default destination of export or source for import is current directory.');
    debugln(' Also can use a KEY, "TOMBOY" or "GNOTE", to use their local defaults.');
    debugln(' -a --action=[note2md; note2txt; md2note; txt2note, note2man] - Required');
    debugln(' -s --source=[dir or KEY]   -d --destination=[dir or KEY]');
    debugln(' -b --notebook="Notebook Name"   For export and import.');
    debugln(' -n --note="Note Title"          For export only, don''t use with -b ');
    debugln(' -f --file="File Name"           Without path, name of file.');
    debugln(' -t --title-line   Retain filename, Note Title is first line of a file.');
    debugln(' -V --verbose      Tells us what its doing');
    debugln(' -e --examples     Print some example command lines');
    debugln(' --help            Print this, exit');

    debugln(' ');
end;

procedure ShowExamples();
begin
    debugln('Examples - ');
    debugln('$> tomboytools --action=note2md --destination=~/Desktop -b "My Notebook"');
    debugln(' Will export all notes in "My Notebook" from the tomboy-ng note repo');
    debugln(' to the desktop in markdown format.');
    debugln(' ');
    debugln('$> tomboytools -a md2note -d TOMBOY -s ~/Desktop -f myMDfile.md -t');
    debugln(' Will import all the markdown files on my desktop to my Tomboy repo using');
    debugln(' the first line as the title.');
    debugln(' ');
    debugln('$> tomboytools -a note2txt -n "Some Title"');
    debugln(' Will export a note from the tomboy-ng repo called "Some Title" as text');
    debugln(' and place it in the current working directory.');
    debugln(' ');
    debugln('$> tomboytools -a txt2note -b "Special Notes"');
    debugln(' Will import all the text files in the current directory into tomboy-ng.');
    debugln(' The file names (without extension) will be used for the note titles.');
    debugln(' Notes will be put in Special Notes notebook which MUST exist.');
    debugln(' ');
    debugln('$> tomboytools -s ./.  -d ./.');
    debugln(' Will use the GUI with both source and destination preset to current dir');
    debugln(' ');
    debugln('$> tomboytools  -a note2man  -f tomboy-ng-man.note');
    debugln(' Will convert a note callled tomboy-ng-man.note from current dir to a');
    debugln(' man page and save it in the current dir. It must have been prepared');
    debugln(' using the special Man Page Template available from TomboyTools Github');


    debugln(' ');
end;

function GetDefaultNoteDir(OldTB : boolean = false) : string;
begin
    {$IFDEF UNIX}
    Result := GetEnvironmentVariable('HOME') + '/.local/share/tomboy-ng/';
    {$ENDIF}
    {$IFDEF DARWIN}
    // try the correct place first, if not there, lets try the old, wrong place
    // if at neither, we go back to correct place.
    Result := GetEnvironmentVariable('HOME') + '/Library/Application Support/Tomboy-ng/Notes/';
    if DirectoryExistsUTF8(Result) then exit;
    Result := GetEnvironmentVariable('HOME') + '/.local/share/tomboy-ng/';
    if not DirectoryExistsUTF8(Result) then
        Result := GetEnvironmentVariable('HOME') + '/Library/Application Support/Tomboy-ng/Notes/';
    {$ENDIF}
    {$IFDEF WINDOWS}
    Result := GetEnvironmentVariable('APPDATA') + '\tomboy-ng\notes\';
    // %APPDATA%\Tomboy\notes\
    {$ENDIF}
    if OldTB then
        delete(Result, pos('-ng', Result), 3);
end;

    // must have either a single note title, ID or --all-notes
procedure ExportSomeNotes(Mode : TToolMode);
var
    Exporter : TExportNote;
    SrcDir, DestDir : string;
    ExPot : TExportPOT;
begin
    // when exporting, default src is tomboy-ng's repo. Default destination is current dir.
    if Application.HasOption('s', 'source') then begin
        SrcDir := Application.GetOptionValue('s', 'source');
        if SrcDir = 'TOMBOY' then
            SrcDir := GetDefaultNoteDir(True);
    end else
        SrcDir := GetDefaultNoteDir();
    if Application.HasOption('d', 'destination') then
        DestDir := Application.GetOptionValue('d', 'destination')
    else
        DestDir := GetCurrentDirUTF8();
    if not DirectoryIsWritable(DestDir) then begin
        debugln('ERROR, cannot write to ' + DestDir);
        exit();
    end;
    // Hmm, maybe check if there are notes in sourcedir ?
    if Mode = ActNote2POT then begin
        ExPot := TExportPOT.Create('tomboy-ng.note');
        if ExPot.ErrorString <> '' then
            debugln('Export to POT ' + ExPot.ErrorString);
        ExPot.Free;
        exit;
    end;
    Exporter := TExportNote.Create;
    try
        // Firstly, what are we exporting ?
        if Application.HasOption('n', 'note') then                              // all-notes is implied by Title and Notebook being empty
            Exporter.NoteTitle := Application.GetOptionValue('n', 'note')       // we don't bother with exporting notes by filename
        else if Application.HasOption('b', 'notebook') then
            Exporter.Notebook := Application.GetOptionValue('b', 'notebook');
        Exporter.FileNameIsTitle:= not Application.HasOption('t', 'title-line');
        Exporter.NoteDir := AppendPathDelim(SrcDir);
        Exporter.DestDir:= appendpathdelim(DestDir);
        // What format should they be executed in ?
        case Mode of
            ActNote2Txt : Exporter.OutFormat := 'text';
            ActNote2md  : Exporter.OutFormat := 'md';
            ActNote2Man : Exporter.OutFormat := 'man';
        end;
        (*if Mode = ActNote2Txt then
                Exporter.OutFormat := 'text'
        else Exporter.OutFormat := 'md';     *)
        if Application.HasOption('V', 'verbose') then begin
            debugln('Exporting from ' + SrcDir + ' to ' + DestDir);
            debugln('Notebook   : ' + Exporter.Notebook);
            debugln('Note Title : ' + Exporter.NoteTitle);
            if Mode = ActNote2Man then
                debugln('Note to become man : ' + Application.GetOptionValue('f', 'file'));
        end;
        if Mode = ActNote2Man then begin
            if not (Application.HasOption('f', 'file') and
                fileexists(SrcDir + Application.GetOptionValue('f', 'file'))) then begin
                    debugln('ERROR - note2man requires a -f file.note');
                    exit();
            end;
            Exporter.ExportOneFile(Application.GetOptionValue('f', 'file'))     // we MUST have -f filename set
        end
        else exporter.Execute;                 // Execute is always a bulk export
        if Exporter.ErrorMessage <> '' then
            Debugln('ERROR ' + Exporter.ErrorMessage);
	finally
        Exporter.Free;
	end;
end;

function CheckCmdOptions() : TToolMode;        // ret an action from TToolMode
var
    CmdLineErrorMsg : string;
    Action : string;
begin
    result := ActExit;
    if ParamCount = 0 then exit(ActGUI);         // no parameters, must be a GUI app
    CmdLineErrorMsg := Application.CheckOptions('Vha:s:d:b:n:f:te', 'examples verbose help action: source: destination: notebook: note: file: title-line');
    if CmdLineErrorMsg <> '' then begin
        debugln( CmdLineErrorMsg);
        showhelp();
        exit(ActExit);
	end;
(*    if Application.HasOption('a', 'action') and
        (Application.GetOptionValue('a', 'action') = 'note2man')  then     // We must have a file passed if note2man
            if not (Application.HasOption('f', 'file') and
                fileexists(Application.GetOptionValue('f', 'file'))) then begin
                    debugln('ERROR - note2man requires a -f file.note');
                    showhelp();
                    exit(ActExit);
                end;                    *)
    if Application.HasOption('help') then begin
        showhelp();
        exit(ActExit);
	end;
    if Application.HasOption('e', 'examples') then begin
        showExamples();
        exit(ActExit);
    end;

    // OK, its got cmd parameters without errors but is it correct ?
    if Application.HasOption('a', 'action') then
        Action := Application.GetOptionValue('a', 'action')
    else begin
        if Application.HasOption('s', 'source') or Application.HasOption('d', 'destination') then
            exit(ActGUI);           // Special case, we use the source and dest dir in the Export GUI, ignore in Import
        showhelp();
        exit(ActExit);
	end;
    // OK, we have an action, is it a valid one ?
    case Action of
        'note2md'  : Result := ActNote2md;
        'note2txt' : Result := ActNote2txt;
        'note2man' : Result := ActNote2Man;
        'md2note'  : Result := ActMd2note;
        'txt2note' : Result := ActTxt2note;
        'note2pot' : result := ActNote2POT;
    else begin
            showhelp();
            exit(ActExit);
        end;
    end
end;


procedure ImportSomeFiles(Mode : TToolMode) ;
var
    SrcDir, DestDir : string;
    FileList : TStringList;
    Importer : TImportNotes;
begin
    // when importing default destination is tomboy-ng's repo. Default source dir is current dir.
    if Application.HasOption('s', 'source') then
        SrcDir := Application.GetOptionValue('s', 'source')
    else
        SrcDir := GetCurrentDirUTF8();
    if Application.HasOption('d', 'destination') then begin
        DestDir := Application.GetOptionValue('d', 'destination');
        if DestDir = 'TOMBOY' then  DestDir := GetDefaultNoteDir(True);
    end else
        DestDir := GetDefaultNoteDir();
    if not DirectoryIsWritable(DestDir) then begin
        debugln('ERROR, cannot write to ' + DestDir);
        exit();
    end;
    FileList := FindAllFiles(SrcDir, '*.txt; *.text', False);
    debugln(FileList.Text);
    Importer := TImportNotes.create;
    try
        Importer.DestinationDir:= DestDir;
        Importer.ImportNames := FileList;
        if Mode = ActTxt2Note then
            Importer.Mode := 'plaintext';
        if Mode = ActMd2Note then
            Importer.Mode := 'markdown';

        if Application.HasOption('b', 'notebook') then
            Importer.NoteBook:= Application.GetOptionValue('b', 'notebook');

        if Application.HasOption('t', 'title-line') then
            Importer.FirstLineIsTitle:= True;;
        Importer.Execute();
    finally
        Importer.Free;
        FileList.Free;
    end;
end;

function Finished()  : boolean;
var
  AppMode : TToolMode = ActGUI;

begin
    result := true;
    AppMode := CheckCmdOptions();
    if AppMode = ActGUI then exit(False);
    if AppMode = ActExit then exit(True);
    // OK, we are here to do stuff, lets do it !
    if AppMode in [ActNote2md, ActNote2Txt, ActNote2Man, ActNote2POT] then
        ExportSomeNotes(AppMode);
    if AppMode in [ActTxt2note, ActMd2Note] then
        ImportSomeFiles(AppMode);
end;

end.

