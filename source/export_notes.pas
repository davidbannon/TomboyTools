unit export_notes;

{$mode objfpc}{$H+}    {$assertions on}

{ License - see tomboy-ng license information }

{ Will export a Tomboy note in a range of different formats.
  Uses CommonMark to do its MarkDown exporting.


}

interface
{   Copyright (C) 2017-2020 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    ------------------

    }

uses
        Classes, SysUtils;

type

{ UTB2md }

 { TExportNote }

 TExportNote = class
  private

    function ExportAll()               : boolean;
                                    // Expects just the ID or base filename,
    function ExportFile(ID: string)    : boolean;
    function ExportHTML(ID: string): boolean;
    function ExportMan(ID: string): boolean;
                    {  Expects just ID, ie basename, if STL <> nil, does not do any file
                       i/o stuff, just returns with the list populated with md content. }
    function ExportMD(ID: string; STL: TStringList=nil): boolean;
    function ExportText(ID: string)    : boolean;   // Expects just ID, ie basename
    function NoteInNoteBook(const FileName: string): boolean;   // Expects just ID, ie basename
    function ExportNoteBook() : boolean;

  public
    FileNameIsTitle : boolean;      // Else its the existing note base file name, the ID
    NoteTitle : string;             // If not empty we are exporting just this note.
    NoteDir   : string;             // Dir containg the note or notes to export
    DestDir   : string;             // where to save notes to.
    NoteFileName : string;          // Note Filename without path
    Notebook  : string;             // Name of a notebook who's members we'll convert.
    OutFormat : string;             // Either 'md' or 'text', maybe add more later.
    FollowLinks : boolean;
    ErrorMessage : string;          // Empty unless something bad happened.
    NotesProcessed : integer;       // How many things have we done something to ?
                            // Public : Exports just the FFN file passed, can call repeatedly
                            // The FFN must be consistent with NoteDir and end with '.note';
    function ExportOneFile(FFN: string): boolean;
                        { Public : Gets passed an ID and created, empty stringlist.
                          Will load indicate file as md. Observes only NoteDir
                          This is for the use of NextCloud Exporter and maybe only temporary. }
    function GetMDcontent(ID : string; STL : TstringList) : boolean;
    function Execute() : boolean;   // you know all you need, go do it.
    constructor Create;
    destructor Destroy; override;
end;

implementation

{ UTB2md }

uses LCLProc, laz2_DOM, laz2_XMLRead, LazFileUtils, TB_utils, tt_utils,
        commonmark, exporthtml, export2man;


function TExportNote.ExportAll(): boolean;
var
    Info : TSearchRec;
begin
    if FindFirst(NoteDir + '*.note', faAnyFile, Info) = 0 then
        try
            repeat
	            ExportFile(copy(Info.Name, 1, length(Info.name) - 5));
            until FindNext(Info) <> 0;
        finally
            FindClose(Info);
        end
    else begin
        ErrorMessage := 'ERROR : No notes were found in ' + NoteDir;
        Debugln(ErrorMessage);
        exit(false);
	end;
	result := true;
end;

function TExportNote.NoteInNoteBook(const FileName : string) : boolean;
var
    SLContent : tStringList;
begin
    Result := False;
    SLContent := TStringList.create;
    SLContent.LoadFromFile(NoteDir + FileName);
    if (FindInStringList(SLContent, '<tag>system:notebook:' + Notebook) > -1) then
        if FindInStringList(SLContent, '<tag>system:template</tag>') = -1 then      // Do not count Template Notes
            Result := True;
    SLContent.free;
end;

function TExportNote.ExportNoteBook() : boolean;
var
    Info : TSearchRec;
begin
    if FindFirst(NoteDir + '*.note', faAnyFile, Info) = 0 then
        try
            repeat
                if NoteInNoteBook(Info.Name) then begin
	                ExportFile(copy(Info.Name, 1, length(Info.name) - 5));
				end;
			until FindNext(Info) <> 0;
        finally
            FindClose(Info);
        end
    else begin
        ErrorMessage := 'ERROR : No notes were found in ' + Notebook;
        Debugln(ErrorMessage);
        exit(false);
	end;
    result := true;
end;

(*
function TExportNote.IDfromTitle(Title: string): string;
var
    Info : TSearchRec;
    LTitle : integer;
begin
    if FindFirst(NoteDir + '*.note', faAnyFile, Info) = 0 then
        try
            repeat
                if Title = TitleFromID(copy(Info.Name, 1, length(Info.name) - 5), False, LTitle) then
                    exit(copy(Info.Name, 1, length(Info.name) - 5));
            until FindNext(Info) <> 0;
        finally
            FindClose(Info);
        end
    else
    Debugln('ERROR : No notes was found with Title ' + Title);
    result := '';
end;

function TExportNote.TitleFromID(ID: string; Munge : boolean; out LenTitle : integer): string;
var
    Doc : TXMLDocument;
    Node : TDOMNode;
    Index : integer = 1;
begin
    if not FileExists(NoteDir + ID + '.note') then begin
        debugln('ERROR : File does not exist = '  + NoteDir + ID + '.note');
        LenTitle := 0;
        exit('');
	end;
	ReadXMLFile(Doc, NoteDir + ID + '.note');
    try
        Node := Doc.DocumentElement.FindNode('title');
        result := Node.FirstChild.NodeValue;
    finally
        Doc.free;
    end;
    if Munge then begin
        // remove char that don't belong in a file name
        while Index <= length(Result) do begin
                if Result[Index] in [ ' ', ':', '.', '/', '\', '|', '"', '''' ] then begin
                    Result[Index] := '_';
                end;

                inc(Index);
        end;
        Result := copy(Result, 1, 42);      // Because 42 is the meaning of life
	end;
    LenTitle := length(Result);
end;
*)


function TExportNote.ExportOneFile(FFN : string) : boolean;
begin
    result := ExportFile(ExtractFileNameOnly(FFN));
end;


function TExportNote.ExportFile(ID: string): boolean;
begin
    case OutFormat of
        'md', 'mark down', 'markdown', 'po file' : result := ExportMd(ID);
        'text', 'plain text', 'txt' : result := ExportText(ID);
        'html' : result := ExportHTML(ID);
        'man'  : result := ExportMan(ID);
    else  begin
        ErrorMessage := 'ERROR : unidentified outformat requested ' + OutFormat;
        Debugln(ErrorMessage);
        exit(False);
	    end;
	end;
    inc(NotesProcessed);
    Result := True;
end;

(*
function TExportNote.OnTagAtEnd(St : string) : integer;
var
    I, L : integer;
begin
    if St = '' then exit(0);
    L := length(st);
    if St[L] <> '>' then exit(0);
    i := 1;
    while St[L-i] <> '<' do begin       // march backwards until we find start of tag
        inc(i);
        if i > L then  begin
            debugln('ERROR : Overrun looking for tag start');
            exit(-1);
		end;
	end;
    if  St[L-i+1] = '/' then exit(0);   // not our problems, tags at the end should be 'off' tags.
    result := i+1;
end;

// Looks for an 'off' tag at the start of a line, they belong further up the list, 0 says none found
function TExportNote.OffTagAtStart(St : string) : integer;
var
    I : integer = 2;
    L : integer;
begin
    if (St = '') or (St[1] <> '<') or (St[2] <> '/') then   // Hmm, a single unescaed < on a line will crash
        exit(0);
    L := length(St);
    while St[i] <> '>' do begin
        inc(i);
        if i > L then begin
            debugln('ERROR : overrun looking for tag end, line =[' + st + ']');
            exit(-1);
		end;
	end;
    result := i;
end;                            *)

(*
        // Deals with 'on' tags that need to be moved down to the paras that they apply to
procedure TExportNote.MoveTagDown(const StL : TStringList; const StIndex, TagSize : integer);
var
    Tag : string;
begin
    Tag := copy(StL.strings[StIndex], length(StL.strings[StIndex])-TagSize+1, TagSize);
    StL.Insert(StIndex, copy(StL.strings[StIndex], 1, length(StL.strings[StIndex])-TagSize));
    StL.Delete(StIndex+1);
    StL.Insert(StIndex+1, Tag+StL.Strings[StIndex+1]);
    StL.Delete(StIndex+2);
end;

        // Deals with 'off' tags that need to be moved up to the para they apply to.
procedure TExportNote.MoveTagUp(const StL : TStringList; const StIndex : integer; var TagSize : integer);
var
    Tag : string;
begin
        // we have to detect when our line starts with  </note-content> or </text> and
        // terminate processing of this string, they are not text markup.
    Tag := copy(StL.strings[StIndex], 1, TagSize);
    if (Tag = '</note-content>') or (Tag = '</text>') then begin
        TagSize := 0;
        exit;
	end;
	StL.Insert(StIndex, copy(StL.Strings[StIndex], TagSize+1, length(StL.Strings[StIndex])));
    StL.Delete(StIndex+1);
    StL.Insert(StIndex-1, StL.strings[StIndex-1]+Tag);
    StL.Delete(StIndex);
end;

function TExportNote.MoveTagRight(var St: string): boolean;
var
    Index, TagStart, StartAt : integer;
begin
    Index := Pos('> ', St);
    if Index = 0 then exit(False);
    StartAt := 1;
    repeat
        Index := St.IndexOf('> ', StartAt);
        if Index < 0 then exit(False);
        TagStart := Index;
        while St[TagStart] <> '<' do dec(TagStart);
        if St[TagStart+1] = '/' then begin          // Not interested, an 'off' tag
            StartAt := Index+1;
            continue;
		end else break;
	until false;
    delete(St, Index+2, 1);
    insert(' ', St, TagStart);
    result := True;
end;

    { Will move a tag to the left if it has a space there, ret T if it moved one.}
function TExportNote.MoveTagLeft(var St: string): boolean;
var
    Index : integer;
begin
    Index := Pos(' </', St);
    if Index = 0 then exit(False);
    delete(St, Index, 1);
    insert(' ', St, St.IndexOf('>', Index)+2);          // 2 ? IndexOf rets a zero based and we want to go one past
    Result := true;
end;              *)


function TExportNote.GetMDcontent(ID: string; STL: TstringList): boolean;
begin
    self.OutFormat:= 'md';
    Result := ExportMD(ID, STL);
end;


// Rewrite much of this.

function TExportNote.ExportMD(ID : string; STL : TStringList = nil): boolean;
var
    StList : TStringList;
//    LTitle : integer;
//    Index : integer;
    OutFileName : string;
    CM : TExportCommon;
begin
    if not FileExists(NoteDir + ID + '.note') then exit(False);
    //debugln('export ' + NoteDir + ID + '.note to ' + DestDir + TitleFromID(ID, True, LTitle) + '.md');
    result := true;
    if STL = nil then
        StList := TStringList.Create
    else
        StList := STL;
    CM := TExportCommon.Create();
    try
        CM.NotesDir:= NoteDir;
        //CM.DoPOFile := (OutFormat = 'po file');
        CM.GetMDcontent(ID, StList);
        // ToDo : track success or otherwise here.
    finally
        CM.Free;
	end;

     try
        if FileNameIsTitle then
            // OutFileName := DestDir + TitleFromID(ID, True, LTitle)
            OutFileName := DestDir + GetTitleFromFFN(NoteDir + ID + '.note', True)   //    TitleFromID(ID, True, LTitle)
        else OutFileName := DestDir + ID;

        if (OutFormat = 'po file') then
            OutFileName := OutFileName + '.po'
        else
            OutFileName := OutFileName + '.md';
        if STL <> nil then exit(True);

        if FileExistsUTF8(OutFileName) then
            DeleteFileUTF8(OutFileName);
        if FileExistsUTF8(OutFileName) then begin
            ErrorMessage := 'Failed to overwrite ' + OutFileName;
            exit(False);
        end;
        try
            StList.SaveToFile(OutFileName);
        except on E: EStreamError do begin
                ErrorMessage := 'Save error against ' + OutFileName;
                exit(False);
            end;
        end;
    finally
        if STL = Nil then
            StList.free;
	end;

end;

function TExportNote.ExportMan(ID : string): boolean;
var
    Man : TExport2Man;
begin
    if not FileExists(NoteDir + ID + '.note') then exit(False);
    //debugln('export ' + NoteDir + ID + '.note to ' + DestDir + TitleFromID(ID, True, LTitle) + '.md');
    result := true;
{    if FileNameIsTitle then
        OutFileName := DestDir + TitleFromID(ID, True, LTitle)
    else OutFileName := DestDir + ID;
    OutFileName := OutFileName + '.html'; }
    Man := TExport2Man.Create();
    Man.NotesDir:= AppendPathDelim(NoteDir);
    Man.OutDir := AppendPathDelim(DestDir);
    try
        Result := Man.ExportFile(NoteDir + ID + '.note');
        self.ErrorMessage := Man.ErrorString;
        exit;
    finally
        Man.Free;
    end;
end;


function TExportNote.ExportHTML(ID : string): boolean;
var
    HTML : TExportHTML;
begin
    if not FileExists(NoteDir + ID + '.note') then exit(False);
    result := true;
    HTML := TExportHTML.Create();
    HTML.NotesDir:= AppendPathDelim(NoteDir);
    HTML.OutDir := AppendPathDelim(DestDir);
    HTML.FollowLinks := FollowLinks;
    try
        exit(HTML.ExportFile(NoteDir + ID + '.note'));     // does that make sense ?
    finally
        HTML.Free;
    end;
end;

(*
function TExportNote.RemoveTags(var St : string; out Tag : string) : boolean;
var
    TStart, TEnd : integer;
begin
    Result := True;
    Tag := '';
    TStart := pos('<', St);
    TEnd   := pos('>', St);
    if (TStart > 0) and (TEnd > 0) and (TEnd > TStart) then begin
        Tag := copy(St, TStart, TEnd - TStart +1);
        delete(St, TStart, TEnd - TStart +1)
	end
	else Result := False;
end;       *)

function TExportNote.ExportText(ID : string): boolean;      // ToDo : quality of exported text is horrible, needs newlines added.
var
    Doc : TXMLDocument;
    Node : TDOMNode;
    Content : string;
    OutFile: TextFile;
    Title : string;
//    LTitle : integer;
begin
    if not FileExists(NoteDir + ID + '.note') then exit(False);
    ReadXMLFile(Doc, NoteDir + ID + '.note');
    try
        try
            Node := Doc.DocumentElement.FindNode('text');
            Content := Node.TextContent;
        except on E: Exception do debugln(ID + ' Bad things ' + E.Message);     // ToDo : yeah !
		end;
    finally
        Doc.free;
    end;
    Title := GetTitleFromFFN(NoteDir + ID + '.note', True);
    //AssignFile(OutFile, DestDir + TitleFromID(ID, True, LTitle) + '.txt');      // ToDo : this does not respect not FileNameIsTitle.
    AssignFile(OutFile, DestDir + Title + '.txt');
    insert(LineEnding+LineEnding, Content, length(Title)+1);
    try
        try
            Rewrite(OutFile);
            writeln(OutFile, RestoreBadXMLChar(Content));
		finally
            CloseFile(OutFile);
		end;
    except
        on E: EInOutError do begin
                debugln('File handling error occurred updating clean note location. Details: ' + E.Message);
                exit(False);
            end;
    end;
end;


// Call this if we have an Notebook already poked into class. Or want to export All
// It used to support a Title as well but no more !

function TExportNote.Execute(): boolean;
{var
    ID : string; }
begin
    Result := False;
    NotesProcessed := 0;
{    if (Notebook = '') and (self.NoteTitle = '') then
        exit(ExportAll());      }
    if NoteTitle <> '' then begin           // we have a title, just do this note.
        SayDebugSafe('*** ERROR *** TEXPORTNOTE.EXECUTE called with NOTETITLE set.');
        exit(False);
    end;
    if (Notebook = '') then
        Result := ExportAll()
    else Result := ExportNoteBook();
{

        ID := IDfromTitle(NoteTitle);
        if ID = '' then
            debugln('ERROR : Unable to find note with Title = ' + NoteTitle)
        else
            Result := ExportFile(ID);
    end else                                // we must have a Notebook
        result := ExportNoteBook();             }
end;


constructor TExportNote.Create;
begin
    NotesProcessed := 0;
end;

destructor TExportNote.Destroy;
begin
	inherited Destroy;
end;

end.

