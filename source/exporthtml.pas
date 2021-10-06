unit exporthtml;

{$mode objfpc}{$H+}

{   Copyright (C) 2017-2020 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    Exports a note in a subset of html, the new file(s) always get their name
    from the Note Title

    Create the object, give it a directory to look in, a file name (an ID without
    path or extension) and a directory to write to. If FollowLinks is set, other,
    linked html files will also be created in the same directory, with their own
    file name based on Note Title.

    Note that unlike CommnoMark, this unit saves its own files and does not get passed
    a StringList, so,output cannot be used programatically.

    HISTORY
    2021-07-27  Started up.
    2021/10/04  Added Tomboy's CSS in response to user request, seems to have a lot of whitespace ....
    2021/10/04  Tweaked the CSS just a little, cleaner HTML and nicer spacing ....

}

{x$define TOMBOYCSS}

interface

uses
        Classes, SysUtils;

type
      PNote=^TNote;
      TNote = record
            Title : string;
            FFN   : string;
            Required : boolean;
            Exported : boolean;
      end;

type
    { TNoteList }

    TNoteList = class(TFPList)
    private
        SearchTitleIndex  : integer;
        SearchExportIndex : integer;
        procedure DumpList;
      function Get(Index: integer): PNote;
                             // Reads all notes in dir, pops data into Items, stores FFN if Input note
        procedure BuildIndex(const SearchDir : string);
     public
         constructor Create(const SearchDir : string);
         destructor Destroy; override;
         function Add(ANote : PNote) : integer;
         function TitleExists(const Title: ANSIString): boolean;
                            // Returns the Title for the FFN of the InPut Note.
         function GetTitleForFFN(FFN : string) : string;
         procedure SetRequired(const Title : string);
         procedure SetExported(const Title : string);
                            // Initiate search for Title
         procedure StartTitleSearch;
                            //Initiate search for Title
         procedure StartExportSearch;
                            // Returns True if it found another title.
         function FindNextTitle(out Title : string) : boolean;
                                // Returns with the next note to be exported, has the FFN of the input Note.
         function FindNextExport(out FFN : string) : boolean;
         property Items[Index: integer]: PNote read Get; default;
     end;




type                { TExportHTML }

TExportHTML = class

    private
        OutFileName : string;   // FFName of (possibly first) output file.
        procedure AddHeaderFooter(STL: Tstringlist);
        procedure AddParaMarks(STL: Tstringlist);
        procedure ConvertTags(StL: TStringList);
        procedure MakeLocalLinks(const STL: TStringList; const CurrentTitle: string);
        procedure MarkupCodeBlock(STL: Tstringlist);
        procedure MarkUpHeading(STL: Tstringlist);
		//procedure SayDebug(st: string; Always: boolean=false);

                                { Passed FFName of a note,  loads into STL, Normalises, remove header and footer
                                Converts tags, marks up heading (in line 0), adds Para marks and header. The
                                first note through gets OutFileName as filename, remainder are generated from
                                their own title. }
        function GetHTMLcontent(InFFN: string; STL: TStringList): boolean;
        procedure MarkupLists(STL: Tstringlist);


    public
        NoteList : TNoteList;
        DebugMode : boolean;
        NotesDir : string;      // dir were we expect to find our TB notes, must have trailing path delim
        OutDir   : string;      // a dir to write our html files to, must have trailing path delim

        FollowLinks : boolean;  // Make html files for all notes that link to main one.
                        // Public : Accepts a file name (just ID, no path, no extension) of a
                        // note, will export it and any other notes, in the same dir that it
                        // appears to be linked to. Returns false on error.

        Title : string; // Set in MarkUpHeading() to a plain text title.

        function ExportFile(InFile: string): boolean;
        constructor Create();

end;




implementation

uses tb_utils, LazFileUtils{$ifdef LCL}, lazlogger {$endif}, laz2_DOM, laz2_XMLRead, notenormal, FileUtil;

const TagMax = 40;           // We must not access TagList[TagMax] !

var
    // The input tag is n*2, the replacement is n*2+1, we must not access TagList[TagMax] !
    // Adjust TagMax when you add a tag !
    TagList : array of string = ('<bold>', '<b>', '</bold>', '</b>',
            '<italic>', '<i>', '</italic>', '</i>',
            '<size:small>', '<small>', '</size:small>', '</small>',
            '<size:huge>', '<h2>', '</size:huge>', '</h2>',
            '<size:large>', '<h3>', '</size:large>', '</h3>',
            '<strikeout>', '<del>', '</strikeout>', '</del>',
            '<highlight>', '<mark>',  '</highlight>', '</mark>',
            '<monospace>', '<code>', '</monospace>', '</code>',
            '<list><list-item dir="ltr">','<li>',  '</list-item></list>','</li>',
            '<list-item dir="ltr">','<li>',  '</list-item>','</li>'
            );
    // Plain test Title between 1 + Title + 2 + Title + 3 + Title + 4 + '<h1>Title</h1>'Content .....
    CSSHeader1 : string = '<html><head><META http-equiv="Content-Type" content="text/html; charset=utf-8"><title>';
    CSSHeader2 : string = '</title><style type="text/css">'#10 + 'body { font-family:''FreeSans''; }'#10
        + 'h1 { font-size: xx-large;'#10 + '  font-weight: bold;'#10 + '  border-bottom: 1px solid black; }'#10
        {$ifndef TOMBOYCSS} + 'code {white-space: pre-wrap;}'#10 + 'li {margin-top: 7px;}'#10 {$endif}
        + 'div.note {'#10 + '  position: relative;'#10 + '  display: block;'#10 + '  padding: 5pt;'#10
        + '  margin: 5pt;'#10 + '  white-space: -moz-pre-wrap; /* Mozilla */'#10 + '  white-space: -pre-wrap;     /* Opera 4 - 6 */'#10
        + '  white-space: -o-pre-wrap;   /* Opera 7 */'#10
        {$ifdef TOMBOYCSS}+ '  white-space: pre-wrap;      /* CSS3 */'#10 {$endif}
        + '  word-wrap: break-word;      /* IE 5.5+ */ }'#10 + '</style></head><body><div class="note" id="';
    CSSHeader3 : string = '"><a name="';
    CSSHeader4 : string = '"></a>';


{ TNoteList }

// ===================== N O T E   L I S T  ===========================

function TNoteList.Get(Index: integer): PNote;
begin
    Result := PNote(inherited get(Index));
end;


procedure TNoteList.BuildIndex(const SearchDir: string);
var
        Info : TSearchRec;

    procedure GetNoteData();
    var
        NoteP : PNote;
        Doc : TXMLDocument;
	    Node : TDOMNode;
    begin
        new(NoteP);
        ReadXMLFile(Doc, SearchDir + Info.Name);
        try
            Node := Doc.DocumentElement.FindNode('title');
            NoteP^.Title := Node.FirstChild.NodeValue;          // This restores & etc.
            if TitleExists(NoteP^.Title) then begin
                debugln('*** ERROR *** duplicate note title, results unpredictable. [' + NoteP^.Title + ']');
                dispose(NoteP);
                exit;
            end;
            NoteP^.FFN := SearchDir + Info.Name;
            NoteP^.Required := False;
            NoteP^.Exported := False;
            // on error you should call dispose(NoteP);
            Add(NoteP);
        finally
            Doc.Free;
        end;
    end;

begin
  	if FindFirst(SearchDir + '*.note', faAnyFile, Info)=0 then
    		repeat
    		    GetNoteData();
    		until FindNext(Info) <> 0;
    	FindClose(Info);
end;

constructor TNoteList.Create(const SearchDir: string);
begin
    inherited Create;
    BuildIndex(SearchDir);
end;

destructor TNoteList.Destroy;
var
  I : integer;
begin
	for I := 0 to Count-1 do begin
    	dispose(Items[I]);
	end;
    inherited Destroy;
end;

function TNoteList.Add(ANote: PNote): integer;
begin
    result := inherited Add(ANote);
end;

function TNoteList.TitleExists(const Title: ANSIString): boolean;
var
  i : integer = 0;
begin
    while i < count do begin
        if Items[i]^.Title = Title then
            exit(True);
        inc(i);
    end;
    result := False;
end;

function TNoteList.GetTitleForFFN(FFN: string): string;
var
  i : integer = 0;
begin
    while i < count do begin
        if Items[i]^.FFN = FFN then
            exit(Items[i]^.Title);
        inc(i);
    end;
    result := '';
end;

procedure TNoteList.SetRequired(const Title: string);
var
    Index : longint;
begin
    for Index := 0 to Count do begin
        if Items[Index]^.Title = Title then begin
            Items[Index]^.Required := True;
            exit()
		end;
	end;
    SayDebugSafe('^^^ERROR^^^ - failed to find Title [' + Title + '] when setting Required');
end;

procedure TNoteList.SetExported(const Title: string);
var
    Index : longint;
begin
    for Index := Count-1 downto 0 do begin
        if Items[Index]^.Title = Title then begin
            Items[Index]^.Exported := True;
            exit()
		end;
	end;
    SayDebugSafe('^^^ERROR^^^ - failed to find Title [' + Title + '] when setting Exported');
end;

procedure TNoteList.StartTitleSearch;
begin
    SearchTitleIndex := 0;
end;

procedure TNoteList.StartExportSearch;
begin
    SearchExportIndex := 0;
end;

function TNoteList.FindNextTitle(out Title: string): boolean;
begin
    if SearchTitleIndex < count then begin
        Title := Items[SearchTitleIndex]^.Title;
        inc(SearchTitleIndex);
        result := true;
    end else Result := False;
end;


function TNoteList.FindNextExport(out FFN: string): boolean;
begin
    while SearchExportIndex < Count do begin
        if Items[SearchExportIndex]^.Required and (not Items[SearchExportIndex]^.Exported) then begin
            FFN := Items[SearchExportIndex]^.FFN;
            inc(SearchExportIndex);
            exit(True);
		end;
        inc(SearchExportIndex);
	end;
    Result := False;
end;

procedure TNoteList.DumpList;
var
    i : integer = 0;
begin
    while i < count do begin
    writeln('List - T=[', Items[i]^.Title + '] Req=' + booltostr(Items[i]^.Required, true)
                + ' E=' + booltostr(Items[i]^.Exported, true) + ' FFN=' + Items[i]^.FFN);
    inc(i);
    end;
end;



// ================= E X P O R T   H T M L ====================================

constructor TExportHTML.Create();
begin

end;

function TExportHTML.ExportFile(InFile : string) : boolean;
var
    STL : TStringList;
    FFN : string;
begin
    if FollowLinks then begin
        NoteList := TNoteList.Create(appendPathDelim(NotesDir));
    end
    else NoteList := Nil;
    OutFileName := OutDir + lowercase(GetTitleFromFFN(InFile, True)) +'.html';    // this only for main, index, file
    STL := TStringList.create;
    try
        Result := GetHTMLcontent(InFile, STL);
        if FollowLinks then begin
            // Also convert the daughter or linked notes
            NoteList.StartExportSearch;
            while NoteList.FindNextExport(FFN) do begin
                STL.Clear;
                GetHTMLcontent(FFN, STL);
                NoteList.StartExportSearch;     // to ensure we see all new ones
            end;
        end;
    finally
        STL.Free;
        if assigned(NoteList) then NoteList.Free;
    end;
end;

// Scans over the list, looking for any text that matches another note's title
// If it finds one, it marks the text as a html link and advises NoteList.
procedure TExportHTML.MakeLocalLinks(const STL : TStringList; const CurrentTitle : string);
var
    LineNo : integer = 0;
    TheTitle, St  :  string;
begin
    while LineNo < STL.Count do begin
        St := STL[LineNo];
        NoteList.StartTitleSearch;
        while NoteList.FindNextTitle(TheTitle) do begin
            if TheTitle = CurrentTitle then continue;
            if pos(lowercase(Title), lowercase(St)) > 0 then begin
                St := St.Replace(TheTitle, '<a href="' + lowercase(TB_MakeFileName(TheTitle))+'.html' + '">'
                            + TheTitle + '</a>', [rfReplaceAll, rfIgnoreCase]);
                StL.Delete(LineNo);
                StL.Insert(LineNo, St);
                NoteList.SetRequired(TheTitle);
            end;
        end;
        inc(LineNo);
    end;
end;

procedure TExportHTML.MarkUpHeading(STL : Tstringlist);
var
    St : string;
begin
    if STL.Count < 1 then exit;
    St := STL[0];
    {$ifndef TOMBOYCSS}
    St := St.Replace('<underline>', '');   // no flags needed, it only appears once
    St := St.Replace('</underline>', '');
    Title := St;
    {$endif}
    St := '<h1>' + St + '</h1>';
    StL.Delete(0);
    STL.Insert(0, St);
end;


function TExportHTML.GetHTMLcontent(InFFN : string; STL : TStringList): boolean;
var
    Normaliser : TNoteNormaliser;
begin
        if not FileExists(InFFN) then begin
            SayDebugSafe('ERROR, could not find ' + InFFN);
            exit(False);
        end;
        StL.LoadFromFile(InFFN);
        Normaliser := TNoteNormaliser.Create;
        Normaliser.NormaliseList(StL);
        Normaliser.Free;
        RemoveNoteMetaData(STL);// tt_utils
        if FollowLinks then begin
            MakeLocalLinks(STL, NoteList.GetTitleForFFN(InFFN));
            NoteList.SetExported(NoteList.GetTitleForFFN(InFFN));
            NoteList.DumpList;
        end;
        Stl.SaveToFile('DEBUG-A-beforeConvert.txt');
        ConvertTags(StL);
        Stl.SaveToFile('DEBUG-B-afterConvert.txt');
        MarkUpHeading(STL);
        MarkupLists(STL);
        {$ifndef TOMBOYCSS}
        Stl.SaveToFile('DEBUG-C-beforePara.txt');
        AddParaMarks(STL);
        Stl.SaveToFile('DEBUG-E-afterPara.txt');
        MarkupCodeBlock(STL);
        Stl.SaveToFile('DEBUG-F-afterCode.txt');
        {$endif}
        AddHeaderFooter(STL);
        if OutFileName = '' then
            if FollowLinks then
                OutFileName := OutDir + lowercase(GetTitleFromFFN(InFFN, True)) + '.html'
            else begin
                SayDebugSafe('*** ERROR *** Outfile name is blank');
                exit(false);
            end;
        if (Stl.Count < 2) then begin
            SayDebugSafe('Not saving ' + OutFileName + ' because its less than two lines');
            exit(False);
        end;
        if FileExistsUTF8(OutFileName) then
            DeleteFileUTF8(OutFileName);
        if FileExistsUTF8(OutFileName) then exit(False)
        else StL.SaveToFile(OutFileName);
        Result := True;
        OutFileName := '';      // That indicates we have done first one, any more need a name generated.
end;

procedure TExportHTML.AddParaMarks(STL : Tstringlist);
var
    i : integer = 0;
    TempSt, FirstTag : string;

    function ExcludedTag() : boolean;  // These do not get an auto <p>.</p> wrap
    begin
        result := false;
        case FirstTag of
            '<h1>', '<h2>', '<h3>', '<ul>', '<li>', '<cod' : exit(true);
        end;
    end;

begin
    while I < STL.Count do begin
        TempSt := STL[i];
        FirstTag := copy(TempSt, 1, 4);
        //if not ((FirstTag = '<h2>') or (FirstTag = '<h3>') or (FirstTag = '<h1>')) then begin
        if not ExcludedTag then begin
            TempSt := '<p>' + TempSt + '</p>';
            StL.Delete(i);
            STL.Insert(i, TempSt);
        end;
        inc(i);
    end;
end;

{ At this stage, list items have their <li>stuff</li> but we need to add <ul>block</ul>}
procedure TExportHTML.MarkupLists(STL : Tstringlist);
var
    i : integer = 0;
    InList : Boolean = false;
    TempSt, FirstTag : string;

    function IsListItem(St : string):boolean;      // True if passed st looks like a list item, may have leading whitespace
    var
        Index : integer = 0;
    begin
        Result := False;
        while Index < (St.Length) do begin         // no point in checking if we have less than necessary content left
            inc(index);
            if St[Index] = ' ' then continue;      // skip over any whitespace
            exit(copy(ST, Index, 4) = '<li>');
        end;
    end;

begin
    while I < STL.Count do begin
        //if copy(STL[i], 1, 4) = '<li>' then begin           // Its a list item
        if IsListItem(STL[i]) then begin
            TempSt := STL[i];
            while TempSt[1] = ' ' do delete(TempSt, 1, 1); // We know it has something after the whitespace
            StL.Delete(i);
            if not InList then begin
                STL.Insert(i, '<ul>' + TempSt);
                InList := True
            end else STL.Insert(i, TempSt);
        end else
            if InList then begin                            // its after a block of lists
                TempSt := STL[i];
                StL.Delete(i);
                STL.Insert(i, '</ul>' + TempSt);            // OK, thats on the next line, how bad is it ?
                InList := False
            end;
        inc(i);
    end;
end;


{ At this stage, list items have their <li>stuff</li> but we need to add <ul>block</ul>}
procedure TExportHTML.MarkupCodeBlock(STL : Tstringlist);
var
    i : integer = 0;
    InList : Boolean = false;
    TempSt, FirstTag : string;

begin
    while I < STL.Count do begin
        if copy(STL[i], 1, 6) = '<code>' then begin         // Its a code item
            TempSt := STL[i];
             StL.Delete(i);
            if not InList then begin                        // first code line of a block

                TempSt := TempSt.Replace('</code>', '');
                STL.Insert(i, '<p>' + TempSt);
                InList := True
            end else begin                                  // an intermediate code line
                TempSt := TempSt.Replace('<code>', '');
                TempSt := TempSt.Replace('</code>', '');
                STL.Insert(i, TempSt);                      // remove all code tags, we may need re-add last one
            end;
        end else
            if InList then begin                            // its fist line after a block of code
                TempSt := STL[i];
                StL.Delete(i);
                STL.Insert(i, '</code></p>' + TempSt);      // OK, thats on the next line, how bad is that ?
                InList := False
            end;
        inc(i);
    end;
end;




procedure TExportHTML.AddHeaderFooter(STL : Tstringlist);
begin
{    STL.Insert(0, '<body>');
    STL.Insert(0, '<html>');
    STL.Insert(0, '<!DOCTYPE>');    }
    Stl.Insert(0, CSSHeader1 + Title + CSSHeader2 + Title + CSSHeader3 + Title + CSSHeader4) ;
    StL.Add('</div>');
    StL.Add('</body>');
    StL.Add('</html>');
end;

procedure TExportHTML.ConvertTags(StL : TStringList);
var
    Index : integer = 0;
    i     : integer = 0;
    TempSt : string;
begin
    while Index < STL.Count do begin
        TempSt := STL[Index];
        i := 0;
        while (i*2) < TagMax do begin
            TempSt := TempSt.Replace(TagList[i*2], TagList[i*2+1], [rfReplaceAll]);
            inc(i);
        end;
        if TempSt <> STL[Index] then begin
            STL.Delete(Index);
            STL.Insert(Index, TempSt);
        end;
        inc(Index);
    end;
end;

end.

