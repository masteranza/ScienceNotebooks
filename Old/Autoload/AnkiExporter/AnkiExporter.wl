(* ::Package:: *)

(* ::Title:: *)
(*Anki Exporter*)


(* ::Author:: *)
(*author: Michal Mandrysz*)


(* ::Affiliation:: *)
(*Marian Smoluchowski Institute of Physics, Jagiellonian University, Krakow, Poland*)


(* ::Abstract:: *)
(*This package allows for exporting mathematica highlights as Anki cloze cards using AnkiConnect protocol.*)


(* ::Text:: *)
(*Version: 2.0.0*)


(* ::Input::Initialization:: *)
BeginPackage["AnkiExporter`",{"PackageUtils`"}];

(*Highlight Selection  key shortcut and menu*)
FrontEndExecute[
 FrontEnd`AddMenuCommands[
  "DuplicatePreviousOutput", {Delimiter, 
   MenuItem["Anki Highlight", 
    FrontEndExecute@Module[{nb, t}, nb = InputNotebook[];
      t = CurrentValue[NotebookSelection[nb], "SelectionData"];
      If[(MatchQ[t, GraphicsBox[BlankSequence[]]]), 
       NotebookWrite[nb, 
        ReplaceAll[
         t, {GraphicsBox[Pattern[E, Blank[TagBox]], 
            Pattern[D, BlankNullSequence[]]] :> 
           GraphicsBox[{E, {FaceForm[{RGBColor[0.88, 1, 0.88], 
                Opacity[0.6]}], 
              RectangleBox[{1.0, 1.0}, {75.0, 75.0}]}}, D], 
          GraphicsBox[{Pattern[E, 
              BlankSequence[]], {FaceForm[{RGBColor[0.88, 1, 0.88], 
                Opacity[0.6]}], Pattern[RR, BlankSequence[]]}, 
             Pattern[C, BlankNullSequence[]]}, 
            Pattern[D, BlankNullSequence[]]] :> 
           GraphicsBox[{E, 
             C, {FaceForm[{RGBColor[0.88, 1, 0.88], Opacity[0.6]}], 
              RectangleBox[{6.0, 6.0}, {80.0, 80.0}], RR}}, D], 
          GraphicsBox[{Pattern[C, BlankNullSequence[]]}, 
            Pattern[D, BlankNullSequence[]]] :> 
           GraphicsBox[{C, {FaceForm[{RGBColor[0.88, 1, 0.88], 
                Opacity[0.6]}], 
              RectangleBox[{1.0, 1.0}, {2.0, 2.0}]}}, D]}]], 
       CurrentValue[NotebookSelection[nb], Background] = 
        If[CurrentValue[NotebookSelection[nb], Background] == None, 
         LightGreen, None]];
      ], MenuKey["d", Modifiers -> {"Command"}], 
    System`MenuEvaluator -> Automatic]}]];
	
ExportToAnki::usage ="Function exporting selected sections of the notebook to Anki using cloze notes.";
ExportToCloud::usage="Exports Notebook to the Wolfram Cloud. Path follows the directory structure.";

Begin["`Private`"];

(*Anki Connect: Pass Action and param*)
AnkiRequest[action_,params_:<||>]:=Block[{req,json},PrintToConsole[params];json=ImportString[ExportString[<|"action"->action,"version"->6,"params"->params|>,"JSON","Compact"->False],"Text"];PrintToConsole[json];req=HTTPRequest[<|"Scheme"->"http","Domain"->"localhost","Port"->8765,Method -> "POST","Body"->json|>];URLRead[req,"Body"]
];

(*Anki Connect: Pass deck name then for params and tags (opt)*)
PrepareAnkiNote[deckName_,cellID_,clozed_,title_,link_,tags_:{}]:={"deckName"->deckName,"modelName"->"MathematicaCloze","fields"->{"CellID"->ToString[cellID],"Text"->clozed,"Extra"->title,"Link"->link},"options"->{"allowDuplicate"->False},"tags"->tags};

(*Anki Connect: Pass deck name then all notes*)
AddOrUpdateNotes[deck_,rawNotes_]:=Block[{res,resi,toUpdate,ids,ankiIds},
res=(ImportString[AnkiRequest["addNotes",{"notes"->(PrepareAnkiNote[deck,#[[1]],#[[2]],#[[3]],#[[4]],#[[5]]]&/@rawNotes)}],"JSON"]);
PrintToConsole[res];
res=("result"/.res);
toUpdate=#[[2]]&/@Select[Thread[{res,rawNotes}],#[[1]]==Null&];
PrintToConsole["Need to update "<>ToString[Length[toUpdate]]<> " notes"];
If[Length[toUpdate]>0,res={"actions"->({"action"->"findNotes","params"->{"query"->"CellID:"<>ToString[#[[1]]]}}&/@toUpdate)};ankiIds=#[[1]]&/@("result"/.ImportString[AnkiRequest["multi",res],"JSON"]);toUpdate=Thread[{ankiIds,toUpdate}];PrintToConsole[ankiIds];PrintToConsole[toUpdate];res={"actions"->({"action"->"updateNoteFields","params"->{"note"->{"id"->ToString[#[[1]]],"deckName"->deck,"modelName"->"MathematicaCloze","fields"->{"CellID"->ToString[#[[2,1]]],"Text"->#[[2,2]],"Extra"->#[[2,3]],"Link"->#[[2,4]]},"options"->{"allowDuplicate"->False}}}}&/@toUpdate)};ImportString[AnkiRequest["multi",res],"JSON"]];
(*PrintToConsole[ImportString[AnkiRequest["updateNoteFields",{"note"\[Rule]{"id"\[Rule]#[[1]],"fields"\[Rule]{"CellID"\[Rule]ToString[#[[2,1]]],"Text"\[Rule]#[[2,2]],"Extra"\[Rule]#[[2,3]],"Link"\[Rule]#[[2,4]]}}}],"JSON"]]&/@toUpdate;*)
(*PrintToConsole[ImportString[AnkiRequest["notesInfo",{"notes"\[Rule]toUpdate[[All,1]]}],"JSON"]];*)
];

(*Joins Anki clozes in TeX string*)
TeXFix[what_]:=StringReplace[StringReplace[what,{
"}}"~~WhitespaceCharacter...~~"{{c"~~Shortest[___]~~"::"->"",
"\\({{c"~~Shortest[c__]~~"::"~~Shortest[d__]~~"}}\\)":>("{{c"<>c<>"::\\("<>d<>"\\)}}")}],
{"}}"~~WhitespaceCharacter...~~"{{c"~~Shortest[___]~~"::"->"","\\)\\("->""}];

(*Embedes Zotero references using latex tag*)
EmbedEq[what_]:=Replace[Replace[what,{Cell[BoxData[ButtonBox[___,TaggingRules->{___,"citekey"->d_,___,"zotero"->c_,___},___],___],___]:>("<a href=\""<>c<>"\">["<>d<>"]</a>"),ButtonBox[___,Tooltip->DynamicBox[c__,UpdateInterval->\[Infinity]],___]:>TempAnki[c]},Infinity],{TempAnki[RowBox[{_,d___}]]:>RowBox[{d}]},Infinity];

(*Fixes figures embedding*)
FixFigures[what_]:=Replace[what,{BoxData[GraphicsBox[C___]]:>BoxData[FormBox[GraphicsBox[C],TraditionalForm]]},Infinity];

(*Cloud Export: Open/Closes all cells*)
openCloseAll[nb_,target_String,to:(Open|Closed)]:=Do[SelectionMove[cell,All,CellGroup,AutoScroll->False];
With[{content=Block[{$Context="FrontEnd`",$ContextPath={"System`"}},NotebookRead[nb]],from=to/.{Closed->Open,Open->Closed}},If[MatchQ[content,Cell[CellGroupData[{Cell[_,target,___],__},from]]],NotebookWrite[nb,Cell[CellGroupData[content[[1,1]],to]],AutoScroll->False]]];,{cell,Cells[CellStyle->target]}];
cell[content_,opts___]:=Cell[BoxData[ToBoxes[content]],opts,ShowStringCharacters->False];
ExportToCloud[]:=Block[{s,fn,cn,su},
fn=StringReplace[NotebookFileName[EvaluationNotebook[]],e___~~"/Knowledge/" ~~ f___ :> "/Knowledge/"<>f];
cn="user:"<>StringSplit[CloudConnect[],"@"][[1]]<>fn;
s=CloudDeploy[EvaluationNotebook[],cn];
su=Quiet@CloudDeploy[URLDispatcher[{"/"~~EndOfString:>Delayed@CloudImport@s,"/"~~ base:Repeated[DigitCharacter,20]:>Delayed@ExportForm[Notebook[Append[First@CloudGet@s,cell@EmbeddedHTML["<script> function openCloseGroup() { wolfram.cloud.parentNotebook.evaluateExpression({     expression: 'FrontEndExecute[NotebookLocate[\""<>base<>"\"]]' }); } </script> <body onload='openCloseGroup()'>"]],(Rest@s)/.{Notebook->Sequence}],"CloudCDF"]}],StringDrop[cn,-3]];
StringDrop[StringSplit[CloudConnect[],"@"][[1]]<>fn,-3]
];
ExportToAnki[sync_:True]:=Module[{separator,styleTags,cells,sections,subsections,subsubsections,subsubsubsections,allinfo,cellids,celltags,data,ids,cloze,matchEq,encoding,eqCloze,GetTOC,exported,filtered,splited,marked,paths,fixed,final,threaded,deck,title, base,dat,ndir,tempPicPath, allspecial,npath,backupc,cloudex},
cloudex=If[!$CloudConnected&&$WolframID===None,If[CloudConnect[]===$Failed||CloudConnect[]===$Canceled,False,True],True];
separator="#";
ShowStatus["Export starts"];
PrintToConsole["Export starts"];
ExportString["Exp","TeXFragment"];
(*System`Convert`TeXFormDump`maketex["\[LeftSkeleton]"]="\\ll ";
System`Convert`TeXFormDump`maketex["\[RightSkeleton]"]="\\gg ";*)
System`Convert`TeXFormDump`maketex["\[OAcute]"]="\[OAcute]";
System`Convert`TeXFormDump`maketex["\[CloseCurlyQuote]"]="'";
System`Convert`TeXFormDump`maketex["\:015b"]="\:015b";
System`Convert`TeXFormDump`maketex["\[CAcute]"]="\[CAcute]";
System`Convert`TeXFormDump`maketex["\:0119"]="\:0119";
System`Convert`TeXFormDump`maketex["\:0105"]="\:0105";
System`Convert`TeXFormDump`maketex["\[LSlash]"]="\[LSlash]";
System`Convert`TeXFormDump`maketex["\:017c"]="\:017c";
System`Convert`TeXFormDump`maketex["\:017a"]="\:017a";
System`Convert`TeXFormDump`maketex["\:0144"]="\:0144";
(*System`Convert`TeXFormDump`maketex["&"]="\\$ ";*)
System`Convert`TeXFormDump`maketex["~"]="\\sim ";
System`Convert`TeXFormDump`maketex["\[Perpendicular]"]="\\perp ";
System`Convert`TeXFormDump`maketex["\[TensorWedge]"]="\\wedge ";System`Convert`TeXFormDump`maketex["\[Wedge]"]="\\wedge ";
System`Convert`TeXFormDump`maketex["\[TensorProduct]"]="\\otimes ";
System`Convert`TeXFormDump`maketex["\[GreaterTilde]"]="\\gtrsim ";
System`Convert`TeXFormDump`maketex["\[LineSeparator]"]="\n";
System`Convert`TeXFormDump`maketex[":="]=":=";

(*nie zamieniaj zwyk\[LSlash]ego tekstu*)
System`Convert`CommonDump`ConvertTextData[contents_String,toFormat_,toFormatStream_,conversionRules_,opts___?OptionQ]:=Module[{fpre,frule,fpost,pstyle,popts,str=contents},System`Convert`CommonDump`DebugPrint["CONVERTCOMMON:ConvertTextData-general content: ",contents];
pstyle=System`Convert`CommonDump`ParentCellStyle/.{opts}/.System`Convert`CommonDump`ParentCellStyle->"";
popts=Flatten[System`Convert`CommonDump`ParentOptions/.List/@{opts}/.System`Convert`CommonDump`ParentOptions->{}];
System`Convert`CommonDump`DebugPrint["pstyle: ",pstyle];
{fpre,frule,fpost}=System`Convert`CommonDump`ConvertFormatRule[pstyle/.conversionRules,False];
System`Convert`CommonDump`DebugPrint["{fpre, frule, fpost}: ","InputForm"[{fpre,frule,fpost}]];
If[frule===Automatic,frule=System`Convert`CommonDump`ConvertText[#1,toFormat,opts]&];
If[!(System`Convert`CommonDump`ShowQuotesQ[pstyle]||TrueQ[System`Convert`CommonDump`ShowQuotes/.Flatten[{opts}]]||TrueQ[ShowStringCharacters/.popts]),str=System`Convert`CommonDump`RemoveQuotes[str]];
System`Convert`CommonDump`DebugPrint["str: ",frule];
(*If[!TrueQ[System`Convert`CommonDump`ConvertText/.Flatten[{opts}]],str=frule[str]];*)
System`Convert`CommonDump`DebugPrint["str: ",str];
System`Convert`CommonDump`DebugPrint["CONVERTCOMMON-ConvertTextData.  Writing the string. HIJACK"];
System`Convert`CommonDump`DebugPrint["------------------------------------------"];
WriteString[toFormatStream,str];];
(*przekieruj zwyk\[LSlash]y tekst*)
System`Convert`CommonDump`ConvertTextData[contents_,toFormat_,toFormatStream_,conversionRules_,others___]:=Module[{cell},
If[MatchQ[contents,Anki[_,_String]],WriteString[toFormatStream,contents/.{Anki[nr_,s_String]:>("{{c"<>ToString[nr]<>"::"<>StringReplace[s,"}}"->"} }"]<>" }} ")}];,
System`Convert`CommonDump`DebugPrint["CONVERTCOMMON-ConvertTextData of unknown content: ",contents];
cell=Cell[BoxData[contents],""];
System`Convert`CommonDump`PreConvertCell[cell,toFormat,toFormatStream,conversionRules,(*System`Convert`CommonDump`*)inlineCell->True,others];
System`Convert`CommonDump`ConvertCell[cell,toFormat,toFormatStream,conversionRules,(*System`Convert`CommonDump`*)inlineCell->True,others];
System`Convert`CommonDump`PostConvertCell[cell,toFormat,toFormatStream,conversionRules,(*System`Convert`CommonDump`*)inlineCell->True,others];]];
(*blokowanie zamiany na unicode wrappers - raczej nie potrzebne*)
(*System`Convert`TeXFormDump`maketex[str_String /; StringLength[str] === 1] := (System`Convert`CommonDump`DebugPrint["------------------------------------"];
    	System`Convert`CommonDump`DebugPrint["maketex[str_String/;(StringLength@str===1)]"];
    	System`Convert`CommonDump`DebugPrint["str: ", str];
  str  	
(*If[$Language === "Japanese" || 
        MemberQ[{"ShiftJIS", "EUC"}, $CharacterEncoding], str, 
      "\\unicode{" <> System`Convert`TeXFormDump`ToCharacterHexCode[str] <> "}"]*));*)

(*Anki markup*)
System`Convert`TeXFormDump`maketex[Anki[nr_,boxes_]]:=(System`Convert`CommonDump`DebugPrint["------------------------------------"];
System`Convert`CommonDump`DebugPrint["maketex[Anki[nr_, boxes__]]"];
System`Convert`CommonDump`DebugPrint["boxes: ",boxes];
If[StringQ[boxes],"{{c"<>ToString[nr]<>"::"<>StringReplace[boxes,"}}"->"} }"]<>" }} ",
"{{c"<>ToString[nr]<>"::"<>StringReplace[System`Convert`TeXFormDump`MakeTeX[boxes],"}}"->"} }"]<>" }} "]);
(*fixes an old MMA bug *)
System`Convert`TeXFormDump`$TeXDelimiterReplacements = System`Convert`TeXFormDump`$TeXDelimiterReplacements /. {"\\left| " | "\\right| " -> "|","\\left\\| " | "\\right\\| " -> "\\| "};
deb=Convert`TeX`BoxesToTeX[""];
System`Convert`TeXFormDump`$TeXDelimiterReplacements = System`Convert`TeXFormDump`$TeXDelimiterReplacements /. {"\\left| " | "\\right| " -> "|","\\left\\| " | "\\right\\| " -> "\\| "};
(*use defaults*)
myBoxRule[TemplateBox[boxes_,rule_]]:=System`Convert`TeXFormDump`maketex[TemplateBox[boxes,rule]];
myBoxRule[TemplateBox[boxes_,rule_,ruleb_]]:=System`Convert`TeXFormDump`maketex[TemplateBox[boxes,rule,ruleb]];

(*rules for 'at' notions*)
myBoxRule[TemplateBox[{boxes_,_,lima_,limb_},___]]:=StringJoin["\\left."<>System`Convert`TeXFormDump`maketex[boxes],"\\right|_{",System`Convert`TeXFormDump`maketex[lima],"}^{",System`Convert`TeXFormDump`maketex[limb],"}"];

myBoxRule[FormBox[TemplateBox[{boxes_,_,lima_,limb_},___],___]]:=StringJoin["\\left."<>System`Convert`TeXFormDump`maketex[boxes],"\\right|_{",System`Convert`TeXFormDump`maketex[lima],"}^{",System`Convert`TeXFormDump`maketex[limb],"}"];

myBoxRule[FormBox[C__]]:=System`Convert`TeXFormDump`maketex[FormBox[C]];
EqBoxToTeX[c_]:=Convert`TeX`BoxesToTeX[c,"BoxRules"->{box:(_TemplateBox|_FormBox):>(myBoxRule[box])}];

ShowStatus["Export to Anki begins..."];
If[NotebookDirectory[]===$Failed,ShowStatus["Nothing to export"]; Abort[]];
tempPicPath=Quiet@Check[CreateDirectory["~/Dropbox/Anki/Ranza/collection.media/",CreateIntermediateDirectories-> True],"~/Dropbox/Anki/Ranza/collection.media/",CreateDirectory::filex];

FixStrings[data_]:=StringReplace[data,{"\[Lambda]":>"\(\\lambda\)","\[Dash]":>"-","\[Rule]":>"\(\\rightarrow\)"}];
(*TeXFix[what_]:=StringReplace[what,{"\)}}\(\)"\[Rule] "\)}}",("\\text{"~~c:Except["}"]..~~"}"):>(ToString@c)}];*)
TeXFixPoor[what_]:=StringReplace[what,{"\)}}\(\)"-> "\)}}"}];
EncodingFix[what_]:=FromCharacterCode[ToCharacterCode[what],"UTF8"];
ToTex[what_,n_:1]:=Convert`TeX`BoxesToTeX[what, "BoxRules"->{
"\[Transpose]":>"^{\\mathsf{T}}",
"\[ConjugateTranspose]":>"^{\\dagger} ",
"\[HermitianConjugate]":>"^{\\dagger} ",
"\[Conjugate]":>"^{*} ",
"\[OAcute]":> "\[OAcute]",
"\[CapitalOAcute]":> "\[CapitalOAcute]",
"\:015b":> "\:015b",
"\:015a":> "\:015a",
"\[CAcute]":> "\[CAcute]",
"\[CapitalCAcute]":> "\[CapitalCAcute]",
"\:0119":> "\:0119",
"\:0118":> "\:0118",
"\:0105":> "\:0105",
"\:0104":> "\:0104",
"\[LSlash]":> "\[LSlash]",
"\[CapitalLSlash]":> "\[CapitalLSlash]",
"\:017c":> "\:017c",
"\:017b":> "\:017b",
"\:017a":>"\:017a",
"\:0179":>"\:0179",
"\:0144":>"\:0144",
"\:0143":>"\:0143",

FormBox[GraphicsBox[{C__},E___,ImageSize->D_],__]|GraphicsBox[{C__},E___,ImageSize->D_]:>("\\includegraphics[natwidth="<>ToString[(First@D)/4]<>",natheight="<>ToString[(First@D)/4]<>"]{"<>FileNameTake@Export[tempPicPath<>"f"<>ToString[Hash[Graphics[{C},E]]]<>".png",Graphics[{C},E],ImageSize->{First@D,First@D}]<>"} "),
FormBox[GraphicsBox[___],___]:> "",
GraphicsBox[___]:> "",

StyleBox[D_,Background->LightGreen]:>"\\color[HTML]{1111FF}{{c"<>ToString[n]<>"::"<>StringReplace[ToTex[D],{"{{":>" { { ","}}":>" } } "}]<>" }}\\color[HTML]{000000}"}];
(*the above is old*)
cells=Cells[EvaluationNotebook[],CellStyle->{"Text","EquationNumbered","Equation",(*"Figure",*)"Item1","Item2","Item3","Item1Numbered","Item2Numbered","Item3Numbered","Example","Exercise","Solution","Question","Remark","Comment","Theorem","Proof","Axiom","Definition","Lemma","Corollary"}];
title=First@(Cases[NotebookGet@EvaluationNotebook[],Cell[name_,style:"Title",___]:>name,Infinity]/.{}-> {""});
ShowStatus["Gathering section info..."];
sections=CurrentValue[#,{"CounterValue","Section"}]&/@cells;
subsections=CurrentValue[#,{"CounterValue","Subsection"}]&/@cells;
subsubsections=CurrentValue[#,{"CounterValue","Subsubsection"}]&/@cells;
subsubsubsections=CurrentValue[#,{"CounterValue","Subsubsubsection"}]&/@cells;

celltags=Riffle[If[MatchQ[CurrentValue[#,{"CellTags"}],_String],{CurrentValue[#,{"CellTags"}]},CurrentValue[#,{"CellTags"}]]," "]&/@cells;

allinfo=DeleteCases[Replace[Thread[{sections,subsections,subsubsections,subsubsubsections}],{x___,0...}:>{x},1],0,2];
ShowStatus["Gathering table of contents"];
GetTOC=Cases[NotebookGet@EvaluationNotebook[],Cell[name_,style:"Section"|"Subsection"|"Subsubsection"|"Subsubsubsection",___]:>{style,Convert`TeX`BoxesToTeX[ name,"BoxRules"->{D_String:>D}]},Infinity]/.{"Subsubsubsection",x_}:>x[]//.
{x___,{"Subsubsection",y_},z:Except[_List]...,w:PatternSequence[{_,_},___]|PatternSequence[]}:>{x,y[z],w}//.{x___,{"Subsection",y_},z:Except[_List]...,w:PatternSequence[{_,_},___]|PatternSequence[]}:>{x,y[z],w}//.{x___,{"Section",y_},z:Except[_List]...,w:PatternSequence[{_,_},___]|PatternSequence[]}:>{x,y[z],w};
ShowStatus["Preparing paths..."];
paths=(title<>"/"<>Riffle[Head/@(GetTOC[[#/.List->Sequence]]&/@Reverse@NestList[Most,#,Length[#]-1]),"/"])&/@allinfo;
ShowStatus["Extracting data... (1/3)"];
(*line splitter extended to 5000 characters*)
System`Convert`TeXDump`cleanUpFile[fileName_String] := 
 Module[{streamIn, streamOut, insideComment = False, charNum = 0, 
   scratchFileName, charIn, charLast = ""}, 
   	 {streamIn, streamOut} = {OpenRead[fileName], System`ConvertersDump`Utilities`OpenTempFile[CharacterEncoding -> {}]};
  	While[(charIn = Read[streamIn, Character]) =!= EndOfFile, 
   If[charLast =!= "\\" && charIn === "%", insideComment = True];
   	If[charIn === "\n", insideComment = False;
    	charNum = -1];
   	If[charNum >= 5000 && Or @@ (charIn === #1 &) /@ {"\t", " "}, 
    WriteString[streamOut, If[! insideComment, "\n", "\n%"]];
    	charNum = 0, WriteString[streamOut, charIn];
    	++charNum];
   	charLast = charIn;];
  	Scan[Close, {streamIn, streamOut}];
  	DeleteFile[fileName];
  	CopyFile[scratchFileName = First[streamOut], fileName];
  	DeleteFile[scratchFileName];];
base=TeXFix[ImportString[ExportString[n=0;EmbedEq[FixFigures@Replace[NotebookRead[#],{StyleBox[C_String,Background->RGBColor[0.88, 1, 0.88]]:>(n+=1;Anki[n,C]),StyleBox[C___,Background->RGBColor[0.88, 1, 0.88],D___]:>(n+=1;Anki[n,StyleBox[C,D]]), Cell[C___,Background->RGBColor[0.88, 1, 0.88],D___]:>(n+=1;Anki[n,Cell[C,D]])},Infinity]],"TeXFragment","BoxRules"->{box:(_TemplateBox|_FormBox):>(myBoxRule[box]),
"\[Transpose]":>"^{\\mathsf{T}}",
"\[ConjugateTranspose]":>"^{\\dagger} ",
"\[HermitianConjugate]":>"^{\\dagger} "},"ConversionRules"->{"Equation"->{"\\(\\begin{equation*}",EqBoxToTeX[#]&,"\\end{equation*}\\)"},
"EquationNumbered"->{"\\(\\begin{equation*}",EqBoxToTeX[#]&,"\\end{equation*}\\)"}}],
"Text"]]&/@cells;
(*line splitter extended to 5000 characters*)
System`Convert`TeXDump`cleanUpFile[fileName_String] := 
 Module[{streamIn, streamOut, insideComment = False, charNum = 0, 
   scratchFileName, charIn, charLast = ""}, 
   	 {streamIn, streamOut} = {OpenRead[fileName], System`ConvertersDump`Utilities`OpenTempFile[CharacterEncoding -> {}]};
  	While[(charIn = Read[streamIn, Character]) =!= EndOfFile, 
   If[charLast =!= "\\" && charIn === "%", insideComment = True];
   	If[charIn === "\n", insideComment = False;
    	charNum = -1];
   	If[charNum >= 144 && Or @@ (charIn === #1 &) /@ {"\t", " "}, 
    WriteString[streamOut, If[! insideComment, "\n", "\n%"]];
    	charNum = 0, WriteString[streamOut, charIn];
    	++charNum];
   	charLast = charIn;];
  	Scan[Close, {streamIn, streamOut}];
  	DeleteFile[fileName];
  	CopyFile[scratchFileName = First[streamOut], fileName];
  	DeleteFile[scratchFileName];];
(*PrintToConsole[base];*)

ShowStatus["Fixing data... (1/2)"];
ids=CurrentValue[#,"CellID"]&/@ cells;
ShowStatus["Fixing data... (2/2)"];
base=StringReplace[base,{
"\\medspace"->"",
"\n\\)"->"\\)",
"<br>\\)"->"\\)",
("}}\\color[HTML]{000000}\\color[HTML]{1111FF}{{c"~~Shortest[c__]~~"::")->"",
"}}{{c"~~Shortest[c__]~~"::"->"",
"\(\)"->"",
"{{c1::}}"->"",
"\n"->"<br>",
"\n"->"<br>",
"{{c1::<br>}}"->"<br>",
("^{"~~Shortest[c__]~~"}^{"~~WhitespaceCharacter...~~"\\dagger"~~WhitespaceCharacter...~~"}")/;StringFreeQ[c,"}"|"{"]:>"^{"<>c<>"\\dagger}",
("^"~~c_~~"^{"~~WhitespaceCharacter...~~"\\dagger"~~WhitespaceCharacter...~~"}")/;StringFreeQ[c,"}"|"{"]:>"^{"<>c<>"\\dagger}",

("^{"~~Shortest[c__]~~"}^{\\mathsf{T}"~~WhitespaceCharacter...~~"}")/;StringFreeQ[c,"}"|"{"]:>"^{"<>c<>"\\mathsf{T}}",
("^"~~c_~~"^{"~~WhitespaceCharacter...~~"\\mathsf{T}"~~WhitespaceCharacter...~~"}")/;StringFreeQ[c,"}"|"{"]:>"^{"<>c<>"\\mathsf{T}}",

("^{"~~Shortest[c__]~~"}^{"~~WhitespaceCharacter...~~"*"~~WhitespaceCharacter...~~"}")/;StringFreeQ[c,"}"|"{"]:>"^{"<>c<>"*}",
("^"~~c_~~"^{"~~WhitespaceCharacter...~~"*"~~WhitespaceCharacter...~~"}")/;StringFreeQ[c,"}"|"{"]:>"^{"<>c<>"*}",

"\\overset{"~~WhitespaceCharacter...~~"\\mathsym{"~~WhitespaceCharacter...~~"\\OverBracket"~~WhitespaceCharacter...~~"}"~~WhitespaceCharacter...~~"}":> "\\overbrace",
"\\underset{"~~WhitespaceCharacter...~~"\\mathsym{"~~WhitespaceCharacter...~~"\\UnderBracket"~~WhitespaceCharacter...~~"}"~~WhitespaceCharacter...~~"}":> "\\underbrace",
(*,("\\text{"~~Shortest[c__]~~"}")\[RuleDelayed]ToString@StringReplace[c,{"$"\[RuleDelayed]  ""}] *)
"\\left\\left|"~~Shortest[c__]~~"\\right\\right|":> "\\left|"~~c ~~"\\right|",
"\\right\\right| "~~WhitespaceCharacter...~~"{}_":> "\\right|_",
"\\right\\right| "~~WhitespaceCharacter...~~"_":> "\\right|_"
}];
base=StringReplace[base,{"\\(\\("->"\\(","\\)\\)"->"\\)"}];
base=StringReplace[base,{"\\)"~~Shortest[C__]~~"\\(":>StringJoin["\\)",StringReplace[C,"\\pmb{"~~Shortest[D__]~~"}":>StringJoin["<b>",D,"</b>"]],"\\("],
StartOfString~~Shortest[C__]~~"\\(":>StringJoin[StringReplace[C,"\\pmb{"~~Shortest[D__]~~"}":>StringJoin["</b>",D,"</b>"]],"\\("],
"\\)"~~Shortest[C__]~~EndOfString:>StringJoin["\\)",StringReplace[C,"\\pmb{"~~Shortest[D__]~~"}":>StringJoin["<b>",D,"</b>"]]]
}];
base=StringReplace[base,{"\\)"~~Shortest[C__]~~"\\(":>StringJoin["\\)",StringReplace[C,"\\textit{"~~Shortest[D__]~~"}":>StringJoin["<i>",D,"</i>"]],"\\("],
StartOfString~~Shortest[C__]~~"\\(":>StringJoin[StringReplace[C,"\\textit{"~~Shortest[D__]~~"}":>StringJoin["</i>",D,"</i>"]],"\\("],
"\\)"~~Shortest[C__]~~EndOfString:>StringJoin["\\)",StringReplace[C,"\\textit{"~~Shortest[D__]~~"}":>StringJoin["<i>",D,"</i>"]]]
}];
(*extra case with no eq*)
base=If[StringContainsQ[#,"\\("],#,StringReplace[#,{"\\pmb{"~~Shortest[D__]~~"}":>StringJoin["<b>",D,"</b>"],
"\\textit{"~~Shortest[D__]~~"}":>StringJoin["<i>",D,"</i>"]}]]&/@base;
base=StringTrim[base,"<br>"];
ShowStatus["Exporting to Wolfram Cloud..."];
ndir=NotebookDirectory[EvaluationNotebook[]];
npath=NotebookFileName[EvaluationNotebook[]];
If[cloudex,
backupc=(Flatten[{CurrentValue[#,{"CellTags"}]}])&/@Cells[EvaluationNotebook[],CellID->ids];
MapThread[SetOptions[#1,CellTags->Append[#2,ToString@CurrentValue[#,"CellID"]]]&,{Cells[EvaluationNotebook[],CellID->ids],backupc}];
FrontEndExecute[FrontEndToken[EvaluationNotebook[],"SelectAll"]];
	FrontEndTokenExecute[EvaluationNotebook[],"SelectionCloseAllGroups"];
filtered=Select[Thread[{ids,base,paths,ExportToCloud[],celltags}],StringMatchQ[#[[2]],"*{{c@::*"] & ];
FrontEndExecute[FrontEndToken[EvaluationNotebook[],"SelectAll"]];
FrontEndTokenExecute[EvaluationNotebook[],"SelectionOpenAllGroups"];
MapThread[SetOptions[#1,CellTags->#2]&,{Cells[EvaluationNotebook[],CellID->ids],backupc}];
,filtered=Select[Thread[{ids,base,paths,npath,celltags}],StringMatchQ[#[[2]],"*{{c@::*"]&];
];
ShowStatus["Exporting to Anki..."];
deck=StringReplace[StringReplace[ndir,e___~~"/Knowledge/" ~~ f___ ~~"/":> f],"/":>"::"];
PrintToConsole[deck];
PrintToConsole[AnkiRequest["createDeck",<|"deck"->deck|>]];
AddOrUpdateNotes[deck,filtered];
If[sync,PrintToConsole[AnkiRequest["sync"]]];
ShowStatus["Notebook Exported"];
];
End[];
EndPackage[];
