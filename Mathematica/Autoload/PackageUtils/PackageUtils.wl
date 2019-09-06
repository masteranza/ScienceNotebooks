(* ::Package:: *)

(* ::Title:: *)
(*PackageUtils*)


(* ::Author:: *)
(*author: Michal Mandrysz*)


(* ::Affiliation:: *)
(*Marian Smoluchowski Institute of Physics, Jagiellonian University, Krakow, Poland*)


(* ::Abstract:: *)
(*This package is a collection of function used for notebook formatting, pdf exporting etc.*)


(* ::Text:: *)
(*Version: 1.0.0*)


BeginPackage["PackageUtils`"];


(* ::Section:: *)
(*Public messages*)


(* ::Text:: *)
(*It is useful to see more recently opened notebooks*)


SetOptions[$FrontEnd,"NotebooksMenuHistoryLength"->20]


ShowStatus::usage="Prints the message in the status bar";
EmbedNote::usage="Embedes cells with tag from a notebook located at path";
DuplicateNotebook::usage="Makes a copy of the notebook";
PrintToConsole::usage="Send to console";
HideOutput::usage="Hides output cells";
MergeStyle::usage ="Merges stylesheet with the notebook and saves in the same directory with the postfix _sm";
CloseCollapsed::usage="Closes collapsed Section group cells \[Dash] good for retaining numbering during export of single sections";
CodeVisible::usage = "Shows/Hides code and cell tags";
PublishToPDF::usage="Saves a publishing ready version, optional argument for copy (pendrive)";
ExportToTeX::usage="Generates a draft in Tex with an option to compile to PDF";
CreateTOC::usage="Create table of contents";
CellStrip::usage="Simple cell stripper, removes BoxData and Cell";
StyleButton::usage="Creates a button to create a style Cell of a specific name";
WordStats::usage="Prints current notebook word/character stats in Status area";
GetReal::usage="Returns items with real coefs";
SearchBar::usage="Search bar";
Outline::usage ="Displays document outline";
WorkingEnv::usage="Show all cell metadata and Input cells";
PrintoutEnv::usage="Hides all cell metadata and Input cells";
Recent::usage="Shows recently opened notebooks";
TranslateSpecialCellStyleNames::usage="For internal usage";
IncludePath::usage="Includes a path to $Path variable, for easy loading";


(* ::Section:: *)
(*Implementations*)


Begin["`Private`"];

DuplicateNotebook[]:=NotebookPut@NotebookGet[EvaluationNotebook[]];
TranslateSpecialCellStyleNames[name_]:=If[AbsoluteCurrentValue["Language"] == "Polish",Switch[name,"Example","Przyk\[LSlash]ad","Exercise","Zadanie","Solution","Rozwi\:0105zanie","Question","Pytanie","Remark","Uwaga","Comment","Komentarz",
"Theorem","Twierdzenie","Proof","Dow\[OAcute]d","Axiom","Aksjomat","Definition","Definicja","Lemma","Lemat","Corollary","Wniosek","Title","Tytu\[LSlash]","Subtitle","Podtytu\[LSlash]","Author","Autor","Section","Sekcja","Subsection","Podsekcja","Subsubsection","Podpodsekcja","Text","Tekst","Item1","Pozycja","Equation","R\[OAcute]wnanie","EquationNumbered","R\[OAcute]wnanie numerowane","Figure","Rysunek","Table","Tabela",_,name],name]
CloseCollapsed[]:=(SetOptions[#[[1]],CellOpen->!(CellOpen/.Options[#[[1]],CellOpen])];&/@Select[{#,CurrentValue[#,"CellGroupOpen"]}&/@Cells[EvaluationNotebook[], CellStyle -> "Section"],#[[2]]==$Failed||#[[2]]==Closed&];)

Outline[]:=CreatePalette[
Dynamic[
Refresh[
Column[
First/@
DeleteCases[
Cases[
NotebookRead@{#},
Cell[name_,style:"Title"|"Section"|"Subsection"|"Subsubsection"|"Subsubsubsection",___,CellID->id_,___]:>
Button[DisplayForm[
If[style=="Title",Cell[Style[name,TextAlignment->Axis],"Text",(*FontWeight\[Rule]Bold,*) FontSize->16,TextAlignment->Center,CellSize->{Full,Automatic}],
Short@Row[{Style[(StringPadLeft["     ",IntegerPart[(StringLength[style]-7)/3]*2]<>ToString[CurrentValue[#,{"CounterValue",style}]]<>". "),Bold,13],Cell[name,"Text",FontSize->13,CellSize->{Full,Automatic}]},ImageSize->Full,BaseStyle->"Text"]
]
],(
NotebookFind[SelectedNotebook[],id,All,CellID];
SelectionMove[SelectedNotebook[],All,CellGroup];
FrontEndTokenExecute["OpenCloseGroup"];
), Appearance->"Frameless", Alignment->Center, ImageMargins->0, FrameMargins->{{5,0},{0,0}},Background->If[TrueQ[CurrentValue[#,"CellGroupOpen"]==Open],White,Lighter@LightGray],ImageSize->{Full,Full}]]
&/@Cells[SelectedNotebook[],CellStyle->{"Title","Section","Subsection","Subsubsection","Subsubsubsection"}],{}],ItemSize->{30,Full},Alignment->{Center,Left}],UpdateInterval->0.5],"Temporary"],WindowSize->{Fit,650},WindowFloating->True,WindowMargins->{{Automatic,50},{Automatic,58}},WindowElements->{"VerticalScrollBar"},Saveable->False,WindowTitle->Dynamic[AbsoluteCurrentValue[SelectedNotebook[],"WindowTitle"],"Temporary",SynchronousUpdating->True]
];

SearchBar=ExpressionCell[Row@{InputField[Dynamic[search],String,ContinuousAction->True],"  ",Button["search",sdm],"  ",Button["show all",sa]}];

WorkingEnv[]:=(DynamicModule[{nb},nb=SelectedNotebook[];
SetOptions[#,CellOpen->True,ShowCellTags->True, ShowCellLabel->True,ShowCellBracket->True]&/@Cells[nb];SetOptions[EvaluationNotebook[],ScreenStyleEnvironment->"Working"]]);

Recent[]:=Column[NotebooksMenu/.Options[$FrontEnd]//MapAt[ToFileName@@#[[1, ;;2]]&,{All,2}]];

IncludePath[path_]:=If[Not[MemberQ[$Path,path]],$Path=Flatten[{$Path,path}]];

PrintoutEnv[]:=DynamicModule[{nb},nb=SelectedNotebook[];
SetOptions[#,ShowCellLabel->False,ShowCellTags->False,ShowCellBracket->False]&/@Cells[nb];
SetOptions[#,CellOpen->False]&/@Cells[nb,CellStyle->"Input"];
SetOptions[EvaluationNotebook[],ScreenStyleEnvironment->"Printout"]];

sa:=DynamicModule[{nb},nb=EvaluationNotebook[];
SetOptions[#,CellOpen->True,ShowCellLabel->True, ShowCellBracket->True]&/@Cells[nb]]

sdm:=DynamicModule[{nb},nb=EvaluationNotebook[];
NotebookFind[nb,search,All];
SetOptions[#,CellOpen->False,ShowCellBracket->False]&/@Cells[nb];
SetOptions[#,CellOpen->True,ShowCellBracket->True]&/@SelectedCells[nb];]

PrintToConsole[expr_]:=(SetSelectedNotebook[MessagesNotebook[]];
NotebookWrite[SelectedNotebook[],Cell[BoxData[ToBoxes[expr]],"Print"]]);

ShowStatus[status_]:=LinkWrite[$ParentLink,SetNotebookStatusLine[FrontEnd`EvaluationNotebook[],ToString[status]]];

StyleButton[name_]:=Button[name,SelectionMove[SelectedNotebook[],All,Cell];FrontEndExecute@FrontEndToken[SelectedNotebook[],"Style",name]];

GetReal[sols_]:=Module[{thrd,rule},
thrd=Thread[Variables[sols]->1];
Pick[sols,Element[#,Reals]&/@(sols/.thrd)]
];

CellStrip[data_]:=ReplaceRepeated[data,{Cell[c_,___]:>c,BoxData[d__]:>d,TextData[ff_]:>ff}]

EmbedNote[notebookpath_,tag_]:=Block[{m},
Catch[
NotebookWrite[
EvaluationNotebook[],
Cell[CellGroupData[
Cases[First@Get[If[(m=FindFile[notebookpath])=!=$Failed,m,Throw["File \""<> notebookpath <>"\" not found"]]],Cell[__,CellTags->(tag|{___,tag,___}),___],\[Infinity]]],CellFrame->{{0,0},{0,0}},CellDingbat->ButtonBox[StyleBox["\[LeftAngleBracket]","Dingbat"],Appearance->"Frameless",Tooltip->notebookpath,ButtonFunction->(NotebookLocate[#]&),ButtonData->{m,tag}]
]
]
]
];

WordStats[]:=
Module[{cells,dat,data,characters,words},
cells=Cells[EvaluationNotebook[],CellStyle->{"Title","Section","Subsection","Subsubsection","Subsubsubection","Text","EquationNumbered","Equation","Item1","Item2","Item3","Item1Numbered","Item2Numbered","Item3Numbered"}];
data=NotebookRead@cells;
dat=ReplaceRepeated[data,
{
RowBox[{C__String}]:>StringJoin@C,
StyleBox[D__,Background->None]:>StyleBox[D],
StyleBox[D_String]:>D,
Cell[TextData[data_],___]:> data,
Cell[BoxData[data_],___]:> data,
Cell[data_,___]:> data,
Cell[BoxData[data_],___]:> data,
FormBox[C_String,TextForm|TraditionalForm]:> C
}];
dat=Block[{n=1},ReplaceAll[dat,{
(FormBox[RowBox[{C__String}],TextForm]|FormBox[RowBox[{C__String}],TraditionalForm]):>StringJoin@C,
(FormBox[C__, TraditionalForm]|FormBox[C__, TextForm]):>"[$]"<>Convert`TeX`BoxesToTeX[C, "BoxRules"->{StyleBox[D_,Background->LightGreen]:> "[/$]{{c"<>ToString[n]<>"::[$]" <>StringReplace[Convert`TeX`BoxesToTeX [D],{"{{":> " { { ","}}":> " } } "}] <>"[/$]}}[$]"}]<>"[/$] ",

StyleBox[D_, Background->LightGreen]:>("{{c"<>ToString[n]<>"::"<>D<>"}}"),
StyleBox[D_String,___,FontWeight->"Bold",___]:> D,
StyleBox[D_String,___,FontSlant->"Italic",___]:> D,
StyleBox[D_String,___,FontWeight->"Plain",___]:> D,
StyleBox[D_String,___,FontVariations->{___,"Underline"->True,___},___]:> (D),
StyleBox[D_String,___,FontVariations->__,___]:> D,
StyleBox[D_, Background->_]:>ToString[n],
ButtonBox[RowBox[{_,CounterBox[_,N_],_}],___]:> (Convert`TeX`BoxesToTeX@First@Cases[data,Cell[name_,___,CellTags->N,___]:>name,Infinity]),
ButtonBox[RowBox[{_,CounterBox[_],_}],___]:> "",
CounterBox["FigureCaptionNumbered",N_]:> "",
CounterBox["FigureCaptionNumbered"]:> "",
ButtonBox[___]:> ""
}]];
words=ToString@Total[WordCount[#]&/@Flatten[dat]];
characters=ToString@Total[StringLength[#]&/@Flatten[dat]];
ShowStatus["Counted: "<>characters<>" characters (with spaces) and " <> words<> " words"];
];

MergeStyle[]:=Module[{path,child,childstyles,parent,nb,tmp,parentparent,parentstyles, old},(*find the parent stylesheet from the private stylesheet,a.k.a child*)
If[NotebookDirectory[]===$Failed, Abort[]];
path=ToFileName[{$UserBaseDirectory,"SystemFiles","FrontEnd","StyleSheets"}];
child=Options[EvaluationNotebook[],StyleDefinitions];
parent=First@Cases[Options[EvaluationNotebook[],StyleDefinitions],(StyleDefinitions-> x_):>x,\[Infinity]] ;
childstyles=Cases[child,Cell[StyleData[_,___],__],\[Infinity]];
(*get the parent*)
nb=NotebookOpen[path<>parent];
tmp=NotebookGet[nb];
NotebookClose[nb];
(*find the parent definition*)parentparent=Cases[tmp,Cell[StyleData[StyleDefinitions->x_]],\[Infinity]];
(*scrape the styles*)parentstyles=Cases[tmp,Cell[StyleData[x_,y___],z__],\[Infinity]];
(*merge parents parent,child styles,parent styles*)SetOptions[EvaluationNotebook[],StyleDefinitions->Notebook[Join[parentparent,childstyles,parentstyles],StyleDefinitions->"PrivateStylesheetFormatting.nb"],DockedCells->None];

old=FileBaseName[NotebookFileName[]];
NotebookSave[EvaluationNotebook[],NotebookDirectory[EvaluationNotebook[]]<>old<>"_sm.nb"];
NotebookOpen[NotebookDirectory[EvaluationNotebook[]]<>old<>".nb"];
];

CodeVisible[] := Module[{cells,show},
   cells = Cells[EvaluationNotebook[], CellStyle -> "Input"];
show=Last@First@Options[EvaluationNotebook[],ShowCellTags];

SetOptions[EvaluationNotebook[],ShowCellLabel->!show];
SetOptions[EvaluationNotebook[],ShowCellTags->!show];
   Map[SetOptions[#, CellOpen -> !show] &, cells];
   ];

CodeVisible[flag_] := Module[{cells,show},
   cells = Cells[EvaluationNotebook[], CellStyle -> "Input"];
SetOptions[EvaluationNotebook[],ShowCellLabel->flag];
SetOptions[EvaluationNotebook[],ShowCellTags->flag];
   Map[SetOptions[#, CellOpen -> flag] &, cells];
   ];

CreateTOC[typeList_]:=Module[{toc,book,createCell,counter,cell,type,tag,tocreate,sel},
sel=False;
If[SelectedCells[]=={},
NotebookWrite[EvaluationNotebook[],Cell["Temp","Subtitle",CellTags->"TempIndex"]];
sel=True;
];
If[sel==False, Return];
tocreate={};
book=EvaluationNotebook[];
SetOptions[book,ShowPageBreaks->True];
(*helper file for creating cell*)createCell[text_,tag_,level_]:={"",DynamicBox@Refresh[StringJoin[Table["\t",{i,1,level-1}]]<>(NotebookRead[Cells[EvaluationNotebook[],CellTags->tag]][[1]]/.{Cell[t_,___]:>t}),UpdateInterval->1],DynamicBox@CounterBox["Page",tag]};
(*iterate over cells to set tags and write lines to TOC*)Scan[(counter[#]=0)&,typeList];
SelectionMove[book,Next,Cell];
While[(cell=NotebookRead[book])=!={},If[Length[cell]>=2,type=cell[[2]];
If[MemberQ[typeList,type],counter[type]+=1;
tag=type<>ToString[counter[type]];
SetOptions[NotebookSelection[book],CellTags->tag];
SelectionMove[book,All,CellContents];
AppendTo[tocreate, createCell[SelectedCells[],tag,Position[typeList,type][[1,1]]]]];
SelectionMove[book,Next,Cell]];
];
If[sel,
NotebookLocate["Index"];
If[SelectedCells[]!={},
NotebookWrite[EvaluationNotebook[],Cell[BoxData[StyleBox[DynamicBox@GridBox[tocreate,GridBoxAlignment->{"Columns"->{Left,Left,Right}},GridBoxItemSize->{"Columns"->{1,32,-5}}],"Text"]],CellTags->"Index"]];
NotebookLocate["TempIndex"];
NotebookDelete[];,

NotebookLocate["TempIndex"];
NotebookDelete[];
NotebookWrite[EvaluationNotebook[],Cell[BoxData[StyleBox[DynamicBox@GridBox[tocreate,GridBoxAlignment->{"Columns"->{Left,Left,Right}},GridBoxItemSize->{"Columns"->{2 -1,35-2,-5}}],"Text"]],CellTags->"Index"]];
];
];
];

PublishToPDF[copyPath_: "",codeVisible_:False] := Module[{nb, nb2},
ShowStatus["Exprting PDF..."];
  nb = ToFileName[{NotebookDirectory[EvaluationNotebook[]]}, "WindowTitle" /. NotebookInformation[SelectedNotebook[]]];
  CodeVisible[codeVisible];
  Export[ nb <> ".pdf", SelectedNotebook[]];
nb2 = copyPath <> ToString["WindowTitle" /. NotebookInformation[SelectedNotebook[]]] <> ".pdf";
  If[copyPath != "", CopyFile[nb <> ".pdf", nb2]];
  CodeVisible[True];
SystemOpen[nb<>".pdf"];
ShowStatus["PDF exported successfully"];
{nb, nb2};
  ];
  
  (*Options[ExportToTeX]={ExportToPDF->False,EmbedRefrencesBeforeExport->False};*)
ExportToTeX[(*opt:OptionsPattern[]*)]:=Module[{cells,base,prolog,epilog,styles,deb},
ShowStatus["Exporting to TeX..."];
deb=TeXForm["1"];
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
System`Convert`TeXFormDump`$TeXDelimiterReplacements = System`Convert`TeXFormDump`$TeXDelimiterReplacements /. {"\\left| " | "\\right| " -> "|","\\left\\| " | "\\right\\| " -> "\\| "};
deb=Convert`TeX`BoxesToTeX[""];
System`Convert`TeXFormDump`$TeXDelimiterReplacements = System`Convert`TeXFormDump`$TeXDelimiterReplacements /. {"\\left| " | "\\right| " -> "|","\\left\\| " | "\\right\\| " -> "\\| "};


EmbedEq[what_]:=Replace[Replace[what,{ButtonBox[___,Tooltip->DynamicBox[c__,UpdateInterval->\[Infinity]],___]:>Temp[c]},Infinity],Temp[RowBox[{_,d___}]]:>d,Infinity];

RefEq[what_]:=Replace[what,{Cell[C_,D___,FormatType->"TraditionalForm",E___,CellTags->t_]:>Cell[C,"InlineCell",D,FormatType->"TraditionalForm",E,CellTags->t],
Cell[BoxData[ButtonBox[___,TaggingRules->{"deeptag"->c_},___],___],___]:>("\\ref{"<>ToString[c]<>"}")},Infinity];

FixFigures[what_]:=Replace[what,{BoxData[GraphicsBox[C___]]:>BoxData[FormBox[GraphicsBox[C],TraditionalForm]],StyleBox[C_String,Background->RGBColor[0.88, 1, 0.88],D___]:>(C)},Infinity];

NameThisFigure[i_]:=Block[{labels}, labels=(CellTags/.Options[i]); 
If[StringQ[labels]||(ListQ[labels]&&Length[labels]==1),ToString[CellTags/.Options[i]],ToString[CellID/.Options[i]]]];

NameAndExport[i_,form_]:=Block[{name},
name=NameThisFigure[i];
FileNameTake[Export[NotebookDirectory[EvaluationNotebook[]]<>NameThisFigure[i]<>".pdf",FormBox[form,TraditionalForm],"pdf"]]
];

InsertLabel[i_]:=Block[{labels},
labels=(CellTags/.Options[i]);
If[StringQ[labels]||(ListQ[labels]&&Length[labels]==1),"\\label{"<>ToString[CellTags/.Options[i]]<>"}",""]];

AddLabels[i_,form_]:=Block[{labels},
labels=Replace[Cases[NotebookRead[i],Cell[___,_[form],___,CellTags->_,___],Infinity],Cell[___,_[form],___,CellTags->c_,___]:>c,Infinity];
(If[Length[labels]==1,"{",""]<>(StringTrim@ExportString[Cell[BoxData[form],FormatType->TraditionalForm],"TeXFragment"])<>If[Length[labels]==1,"\\label{"<>First[labels]<>"}} ",""])
(*old code for latex ref*)
(*(If[Length[labels]\[Equal]1,"\\labeltext{",""]<>(StringTrim@ExportString[Cell[BoxData[form],FormatType\[Rule]TraditionalForm],"TeXFragment"])<>If[Length[labels]\[Equal]1,"}{"<>First[labels]<>"} ",""])*)];

(*myBoxRule[TemplateBox[{boxes_,"Automatic",var_,lima_,limb_},___]]:=
StringJoin[System`Convert`TeXFormDump`maketex[boxes],"\\bigg|_{",System`Convert`TeXFormDump`maketex[var],"=",System`Convert`TeXFormDump`maketex[lima],"}{",System`Convert`TeXFormDump`maketex[limb],"}"];*)
myBoxRule[TemplateBox[{boxes_,_,lima_,limb_},___]]:=StringJoin["\\left."<>System`Convert`TeXFormDump`maketex[boxes],"\\right|_{",System`Convert`TeXFormDump`maketex[lima],"}^{",System`Convert`TeXFormDump`maketex[limb],"}"];

myBoxRule[FormBox[TemplateBox[{boxes_,_,lima_,limb_},___],___]]:=StringJoin["\\left."<>System`Convert`TeXFormDump`maketex[boxes],"\\right|_{",System`Convert`TeXFormDump`maketex[lima],"}^{",System`Convert`TeXFormDump`maketex[limb],"}"];

myBoxRule[FormBox[C__]]:=System`Convert`TeXFormDump`maketex[FormBox[C]];
EqBoxToTeX[c_]:=Convert`TeX`BoxesToTeX[c,"BoxRules"->{box:(_TemplateBox|_FormBox):>(myBoxRule[box])}];
prolog="% \\documentclass[titlepage, a4paper]{mwart}
%\\documentclass[aps,prl,showpacs,10pt,superscriptaddress,nidanfloat,twocolumn,% draft]{revtex4-1}
\\documentclass{article}
\\usepackage{amsmath,amsfonts,amssymb,amsthm}
\\usepackage{graphicx, setspace}
\\usepackage[export]{adjustbox}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
% \\usepackage{polski}
\\usepackage[pdftex,colorlinks=true,citecolor=blue,linkcolor=magenta]{hyperref}
\\usepackage{showlabels}
% \\makeatletter
% \\newcommand{\\labeltext}[3][]{%
%     \\@bsphack%
%     \\csname phantomsection\\endcsname% in case hyperref is used
%     \\def\\tst{#1}%
%     \\def\\labelmarkup{\\emph}% How to markup the label itself
% %\\def\\refmarkup{\\labelmarkup}% How to markup the reference
%     \\def\\refmarkup{}%
%     \\ifx\\tst\\empty\\def\\@currentlabel{\\refmarkup{#2}}{\\label{#3}}%
%     \\else\\def\\@currentlabel{\\refmarkup{#1}}{\\label{#3}}\\fi%
%     \\@esphack%
%     \\labelmarkup{#2}% visible printed text.
% }
% \\makeatother
\\begin{document}\n";
epilog="\\end{document}";
(*styles=DeleteCases[FEPrivate`GetPopupList[EvaluationNotebook[],"MenuListStyles"]//FE`Evaluate//Cases[_[s_String,_]:>s],"Input"|"Output"];*)
cells=Cells[EvaluationNotebook[]];
cells=Select[Select[Select[cells,(CurrentValue[#,"CellStyle"]!={"Input"})&],(CurrentValue[#,"CellStyle"]!={"Output"})&],(CurrentValue[#,"CellStyle"]!={"Print"})&];
base=StringJoin@Riffle[#,"\n"]&@Table[ImportString[ExportString[If[False(*OptionValue[EmbedRefrencesBeforeExport]*),EmbedEq,RefEq]@FixFigures[NotebookRead[i]],"TeXFragment",
"BoxRules"->{box:(_FormBox):>(myBoxRule[box]),
"\[Transpose]":>"^{\\mathsf{T}}",
"\[ConjugateTranspose]":>"^{\\dagger} ",
"\[HermitianConjugate]":>"^{\\dagger} "},
"ConversionRules"->{
"Section"->{"\\section{",Automatic,InsertLabel[i]<>"}"},
"Subsection"->{"\\subsection{",Automatic,InsertLabel[i]<>"}"},
"Subsubsection"->{"\\subsubsection{",Automatic,InsertLabel[i]<>"}"},
"Subsubsection"->{"\\subsubsection{",Automatic,InsertLabel[i]<>"}"},
"Figure"->{"\\begin{figure}[ht!]\\centering\\includegraphics[max width=\\textwidth]{",NameAndExport[i,#]&,"}"<>InsertLabel[i]<>"\\end{figure}"},
"EquationNumbered"->{"\\begin{equation}",EqBoxToTeX[#]&,InsertLabel[i]<>"\\end{equation}"},
"Equation"->{"\\begin{equation*}",EqBoxToTeX[#]&,InsertLabel[i]<>"\\end{equation*}"},
"InlineCell"->{"",AddLabels[i,#]&,""}
}
],"Text"],{i,cells}];
base=StringReplace[base,{"\\(\\("->"\\(","\\)\\)"->"\\)"}];
base=StringReplace[StringReplace[base,{"\\)"~~ShortestMatch[C___]~~"\\(":>StringJoin["\\)",StringReplace[C,"\\pmb{"->"\\textbf{"],"\\("],
StartOfString~~ShortestMatch[C___]~~"\\(":>StringJoin[StringReplace[C,"\\pmb{"->"\\textbf{"],"\\("],
"\\)"~~ShortestMatch[C___]~~EndOfString:>StringJoin["\\)",StringReplace[C,"\\pmb{"->"\\textbf{"]]
}],{"\\pmb{"->"\\bf{"}];
(*base=StringReplace[base,{"\\&"\[Rule]"&","{array}" -> "{aligned}"}];*)
Export[StringDrop[NotebookFileName[],-2]<>"tex",prolog<>base<>epilog,"Text"];
ShowStatus["Trying to build the PDF from TeX (might fail)..."];
If[True(*OptionValue[ExportToPDF]*),
PrintToConsole["Running command: "<>"!cd "<>NotebookDirectory[EvaluationNotebook[]]<>"; ls; pdflatex "<>StringDrop[FileNameTake[NotebookFileName[EvaluationNotebook[]]],-2]<>"tex"];
(*SetEnvironment["PATH"\[Rule]Import["!source ~/.bash_profile; echo $PATH","Text"]];*)
Quiet@ReadList@OpenRead["!cd "<>NotebookDirectory[EvaluationNotebook[]]<>"; /Library/TeX/texbin/pdflatex "<>StringDrop[FileNameTake[NotebookFileName[EvaluationNotebook[]]],-2]<>"tex"];
(*RunProcess@{"pdflatex.exe",StringDrop[FileNameTake[NotebookFileName[EvaluationNotebook[]]],-2]<>"tex"}*)];
ShowStatus["Done."];
];



End[];
EndPackage[];
