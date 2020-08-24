(* ::Package:: *)

(* ::Title:: *)
(*ScienceNotebooks*)


(* ::Author:: *)
(*author: Michal Mandrysz*)


(* ::Affiliation:: *)
(*Marian Smoluchowski Institute of Physics, Jagiellonian University, Krakow, Poland*)


(* ::Abstract:: *)
(*This package is a collection of functions used for notebook formatting, pdf and anki exporting etc.*)


BeginPackage["ScienceNotebooks`"];


(* ::Text:: *)
(*It is useful to always be able to see more of recently opened notebooks*)


SetOptions[$FrontEnd,"NotebooksMenuHistoryLength"->20]


(* ::Text:: *)
(*Fix to T EX Exporter*)


System`Convert`TeXFormDump`$TeXDelimiterReplacements = 
  System`Convert`TeXFormDump`$TeXDelimiterReplacements /. {"\\left| " \
| "\\right| " -> "|", "\\left\\| " | "\\right\\| " -> "\\| "};


(* ::Text:: *)
(*Empty expression for special use in FieldTheoryForm*)


emptyExp=\[Null];


(* ::Section:: *)
(*Keyboard shortcuts and menus*)


(* Shortcut for inputing Zotero references to Mathematica (Ctrl-Z) *)
 FrontEndExecute[
 FrontEnd`AddMenuCommands[
  "DuplicatePreviousOutput", {
   MenuItem["Zotero link", FrontEndExecute[ScienceNotebooks`LinkZotero[]], System`MenuKey["z", System`Modifiers -> {Control}], 
    System`MenuEvaluator -> Automatic]}]]; 

(* Insert a reference to another cell *)
 FrontEndExecute[
 FrontEnd`AddMenuCommands[
  "DuplicatePreviousOutput", { 
   MenuItem["Link to tagged ref.", FrontEndExecute[ScienceNotebooks`LinkTaggedRef[]], System`MenuKey["e", System`Modifiers -> {Control}], 
    System`MenuEvaluator -> Automatic]}]]; 
    
emptyExp=\[Null];
Begin["`Private`"];

LinkZotero[]:=Module[{nb, clip}, clip = Evaluate@ToExpression[NotebookGet[ClipboardNotebook[]][[1, 1, 1]]];
      NotebookWrite[SelectedNotebook[], With[{t = clip[[1]], s = clip[[2]], r = clip[[3]], r2 = clip[[4]], c = clip[[-1]]}, 
  			Cell[BoxData[ButtonBox[RowBox[{"[",s,"]"}],TaggingRules -> {"citekey" -> s, "zotero" -> t, "collection" -> c},
  			BaseStyle -> {"Hyperlink"}, ButtonFunction :> SystemOpen[t], Tooltip -> (r<>"\n"<>r2), TooltipStyle->{Background->RGBColor[1, 1, 1], CellFrame -> 1}]], "Cite"]]
      ]
    ];

LinkTaggedRef[]:=Module[{nb, t, cell, ysubt, subt},
            nb = InputNotebook[];
            SelectionMove[nb, All, Expression];
            t = NotebookRead[SelectedNotebook[]];           
            t=If[Head[t]===StyleBox,First[t],t];
            If[StringQ[t],
    		ysubt=(StringContainsQ[t,"`"] && Length[StringSplit[t,"`"]]>1);
    		If[ysubt, subt=Last[StringSplit[t,"`"]]; t=First[StringSplit[t,"`"]];,subt="";];
            With[{x=t, y=subt, z=ysubt}, NotebookWrite[nb, ButtonBox[DynamicBox[ScienceNotebooks`ReferenceBox[x,z,y], UpdateInterval -> Infinity], 
            TaggingRules -> {"deeptag" -> If[ysubt, subt, t], "TeXtag" -> If[ysubt, StringJoin[t,subt], t]}, 
            BaseStyle -> {"Hyperlink"}, ButtonData -> t, TooltipDelay -> 0.05, TooltipStyle -> {Background -> RGBColor[1, 1, 1], CellFrame -> 1}, 
            Tooltip -> DynamicBox[ScienceNotebooks`RefCellTooltip[x, If[z, y, None]], UpdateInterval -> Infinity]]]]
            ];
        ];


(* ::Section::Closed:: *)
(*Debug and communication with the user*)


ScienceNotebooks`PrintToConsole::usage="Send to console";
PrintToConsole[expr_]:=(SetSelectedNotebook[MessagesNotebook[]]; NotebookWrite[SelectedNotebook[],Cell[BoxData[ToBoxes[expr]],"Print"]]);

ScienceNotebooks`ShowStatus::usage="Prints the message in the status bar";
ShowStatus[status_]:=LinkWrite[$ParentLink,SetNotebookStatusLine[FrontEnd`EvaluationNotebook[],ToString[status]]];


(* ::Section:: *)
(*Advanced figure and tables*)


(*Internal - for handling Polish/English*)
ScienceNotebooks`TranslateSpecialCellStyleNames::usage="Translates style names between English and Polish";
TranslateSpecialCellStyleNames[name_]:=If[AbsoluteCurrentValue["Language"] == "Polish",Switch[name,"Example","Przyk\[LSlash]ad","Exercise","Zadanie","Solution","Rozwi\:0105zanie","Question","Pytanie","Remark","Uwaga","Comment","Komentarz",
"Theorem","Twierdzenie","Proof","Dow\[OAcute]d","Axiom","Aksjomat","Definition","Definicja","Lemma","Lemat","Corollary","Wniosek","Title","Tytu\[LSlash]","Subtitle","Podtytu\[LSlash]","Author","Autor","Section","Sekcja","Subsection","Podsekcja","Subsubsection","Podpodsekcja","Text","Tekst","Item1","Pozycja","Equation","R\[OAcute]wnanie","EquationNumbered","R\[OAcute]wnanie numerowane","Figure","Rysunek","Table","Tabela","Board","Tabela",_,name],Switch[name,"Board","Tabela",_,name]];


ObjCaption[style_String]:=RowBox[{ScienceNotebooks`TranslateSpecialCellStyleNames[style],CurrentValue[{"CounterValue","Section"}],".",CurrentValue[{"CounterValue",If[style==="Board","Table",style]}]}];
TagInput[base_String,tag_:Automatic]:=Block[{tagt,baseTag,tmp},
baseTag="@"<>base;
t=(TaggingRules/.Options[EvaluationCell[],TaggingRules]);
tmp=If[ListQ@t,baseTag/.t,baseTag];
tagt=If[tag=!=Automatic,tag,
(*If[Head@Unevaluated@content===Symbol,SymbolName[Unevaluated[content]],*)
If[tmp=!=baseTag,tmp,
tmp=StringJoin[Capitalize@RandomWord[#,IncludeInflections->True,Language->"English"]&/@{"Adjective","Noun"}];
SetOptions[EvaluationCell[],TaggingRules->If[ListQ[t],Join[t,{baseTag->tmp}],If[StringQ[t],Join[{t},{baseTag->tmp}],{baseTag->tmp}]]];
tmp
]
]
];

CreateObj[style_String,capstyle_String,tag_:Automatic,captionFirst_:False]:=Block[{c,tagt,tmp},
tagt=TagInput[style,tag];
c=NotebookFind[EvaluationNotebook[],tagt,All,CellTags];
tmp=c===$Failed;
If[tmp,If[captionFirst,FrontEnd`NotebookWrite[EvaluationNotebook[],CellPrint[TextCell["\[Placeholder]",capstyle,CellAutoOverwrite->False]
],All,AutoScroll->False];
FrontEnd`NotebookWrite[EvaluationNotebook[],
CellPrint[Cell[BoxData[FormBox["\[Placeholder]",TraditionalForm]],style,CellAutoOverwrite->False,CellTags->tagt]
],All,AutoScroll->False];
,FrontEnd`NotebookWrite[EvaluationNotebook[],
CellPrint[Cell[BoxData[FormBox["\[Placeholder]",TraditionalForm]],style,CellAutoOverwrite->False,CellTags->tagt]
],All,AutoScroll->False];
FrontEnd`NotebookWrite[EvaluationNotebook[],CellPrint[TextCell["\[Placeholder]",capstyle,CellAutoOverwrite->False]
],All,AutoScroll->False]];
];
c=NotebookFind[EvaluationNotebook[],tagt,All,CellTags];
tmp
];

ScienceNotebooks`Figure::usage="Creates reusable Figure with caption from input";
SetAttributes[Figure, HoldFirst];
Figure[content_,tag_:Automatic]:=Block[{createdNew},
createdNew=CreateObj["Figure","FigureCaption",If[tag===Automatic&&Head@Unevaluated@content===Symbol,SymbolName[Unevaluated[content]],tag]];
Paste[EvaluationNotebook[],content];
SelectionMove[EvaluationNotebook[],Next,Cell,1,AutoScroll->False];
SelectionMove[EvaluationNotebook[],After,CellContents,1,AutoScroll->False];
];

ScienceNotebooks`Board::usage="Creates reusable Table with caption from input";
SetAttributes[Board, HoldFirst];
Board[content_,tag_:Automatic,row_:True,opts___]:=Block[{createdNew},
createdNew=CreateObj["Table","TableTitle",If[tag===Automatic&&Head@Unevaluated@content===Symbol,SymbolName[Unevaluated[content]],tag],True];
Paste[EvaluationNotebook[],TableForm[content,opts]];
SelectionMove[EvaluationNotebook[],Previous,Cell,1,AutoScroll->False];
SelectionMove[EvaluationNotebook[],Before,CellContents,1,AutoScroll->False];
(*SelectionMove[EvaluationNotebook[],After,CellContents,AutoScroll->False];*)
(*SelectionMove[EvaluationNotebook[],Previous,Line,1,AutoScroll->False];
If[!createdNew,SelectionMove[EvaluationNotebook[],All,Expression,AutoScroll->False],Unevaluated[Sequence[]]];
NotebookWrite[EvaluationNotebook[],GridBox[content,opts,GridBoxAlignment->{"Columns"->{{Left}},"ColumnsIndexed"->{},"Rows"->{{Center}},"RowsIndexed"->{}},GridBoxDividers->{"Columns"->{False,If[!row,AbsoluteThickness[1],Nothing],{False},False},"ColumnsIndexed"->{},"Rows"->{AbsoluteThickness[2],If[row,AbsoluteThickness[1],Nothing],{False},AbsoluteThickness[2]},"RowsIndexed"->{}},GridBoxItemSize->{"Columns"->{{All}},"ColumnsIndexed"->{},"Rows"->{{1.2}},"RowsIndexed"->{}},GridDefaultElement:>"\[Placeholder]"]];
SelectionMove[EvaluationNotebook[],Previous,Expression,2,AutoScroll->False];*)
];

(*ExtFT[cs_,cont_]:=If[cs==="Board",Replace[cont,{FormBox[GridBox[{_,{C___},{D___}},E___],F___]:>FormBox[GridBox[{{C},{D}},E],F]},Infinity],If[cs==="Figure",Replace[cont,{FormBox[GridBox[{{C___},_,{D___}},E___],F___]:>FormBox[GridBox[{{C},{D}},E],F]},Infinity],cont]];*)
(*BoardColumn::usage="The same as board but with vertical split";*)
(*SetAttributes[BoardColumn, HoldFirst];*)
(*BoardColumn[content_,tag_:Automatic,opts___]:=Board[content,tag,False,opts];*)


(* ::Section:: *)
(*Notebook organization*)


(* ::Subsection:: *)
(*Table of contents*)


ScienceNotebooks`CreateTOC::usage="Create table of contents";
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

Toc[]:=EventHandler[Dynamic[ActionMenu["Go to",Replace[$inputNotebookTOC,Except[_List]->{}],Appearance->"PopupMenu",BaseStyle->Directive[FontSize->12,"Text"]]],"MouseEntered":>Set[$inputNotebookTOC,Inner[RuleDelayed,Composition[Map[First@FrontEndExecute@FrontEnd`ExportPacket[#,"InputText"]&],Map[(NotebookRead[#]/.CounterBox[style_]:>ToBoxes@CurrentValue[#,{"CounterValue",style}])&],Cells[#,CellStyle->{"AbstractSection","Section","Subsection","Subsubsection","ReferenceSection","EndnoteSection"}]&]@InputNotebook[],Map[Unevaluated@NotebookFind[InputNotebook[],#,All,CellID]&,CurrentValue[Cells[CellStyle->{"AbstractSection","Section","Subsection","Subsubsection","ReferenceSection","EndnoteSection"}],CellID],1],List]]];


(* ::Subsection:: *)
(*Outline*)


ScienceNotebooks`Outline::usage ="Displays document outline";
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

ScienceNotebooks`QuickOutline::usage ="Serves a quick document outline menu";
QuickOutline=EventHandler[Dynamic[ActionMenu["Toc",(*creating list of action menu item names and list of commands*)Replace[$inputNotebookTOC,Except[_List]->{}]]],"MouseEntered":>Set[$inputNotebookTOC,Inner[RuleDelayed,(*1 create list of names*)Composition[Map[(*1.3.Extract modified input text into list*)First@FrontEndExecute@FrontEnd`ExportPacket[#,"InputText"]&],(*1.2.read objects into an expression and replace CounterBox["typeOfCell"] with counter values*)Map[(NotebookRead[#]/.CounterBox[style_]:>ToBoxes@CurrentValue[#,{"CounterValue",style}])&],(*1.1.extract list of section cells objects to put in TOC*)Cells[#,CellStyle->{"AbstractSection","Section","Subsection","Subsubsection","ReferenceSection","EndnoteSection"}]&]@InputNotebook[],(*2. create list of commands*)Map[Unevaluated@NotebookFind[InputNotebook[],#,All,CellID]&,CurrentValue[Cells[CellStyle->{"AbstractSection","Section","Subsection","Subsubsection","ReferenceSection","EndnoteSection"}],CellID],1],List]]];



(* ::Subsection:: *)
(*Search*)


sa:=DynamicModule[{nb},nb=EvaluationNotebook[];search="";
SetOptions[#,CellOpen->True,ShowCellLabel->True, ShowCellBracket->True]&/@Cells[nb]]
sdm:=DynamicModule[{nb},nb=EvaluationNotebook[];
NotebookFind[nb,search,All];
SetOptions[#,CellOpen->False,ShowCellBracket->False]&/@Cells[nb];
SetOptions[#,CellOpen->True,ShowCellBracket->True]&/@SelectedCells[nb];]

ScienceNotebooks`SearchBar::usage="Search bar";
SearchBar=ExpressionCell[EventHandler[InputField[Dynamic[search],String,ContinuousAction->True,FieldHint->"Search",Appearance-> FEPrivate`FrontEndResource["MUnitExpressions","ButtonAppearances"]],{"ReturnKeyDown":>sdm},{"EscapeKeyDown":>sa}]];



(* ::Subsection:: *)
(*Recent*)


ScienceNotebooks`Recent::usage="Shows recently opened notebooks";
Recent[]:=NotebooksMenu/.Options[$FrontEnd]//MapAt[ToFileName@@#[[1, ;;2]]&,{All,2}];


(* ::Subsection:: *)
(*Operations on notebooks*)


ScienceNotebooks`DuplicateNotebook::usage="Makes a copy of the notebook";
DuplicateNotebook[]:=NotebookPut@NotebookGet[EvaluationNotebook[]];

ScienceNotebooks`PublishToPDF::usage="Saves a publishing ready version, optional argument for copy (pendrive)";
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


(* ::Subsection:: *)
(*Operations on notebook contents*)


(* ::Subsubsection::Closed:: *)
(*Extraction*)


ScienceNotebooks`CellStrip::usage="Simple cell stripper, removes BoxData and Cell";
CellStrip[data_]:=ReplaceRepeated[data,{Cell[c_,___]:>c,BoxData[d__]:>d,TextData[ff_]:>ff}]

ScienceNotebooks`ContentsByCellStyle::usage="Extracts (assumed)text contents of a cells by style";
ContentsByCellStyle[style_]:=Block[{cs},cs=Cells[EvaluationNotebook[],CellStyle->style];
If[Length[cs]>0,CellStrip/@NotebookRead[cs],{""}]];


(* ::Subsubsection:: *)
(*References and embedding*)


ScienceNotebooks`ReferenceBox::usage="Used for printing dynamic references in text. Takes tag, subtag";
ReferenceBox[t_,ysubt_,subt_]:=Block[{style,secID},
 style=(If[#==="Board","Table",#])&@AbsoluteCurrentValue[First[Cells[EvaluationNotebook[],CellTags->{t}]],"CellStyleName"];
 secID=AbsoluteCurrentValue[First[Cells[EvaluationNotebook[],CellTags->{t}]],{"CounterValue","Section"}];
 RowBox[
   If[style==="Section",
     {"(",secID,")"},
     If[secID===0,
       If[ysubt,{"(",AbsoluteCurrentValue[First[Cells[EvaluationNotebook[],CellTags->{t}]],{"CounterValue",style}],".",subt,")"},{"(",AbsoluteCurrentValue[First[Cells[EvaluationNotebook[],CellTags->{t}]],{"CounterValue",style}],")"}],
       If[ysubt,{"(",secID,".",AbsoluteCurrentValue[First[Cells[EvaluationNotebook[],CellTags->{t}]],{"CounterValue",style}],".",subt,")"},{"(",secID,".",AbsoluteCurrentValue[First[Cells[EvaluationNotebook[],CellTags->{t}]],{"CounterValue",style}],")"}]
     ]
   ]]];

ScienceNotebooks`RefCellTooltip::usage="Generates a tooltip for referenced cells, takes tag and subtag";
 RefCellTooltip[x_,y_:None]:=Block[{ce,cs,secID,capt},
 ce=First@Cells[EvaluationNotebook[], CellTags -> {x}];
 cs=AbsoluteCurrentValue[ce,"CellStyleName"]; 
 secID=AbsoluteCurrentValue[ce,{"CounterValue","Section"}];
 capt=If[secID===0,{If[y=!=None,y,ScienceNotebooks`TranslateSpecialCellStyleNames[cs]], " ", AbsoluteCurrentValue[ce,{"CounterValue", If[cs==="Board","Table",cs]}], "\n"},
 {If[y=!=None,y,ScienceNotebooks`TranslateSpecialCellStyleNames[cs]], " ", secID, ".", AbsoluteCurrentValue[ce,{"CounterValue", If[cs==="Board","Table",cs]}], "\n"}];
 RowBox[Prepend[Flatten[{CellStrip[If[y=!=None,Cases[NotebookRead[ce], Cell[Pattern[A, BlankSequence[]], CellTags->y] -> Cell[A], Infinity], NotebookRead[ce]]]}], 
 StyleBox[RowBox[capt], "Subsubsection"]]]];
 
ScienceNotebooks`EmbedNote::usage="Embedes cells with tag from a notebook located at path";
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


(* ::Subsubsection:: *)
(*Visibility*)


ScienceNotebooks`CloseCollapsed::usage="Closes collapsed Section group cells \[Dash] good for retaining numbering during export of single sections";
CloseCollapsed[]:=(SetOptions[#[[1]],CellOpen->!(CellOpen/.Options[#[[1]],CellOpen])];&/@Select[{#,CurrentValue[#,"CellGroupOpen"]}&/@Cells[EvaluationNotebook[], CellStyle -> "Section"],#[[2]]==$Failed||#[[2]]==Closed&];)

ScienceNotebooks`CodeVisible::usage = "Shows/Hides code and cell tags";
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

ScienceNotebooks`WorkingEnv::usage="Show all cell metadata and Input cells";
WorkingEnv[]:=(DynamicModule[{nb},nb=SelectedNotebook[];
SetOptions[#,CellOpen->True,ShowCellTags->True, ShowCellLabel->True,ShowCellBracket->True]&/@Cells[nb];SetOptions[EvaluationNotebook[],ScreenStyleEnvironment->"Working"]]);

ScienceNotebooks`PrintoutEnv::usage="Hides all cell metadata and Input cells";
PrintoutEnv[]:=DynamicModule[{nb},nb=SelectedNotebook[];
SetOptions[#,ShowCellLabel->False,ShowCellTags->False,ShowCellBracket->False]&/@Cells[nb];
SetOptions[#,CellOpen->False]&/@Cells[nb,CellStyle->"Input"];
SetOptions[EvaluationNotebook[],ScreenStyleEnvironment->"Printout"]];


(* ::Subsubsection:: *)
(*Stylesheet related*)


ScienceNotebooks`StyleButton::usage="Creates a button to create a style Cell of a specific name";
StyleButton[name_]:=Button[name,SelectionMove[SelectedNotebook[],All,Cell];FrontEndExecute@FrontEndToken[SelectedNotebook[],"Style",name]];

ScienceNotebooks`ListScienceStyles::usage="List styles from current StyleSheet";
ListScienceStyles={"Title":>FrontEndTokenExecute[InputNotebook[],"Style","Title"],
"Chapter":>FrontEndTokenExecute[InputNotebook[],"Style","Chapter"],
"Subchapter":>FrontEndTokenExecute[InputNotebook[],"Style","Subchapter"],
"Section":>FrontEndTokenExecute[InputNotebook[],"Style","Section"],
"Text":>FrontEndTokenExecute[InputNotebook[],"Style","Text"],
"Input":>FrontEndTokenExecute[InputNotebook[],"Style","Input"],
"CodeText":>FrontEndTokenExecute[InputNotebook[],"Style","CodeText"],
"Abstract":>FrontEndTokenExecute[InputNotebook[],"Style","Abstract"],
"Author":>FrontEndTokenExecute[InputNotebook[],"Style","Author"],
"Affiliation":>FrontEndTokenExecute[InputNotebook[],"Style","Affiliation"],
"Item":>FrontEndTokenExecute[InputNotebook[],"Style","Item"],
"ItemNumbered":>FrontEndTokenExecute[InputNotebook[],"Style","ItemNumbered"],
"ItemParagraph":>FrontEndTokenExecute[InputNotebook[],"Style","ItemParagraph"],
"Subitem":>FrontEndTokenExecute[InputNotebook[],"Style","Subitem"],
"SubitemNumbered":>FrontEndTokenExecute[InputNotebook[],"Style","SubitemNumbered"],
"SubitemParagraph":>FrontEndTokenExecute[InputNotebook[],"Style","SubitemParagraph"],
"Subsubitem":>FrontEndTokenExecute[InputNotebook[],"Style","Subsubitem"],
"SubsubitemNumbered":>FrontEndTokenExecute[InputNotebook[],"Style","SubsubitemNumbered"],
"SubsubitemParagraph":>FrontEndTokenExecute[InputNotebook[],"Style","SubsubitemParagraph"],
"InlineFormula":>FrontEndTokenExecute[InputNotebook[],"Style","InlineFormula"],
"DisplayFormula":>FrontEndTokenExecute[InputNotebook[],"Style","DisplayFormula"],
"DisplayFormulaNumbered":>FrontEndTokenExecute[InputNotebook[],"Style","DisplayFormulaNumbered"],
"ExternalLanguage":>FrontEndTokenExecute[InputNotebook[],"Style","ExternalLanguage"],
"Program":>FrontEndTokenExecute[InputNotebook[],"Style","Program"],
"Code":>FrontEndTokenExecute[InputNotebook[],"Style","Code"],
"Equation":>FrontEndTokenExecute[InputNotebook[],"Style","Equation"],
"EquationNumbered":>FrontEndTokenExecute[InputNotebook[],"Style","EquationNumbered"],
"Example":>FrontEndTokenExecute[InputNotebook[],"Style","Example"],
"Figure":>FrontEndTokenExecute[InputNotebook[],"Style","Figure"],
"Item1":>FrontEndTokenExecute[InputNotebook[],"Style","Item1"]};
(*Evaluate[(#:>FrontEndTokenExecute[InputNotebook[],"Style",#]&/@FE`Evaluate[FEPrivate`GetPopupList["MenuListStyles"]][[2;;,1]])];*)

ScienceNotebooks`MergeStyle::usage ="Merges stylesheet with the notebook and saves in the same directory with the postfix _sm";
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


(* ::Subsubsection::Closed:: *)
(*Printing forms*)


ScienceNotebooks`PrintManyEqs::usage="First arg is the l.h.s, second is the list of equalities, rest should be style and CellTags->tag";
PrintManyEqs[first_,list_List,args___:"EquationNumbered"]:=CellPrint[ExpressionCell[TraditionalForm@Column@(MapThread[#1==#2&,{Prepend[ConstantArray[emptyExp,Length@list-1],first],list}]),args]];

ScienceNotebooks`FullDerivativesForm::usage="Prints eqs in TraditionalForm transforming derivatives to standard, LaTeX readable form";
FullDerivativesForm[f_]:=TraditionalForm[f/.Derivative[inds__][g_][vars__]:>Apply[Defer[D[g[vars],##]]&,Transpose[{{vars},{inds}}]/.{{var_,0}:>Sequence[],{var_,1}:>{var}}]];

ScienceNotebooks`FieldTheoryForm::usage="Prints eqs in TraditionalForm and moves derivatives to subscripts, best used on not-already-subscripted symbols";
FieldTheoryForm[f_,parameterVars_List:{}]:=TraditionalForm[f//.{Derivative[inds__][g_Subscript][vars__Symbol]:>(Subscript[g[[1]](*,ToString/@*),##2&@@g,(*ToExpression@RowBox@*)Symbol@StringJoin[ToString/@Riffle[MapThread[ConstantArray[#1,#2]//.{List->Sequence}&,{{vars},{inds}}],"\[Null]"] ]]),Derivative[inds__][g_][vars__Symbol]:>Subscript[g,(*ToExpression@RowBox@*)Symbol@StringJoin[ToString/@Riffle[MapThread[ConstantArray[#1,#2]//.{List->Sequence}&,{{vars},{inds}}],"\[Null]"]]]}/.{(F_Symbol|F_Subscript)[a__]/;(ToString[Quiet[Check[Context[F],"SubscriptedSymbol",General::ssle],General::ssle]]=!="System`"):>F},ParameterVariables->parameterVars];


(* ::Subsubsection:: *)
(*Statistics*)


ScienceNotebooks`WordStats::usage="Prints current notebook word/character stats in Status area";
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






(* ::Subsection:: *)
(*Other*)


ScienceNotebooks`IncludePath::usage="Includes a path to $Path variable, for easy loading";
IncludePath[path_]:=If[Not[MemberQ[$Path,path]],$Path=Flatten[{$Path,path}]];

ScienceNotebooks`GetReal::usage="Returns items with real coefs";
GetReal[sols_]:=Module[{thrd,rule},
thrd=Thread[Variables[sols]->1];
Pick[sols,Element[#,Reals]&/@(sols/.thrd)]
];

(*ScienceNotebooks`PlusMinus::usage="Useful error notion";
PlusMinus[{x_, err_}] := 
 Module[{errE = Last@MantissaExponent[err], 
   xE = Last@MantissaExponent[x]}, 
  Row[{"(", 
    NumberForm[
     N@Round[x, 10^(errE - 1)]*10^(-xE + 1), {-errE, -errE - 1}], 
    " \[PlusMinus] ", 
    NumberForm[
     N@Round[err, 10^(errE - 1)]*10^(-xE + 1), {1, -errE - 1}, 
     ExponentFunction -> (Null &)], ")", " \[Times] ", 
    DisplayForm@SuperscriptBox["10", ToString[xE - 1]]}]];
PlusMinus[x_, err_] := PlusMinus[{x, err}];*)

ScienceNotebooks`EvaluatedAt::usage="Shorthand for printing expressions evaluated at points";
EvaluatedAt[expr_, Automatic, min_, max_] := EvaluatedAt[expr, Replace[Reduce`FreeVariables[expr], {{v_,___}->v, _->None}], min, max];
EvaluatedAt[expr_, x_, min_, max_] := (expr /. x->max) - (expr /. x->min);

ScienceNotebooks`EvaluatedAt::usage="Automatic differential operator on free variables";
DiffOp[expr_] := Replace[Reduce`FreeVariables[expr],
    {
    {} -> expr,
    {x_} :> D[expr, x],
    x_List :> D[expr, {x}]
    }
];
End[];
EndPackage[];
