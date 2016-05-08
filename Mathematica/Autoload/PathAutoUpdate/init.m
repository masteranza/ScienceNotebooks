Begin["PathAutoUpdate`"]

(* Variable containing list of notebook directories
   that where automatically added to $Path. *)
$nbDirsOnPath = {}

(* Variable containing function selecting notebook objects,
   which directories are intended to be on $Path. *)
$nbIntendedForPathQ = (AbsoluteCurrentValue[#, StyleDefinitions] ===  "Default.nb" || AbsoluteCurrentValue[#, StyleDefinitions] ===  "Science.nb")&

updatePath[] := (
    (* Remove from $Path automatically added notebook directories. *)
    $Path = DeleteCases[$Path, Alternatives @@ $nbDirsOnPath];

    (* List of notebook directories, of all currently open and saved notebooks,
       that are not already on $Path. *)
    $nbDirsOnPath =
        Complement[
            Cases[
                Quiet[
                    NotebookDirectory /@ Select[Notebooks[], $nbIntendedForPathQ],
                    {NotebookDirectory::nosv}
                ]
                ,
                _String
            ]
            ,
            $Path
        ];

    $Path = Join[$Path, $nbDirsOnPath];
)

(* If previous CellProlog was set by setCellProlog use one before previous. *)
updatePathRestoreCellProlog[CellProlog :> updatePathRestoreCellProlog[oldCellProlog_]] :=
    updatePathRestoreCellProlog[oldCellProlog]
updatePathRestoreCellProlog[oldCellProlog_] := (
    updatePath[];
    oldCellProlog[[2]];
    SetOptions[$FrontEndSession, oldCellProlog]
)

(* Function setting CellProlog to function that updates $Path only on first evaluation. *)
setCellProlog[] :=
    With[{oldCellProlog = First[Options[$FrontEndSession, CellProlog]]},
        SetOptions[$FrontEndSession,
            CellProlog :> updatePathRestoreCellProlog[oldCellProlog]
        ]
    ]

switchOn[global_:True] :=
    SetOptions[If[TrueQ[global], $FrontEnd, $FrontEndSession],
        FrontEndEventActions -> {
            {"MenuCommand", "Save"} :> setCellProlog[],
            {"MenuCommand", "Open"} :> setCellProlog[],
            "WindowClose" :> setCellProlog[],
            PassEventsDown -> True
        },
        CellProlog :> updatePathRestoreCellProlog[CellProlog -> None]
    ]

switchOff[global_:True] :=
    SetOptions[If[TrueQ[global], $FrontEnd, $FrontEndSession],
        FrontEndEventActions -> None,
        CellProlog -> None
    ]

End[]