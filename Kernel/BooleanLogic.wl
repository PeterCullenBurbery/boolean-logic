(* ::Package:: *)

BeginPackage["PeterBurbery`BooleanLogic`"];

(* Declare your packages public symbols here. *)

BooleanStructureData;
FindBooleanAlternative;
BooleanTruthInputData;
VennDiagram;
Begin["`Private`"];



(* Define your public and private symbols here. *)
ClearAll[BooleanStructureData];
BooleanStructureData[func_]:= <|"truth-table"->
BooleanTable[func],"truth-vector"->Boole[BooleanTable[func]],
"input-variables"->BooleanVariables[func],"positive-unate-monotone"->UnateQ[func],"negative-unate"->UnateQ[func]|>

ClearAll[FindBooleanAlternative]

Options[FindBooleanAlternative] = {"MaxSize" -> 8};

arities = Prepend[Not -> 1] @ AssociationThread[{And, Or, Nand, Nor, Xor, Implies, Equivalent} -> 2];

FindBooleanAlternative::wrongOps = "List can contain only the following operators: Not, And, Or, Nand, Nor, Xor, Implies, Equivalent";

FindBooleanAlternative[expr_, ops_List, n : _Integer?Positive | All | Automatic : Automatic, OptionsPattern[]] /;
	ContainsOnly[ops, Keys @ arities] || ResourceFunction["ResourceFunctionMessage"][FindBooleanAlternative::wrongOps] :=
Enclose @ Module[{
	vars = Replace[BooleanVariables[expr], k_Integer :> \[FormalX] /@ Range[k]],
	opsArities = Normal @ arities[[Key /@ ops]],
	tab = BooleanTable[expr],
	size = ConfirmBy[OptionValue["MaxSize"], IntegerQ],
	limit = Replace[n, {Automatic -> 1, All -> Infinity}]
},
	If[n === Automatic, Replace[{x_, ___} :> x], Identity] @ Take[
		SortBy[LeafCount] @ FoldWhile[
			{r, s} |-> Join[r,
				Select[
					DeleteDuplicates @ Groupings[Tuples[vars, s], opsArities],
					BooleanTable[#, vars] === tab &
				]
			],
			{},
			Range[size],
			Length[#] < limit &
		],
		UpTo[limit]
	]
]

VennDiagram // ClearAll;
VennDiagram // Attributes = {};
VennDiagram[ args___ ] := 
    Module[ { res },
        update[ ];
        res = Symbol[ "ResourceFunctionHelpers`VennDiagram" ][ args ];
        res /; Head @ res =!= Symbol[ "ResourceFunctionHelpers`VennDiagram" ]
    ];
    
BooleanTruthInputData//ClearAll;
BooleanTruthInputData[func_]:=<|"satisfiable"->SatisfiableQ[func],
"true-outputs-count"->SatisfiabilityCount[func],"input-that-makes-output-true"->
SatisfiabilityInstances[func,BooleanVariables[func]],"tautology"->TautologyQ[func]|>


End[]; (* End `Private` *)

EndPackage[];
