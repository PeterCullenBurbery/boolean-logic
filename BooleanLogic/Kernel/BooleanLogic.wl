(* ::Package:: *)

BeginPackage["PeterBurbery`BooleanLogic`"];

(* Declare your packages public symbols here. *)

BooleanStructureData;
FindBooleanAlternative;
BooleanTruthInputData;
VennDiagram;
TruthTable;
BooleanCompose;

Begin["`Private`"];



(* Define your public and private symbols here. *)
ClearAll[BooleanStructureData];
BooleanStructureData[func_]:= <|"truth-table"->
BooleanTable[func],"truth-vector"->Boole[BooleanTable[func]],
"input-variables"->BooleanVariables[func],"positive-unate-monotone"->UnateQ[func],"negative-unate"->UnateQ[func,If[ListQ[BooleanVariables[func]],Not/@BooleanVariables[func],ConstantArray[False,BooleanVariables[func]]]]|>

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
VennDiagram // ClearAll;
VennDiagram // Attributes = {};
VennDiagram[ args___ ] := 
    Module[ { res },
        update[ ];
        res = Symbol[ "ResourceFunctionHelpers`VennDiagram" ][ args ];
        res /; Head @ res =!= Symbol[ "ResourceFunctionHelpers`VennDiagram" ]
    ];
update // ClearAll;
update[ ] := 
    Once[
        PacletManager`PacletUpdate[
            "ResourceFunctionHelpers",
            "Site" -> "http://pacletserver.wolfram.com",
            "UpdateSites" -> True
        ];
        Quiet @ Block[ { $ContextPath }, Get[ "ResourceFunctionHelpers`" ] ]
    ];
    
TruthTable//ClearAll
TruthTable[booleanfunction_]:=Block[{input},input=Join[BooleanVariables[booleanfunction],{booleanfunction}];
TableForm[BooleanTable[input],TableHeadings->{None,input}]]/;!MatchQ[booleanfunction,_BooleanFunction]
TruthTable[booleanfunction_,"Symbols"->{truesymbol_,falsesymbol_}]:=Block[{input},input=Join[BooleanVariables[booleanfunction],{booleanfunction}];
TableForm[BooleanTable[input]/.{True->truesymbol,False->falsesymbol},TableHeadings->{None,input}]]/;!MatchQ[booleanfunction,_BooleanFunction]
 BooleanCompose[expr_][args__]:=expr/.(Verbatim[#]->#[args]&/@Union@BooleanVariables[expr&&(True&)]) 
 ResourceFunction[ResourceObject[<|"Name" -> "FormatAsResourceFunction", "ShortName" -> "FormatAsResourceFunction", "UUID" -> "a04b8cc2-23c4-424e-9846-fb9e83ff42ef", "ResourceType" -> "Function", "Version" -> "1.0.0", "Description" -> "Format a symbol as a ResourceFunction in outputs", "RepositoryLocation" -> URL["https://www.wolframcloud.com/objects/resourcesystem/api/1.0"], "SymbolName" -> "FunctionRepository`$9a23e344af2a439b9bf754d5e4b52c65`FormatAsResourceFunction", "FunctionLocation" -> CloudObject["https://www.wolframcloud.com/objects/ef623453-1f3d-4647-a91c-c7e8a948cbc7"]|>, ResourceSystemBase -> "https://www.wolframcloud.com/objects/resourcesystem/api/1.0"]][BooleanCompose]


End[]; (* End `Private` *)

EndPackage[];
