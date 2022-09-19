(* ::Package:: *)

BeginPackage["PeterBurbery`BooleanLogic`"];

(* Declare your packages public symbols here. *)

SayHello;
BooleanFunctionInformation;
FindBooleanAlternative;
Begin["`Private`"];



(* Define your public and private symbols here. *)
ClearAll[BooleanFunctionInformation];
SayHello[name_?StringQ] := Print["Hello ", name, "!"];
BooleanFunctionInformation[booleanfunction_]:=Block[{dnf,cnf
sop,pos},
dnf=BooleanConvert[booleanfunction,"DNF"];
<|"disjunctive-normal-form"->Iconize[TraditionalForm[dnf]],
"DNFlength"->LeafCount[dnf],"sum-of-products"|>
]
(*BooleanFunctionInformation[booleanfunction_]:=Block[{booleanforms,
minimalbooleanforms},booleanforms=AssociationMap[BooleanConvert[booleanfunction,#]&,
{"DNF","CNF","SOP","POS","ESOP","ANF","NOR","NAND","AND","OR",
"IMPLIES","ITE","IF","BFF","BDT"}];
(*<|"boolean-forms"->
booleanforms,
,"minimal-forms"->AssociationMap[BooleanMinimize[booleanfunction,#]&,

{"DNF","CNF","SOP","POS","ANF","NAND","AND","OR"}],"sati
sfiable"->SatisfiableQ[booleanfunction]|>*)]*)
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


End[]; (* End `Private` *)

EndPackage[];
