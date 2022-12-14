(* ::Package:: *)

BeginPackage["PeterBurbery`BooleanLogic`"];

(* Declare your packages public symbols here. *)
InverseBoole;
BooleanStructureData;
AllBooleanForms;
FindBooleanAlternative;
BooleanTruthInputData;
VennDiagram;
TruthTable;
BooleanCompose;
RandomBooleanFunction;
AllMinimalBooleanForms;
AllBooleanFormsLiteralCounts;
AllMinimalBooleanFormsLiteralCounts;
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


ClearAll[InverseBoole];

Attributes[InverseBoole]={Listable};

InverseBoole[1]=True;
InverseBoole[0]=False;
InverseBoole[a_->b_]:=a->InverseBoole[b];
InverseBoole[args___]:=InverseBoole[args]/;(ArgumentCountQ[InverseBoole,Length@{args},1,1]&&False);


AllBooleanForms//ClearAll;
AllBooleanForms[func_]:=AssociationMap[form|->BooleanConvert[func,form],{"DNF","SOP","CNF","POS","ESOP","ANF","NOR","NAND","AND","OR","IMPLIES","ITE","IF","BFF","BDT"}]
(*AllBooleanForms[func_,"Output-Form"->"Long-Form"]:=AssociationMap[form|->BooleanConvert[func,form],{"disjunctive-normal-form","sum-of-products","conjunctive-normal-form","product-of-sums","exclusive-sum-of-products","algebraic-normal-form","nor-and-not","nand-and-not","and-and-not","or-and-not","implies-and-not","if-and-constants","if-and-constants","boolean-function-form","boolean-decision-tree"}]
*)
AllBooleanForms[func_,"Output-Form"->"Long-Form"]:=AssociationThread[{"disjunctive-normal-form","sum-of-products","conjunctive-normal-form","product-of-sums","exclusive-sum-of-products","algebraic-normal-form","nor-and-not","nand-and-not","and-and-not","or-and-not","implies-and-not","if-and-constants","if-and-constants","boolean-function-form","boolean-decision-tree"}->Map[form|->BooleanConvert[func,form],{"DNF","SOP","CNF","POS","ESOP","ANF","NOR","NAND","AND","OR","IMPLIES","ITE","IF","BFF","BDT"}]]

RandomBooleanFunction//ClearAll;
RandomBooleanFunction[inputno_]:=BooleanFunction[RandomInteger[2^(2 inputno)],inputno]

AllMinimalBooleanForms//ClearAll
AllMinimalBooleanForms[func_]:=AssociationMap[form|->BooleanMinimize[func,form],{"DNF","SOP","CNF","POS","ANF","NOR","NAND","AND","OR"}]
AllMinimalBooleanForms[func_,"Output-Form"->"Long-Form"]:=AssociationThread[{"disjunctive-normal-form","sum-of-products","conjunctive-normal-form","product-of-sums","algebraic-normal-form","nor-and-not","nand-and-not","and-and-not","or-and-not"}->Map[form|->BooleanMinimize[func,form],{"DNF","SOP","CNF","POS","ANF","NOR","NAND","AND","OR"}]]

AllBooleanFormsLiteralCounts//ClearAll
AllBooleanFormsLiteralCounts[func_]:=AssociationMap[form|->LeafCount[BooleanConvert[func,form],Heads->False],{"DNF","SOP","CNF","POS","ESOP","ANF","NOR","NAND","AND","OR","IMPLIES","ITE","IF","BFF","BDT"}]
AllBooleanFormsLiteralCounts[func_,"Output-Form"->"Long-Form"]:=AssociationThread[{"disjunctive-normal-form","sum-of-products","conjunctive-normal-form","product-of-sums","exclusive-sum-of-products","algebraic-normal-form","nor-and-not","nand-and-not","and-and-not","or-and-not","implies-and-not","if-and-constants","if-and-constants","boolean-function-form","boolean-decision-tree"}->Map[form|->LeafCount[BooleanConvert[func,form],Heads->False],{"DNF","SOP","CNF","POS","ESOP","ANF","NOR","NAND","AND","OR","IMPLIES","ITE","IF","BFF","BDT"}]]





AllMinimalBooleanFormsLiteralCounts//ClearAll;
AllMinimalBooleanFormsLiteralCounts[func_]:=AssociationMap[form|->LeafCount[BooleanMinimize[func,form],Heads->False],{"DNF","SOP","CNF","POS","ANF","NOR","NAND","AND","OR"}]
AllMinimalBooleanFormsLiteralCounts[func_,"Output-Form"->"Long-Form"]:=AssociationThread[{"disjunctive-normal-form","sum-of-products","conjunctive-normal-form","product-of-sums","algebraic-normal-form","nor-and-not","nand-and-not","and-and-not","or-and-not"}->Map[form|->LeafCount[BooleanMinimize[func,form],Heads->False],{"DNF","SOP","CNF","POS","ANF","NOR","NAND","AND","OR"}]]



End[]; (* End `Private` *)

EndPackage[];
