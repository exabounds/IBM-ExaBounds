(*******************************************************************************
 * (C) Copyright IBM Corporation 2017
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    IBM Algorithms & Machines team
 *******************************************************************************)
 
 (* Mathematica Package *)

BeginPackage["AlgorithmVisualization`"]

Needs["ExaBoundsGeneric`"];
Needs["InstructionMix`"];

(* Exported symbols added here with SymbolName::usage *)

PrintInstructionMix::usage =
	"PrintInstructionMix[algProperties]"  
	
PrintILP::usage =
	"PrintILP[algProperties_]"

Begin["`Private`"] (* Begin Private Context *)

propertyList = {"F0load", "F0store", "F0mem",
					"F0addr", "F0int",  "F0intOnly", "F0intmul",
					"F0intdiv", "F0fp", "F0fpmul", "F0fpdiv",
					"F0control", "F0vector", "F0other"};
totalsPropertyList = {"F0mem", "F0addr", "F0int", "F0fp", "F0control", "F0vector", "F0other"};

ConstructTableBitsColumn[algProperties_, vec_, bits_] :=
Block[{val},
	Return[
		Column[
			Flatten[{
				ToString[bits] <> " bit",
				Table[
					val = GetKeyValue[algProperties, key];
					If[Head[val] === Integer,
						If[bits == 0, val, ""],
						VMixGetData[val, VectorLength->vec, DataLength->bits]
					],
					{key, propertyList}
				],
				"----",
				VMixGetData[
					VMixAccumulate[
						Table[GetKeyValue[algProperties, key], {key, totalsPropertyList}]
					], VectorLength->vec, DataLength->bits
				]
			},1]
		]
	];
];

ConstructTableTotalsColumn[algProperties_, vec_] :=
Block[{val},
	Return[
		Column[
			Flatten[{
				"Total ",
				Table[
					val = GetKeyValue[algProperties, key];
					If[Head[val] === Integer,
						val,
						VMixGetData[val, VectorLength->vec, DataLength->"Accumulate"]
					],
					{key, propertyList}
				],
				"----",
				VMixGetData[
					VMixAccumulate[
						Table[GetKeyValue[algProperties, key], {key, totalsPropertyList}]
					], VectorLength->vec, DataLength->"Accumulate"
				]
			},1]
		]
	];
];

ConstructTableVectorColumn[algProperties_, vec_] :=
Block[{},
	Return[
		Column[{
				"Vector " <> ToString[vec],
				Row[
					Flatten[{
						Column[
							Flatten[{"", propertyList, "---", ""},1]
						],
						Table[ConstructTableBitsColumn[algProperties, vec, bits], {bits, GetDataLengths[]}],
						ConstructTableTotalsColumn[algProperties,vec]
					},1]
				]
		}]
	];
];

PrintInstructionMix[algProperties_] :=
Block[{ret, tableHeadings},
	(* Display a table with the vector properties *)
	Return[Row[
		Flatten[{
			Table[ConstructTableVectorColumn[algProperties, vec], {vec, GetVectorLengths[]}],
			VMixGetData[
					VMixAccumulate[
						Table[GetKeyValue[algProperties, key], {key, totalsPropertyList}]
					], VectorLength->"Accumulate", DataLength->"Accumulate"
				]
		},1]
	]];
]; 

ILPtags = {"ILP0", "ILP0mem", "ILP0int", "ILP0fp", "ILP0control"}
PrintILP[algProperties_] :=
Block[{},
	Return[Column[
		Table[Row[{tag <> ": ", GetKeyValue[algProperties, tag]}], {tag, ILPtags}]
	]]; 
];

End[] (* End Private Context *)

EndPackage[]