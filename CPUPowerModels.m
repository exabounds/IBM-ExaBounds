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

(* NOTE: DO NOT EXPOSE CACHED FUNCTIONS TO THE OUTSIDE WORLD. ALWAYS CALL INDIRECTLY AND CHECK FOR RETURN VALUES. IN CASE RETURN VALUES ARE INVALID, REMOVE THE WRONGLY CACHE RESULTS *)

BeginPackage["CPUPowerModels`"]

Needs["Support`"]
Needs["ExaBoundsGeneric`"]
Needs["ModelWarnings`"]
Needs["CACTI`"]
Needs["McPAT`"]
Needs["DRAMPowerModel`"] (* MeSAP*)

(* Exported symbols added here with SymbolName::usage *)  

Poweri::usage =
  "Poweri[i_, archProperties_, algProperties_, homogeneousThreads_Integer : 1]"
  
PoweriMemory::usage =
  "PoweriMemory[i_, archProperties_, algProperties_, homogeneousThreads_Integer : 1]"
  
PoweriGlu::usage =
  "PoweriGlu[i_, archProperties_, algProperties_, homogeneousThreads_Integer : 1]"
  
AreaProcessor::usage =
  "AreaProcessor[archProperties_, algProperties_, homogeneousThreads_Integer : 1]"
  
ClearCPUPowerModelsCache::usage =
  "ClearCPUPowerModelsCache[]"
  
AvailablePowerModels::usage =
  "AvailablePowerModels[]"
  
DRAMmodel::usage =
  "DRAMmodel"

Begin["`Private`"] (* Begin Private Context *)

(* NOTE: all external functions need to be based on BlockCheck[] instead of Block[] for correct display of warnings when using GetArch/AlgKeyValue *)

(* Input:
	archProperties: the architecture under test
	algProperties: either one algorithm under test (same on all cores) from ExaBoundsAlgInfo or a list of replacement rules where n -> algProperties refers to running algProperties on thread/core n. if the total number of rules mismatches the number of cores, it is assumed that the remaining rules or cores are not used *)

(* Global Variable to choose DRAM power model at socket layer
 choices: 1. MeSAP
          2. CACTI
*)
DRAMmodel = "MeSAP";

AvailablePowerModels[] = {"MeSAP", "CACTI"}; 

(* Clear caches of all cached functions *)
(* KEEP UP TO DATE! *)
ClearCPUPowerModelsCache[] :=
Block[{},
	ClearCache[Poweri];
	ClearMcPATCache[];
	ClearCACTICache[];
]; 

GetPowerList[static_, dynamic_] := {static, dynamic}; (*{{"static", static}, {"dynamic", dynamic}};*) (* Simple tuple as those have + and * and / defined *)
(*AddPower[A_, B_] :=
If[Head[A] === List && Head[B] === List, 
	GetPowerList[
		GetKeyValue[A, "static"] + GetKeyValue[B, "static"],
		GetKeyValue[A, "dynamic"] + GetKeyValue[B, "dynamic"]
	],
	A + B (* If not list, normal add *)
];*)


(* Core power *)
PowerCore[archProperties_List, algProperties_List, homogeneousThreads_Integer : 1, aggregatePower_Symbol : True] :=
Block[{McPATKeyValueList},
	(* PowerCore includes everything McPAT core + L2 *)
	McPATKeyValueList = GetMcPATProcessorParameters[archProperties, algProperties, homogeneousThreads];
	Return[
		If[aggregatePower,
			If[McPATKeyValueList == {}, 0, GetKeyValueCheck[McPATKeyValueList, "totalCore" (* static + dynamic *), defaultValueFunction -> None]],
			If[McPATKeyValueList == {}, GetPowerList[0, 0], 
				GetPowerList[GetKeyValueCheck[McPATKeyValueList, "staticCore" (* static + dynamic *), defaultValueFunction -> None],
					GetKeyValueCheck[McPATKeyValueList, "dynamicCore" (* static + dynamic *), defaultValueFunction -> None]]]
		]			
	];
];

(* Data motion power at level i *)
PoweriDataMotion[i_Integer, archProperties_List, algProperties_List, homogeneousThreads_Integer : 1, aggregatePower_Symbol : True] :=
Block[{},
	(* TODO: data motion models *)
	Return[If[aggregatePower, 0, GetPowerList[0,0]]];
];

(* Memory power at level i *)
PoweriMemory[i_Integer, archProperties_List, algProperties_List, homogeneousThreads_Integer : 1, aggregatePower_Symbol : True] :=
BlockCheck[{McPATKeyValueList, CACTIKeyValueList, MeSAPKeyValueList},
	Return[
		Which[ i == DieLayer[] (* Memory per die (L3) *), 			McPATKeyValueList = GetMcPATProcessorParameters[archProperties, algProperties, homogeneousThreads];
										  							If[aggregatePower,
										  								If[McPATKeyValueList == {}, 0, GetKeyValueCheck[McPATKeyValueList, "totalL3" (* static + dynamic *), defaultValueFunction -> None]],
										  								If[McPATKeyValueList == {}, GetPowerList[0, 0], 
																			GetPowerList[GetKeyValueCheck[McPATKeyValueList, "staticL3", defaultValueFunction -> None],
																			GetKeyValueCheck[McPATKeyValueList, "dynamicL3", defaultValueFunction -> None]]]
																		],	
			   i == SocketLayer[] (* Memory per socket (DRAM) *),	Switch[DRAMmodel,
			                        									     "CACTI", CACTIKeyValueList = GetCACTIMemoryParameters["M2" (* Request power for M2 memory (DRAM) *), archProperties, algProperties, homogeneousThreads];
			   		                         										  If[aggregatePower,
										  												If[CACTIKeyValueList == {}, 0, GetKeyValueCheck[CACTIKeyValueList, "power" (* static + dynamic *), defaultValueFunction -> None]],
										  												GetPowerList[If[CACTIKeyValueList == {}, 0, GetKeyValueCheck[CACTIKeyValueList, "staticpower", defaultValueFunction -> None]],
										  												If[CACTIKeyValueList == {}, 0, GetKeyValueCheck[CACTIKeyValueList, "dynamicpower", defaultValueFunction -> None]]]
			   		                         										  ],
			                        										 "MeSAP", MeSAPKeyValueList = GetMeSAPMemoryParameters["M2" (* Request power for M2 memory (DRAM) *), archProperties, algProperties, homogeneousThreads];
			   			                    										  If[aggregatePower,
			   			                    										  	If[MeSAPKeyValueList == {}, 0, GetKeyValueCheck[MeSAPKeyValueList, "power" (* drampowerExcludingIOTermPower + ioTermPower *), defaultValueFunction -> None]],
			   			                    										  	GetPowerList[If[MeSAPKeyValueList == {}, 0, GetKeyValueCheck[MeSAPKeyValueList, "static", defaultValueFunction -> None]],
			   			                    										  	If[MeSAPKeyValueList == {}, 0, GetKeyValueCheck[MeSAPKeyValueList, "dynamic", defaultValueFunction -> None]]]
			   			                    										  ]
			                 										],
			   True (* Other levels *), 0
		]
	];
];
PoweriMemory::mesapdynamic = "Warning: MeSAP power model does not make a distinction between static and dynamic power. Total power added to dynamic power consumption."

(* Glue-logic power at level i *)
PoweriGlu[i_Integer, archProperties_List, algProperties_List, homogeneousThreads_Integer : 1, aggregatePower_Symbol : True] :=
BlockCheck[{McPATKeyValueList},
	Return[
		Which[ i == DieLayer[] (* Glue logic per die *),			McPATKeyValueList = GetMcPATProcessorParameters[archProperties, algProperties, homogeneousThreads];
										  							If[aggregatePower,
										  								If[McPATKeyValueList == {}, 0, GetKeyValueCheck[McPATKeyValueList, "totalNoC" (* static + dynamic *), defaultValueFunction -> None]],
										  								If[McPATKeyValueList == {}, GetPowerList[0, 0], 
																			GetPowerList[GetKeyValueCheck[McPATKeyValueList, "staticNoC", defaultValueFunction -> None],
																			GetKeyValueCheck[McPATKeyValueList, "dynamicNoC", defaultValueFunction -> None]]]
										  							],
			   True (* Other levels *), 0
		]
	];
];

(* We cannot cache Poweri: if McPAT returns a failed result we cannot identify the incorrect DownValue and remove it (it will have a random value). *)
Poweri[i_Integer, archProperties_List, algProperties_List, homogeneousThreads_Integer : 1, aggregatePower_Symbol : True] :=
(*Poweri[i, archProperties, algProperties] = (* Caching for speedup *)*)
BlockCheck[{etai, ni},
	etai = GetArchKeyValueCheck[archProperties, "Eta" <> ToString[i]];
	ni = GetArchKeyValueCheck[archProperties, "n" <> ToString[i]];
	
	Return[
		Which[ i == CoreLayer[] (* core *), PowerCore[archProperties, algProperties, homogeneousThreads, aggregatePower] / etai,
		 	   i > CoreLayer[], (ni * Poweri[i-1, archProperties, algProperties, homogeneousThreads, aggregatePower]
		    			+ PoweriDataMotion[i, archProperties, algProperties, homogeneousThreads, aggregatePower]
		     			+ PoweriMemory[i, archProperties, algProperties, homogeneousThreads, aggregatePower]
		      			+ PoweriGlu[i, archProperties, algProperties, homogeneousThreads, aggregatePower]
		      			  ) / etai,
		      	True (* Other levels *), 0
		]
	];
];

AreaProcessor[archProperties_List, algProperties_List, homogeneousThreads_Integer : 1] :=
BlockCheck[{McPATKeyValueList, area},
	McPATKeyValueList = GetMcPATProcessorParameters[archProperties, algProperties, homogeneousThreads];
	area = If[McPATKeyValueList == {}, 0, GetKeyValueCheck[McPATKeyValueList, "totalArea", defaultValueFunction -> None]];
	Return[area];
];

End[] (* End Private Context *)

EndPackage[]