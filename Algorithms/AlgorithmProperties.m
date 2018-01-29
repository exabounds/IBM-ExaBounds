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

BeginPackage["AlgorithmProperties`"]
(* Exported symbols added here with SymbolName::usage *)

Needs["ExaBoundsGeneric`"];
Needs["AlgorithmsFromFile`"];
Needs["AlgorithmFromFileJSON`"];

InitAlgorithmMinMax::usage = 
  "InitAlgorithmMinMax[]"  
  
GetDefaultAlgorithmProperties::usage =
  "GetDefaultAlgorithmProperties[]"

ShowAlgorithmsAdminGUI::usage =
  "ShowAlgorithmsAdminGUI[algInfoStructure_]"
  
AppendAlgorithmAnalysis::usge =
  "AppendAlgorithmAnalysis[algInfoStructure_,newAlgoAnalysis_]"

(*	AlgorithmFromFile and ExtrapolateAlgorithm return only the per-thread data list.
	To organize in algorithmFormat and append it, please call use this function *)
StructureAlgorithmAnalysis::usage =
  "StructureAlgorithmAnalysis[applicationName_,scalingParametersAssociation_,threadsData_]"
  
AddAlgorithm::usage =  
  "AddAlgorithm[algInfoStructure_, path_, name_]"
  
GetAlgorithmDefaultValue::usage = 
  "GetAlgorithmDefaultValue[key_]"
  	
Begin["`Private`"] (* Begin Private Context *) 

(* 	This function append in the right place a new application analysis.
	It groups applications with the same name and it keep track of the scaling parameters *)
(*  Update: the function now returns a list of {name, scaling, dataprofile, 1} such that the just-inserted algorithms can be re-identified. It defaults to thread 1*)
Options[AppendAlgorithmAnalysis] = {
	"Safe" -> True	(* True: checks the data structure and place data where it should go *)
					(* False: appends directly the datastructure without any check. *)
}
AppendAlgorithmAnalysis[algInfoStructure_,newAlgoAnalysis_,OptionsPattern[]] :=
Module[{aName,newAlgoData,scalingParams,sortedScalingParameters,profileList, profileIdx,thread, returnList},
	If[newAlgoAnalysis == Null, Return[Null]];
	
	newAlgoAnalysis = Replace[newAlgoAnalysis, {"F0DPFP" -> "F0fp", "ILP0DPFP" -> "ILP0fp", "ILPinorder0DPFP" -> "ILPinorder0fp"}, All];
	
	If[OptionValue["Safe"],
		returnList = {};
		Do[(* Support for multiple newAlgoAnalysis *)
			newAlgoData = newAlgoAnalysis[aName];
			
			(* 	If algorithms are loaded from the appScaling, not all properties might be present.
				We merge with the defaults to make sure all properties are available *)
			Do[
				If[Head[scalingParams] === Association,
					Do[
						Do[
							newAlgoData[[Key[scalingParams]]][[profileIdx]][[Key[thread]]] = ExaBoundsMergeState[GetDefaultAlgorithmProperties[], newAlgoData[scalingParams][[profileIdx]][thread]];
						,{thread, Keys[newAlgoData[scalingParams][[profileIdx]]]}];
					,{profileIdx, Length[newAlgoData[scalingParams]]}];
				];
			,{scalingParams,Keys[newAlgoData]}];
			
			If[algInfoStructure[aName] === Missing["KeyAbsent",aName],
				(* Append the new algorithm *)
				algInfoStructure = Append[algInfoStructure,Association[aName->newAlgoData]];
				(* The algorithm didn't yet exist, fill the returnList for all scalingParams, thread and profileIdx *)
				Do[
					If[Head[scalingParams] === Association,
					Do[
						AppendTo[returnList, {aName, scalingParams, profileIdx, 1}];
					,{profileIdx, Length[newAlgoData[scalingParams]]}];
					];
				,{scalingParams,Keys[newAlgoData]}];
				
				,
				(* Add new profile analysis for an existing algorithm *)
				Do[(* search for the associations, i.e. list of scaling parameters. Add the new analysis *)
					If[Head[scalingParams] === Association,
						sortedScalingParameters = KeySort[scalingParams];
						
						profileList = algInfoStructure[aName,sortedScalingParameters];
						If[ profileList === Missing["KeyAbsent",sortedScalingParameters],
							(* There is no profile with the current scaling parameters yet *)
							profileList = newAlgoData[scalingParams];
							(* Add the new profileIdxs to the returnList *)
							Do[
								AppendTo[returnList, {aName, scalingParams, profileIdx, 1}];
							,{profileIdx, Length[newAlgoData[scalingParams]]}];
							,				
							(* There is already a profile with the current scaling parameters, profileList contains the current data profiles *)
							(* Add the new profileIdxs to the returnList *)
							Do[
								AppendTo[returnList, {aName, scalingParams, Length[profileList] + profileIdx, 1}];
							,{profileIdx, Length[newAlgoData[scalingParams]]}];
							profileList = Flatten[{profileList,newAlgoData[scalingParams]},1];
						];
						algInfoStructure[aName,sortedScalingParameters] = profileList;
					];
					
				,{scalingParams,Keys[newAlgoData]}];
			];
		,{aName,Keys[newAlgoAnalysis]}];
		
		Return[returnList];
		
		,(* Unsafe *)
		AppendTo[algInfoStructure,newAlgoAnalysis];
		
		Return[Null];
	];
];
SetAttributes[AppendAlgorithmAnalysis, HoldAll]; (* Set HoldAll such that we can update the structure passed in algInfoStructure *)

StructureAlgorithmAnalysis[applicationName_,scalingParametersAssociation_,threadsData_] := 
Association[applicationName->Association[{	"scalingParameters"-> Sort[Keys[scalingParametersAssociation]]
											,KeySort[scalingParametersAssociation]->{threadsData}}]];

(* Small function to be called by ShowAlgorithmsAdminGUI to actually load and add the new algorithm to the algInfoStructure*)
(* Old interface for AlgorithmsFromFile *)
(*  Update: the function now returns a list of {name, scaling, dataprofile, 1} such that the just-inserted algorithms can be re-identified. It defaults to thread 1*)
AddAlgorithm[algInfoStructure_, path_, name_] :=
Module[{keyValueList,newAnalysisData, returnList},
	returnList = {};
	(* FromFiles *)
	keyValueList = LoadAlgorithmFromFiles[path, name];
	If[keyValueList != {},
		newAnalysisData = Association[{
							"scalingParameters"-> {},
							Association[] (* empty scaling parameters *) -> {(* List for different executions over the same scaling parameter
																				to store content-related variability *)
																				Association[{{0,0} -> keyValueList}](* single threaded application *)
																			}
						}];
		returnList = AppendAlgorithmAnalysis[algInfoStructure, Association[{name->newAnalysisData}]][[1]]
	];
	Return[returnList];
];

(* Overloaded AddAlgorithm only for JSON files: *)
(*  Update: the function now returns a list of {name, scaling, dataprofile, 1} such that the just-inserted algorithms can be re-identified. It defaults to thread 1*)
(* We return [[1]] as we assume for now that a JSON file can only contain one algorithm/scaling/dataset pair (although with multiple threads) *)
Options[AddAlgorithm] = {"ILP0WindowSize"->54};
AddAlgorithm[algInfoStructure_, jsonfile_, OptionsPattern[]] := 
Block[{ILP0WindowSize, newAlg},
	ILP0WindowSize = OptionValue["ILP0WindowSize"];
	newAlg = LoadAlgorithmJSON[jsonfile, "ILP0WindowSize" -> ILP0WindowSize];
	If[newAlg == Null, Return[Null]];
	Return[AppendAlgorithmAnalysis[algInfoStructure, newAlg][[1]]];
];
SetAttributes[AddAlgorithm, HoldAll]; (* Set HoldAll such that we can update the structure passed in algInfoStructure *)

(* This function shows GUI controls to administer loaded algorithms *)
(* It contains controls to load new algorithms into ExaBounds from File *)
ShowAlgorithmsAdminGUI[algInfoStructure_] :=
Module[{index, jsonFile, mFileSymbolName, mFileSymbol, mFile},
	index = {}; (* Need to initialize the local variables *)
	mFileSymbolName = "predictedAlgorithm";

	Return[Panel[
		Column[{
			Style["Imported algorithms:"],
			Dynamic[ListPicker[index, If[Keys[algInfoStructure]=={},{Blank[]},Keys[algInfoStructure]], Multiselection->False]],
			Button["Import IBM Platform-Independent Software Analysis file (JSON format)", jsonFile = SystemDialogInput["FileOpen"]; If[jsonFile =!= $Canceled, AddAlgorithm[algInfoStructure, jsonFile]], Method->"Queued"],
			Row[{Style["Symbol name: "], InputField[Dynamic[mFileSymbolName], String]}],
			Button["Import IBM Exascale Extrapolator file (*.m format)", mFile = SystemDialogInput["FileOpen"]; If[mFile =!= $Canceled, Get[mFile]; mFileSymbol = Symbol[mFileSymbolName]; AppendAlgorithmAnalysis[algInfoStructure, mFileSymbol]], Method->"Queued"]
		}]
	]]
];
SetAttributes[ShowAlgorithmsAdminGUI, HoldAll]; (* Set HoldAll such that we can update the structure passed in algInfoStructure *)

AlgorithmRangeDefaultsTXT = " 
  ILP0MIN				1		Dimensionless
  ILP0MAX				131072		Dimensionless
  
  ILP0WindowSizeMIN			1		Dimensionless
  ILP0WindowSizeMAX			131072		Dimensionless
  
  ILPinorder0MIN			1		Dimensionless
  ILPinorder0MAX			131072		Dimensionless
  
  ILP0memMIN				0.01		Dimensionless
  ILP0memMAX				1024		Dimensionless
  ILP0intMIN				0.01		Dimensionless
  ILP0intMAX				1024		Dimensionless
  ILP0controlMIN			0.01		Dimensionless
  ILP0controlMAX			1024		Dimensionless
  ILP0fpMIN				0.01		Dimensionless
  ILP0fpMAX				1024		Dimensionless
  
  ILPinorder0memMIN			0.01		Dimensionless
  ILPinorder0memMAX			1024		Dimensionless
  ILPinorder0intMIN			0.01		Dimensionless
  ILPinorder0intMAX			1024		Dimensionless
  ILPinorder0controlMIN			0.01		Dimensionless
  ILPinorder0controlMAX			1024		Dimensionless
  ILPinorder0fpMIN			0.01		Dimensionless
  ILPinorder0fpMAX			1024		Dimensionless
  
  LSysMIN				1E6		Instruction
  LSysMAX				1E25		Instruction
	
  F0controlMIN 				0.01		Dimensionless
  F0controlMAX 				0.9		Dimensionless
  F0addrMIN 				0.01		Dimensionless
  F0addrMAX 				0.9		Dimensionless
  F0intMIN 				0.01		Dimensionless
  F0intMAX 				0.9		Dimensionless
  F0intOnlyMIN 				0.01		Dimensionless
  F0intOnlyMAX 				0.9		Dimensionless
  F0fpMIN 				0.01		Dimensionless
  F0fpMAX 				0.9		Dimensionless
  F0memMIN 				0.01		Dimensionless
  F0memMAX 				0.9		Dimensionless
  F0loadMIN				0.01		Dimensionless
  F0loadMAX				0.99		Dimensionless
  F0storeMIN				0.01		Dimensionless
  F0storeMAX				0.99		Dimensionless
  F0vectorMIN				0.01		Dimensionless
  F0vectorMAX				0.99		Dimensionless
  F0otherMIN 				0.01		Dimensionless
  F0otherMAX 				0.9		Dimensionless
  
  F0intmulMIN				0.01		Dimensionless
  F0intmulMAX				0.9		Dimensionless
  F0intdivMIN				0.01		Dimensionless
  F0intdivMAX				0.9		Dimensionless
  
  F0fpmulMIN				0.01		Dimensionless
  F0fpmulMAX				0.9		Dimensionless
  F0fpdivMIN				0.01		Dimensionless
  F0fpdivMAX				0.9		Dimensionless
  
  F0regreadsMIN				0.01		Dimensionless
  F0regreadsMAX				10.0		Dimensionless
  F0regwritesMIN			0.01		Dimensionless
  F0regwritesMAX			10.0		Dimensionless
  
  F0bestMispredictMIN			0.0		Dimensionless
  F0bestMispredictMAX			1.0		Dimensionless
  
  NThreadsMIN				0		Dimensionless
  NThreadsMAX				1000000000	Dimensionless
  		
  MPIinstructionsMIN			0		Instruction
  MPIinstructionsMAX			10000000000	Instruction
  MPIisendMIN				0		Instruction
  MPIisendMAX				10000000000	Instruction
  MPIirecvMIN				0		Instruction
  MPIirecvMAX				10000000000	Instruction
  MPIsendMIN				0		Instruction
  MPIsendMAX				10000000000	Instruction
  MPIrecvMIN				0		Instruction
  MPIrecvMAX				10000000000	Instruction
  MPIbarrierMIN				0		Instruction
  MPIbarrierMAX				10000000000	Instruction
  MPIbcastMIN				0		Instruction
  MPIbcastMAX				10000000000	Instruction
  MPIsentBytesMIN			0		Dimensionless
  MPIsentBytesMAX			10000000000	Dimensionless
  MPIrecvBytesMIN			0		Dimensionless
  MPIrecvBytesMAX			10000000000	Dimensionless
  MPIsentMessagesMIN			0		Dimensionless
  MPIsentMessagesMAX			10000000000	Dimensionless
  MPIrecvMessagesMIN			0		Dimensionless
  MPIrecvMessagesMAX			10000000000	Dimensionless
  MPIcommVectorMIN			0		Dimensionless
  MPIcommVectorMAX			0		Dimensionless
   
  MPIreduceMIN				0		Dimensionless
  MPIallreduceMIN			0		Dimensionless
  MPIgatherMIN				0		Dimensionless
  MPIallgatherMIN			0		Dimensionless
  MPIalltoallMIN			0		Dimensionless
  MPIbcastSentBytesMIN			0		Dimensionless
  MPIbcastRecvBytesMIN			0		Dimensionless
  MPIalltoallSentBytesMIN		0		Dimensionless
  MPIalltoallRecvBytesMIN		0		Dimensionless
  MPIgatherSentBytesMIN			0		Dimensionless
  MPIgatherRecvBytesMIN			0		Dimensionless
  MPIallgatherSentBytesMIN		0		Dimensionless
  MPIallgatherRecvBytesMIN		0		Dimensionless
  MPIreduceRootBytesMIN			0		Dimensionless
  MPIreduceNotRootBytesMIN		0		Dimensionless
  MPIallreduceBytesMIN			0		Dimensionless
  MPIreduceMAX				10000000000	Dimensionless
  MPIallreduceMAX			10000000000	Dimensionless
  MPIgatherMAX				10000000000	Dimensionless
  MPIallgatherMAX			10000000000		 Dimensionless
  MPIalltoallMAX			10000000000		 Dimensionless
  MPIbcastSentBytesMAX			10000000000		 Dimensionless
  MPIbcastRecvBytesMAX			10000000000		 Dimensionless
  MPIalltoallSentBytesMAX		10000000000		 Dimensionless
  MPIalltoallRecvBytesMAX		10000000000		 Dimensionless
  MPIgatherSentBytesMAX			10000000000		 Dimensionless
  MPIgatherRecvBytesMAX			10000000000		 Dimensionless
  MPIallgatherSentBytesMAX		10000000000		 Dimensionless
  MPIallgatherRecvBytesMAX		10000000000		 Dimensionless
  MPIreduceRootBytesMAX			10000000000		 Dimensionless
  MPIreduceNotRootBytesMAX		10000000000		 Dimensionless
  MPIallreduceBytesMAX			10000000000		 Dimensionless
  
  OpenMPtotalMAX			10000000000		Dimensionless
  OpenMPtotalMIN			0			Dimensionless
  OpenMPsynchronizationMAX		10000000000		Dimensionless
  OpenMPsynchronizationMIN		0			Dimensionless
  OpenMPatomicMAX			10000000000		Dimensionless
  OpenMPatomicMIN			0			Dimensionless
  OpenMPstartupShutdownMAX		10000000000		Dimensionless
  OpenMPstartupShutdownMIN		0			Dimensionless
  OpenMPparallelMAX			10000000000		Dimensionless
  OpenMPparallelMIN			0			Dimensionless
  OpenMPthreadInfoMAX			10000000000		Dimensionless
  OpenMPthreadInfoMIN			0			Dimensionless
  OpenMPworkSharingMAX			10000000000		Dimensionless
  OpenMPworkSharingMIN			0			Dimensionless
  OpenMPthreadPrivateDataSupportMAX	10000000000		Dimensionless
  OpenMPthreadPrivateDataSupportMIN	0			Dimensionless
  OpenMPtaskingSupportMAX		10000000000		Dimensionless
  OpenMPtaskingSupportMIN		0			Dimensionless
  OpenMPothersMAX			10000000000		Dimensionless
  OpenMPothersMIN			0			Dimensionless
  
  M0MemoryFootprintMIN			0			Bytes
  M0MemoryFootprintMAX			10000000000000000	Bytes
";

AlgorithmRangeDefaults = {#[[1]], If[StringQ[#[[2]]],ToExpression[#[[2]]],#[[2]]]} & /@
							Select[ImportString[AlgorithmRangeDefaultsTXT, "Table"], Length[#] >= 2 &];

InitAlgorithmMinMax[] :=
Block[{AlgMinMax,
	ILP0MIN, ILP0WindowSizeMIN, ILP0memMIN, ILP0intMIN, ILP0controlMIN, ILP0fpMIN,
    ILPinorder0MIN, ILPinorder0memMIN, ILPinorder0intMIN, ILPinorder0controlMIN, ILPinorder0fpMIN,
    LSysMIN, F0controlMIN, F0addrMIN, F0intMIN, F0intOnlyMIN, 
    F0fpMIN, F0memMIN, F0loadMIN, F0storeMIN, F0vectorMIN, F0otherMIN, F0regreadsMIN, F0regwritesMIN,
    F0intmulMIN, F0intdivMIN, F0fpmulMIN, F0fpdivMIN, F0bestMispredictMIN,
    NThreadsMIN, MPIinstructionsMIN, MPIisendMIN, MPIirecvMIN, MPIsendMIN, MPIrecvMIN, MPIbarrierMIN, MPIbcastMIN,
    MPIsentBytesMIN, MPIrecvBytesMIN, MPIsentMessagesMIN, MPIrecvMessagesMIN, MPIcommVectorMIN,
    MPIreduceMIN,   MPIallreduceMIN,   MPIgatherMIN,   MPIallgatherMIN,   MPIalltoallMIN,  
    MPIbcastSentBytesMIN,  MPIbcastRecvBytesMIN,   MPIalltoallSentBytesMIN,   MPIalltoallRecvBytesMIN,  
    MPIgatherSentBytesMIN,   MPIgatherRecvBytesMIN,   MPIallgatherSentBytesMIN,   MPIallgatherRecvBytesMIN,   MPIreduceRootBytesMIN,   MPIreduceNotRootBytesMIN,   MPIallreduceBytesMIN,
	OpenMPtotalMIN, OpenMPsynchronizationMIN, OpenMPatomicMIN, OpenMPstartupShutdownMIN, OpenMPparallelMIN,
	OpenMPthreadInfoMIN, OpenMPworkSharingMIN, OpenMPthreadPrivateDataSupportMIN, OpenMPtaskingSupportMIN,
	OpenMPothersMIN, M0MemoryFootprintMIN,
	
	ILP0MAX, ILP0WindowSizeMAX, ILP0memMAX, ILP0intMAX, ILP0controlMAX, ILP0fpMAX,
    ILPinorder0MAX, ILPinorder0memMAX, ILPinorder0intMAX, ILPinorder0controlMAX, ILPinorder0fpMAX,
    LSysMAX, F0controlMAX, F0addrMAX, F0intMAX, F0intOnlyMAX,
    F0fpMAX, F0memMAX, F0loadMAX, F0storeMAX, F0vectorMAX, F0otherMAX, F0regreadsMAX, F0regwritesMAX,
    F0intmulMAX, F0intdivMAX, F0fpmulMAX, F0fpdivMAX, F0bestMispredictMAX,
    NThreadsMAX, MPIinstructionsMAX, MPIisendMAX, MPIirecvMAX, MPIsendMAX, MPIrecvMAX, MPIbarrierMAX, MPIbcastMAX,
    MPIsentBytesMAX, MPIrecvBytesMAX, MPIsentMessagesMAX, MPIrecvMessagesMAX, MPIcommVectorMAX,
    MPIreduceMAX,   MPIallreduceMAX,   MPIgatherMAX,   MPIallgatherMAX,   MPIalltoallMAX,  
    MPIbcastSentBytesMAX,   MPIbcastRecvBytesMAX,   MPIalltoallSentBytesMAX,   MPIalltoallRecvBytesMAX,  
    MPIgatherSentBytesMAX,   MPIgatherRecvBytesMAX,   MPIallgatherSentBytesMAX,   MPIallgatherRecvBytesMAX,   MPIreduceRootBytesMAX,   MPIreduceNotRootBytesMAX,   MPIallreduceBytesMAX,
	OpenMPtotalMAX, OpenMPsynchronizationMAX, OpenMPatomicMAX, OpenMPstartupShutdownMAX, OpenMPparallelMAX,
	OpenMPthreadInfoMAX, OpenMPworkSharingMAX, OpenMPthreadPrivateDataSupportMAX, OpenMPtaskingSupportMAX,
	OpenMPothersMAX, M0MemoryFootprintMAX
    },
    
    (*Generate AlgMinMax in a slightly less error-prone way than before *)
	(* We construct the list of tuples using SymbolName *)
	(* First setup the structure of the datastructure, as SymbolName[] only works on unevaluated definitions (Map[] or /@ evaluates them already) *)
	AlgMinMax = {SymbolName[#], ExaBoundsGetTableValue[AlgorithmRangeDefaults, SymbolName[#]]} & /@ {
										ILP0MIN, ILP0WindowSizeMIN, ILP0memMIN, ILP0intMIN, ILP0controlMIN, ILP0fpMIN,
										ILPinorder0MIN, ILPinorder0memMIN, ILPinorder0intMIN, ILPinorder0controlMIN, ILPinorder0fpMIN,
										LSysMIN, 
										F0controlMIN, F0addrMIN, F0intMIN, F0intOnlyMIN, F0fpMIN, F0memMIN, F0loadMIN, F0storeMIN, F0vectorMIN, F0otherMIN, F0regreadsMIN, F0regwritesMIN, F0intmulMIN, F0intdivMIN, F0fpmulMIN, F0fpdivMIN, 
										F0bestMispredictMIN,
    									NThreadsMIN, MPIinstructionsMIN, MPIisendMIN, MPIirecvMIN, MPIsendMIN, MPIrecvMIN, MPIbarrierMIN,
    									MPIbcastMIN, MPIsentBytesMIN, MPIrecvBytesMIN, MPIsentMessagesMIN, MPIrecvMessagesMIN, MPIcommVectorMIN,
    									MPIreduceMIN,   MPIallreduceMIN,   MPIgatherMIN,   MPIallgatherMIN,   MPIalltoallMIN,   MPIbcastSentBytesMIN,   MPIbcastRecvBytesMIN,   MPIalltoallSentBytesMIN,   MPIalltoallRecvBytesMIN,  
    									MPIgatherSentBytesMIN,   MPIgatherRecvBytesMIN,   MPIallgatherSentBytesMIN,   MPIallgatherRecvBytesMIN,   MPIreduceRootBytesMIN,   MPIreduceNotRootBytesMIN,   MPIallreduceBytesMIN,
										OpenMPtotalMIN, OpenMPsynchronizationMIN, OpenMPatomicMIN, OpenMPstartupShutdownMIN, OpenMPparallelMIN,
										OpenMPthreadInfoMIN, OpenMPworkSharingMIN, OpenMPthreadPrivateDataSupportMIN, OpenMPtaskingSupportMIN,
										OpenMPothersMIN, M0MemoryFootprintMIN,
										
										ILP0MAX, ILP0WindowSizeMAX, ILP0memMAX, ILP0intMAX, ILP0controlMAX, ILP0fpMAX,
										ILPinorder0MAX, ILPinorder0memMAX, ILPinorder0intMAX, ILPinorder0controlMAX, ILPinorder0fpMAX,
										LSysMAX, 
										F0controlMAX, F0addrMAX, F0intMAX, F0intOnlyMAX, F0fpMAX, F0memMAX, F0loadMAX, F0storeMAX, F0vectorMAX, F0otherMAX, F0regreadsMAX, F0regwritesMAX, F0intmulMAX, F0intdivMAX, F0fpmulMAX, F0fpdivMAX,
										F0bestMispredictMAX,
	    								NThreadsMAX, MPIinstructionsMAX, MPIisendMAX, MPIirecvMAX, MPIsendMAX, MPIrecvMAX, MPIbarrierMAX,
    									MPIbcastMAX, MPIsentBytesMAX, MPIrecvBytesMAX, MPIsentMessagesMAX, MPIrecvMessagesMAX, MPIcommVectorMAX,
    									MPIreduceMAX,   MPIallreduceMAX,   MPIgatherMAX,   MPIallgatherMAX,   MPIalltoallMAX,   MPIbcastSentBytesMAX,   MPIbcastRecvBytesMAX,   MPIalltoallSentBytesMAX,   MPIalltoallRecvBytesMAX,   
    									MPIgatherSentBytesMAX,   MPIgatherRecvBytesMAX,   MPIallgatherSentBytesMAX,   MPIallgatherRecvBytesMAX,   MPIreduceRootBytesMAX,   MPIreduceNotRootBytesMAX,   MPIallreduceBytesMAX,
										OpenMPtotalMAX, OpenMPsynchronizationMAX, OpenMPatomicMAX, OpenMPstartupShutdownMAX, OpenMPparallelMAX,
										OpenMPthreadInfoMAX, OpenMPworkSharingMAX, OpenMPthreadPrivateDataSupportMAX, OpenMPtaskingSupportMAX,
										OpenMPothersMAX, M0MemoryFootprintMAX
										};
    Return[AlgMinMax];
];
	
InitAlgorithmDefaults[] :=
Block[{algDefaults, algMinMax, ILP0, ILP0WindowSize,
	ILP0mem, ILP0int, ILP0control, ILP0fp,
	ILPinorder0, ILPinorder0mem, ILPinorder0int, ILPinorder0control, ILPinorder0fp,
	D0dreuse, D0dreuse128, D0ireuse,
	LSys, F0addr, F0int, F0intOnly, F0fp, F0control, F0mem, F0load, F0store, F0vector, F0other, F0regreads, F0regwrites, 
	F0intmul, F0intdiv, F0fpmul, F0fpdiv, H0branch, F0bestMispredict, FunGetDefault,
	NThreads, MPIinstructions, MPIisend, MPIsend, MPIrecv, MPIbarrier,
    MPIbcast, MPIirecv, MPIsentBytes, MPIrecvBytes, MPIsentMessages, MPIrecvMessages, MPIcommVector,
    MPIreduce,   MPIallreduce,   MPIgather,   MPIallgather,   MPIalltoall,   MPIbcastSentBytes,   MPIbcastRecvBytes,   MPIalltoallSentBytes,  
    MPIalltoallRecvBytes,   MPIgatherSentBytes,   MPIgatherRecvBytes,   MPIallgatherSentBytes,   MPIallgatherRecvBytes,   MPIreduceRootBytes,  
    MPIreduceNotRootBytes,   MPIallreduceBytes,
	OpenMPtotal, OpenMPsynchronization, OpenMPatomic, OpenMPstartupShutdown, OpenMPparallel,
	OpenMPthreadInfo, OpenMPworkSharing, OpenMPthreadPrivateDataSupport, OpenMPtaskingSupport,
	OpenMPothers,LibraryCalls,M0MemoryFootprint,CommunicationPattern},
	
	algMinMax = InitAlgorithmMinMax[];
	
	(* First setup the structure of the datastructure, as SymbolName[] only works on unevaluated definitions (Map[] or /@ evaluates them already) *)
	algDefaults = {SymbolName[#], FunGetDefault[SymbolName[#]]} & /@ 	{ILP0, ILP0WindowSize, ILP0mem, ILP0int, ILP0control, ILP0fp,
											ILPinorder0, ILPinorder0mem, ILPinorder0int, ILPinorder0control, ILPinorder0fp,
											LSys,
											F0addr, F0int, F0intOnly, F0fp, F0control, F0mem, F0load, F0store, F0vector, F0other, F0regreads, F0regwrites, F0intmul, F0intdiv, F0fpmul, F0fpdiv,
											F0bestMispredict,
    										NThreads, MPIinstructions, MPIisend, MPIirecv, MPIsend, MPIrecv, MPIbarrier,
    										MPIbcast, MPIsentBytes, MPIrecvBytes, MPIsentMessages, MPIrecvMessages, MPIcommVector,
    										MPIreduce,   MPIallreduce,   MPIgather,   MPIallgather,   MPIalltoall,   MPIbcastSentBytes,   MPIbcastRecvBytes,   MPIalltoallSentBytes,   MPIalltoallRecvBytes,  
    										MPIgatherSentBytes,   MPIgatherRecvBytes,   MPIallgatherSentBytes,   MPIallgatherRecvBytes,   MPIreduceRootBytes,   MPIreduceNotRootBytes,   MPIallreduceBytes,
											OpenMPtotal, OpenMPsynchronization, OpenMPatomic, OpenMPstartupShutdown, OpenMPparallel,
											OpenMPthreadInfo, OpenMPworkSharing, OpenMPthreadPrivateDataSupport, OpenMPtaskingSupport,
											OpenMPothers,M0MemoryFootprint
											};
		
	FunGetDefault[property_] := Max[GetKeyValue[algMinMax, property<>"MIN"], ExaBoundsGetTableValue[ExaBoundsAppDefaults, property]];

	D0dreuse = ExaBoundsGetTableValue[ExaBoundsAppDefaults, "D0dreuse"];
	D0dreuse128 = ExaBoundsGetTableValue[ExaBoundsAppDefaults, "D0dreuse128"];
	D0ireuse = ExaBoundsGetTableValue[ExaBoundsAppDefaults, "D0ireuse"];
	LibraryCalls = ExaBoundsGetTableValue[ExaBoundsAppDefaults, "LibraryCalls"];
	H0branch = ExaBoundsGetTableValue[ExaBoundsAppDefaults, "H0branch"];

	algDefaults = Join[algDefaults,
		{{"D0dreuse", D0dreuse}, {"D0dreuse128", D0dreuse128}, {"D0ireuse", D0ireuse}, {"LibraryCalls", LibraryCalls}, {"H0branch", H0branch}}
	];

	CommunicationPattern = ToExpression[ExaBoundsGetTableValue[ExaBoundsAppDefaults, "CommunicationPattern"]];
	algDefaults = Join[{{"CommunicationPattern", CommunicationPattern}}, algDefaults];
	
	Return[algDefaults];
];

ExaBoundsAppDefaultsTXT = "
  ILP0 			1
  
  ILP0WindowSize 	1
  
  ILPinorder0 		1
  
  ILP0mem		1000
  ILP0int		1000
  ILP0control		1000
  ILP0fp		1000
  
  ILPinorder0mem	1000
  ILPinorder0int	1000
  ILPinorder0control	1000
  ILPinorder0fp		1000
  
  LSys 			1E15
  
  D0dreuse 		{{1,0}}
  D0dreuse128		{{1,0}}
  D0ireuse 		{{1,0}}
  F0control 		0
  F0addr 		0
  F0int 		0
  F0intOnly		0
  F0fp 			0
  F0mem 		0
  F0load		0
  F0store		0
  F0vector      	0
  F0other 		0
  
  F0intmul		0
  F0intdiv		0
  F0fpmul		0
  F0fpdiv		0
  
  F0regreads		0
  F0regwrites		0
  
  H0branch		{}
  F0bestMispredict	0
  
  NThreads 		1
  
  MPIinstructions	0
  MPIisend		0
  MPIirecv 		0 
  MPIsend		0
  MPIrecv 		0 
  MPIbarrier		0
  MPIbcast 		0
  MPIsentBytes		0
  MPIrecvBytes		0
  MPIsentMessages	0
  MPIrecvMessages	0
  MPIcommVector		0
  MPIreduce		0
	MPIallreduce	0
	MPIgather	0
	MPIallgather	0
	MPIalltoall	0
	MPIbcastSentBytes	0
	MPIbcastRecvBytes	0
	MPIalltoallSentBytes	0
	MPIalltoallRecvBytes	0
	MPIgatherSentBytes	0
	MPIgatherRecvBytes	0
	MPIallgatherSentBytes	0
	MPIallgatherRecvBytes	0
	MPIreduceRootBytes	0
	MPIreduceNotRootBytes	0
	MPIallreduceBytes	0
  
  OpenMPtotal		0
  OpenMPsynchronization	0
  OpenMPatomic		0
  OpenMPstartupShutdown	0
  OpenMPparallel	0
  OpenMPthreadInfo	0
  OpenMPworkSharing	0
  OpenMPthreadPrivateDataSupport	0
  OpenMPtaskingSupport	0
  OpenMPothers		0
  
  LibraryCalls		{}
  CommunicationPattern 	{}

  M0MemoryFootprint	0
  ";

ExaBoundsAppDefaults = {#[[1]], If[StringQ[#[[2]]],ToExpression[#[[2]]],#[[2]]]} & /@
							Select[ImportString[ExaBoundsAppDefaultsTXT, "Table"], Length[#] >= 2 &]; 

GetAlgorithmDefaultValue[key_] := GetKeyValue[ExaBoundsAppDefaults, key];

(* Locally global algDefaults variable. We let InitAlgorithmMinMax setup this structure with a default set of algorithm parameters. This is returned by GetDefaultAlgorithmProperties[] *)
(* Note: direct assignment, we only need to set this on import *)  
GetDefaultAlgorithmProperties[] = InitAlgorithmDefaults[];

End[] (* End Private Context *)

EndPackage[]
