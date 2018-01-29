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

BeginPackage["CPUMultithreadedModels`"]
(* Exported symbols added here with SymbolName::usage *)  

Needs["ExaBoundsGeneric`"];
Needs["ModelWarnings`"];
Needs["CPUPerformanceModels`"];
Needs["Support`"];

ClearCPUMultithreadedModelsCache::usage = 
  "ClearCPUMultithreadedModelsCache[]"

AccessesPerSecond::usage = ""

SolveCachePressure::usage =
  "SolveCachePressure[archProperties_, algProperties_, homogeneousThreads_:1]"

SolveDRAMBandwidthPressure::usage =
  "SolveDRAMBandwidthPressure[archProperties_, algProperties_, homogeneousThreads_:1]"
  
MCd0::usage = "MCd0[archProperties_, algProperties_] or MCd0[thread_, archProperties_, algProperties_]"
MCExecutionSeconds::usage = "MCExecutionSeconds[archProperties_, algProperties_] or MCExecutionSeconds[thread_, archProperties_, algProperties_]"
MCExecutionCycles::usage = "MCExecutionCycles[archProperties_, algProperties_] or MCExecutionSeconds[thread_, archProperties_, algProperties_]"
MCFMispredicted::usage = "MCFMispredicted[archProperties_, algProperties_] or MCFMispredicted[thread_, archProperties_, algProperties_]"
MCCacheMissrate::usage = "MCCacheMissrate[level_, archProperties_, algProperties_] or MCCacheMissrate[thread_, level_ archProperties_, algProperties_]"
MCFiMem::usage = "MCFiMem[level_, archProperties_, algProperties_] or MCFiMem[thread_, level_ archProperties_, algProperties_]"
MCBappiDmo::usage = "MCBappiDmo[level_, archProperties_, algProperties_] or MCBappiDmo[thread_, level_ archProperties_, algProperties_]"
MCAchievedFLOPSi::usage = "MCAchievedFLOPSi[level_, archProperties_, algProperties_] or MCAchievedFLOPSi[thread_, level_ archProperties_, algProperties_]"
MCGetKeyValue::usage = "MCGetKeyValue[algProperties_, key_]"
MCGetArchKeyValueCheck::usage = "MCGetArchKeyValueCheck[algProperties_, key_]"
MCGetAlgKeyValueCheck::usage = "MCGetAlgKeyValueCheck[algProperties_, key_]"
MCGetTotalOperationCount::usage = "MCGetTotalOperationCount[algProperties_, key_]"


Begin["`Private`"] (* Begin Private Context *)

ClearCPUMultithreadedModelsCache[] :=
Block[{},
	ClearCache[SolveCachePressure];
	ClearCache[Symbol[#]] & /@ multithreadedModelFunctions;
	ClearCache[Symbol[#]] & /@ multithreadedModelFunctionsIndexArgument;
];

(* NOTE: all external functions need to be based on BlockCheck[] instead of Block[] for correct display of warnings when using GetArch/AlgKeyValue *)

AccessesPerSecond[archProperties_, algProperties_, M1_?NumericQ] := BlockCheck[{architectedM1, accessesPerInstruction},
	architectedM1 = SetKeyValue[archProperties, "M1", M1];
	accessesPerInstruction = FiMem[MemoryL3Cache[], architectedM1, algProperties];
	Return[accessesPerInstruction/d0[architectedM1, algProperties]];
]; 

(* We solve for the cache pressure using the method presented in
"cache contention and application performance prediction for multi-core systems by Xu, Chen, Dick, and Mao, ISPASS 2010 *)
(* We implement the Access Based (AB) technique, as it is straightforward using available ExaBounds metrics and it's error of ~3% is good enough for our models *)
(* We extend their model to two layers of shared cache *)
(* Input:
	archProperties: the architecture under test
	algProperties: either one algorithm under test from ExaBoundsAlgInfo or a list of replacement rules where n -> algProperties refers to running algProperties on thread/core n. if the total number of rules mismatches the number of cores, it is assumed that the remaining rules or cores are not used *)
(* Return:
	Either one list of replacement rules if there is only one algorithm under test, or a list of a list of replacement rules per thread *) 
(* cores and threads are counted core-first: for a 4-core, smt-2 machine, thread 5 refers to a workload on core 1, second thread *)
SolveCachePressure[archProperties_List, algProperties_List, homogeneousThreads_Integer : 1] :=
SolveCachePressure[archProperties, algProperties, homogeneousThreads] =
BlockCheck[{smtmode, n1, n0threads, hwThreads, M0L1, M0L2, M1, M1Functions, M1EffectiveSizes, M1EffectiveSize1, roots, initialValues},
	
	smtmode = GetKeyValue[archProperties, "SMTmode"];
	n0threads = GetKeyValue[archProperties, "n0threads"];
	n1 = GetKeyValue[archProperties, "n1"]; (* core count *)
	M0L1 = GetKeyValue[archProperties, "M0L1"]; (* L1 cache *)
	M0L2 = GetKeyValue[archProperties, "M0L2"]; (* L2 cache *)
	M1 = GetKeyValue[archProperties, "M1"]; (* L3 *)
		
	hwThreads = If[smtmode != SMTModeDynamicResources[], smtmode, n0threads]; (* The total number of HW threads/core. This is either the smtmode (1 = disabled), 2 = fixed to two) or n0threads for the dynamic mode *)
	
	(* TODO: SMT *)
	
	If [AllTrue[algProperties, Head[#] === List &],
		(* Running the same algorithm on all threads: equal share *)
		If[n1 < homogeneousThreads, ThreadMismatchWarning[n1, homogeneousThreads]];
		Return[Table[i->{"M0L1" -> M0L1, "M0L2" -> M0L2, "M1" -> M1 / Min[homogeneousThreads, n1]}, {i, Min[homogeneousThreads, n1]}]]
	];
	
	(* If not all threads are equal, we need to solve for the cache pressure *)
	
	If [!AllTrue[algProperties, Head[#] === Rule &],
		(* No valid replacement list *)
		Return[Null]
	];
	
	If [Length[algProperties] == 1,
		(* Only one thread, gets full cache *)
		Return[{algProperties[[1,1]] -> {"M0L1" -> M0L1, "M0L2" -> M0L2, "M1" -> M1}}]
	];
	
		(* First, the easy case: no SMT *)
	(* TODO: fix situation in which the replacement list is too long! No check on total number of threads is done *)
	If [hwThreads == 1 && AllTrue[algProperties, #[[1]] <= n1 &],
		(* Only interested in L3 cache *)
		(* First, we generate a list of symbols, one for each thread, to use in the equations *)
		M1EffectiveSizes = Table[Symbol["M1EffectiveSize" <> ToString[i]], {i, Length[algProperties]}];
		(* We define the cache pressure functions according to the AB model in Xu et al. Eq. 17 *)
		(* We use the above defined symbols, except for the M1EffectiveSize1 for which we need an explicit symbol where we can assign to *)
		M1Functions := Table[AccessesPerSecond[archProperties, algProperties[[1,2]], M1EffectiveSize1] / AccessesPerSecond[archProperties, algProperties[[i,2]], M1EffectiveSizes[[i]]] - M1EffectiveSize1 / M1EffectiveSizes[[i]], {i, 2, Length[algProperties]}];
		(* The sum of all effective sizes is bounded by the actual L3 cache size *)
		M1EffectiveSize1 = M1 - Total[Table[M1EffectiveSizes[[i]], {i, 2 , Length[algProperties]}]];
		(* The initial guess for the solver is an equal cache pressure for all threads *) 
		initialValues = Table[{Symbol["M1EffectiveSize" <> ToString[i]], M1/Length[algProperties]}, {i, 2, Length[algProperties]}];
		(* Solve the model *)
		roots = FindRoot[M1Functions, initialValues];
		Return[Table[
			(* M0L1 and M0L2 are the full size without SMT *)
			algProperties[[i,1]] -> {
				"M0L1" -> M0L1,
				"M0L2" -> M0L2,
				"M1" -> If[i == 1,
							(* For i = 1, calculate basd on solutions for i != 1 *)
							M1 - Total[roots[[;;,2]]],
							(* otherwise (i!=1), the solution *)
							roots[[i-1,2]]
						]
				},
			{i, Length[algProperties]}
		]];
	];
	
	(* Provide warning if any thread number is larger than the number of cores *)
	If[n1 < Max[algProperties[[;;,1]]], ThreadMismatchWarning[n1, Max[algProperties[[;;,1]]]]];
	
	(* For SMT, we need to incorporate M0L1 and M0L2 as well *)
		
];

(* We solve for the DRAM bandwidth  pressure using the method presented in our ICCD paper *)
(* Input:
	archProperties: the architecture under test
	algProperties: either one algorithm under test from ExaBoundsAlgInfo or a list of replacement rules where n -> algProperties refers to running algProperties on thread/core n. if the total number of rules mismatches the number of cores, it is assumed that the remaining rules or cores are not used *)
(* Return:
	Either one list of replacement rules if there is only one algorithm under test, or a list of a list of replacement rules per thread *) 
(* cores and threads are counted core-first: for a 4-core, smt-2 machine, thread 5 refers to a workload on core 1, second thread *)
SolveDRAMBandwidthPressure[archProperties_List, algProperties_List, homogeneousThreads_Integer : 1] :=
BlockCheck[{smtmode, n1, n0threads, hwThreads, B0Dmo, B1Dmo, B2Dmo, unrestrictedBapp2Dmo, totalBapp2Dmo, newB2Dmo},
	
	smtmode = GetKeyValue[archProperties, "SMTmode"];
	n0threads = GetKeyValue[archProperties, "n0threads"];
	n1 = GetKeyValue[archProperties, "n1"]; (* core count *)
	B0Dmo = GetKeyValue[archProperties, "B0Dmo"]; (* core <-> L1/L2 BW*)
	B1Dmo = GetKeyValue[archProperties, "B1Dmo"]; (* L2 <-> L3 BW *)
	B2Dmo = GetKeyValue[archProperties, "B2Dmo"]; (* L3 <-> DRAM BW *)
		
	hwThreads = If[smtmode != SMTModeDynamicResources[], smtmode, n0threads]; (* The total number of HW threads/core. This is either the smtmode (1 = disabled), 2 = fixed to two) or n0threads for the dynamic mode *)
	
	(* TODO: SMT *)
	(* Without SMT only B2DMo is influenced *)
	
	If [AllTrue[algProperties, Head[#] === List &],
		(* Running the same algorithm on all threads: equal share *)
		If[n1 < homogeneousThreads, ThreadMismatchWarning[n1, homogeneousThreads]];
		Return[Table[i->{"B0Dmo" -> B0Dmo, "B1Dmo" -> B1Dmo, "B2Dmo" -> B2Dmo / Min[homogeneousThreads, n1]}, {i, Min[homogeneousThreads, n1]}]]
	];
	
	(* If not all threads are equal, we need to solve for the cache pressure *)
	
	If [!AllTrue[algProperties, Head[#] === Rule &],
		(* No valid replacement list *)
		Return[Null]
	];
	
	If [Length[algProperties] == 1,
		(* Only one thread, gets full cache *)
		Return[{algProperties[[1,1]] -> {"B0Dmo" -> B0Dmo, "B1Dmo" -> B1Dmo, "B2Dmo" -> B2Dmo}}]
	];
	
	(* First, the easy case: no SMT *)
	(* TODO: fix situation in which the replacement list is too long! No check on total number of threads is done *)
	If [hwThreads == 1 && AllTrue[algProperties, #[[1]] <= n1 &],
		(* Only calculate L3 <-> DRAM BW *)
		(* Get the achieved bandwidths *)
		unrestrictedBapp2Dmo = MCBappiDmo[SocketLayer[], archProperties, algProperties, homogeneousThreads, False];
		(* Calculate total runtime *)
		totalBapp2Dmo = Total[unrestrictedBapp2Dmo];
		(* Calculate the individual limimts *)
		newB2Dmo = unrestrictedBapp2Dmo * (B2Dmo / totalBapp2Dmo);
		(* Above, B2Dmo / totalBapp2Dmo = 1 if it matches the total BW and we simply set the unrestricted BW which should not influence. *)
		(* If totalBapp2Dmo is larger than B2Dmo, we set newB2Dmo to a smaller value. The total should not exceed B2Dmo as Total[unrestrictedBapp2Dmo / totalBapp2Dmo] == 1 *)
		(* If totalBapp2Dmo the same holds: newB2Dmo is set to a larger value, but the total cannot exceed B2Dmo *)
		 
		(* For safety, we check the total of newB2Dmo *)
		If[Total[newB2Dmo] > B2Dmo, Message[SolveDRAMBandwidthPressure::totalBWtoolarge, newB2Dmo, B2Dmo]];
		
		Return[Table[
			(* B0Dmo and B1Dmo are the full size without SMT *)
			algProperties[[i,1]] -> {
				"B0Dmo" -> B0Dmo,
				"B1Dmo" -> B1Dmo,
				"B2Dmo" -> newB2Dmo[[i]]
				},
			{i, Length[algProperties]}
		]];
	];
	
	(* Provide warning if any thread number is larger than the number of cores *)
	If[n1 < Max[algProperties[[;;,1]]], ThreadMismatchWarning[n1, Max[algProperties[[;;,1]]]]];
	
	(* For SMT, we need to incorporate M0L1 and M0L2 as well *)
		
];
SolveDRAMBandwidthPressure::totalBWtoolarge = "Error: the total effective DRAM BW per thread of `1` B/s exceeds the architected bandwidth B2Dmo of '2' B/s"

(* ************* *)
(* Multi-core versions of single-core model APIs *)
(* ************* *)

(* Input:
	archProperties: the architecture under test
	algProperties: either one algorithm under test (same on all cores) from ExaBoundsAlgInfo or a list of replacement rules where n -> algProperties refers to running algProperties on thread/core n. if the total number of rules mismatches the number of cores, it is assumed that the remaining rules or cores are not used *)

(* Function to automatically generate calls, first element is name, second element is Function definition sequence, third and fourth element are different call sequences *)
multithreadedModelFunctions = {
	"MCd0", 
	"MCExecutionSeconds",
	"MCExecutionCycles",
	"MCFMispredicted"
};
	
multithreadedModelFunctionsIndexArgument = {
	"MCCacheMissrate",
	"MCFiMem",
	"MCBappiDmo",
	"MCAchievedFLOPSi"
};

multithreadedModelFunctionsKeyValueListKey = {
	"MCGetKeyValue",
	"MCGetArchKeyValueCheck",
	"MCGetAlgKeyValueCheck",
	"MCGetTotalOperationCount"
};

(* The function is per-thread threads, the overloaded returns a list with all threads *)

Symbol[#][thread_Integer, archProperties_List, algProperties_List, homogeneousThreads_Integer : 1] :=
Symbol[#][thread, archProperties, algProperties, homogeneousThreads] = 
BlockCheck[{ret, sel, threadArchProperties, threadAlgProperties, cachePressures, DRAMPressures},
	(* Check what we have *)
	(* First, the lists *)
	Which[
		AllTrue[algProperties, Head[#] === List &] && homogeneousThreads == 1, (* List + homogeneousThreads == 1 means: 1 thread running on core 1 *)
			If[thread > 1, Return[Nothing]]; (* Return Nothing if requesting a thread numer larger than 1 *)
			threadArchProperties = archProperties; (* Nothing to do, just set the properties as they are passed *)
			threadAlgProperties = algProperties;
		,
		AllTrue[algProperties, Head[#] === List &], (* List + homogeneousThreads != 1: means: we have N homogeneous threads running on cores 1 to N *)
			If[n1 < homogeneousThreads, ThreadMismatchWarning[n1, homogeneousThreads]];
			If[thread > homogeneousThreads, Return[Nothing]]; (* Return Nothing if requesting a thread numer larger than N *)
			cachePressures = SolveCachePressure[archProperties, algProperties, homogeneousThreads]; (* Solve effective cache size *) 
			If[cachePressures === Null, Return[Null]];
			threadArchProperties = SetKeyValue[archProperties, "M0L1", "M0L1" /. cachePressures[[thread,2]]];	(* Set correct cache size *)
			threadArchProperties = SetKeyValue[threadArchProperties, "M0L2", "M0L2" /. cachePressures[[thread,2]]];
			threadArchProperties = SetKeyValue[threadArchProperties, "M1", "M1" /. cachePressures[[thread,2]]];
			(* Next, we solve for the DRAM BW *)
			DRAMPressures = SolveDRAMBandwidthPressure[threadArchProperties, algProperties, homogeneousThreads];
			If[DRAMPressures === Null, Return[Null]];
			threadArchProperties = SetKeyValue[threadArchProperties, "B0Dmo", "B0Dmo" /. DRAMPressures[[thread,2]]];	(* Set correct BW *)
			threadArchProperties = SetKeyValue[threadArchProperties, "B1Dmo", "B1Dmo" /. DRAMPressures[[thread,2]]];
			threadArchProperties = SetKeyValue[threadArchProperties, "B2Dmo", "B2Dmo" /. DRAMPressures[[thread,2]]];
			threadAlgProperties = algProperties; (* Set the algorithm*) 
		, (* Now, the replacement list cases *)
		AllTrue[algProperties, Head[#] === Rule &] && Length[algProperties] == 1, (* Length 1 = one thread *)
			If[n1 < Max[algProperties[[;;,1]]], ThreadMismatchWarning[n1, Max[algProperties[[;;,1]]]]];
			If[thread != algProperties[[1,1]], Return[Nothing]]; (* Check if this thread is the thread specified in the single replacement rule. If not, return Nothing *)
			threadArchProperties = archProperties;
			threadAlgProperties = algProperties[[1,2]]; (* Set the algorithm*)
		,
		AllTrue[algProperties, Head[#] === Rule &], (* The general case for a replacement rule; heterogeneous threads *)
			If[n1 < Max[algProperties[[;;,1]]], ThreadMismatchWarning[n1, Max[algProperties[[;;,1]]]]];
			sel = FirstPosition[algProperties[[;;,1]], thread];
			If[MissingQ[sel], Return[Nothing]]; (* Nothing running on this thread *)
			cachePressures = SolveCachePressure[archProperties, algProperties]; (* Solve effective cache size *) 
			If[cachePressures === Null, Return[Null]];
			threadArchProperties = SetKeyValue[archProperties, "M0L1", "M0L1" /. cachePressures[[sel[[1]],2]]];	(* Set correct cache size *)
			threadArchProperties = SetKeyValue[threadArchProperties, "M0L2", "M0L2" /. cachePressures[[sel[[1]],2]]];
			threadArchProperties = SetKeyValue[threadArchProperties, "M1", "M1" /. cachePressures[[sel[[1]],2]]];
			(* Next, we solve for the DRAM BW *)
			DRAMPressures = SolveDRAMBandwidthPressure[threadArchProperties, algProperties, homogeneousThreads];
			If[DRAMPressures === Null, Return[Null]];
			threadArchProperties = SetKeyValue[threadArchProperties, "B0Dmo", "B0Dmo" /. DRAMPressures[[sel[[1]],2]]];	(* Set correct BW *)
			threadArchProperties = SetKeyValue[threadArchProperties, "B1Dmo", "B1Dmo" /. DRAMPressures[[sel[[1]],2]]];
			threadArchProperties = SetKeyValue[threadArchProperties, "B2Dmo", "B2Dmo" /. DRAMPressures[[sel[[1]],2]]];
			threadAlgProperties = algProperties[[sel[[1]],2]]; (* Set the algorithm*)
		, (* Otherwise, something is wrong *)
		_,
			Return[Null];
	];
	(* Call the approporiate single-core function *)
	Return[
		Symbol[StringDrop[#,2]][threadArchProperties, threadAlgProperties] (* Cut MC of the start to get the name of the regular function *)
	];
]; & /@ multithreadedModelFunctions

Symbol[#][archProperties_List, algProperties_List, homogeneousThreads_Integer : 1] := 
BlockCheck[{ret, n1},
	n1 = GetArchKeyValueCheck[archProperties, "n1"];
	ret = {};
	If[AllTrue[algProperties, Head[#] === List &],
		(* List input, single thread profile *)
		If[n1 < homogeneousThreads, ThreadMismatchWarning[n1, homogeneousThreads]];
		ret = Symbol[#][1, archProperties, algProperties, homogeneousThreads];
		, (* else, replacement list *)
		If[n1 < Max[algProperties[[;;,1]]], ThreadMismatchWarning[n1, Max[algProperties[[;;,1]]]]];
		Do[
			AppendTo[ret, Symbol[#][thread, archProperties, algProperties, homogeneousThreads]];
			, {thread, n1}
		];
	];
	Return[ret];
]; & /@ multithreadedModelFunctions

(* A copy for additional arg... Should be combined with above code in one way or the other...  *)
(* DRAM Pressure is optional to prevent recursion in SolveDRAMBandwidthPressure*)
Symbol[#][thread_Integer, level_Integer, archProperties_List, algProperties_List, homogeneousThreads_Integer : 1, performDRAMPressure_Symbol: True] :=
Symbol[#][thread, level, archProperties, algProperties, homogeneousThreads, performDRAMPressure] = 
BlockCheck[{ret, sel, threadArchProperties, threadAlgProperties, cachePressures, DRAMPressures},
	(* Check what we have *)
	(* First, the lists *)
	Which[
		AllTrue[algProperties, Head[#] === List &] && homogeneousThreads == 1, (* List + homogeneousThreads == 1 means: 1 thread running on core 1 *)
			If[thread > 1, Return[Nothing]]; (* Return Nothing if requesting a thread numer larger than 1 *)
			threadArchProperties = archProperties; (* Nothing to do, just set the properties as they are passed *)
			threadAlgProperties = algProperties;
		,
		AllTrue[algProperties, Head[#] === List &], (* List + homogeneousThreads != 1: means: we have N homogeneous threads running on cores 1 to N *)
			If[n1 < homogeneousThreads, ThreadMismatchWarning[n1, homogeneousThreads]];
			If[thread > homogeneousThreads, Return[Nothing]]; (* Return Nothing if requesting a thread numer larger than N *)
			cachePressures = SolveCachePressure[archProperties, algProperties, homogeneousThreads]; (* Solve effective cache size *) 
			threadArchProperties = SetKeyValue[archProperties, "M0L1", "M0L1" /. cachePressures[[thread,2]]];	(* Set correct cache size *)
			threadArchProperties = SetKeyValue[threadArchProperties, "M0L2", "M0L2" /. cachePressures[[thread,2]]];
			threadArchProperties = SetKeyValue[threadArchProperties, "M1", "M1" /. cachePressures[[thread,2]]];
			(* Next, we solve for the DRAM BW *)
			If[performDRAMPressure, (* DRAM Pressure is optional to prevent recursion in SolveDRAMBandwidthPressure*)
				DRAMPressures = SolveDRAMBandwidthPressure[threadArchProperties, algProperties, homogeneousThreads];
				If[DRAMPressures === Null, Return[Null]];
				threadArchProperties = SetKeyValue[threadArchProperties, "B0Dmo", "B0Dmo" /. DRAMPressures[[thread,2]]];	(* Set correct BW *)
				threadArchProperties = SetKeyValue[threadArchProperties, "B1Dmo", "B1Dmo" /. DRAMPressures[[thread,2]]];
				threadArchProperties = SetKeyValue[threadArchProperties, "B2Dmo", "B2Dmo" /. DRAMPressures[[thread,2]]];
			];
			threadAlgProperties = algProperties; (* Set the algorithm*) 
		, (* Now, the replacement list cases *)
		AllTrue[algProperties, Head[#] === Rule &] && Length[algProperties] == 1, (* Length 1 = one thread *)
			If[n1 < Max[algProperties[[;;,1]]], ThreadMismatchWarning[n1, Max[algProperties[[;;,1]]]]];
			If[thread != algProperties[[1,1]], Return[Nothing]]; (* Check if this thread is the thread specified in the single replacement rule. If not, return Nothing *)
			threadArchProperties = archProperties;
			threadAlgProperties = algProperties[[1,2]]; (* Set the algorithm*)
		,
		AllTrue[algProperties, Head[#] === Rule &], (* The general case for a replacement rule; heterogeneous threads *)
			If[n1 < Max[algProperties[[;;,1]]], ThreadMismatchWarning[n1, Max[algProperties[[;;,1]]]]];
			sel = FirstPosition[algProperties[[;;,1]], thread];
			If[MissingQ[sel], Return[Nothing]]; (* Nothing running on this thread *)
			cachePressures = SolveCachePressure[archProperties, algProperties]; (* Solve effective cache size *) 
			If[cachePressures === Null, Return[Null]];
			threadArchProperties = SetKeyValue[archProperties, "M0L1", "M0L1" /. cachePressures[[sel[[1]],2]]];	(* Set correct cache size *)
			threadArchProperties = SetKeyValue[threadArchProperties, "M0L2", "M0L2" /. cachePressures[[sel[[1]],2]]];
			threadArchProperties = SetKeyValue[threadArchProperties, "M1", "M1" /. cachePressures[[sel[[1]],2]]];
			(* Next, we solve for the DRAM BW *)
			If[performDRAMPressure,
				DRAMPressures = SolveDRAMBandwidthPressure[threadArchProperties, algProperties, homogeneousThreads];
				If[DRAMPressures === Null, Return[Null]];
				threadArchProperties = SetKeyValue[threadArchProperties, "B0Dmo", "B0Dmo" /. DRAMPressures[[sel[[1]],2]]];	(* Set correct BW *)
				threadArchProperties = SetKeyValue[threadArchProperties, "B1Dmo", "B1Dmo" /. DRAMPressures[[sel[[1]],2]]];
				threadArchProperties = SetKeyValue[threadArchProperties, "B2Dmo", "B2Dmo" /. DRAMPressures[[sel[[1]],2]]];
			];
			threadAlgProperties = algProperties[[sel[[1]],2]]; (* Set the algorithm*)
		, (* Otherwise, something is wrong *)
		_,
			Return[Null];
	];
	(* Call the approporiate single-core function *)
	Return[
		Symbol[StringDrop[#,2]][level, threadArchProperties, threadAlgProperties] (* Cut MC of the start to get the name of the regular function *)
	];
]; & /@ multithreadedModelFunctionsIndexArgument

Symbol[#][level_Integer, archProperties_List, algProperties_List, homogeneousThreads_Integer : 1, performDRAMPressure_Symbol: True] := 
BlockCheck[{ret, n1},
	n1 = GetArchKeyValueCheck[archProperties, "n1"];
	ret = {};
	If[AllTrue[algProperties, Head[#] === List &],
		(* List input, single thread profile *)
		If[n1 < homogeneousThreads, ThreadMismatchWarning[n1, homogeneousThreads]];
		ret = Symbol[#][1, level, archProperties, algProperties, homogeneousThreads, performDRAMPressure];
		, (* else, replacement list *)
		If[n1 < Max[algProperties[[;;,1]]], ThreadMismatchWarning[n1, Max[algProperties[[;;,1]]]]];
		Do[
			AppendTo[ret, Symbol[#][thread, level, archProperties, algProperties, homogeneousThreads, performDRAMPressure]];
			, {thread, n1}
		];
	];
	Return[ret];
]; & /@ multithreadedModelFunctionsIndexArgument

(* Construct MC version of GetKeyValue and GetTotalOperationCount, returning lists if needed *)
(* This returns a list when a replacement list is given as input and a single value when no replacement list is given *)
(* This means that based oon the output no distinction can be made between a replacement list of lengthe one or a normal homogeneous algorithmProperties *)
Symbol[#][algorithmProperties_, key_] :=
Block[{},
	If[AllTrue[algorithmProperties, Head[#] === Rule &],
		(* Replacement list, so list of stuffs *)
		Return[
			Table[
				Symbol[StringDrop[#,2]][algorithmProperties[[i,2]], key]
				, {i, Length[algorithmProperties]}
			]
		]
	];
	If[AllTrue[algProperties, Head[#] === List &],
		(* Single property, so homogeneous workload *)
		Return[
			Symbol[StringDrop[#,2]][algorithmProperties, key]
		],
		Return[Null];
	]
]; & /@ multithreadedModelFunctionsKeyValueListKey 


End[] (* End Private Context *)

EndPackage[]