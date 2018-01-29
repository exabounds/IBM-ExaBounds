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

BeginPackage["CPUPerformanceModels`"]
(* Exported symbols added here with SymbolName::usage *)

Needs["Support`"]
Needs["ExaBoundsGeneric`"]
Needs["ModelWarnings`"]
Needs["InstructionMix`"]
Needs["VectorSizeConvert`"]
Needs["AlgorithmProperties`"]

CacheHitrate::usage =
  "CacheHitrate[cacheLevel_, archProperties_, algProperties_]"
  
CacheMissrate::usage =
  "CacheMissrate[cacheLevel_, archProperties_, algProperties_]"

ClearCPUPerformanceModelsCache::usage = 
  "ClearCPUPerformanceModelsCache[]"

PeakFLOPSi::usage =
  "PeakFLOPSi[i_, archProperties_]"
  
AchievedFLOPSi::usage = 
  "AchievedFLOPSi[i_, archProperties_, algProperties_]"
  
(*d0icache::usage = 
  "d0icache[archProperties_, algProperties_]"*)

d0branch::usage = 
  "d0branch[archProperties_, algProperties_]"
 
FiMem::usage =
  "FiMem[cacheLevel_, archProperties_, algProperties_]"
  
GetNumberCacheLevels::usage =
  "GetNumberCacheLevels[archProperties_]"

ExistsCacheLevel::usage = 
  "ExistsCacheLevel[cacheLevel_, archProperties_]"
  
BappiDmo::usage =
  "BappiDmo[i_, archProperties_, algProperties_]"

d0::usage = 
  "d0[archProperties_, algProperties_, base_]"
  
d0Optimize::usage =
  "d0Optimize[archProperties_, algProperties_]"
  
ExecutionCycles::usage =
  "ExecutionCycles[archProperties_, algProperties_]"

ExecutionSeconds::usage = 
  "ExecutionSeconds[archProperties_, algProperties_]"

FMispredicted::usage =
  "FMispredicted[ machineProperties_, algorithmProperties_ ]"
  
(* -------- *)
(* Export all optimization LPs for visualization *)
LPIssue::usage =
  "LPIssue[ipc_, archProperties_, algProperties_]"

LPDispatch::usage =
  "LPDispatch[ipc_, archProperties_, algProperties_]"

LPMemMSHR::usage =
  "LPMemMSHR[ipc_, archProperties_, algProperties_]"
		
LPILP::usage =
  "LPILP[ipc_, archProperties_, algProperties_]"

LPMemFU::usage =
  "LPMemFU[ipc_, archProperties_, algProperties_]"

LPIntFU::usage =
  "LPIntFU[ipc_, archProperties_, algProperties_]"

LPFPFU::usage =
  "LPFPFU[ipc_, archProperties_, algProperties_]"

LPControlFU::usage =
  "LPControlFU[ipc_, archProperties_, algProperties_]"

LPVectorFU::usage =
  "LPVectorFU[ipc_, archProperties_, algProperties_]"

LPILPType::usage =
  "LPILPType[ipc_, type_, archProperties_, algProperties_]"
  
LPBandwidthCoreToL1::usage = 
  "LPBandwidthCoreToL1[ipc_, archProperties_, algProperties_]"
  
LPBandwidthL2ToL3::usage = 
  "LPBandwidthL2ToL3[ipc_, archProperties_, algProperties_]"
  
LPBandwidthL3ToDRAM::usage = 
  "LPBandwidthL3ToDRAM[ipc_, archProperties_, algProperties_]"
  
CalculateIQROBStallCycles::usage =
  "CalculateIQROBStallCycles[ipc, archProperties, algProperties]"
  
TotalIQROBBranchStallCycles::usage =
  "TotalIQROBBranchStallCycles[ipc_, archProperties_, algProperties_]"

Begin["`Private`"] (* Begin Private Context *)

(* NOTE: all external functions need to be based on BlockCheck[] instead of Block[] for correct display of warnings when using GetArch/AlgKeyValue *)

(* Clear caches of all cached functions *)
(* KEEP UP TO DATE! *)
ClearCPUPerformanceModelsCache[] :=
Block[{},
	ClearCache[FiMem];
	ClearCache[BappiDmo];
	ClearCache[d0];
];
   
(* Calculate the divisor of core resources for SMT. Static division for SMT modes and static for homogeneous workloads *)
CoreSMTResourceDivisor[archProperties_, algProperties_] :=
Block[{maxTLP, smtmode},
	maxTLP = GetArchKeyValueCheck[archProperties, "maxTLP"];
	smtmode = GetArchKeyValueCheck[archProperties, "SMTmode"];
	
	(* If TLP is maxed, we run on every thread/core/processor *)
	If[maxTLP && smtmode != SMTModeDynamicResources[],
		Return[smtmode]
	];
	
	Return[1]; (* Default to 1 *)
	
];

(* Calculate peak FLOPs performance of each level of the hierarchy *)
PeakFLOPSi[i_, archProperties_] :=
BlockCheck[{},
	If[i > CoreLayer[],
		Return[PeakFLOPSi[i-1, archProperties] * GetArchKeyValueCheck[archProperties, "n" <> ToString[i]]],
		Return[PeakFLOPS0[archProperties]]
	];
];

(* Calculate peak FLOPs on level 0 *)
PeakFLOPS0[archProperties_] :=
Block[{n0float, f0, FLOPS, n0vector, n0vectorbits},
	n0float = n0type[archProperties, "fp"];
	f0 = GetArchKeyValueCheck[archProperties, "f0"];
	n0vector = GetArchKeyValueCheck[archProperties, "n0vectorFU"];
	n0vectorbits = GetArchKeyValueCheck[archProperties, "n0vectorbits"];
	
	FLOPS = n0float * f0 + n0vector * (n0vectorbits / 32) * f0;
	
	Return[FLOPS];
];

(* Calculate the achieved FLOPs on each level of the hierarchy *)
(*  NOTE: this function does not calculate the overall performance of the system, only for the single core case. Other code expects it to return the thread performance when called for any layer i*)
AchievedFLOPSi[i_, archProperties_, algProperties_] :=
BlockCheck[{},
	Return[If[i > 0,
		AchievedFLOPSi[i-1, archProperties, algProperties],
		AchievedFLOPS0[archProperties, algProperties]
	]];
];

(* Core FLOPs performance including SMT *)
AchievedFLOPS0[archProperties_, algProperties_] :=
Block[{F0float, f0, CPI, FLOPSperthread},
	f0 = GetArchKeyValueCheck[archProperties, "f0"];
	F0float = GetAlgKeyValueCheck[algProperties, "F0fp"];
		
	CPI = d0[archProperties, algProperties];
	
	If[Head[F0float]=!=List,
		FLOPSperthread = F0float * f0 / CPI,
		FLOPSperthread = Total[# * (f0 / CPI) * VMixGetData[F0float, VectorLength -> #] & /@ GetVectorLengths[]]
	];
	
	(* Achieved Flops at core level (including SMT):
	Lsys * CPI / f0 = runtime [s]
	Lsys * FFP = Total floating-point ops.
	(Lsys * FFP) / (Lsys * CPI / f0) = FFP * f0 / CPI = FLOPs per thread *)
	
	Return[FLOPSperthread];
];

(* Returned value is a latency in _cycles_ of clock frequency f0 (not seconds) *)
(* TODO: FIX MODEL *)
(*d0icache[archProperties_, algProperties_] := 
BlockCheck[{T0L2latency,  T0DRAMlatency, D0ireuse, L1ireusecap, 
  L1iagran, L1iassoc, TLBreusecap, TLBagran, TLBassoc, cycletime,
  delayL2, delayDRAM, totalDelay},

  
  T0L2latency = GetArchKeyValueCheck[archProperties, "T0L2latency"] / GetArchKeyValueCheck[archProperties, "f0"];
  T0DRAMlatency = GetArchKeyValueCheck[archProperties, "T0DRAMlatency"] / GetArchKeyValueCheck[archProperties, "f0"];
  D0ireuse = GetAlgKeyValueCheck[algProperties, "D0ireuse"];
  L1ireusecap = GetArchKeyValueCheck[archProperties, "L1ireusecap"];
  L1iagran = GetArchKeyValueCheck[archProperties, "L1iagran"];
  L1iassoc = GetArchKeyValueCheck[archProperties, "L1iassoc"];
  TLBreusecap = GetArchKeyValueCheck[archProperties, "TLBreusecap"];
  TLBagran = GetArchKeyValueCheck[archProperties, "TLBagran"];
  TLBassoc = GetArchKeyValueCheck[archProperties, "TLBassoc"];
  cycletime = 1/GetArchKeyValueCheck[archProperties, "f0"];
  
  delayL2 = T0L2latency / cycletime *
    (1 - reuse2hitrate[ D0ireuse, {{L1ireusecap, L1iagran, L1iassoc}} ][[1]]);
  delayDRAM = T0DRAMlatency / cycletime *
    (1 - reuse2hitrate[ D0ireuse, {{TLBreusecap, TLBagran, TLBassoc}} ][[1]]);
  totalDelay = delayL2 + delayDRAM;
  
  Return[ Max[0, totalDelay - WindowDrainTime[archProperties, algProperties] ] ];
];*)

n0type[archProperties_, type_] := GetArchKeyValueCheck[archProperties, "n0" <> type];

(* ------------------------------------------------------------------------------------- *)
(* This is our latest model based on the optimization problem to maximize IPC            *)
(* ------------------------------------------------------------------------------------- *)

(* Define all the constrains for the optimization problem *)

(* Issue width constraint *)
LPIssue[ipc_, archProperties_, algProperties_] := (ipc <= GetArchKeyValueCheck[archProperties, "n0"] / CoreSMTResourceDivisor[archProperties, algProperties]);
(* On average, each thread has 1/SMT parts of the issue and dispatch widths, not architectural *)
(* Dispatch width constraint *)
LPDispatch[ipc_, archProperties_, algProperties_] := (ipc <= GetArchKeyValueCheck[archProperties, "n0dispatch"] / CoreSMTResourceDivisor[archProperties, algProperties]);

(* Calculate fraction of total instructions that perform a memory access that hits a certain level of the cache *)
FiMemHit[cacheLevel_, archProperties_, algProperties_] :=
Which[
	cacheLevel >= MemoryL1Cache[] && cacheLevel < MemoryDRAM[], FiMem[cacheLevel, archProperties, algProperties] - FiMem[cacheLevel+1, archProperties, algProperties],
	cacheLevel == MemoryDRAM[], FiMem[cacheLevel, archProperties, algProperties],
	True, 0
];

(* Mis-status holding register constraint *)
LPMemMSHR[ipc_, archProperties_, algProperties_] :=
BlockCheck[{FeL2, PeL2, FeL3, PeL3, FeDRAM, PeDRAM, n0MSHR},
	FeL2 = FiMemHit[MemoryL2Cache[], archProperties, algProperties];
	PeL2 = CacheLiLatency[MemoryL2Cache[], archProperties, algProperties];
	FeL3 = FiMemHit[MemoryL3Cache[], archProperties, algProperties];
	PeL3 = CacheLiLatency[MemoryL3Cache[], archProperties, algProperties];
	FeDRAM = FiMemHit[MemoryDRAM[], archProperties, algProperties];
	PeDRAM = CacheLiLatency[MemoryDRAM[], archProperties, algProperties];
	n0MSHR = GetArchKeyValueCheck[archProperties, "n0MSHR"];
	
	Return[ipc * (FeL2 * PeL2 + FeL3 * PeL3 + FeDRAM * PeDRAM) <= n0MSHR / CoreSMTResourceDivisor[archProperties, algProperties]];
	(* Each SMT thread has a fraction of the MSHR registers, this is not architectural, but an average *)
];

CheckILPIQSize[archProperties_, algProperties_] :=
Block[{n0IQ, ILP0WindowSize },
	n0IQ = GetArchKeyValueCheck[archProperties, "n0IQ"];
	ILP0WindowSize = GetAlgKeyValueCheck[algProperties, "ILP0WindowSize"];
	
	If[n0IQ != ILP0WindowSize, ILPIQSizeMismatchWarning[n0IQ, ILP0WindowSize]];
];

(* Constraint on instruction-level parallelism (agregate, all instruction types) *)
LPILP[ipc_, archProperties_, algProperties_] :=
BlockCheck[{inorder, ILP, n0ROB},
	CheckILPIQSize[archProperties, algProperties];
	inorder = GetArchKeyValueCheck[archProperties, "inorder"];
	
	ILP = If[inorder, GetAlgKeyValueCheck[algProperties, "ILP0"], GetAlgKeyValueCheck[algProperties, "ILP0"]]; (* The OoO ILP is used for both inorder and OoO architectures *)

	Return[ipc * (EventListMem[archProperties, algProperties] + EventListInt[archProperties, algProperties, True] + EventListFP[archProperties, algProperties]) <= ILP];
];

(* Average delay of integer instructions. Optional argument determines in Address instructions are included in IntOther or not *)
EventListInt[archProperties_, algProperties_, includeAddr_ : False] :=
BlockCheck[{FeIntMul, PeIntMul, FeIntDiv, PeIntDiv, FeIntOther, PeIntOther},
	FeIntMul = GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0intmul"]];
	PeIntMul = GetArchKeyValueCheck[archProperties, "T0intmul"];
	FeIntDiv = GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0intdiv"]];
	PeIntDiv = GetArchKeyValueCheck[archProperties, "T0intdiv"];
	FeIntOther = If[includeAddr,
					(GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0addr"]] + GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0intOnly"]]),
					GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0intOnly"]]] - (FeIntMul + FeIntDiv);
	PeIntOther = GetArchKeyValueCheck[archProperties, "T0intop"];
	
	Return[FeIntMul * PeIntMul + FeIntDiv * PeIntDiv + FeIntOther * PeIntOther];
];

(* Average delay of memory instructions *)
EventListMem[archProperties_, algProperties_] :=
BlockCheck[{FeL1, PeL1, FeL2, PeL2, FeL3, PeL3, FeDRAM, PeDRAM},
	FeL1 = FiMemHit[MemoryL1Cache[], archProperties, algProperties];
	PeL1 = CacheLiLatency[MemoryL1Cache[], archProperties, algProperties];
	FeL2 = FiMemHit[MemoryL2Cache[], archProperties, algProperties];
	PeL2 = CacheLiLatency[MemoryL2Cache[], archProperties, algProperties];
	FeL3 = FiMemHit[MemoryL3Cache[], archProperties, algProperties];
	PeL3 = CacheLiLatency[MemoryL3Cache[], archProperties, algProperties];
	FeDRAM = FiMemHit[MemoryDRAM[], archProperties, algProperties];
	PeDRAM = CacheLiLatency[MemoryDRAM[], archProperties, algProperties];
	
	Return[FeL1 * PeL1 + FeL2 * PeL2 + FeL3 * PeL3 + FeDRAM * PeDRAM];
];

(* Average delay of control instructions *)
EventListControl[ archProperties_, algProperties_] :=
BlockCheck[{FeControl, PeControl},
	FeControl = GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0control"]];
	PeControl = 1;
	
	Return[FeControl * PeControl];
];

(* Average delay of floating-point instructions *)
EventListFP[archProperties_, algProperties_] :=
BlockCheck[{FeFPMul, PeFPMul, FeFPDiv, PeFPDiv, FeFPOther, PeFPOther},
	FeFPMul = GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0fpmul"]];
	PeFPMul = GetArchKeyValueCheck[archProperties, "T0fpmul"];
	FeFPDiv = GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0fpdiv"]];
	PeFPDiv = GetArchKeyValueCheck[archProperties, "T0fpdiv"];
	FeFPOther = GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0fp"]] - (FeFPMul + FeFPDiv);
	PeFPOther = GetArchKeyValueCheck[archProperties, "T0fpop"];
	
	Return[FeFPMul * PeFPMul + FeFPDiv * PeFPDiv + FeFPOther * PeFPOther]
];

(* Constraint on ILP per type of instructions *)
LPILPType[ipc_, type_, archProperties_, algProperties_] :=
BlockCheck[{inorder, ILPtype, eventList},
	CheckILPIQSize[archProperties, algProperties];
	inorder = GetArchKeyValueCheck[archProperties, "inorder"];
	ILPtype = If[inorder, GetAlgKeyValueCheck[algProperties, "ILP0" <> type], GetAlgKeyValueCheck[algProperties, "ILP0" <> type]]; (* The OoO ILP is used for both inorder and OoO architectures *)
	
	eventList = Switch[type,
						"int", EventListInt[archProperties, algProperties],
						"mem", EventListMem[archProperties, algProperties],
						"control", EventListControl[archProperties, algProperties],
						"fp", EventListFP[archProperties, algProperties],
						_, 0
	];
	
	Return[ipc * eventList <= ILPtype];
];

(* Create scalar constraint (only take properties with vector length scalar) or accumulate over all fractions. This is used as, e.g., vector-memory operation or a scalar-memory operation are used by the same FU, while for fp a scalar and a vector unit exist *)
ConstraintFUScalar[ipc_, type_, archProperties_, algProperties_] := ipc * (GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0" <> type], VectorLength->"Scalar"] + If[ type == "int", GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0addr"], VectorLength->"Scalar"], 0]) <= GetArchKeyValueCheck[archProperties, "n0" <> type] / CoreSMTResourceDivisor[archProperties, algProperties]; (* Each SMT thread has only fraction of the units *)
ConstraintFUAll[ipc_, type_, archProperties_, algProperties_]    := ipc * (GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0" <> type], VectorLength->"Accumulate"] + If[ type == "int", GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0addr"], VectorLength->"Accumulate"], 0]) <= GetArchKeyValueCheck[archProperties, "n0" <> type] / CoreSMTResourceDivisor[archProperties, algProperties]; (* Each SMT thread has only fraction of the units *)

(* Create constraints on functional units (only scalar for int / fp, all ops (including vector) for the rest *)
LPMemFU[ipc_, archProperties_, algProperties_] := ConstraintFUAll[ipc, "mem", archProperties, algProperties]
LPIntFU[ipc_, archProperties_, algProperties_] := ConstraintFUScalar[ipc, "int", archProperties, algProperties]
LPFPFU[ipc_, archProperties_, algProperties_] := ConstraintFUScalar[ipc, "fp", archProperties, algProperties]
LPControlFU[ipc_, archProperties_, algProperties_] := ConstraintFUAll[ipc, "control", archProperties, algProperties]

(* Constraint on vector FUs*)
LPVectorFU[ipc_, archProperties_, algProperties_] :=
BlockCheck[{n0vectorFU, n0vectorbits, FeInt, FeFP, maxbits},
	n0vectorFU = GetArchKeyValueCheck[archProperties, "n0vectorFU"];
	n0vectorbits = GetArchKeyValueCheck[archProperties, "n0vectorbits"];
	(* only int and fp can go to the vector units *)
	FeInt = GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0int"], VectorLength->"Vector"] + GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0addr"], VectorLength->"Vector"] + GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0vector"], VectorLength->"Accumulate"];
	FeFP = GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0fp"], VectorLength->"Vector"];
	maxbits = GetMaxVectorSizeBits[algProperties];
		
	(* If the n0vectorFU == 0, we just return ipc < a-very-large-number as the constraint *)
	If[n0vectorbits == 0,
		If[maxbits > 0, (* profile contains vector instructions, but no vector units configured *)
			Message[LPVectorFU::novec, maxbits]
		];
		Return[ipc <= 9999]
	];
	
	If[maxbits > n0vectorbits, (* vector width mismatch, show warning *)
		Message[LPVectorFU::mismatch, maxbits, n0vectorbits]
	];
	
	Return[ipc * (FeInt + FeFP) <= n0vectorFU];
];
LPVectorFU::mismatch = "Vector size mismatch: application contains vectors of size `1` bits while architecture supports vectors of size `2`. Missing GetAlgorithmVectorProperties[...]?" 
LPVectorFU::novec = "Vector size mismatch: application contains vectors of size `1`  while architecture has no vector units. Missing GetAlgorithmVectorProperties[...]?"

(* Core to L1 cache bandwidth constraint *)
LPBandwidthCoreToL1[ipc_, archProperties_, algProperties_] :=
BlockCheck[{Fe, width, maxBW, f0},
	Fe = FiMem[MemoryL1Cache[], archProperties, algProperties];
	width = 4; (*32-bit for now*) (*GetArchKeyValueCheck[archProperties, "L1dagran"];*) (* TODO: get right number from app. char *)
	maxBW = GetArchKeyValueCheck[archProperties, "B0Dmo"];
	f0 = GetArchKeyValueCheck[archProperties, "f0"];
	
	(* Bandwidth is in B/s. f0 [c/s] * ipc [i/c] * Fe [1/i] * width [B] *)
	Return[ipc * f0 * Fe * width <= maxBW];
];

(* L2 to L3 cache bandwidth constraint *)
LPBandwidthL2ToL3[ipc_, archProperties_, algProperties_] := 
BlockCheck[{Fe, width, maxBW, f0},
	Fe = FiMem[MemoryL3Cache[], archProperties, algProperties];
	width = GetArchKeyValueCheck[archProperties, "L3dagran"];
	maxBW = GetArchKeyValueCheck[archProperties, "B1Dmo"];
	f0 = GetArchKeyValueCheck[archProperties, "f0"];
	
	(* Bandwidth is in B/s. f0 [c/s] * ipc [i/c] * Fe [1/i] * width [B] *)
	Return[ipc * f0 * Fe * width <= maxBW];
];

(* L3 to DRAM bandwidth constraint *)
LPBandwidthL3ToDRAM[ipc_, archProperties_, algProperties_] :=
BlockCheck[{Fe, width, maxBW, f0},
	Fe = FiMem[MemoryDRAM[], archProperties, algProperties];
	width = GetArchKeyValueCheck[archProperties, "L3dagran"];
	maxBW = GetArchKeyValueCheck[archProperties, "B2Dmo"];
	f0 = GetArchKeyValueCheck[archProperties, "f0"];
	
	(* Bandwidth is in B/s. f0 [c/s] * ipc [i/c] * Fe [1/i] * width [B] *)
	Return[ipc * f0 * Fe * width <= maxBW (*/ GetArchKeyValueCheck[archProperties,"n1"]*)]; 
];

(* Solve for IPC in step 1 of the model (see ICCD 2015 paper by Jongerius et al.) *)
SolveIPC[archProperties_, algProperties_] :=
Block[{ipc, ipcsolved},
	ipcsolved = NMaximize[	{ipc, (* max IPC *)
							(* constraints *)
							LPIssue[ipc, archProperties, algProperties],
							LPMemMSHR[ipc, archProperties, algProperties],
							LPILP[ipc, archProperties, algProperties],
							LPILPType[ipc, "int", archProperties, algProperties],
							LPILPType[ipc, "mem", archProperties, algProperties],
							LPILPType[ipc, "control", archProperties, algProperties],
							LPILPType[ipc, "fp", archProperties, algProperties],
							LPMemFU[ipc, archProperties, algProperties],
							LPIntFU[ipc, archProperties, algProperties],
							LPFPFU[ipc, archProperties, algProperties],
							LPControlFU[ipc, archProperties, algProperties],
							LPVectorFU[ipc, archProperties, algProperties]
							}, ipc (* Variable to maximize *)
	];
	Return[ipcsolved[[1]]];
];

(* Solve for IPC in step 3 of the model (see ICCD 2015 paper by Jongerius et al.) *)
SolveIPCLevel2Constraints[maxipc_, archProperties_, algProperties_] :=
Block[{ipc, ipcsolved},
	ipcsolved = NMaximize[	{ipc, (* max IPC *)
							(* Level 1 constraints *)
							ipc <= maxipc,
							(* Level 2 constraints *)
							LPDispatch[ipc, archProperties, algProperties],
							LPBandwidthCoreToL1[ipc, archProperties, algProperties],
							LPBandwidthL2ToL3[ipc, archProperties, algProperties],
							LPBandwidthL3ToDRAM[ipc, archProperties, algProperties]
							}, ipc (* Variable to maximize *)
	];
	Return[ipcsolved[[1]]];
];

(* Determine the memory stall penalty: the time that issuing stops until that it starts again. Account for the cycles during a stall for which still work can be performed *)
MemoryStallPenalty[cacheLevel_, cyclesUntilStall_, ipc_, archProperties_, algProperties_] :=
Block[{F, P, Fevents, n0ROB},
	F = FiMemHit[cacheLevel, archProperties, algProperties];
	P = CacheLiLatency[cacheLevel, archProperties, algProperties];
	n0ROB = GetArchKeyValueCheck[archProperties, "n0ROB"];
	
	(* Fraction of overlapping events *)
	Fevents = F / Max[1, Min[F * ipc * P, F * n0ROB]];

	Return[
		If[P <= cyclesUntilStall,
			0, (* No Stall *)
			Fevents * (P - cyclesUntilStall)
		]
	];
];

(* Determine the average stall penalty of instructions in the IQ for inorder architectures *)
AverageStallPenalty[archProperties_, algProperties_, ilp_, cyclesUntilIQStall_] :=
Block[{FeL1, PeL1, FeL2, PeL2, FeL3, PeL3, FeDRAM, PeDRAM, FeIntMul, PeIntMul, FeIntDiv, PeIntDiv, FeIntOther, PeIntOther, FeFPMul, PeFPMul, FeFPDiv, PeFPDiv, FeFPOther, PeFPOther,
	PeList, StallProbability, AverageStallCycles, EventProbabilityPerILPInstructions, StallProbabilityOfEvent, penaltyPerILPinstructions},
	FeL1 = FiMemHit[MemoryL1Cache[], archProperties, algProperties];
	PeL1 = CacheLiLatency[MemoryL1Cache[], archProperties, algProperties];
	FeL2 = FiMemHit[MemoryL2Cache[], archProperties, algProperties];
	PeL2 = CacheLiLatency[MemoryL2Cache[], archProperties, algProperties];
	FeL3 = FiMemHit[MemoryL3Cache[], archProperties, algProperties];
	PeL3 = CacheLiLatency[MemoryL3Cache[], archProperties, algProperties];
	FeDRAM = FiMemHit[MemoryDRAM[], archProperties, algProperties];
	PeDRAM = CacheLiLatency[MemoryDRAM[], archProperties, algProperties];
	FeIntMul = GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0intmul"]];
	PeIntMul = GetArchKeyValueCheck[archProperties, "T0intmul"];
	FeIntDiv = GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0intdiv"]];
	PeIntDiv = GetArchKeyValueCheck[archProperties, "T0intdiv"];
	FeIntOther = (GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0addr"]] + GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0int"]]) - (FeIntMul + FeIntDiv);
	PeIntOther = GetArchKeyValueCheck[archProperties, "T0intop"];
	FeFPMul = GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0fpmul"]];
	PeFPMul = GetArchKeyValueCheck[archProperties, "T0fpmul"];
	FeFPDiv = GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0fpdiv"]];
	PeFPDiv = GetArchKeyValueCheck[archProperties, "T0fpdiv"];
	FeFPOther = GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0fp"]] - (FeFPMul + FeFPDiv);
	PeFPOther = GetArchKeyValueCheck[archProperties, "T0fpop"];
	
	(* We need to order the events in stall length *)
	PeList = {{PeL1, FeL1}, {PeL2, FeL2}, {PeL3, FeL3}, {PeDRAM, FeDRAM},
		{PeIntMul, FeIntMul}, {PeIntDiv, FeIntDiv}, {PeIntOther, FeIntOther},
		{PeFPMul, FeFPMul}, {PeFPDiv, FeFPDiv}, {PeFPOther, FeFPOther}};
	PeList = Sort[PeList, #1[[1]] > #2[[1]] &];

	(* We consider at most ILP instructions and need to calculate how long this stalls. *)
	(* Loop through all events to calculate the average penalty length longer than the cyclesUntilIQStall *)
	StallProbability = 0.;
	AverageStallCycles = 0.;
	Do[
		If[event[[1]] <= cyclesUntilIQStall, Break[]]; (* If the event penalty is shorter or equal to the cycles until IQ stall, this and subsequent events will not cause a stall *)
		EventProbabilityPerILPInstructions = Min[1, event[[2]] * ilp]; (* The probability that a certain event happens in ILP instructions *)
		StallProbabilityOfEvent = (1 - StallProbability) * EventProbabilityPerILPInstructions; (* The probability that this unique event happens and wasn't hidden by longer stalls *)
		StallProbability += StallProbabilityOfEvent; (* Cumulative total stall probability *) 
		AverageStallCycles += StallProbabilityOfEvent * (event[[1]] - cyclesUntilIQStall); (* Cumulative stall cycles, corrected for the cycles until stall, based on the stall probability for this unique event *)
		If[EventProbabilityPerILPInstructions == 1, Break[]]; (* If we have an event probability of one, all subsequent events are hidden *)
		,
	{event, PeList}]; 
	
	penaltyPerILPinstructions = AverageStallCycles;
	
	Return[penaltyPerILPinstructions / ilp]; 
];

(* Calcualte the IQ and ROB stall penalties for inorder and out-of-order architectures (see ICCD 2015 paper by Jongerius et al. for out-of-order model *)
CalculateIQROBStallCycles[ipc_, archProperties_, algProperties_] :=
BlockCheck[{inorder, FeL3, PeL3, FeDRAM, PeDRAM, n0IQ, n0ROB, ilp,
	cyclesUntilROBStall, cyclesUntilIQStall, instructionsUntilIQFill, drainedIQInstructions, parallelEvents, cpi, IQstallCycles,
	penaltyROB, penaltyIQ},
	
	CheckILPIQSize[archProperties, algProperties];
	
	inorder = GetArchKeyValueCheck[archProperties, "inorder"];
	
	(* IN ORDER ARCHITECTURES *)
	If[inorder,
		(* We have no ROB stalls, only IQ stalls *)
		(* We stall as soon as there is no ILP available anymore to issue *)
		ilp = GetAlgKeyValueCheck[algProperties, "ILP0"]; (* The OoO ILP is used for both inorder and OoO architectures *)
		cyclesUntilIQStall = ilp / ipc; (* always >= 1 *)
		penaltyIQ = AverageStallPenalty[archProperties, algProperties, ilp, cyclesUntilIQStall];
		penaltyROB = 0; (* no ROB, no stalls *)
		Return[{penaltyIQ, penaltyROB}]
	]; 
	
	(* OUT OF ORDER ARCHITECTURES *)
	(* We have two types of stalls: ROB stalls and IQ stalls *)
	(* The ROB stalls if a long-latency instruction blocks the ROB and it fills completely *)
	(* The IQ stalls if long-latency instructions let the IQ fill with dependent instructions *)
	
	FeL3 = FiMemHit[MemoryL3Cache[], archProperties, algProperties];
	PeL3 = CacheLiLatency[MemoryL3Cache[], archProperties, algProperties];
	FeDRAM = FiMemHit[MemoryDRAM[], archProperties, algProperties];
	PeDRAM = CacheLiLatency[MemoryDRAM[], archProperties, algProperties];
	n0IQ = GetArchKeyValueCheck[archProperties, "n0IQ"] / CoreSMTResourceDivisor[archProperties, algProperties]; (* We assign part of the IQ to each thread in the core, this could be architectural *)
	n0ROB = GetArchKeyValueCheck[archProperties, "n0ROB"] / CoreSMTResourceDivisor[archProperties, algProperties]; (* The effective ROB size is smaller as the actual IPC grows with the thread count *) (* TODO: validate assumption. If the ROB blocks for one thread, it blocks all threads *)
	ilp = GetAlgKeyValueCheck[algProperties, "ILP0"];
	
	(* First, we determine which stall occurs first *)
	(* Cycles until ROB stall depends on the size of the ROB and the IPC *)
	cyclesUntilROBStall = n0ROB / ipc;
	
	(* TEMP CALCULTE WITH NEW METHOD *)
	cyclesUntilROBStall=(n0ROB - (0.5 * n0ROB * Max[1, Min[FeDRAM * ipc * PeDRAM, FeDRAM * n0ROB]])/ilp)/ipc;
	
	(* Determining the IQ stall is more difficult, as this depends on the frequency and penalty of events as well *)
	(* We first calculate the effective number of instructions which need to be issued before the IQ fills with blocked instructions *)
	(* For now, we only consider L3 and DRAM events, as these are long enough to cause such as a stall *)
	
	(* Calculate the number of par events for increasing combinations of penalties *)
	parallelEvents = {};
	AppendTo[parallelEvents, ipc * (FeL3 * PeL3 + FeDRAM * PeDRAM)];
	AppendTo[parallelEvents, ipc * (FeDRAM * PeDRAM)];
	
	instructionsUntilIQFill = If[ # > 0, ilp * n0IQ / (#), Infinity] & /@ parallelEvents;
	
	(* In case of frequeny events which stall the IQ, we need to correct for the instructions which remain in the IQ and cause the IQ to fill faster *)
	drainedIQInstructions = {};
	AppendTo[drainedIQInstructions, If[FeL3 (*+ FeDRAM*) > 0, 1 / (FeL3 (*+ FeDRAM*)), Infinity]];
	AppendTo[drainedIQInstructions, If[FeDRAM > 0, 1 / (FeDRAM), Infinity]];
	
	cyclesUntilIQStall = Table[Min[instructionsUntilIQFill[[i]], drainedIQInstructions[[i]]] / ipc, {i, Length[instructionsUntilIQFill]}];
	
	(* Select the first one for which the cycles until stalls is larger than it's smallest penalty. Otherwise, the smallest penalty has no influence on IQ stalls and we need to eveluate the next *)
	cyclesUntilIQStall = Which[ cyclesUntilIQStall[[1]] <= PeL3, (*CellPrint["L3"];*) cyclesUntilIQStall[[1]],
								cyclesUntilIQStall[[2]] <= PeDRAM, (*CellPrint["DRAM"];*) cyclesUntilIQStall[[2]],
								True, (*CellPrint["None"];*) Infinity (* It never happens *)
	];
	
	If[cyclesUntilROBStall <= cyclesUntilIQStall,
		 (* Only ROB stalls *)
		penaltyIQ = 0;
		penaltyROB = MemoryStallPenalty[MemoryL3Cache[], cyclesUntilROBStall, ipc, archProperties, algProperties] + MemoryStallPenalty[MemoryDRAM[], cyclesUntilROBStall, ipc, archProperties, algProperties],
		(* Only IQ stalls *)
		penaltyIQ = MemoryStallPenalty[MemoryL3Cache[], cyclesUntilIQStall, ipc, archProperties, algProperties];
		cpi = 1 / ipc + penaltyIQ;
		cyclesUntilROBStall = n0ROB * cpi;
		cyclesUntilROBStall=(n0ROB - (0.5 * n0ROB * Max[1, Min[FeDRAM * ipc * PeDRAM, FeDRAM * n0ROB]])/ilp)*cpi;
		penaltyROB = MemoryStallPenalty[MemoryDRAM[], cyclesUntilROBStall, ipc, archProperties, algProperties]
	];

	Return[{penaltyIQ, penaltyROB}];
];

(* Overal function to calculate achieved CPI by applying the four modeling steps. Returns a list for some backwards compatibility. *)
d0Optimize[archProperties_, algProperties_] :=
BlockCheck[{ipc, cpi, d0IQROB},
	
	(* Solve the IPC model *)
	ipc = SolveIPC[archProperties, algProperties];
	
	(* Check if IPC is zero, otherwise we have a problem *)
	If[ipc == 0, Message[d0Optimize::ipczero]];
	(*ipc = SolveIPCLevel2Constraints[ipc, archProperties, algProperties];*)
	
	(* Retrieve IQ and ROB stalls *)
	d0IQROB = CalculateIQROBStallCycles[ipc, archProperties, algProperties];
	
	(* Solve for the higher level constraints. e.g. bandwidths and dispatch width (which should be applied after IQ/ROB) *)
	ipc = SolveIPCLevel2Constraints[1/(1/ipc + Total[d0IQROB](* + d0branch[archProperties, algProperties, ipc]*)), archProperties, algProperties];
	
	Return[{1/ipc, d0branch[archProperties, algProperties, ipc]}];
];

d0Optimize::ipczero = "Warning: core IPC is 0"

TotalIQROBBranchStallCycles[ipc_, archProperties_, algProperties_] := Total[CalculateIQROBStallCycles[ipc, archProperties, algProperties]] + d0branch[archProperties, algProperties, ipc];

(* Return miss latencies on given cache level in core clock cycles *)
CacheLiLatency[cacheLevel_, archProperties_, algProperties_] :=
Block[{},
  Return[Which[
  	cacheLevel == MemoryL1Cache[], GetArchKeyValueCheck[archProperties, "T0L1latency"],
  	cacheLevel == MemoryL2Cache[], GetArchKeyValueCheck[archProperties, "T0L2latency"],
  	cacheLevel == MemoryL3Cache[], GetArchKeyValueCheck[archProperties, "T0L3latency"],
  	cacheLevel == MemoryDRAM[], GetArchKeyValueCheck[archProperties, "T0DRAMlatency"]
  ]];
];

(* Calculate branch mis penalty *)
d0branch[archProperties_, algProperties_, ipc_] := 
  BlockCheck[{n0front, n0frontpipe, n0ROB, F0control, divisor, n0IQ},
   n0front = GetArchKeyValueCheck[archProperties, "n0front"];
   n0frontpipe = GetArchKeyValueCheck[archProperties, "n0frontpipe"];
   n0ROB = GetArchKeyValueCheck[archProperties, "n0ROB"];
   F0control = GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0control"]];
   n0IQ = GetArchKeyValueCheck[archProperties, "n0IQ"] / CoreSMTResourceDivisor[archProperties, algProperties];

   (* Returned value is a latency in _cycles_ (not seconds) *)
   
   (* branch penalty goes down with SMT: not all instructions in the pipe are ours *)
   divisor = CoreSMTResourceDivisor[archProperties, algProperties];
   
   (* TODO: We only account for the static front-end pipeline refil here. Eyerman, 2006, states that the branch resolution time (which can be approximated as the window drain time) is actually the biggest contributor to the penalty. See his work for models (probably requires more analysis from the application characterization *)
 
   	Return[
   		F0control * ((n0frontpipe/divisor) + n0IQ/ipc) * (*0.02*)FMispredicted[archProperties, algProperties](* + WindowDrainTime[archProperties, algProperties] - WindowDrainTime[archProperties, algProperties] *) (* Total penalty is front-end refil + window drain time, but for window drain time there are useful instructions being completed *)
   		(*F0control * (n0frontpipe/divisor) * FMispredicted[archProperties, algProperties]*)
	];
];

(* Calculate the fraction of memory accesses generated to each level of the cache.
   Note that this is not the same level index as in the hierarchical model
   L1 cache: cacheLevel = 1 (hierarchical level 0) (MemoryL1Cache[])
   L2 cache: cacheLevel = 2 (hierarchical level 0) (MemoryL2Cache[])
   L3 cache: cacheLevel = 3 (hierarchical level 1) (MemoryL3Cache[])
   DRAM: "cache"Level = 4 (hierarchical level 2) (MemoryDRAM[])
*)
(* TODO: Returns VMix *)
FiMem[cacheLevel_, archProperties_, algProperties_] :=
FiMem[cacheLevel, archProperties, algProperties] = (* Caching for speedup *)
BlockCheck[{F0mem},
	F0mem = GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0mem"]];
	
	Which[
		cacheLevel == MemoryL1Cache[], F0mem,
		cacheLevel > MemoryL1Cache[] && cacheLevel <= MemoryDRAM[], FiMem[cacheLevel-1, archProperties, algProperties] * CacheMissrate[cacheLevel-1, archProperties, algProperties],
		True, 0
	]	
];

(* Return the number of cache levels *)
GetNumberCacheLevels[archProperties_] :=	Boole[GetArchKeyValueCheck[archProperties, "M0L1"] > 0] +
											Boole[GetArchKeyValueCheck[archProperties, "M0L2"] > 0] +
											Boole[GetArchKeyValueCheck[archProperties, "M1"] > 0];

(* Return True or False if a given cache level exists *)
ExistsCacheLevel[cacheLevel_, archProperties_] :=	(cacheLevel == MemoryL1Cache[] && GetArchKeyValueCheck[archProperties, "M0L1"] =!= 0) ||
													(cacheLevel == MemoryL2Cache[] && GetArchKeyValueCheck[archProperties, "M0L2"] =!= 0) ||
													(cacheLevel == MemoryL3Cache[] && GetArchKeyValueCheck[archProperties, "M1"] =!= 0);

(* Get the percentage of access that hit a certain cache level *)
CacheHitrate[cacheLevel_, archProperties_, algProperties_] :=
BlockCheck[{D0dreuse, D0dreuse64, D0dreuse128, hitrateL1, hitrateL2, hitrateL3, relHitrateL1, relHitrateL2, relHitrateL3,
	sizeBytesL1, sizeBytesL2, sizeBytesL3, lineBytesL1, lineBytesL2, lineBytesL3, effectiveSizeL1, effectiveSizeL2, effectiveSizeL3},
	D0dreuse64 = GetAlgKeyValueCheck[algProperties, "D0dreuse"];
	D0dreuse128 = GetAlgKeyValueCheck[algProperties, "D0dreuse128"];
	
	If[D0dreuse64 == GetAlgorithmDefaultValue["D0dreuse"] && D0dreuse128 == GetAlgorithmDefaultValue["D0dreuse128"],
		Message[CacheAnalysis::novalidcdf];
		Return[0.];
	];
	
	(* Note that we always need to calculate the hitrate of each level of the cache, as the required hitrates are relative to each other *)
	
	(* We calculate the absolute hitrate for each level of the cache using linear interpolation. *)
	(* The CDFs are per cache line, so we need to calcualte the cache size in lines first *)
	(* Get the cache line size in bytes. Note that we assume L1 and L2 to be exclusive, meaning that their sizes add up. L3 is inclusive of L1 and L2 *)
	sizeBytesL1 = GetArchKeyValueCheck[archProperties, "M0L1"];
	sizeBytesL2 = GetArchKeyValueCheck[archProperties, "M0L1"] + GetArchKeyValueCheck[archProperties, "M0L2"];
	sizeBytesL3 = GetArchKeyValueCheck[archProperties, "M1"];
	(* Get the line size of the cache we are interested in *)
	lineBytesL1 = GetArchKeyValueCheck[archProperties, "L1dagran"];
	lineBytesL2 = GetArchKeyValueCheck[archProperties, "L2dagran"];
	lineBytesL3 = GetArchKeyValueCheck[archProperties, "L3dagran"];
	(* calculate the effective size *)
	effectiveSizeL1 = If[ExistsCacheLevel[MemoryL1Cache[], archProperties], Floor[sizeBytesL1 / lineBytesL1], 0];
	effectiveSizeL2 = If[ExistsCacheLevel[MemoryL2Cache[], archProperties], Floor[sizeBytesL2 / lineBytesL2], 0];
	effectiveSizeL3 = If[ExistsCacheLevel[MemoryL3Cache[], archProperties], Floor[sizeBytesL3 / lineBytesL3], 0];

	(* Retrieve the correct CDF, for all lower sizes smaller than 96 we select 64, for all larger we select 128 *)
	(* One of the two has to be a correct CDF, otherwise the earlier check should have failed already *)
	selectCDF[size_] := Which[size == 64,
						If[D0dreuse64 != GetAlgorithmDefaultValue["D0dreuse"],
							D0dreuse64,
							Message[CacheAnalysis::usedothercdf, 64, 128];
							D0dreuse128 (* Return 128 if 64 is not calid *)
						],
						size == 128,
						If[D0dreuse128 != GetAlgorithmDefaultValue["D0dreuse128"],
							D0dreuse128,
							Message[CacheAnalysis::usedothercdf, 128, 64];
							D0dreuse64 (* Return 128 if 64 is not calid *)
						],
						size <= 96,
						If[D0dreuse64 != GetAlgorithmDefaultValue["D0dreuse"],
							Message[CacheAnalysis::usedothercdf, size, 64];
							D0dreuse64,
							Message[CacheAnalysis::usedothercdf, size, 128];
							D0dreuse128 (* Return 128 if 64 is not calid *)
						],
						size > 96,
						If[D0dreuse128 != GetAlgorithmDefaultValue["D0dreuse128"],
							Message[CacheAnalysis::usedothercdf, size, 128];
							D0dreuse128,
							Message[CacheAnalysis::usedothercdf, size, 64];
							D0dreuse64 (* Return 128 if 64 is not calid *)
						]
	];

	D0dreuse = selectCDF[lineBytesL1];
	(* Now, we can calculate the hitrates of each level. *)
	hitrateL1 = If[ExistsCacheLevel[MemoryL1Cache[], archProperties],
		 			LinearInterpolation[D0dreuse, 1, effectiveSizeL1],
		 			0];
	(* L1/2 are exclusive *)
	D0dreuse = selectCDF[lineBytesL2];
	hitrateL2 = If[ExistsCacheLevel[MemoryL2Cache[], archProperties],
					LinearInterpolation[D0dreuse, 1, effectiveSizeL2],
		 			0];
	(* L3 is inclusive *)
	D0dreuse = selectCDF[lineBytesL3];
	hitrateL3 = If[ExistsCacheLevel[MemoryL3Cache[], archProperties],
					LinearInterpolation[D0dreuse, 1, effectiveSizeL3],
		 			0];
		 			
	(* Calculate the relative levels *)
	relHitrateL1 = hitrateL1;
	relHitrateL2 = If[ExistsCacheLevel[MemoryL2Cache[], archProperties], (hitrateL2 - hitrateL1) / (1 - hitrateL1), 0];
	relHitrateL3 = If[ExistsCacheLevel[MemoryL3Cache[], archProperties], 
						If[ExistsCacheLevel[MemoryL2Cache[], archProperties], 
							(hitrateL3 - hitrateL2) / (1 - hitrateL2),
							(hitrateL3 - hitrateL1) / (1 - hitrateL1)],
					0];
	Return[Switch[cacheLevel,
		MemoryL1Cache[], relHitrateL1,
		MemoryL2Cache[], relHitrateL2,
		MemoryL3Cache[], relHitrateL3,
		_, 0
	]];
];
CacheAnalysis::novalidcdf = "No valid data reuse CDF available for application"
CacheAnalysis::usedothercdf = "No valid data reuse CDF available for cache line size `1`B, used CDF for cache line size `2`B instead"

(* Get the percentage of accesses that mis a certain cache level *)
CacheMissrate[cacheLevel_, archProperties_, algProperties_] := 1 - CacheHitrate[cacheLevel, archProperties, algProperties];

(* Calculate the data movement bandwidth induced by the application for each element on each layer, in bytes/second *)
(* Base indicates whether to calculate base data motion (unrestricted) or actual data motion (BW restricted) *)
BappiDmo[i_, archProperties_, algProperties_] :=
BappiDmo[i, archProperties, algProperties] = (* Caching for speedup *)
BlockCheck[{n0Bits, f0},
	(* On the lower levels:
	   Level 0: L1 bandwidth (core layer)
	   Level 1: L3 bandwidth (die layer)
	   Level 2: L3 to DRAM bandwidth (socket layer) *)
	n0Bits = GetArchKeyValueCheck[archProperties, "n0Bits"];
	f0 = GetArchKeyValueCheck[archProperties, "f0"];
	
	(* Bandwidths are based on loading cache lines *)
	Return[Which[	i==CoreLayer[], (f0 / d0[archProperties, algProperties]) * FiMem[MemoryL1Cache[], archProperties, algProperties] * GetArchKeyValueCheck[archProperties, "L1dagran"],
					i==DieLayer[], (f0 / d0[archProperties, algProperties]) * FiMem[MemoryL3Cache[], archProperties, algProperties] * GetArchKeyValueCheck[archProperties, "L3dagran"],
					i==SocketLayer[], (f0 / d0[archProperties, algProperties]) * FiMem[MemoryDRAM[], archProperties, algProperties] * GetArchKeyValueCheck[archProperties, "L3dagran"],
					True, 0]];
];

(* Performance in CPI *)
d0[archProperties_, algProperties_] :=
d0[archProperties, algProperties] = (* Caching for speedup *) Total[d0Optimize[archProperties, algProperties]];

(* Calculate cycles *)
ExecutionCycles[archProperties_, algProperties_] := d0[archProperties, algProperties] * GetAlgKeyValueCheck[algProperties, "LSys"];

(* Calculate seconds *)
ExecutionSeconds[archProperties_, algProperties_] := d0[archProperties, algProperties] * GetAlgKeyValueCheck[algProperties, "LSys"] / GetArchKeyValueCheck[archProperties, "f0"];

(* Returns the fraction of branches that are mispredicted. *)
FMispredicted[ machineProperties_, algorithmProperties_ ] := 
BlockCheck[{F0bestMispredict, branchModelFactor},
	F0bestMispredict = GetAlgKeyValueCheck[algorithmProperties, "F0bestMispredict"];
	branchModelFactor = 1.71;(* Haswell model fit *)
	
	Return[F0bestMispredict * branchModelFactor];
];

End[] (* End Private Context *)

EndPackage[]