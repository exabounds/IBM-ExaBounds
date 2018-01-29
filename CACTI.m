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

(* NOTE: Contains functionality to integrate CACTI 5.3 and CACTI 6. CACTI 5.3 is used by the model and is an up-to-date implementation. CACTI 6 code is still there, but not maintained. CACTI 5.3 gives more accurate results for DRAM in our opinion *)

BeginPackage["CACTI`"]
(* Exported symbols added here with SymbolName::usage *)

Needs["Support`"]
Needs["ExaBoundsGeneric`"]
Needs["ModelWarnings`"]
Needs["CPUPerformanceModels`"]
Needs["CPUMultithreadedModels`"]
Needs["VectorSizeConvert`"]
Needs["PreDefinedMemorySpec`"]

ClearCACTICache::usage =
  "ClearCACTICache[]"
  
GetCACTIMemoryParameters::usage =
  "GetCACTIMemoryParameters[memory_, archProperties_, algProperties_, homogeneousThreads : 1]"
  
Begin["`Private`"] (* Begin Private Context *)

(* Clear caches of all cached functions *)
(* KEEP UP TO DATE! *)
ClearCACTICache[] :=
Block[{},
	ClearCache[CachedCACTI5];
]; 

(* Some private variables used to build the CACTI configs and which are needed in other functions to calculate final power *)
memoryBanks = 8;
memoryOutputBits = 8;
CACTI5outFile = "out.csv";

CachedCACTI5[cfgString_] :=
CachedCACTI5[cfgString] =
Module[{},
	If[ExecuteCACTI5[cfgString] != 0, (* CACTI didn't process properly *) Return[{}]];
	(* Parse output (in out.csv) *)
	Return[ParseCACTI5Output[]];
];

(* Generate config file for CACTI with file name fileName *)
(* Memory can be:
   M2: DRAM per socket *)
GetCACTIMemoryParameters[memory_, archProperties_, algProperties_, homogeneousThreads_Integer : 1] :=
BlockCheck[{cfgString, CACTIData, totalInstructions, loadInstructions, storeInstructions, numberOfDies, readsPerSecond, writesPerSecond, staticPower, dynamicPower, totalArea, homogeneousMultiplier},
	If[memory !=  "M2", Return[{}]];
	
	(* CACTI 5 *)
	(* Generate parameter string *)
	cfgString = GenerateCACTI5CfgString[memory, archProperties];
	If[cfgString == "", (* Empty string returend *) Return[{}]];
	(*
	(* Execute CACTI *)
	If[ExecuteCACTI5[cfgString] != 0, (* CACTI didn't process properly *) Return[{}]];
	(* Parse output (in out.csv) *)
	CACTIData = ParseCACTI5Output[];
	*)
	
	(* We need to cache the cfgString to make sure to cache the cfgstring such that we do not execute CACTI if the string didn't change *)
	CACTIData = CachedCACTI5[cfgString];
	
	(* Calculate actual power consumption of requested memory *)
	totalInstructions = MCGetAlgKeyValueCheck[algProperties, "LSys"]; (* per thread *)
	If[AllTrue[algProperties, Head[#] === List &],
		loadInstructions = {IntegerPart[GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0load"]] * totalInstructions]};
 		storeInstructions = {IntegerPart[GetTotalVMixFraction[GetAlgKeyValueCheck[algProperties, "F0store"]] * totalInstructions]};
    	, (* Replacement list *)
    	loadInstructions = GetTotalVMixFraction[#] & /@ MCGetAlgKeyValueCheck[algProperties, "F0load"];
    	loadInstructions = IntegerPart[loadInstructions * totalInstructions];
 		storeInstructions = GetTotalVMixFraction[#] & /@ MCGetAlgKeyValueCheck[algProperties, "F0store"];
 		storeInstructions = IntegerPart[storeInstructions * totalInstructions];
    ];	
  	
	readsPerSecond = (loadInstructions / (loadInstructions + storeInstructions)) * MCBappiDmo[SocketLayer[], archProperties, algProperties, homogeneousThreads]; (* per thread *)
	writesPerSecond = (storeInstructions / (loadInstructions + storeInstructions)) * MCBappiDmo[SocketLayer[], archProperties, algProperties, homogeneousThreads];
	
    (* Accumulate over threads *)
    readsPerSecond = Total[readsPerSecond];
    writesPerSecond = Total[writesPerSecond];
    
    If[AllTrue[algProperties, Head[#] === List &],
    	(* Homogeneous workload multiplier *)
    	homogeneousMultiplier = Min[homogeneousThreads, GetArchKeyValueCheck[archProperties, "n1"]];
 		readsPerSecond =  readsPerSecond * homogeneousMultiplier;
 		writesPerSecond =  writesPerSecond * homogeneousMultiplier;
  	];
  	
  	(* HW property : *)
  	numberOfDies = Ceiling[GetArchKeyValueCheck[archProperties, memory] / GetKeyValue[CACTIData, "capacity"]];	
	
	staticPower = numberOfDies * memoryBanks * GetKeyValue[CACTIData, "leakageperbank"];
	dynamicPower = readsPerSecond * GetKeyValue[CACTIData, "readenergy"] + writesPerSecond * GetKeyValue[CACTIData, "writeenergy"];
	totalArea = GetKeyValue[CACTIData, "area"] * numberOfDies;
	
	Return[{
		{"power", staticPower + dynamicPower},
		{"staticpower", staticPower},
		{"dynamicpower", dynamicPower},
		{"area", totalArea}
	}]; 
];

(* Generate config file for CACTI with file name fileName *)
(* Memory can be:
   M2: DRAM per socket *)
GenerateCACTI6Cfg[memory_, archProperties_, algProperties_, fileName_] :=
Module[{storeFile, memoryType, memorySize, techNode, memoryBytesPerDie, memoryTemp, nCores, cacheLevel, memoryECC, memorySpec},
	If[memory !=  "M2", Return[""]];
	
	(* Retrieve the memory spec (as introduced by MeSAP) *)
	memorySpec = predefinedconfigMem[GetArchKeyValueCheck[archProperties, "DRAMType"]];
	
	memoryType = "main memory";
	memorySize = GetArchKeyValueCheck[archProperties, memory];
	memoryBytesPerDie = GetArchKeyValueCheck[memorySpec, "DeviceChipSize"] * 1024^2; (*536870912;*) (*~4Gb 4*10^9; *)
	techNode = NumberForm[N[GetArchKeyValueCheck[archProperties, "Lnode"]/1000],3]; (* TODO: Now sames as core tech node? Correct? *)
	memoryTemp = GetArchKeyValueCheck[archProperties, "T"];
	memoryECC = False; (* TODO: archProperty? from ITRS? *)
	
	nCores = GetArchKeyValueCheck[archProperties, "n1"];
	cacheLevel = "L3"; (* TODO: finish cache CFG if needed *)
	
	storeFile = OpenWrite[fileName];
	
	WriteString[storeFile, "-cache type \"" <> memoryType <> "\"\n"];
	WriteString[storeFile, "-size (bytes) " <> ToString[IntegerPart[memoryBytesPerDie]] <> "\n"];
	WriteString[storeFile, "-read-write port 1\n"];
	WriteString[storeFile, "-exclusive read port 0\n"];
	WriteString[storeFile, "-exclusive write port 0\n"];
	WriteString[storeFile, "-single ended read ports 0\n"];
	WriteString[storeFile, "-UCA bank count " <> ToString[memoryBanks] <> "\n"];
	WriteString[storeFile, "-NUCA bank count 0\n"];
	WriteString[storeFile, "-technology (u) " <> ToString[techNode] <> "\n"];
	Which[
		memoryType == "main memory",
			WriteString[storeFile, "-page size (bits) 8192\n"];
			WriteString[storeFile, "-burst length 1\n"];
			WriteString[storeFile, "-internal prefetch width 1\n"];
			WriteString[storeFile, "-Data array cell type - \"comm-dram\"\n"];
			WriteString[storeFile, "-Data array peripheral type - \"itrs-lstp\"\n"];	(* low-leakage power transistors for DRAM logic according to C-CACTI paper *)
			WriteString[storeFile, "-Tag array cell type - \"comm-dram\"\n"];
			WriteString[storeFile, "-Tag array peripheral type - \"itrs-lstp\"\n"];
			WriteString[storeFile, "-output/input bus width 64\n"]; (* 8 bits out x 8 (according to online cacti FAQ) *)
			WriteString[storeFile, "-block size (bytes) 8\n"]; (* Has to be set, does it influence the results? *)
			(*WriteString[storeFile, "-tag size (b) 0\n"];*)
			WriteString[storeFile, "-associativity 1\n"],
			(*WriteString[storeFile, "-Core count 1\n"];
			WriteString[storeFile, "-Cache level (L2/L3) \"L3\"\n"],*)
		memoryType == "cache",
			WriteString[storeFile, "-Core count " <> ToString[nCores] <> "\n"];
			WriteString[storeFile, "-Cache level (L2/L3) \"" <> ToString[cacheLevel] <> "\"\n"];
			WriteString[storeFile, "-Cache model (NUCA, UCA)  - \"UCA\"\n"];
			(* TODO: CACHE INCOMPLETE *)		
	];
	WriteString[storeFile, "-operating temperature (K) " <> ToString[memoryTemp] <> "\n"];
	WriteString[storeFile, "-design objective (weight delay, dynamic power, leakage power, cycle time, area) 0:0:0:0:100\n"];
	WriteString[storeFile, "-deviate (delay, dynamic power, leakage power, cycle time, area) 60:100000:100000:100000:1000000\n"];
	WriteString[storeFile, "-NUCAdesign objective (weight delay, dynamic power, leakage power, cycle time, area) 100:100:0:0:100\n"];
	WriteString[storeFile, "-NUCAdeviate (delay, dynamic power, leakage power, cycle time, area) 10:10000:10000:10000:10000\n"];
	WriteString[storeFile, "-Optimize ED or ED^2 (ED, ED^2, NONE): \"NONE\"\n"];
	WriteString[storeFile, "-Wire signalling (fullswing, lowswing, default) - \"default\"\n"];
	WriteString[storeFile, "-Wire inside mat - \"semi-global\"\n"];
	WriteString[storeFile, "-Wire outside mat - \"semi-global\"\n"];
	WriteString[storeFile, "-Interconnect projection - \"conservative\"\n"];
	WriteString[storeFile, "-Add ECC - \"" <> If[memoryECC, "true", "false"] <> "\"\n"];
	WriteString[storeFile, "-Print level (DETAILED, CONCISE) - \"DETAILED\"\n"];
	
	Close[storeFile];
]; 

(* Generate config file for CACTI with file name fileName *)
(* Memory can be:
   M2: DRAM per socket *)
GenerateCACTI5CfgString[memory_, archProperties_] :=
Module[{cfgString, memoryType, memorySize, techNode, memoryBytesPerDie, memoryTemp, nCores, cacheLevel, memoryPageSize, memorySpec},
	If[memory !=  "M2", Return[""]];
	
	(* Retrieve the memory spec (as introduced by MeSAP) *)
	memorySpec = predefinedconfigMem[GetArchKeyValueCheck[archProperties, "DRAMType"]];
	
	memoryType = "main memory";
	memorySize = GetArchKeyValueCheck[archProperties, memory];
	If[memorySize == 0, Return[""]]; (* If the memorySize is zero, no need to compute *)
	memoryBytesPerDie = GetArchKeyValueCheck[memorySpec, "DeviceChipSize"] * 1024^2; (*536870912;*) (*~4Gb 4*10^9; *)
	techNode = Max[GetArchKeyValueCheck[archProperties, "Lnode"], 32]; (* TODO: Now sames as core tech node? Correct? *) (* Minimum value is 32 *)
	memoryTemp = Round[GetArchKeyValueCheck[archProperties, "T"], 10];
	
	nCores = GetArchKeyValueCheck[archProperties, "n1"];
	cacheLevel = "L3"; (* TODO: finish cache CFG if needed *)
	
	memoryPageSize = 1024; (* Page size set to something default, ~2014 dies are either 1024 or 2048 *)
	
	cfgString = ToString[memoryBytesPerDie] <> " " <> (* Capacity [bytes] *)
				ToString[64] <> " " <> (* Block width [bytes] *)
				ToString[1] <> " " <> (* Assocoativity, 0 = full *)
				ToString[1] <> " " <> (* Number of read/write ports *)
				ToString[0] <> " " <> (* Number of read ports *)
				ToString[0] <> " " <> (* Number of write ports *)
				ToString[0] <> " " <> (* Number of single-ended read ports *)
				ToString[memoryBanks] <> " " <> (* Number of banks *)
				ToString[techNode] <> " " <> (* Technology Node *)
				ToString[memoryOutputBits] <> " " <> (* Output width [bits] *)
				ToString[0] <> " " <> (* Custom tag width (yes=1) *)
				ToString[0] <> " " <> (* Desited tag width *)
				ToString[0] <> " " <> (* Access mode (normal=0, sequential=1, fast=2) *)
				If[memoryType == "main memory", ToString[0], ToString[1]] <> " " <> (* Is cache? (yes=1) *)
				If[memoryType == "main memory", ToString[1], ToString[0]] <> " " <> (* Is main memory? *)
				ToString[0] <> " " <> (* Optimize for dynamic energy *)
				ToString[0] <> " " <> (* Optimize for dynamic power *)
				ToString[0] <> " " <> (* Optimize for leakage power *)
				ToString[1] <> " " <> (* Optimze for random cycle time *)
				ToString[memoryTemp] <> " " <> (* Temperature *)
				If[memoryType == "main memory", ToString[4], ToString[0]] <> " " <> (* Data array cell type 0 = ITRS-HP, 1 = ITRS-LSTP, 2 = ITRS-LOP, 3 = LP-DRAM, 4 = COMM-DRAM *)
				If[memoryType == "main memory", ToString[1], ToString[0]] <> " " <> (* Data array peripheral circuit cell type *)
				If[memoryType == "main memory", ToString[4], ToString[0]] <> " " <> (* Tag array cell type *)
				If[memoryType == "main memory", ToString[1], ToString[0]] <> " " <> (* Tag array peripheral circuit cell type *)
				ToString[1] <> " " <> (* Interconnect projection type (aggresive=0, conservative=1) *)
				ToString[1] <> " " <> (* Wire type inside MAT (semi-global=1, global=2) *)
				ToString[1] <> " " <> (* Wire type outside MAT *)
				ToString[1] <> " " <> (* Repeaters in H-tree (yes=1) *)
				ToString[0] <> " " <> (* Vertical H-tree wires traverse over array (yes=1) *)
				ToString[0] <> " " <> (* Address and data broadcast over vertical H-tree to all banks (yes=1) *)
				ToString[10] <> " " <> (* Max. area constraint [%] *)
				ToString[10] <> " " <> (* Max. access time contant [%] *)
				ToString[10] <> " " <> (* Max. repeater delay constraint [%] *)
				ToString[memoryPageSize*8] <> " " <> (* Page size [kbit] *)
				ToString[8] <> " " <> (* Burst length *)
				ToString[8] <> " " <> (* Internal prefetch DDR=2, DDR2=4, DD3=8*)
				"";
				
	Return[cfgString];	
]; 

ExecuteCACTI5[cfgString_] :=
Module[{cmd, returnVal},
	(* CACTI 5 Windows: compile with -m64 -ansi using mingw-w64. -m32 -ansi has not been checked (only -m32 doesn't work) with mingw 32b *)
	cmd = "cacti53"; (* Look for cacti53 executable *)
	
	If[FileExistsQ[CACTI5outFile], DeleteFile[CACTI5outFile]]; (* Delete previous outFile if it exists *)
	(* Environment PATH variable should be configured with path to CACTI 5.x *)
	returnVal = Run[cmd <> " " <> cfgString(* <> " & pause"*)];
	
	If[returnVal != 0, CellPrint["CACTI returned an error\n"]];
	
	Return[returnVal];
];

ParseCACTI5Output[] :=
Module[{rawData, columnAreaEfficiency, columnCapacity, columnReadEnergy, columnWriteEnergy, columnStaticLeakagePerBank, columnArea},
	
	columnAreaEfficiency = 34; (* Area efficiency is on column 34 in CACTI 5.3 output *)
	columnCapacity = 2;
	columnReadEnergy = 15;
	columnWriteEnergy = 16;
	columnStaticLeakagePerBank = 18;
	columnArea = 21;
	
	rawData = Import[CACTI5outFile]; (* Read CSV file *)
	
	(* Drop the first line which only contains the table headers *)
	rawData = Drop[rawData, 1];
	
	(* we need to trim all lines with unreasonable results (usually containing " nan" somewhere *)
	rawData = Select[rawData, !MemberQ[#, " nan"] &]; (* Select all lines which do not contain " nan" *)
	
	If[Length[rawData] == 0, Return[{}]]; (* Return an empty list if there is no data left anymore *)
	
	(* CACTI-D work selects the solution with highest area efficiency as this is usually the most important factor for DRAM manufacturers (reduces price per bit) *)
	rawData = Flatten[Take[Select[rawData, #[[columnAreaEfficiency]] == Max[rawData[[;; , columnAreaEfficiency]]] &], 1]]; (* Find all elements in the list with maximum area efficiency and pick the first configuration with that area efficiency *)
	
	(* Construct the key-value list for returning *)
	Return[{
		{"capacity", rawData[[columnCapacity]]},
		{"readenergy", rawData[[columnReadEnergy]] * (*nJ*) 10^-9},
		{"writeenergy", rawData[[columnWriteEnergy]] * (*nJ*) 10^-9},
		{"leakageperbank", rawData[[columnStaticLeakagePerBank]] * (*mW*) 10^-3},
		{"area", rawData[[columnArea]]}
	}];
];

End[] (* End Private Context *)

EndPackage[]