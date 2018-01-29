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
 
 (* ::Package:: *)

(* Mathematica Package *)

(* NOTE: DO NOT EXPOSE CACHED FUNCTIONS TO THE OUTSIDE WORLD. ALWAYS CALL INDIRECTLY AND CHECK FOR RETURN VALUES. IN CASE RETURN VALUES ARE INVALID, REMOVE THE WRONGLY CACHE RESULTS *)

BeginPackage["McPAT`"]

Needs["Support`"]
Needs["ExaBoundsGeneric`"]
Needs["ModelWarnings`"]
Needs["CPUPerformanceModels`"]
Needs["CPUMultithreadedModels`"]
Needs["VectorSizeConvert`"]

(* Exported symbols added here with SymbolName::usage *)  

CallMcPAT::usage =
  "CallMcPAT[ machineProperties_, algorithmProperties_, experimentName_, resultsPath_ ]"

GetMcPATProcessorParameters::usage =
  "GetMcPATProcessorParameters[machineProperties_, algorithmProperties_]"
  
(*ParseMcPATOutput::usage = 
  "ParseMcPATOutput[ experimentName_, resultsPath_ ]"*)
  
ClearMcPATCache::usage =
  "ClearMcPATCache[]"

Begin["`Private`"] (* Begin Private Context *)

(* Clear caches of all cached functions *)
(* KEEP UP TO DATE! *)
ClearMcPATCache[] :=
Block[{},
	ClearCache[GetMcPATCachedParameters];
	ClearCache[GetMcPATProcessorParametersCached];
]; 

(* In case that McPAT returns an error, {} is cached and we need to remove that wrong cached result from the DownValues of GetMcPATCachedParameters and GetMcPATProcessorParameters (which is the subexpression Return[{}])*)
ClearIncorrectCachedResults[] :=
Block[{},
	DownValues[GetMcPATCachedParameters] = DeleteCases[DownValues[GetMcPATCachedParameters], _?(!FreeQ[#, Return[{}]] &)];
	DownValues[GetMcPATProcessorParametersCached] = DeleteCases[DownValues[GetMcPATProcessorParametersCached], _?(!FreeQ[#, Return[{}]] &)];
];

GetMcPATProcessorParameters[machineProperties_, algorithmProperties_, homogeneousThreads_Integer : 1] :=
BlockCheck[{retval},
	retval = GetMcPATProcessorParametersCached[machineProperties, algorithmProperties, homogeneousThreads];
	If [retval == {}, ClearIncorrectCachedResults[]];
	Return[retval];
];

GetMcPATProcessorParametersCached[machineProperties_, algorithmProperties_, homogeneousThreads_Integer : 1] :=
GetMcPATProcessorParametersCached[machineProperties, algorithmProperties, homogeneousThreads] =
Module[{configFile},
	(*CellPrint["Request McPAT!"];*)
	configFile = GetMcPATConfig[ machineProperties, algorithmProperties, homogeneousThreads ];
	Return[GetMcPATCachedParameters[configFile]];
];

GetMcPATCachedParameters[configFile_] :=
GetMcPATCachedParameters[configFile] = (* Cache results *)
Module[{experimentName},
	(* Temporary experiment name for this McPAT run *)
	experimentName = "ExaBoundsMcPATOut"(*<>ToString[SessionTime[]]*);
	(* Run McPAT and parse results *)
	(*CellPrint["Run McPAT!"];*)
	RunMcPAT[ configFile, experimentName, ExaBoundsDirectory[]];
	Return[ParseMcPATOutput[ experimentName, ExaBoundsDirectory[] ]];
];

GetParameterTuple[ file_ ] :=
Module[{subleakage, gateleakage, dynamic, area, string, tuple, key, value},
	(* Read dynamic and static power from the McPAT output file *)
	subleakage = 0;
	gateleakage = 0;
	dynamic = 0;
	area = 0;
	While[True,
		string = Read[file, {String}];
		If [string == EndOfFile, Return[EndOfFile]];
		(*If [string[[1]] == "", Break[]];*)
		tuple = StringTrim[StringSplit[string[[1]], "="]];
		key = tuple[[1]];
		If[StringMatchQ[key, "Device Type"~~___], Continue[]]; (* Skip the device type entry as it will fail on matching a number *)
		value = Read[StringToStream[tuple[[2]]], {Number}][[1]];
		Switch[key,
			"Area", area = value,
			"Subthreshold Leakage", subleakage = value,
			"Gate Leakage", gateleakage = value,
			"Runtime Dynamic", dynamic = value; Break[] (* Runtime dynamic is the last entry of the list, break the while *)
		];
	];
	Return[{
		{"static", subleakage + gateleakage},
		{"dynamic", dynamic},
		{"area", area}
	}];
		
];

FindStringStart[file_, str_] :=
Module[{string},
	(* Search the file until the first occurance of a line starting with str (white space is trimmed) *)
	While[True,
		string = Read[file, {String}];
		If[string == EndOfFile, Return[EndOfFile]];
		If[StringMatchQ[StringTrim[string[[1]]], str~~___], Break[]];
	];
	Return[string];
];

ParseMcPATOutput[ experimentName_, resultsPath_ ] :=
Module[{resultsFile, file, string, cores, procTuple, coreTuple, l3Tuple, nocTuple},
	resultsFile = FileNameJoin[{resultsPath, experimentName<> ".txt"}] ;
	
	If [!FileExistsQ[resultsFile], MessageDialog["File " <> resultsFile <> " does not exist"]; Return[{}]];
	
	file = OpenRead[resultsFile];
	
	string = Find[file, "Processor: "]; (* Find the "Processor: " line where all data starts *)
	If[string == EndOfFile, CellPrint["Unexpected end of file, could not find \"Processor: \" in file " <> resultsFile]; Close[file]; Return[{}]];
	
	(* The next code has to be executed in the order in which the data appears in the McPAT output: *)
	
	(* Read processor tuples *)
	procTuple = GetParameterTuple[file];
	If[procTuple == EndOfFile, CellPrint["Unexpected end of file, could not find \"Processor: \" in file " <> resultsFile]; Close[file]; Return[{}]];
	
	(* Find the total core count and read core data *)
	string = FindStringStart[file, "Total Cores"];
	If[string == EndOfFile, CellPrint["Unexpected end of file, could not find \"Processor: \" in file " <> resultsFile]; Close[file]; Return[{}]];
	cores = Read[StringToStream[string[[1]]], {Word, Word, Number, Word}][[3]];
	coreTuple = GetParameterTuple[file];
	If[coreTuple == EndOfFile, CellPrint["Unexpected end of file, could not find \"Processor: \" in file " <> resultsFile]; Close[file]; Return[{}]];
	
	(* Find the L3 data and read *)
	string = FindStringStart[file, "Total L3s"];
	If[string == EndOfFile, CellPrint["Unexpected end of file, could not find \"Processor: \" in file " <> resultsFile]; Close[file]; Return[{}]];
	l3Tuple = GetParameterTuple[file];
	If[l3Tuple == EndOfFile, CellPrint["Unexpected end of file, could not find \"Processor: \" in file " <> resultsFile]; Close[file]; Return[{}]];
	
	(* Find the NoC data and read *)
	string = FindStringStart[file, "Total NoCs"];
	If[string == EndOfFile, CellPrint["Unexpected end of file, could not find \"Processor: \" in file " <> resultsFile]; Close[file]; Return[{}]];
	nocTuple = GetParameterTuple[file];
	If[nocTuple == EndOfFile, CellPrint["Unexpected end of file, could not find \"Processor: \" in file " <> resultsFile]; Close[file]; Return[{}]];
	
	Close[file];
	
	(* Create a key-value set to return *)
	Return[{
		{"totalPower", GetKeyValue[procTuple, "static"] + GetKeyValue[procTuple, "dynamic"]},
		{"totalArea", GetKeyValue[procTuple, "area"]},
		{"dynamicPower", GetKeyValue[procTuple, "dynamic"]},
		{"staticPower", GetKeyValue[procTuple, "static"]},
		{"totalCore", (GetKeyValue[coreTuple, "static"] + GetKeyValue[coreTuple, "dynamic"]) / cores}, (* This is the average power per core! *)
		{"dynamicCore", GetKeyValue[coreTuple, "dynamic"] / cores},
		{"staticCore", GetKeyValue[coreTuple, "static"] / cores},
		{"totalL3", GetKeyValue[l3Tuple, "static"] + GetKeyValue[l3Tuple, "dynamic"]}, 
		{"dynamicL3", GetKeyValue[l3Tuple, "dynamic"]},
		{"staticL3", GetKeyValue[l3Tuple, "static"]},
		{"totalNoC", GetKeyValue[nocTuple, "static"] + GetKeyValue[nocTuple, "dynamic"]},
		{"dynamicNoC", GetKeyValue[nocTuple, "dynamic"]},
		{"staticNoC", GetKeyValue[nocTuple, "static"]}
	}];
];

CallMcPAT[ machineProperties_, algorithmProperties_, experimentName_ , resultsPath_ ] :=
Module[{configFile},
	(* Gero's original behavior *)
	configFile = GetMcPATConfig[ machineProperties, algorithmProperties];
	RunMcPAT[ configFile, experimentName, resultsPath ]; (* Not cached ! *)
]

(* experimentName is used as the file-name base for McPAT input and output,
  adding .xml and .txt suffixes, respectively. XML config file and results
  are written to resultsPath. *)
GetMcPATConfig[ machineProperties_List, algorithmProperties_List, homogeneousThreads_Integer : 1] :=
Module[{ path, configFile, temperature, bitness, physicalAddressWidth, pipelineDepth, cacheCapacity,
  cacheBlockWidth, cacheAssociativity, cacheBank, cacheThroughput, cacheLatency,
  cacheOutputWidth, cachePolicy, iCacheConfig, dCacheConfig, l2Config, l3Config,
  totalInstructions, intInstructions, fpInstructions, branchInstructions,
  loadInstructions, storeInstructions, cpi, executionCycles, l1MissRate, l2MissRate,
  l3MissRate, memoryAccessRate, vdd,
  vectorFUmultiplier, vectorFUs,
  homogeneousMultiplier,
  branchMispredictions, pipelineDutyCycle, renameReads, renameWrites, fpRenameReads, fpRenameWrites,
  intRegfileReads, fpRegfileReads, intRegfileWrites, fpRegfileWrites,
  L1ReadMisses, L1WriteMisses, L2ReadAccesses, L2WriteAccesses, L2ReadMisses, L2WriteMisses,
  L3ReadAccesses, L3WriteAccesses, L3ReadMisses, L3WriteMisses, NOCAccesses, MemAccesses,
  MemReads, MemWrites, machineType},
  
  (*resultsPathAndBaseName = FileNameJoin[{resultsPath, experimentName}];
  Print["resultsPathAndBaseName: "<>resultsPathAndBaseName];
  Return[];*)
  
  (* We have three options: a single-threaded workload, a homogeneous workload on all cores, or a heterogeneous workload *)
  (* For the single-threaded and homogeneous case, we provide an XML file with just one core configured (as all are the same) *)
  (* In case of a homogenous workload, we need to multiply the statistics with the core count *)
  
  (* load XML template for McPAT config *)
  path = ExaBoundsDirectory[]; (* assuming XML file in the same directory as the Mathematica notebook *)
  (*Print["Looking for McPAT.xml at "<>path(*<>"\r with files \r"<>ToString[FileNames["*", path]]*)];*)
  configFile = Import[ "McPAT.xml", "XML", Path -> {path} ];
  
  (* Get all HW parameters *)
  
  (* CACTI requires a temperature "between 300 and 400 Kelvin and multiple of 10." *)
  temperature = Round[ GetArchKeyValueCheck[machineProperties, "T"], 10];
  If[temperature < 300 || temperature > 400, Message[GetMcPATConfig::invalidtemperature, temperature]];
  
  (* current 64 bit machines have less than 64 address bits -- commonly 48 or 52 *)
  bitness = GetArchKeyValueCheck[machineProperties, "n0Bits" ];
  If[ bitness < 64, physicalAddressWidth = bitness, physicalAddressWidth = 52 ];
  
  pipelineDepth = ToString[{GetArchKeyValueCheck[machineProperties, "n0pipe"], GetArchKeyValueCheck[machineProperties, "n0pipe"]}];
  pipelineDepth = StringReplace[pipelineDepth, {"{" -> "", "}" -> ""}]; (* drop curly brackets *)
  
  machineType = If[GetArchKeyValueCheck[machineProperties, "inorder"], 1, 0];
  
  (* common I cache, D cache parameters *)
  cacheCapacity = IntegerPart[GetArchKeyValueCheck[machineProperties, "M0L1" ]]; (* M0L1 represents each, not the total of both together *)
  cacheBank = 1; (* TODO: check value *)
  cacheThroughput = 8; (* w.r.t. core clock *) (* TODO: check value *)
  cachePolicy = 0; (* 0 no write or write-though with non-write allocate; 1 write-back with write-allocate *)
  
  (* I cache config *)
  cacheBlockWidth = GetArchKeyValueCheck[machineProperties, "L1iagran" ];
  cacheAssociativity = GetArchKeyValueCheck[machineProperties, "L1iassoc" ];
  cacheLatency = GetArchKeyValueCheck[machineProperties, "T0L1latency" ]; (* w.r.t. core clock *)
  cacheOutputWidth = GetArchKeyValueCheck[machineProperties, "L1iagran" ];
  iCacheConfig = ToString[{cacheCapacity,cacheBlockWidth,cacheAssociativity,cacheBank,cacheThroughput,cacheLatency,cacheOutputWidth,cachePolicy}];
  iCacheConfig = StringReplace[iCacheConfig, {"{" -> "", "}" -> ""}]; (* drop curly brackets *)
  
  (* D cache config *)
  cacheBlockWidth = GetArchKeyValueCheck[machineProperties, "L1dagran" ];
  cacheAssociativity = GetArchKeyValueCheck[machineProperties, "L1dassoc" ];
  cacheLatency = GetArchKeyValueCheck[machineProperties, "T0L1latency" ]; (* w.r.t. core clock *)
  cacheOutputWidth = GetArchKeyValueCheck[machineProperties, "L1dagran" ];
  dCacheConfig = ToString[{cacheCapacity,cacheBlockWidth,cacheAssociativity,cacheBank,cacheThroughput,cacheLatency,cacheOutputWidth,cachePolicy}];
  dCacheConfig = StringReplace[dCacheConfig, {"{" -> "", "}" -> ""}]; (* drop curly brackets *)
  
  (* L2 config *)
  cacheCapacity = IntegerPart[GetArchKeyValueCheck[machineProperties, "M0L2" ]];
  cacheBlockWidth = GetArchKeyValueCheck[machineProperties, "L2dagran" ];
  cacheAssociativity = GetArchKeyValueCheck[machineProperties, "L2dassoc" ];
  cacheBank = 8; (* TODO: check value *)
  cacheThroughput = 8; (* w.r.t. core clock *) (* TODO: check value *)
  cacheLatency = GetArchKeyValueCheck[machineProperties, "T0L2latency" ]; (* w.r.t. core clock *)
  cacheOutputWidth = GetArchKeyValueCheck[machineProperties, "L2dagran" ];
  cachePolicy = 1; (* 0 no write or write-though with non-write allocate; 1 write-back with write-allocate *)
  l2Config = ToString[{cacheCapacity,cacheBlockWidth,cacheAssociativity,cacheBank,cacheThroughput,cacheLatency,cacheOutputWidth,cachePolicy}];
  l2Config = StringReplace[l2Config, {"{" -> "", "}" -> ""}]; (* drop curly brackets *)
  
  (* L3 config *)
  cacheCapacity = IntegerPart[GetArchKeyValueCheck[machineProperties, "M1" ]];
  cacheBlockWidth = GetArchKeyValueCheck[machineProperties, "L3dagran" ];
  cacheAssociativity = GetArchKeyValueCheck[machineProperties, "L3dassoc" ];
  cacheBank = 16; (* TODO: check value *)
  cacheThroughput = 16; (* w.r.t. core clock *) (* TODO: check value *)
  cacheLatency = GetArchKeyValueCheck[machineProperties, "T0L3latency" ]; (* w.r.t. core clock *)
  cacheOutputWidth = GetArchKeyValueCheck[machineProperties, "L3dagran" ];
  cachePolicy = 1; (* 0 no write or write-though with non-write allocate; 1 write-back with write-allocate *)
  l3Config = ToString[{cacheCapacity,cacheBlockWidth,cacheAssociativity,cacheBank,cacheThroughput,cacheLatency,cacheOutputWidth,cachePolicy}];
  l3Config = StringReplace[l3Config, {"{" -> "", "}" -> ""}]; (* drop curly brackets *)
  
  (* Vector stuff *)
  vectorFUmultiplier = GetArchKeyValueCheck[machineProperties, "n0vectorbits"] / 32; (* Assume that we need to add n0vectorbits/32 additional FUs *)
  vectorFUs = GetArchKeyValueCheck[machineProperties, "n0vectorFU"];
  
  vdd = GetArchKeyValueCheck[machineProperties, "V0" ];
  
  (* Update HW XML *)
  (* update tags with ExaBounds values, applying a list of replacement rules *)
  configFile = configFile /.
    (* HW parameters *)
    XMLComponentNestedUpdateRule["system", "param", "number_of_cores", GetArchKeyValueCheck[machineProperties, "n1"]] /.
    XMLComponentNestedUpdateRule["system", "param", "number_of_L2s", Boole[ExistsCacheLevel[2 (*L3*), machineProperties]]] /.
    XMLComponentNestedUpdateRule["system", "param", "Private_L2", Boole[ExistsCacheLevel[2 (*L3*), machineProperties]]] /.
    XMLComponentNestedUpdateRule["system", "param", "number_of_L3s", Boole[ExistsCacheLevel[3 (*L3*), machineProperties]]] /.
    XMLComponentNestedUpdateRule["system", "param", "core_tech_node", GetArchKeyValueCheck[machineProperties, "Lnode"]] /. 
    XMLComponentNestedUpdateRule["system", "param", "target_core_clockrate", GetMcPATFrequency[machineProperties, "f0"]] /.
    XMLComponentNestedUpdateRule["system", "param", "temperature", temperature] /.
    XMLComponentNestedUpdateRule["system", "param", "number_cache_levels", GetNumberCacheLevels[machineProperties]] /.
    XMLComponentNestedUpdateRule["system", "param", "machine_bits", bitness] /.
    XMLComponentNestedUpdateRule["system", "param", "virtual_address_width", bitness] /.
    XMLComponentNestedUpdateRule["system", "param", "physical_address_width", physicalAddressWidth] /.
    XMLComponentNestedUpdateRule["system.core0", "param", "clock_rate", GetMcPATFrequency[machineProperties, "f0"]] /.
    XMLComponentNestedUpdateRule["system.core0", "param", "vdd", vdd] /.
    XMLComponentNestedUpdateRule["system.core0", "param", "machine_type", machineType] /.
    XMLComponentNestedUpdateRule["system.core0", "param", "number_hardware_threads", GetArchKeyValueCheck[machineProperties, "n0threads"]] /.
    XMLComponentNestedUpdateRule["system.core0", "param", "decode_width", GetArchKeyValueCheck[machineProperties, "n0"]] /.
    XMLComponentNestedUpdateRule["system.core0", "param", "issue_width", GetArchKeyValueCheck[machineProperties, "n0"]] /.
    XMLComponentNestedUpdateRule["system.core0", "param", "peak_issue_width", GetArchKeyValueCheck[machineProperties, "n0"]] /.
    XMLComponentNestedUpdateRule["system.core0", "param", "commit_width", GetArchKeyValueCheck[machineProperties, "n0"]] /.
    XMLComponentNestedUpdateRule["system.core0", "param", "fp_issue_width", GetArchKeyValueCheck[machineProperties, "n0fp"]] /. 
    XMLComponentNestedUpdateRule["system.core0", "param", "prediction_width", GetArchKeyValueCheck[machineProperties, "n0control"]] /.
    XMLComponentNestedUpdateRule["system.core0", "param", "pipeline_depth", pipelineDepth] /.
    XMLComponentNestedUpdateRule["system.core0", "param", "ALU_per_core", GetArchKeyValueCheck[machineProperties, "n0int"] + vectorFUmultiplier * vectorFUs] /.
    XMLComponentNestedUpdateRule["system.core0", "param", "MUL_per_core", GetArchKeyValueCheck[machineProperties, "n0int"] + vectorFUmultiplier * vectorFUs] /. (* assuming each FU can do everything *)
    XMLComponentNestedUpdateRule["system.core0", "param", "FPU_per_core", GetArchKeyValueCheck[machineProperties, "n0fp"] + vectorFUmultiplier * vectorFUs] /.
    XMLComponentNestedUpdateRule["system.core0", "param", "instruction_window_size", GetArchKeyValueCheck[machineProperties, "n0ROB"]] /. (* here, ROB = issue window (see Kharkanis) *)
    XMLComponentNestedUpdateRule["system.core0", "param", "fp_instruction_window_size", GetArchKeyValueCheck[machineProperties, "n0ROB"]] /. (* here, ROB = issue window (see Kharkanis) *)
    XMLComponentNestedUpdateRule["system.core0", "param", "ROB_size", GetArchKeyValueCheck[machineProperties, "n0ROB"]] /.
    XMLComponentNestedUpdateRule["system.core0", "param", "archi_Regs_IRF_size", 32] /. (*TODO: Add ExaBounds parameter for int register number.*)
    XMLComponentNestedUpdateRule["system.core0", "param", "archi_Regs_FRF_size", 32] /. (*TODO: Add ExaBounds parameter for FP register number.*)
    XMLComponentNestedUpdateRule["system.core0.icache", "param", "icache_config", iCacheConfig] /.
    XMLComponentNestedUpdateRule["system.core0.dcache", "param", "dcache_config", dCacheConfig] /.
    XMLComponentNestedUpdateRule["system.L1Directory0", "param", "clockrate", GetMcPATFrequency[machineProperties, "f0"]] /.
    XMLComponentNestedUpdateRule["system.L1Directory0", "param", "vdd", vdd] /.
    XMLComponentNestedUpdateRule["system.L2Directory0", "param", "clockrate", GetMcPATFrequency[machineProperties, "f0"]] /.
    XMLComponentNestedUpdateRule["system.L2Directory0", "param", "vdd", vdd] /.
    XMLComponentNestedUpdateRule["system.L20", "param", "L2_config", l2Config] /.
    XMLComponentNestedUpdateRule["system.L20", "param", "clockrate", GetMcPATFrequency[machineProperties, "f0"]] /.
    XMLComponentNestedUpdateRule["system.L20", "param", "vdd", vdd] /.
    XMLComponentNestedUpdateRule["system.L30", "param", "L3_config", l3Config] /.
    XMLComponentNestedUpdateRule["system.L30", "param", "clockrate", GetMcPATFrequency[machineProperties, "f1"]] /.
    XMLComponentNestedUpdateRule["system.L30", "param", "vdd", vdd] /.
    XMLComponentNestedUpdateRule["system.NoC0", "param", "clockrate", GetMcPATFrequency[machineProperties, "f1"]] /.
    XMLComponentNestedUpdateRule["system.NoC0", "param", "vdd", vdd] /.
    XMLComponentNestedUpdateRule["system.mc", "param", "vdd", vdd] /.
    XMLComponentNestedUpdateRule["system.niu", "param", "vdd", vdd] /.
    XMLComponentNestedUpdateRule["system.pcie", "param", "vdd", vdd] /.
    XMLComponentNestedUpdateRule["system.flashc", "param", "vdd", vdd];  
  
  (* Now we go for SW parameters, which we need per-thread *)
  (* All these things return lists! *)
   totalInstructions = IntegerPart[MCGetAlgKeyValueCheck[algorithmProperties, "LSys"]];
  intInstructions = IntegerPart[MCGetTotalOperationCount[algorithmProperties, "F0int"]] + IntegerPart[MCGetTotalOperationCount[algorithmProperties, "F0addr"]];
  fpInstructions = IntegerPart[MCGetTotalOperationCount[algorithmProperties, "F0fp"]];
  branchInstructions = IntegerPart[MCGetTotalOperationCount[algorithmProperties, "F0control"]];
  
  loadInstructions = IntegerPart[MCGetTotalOperationCount[algorithmProperties, "F0load"]];
  storeInstructions = IntegerPart[MCGetTotalOperationCount[algorithmProperties, "F0store"]];
  cpi = MCd0[machineProperties, algorithmProperties, homogeneousThreads];
  executionCycles = IntegerPart[MCExecutionCycles[machineProperties, algorithmProperties, homogeneousThreads]];
  l1MissRate = MCCacheMissrate[MemoryL1Cache[], machineProperties, algorithmProperties, homogeneousThreads];
  l2MissRate = MCCacheMissrate[MemoryL2Cache[], machineProperties, algorithmProperties, homogeneousThreads];
  l3MissRate = MCCacheMissrate[MemoryL3Cache[], machineProperties, algorithmProperties, homogeneousThreads];
  memoryAccessRate = l1MissRate * l2MissRate * l3MissRate;
  
  branchMispredictions = IntegerPart[MCFMispredicted[machineProperties, algorithmProperties, homogeneousThreads] * branchInstructions];
  pipelineDutyCycle = 1/(cpi * GetArchKeyValueCheck[machineProperties, "n0"]);
  renameReads = IntegerPart[MCGetAlgKeyValueCheck[algorithmProperties, "F0regreads"] * totalInstructions]; (* TODO: should this exclude FP? *)
  renameWrites = IntegerPart[MCGetAlgKeyValueCheck[algorithmProperties, "F0regwrites"] * totalInstructions]; (* TODO: should this exclude FP? *)
  fpRenameReads = IntegerPart[MCGetAlgKeyValueCheck[algorithmProperties, "F0regreads"] * fpInstructions];
  fpRenameWrites = IntegerPart[MCGetAlgKeyValueCheck[algorithmProperties, "F0regwrites"] * fpInstructions];
  intRegfileReads = IntegerPart[MCGetAlgKeyValueCheck[algorithmProperties, "F0regreads"] * intInstructions];
  fpRegfileReads = IntegerPart[MCGetAlgKeyValueCheck[algorithmProperties, "F0regreads"] * fpInstructions];
  intRegfileWrites = IntegerPart[MCGetAlgKeyValueCheck[algorithmProperties, "F0regwrites"] * intInstructions];
  fpRegfileWrites = IntegerPart[MCGetAlgKeyValueCheck[algorithmProperties, "F0regwrites"] * fpInstructions];
  
  (* At this point we have either a list of items or a single item per property. If we have a single item: homogeneous workload on all cores. If we have a list of length 1: one thread. Everything else: heterogeneous*)
  (* We simply use a single system.core0 in the XML for all types of workloads *)
   
  (* Convert core0 parameters to correct number *)
  Which[	AllTrue[algorithmProperties, Head[#] === List &],
  				(* homogeneous workload case *)
  				(* For a single system.core0, we define a multiplier first *)
 				homogeneousMultiplier = Min[homogeneousThreads, GetArchKeyValueCheck[machineProperties, "n1"]];
 				(* Aggregate McPAT stats as per manual *)
 				(* First all dependent statistics (before totalInstructions etc is multiplied) *)
  				L1ReadMisses = IntegerPart[loadInstructions * l1MissRate] * homogeneousMultiplier;
  				L1WriteMisses = IntegerPart[storeInstructions * l1MissRate] * homogeneousMultiplier;
  				L2ReadAccesses = L1ReadMisses;
  				L2WriteAccesses = L1WriteMisses;
  				L2ReadMisses = IntegerPart[loadInstructions * l1MissRate * l2MissRate] * homogeneousMultiplier;
  				L2WriteMisses = IntegerPart[storeInstructions * l1MissRate * l2MissRate] * homogeneousMultiplier;
  				L3ReadAccesses = L2ReadMisses;
  				L3WriteAccesses = L2WriteMisses;
  				L3ReadMisses = IntegerPart[loadInstructions * l1MissRate * l2MissRate * l3MissRate] * homogeneousMultiplier;
  				L3WriteMisses = IntegerPart[storeInstructions * l1MissRate * l2MissRate * l3MissRate] * homogeneousMultiplier;
  				NOCAccesses = IntegerPart[(loadInstructions + storeInstructions) * (l1MissRate * l2MissRate + memoryAccessRate)] * homogeneousMultiplier;
  				MemAccesses = IntegerPart[(loadInstructions + storeInstructions) * memoryAccessRate] * homogeneousMultiplier;
  				MemReads = IntegerPart[loadInstructions * memoryAccessRate] * homogeneousMultiplier;
  				MemWrites = IntegerPart[storeInstructions * memoryAccessRate] * homogeneousMultiplier;
  				(* Rest *)
 				executionCycles = executionCycles; (* I guess this doesn't change *)
 				totalInstructions = totalInstructions * homogeneousMultiplier; (* We run n1 times the workload *)
 				intInstructions = intInstructions * homogeneousMultiplier;
  				fpInstructions = fpInstructions * homogeneousMultiplier;
  				branchInstructions = branchInstructions * homogeneousMultiplier;
  				branchMispredictions = branchMispredictions * homogeneousMultiplier;
  				loadInstructions = loadInstructions * homogeneousMultiplier;
  				storeInstructions = storeInstructions * homogeneousMultiplier;
  				pipelineDutyCycle = pipelineDutyCycle; (* I guess this doesn't change *)
  				renameReads = renameReads * homogeneousMultiplier;
  				renameWrites = renameWrites * homogeneousMultiplier;
  				fpRenameReads = fpRenameReads * homogeneousMultiplier;
  				fpRenameWrites = fpRenameWrites * homogeneousMultiplier;
  				intRegfileReads = intRegfileReads * homogeneousMultiplier;
  				fpRegfileReads = fpRegfileReads * homogeneousMultiplier;
  				intRegfileWrites = intRegfileWrites * homogeneousMultiplier;
  				fpRegfileWrites = fpRegfileWrites * homogeneousMultiplier;
  				,
  			AllTrue[algProperties, Head[#] === Rule &], 
  				(* heterogeneous workload case *)
  				(* We still have a sinlge system.core0, so we sum the properties *)
  				executionCycles = Max[executionCycles]; (* We select the maximum execution cycles as the execution cycles *)
  				(* First all dependent statistics (before totalInstructions etc is summed) *)
  				L1ReadMisses = Total[IntegerPart[loadInstructions * l1MissRate]];
  				L1WriteMisses = Total[IntegerPart[storeInstructions * l1MissRate]];
  				L2ReadAccesses = L1ReadMisses;
  				L2WriteAccesses = L1WriteMisses;
  				L2ReadMisses = Total[IntegerPart[loadInstructions * l1MissRate * l2MissRate]];
  				L2WriteMisses = Total[IntegerPart[storeInstructions * l1MissRate * l2MissRate]];
  				L3ReadAccesses = L2ReadMisses;
  				L3WriteAccesses = L2WriteMisses;
  				L3ReadMisses = Total[IntegerPart[loadInstructions * l1MissRate * l2MissRate * l3MissRate]];
  				L3WriteMisses = Total[IntegerPart[storeInstructions * l1MissRate * l2MissRate * l3MissRate]];
  				NOCAccesses = Total[IntegerPart[(loadInstructions + storeInstructions) * (l1MissRate * l2MissRate + memoryAccessRate)]];
  				MemAccesses = Total[IntegerPart[(loadInstructions + storeInstructions) * memoryAccessRate]];
  				MemReads = Total[IntegerPart[loadInstructions * memoryAccessRate]];
  				MemWrites = Total[IntegerPart[storeInstructions * memoryAccessRate]];
  				(* Rest *)
 				totalInstructions = Total[totalInstructions];
 				intInstructions = Total[intInstructions];
  				fpInstructions = Total[fpInstructions];
  				branchInstructions = Total[branchInstructions];
  				branchMispredictions = Total[branchMispredictions];
  				loadInstructions = Total[loadInstructions];
  				storeInstructions = Total[storeInstructions];
  				pipelineDutyCycle = Mean[pipelineDutyCycle]; (* we take the average of all threads *)
  				renameReads = Total[renameReads];
  				renameWrites = Total[renameWrites];
  				fpRenameReads = Total[fpRenameReads];
  				fpRenameWrites = Total[fpRenameWrites];
  				intRegfileReads = Total[intRegfileReads]; 
  				fpRegfileReads = Total[fpRegfileReads];
  				intRegfileWrites = Total[intRegfileWrites];
  				fpRegfileWrites = Total[fpRegfileWrites];
  				,
  			_,
  				(* single-thread case nothing changes, just calculate the cache/memory access rates *)
  				L1ReadMisses = IntegerPart[loadInstructions * l1MissRate];
  				L1WriteMisses = IntegerPart[storeInstructions * l1MissRate];
  				L2ReadAccesses = L1ReadMisses;
  				L2WriteAccesses = L1WriteMisses;
  				L2ReadMisses = IntegerPart[loadInstructions * l1MissRate * l2MissRate];
  				L2WriteMisses = IntegerPart[storeInstructions * l1MissRate * l2MissRate];
  				L3ReadAccesses = L2ReadMisses;
  				L3WriteAccesses = L2WriteMisses;
  				L3ReadMisses = IntegerPart[loadInstructions * l1MissRate * l2MissRate * l3MissRate];
  				L3WriteMisses = IntegerPart[storeInstructions * l1MissRate * l2MissRate * l3MissRate];
  				NOCAccesses = IntegerPart[(loadInstructions + storeInstructions) * (l1MissRate * l2MissRate + memoryAccessRate)];
  				MemAccesses = IntegerPart[(loadInstructions + storeInstructions) * memoryAccessRate];
  				MemReads = IntegerPart[loadInstructions * memoryAccessRate];
  				MemWrites = IntegerPart[storeInstructions * memoryAccessRate];
  ]; 

  (* Set homogeneous_cores flag *)
  configFile = configFile /.
    XMLComponentNestedUpdateRule["system", "param", "homogeneous_cores", 1];
  	
  (* Update SW XML *)
  (* update tags with ExaBounds values, applying a list of replacement rules *)
  configFile = configFile /.
    XMLComponentNestedUpdateRule["system", "stat", "total_cycles", executionCycles] /.
    XMLComponentNestedUpdateRule["system", "stat", "busy_cycles", executionCycles] /.
    (* TODO: total_instructions = committed_instructions assumes no mis-speculation! *)
    XMLComponentNestedUpdateRule["system.core0", "stat", "total_instructions", totalInstructions] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "int_instructions", intInstructions] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "fp_instructions", fpInstructions] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "branch_instructions", branchInstructions] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "branch_mispredictions", branchMispredictions] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "load_instructions", loadInstructions] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "store_instructions", storeInstructions] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "committed_instructions", totalInstructions] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "committed_int_instructions", intInstructions] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "committed_fp_instructions", fpInstructions] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "pipeline_duty_cycle", pipelineDutyCycle] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "total_cycles", executionCycles] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "busy_cycles", executionCycles] /.
    (* rename = RAT accesses *)
    XMLComponentNestedUpdateRule["system.core0", "stat", "rename_reads", renameReads] /. (* TODO: should this exclude FP? *)
    XMLComponentNestedUpdateRule["system.core0", "stat", "rename_writes", renameWrites] /. (* TODO: should this exclude FP? *)
    XMLComponentNestedUpdateRule["system.core0", "stat", "fp_rename_reads",fpRenameReads] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "fp_rename_writes",fpRenameWrites] /.
    (* inst. window: guesses based on Xeon.xml *)
    XMLComponentNestedUpdateRule["system.core0", "stat", "inst_window_reads", totalInstructions] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "inst_window_writes", totalInstructions] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "inst_window_wakeup_accesses", 2 * totalInstructions] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "fp_inst_window_reads", fpInstructions] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "fp_inst_window_writes", fpInstructions] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "fp_inst_window_wakeup_accesses", 2 * fpInstructions] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "int_regfile_reads", intRegfileReads] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "float_regfile_reads", fpRegfileReads] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "int_regfile_writes", intRegfileWrites] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "float_regfile_writes", fpRegfileWrites] /.
    (* McPAT doesn't even seem to use this paramter! Arbitrarily assuming an
    average procedure length of 200 instructions including call and return,
    i.e., 1% of instructions are calls or returns. Xeon.xml has 65%! *)
    XMLComponentNestedUpdateRule["system.core0", "stat", "context_switches", IntegerPart[0.01 * totalInstructions]] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "ialu_accesses", intInstructions] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "fpu_accesses", fpInstructions] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "mul_accesses", 0] /. (* TODO: Included in the other two? *)
    (* CDB = common data bus? What's the difference? *)
    XMLComponentNestedUpdateRule["system.core0", "stat", "cdb_alu_accesses", intInstructions] /.
    XMLComponentNestedUpdateRule["system.core0", "stat", "cdb_mul_accesses", 0] /. (* TODO: Included in the other two? *)
    XMLComponentNestedUpdateRule["system.core0", "stat", "cdb_fpu_accesses", fpInstructions] /.
    XMLComponentNestedUpdateRule["system.core0.icache", "stat", "read_accesses", totalInstructions] /.
    (* TODO: i-cache misses are missing *)
    XMLComponentNestedUpdateRule["system.core0.dcache", "stat", "read_accesses", loadInstructions] /.
    XMLComponentNestedUpdateRule["system.core0.dcache", "stat", "write_accesses", storeInstructions] /.
    XMLComponentNestedUpdateRule["system.core0.dcache", "stat", "read_misses", L1ReadMisses] /.
    XMLComponentNestedUpdateRule["system.core0.dcache", "stat", "write_misses", L1WriteMisses] /.
    XMLComponentNestedUpdateRule["system.core0.BTB", "stat", "read_accesses", branchInstructions] /.
    XMLComponentNestedUpdateRule["system.L1Directory0", "stat", "read_accesses", (loadInstructions + storeInstructions)] /.
    XMLComponentNestedUpdateRule["system.L1Directory0", "stat", "write_accesses", L1ReadMisses + L1WriteMisses] /.
    XMLComponentNestedUpdateRule["system.L1Directory0", "stat", "read_misses", 0] /. (* Not modeled *)
    XMLComponentNestedUpdateRule["system.L1Directory0", "stat", "write_misses", 0] /.
    XMLComponentNestedUpdateRule["system.L2Directory0", "stat", "read_accesses", L1ReadMisses + L1WriteMisses] /.
    XMLComponentNestedUpdateRule["system.L2Directory0", "stat", "write_accesses", L2ReadMisses + L2WriteMisses] /.
    XMLComponentNestedUpdateRule["system.L2Directory0", "stat", "read_misses", 0] /.  (* Not modeled *)
    XMLComponentNestedUpdateRule["system.L2Directory0", "stat", "write_misses", 0] /.
    XMLComponentNestedUpdateRule["system.L20", "stat", "read_accesses", L2ReadAccesses] /.
    XMLComponentNestedUpdateRule["system.L20", "stat", "write_accesses", L2WriteAccesses] /.
    XMLComponentNestedUpdateRule["system.L20", "stat", "read_misses", L2ReadMisses] /.
    XMLComponentNestedUpdateRule["system.L20", "stat", "write_misses", L2WriteMisses] /.
    XMLComponentNestedUpdateRule["system.L30", "stat", "read_accesses", L3ReadAccesses] /.
    XMLComponentNestedUpdateRule["system.L30", "stat", "write_accesses", L3WriteAccesses] /.
    XMLComponentNestedUpdateRule["system.L30", "stat", "read_misses", L3ReadMisses] /.
    XMLComponentNestedUpdateRule["system.L30", "stat", "write_misses", L3WriteMisses] /.
    (* assuming that L2 misses (l1MissRate * l2MissRate) and L3 misses (memoryAccessRate) go over the bus/NoC: *)
    XMLComponentNestedUpdateRule["system.NoC0", "stat", "total_accesses", NOCAccesses] /.
    XMLComponentNestedUpdateRule["system.mc", "stat", "memory_accesses", MemAccesses] /.
    XMLComponentNestedUpdateRule["system.mc", "stat", "memory_reads", MemReads] /.
    XMLComponentNestedUpdateRule["system.mc", "stat", "memory_writes", MemWrites];
(* /.
    XMLComponentNestedUpdateRule["system.", "stat", "", ]
*)
  Return[ configFile ] ;
];
GetMcPATConfig::invalidtemperature = "Temperature of `1` K is out of 300 to 400 degree Kelvin range"

RunMcPAT[ configFile_, experimentName_, resultsPath_ ] :=
Module[{inFile, outFile, returnVal},
  (*Print[ExportString[configFile, "XML"]];*)
  inFile = FileNameJoin[{resultsPath, experimentName<>".xml"}];
  outFile = FileNameJoin[{resultsPath, experimentName<>".txt"}];
  Export[inFile, configFile, "XML"];
  (*Print["Wrote "<>resultsPathAndBaseName<>".xml."];*)
  (*Print["Wrote "<>resultsPathAndBaseName<>".xml, running McPAT."];*)
  (* Delete the output file to prevent us from keeping the old file for unexpected reasons *)
  DeleteFile[outFile];
  returnVal = Run["mcpat -infile \""<>inFile<>"\" > \""<>outFile<>"\" 2>&1"];
  If[returnVal != 0,
  	CellPrint["McPAT returned an error\n"](*,
  	Print["McPAT completed, result written to "<>resultsPathAndBaseName<>".txt"];*)
  ];
];

(* Rule to replace current value of existing tag *)
XMLElementUpdateRule[ tag_, name_, value_ ] =
  XMLElement[tag, {"name" -> name, "value" -> _}, {}] :>
  XMLElement[tag, {"name" -> name, "value" -> ToString[value]}, {}]

(* Rule to replace current value of existing tag nested in component *)
XMLComponentNestedUpdateRule[ componentID_, tag_, name_, value_ ] = 
 XMLElement["component", {"id" -> componentID, componentName_}, {contentStart___, XMLElement[tag, {"name" -> name, "value" -> _}, {}], contentEnd___}] :> 
 XMLElement["component", {"id" -> componentID, componentName}, {contentStart, XMLElement[tag, {"name" -> name, "value" -> ToString[value]}, {}], contentEnd}]

(* ExaBounds frequencies are in Hz and scientific notation, whereas McPAT expects MHz in "accounting" notation. *)
GetMcPATFrequency[ machineProperties_, frequencyName_ ] := IntegerPart[ GetArchKeyValueCheck[machineProperties, frequencyName] / mega (* Hz to MHz *)]

mega = 10^6;

End[] (* End Private Context *)

EndPackage[]