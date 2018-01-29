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

(* INTEGRATED MeSAP DRAM POWER MODEL *)

BeginPackage["DRAMPowerModel`"]
(* Exported symbols added here with SymbolName::usage *)

Needs["Support`"]
Needs["ExaBoundsGeneric`"]
Needs["ModelWarnings`"]
Needs["PreDefinedMemorySpec`"] (* file contaning list of memory specifications*)
(*Needs["InstructionMix`"]*)
(*Needs["CPUPerformanceModels`"]*)
Needs["CPUMultithreadedModels`"]
Needs["VectorSizeConvert`"]

(*
TotalMemoryCycles::usage = 
 "TotalMemoryCycles[memSpec_, archProperties_, stats_]"

ReadBurstPerRankDevice::usage =
  "ReadBurstPerRankDevice[archProperties_, stats_]"

WriteBurstPerRankDevice::usage =
  "WriteBurstPerRankDevice[archProperties_, stats_]"
*)
(*
CmdStatsPerRankDevice::usage =
  "CmdStatsPerRankDevice[memSpec_, archProperties_, stats_]"
  *)
(*for use from notebox to test*)
DRAMPower::usage = 
  "DRAMPower[archProperties_, stats_]"

GetMeSAPMemoryParameters::usage =
  "GetMeSAPMemoryParameters[memory_, archProperties_, algProperties_, homogeneousThreads_ : 1]"

Begin["`Private`"] (* Begin Private Context *)

GetMeSAPMemoryParameters[memory_String, archProperties_List, algProperties_List, homogeneousThreads_Integer : 1] :=
  Module[{ algVectorProperties, applicationCycles, DRAMaccesses, fload, fstore, cacheLine, DRAMreadBytes, staticPower, dynamicPower, PowerRankDevice,
  	DRAMwriteBytes, applicationStats, drampowerExcludingIOTermPower, ioTermPower, homogeneousMultiplier},

    If[memory !=  "M2" && memory != "M3", Return[{}]]; (* M2 or M3 not really used here *)

    (* convert to vector properties if not done before, else can remove this line *)
    algVectorProperties = algProperties;
    (*algVectorProperties = GetAlgorithmVectorProperties[archProperties, algProperties];*) (* Conversion of vector properties has to be done before calling this function *)

    (*calculate performance statistics*)  	
  	applicationCycles = Max[MCExecutionCycles[archProperties, algVectorProperties, homogeneousThreads]];
    (*Print["Execution seconds: "<> ToString[ExecutionSeconds[archProperties, algVectorProperties]]];*)
    (* 1 access = 1 cacheLine *)
    DRAMaccesses = MCFiMem[MemoryDRAM[], archProperties, algVectorProperties, homogeneousThreads] * MCGetAlgKeyValueCheck[algVectorProperties, "LSys"]; (* Per thread *)
    cacheLine = GetArchKeyValueCheck[archProperties, "L3dagran" ];
    If[AllTrue[algProperties, Head[#] === List &],
    	fload = {GetTotalVMixFraction[GetAlgKeyValueCheck[algVectorProperties, "F0load" ]]};
    	fstore = {GetTotalVMixFraction[GetAlgKeyValueCheck[algVectorProperties, "F0store" ]]};
    	, (* Replacement list *)
    	fload = GetTotalVMixFraction[#] & /@ MCGetAlgKeyValueCheck[algVectorProperties, "F0load" ];
    	fstore = GetTotalVMixFraction[#] & /@ MCGetAlgKeyValueCheck[algVectorProperties, "F0store" ];
    ];
    
    DRAMreadBytes = Total[DRAMaccesses * cacheLine * fload / (fload + fstore)];
    DRAMwriteBytes = Total[DRAMaccesses * cacheLine * fstore / (fload + fstore)];
    
    (*for DSE as discussed with Rik*)
    (* If it is a homogeneous workload, we still need to multiply the DRAMreadBytes for with the multiplier for all threads. Otherwise, they are already OK *)
  	If[AllTrue[algProperties, Head[#] === List &],
  		(* homogeneous workload case *)
  		(* For a single system.core0, we define a multiplier first *)
 		homogeneousMultiplier = Min[homogeneousThreads, GetArchKeyValueCheck[archProperties, "n1"]];
 		DRAMreadBytes =  DRAMreadBytes * homogeneousMultiplier;
 		DRAMwriteBytes =  DRAMwriteBytes * homogeneousMultiplier;
  	];

    (*create stats key value list format accepted by TotalDRAMPower[] module*)
    applicationStats = {{"TotalCycles", applicationCycles}, {"nReadBytes", DRAMreadBytes}, {"nWriteBytes", DRAMwriteBytes}};
  	
  	PowerRankDevice = PowerPerRankDevice[predefinedconfigMem[GetArchKeyValueCheck[archProperties, "DRAMType"]], archProperties, applicationStats] * NumberOfRankDevices[archProperties];
  	ioTermPower = IOTermPower[predefinedconfigMem[GetArchKeyValueCheck[archProperties, "DRAMType"]], archProperties, applicationStats];
  	
  	staticPower = PowerRankDevice[[1]]; 
  	dynamicPower = PowerRankDevice[[2]] + ioTermPower;
  	
  	drampowerExcludingIOTermPower = Total[PowerRankDevice];
  	
  	Return[{
		{"power", drampowerExcludingIOTermPower + ioTermPower},
		{"drampowerExcludingIOTermPower", drampowerExcludingIOTermPower},
		{"IOTermPower", ioTermPower},
		{"static", staticPower},
		{"dynamic", dynamicPower}
	}];
  	
  ];

(*for use from notebox to test*)
DRAMPower[archProperties_, stats_] := TotalDRAMPower[predefinedconfigMem[GetKeyValue[archProperties, "DRAMType"]], archProperties, stats];

(* Block 3 
   total power including IO and term power 
*)(* returns power in W *)
TotalDRAMPower[memSpec_, archProperties_, stats_] := (Total[PowerPerRankDevice[memSpec, archProperties, stats]] * NumberOfRankDevices[archProperties]) +  IOTermPower[memSpec, archProperties, stats];
  
(* Block for calculating IO term power 
   Calculated independently for total read and write bytes from DRAM
*)
IOTermPower[memSpec_, archProperties_, stats_] :=
  Module[{readIOEnergy, writeTermEnergy, nReadBytes, nWriteBytes, memFreqMHz, clkPeriod, ddrPeriod, dataRate, dataWidth,
  	ioPower, wrODTPower, dqPlusDqsBits, dqPlusDqsPlusMaskBits},
  	
  	nReadBytes = GetKeyValue[stats, "nReadBytes"];
  	nWriteBytes = GetKeyValue[stats, "nWriteBytes"];
  	
  	memFreqMHz = GetKeyValue[memSpec, "ClkMHz"];
  	clkPeriod  = 1000 / memFreqMHz; (* ns *);
  	dataRate    = GetKeyValue[memSpec, "DataRate"];
  	ddrPeriod = clkPeriod / dataRate;
  	
  	dataWidth   = GetKeyValue[memSpec, "DataWidth"];
  	dqPlusDqsBits = dataWidth + (dataWidth / 8); (* 1 DQS pin is associated with every data byte *)
  	dqPlusDqsPlusMaskBits = dataWidth + (dataWidth / 8) + (dataWidth / 8); (* 1 DQS pin and 1 DM pin are associated with every data byte *)
  	
  	ioPower = GetIOPower[memSpec]; (* mW *)
  	wrODTPower = GetWrODTPower[memSpec];  (* mW *)  	
  	
  	readIOEnergy = nReadBytes * ddrPeriod * ioPower * dqPlusDqsBits; (* pJ *)
	writeTermEnergy = nWriteBytes * ddrPeriod * wrODTPower * dqPlusDqsPlusMaskBits; (* pJ *)  
	
	(* returns power in W *)	
  	Return[((readIOEnergy + writeTermEnergy) / 10^12) * (memFreqMHz * 10^6 / TotalMemoryCycles[memSpec, archProperties, stats])];
  ];

(* TODO: it would nice if it can be calculated somehow *)
GetIOPower[memSpec_] :=
  Module[{memType, ioPower},
  	
  	memType = GetKeyValue[memSpec, "MemoryType"];
  	
  	If[memType == "DDR2", ioPower = 1.5, 
  	   If[memType == "DDR3", ioPower = 4.6, 
          If[memType == "DDR4", ioPower = 3.7, ioPower = 0.0]]]; (* mW *)
  	
  	(* mW *)
  	Return[ioPower];
  ];
  
(* TODO: it would nice if it can be calculated somehow *)
GetWrODTPower[memSpec_] :=
  Module[{memType, wrODTPower},
  	
  	memType = GetKeyValue[memSpec, "MemoryType"];
  	
  	If[memType == "DDR2", wrODTPower = 8.2, 
  	   If[memType == "DDR3", wrODTPower = 21.2,
          If[memType == "DDR4", wrODTPower = 17.0, wrODTPower = 0.0]]]; (* mW *)
  	
  	(* mW *)
  	Return[wrODTPower];
  ];

(* Block 2, Power per rank (without IO Term Power)
*)
PowerPerRankDevice[memSpec_, archProperties_, stats_] :=
  Module[{memFreqMHz, vdd, idd0, idd2n, idd3n, idd4r, idd4w, idd5b,
  	      clkPeriod, tRAS, tRP, tRFC, burstLength, dataRate, devicesPerRank,
  	      tempCmdStats, nACT, nPRE, nRD, nWR, nREF, nPreStdByCycle, nActStdByCycle,
  	      Eread, Ewrite, Eact, Epre, Eref, EactStdBy, EpreStdBy, static, dynamic},
  	      
  	      memFreqMHz = GetKeyValue[memSpec, "ClkMHz"];
  	      clkPeriod  = 1000 / memFreqMHz; (* ns *)
  	      
  	      tRFC = GetKeyValue[memSpec, "RFC"];
  	      tRAS = GetKeyValue[memSpec, "RAS"];
  	      tRP  = GetKeyValue[memSpec, "RP"];
  	      
  	      burstLength = GetKeyValue[memSpec, "BurstLength"];
  	      dataRate    = GetKeyValue[memSpec, "DataRate"];
  	      
  	      vdd   = GetKeyValue[memSpec, "vdd"]; (* V *)
  	      idd0  = GetKeyValue[memSpec, "idd0"];  (* mA *)
  	      idd2n = GetKeyValue[memSpec, "idd2n"]; (* mA *)
  	      idd3n = GetKeyValue[memSpec, "idd3n"]; (* mA *)
  	      idd4r = GetKeyValue[memSpec, "idd4r"]; (* mA *)
  	      idd4w = GetKeyValue[memSpec, "idd4w"]; (* mA *)
  	      idd5b = GetKeyValue[memSpec, "idd5b"]; (* mA *)
  	        
          devicesPerRank   = GetKeyValue[memSpec, "DevicesPerRank"];
  	      
  	      (* tempCmdStats = {nACT, nPRE, nRD, nWR, nREF, nPreStdByCycle, nActStdByCycle, check} *)
  	      tempCmdStats = CmdStatsPerRankDevice[memSpec, archProperties, stats];
  	      nACT = Part[tempCmdStats,1];
  	      nPRE = Part[tempCmdStats,2];
  	      nRD  = Part[tempCmdStats,3];
  	      nWR  = Part[tempCmdStats,4];
  	      nREF = Part[tempCmdStats,5];
  	      nPreStdByCycle = Part[tempCmdStats,6];
  	      nActStdByCycle = Part[tempCmdStats,7];
  	      
  	      Eread     = nRD * vdd * (idd4r - idd3n) * (burstLength / dataRate) * clkPeriod;(* pJ *) 
  	      Ewrite    = nWR * vdd * (idd4w - idd3n) * (burstLength / dataRate) * clkPeriod;(* pJ *)
  	      
  	      Eact      = nACT * vdd * (idd0 - idd3n)  * tRAS * clkPeriod;(* pJ *)
  	      Epre      = nPRE * vdd * (idd0 - idd2n)  * tRP  * clkPeriod;(* pJ *)
  	      Eref      = nREF * vdd * (idd5b - idd3n) * tRFC * clkPeriod;(* pJ *)
  	      
  	      EactStdBy = nActStdByCycle * vdd * idd3n * 1 * clkPeriod;(* pJ *)
  	      EpreStdBy = nPreStdByCycle * vdd * idd2n * 1 * clkPeriod;(* pJ *)
  	         
  	(* returns power in W *)      
  	static = ((EactStdBy + EpreStdBy) * devicesPerRank    
  	       / 10^12) * (memFreqMHz * 10^6 / TotalMemoryCycles[memSpec, archProperties, stats]);
  	       
  	dynamic = ((Eread + Ewrite + Eact + Epre + Eref) * devicesPerRank    
  	       / 10^12) * (memFreqMHz * 10^6 / TotalMemoryCycles[memSpec, archProperties, stats]);
  	       
  	Return[{static, dynamic}];
  	         
  ];

(* Block1 *)
CmdStatsPerRankDevice[memSpec_, archProperties_, stats_] :=
  Module[{MemCycles, nRefCyclesReq, nLeftCycles, nReqRdWrCycles, nActiveCycles, nIdleCycles, nActBackgroundCycleACTIVE,
  	      nPreBackgroundCycleACTIVE, nPreBackgroundCyclePRECHARGE, nActBackgroundCycleREFRESH, nPreBackgroundCycleREFRESH,
  	      tREFI, tRFC, tRAS, tRP,
  	      nACT, nPRE, nRD, nWR, nREF, nPreStdByCycle, nActStdByCycle, check},
  	      
  	      MemCycles = TotalMemoryCycles[memSpec, archProperties, stats]; 
  	      
  	      tREFI = GetKeyValue[memSpec, "REFI"]; 
  	      tRFC = GetKeyValue[memSpec, "RFC"];
  	      tRAS = GetKeyValue[memSpec, "RAS"];
  	      tRP = GetKeyValue[memSpec, "RP"];

  	      nRD = ReadBurstPerRankDevice[memSpec, archProperties, stats];
  	      nWR = WriteBurstPerRankDevice[memSpec, archProperties, stats];  	      
  	      nACT = nRD + nWR;
  	      nPRE = nACT;
  	      nREF = Floor[ MemCycles / tREFI  ];
  	      
  	      nRefCyclesReq = nREF * (tRFC + tRP);
  	      nLeftCycles = MemCycles - nRefCyclesReq;
  	      nReqRdWrCycles = (nRD + nWR) * (tRAS + tRP);
  	      nActiveCycles = If[nLeftCycles > nReqRdWrCycles, nReqRdWrCycles, nLeftCycles];
  	      nIdleCycles = nLeftCycles - nActiveCycles;
  	        
  	      nActBackgroundCycleACTIVE = nActiveCycles * tRAS / (tRAS + tRP); 
  	      nPreBackgroundCycleACTIVE = nActiveCycles * tRP / (tRAS + tRP);
  	      nPreBackgroundCyclePRECHARGE = nIdleCycles;
  	      nActBackgroundCycleREFRESH = nREF * (tRFC - tRP);
  	      nPreBackgroundCycleREFRESH = nREF * (tRP + tRP);
  	       
  	      nPreStdByCycle = nPreBackgroundCycleACTIVE + nPreBackgroundCyclePRECHARGE + nPreBackgroundCycleREFRESH;
  	      nActStdByCycle = nActBackgroundCycleACTIVE + nActBackgroundCycleREFRESH;
  	     
  	      check = If[MemCycles == (nPreStdByCycle + nActStdByCycle), 0, -1]; (* -1 indicates error..TODO add error code*)
  	
  	Return[{nACT, nPRE, nRD, nWR, nREF, nPreStdByCycle, nActStdByCycle, check}];
  ];



(* Block0 *)

TotalMemoryCycles[memSpec_, archProperties_, stats_] :=
  Block[{memFreqMHz, cpuFreqMHz, totalCPUcycles},
   memFreqMHz = GetKeyValue[memSpec, "ClkMHz"];   
   cpuFreqMHz = GetKeyValue[archProperties, "f0"] / 10^6; (* f0 is in Hz *)
   totalCPUcycles = GetKeyValue[stats, "TotalCycles"];
 
   Return[totalCPUcycles * memFreqMHz / cpuFreqMHz];
 
   ];
(* 
DRAMReadTransaction[archProperties_, stats_] :=
  Block[{ReadBytes, CacheLineSize},
   CacheLineSize = GetKeyValue[archMemCPU, "CacheLineSizeBytes"];
   ReadBytes = GetKeyValue[stats, "nReadBytes"];
 
   ret =  ReadBytes / CacheLineSize
   ];
  
DRAMWriteTransaction[archProperties_, stats_] :=
  Block[{WriteBytes, CacheLineSize},
   CacheLineSize = GetKeyValue[archMemCPU, "CacheLineSizeBytes"];
   WriteBytes = GetKeyValue[stats, "nWriteBytes"];
 
   ret =  WriteBytes / CacheLineSize
   ];
*)
NumberOfRankDevices[archProperties_] :=
  Block[{nDIMMs, rankPerDIMM},
   nDIMMs = GetKeyValue[archProperties, "nDIMMs"];
   rankPerDIMM = GetKeyValue[archProperties, "nRanksPerDIMM"];
 
   Return[nDIMMs * rankPerDIMM];
   ];
(*
ReadBurstPerRankDevice[archProperties_, stats_] :=
  Block[{readBytes, cacheLineSize},
   cacheLineSize = GetKeyValue[archProperties, "L3dagran"]; (* L3dagran in bytes *)
   readBytes = GetKeyValue[stats, "nReadBytes"];
    
   ret =  readBytes / cacheLineSize / NumberOfRankDevices[archProperties]
   ];

WriteBurstPerRankDevice[archProperties_, stats_] :=
  Block[{writeBytes, cacheLineSize},
   cacheLineSize = GetKeyValue[archProperties, "L3dagran"]; (* L3dagran in bytes *)
   writeBytes = GetKeyValue[stats, "nWriteBytes"];
    
   ret =  writeBytes / cacheLineSize / NumberOfRankDevices[archProperties]
   ];
*)
ReadBurstPerRankDevice[memSpec_, archProperties_, stats_] :=
  Block[{readBytes, burstSize, devicesPerRank, dataWidth, burstLength},
   burstLength = GetKeyValue[memSpec, "BurstLength"];
   dataWidth   = GetKeyValue[memSpec, "DataWidth"];
   devicesPerRank   = GetKeyValue[memSpec, "DevicesPerRank"];
   burstSize = devicesPerRank * burstLength * dataWidth / 8; (* bytes, 8 bit = 1 byte *)
   readBytes = GetKeyValue[stats, "nReadBytes"];
    
   Return[readBytes / burstSize / NumberOfRankDevices[archProperties]];
   ];

WriteBurstPerRankDevice[memSpec_, archProperties_, stats_] :=
  Block[{writeBytes, burstSize, devicesPerRank, dataWidth, burstLength},
   burstLength = GetKeyValue[memSpec, "BurstLength"];
   dataWidth   = GetKeyValue[memSpec, "DataWidth"];
   devicesPerRank   = GetKeyValue[memSpec, "DevicesPerRank"];
   burstSize = devicesPerRank * burstLength * dataWidth / 8; (* bytes, 8 bit = 1 byte *)
   writeBytes = GetKeyValue[stats, "nWriteBytes"];
    
   Return[writeBytes / burstSize / NumberOfRankDevices[archProperties]];
   ];


End[] (* End Private Context *)

EndPackage[]