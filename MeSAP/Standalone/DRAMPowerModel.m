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

(* STAND-ALONE MeSAP MODEL: DRAM POWER *)

BeginPackage["DRAMPowerModel`"]
(* Exported symbols added here with SymbolName::usage *)

Needs["ExaBoundsGeneric`"]

(*
TotalMemoryCycles::usage = 
 "TotalMemoryCycles[memSpec_, archMemCPU_, stats_]"

ReadBurstPerRankDevice::usage =
  "ReadBurstPerRankDevice[archMemCPU_, stats_]"

WriteBurstPerRankDevice::usage =
  "WriteBurstPerRankDevice[archMemCPU_, stats_]"
*)
(*
CmdStatsPerRankDevice::usage =
  "CmdStatsPerRankDevice[memSpec_, archMemCPU_, stats_]"
  *)
  
TestRead::usage = 
  "TestRead[memSpec_, archMemCPU_, stats_]"
  


Begin["`Private`"] (* Begin Private Context *)


(* Call this function with a specification from PreDefinedMemorySpec.m, a architecture from PreDefinedMemoryHierarchy.m and a statistics set from PreDefinedStatistics.m *)
TestRead[memSpec_, archMemCPU_, stats_] :=
  Module[{a},
  	a = TotalDRAMPower[memSpec, archMemCPU, stats];
  	ret = a
  ];

(* Block 3 
   total power including IO term power 
*)
TotalDRAMPower[memSpec_, archMemCPU_, stats_] :=
  Module[{},
  	(* returns power in W *)
  	Return[ (PowerPerRankDevice[memSpec, archMemCPU, stats] * NumberOfRankDevices[archMemCPU])
  		    +  IOTermPower[memSpec, archMemCPU, stats]
  	]
  ];
  
(* Block for calculating IO term power 
   Calculated independently for total read/write bytes
*)
IOTermPower[memSpec_, archMemCPU_, stats_] :=
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
  	ret =  (readIOEnergy + writeTermEnergy) / 10^12 * memFreqMHz * 10^6 / TotalMemoryCycles[memSpec, archMemCPU, stats]
  ];

(* TODO: it would nice if it can be calculated somehow *)
GetIOPower[memSpec_] :=
  Module[{memType, ioPower},
  	
  	memType = GetKeyValue[memSpec, "MemoryType"];
  	
  	If[memType == "DDR2", ioPower = 1.5, 
  	   If[memType == "DDR3", ioPower = 4.6, 
          If[memType == "DDR4", ioPower = 3.7, ioPower = 0.0]]]; (* mW *)
  	
  	(* mW *)
  	ret = ioPower
  ];
  
(* TODO: it would nice if it can be calculated somehow *)
GetWrODTPower[memSpec_] :=
  Module[{memType, wrODTPower},
  	
  	memType = GetKeyValue[memSpec, "MemoryType"];
  	
  	If[memType == "DDR2", wrODTPower = 8.2, 
  	   If[memType == "DDR3", wrODTPower = 21.2,
          If[memType == "DDR4", wrODTPower = 17.0, wrODTPower = 0.0]]]; (* mW *)
  	
  	(* mW *)
  	ret = wrODTPower
  ];

(* Block 2 without IO Term Power
   Added as a separate block in the final calculation, if required
*)
PowerPerRankDevice[memSpec_, archMemCPU_, stats_] :=
  Module[{memFreqMHz, vdd, idd0, idd2n, idd3n, idd4r, idd4w, idd5b,
  	      clkPeriod, tRAS, tRP, tRFC, burstLength, dataRate, devicesPerRank,
  	      tempCmdStats, nACT, nPRE, nRD, nWR, nREF, nPreStdByCycle, nActStdByCycle,
  	      Eread, Ewrite, Eact, Epre, Eref, EactStdBy, EpreStdBy},
  	      
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
  	      tempCmdStats = CmdStatsPerRankDevice[memSpec, archMemCPU, stats];
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
  	ret = (Eread + Ewrite + Eact + Epre + Eref + EactStdBy + EpreStdBy) * devicesPerRank    
  	       / 10^12 * memFreqMHz * 10^6 / TotalMemoryCycles[memSpec, archMemCPU, stats]
  	         
  ];

(* Block1 *)
CmdStatsPerRankDevice[memSpec_, archMemCPU_, stats_] :=
  Module[{MemCycles, nRefCyclesReq, nLeftCycles, nReqRdWrCycles, nActiveCycles, nIdleCycles, nActBackgroundCycleACTIVE,
  	      nPreBackgroundCycleACTIVE, nPreBackgroundCyclePRECHARGE, nActBackgroundCycleREFRESH, nPreBackgroundCycleREFRESH,
  	      tREFI, tRFC, tRAS, tRP,
  	      nACT, nPRE, nRD, nWR, nREF, nPreStdByCycle, nActStdByCycle, check},
  	      
  	      MemCycles = TotalMemoryCycles[memSpec, archMemCPU, stats]; 
  	      
  	      tREFI = GetKeyValue[memSpec, "REFI"]; 
  	      tRFC  = GetKeyValue[memSpec, "RFC"];
  	      tRAS  = GetKeyValue[memSpec, "RAS"];
  	      tRP   = GetKeyValue[memSpec, "RP"];

  	      nRD = ReadBurstPerRankDevice[archMemCPU, memSpec, stats];
  	      nWR = WriteBurstPerRankDevice[archMemCPU, memSpec, stats];  	      
  	      nACT = nRD + nWR;
  	      nPRE = nACT;
  	      nREF = Floor[MemCycles / tREFI];
  	      
  	      nRefCyclesReq = nREF * (tRFC + tRP);
  	      nLeftCycles = MemCycles - nRefCyclesReq;
  	      nReqRdWrCycles = (nRD + nWR) * (tRAS + tRP);
  	      nActiveCycles = If[nLeftCycles > nReqRdWrCycles, nReqRdWrCycles, nLeftCycles];
  	      nIdleCycles = nLeftCycles - nActiveCycles;
  	        
  	      nActBackgroundCycleACTIVE = nActiveCycles * tRAS / (tRAS + tRP); 
  	      nPreBackgroundCycleACTIVE = nActiveCycles * tRP  / (tRAS + tRP);
  	      nPreBackgroundCyclePRECHARGE = nIdleCycles;
  	      nActBackgroundCycleREFRESH = nREF * (tRFC - tRP);
  	      nPreBackgroundCycleREFRESH = nREF * (tRP  + tRP);
  	       
  	      nPreStdByCycle = nPreBackgroundCycleACTIVE + nPreBackgroundCyclePRECHARGE + nPreBackgroundCycleREFRESH;
  	      nActStdByCycle = nActBackgroundCycleACTIVE + nActBackgroundCycleREFRESH;
  	     
  	      check = If[MemCycles == (nPreStdByCycle + nActStdByCycle), 0, -1]; (* -1 indicates error..TODO add error code*)
  	
  	ret = {nACT, nPRE, nRD, nWR, nREF, nPreStdByCycle, nActStdByCycle, check}
  ];



(* Block0 *)

TotalMemoryCycles[memSpec_, archMemCPU_, stats_] :=
  Block[{memFreqMHz, cpuFreqMHz, totalCPUcycles},
   memFreqMHz = GetKeyValue[memSpec, "ClkMHz"];
   cpuFreqMHz = GetKeyValue[archMemCPU, "CpuFreqMHz"];
   totalCPUcycles = GetKeyValue[stats, "TotalCycles"];
 
   ret = totalCPUcycles * memFreqMHz / cpuFreqMHz
 
   ];
(* 
DRAMReadTransaction[archMemCPU_, stats_] :=
  Block[{ReadBytes, CacheLineSize},
   CacheLineSize = GetKeyValue[archMemCPU, "CacheLineSizeBytes"];
   ReadBytes = GetKeyValue[stats, "nReadBytes"];
 
   ret =  ReadBytes / CacheLineSize
   ];
  
DRAMWriteTransaction[archMemCPU_, stats_] :=
  Block[{WriteBytes, CacheLineSize},
   CacheLineSize = GetKeyValue[archMemCPU, "CacheLineSizeBytes"];
   WriteBytes = GetKeyValue[stats, "nWriteBytes"];
 
   ret =  WriteBytes / CacheLineSize
   ];
*)
NumberOfRankDevices[archMemCPU_] :=
  Block[{nDIMMs, rankPerDIMM},
   nDIMMs = GetKeyValue[archMemCPU, "nDIMMs"];
   rankPerDIMM = GetKeyValue[archMemCPU, "nRanksPerDIMM"];
 
   ret =  nDIMMs * rankPerDIMM
   ];
(*
ReadBurstPerRankDevice[archMemCPU_, stats_] :=
  Block[{readBytes, cacheLineSize},
   cacheLineSize = GetKeyValue[archMemCPU, "CacheLineSizeBytes"];
   readBytes = GetKeyValue[stats, "nReadBytes"];
    
   ret =  readBytes / cacheLineSize / NumberOfRankDevices[archMemCPU]
   ];

WriteBurstPerRankDevice[archMemCPU_, stats_] :=
  Block[{writeBytes, cacheLineSize},
   cacheLineSize = GetKeyValue[archMemCPU, "CacheLineSizeBytes"];
   writeBytes = GetKeyValue[stats, "nWriteBytes"];
    
   ret =  writeBytes / cacheLineSize / NumberOfRankDevices[archMemCPU]
   ];
*)
ReadBurstPerRankDevice[archMemCPU_, memSpec_, stats_] :=
  Block[{readBytes, burstSize, devicesPerRank, dataWidth, burstLength},
   burstLength = GetKeyValue[memSpec, "BurstLength"];
   dataWidth   = GetKeyValue[memSpec, "DataWidth"];
   devicesPerRank   = GetKeyValue[memSpec, "DevicesPerRank"];
   burstSize = devicesPerRank * burstLength * dataWidth / 8; (* bytes, 8 bit = 1 byte *)
   readBytes = GetKeyValue[stats, "nReadBytes"];
    
   ret =  readBytes / burstSize / NumberOfRankDevices[archMemCPU]
   ];

WriteBurstPerRankDevice[archMemCPU_, memSpec_, stats_] :=
  Block[{writeBytes, burstSize, devicesPerRank, dataWidth, burstLength},
   burstLength = GetKeyValue[memSpec, "BurstLength"];
   dataWidth   = GetKeyValue[memSpec, "DataWidth"];
   devicesPerRank   = GetKeyValue[memSpec, "DevicesPerRank"];
   burstSize = devicesPerRank * burstLength * dataWidth / 8; (* bytes, 8 bit = 1 byte *)
   writeBytes = GetKeyValue[stats, "nWriteBytes"];
    
   ret =  writeBytes / burstSize / NumberOfRankDevices[archMemCPU]
   ];



End[] (* End Private Context *)

EndPackage[]