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

BeginPackage["ArchitectureProperties`"]
(* Exported symbols added here with SymbolName::usage *)  

Needs["ExaBoundsGeneric`"]

InitArchitectureProperties::usage = 
  "InitArchitectureProperties[]"
  
GetArchitectureDefaultValue::usage = 
  "GetArchitectureDefaultValue[key_]"

Begin["`Private`"] (* Begin Private Context *) 

ExaBoundsRangeDefaultsTXT = "
  yearMIN			1970			Year
  yearMAX			2030			Year
  
  LnodeMIN			5			nm
  LnodeMAX			10000			nm
  
  M0L1MIN			1			Byte
  M0L1MAX			64000000		Byte
  M0L2MIN			1			Byte
  M0L2MAX			64000000		Byte
  M1MIN				1			Byte
  M1MAX				64000000		Byte
  M2MIN				1			Byte
  M2MAX				128000000		Byte
  
  n0MIN				1			Dimensionless
  n0MAX				1024			Dimensionless
  n1MIN				1			Dimensionless
  n1MAX				64			Dimensionless
  n2MIN				1			Dimensionless
  n2MAX				64			Dimensionless
  n3MIN				1			Dimensionless
  n3MAX				64			Dimensionless
  n4MIN				1			Dimensionless
  n4MAX				64			Dimensionless
  n5MIN				1			Dimensionless
  n5MAX				64			Dimensionless
  n6MIN				1			Dimensionless
  n6MAX				64			Dimensionless
  n7MIN				1			Dimensionless
  n7MAX				32			Dimensionless
  
  B0DmoMIN			1			Byte/Second
  B0DmoMAX			32000000000		Byte/Second
  B1DmoMIN			1			Byte/Second
  B1DmoMAX			32000000000		Byte/Second
  B2DmoMIN			1			Byte/Second
  B2DmoMAX			32000000000		Byte/Second
   
  V0MIN				0.1		   	Volt
  V0MAX				5		   	Volt
  
  TMIN				273		   	Kelvin
  TMAX				400		   	Kelvin
  
  f0MIN				100		   	Hertz
  f0MAX				10000000000		Hertz
  f1MIN				100		   	Hertz
  f1MAX				10000000000		Hertz  
  
  fmispredictionMIN		0.00	Dimensionless
  fmispredictionMAX		1.00	Dimensionless	
  
  
  n0threadsMIN			1		Dimensionless
  n0threadsMAX			64		Dimensionless
  n0dispatchMIN			1		Dimensionless
  n0dispatchMAX			64		Dimensionless
  n0fpMIN			1		Dimensionless
  n0fpMAX			1000		Dimensionless
  n0intMIN			1		Dimensionless
  n0intMAX			1000		Dimensionless
  n0controlMIN			1		Dimensionless
  n0controlMAX			1000		Dimensionless
  n0memMIN			1		Dimensionless
  n0memMAX			1000		Dimensionless

  n0BitsMIN			1  	  	Dimensionless
  n0BitsMAX			1000		Dimensionless
  
  n0vectorbitsMIN		1		Bit
  n0vectorbitsMAX		1024		Bit
  n0vectorFUMIN			1		Dimensionless
  n0vectorFUMAX			1024		Dimensionless
  
  L1dagranMIN 			1		   Byte
  L1dagranMAX 			1024		   Byte
  L1dassocMIN 			1		   Dimensionless
  L1dassocMAX 			8192		   Dimensionless
  L1dreusecapMIN 		1		   Byte
  L1dreusecapMAX 		1048576		   Byte
  L1iagranMIN 			1		   Byte
  L1iagranMAX 			1024		   Byte
  L1iassocMIN 			1		   Dimensionless
  L1iassocMAX 			8192		   Dimensionless
  L1ireusecapMIN 		1		   Byte
  L1ireusecapMAX 		1048576		   Byte
  L2dagranMIN 			1		   Byte
  L2dagranMAX 			1024		   Byte
  L2dassocMIN 			1		   Dimensionless
  L2dassocMAX 			8192		   Dimensionless
  L2dreusecapMIN 		1		   Byte
  L2dreusecapMAX 		1073741824	   Byte
  L3dagranMIN 			1		   Byte
  L3dagranMAX 			1024		   Byte
  L3dassocMIN 			1		   Dimensionless
  L3dassocMAX 			8192		   Dimensionless
  L3dreusecapMIN 		1		   Byte
  L3dreusecapMAX 		1073741824	   Byte
  
  n0frontMIN 			1		Dimensionless
  n0frontMAX 			64		Dimensionless
  n0frontpipeMIN 		1		Dimensionless
  n0frontpipeMAX 		64		Dimensionless
  n0pipeMIN 			1		Dimensionless
  n0pipeMAX 			64		Dimensionless
  n0ROBMIN 			1		Dimensionless
  n0ROBMAX 			512		Dimensionless
  n0IQMIN 			1		Dimensionless
  n0IQMAX 			512		Dimensionless
  n0MSHRMIN			1 		Dimensionless
  n0MSHRMAX			512		Dimensionless
  
  T0DRAMlatencyMIN 		1		Cycles
  T0DRAMlatencyMAX 		1000	   	Cycles
  T0L1latencyMIN 		1		Cycles
  T0L1latencyMAX 		50		Cycles
  T0L2latencyMIN 		1		Cycles
  T0L2latencyMAX 		500		Cycles
  T0L3latencyMIN 		1		Cycles
  T0L3latencyMAX 		500		Cycles
  
  TLBagranMIN 			1	   	Byte
  TLBagranMAX 			1024		Byte
  TLBassocMIN 			1		Dimensionless
  TLBassocMAX 			8192		Dimensionless
  TLBreusecapMIN 		1		Byte
  TLBreusecapMAX 		1048576		Byte
  
  T0intopMIN		1			Cycles
  T0intopMAX		100			Cycles
  T0intmulMIN		1			Cycles
  T0intmulMAX		100			Cycles
  T0intdivMIN		1			Cycles
  T0intdivMAX		100			Cycles
  
  T0fpopMIN		1			Cycles
  T0fpopMAX		100			Cycles
  T0fpmulMIN		1			Cycles
  T0fpmulMAX		100			Cycles
  T0fpdivMIN		1			Cycles
  T0fpdivMAX		100			Cycles
  
  T0vectorintopMIN	1			Cycles
  T0vectorintopMAX	100			Cycles
  T0vectorintmulMIN	1			Cycles
  T0vectorintmulMAX	100			Cycles
  T0vectorintdivMIN	1			Cycles
  T0vectorintdivMAX	100			Cycles
  
  T0vectorfpopMIN		1			Cycles
  T0vectorfpopMAX		100			Cycles
  T0vectorfpmulMIN		1			Cycles
  T0vectorfpmulMAX		100			Cycles
  T0vectorfpdivMIN		1			Cycles
  T0vectorfpdivMAX		100			Cycles
  
  Eta0MIN 		0.1		   Dimensionless
  Eta0MAX 		1		   Dimensionless
  Eta1MIN 		0.1		   Dimensionless
  Eta1MAX 		1		   Dimensionless
  Eta2MIN 		0.1		   Dimensionless
  Eta2MAX 		1		   Dimensionless
  Eta3MIN 		0.1		   Dimensionless
  Eta3MAX 		1		   Dimensionless
  Eta4MIN 		0.1		   Dimensionless
  Eta4MAX 		1		   Dimensionless
  Eta5MIN 		0.1		   Dimensionless
  Eta5MAX 		1		   Dimensionless
  Eta6MIN 		0.1		   Dimensionless
  Eta6MAX 		1		   Dimensionless
  Eta7MIN 		0.1		   Dimensionless
  Eta7MAX 		1		   Dimensionless
  
  #Network model

  nodeStackLatencyMIN			0	Seconds
  nodeSwitchLinkLatencyMIN		0	Seconds
  switch1Switch2LinkLatencyMIN		0	Seconds
  switch2Switch3LinkLatencyMIN		0	Seconds
  intraGroupSwitchSwitchLinkLatencyMIN	0	Seconds
  interGroupSwitchSwitchLinkLatencyMIN	0	Seconds
  switchLatencyMIN			0	Seconds
  nodeSwitchBandwidthMIN		0	Byte/Second
  switchSwitchBandwidthMIN		0	Byte/Second
  electricalLinkEnergyPerBitMIN		0	Joule
  opticalLinkEnergyPerBitMIN		0	Joule
  switchLogicEnergyPerBitMIN		0	Joule
  switchStaticPowerMIN			0	Watt

  nodeStackLatencyMAX			1024	Seconds
  nodeSwitchLinkLatencyMAX		1024	Seconds
  switch1Switch2LinkLatencyMAX		1024	Seconds
  switch2Switch3LinkLatencyMAX		1024	Seconds
  intraGroupSwitchSwitchLinkLatencyMAX	1024	Seconds
  interGroupSwitchSwitchLinkLatencyMAX	1024	Seconds
  switchLatencyMAX			1024	Seconds
  nodeSwitchBandwidthMAX		1024000000000	Byte/Second
  switchSwitchBandwidthMAX		1024000000000	Byte/Second
  electricalLinkEnergyPerBitMAX		1	Joule
  opticalLinkEnergyPerBitMAX		1	Joule
  switchLogicEnergyPerBitMAX		1	Joule
  switchStaticPowerMAX			1024	Watt
  
  #MeSAP DRAM Power

  nDIMMsMIN			1
  nRanksPerDIMMMIN		1
  
  nDIMMsMAX			1024
  nRanksPerDIMMMAX		2
  ";
  
ExaBoundsRangeDefaults = {#[[1]], #[[2]]} & /@ Select[ImportString[ExaBoundsRangeDefaultsTXT, "Table"], Length[#] >= 2 &];
  
ExaBoundsHWDefaultsTXT = "
  year			2000
  
  Lnode			32		# Technology node in nm
  
  inorder		False

  maxTLP		False
  SMTmode		1
  
  B0Dmo			1E9		# So that B0dmo*n0IO is 256GB/s, which is the L2 bandwidth on the IBM Power7
  B1Dmo			1E9		# So that B1dmo*n1IO is 128GB/s, which is the L3 bandwidth on the IBM Power7
  B2Dmo			1E9		# So that B2dmo*n2IO is 100GB/s, which is the DRAM bandwidth on the IBM Power7

  T			298		#	Room temperature
  
  V0			1.5		#	1V cores
  
  f0			1E9		#	1GHz
  f1			1E9		#	1GHz
  
  M0L1			128E3		#	128K memory on-core (L1)
  M0L2			128E3		#	128K memory on-core (L2)
  M1			8E6		#	8Mb = 1MB of memory on-die (L3)
  M2			32E9		#	32Gb = 4GB memory per socket
  
  fmisprediction	0.04		# 4% branch misprediction rate, based on Hennesy and Patterson, fifth edition
  
  n0			4		#	Issue width
  n0threads		1		#   Threads per core
  n0dispatch		4		#	Dispatch width
  n0fp			1
  n0int			1
  n0control		1		# BRUs
  n0mem			1		# LSUs	
  n0Bits		32
  n0pipe		10
  n0MSHR		10		# Number of Miss Status Holding Registers (MSHR) (Intel/Nvidia) / Line Fill Buffers (LFB) (Intel) / Load Miss Queue (LMQ) (IBM) (these are not all the same things but they limit the number of hits-under-misses and miss-under-misses_
  
  n0vectorbits			256	Bit
  n0vectorFU			0	Dimensionless		# Default no vector FUs
  
  n1				4
  n2				1
  n3				1
  n4				1
  n5				1
  n6				1
  n7				1
  
  L1dagran			64
  L1dassoc			1
  L1dreusecap			32768
  
  L1iagran			64
  L1iassoc			1
  L1ireusecap			32768
  
  L2dagran			64
  L2dassoc			4
  L2dreusecap			4194304
  
  L3dagran			64
  L3dassoc			4
  L3dreusecap			16777216
  
  TLBagran			64
  TLBassoc			8
  TLBreusecap			32768
  
  n0front			4	# Front-end pipeline capacity
  n0frontpipe			4	# Front-end pipeline length
  n0pipe			5
  n0ROB				128
  n0IQ				32
  
  T0DRAMlatency			400
  T0L1latency			2
  T0L2latency			8
  T0L3latency			25
  
  T0intop			1
  T0intmul			1
  T0intdiv			1
  
  T0fpop			1
  T0fpmul			1
  T0fpdiv			1
  
  T0vectorintop			1
  T0vectorintmul		1
  T0vectorintdiv		1
  
  T0vectorfpop			1
  T0vectorfpmul			1
  T0vectorfpdiv			1
  
  
  Eta0				1
  Eta1				1
  Eta2				1
  Eta3				1
  Eta4				1
  Eta5				1
  Eta6				1
  Eta7				1
  

  #Network model

  networkConfiguration		none
  topologyDescription       	{}
  mappingDescription        	{}
  nodeStackLatency		10
  nodeSwitchLinkLatency		0
  nodeSwitchLinkType		electrical
  switch1Switch2LinkLatency	0
  switch1Switch2LinkType	electrical
  switch2Switch3LinkLatency	0
  switch2Switch3LinkType	electrical
  intraGroupSwitchSwitchLinkLatency	0
  intraGroupSwitchSwitchLinkType	optical
  interGroupSwitchSwitchLinkLatency	0
  interGroupSwitchSwitchLinkType	optical
  switchLatency				0	

  nodeSwitchBandwidth		0
  switchSwitchBandwidth		0
  electricalLinkEnergyPerBit	0
  opticalLinkEnergyPerBit	0
  switchLogicEnergyPerBit	0	
  switchStaticPower		0
  
  #MeSAP DRAM Power

  nDIMMs			4
  nRanksPerDIMM			2
  DRAMType			DDR4-GEM5-CONFIG
  ";
(* GED TODO: Does T = room temperature make sense? *)

ExaBoundsHWDefaults = {#[[1]], #[[2]]} & /@ Select[ImportString[ExaBoundsHWDefaultsTXT, "Table"], Length[#] >= 2 &];

GetArchitectureDefaultValue[key_] := GetKeyValue[ExaBoundsHWDefaults, key];

InitArchMinMax[] :=
Block[{archMinMax,
	B0DmoMIN, B1DmoMIN, B2DmoMIN,
    M0L1MIN, M0L2MIN, M1MIN, M2MIN, TMIN, V0MIN, f0MIN, f1MIN, 
    n0MIN, n0threadsMIN, n0dispatchMIN, n0fpMIN, 
    n0intMIN, n0controlMIN, n0memMIN, n0BitsMIN, n0vectorbitsMIN, n0vectorFUMIN,
    n1MIN, n2MIN, n3MIN, n4MIN, n5MIN, n6MIN, n7MIN, 
    L1dagranMIN, L1dassocMIN, 
    L1dreusecapMIN, L1iagranMIN, L1iassocMIN, L1ireusecapMIN, 
    L2dagranMIN, L2dassocMIN, L2dreusecapMIN, L3dagranMIN, L3dassocMIN, L3dreusecapMIN, n0frontMIN, n0frontpipeMIN, n0pipeMIN, n0MSHRMIN, 
    n0ROBMIN, n0IQMIN, T0DRAMlatencyMIN, T0L1latencyMIN, T0L2latencyMIN, T0L3latencyMIN, TLBagranMIN, 
    TLBassocMIN, TLBreusecapMIN, 
    T0intopMIN, T0intmulMIN, T0intdivMIN, T0fpopMIN, T0fpmulMIN, T0fpdivMIN,
    T0vectorintopMIN, T0vectorintmulMIN, T0vectorintdivMIN, T0vectorfpopMIN, T0vectorfpmulMIN, T0vectorfpdivMIN,
    Eta0MIN, Eta1MIN, Eta2MIN, 
    Eta3MIN, Eta4MIN, Eta5MIN, Eta6MIN, Eta7MIN, yearMIN, LnodeMIN, fmispredictionMIN, nDIMMsMIN, nRanksPerDIMMMIN,
    nodeStackLatencyMIN, nodeSwitchLinkLatencyMIN, switch1Switch2LinkLatencyMIN, switch2Switch3LinkLatencyMIN, switchLatencyMIN,
    intraGroupSwitchSwitchLinkLatencyMIN, interGroupSwitchSwitchLinkLatencyMIN,nodeSwitchBandwidthMIN,
    switchSwitchBandwidthMIN,electricalLinkEnergyPerBitMIN,opticalLinkEnergyPerBitMIN,switchLogicEnergyPerBitMIN,
	switchStaticPowerMIN,

      
    B0DmoMAX, B1DmoMAX, B2DmoMAX,
    M0L1MAX, M0L2MAX, M1MAX, M2MAX, TMAX, V0MAX, f0MAX, f1MAX, 
    n0MAX, n0threadsMAX, n0dispatchMAX, n0fpMAX, 
    n0intMAX, n0controlMAX, n0memMAX, n0BitsMAX, n0vectorbitsMAX, n0vectorFUMAX, 
    n1MAX, n2MAX, n3MAX, n4MAX, n5MAX, n6MAX, n7MAX, 
    L1dagranMAX, L1dassocMAX, 
    L1dreusecapMAX, L1iagranMAX, L1iassocMAX, L1ireusecapMAX, 
    L2dagranMAX, L2dassocMAX, L2dreusecapMAX, L3dagranMAX, L3dassocMAX, L3dreusecapMAX, n0frontMAX, n0frontpipeMAX, n0pipeMAX, n0MSHRMAX, 
    n0ROBMAX, n0IQMAX, T0DRAMlatencyMAX, T0L1latencyMAX, T0L2latencyMAX, T0L3latencyMAX, TLBagranMAX, 
    TLBassocMAX, TLBreusecapMAX, 
    T0intopMAX, T0intmulMAX, T0intdivMAX, T0fpopMAX, T0fpmulMAX, T0fpdivMAX,
    T0vectorintopMAX, T0vectorintmulMAX, T0vectorintdivMAX, T0vectorfpopMAX, T0vectorfpmulMAX, T0vectorfpdivMAX,
    Eta0MAX, Eta1MAX, Eta2MAX, 
    Eta3MAX, Eta4MAX, Eta5MAX, Eta6MAX, Eta7MAX, yearMAX, LnodeMAX, fmispredictionMAX, nDIMMsMAX, nRanksPerDIMMMAX,
    nodeStackLatencyMAX, nodeSwitchLinkLatencyMAX, switch1Switch2LinkLatencyMAX, switch2Switch3LinkLatencyMAX, switchLatencyMAX,
    intraGroupSwitchSwitchLinkLatencyMAX, interGroupSwitchSwitchLinkLatencyMAX,nodeSwitchBandwidthMAX,
    switchSwitchBandwidthMAX,electricalLinkEnergyPerBitMAX,opticalLinkEnergyPerBitMAX,switchLogicEnergyPerBitMAX,
    switchStaticPowerMAX
    },
   (*
   		Parameter value ranges
   *)
   
   	archMinMax = {SymbolName[#], ExaBoundsGetTableValue[ExaBoundsRangeDefaults, SymbolName[#]]} & /@ {B0DmoMIN, B1DmoMIN, B2DmoMIN, 
    								M0L1MIN, M0L2MIN, M1MIN, M2MIN, TMIN, V0MIN, f0MIN, f1MIN, 
    								n0MIN, n0threadsMIN, n0dispatchMIN, n0fpMIN, 
    								n0intMIN, n0controlMIN, n0memMIN, n0BitsMIN, n0vectorbitsMIN, n0vectorFUMIN,
    								n1MIN, n2MIN, n3MIN, n4MIN, n5MIN, n6MIN, n7MIN, 
    								L1dagranMIN, L1dassocMIN, 
    								L1dreusecapMIN, L1iagranMIN, L1iassocMIN, L1ireusecapMIN, 
    								L2dagranMIN, L2dassocMIN, L2dreusecapMIN, L3dagranMIN, L3dassocMIN, L3dreusecapMIN, n0frontMIN, n0frontpipeMIN, n0pipeMIN, n0MSHRMIN, 
    								n0ROBMIN, n0IQMIN, T0DRAMlatencyMIN, T0L1latencyMIN, T0L2latencyMIN, T0L3latencyMIN, TLBagranMIN, 
    								TLBassocMIN, TLBreusecapMIN, 
    								T0intopMIN, T0intmulMIN, T0intdivMIN, T0fpopMIN, T0fpmulMIN, T0fpdivMIN,
    								T0vectorintopMIN, T0vectorintmulMIN, T0vectorintdivMIN, T0vectorfpopMIN, T0vectorfpmulMIN, T0vectorfpdivMIN,
    								Eta0MIN, Eta1MIN, Eta2MIN, 
    								Eta3MIN, Eta4MIN, Eta5MIN, Eta6MIN, Eta7MIN, yearMIN, LnodeMIN, fmispredictionMIN, nDIMMsMIN, nRanksPerDIMMMIN,
    								nodeStackLatencyMIN, nodeSwitchLinkLatencyMIN, switch1Switch2LinkLatencyMIN, switch2Switch3LinkLatencyMIN, switchLatencyMIN,
    								intraGroupSwitchSwitchLinkLatencyMIN, interGroupSwitchSwitchLinkLatencyMIN,
                                     nodeSwitchBandwidthMIN,switchSwitchBandwidthMIN,electricalLinkEnergyPerBitMIN,
                                     opticalLinkEnergyPerBitMIN,switchLogicEnergyPerBitMIN,switchStaticPowerMIN,
    
    
    								B0DmoMAX, B1DmoMAX, B2DmoMAX, 
    								M0L1MAX, M0L2MAX, M1MAX, M2MAX, TMAX, V0MAX, f0MAX, f1MAX, 
    								n0MAX, n0threadsMAX, n0dispatchMAX, n0fpMAX, 
    								n0intMAX, n0controlMAX, n0memMAX, n0BitsMAX, n0vectorbitsMAX, n0vectorFUMAX,
    								n1MAX, n2MAX, n3MAX, n4MAX, n5MAX, n6MAX, n7MAX, 
    								L1dagranMAX, L1dassocMAX, 
    								L1dreusecapMAX, L1iagranMAX, L1iassocMAX, L1ireusecapMAX, 
    								L2dagranMAX, L2dassocMAX, L2dreusecapMAX, L3dagranMAX, L3dassocMAX, L3dreusecapMAX, n0frontMAX, n0frontpipeMAX, n0pipeMAX, n0MSHRMAX, 
    								n0ROBMAX, n0IQMAX, T0DRAMlatencyMAX, T0L1latencyMAX, T0L2latencyMAX, T0L3latencyMAX, TLBagranMAX, 
    								TLBassocMAX, TLBreusecapMAX, 
    								T0intopMAX, T0intmulMAX, T0intdivMAX, T0fpopMAX, T0fpmulMAX, T0fpdivMAX,
    								T0vectorintopMAX, T0vectorintmulMAX, T0vectorintdivMAX, T0vectorfpopMAX, T0vectorfpmulMAX, T0vectorfpdivMAX,
    								Eta0MAX, Eta1MAX, Eta2MAX, 
    								Eta3MAX, Eta4MAX, Eta5MAX, Eta6MAX, Eta7MAX, yearMAX, LnodeMAX, fmispredictionMAX, nDIMMsMAX, nRanksPerDIMMMAX,
    								nodeStackLatencyMAX, nodeSwitchLinkLatencyMAX, switch1Switch2LinkLatencyMAX, switch2Switch3LinkLatencyMAX, switchLatencyMAX,
    								intraGroupSwitchSwitchLinkLatencyMAX, interGroupSwitchSwitchLinkLatencyMAX,nodeSwitchBandwidthMAX,
                                     switchSwitchBandwidthMAX,electricalLinkEnergyPerBitMAX,opticalLinkEnergyPerBitMAX,switchLogicEnergyPerBitMAX,
                                     switchStaticPowerMAX
									};
	Return[archMinMax];
];

InitArchitectureProperties[] :=
  Block[{archState, archMinMax, FunGetDefault,
  	inorder,
  	SMTmode, maxTLP,			(* JRI: execmode and SMTmode added to set different types of parallel execution *)
  	B0Dmo, 
    B1Dmo, B2Dmo, 
    M0L1, M0L2, M1, M2, T, V0, f0, f1, n0, n0threads, n0dispatch, n0fp, 
    n0int, n0control, n0mem, n0Bits, n0vectorbits, n0vectorFU, 
    n1, n2, n3, n4, n5, n6, 
    n7, L1dagran, L1dassoc, L1dreusecap,  
    L1iagran, L1iassoc, L1ireusecap, L2dagran, L2dassoc, L2dreusecap, L3dagran, L3dassoc, L3dreusecap, 
    n0front, n0frontpipe, n0pipe, n0MSHR, n0ROB, n0IQ, T0DRAMlatency, T0L1latency, T0L2latency, T0L3latency, TLBagran, TLBassoc, TLBreusecap,
    T0intop, T0intmul, T0intdiv, T0fpop, T0fpmul, T0fpdiv,
    T0vectorintop, T0vectorintmul, T0vectorintdiv, T0vectorfpop, T0vectorfpmul, T0vectorfpdiv,
    Eta0, Eta1, 
    Eta2, Eta3, Eta4, Eta5, Eta6, Eta7, year, Lnode, 
    fmisprediction, nDIMMs, nRanksPerDIMM, DRAMType,
    networkConfiguration, topologyDescription, mappingDescription, nodeStackLatency, 
    nodeSwitchLinkType, switch1Switch2LinkType, switch2Switch3LinkType,
    nodeSwitchLinkLatency, switch1Switch2LinkLatency, switch2Switch3LinkLatency, switchLatency,
    intraGroupSwitchSwitchLinkType, interGroupSwitchSwitchLinkType,
    intraGroupSwitchSwitchLinkLatency, interGroupSwitchSwitchLinkLatency,
    nodeSwitchBandwidth,switchSwitchBandwidth,electricalLinkEnergyPerBit,
    opticalLinkEnergyPerBit,switchLogicEnergyPerBit,switchStaticPower},
    
    archMinMax = InitArchMinMax[];
    			
    archState = {SymbolName[#], FunGetDefault[SymbolName[#]]} & /@ 	{B0Dmo, B1Dmo, B2Dmo,   
       								M0L1, M0L2, M1, M2, T, V0, f0, f1, n0, n0threads, n0dispatch, n0fp, 
       								n0int, n0control, n0mem, n0Bits, n0vectorbits, n0vectorFU, n1, n2, n3, n4, n5, n6, 
       								n7, L1dagran, L1dassoc, L1dreusecap,  
       								L1iagran, L1iassoc, L1ireusecap, L2dagran, L2dassoc, L2dreusecap, L3dagran, L3dassoc, L3dreusecap, 
       								n0front, n0frontpipe, n0pipe, n0MSHR, n0ROB, n0IQ, T0DRAMlatency, T0L1latency, T0L2latency, T0L3latency, TLBagran, TLBassoc, TLBreusecap,
       								T0intop, T0intmul, T0intdiv, T0fpop, T0fpmul, T0fpdiv,
       								T0vectorintop, T0vectorintmul, T0vectorintdiv, T0vectorfpop, T0vectorfpmul, T0vectorfpdiv,
       								Eta0, Eta1, Eta2, Eta3, Eta4, Eta5, Eta6, Eta7, year, Lnode, 
       								fmisprediction, nDIMMs, nRanksPerDIMM,
								nodeStackLatency, nodeSwitchLinkLatency, switch1Switch2LinkLatency, switch2Switch3LinkLatency, 									switchLatency,intraGroupSwitchSwitchLinkLatency, interGroupSwitchSwitchLinkLatency,
                                        			nodeSwitchBandwidth,switchSwitchBandwidth,electricalLinkEnergyPerBit,
                                        			opticalLinkEnergyPerBit,switchLogicEnergyPerBit,switchStaticPower
									};
		
	FunGetDefault[property_] := Max[GetKeyValue[archMinMax, property<>"MIN"], ExaBoundsGetTableValue[ExaBoundsHWDefaults, property]];
	
	(* Special cases (these don't have a MIN and MAX statement) *)
	inorder = ToExpression[ExaBoundsGetTableValue[ExaBoundsHWDefaults, "inorder"]];
 	SMTmode = ToExpression[ExaBoundsGetTableValue[ExaBoundsHWDefaults, "SMTmode"]];
    	maxTLP = ToExpression[ExaBoundsGetTableValue[ExaBoundsHWDefaults, "maxTLP"]];
    	DRAMType = ExaBoundsGetTableValue[ExaBoundsHWDefaults, "DRAMType"];
   
   	networkConfiguration = ToExpression[ExaBoundsGetTableValue[ExaBoundsHWDefaults, "networkConfiguration"]];
    	topologyDescription = ToExpression[ExaBoundsGetTableValue[ExaBoundsHWDefaults, "topologyDescription"]];
    	mappingDescription = ToExpression[ExaBoundsGetTableValue[ExaBoundsHWDefaults, "mappingDescription"]];
    	nodeSwitchLinkType = ToExpression[ExaBoundsGetTableValue[ExaBoundsHWDefaults, "nodeSwitchLinkType"]];
    	switch1Switch2LinkType = ToExpression[ExaBoundsGetTableValue[ExaBoundsHWDefaults, "switch1Switch2LinkType"]];
    	switch2Switch3LinkType = ToExpression[ExaBoundsGetTableValue[ExaBoundsHWDefaults, "switch2Switch3LinkType"]];
    	intraGroupSwitchSwitchLinkType = ToExpression[ExaBoundsGetTableValue[ExaBoundsHWDefaults, "intraGroupSwitchSwitchLinkType"]];
    	interGroupSwitchSwitchLinkType = ToExpression[ExaBoundsGetTableValue[ExaBoundsHWDefaults, "interGroupSwitchSwitchLinkType"]];
 	   	
    	archState = Join[{{"inorder", inorder}, {"SMTmode", SMTmode}, {"maxTLP", maxTLP}, {"DRAMType", DRAMType},
   			{"networkConfiguration", networkConfiguration}, {"topologyDescription", topologyDescription}, 
                        {"mappingDescription", mappingDescription}, {"nodeSwitchLinkType", nodeSwitchLinkType},
   			{"switch1Switch2LinkType", switch1Switch2LinkType}, {"switch2Switch3LinkType", switch2Switch3LinkType},
   			{"intraGroupSwitchSwitchLinkType", intraGroupSwitchSwitchLinkType}, 
                        {"interGroupSwitchSwitchLinkType", interGroupSwitchSwitchLinkType}}, archState, archMinMax];
	   	
	Return[archState];
  
   ];

End[] (* End Private Context *)

EndPackage[]
