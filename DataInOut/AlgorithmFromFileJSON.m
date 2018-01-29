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

(* This package contains functions to load algorithm properties from JSON files
   generated by the application characterization framework *)

BeginPackage["AlgorithmFromFileJSON`"]

Needs["ExaBoundsGeneric`"];
Needs["AlgorithmProperties`"];  
Needs["InstructionMix`"];

(* Exported symbols added here with SymbolName::usage *)
LoadAlgorithmJSON::usage =
  "LoadAlgorithmJSON[ fileName_ ], returns key-value list"

Begin["`Private`"] (* Begin Private Context *) 

Options[LoadAlgorithmJSON] = {"D0dreuseTag"->"dataCDF"(* either "data" or "dataCDF" *), "ILP0WindowSize"->54};
LoadAlgorithmJSON[ fileName_, OptionsPattern[] ] :=
Module[{dataRaw, dataIn, threadData, keyValueLists,appName,scenario,threadN,ret, defaultProperties,
	D0dreuseTag, ILP0WindowSize,
	missingProperties, tryRead, iDs, maxPid},
  If[ !FileExistsQ[ fileName ],
    MessageDialog[ "File " <> fileName <> " does not exist" ];
    Return[Null]
  ];
  (* get options *)
  D0dreuseTag = OptionValue["D0dreuseTag"];
  ILP0WindowSize = OptionValue["ILP0WindowSize"];
  
  (* import JSON data and drop root node, "swProperties" *)
  dataRaw = Check[Import[fileName,"JSON"], Message[LoadJSON::invalidjson, file]; Return[Null]];
  dataIn = Association[dataRaw];
  appName = dataIn["application"];
  If[!KeyExistsQ[dataIn,"appScale"],
  	scenario = Association[];
  	,
  	scenario = Association[SortBy[dataIn["appScale"],Left]];
  ];
  threadN = Length[dataIn["threads"]];
  threadData = dataIn["threads"];
  
  defaultProperties = GetDefaultAlgorithmProperties[];
  
  
  (* error management initialization *)
  missingProperties = {};
  tryRead[readStatement_,metricName_] :=
  		Quiet[Check[
				ReleaseHold[readStatement],
				missingProperties = Union[missingProperties,{metricName}];
				{} (* Return value in case of warnings *)
		]];
  
  (* Actual read *)
  keyValueLists = 
    Join[
      tryRead[Hold[ReadInstructionMix[threadData[[#]]]],"instructionMix"],
        
        tryRead[Hold[{{"D0dreuse", ReadDataReuseDistribution[threadData[[#]], D0dreuseTag ,64]}}],"dataReuseDistribution (64 byte)"],
        tryRead[Hold[{{"D0dreuse128", ReadDataReuseDistribution[threadData[[#]], D0dreuseTag ,128]}}],"dataReuseDistribution  (128 byte)"],
        tryRead[Hold[{{"D0ireuse", ReadInstrReuseDistribution[threadData[[#]] ]}}],"instrReuseDistribution"],
		tryRead[Hold[ReadMemoryFootprint[threadData[[#]]]],"memoryFootprint"],
        tryRead[Hold[ReadBranchEntropies[threadData[[#]]]],"branchEntropy"],
        tryRead[Hold[ReadBestBranchMispredictionRate[threadData[[#]]]],"bestMisprectionRate"], 
		tryRead[Hold[ReadILP0[threadData[[#]], "ILP0WindowSize" -> ILP0WindowSize]],"ilp"],
		tryRead[Hold[ReadILPType[threadData[[#]], "ILP0WindowSize" -> ILP0WindowSize]],"ilptype"],
		tryRead[Hold[ReadInorderILP0[threadData[[#]], "ILP0WindowSize" -> ILP0WindowSize]],"inorder ilp"],
		tryRead[Hold[ReadInorderILPType[threadData[[#]], "ILP0WindowSize" -> ILP0WindowSize]],"inorder ilptype"],
		tryRead[Hold[ReadRegisterAccesses[threadData[[#]]]],"registerAccesses"],
		tryRead[Hold[ReadOpenMPinstructionMix[threadData[[#]]]],"openMPinstructionMix"],
		tryRead[Hold[ReadMPIinstructionMix[threadData[[#]]]],"mpiInstructionMix"],
		tryRead[Hold[ReadMPIdataExchanged[threadData[[#]]]],"mpiDataExchanged"],
		tryRead[Hold[ReadMPIcollectiveDataExchanged[threadData[[#]]]],"mpiDataExchanged (collective)"],
		tryRead[Hold[ReadMPIcommVector[threadData[[#]]]],"mpiCommunicationVector"],
		tryRead[Hold[ReadLibraryCalls[threadData[[#]]]],"externalLibraryCallCount"]
    ]& /@ Range[threadN];
    
  iDs = {	tryRead[Hold[ReadProcessId[threadData[[#]]]],"processId"],
    		tryRead[Hold[ReadThreadId[threadData[[#]]]],"threadId"]}
    & /@ Range[threadN];
  
  If[missingProperties =!= {},(* Check for errors *)
  	If[Length[missingProperties]>1,
  		Message[LoadAlgorithmJSON::missingproperties, StringJoin[Riffle[missingProperties,", "]], fileName];
  		(*Print["The properties " <> "{"<> StringJoin[Riffle[missingProperties,", "]] <> "}" <> " were not read correctly." ];*)
  		,
  		Message[LoadAlgorithmJSON::missingproperty, StringJoin[Riffle[missingProperties,", "]], fileName];
  		(*Print["The property " <> "\""<> StringJoin[Riffle[missingProperties,", "]] <> "\"" <> " was not read correctly." ];*)
  	];
  ];
  
  (* Fill in missing defaults. NOTE: TLP analysis omitted, which was used as ALP1 by Phillip, but that is likely wrong *)
  keyValueLists = ExaBoundsMergeState[defaultProperties, #] &/@keyValueLists;
     
  (* Additional instruction mix test *)
  TestInstrMix[#] & /@ keyValueLists;
  
  (*
  	Relative communication ids should all be positive.
  	Otherwise in a 2D tourus, the first line and the last line will be placed
  	on 2 different communication groups.
  *)
  maxPid = Max[ iDs[[All,1]] ];
  AdjusteRelativeCommunicationIds[keyValueLists[[#]],maxPid] & /@ Range[Length[keyValueLists] ];
  
  (* Organize the overall structure *)
	ret = Association[{
						"scalingParameters"-> Keys[scenario],
						scenario -> 
									{(* List for different executions over the same scaling parameter
										to store content-related variability *)
										Association[(iDs[[#]](*"thread"<>ToString[#]*) -> keyValueLists[[#]] & /@ Range[threadN])]
									}
					}];
	ret = Association[{appName -> ret}];
	
  Return[ret];
];
(* Warnings: *)
LoadAlgorithmJSON::missingproperties = "The properties {`1`} were not read correctly for the file: `2`."
LoadAlgorithmJSON::missingproperty = "The property \"`1`\" was not read correctly for the file: `2`."

ReadProcessId[threadData_] := Quiet[Check[(* including in the addr instructions for scalar and undefined data-size *)
   	ToExpression[OptionValue[threadData, "processId"]]
   	,
   	0
  ]];
  
ReadThreadId[threadData_] := Quiet[Check[(* including in the addr instructions for scalar and undefined data-size *)
   	ToExpression[OptionValue[threadData, "threadId"]]
   	,
   	0
  ]];

ReadInstructionMix[threadData_] :=
Module[{getVectorInstructionMix,getScalarInstructionCount, totalInstructions,res,missingProperties={},labels,
		load, store, mem, addr, int, intOnly, intMul, intDiv, dpfp, fpMul, fpDiv, control, vectorExtractInsert, other, otherScalar, conversion, bitwise },
  getScalarInstructionCount[tag_] := (* shorthand *)
  Quiet[Check[(* including in the addr instructions for scalar and undefined data-size *)
   	ToExpression[OptionValue[threadData, "instructionMix"->tag]]
   	,
	missingProperties = Union[missingProperties,{tag}];
   	0.
  ]];
  
  getVectorInstructionMix[tag_] := (* shorthand *)
  Module[{vectorMix,pisaMixData,pisaVectorData,vl,dl,count,dataLengthInfo,e,miscCount,totCount},
    Quiet[Check[
    	pisaMixData = OptionValue[threadData, "instructionMix"->tag->"mix"];
    	miscCount = OptionValue[threadData, "instructionMix"->tag->"misc_instructions"] / totalInstructions;
    	totCount = OptionValue[threadData, "instructionMix"->tag->"total_instructions"] / totalInstructions;
    	(* Gather data *)
    	
    	vectorMix = Reap[Do[
    		vl = OptionValue[pisaVectorData,"vector_size"];
    		dataLengthInfo = Reap[Do[
    			(* This includes a data length information *)
    			If[StringContainsQ[First[e],"bits"],(* data length known *)
    				count = Last[e];
    				dl = First[e];
    				dl = StringReplace[dl,RegularExpression["[a-z]"]->""];
    				dl = StringReplace[dl,RegularExpression["[A-Z]"]->""];
    				dl = StringReplace[dl,"_"->""];
    				dl = ToExpression[dl];
    				
    				Sow[{dl,count/totalInstructions}];
    				
    				,
    				If[StringContainsQ[First[e],"misc"] || StringContainsQ[First[e],"type"],(* data length unknown *)
    					count = Last[e];
    					dl=0;
    					Sow[{dl,count/totalInstructions}];
    				];
    			];
    			
    		,{e,pisaVectorData}]][[2,1]];
    		
    		Sow[{vl,dataLengthInfo}];
    	,{pisaVectorData,pisaMixData}]][[2,1]];
    	
    	VMixCanonize[vectorMix];
    	
    	vectorMix[[1,2,1,2]] = vectorMix[[1,2,1,2]] + miscCount; (* Accumulating in the ferst vector length, first data length *)
    	If[Total[VMixFlatten[vectorMix]]<totCount ,(* TODO: FIXME: miscCount for some types in PISA is not correct, one need to take the total_instructions and subtract all the other ones *)
    		vectorMix[[1,2,1,2]] = vectorMix[[1,2,1,2]] + totCount - Total[VMixFlatten[vectorMix]];  
    	];
    	vectorMix
    	
    	
    	,
		missingProperties = Union[missingProperties,{tag}];
		vectorMix = {};
    	VMixCanonize[vectorMix];
    	vectorMix
    ]]
  ];
  
  totalInstructions = getScalarInstructionCount["instructions_analyzed"];
  
  (* memory instructions *)
  load = getVectorInstructionMix["load_instructions"];
  store = getVectorInstructionMix["store_instructions"];
  mem = VMixAccumulate[{load,store}];
  
  addr = getVectorInstructionMix["address_arith_get_elem_ptr_arith_instructions"];
  addr[[1,2,1,2]](* vectorLengt:1 dataLengt:? *) =
  					addr[[1,2,1,2]] + getScalarInstructionCount["address_arith_alloca_arith_instructions"]/totalInstructions; 
  
  (* integer instructions *)
  intMul = getVectorInstructionMix["int_arith_mul_instructions"];
  labels = { "int_arith_udiv_instructions","int_arith_sdiv_instructions"
 			,"int_arith_urem_instructions","int_arith_srem_instructions"};
  intDiv = VMixAccumulate[getVectorInstructionMix[#]&/@labels];
  
  intOnly = VMixAccumulate[{getVectorInstructionMix["int_arith_add_instructions"],
  							getVectorInstructionMix["int_arith_sub_instructions"],
  							intMul,intDiv
  							}];
  labels = { "conversion_trunc_instructions","conversion_zext_instructions"
 			,"conversion_sext_instructions","conversion_fptrunc_instructions"
 			,"conversion_fpext_instructions","conversion_fptoui_instructions"
 			,"conversion_fptosi_instructions","conversion_uitofp_instructions"
 			,"conversion_sitofp_instructions","conversion_inttoptr_instructions"
 			,"conversion_ptrtoint_instructions","conversion_bitcast_instructions"
 			,"conversion_address_space_cast_instructions"};
  conversion = VMixAccumulate[getVectorInstructionMix[#]&/@labels];
  labels = { "bitwise_and_instructions", "bitwise_or_instructions"
  			,"bitwise_xor_instructions","bitwise_shift_left_instructions"
  			,"bitwise_logical_shift_right_instructions","bitwise_arith_shift_right_instructions"};
  bitwise = VMixAccumulate[getVectorInstructionMix[#]&/@labels];
  int = VMixAccumulate[{intOnly
					  	,bitwise 
					  	,getVectorInstructionMix["int_cmp_instructions"] 
					  	,conversion 
					  	}];
  
  (* float instructions *)
  
  fpMul = getVectorInstructionMix["fp_arith_mul_instructions"];
  labels = {"fp_arith_div_instructions","fp_arith_rem_instructions"};
  fpDiv = VMixAccumulate[getVectorInstructionMix[#]&/@labels];
  dpfp = VMixAccumulate[{getVectorInstructionMix["fp_arith_add_instructions"]
  						,getVectorInstructionMix["fp_arith_sub_instructions"]
  						,getVectorInstructionMix["fp_cmp_instructions"]
  						,fpMul,fpDiv
  						}];
  						
  (* control from scalar *)
  control = {};
  VMixCanonize[control];
    	
  control[[1,2,1,2]] (* vectorLengt:1 dataLengt:? *) = control[[1,2,1,2]] +
  					getScalarInstructionCount["control_instructions"]/totalInstructions;
  
  (* vectorExtractInsert *)
  labels = {"llvm_vector_extract_element_instructions","llvm_vector_insert_element_instructions"
  			,"llvm_vector_shuffle_vector_instructions"};
  vectorExtractInsert = VMixAccumulate[getVectorInstructionMix[#]&/@labels];
  
  (* other *)
  labels = {"atomic_memory_instructions", "phi_instructions" ,"sync_instructions" 
			, "misc_landingpad_instructions","misc_va_arg_instructions"
  			};
  
  otherScalar = Total[getScalarInstructionCount[#]&/@labels/totalInstructions];
  
  labels = {"aggregate_extract_value_instructions", 
  			"aggregate_insert_value_instructions",
  			"misc_select_instructions"
  			};
  other = VMixAccumulate[getVectorInstructionMix[#]&/@labels];
  other[[1,2,1,2]] (* vectorLengt:1 dataLengt:? *) = other[[1,2,1,2]] + otherScalar;
  
  
  
  
  (* organize results *)
  res ={
 		{"LSys", totalInstructions},
		{"F0load", load},
		{"F0store", store},
		{"F0mem", mem},
		{"F0addr", addr},
		{"F0int", int},
		{"F0intOnly", intOnly},
 		{"F0intmul", intMul},
 		{"F0intdiv", intDiv},
		{"F0fp", dpfp},
 		{"F0fpmul", fpMul},
 		{"F0fpdiv", fpDiv},
		{"F0control", control},
		{"F0vector", vectorExtractInsert},
		{"F0other", other}
  }; 
  
  (* TODO: here we would need to send a Message if missingProperties =!= {}.
  		However this would let the raised here to be managed in LoadAlgorithmJSON
  		removing the partially loaded data *)
  
  (* Convert data to ExaBounds algorithm properties *)
  Return[res];
]

ReadDataReuseDistribution[threadData_, D0dreuseTag_, CacheLineSize_Integer] :=
Block[{reuseData,res,dMin,dMax,required,requiredIter,iter,nextVal,prevVal},
	reuseData = OptionValue[threadData, "dataReuseDistribution"];
	
	res = SelectFirst[reuseData, (OptionValue[#, "cacheLineSize"] == ToString[CacheLineSize] || OptionValue[#, "cacheLineSize"] == CacheLineSize) &];
	If[MissingQ[res], Message[ReadDataReuseDistribution::novalidcdf, CacheLineSize]];
	res = ToExpression[ OptionValue[res, "statistics" -> D0dreuseTag]];
		
	(* Fit in the missing values and remove the repeating ones. Assume sorted vector. *)
	dMin = res[[1,1]];(*Max[Min[available],1];*)
	dMax = res[[-1,1]];(*Max[available];*)
	
	required = 2^# & /@ Range[Ceiling[Log2[Max[1,dMin]]],Ceiling[Log2[Max[1,dMax]]]];
	res = Reap[For[iter=1,iter<=Length[res],iter++,
		Which[
			iter==1,
			(* Initialization *)
			requiredIter = 1;
			Sow[res[[iter]]];
			prevVal = res[[iter,2]];
			If[required[[requiredIter]] <= res[[iter,1]],
				requiredIter++;
			];
			
			,
			iter==Length[res],
			(* Termination *)
			Sow[res[[iter]]];
			
			,
			True,
			(* Search *)
			Which[
				required[[requiredIter]] == res[[iter,1]],(* This has to be sowed *)
				requiredIter++;
				prevVal = res[[iter,2]];
				Sow[res[[iter]]];
				Continue[];(* Do not process this element further, it is sowed already *)
				
				,
				required[[requiredIter]] < res[[iter,1]],(* A mandatory one has to be added. *)
				Sow[{required[[requiredIter]],prevVal}];
				requiredIter++;
				(* process further the current item we may need to sow it as well. *)
			];
			
			nextVal = res[[iter+1,2]];
			If[!(nextVal==res[[iter,2]] && prevVal == res[[iter,2]]),
				Sow[res[[iter]]];
				prevVal = res[[iter,2]];
			];
		];
	]][[2,1]];
	
	Return[res];
];
ReadDataReuseDistribution::novalidcdf = "No valid data reuse CDF found for cache line size `1`B"
	
ReadInstrReuseDistribution[threadData_] :=
Block[{reuseInstr},
	reuseInstr = OptionValue[threadData, "instReuseDistribution"];

	If[Head[reuseInstr[[1]]]===List,(* In some JSON file the analysis is carried out for a single value *)
		(* TODO: make more robust, cache-line-size is not necessary at #[[1]] *)
		Return[ToExpression[ OptionValue[threadData, "instReuseDistribution" -> "statistics" -> "instructionsCDF" ]] ];
		,
		Return[ToExpression[ OptionValue[threadData, "instReuseDistribution" -> "statistics" -> "instructionsCDF"]]];
	];
];

ReadBestBranchMispredictionRate[threadData_] :=
Block[{bestMispredictionRate},
	bestMispredictionRate = OptionValue[threadData, "branchEntropy" -> "bestMispredictionRate"];
	
	Return[{{"F0bestMispredict", ToExpression[bestMispredictionRate]}}];
];

ReadBranchEntropies[threadData_] :=
Block[{branchData},	
	branchData = {#[[1,2]], #[[2,2]]} & /@ OptionValue[threadData, "branchEntropy" -> "entropies"];
	
	Return[{{"H0branch", ToExpression[branchData]}}];
];

Options[ReadILPType] = {"ILP0WindowSize" -> 54};
ReadILPType[threadData_, OptionsPattern[]] :=
Block[{ilpData, requestedWindowSize, res},
	requestedWindowSize = OptionValue["ILP0WindowSize"];
	
	ilpData = OptionValue[threadData, "ilp"];
	
	res = SelectFirst[ilpData, (OptionValue[#, "windowSize"] == ToString[requestedWindowSize]) || (OptionValue[#, "windowSize"] == requestedWindowSize) &];
	
	If[MissingQ[res], (* If we cannot find the requested size, we just select the first one *)
		res = ilpData[[1]];
	];
	Return[{
		{"ILP0mem", ToExpression[OptionValue[res, "statistics"->"arithmetic_mean_mem"]]},
	  	{"ILP0int", ToExpression[OptionValue[res, "statistics"->"arithmetic_mean_int"]]},
	  	{"ILP0control", ToExpression[OptionValue[res, "statistics"->"arithmetic_mean_ctrl"]]},
	  	{"ILP0fp", ToExpression[OptionValue[res, "statistics"->"arithmetic_mean_fp"]]}
	}];
];

Options[ReadInorderILPType] = {"ILP0WindowSize" -> 54};
ReadInorderILPType[threadData_, OptionsPattern[]] :=
Block[{ilpData, requestedWindowSize, res},
	requestedWindowSize = OptionValue["ILP0WindowSize"];
	
	ilpData = OptionValue[threadData, "ilp"];
	
	res = SelectFirst[ilpData, (OptionValue[#, "windowSize"] == ToString[requestedWindowSize]) || (OptionValue[#, "windowSize"] == requestedWindowSize) &];
	
	If[MissingQ[res], (* If we cannot find the requested size, we just select the first one *)
		res = ilpData[[1]];
	];
	Return[{
		{"ILP0mem", ToExpression[OptionValue[res, "in-order"->"arithmetic_mean_mem"]]},
	  	{"ILP0int", ToExpression[OptionValue[res, "in-order"->"arithmetic_mean_int"]]},
	  	{"ILP0control", ToExpression[OptionValue[res, "in-order"->"arithmetic_mean_ctrl"]]},
	  	{"ILP0fp", ToExpression[OptionValue[res, "in-order"->"arithmetic_mean_fp"]]}
	}];
];

Options[ReadILP0] = {"ILP0WindowSize" -> 54};
ReadILP0[threadData_, OptionsPattern[]] :=
Block[{ilpData,requestedWindowSize,loadedWindowSize, res},
	requestedWindowSize = OptionValue["ILP0WindowSize"];
	
	ilpData = OptionValue[threadData, "ilp"];
	
	res = SelectFirst[ilpData, (OptionValue[#, "windowSize"] == ToString[requestedWindowSize]) || (OptionValue[#, "windowSize"] == requestedWindowSize) &];

	If[MissingQ[res], (* If we cannot find the requested size, we just select the first one *)
		res = ilpData[[1]];
		loadedWindowSize = ToExpression[OptionValue[res,  "windowSize"]]; (* ToExpression in case it is a string *)
		,
		loadedWindowSize = requestedWindowSize
	];
	Return[{
		{"ILP0", ToExpression[OptionValue[res, "statistics" -> "arithmetic_mean"]]},
		{"ILP0WindowSize", loadedWindowSize}
	}];	
];

Options[ReadInorderILP0] = {"ILP0WindowSize" -> 54};
ReadInorderILP0[threadData_, OptionsPattern[]] :=
Block[{ilpData,requestedWindowSize, res},
	requestedWindowSize = OptionValue["ILP0WindowSize"];
	
	ilpData = OptionValue[threadData, "ilp"];
	
	res = SelectFirst[ilpData, (OptionValue[#, "windowSize"] == ToString[requestedWindowSize]) || (OptionValue[#, "windowSize"] == requestedWindowSize) &];
	If[MissingQ[res], (* If we cannot find the requested size, we just select the first one *)
		res = ilpData[[1]];
	];
	Return[{
		{"ILP0", ToExpression[OptionValue[res, "in-order" -> "arithmetic_mean"]]} (* Do not return the loaded size as that is set for ILP0 *)
	}];	
];

ReadRegisterAccesses[threadData_] :=
Module[{getRegisterAccesses, totalInstructions},
  getRegisterAccesses[tag_] :=
    ToExpression[OptionValue[threadData, "registerAccesses" -> tag]];
  totalInstructions = ToExpression[OptionValue[threadData, "instructionMix"->"instructions_analyzed"]];
  
  Return[{
    {"F0regreads", getRegisterAccesses["reads"] / totalInstructions},
    {"F0regwrites", getRegisterAccesses["writes"] / totalInstructions}
  }]
]

ReadOpenMPinstructionMix[threadData_] :=
Module[{categories={"openMP_synchronization","openMP_atomic","openMP_startup_shutdown",
					"openMP_parallel","openMP_thread_info","openMP_work_sharing",
					"openMP_thread_private_data_support","openMP_tasking_support",
					"openMP_others"},
		getOpenMPCount, result},
		
	getOpenMPCount[tag_] :=
		ToExpression[OptionValue[threadData, "openMPinstructionMix" -> tag]];
		
	result = {#,getOpenMPCount[#]} & /@ categories;
	PrependTo[result,{"openMP_total",Total[result[[All,2]]]}];
	result[[All,1]] = StringReplace[#,{"openMP_"->"OpenMP","_s"->"S","_i"->"I","_p"->"P"
		,"_d"->"D"}] & /@result[[All,1]];
	Return[result];
];

ReadMPIinstructionMix[threadData_] :=
Module[{getMPIStat,isend,irecv,send,recv,barrier,
	bcast,reduce,allreduce,gather,allgather,alltoall,instructions},
  getMPIStat[superTag_,tag_] :=
    ToExpression[OptionValue[threadData, "mpiInstructionMix" -> superTag -> tag]];
    
    instructions = ToExpression[OptionValue[threadData,"mpiInstructionMix"->"MPI_instructions_analyzed"]];
    
    isend = getMPIStat["p2pCommunicationRoutines","MPI_Isend"];
    irecv = getMPIStat["p2pCommunicationRoutines","MPI_Irecv"];
    send = getMPIStat["p2pCommunicationRoutines","MPI_Send"];
    recv = getMPIStat["p2pCommunicationRoutines","MPI_Recv"];
    barrier = getMPIStat["collectiveCommunicationRoutines","MPI_Barrier"];
    bcast = getMPIStat["collectiveCommunicationRoutines","MPI_Bcast"];
    reduce = getMPIStat["collectiveCommunicationRoutines","MPI_Reduce"];
    allreduce = getMPIStat["collectiveCommunicationRoutines","MPI_Allreduce"];
    gather = getMPIStat["collectiveCommunicationRoutines","MPI_Gather"];
    allgather = getMPIStat["collectiveCommunicationRoutines","MPI_Allgather"];
    alltoall = getMPIStat["collectiveCommunicationRoutines","MPI_Alltoall"]
    			+ getMPIStat["collectiveCommunicationRoutines","MPI_Alltoallv"];
    
    Return[{
  		{"MPIinstructions",instructions},
  		{"MPIisend",isend},
  		{"MPIirecv",irecv},
  		{"MPIsend",send},
  		{"MPIrecv",recv},
  		{"MPIbarrier",barrier},
  		{"MPIbcast",bcast},
  		{"MPIreduce",reduce},
  		{"MPIallreduce",allreduce},
  		{"MPIgather",gather},
  		{"MPIallgather",allgather},
  		{"MPIalltoall",alltoall}
    }]
];

ReadMPIdataExchanged[threadData_] :=
Module[{getMPIStat,send,recv,sendMsg,recvMsg},
  getMPIStat[tag_] :=
    ToExpression[OptionValue[threadData, "mpiDataExchanged" -> tag]];
    
    send = getMPIStat["sent_bytes"];
    recv = getMPIStat["received_bytes"];
    sendMsg = getMPIStat["sent_messages"];
    recvMsg = getMPIStat["received_messages"];
    
    Return[{
  		{"MPIsentBytes",send},
  		{"MPIrecvBytes",recv},
  		{"MPIsentMessages",sendMsg},
  		{"MPIrecvMessages",recvMsg}
    }]
];

ReadMPIcollectiveDataExchanged[threadData_] :=
Module[{getMPIStat,
	bcastSend,bcastRecv,alltoallSend,alltoallRecv,
	gatherSend,gatherRecv,allgatherSend,allgatherRecv,
	reduceRoot,reduceNonRoot,allReduce},
  getMPIStat[tag_] :=
    ToExpression[OptionValue[threadData, "mpiDataExchanged" -> tag]];
    
    bcastSend = getMPIStat["bcast_sent_bytes"];
    bcastRecv = getMPIStat["bcast_received_bytes"];
    alltoallSend = getMPIStat["alltoall_sent_bytes"];
    alltoallRecv = getMPIStat["alltoall_received_bytes"];
    gatherSend = getMPIStat["gather_sent_bytes"];
    gatherRecv = getMPIStat["gather_received_bytes"];
    allgatherSend = getMPIStat["allgather_sent_bytes"];
    allgatherRecv = getMPIStat["allgather_received_bytes"];
    reduceRoot = getMPIStat["reduce_root_bytes"];
    reduceNonRoot = getMPIStat["reduce_not_root_bytes"];
    allReduce = getMPIStat["allreduce_bytes"];
    
    Return[{
  		{"MPIbcastSentBytes",bcastSend},
  		{"MPIbcastRecvBytes",bcastRecv},
  		{"MPIalltoallSentBytes",alltoallSend},
  		{"MPIalltoallRecvBytes",alltoallRecv},
  		{"MPIgatherSentBytes",gatherSend},
  		{"MPIgatherRecvBytes",gatherRecv},
  		{"MPIallgatherSentBytes",allgatherSend},
  		{"MPIallgatherRecvBytes",allgatherRecv},
  		{"MPIreduceRootBytes",reduceRoot},
  		{"MPIreduceNotRootBytes",reduceNonRoot},
  		{"MPIallreduceBytes",allReduce}
    }]
];

ReadMPIcommVector[threadData_] :=
Module[{vector,pId},
  pId = ToExpression[OptionValue[threadData, "processId"]];
  vector = OptionValue[threadData, "mpiCommunicationVector"];
  
  
  vector = {OptionValue[#,"source"] - pId ,
  	OptionValue[#,"received_bytes"],OptionValue[#,"received_messages"]}& /@ vector;
  Return[{
  	{"MPIcommVector",vector}
  }];
];

ReadLibraryCalls[threadData_] :=
Module[{libCallVector,c},
  libCallVector = (OptionValue[threadData, "externalLibraryCallCount"]) /. Rule -> List;
  
	(* This part is for FORTRAN support (no case sensitive and delte the ending "_" *)
	libCallVector = Reap[Sow[Nothing];
    Do[
     If[StringMatchQ[c[[1]], RegularExpression[".+_"]],
       Sow[{StringTake[ToLowerCase[c[[1]]], {1, -2}], c[[2]]}];
       ,
       
       Sow[{ToLowerCase[c[[1]]], c[[2]]}];
       ];
     , {c, libCallVector}];
    ][[2, 1]];
  
  
  Return[{
  	{"LibraryCalls",libCallVector}
  }];
];

ReadMemoryFootprint[threadData_] :=
Block[{footprintData,usedBytes,res},
	footprintData = OptionValue[threadData, "memoryFootprint"];
	
	usedBytes = 
    	ToExpression[OptionValue[threadData, "memoryFootprint" -> "total_distinct_addresses_byte_granularity"]];
	Return[{{"M0MemoryFootprint",usedBytes}}];
];

SetAttributes[AdjusteRelativeCommunicationIds,HoldFirst];
AdjusteRelativeCommunicationIds[keyValueList_,maxId_] := Module[{commVector,c},
	commVector = GetKeyValue[keyValueList,"MPIcommVector"];
	commVector = Reap[ Sow[Nothing];
		Do[
			If[c[[1]] >= 0,
				Sow[c];
				,
				Sow[{Mod[c[[1]],maxId+1],c[[2]],c[[3]]}];
			];
		,{c,commVector}];
	][[2,1]];
	
	commVector = SortBy[commVector,First];
	
	keyValueList = SetKeyValue[keyValueList, "MPIcommVector", commVector];
];


End[] (* End Private Context *)

EndPackage[]