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

BeginPackage["CommunicationPerformanceModels`"]
(* Exported symbols added here with SymbolName::usage *)

Needs["ExaBoundsGeneric`"]

NodeEffectiveBandwidth::usage = 
"NodeEffectiveBandwidth[archProperties_, algProperties_]"

UniformNodeEffectiveBandwidth::usage = 
"UniformNodeEffectiveBandwidth[archProperties_]"

NNE2DNodeEffectiveBandwidth::usage = 
"NNE2DNodeEffectiveBandwidth[archProperties_, D1_, D2_,]"

ShiftNodeEffectiveBandwidth::usage = 
"ShiftNodeEffectiveBandwidth[archProperties_, shiftValue_]"


GetAverageNetworkLinkLatency::usage = 
"GetAverageNetworkLinkLatency[archProperties_, algProperties_, tag_]"

UniformAverageNetworkLinkLatency::usage =
"UniformAverageNetworkLinkLatency[archProperties_,tag_]"

NNE2DAverageNetworkLinkLatency::usage =
"NNE2DAverageNetworkLinkLatency[archProperties_, tag_, D1_, D2_]"

ShiftAverageNetworkLinkLatency::usage =
"ShiftAverageNetworkLinkLatency[archProperties_, tag_, shiftValue_]"


GetCommunicationTime::usage =
"GetCommunicationTime[archProperties_, algProperties_]"


Begin["`Private`"]

messageLinearType = "mappingDescription:type should be set to linear.";
messageMappingHyperX = "mapping not supported. D1 should divide d1*p and p should divide D1.";
messageMappingFt3L = "mappingDescription not properly set. Conditions to be met: d11*d122=m1, d21*d22=m1*m2, d11,d12,d21,d22 should divide d21,d22,D1,D2, resp."
messageMappingFt2L = "mappingDescription not properly set. Conditions to be met: d11*d12=m1. d11, d12 should divide D1, D2, resp."
messageMappingFm = "mappingDescription not properly set. Conditions to be met: d11*d12=p. d11, d12 should divide D1, D2, resp.";

archProperties::wrongmapping = "MPI rank mapping not supported for \"`1`\" for nne2D. \"`2`\"";
archProperties::missingkey = "Missing key \"`2`\" for topology \"`1`\"";
archProperties::missingmodel = "Missing \"`1`\" model for topology \"`2`\" and communication pattern \"`3`\"";

(* Average number of links or Average link latency per message for the uniform communication pattern *)
UniformAverageNetworkLinkLatency[archProperties_, tag_] :=
  Module[{ topology, bw, topoDescription, mappingDescription, mappingType, 
            a, p, h, w0, w1, w2, m1, m2, m3, 
            d1, d2, d3, d4, d5, l0, l1, l2, l3, l4, l5, retVal, s1, s2, s3, s4, s5 },

    topology = GetKeyValue[archProperties,"networkConfiguration"];
    bw = GetKeyValue[archProperties,"switchSwitchBandwidth"]; 
    topoDescription = GetKeyValue[archProperties,"topologyDescription"];
    mappingDescription = GetKeyValue[archProperties,"mappingDescription"];
    mappingType = mappingDescription[["type"]];
    retVal = 0;

    If[mappingType != "linear", 
        Message[archProperties::wrongmapping, topology, messageLinearType];
        Abort[], continue];
    
    If[topology == "full-mesh",
    (* Network parameters: (p, a, l0, l1) *)

        a = topoDescription[["a"]];
        p = topoDescription[["p"]];
        
        l0 = GetKeyValue[archProperties,"nodeSwitchLinkLatency"];
        l1 = GetKeyValue[archProperties,"switch1Switch2LinkLatency"];
        
        If[tag==0,
            retVal = (a*p*(p-1)*2*l0 + a*p*p*(a-1)*(2*l0+l1))/((a*p)^2-a*p),
            retVal = (a*p*(p-1)*2 + a*p*p*(a-1)*(2+1))/((a*p)^2-a*p)
        ];

        Return[ retVal ],

    If[topology == "dragonfly",
    (* Network parameters: (p, a, h, l0, l1, l2) *)

        a = topoDescription[["a"]];
        p = topoDescription[["p"]];
        h = topoDescription[["h"]];
        
        l0 = GetKeyValue[archProperties,"nodeSwitchLinkLatency"];
        l1 = GetKeyValue[archProperties,"intraGroupSwitchSwitchLinkLatency"];
        l2 = GetKeyValue[archProperties,"interGroupSwitchSwitchLinkLatency"];
        
        If[tag==0,
            retVal = ((p-1)*2*l0 + p*(a-1)*(2*l0+l1) + h*p*(a-1)*(2*l0+l1+l2) + 
                     (a-1)^2*p*h*(2*l0+2*l1+l2) + p*h*(2*l0+l2) + 
                      p*(a-1)*h*(2*l0+l1+l2)) / ((a*h+1)*a*p),
            retVal = ((p-1)*2 + p*(a-1)*(2+1) + h*p*(a-1)*(2+1+1) + 
                     (a-1)^2*p*h*(2+2+1) + p*h*(2+1) + 
                      p*(a-1)*h*(2+1+1)) / ((a*h+1)*a*p)
        ];
        
        Return[ retVal ],
       
    If[topology == "2DhyperX",
    (* Network parameters: (p, d1, d2, l0, l1) *)

        p = topoDescription[["p"]];
        d1 = topoDescription[["d1"]];
        d2 = topoDescription[["d2"]];
        
        l0 = GetKeyValue[archProperties,"nodeSwitchLinkLatency"];
        l1 = GetKeyValue[archProperties,"switch1Switch2LinkLatency"];
        l2 = l1;
     
        If[tag==0,
            retVal = ((p-1)*2*l0 + p*(d1-1)*(2*l0+l1) + p*(d2-1)*(2*l0+l2) + 
                     p*(d1-1)*(d2-1)*(2*l0+l1+l2)) / (d1*d2*p-1),
            retVal = ((p-1)*2 + p*(d1-1)*(2+1) + p*(d2-1)*(2+1) + 
                     p*(d1-1)*(d2-1)*(2+1+1)) / (d1*d2*p-1)
        ];
      
        Return[ retVal ],

    If[topology == "fat-tree-3L",
    (* Network parameters: (w0, w1, w2, m1, m2, m3, l0, l1, l2) *)

        w0 = topoDescription[["w0"]];
        w1 = topoDescription[["w1"]];
        w2 = topoDescription[["w2"]];
        m1 = topoDescription[["m1"]];
        m2 = topoDescription[["m2"]];
        m3 = topoDescription[["m3"]];
        
        l0 = GetKeyValue[archProperties,"nodeSwitchLinkLatency"];
        l1 = GetKeyValue[archProperties,"switch1Switch2LinkLatency"];
        l2 = GetKeyValue[archProperties,"switch2Switch3LinkLatency"];
        
        If[tag==0,
            retVal = ((m1-1)*2*l0 + (m2-1)*m1*(2*l0+2*l1) + 
                     m1*m2*(m3-1)*(2*l0+2*l1+2*l2))/(m1*m2*m3-1),
            retVal = ((m1-1)*2 + (m2-1)*m1*(2+2) + 
                     m1*m2*(m3-1)*(2+2+2))/(m1*m2*m3-1)
        ];

        Return[ retVal ],

    If[topology == "fat-tree-2L",
    (* Network parameters: (w0, w1, m0, m1, l0, l1) *)

        w0 = topoDescription[["w0"]];
        w1 = topoDescription[["w1"]];
        m1 = topoDescription[["m1"]];
        m2 = topoDescription[["m2"]];
        
        l0 = GetKeyValue[archProperties,"nodeSwitchLinkLatency"];
        l1 = GetKeyValue[archProperties,"switch1Switch2LinkLatency"];
        
        If[tag==0,
            retVal = ((m1-1)*2*l0 + (m2-1)*m1*(2*l0+2*l1))/(m1*m2-1),
            retVal = ((m1-1)*2 + (m2-1)*m1*(2+2))/(m1*m2-1)
        ];

        Return[ retVal ],

    If[topology == "torus1D",
    (* Network parameters: (p, d1, l0, l1) *)
       
        p = topoDescription[["p"]];
        d1 = topoDescription[["d1"]];
        
        l0 = GetKeyValue[archProperties,"nodeSwitchLinkLatency"];
        l1 = GetKeyValue[archProperties,"switch1Switch2LinkLatency"];

        If[tag==0,
            s1 = 2*l0 + Ceiling[d1/2]*Floor[d1/2]/d1*l1 - 2*l0/p/d1;
            retVal = s1,
            
            s1 = 1 + Ceiling[d1/2]*Floor[d1/2]/d1 - 1/p/d1;
            retVal = s1
        ];

        Return[ retVal ],

    If[topology == "torus2D",
    (* Network parameters: (p, d1, d2, l0, l1, l2) *)

        p = topoDescription[["p"]];
        d1 = topoDescription[["d1"]];
        d2 = topoDescription[["d2"]];
        
        l0 = GetKeyValue[archProperties,"nodeSwitchLinkLatency"];
        l1 = GetKeyValue[archProperties,"switch1Switch2LinkLatency"];
        l2 = l1;
                
        If[tag==0,
            s2 = 2*l0 + Ceiling[d1/2]*Floor[d1/2]/d1*l1 + Ceiling[d2/2]*Floor[d2/2]/d2*l2 - 2*l0/p/d1/d2;
            retVal = s2, 
            
            s2 = 1 + Ceiling[d1/2]*Floor[d1/2]/d1 + Ceiling[d2/2]*Floor[d2/2]/d2 - 1/p/d1/d2;
            retVal = s2
        ];

        Return[ retVal ],

    If[topology == "torus3D",
    (* Network parameters: (p, d1, d2, d3, l0, l1, l2, l3) *)

        p = topoDescription[["p"]];
        d1 = topoDescription[["d1"]];
        d2 = topoDescription[["d2"]];
        d3 = topoDescription[["d3"]];
        
        l0 = GetKeyValue[archProperties,"nodeSwitchLinkLatency"];
        l1 = GetKeyValue[archProperties,"switch1Switch2LinkLatency"];
        l2 = l1;
        l3 = l1;
        
        If[tag==0,
            s3 = 2*l0 + Ceiling[d1/2]*Floor[d1/2]/d1*l1 + 
                 Ceiling[d2/2]*Floor[d2/2]/d2*l2 + Ceiling[d3/2]*Floor[d3/2]/d3*l3 - 2*l0/p/d1/d2/d3;
            retVal = s3,

            s3 = 1 + Ceiling[d1/2]*Floor[d1/2]/d1 + 
                 Ceiling[d2/2]*Floor[d2/2]/d2 + Ceiling[d3/2]*Floor[d3/2]/d3 - 1/p/d1/d2/d3;
            retVal = s3
        ];

        Return[ retVal ],

    If[topology == "torus5D",
    (* Network parameters: (p, d1, d2, d3, d4, d5, l0, l1, l2, l3, l4, l5) *)

        p = topoDescription[["p"]];
        d1 = topoDescription[["d1"]];
        d2 = topoDescription[["d2"]];
        d3 = topoDescription[["d3"]];
        d4 = topoDescription[["d4"]];
        d5 = topoDescription[["d5"]];
        
        l0 = GetKeyValue[archProperties,"nodeSwitchLinkLatency"];
        l1 = GetKeyValue[archProperties,"switch1Switch2LinkLatency"];
        l2 = l1;
        l3 = l1;
        l4 = l1;
        l5 = l1;
        
        If[tag==0,
            s5 = 2*l0 + Ceiling[d1/2]*Floor[d1/2]/d1*l1 + 
                 Ceiling[d2/2]*Floor[d2/2]/d2*l2 + Ceiling[d3/2]*Floor[d3/2]/d3*l3 +
                 Ceiling[d4/2]*Floor[d4/2]/d4*l4 + Ceiling[d5/2]*Floor[d5/2]/d5*l5 - 
                 2*l0/p/d1/d2/d3/d4/d5;
            retVal = s5,

            s5 = 1 + Ceiling[d1/2]*Floor[d1/2]/d1 + 
                 Ceiling[d2/2]*Floor[d2/2]/d2 + Ceiling[d3/2]*Floor[d3/2]/d3 +
                 Ceiling[d4/2]*Floor[d4/2]/d4 + Ceiling[d5/2]*Floor[d5/2]/d5 - 
                 2*l0/p/d1/d2/d3/d4/d5;
            retVal = s5
        ];

        Return[ retVal ];
    ];
    ];
    ];
    ];
    ];
    ];
    ];
    ];
    ];
 
    If[tag==0,
        Message[archProperties::missingmodel, "average link latency", topology, "uniform"],
        Message[archProperties::missingmodel, "average number network links", topology, "uniform"]
    ];
    
    Return[ retVal ];
];

(* Average number of links or average link latency per message for the 2D nearest-neighbor communication pattern *)
NNE2DAverageNetworkLinkLatency[archProperties_, tag_, D1_, D2_] :=
  Module [{topology, topoDescription, mappingDescription, mappingType,
            bw, a, p, h, w0, w1, w2, m1, m2, m3, d1, d2, d3, latency, 
            s1, s2, s3, l0, l1, l2, d11, d12, d21, d22, x1, x2, x3, k, m, retVal},

    topology = GetKeyValue[archProperties,"networkConfiguration"];
    bw = GetKeyValue[archProperties,"switchSwitchBandwidth"]; 
    topoDescription = GetKeyValue[archProperties,"topologyDescription"];
    mappingDescription = GetKeyValue[archProperties,"mappingDescription"];
    mappingType = mappingDescription[["type"]];
    retVal = 0;

    If[mappingType != "linear", 
        Message[archProperties::wrongmapping, topology, messageLinearType];
        Abort[], continue];

    If[topology == "full-mesh",
    (* Network parameters: (p, a, l0, l1, D1, D2, d11, d12) *)

        a = topoDescription[["a"]];
        p = topoDescription[["p"]];
        
        l0 = GetKeyValue[archProperties,"nodeSwitchLinkLatency"];
        l1 = GetKeyValue[archProperties,"switch1Switch2LinkLatency"];
        
        d11 = mappingDescription[["d11"]];
        d12 = mappingDescription[["d12"]];
        
	If [(d11*d12==p) && (Mod[D1,d11]==0) && (Mod[D2,d12]==0), 
        
	If[tag == 0,
        
            If[ (d11 == D1) && (Mod[D2, d12] == 0), 
                         x1 = 2*d11*(2*l0+l1);
                         x2 = (4*d11*d12 - 2*d11)*2*l0;
                         retVal = (x1 + x2) / (4*d11*d12),
            If[ (d12 == D2) && (Mod[D1, d11] == 0), 
                         x1 = 2*d12*(2*l0+l1);
                         x2 = (4*d11*d12 - 2*d12)*2*l0;
                         retVal = (x1 + x2) / (4*d11*d12),
            If[ (Mod[D2, d12] == 0) && (Mod[D1, d11] == 0), 
                         x1 = (2*d12+2*d11)*(2*l0+l1);
                         x2 = (4*d11*d12 - 2*d12 - 2*d11)*2*l0;
                         retVal = (x1 + x2) / (4*d11*d12),
               (* This should never happen. *)
               Message[archProperties::wrongmapping, topology, messageMappingFm];      
            ];
            ];
            ],
            
            If[ (d11 == D1) && (Mod[D2, d12] == 0), 
                         x1 = 2*d11*(2+1);
                         x2 = (4*d11*d12 - 2*d11)*2;
                         retVal = (x1 + x2) / (4*d11*d12),
            If[ (d12 == D2) && (Mod[D1, d11] == 0), 
                         x1 = 2*d12*(2+1);
                         x2 = (4*d11*d12 - 2*d12)*2;
                         retVal = (x1 + x2) / (4*d11*d12),
            If[ (Mod[D2, d12] == 0) && (Mod[D1, d11] == 0), 
                         x1 = (2*d12+2*d11)*(2+1);
                         x2 = (4*d11*d12 - 2*d12 - 2*d11)*2;
                         retVal = (x1 + x2) / (4*d11*d12),
               (* This should never happen. *)
               Message[archProperties::wrongmapping, topology, messageMappingFm];      
            ];
            ];
            ];
        ],

	Message[archProperties::wrongmapping, topology, messageMappingFm];
	];
	 
        Return[ retVal ],

    If[topology == "dragonfly",
    (* Network parameters: (p, a, h, l0, l1, l2) *)

        If[tag==0,
            Message[archProperties::missingmodel, "average link latency", topology, "nne2D"],
            Message[archProperties::missingmodel, "average number of network links", topology, "nne2D"];
        ];
       
        Return[ retVal ],
    
    If[topology == "2DhyperX",
    (* Network parameters: (p, d1, d2) *)

        p = topoDescription[["p"]];
        d1 = topoDescription[["d1"]];
        d2 = topoDescription[["d2"]];
        
        l0 = GetKeyValue[archProperties,"nodeSwitchLinkLatency"];
        l1 = GetKeyValue[archProperties,"switch1Switch2LinkLatency"];
        l2 = l1;

        k = d1*p/D1;
        m = D1/p;

        If[(Mod[d1*p,D1]==0) && (Mod[D1,p]==0),

        If[tag==0,
            If[(m==1) && (k==1), 
                 retVal = (2*l0*2*p*m+(2*l0+l2)*2*p*m)/(2*p*m+2*p*m),
            If[(m==1) && (k==2),
                 retVal = (2*l0*2*k*p*m+(2*l0+l1)*2*p*m+(2*l0+l1+l2)*2*p*m)/(2*k*p*m+2*p*m+2*p*m),
            If[(m==1) && (k>2),
                 retVal = (2*l0*2*k*p*m +(2*l0+l1)*2*p*m +(2*l0+l1+l2)*2*p*m + (2*l0+l1)*(k-2)*2*p*m)/(2*k*p*m + 2*p*m + 2*p*m + (k-2)*2*p*m),
            If[(m>=2) && (k==1),
                 retVal = (2*l0*(2*p*m-2*m)+(2*l0+l1)*2*m+ (2*l0+l2)*2*p*m)/(2*p*m -2*m + 2*m + 2*p*m),
            If[(m>=2) && (k==2),
                 retVal = (2*l0*k*(2*p*m-2*m)+(2*l0+l1)*2*k*m + (2*l0+l1)*2*p*m + (2*l0+l1+l2)*2*p*m)/(2*k*p*m - 2*k*m + 2*k*m + 2*p*m + 2*p*m),
            If[(m>=2) && (k>2), 
                 retVal = (2*l0*k*(2*p*m-2*m)+(2*l0+l1)*k*2*m + (2*l0+l1)*2*p*m + (2*l0+l1+l2)*2*p*m + (2*l0+l1)*(k-2)*2*p*m)/(2*k*p*m - 2*k*m + 2*k*m + 2*p*m + 2*p*m + (k-2)*2*p*m),
            (* This should never happen. *)
            Message[archProperties::missingmodel, "average link latency ", topology, "nne2D"];
            ];
            ];
            ];
            ];
            ];
            ];
        ];

        If[tag==1,
            If[ (m==1) && (k==1), 
                 retVal = (2*2*p*m+(2+1)*2*p*m)/(2*p*m+2*p*m),
            If[ (m==1) && (k==2),
                 retVal = (2*2*k*p*m+(2+1)*2*p*m+(2+1+1)*2*p*m)/(2*k*p*m + 2*p*m + 2*p*m),
            If[ (m==1) && (k>2),
                 retVal = (2*2*k*p*m +(2+1)*2*p*m +(2+1+1)*2*p*m + (2+1)*(k-2)*2*p*m)/(2*k*p*m + 2*p*m + 2*p*m + (k-2)*2*p*m),
            If[ (m>=2) && (k==1),
                 retVal = (2*(2*p*m-2*m)+(2+1)*2*m+ (2+1)*2*p*m)/(2*p*m -2*m + 2*m + 2*p*m),
            If[ (m>=2) && (k==2),
                 retVal = (2*k*(2*p*m-2*m)+(2+1)*2*k*m + (2+1)*2*p*m + (2+1+1)*2*p*m)/(2*k*p*m - 2*k*m + 2*k*m + 2*p*m + 2*p*m),
            If[ (m>=2) && (k>2), 
                 retVal = (2*k*(2*p*m-2*m)+(2+1)*k*2*m + (2+1)*2*p*m + (2+1+1)*2*p*m + (2+1)*(k-2)*2*p*m)/(2*k*p*m - 2*k*m + 2*k*m + 2*p*m + 2*p*m + (k-2)*2*p*m),
            (* This should never happen. *)
            Message[archProperties::missingmodel, "average number of network links ", topology, "nne2D"];
            ];
            ];
            ];
            ];
            ];
            ];
        ],

        Message[archProperties::wrongmapping, topology, messageMappingHyperX];
        ];

        Return[ retVal ],
    
    If[topology == "fat-tree-3L",
    (* Network parameters: (w0, w1, w2, m1, m2, m3, l0, l1, l2) *)

        w0 = topoDescription[["w0"]];
        w1 = topoDescription[["w1"]];
        w2 = topoDescription[["w2"]];
        m1 = topoDescription[["m1"]];
        m2 = topoDescription[["m2"]];
        m3 = topoDescription[["m3"]];
        
        l0 = GetKeyValue[archProperties,"nodeSwitchLinkLatency"];
        l1 = GetKeyValue[archProperties,"switch1Switch2LinkLatency"];
        l2 = GetKeyValue[archProperties,"switch2Switch3LinkLatency"];
        
        d11 = mappingDescription[["d11"]];
        d12 = mappingDescription[["d12"]];
        d21 = mappingDescription[["d21"]];
        d22 = mappingDescription[["d22"]];

	If [(d11*d12==m1*m2) && (d21*d22==m1) && (Mod[D1,d11]==0) && (Mod[D2,d12]==0) && (Mod[d11,d21]==0) && (Mod[d12,d22]==0),
       
        If[tag==0,
        
            If[ (d21 == d11) &&  (d11 == D1) && (Mod[D2, d12] == 0) && (Mod[d12, d22] == 0),
                            x1 = 2*d22*(2*l0+2*l1+2*l2);
                            x2 = (4*d11*d12-4*d21*d22)*(2*l0+2*l1);
                            x3 = (4*d21*d22-2*d21)*2*l0;
                            retVal = (x1 + x2 + x3) / (4*d11*d12),
            If[ ((d22 == d12) &&  (Mod[D2, d12] == 0) && (Mod[d11, d21] == 0) && (d11 == D1)) || 
               ((Mod[d12, d22] == 0) &&  (Mod[D2, d22] == 0) && (Mod[d11, d21] == 0) && (d11 == D1)),
                            x1 = 2*d22*(2*l0+2*l1+2*l2);
                            x2 = (4*d11*d12-4*d21*d22+2*d21+2*d22-2*d11)*(2*l0+2*l1);
                            x3 = (4*d21*d22 - 2*d21 - 2*d22)*2*l0;
                            retVal = (x1 + x2 + x3) / (4*d11*d12),
            If[ (d22 == d12) &&  (d12 == D2) && (Mod[D1, d11] == 0) && (Mod[d11, d21] == 0),
                            x1 = 2*d12*(2*l0+2*l1+2*l2);
                            x2 = (4*d11*d12-4*d21*d22)*(2*l0+2*l1);
                            x3 = (4*d21*d22-2*d22)*2*l0;
                            retVal = (x1 + x2 + x3) / (4*d11*d12),
            If[ ((d21 == d11) &&  (Mod[D1, d11] == 0) && (Mod[d12, d22] == 0) && (d12 == D2)) || 
               ((Mod[d11, d21] == 0) &&  (Mod[D1, d11] == 0) && (Mod[d12, d22] == 0) && (d12 == D2)),
                            x1 = 2*d12*(2*l0+2*l1+2*l2);
                            x2 = (4*d11*d12-4*d21*d22+2*d21+2*d22-2*d12)*(2*l0+2*l1);
                            x3 = (4*d21*d22-2*d21-2*d22)*2*l0;
                            retVal = (x1 + x2 + x3) / (4*d11*d12),
            If[ ((d21 == d11) &&  (Mod[D1, d11] == 0) && (Mod[d12, d22] == 0) && (Mod[D2, d12] == 0)) || 
               ((Mod[d11, d21] == 0) &&  (Mod[D1, d11] == 0) && (Mod[D2, d12] == 0) && (d22 == d12)) || 
               ((Mod[d11, d21] == 0) &&  (Mod[D1, d11] == 0) && (Mod[d12, d22] == 0) && (Mod[D2, d12] == 0)),
                            x1 = (2*d12+2*d11)*(2*l0+2*l1+2*l2);
                            x2 = (4*d11*d12 - 4*d21*d22 + 2*d21 + 2*d22 - 2*d12 -2*d11)*(2*l0+2*l1);
                            x3 = (4*d21*d22 - 2*d21 - 2*d22)*2*l0;
                            retVal = (x1 + x2 + x3) / (4*d11*d12),
                    (* This should never happen. *)
                    Message[archProperties::wrongmapping, topology, messageMappingFt3L];                         
            ];                 
            ];
            ];
            ];
            ],
            
 	     If[ (d21 == d11) &&  (d11 == D1) && (Mod[D2, d12] == 0) && (Mod[d12, d22] == 0),
                            x1 = 2*d22*(2+2+2);
                            x2 = (4*d11*d12-4*d21*d22)*(2+2);
                            x3 = (4*d21*d22-2*d21)*2;
                            retVal = (x1 + x2 + x3) / (4*d11*d12),
            If[ ((d22 == d12) &&  (Mod[D2, d12] == 0) && (Mod[d11, d21] == 0) && (d11 == D1)) || 
               ((Mod[d12, d22] == 0) &&  (Mod[D2, d22] == 0) && (Mod[d11, d21] == 0) && (d11 == D1)),
                            x1 = 2*d22*(2+2+2);
                            x2 = (4*d11*d12-4*d21*d22+2*d21+2*d22-2*d11)*(2+2);
                            x3 = (4*d21*d22 - 2*d21 - 2*d22)*2;
                            retVal = (x1 + x2 + x3) / (4*d11*d12),
            If[ (d22 == d12) &&  (d12 == D2) && (Mod[D1, d11] == 0) && (Mod[d11, d21] == 0),
                            x1 = 2*d12*(2+2+2);
                            x2 = (4*d11*d12-4*d21*d22)*(2+2);
                            x3 = (4*d21*d22-2*d22)*2;
                            retVal = (x1 + x2 + x3) / (4*d11*d12),
            If[ ((d21 == d11) &&  (Mod[D1, d11] == 0) && (Mod[d12, d22] == 0) && (d12 == D2)) || 
               ((Mod[d11, d21] == 0) &&  (Mod[D1, d11] == 0) && (Mod[d12, d22] == 0) && (d12 == D2)),
                            x1 = 2*d12*(2+2+2);
                            x2 = (4*d11*d12-4*d21*d22+2*d21+2*d22-2*d12)*(2+2);
                            x3 = (4*d21*d22-2*d21-2*d22)*2;
                            retVal = (x1 + x2 + x3) / (4*d11*d12),
            If[ ((d21 == d11) &&  (Mod[D1, d11] == 0) && (Mod[d12, d22] == 0) && (Mod[D2, d12] == 0)) || 
               ((Mod[d11, d21] == 0) &&  (Mod[D1, d11] == 0) && (Mod[D2, d12] == 0) && (d22 == d12)) || 
               ((Mod[d11, d21] == 0) &&  (Mod[D1, d11] == 0) && (Mod[d12, d22] == 0) && (Mod[D2, d12] == 0)),
                            x1 = (2*d12+2*d11)*(2+2+2);
                            x2 = (4*d11*d12 - 4*d21*d22 + 2*d21 + 2*d22 - 2*d12 -2*d11)*(2+2);
                            x3 = (4*d21*d22 - 2*d21 - 2*d22)*2;
                            retVal = (x1 + x2 + x3) / (4*d11*d12),
                    (* This should never happen. *)
                    Message[archProperties::wrongmapping, topology, messageMappingFt3L];                          
            ];                 
            ];
            ];
            ];
            ];
        ],

	Message[archProperties::wrongmapping, topology, messageMappingFt3L];    
	];
       
        Return[ retVal ],

    If[topology == "fat-tree-2L",
    (* Network parameters: (w0, w1, m0, m1, l0, l1) *)

        w0 = topoDescription[["w0"]];
        w1 = topoDescription[["w1"]];
        m1 = topoDescription[["m1"]];
        m2 = topoDescription[["m2"]];
        
        l0 = GetKeyValue[archProperties,"nodeSwitchLinkLatency"];
        l1 = GetKeyValue[archProperties,"switch1Switch2LinkLatency"];
        
        d11 = mappingDescription[["d11"]];
        d12 = mappingDescription[["d12"]];
       
	If[(d11*d12==m1) && (Mod[D1,d11]==0) && (Mod[D2,d12]==0), 

        If[tag==0,
        
            If[ (d11 == D1) && (Mod[D2, d12] == 0), 
                        x1 = 2*d11*(2*l0+2*l1);
                        x2 = (4*d11*d12 - 2*d11)*2*l0;
                        retVal = (x1 + x2) / (4*d11*d12),
            If[ (d12 == D2) && (Mod[D1, d11] == 0), 
                        x1 = 2*d12*(2*l0+2*l1);
                        x2 = (4*d11*d12 - 2*d12)*2*l0;
                        retVal = (x1 + x2) / (4*d11*d12),
            If[ (Mod[D2, d12] == 0) && (Mod[D1, d11] == 0), 
                        x1 = (2*d12+2*d11)*(2*l0+2*l1);
                        x2 = (4*d11*d12 - 2*d12 - 2*d11)*2*l0;
                        retVal = (x1 + x2) / (4*d11*d12),
                (* This should never happen. *)
                Message[archProperties::wrongmapping, topology, messageMappingFt2L];   
            ];
            ];
            ],
            
            If[ (d11 == D1) && (Mod[D2, d12] == 0), 
                        x1 = 2*d11*(2+2);
                        x2 = (4*d11*d12 - 2*d11)*2;
                        retVal = (x1 + x2) / (4*d11*d12),
            If[ (d12 == D2) && (Mod[D1, d11] == 0), 
                        x1 = 2*d12*(2+2);
                        x2 = (4*d11*d12 - 2*d12)*2;
                        retVal = (x1 + x2) / (4*d11*d12),
            If[ (Mod[D2, d12] == 0) && (Mod[D1, d11] == 0), 
                        x1 = (2*d12+2*d11)*(2+2);
                        x2 = (4*d11*d12 - 2*d12 - 2*d11)*2;
                        retVal = (x1 + x2) / (4*d11*d12),
                (* This should never happen. *)
                Message[archProperties::wrongmapping, topology, messageMappingFt2L];   
            ];
            ];
            ];            
        ],

	Message[archProperties::wrongmapping, topology, messageMappingFt2L];
	];
                     
        Return[ retVal ],

    If[topology == "torus1D",
    (* Network parameters: (p, d1, l0, l1) *)

        If[tag==0,
            Message[archProperties::missingmodel, "average link latency ", topology, "nne2D"],
            Message[archProperties::missingmodel, "average number of network links ", topology, "nne2D"]
        ];
        
        Return[ retVal ],
      
    If[topology == "torus2D",
    (* Network parameters: (p, d1, d2, l0, l1, l2) *)
    
        If[tag==0,
            Message[archProperties::missingmodel, "average link latency ", topology, "nne2D"],
            Message[archProperties::missingmodel, "average number of network links ", topology, "nne2D"]
        ];
        
        Return[ retVal ],
     
    If[topology == "torus3D",
    (* Network parameters: (p, d1, d2, d3, l0, l1, l2, l3) *)
     
        If[tag==0,
            Message[archProperties::missingmodel, "average link latency ", topology, "nne2D"],
            Message[archProperties::missingmodel, "average number of network links ", topology, "nne2D"]
        ];
        
        Return[ retVal ];
    ];
    ];
    ];
    ];
    ];
    ];
    ];
    ];

    If[tag==0,
        Message[archProperties::missingmodel, "average link latency ", topology, "nne2D"],
        Message[archProperties::missingmodel, "average number of network links ", topology, "nne2D"]
    ];
    
    Return[ retVal ];    
   
];

(* Average number of links or average link latency per message for the shift communication pattern *)
ShiftAverageNetworkLinkLatency[archProperties_, tag_] :=
  Module [{topology, a, p, h, w0, w1, w2, m1, m2, m3, 
            d1, d2, d3, links, s1, s2, s3},
    
    topology = GetKeyValue[archProperties,"networkConfiguration"];

    (* TODO: add shift-specific models.*)

    If[tag==0,
        Message[archProperties::missingmodel, "average link latency ", topology, "shift"],
        Message[archProperties::missingmodel, "average number of network links ", topology, "shift"]
    ];
    
    Return[ 0. ];
];

(* Average number of links or average link latency per message *)
GetAverageNetworkLinkLatency[archProperties_, algProperties_, tag_] :=
  Module[{topology, communicationPattern, pattern, D1, D2, shiftValue},

    topology = GetKeyValue[archProperties,"networkConfiguration"];
    communicationPattern = GetKeyValue[algProperties, "CommunicationPattern"];
    pattern = communicationPattern[["type"]]; 
    
    If[pattern == "uniform",
       Return[UniformAverageNetworkLinkLatency[archProperties, tag]],
    If[pattern == "nne2D",
       D1 = communicationPattern[["D1"]];
       D2 = communicationPattern[["D2"]];
       Return[NNE2DAverageNetworkLinkLatency[archProperties, tag, D1, D2]],
    If[pattern == "shift",
       shiftValue = communicationPattern[["value"]];
       Return[ShiftAverageNetworkLinkLatency[archProperties, tag, shiftValue]];
    ];
    ];
    ];
   
    If[tag==0,
        Message[archProperties::missingmodel, "average link latency ", topology, pattern],
        Message[archProperties::missingmodel, "average number of network links ", topology, pattern]
    ];
    
    Return[ 0. ];
];



(* Node effective TX bandwidth for the uniform communication pattern *)
UniformNodeEffectiveBandwidth[archProperties_] :=
  Module[{ topology, bw, topoDescription, mappingDescription, mappingType,
            tmp1, tmp2, a, p, h, w0, w1, w2, m1, m2, m3, 
            d1, d2, d3, d4, d5, bandwidth, bw0, bw1, bw2, bw3, bw4, bw5, x0, x1, x2, x12, x3, x4, x5 },

    topology = GetKeyValue[archProperties,"networkConfiguration"];
    bw0 = GetKeyValue[archProperties,"nodeSwitchBandwidth"];
    bw = GetKeyValue[archProperties,"switchSwitchBandwidth"]; 
    topoDescription = GetKeyValue[archProperties,"topologyDescription"];
    mappingDescription = GetKeyValue[archProperties,"mappingDescription"];
    mappingType = mappingDescription[["type"]];

    If[mappingType != "linear", 
        Message[archProperties::wrongmapping, topology, messageLinearType];
        Abort[], continue
    ];
        
    If[topology == "full-mesh",
    (* Network parameters: (p, a) *)

        a = topoDescription[["a"]];
        p = topoDescription[["p"]];

        bandwidth = Min[bw0,bw*a/p];

        Return[ bandwidth ],

    If[topology == "dragonfly",
    (* Network parameters: (p, a, h) *)

    (* 
       TODO: Adapt models to the latest min effective bandwidth calculation methodology 
             that takes into account different bandwidths. The current model has been 
             validated with Venus simulations. 
    *)

        a = topoDescription[["a"]];
        p = topoDescription[["p"]];
        h = topoDescription[["h"]];

        tmp1 = (a*h+1)*a*p/p/p/(2*a*h+1);
        tmp2 = (a*h+1)*a*p/p/p/a/a;
        bandwidth = Min[bw0,bw*tmp1,bw*tmp2];

        Return[ bandwidth ],

    If[topology == "fat-tree-3L",
    (* Network parameters: (w0, w1, w2, m1, m2, m3) *)

        w0 = topoDescription[["w0"]];
        w1 = topoDescription[["w1"]];
        w2 = topoDescription[["w2"]];
        m1 = topoDescription[["m1"]];
        m2 = topoDescription[["m2"]];
        m3 = topoDescription[["m3"]];

        (* 
           TODO: if the link bandwidths are different in different layers of switching, adjust the Network 
                 Architecture in Architectures/DefaultNetwork/\*.json and change ArchitetureProperties.m 
                 with the new bandwidth paramters. 
        *)

        bw1 = bw;
        bw2 = bw;

        tmp1 = w0*w1/m1/(1-1/m2/m3);
        tmp2 = w0*w1*w2/(m1*m2*(1-1/m3));
        bandwidth = Min[bw0*w0,bw1*tmp1,bw2*tmp2];

        Return[ bandwidth ],

    If[topology == "fat-tree-2L",
    (* Network parameters: (w0, w1, m0, m1) *)

        w0 = topoDescription[["w0"]];
        w1 = topoDescription[["w1"]];
        m1 = topoDescription[["m1"]];
        m2 = topoDescription[["m2"]];
        
        tmp1 = w0*w1/m1/(1-1/m2);
        bandwidth = Min[bw0*w0,bw*tmp1];

        Return[ bandwidth ],

    If[topology == "2DhyperX",
    (* Network parameters: (p, d1, d2) *)

        p = topoDescription[["p"]];
        d1 = topoDescription[["d1"]];
        d2 = topoDescription[["d2"]];

        (* 
           TODO: if the link bandwidths are different in different layers of switching, adjust the Network 
                 Architecture in Architectures/DefaultNetwork/\*.json and change ArchitetureProperties.m 
                 with the new bandwidth paramters. 
        *)

        bw1 = bw;
        bw2 = bw;

        bandwidth = Min[bw0, bw1*d1/p,bw2*d2/p];
      
        Return[ bandwidth ],

    If[topology == "torus5D",
    (* Network parameters: (p, d1, d2, d3) *)

        p = topoDescription[["p"]];
        d1 = topoDescription[["d1"]];
        d2 = topoDescription[["d2"]];
        d3 = topoDescription[["d3"]];
        d4 = topoDescription[["d4"]];
        d5 = topoDescription[["d5"]];

        (* 
           TODO: if the link bandwidths are different in different layers of switching, adjust the Network 
                 Architecture in Architectures/DefaultNetwork/\*.json and change ArchitetureProperties.m 
                 with the new bandwidth paramters. 
        *)

        bw1 = bw;
        bw2 = bw;
        bw3 = bw;
        bw4 = bw;
        bw5 = bw;

        x1 = d1/Floor[d1/2]/Ceiling[d1/2]/p;
        x2 = d2/Floor[d2/2]/Ceiling[d2/2]/p;
        x3 = d3/Floor[d3/2]/Ceiling[d3/2]/p;
        x4 = d4/Floor[d4/2]/Ceiling[d4/2]/p;
        x5 = d5/Floor[d5/2]/Ceiling[d5/2]/p;

        bandwidth = Min[bw0, 2*bw1*x1, 2*bw2*x2, 2*bw3*x3, 2*bw4*x4, 2*bw5*x5];
      
        Return[ bandwidth ],

    If[topology == "torus3D",
    (* Network parameters: (p, d1, d2, d3) *)

        p = topoDescription[["p"]];
        d1 = topoDescription[["d1"]];
        d2 = topoDescription[["d2"]];
        d3 = topoDescription[["d3"]];
        
        (* 
           TODO: if the link bandwidths are different in different layers of switching, adjust the Network 
                 Architecture in Architectures/DefaultNetwork/\*.json and change ArchitetureProperties.m 
                 with the new bandwidth paramters. 
        *)

        bw1 = bw;
        bw2 = bw;
        bw3 = bw;

        x1 = d1/Floor[d1/2]/Ceiling[d1/2]/p;
        x2 = d2/Floor[d2/2]/Ceiling[d2/2]/p;
        x3 = d3/Floor[d3/2]/Ceiling[d3/2]/p;

        bandwidth = Min[bw0, 2*bw1*x1, 2*bw2*x2, 2*bw3*x3];
      
        Return[ bandwidth ],

    If[topology == "torus2D",
    (* Network parameters: (p, d1, d2) *)

        p = topoDescription[["p"]];
        d1 = topoDescription[["d1"]];
        d2 = topoDescription[["d2"]];
        
        (* 
           TODO: if the link bandwidths are different in different layers of switching, adjust the Network 
                 Architecture in Architectures/DefaultNetwork/\*.json and change ArchitetureProperties.m 
                 with the new bandwidth paramters. 
        *)

        bw1 = bw;
        bw2 = bw;

        x1 = d1/Floor[d1/2]/Ceiling[d1/2]/p;
        x2 = d2/Floor[d2/2]/Ceiling[d2/2]/p;

        bandwidth = Min[bw0, 2*bw1*x1, 2*bw2*x2];
      
        Return[ bandwidth ],

    If[topology == "torus1D",
    (* Network parameters: (p, d1) *)

        p = topoDescription[["p"]];
        d1 = topoDescription[["d1"]];    
  
        (* 
           TODO: if the link bandwidths are different in different layers of switching, adjust the Network 
                 Architecture in Architectures/DefaultNetwork/\*.json and change ArchitetureProperties.m 
                 with the new bandwidth paramters. 
        *)

        bw1 = bw;

        x1 = d1/Floor[d1/2]/Ceiling[d1/2]/p;
        bandwidth = Min[bw0, 2*bw1*x1];
      
        Return[ bandwidth ];

    ];
    ];
    ];
    ];
    ];
    ];
    ];
    ];
    ];

    Message[archProperties::missingmodel, "effective bandwidth", topology, "uniform"];
   
    Return [ bw ]; 
];

(* Node effective TX bandwidth for the 2D nearest-neighbor communication pattern *)
NNE2DNodeEffectiveBandwidth[archProperties_, D1_, D2_] :=
  Module[{ topology, topoDescription, mappingDescription, mappingType,
            bw, tmp1, tmp2, a, p, h, w0, w1, w2, m1, m2, m3, 
            d1, d2, d3, bandwidth, bw0, bw1, bw2, x0, x1, x2, x12, 
            d11, d12, d21, d22, k, m, b, x, y },

    topology = GetKeyValue[archProperties,"networkConfiguration"];
    bw0 = GetKeyValue[archProperties,"nodeSwitchBandwidth"];
    bw = GetKeyValue[archProperties,"switchSwitchBandwidth"]; 
    topoDescription = GetKeyValue[archProperties,"topologyDescription"];
    mappingDescription = GetKeyValue[archProperties,"mappingDescription"];
    mappingType = mappingDescription[["type"]];
    
    If[mappingType != "linear", 
        Message[archProperties::wrongmapping, topology, messageLinearType];
        Abort[], continue
    ];
        
    If[topology == "full-mesh",
    (* Network parameters: (p, a) *)

        a = topoDescription[["a"]];
        p = topoDescription[["p"]];
        
        d11 = mappingDescription[["d11"]];
        d12 = mappingDescription[["d12"]];
        
        bw1 = bw;

        If[(d11*d12==p) && (Mod[D1,d11]==0) && (Mod[D2,d12]==0),

        If[(D1 > 2*d11) && (D2 > 2*d12),     
            bandwidth = 4*Min[bw0/4, bw1/d11, bw1/d12],
        If[(D1 == 2*d11) && (D2 > 2*d12), 
            bandwidth = 4*Min[bw0/4, bw1/d11, bw1/2/d12],
        If[(D1 > 2*d11) && (D2 == 2*d12), 
            bandwidth = 4*Min[bw0/4, bw1/2/d11, bw1/d12],
        If[(D1 == 2*d11) && (D2 == 2*d12), 
            bandwidth = 4*Min[bw0/4, bw1/2/d11, bw1/2/d12],
        If[(D1 == d11) && (D2 > 2*d12),   
            bandwidth = 4*Min[bw0/4, bw1/d11],
        If[(D1 > 2*d11) && (D2 == d12),   
            bandwidth = 4*Min[bw0/4, bw1/d12],
        If[(D1 == 2*d11) && (D2 == d12),   
            bandwidth = 4*Min[bw0/4, bw1/2/d12],
            bandwidth = bw;
        ];
        ];    
        ];
        ];
        ];
        ];
        ],

        Message[archProperties::wrongmapping, topology, messageMappingFm];
        ];

        Return[ bandwidth ],

    If[topology == "fat-tree-3L",
    (* Network parameters: (w0, w1, w2, m1, m2, m3) *)

        w0 = topoDescription[["w0"]];
        w1 = topoDescription[["w1"]];
        w2 = topoDescription[["w2"]];
        m1 = topoDescription[["m1"]];
        m2 = topoDescription[["m2"]];
        m3 = topoDescription[["m3"]];
        
        d11 = mappingDescription[["d11"]];
        d12 = mappingDescription[["d12"]];
        d21 = mappingDescription[["d21"]];
        d22 = mappingDescription[["d22"]];

        bw1 = bw;
        bw2 = bw;

        a = D1/d11;
        b = D2/d12;
        x = d11/d21;
        y = d12/d22;
        
        If [(d11*d12==m1*m2) && (d21*d22==m1) && (Mod[D1,d11]==0) && (Mod[D2,d12]==0) && (Mod[d11,d21]==0) && (Mod[d12,d22]==0),

        If[(a >= 2) && (b >= 2) && (x >= 2) && (y >= 2),    
            bandwidth = 4*Min[bw0/4, bw1*w1/2/(d21+d22), bw2*w1*w2/2/(d11+d12)],
        If[(a == 1) && (b >= 2) && (x >= 2) && (y >= 1), 
            bandwidth = 4*Min[bw0/4, bw1*w1/2/(d21+d22), bw2*w1*w2/2/d11],
        If[(a == 1) && (b >= 2) && (x == 1) && (y >= 2),  
            bandwidth = 4*Min[bw0/4, bw1*w1/2/d21, bw2*w1*w2/2/d11],
        If[(a >= 1) && (b == 1) && (x >= 1) && (y >= 2), 
            bandwidth = 4*Min[bw0/4, bw1*w1/2/(d21+d22), bw2*w1*w2/2/d12],
        If[(a >= 2) && (b == 1) && (x >= 2) && (y == 1),  
            bandwidth = 4*Min[bw0/4, bw1*w1/2/d22, bw2*w1*w2/2/d12],
            bandwidth = bw;
        ];
        ];
        ];
        ];
        ],

        Message[archProperties::wrongmapping, topology, messageMappingFt3L];
        ];
        
        Return[ bandwidth ],

    If[topology == "fat-tree-2L",
    (* Network parameters: (w0, w1, m0, m1) *)

        w0 = topoDescription[["w0"]];
        w1 = topoDescription[["w1"]];
        m1 = topoDescription[["m1"]];
        m2 = topoDescription[["m2"]];
        
        d11 = mappingDescription[["d11"]];
        d12 = mappingDescription[["d12"]];

        bw1 = bw;
        
        a = D1/d11;
        b = D2/d12;

        If [(d11*d12==m1) && (Mod[D1,d11]==0) && (Mod[D2,d12]==0),

        If[(a >= 2) && (b >= 2),    
            bandwidth = 4*Min[bw0/4, bw1*w1/2/(d11+d12)],
        If[(a == 1) && (b >= 2), 
            bandwidth = 4*Min[bw0/4, bw1*w1/2/d11],
        If[(a >= 2) && (b == 1), 
            bandwidth = 4*Min[bw0/4, bw1*w1/2/d12],
        If[(a == 1) && (b == 1),  
            bandwidth = 4*Min[bw0/4],
            bandwidth = bw;
        ];
        ];
        ];
        ],

        Message[archProperties::wrongmapping, topology, messageMappingFt2L];

        ];
        
        Return[ bandwidth ],

    If[topology == "2DhyperX",
    (* Network parameters: (p, d1, d2) *)

        p = topoDescription[["p"]];
        d1 = topoDescription[["d1"]];
        d2 = topoDescription[["d2"]];

        bw1 = bw;
        bw2 = bw;

        k = d1*p/D1;
        m = D1/p;

        (* Check if k and m are integer or fractional. *)
        If[Mod[D1,p]==0 && Mod[d1*p,D1]==0,

        If[(m == 1) && (k > 2), 
            bandwidth = 4*Min[bw0/4, bw1/p, 2*bw2/p],
        If[(m == 1) && (k == 1),
            bandwidth = 4*Min[bw0/4, bw2/p],
        If[(m == 1) && (k == 2),
            bandwidth = 4*Min[bw0/4, bw1/2/p, 2*bw2/p],
        If[(m == 2) && (k > 2),
            bandwidth = 4*Min[bw0/4, bw1/2, bw1/p, 2*bw2/p],
        If[(m == 2) && (k == 1),
            bandwidth = 4*Min[bw0/4, bw1/2, bw2/p],
        If[(m == 2) && (k == 2),
            bandwidth = 4*Min[bw0/4, bw1/2/p, 2*bw2/p],
        If[(m > 2) && (k > 2),
            bandwidth = 4*Min[bw0/4, bw1/p, 2*bw2/p],
        If[(m > 2) && (k == 1),
            bandwidth = 4*Min[bw0/4, bw1, bw2/p],
        If[(m > 2) && (k == 2),
            bandwidth = 4*Min[bw0/4, bw1/2/p, 2*bw2/p],
            bandwidth = bw;
        ];
        ];
        ];
        ];
        ];
        ];
        ];
        ];    
        ],

        Message[archProperties::wrongmapping, topology, messageMappingHyperX];
        ];
    
        Return[ bandwidth ];
    ];
    ];
    ];
    ];

    Message[archProperties::missingmodel, "effective bandwidth", topology, "nne2D"];
    Return [ bw ]; 
   
];

(* Node effective TX bandwidth for the shift communication pattern *)
ShiftNodeEffectiveBandwidth[archProperties_, shiftValue_] :=
  Module[{  topology, topoDescription, mappingDescription, mappingType, s, 
            bw, tmp1, tmp2, k, l, m, b1, b2, b3, a, p, h, w0, w1, w2, m1, m2, m3, 
            d1, d2, d3, bandwidth, bw0, bw1, bw2, x0, x1, x2, x12 },

    s = shiftValue;

    topology = GetKeyValue[archProperties,"networkConfiguration"];
    bw0 = GetKeyValue[archProperties,"nodeSwitchBandwidth"];
    bw = GetKeyValue[archProperties,"switchSwitchBandwidth"]; 
    topoDescription = GetKeyValue[archProperties,"topologyDescription"];
    mappingDescription = GetKeyValue[archProperties,"mappingDescription"];
    mappingType = mappingDescription[["type"]];
    
    If[mappingType != "linear", 
        Message[archProperties::wrongmapping, topology, messageLinearType];
        Abort[], continue
    ];   
    
    If[topology == "full-mesh",
    (* Network parameters: (p, a) *)

        a = topoDescription[["a"]];
        p = topoDescription[["p"]];

        bw1 = bw;         

        If[s<p, bandwidth = Min[bw0, bw1/s], 
           If[Mod[s,p]== 0, bandwidth = Min[bw0, bw1/p], 
              bandwidth = Min[bw0, bw1/Mod[s,p], bw1/(p-Mod[s,p])]
           ];
        ];
        
        Return[ bandwidth ],

    If[topology == "dragonfly",
    (* Network parameters: (p, a, h) *)

    (* 
       TODO: Adapt models to the latest min effective bandwidth calculation methodology 
             that takes into account different bandwidths. 
    *)

        a = topoDescription[["a"]];
        p = topoDescription[["p"]];
        h = topoDescription[["h"]];
        
        tmp1 = Mod[s,a*p];
        
        If[ s<=p, 
            bandwidth = bw*(p-s+1)/p, 
        If[((s>p) && (s<a*p)), bandwidth = bw*2*(a-s/p)/a/p,
            bandwidth = bw*((a*p-tmp1)*Min[1/p, 1/(a*p-tmp1)]+tmp1*Min[1/p, 1/tmp1])/a/p;
        ];
        ];

        Return[ bandwidth ],

    If[topology == "fat-tree-3L",
    (* Network parameters: (w0, w1, w2, m1, m2, m3) *)
    
        w0 = topoDescription[["w0"]];
        w1 = topoDescription[["w1"]];
        w2 = topoDescription[["w2"]];
        m1 = topoDescription[["m1"]];
        m2 = topoDescription[["m2"]];
        m3 = topoDescription[["m3"]];
       
        bw1 = bw;
        bw2 = bw;

        If[s<m1, 
            bandwidth = Min[bw0, bw1*w1/s, bw2*w1*w2/s],
        If[((s>=m1) && (s<m1*m2)), 
            bandwidth = Min[bw0, bw1*w1/m1, bw2*w1*w2/s],
            bandwidth = Min[bw0, bw1*w1/m1, bw2*w1*w2/m1/m2];
        ];
        ];
        
        Return[ bandwidth ],

    If[topology == "fat-tree-2L",
    (* Network parameters: (w0, w1, m0, m1) *)

        w0 = topoDescription[["w0"]];
        w1 = topoDescription[["w1"]];
        m1 = topoDescription[["m1"]];
        m2 = topoDescription[["m2"]];

        bw1 = bw;

        If[s<m1, 
            bandwidth = Min[bw0, bw1*w1/s], 
            bandwidth = Min[bw0, bw1*w1/m1]
        ];
            
        Return[ bandwidth ],

    If[topology == "2DhyperX",
    (* Network parameters: (p, d1, d2) *)

    (* 
       TODO: Create model based on the latest min effective bandwidth calculation methodology 
             that also takes into account different bandwidths. 
    *)
        p = topoDescription[["p"]];
        d1 = topoDescription[["d1"]];
        d2 = topoDescription[["d2"]];
        
        Message[archProperties::missingmodel, "effective bandwidth", topology, "shift"];

        Return[ bw ],

    If[topology == "torus3D",
    (* Network parameters: (p, d1, d2, d3) *)

    (* 
       TODO: Adapt models to the latest min effective bandwidth calculation methodology 
             that takes into account different bandwidths. 
    *)

        p = topoDescription[["p"]];
        d1 = topoDescription[["d1"]];
        d2 = topoDescription[["d2"]];
        d3 = topoDescription[["d3"]];
        
        k = Floor[s/(d1*d2)];
        l = Floor[Mod[s,d1*d2]/d1];
        m = Mod[Mod[s,d1*d2],d1];
       
        If[ l==0, b1=bw,
        If[ l!=d1/2, b1=bw*1/Min[d1-l, l],
            b1 = bw*2/l;
        ];
        ];
        
        If[ k==0, b2=bw,
        If[ k!=d2/2, b2=bw*1/Min[d2-k, k],
            b2 = bw*2/k;
        ];
        ];
      
        If[ m==0, b3=bw,
        If[ m!=d3/2, b3=bw*1/Min[d3-m, m],
            b3 = bw*2/m;
        ];
        ];
        
        bandwidth = Min[b1, b2, b3];
       
        Return[ bandwidth ],

    If[topology == "torus2D",
    (* Network parameters: (p, d1, d2) *)
        
    (* 
       TODO: Adapt models to the latest min effective bandwidth calculation methodology 
             that takes into account different bandwidths. 
    *)

        p = topoDescription[["p"]];
        d1 = topoDescription[["d1"]];
        d2 = topoDescription[["d2"]];
        
        k = Floor[s/d1];
        l = Mod[s,d1];
       
        If[ l==0, b1=bw,
        If[ l!=d1/2, b1=bw*1/Min[d1-l, l],
            b1=bw*2/l;
        ];
        ];
        
        If[ k==0, b2=bw,
        If[ k!=d2/2, b2=bw*1/Min[d2-k, k],
            b2=bw*2/k;
        ];
        ];

        bandwidth = Min[b1, b2];
       
        Return[ bandwidth ],

    If[topology == "torus1D",
    (* Network parameters: (p, d1) *)

    (* 
       TODO: Adapt models to the latest min effective bandwidth calculation methodology 
             that takes into account different bandwidths. 
    *)

        p = topoDescription[["p"]];
        d1 = topoDescription[["d1"]];    
        
        l = Mod[s,d1];
       
        If[ l==0, b1=bw,
        If[ l!=d1/2, b1=bw*1/Min[d1-l, l],
            b1 = bw*2/l;
        ];
        ];
        
        bandwidth = b1;
      
        Return[ bandwidth ];

    ];
    ];
    ];
    ];
    ];
    ];
    ];
    ];
   
    Message[archProperties::missingmodel, "effective bandwidth", topology, "shift"];
   
    Return [ bw ]; 
];

(* Node effective TX bandwidth *)
NodeEffectiveBandwidth[archProperties_, algProperties_] :=
  Module[{ pattern, communicationPattern, effectiveBw, topology, shiftValue, D1, D2},

    effectiveBw = GetKeyValue[archProperties,"switchSwitchBandwidth"];   
    topology = GetKeyValue[archProperties,"networkConfiguration"];
    communicationPattern = GetKeyValue[algProperties, "CommunicationPattern"];
    pattern = communicationPattern[["type"]];
    
    If[pattern == "uniform",
        Return[UniformNodeEffectiveBandwidth[archProperties]],
    If[pattern == "nne2D",
        (* D1 and D2 are the two dimensions of the nearest-neighbor at the application level *)
        D1 = communicationPattern[["D1"]];
        D2 = communicationPattern[["D2"]];
        Return[NNE2DNodeEffectiveBandwidth[archProperties, D1, D2]],
    If[pattern == "shift",
        shiftValue = communicationPattern[["value"]];
        Return[ShiftNodeEffectiveBandwidth[archProperties, shiftValue]];
    ];
    ];
    ];

    Message[archProperties::missingmodel, "effective bandwidth", topology, pattern];
    Return[ effectiveBw ];

];

(* Communication time per process (PISA profile) or class of process (ExtraX profile with clusters of processes) *)
GetCommunicationTime[archProperties_, algProperties_] :=
  Module[{ nodeStackLatency, switchLatency, averageLinkLatency, averageNumberSwitches, nodeEffBandwidth,
           mpiSentBytes, mpiReceivedBytes, commVector, messages, i, msgSize,
           mpiISentBytes, sentBytes, receivedBytes, sentMessages, recvMessages},
       
    averageLinkLatency = GetAverageNetworkLinkLatency[archProperties, algProperties, 0];
    (* Print["GetCommunicationTime: averageLinkLatency"]; Print[averageLinkLatency]; *)
       
    averageNumberSwitches = GetAverageNetworkLinkLatency[archProperties, algProperties, 1] - 1;
    (* Print["GetCommunicationTime: averageNumberSwitches"]; Print[averageNumberSwitches]; *)
       
    nodeEffBandwidth = NodeEffectiveBandwidth[archProperties, algProperties];
    (* Print["GetCommunicationTime: nodeEffBandwidth"]; Print[nodeEffBandwidth]; *)

    nodeStackLatency = GetKeyValue[archProperties,"nodeStackLatency"];
    switchLatency = GetKeyValue[archProperties,"switchLatency"];

    mpiSentBytes = GetKeyValue[algProperties,"MPIsentBytes"];
    (* Print["GetCommunicationTime: mpiSentBytes"]; Print[mpiSentBytes]; *)
    
    mpiReceivedBytes = GetKeyValue[algProperties,"MPIrecvBytes"];
    (* Print["GetCommunicationTime: mpiReceivedBytes"]; Print[mpiReceivedBytes]; *)
    
    sentMessages = GetKeyValue[algProperties,"MPIsentMessages"];
    recvMessages = GetKeyValue[algProperties,"MPIrecvMessages"];
    
    If[recvMessages==0, 
    (* Check whether there are also MPIIrecv calls. *)
    (* messages = GetKeyValue[algProperties,"MPIrecv"] + GetKeyValue[algProperties,"MPIirecv"]; *)
	messages = GetKeyValue[algProperties,"MPIrecv"],
    messages = recvMessages;
    ];

    (* Print["GetCommunicationTime: messages"]; Print[messages]; *)

    (* 
    commVector = GetKeyValue[algProperties,"MPIcommVector"];
    messages = 0;
    For[i=1, i<= Length[commVector], i++, messages = messages + commVector[[i]][[3]]];
    *)

    msgSize = mpiReceivedBytes/messages*1.0;
    (* Print["GetCommunicationTime: msgSize"]; Print[msgSize]; *)

    Return[ messages * (nodeStackLatency + averageLinkLatency + 
            averageNumberSwitches*switchLatency + msgSize/nodeEffBandwidth) ];

];


End[] (* End Private Context *)

EndPackage[]
