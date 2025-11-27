(* ::Package:: *)

ClearLiteRedFile[tt_] := If[FileExistsQ[Topology[tt][LiteRedFile]],
        DeleteFileIfExistsQ[Topology[tt][LiteRedFile]];
        KeyDropFrom[Topology[tt], {LiteRedFile,LiteRedTopology,LiteRedMIs}];
        ];
ClearLiteRed[tt__] := ClearLiteRedFile /@ {tt};
ClearLiteRed[] := ClearLiteRedFile /@ AllTopologies[];


CheckForLiteRed[] := If[Not[ValueQ[LiteRed`$LiteRedVersion]],
      Message[LiteRed::NoFound];False,
      If[ToExpression[StringSplit[LiteRed`$LiteRedVersion,"."][[1]]]<2,
         Message[LiteRed::Version];False,
         True]
      ];


LiteRed::NoFile = "No LiteRed input file found for this topology.";
LiteRed::NoTopo = "`1` is not defined as a topology.";
LiteRed::FileExists = "A LiteRed file already exists for topology `1`. Please execute ClearLiteRed[`1`], and then run again.";

Options[RunLiteRed] = {Dimension -> d, AdditionalRules -> {}, FileName -> Automatic, Directory -> Automatic, Save -> True, LiteRedOptions->{LiteRed`SolvejSector -> True},Verbose -> True, Vectors -> {}, Scalars -> {}};

RunLiteRed[name2:(_List|All), OptionsPattern[] ]:= Module[{opto, name},

   name = If[name2 === All, AllTopologies[], name2];

   opto = First/@Options[RunLiteRed];
   opto = Rule[#,OptionValue[#] ]&/@opto;

   RunLiteRed[#,Sequence@@opto]&/@name;
];

RunLiteRed[name_,OptionsPattern[]] := Module[{filename,in,opts,opto,check},

   If[Not[CheckForLiteRed[] ], Return[$Failed] ];
      

    If[KeyExistsQ[Topology[name] ],
       Message[LiteRed::FileExists,name];
       Return[$Failed]
       ];

    If[!MemberQ[AllTopologies[], name],
       Message[LiteRed::NoTopo,name];Return[$Failed]
       ];


    in = Topology[name];
    If[AssociationQ[in],
       If[KeyExistsQ[in,LiteRedFile],
          filename = in[LiteRedFile],
          opts = Rule[#,OptionValue[#]]& /@ (First /@ Options[RunLiteRed]);
          WriteLiteRedFile[name, Sequence@@opts, Verbose -> False];
          in = Topology[name];
          filename = in[LiteRedFile]
          ]
     ];
    If[StringQ[filename],
       If[Not[FileExistsQ[filename]],
          Message[LiteRed::NoFile];Return[$Failed]
          ]
      ];

   Print[Style["Running LiteRed!", Red]];
   Get[filename];

   If[ValueQ[LiteRed`MIs[CreateLiteRedTopoName[name] ]],
      Topology[name] = Append[Topology[name], LiteRedMIs -> LiteRed`MIs[CreateLiteRedTopoName[name] ]],
      Topology[name] = Append[Topology[name], MasterIntegrals -> LiteRed`MIs[CreateLiteRedTopoName[name] ]]
      ];

 ]


(* ::Section:: *)
(*Setting it all up*)


CreateLiteRedTopoName[name_] := Symbol[ToString[name]<>"LiteRed"];


Options[WriteLiteRedFile] := Options[RunLiteRed];



WriteLiteRedFile[name_:All, OptionsPattern[]] := Module[{topo,opto,
   props, loop, ext,
   scalars, file, filename = OptionValue[FileName], 
   dir = OptionValue[Directory] ,
   rules = OptionValue[AdditionalRules], 
   literedoptions = StringRiffle[ToString[#, InputForm]&/@OptionValue[LiteRedOptions], ", "]
   },
   
   If[Not[CheckForLiteRed[]], Return[$Failed]];
      
   
   If[name === All,
      name = AllTopologies[]
      ];
   If[Head[name] === List,
      opto = First/@Options[WriteLiteRedFile];
      opto = Rule[#,OptionValue[#]]& /@ opto;
      WriteLiteRedFile[#,Sequence@@opto]&/@name
      ];
      
      
   topo = Topology[name];
   props = topo[Propagators];
   If[Not[VectorQ[props]], props = {props}];
   loop = topo[LoopMomenta];
   loop = Join[If[Not[VectorQ[loop]], {loop}, loop], OptionValue[Vectors]];
   ext = topo[ExternalMomenta];
   ext = Join[If[Not[VectorQ[ext]], {ext}, ext], OptionValue[Vectors]];
   
   If[filename === Automatic,
      filename = "LiteRed_"<>ToString[name]<>".m"
      ];
      
   If[dir === Automatic,
      dir = "LiteRed_"<>ToString[name]
      ];
   
   scalars = Complement[Union[Flatten[Variables/@Sequence@@@props]],Join[loop,ext]];
   
   file = OpenWrite[filename];
   WriteString[file, "(* This file was automatically created by ANATAR *)\n"];
   WriteString[file, "(* Date: ",ToString[Date[][[3]]]<>"."<>ToString[Date[][[2]]]<>"."<>ToString[Date[][[1]]]," *)\n\n"];

   WriteString[file, "SetDim[", ToString[OptionValue[Dimension]],"];\n"];
   WriteString[file, "Declare[",Join[loop,ext],", Vector];\n\n"];
   If[scalars =!= {}, 
      WriteString[file, "Declare[",scalars,", Number];\n\n"]
      ];
   WriteString[file, ToString[#[[1]]]<>" = "<>ToString[#[[2]]]<>";"]&/@rules;
   WriteString[file, "\n\n"];
   
   
   WriteString[file, "NewDsBasis[", name ,"LiteRed, ",ToString[props //. Den[$k_,$m_] :> Vectors`sp[$k]-$m^2, InputForm], ", ", loop,", Directory -> \"",dir,"\"",If[literedoptions=!="",", "<>literedoptions,""],"];\n\n"];
     
   If[OptionValue[Save],
      WriteString[file, "DiskSave[",name,"LiteRed];\n\n"]
      ];
   Close[file];
   
   Topology[name] = Join[topo, Association[LiteRedFile -> filename, LiteRedTopology -> Symbol[ToString[name] <> "LiteRed"]]];
   AN$FromLiteRedTopology[Symbol[ToString[name] <> "LiteRed"]] = name;
   If[OptionValue[Verbose],
      Print["Created LiteRed input file "<>filename<>"."];
      Print["If LiteRed is loaded, you can run LiteRed by calling either RunLiteRed[" <> ToString[name] <> "] or Get[\""<>filename<>"\"]." ];
      ];
];


(* ::Section:: *)
(*LiteRed*)


FromLiteRedj[expr_, topolist_:Automatic] := expr //. LiteRed`j[tt_, as__] :> TopInt[AN$FromLiteRedTopology[tt],as];
ToLiteRedj[expr_, topolist_:Automatic] := expr //. TopInt[tt_?(MemberQ[If[topolist === Automatic, AllTopologies[], topolist],#]&),as__] :> LiteRed`j[Topology[tt][LiteRedTopology],as];


LiteRed[func_][expr_] := Which[(func === LiteRed`IBPReduce) && (Head[expr] === Association), AN$KeyReplace[expr, Amplitudes, FromLiteRedj[func[ToLiteRedj[expr[Amplitudes]]]]], 
         True, FromLiteRedj[func[ToLiteRedj[expr]]]
         ];
