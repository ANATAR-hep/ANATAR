
Options[MakeGreen] := {Boldmg->False};

MakeGreen[string_,opts: OptionsPattern[] ] := Module[{bold = OptionValue[Boldmg] },
    
  If[bold===False,
     ToString[Style[string,Darker@Green],StandardForm],
     ToString[Style[string,Bold,Darker@Green],StandardForm]
  ]

];
gaugeDefaultsList = {"QCD" -> "Feynman", "EW" -> "Feynman"};
toAssoc[spec_] := Association@Replace[spec, {a_Association :> Normal[a], l : {(_String -> _) ..} :> l, _ :> {}}];

(*********************************************)
(*     Function that writes the amplitude    *)
(*********************************************)

Options[GenerateAmplitude] := Join[Options[QGDiagrams], {Model -> "None", OutputName->"None",Dimension -> "None",SimplifiedOutput-> False,PolarizationTrim->False, SelectAmplitudes-> "All", RecycleAmplitude -> False, SelectCouplingsOrder->"All",SubstitutionRules->"None", Gauges->gaugeDefaultsList, OffShell->False,OnlyDiagrams->False }];

GenerateAmplitude::noModel          = " No model has been specified. Load a model using the option Model of GenerateAmplitude or the function LoadModel.";
GenerateAmplitude::ModelNotFound    = " Model \"`1`\" not found. ";
GenerateAmplitude::noQGModel        = " The QGraf model file of model \"`1`\" not found.";
GenerateAmplitude::InvalidProcess   = " Wrong syntax in the specified process, numbers cannot be fields.";
GenerateAmplitude::InvalidLoopOrder = "InvalidLoopOrder:`1`";
GenerateAmplitude::InvalidModel     = "InvalidModel:`1`";
GenerateAmplitude::undefinedFields  = " The fields `1` are not defined in the model `2`";
GenerateAmplitude::noDiagrams       = " Diagrams not generated, check the file diagrams_`1`.log for more information.";
GenerateAmplitude::noDiagrams2      = " Zero diagrams generated for the given model, fields and selection of coupligns order."
GenerateAmplitude::treeLevelOtoO    = " The one-to-one amplitude has been requested at Tree-level. Try adding loops."
GenerateAmplitude::longName         = " OutputName `1` is too long, please use a shorter string."
GenerateAmplitude::noFORM           = " Mathematica is not able to find the folder where FORM is installed. Please update the 'Environment[\"PATH\"]' with the path to installation folder of FORM via the function SetEnvironment. "
GenerateAmplitude::diagButNoAmp     = " Diagrams have been generated but not the Amplitudes."

GenerateAmplitude[initial_->final_,opts: OptionsPattern[] ]:=Block[{outPath,diagramsPath,amplitudesLog,logFile,processName,nLines,QGModel,QGModelPath,in,outh,in2,out2,QGin,QGout,nDiagrams,nDiagrams1,nDiagrams2,nDiagramsPosition,amplitudesRunOutput,optsQGDiagrams,nLoops,ListKinematics,TraceDimension,amplitudesResult,subsCleaningOutput,couplingsString,subsCleaningCouplings,KinematicInvariants,SimplifiedAmplitud,selectamplitudes,ll,ampCouplings,simpRules,gauges,GeneralModelPath,modelfiles,modelname,Loops,undefField},

  (* Checking if all the arguments are correctly given by the user or not *)

  If[!FreeQ[Join[initial, final], _?NumericQ],
    Message[GenerateAmplitude::InvalidProcess ];
    Abort[];
  ];
  
  If[ Head[AN$ModelName]===Symbol&&OptionValue[Model]==="None", Message[GenerateAmplitude::noModel]&&Abort[]; ];

  If[OptionValue[Model]==="None", 
      modelname = AN$Model, 
      
      modelname = OptionValue[Model];
      (* Quiet[LoadModel[modelname] ]; *)
      Block[{$Output = {} }, LoadModel[modelname] ];
      (* AN$Model = OptionValue[Model]  *)
    ];

  If[Head[ ToExpression[OptionValue[Loops] ] ] =!= Integer, Message[GenerateAmplitude::InvalidLoopOrder,Style["Invalid loop order",Bold,Darker@Red] ];Abort[] ];


  (* Read the fields of the given process *)

  in=(StringTrim[#,("{"|"}")])&@ToString@initial;
  outh=(StringTrim[#,("{"|"}")])&@ToString@final;
  in2=(StringReplace[#,{Whitespace->"",","->""}])&@in;
  out2=(StringReplace[#,{Whitespace->"",","->""}])&@outh;
 
  nLoops=ToExpression[OptionValue[Loops] ];

  If[ OptionValue[OutputName]==="None", processName=in2<>"_"<>out2, processName=OptionValue[OutputName] ];
  
  Which[ 
      OptionValue[Dimension]==="None"&&nLoops===0,TraceDimension=$DimensionST,
      OptionValue[Dimension]=!="None",TraceDimension=OptionValue[Dimension],
      OptionValue[Dimension]==="None"&&nLoops>0,TraceDimension=$DimensionST
  ];

  (* Message when requested 1to1 amp at tree level *)
  If[nLoops===0&&Length[initial]===1&&Length[final]===1, 

      Message[GenerateAmplitude::treeLevelOtoO];
      Return[Null];
  ];

  (* Message for undefined fields *)

  undefField = Select[Join[initial,final], ! MemberQ[Join[AN$Fields, AN$antiFields], #] &];
  If[Length[undefField] > 0, Message[GenerateAmplitude::undefinedFields,ToString[undefField],modelname]&&Abort[] ];

  SimplifiedAmplitud=OptionValue[SimplifiedOutput];
  ampCouplings=OptionValue[SelectCouplingsOrder];
  simpRules=OptionValue[SubstitutionRules];


  gaugeDefaults = Association[gaugeDefaultsList];
  gAssoc=Join[gaugeDefaults,toAssoc@OptionValue[Gauges]];
  gauges=Normal@KeyTake[gAssoc,{"QCD","EW"}];

  (* Set Off-Shell ? *)

  SetOffShell=OptionValue[OffShell];

  AN$ExternalMomenta=Array[Symbol["p"<>ToString[#] ]&,Length[initial]+Length[final] ];
  AN$InitialMomenta=Take[AN$ExternalMomenta,Length[initial] ];
  AN$FinalMomenta=Complement[AN$ExternalMomenta,AN$InitialMomenta];

  AN$InitialState=Inner[#@#2&,initial,AN$InitialMomenta,List];
  AN$FinalState=Inner[#@#2&,final,AN$FinalMomenta,List];
  QGin=(StringTrim[#,("{"|"}")])&@ToString@InputForm@AN$InitialState;
  QGout=(StringTrim[#,("{"|"}")])&@ToString@InputForm@AN$FinalState;

  process={(#[[1]][#[[2]]]&/@(Transpose@{initial,AN$InitialMomenta}))->(#[[1]][#[[2]]]&/@(Transpose@{final,AN$FinalMomenta}))};
  
  (* Import model *)

  GeneralModelPath=Global`$AnatarPath<>"/Models/"<>modelname<>"/General.m";
  If[ FileExistsQ[GeneralModelPath]===False, Message[GenerateAmplitude::ModelNotFound,modelname]&&Abort[] ];
  Import[GeneralModelPath];
  
  AN$allFields=Join[AN$Fields,AN$antiFields];

  ClassifyFieldsFO[AN$allFields];


  (* Create Directory of the process *)

  outPath=Global`$AnatarPath<>"/Outputs/"<>processName;

  If[ Not[OptionValue[RecycleAmplitude] ],

    If[DirectoryQ[outPath]==False, CreateDirectory[outPath] ];

    If[DirectoryQ["Save_"<>ToString[nLoops] ],DeleteDirectory["Save_"<>ToString[nLoops], DeleteContents->True] ];
    If[FileExistsQ["Amp_"<>ToString[nLoops]<>".frm"],DeleteFiles["Amp_"<>ToString[nLoops]<>".frm"] ]
    If[FileExistsQ["Amp_"<>ToString[nLoops]<>".log"],DeleteFiles["Amp_"<>ToString[nLoops]<>".log"] ]
    If[FileExistsQ["diagrams_" <>ToString[nLoops]<>".log"],DeleteFiles["diagrams_" <>ToString[nLoops]<>".log"] ]
    If[FileExistsQ[processName<>"_"<>ToString[nLoops] ],DeleteFiles[processName<>"_"<>ToString[nLoops] ] ];

  (* CLEAR MasterIntegrals[] and AllTopologies[] whenever GenerateAmplitudes[] is called *)

  AllTopologies[]={};
  Clear[MasterIntegrals];

    (* Generate diagrams *)

    QGModel = AN$ModelName<>"QG.qgraf";
    QGModelPath = Global`$AnatarPath<>"/Models/"<>modelname<>"/"<>QGModel;
    (* If[FileExistsQ[QGModelPath]===False,Print[Style["Error: ",Darker@Red], "QGmodel "<>QGModel<>" not found"]&&Abort[] ]; *)
    If[ FileExistsQ[QGModelPath]===False, Message[GenerateAmplitude::noQGModel,modelname]&&Abort[] ];

    If[DirectoryQ[QGModelPath<>"/"<>QGModel]===False, Run["cp "<>QGModelPath<>" "<>Global`$QGrafPath<>"/"<>QGModel] ];
    (* QGDiagrams[QGModel,outPath,initial->final,QGLoops -> OptionValue[QGLoops]] *)
    optsQGDiagrams = FilterRules[{opts}, First /@ Options[QGDiagrams] ];
    QGDiagrams[QGModel,outPath,initial->final, Sequence@@optsQGDiagrams]
    (* Is this necessary *)
    SetDirectory[outPath];
    
    logFile = Import["diagrams_"<>ToString[nLoops]<>".log", "Words"];
    nDiagramsPosition = Position[logFile, "total"] + 2;
    nDiagrams = ToExpression[(logFile[[#]]&)/@nDiagramsPosition][[1, 1]];

    If[ SubsetQ[logFile, {"error:", "line", "too", "long,", "control-file,", 
    "line", "6"}] ,Print[Style["Error: ",Darker@Red], "OutputName "<>processName<>" is too long, please use a shorter string."]&&Abort[] ];

    If[ SubsetQ[logFile, {"error:", "line", "too", "long,", "control-file,", "line", "6"}], Message[GenerateAmplitude::longName]&&Abort[] ];
    
    If[Head[nDiagrams]=!=Integer,Message[GenerateAmplitude::noDiagrams,nLoops]&&Abort[] ];
    If[nDiagrams===0,Message[GenerateAmplitude::noDiagrams2 ]&&Abort[] ];
    
    If[OptionValue[OnlyDiagrams] === True,
        Print["A total of "<>MakeGreen[ToString[nDiagrams],Boldmg->True]<>" diagrams have been generated for the process "<>MakeGreen[ToString[initial -> final ], Boldmg->True] ];
        Print["OnlyDiagrams option set to True. No amplitudes generated."];
        Return[Null]&&Abort[]
    ];
    nLines=30;
    WriteDefinitions[outPath,nLines,nDiagrams,TraceDimension,AN$Fields,AN$antiFields,AN$allFields];

    TrimAmp=OptionValue[PolarizationTrim]; (* Option to remove the Polarizations *)
      selectamplitudes=OptionValue[SelectAmplitudes]; (* Option to select some amplitudes instead of taking all the QGraf diagrams *)
      If[selectamplitudes=!="All"&&Length[selectamplitudes]=!=1, selectamplitudes = selectamplitudes];
      If[selectamplitudes=!="All"&&Length[selectamplitudes]==1, 
         {If[Head[selectamplitudes[[1]]] === Integer, selectamplitudes = {First[selectamplitudes] ;; First[selectamplitudes]}];
          If[selectamplitudes=!="All" && Head[selectamplitudes[[1]]] === Span, selectamplitudes = selectamplitudes];}];
     
      If[selectamplitudes==="All",   selectamplitudes = {1;;nDiagrams}  ];
  
  (* Check if Kinematics are defined by the user, if not they are then generated *)

  If[Head[Kinematics[processName] ] =!= Association,
  
        Kine[initial,final,AN$InitialMomenta,AN$FinalMomenta] ];


  AN$Kinematics = AN$Kinematics /. {S->SS,T->TT,U->UU};
   
   WriteKinematics[outPath,KinematicInvariants,AN$Kinematics];

  (* Change syntax of list of kinematics from FORM to mathematica *)
  AN$Kinematics = AN$Kinematics /. {Dot[p1_, Power[p2_, a_] ] :> Power[(p1 . p2), a],SS->S,TT->T,UU->U};

    (* Generate amplitudes *)

    diagramsPath = outPath<>"/"<>processName<>"_"<>ToString[nLoops];
    WriteMasterFO[outPath,nDiagrams,nLoops,diagramsPath,modelname,processName,TraceDimension,SimplifiedAmplitud,TrimAmp,selectamplitudes,ampCouplings,simpRules,gauges];

    amplitudesLog = "Amp_"<>ToString[nLoops]<>".log";

    Print["Computing amplitudes..."];

    amplitudesRunOutput = Run["form "<>outPath<>"/Amp_"<>ToString[nLoops]<>".frm > "<>amplitudesLog];

    If[amplitudesRunOutput===0, Print["A total of "<>MakeGreen[ToString[nDiagrams],Boldmg->True]<>" diagrams have been generated for the process "<>MakeGreen[ToString[initial -> final ], Boldmg->True] ] ];

    If[amplitudesRunOutput===0, Print["Amplitudes were obtained succesfully and are written in the file "<>MakeGreen[ToString[amplitudesLog] ] ];]


    If[amplitudesRunOutput===32512, Message[GenerateAmplitude::noFORM] ],

    (* To recycle the amplitude *)

    SetDirectory[outPath];

    logFile = Import["diagrams_"<>ToString[nLoops]<>".log", "Words"];
    nDiagramsPosition = Position[logFile, "total"] + 2;
    nDiagrams = ToExpression[(logFile[[#]]&)/@nDiagramsPosition][[1, 1]];
    selectamplitudes = {1;;nDiagrams}; nDiagrams1=1; nDiagrams2=nDiagrams;

  ];

    AN$CurrentProcessPath = outPath;


  (* Generate substitutions for the GCouplings *)

  subsCleaningCouplings = {
      "**"~~Shortest[___]~~"**\n":>"",
      "#"~~Shortest[___]~~"\n":>"",
      ".sort\n":>"",
      ";\n\n#endprocedure Couplings"->"",
      "id" -> "",
      "="->"->",
      ";"->",",
      "sqrt_("~~Shortest[x__]~~")":>"Sqrt["~~x~~"]"
      };

  couplingsString = Import[Global`$AnatarPath<>"/Models/"<>modelname<>"/Couplings.frm","String"];
  (*couplingsString = StringRiffle[Drop[StringSplit[couplingsString, "\n"], 10], "\n"];*)
  couplingsString = StringReplace[couplingsString, subsCleaningCouplings];
  couplingsString = StringCases[couplingsString,RegularExpression["(?m)\\b([A-Za-z]\\w*)\\s*->\\s*([^,\\n\\r}]+)\\s*(?:,|$)"]
    -> {"$1"->"$2"}];
   AN$GCouplings = Flatten@couplingsString/. s_String :> ToExpression[StringTrim[s]];

  (* Store the amplitudes to be read by Mathematica *)


     subsCleaningOutput={
         ("N"~~x:DigitCharacter..~~"_?"):>"indexN"~~x,
         ("[N"~~x:DigitCharacter..~~"_?,"):>"[indexN"~~x~~",",
         (",N"~~x:DigitCharacter..~~"_?]"):>",indexN"~~x~~"]",
         (" N"~~x:DigitCharacter..~~"_?,"):>" indexN"~~x~~",",
         (",N"~~x:DigitCharacter..~~"_?,"):>",indexN"~~x~~",",
         (",\n         N"~~x:DigitCharacter..~~"_?,"):>",indexN"~~x~~",",
         (",\n         N"~~x:DigitCharacter..~~"_?]"):>",indexN"~~x~~"]",
         (",N"~~x:DigitCharacter..~~"_?\n         ,"):>",indexN"~~x~~",",
         "*im*"->"*I*",
         " im*"->" I*",
         "*D*"->"*"<>ToString[$DimensionST]<>"*",
         " D*"->" "<>ToString[$DimensionST]<>"*",
         "d_("~~Shortest[x__]~~","~~Shortest[y__]~~")":>"Metric["<>x<>","<>y<>"]",
         "sqrt_["~~Shortest[x__]~~"]":>"Sqrt["~~x~~"]",
         "e_["~~Shortest[x__]~~"]":>"LeviCivita["~~x~~"]",
         "Levieps["~~Shortest[x__]~~"]":>"LeviCivita["~~x~~"]",
         "i_*":>"I*",
         "*i_\n":>"*I\n"
         };
        

    If[ Not@FileExistsQ["Amp_"<>ToString[nLoops]<>".log" ], Message[GenerateAmplitude::diagButNoAmp]&&Abort[] ];

  amplitudesResult = Import["Amp_"<>ToString[nLoops]<>".m", "String"];
  amplitudesResult = (StringReplace[#,{"\n"->""," "->"","AN$Amplitude"->"\nAN$Amplitude"}]&)@amplitudesResult;
  amplitudesResult = (StringReplace[#,subsCleaningOutput]&)@amplitudesResult; 
  ll=1;
  While[StringContainsQ[amplitudesResult, "_?"] && ll<5, 
        amplitudesResult = StringReplace[amplitudesResult, subsCleaningOutput];
        ll++;
        ];
  amplitudesResult = ToExpression[amplitudesResult]/. {Dot[p1_, Power[p2_, a_] ] :> Power[(p1 . p2), a]}/.AN$Kinematics;
  amplitudesResult = amplitudesResult /.{D->$DimensionST};


  amplitudesResult = Table[DiagramID[i] -> AN$Amplitude[i] , {i, Evaluate[Flatten[List @@ (selectamplitudes) /. Span -> Range]]}]/.{im->I};
  

  Print[MakeGreen["Done!",Boldmg->True] ];
  nDiagrams = Length[Evaluate[Flatten[List @@ (selectamplitudes) /. Span -> Range] ] ];


  (*** Changed to associations and Claude's convention names ***)
  Return[Association[{Process -> process, Total -> selectamplitudes, Name ->  processName,  LoopOrder ->  nLoops, Kinematics ->   AN$Kinematics, Amplitudes -> amplitudesResult}] ]



  (* Generate Couplings substitutions  *)

   
]



(**************************************************)
(*  Function that writes the amplitude conjugate  *)
(**************************************************)

Options[GenerateAmplitudeConjugate] := Join[Options[QGDiagrams], {Model -> "None",OutputName->"None",Dimension -> "None",SimplifiedOutput-> False, Gauges->gaugeDefaultsList, SelectAmplitudes-> "All",SelectCouplingsOrder->"All",SubstitutionRules->"None",PolarizationTrim->False, OffShell->False}]

GenerateAmplitudeConjugate::noModel           = " No model has been specified. Load a model using the option Model of GenerateAmplitude or the function LoadModel.";
GenerateAmplitudeConjugate::ModelNotFound    = " Model \"`1`\" not found. ";
GenerateAmplitudeConjugate::undefinedFields   = " The fields `1` are not defined in the model `2`";
GenerateAmplitudeConjugate::InvalidProcess    = " Wrong syntax in the specified process, numbers cannot be fields.";
GenerateAmplitudeConjugate::noQGModel         = " the qgraf model file of model \"`1`\" not found.";
GenerateAmplitudeConjugate::InvalidLoopOrder  = " InvalidLoopOrder:`1`";
GenerateAmplitudeConjugate::InvalidModel      = " InvalidModel:`1`";
GenerateAmplitudeConjugate::noDiagrams        = " Diagrams not generated, check the file diagrams_`1`.log for more information.";
GenerateAmplitudeConjugate::noDiagrams2       = " Zero diagrams generated for the given model, fields and selection of coupligns order."
GenerateAmplitudeConjugate::noFORM           = " Mathematica is not able to find the folder where FORM is installed. Please update the 'Environment[\"PATH\"]' with the path to installation folder of FORM via the function SetEnvironment. "

GenerateAmplitudeConjugate[initial_->final_, opts: OptionsPattern[] ]:=Block[{outPath,diagramsPath,amplitudesLog,logFile,process,processName,nLines,QGModel,QGModelPath,in,outh,in2,out2,QGin,QGout,nDiagrams,nDiagramsPosition,amplitudesRunOutput,optsQGDiagrams,nLoops,TraceDimension,amplitudesResult,subsCleaningOutput,SimplifiedAmplitud,gauges,selectamplitudes,ampCouplings,simpRules,modelname,GeneralModelPath},

(* Checking if all the arguments are correctly given by the user or not *)

  If[!FreeQ[Join[initial, final], _?NumericQ],
    Message[GenerateAmplitudeConjugate::InvalidProcess ];
    Abort[];
  ];

  If[ Head[AN$ModelName]===Symbol&&OptionValue[Model]==="None", Message[GenerateAmplitudeConjugate::noModel]&&Abort[]; ];


  If[OptionValue[Model]==="None", 
      modelname = AN$Model, 
      
      modelname = OptionValue[Model];

      Block[{$Output = {} }, LoadModel[modelname] ];

    ];


  If[Head[ ToExpression[OptionValue[Loops] ] ] =!= Integer, Message[GenerateAmplitudeConjugate::InvalidLoopOrder, Style["Invalid loop order",Bold,Darker@Red] ];Abort[] ];

  (* Message for undefined fields *)

  undefField = Select[Join[initial,final], ! MemberQ[Join[AN$Fields, AN$antiFields], #] &];
  If[Length[undefField] > 0, Message[GenerateAmplitudeConjugate::undefinedFields,ToString[undefField],modelname]&&Abort[] ];

  (* Set Off-Shell ? *)

  SetOffShell=OptionValue[OffShell];

  (* Read the fields of the given process *)

  in=(StringTrim[#,("{"|"}")])&@ToString@initial;
  outh=(StringTrim[#,("{"|"}")])&@ToString@final;
  in2=(StringReplace[#,{Whitespace->"",","->""}])&@in;
  out2=(StringReplace[#,{Whitespace->"",","->""}])&@outh;

  nLoops=ToExpression[OptionValue[Loops] ];

  If[ OptionValue[OutputName]==="None", processName=in2<>"_"<>out2, processName=OptionValue[OutputName] ];
  Which[ 
      OptionValue[Dimension]==="None"&&nLoops===0,TraceDimension=$DimensionST,
      OptionValue[Dimension]=!="None",TraceDimension=OptionValue[Dimension],
      OptionValue[Dimension]==="None"&&nLoops>0,TraceDimension=$DimensionST
      ];
  SimplifiedAmplitud=OptionValue[SimplifiedOutput];
  ampCouplings=OptionValue[SelectCouplingsOrder];
  simpRules=OptionValue[SubstitutionRules];

  gaugeDefaults = Association[gaugeDefaultsList];
  gAssoc=Join[gaugeDefaults,toAssoc@OptionValue[Gauges]];
  gauges=Normal@KeyTake[gAssoc,{"QCD","EW"}];

  AN$ExternalMomenta=Array[Symbol["p"<>ToString[#] ]&,Length[initial]+Length[final] ];
  AN$InitialMomenta=Take[AN$ExternalMomenta,Length[initial] ];
  AN$FinalMomenta=Complement[AN$ExternalMomenta,AN$InitialMomenta];

  AN$InitialState=Inner[#@#2&,initial,AN$InitialMomenta,List];
  AN$FinalState=Inner[#@#2&,final,AN$FinalMomenta,List];
  QGin=(StringTrim[#,("{"|"}")])&@ToString@InputForm@AN$InitialState;
  QGout=(StringTrim[#,("{"|"}")])&@ToString@InputForm@AN$FinalState;


  process={(#[[1]][#[[2]]]&/@(Transpose@{initial,AN$InitialMomenta}))->(#[[1]][#[[2]]]&/@(Transpose@{final,AN$FinalMomenta}))};

  (* Import model *)

  Import[Global`$AnatarPath<>"/Models/"<>modelname<>"/General.m"];

  GeneralModelPath=Global`$AnatarPath<>"/Models/"<>modelname<>"/General.m";
  If[ FileExistsQ[GeneralModelPath]===False, Message[GenerateAmplitudeConjugate::ModelNotFound,modelname]&&Abort[] ];
  Import[GeneralModelPath];
  
  AN$allFields=Join[AN$Fields,AN$antiFields];

  ClassifyFieldsFO[AN$allFields];


  (* Create Directory of the process *)

  outPath=Global`$AnatarPath<>"/Outputs/"<>processName;

  If[DirectoryQ[outPath]==False, CreateDirectory[outPath] ];

  (* Generate diagrams *)

  QGModel = AN$ModelName<>"QG.qgraf";
  QGModelPath = Global`$AnatarPath<>"/Models/"<>modelname<>"/"<>QGModel;
  
  If[DirectoryQ[QGModelPath<>"/"<>QGModel]===False, Run["cp "<>QGModelPath<>" "<>Global`$QGrafPath<>"/"<>QGModel] ];

  optsQGDiagrams = FilterRules[{opts}, First /@ Options[QGDiagrams] ];
  QGDiagrams[QGModel,outPath,initial->final,Sequence@@optsQGDiagrams];

  SetDirectory[outPath];

  If[DirectoryQ["SaveC_"<>ToString[nLoops] ],DeleteDirectory["SaveC_"<>ToString[nLoops], DeleteContents->True] ];
  If[FileExistsQ["AmpC_"<>ToString[nLoops]<>".frm"],DeleteFiles["AmpC_"<>ToString[nLoops]<>".frm"] ];
  If[FileExistsQ["AmpC_"<>ToString[nLoops]<>".log"],DeleteFiles["AmpC_"<>ToString[nLoops]<>".log"] ];
  If[FileExistsQ["diagrams_" <>ToString[nLoops]<>".log"],DeleteFiles["diagrams_" <>ToString[nLoops]<>".log"] ];
  If[FileExistsQ[processName<>"_"<>ToString[nLoops] ],DeleteFiles[processName<>"_"<>ToString[nLoops] ] ];

  logFile = Import["diagrams_" <>ToString[nLoops]<>".log", "Words"];
  nDiagramsPosition = Position[logFile, "total"] + 2;
  nDiagrams = ToExpression[(logFile[[#]]&)/@nDiagramsPosition][[1, 1]];

  nLines=30;
  If[!FileExistsQ["Definitions.h"],  WriteDefinitions[outPath,nLines,nDiagrams,TraceDimension,AN$Fields,AN$antiFields,AN$allFields]; ];

  If[Head[Kinematics[processName] ] =!= Association,
  
        Kine[initial,final,AN$InitialMomenta,AN$FinalMomenta]

   ];

  AN$Kinematics = AN$Kinematics /. {S->SS,T->TT,U->UU};
  WriteKinematics[outPath,KinematicInvariants,AN$Kinematics];
  AN$Kinematics = AN$Kinematics /. {Dot[p1_, Power[p2_, a_] ] :> Power[(p1 . p2), a],SS->S,TT->T,UU->U};

  If[Head[nDiagrams]=!=Integer,Message[GenerateAmplitudeConjugate::noDiagrams,nLoops]&&Abort[] ];  
  If[nDiagrams===0,Message[GenerateAmplitudeConjugate::noDiagrams2 ]&&Return[Null] ];

  TrimAmp=OptionValue[PolarizationTrim]; (* Option to remove the Polarizations *)
  selectamplitudes=OptionValue[SelectAmplitudes]; (* Option to select some amplitudes instead of taking all the QGraf diagrams *)
  If[selectamplitudes=!="All"&&Length[selectamplitudes]=!=1, selectamplitudes = selectamplitudes];
  If[selectamplitudes=!="All"&&Length[selectamplitudes]==1, 
     {If[Head[selectamplitudes[[1]]] === Integer, selectamplitudes = {First[selectamplitudes] ;; First[selectamplitudes]}];
      If[selectamplitudes=!="All" && Head[selectamplitudes[[1]]] === Span, selectamplitudes = selectamplitudes];}];
  If[selectamplitudes==="All", selectamplitudes = {1;;nDiagrams}];
  (* Generate amplitudes *)

  diagramsPath = outPath<>"/"<>processName<>"_"<>ToString[nLoops];
  WriteMasterCFO[outPath,nDiagrams,nLoops,diagramsPath,modelname,processName,TraceDimension,SimplifiedAmplitud,TrimAmp,selectamplitudes,ampCouplings,simpRules,gauges];

  amplitudesCLog = "AmpC_"<>ToString[nLoops]<>".log";

  Print["Computing amplitudes..."];

  amplitudesCRunOutput = Run["form "<>outPath<>"/AmpC_"<>ToString[nLoops]<>".frm > "<>amplitudesCLog];
  
    (* If[amplitudesCRunOutput===0, Print[Style["The total number of diagrams generated are "<>ToString[nDiagrams]<>" for the process"<>ToString[process],Bold,Darker@Green ] ]]; *)

    If[amplitudesCRunOutput===0, Print["A total of "<>MakeGreen[ToString[nDiagrams],Boldmg->True]<>" diagrams have been generated for the process "<>MakeGreen[ToString[initial -> final ], Boldmg->True] ] ];

    If[amplitudesCRunOutput===0, Print["Amplitudes were obtained succesfully and are written in the file "<>MakeGreen[ToString[amplitudesCLog] ] ];]


    If[amplitudesRunOutput===32512, Message[GenerateAmplitude::noFORM] ];


  subsCleaningOutput={
         ("N"~~x:DigitCharacter..~~"_?"):>"indexN"~~x,
         ("[N"~~x:DigitCharacter..~~"_?,"):>"[indexN"~~x~~",",
         (",N"~~x:DigitCharacter..~~"_?]"):>",indexN"~~x~~"]",
         (" N"~~x:DigitCharacter..~~"_?,"):>" indexN"~~x~~",",
         (",N"~~x:DigitCharacter..~~"_?,"):>",indexN"~~x~~",",
         (",\n         N"~~x:DigitCharacter..~~"_?,"):>",indexN"~~x~~",",
         (",\n         N"~~x:DigitCharacter..~~"_?]"):>",indexN"~~x~~"]",
         (",N"~~x:DigitCharacter..~~"_?\n         ,"):>",indexN"~~x~~",",
         "*im*"->"*I*",
         " im*"->" I*",
         "*D*"->"*"<>ToString[$DimensionST]<>"*",
         " D*"->" "<>ToString[$DimensionST]<>"*",
         "d_("~~Shortest[x__]~~","~~Shortest[y__]~~")":>"Metric["<>x<>","<>y<>"]",
         "sqrt_["~~Shortest[x__]~~"]":>"Sqrt["~~x~~"]",
         "e_["~~Shortest[x__]~~"]":>"LeviCivita["~~x~~"]",
         "Levieps["~~Shortest[x__]~~"]":>"LeviCivita["~~x~~"]",
         "i_*":>"I*",
         "*i_\n":>"*I\n"
         };     



  amplitudesResult = Import["AmpC_"<>ToString[nLoops]<>".m", "String"];
  amplitudesResult = (StringReplace[#,{"\n"->""," "->"","AN$AmplitudeC"->"\nAN$AmplitudeC"}]&)@amplitudesResult;
  amplitudesResult = (StringReplace[#,subsCleaningOutput]&)@amplitudesResult; 
  ll=1;
  While[StringContainsQ[amplitudesResult, "_?"] && ll<5, 
        amplitudesResult = StringReplace[amplitudesResult, subsCleaningOutput];
        ll++;
        ];
  amplitudesResult = ToExpression[amplitudesResult]/. {Dot[p1_, Power[p2_, a_] ] :> Power[(p1 . p2), a]}/.AN$Kinematics;
  amplitudesResult = amplitudesResult /.{D->$DimensionST};


  amplitudesResult = Table[DiagramID[i] -> AN$AmplitudeC[i] , {i, Evaluate[Flatten[List @@ (selectamplitudes) /. Span -> Range] ]}]/.{im->I};


  Print[MakeGreen["Done!",Boldmg->True] ];
  nDiagrams = Length[Evaluate[Flatten[List @@ (selectamplitudes) /. Span -> Range] ] ];


(*** Changed to associations and Claude's convention names ***)

Return[Association[{Process -> process, Total -> selectamplitudes, Name ->  processName,  LoopOrder ->  nLoops, Kinematics -> AN$Kinematics, Amplitudes -> amplitudesResult}] ]

]

transform[input_String] := Module[
  {lhs1, lhs, rhs, lhsParts, rhsParts},
  {lhs, rhs} = 
   StringCases[input, RegularExpression["\\{(.*?)\\}"] -> "$1"];
  
  lhsParts = StringSplit[lhs, ","];
  rhsParts = StringSplit[rhs, ","];
  
  lhsParts = 
   StringReplace[lhsParts, RegularExpression["\\[.*?\\]"] -> ""];
  rhsParts = 
   StringReplace[rhsParts, RegularExpression["\\[.*?\\]"] -> ""];
  
  lhsParts = StringTrim[StringJoin[lhsParts], "{"];
  lhsParts = StringReplace[lhsParts, " " -> ""];
  rhsParts = StringJoin[rhsParts];
  
  rhsParts = StringReplace[rhsParts, " " -> ""];
  
  lhsParts <> "_" <> rhsParts
  ]

(****************************************************)
(*     Function that simplifies auxillary momentum  *)
(*     part of polarization sum                     *)
(****************************************************)

SetAttributes[ReorderDot, HoldAll]

ReorderDot[EXPR_] := ReplaceAll[EXPR, Dot[a___, b_, c___] /; OrderedQ[{b, c}] === False :> ReorderDot[Dot @@ Sort[{a, b, c}] ] ];

PolarizationSimplification[expr_] :=
 Module[{expr1, ListOfDen, Listofnn, SimpDen, Simpofnn, SimpDenSubs,
   Rulesnn, result, rhskinematics, lhskinematics, sol, eq,final,invariants},
  expr1 = expr /. AN$Kinematics;

  (*Ordering of the Dot[,]*)
  expr1=ReorderDot[expr1];

  ListOfDen = DeleteDuplicates@Cases[expr1, Den[a_, b_], Infinity];
  Listofnn =
   DeleteDuplicates@
    Cases[expr1, Dot[nn, b_] | Dot[a_, nn], Infinity];
  Simpofnn =
   Distribute[#] & /@ (Listofnn /. AN$Kinematics) /.
      Dot[a_, -b_] :> -Distribute[Dot[a, b]] /.
     Dot[-a_, -b_] :> Dot[a, b] /.
    Dot[-a_, b_] :> -Distribute[Dot[a, b]];
  Rulesnn = Thread[Listofnn -> Simpofnn];
  SimpDen = (Distribute[#] & /@ (ListOfDen /. {Den[a_,
              b_] :> (Dot[a, a] - b^2)})) /. {Plus[a_, b_] :>
         Plus[Distribute[a], Distribute[b]]} /.
      Dot[a_, -b_] :> -Distribute[Dot[a, b]] /.
     Dot[-a_, -b_] :> Dot[a, b] /.
    Dot[-a_, b_] :> -Distribute[Dot[a, b]];
  SimpDenSubs = Thread[Rule[ListOfDen, 1/SimpDen]];
  lhskinematics = (Distribute[
           Dot[(Last[AN$Kinematics][[1]]), (Last[AN$Kinematics][[
              1]])]] /. Dot[a_, -b_] :> -Distribute[Dot[a, b]] /.
         Dot[-a_, -b_] :> Dot[a, b] /.
        Dot[-a_, b_] :> -Distribute[Dot[a, b]] /. AN$Kinematics /.
      Dot[a_, b_] :> Dot[b, a] /. AN$Kinematics);
  rhskinematics = (Distribute[
           Dot[(Last[AN$Kinematics][[2]]), (Last[AN$Kinematics][[
              2]])]] /. Dot[a_, -b_] :> -Distribute[Dot[a, b]] /.
         Dot[-a_, -b_] :> Dot[a, b] /.
        Dot[-a_, b_] :> -Distribute[Dot[a, b]] /. AN$Kinematics /.
      Dot[a_, b_] :> Dot[b, a] /. AN$Kinematics);
  invariants = DeleteDuplicates@Cases[Last /@ (Most@AN$Kinematics), _Symbol, Infinity];
  If[invariants =!= {}, sol = Solve[lhskinematics == rhskinematics, invariants[[1]]]];
  If[sol==={{}} , 
	result = Expand[expr1/.SimpDenSubs/.Rulesnn/.AN$Kinematics /.Dot[a_, b_] :> Dot[b, a]/; !(MatchQ[a, nn] || MatchQ[b, nn])/.AN$Kinematics/.Flag->1], 
  {eq = First[Solve[((Last[AN$Kinematics][[1]])^2 /. AN$Kinematics) == rhskinematics, First[sol][[1,1]] ]] ;
  result = Expand[ expr1 /. SimpDenSubs /. Rulesnn /. AN$Kinematics /.  Dot[a_, b_] :> Dot[b, a] /.  AN$Kinematics/.Flag->1/.eq]}];
  Return[result]]

(****************************************************)
(*     Function that writes the amplitude square    *)
(****************************************************)

Options[GenerateAmplitudeSquare] := DeleteDuplicates@Join[Options[GenerateAmplitude],Options[GenerateAmplitudeConjugate], {nnVectorSimplification -> False, OverallFactor->"None",CasimirValues -> False,SubstitutionRules->"None",CouplingExpand -> True} ]

GenerateAmplitudeSquare::undefinedFields  = " The fields `1` are not defined in the model `2`";
GenerateAmplitudeSquare::InvalidProcess   = " Wrong syntax in the specified process, numbers cannot be fields.";
GenerateAmplitudeSquare::InvalidModel     = "InvalidModel: `1`";
GenerateAmplitudeSquare::kineMismatch     = " The kinematics of the input amplitude and amplitude conjugate are different."
GenerateAmplitudeSquare::processMismatch = " The amplitude and amplitude conjugate do not seem to correspond to the same process. Their respective Process key are `1` and `2`."
GenerateAmplitudeSquare::noFORM           = " Mathematica is not able to find the folder where FORM is installed. Please update the 'Environment[\"PATH\"]' with the path to installation folder of FORM via the function SetEnvironment. "
GenerateAmplitudeSquare::undefinedVar     = " The variables `1` are undeclared. Try adding those variables manually to the corresponding category in the model file General.m "

GenerateAmplitudeSquare[AmpOption0_,AmpCOption0_, opts: OptionsPattern[] ]:=Block[{outPath,diagramsPath,amplitudesLog,logFile,processName,nLines,QGModel,QGModelPath,in,outh,in2,out2,QGin,QGout,nDiagrams1,nDiagrams2,nDiagramsC1,nDiagramsC2,nLoops,nLoopsC,AmpName,AmpCName,ListKinematics,KinematicInvariants,Output,subsCleaningOutput,SelectLines,UndeclaredSymbols,AmpSqprocess,AmpProcess,AmpCProcess,overallfactors,model,casimiroption,AmpSqProcess,simpRules,couplingexpand,amplitudesSqRunOutput,amplitudesSqLog},


(* Checking if all the arguments are correctly given by the user or not *)

  model = AN$Model;
  modelFiles = Select[{"Couplings.frm","General.m","Polarizations.frm","Propagators.frm",model<>"QG.qgraf","Vertices.frm"}, !FileExistsQ[FileNameJoin[{Global`$AnatarPath<>"/Models/"<>model, #}] ] &];
  If[Head[model] =!= String || Length[modelFiles]>0,
    Message[GenerateAmplitudeSquare::InvalidModel, Style["Please check the model name or the model folder in "<>Global`$AnatarPath<>"/Models/"<>model, Bold, Darker@Red] ];
    Abort[]
  ];


  AmpOption = ConvertFromAmplitudeAssociation[AmpOption0];
  AmpCOption = ConvertFromAmplitudeAssociation[AmpCOption0];

  (* Read the fields of the given process *)

  AmpProcess = AmpOption[[1]];
  AmpCProcess  = AmpCOption[[1]];

  If[ AmpProcess === AmpCProcess, AmpSqProcess = AmpProcess, Message[GenerateAmplitudeSquare::processMismatch,ToString[AmpProcess],ToString[AmpCProcess] ]&&Abort[] ];
 
  initial = AmpSqProcess[[1,1]];
  final = AmpSqProcess[[1,2]];

  in=(StringTrim[#,("{"|"}")])&@ToString@initial;
  outh=(StringTrim[#,("{"|"}")])&@ToString@final;
  in2=(StringReplace[#,{Whitespace->"",","->""}])&@in;
  out2=(StringReplace[#,{Whitespace->"",","->""}])&@outh;

  AN$InitialMomenta=Take[AN$ExternalMomenta,Length[initial] ];
  AN$FinalMomenta=Complement[AN$ExternalMomenta,AN$InitialMomenta];

  AmpSqprocess=in2<>"_"<>out2;

  nLoops = AmpOption[[4,2]];
  nLoopsC = AmpCOption[[4,2]];
 
  overallfactors=OptionValue[OverallFactor];
  casimiroption = OptionValue[CasimirValues];
  simpRules=OptionValue[SubstitutionRules];
  couplingexpand = OptionValue[CouplingExpand];


  Which[ 
      OptionValue[Dimension]==="None"&&nLoops===0&&nLoopsC===0,TraceDimension=$DimensionST,
      OptionValue[Dimension]=!="None",TraceDimension=OptionValue[Dimension],
      OptionValue[Dimension]==="None"&&(nLoops>0||nLoopsC>0),TraceDimension=$DimensionST
      ];
  

  AmpName     = AmpOption[[3,2]];
  AmpCName    = AmpCOption[[3,2]];
  nDiagrams   = AmpOption[[2,2]];
  nDiagramsC  = AmpCOption[[2,2]];


   nDiagrams1     =  AmpOption[[6,2]][[1,1]]/.{DiagramID[i_]:>i};
   nDiagrams2     =  AmpOption[[6,2]][[-1,1]]/.{DiagramID[i_]:>i};
   nDiagramsC1     =  AmpCOption[[6,2]][[1,1]]/.{DiagramID[i_]:>i};
   nDiagramsC2     =  AmpCOption[[6,2]][[-1,1]]/.{DiagramID[i_]:>i};

  AmpProcess = transform[ToString[AmpOption[[1]]] ];
  AmpCProcess = transform[ToString[AmpCOption[[1]]] ];


  AmpPath=Global`$AnatarPath<>"/Outputs/"<>AmpName;
  AmpCPath=Global`$AnatarPath<>"/Outputs/"<>AmpCName;

  nLines=30;
  
 (* WriteDefinitions[outPath,nLines,AN$Fields,AN$antiFields,AN$allFields]; *)  

  SetDirectory[AmpPath];

  (* Sort out Kinematics *)

  If[AmpOption0[Kinematics] =!= AmpCOption0[Kinematics], Message[GenerateAmplitudeSquare::kineMismatch]&&Abort[] ];


  (* ANDRES *)
  If[Head[Kinematics[AmpName] ] =!= Association,

      AN$Kinematics=AmpOption0[Kinematics];

  ];
  
  AN$Kinematics = AN$Kinematics /. {S->SS,T->TT,U->UU};
  WriteKinematics[AmpPath,KinematicInvariants,AN$Kinematics];
  AN$Kinematics = AN$Kinematics /. {Dot[p1_, Power[p2_, a_] ] :> Power[(p1 . p2), a],SS->S,TT->T,UU->U};

  (* Generate amplitudes square*)

  WriteMasterSqFO[model,AmpPath,AmpCPath,nDiagrams,nDiagramsC,nDiagrams1,nDiagrams2,nDiagramsC1,nDiagramsC2,nLoops,nLoopsC,TraceDimension,overallfactors,casimiroption,simpRules,couplingexpand];

  amplitudesSqLog = "AmpSq_"<>ToString[nLoops]<>ToString[nLoopsC]<>".log";
  amplitudesSqRunOutput = Run["form "<>AmpPath<>"/AmpSq_"<>ToString[nLoops]<>ToString[nLoopsC]<>".frm > "<>amplitudesSqLog];
  
  If[amplitudesSqRunOutput===0, Print["Amplitude Square were obtained succesfully and written in "<>ToString[Style[amplitudesSqLog,Darker@Green],StandardForm] ] ];
  
  If[amplitudesRunOutput===32512, Message[GenerateAmplitude::noFORM] ];

  Output = Import[Global`$AnatarPath<>"/Outputs/"<>AmpName<>"/AmpSq_"<>ToString[nLoops]<>ToString[nLoopsC]<>".m","String"];


        subsCleaningOutput={
         ("N"~~x:DigitCharacter..~~"_?"):>"indexN"~~x,
         ("[N"~~x:DigitCharacter..~~"_?,"):>"[indexN"~~x~~",",
         (",N"~~x:DigitCharacter..~~"_?]"):>",indexN"~~x~~"]",
         (" N"~~x:DigitCharacter..~~"_?,"):>" indexN"~~x~~",",
         (",N"~~x:DigitCharacter..~~"_?,"):>",indexN"~~x~~",",
         (",\n         N"~~x:DigitCharacter..~~"_?,"):>",indexN"~~x~~",",
         (",\n         N"~~x:DigitCharacter..~~"_?]"):>",indexN"~~x~~"]",
         (",N"~~x:DigitCharacter..~~"_?\n         ,"):>",indexN"~~x~~",",
         "*im*"->"*I*",
         " im*"->" I*",
         "d_("~~Shortest[x__]~~","~~Shortest[y__]~~")":>"Metric["<>x<>","<>y<>"]",
         "sqrt_["~~Shortest[x__]~~"]":>"Sqrt["~~x~~"]",
         "e_["~~Shortest[x__]~~"]":>"LeviCivita["~~x~~"]",
         "Levieps["~~Shortest[x__]~~"]":>"LeviCivita["~~x~~"]",
         "i_*":>"I*",
         "*i_\n":>"*I\n",
         "*D*"->"*"<>ToString[$DimensionST]<>"*",
         " D*"->" "<>ToString[$DimensionST]<>"*",
         "SS"->"S",
         "TT"->"T",
         "UU"->"U",
         "sin_[" ~~ Shortest[x__] ~~ "]" :> "Sin[" ~~ x ~~ "]",
         "cos_[" ~~ Shortest[x__] ~~ "]" :> "Cos[" ~~ x ~~ "]"
         };

  (* Useful function to select lines containing a substring *)

   SelectLines[text_, search_]:=StringJoin@Riffle[StringCases[StringSplit[text,"\n"],___~~search~~___]/.{}->Nothing,"\n"];

  (*  Conditional  - Errors in computation of Square Amplitude *)

   If[StringContainsQ[Output,"Errors in Loop"],

      (* If errors are found *)

      If[StringContainsQ[Output,"Undeclared variable"], (* Symbols are not being defined *)

          UndeclaredSymbols=(StringReplace[#,___~~"Undeclared variable "~~x___:>x]&)@(StringSplit[#,"\n"]&)@SelectLines[Output,"Undeclared variable"];
          UndeclaredSymbols = ToExpression/@UndeclaredSymbols;

          Message[GenerateAmplitudeSquare::undefinedVar]&&Abort[]

        ],


      (* Else *)

      AN$Kinematics = AN$Kinematics /. {Dot[p1_, Power[p2_, a_] ] :> Power[Dot[p1 , p2], a],SS->S,TT->T,UU->U};

        Output = (StringReplace[#,{"\n"->""," "->"","AN$AmpSq"->"\nAN$AmpSq"}]&)@Output;
        Output = (StringReplace[#,subsCleaningOutput]&)@Output; 

        ll=1;
        While[StringContainsQ[Output, "_?"] && ll<5, 
          Output = StringReplace[Output, subsCleaningOutput];
          ll++;
        ];
        Output = ToExpression[Output]/. {Dot[p1_, Power[p2_, a_] ] :> Power[Dot[p1 , p2], a]}/.AN$Kinematics;


        Output = Table[ AN$AmpSq[i,jj], {i, Evaluate[Flatten[List @@ (nDiagrams) /. Span -> Range]]}, {jj,Evaluate[Flatten[List @@ (nDiagramsC) /. Span -> Range]]}];
        Output = Total[ Output, 2];
        Output = Output/.{D->$DimensionST,im->I}/.{Dot[p1_, Power[p2_, a_] ] :> Power[Dot[p1 , p2], a]};

(*** Changed to associations and Claude's convention names ***)


  If[OptionValue[nnVectorSimplification]===True,
      Return[Association[{Process -> process,  Name ->  AmpName,  LoopOrder ->  nLoops + nLoopsC, Kinematics -> AN$Kinematics, Amplitudes -> PolarizationSimplification[Output]}] ],
      Return[Association[{Process -> process,  Name ->  AmpName,  LoopOrder ->  nLoops + nLoopsC, Kinematics -> AN$Kinematics, Amplitudes -> Output/.Flag->1}] ] ];

   ]

];

Options[FormToReadable]={PolarizationTrim->False};

FormToReadable[amplitudes_List,opts: OptionsPattern[] ]:=Module[{expr,expr2,nLoops,process,Factor0,FactorGammaM,SubstitutionsTrim,FinalAmps},
	
	process=amplitudes[[1]];
	nLoops=amplitudes[[4,2]];
	AN$LoopMomenta=Table[Symbol["k"<>ToString@i],{i,nLoops}];
	AN$ExternalMomenta=(Join[#[[1,1]],#[[1,2]]]&)@(process/.{F_[x_]/;(F=!=List):>x});

	AN$Momenta=Join[AN$ExternalMomenta,AN$LoopMomenta];
	AN$Lorentz=Table[Symbol["Lor"<>ToString@i],{i,10}];
	AN$Lorentz2=Table[Symbol["\[Mu]"<>ToString@i],{i,10}];
	AN$iLorentz=Table[Symbol["iLor"<>ToString@i],{i,10}];
	AN$iLorentz2=Table[Symbol["\[Nu]"<>ToString@i],{i,10}];
	

	expr=amplitudes[[5,;;,2]];

	FinalAmps={};

Do[
	expr2=expr[[ii]];

	If[expr2===0,

	  FinalAmps=AppendTo[FinalAmps,0],


	  expr2=Level[expr2,1]/.{GammaM[k_,Spin1_,Spin2_]+MM_ GammaM[1,Spin1_,Spin2_]:>GammaM[k+MM,Spin1,Spin2]};
	  expr2=Times@@expr2;

	  (* Split the amplitude into the Dirac matrices factor and the rest to speed up the substitutions *)
	  Factor0=Select[expr2,FreeQ[#,GammaM]&];
	  FactorGammaM=Select[expr2,!FreeQ[#,GammaM]&];

	  If[expr2=!=Factor0*FactorGammaM||Head[expr2]===List,Print[Style["Error: ",Darker@Red],"The amplitude could not be brought into a more readable form"]&&Abort[] ];


    FactorGammaM=Expand[FactorGammaM]//.Times[aa___,GammaM[x__,Spin1_,iSpin_],cc___,GammaM[y__,iSpin_,Spin2_],ee___]:>Times[aa,cc,GammaM[x,y,Spin1,Spin2],ee];
    FactorGammaM=FactorGammaM//.{GammaM[x___,1,y___,Spin1_,Spin2_]:>GammaM[x,y,Spin1,Spin2],GammaM[x___,1,y___]:>GammaM[x,y]}//Simplify;


	  expr2=Factor0*FactorGammaM/.{GammaM[x___,iSpin_,iSpin_]:>DiracTrace[x]};

	  (*Remove Spinors if PolarizationTrim option set to True*)

	  (*THIS IS NOT SAFE. MISTAKENLY SUBS PolV[k1,p1,0,Gluon1]->1 *)

	  SubstitutionsTrim={
      PolV[Lor_,y___]/;(MemberQ[AN$Lorentz,Lor]||MemberQ[AN$iLorentz,Lor]):>1,
      PolV[k_,p_,y___]/;(MemberQ[AN$Momenta,k]):>FourVector[k,Sequence@@AN$Lorentz[[Position[AN$Momenta,p][[1]]]] ],
	    SpinorU[___]:>1,
	    SpinorV[___]:>1,
	    SpinorUbar[___]:>1,
	    SpinorVbar[___]:>1
	  };

	  If[OptionValue[PolarizationTrim]===True,expr2 = expr2/.SubstitutionsTrim];

	  FinalAmps=AppendTo[FinalAmps,expr2]
    ],

  {ii,1,Length[expr]}
];

Return[FinalAmps];

]


Options[FormToFeynCalc]=Options[FormToReadable];

FormToFeynCalc[amplitudes_,opts: OptionsPattern[] ]:=Block[{expr,expr2,Factor0,FactorT,AN$Lorentz,AN$Lorentz2,AN$iLorentz,AN$iLorentz2,DiracSubstitutions,SUNSubstitutions,SpinorsSubstitutions,LorentzSubstitutions,FinalAmps,trimValue,SpinorContractionSubs},

  trimValue=OptionValue[PolarizationTrim];

  If[Head[amplitudes]===List,
    expr=FormToReadable[amplitudes,PolarizationTrim -> trimValue],
    expr={amplitudes}];
  
  FinalAmps={};
  
  Do[
  
    expr2=expr[[ii]];

    If[expr2===0,

      FinalAmps=AppendTo[FinalAmps,0],
  
      (*Lorentz *)
  
      LorentzSubstitutions = {
      Dot[k1_,k2_^2]:>Pair[Momentum[k1,D],Momentum[k2,D] ]^2,
      Dot[k1_,k2_]:>Pair[Momentum[k1,D],Momentum[k2,D] ]
      };
  
      expr2=expr2//.LorentzSubstitutions;
  
       (*Dirac Algebra*)
  
      DiracSubstitutions={
      DiracTrace[x___,5,y___] :> DiracTrace[x,GA5,y],
      DiracTrace[x___,k_+mm_,y___]/;(MemberQ[AN$Masses,mm,2]&&Head[k]=!=GSD):>DiracTrace[x,GSD[k]+mm,y],
      DiracTrace[x___,k_,y___]/;(MemberQ[AN$Momenta,k]):>DiracTrace[x,GSD[k],y],
      DiracTrace[x___,mu_,y___]/;(MemberQ[AN$Lorentz,mu]&&Head[mu]=!=GAD):>DiracTrace[x,GAD[Sequence@@AN$Lorentz2[[Position[AN$Lorentz,mu][[1]]]]],y],
      DiracTrace[x___,mu_,y___]/;(MemberQ[AN$iLorentz,mu]&&Head[mu]=!=GAD):>DiracTrace[x,GAD[Sequence@@AN$iLorentz2[[Position[AN$iLorentz,mu][[1]]]]],y],
      GammaM[x___,5,y___] :> GammaM[x,GA5,y],
      GammaM[x___,k_+mm_,y___]/;(MemberQ[AN$Masses,mm,2]&&Head[k]=!=GSD):>GammaM[x,GSD[k]+mm,y],
      GammaM[x___,k_,y___]/;(MemberQ[AN$Momenta,k]&&Head[k]=!=GSD):>GammaM[x,GSD[k],y],
      GammaM[x___,sum_,y___]/;((DeleteDuplicates[#]&)@(MemberQ[Abs/@AN$Momenta,#]&/@Abs/@Level[sum,1])&&Head[sum]=!=GSD)==={True}:>GammaM[x,GSD[sum],y],
      GammaM[x___,mu_,y___]/;(MemberQ[AN$Lorentz,mu]&&Head[mu]=!=GAD):>GammaM[x,GAD[Sequence@@AN$Lorentz2[[Position[AN$Lorentz,mu][[1]]]]],y],
      GammaM[x___,nu_,y___]/;(MemberQ[AN$iLorentz,nu]&&Head[nu]=!=GAD):>GammaM[x,GAD[Sequence@@AN$iLorentz2[[Position[AN$iLorentz,nu][[1]]]]],y]
      };
  
  
      expr2=expr2//.DiracSubstitutions;
      expr2=expr2//.{GSD[p_+k_]:>GSD[p]+GSD[k],GSD[-p_]:>-GSD[p]};
  
      (*SUN Algebra*)
  
      SUNSubstitutions={
      Times[aa___,T[x_,y_,GluonA_],cc___,T[y_,x_,GluonB_],ee___]:>Times[aa,SUNTrace[SUNT[GluonA,GluonB] ],cc,ee],Times[aa___,T[x_,y_,GluonA_],cc___,T[y_,z_,GluonB_],ee___]/;x=!=z:>Times[aa,Dot[SUNT[GluonA,GluonB] ],cc,ee],
      Times[aa___,TM[x_,y_,GluonA_],cc___,TM[y_,x_,GluonB_],ee___]:>Times[aa,SUNTrace[SUNT[GluonA,GluonB] ],cc,ee],Times[aa___,TM[x_,y_,GluonA_],cc___,TM[y_,z_,GluonB_],ee___]/;x=!=z:>Times[aa,Dot[SUNT[GluonA,GluonB] ],cc,ee]
      };

      (* NOTE: Substitution for structure constant, unify notation *)
  
      Factor0=Select[expr2,FreeQ[#,T]&];
      FactorT=Select[expr2,!FreeQ[#,T]&];
  
      FactorT=FactorT//.SUNSubstitutions;
      expr2=Factor0*FactorT/.{Metric[Colour1_,Colour2_]:>SUNDelta[SUNIndex[Colour1],SUNIndex[Colour2] ]};

      Factor0=Select[expr2,FreeQ[#,TM]&];
      FactorT=Select[expr2,!FreeQ[#,TM]&];
  
      FactorT=FactorT//.SUNSubstitutions;
      expr2=Factor0*FactorT/.{Metric[Colour1_,Colour2_]:>SUNDelta[SUNIndex[Colour1],SUNIndex[Colour2] ],
      f[indices___]:>SUNF[indices],
      fabc[indices___]:>SUNF[indices]};
  
      SpinorContractionSubs={Times[m___,SpinorUbar[a___,Spin1_,b___],o___,GammaM[x___,Spin1_,Spin2_],p___,SpinorV[c___,Spin2_,d___],n___] :> Times[m,o,Dot[SpinorUbar[a,b],GammaM[x],SpinorV[c,d] ],p,n],
      Times[m___,SpinorVbar[a___,Spin1_,b___],o___,GammaM[x___,Spin1_,Spin2_],p___,SpinorV[c___,Spin2_,d___],n___]:>Times[m,o,Dot[SpinorVbar[a,b],GammaM[x],SpinorV[c,d] ],p,n],
      Times[m___,SpinorUbar[a___,Spin1_,b___],o___,GammaM[x___,Spin1_,Spin2_],p___,SpinorU[c___,Spin2_,d___],n___]:> Times[m,o,Dot[SpinorUbar[a,b],GammaM[x],SpinorU[c,d] ],p,n],
      Times[m___,SpinorVbar[a___,Spin1_,b___],o___,GammaM[x___,Spin1_,Spin2_],p___,SpinorU[c___,Spin2_,d___],n___]:>Times[m,o,Dot[SpinorVbar[a,b],GammaM[x],SpinorU[c,d] ],p,n]
      };

      (* Distinguish between amplitudes compose of sums or of a single term *)

      If[trimValue===False,
        If[Head[expr2]===Plus,

          expr2=Apply[List,Expand[expr2] ]//.SpinorContractionSubs;
          expr2=Total[expr2]//.{Dot[aa___,GammaM[x___],bb___]/;Length[{x}]>0:>Dot[aa,x,bb],Dot[aa___,GammaM[x___],bb___]/;x===1:>Dot[aa,bb]},

          expr2=Expand[expr2] //.SpinorContractionSubs;
          expr2=expr2/.{Dot[aa___,GammaM[x___],bb___]/;Length[{x}]>0:>Dot[aa,x,bb],Dot[aa___,GammaM[x___],bb___]/;x===1:>Dot[aa,bb]};
        ],

        expr2=expr2/.{GammaM[Spin1_,Spin2_]:>1,GammaM[x___,Spin1_,Spin2_]/;Length[{x}]>0:>Dot[x],GammaM[x___,Spin1_,Spin2_]/;x===1:>1, FourVector[p_,mu_]/;(MemberQ[AN$Lorentz,mu] ):>FVD[p,Sequence@@AN$Lorentz2[[Position[AN$Lorentz,mu][[1]] ]] ], FourVector[p_,mu_]/;(MemberQ[AN$iLorentz,mu] ):>FVD[p,Sequence@@AN$iLorentz2[[Position[AN$iLorentz,mu][[1]] ]] ] };
      ];
  
  
      SpinorsSubstitutions={
      PolV[mu_,p_,m_,Gluon_]/;MemberQ[AN$Lorentz,mu]:>PolarizationVector[p,Sequence@@AN$Lorentz2[[Position[AN$Lorentz,mu][[1]] ]]],
      PolV[mu_,p_,m_,Gluon_]/;MemberQ[AN$iLorentz,mu]:>PolarizationVector[p,Sequence@@AN$iLorentz2[[Position[AN$iLorentz,mu][[1]] ]]],
      SpinorUbar[_,p1_,m_,x___]:>SpinorUBarD[p1,m],
      SpinorVbar[_,p1_,m_,x___]:>SpinorVBarD[p1,m],
      SpinorU[_,p1_,m_,x___]:>SpinorUD[p1,m],
      SpinorV[_,p1_,m_,x___]:>SpinorVD[p1,m]
      };
  
      If[trimValue===False, expr2=expr2/.SpinorsSubstitutions ];
  
      FinalAmps=AppendTo[FinalAmps,expr2]
    ],

    {ii,1,Length[expr]}
  ];
  
  Return[FinalAmps];
  
]
