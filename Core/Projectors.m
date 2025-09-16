(* ::Package:: *)


Lorentz[struct_,a_] := struct[a][[2,2]] ;

(* Identify the type of field *)
AN$FieldType[field_]/;MemberQ[AN$VectorFields,field]:=VV
AN$FieldType[field_]/;MemberQ[AN$ScalarFields,field]:=SS
AN$FieldType[field_]/;MemberQ[AN$Fermions,field]:=FF
AN$FieldType[field_]/;NumberQ[field]:=field

GluonedQ[Field_] := MemberQ[AN$FieldsClassified[[1, ;; , 2]], Field]
ColouredQ[Field_] := MemberQ[AN$FieldsClassified[[2, ;; , 2]], Field]

(* Function that converts the projectors from Mathematica syntax to FORM *)

ProjectorsToForm[projectorsList_List]:=Module[{listStructures,ProjUnited,currentProj,arg,list},

   ProjectorsList={};

   Table[listStructures[ii]=projectorsList[[ii,2]],{ii,1,Length[projectorsList]-1}];

   nProj=Length[Proj$Union/.projectorsList];

   Do[

      currentProj=projectorsList[[-1,2,jj]];
      
      arg=currentProj/.Projector[sequence___]:>{sequence};
      
      ProjUnited=Table[listStructures[ii][[arg[[ii]]]],{ii,1,Length[arg]}];
      ProjUnited=Times[Sequence@@ProjUnited];
      
      ProjectorsList=Append[ProjectorsList,ProjUnited],

   {jj,1,nProj}];

   Return[ProjectorsList]

]

(* Functions to add indices to the fields defined in projectors lib *)

convertToPattern[expr_] := Module[{head,args},

   head=Head[expr];
   args=List@@expr;
   Apply[head,ToExpression[ToString[#]<>"_"]&/@args]

 ]

AddDummyIndices[fields_List] := Module[{newFields,LorentzInd,SpinInd,ColourInd,GluonInd,MomInd},

   LorentzInd=Table[Symbol["Lor"<>ToString@i],{i,Length[fields]+1} ];
   SpinInd=Table[Symbol["Spin"<>ToString@i],{i,Length[fields]+1}];
   ColourInd=Table[Symbol["Colour"<>ToString@i],{i,Length[fields]+1}];
   GluonInd=Table[Symbol["Gluon"<>ToString@i],{i,Length[fields]+1}];
   MomInd = Table[Symbol["p"<>ToString@i], {i,Length[fields]+1}];

   newFields=MapIndexed[If[MemberQ[AN$Bosons,#1], #1[MomInd[[First[#2] ]], LorentzInd[[First[#2] ]] ], #1]&, fields];
   newFields=MapIndexed[If[GluonedQ[Head[#1] ], Head[#1][Sequence@@List@@#1, GluonInd[[First[#2] ]] ], #1]&, newFields];
   newFields=MapIndexed[If[MemberQ[AN$Fermions,#1], #1[MomInd[[First[#2] ]], SpinInd[[First[#2] ]] ], #1]&, newFields];
   newFields=MapIndexed[If[ColouredQ[Head[#1] ], Head[#1][Sequence@@List@@#1, ColourInd[[First[#2] ]] ], #1]&, newFields];

   newFields=convertToPattern/@newFields;

   Return[newFields];
]

(* Function for exchanging the indices in projectors of fermions in the final state *)

fermionFlowProjectors[structure_List,OutFields_List,InFields_List]:=Module[{condition, groups,replacements,swapIndices},

   condition = (AN$FieldType[#[[1]] ]===FF &&MemberQ[OutFields,#[[1]] ])&;
   groups=Select[structure,condition];
   If[Length[groups]===0||(Length[OutFields]===1&&Length[InFields]===1), Return[structure] ];
   swapIndices = Table[ii, {ii, 2, Length[groups[[1]] ]} ];
   replacements=Reverse[groups];

   Replace[structure,(entry_/;condition[entry]):>ReplacePart[entry,Thread[swapIndices->Part[replacements,Position[groups,entry][[1,1]] ][[swapIndices]] ] ],{1}]
];


(* Functions to Gather and Flatten nested lists of amplitudes for several projectors  *)

GatherProjectors[ass_] := Association[
   Append[
    Normal[KeyDrop[ass, Amplitudes] ],
    Rule[Amplitudes, 
     GatherBy[ass[Amplitudes], #[[1, 2]] &] /. {x_List} :> x]
    ]
   ];

FlattenProjectors[ass_] := Association[
  Append[Normal[KeyDrop[ass, Amplitudes] ], 
      Rule[Amplitudes, Flatten[ass[Amplitudes] ] ] 
    ]  
   ];

(*************************************)
(*       MAIN ProjectAmplitude       *)
(*************************************)

ProjectAmplitude::noProjectors=": The projectors for the process `1` were not found in the libraries. Please add them via the Projectors option or manually in the file ProjectorsLib.m ";

ProjectAmplitude::wrongProjInput=": The provided projectors \"`1`\" should be provided as a list ";

ProjectAmplitude::undefProjectors=": The selected projector `1` does not seem to be properly defined, please provide it via DefineProjector.";

Options[ProjectAmplitude] := {SubstitutionRules->"None",Projectors->Default,Kinematics->Default,CasimirValues -> False};

ProjectAmplitude[AmpOutput_Association, projectorsByHand_List, opts: OptionsPattern[] ]:=Block[{process,nDiagrams,AmpName,nLoops,extFields,structure,LorentzInd,structureSortRules,amplitudesProjLog,amplitudesProjRunOutput,amplitudesResult,subsCleaningOutput,amplitudeTest,polFactor,polTags,jj,polString,sortedDiagrams,simpRules,subsStructures,SpinInd,ColourInd,MomInd,a1,projectorsList,nProj,output1,ll,subsa1,listNames,GluonInd,projFields,currentProj,kine,finalFields,initialFields,casimiroption},
   Clear[AN$ProjAmplitude]; (* Clean previous run *)

   AmpOutputlist  =  ConvertFromAmplitudeAssociation[AmpOutput];
   process        =  AmpOutputlist[[1,1]];
   nDiagrams      =  AmpOutputlist[[2,2]];
   AmpName        =  AmpOutputlist[[3,2]];
   nLoops         =  AmpOutputlist[[4,2]];
   kine           =  AmpOutputlist[[5,2]];

   nDiagrams1     =  AmpOutputlist[[6,2]][[1,1]]/.{DiagramID[i_]:>i};
   nDiagrams2     =  AmpOutputlist[[6,2]][[-1,1]]/.{DiagramID[i_]:>i};
   
   casimiroption = OptionValue[CasimirValues];

   AN$ProcessPath=Global`$AnatarPath<>"/Outputs/"<>AmpName;

   simpRules = OptionValue[SubstitutionRules];

   Do[
   
      currentProj = projectorsByHand[[ii]];
      If[ Head[Projector[currentProj] ] =!= Association&& Length[Projector[currentProj] ]=!=6, Message[ProjectAmplitude::undefProjectors,currentProj]&&Abort[] ]
   
   , {ii,1,Length[projectorsByHand]} ];


   
   If[ Head[projectorsByHand] =!= List && projectorsByHand =!= Default, Message[ProjectAmplitude::wrongProjInput,projectorsByHand]&&Abort[] ];


   (* Delete previous files associated to projectors *)

   SetDirectory[AN$ProcessPath];

   DeleteFileIfExistsQ[AN$ProcessPath<>"/ProjectAmp_"<>ToString[nLoops]<>".frm"];
   DeleteFileIfExistsQ[AN$ProcessPath<>"/ProjAmp_"<>ToString[nLoops]<>".log"];
   DeleteFileIfExistsQ[AN$ProcessPath<>"/Amp_Proj"<>ToString[nLoops]<>".m"];

   (* Obtain the structure required by this process *)

   jj = 1;
   While[AmpOutputlist[[6,2]][[jj, 2]] === 0 && jj<nDiagrams, jj++];
   amplitudeTest = AmpOutputlist[[6,2]][[jj, 2]];

   extFields=Flatten@(Join[{process[[1]]},{process[[2]]}]);
   initialFields=process[[1]];
   finalFields=process[[2]];
   polString=Import[AN$ProcessPath<>"/"<>AmpName<>"_"<>ToString[nLoops],"String"];
   polString=StringCases[polString,(" a1 ="~~___~~";"),Overlaps->All];

   subsa1 = { 
      "= (+"~~Shortest[___]~~")\n":>"= ",
      "= (-"~~Shortest[___]~~")\n":>"= ",
      "= (+"~~Shortest[___]~~")*\n":>"= ",
      "= (-"~~Shortest[___]~~")*\n":>"= ",
      "("->"[",")"->"]" 
      };

   
   polString=StringReplace[polString[[-1]], subsa1 ];
   ToExpression[polString];
   polFactor=a1/.{pro[___]->1,vrtx[___]->1};

   LorentzInd=Table[Symbol["Lor"<>ToString@i],{i,Length[extFields]+1} ];
   SpinInd=Table[Symbol["Spin"<>ToString@i],{i,Length[extFields]+1}];
   ColourInd=Table[Symbol["Colour"<>ToString@i],{i,Length[extFields]+1}];
   GluonInd=Table[Symbol["Gluon"<>ToString@i],{i,Length[extFields]+1}];
   MomInd = Table[Symbol["p"<>ToString@i], {i,Length[extFields]+1}];

   (* Add indices to the fields. THE FINAL RESULT OF THIS IS STRUCTURES. *)
   (* This was implemented in the OLD VERSION, where user DID NOT have to mandatory provide the list of Projectors *)
   (* This is not fully necessary in the NEW version. However kept so that this is clearly written in FORM *)

   subsStructures = { 
      {Fi_, p_} /; (AN$FieldType@Fi === VV && Fi=!=G ) :> { Fi, p, LorentzInd[[Abs[polTags[[Position[polTags, p][[1, 1]], 1]] ] ]] },
      {Fi_, p_} /; (AN$FieldType@Fi === SS ) :> { Fi, p, LorentzInd[[Abs[polTags[[Position[polTags, p][[1, 1]], 1]] ] ]] },
      {Fi_, p_} /; (Fi===G ) :> { Fi, p, LorentzInd[[Abs[polTags[[Position[polTags, p][[1, 1]], 1]] ] ]], GluonInd[[Abs[polTags[[Position[polTags, p][[1, 1]], 1]] ] ]] }, 
      {Fi_, p_} /; (AN$FieldType@Fi === FF && Not@ColouredQ[Fi]) :> {Fi, p, SpinInd[[Abs[polTags[[Position[polTags, p][[1, 1]], 1]] ] ]]}, 
      {Fi_, p_} /; (AN$FieldType@Fi === FF && ColouredQ[Fi]) :> {Fi, p, SpinInd[[Abs[polTags[[Position[polTags, p][[1, 1]], 1]] ] ]], ColourInd[[Abs[polTags[[Position[polTags, p][[1, 1]], 1]] ] ]]}
      };

   polTags=Level[polFactor,1]/.{pol[FF_[aa_,p_] ] :> {aa,LorentzInd[[Abs[aa] ]],p} };
   structure=extFields/.{Fi_[p_]:>{Fi,p}};

   structure=structure/.subsStructures;

   (* Ordering of the fields *)

   structure=Replace[#,{Fi_,y___}:>{Fi,AN$FieldType[Fi],y}]&/@structure;

   structure=fermionFlowProjectors[structure, Head/@finalFields, Head/@initialFields];

   structureSortRules={
      {xx___,{Fi1_,FF,ind1___},{Fi2_,VV,ind2___},yy___}:>{xx,{Fi2,VV,ind2},{Fi1,FF,ind1},yy},
      {xx___,{Fi1_,SS,ind1___},{Fi2_,FF,ind2___},yy___}:>{xx,{Fi2,FF,ind2},{Fi1,SS,ind1},yy},
      {xx___,{Fi1_,SS,ind1___},{Fi2_,VV,ind2___},yy___}:>{xx,{Fi2,VV,ind2},{Fi1,SS,ind1},yy}};

   structure=structure//.structureSortRules;
   structure=Replace[#,{Fi_,VV_,y___}:>{Fi,y}]&/@structure;
      
   structure=Replace[#,({Fi_,y___}):>Fi[y] ]&/@structure;
      
   (* structure at this point has the syntax of the form {t(p1,Spin1,Colour1),t(p2,Spin2,Colour2)} *)

   (* Import Projectors library *)
   Import[Global`$AnatarPath<>"/Core/ProjectorsLib.m"];
   
   (* Option select projectors *)

         listNames = projectorsByHand;

   ActiveProjectors[] = listNames; (* Redefine the list of Projectors *)

   listProj = Projector[#][Operator]&/@projectorsByHand;

   projFields=Head/@Flatten[process/.{Rule -> List}];

   tempProj[Sequence@@AddDummyIndices[projFields ]  ] = listProj;
   projectorsList = subsOnShell@tempProj[Sequence@@structure];

   output1 = projectorsList//. {(head_Plus)[x_]:>Total@Map[#[x]&,List@@head],(head_Times)[x_]:>(List@@head)[[1]]*(List@@head)[[2]][x]}; (* Changes (p1-p2)[Lor] into p1[Lor] - p2[Lor] *)
   output1 = output1//.{Dot[a_, b_ + c_] :> Dot[a, b] + Dot[a, c], Dot[a_, -b_] :> -Dot[a, b]};
   nProj=Length[listNames];

   If[ Head[projectorsList] === Projector, Message[ProjectAmplitude::noProjectors,process]&&Abort[], Print["Projectors for the process "<>ToString[process]<>" were found"] ];

   (* nProj number of projectors is obtained through the function ProjectorsToForm *)

   (* output1 contains the list with the operators used as projectors *)

   WriteProjectorsRoutine[AN$ProcessPath,structure,nDiagrams,nDiagrams1,nDiagrams2,nLoops,D,output1,nProj,simpRules,casimiroption];

   Print["Contracting the amplitudes with the projectors..."];
   amplitudesProjLog=AN$ProcessPath<>"/ProjAmp_"<>ToString[nLoops]<>".log";
   amplitudesProjRunOutput=Run["form "<>AN$ProcessPath<>"/ProjectAmp_"<>ToString[nLoops]<>".frm > "<>amplitudesProjLog];


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

   amplitudesResult = Import[AN$ProcessPath<>"/Amp_Proj_"<>ToString[nLoops]<>".m", "String"];
   amplitudesResult = (StringReplace[#,subsCleaningOutput]&)@amplitudesResult; 
  
   ll=1;
   While[StringContainsQ[amplitudesResult, "_?"] && ll<5, 
        amplitudesResult = StringReplace[amplitudesResult, subsCleaningOutput];
        ll++;
        ];
   amplitudesResult = ToExpression[amplitudesResult]/.{Dot[x1_, x2_^2] :> (Dot[ x1 , x2] )^2};

   amplitudesResult = Table[Table[{DiagramID[ii], listNames[[jj]] } -> AN$ProjAmplitude[ii,jj], {ii, Evaluate[Flatten[List @@ (nDiagrams) /. Span -> Range]]}] ,{jj, 1, nProj}];
   amplitudesResult = Flatten[amplitudesResult];

   amplitudesResult = subsOnShell[amplitudesResult/.{SS->S, TT->T, UU->U}];

   amplitudesResult = amplitudesResult/.{D->$DimensionST}/.{Dot[p1_, Power[p2_, a_] ] :> Power[(p1 . p2), a]}/.kine;
   amplitudesResult = amplitudesResult//.{Dot[a_ + b_, c_] :> Dot[a, c] + Dot[b, c], Dot[a_, b_ + c_] :> Dot[a, b] + Dot[a, c], Dot[-a_, b_] :> -Dot[a, b], Dot[a_, -b_] :> -Dot[a, b]};


   If[nDiagrams===1, amplitudesResult=Flatten@@@amplitudesResult ];

   Print[MakeGreen["Done!",Boldmg->True] ];
   Return[Association[{Process -> List[process],Total -> nDiagrams, Name ->  AmpName,  LoopOrder ->  nLoops, Projectors -> nProj, Kinematics -> kine, Amplitudes -> amplitudesResult}] ]
]
