
toLowerCase[symbols_List]  := Table[symbols[[ii]]->ToExpression@ToLowerCase[ToString[symbols[[ii]]] ],{ii,1,Length[symbols]}];
tokiraMasses[symbols_List] := Table[symbols[[ii]]->ToExpression@ToLowerCase[ToString[symbols[[ii]]]<>"2" ],{ii,1,Length[symbols]}];
tokiraMasses2[symbols_List] := Table[symbols[[ii]]^2->ToExpression@ToLowerCase[ToString[symbols[[ii]]]<>"2" ],{ii,1,Length[symbols]}];


canonicalDen[a_, b_] := Module[{expanded, terms, extractKey, sortedTerms, expr, leading},

        expanded = Expand[a];
        terms = If[Head[expanded] === Plus, List @@ expanded, {expanded}];

        extractKey[term_] := Module[{symbol, name, base, index},

            symbol = If[Head[term] === Times, Last[List @@ term], term];
            name = SymbolName[symbol];

            {base, index} = First@(StringCases[name, x : LetterCharacter .. ~~ y : DigitCharacter .. :> {x,ToExpression[y]}]/.{}->{{name, 0}}); 
            {base, index}
            ];

        sortedTerms = SortBy[terms, extractKey];
        expr = Total[sortedTerms];
        leading = First[sortedTerms]; 

        If[MatchQ[leading, Times[-1, ___] ], Den[-expr, b], Den[expr, b] ]
        
        ];

(**********************************************)
(* Functions for writing files in Kira format *)
(**********************************************)

Options[WriteFamiliesKira] := Join[ Options[IntegralReduceKira], {BasisMI->"None", KiraSectors->Automatic, SeedSelection -> "Default", Kinematics -> Default, CutPropagators->{} } ];


WriteFamiliesKira[topologies_,outDir_,loopMomenta_,Init_,Fin_,rKira_,sKira_,basisMI_String,myintegrals_, opts: OptionsPattern[] ]:=Block[{propagatorsID,propagators,dir,invariants,momConservation,aa,momSq,PP,SS,TT,SInvariants,nInit,subsets,ScalarProducts,sectorsKira,jobInfo,lowerCaseMasses,lowerCaseMasses2,lowerCaseInvariants,kiramasses,finalsubs,upperCaseMasses,upperCaseInvariants,seeds,kinematics,offshellSubs,varMomSqRight,varMomSqSub,Invariants,lowerInvariants,resPowers,VariablePowers,simplifyAssociationPowers,variableswPowers,SimplerKinematics,subbedMom,momReplaced,importIBPQ,cutpositions,CutSubs,comStr,lineLength},

   lowerCaseMasses=tokiraMasses[#]&@DeleteDuplicates@AN$Masses[[;; , 2]];

   propagatorsID=topologies[[;;,1]];
   propagators=topologies[[;;,2]]/.lowerCaseMasses;

   seeds = OptionValue[SeedSelection];

   kinematics = OptionValue[Kinematics];

   importIBPQ = OptionValue[OnlyImport];

   sectorsKira = OptionValue[KiraSectors];
   If[sectorsKira === Automatic,

       sectorsKira=Table["b"<>StringJoin[ConstantArray["1", Length[propagators[[ii]] ] ] ], {ii,1,Length[propagatorsID]} ],

       sectorsKira = SortBy[sectorsKira, FirstPosition[propagatorsID, First[#], \[Infinity]] &];
       If[Length[propagatorsID]=!=Length[sectorsKira], Message[IntegralReduceKira::difLengthOptions,propagatorsID,sectorsKira[[;;,2]]]&&Abort[] ];
       If[sectorsKira[[;;,1]]=!=propagatorsID,Message[IntegralReduceKira::notSameTopos,propagatorsID,sectorsKira[[;;,1]]]&&Abort[] ];
       sectorsKira = sectorsKira[[;;,2]];
       sectorsKira = ToString/@sectorsKira;
   ];

   momConservation = First@Select[AN$Kinematics, MatchQ[#, _Symbol -> _Plus | _Symbol] &]/.{(x_->y_):>{x,y}};
   subbedMom = momConservation[[1]];

   PP=Join[Init,Fin];
   nInit=Length[Init];
   aa=Length[PP];

   SimplerKinematics = DeleteCases[AN$Kinematics, Rule[Power[Dot[p1_, p2_], -1], y_] /; (y =!= 0)];

   If[SetOffShell===False,

      (* If on-shell *)
      If[aa<=2,
         momSq = {{{p1,p1}, S}},

         momSq = DeleteCases[AN$Kinematics, Except[Rule[Power[pp_, 2], _] ] ];
         momSq = DeleteCases[momSq, Power[subbedMom, 2] -> ___];
         momSq = momSq/.{(pp_^2->mm_):>{{pp,pp},mm}};
      ],

      (* If off-shell *)

      momSq = DeleteCases[AN$Kinematics, Except[Rule[Power[pp_, 2], _] ] ];

   ];

   subsets=Subsets[Table[i,{i,aa}],{2}];
   Table[(SS[subsets[[k,1]],subsets[[k,2]]]=(ToExpression["S"<>ToString@subsets[[k,1]]<>ToString@subsets[[k,2]]])),{k,Binomial[aa,2]}];
   Table[(TT[subsets[[k,1]],subsets[[k,2]]]=(ToExpression["T"<>ToString@subsets[[k,1]]<>ToString@subsets[[k,2]]])),{k,Binomial[aa,2]}];
   SInvariants=Drop[#,1]&/@Table[If[(i<=nInit&&jj<=nInit)||(i>nInit&&jj>nInit),{{PP[[i]]+PP[[jj]],PP[[i]]+PP[[jj]]},SS[i,jj]},{{PP[[i]]-PP[[jj]],PP[[i]]-PP[[jj]]},TT[i,jj]}],{i,1,aa-1},{jj,i,aa}];
   ScalarProducts = Flatten[SInvariants, 1];

   Which[

      aa===2,

      ScalarProducts = {{{p1,p1},S}},

      aa===3&&nInit===2, 

      ScalarProducts = {{{p1+p2,p1+p2}, S} },

      aa===3&&nInit===1, 

      ScalarProducts = {{{p1,p2}, p1p2}};

   ];
  

   lowerCaseInvariants = toLowerCase[ScalarProducts[[;;,2]] ];
   lowerCaseMasses2 = tokiraMasses2[#]&@DeleteDuplicates@AN$Masses[[;; , 2]];

   (**********************************************************************)
   (* Functions for identifying the power of variables in the Kinematics *)

   VariablePowers[expr_] := Module[{vars},
   
   vars = Variables[expr];
   AssociationThread[vars, Exponent[expr, #] & /@ vars] 
   ];

   simplifyAssociationPowers[assoc_] := KeyValueMap[#1 -> First[DeleteDuplicates[#2] ] &, assoc]//Association;

   (**********************************************************************)


   (* Find the masses that must be defined in the kinematics file *)

      kiramasses = Flatten[topologies[[;; , 2]] /. Den[x_, m_] :> m];
      kiramasses = Select[#, (# =!= 0) &]&@DeleteDuplicates@kiramasses;
      kiramasses = kiramasses/.lowerCaseMasses;

   If[ SetOffShell === False,

      (* ON-SHELL CASE *)

      (* Identify the scalar products in Kinematics list *)
      If[aa>3,
         ScalarProducts = Select[SimplerKinematics, MatchQ[#, Dot[p1_, p2_] -> _] &];
         If[aa>2, ScalarProducts = DeleteCases[ScalarProducts, Dot[p1_, p1_] -> _] ];
      ];

      (* One needs to find the new squared variables in case the user changes the default kinematics. This is to Kira syntax *)

      resPowers = VariablePowers[#]&/@ScalarProducts[[;;,2]];

      resPowers = Merge[resPowers, Identity];
      resPowers = simplifyAssociationPowers[resPowers];
      resPowers = Normal[resPowers];

      variableswPowers = resPowers/.{
            Rule[x_, pwr_]/;(pwr > 1)  :> ToExpression[ToLowerCase[ToString[x] ] ~~ ToString[pwr] ],
            Rule[x_, 1]                :> ToExpression[ToLowerCase[ToString[x] ] ]
               };

      InvariantsSubs = resPowers/.{Rule[x_,pwr_] :> Power[x,pwr]};
      InvariantsSubs = Thread[InvariantsSubs->variableswPowers];

      variableswPowers = DeleteCases[variableswPowers, x_/;(MemberQ[kiramasses, x])];

      (* Identify variables from the squares *)

      varMomSqRight = momSq[[;; , 2]] /. {Power[x_, 2] :> ToExpression[ToLowerCase[ToString[x] ] ~~ "2"], Dot[p1_,p2_]:>ToExpression[ToString[p1]<>ToString[p2] ]};
      varMomSqSub = DeleteCases[ Thread[momSq[[;; , 2]] -> varMomSqRight], 0 -> 0]; 

      varMomSqRight=Select[varMomSqRight, Head[#] === Symbol &];
      variableswPowers=Join[variableswPowers,varMomSqRight]/.lowerCaseInvariants;
      variableswPowers=DeleteDuplicates[variableswPowers];

      lowerCaseMasses2 = DeleteDuplicates[Join[lowerCaseMasses2, varMomSqSub] ];

      momSq = momSq/.lowerCaseInvariants/.lowerCaseMasses2;

      ScalarProducts = ScalarProducts/.lowerCaseMasses2/.InvariantsSubs/.{Dot[p1_,p2_]:>{p1,p2}},

      (* OFF-SHELL CASE *)

      Which[

         aa===3&&nInit===2, 

         ScalarProducts = Join[{{{p1,p1}, p1p1}, {{p2,p2}, p2p2} },ScalarProducts],

         aa===3&&nInit===1, 

         ScalarProducts = Join[{{{p1,p1}, s}, {{p2,p2}, p2p2} },ScalarProducts],

         aa>3,

         ScalarProducts =  Join[Table[{{PP[[ii]],PP[[ii]]},ToExpression["p"~~ToString[ii]~~"p"~~ToString[ii] ]},{ii,1,aa}] ,ScalarProducts];
                
      ];

      variableswPowers = ScalarProducts[[;;,2]];
      variableswPowers = variableswPowers/.{ x_ :> ToExpression[ToLowerCase[ToString[x] ] ] };
   
      InvariantsSubs = Thread[ScalarProducts[[;;,2]]->variableswPowers];
      ScalarProducts = ScalarProducts/.InvariantsSubs;

      momReplaced = momConservation[[1]];

      momReplaced = ToExpression[StringReplace[ToString[momReplaced], "p" -> ""] ];
      variableswPowers = DeleteCases[variableswPowers, x_ /; StringContainsQ[SymbolName[x], ToString[momReplaced] ] ];

      ScalarProducts = Select[ScalarProducts, !StringContainsQ[ToString[Part[#, 2] ], ToString[momReplaced] ] &];

   ];


   If[importIBPQ===False,

      dir=outDir<>"/config";

      If[DirectoryQ[dir], DeleteDirectory[dir,DeleteContents->True];CreateDirectory[dir], CreateDirectory[dir] ];

      (* INTEGRAL FAMILIES *)
      CutSubs = {Den[a_, b_] :> Den[a/.{Rule[momConservation[[1]], momConservation[[2]]]}, b /. lowerCaseMasses]};

      outfile=dir<>"/integralfamilies.yaml";
      lineLength=50;
      wsp="                                                                                                                             ";   
      comStr="###########################################################################################";
      OpenWrite[outfile];
         WriteString[outfile,StringTake[comStr,lineLength]<>"\n" ];
         WriteString[outfile,StringTake["#    File automatically generated by ANATAR    "<>wsp,lineLength]<>"\n" ];
         WriteString[outfile,StringTake[comStr,lineLength]<>"\n\n" ];
         WriteString[outfile,"integralfamilies:\n"];
         Do[
            WriteString[outfile,"  - name: \""<>ToString[propagatorsID[[ii]] ]<>"\"\n"];
               WriteString[outfile,"    loop_momenta: ["<>((StringDelete[#,"}"]&)@(StringDelete[#,"{"]&)@ToString[loopMomenta])<>"]\n"];
            WriteString[outfile,"    propagators:\n"];
               (WriteString[outfile,"      - [ \""<>ToString[#[[1]] ]<>"\",    \""<>ToString[#[[2]] ]<>"\" ]\n"]&)/@(propagators[[ii]]);
            If[OptionValue[CutPropagators]=!={},
	       cutpositions=Position[propagators[[ii]]/. {Den[a_, b_] :> canonicalDen[a, b]}, 
	                             #/. {Den[a_, b_] :> canonicalDen[a, b]} ]&/@(OptionValue[CutPropagators]/. CutSubs)//Flatten;
               WriteString[outfile,"    cut_propagators: ["<>StringRiffle[ToString /@cutpositions, ", "]<> "]\n"] ],
         {ii,1,Length[topologies]}];
      Close[outfile];


      (* KINEMATICS *)


      outfile=dir<>"/kinematics.yaml";
      OpenWrite[outfile];
         WriteString[outfile,StringTake[comStr,lineLength]<>"\n" ];
         WriteString[outfile,StringTake["#    File automatically generated by ANATAR    "<>wsp,lineLength]<>"\n" ];
         WriteString[outfile,StringTake[comStr,lineLength]<>"\n\n" ];
         WriteString[outfile,"kinematics:\n"];
         If[ kinematics === Default,

            WriteString[outfile,"  incoming_momenta: ["<>((StringDelete[#,"}"]&)@(StringDelete[#,"{"]&)@ToString[Init])<>"]\n"];
            If[OptionValue[CutPropagators]=!={}, 
	            WriteString[outfile,"  outgoing_momenta: []\n"],
	            WriteString[outfile,"  outgoing_momenta: ["<>((StringDelete[#,"}"]&)@(StringDelete[#,"{"]&)@ToString[Fin])<>"]\n"] ];
            WriteString[outfile,"  momentum_conservation: ["<>((StringDelete[#,"}"]&)@(StringDelete[#,"{"]&)@ToString[momConservation])<>"]\n"],

            WriteString[outfile,"  incoming_momenta: ["<>((StringDelete[#,"}"]&)@(StringDelete[#,"{"]&)@ToString[kinematics[[1]] ])<>"]\n"];
            WriteString[outfile,"  outgoing_momenta: ["<>((StringDelete[#,"}"]&)@(StringDelete[#,"{"]&)@ToString[kinematics[[2]] ])<>"]\n"];
            WriteString[outfile,"  momentum_conservation: ["<>((StringDelete[#,"}"]&)@(StringDelete[#,"{"]&)@ToString[kinematics[[3]] ])<>"]\n"]
         ];
         WriteString[outfile,"  kinematic_invariants:\n"];
         (WriteString[outfile,"      - ["<>ToString[#]<>",    2]\n"]&)/@(variableswPowers);
         (WriteString[outfile,"      - ["<>ToString[#]<>",    2]\n"]&)/@(kiramasses);
         WriteString[outfile,"  scalarproduct_rules:\n"];
         If[SetOffShell===False,
            (WriteString[outfile,"      - [ [ "<>((StringDelete[#,"}"]&)@(StringDelete[#,"{"]&)@ToString[#[[1]]])<>" ],    "<>(ToString[#[[2]] ])<>"]\n"]&)/@(momSq);
         ]
         If[aa>2,
            (WriteString[outfile,"      - [ [ "<>((StringDelete[#,"}"]&)@(StringDelete[#,"{"]&)@ToString[#[[1]]])<>" ],    "<>(ToString[InputForm[#[[2]] ] ])<>"]\n"]&)/@(ScalarProducts);
         ];
         WriteString[outfile,"  symbol_to_replace_by_one: "<>ToString[variableswPowers[[1]] ] ];
      Close[outfile];


      (* JOBS FILE *)

      jobInfo = Thread[{propagatorsID,rKira,sectorsKira,sKira}];

      outfile=outDir<>"/jobs.yaml";
      OpenWrite[outfile];
         WriteString[outfile,StringTake[comStr,lineLength]<>"\n" ];
         WriteString[outfile,StringTake["#    File automatically generated by ANATAR    "<>wsp,lineLength]<>"\n" ];
         WriteString[outfile,StringTake[comStr,lineLength]<>"\n\n" ];
         WriteString[outfile,"jobs:\n"];
         WriteString[outfile," - reduce_sectors:\n"];
         WriteString[outfile,"    reduce:\n"];
         WriteString[outfile,"     - {topologies: ["<>ToString[#[[1]] ]<>"], sectors: ["<>#[[3]]<>"], r: "<>ToString[#[[2]] ]<>", s: "<>ToString[#[[4]] ]<>"}\n"]&/@jobInfo;
         WriteString[outfile,"    select_integrals:\n"];
         WriteString[outfile,"     select_mandatory_list:\n"];
         WriteString[outfile,"      - ["<>ToString[#[[1]] ]<>", "<>myintegrals<>", r: "<>ToString[#[[2]] ]<>", s: "<>ToString[#[[4]] ]<>" ]\n"]&/@jobInfo;
         If[basisMI=!="None",
            WriteString[outfile,"    preferred_masters: \""<>basisMI<>"\"\n"];
         ];
         WriteString[outfile,"    run_initiate: true\n"];
         WriteString[outfile,"    run_triangular: true\n"];
         WriteString[outfile,"    run_back_substitution: true\n"];
         Close[outfile]
   ];


   upperCaseMasses = lowerCaseMasses/.{(x_ -> y_) :> (y -> x^2)};
   upperCaseInvariants = lowerCaseInvariants/.{(x_ -> y_) :> (y -> x)};
   finalsubs = Join[upperCaseMasses,upperCaseInvariants];
   finalsubs = MapAt[ToExpression, finalsubs, {All, 1}];
   offshellSubs = Table[ToExpression["p"<>ToString[x]<>"p"<>ToString[x]<>"-> p"<>ToString[x]<>".p"<>ToString[x] ],{x,1,aa}];
   offshellSubs=Join[offshellSubs,{p1p2->Dot[p1.p2] }];
   finalsubs = Join[finalsubs,offshellSubs]

]



WriteTopoFiles[expr_,directory_,loopMomenta_]:=Block[{ProdDen,UniqueRoutings,outDir,momConservation,momenta,lastMom,Init,Fin,output},

   outDir=directory<>"/KiraFiles/"; 
   If[DirectoryQ[outDir], DeleteDirectory[outDir,DeleteContents->True];CreateDirectory[outDir], CreateDirectory[outDir] ];

   (*Extract the initial and final momenta*)

   momConservation = First@Select[AN$Kinematics, MatchQ[#, _Symbol -> _Plus | _Symbol] &];

   (*Distinguish between self-energy cases and the rest*)

   If[Length[momConservation[[2]]]>1,

   momConservation=Level[momConservation,1];
   momenta=Level[momConservation[[2]],1];
   momenta=FactorTermsList/@momenta;
   lastMom=momConservation[[1]];
   Init={};
   Fin={};
   If[#[[1]]===-1,Fin=AppendTo[Fin,#[[2]]],Init=AppendTo[Init,#[[2]]] ]&/@momenta;
   Init;
   Fin=AppendTo[Fin,lastMom];
   ];

   (*Extract the propagators*)

   Which[
   	Head[expr]===List,ProdDen=Select[#,!FreeQ[#,Den]&]&/@(Level[expr,1]),
   	Head[expr]===Times,ProdDen=Select[#,!FreeQ[#,Den]&]&@expr,expr===0,Return[0]
   	];

   ProdDen = (Level[#, 1] & /@ ProdDen) /. {Den[x___] :> {x}};

   UniqueRoutings=(DeleteDuplicates@FullSimplify@Abs[ProdDen])/.{Abs[x___]:>x};

   UniqueRoutings=Table[{"topo"<>ToString[ii],UniqueRoutings[[ii]]},{ii,Length[UniqueRoutings]}];

   WriteFamiliesKira[#,outDir,loopMomenta,Init,Fin]&/@UniqueRoutings;

   Print[UniqueRoutings]

];


   (* Export FILE *)

   WriteExportKira[topologies_,outDir_,myintegrals_] := Module[{propagatorsID},

      propagatorsID=topologies[[;;,1]];
   
      outfile=outDir<>"/export.yaml";
      OpenWrite[outfile];
         WriteString[outfile,"jobs:\n"];
         WriteString[outfile," - kira2math:\n"];
         WriteString[outfile,"    target:\n"];
         WriteString[outfile,"      - ["<>ToString[#]<>", "<>myintegrals<>"]\n"]&/@propagatorsID;
         WriteString[outfile,"    reconstruct_mass: true\n"];
      Close[outfile];

   ];


(*********************************)
(*     Kira General Function     *)
(*********************************)

IntegralReduceKira::noKira                = "Not output generated. Check if Kira is installed and correctly linked to Mathematica.  ";
IntegralReduceKira::noResultsKira         = "Kira has been found, but it was not possible to perform any reduction. Common errors involve incorrect syntax of topologies or incorrect kinematics.";
IntegralReduceKira::wrongKinematicsKira   = "The kinematics in the input amplitude is not compatible with Kira. Check that the same quantity is not defined twice under different names.";
IntegralReduceKira::FileNotFound          = "The file `1` was not found. No reduction performed.";
IntegralReduceKira::inccompatibleOptions  = "The use of the option OnlyWriteFiles is incompatible with the option OnlyImport set to True."
IntegralReduceKira::invalidinput          = "Amplitudes are not given in correct format, check the manual."
IntegralReduceKira::difLengthOptions      = "The number of selected topologies `1` does not match the number of provided sectors `2`."
IntegralReduceKira::notSameTopos          = "The selected topologies `1` do not match the topologies `2` provided via the KiraSectors option."

Options[IntegralReduceKira] := {Topologies-> All, BasisMI->"None", KiraSectors->Automatic, SeedSelection -> "Default", Kinematics -> Default, OnlyWriteFiles -> False, OnlyImport -> False, CutPropagators->{} };

IntegralReduceKira[amp0_, opts: OptionsPattern[] ]:= Catch[Module[{sKira,rKira,intVectors,ampMI,nRHS,partialMasterIntegrals,nMI,listbasisMI,backSubs,seeds,output,kinematics,amp1,amp,TopoPropRules,processName,nDiagrams,nLoops,numLines,extractMomenta,loopMomenta,Init,Fin,runKiraQ,importIBPQ,currentDir,fileAmpInt},

   currentDir=Directory[];

   If[ Length[amp0[Amplitudes] ]===1&&Head[amp0[Amplitudes][[1]] ]=!=Rule, amp1=amp0, amp1 = GatherProjectors[amp0] ];

   amp1        =  Normal[amp1];
   If[Head[amp1] != List && (Length[amp1] === 6 || Length[amp1] === 5), Message[IntegralReduceKira::invalidinput ] && Abort[] ];
   
   If[Length[amp1]===6,
   {process     =  amp1[[1,2]];
   nDiagrams   =  amp1[[2,2]];
   processName =  amp1[[3,2]];
   nLoops      =  amp1[[4,2]];
   kine        =  amp1[[5,2]];
   amp         =  amp1[[6,2]];}];

   If[Length[amp1]===5,
   {process     =  amp1[[1,2]];
   processName =  amp1[[2,2]];
   nLoops      =  amp1[[3,2]];
   kine        =  amp1[[4,2]];
   amp         =  amp1[[5,2]];}];


   loopMomenta = Union@Flatten[Topology[#][LoopMomenta] & /@ AllTopologies[], 1];
   extractMomenta[listFields_] := Cases[listFields, x_Symbol, Infinity];

   Init = extractMomenta[process[[1, 1]]];
   Fin = extractMomenta[process[[1, 2]]];

   TopoPropRules[list_List] := TopoPropRules/@list;

   TopoPropRules[top_] := TopoPropRules[top] = Rule[top, Topology[top][Propagators] ];
   If[OptionValue[Topologies] === All, 
         topologies = TopoPropRules[AllTopologies[] ],
         topologies = TopoPropRules[OptionValue[Topologies] ]; 
   ];


   outDir = AN$CurrentProcessPath<>"/kira_reduction"<>ToString[nLoops]<>"L";

   seeds = OptionValue[SeedSelection];

   runKiraQ = OptionValue[OnlyWriteFiles];
   importIBPQ = OptionValue[OnlyImport];

   If[OptionValue[Kinematics] == Default, kinematics = kine, kinematics = OptionValue[Kinematics] ];


   (* r and s Seed values*)
   If[seeds==="Default",

      (* Find the s Kira parameter in amplitude amp *)
      integralsinAmp = IdentifyIntegrals[amp];
      intVectors = integralsinAmp /. {TopInt[f_, x___ ] :> {x} };
      sKira = Abs[#]&/@(Total[Select[#, Negative] ] & /@ intVectors);
      sKira = Max[sKira];
      rKira = Abs[#]&/@(Total[Select[#, Positive] ] & /@ intVectors);
      rKira = Max[rKira];
      (* r Kira parameter is the max of the length of unit vector and sum of positive powers of propagators *)
      rKira = Max[rKira,Length@intVectors[[1]] ],

      rKira = seeds[[1]];
      sKira = seeds[[2]];
   ];

   basisMI = OptionValue[BasisMI];

   (* Create folder for Kira's results *)

   Run["mkdir "<>outDir];

   (* Print basis to file if given as a list *)

   If[Head[basisMI]===List, 

      listbasisMI = basisMI;
      basisMI = "preferredBasis";
      WriteAmpIntegrals[outDir, listbasisMI, OutputName -> basisMI];

      ];

   fileAmpInt = "amplitude"<>ToString[nLoops]<>"L";
   (* Write Kira files and return the substitutions converting the lower cases symbols used by Kira into our notation *)

   backSubs = WriteFamiliesKira[topologies,outDir,loopMomenta,Init,Fin,rKira,sKira,basisMI,fileAmpInt,opts];

   SetDirectory[outDir];


   pathToReducedInt = outDir<>"/results/"<>ToString[topologies[[-1,1]] ]<>"/kira_amplitude"<>ToString[nLoops]<>"L.m";

   (* Ways to obtain AN$IntegralsReduced. Option of importing results without running *)

Which[
      
   (* Default does nothing here *)   
   importIBPQ === False,

      (* Integrals from the Amplitudes *)
      WriteAmpIntegrals[outDir, amp, OutputName -> fileAmpInt];

      WriteExportKira[topologies,outDir,fileAmpInt];

      SetDirectory[outDir];

      If[!runKiraQ,
         Print["Running Kira"];
         Run["kira jobs.yaml > kira_jobs.log"],

         Print["Kira files have been written"];
         Abort[];
      ];

      output=Import[outDir<>"/kira_jobs.log","Text"];
      numLines = StringCount[output, "\n"] + 1;

      (* ERROR MESSAGES *)
      Which[
         output==="", Message[IntegralReduceKira::noKira]&&Abort[],

         StringContainsQ[output,"The user defined environment variable FERMATPATH for Fermat is set:"]&&numLines===2, Message[IntegralReduceKira::noResultsKira]&&Abort[],

         StringContainsQ[output,"Error: your kinematics in config/kinematics.yaml\nseem to be wrong."], Message[IntegralReduceKira::wrongKinematicsKira]&&Abort[]

         ];

      Print["IBP's computed."];


      Print["Performing the reduction of integrals in the amplitude..."];
      Run["kira export.yaml"];
      
      pathToReducedInt = outDir<>"/results/"<>ToString[topologies[[-1,1]] ]<>"/kira_amplitude"<>ToString[nLoops]<>"L.m";
      
      If[ FileExistsQ[pathToReducedInt],

         AN$IntegralsReduced = Import[pathToReducedInt],

         Print["No reduction has been found for the integrals in the amplitude."];
         Print[MakeGreen["Done!",Boldmg->True] ];
         Throw[amp0];
      ],

   (* True gets ANATAR to look for the usual folder with results *)   
   importIBPQ === True,
      If[runKiraQ,
         Message[IntegralReduceKira::inccompatibleOptions];
         Abort[];
      ];

      Print["The user has chosen to only import IBP's without running Kira."];
      pathToReducedInt = outDir<>"/results/"<>ToString[topologies[[-1,1]] ]<>"/kira_amplitude"<>ToString[nLoops]<>"L.m";
      If[ FileExistsQ[pathToReducedInt],

         AN$IntegralsReduced = Import[pathToReducedInt],

         Message[IntegralReduceKira::FileNotFound,pathToReducedInt];
         Abort[];
      ]
      Print["Importing IBP's from the default path "<>MakeGreen[pathToReducedInt] ],
      
   (* Specific path provided by the user *)
   Head[importIBPQ] === String,
      If[runKiraQ,
         Message[IntegralReduceKira::inccompatibleOptions];
         Abort[];
      ];
      Print["The user has chosen to only import IBP's without running Kira."];
      pathToReducedInt = importIBPQ;
      If[ FileExistsQ[pathToReducedInt],

         AN$IntegralsReduced = Import[pathToReducedInt],

         Message[IntegralReduceKira::FileNotFound,pathToReducedInt];
         Abort[];
      ]
      Print["Importing IBP's from the path "<>MakeGreen[pathToReducedInt] ];

   ];


   AN$IntegralsReduced = ReplaceAll[#, {topo_[x___]/; (VectorQ[{x}, IntegerQ[#] &]&& topo =!= Rational) :> TopInt[topo, x]}] &/@AN$IntegralsReduced/.backSubs/.{d->$DimensionST};
   
   MasterIntegrals[]  = IdentifyIntegrals[AN$IntegralsReduced];
   nRHS = ToString[Length[MasterIntegrals[] ] ];

   ampMI = amp /. AN$IntegralsReduced;
   ampMI = ampMI /. kine;
   ampMI = ampMI /.backSubs/.{s->S,p1p2->Dot[p1.p2] };

   Print["Summing up all the amplitudes and simplifying..."];


   (* Identify the integrals in the final expressions *)


   partialMasterIntegrals = IdentifyIntegrals[ampMI];
   MasterIntegrals[]  = Join[MasterIntegrals[] ,partialMasterIntegrals];
   MasterIntegrals[]  = DeleteDuplicates[MasterIntegrals[] ];

   nMI = ToString[Length[MasterIntegrals[] ] ];

   Print[ToString[Style[nMI<>" Master Integrals",Bold,Darker@Green],StandardForm]<>" found in the final expression of the amplitude. They can be found in the MasterIntegrals[] list."];

   Print[MakeGreen["Done!",Boldmg->True] ];

   If[Length[amp1]===6, finalAss = Association[{Process -> process,"Total" -> nDiagrams, "Name" ->  processName,  "LoopOrder" ->  nLoops, Kinematics -> kine, Amplitudes -> ampMI}] , 
      finalAss = Association[{Process -> process, "Name" ->  processName,  "LoopOrder" ->  nLoops, Kinematics -> kine, Amplitudes -> ampMI}] ];

   finalAss = FlattenProjectors[finalAss];
   
   SetDirectory[currentDir];
   Return[finalAss]

]
]
