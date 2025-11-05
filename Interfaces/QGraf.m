(* ::Package:: *)

(*******************************)
(*      Feynman Diagrams       *)
(*******************************)



(* The user should set the path to QG *)

     (*$QGrafPath="";*)
     
     (* The implementation below has the limitation of the length for the output entry in the control card of QG. Change the 'bar' of antiparticles for 'x'. Also bad for processes with several external particles. This is quite bad if the location of the Notebook directory is nested in many folders. Create the option of giving the name to the process by the user *)

(* Options *)

Options[QGDiagrams]={Loops->"0",QGStyle->"array_FR.sty",QGMessage->"None",QGPartition->"None",QGrafOptions->"None",CouplingsOrder->"None",RemovePropagator->"None"};

(* Generate Feynman Diagrams using QGraf *)

QGDiagrams::InvalidOptions="The selected QGrafOptions `1` are invalid. "


QGDiagrams[model0_,outp_,initial_->final_,options___]:=Block[{style,model=model0,outfile,outcard,outlog,log,outdiagrams=outdiagrams0,outdiagrams2,messageFile,partition,loops,qgoptions,processFolder,couplingsSpec,nopropSpec,framedLog,validOptions,invalidOptions,completeSyntaxListsQGraf},

   
   (* evaluating options *)

   messageFile=QGMessage/.{options}/.Options[QGDiagrams];
   style=QGStyle/.{options}/.Options[QGDiagrams];
   partition=QGPartition/.{options}/.Options[QGDiagrams];
   loops=Loops/.{options}/.Options[QGDiagrams]//ToString;
   qgoptions=QGrafOptions/.{options}/.Options[QGDiagrams];
   couplingsSpec=CouplingsOrder/.{options}/.Options[QGDiagrams];
   nopropSpec=RemovePropagator/.{options}/.Options[QGDiagrams];
   If[Depth[nopropSpec] === 2, nopropSpec = ((#)/.{x_:> {x,0,0}})&/@nopropSpec , nopropSpec = nopropSpec];
   
   completeSyntaxListsQGraf[x0_]:=Module[{x},
      Which[
         Length[x0] === 1, x=x0,
         Length[x0] === 3, x=x0,
         Length[x0] === 2, x=x0/.{{alpha_,r1_}:>{alpha,r1,r1} }
      ];
      Return[x];
   ];
   couplingsSpec = completeSyntaxListsQGraf/@couplingsSpec;
   nopropSpec = completeSyntaxListsQGraf/@nopropSpec;

   (*** Error messages ***)

   If[!FileExistsQ[FileNameJoin[{Global`$QGrafPath, "array_FR.sty"}]], Print[Style["QGRAF style file is not found", Bold, Darker@Red]];Abort[]];

   validOptions = {"None", "onepi", "onshell","nosigma","nosnail","simple","notadpole","floop","offshell","sigma","snail","tadpole","notsimple"};
   invalidOptions = Complement[Symbol/@(StringSplit[qgoptions,","]), Symbol/@(validOptions)];
   If[invalidOptions =!= {}, Message[QGDiagrams::InvalidOptions,invalidOptions] && Abort[] ];

   If[loops==="0",Print[Style["Starting diagrams computation at tree-level in model "<>AN$Model,Bold,Darker@Green] ]];
   If[loops==="1",Print[Style["Starting diagrams computation at one-loop in model "<>AN$Model,Bold,Darker@Green] ]];
   If[loops==="2",Print[Style["Starting diagrams computation at two-loop in model "<>AN$Model,Bold,Darker@Green] ]];
   If[loops==="3",Print[Style["Starting diagrams computation at three-loop in model "<>AN$Model,Bold,Darker@Green] ]];
   If[loops==="4",Print[Style["Starting diagrams computation at four-loop in model "<>AN$Model,Bold,Darker@Green] ]];
  
   
   If[couplingsSpec=!="None",
      If[Dimensions[couplingsSpec][[2]]=!=3,Print[Style["Error: ",Red], "Incorrect syntax in CouplingsOrder"]&&Abort[]];
   ];

   If[nopropSpec=!="None",
      If[Dimensions[nopropSpec][[2]]=!=3,Print[Style["Error: ",Red], "Incorrect syntax in RemovePropagator"]&&Abort[]];
   ];

   (* Create directory of the process *)

   (* The log and diagrams file are written in the new directory of the process  *)

   outlog=outp<>"/diagrams_"<>loops<>".log";
   outdiagrams=processName<>"_"<>loops;

   (* The control card of QG has to be saved in the folder where QGraf is installed *)

   outcard="cardQG_"<>processName<>".dat";
   outfile=Global`$QGrafPath<>"/"<>outcard;

   Print["Writing control file for QGraf under the name "<>outcard];
   OpenWrite[outfile];
      WriteString[outfile,"\n config= noblanks ;"<>"\n\n"];
      If[messageFile=="None",WriteString[outfile,"% messages= 'msg.txt' ;"<>"\n\n"],WriteString[outfile," messages= '"<>messageFile<>"' ;\n\n"]];
      WriteString[outfile," output= '"<>outdiagrams<>"';\n\n"];
      WriteString[outfile," style= '"<>style<>"' ;\n\n"];
      WriteString[outfile," model= '"<>model<>"' ;\n\n"];
      WriteString[outfile," in= "<>QGin<>" ;\n\n"];
      WriteString[outfile," out= "<>QGout<>" ;\n\n"];
      WriteString[outfile," loops= "<>loops<>" ;\n\n"];
      WriteString[outfile," loop_momentum= k;"<>"\n\n"];
      If[qgoptions=="None",WriteString[outfile," options=  ;"<>"\n\n"],WriteString[outfile," options= "<>qgoptions<>" ;\n\n"]];
      If[partition=="None",WriteString[outfile,""],WriteString[outfile," partition= "<>partition<>" ;\n\n"]];
      WriteString[outfile,"% index_offset= 255 ;"<>"\n\n"];
      WriteString[outfile,"% true= vsum[w,0,1] ;"<>"\n\n"];
      If[couplingsSpec=!="None", (WriteString[outfile," true = vsum["<>((StringDelete[#,"}"]&)@(StringDelete[#,"{"]&)@ToString[#])<>"];\n\n"]&)/@couplingsSpec];
      If[nopropSpec=!="None", (WriteString[outfile," true = iprop["<>((StringDelete[#,"}"]&)@(StringDelete[#,"{"]&)@ToString[#])<>"];\n\n"]&)/@nopropSpec];
   Close[outfile];

   (* Generate output in QGraf folder to avoid column limit in the output path *)
   outdiagrams2=outp<>"/"<>processName<>"_"<>loops;

   (* Running in the command line *)

   Print["Running QGraf..."];
   SetDirectory[Global`$QGrafPath];

  DeleteFileIfExistsQ[outdiagrams];

  DeleteFileIfExistsQ[outlog];
   Run["./qgraf "<>outcard<>" > "<>outlog];

   MoveFileIfExistsQ[outdiagrams, outdiagrams2];
   log=Import[outlog, "Text"];
   log=StringReplace[log,"--------------------------------------------------------------\n"~~x___~~"--------------------------------------------------------------\n":>""];
   (* Set directory back to the Notebook directory *)

   If[Quiet[NotebookDirectory[] ] === $Failed, SetDirectory[HomeDirectory[] ], SetDirectory[NotebookDirectory[] ]  ];

   Print["Diagrams written in the file "<>ToString[Style[outdiagrams2,Darker@Green],StandardForm] ];
];







