
#procedure Gamma5Subs
.sort

repeat;
id GammaM(5,Spin1?,Spin2?)*GammaM(1,Spin2?,Spin3?) = GammaM(5,Spin1,Spin3);
id GammaM(1,Spin1?,Spin2?)*GammaM(5,Spin2?,Spin3?) = GammaM(5,Spin1,Spin3);
id GammaM(5,Spin1?,Spin2?)*GammaM(5,Spin2?,Spin3?) = GammaM(1,Spin1,Spin3);
endrepeat;
.sort

*************FOR IN-BUILT Gamma5 NO EXTRA "im" REQUIRED ********

*if( match( GammaM(?a,5,?b,Spin1?,Spin2?) ));
*	  repeat id GammaM(?a,5,?b,Spin1?,Spin2?) = im*GammaM(?a,five,?b,Spin1,Spin2); 
*	  repeat id GammaM(?a,five,?b,Spin1?,Spin2?) = GammaM(?a,5,?b,Spin1,Spin2); 
*endif;
*.sort

*id GammaM(Lor1?,Spin1?,Spin2?)*GammaM(5,Spin2?,Spin3?) = GammaM(Lor1,5,Spin1,Spin3);
*id GammaM(p1?,Spin1?,Spin2?)*GammaM(5,Spin2?,Spin3?) = GammaM(p1,5,Spin1,Spin3);
*.sort
*id T(?a) = TM(?a);
*id f = fabc;
.sort

#endprocedure Gamma5Subs

#procedure PolarizationSum
.sort

id SpinorU(?a,p1?,mass?,Spin1?,Colour1?)*SpinorUbarC(?a,p1?,mass?,SpinC1?,ColourC1?) = ( GammaM(p1,Spin1,SpinC1) + mass*GammaM(1,Spin1,SpinC1) )*d_(Colour1,ColourC1);
id SpinorV(?a,p1?,mass?,Spin1?,Colour1?)*SpinorVbarC(?a,p1?,mass?,SpinC1?,ColourC1?) = ( GammaM(p1,Spin1,SpinC1) - mass*GammaM(1,Spin1,SpinC1) )*d_(Colour1,ColourC1);
id SpinorUC(?a,p1?,mass?,Spin1?,Colour1?)*SpinorUbar(?a,p1?,mass?,SpinC1?,ColourC1?) = ( GammaM(p1,Spin1,SpinC1) + mass*GammaM(1,Spin1,SpinC1) )*d_(Colour1,ColourC1);
id SpinorVC(?a,p1?,mass?,Spin1?,Colour1?)*SpinorVbar(?a,p1?,mass?,SpinC1?,ColourC1?) = ( GammaM(p1,Spin1,SpinC1) - mass*GammaM(1,Spin1,SpinC1) )*d_(Colour1,ColourC1);
.sort

id SpinorU(?a,p1?,mass?,Spin1?)*SpinorUbarC(?a,p1?,mass?,SpinC1?) = GammaM(p1,Spin1,SpinC1) + mass*GammaM(1,Spin1,SpinC1);
id SpinorV(?a,p1?,mass?,Spin1?)*SpinorVbarC(?a,p1?,mass?,SpinC1?) = GammaM(p1,Spin1,SpinC1) - mass*GammaM(1,Spin1,SpinC1);
id SpinorUC(?a,p1?,mass?,Spin1?)*SpinorUbar(?a,p1?,mass?,SpinC1?) = GammaM(p1,Spin1,SpinC1) + mass*GammaM(1,Spin1,SpinC1);
id SpinorVC(?a,p1?,mass?,Spin1?)*SpinorVbar(?a,p1?,mass?,SpinC1?) = GammaM(p1,Spin1,SpinC1) - mass*GammaM(1,Spin1,SpinC1);
.sort

id PolV(?a,Lor2?,p3?,0)*PolVC(?a,LorC2?,p3?,0) = - d_(Lor2, LorC2);
id PolV(?a,Lor2?,p3?,mass?)*PolVC(?a,LorC2?,p3?,mass?) = - d_(Lor2, LorC2) + ( p3(Lor2)*p3(LorC2) )/mass^2;
id PolV(?a,Lor2?, p1?, 0, Gluon1?)*PolVC(?a,LorC2?, p1?, 0, GluonC1?) = (-d_(Lor2, LorC2) + Flag*( p1(Lor2) * nn(LorC2) + nn(Lor2) * p1(LorC2) )/nn.p1  )*d_(Gluon1, GluonC1);
.sort

#endprocedure PolarizationSum

#procedure Conjugation
.sort

id im = -im;
id SpinorU(?a) = SpinorUbarC(?a);
id SpinorV(?a) = SpinorVbarC(?a);
id SpinorUbar(?a) = SpinorUC(?a);
id SpinorVbar(?a) = SpinorVC(?a);
*if( match( GammaM(Lor1?, 5, Spin1?,Spin2?) ) || match( GammaM(5, Lor1?, Spin1?,Spin2?) ) );
id GammaM(Lor1?, 5, Spin1?, Spin2?) = -GammaM(Lor1, 5, Spin1, Spin2);
id GammaM(5, Lor2?, Spin1?, Spin2?) = -GammaM(5, Lor2, Spin1, Spin2);
*endif;
id GammaM(Lor1?, Lor2?, Spin1?, Spin2?) = GammaM(Lor2, Lor1, Spin2, Spin1);
*if( match( GammaM(5, Spin1?,Spin2?) )  );
id GammaM(5, Spin1?, Spin2?) = -GammaM(5, Spin1, Spin2);
*endif;
id GammaM(Lor1?, Spin1?, Spin2?) = GammaM(Lor1, Spin2, Spin1);
id TM(Colour1?, Colour2?,?a) = TM(Colour2, Colour1,?a);
id PolV(?a) = PolVC(?a);
.sort
argument;
#do i = 1,999
  id Spin`i' = SpinC`i';
  id iSpin`i' = iSpinC`i';
  id Lor`i' = LorC`i';
  id iLor`i' = iLorC`i';
  id Colour`i' = ColourC`i';
  id iColour`i' = iColourC`i';
  id Gluon`i' = GluonC`i';
  id iGluon`i' = iGluonC`i';
#enddo
endargument;
.sort
#endprocedure Conjugation

#procedure ColourSubs
.sort

id fabc(?a) = f(?a);
id TM(?a) = T(?a);
.sort

#endprocedure ColourSubs


#procedure DiracTraces
.sort

id GammaM(mu1?,Spin1?,Spin2?) =  GammaM(mu1,Spin1,Spin2);
id GammaM(p1?,Spin1?,Spin2?) =  GammaM(p1,Spin1,Spin2);
.sort

repeat;
id GammaM(1,Spin1?,Spin2?)*GammaM(?a,Spin2?,Spin3?) = GammaM(?a,Spin1,Spin3);
id GammaM(1,Spin3?,Spin2?)*GammaM(?a,Spin1?,Spin3?) = GammaM(?a,Spin1,Spin2);
id GammaM(1,Spin1?,Spin2?)*GammaM(1,Spin2?,Spin3?) = GammaM(1,Spin1,Spin3);
id GammaM(5,Spin1?,Spin2?)*GammaM(5,Spin2?,Spin3?) = GammaM(1,Spin1,Spin3);
endrepeat;
.sort

id GammaM(mu1?,Spin1?,Spin2?) =  GammaM(mu1,Spin1,Spin2);
id GammaM(p1?,Spin1?,Spin2?) =  GammaM(p1,Spin1,Spin2);
.sort

repeat;
id GammaM(?a,Spin1?,iSpin2?)*GammaM(?b,iSpin2?,Spin3?) = GammaM(?a,?b,Spin1,Spin3);
id GammaM(?a,Spin1?,iSpin2?)*GammaM(?b,iSpin2?,Spin1?) = GammaM(?a,?b,Spin1,Spin1);
endrepeat;
.sort


id GammaM(?a, Spin1?, Spin1?)*GammaM(?b, Spin2?, Spin2?)*GammaM(?c, Spin3?, Spin3?)*GammaM(?d, Spin4?, Spin4?)*GammaM(?e, Spin5?, Spin5?)*GammaM(?f, Spin6?, Spin6?) = GammaMF(1, ?a)*GammaMF(2, ?b)*GammaMF(3, ?c)*GammaMF(4, ?d)*GammaMF(5, ?e)*GammaMF(6, ?f);
id GammaM(?a, Spin1?, Spin1?)*GammaM(?b, Spin2?, Spin2?)*GammaM(?c, Spin3?, Spin3?)*GammaM(?d, Spin4?, Spin4?)*GammaM(?e, Spin5?, Spin5?) = GammaMF(1, ?a)*GammaMF(2, ?b)*GammaMF(3, ?c)*GammaMF(4, ?d)*GammaMF(5, ?e);
id GammaM(?a, Spin1?, Spin1?)*GammaM(?b, Spin2?, Spin2?)*GammaM(?c, Spin3?, Spin3?)*GammaM(?d, Spin4?, Spin4?) = GammaMF(1, ?a)*GammaMF(2, ?b)*GammaMF(3, ?c)*GammaMF(4, ?d);
id GammaM(?a, Spin1?, Spin1?)*GammaM(?b, Spin2?, Spin2?)*GammaM(?c, Spin3?, Spin3?) = GammaMF(1, ?a)*GammaMF(2, ?b)*GammaMF(3, ?c);
id GammaM(?a, Spin1?, Spin1?)*GammaM(?b, Spin2?, Spin2?) = GammaMF(1, ?a)*GammaMF(2, ?b);
*id GammaM(5, Spin1?, Spin1?) = GammaMF(1,5);
*repeat id GammaM(1, Spin1?, Spin1?) = gi_(1);
*repeat id GammaMF(1, Spin1?, Spin1?) = gi_(1);
id GammaM(?a, Spin1?, Spin1?) = GammaMF(1, ?a);
.sort


if( match( GammaMF(1,?a) ));
	repeat id GammaMF(1,?a) = GammaMF1(?a);
	Chainout GammaMF1;
	id GammaMF1(mu1?) =  GammaMF1(mu1);
	id GammaMF1(5) = g5_(1);
	id GammaMF1(1) = gi_(1);
	Chainin GammaMF1;
	repeat id GammaMF1(?a)*g5_(1)*GammaMF1(?b) = g_(1,?a,5_,?b);
	repeat id GammaMF1(?a)*gi_(1)*GammaMF1(?b) = g_(1,?a,?b);
	repeat id GammaMF1(?a)*GammaMF1(5)*GammaMF1(?b) = g_(1,?a,5_,?b);
	repeat id GammaMF1(?a)*GammaMF1(1)*GammaMF1(?b) = g_(1,?a,?b);
	repeat id GammaMF1(?a) = g_(1,?a);
endif;

if( match( GammaMF(2,?a) ));
	repeat id GammaMF(2,?a) = GammaMF2(?a);
	Chainout GammaMF2;
	id GammaMF2(mu1?) =  GammaMF2(mu1);
	id GammaMF2(5) = g5_(2);
	id GammaMF2(1) = gi_(2);
	Chainin GammaMF2;
	repeat id GammaMF2(?a)*g5_(2)*GammaMF2(?b) = g_(2,?a,5_,?b);
	repeat id GammaMF2(?a)*gi_(2)*GammaMF2(?b) = g_(2,?a,?b);
	repeat id GammaMF2(?a)*GammaMF2(5)*GammaMF2(?b) = g_(2,?a,5_,?b);
	repeat id GammaMF2(?a)*GammaMF2(1)*GammaMF2(?b) = g_(2,?a,?b);
	repeat id GammaMF2(?a) = g_(2,?a);
endif;

if( match( GammaMF(3,?a) ));
	repeat id GammaMF(3,?a) = GammaMF3(?a);
	Chainout GammaMF3;
	id GammaMF3(mu1?) =  GammaMF3(mu1);
	id GammaMF3(5) = g5_(3);
	id GammaMF3(1) = gi_(3);
	Chainin GammaMF3;
	repeat id GammaMF3(?a)*g5_(3)*GammaMF3(?b) = g_(3,?a,5_,?b);
	repeat id GammaMF3(?a)*gi_(3)*GammaMF3(?b) = g_(3,?a,?b);
	repeat id GammaMF3(?a)*GammaMF3(5)*GammaMF3(?b) = g_(3,?a,5_,?b);
	repeat id GammaMF3(?a)*GammaMF3(1)*GammaMF3(?b) = g_(3,?a,?b);
	repeat id GammaMF3(?a) = g_(3,?a);
endif;

if( match( GammaMF(4,?a) ));
	repeat id GammaMF(4,?a) = GammaMF4(?a);
	Chainout GammaMF4;
	id GammaMF4(mu1?) =  GammaMF4(mu1);
	id GammaMF4(5) = g5_(4);
	id GammaMF4(1) = gi_(4);
	Chainin GammaMF4;
	repeat id GammaMF4(?a)*g5_(4)*GammaMF4(?b) = g_(4,?a,5_,?b);
	repeat id GammaMF4(?a)*gi_(4)*GammaMF4(?b) = g_(4,?a,?b);
	repeat id GammaMF4(?a)*GammaMF4(5)*GammaMF4(?b) = g_(4,?a,5_,?b);
	repeat id GammaMF4(?a)*GammaMF4(1)*GammaMF4(?b) = g_(4,?a,?b);
	repeat id GammaMF4(?a) = g_(4,?a);
endif;

if( match( GammaMF(5,?a) ));
	repeat id GammaMF(5,?a) = GammaMF5(?a);
	Chainout GammaMF5;
	id GammaMF5(mu1?) =  GammaMF5(mu1);
	id GammaMF5(5) = g5_(5);
	id GammaMF5(1) = gi_(5);
	Chainin GammaMF5;
	repeat id GammaMF5(?a)*g5_(5)*GammaMF5(?b) = g_(5,?a,5_,?b);
	repeat id GammaMF5(?a)*gi_(5)*GammaMF5(?b) = g_(5,?a,?b);
	repeat id GammaMF5(?a)*GammaMF5(5)*GammaMF5(?b) = g_(5,?a,5_,?b);
	repeat id GammaMF5(?a)*GammaMF5(1)*GammaMF5(?b) = g_(5,?a,?b);
	repeat id GammaMF5(?a) = g_(5,?a);
endif;

if( match( GammaMF(6,?a) ));
	repeat id GammaMF(6,?a) = GammaMF6(?a);
	Chainout GammaMF6;
	id GammaMF6(mu1?) =  GammaMF6(mu1);
	id GammaMF6(5) = g5_(6);
	id GammaMF6(1) = gi_(6);
	Chainin GammaMF6;
	repeat id GammaMF6(?a)*g5_(6)*GammaMF6(?b) = g_(6,?a,5_,?b);
	repeat id GammaMF6(?a)*gi_(6)*GammaMF6(?b) = g_(6,?a,?b);
	repeat id GammaMF6(?a)*GammaMF6(5)*GammaMF6(?b) = g_(6,?a,5_,?b);
	repeat id GammaMF6(?a)*GammaMF6(1)*GammaMF6(?b) = g_(6,?a,?b);
	repeat id GammaMF6(?a) = g_(6,?a);
endif;
.sort

Bracket g_,GammaM,GammaMF;
.sort
*collect dummy;
*.sort
*id dummy(?a)  = 1;
*.sort

trace4, 1;
.sort
trace4, 2;
.sort
trace4, 3;
.sort
trace4, 4;
.sort
trace4, 5;
.sort
trace4, 6;
.sort

contract;
.sort

#endprocedure DiracTraces

#procedure StripPolSpinor
.sort

 id PolV(x?,p1?,p1?,0,Gluon1?) = 0;
 id PolV(x?partTagExt[n],Lor3?,p1?,0,Gluon1?)*GammaM(Lor3?,?b,Spin1?,Spin2?) = GammaM(ExtLor[n],?b,Spin1,Spin2);
 id PolV(x?partTagExt[n],Lor3?,p1?,0,Gluon1?)*GammaM(?a,Lor3?,?b,Spin1?,Spin2?) = GammaM(?a,ExtLor[n],?b,Spin1,Spin2);
 id PolV(x?partTagExt[n],Lor3?,p1?,0,Gluon1?)*GammaM(?a,Lor3?,Spin1?,Spin2?) = GammaM(?a,ExtLor[n],Spin1,Spin2);
*  id SpinorU(x?partTagExt[n],p1?,mass?,Spin1?,Colour1?)*TM(?a,Colour1?,?b)= TM(?a,ExtColour[n],?b);
*  id SpinorV(x?partTagExt[n],p1?,mass?,Spin1?,Colour1?)*TM(?a,Colour1?,?b)= TM(?a,ExtColour[n],?b);
*  id SpinorUbar(x?partTagExt[n],p1?,mass?,Spin1?,Colour1?)*TM(?a,Colour1?,?b)= TM(?a,ExtColour[n],?b);
*  id SpinorVbar(x?partTagExt[n],p1?,mass?,Spin1?,Colour1?)*TM(?a,Colour1?,?b)= TM(?a,ExtColour[n],?b);
 id SpinorU(x?partTagExt[n],p1?,mass?,Spin1?,Colour1?)*T(Colour2?,Colour1?,?b)= T(Colour2,ExtColour[n],?b);
 id SpinorV(x?partTagExt[n],p1?,mass?,Spin1?,Colour1?)*TM(?a,Colour1?,?b)= TM(?a,ExtColour[n],?b);
 id SpinorUbar(x?partTagExt[n],p1?,mass?,Spin1?,Colour1?)*T(Colour1?,Colour2?,?b)= T(ExtColour[n],Colour2,?b);
 id SpinorVbar(x?partTagExt[n],p1?,mass?,Spin1?,Colour1?)*TM(?a,Colour1?,?b)= TM(?a,ExtColour[n],?b);
 id PolV(x?partTagExt[n],Lor3?,p1?,0)*PolV(y?partTagExt[n2],Lor3?,p2?,0) = d_(ExtLor[n],ExtLor[n2]);
 id PolV(x?partTagExt[n],Lor3?,p1?,0,Gluon1?)*PolV(y?partTagExt[n2],Lor3?,p2?,0,Gluon2?) = d_(ExtLor[n],ExtLor[n2]);
 id PolV(x?partTagExt[n],Lor3?,p1?,mass?)*PolV(y?partTagExt[n2],Lor3?,p2?,mass?) = d_(ExtLor[n],ExtLor[n2]);
 id PolV(x?partTagExt[n],p2?,p1?,0)= p2(ExtLor[n]);
 id PolV(x?partTagExt[n],p2?,p1?,0,Gluon1?)= p2(ExtLor[n]);
 id PolV(x?partTagExt[n],p2?,p1?,mass?)= p2(ExtLor[n]);
 .sort

 id PolV(?a) = 1;
 id SpinorU(?a) = 1;
 id SpinorV(?a) = 1;
 id SpinorUbar(?a) = 1;
 id SpinorVbar(?a) = 1;
 .sort
#endprocedure StripPolSpinor

