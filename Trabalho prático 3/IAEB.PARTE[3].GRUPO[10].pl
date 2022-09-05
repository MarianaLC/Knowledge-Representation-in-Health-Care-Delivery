%------------------------------------------------------------------------------------------------------
% INTELIGÊNCIA ARTIFICIAL EM ENGENHARIA BIOMÉDICA

%---------------------------------------------------------------------------------
% TRABALHO PRÁTICO 3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SWI PROLOG: Declaracoes iniciais
:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%SWI PROLOG: Definições iniciais
:- op( 800,fx,se ).
:- op( 800,fx,facto ).
:- op( 700,xfx,entao ).
:- op( 300,xfy,ou ).
:- op( 200,xfy,e ).
:- op( 900,xfx,porque ).
:- op( 900,xfx,com ).
:- op( 900,xfx,:: ).
:- op( 900,xfx,::: ).
:- dynamic(('::')/2).
:- dynamic((':::')/2).
:- dynamic((facto)/1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: QUESTAO -> { V,F }
nao( QUESTAO ) :-
    QUESTAO, !, fail.
nao( QUESTAO ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado comprimento: Lista, Resultado -> {V, F}
comprimento([], 0) .
comprimento([X|L], R) :-
    comprimento(L,N),
    R is N+1 .

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado concatenar: L1,L2,R -> {V,F}
concatenar( [],L,L ).
concatenar( [X|L1],L2,[X|L3]) :- concatenar( L1,L2,L3 ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado solucoes: Arquetipo, Teorema, Solucoes -> {V,F}
solucoes(X,Y,Z) :- findall(X,Y,Z).

%---------------------------------------------------------------------------------------------------------------
% Extensao do meta-predicado rp:Diagnostico,Grau,Explicacao,Lista de Medicamentos, Lista de Tratamento->{V,F}
rp( D, G, (D com G) porque Jc, [M|LM],[T|LT]):-
	(se C entao D) :: (Gr,M,T),
	rp(C,Gc,Jc,LM,LT),
	prob(Gr,Gc,G).

rp((D1 e D2),G,(J1 e J2),LM,LT):-
	nonvar(D1),
	nonvar(D2),
	rp(D1,G1,J1,LM1,LT1),
	rp(D2,G2,J2,LM2,LT2),
	menor(G1,G2,G),
	concatenar(LM1,LM2,LM),
	concatenar(LT1,LT2,LT).

rp(D1 ou D2,G1,J1, LM1, LT1):-
	nonvar(D1),
	nonvar(D2),
	rp(D1,G1,J1,LM1,LT1),
	nao(rp(D2,G2,J2,LM2,LT2)).

rp(D1 ou D2,G2,J2, LM2, LT2):-
	nonvar(D1),
	nonvar(D2),
	rp(D2,G2,J2,LM2,LT2),
	nao(rp(D1,G1,J1,LM1,LT1)).

rp(D1 ou D2,G,J1 e J2, LM, LT):-
	nonvar(D1),
	nonvar(D2),
	rp(D1,G1,J1,LM1,LT1),
	rp(D2,G2,J2,LM2,LT2),
	maior(G1,G2,G),
	concatenar(LM1,LM2,LM),
	concatenar(LT1,LT2,LT).

rp(D, G , D com G,[],[]):-
	(facto (D))::G.

%-------------------------------------------------------------------------------------------
% Regras de producao
facto stress('ligeiro') :: ('provavel').
facto stress('cronico') :: ('provavel').
facto ('nauseas') :: ('pouco provavel').
facto ('vomitos') :: ('muito provavel').
facto ('fadiga') :: ('certo').
facto anorexia('ligeira') :: ('provavel').
facto ('insonia') :: ('provavel').
facto ('hipertensao') :: ('muito provavel').
facto ('edema') :: ('pouco provavel').
facto ('oliguria') :: ('certo').

se stress('ligeiro') entao 'ansiedade'::('certo','Lorazepam', '2 a 3 comprimidos de 1 mg por dia').
se 'ansiedade' ou stress('cronico') entao anorexia('mental')::('provavel','Psicoterapia', '2 sessoes por semana').
se 'nauseas' e 'vomitos' e 'fadiga' e anorexia('ligeira') entao 'nefrite'::('muito provavel','Prednisona','5mg a 60mg por dia').
se 'nefrite' e 'insonia' e 'hipertensao' entao 'glomerulonefrite'::('provavel', 'Indapamida Eulex', '1 comprimido por dia').
se 'edema' e 'oliguria' e 'glomerulonefrite' entao 'sindrome nefritica'::('pouco provavel' ,'Aldactone' , '25 mg a 200 mg por dia').

%-------------------------------------------------------------------------------------------
% PREDICADOS AUXILIARES
% Extensão de predicado menor: GrauQ1, GrauQ2, Menor ->{V,F}
menor(X,X,X).
menor('improvavel',X,'improvavel').
menor(X,'improvavel','improvavel').
menor('certo',X,X).
menor(X,'certo',X).
menor('muito provavel','provavel','provavel').
menor('provavel','muito provavel','provavel').
menor('muito provavel','pouco provavel','pouco provavel').
menor('pouco provavel','muito provavel','pouco provavel').
menor('pouco provavel','provavel','pouco provavel').
menor('provavel','pouco provavel','pouco provavel'). 

%---------------------------------------------------------------------------------------------------------
% Extensão de predicado maior: GrauQ1, GrauQ2, Maior ->{V,F}
maior(X,X,X).
maior('certo',X,'certo').
maior(X,'certo','certo').
maior('improvavel',X,X).
maior(X,'improvavel',X).
maior('muito provavel','provavel','muito provavel').
maior('provavel','muito provavel','muito provavel').
maior('muito provavel','pouco provavel','muito provavel').
maior('pouco provavel','muito provavel','muito provavel').
maior('pouco provavel','provavel','provavel').
maior('provavel','pouco provavel','provavel'). 

%-----------------------------------------------------------------------------------------------------------
% Extensão de predicado prob: Gr, Gc, G ->{V,F}
prob('certo','certo','certo').
prob('muito provavel','muito provavel','muito provavel').
prob('provavel','provavel','provavel').
prob('pouco provavel','pouco provavel','pouco provavel').
prob('improvavel','improvavel','improvavel').
prob('provavel','certo','muito provavel'). 
prob('certo','provavel','muito provavel').
prob('certo','improvavel','provavel'). 
prob('improvavel','certo','provavel').
prob('muito provavel','certo','muito provavel'). 
prob('certo','muito provavel','muito provavel').
prob('certo','pouco provavel','provavel'). 
prob('pouco provavel','certo','provavel'). 
prob('muito provavel','provavel','muito provavel').
prob('provavel','muito provavel','muito provavel').
prob('muito provavel','improvavel','pouco provavel').
prob('improvavel','muito provavel','pouco provavel'). 
prob('muito provavel','pouco provavel','provavel').
prob('pouco provavel','muito provavel','provavel').
prob('pouco provavel','provavel','pouco provavel').
prob('provavel','pouco provavel','pouco provavel'). 
prob('provavel','improvavel','pouco provavel'). 
prob('improvavel','provavel','pouco provavel').
prob('improvavel','pouco provavel','improvavel').
prob('pouco provavel','improvavel','improvavel').

%-----------------------------------------------------------------------------------------------------------
% Extensão do predicado insercao: Termo, Certeza -> {V, F}
insercao(Termo,Certeza) :-
    assert(facto(Termo)::Certeza).
insercao(Termo, Certeza ) :-
    retract(facto(Termo)::Certeza), !, fail.

% Extensão do predicado teste: Lista -> {V,F}
teste([]).
teste([I|L]) :-
    I, teste(L).

% Extensão do predicado grau: Grau -> {V, F}
grau('improvavel').
grau('pouco provavel').
grau('provavel').
grau('muito provavel').
grau('certo').

%-----------------------------------------------------------------------------------------------------------
% EVOLUÇÃO DE CONHECIMENTO
% Extensão do predicado evolucao_factos: Termo, Certeza -> {V, F}
evolucao_factos(Termo, Certeza) :-
    solucoes( Invariante, +(facto (Termo)::Certeza):::Invariante,Lista),
    insercao(Termo, Certeza),
    grau(Certeza),
    teste(Lista).

% INVOLUÇÃO DE CONHECIMENTO
% Extensão do predicado involucao_factos: Termo, Certeza -> {V, F}
involucao_factos(Termo,Certeza) :-
    solucoes( Invariante, -(facto (Termo)::Certeza):::Invariante,Lista),
    remover(Termo,Certeza),
    grau(Certeza),
    teste( Lista ).

% Extensão do predicado remover: Termo, Certeza -> {V, F}
remover(Termo,Certeza) :-
    retract(facto(Termo)::Certeza).
remover(Termo,Certeza) :-
    assert(facto(Termo)::Certeza), !, fail.

%-----------------------------------------------------------------------------------------------------------
% INVARIANTES
%Invariante que não permite a inserção de conhecimento repetido na base de conhecimento.
+(facto(Termo) :: Certeza) ::: 
    (solucoes((Termo), (facto(Termo) :: Certeza), S), 
    comprimento(S,N), N==1).

%Invariante que permite remover um facto caso este exista na base de conhecimento.
-(facto(Termo) :: Certeza) ::: 
    (solucoes((Termo), (facto(Termo) :: Certeza), S),
    comprimento(S,N), N==0).