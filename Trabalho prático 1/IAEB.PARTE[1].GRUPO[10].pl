% INTELIGÊNCIA ARTIFICIAL EM ENGENHARIA BIOMÉDICA

%------------------------------------------------------------------------------------------------------
% TRABALHO PRÁTICO 1

%------------------------------------------------------------------------------------------------------
% SWI PROLOG: Declarações iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------------------------------------------------------------------------------
% SWI PROLOG: Definições iniciais

:- op (900,xfy,'::').
:- dynamic(utente/5).
:- dynamic(prestador/7).
:- dynamic(ato/6).

%-------------------------------------------------------------------------------------------------------
%Extensão do predicado utente: ID Utente, Nome, Idade, Cidade, Sexo -> {V,F}
utente(1001, laravaz, 22, braga, feminino).
utente(1002, marianalindo, 20, vianadocastelo, feminino).
utente(1003, tiagonovais, 21, fafe, masculino).
utente(1004, luisvaz, 47, braga, masculino).
utente(1005, manuelmartins, 86, barcelos, masculino).
utente(1006, mariasilva, 3, famalicao, feminino).
utente(1007, luisparente, 44 , braga, masculino).
utente(1008, teresaferreira, 5, porto, feminino).
utente(1009, nunocosta, 29, viladoconde, masculino).
utente(1010, catarinacameira, 15, vianadocastelo, feminino).
utente(1011, paulolopes, 58, porto, masculino).

% Extensão do predicado prestador: Id Prestador, Tipo, Especialidade, Nome, Instituição, Cidade e Sexo -> {V,F}
prestador(2001,medico,cardiologia,ruialves,cuf,braga,masculino). 
prestador(2002,enfermeiro,pediatria,anasilva,trofasaude,porto,feminino).
prestador(2003,medico,medicinainterna,teresaalves,publico,vianadocastelo,feminino). 
prestador(2004,enfermeiro,oftalmologia,goretiparente,publico,vianadocastelo, feminino).
prestador(2005,medico,pneumologia,luismartins,cuf,braga,masculino). 
prestador(2006,medico,dentaria,jorgeamorim,trofasaude,porto, masculino).
prestador(2007,medico,dermatologia,diogopereira,trofasaude,braga,masculino). 
prestador(2008,medico,pediatria,rosasilva,publico,braga, feminino).

% Extensão do predicado ato: Id Ato, Data, ID Utente, ID Prestador, Descricao e Custo -> {V,F}
ato(3001, 051121, 1001, 2001, consulta,20). 
ato(3002, 080921, 1008, 2002, curativo,5).
ato(3003, 080921, 1006, 2008, consulta,15).
ato(3004, 070520, 1003, 2003, internamento,50).
ato(3005, 070520, 1007, 2004, exame,16).
ato(3006, 231120, 1002, 2005, consulta,21).
ato(3007, 010921, 1010, 2006, limpezadentaria,34).
ato(3008, 050720, 1004, 2004, consulta,8).
ato(3005, 210821, 1007, 2004, exame,16).
ato(3006, 031021, 1011, 2007, consulta,20).
%-------------------------------------------------------------------------------------------------------
% PREDICADOS AUXILIARES
% Extensão do predicado solucoes: X,Y,Z --> {V,F} 
solucoes( X,Y,Z ) :- 
	findall( X,Y,Z ).

% Extensão do predicado comprimento: Lista, Resultado -> {V,F} comprimento( [],0 ). 
comprimento( [],0 ).
comprimento( [X|L],R ) :- 
	comprimento( L,N ), 
	R is N+1.

% Extensão do predicado eliminarelemento: Elemento, Lista, Resultado -> {V,F}
eliminarelemento( X,[],[] ).
eliminarelemento( X,[X|L],NL ) :- 
	eliminarelemento(X,L,NL).
eliminarelemento( X,[Y|L],[Y|NL] ) :- 
	X \== Y, 
	eliminarelemento( X,L,NL ).

% Extensão do predicado eliminarrepetidos: Lista, Resultado -> {V,F}
eliminarrepetidos( [],[] ).
eliminarrepetidos( [A|B],[A|L] ) :-  
	eliminarelemento( A,B,NL ),
	eliminarrepetidos( NL,L ).

% Extensão do predicado soma: Lista, Resultado --> {V,F}
somal( [X],X ).
somal( [N|L],R ):- 
	somal( L,R1 ),
	R is N+R1.

% Extensão do predicado insercao: Termo -> {V,F}
insercao( Termo ) :-
	assert( Termo ).
insercao( Termo ) :-
	retract( Termo ), !, fail.

% Extensão do predicado teste: Lista -> {V,F}
teste( [] ).
teste( [I|L] ) :- I, teste(L). 

% Extensão do predicado remocao: Termo -> {V,F}
remocao( Termo ) :- retract(Termo).
remocao( Termo ) :- assert (Termo), !, fail.


%-------------------------------------------------------------------------------------------------------
% IDENTIFICAR UTENTES POR CRITERIOS DE SELECAO

% Extensao do predicado que permite identificar utentes por ato
% utentes_ato: Ato, Resultado -> {V,F}
utentes_ato(B,R) :- 
    solucoes(A, (ato(_,_,A,_,B,_), utente(A,_,_,_,_)), Y),
    eliminarrepetidos(Y,R).

% Extensao do predicado que permite identificar utentes por ato
% utentes_tipoprestador: Tipo de Prestador, Resultado -> {V,F}
utentes_tipoprestador(B,R) :- 
    solucoes(C, (prestador(A,B,_,_,_,_,_), ato(_,_,C,A,_,_), utente(C,_,_,_,_)), Y),
    eliminarrepetidos(Y,R).

% Extensão do predicado que permite identificar utentes por especialidade de prestador
%utentes_especialidade: Especialidade, Resultado -> {V,F} 
utentes_especialidade( D,R ) :-
	solucoes( A, ( utente(A,_,_,_,_),
	ato(_,_,A,C,_,_),
	prestador(C,_,D,_,_,_,_) ), Y ),
	eliminarrepetidos( Y,R ).

% Extensao do predicado que permite identificar utentes por cidade
% utentes_cidade: Cidade, Resultado -> {V,F}
utentes_cidade( B,R ) :- 
	solucoes( A, utente(A,_,_,B,_), R ).

% Extensao do predicado que permite identificar utentes por sexo
% utentes_sexo: Sexo, Resultado -> {V,F}
utentes_sexo( B,R ) :-
	solucoes( A, utente(A,_,_,_,B), R ).

% Extensao do predicado que permite identificar utentes por data
% Extensão do predicado utentes_data: Data, Resultado -> {V,F} 
utentes_data( B,R ) :-
	solucoes( A, ( utente(A,_,_,_,_), ato(_,B,A,_,_,_)), Y),
	eliminarrepetidos( Y,R ).

% Extensao do predicado que permite identificar utentes por custo
% Extensão do predicado utentes_custo: Custo, Resultado -> {V,F} 
utentes_custo( B,R ) :-
	solucoes( A, ( utente(A,_,_,_,_), ato(_,_,A,_,_,B)), Y),
	eliminarrepetidos( Y,R ).

% Extensao do predicado que permite identificar utentes por prestador
% utentes_prestador: Prestador, Resultado -> {V,F}
utentes_prestador(B,R) :- 
    solucoes(A, (ato(_,_,A,B,_,_), utente(A,_,_,_,_), prestador(B,_,_,_,_,_,_)), Y),
    eliminarrepetidos(Y,R).

% Extensão do predicado que permite identificar os utentes de uma instituição de uma  cidade
% utentes_instituicao: Instituição, Resultado -> {V,F} 
utentes_instituicao(C,R) :- 
    solucoes((A,D), ( utente(A,_,_,_,_), ato(_,_,A,B,_,_), prestador(B,_,_,_,C,D,_)), Y ),
    eliminarrepetidos(Y,R).

%-------------------------------------------------------------------------------------------------------
% IDENTFICAR OS ATOS POR CRITÉRIOS DE SELEÇÃO

% Extensão do predicado que permite identificar os atos prestados por prestador
% atos_prestador: Prestador, Resultado -> {V,F} 
atos_prestador(A,R) :- 
    solucoes(B, (ato(_,_,_,A,B,_), prestador(A,_,_,_,_,_,_)), Y),
    eliminarrepetidos(Y,R).

% Extensão do predicado que permite identificar os atos prestados por tipo de prestador
% atos_tipoprestador: Tipo de Prestador, Resultado -> {V,F} 
atos_tipoprestador(C,R) :- 
    solucoes(B, (ato(_,_,_,A,B,_), prestador(A,C,_,_,_,_,_)), Y),
    eliminarrepetidos(Y,R).
   
% Extensão do predicado que permite identificar os atos prestados por especialidade
% atos_especialidade: Especialidade, Resultado -> {V,F} 
atos_especialidade(C,R) :- 
    solucoes(B, (ato(_,_,_,A,B,_), prestador(A,_,C,_,_,_,_)), Y),
    eliminarrepetidos(Y,R).
   
% Extensão do predicado que permite identificar os atos prestados por cidade
% atos_cidade: Cidade, Resultado -> {V,F} 
atos_cidade(C,R) :-
    solucoes(B, (ato(_,_,_,A,B,_), prestador(A,_,_,_,_,C,_)), Y),
    eliminarrepetidos(Y,R).

% Extensão do predicado que permite identificar atos prestados por datas
% atos_data: Data, Resultado -> {V,F} 
atos_data(A,R) :- 
    solucoes(B, ato(_,A,_,_,B,_),Y), 
    eliminarrepetidos(Y,R).
        
% Extensão do predicado que permite identificar atos prestados por custo
% atos_custo: Custo, Resultado -> {V,F} 
atos_custo(B,R) :- 
    solucoes(A, ato(_,_,_,_,A,B),Y),
    eliminarrepetidos(Y,R).

% Extensão do predicado que permite identificar os atos realizados por utente
% atos_utente: ID Utente, Resultado -> {V,F} 
atos_utente(A,R):- 
    solucoes(B, (ato(_,_,A,_,B,_), utente(A,_,_,_,_)), Y),
    eliminarrepetidos(Y,R).

% Extensão do predicado que permite identificar os atos realizados por instituição
% atos_instituicao: Instituição, Resultado -> {V,F} 
atos_instituicao(C,R) :- 
    solucoes(B, (ato(_,_,_,A,B,_), prestador(A,_,_,_,C,_,_)), Y),
    eliminarrepetidos(Y,R).

%------------------------------------------------------------------------------------------------------------------
% IDENTFICAR OS PRESTADORES POR CRITÉRIOS DE SELEÇÃO
% Extensão do predicado que permite identificar os prestadores por atos
% prestadores_atos: Ato, Resultado -> {V,F} 
prestadores_atos(B,R) :- 
    solucoes(A, (ato(_,_,_,A,B,_), prestador(A,_,_,_,_,_,_)), Y),
    eliminarrepetidos(Y,R).

% Extensão do predicado que permite identificar os prestadores por tipo de prestador
% prestadores_tipoprestador: Tipo de Prestador, Resultado -> {V,F} 
prestadores_tipoprestador(B,R) :- 
    solucoes(A, prestador(A,B,_,_,_,_,_), R).

% Extensão do predicado que permite identificar os prestadores por especialidade
% prestadores_especialidade: Especialidade, Resultado -> {V,F} 
prestadores_especialidade(B,R) :- 
    solucoes(A, prestador(A,_,B,_,_,_,_), R).
   
% Extensão do predicado que permite identificar os prestadores por cidade 
% prestadores_cidade: Cidade, Resultado -> {V,F} 
prestadores_cidade(B,R) :- 
    solucoes(A, prestador(A,_,_,_,_,B,_), R).

% Extensão do predicado que permite identificar os prestadores por sexo
% prestadores_sexo: Sexo, Resultado -> {V,F} 
prestadores_sexo(B,R) :- 
    solucoes(A, prestador(A,_,_,_,_,_,B), R).

% Extensão do predicado que permite identificar os prestadores por data em que prestaram serviço
% prestadores_data: Data, Resultado -> {V,F} 
prestadores_data(A,R) :- 
    solucoes(B, (ato(_,A,_,B,_,_), prestador(B,_,_,_,_,_,_)), Y),
    eliminarrepetidos(Y,R).
        
% Extensão do predicado que permite identificar os prestadores por custo dos seus atos prestados
% prestadores_custo: Custo, Resultado -> {V,F} 
prestadores_custo(B,R) :- 
    solucoes(A, (ato(_,_,_,A,_,B), prestador(A,_,_,_,_,_,_)), Y),
    eliminarrepetidos(Y,R).

% Extensão do predicado que permite identificar os prestadores por utente a que prestaram serviços
% prestadores_utente: Utente, Resultado -> {V,F} 
prestadores_utente(A,R) :- 
    solucoes(B, (ato(_,_,A,B,_,_), prestador(B,_,_,_,_,_,_), utente(A,_,_,_,_)), Y),
    eliminarrepetidos(Y,R).

% Extensão do predicado que permite identificar os prestadores por instituição
% prestadores_instituicao: Instituicao, Resultado -> {V,F} 
prestadores_instituicao(B,R) :- 
    solucoes(A, prestador(A,_,_,_,B,_,_), R).

%--------------------------------------------------------------------------------------------------------------
% EXTENSÃO DO PREDICADO QUE PERMITE CALCULAR O CUSTO TOTAL POR CRITÉRIOS DE SELEÇÃO

% Extensão do predicado que permite calcular o custo total por utente
% custototal_utente: Utente, Resultado -> {V,F} 
custototal_utente(A,R) :- 
    solucoes(B, (utente(A,_,_,_,_), ato(_,_,A,_,_,B)), Y), 
    somal(Y,R).

% Extensão do predicado que permite calcular o custo total por prestador
% custototal_prestador: Prestador, Resultado -> {V,F} 
custototal_prestador(A,R) :- 
    solucoes(B, ( prestador(A,_,_,_,_,_,_), ato(_,_,_,A,_,B)), Y), 
    somal(Y,R).

% Extensão do predicado que permite calcular o custo total por tipo de prestador
% custototal_tipoprestador: Tipo de prestador, Resultado -> {V,F} 
custototal_tipoprestador(B,R) :- 
    solucoes(C, ( prestador(A,B,_,_,_,_,_), ato(_,_,_,A,_,C)), Y), 
    somal(Y,R).

% Extensão do predicado que permite calcular o custo total por especialidade
% custototal_especialidade: Especialidade, Resultado -> {V,F} 
custototal_especialidade(B,R) :- 
    solucoes(C, ( prestador(A,_,B,_,_,_,_), ato(_,_,_,A,_,C)), Y), 
    somal(Y,R).

% Extensão do predicado que permite calcular o custo total por instituição
% custototal_instituicao: Instituição, Resultado -> {V,F} 
custototal_instituicao(B,R) :- 
    solucoes(C, ( prestador(A,_,_,_,B,_,_), ato(_,_,_,A,_,C)), Y), 
    somal(Y,R).

% Extensão do predicado que permite calcular o custo total por ato
% custototal_ato: Ato, Resultado -> {V,F} 
custototal_ato(A,R) :- 
    solucoes(B, ato(_,_,_,_,A,B),Y), 
    somal(Y,R).

% Extensão do predicado que permite calcular o custo total por data
% custototal_data: Data, Resultado -> {V,F} 
custototal_data(A,R) :- 
    solucoes(B, ato(_,A,_,_,_,B),Y), 
    somal(Y,R).

% PREDICADOS PRINCIPAIS
% Extensão do predicado que permite a inserção de conhecimento
% Termo -> {V,F} 
registar( Termo ) :- 
	solucoes(I,+Termo::I,Linv ),
	insercao( Termo ),
	teste( Linv ).

% Extensão do predicado que permite a remoção de conhecimento
%Termo -> {V,F} 
remover( Termo ) :- 
	solucoes( I, -Termo::I, Linv ), 
	teste( Linv ), 
	remocao( Termo ).

%------------------------------------------------------------------------------------------------------------------
% INVARIANTES

% Invariante para registar utente: não permite o registo de um utente com um ID já existente na base de conhecimento.
+utente( A,B,C,D,E ) :: (solucoes(A, utente( A,_,_,_,_), S),
					comprimento( S,N ), 
						N == 1).

%Invariante para remover utente: apenas permite a remoção de um dado utente se este não estiver associado a nenhum ato.
-utente( A,B,C,D,E ) :: ( solucoes( A, ato( _,_,A,_,_,_ ), S ),
					comprimento( S,N ), 
						N == 0).

% Invariante para registar prestador: não permite o registo de um prestador com um ID já existente na base de conhecimento.
+prestador( A,B,C,D,E,F,G ) :: ( solucoes( A, prestador( A,_,_,_,_,_,_), S),
						comprimento( S,N ), 
						N == 1).

%Invariante para remover prestador: apenas permite a remoção de um dado prestador se este não estiver associado a nenhum ato.
-prestador( A,B,C,D,E,F,G ) :: ( solucoes( A, ato( _,_,_,A,_,_ ), S ),
						comprimento( S,N ), 
						N == 0).

% Invariante para registar ato: não permite o registo de um ato com um ID já existente na base de conhecimento.
+ato( A,B,C,D,E,F) :: ( solucoes( A, ato( A,_,_,_,_,_), S ),
						comprimento( S,N ), 
						N == 1).

%Invariante para remover ato: apenas permite a remoção de um ato, se este não tiver nenhum ID, data, utente e prestador associado.
-ato( A,B,C,D,E,F) :: ( solucoes( A, ato( A,B,C,D,, ), S ),
                        comprimento( S,N ), 
                        N == 0).


