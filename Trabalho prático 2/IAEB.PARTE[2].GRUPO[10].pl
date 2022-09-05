%------------------------------------------------------------------------------------------------------
% INTELIGÊNCIA ARTIFICIAL EM ENGENHARIA BIOMÉDICA

%---------------------------------------------------------------------------------
% TRABALHO PRÁTICO 2

%---------------------------------------------------------------------------------
% SWI PROLOG: Declarações iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%---------------------------------------------------------------------------------
% SWI PROLOG: Definiçõess iniciais

:- op( 900, xfy, '::' ).
:- op( 300, xfy, ou ).
:- op( 300, xfy, e ).
:- dynamic(utente/5).
:- dynamic(ato/6).
:- dynamic(prestador/7).
:- dynamic(excecao/1).
:- dynamic('-'/1).
:- dynamic('::'/2).

%---------------------------------------------------------------------------------
% CONHECIMENTO PERFEITO POSITIVO E NEGATIVO

% Predicado UTENTE

%Conhecimento perfeito positivo

% Extensão do predicado utente: ID Utente, Nome, Idade, Cidade, Sexo -> {V,F,D}
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

% Formalização do PMF
-utente(IU,N,I,C,S) :-
		nao(utente(IU,N,I,C,S)),
		nao(excecao(utente(IU,N,I,C,S))).
-utente(1015,josemaria,45,lisboa,masculino).

% Conhecimento nulo incerto
utente(1012, josereis, idade_desconhecido, famalicao, masculino).

excecao(utente(IU,N,I,C,S)):-
		utente(IU,N,idade_desconhecido,C,S).

% Conhecimento nulo impreciso
excecao(utente(1013, clarafernandes, 52, viladoconde, feminino)).
excecao(utente(1013, marafernandes, 52, viladoconde, feminino)).

% Conhecimento nulo interdito
utente(1014, claudiareis, 17, cidadeinterdito, feminino).

interdito(cidadeinterdito).

excecao(utente(IU,N,I,C,S)):-
		utente(IU,N,I,cidadeinterdito,S).
		
+utente(IU,N,I,C,S)::(solucoes(C,(utente(1014, claudiareis, 17, C, feminino),
			nao(interdito(C))),L), 
			comprimento(L,R), R==0).

%---------------------------------------------------------------------------------
% Predicado PRESTADOR

% Conhecimento perfeito positivo

% Extensão do predicado prestador: Id Prestador, Tipo, Especialidade, Nome, Instituição, Cidade e Sexo -> {V,F}
prestador(2001,medico,cardiologia,ruialves,cuf,braga,masculino). 
prestador(2002,enfermeiro,pediatria,anasilva,trofasaude,porto,feminino).
prestador(2003,medico,medicinainterna,teresaalves,publico,vianadocastelo,feminino). 
prestador(2004,enfermeiro,oftalmologia,goretiparente,publico,vianadocastelo, feminino).
prestador(2005,medico,pneumologia,luismartins,cuf,braga,masculino). 
prestador(2006,medico,dentaria,jorgeamorim,trofasaude,porto, masculino).
prestador(2007,medico,dermatologia,diogopereira,trofasaude,braga,masculino). 
prestador(2008,medico,pediatria,rosasilva,publico,braga, feminino).

% Formalização do PMF
-prestador(IP,T,E,N,I,C,S) :-
		nao(prestador(IP,T,E,N,I,C,S)),
		nao(excecao(prestador(IP,T,E,N,I,C,S))).
-prestador(2012,medico,pneumologia,saracosta,publico,porto, feminino).

% Conhecimento nulo incerto
prestador(2009, enfermeiro, especialidade_desconhecido, carlossa,publico, vianadocastelo, masculino).

excecao(prestador(IP,T,E,N,I,C,S)):-
		prestador(IP,T,especialidade_desconhecido,N,I,C,S).

% Conhecimento nulo impreciso
excecao(prestador(2010,enfermeiro,psiquiatria,lauramiquelina,publico,braga, feminino)).
excecao(prestador(2010,enfermeiro,psicologia,lauramiquelina,publico,braga, feminino)).

% Conhecimento nulo interdito
prestador(2011, medico, dentaria, ivonerosas, cuf, cidadeinterdito, feminino).
interdito(cidadeinterdito).

excecao(prestador(IP,T,E,N,I,C,S)):-
		prestador(IP,T,E,N,I,cidadeinterdito,S).
		
+prestador(IP,T,E,N,I,C,S)::(solucoes(C,(prestador(2011, medico, dentaria, ivonerosas, cuf, C, feminino),
			nao(interdito(C))),L), 
			comprimento(L,R), R==0).

%---------------------------------------------------------------------------------
% Predicado ATO

% Conhecimento perfeito positivo

% Extensão do predicado ato: Id Ato, Data, ID Utente, ID Prestador, Descrição, Custo-> {V,F,D}
ato(3001, 051121, 1001, 2001, consulta,20). 
ato(3002, 080921, 1008, 2002, curativo,5).
ato(3003, 080921, 1006, 2008, consulta,15).
ato(3004, 070520, 1003, 2003, internamento,50).
ato(3005, 070520, 1007, 2004, exame,16).
ato(3006, 231120, 1002, 2005, consulta,21).
ato(3007, 010921, 1010, 2006, limpezadentaria,34).
ato(3008, 050720, 1004, 2004, consulta,8).

% Formalização do PMF
-ato(IA,D,IU,IP,D,C):-
		nao(ato(IA,D,IU,IP,D,C)),
		nao(excecao(ato(IA,D,IU,IP,D,C))).
-ato(3011, 210820, 1002, 2001, consulta,17).

% Conhecimento nulo incerto
ato(3009, 180621, 1005, 2001, descricao_desconhecido,13).

excecao(ato(IA,D,IU,IP,D,C)):-
		ato(IA,D,IU,IP,descricao_desconhecido,C).

% Conhecimento nulo impreciso
excecao(ato(3010, 190221, 1011, 2007, consulta,CD)):-
		CD >= 12,
		CD =< 22.

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
remocao( Termo ) :- assert(Termo),!, fail.

% Extensão do meta-predicado nao: Questao -> {V,F}
nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

%-------------------------------------------------------------------------------------------
% INVARIANTES

% UTENTE

% Invariante de Inserção Estrutural: não permitir a inserção de um utente já existente.
+utente(A,B,C,D,E) :: (solucoes((A,B,C,D,E), utente(A,B,C,D,E), S),
					    comprimento( S,N ), N == 1).

% Invariante de Inserção Referencial: não permitir a inserção de um utente com o mesmo ID.
+utente(A,_,_,_,_) :: (solucoes(A, utente(A,_,_,_,_), S),
					    comprimento( S,N ), N == 1).

% Invariante de Remoção: permite remover um utente se este não estiver associado a nenhum ato.
-utente(A,B,C,D,E) :: (solucoes(A, ato(_,_,A,_,_,_ ), S),
					comprimento( S,N ), N == 0).

%----------------------------------------------------------------------------------------------------------
%PRESTADOR

% Invariante de Inserção Estrutural: não permitir a inserção de um prestador já existente.
+prestador(A,B,C,D,E,F,G) :: (solucoes((A,B,C,D,E,F,G), prestador( A,B,C,D,E,F,G), S),
					    comprimento( S,N ), N == 1).

% Invariante de Inserção Referencial: não permitir a inserção de um prestador com o mesmo ID.
+prestador(A,_,_,_,_,_,_) :: (solucoes(A, prestador( A,_,_,_,_,_,_), S),
					    comprimento( S,N ), N == 1).

% Invariante de Remoção: permite remover um prestador se este não estiver associado a nenhum ato.
-prestador(A,B,C,D,E,F,G) :: (solucoes(A, ato( _,_,_,A,_,_ ), S),
					comprimento( S,N ), N == 0).

%----------------------------------------------------------------------------------------------------------
%ATO
% Invariante de Inserção Estrutural: não permitir a inserção de um ato já existente.
+ato(A,B,C,D,E,F) :: (solucoes(A, ato(A,B,C,D,E,F), S),
						comprimento( S,N ), N == 1).

% Invariante de Inserção Referencial: não permitir a inserção de um ato com um ID já existente.
+ato(A,_,_,_,_,_) :: (solucoes(A, ato(A,_,_,_,_,_), S),
						comprimento( S,N ), N == 1).

% Invariante de Inserção Referencial: não permitir a inserção de um ato para um utente que não existe na base de conhecimento.                    
+ato(_,_,A,_,_,_) :: (solucoes(A, utente(A,_,_,_,_))).

% Invariante de Inserção Referencial: não permitir a inserção de um ato realizado por um prestador que não existe na base de conhecimento.                    
+ato(_,_,_,A,_,_) :: (solucoes(A, prestador(A,_,_,_,_,_,_))).

%----------------------------------------------------------------------------------------------------------------------------------------------
% SISTEMA DE INFERÊNCIA

% Extensão do meta-predicado si: Questao, Resposta -> {V,D,F}
si( Questao,verdadeiro ) :-
    Questao.
si( Questao,falso ) :-
    -Questao.
si( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).

% Extensão do predicado conjuncao: Valor da Questão 1, Valor da Questão 2, Conjunção -> {V,D,F}
conjuncao(verdadeiro,verdadeiro,verdadeiro).
conjuncao(_,falso, falso).
conjuncao(falso, _, falso).
conjuncao(verdadeiro,desconhecido,desconhecido).
conjuncao(desconhecido,verdadeiro,desconhecido).
conjuncao(desconhecido,desconhecido,desconhecido).

% Extensão do predicado disjuncao: Valor da Questão 1, Valor da Questão 2, Disjunção -> {V,D,F}
disjuncao(verdadeiro,_,verdadeiro).
disjuncao(_,verdadeiro,verdadeiro).
disjuncao(verdadeiro,desconhecido,verdadeiro).
disjuncao(desconhecido,desconhecido,desconhecido).
disjuncao(falso,falso,falso).
disjuncao(desconhecido,falso,desconhecido).
disjuncao(falso,desconhecido,desconhecido). 

%Extensão do meta-predicado siC: Composição de Questões, Resposta -> {V,D,F}
siC(Q1 e Q2,R) :- siC(Q1,R1), siC(Q2,R2), conjuncao(R1,R2,R).

siC(Q1 ou Q2,R) :- siC(Q1,R1), siC(Q2,R2), disjuncao(R1,R2,R).

siC(Q,R) :- si(Q,R).

%-----------------------------------------------------------------------------------------------------------------------------------------------
% EVOLUÇÃO E INVOLUÇÃO DO CONHECIMENTO

%INSERIR CONHECIMENTO

% Extensão do predicado que permite a inserção de conhecimento
% Termo -> {V,F} 
registar( Termo ) :- 
	solucoes(I,+Termo::I,Linv ),
	insercao( Termo ),
	teste( Linv ).

%------------------------------------------------------------------------------------------------------------------------------------------
% REMOVER CONHECIMENTO
% Extensão do predicado que permite a remoção de conhecimento
%Termo -> {V,F} 
remover( Termo ) :- 
	solucoes( I, -Termo::I, Linv ), 
	remocao( Termo ),
	teste( Linv ).

% --------------------------------------------------------------------------------------
% ATUALIZAR CONHECIMENTO

% Extensão do predicado que permite a atualizar um utente: Termo -> {V,F}
%% Alternativa1 de atualizar
atualizarUtente(utente(IU,N,I,C,S),
    utente(IU,N,In,C,S)) :-
    remover(utente(IU,N,I,C,S)),
    registar(utente(IU,N,In,C,S)).

% Extensão do predicado que permite atualizar o custo do ato: Termo -> {V,F}
atualizarCustoAto(ato(IA,D,IU,IP,D,C), 
    ato(IA,D,IU,IP,D,Cn)):-
    remover(ato(IA,D,IU,IP,D,C)),
    registar(ato(IA,D,IU,IP,D,Cn)).

% Extensão do predicado que permite a atualizar um prestador: Termo -> {V,F}
atualizarPrestador(prestador(IP,TP,E,N,I,C,S), 
    prestador(IP,TP,En,N,In,Cn,S)):-
    remover(prestador(IP,TP,E,N,I,C,S)),
    registar(prestador(IP,TP,En,N,In,Cn,S)).

%---------------------------------------------------------------------------------
% MANIPULAR CONHECIMENTO
% Extensão do predicado que permite a evolucao do conhecimento positivo para positivo: Cnovo -> {V,F}
evolucao_PP(Cnovo,Cpassado):-
    si(Cpassado, verdadeiro),
    remocao(Cpassado),
    insercao(Cnovo).

% Extensão do predicado que permite a evolucao do conhecimento negativo para positivo: Cnovo -> {V,F}
evolucao_NP(Cnovo):-
    si(-Cnovo, verdadeiro),
    remocao(-Cnovo),
    insercao(Cnovo).

% Extensão do predicado que permite a evolucao do conhecimento positivo para negativo: Cnovo -> {V,F}
evolucao_PN(Cnovo):-
    si(Cnovo, verdadeiro),
    registar(-Cnovo),
    remover(Cnovo).
    
% Extensão do predicado verificaImp: Lista -> {V,F}
verificaImp([]).
verificaImp( [H|T] ) :- 
    si(H,desconhecido),
    verificaImp(T).
        
% Extensão do predicado removeExcecoes: Lista -> {V,F}    
removeExcecoes([]).
removeExcecoes([H|T]):-
    remover(excecao(H)),
    removeExcecoes(T).
        
% Extensao do predicado que permite a evolucao do conhecimento impreciso
% para positivo: Cnovo, ListaCpassado -> {V,F}
evolucao_ImpP(Cnovo,[Cpassado1|T]):-
    T\=[],
    verificaImp([Cpassado1|T]),
    removeExcecoes([Cpassado1|T]),
    registar(Cnovo).
    
% Extensão do predicado que permite a evolucao do conhecimento incerto 
% para positivo: Cnovo, Cpassado -> {V,F}  
evolucao_IncP(Cnovo, Cpassado):-
    si(Cpassado,verdadeiro),
    remover(Cpassado),
    registar(Cnovo).

% Extensão do predicado que permite a evolucao do conhecimento incerto 
% para impreciso : Cnovo1, Cnovo2, Cpassado -> {V,F}
evolucao_IncImp(Cnovo1,Cnovo2,Cpassado):-
	si(Cpassado,verdadeiro),
	remover(Cpassado),
	registar(excecao(Cnovo1)),
	registar(excecao(Cnovo2)).

% Extensão do predicado que permite a evolucao do conhecimento impreciso 
% para incerto: Cnovo, Cnovo_x, Cpassado1, Cpassado2 -> {V,F}.
evolucao_ImpInc(Cnovo,Cnovo_x,Cpassado1,Cpassado2):-
	si(Cpassado1,desconhecido),
	si(Cpassado2,desconhecido),
	remover(excecao(Cpassado1)),
	remover(excecao(Cpassado2)),
	registar(Cnovo),
	registar(excecao(Cnovo_x)).
        
