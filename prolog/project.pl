comparator(I,J):-
	integer(I),
	integer(J).
	
/*opg.1*/
is_network([]).
is_network([comparator(_,_)|T]):-
	is_network(T).
	
/*opg.2*/
channels([],_).
channels([comparator(I,J)|T],N):-
	is_network([comparator(I,J)|T]),
	between(1,N,I),
	between(1,N,J),
	channels(T,N).

/*opg.3*/
run([],X,X):-!.
run(_,[],[]):-!.
run([comparator(I,J)|T],L,Ol):-
	nth1(I,L,A),
	nth1(J,L,B),
	sorting(A,B,C,D),
	replace(I,C,L,L2),
	replace(J,D,L2,K),
	run(T,K,Ol).
	
sorting(A,B,B,A):-
	A > B,!.
sorting(A,B,A,B):-
	A =< B,!.

replace(1,X,[_|T],[X|T]):-!.
replace(I,X,[H|T],[H|R]):- 
	I > 0,
	NI is I-1,
	replace(NI,X,T,R).
	
/*opg.4*/
is_SN(S,N):- 
	is_network(S),
	channels(S,N),
	findall(X,net(N,X),L),
	sn_sort(S,L).

net(0,[]):-!.
net(N,[X|T]):-
	N > 0,
	N1 is N-1,
	between(0,1,X),
	net(N1,T).

sn_sort(_,[]):-!.
sn_sort(S,[X|T]):- 
	run(S,X,L),
	msort(X,C),
	is_equal(L,C),
	sn_sort(S,T).

is_equal([],[]):-!.
is_equal([X|T],[X|S]):-
	is_equal(T,S).

/*opg.5*/
find_SN(_,0,[]):-!.
find_SN(N,K,S):-
	0 < K,
	channels(S,N),
	length(S,K),
	is_SN(S,N).

/*opg.6*/
is_standard([]):-!.
is_standard([comparator(I,J)|T]):- I < J,
	is_standard(T).

/*opg.7*/
standardize([],[]):-!.
standardize([comparator(I,J)|T],[comparator(C,D)|S]):-
	msort([I,J],L),
	nth1(1,L,C),
	nth1(2,L,D),
	standardize(T,S).
	
/*opg.8*/
equivalent(_,[],[]):-!.
equivalent(N,C1,C2):-
	is_network(C1),
	length(C1,K),
	length(C2,K),
	standardize(C1,C3),
	standardize(C2,C4),
	check1(N,C3,C4).
	
check1(_,[],_):-!.	
check1(_,[],[]):-!.
check1(N,[comparator(I,J)|T],C2):-
	check2(comparator(I,J),C2),
	check1(N,T,C2).

check2(comparator(I,J),[comparator(X,Y)|_]):-
	X is I,
	Y is J,!.

check2(comparator(I,J),[comparator(X,Y)|T]):-
	dif(comparator(I,J),comparator(X,Y)),
	check2(comparator(I,J),T).

/*opg.9*/
layered_network([]):-!.
layered_network([X|T]):-
	is_network(X),
	layered_network(T).

/*opg.10*/
layered([],_):-!.
layered([comparator(I,J)|T],L):-
	checklist(comparator(I,J),L),
	layered(T,L).
	
checklist(comparator(I,J),[X|_]):-
	checkcomp(comparator(I,J),X),!.

checklist(comparator(I,J),[_|L]):-
	checklist(comparator(I,J),L).

checkcomp(comparator(I,J),[comparator(X,Y)|T]):-
	dif(I,X),
	dif(J,Y),
	checkcomp(comparator(I,J),T).
	
checkcomp(comparator(I,J),[comparator(X,Y)|_]):-
	I is X,
	J is Y,!.

/*opg.11*/
network_to_layered([],[]):-!.
network_to_layered(C,[H|T]):-
	layer(C,A,[],H),
	network_to_layered(A,T).

layer([],[],_,[]):-!.
layer([comparator(I,J)|T],A,Checklist,[comparator(I,J)|Lt]) :-
	checklayer(comparator(I,J),Checklist),
	layer(T,A,[comparator(I,J)|Checklist],Lt). 

layer([comparator(I,J)|T],[comparator(I,J)|Rt],Checklist,L):- 
	not(checklayer(comparator(I,J),Checklist)),
	layer(T,Rt,Checklist,L).
	
checklayer(_,[]):-!.
checklayer(comparator(I,J),[comparator(X,Y)|T]):-
	dif(I,X),
	dif(J,Y),
	dif(I,Y),
	dif(J,X),
	checklayer(comparator(I,J),T).

/*opg.12										done*/
layered_to_network([],[]):-!.
layered_to_network([X|T],C1):-
	append(X,C,C1),
	layered_to_network(T,C).

/*opg.13*/

	
