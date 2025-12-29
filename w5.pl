female(grace).
male(frank).
fermale(dede).
male(jay).
female(gloria).
male(javier).
female(barb).
male(merle).
male(phill).
parent(grace,phill).
parent(frank, phill).
female(claire).
parent(dede,claire).
parent(jay,claire).
male(mitchel).
parent(dede,mitchel).
parent(jay,mitchel).
male(joe).
parent(jay,joe).
parent(gloria,joe).
male(many).
parent(gloria,many).
parent(javier,many).
male(cameron).
parent(barb,cameron).
parent(merle,cameron).
female(pameron).
parent(barb,pameron).
parent(merle,pameron).
male(bo).
male(dylan).
female(haley).
parent(phill,haley).
parent(claire,haley).
female(alex).
parent(phill,alex).
parent(claire,alex).
male(luke).
parent(phill,luke).
parent(claire,luke).
female(lily).
parent(mitchel,lily).
parent(cameron,lily).
male(rexford).
parent(mitchel,rexford).
parent(cameron,rexford).
male(calhoun).
parent(pameron,calhoun).
parent(bo,calhoun).
male(george).
parent(dylan,george).
parent(haley,george).
female(poppy).
parent(dylan,poppy).
parent(haley,poppy).

children(Person, Children):- findall(Child,parent(Person,Child),Children).

children_of([],[]).
children_of([H|_T], [H-Children|Result]):-
	children(H,Children),
	children_of(_T,Result).

couple(X-Y) :-
    parent(X, Child),
    parent(Y, Child),
    X \= Y.

couples(List):- setof(X-Y,couple(X-Y),List).

spouse_children(Person, Spouse-Children):- 
	setof(Child, (parent(Person,Child), parent(Spouse,Child), Person \= Spouse), Children).

immediate_family(Person,Parents-Pairs):-
	findall(Parent,parent(Parent,Person),Parents),
	findall(Spouse/Children,spouse_children(Person,Spouse-Children),Pairs).

parents_of_two(Parents):-
	setof(Parent,(parent(Parent, _), children(Parent,Childs), length(Childs,X), X >= 2),Parents).

teaches(algorithms,adalberto).
teaches(databases,bernardete).
teaches(compilers,capitolino).
teaches(statistics,diogenes).
teaches(networks,ermelinda).
teaches(networks2,ermelinda).


attends(algorithms,alberto).
attends(algorithms,bruna).
attends(algorithms,cristina).
attends(algorithms,diogo).
attends(algorithms,eduarda).

attends(databases,antonio).
attends(databases,bruno).
attends(databases,cristina).
attends(databases,duarte).
attends(databases,eduardo).

attends(statistics,antonio).
attends(statistics,bruna).
attends(statistics,claudio).
attends(statistics,duarte).
attends(statistics,eva).

attends(networks,alvaro).
attends(networks,beatriz).
attends(networks,claudio).
attends(networks,diana).
attends(networks,eduarda).

teachers(T):-
	setof(Teacher,Course^teaches(Course,Teacher),T).

students_of(T,Students):-
	setof(Student,Course^(teaches(Course,T), attends(Course,Student)),Students).

teachers_of(S,Teachers):-
	setof(Teacher,Course^(teaches(Course,Teacher), attends(Course,S)),Teachers).

common_courses(S1,S2,C):-
	setof(Course,(attends(Course,S1),attends(Course,S2)),C).

more_than_one_course(L):-
	setof(Student,_A^_B^(attends(_A,Student),attends(_B,Student), _A \= _B),L).

strangers(L):-
	setof(S1-S2, 
          C^_C1^_C2^( attends(_C1, S1),    
            attends(_C2, S2),      
            S1 @< S2,              
            \+ (attends(C, S1), attends(C, S2))
          ), 
          L).

good_groups(L):-
	setof(S1-S2,Courses^(common_courses(S1,S2,Courses),length(Courses,X), X>=2,S1 @< S2),L).

% good_groups(L) :-
%     setof(S1-S2,
%           ( attends(C1, S1),      % S1 is in Course 1
%             attends(C1, S2),      % S2 is in Course 1
%             attends(C2, S1),      % S1 is in Course 2
%             attends(C2, S2),      % S2 is in Course 2
%             C1 @< C2,             % Ensure C1 and C2 are different (and strictly ordered)
%             S1 @< S2              % Ensure S1 != S2 and avoid duplicate pairs (A-B vs B-A)
%           ),
%           L).
