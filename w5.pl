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


%class(Course, ClassType, DayOfWeek, Time, Duration)
class(pfl, t, '2 Tue', 15, 2).
class(pfl, tp, '2 Tue', 10.5, 2).
class(lbaw, t, '3 Wed', 10.5, 2).
class(lbaw, tp, '3 Wed', 8.5, 2).
class(ipc, t, '4 Thu', 14.5, 1.5).
class(ipc, tp, '4 Thu', 16, 1.5).
class(fsi, t, '1 Mon', 10.5, 2).
class(fsi, tp, '5 Fri', 8.5, 2).
class(rc, t, '5 Fri', 10.5, 2).
class(rc, tp, '1 Mon', 8.5, 2).

same_day(Course1, Course2):- class(Course1,_,Date,_,_), class(Course2,_,Date,_,_), Course1 \= Course2,!.

daily_courses(Day, Courses):-
	findall(Course,class(Course,_Type,Day,_,_),Courses).

short_classes(L):-
	findall(Course-Day/Time,(class(Course,_,Day,_,Time),Time < 2),L).

course_classes(Course, Classes):-
	setof(Day/Time-Type,class(Course,Type,Day,Time,_),Classes).

courses(L):-
	setof(Course,_A^_B^_C^_D^class(Course,_A,_B,_C,_D),L).

schedule:-
	findall(class(Day, Time, Course, Type, Duration), 
            class(Course, Type, Day, Time, Duration), 
            Classes),
	sort(Classes, SortedClasses),
	print_classes(SortedClasses).

print_classes([]).
print_classes([class(Day, Time, Course, Type, Duration) | T]) :-
	day_translation(Day, DayName),
    format('~w ~w: ~w (~w) - ~w hours~n', [DayName, Time, Course, Type, Duration]),
    print_classes(T).

day_translation('1 Mon', 'Mon').
day_translation('2 Tue', 'Tue').
day_translation('3 Wed', 'Wed').
day_translation('4 Thu', 'Thu').
day_translation('5 Fri', 'Fri').

find_class :-
    write('Day: '), 
    read(DayName),
    write('Time: '), 
    read(Time),
    
    (   
        day_translation(Day, DayName),
        class(Course, Type, Day, Start, Duration),
        End is Start + Duration, 
        Time >= Start,             
        Time =< End            
    ->  
        format('~w ~w: ~w (~w) - ~w hours~n', [DayName, Time, Course, Type, Duration])
    ;   
		day_translation(Day, DayName),
        format('No class is taking place on ~w at ~w.~n', [DayName, Time])
    ).
	

%flight(origin, destination, company, code, hour, duration)
flight(porto, lisbon, tap, tp1949, 1615, 60).
flight(lisbon, madrid, tap, tp1018, 1805, 75).
flight(lisbon, paris, tap, tp440, 1810, 150).
flight(lisbon, london, tap, tp1366, 1955, 165).
flight(london, lisbon, tap, tp1361, 1630, 160).
flight(porto, madrid, iberia, ib3095, 1640, 80).
flight(madrid, porto, iberia, ib3094, 1545, 80).
flight(madrid, lisbon, iberia, ib3106, 1945, 80).
flight(madrid, paris, iberia, ib3444, 1640, 125).
flight(madrid, london, iberia, ib3166, 1550, 145).
flight(london, madrid, iberia, ib3163, 1030, 140).
flight(porto, frankfurt, lufthansa, lh1177, 1230, 165).


get_all_nodes(ListOfAirports):-
	setof(Airport,_A^_B^_C^_D^_E^flight(_A,Airport,_B,_C,_D,_E),ListOfAirports).

get_destinations([],_,[]).
get_destinations([C-Dest|_T],Company,Destinations):-
	(C == Company 
		->
		get_destinations(_T,Company,Result), Destinations = [Dest|Result];
		get_destinations(_T,Company,Destinations)
	).

most_diversified(Company):-
	setof(C-Destination,_A^_B^_C^_D^flight(_A,Destination,C,_B,_C,_D),Destinations),
	findall(DestsLength-C,(setof(Comp,_A^_B^_C^_D^_E^flight(_A,_B,Comp,_C,_D,_E),Companies), member(C,Companies),get_destinations(Destinations,C,Dests), length(Dests,DestsLength)),Counts),
	sort(Counts,Sorted),
	last(Sorted,_-Company).

find_flights(Origin, Destination, [Flight]):-
	flight(Origin,Destination,_,Flight,_,_).

find_flights(Origin, Destination, Path) :-
    find_flights(Origin, Destination, [Origin], Path).

find_flights(Destination, Destination, _Visited, []).

find_flights(Origin, Destination, Visited, [Flight|Result]):-
	flight(Origin,Dest,_,Flight,_,_),
	\+ (memberchk(Dest,Visited)),
	find_flights(Dest,Destination,[Dest|Visited],Result).

find_flights_bfs(Origin, Destination, Flights) :-
    bfs([[Origin, []]], Destination, [Origin], RevFlights),
    reverse(RevFlights, Flights).


bfs([[Destination, Flights] | _], Destination, _Visited, Flights).

bfs([[Current, FlightsSoFar] | Rest], Destination, Visited, Result) :-
    findall(
        [Next, [Flight | FlightsSoFar]],
        (
            flight(Current, Next, _, Flight, _, _),
            \+ memberchk(Next, Visited)
        ),
        NewEntries
    ),
    extract_nodes(NewEntries, NewVisited),
    append(Rest, NewEntries, Queue),
    append(Visited, NewVisited, UpdatedVisited),
    bfs(Queue, Destination, UpdatedVisited, Result).

extract_nodes([], []).
extract_nodes([[Node, _] | T], [Node | R]) :-
    extract_nodes(T, R).


find_all_flights(Origin, Destination, ListOfFlights) :-
    findall(
        Flights,
       	find_flights(Origin, Destination, [Origin], Flights),
        ListOfFlights
    ).

find_flights_least_stops(Origin, Destination, ListOfFlights):-
	setof(Size-Flights,(find_flights(Origin, Destination, [Origin], Flights), length(Flights,Size)),Grouped),
	sort(Grouped,[_-ListOfFlights|_]).

find_flights_stops(Origin, Destination, Stops, ListFlights) :-
    findall(
        Flights,
        (
            find_flights(Origin, Destination, Flights),
            path_cities(Origin, Flights, Cities),
            subset(Stops, Cities)
        ),
        ListFlights
    ).


path_cities(City, [], [City]).
path_cities(City, [Flight | Rest], [City | Cities]) :-
    flight(City, Next, _, Flight, _, _),
    path_cities(Next, Rest, Cities).

subset([], _).
subset([H | T], List) :-
    memberchk(H, List),
    subset(T, List).


find_circular_trip(MaxSize, Origin, Cycle) :-
    dfs_cycle(Origin, Origin, [Origin], Cycle),
    length(Cycle, Len),
    Len =< MaxSize.

dfs_cycle(Current, Origin, Visited, []) :-
    Current == Origin,
    Visited \= [Origin].

dfs_cycle(Current, Origin, Visited, [Flight | Rest]) :-
    flight(Current, Next, _, Flight, _, _),
    (
        Next == Origin
    ;
        \+ memberchk(Next, Visited)
    ),
    dfs_cycle(Next, Origin, [Next | Visited], Rest).


find_circular_trips(MaxSize, Origin, Cycles):-
	findall(Cycle,find_circular_trip(MaxSize,Origin,Cycle),Cycles).

strongly_connected(Nodes):-
	 \+ (
        member(Src, Nodes),
        member(Dest, Nodes),
        Src \= Dest,
        \+ find_flights(Src, Dest, _)
    ).

strongly_connected_components(Components) :-
    get_all_nodes(Cities),
    sccs(Cities, Components).

sccs([], []).
sccs([C | Rest], [Component | Components]) :-
    build_scc(C, [C | Rest], Component),
    remove_all([C | Rest], Component, Remaining),
    sccs(Remaining, Components).

build_scc(City, Cities, Component) :-
    findall(
        Other,
        (
            member(Other, Cities),
            reachable(City, Other),
            reachable(Other, City)
        ),
        RawComponent
    ),
	sort(RawComponent,Component).

reachable(A, B) :-
    A == B.
reachable(A, B) :-
    A \= B,
    find_flights(A, B, Path),
    Path \= [].

remove_all([], _, []).
remove_all([H | T], L, R) :-
    memberchk(H, L),
    remove_all(T, L, R).
remove_all([H | T], L, [H | R]) :-
    \+ memberchk(H, L),
    remove_all(T, L, R).


bridges(ListOfBridges) :-
    findall(
        FlightCode,
        is_bridge(FlightCode),
        ListOfBridges
    ).

is_bridge(FlightCode) :-
    flight(A, B, Airline, FlightCode, Time, Dur),
    \+ reachable_without_flight(B, A, FlightCode, Airline, Time, Dur).

reachable_without_flight(A, B, FC, Al, T, D) :-
    dfs_excluding(A, B, [A], FC, Al, T, D).

dfs_excluding(B, B, _Visited, _, _, _, _).
dfs_excluding(Current, Dest, Visited, FC, Al, T, D) :-
    flight(Current, Next, Airline, Flight, Time, Dur),
    \+ (Flight = FC, Airline = Al, Time = T, Dur = D),
    \+ memberchk(Next, Visited),
    dfs_excluding(Next, Dest, [Next | Visited], FC, Al, T, D).
