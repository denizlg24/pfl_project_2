teaches(algorithms,adalberto).
teaches(databases,bernardete).
teaches(compilers,capitolino).
teaches(statistics,diogenes).
teaches(networks,ermelinda).

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

professor(X,Y):- teaches(_Course,X), attends(_Course,Y).
student(X,Y):- professor(Y,X).
both_teach(Student,X,Y):- professor(Student,X), professor(Student,Y), X \= Y.
collegues(X,Y):- teaches(_,X), teaches(_,Y), X @< Y.
collegues(X,Y):- attends(Z,X), attends(Z,Y), X @< Y.

more_than_one_class(Student):- attends(_X,Student),attends(_Y,Student), _X @< _Y.