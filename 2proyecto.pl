:-use_rendering(table).
:-dynamic celda/3.

 /*desplazar(Dir, Num, Cant, Tablero, EvolTablero):-
    guardarTablero(Tablero),
    mover(Dir,Num-1,Cant),
    buscarColapsoDeFilas(L),    
    */

guardarTablero([Fila1,Fila2,Fila3,Fila4,Fila5]):-
    guardarFila(0,Fila1),
    guardarFila(1,Fila2),
    guardarFila(2,Fila3),
    guardarFila(3,Fila4),
    guardarFila(4,Fila5).


guardarFila(X, [E1,E2,E3,E4,E5]):- assert(celda(X,0,E1)),
                                   assert(celda(X,1,E2)),
                                   assert(celda(X,2,E3)),
                                   assert(celda(X,3,E4)),
                                   assert(celda(X,4,E5)).
              


mover(izq, NumDeFila, Cant):- C is 5-Cant, moverFila(NumDeFila,C).
mover(der, NumDeFila, Cant):- moverFila(NumDeFila,Cant).   
mover(arriba, NumDeColumna, Cant):- C is 5-Cant, moverColumna(NumDeColumna,C).
mover(abajo, NumDeColumna, Cant):- moverColumna(NumDeColumna,Cant).


moverColumna(NumDeColumna,Cant):- forall(celda(F,NumDeColumna,X),
                                         (NuevaF is ((F+Cant) mod 5),
                                          assert(celda(NuevaF,NumDeColumna, X)),
                                          retract(celda(F,NumDeColumna,X)))).

moverFila(NumDeFila,Cant):- forall(celda(NumDeFila,C,X),
                                   (NuevaC is ((C+Cant) mod 5),
                                    assert(celda(NumDeFila,NuevaC, X)),
                                    retract(celda(NumDeFila,C,X)))).



/* TRANSFORMAR HECHOS A TABLERO */
mostrarTablero(T):- hechosATablero(T).

hechosATablero(T):- filaALista(0,L1),
                    filaALista(1,L2),
                    filaALista(2,L3),
                    filaALista(3,L4),
                    filaALista(4,L5),
                    T = [L1,L2,L3,L4,L5].
    
filaALista(Fila,Lista):- celda(Fila,0,M1),
                         celda(Fila,1,M2),
                         celda(Fila,2,M3),
                         celda(Fila,3,M4),
                         celda(Fila,4,M5),
                         Lista = [M1,M2,M3,M4,M5].

buscarColapsoFila0():- buscarColapsoFila(0,5);
    				   buscarColapsoFila(0,4);
    				   buscarColapsoFila(0,3).

buscarColapsoFila(0,5):-
    celda(0,0,X),
    celda(0,1,X),
    celda(0,2,X),
    celda(0,3,X),
    celda(0,4,X),
    assert(celdaMarcada(0,0)),
    assert(celdaMarcada(0,1)),
    assert(celdaMarcada(0,2)),
    assert(celdaMarcada(0,3)),
    assert(celdaMarcada(0,4)),
    assert(celdaMarcada(0,5)).

buscarColapsoFila(0,4):-
    celda(0,0,X),
    celda(0,1,X),
    celda(0,2,X),
    celda(0,3,X),
    assert(celdaMarcada(0,0)),
    assert(celdaMarcada(0,1)),
    assert(celdaMarcada(0,2)),
    assert(celdaMarcada(0,3)),
    assert(celdaMarcada(0,4));
    celda(0,1,X),
    celda(0,2,X),
    celda(0,3,X),
    celda(0,4,X),
    assert(celdaMarcada(0,1)),
    assert(celdaMarcada(0,2)),
    assert(celdaMarcada(0,3)),
    assert(celdaMarcada(0,4)).

buscarColapsoFila(0,3):-
    celda(0,0,X),
    celda(0,1,X),
    celda(0,2,X),
    assert(celdaMarcada(0,0)),
    assert(celdaMarcada(0,1)),
    assert(celdaMarcada(0,2));   
    celda(0,1,X),
    celda(0,2,X),
    celda(0,3,X),
    assert(celdaMarcada(0,1)),
    assert(celdaMarcada(0,2)),
    assert(celdaMarcada(0,3));
    celda(0,2,X),
    celda(0,3,X),
    celda(0,4,X),
    assert(celdaMarcada(0,2)),
    assert(celdaMarcada(0,3)),
    assert(celdaMarcada(0,4)).   






/*
CONSULTA
guardarTablero([[r1, v2, a1, r1, a1],[a2, v1, v1, r2, r2],[a1, r2, a3, v1, a2],[r3, r2, r1, a3, v1],[v3, a1, v2,
r2, v1]]), mostrarTablero(Tablero).
*/


/*
OTRA CONSULTA MAS FACIL:
guardarTablero([[1, 2, 3, 4, 5],[6, 7, 8, 9, 10],[11, 12, 13, 14, 15],[16, 17, 18, 19, 20],[21, 22, 23,
24, 25]]),
mostrarTablero(Tablero1),
mover(der,0,1),
mostrarTablero(Tablero2).
*/
