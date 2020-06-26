/*desplazar(Dir, Num, Cant, Tablero, EvolTablero):-
    guardarTablero(Tablero).*/
    
guardarTablero([Fila1,Fila2,Fila3,Fila4,Fila5]):-
    guardarFila1(Fila1),
    guardarFila2(Fila2),
    guardarFila3(Fila3),
    guardarFila4(Fila4),
    guardarFila5(Fila5).
    
guardarFila1([E1,E2,E3,E4,E5]):- celda(1,1,E1),celda(1,2,E2),celda(1,3,E3),celda(1,4,E4),celda(1,5,E5).
guardarFila2([E1,E2,E3,E4,E5]):- celda(2,1,E1),celda(2,2,E2),celda(2,3,E3),celda(2,4,E4),celda(2,5,E5).
guardarFila3([E1,E2,E3,E4,E5]):- celda(3,1,E1),celda(3,2,E2),celda(3,3,E3),celda(3,4,E4),celda(3,5,E5).
guardarFila4([E1,E2,E3,E4,E5]):- celda(4,1,E1),celda(4,2,E2),celda(4,3,E3),celda(4,4,E4),celda(4,5,E5).
guardarFila5([E1,E2,E3,E4,E5]):- celda(5,1,E1),celda(5,2,E2),celda(5,3,E3),celda(5,4,E4),celda(5,5,E5).

celda(_,_,_).

guardarTablero([[r1, v2, a1, r1, a1],[a2, v1, v1, r2, r2],[a1, r2, a3, v1, a2],[r3, r2, r1, a3, v1],[v3, a1, v2,r2, v1]]).
