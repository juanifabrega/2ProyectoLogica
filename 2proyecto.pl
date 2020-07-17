:-use_rendering(table).
:-dynamic celda/4.

/*_____________________________________________________
LEYENDA CELDA:
  celda(Fila,Columna,ValorMamushka,EstadoActual).
  
      0 <= Fila,Columna <= 4
      ValorMamushka: v1,v2,v3, r1,r2,r3, a1,a2,a3
      EstadoActual: sincambios, borrar, agrandar
_______________________________________________________*/


desplazar(Dir, Num, Cant, Tablero, EvolTablero):-

              guardarTablero(Tablero),
              mover(Dir,Num-1,Cant),
              mostrarTablero(Tablero1),
              % Agregrar Tablero1 a EvolTablero
              
              buscarTodosLosColapsos(L),
              marcar(L),
              aplicarEstados,
              mostrarTablero(Tablero2),
              % Agregrar Tablero2 a EvolTablero
             
              gravedad,
              mostrarTablero(Tablero3),
              % Agregrar Tablero3 a EvolTablero
             
              generarMamushkasRandom,
              mostrarTablero(Tablero4),
              % Agregrar Tablero4 a EvolTablero
         

guardarTablero([Fila1,Fila2,Fila3,Fila4,Fila5]):-
    guardarFila(0,Fila1),
    guardarFila(1,Fila2),
    guardarFila(2,Fila3),
    guardarFila(3,Fila4),
    guardarFila(4,Fila5).

guardarFila(X, [E1,E2,E3,E4,E5]):- assert(celda(X,0,E1,sincambios)),
                                   assert(celda(X,1,E2,sincambios)),
                                   assert(celda(X,2,E3,sincambios)),
                                   assert(celda(X,3,E4,sincambios)),
                                   assert(celda(X,4,E5,sincambios)).
              
mover(izq, NumDeFila, Cant):- C is 5-Cant, moverFila(NumDeFila,C).
mover(der, NumDeFila, Cant):- moverFila(NumDeFila,Cant).   
mover(arriba, NumDeColumna, Cant):- C is 5-Cant, moverColumna(NumDeColumna,C).
mover(abajo, NumDeColumna, Cant):- moverColumna(NumDeColumna,Cant).

moverColumna(NumDeColumna,Cant):- forall(celda(F,NumDeColumna,X,_),
                                         (NuevaF is ((F+Cant) mod 5),
                                          assert(celda(NuevaF,NumDeColumna, X,_)),
                                          retract(celda(F,NumDeColumna,X,_)))).

moverFila(NumDeFila,Cant):- forall(celda(NumDeFila,C,X,_),
                                   (NuevaC is ((C+Cant) mod 5),
                                    assert(celda(NumDeFila,NuevaC, X,_)),
                                    retract(celda(NumDeFila,C,X,_)))).
/*------------------------------------------------------------------------*/
/* DPS HAY Q BORRAR ESTOS PREDICADOS PQ NO NOS SIRVER*/
/* mostrar tablero */
mostrarTablero(T):- hechosATablero(T).

hechosATablero(T):- filaALista(0,L1),
                    filaALista(1,L2),
                    filaALista(2,L3),
                    filaALista(3,L4),
                    filaALista(4,L5),
                    T = [L1,L2,L3,L4,L5].
    
filaALista(Fila,Lista):- celda(Fila,0,M1,_),
                         celda(Fila,1,M2,_),
                         celda(Fila,2,M3,_),
                         celda(Fila,3,M4,_),
                         celda(Fila,4,M5,_),
                         Lista = [M1,M2,M3,M4,M5].
/*mostrar estados*/
mostrarEstados(T):- estadosATablero(T).
estadosATablero(T):-filaAListaEstados(0,L1),
                    filaAListaEstados(1,L2),
                    filaAListaEstados(2,L3),
                    filaAListaEstados(3,L4),
                    filaAListaEstados(4,L5),
                    T = [L1,L2,L3,L4,L5].
filaAListaEstados(Fila,Lista):-
                         celda(Fila,0,_,M1),
                         celda(Fila,1,_,M2),
                         celda(Fila,2,_,M3),
                         celda(Fila,3,_,M4),
                         celda(Fila,4,_,M5),
                         Lista = [M1,M2,M3,M4,M5].        
/*----------------------------------------------------------------------*/

insertar_ultimo(X,[],[X]).
insertar_ultimo(X,[Y|L2],[Y|L3]):- insertar_ultimo(X,L2,L3). 

buscarColapsosDeFilas(L,Lrta):-
    buscarColapsoFila(0,L,L1),
    buscarColapsoFila(1,L1,L2),
    buscarColapsoFila(2,L2,L3),
    buscarColapsoFila(3,L3,L4),
    buscarColapsoFila(4,L4,Lrta).

buscarColapsoFila(NumDeFila,Lista,ListaNueva):- 
    buscarColapsoFila(NumDeFila,5,Lista,ListaNueva);
    buscarColapsoFila(NumDeFila,4,Lista,ListaNueva);
    buscarColapsoFila(NumDeFila,3,Lista,ListaNueva);
    ListaNueva=Lista.

buscarColapsoFila(NumDeFila,5,L,Lrta):-
    celda(NumDeFila,0,X,_),
    celda(NumDeFila,1,X,_),
    celda(NumDeFila,2,X,_),
    celda(NumDeFila,3,X,_),
    celda(NumDeFila,4,X,_),
    Lista=[[NumDeFila,0],[NumDeFila,1],[NumDeFila,2],[NumDeFila,3],[NumDeFila,4]],
    insertar_ultimo(Lista,L,Lrta).

buscarColapsoFila(NumDeFila,4,L,Lrta):-
    celda(NumDeFila,0,X,_),
    celda(NumDeFila,1,X,_),
    celda(NumDeFila,2,X,_),
    celda(NumDeFila,3,X,_),
    Lista=[[NumDeFila,0],[NumDeFila,1],[NumDeFila,2],[NumDeFila,3]],
    insertar_ultimo(Lista,L,Lrta);
    celda(NumDeFila,1,X,_),
    celda(NumDeFila,2,X,_),
    celda(NumDeFila,3,X,_),
    celda(NumDeFila,4,X,_),
    Lista=[[NumDeFila,1],[NumDeFila,2],[NumDeFila,3],[NumDeFila,4]],
    insertar_ultimo(Lista,L,Lrta).
 
buscarColapsoFila(NumDeFila,3,L,Lrta):-
    celda(NumDeFila,0,X,_),
    celda(NumDeFila,1,X,_),
    celda(NumDeFila,2,X,_),
    Lista=[[NumDeFila,0],[NumDeFila,1],[NumDeFila,2]],
    insertar_ultimo(Lista,L,Lrta);   
    celda(NumDeFila,1,X,_),
    celda(NumDeFila,2,X,_),
    celda(NumDeFila,3,X,_),
    Lista=[[NumDeFila,1],[NumDeFila,2],[NumDeFila,3]],
    insertar_ultimo(Lista,L,Lrta);
    celda(NumDeFila,2,X,_),
    celda(NumDeFila,3,X,_),
    celda(NumDeFila,4,X,_),
    Lista=[[NumDeFila,2],[NumDeFila,3],[NumDeFila,4]],
    insertar_ultimo(Lista,L,Lrta).   

buscarColapsosDeColumnas(L,Lrta):-
    buscarColapsoColumna(0,L,L1),
    buscarColapsoColumna(1,L1,L2),
    buscarColapsoColumna(2,L2,L3),
    buscarColapsoColumna(3,L3,L4),
    buscarColapsoColumna(4,L4,Lrta).

buscarColapsoColumna(NumDeColumna,Lista,ListaNueva):- 
    buscarColapsoColumna(NumDeColumna,5,Lista,ListaNueva);
    buscarColapsoColumna(NumDeColumna,4,Lista,ListaNueva);
    buscarColapsoColumna(NumDeColumna,3,Lista,ListaNueva);
    ListaNueva=Lista.

buscarColapsoColumna(NumDeColumna,5,L,Lrta):-
    celda(0,NumDeColumna,X,_),
    celda(1,NumDeColumna,X,_),
    celda(2,NumDeColumna,X,_),
    celda(3,NumDeColumna,X,_),
    celda(4,NumDeColumna,X,_),
    Lista=[[0,NumDeColumna],[1,NumDeColumna],[2,NumDeColumna],[3,NumDeColumna],[4,NumDeColumna]],
    insertar_ultimo(Lista,L,Lrta).

buscarColapsoColumna(NumDeColumna,4,L,Lrta):-
    celda(0,NumDeColumna,X,_),
    celda(1,NumDeColumna,X,_),
    celda(2,NumDeColumna,X,_),
    celda(3,NumDeColumna,X,_),
    Lista=[[0,NumDeColumna],[1,NumDeColumna],[2,NumDeColumna],[3,NumDeColumna]],
    insertar_ultimo(Lista,L,Lrta);
    celda(1,NumDeColumna,X,_),
    celda(2,NumDeColumna,X,_),
    celda(3,NumDeColumna,X,_),
    celda(4,NumDeColumna,X,_),
    Lista=[[1,NumDeColumna],[2,NumDeColumna],[3,NumDeColumna],[4,NumDeColumna]],
    insertar_ultimo(Lista,L,Lrta).

buscarColapsoColumna(NumDeColumna,3,L,Lrta):-
    celda(0,NumDeColumna,X,_),
    celda(1,NumDeColumna,X,_),
    celda(2,NumDeColumna,X,_),
    Lista=[[0,NumDeColumna],[1,NumDeColumna],[2,NumDeColumna]],
    insertar_ultimo(Lista,L,Lrta);   
    celda(1,NumDeColumna,X,_),
    celda(2,NumDeColumna,X,_),
    celda(3,NumDeColumna,X,_),
    Lista=[[1,NumDeColumna],[2,NumDeColumna],[3,NumDeColumna]],
    insertar_ultimo(Lista,L,Lrta);
    celda(2,NumDeColumna,X,_),
    celda(3,NumDeColumna,X,_),
    celda(4,NumDeColumna,X,_),
    Lista=[[2,NumDeColumna],[3,NumDeColumna],[4,NumDeColumna]],
    insertar_ultimo(Lista,L,Lrta).

buscarTodosLosColapsos(Lrta):-
    buscarColapsosDeFilas([],L1),
    buscarColapsosDeColumnas(L1,Lrta),!.

obtenerCentro([_,E2,_],E2).     % Lista de 3 elementos
obtenerCentro([_,E2,_,_],E2).   % Lista de 4 elementos
obtenerCentro([_,_,E3,_,_],E3). % Lista de 5 elementos

siguienteEstado(sincambios,borrar).
siguienteEstado(borrar,agrandar).
siguienteEstado(agrandar,agrandar).

obtenerFila([E1,_],E1).
obtenerColumna([_,E2],E2).

marcarColapsosYcentrosComunes(Lista):-
    forall(member(ListaColapso,Lista),
           (forall(member(Elemento,ListaColapso),
                   (obtenerFila(Elemento,Fila),
                   obtenerColumna(Elemento,Columna),
                   celda(Fila,Columna,Valor,Estado),
                   siguienteEstado(Estado,NuevoEstado),
                   retract(celda(Fila,Columna,Valor,Estado)),
                   assert(celda(Fila,Columna,Valor,NuevoEstado)))))).

tieneCentro([E1,E2,E3,E4,E5]):-
    obtenerFila(E1,F1),
    obtenerColumna(E1,C1),
    celda(F1,C1,_,borrar),
    obtenerFila(E2,F2),
    obtenerColumna(E2,C2),
    celda(F2,C2,_,borrar),
    obtenerFila(E3,F3),
    obtenerColumna(E3,C3),
    celda(F3,C3,_,borrar),
    obtenerFila(E4,F4),
    obtenerColumna(E4,C4),
    celda(F4,C4,_,borrar),
    obtenerFila(E5,F5),
    obtenerColumna(E5,C5),
    celda(F5,C5,_,borrar),
    ponerCentro([E1,E2,E3,E4,E5]);
    true.
    
tieneCentro([E1,E2,E3,E4]):-
    obtenerFila(E1,F1),
    obtenerColumna(E1,C1),
    celda(F1,C1,_,borrar),
    obtenerFila(E2,F2),
    obtenerColumna(E2,C2),
    celda(F2,C2,_,borrar),
    obtenerFila(E3,F3),
    obtenerColumna(E3,C3),
    celda(F3,C3,_,borrar),
    obtenerFila(E4,F4),
    obtenerColumna(E4,C4),
    celda(F4,C4,_,borrar),
    ponerCentro([E1,E2,E3,E4]);
    true.
    
tieneCentro([E1,E2,E3]):-
    obtenerFila(E1,F1),
    obtenerColumna(E1,C1),
    celda(F1,C1,_,borrar),
    obtenerFila(E2,F2),
    obtenerColumna(E2,C2),
    celda(F2,C2,_,borrar),
    obtenerFila(E3,F3),
    obtenerColumna(E3,C3),
    celda(F3,C3,_,borrar),
    ponerCentro([E1,E2,E3]);
    true.
    
ponerCentro(L):-
    obtenerCentro(L,Rta),
    obtenerFila(Rta,Fila),
    obtenerColumna(Rta,Columna),
    celda(Fila,Columna,Valor,Estado),
    retract(celda(Fila,Columna,Valor,Estado)),
    assert(celda(Fila,Columna,Valor,agrandar)).
    
marcarCentrosFinal(L):-
    forall(member(ListaColapso,L),
           tieneCentro(ListaColapso)).

marcar(L):- marcarColapsosYcentrosComunes(L), marcarCentrosFinal(L).


/*  Si el Estado es "agrandar", se agranda la mamushka.
    Si es "borrar", se reemplaza la mamushka por una "x".
    Si es "sincambios", no se hace ningun cambio. */
aplicarEstados:- forall(celda(F,C,Mamushka,Estado),
                       (actualizarMamushka(Mamushka,Estado,NuevaMamushka),
                        retract(celda(F,C,Mamushka,Estado)),
                        assert(celda(F,C,NuevaMamushka,sincambios));
                       true)).   

% actualizarMamushka(MamushkaActual,EstadoAAplicarle,NuevaMamushka)   
actualizarMamushka(_,borrar,x).                     
actualizarMamushka(r1,agrandar,r2).
actualizarMamushka(r2,agrandar,r3).
actualizarMamushka(r3,agrandar,r3).
actualizarMamushka(v1,agrandar,v2).
actualizarMamushka(v2,agrandar,v3).
actualizarMamushka(v3,agrandar,v3).
actualizarMamushka(a1,agrandar,a2).
actualizarMamushka(a2,agrandar,a3).
actualizarMamushka(a3,agrandar,a3).

gravedad:-
    recorrerColumna(0),
    recorrerColumna(1),
    recorrerColumna(2),
    recorrerColumna(3),
    recorrerColumna(4).

recorrerColumna(NumDeColumna):-
    celda(0,NumDeColumna,Valor0,_),
    celda(1,NumDeColumna,Valor1,_),
    celda(2,NumDeColumna,Valor2,_),
    celda(3,NumDeColumna,Valor3,_),
    celda(4,NumDeColumna,Valor4,_),
    Lista=[Valor4,Valor3,Valor2,Valor1,Valor0],
    listaOrdenada(Lista,NuevaL),
    rellenarColumna(NuevaL,Rta),
    pasarListaGravedadAHechos(NumDeColumna,Rta).
    
listaOrdenada([],[]).  
listaOrdenada([X|Xs],Lrta):-
    X==x,
    listaOrdenada(Xs,L),
    Lrta=L;
    listaOrdenada(Xs,L),
    Lrta=[X|L].

rellenarColumna([],[x,x,x,x,x]).
rellenarColumna([E1],[E1,x,x,x,x]).
rellenarColumna([E1,E2],[E1,E2,x,x,x]).
rellenarColumna([E1,E2,E3],[E1,E2,E3,x,x]).
rellenarColumna([E1,E2,E3,E4],[E1,E2,E3,E4,x]).
rellenarColumna([E1,E2,E3,E4,E5],[E1,E2,E3,E4,E5]).

pasarListaGravedadAHechos(NumDeColumna,[E4,E3,E2,E1,E0]):-
    retract(celda(4,NumDeColumna,_,_)),
    assert(celda(4,NumDeColumna,E4,sincambios)),
    retract(celda(3,NumDeColumna,_,_)),
    assert(celda(3,NumDeColumna,E3,sincambios)),
    retract(celda(2,NumDeColumna,_,_)),
    assert(celda(2,NumDeColumna,E2,sincambios)),
    retract(celda(1,NumDeColumna,_,_)),
    assert(celda(1,NumDeColumna,E1,sincambios)),
    retract(celda(0,NumDeColumna,_,_)),
    assert(celda(0,NumDeColumna,E0,sincambios)).


generarMamushkasRandom:-
            forall(celda(F,C,x,_),
                   (retract(celda(F,C,x,_)),
                    random_member(R,[r1,v1,a1]),
                    assert(celda(F,C,R,_)))).

