/* list generate example (list_generate.pro) */

theList(0, []).

theList(N, [N|T]) :- NM1 is N-1, theList(NM1, T).
