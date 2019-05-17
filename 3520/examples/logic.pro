/* logic.pro LGN grammar example
  call terminals terma, termb


s --> a, b.
a --> [terma].
b --> [termb].

*/
/*use this for the grammar of string of u, r, d, l
//use to parse for grammar of the square
*/

/* LGN grammar, first arg s,a,b second arg whats left over
    ask for add'l variables/arguments

    fundamental for creating an attribute grammar, u,r,d,l all the same number
*/
s(E1,E2)--> a(E1),b(E2).
a(ta) --> [ta].
b(tb) --> [tb].
