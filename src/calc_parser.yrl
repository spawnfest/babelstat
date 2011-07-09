Nonterminals
	eval expr term factor.

Terminals
	open close add minus multiply divide float power fn pi.

Rootsymbol eval.

Left 100 float.
Left 200 open.
Left 300 close.
Left 400 fn.
Left 500 minus.
Left 600 add.
Left 700 power.
Left 800 multiply.
Left 900 divide.


eval -> expr		     : '$1'.

expr -> expr add term	     : add('$1','$3').
expr -> expr minus term	     : subtract('$1','$3').
expr -> term                 : '$1'.

term -> term multiply factor : multiply('$1','$3').
term -> term divide   factor : divide('$1','$3').
term -> term power    factor : power('$1','$3').
term -> term fn       factor : unwrap('$1').
term -> factor		     : '$1'.

factor -> float		     : unwrap('$1').
factor -> open expr close    : '$2'.
factor -> pi                 : math(unwrap('$1')).
    
    

Erlang code.



unwrap({_,_,V}) -> io:format("unwrap3 ~p~n",[V]),V;
unwrap({_,V}) -> io:format("unwrap2 ~p~n",[V]), V;
unwrap(V) -> io:format("unwrap ~p~n",[V]), V.

add(A,B) ->
    A+B.
subtract(A,B) ->
    A-B.
divide(A,B) ->
    A/B.
multiply(A,B) ->
    A*B.
power(A,B) ->
    math:pow(A,B).
math(pi) ->
    math:pi();
math(Other) ->
    io:format("Unknown operator ~p~n",[Other]),
    0.0.
