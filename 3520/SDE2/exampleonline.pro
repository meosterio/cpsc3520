% printAlist/1
% printAlist(+Alist)
% * adds '.' at the end of a list.
% * does not capitalize first word.
% ** Returns true if syntactically correct **
%

printAlist([]).
printAlist([Word]) :- write(Word), write('.').
printAlist([First|Rest]) :-
   write(First), write(' '), printAlist(Rest).

% scan/1
% Converts input to a list (for the parser).
% scan(Input).
% |: this is my sentence
% this is my sentence.
% Input = [this,is,my,sentence,'.'] .

scan(Input) :-
    read(UserInput),
    tokenize_atom(UserInput,Input).

% sentence/2
% sentence(?What, ?Leftover)
% 'What' is the list form of the sentence to be parsed/generated.
% and 'Leftover' is whatever is leftover.

% Make a dictionary of possible replacements
% adjectives (ADJ): the, a.
% nouns (NOUN): program, computer, plant, process, library, product.
% verbs (VERB): crashes, runs, calls, uses, contains, produces.
% LGN productions:
% 	SENTENCE -> NP VP
% 	NP -> ADJ NOUN
% 	VP -> VERB NP

sentence(X,Z) :- noun_phrase(X,Y), verb_phrase(Y,Z).
noun_phrase(X,Z) :- determiner(X,Y), noun(Y,Z).
verb_phrase(X,Z) :- verb(X,Y), noun_phrase(Y,Z).
verb_phrase(X,Z) :- verb(X,Y), sentence(Y,Z).

determiner([the|Z],Z).
determiner([a|Z],Z).

noun([program|Z],Z).
noun([library|Z],Z).
noun([process|Z],Z).
noun([computer|Z],Z).
noun([plant|Z],Z).

verb([run|Z],Z).
verb([call|Z],Z).
verb([use|Z],Z).
verb([crash|Z],Z).
verb([contain|Z],Z).
verb([produce|Z],Z).
