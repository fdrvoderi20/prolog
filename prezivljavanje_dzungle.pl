% Preživljavanje u džungli
% Autor: Filip Drvoderiæ


% Dinamicki predikati
:-dynamic polozaj/1,
          mjesto/1,
          lokacija/2,
          ima/1,
          putevi/2,
          listaStvari/2,
          popijena/1,
          prokuhana/1,
          vatra/1.


% Brisanje memorije o predikatima
:-retractall(polozaj(_)).
:-retractall(lokacija(_,_)).
:-retractall(ima(_)).
:-retractall(putevi(_,_)).
:-retractall(prolaz(_,_)).

% Operatori
:-op(100,fx,ispusti).
:-op(100,fx,mjesto).
:-op(100,fx,idiU).
:-op(100,fx,uzmi).
:-op(100,xfx,jeU).

% Predikat radi/1 za sucelje
radi(idiU X):-idiU(X),!.
radi(uzmi X):-uzmi(X),!.
radi(ispusti X):-ispusti(X),!.
radi(gledaj):-gledaj,!.
radi(inventar):-inventar,!.
radi(prokuhaj):-prokuhaj,!.
radi(zapaliVatru):-zapaliVatru,!.
radi(popij):-popij,!.
radi(kraj).


% Popis mjesta
mjesto(put).
mjesto(dzungla1).
mjesto(rijeka).
mjesto(livada).
mjesto(dzungla2).
mjesto(dzungla3).
mjesto(cesta).
mjesto(grad).


%Popis lokacija
lokacija(mapa, put).
lokacija(ruksak, livada).
lokacija(lonac, ruksak).
lokacija(sibice, ruksak).
lokacija(litica, dzungla3).
lokacija(litica, cesta).
lokacija(drvo, dzungla2).
lokacija(lijana, dzungla2).
lokacija(voda, rijeka).


%Opis stvari
opisStvari(ruksak,opis(izgubljeni,500)).
opisStvari(lonac,opis(stari,1000)).
opisStvari(sibice,opis(male,20)).
opisStvari(mapa,opis(tajanstvena,100)).
opisStvari(litica,opis(smrtonosna,100000)).
opisStvari(drvo,opis(zapaljivo,1500)).
opisStvari(lijana,opis(snazna,300)).
opisStvari(voda,opis(neprokuhana,100)).


% Putevi
putevi(put, dzungla1).
putevi(dzungla1, dzungla2).
putevi(dzungla1, livada).
putevi(livada, rijeka).
putevi(rijeka, dzungla3).
putevi(dzungla3, cesta).
putevi(cesta, grad).


%Pravilo za prolaz
prolaz(X,Y):-putevi(X,Y).
prolaz(X,Y):-putevi(Y,X).


%Pravilo za je u
jeU(X,Y):-lokacija(X,Y).
jeU(X,Y):-lokacija(Z,Y), jeU(X,Z).


% Pocetni polozaj
polozaj(put).


% Sucelje - pocetak i kraj igre
start:-write('Dobrodosli u Prezivljavanje u dzungli'),nl,
       repeat,write('IGRA> '),
       read(X),radi(X),nl,zavrsi(X).

zavrsi(kraj):-write('Vec odustajes ? Razocaravajuce!!! ').
zavrsi(_) :- polozaj(grad),
             write('Cestitke! Uspjesno se prezivjeli dzunglu').


% Pocetna stanja predmeta
neprokuhana(voda).


% Lista stvari na odreðenome mjestu
listaStvari(Mjesto) :- jeU(X,Mjesto),
                       tab(2),
                       opisStvari(X,opis(Y,_)),
                       write(Y),
                       write(' '),
                       write(X),
                       nl,
                       fail.
listaStvari(_).


%Lista veza na odreðenome mjestu
listaVeza(Mjesto) :- prolaz(X,Mjesto),
                     tab(2),
                     write(X),
                     nl,
                     fail.
listaVeza(_).


% Pravila za gledanje okoline
gledaj :- polozaj(Ovdje),
          write('Nalazite se u '),
          write(Ovdje),
          nl,
          write('Mozete vidjeti: '),
          nl,
          listaStvari(Ovdje),
          nl,
          write('Mozete ici: '),
          nl,
          listaVeza(Ovdje).
          
pogledaj(Ovdje) :- jeU(_,Ovdje),
                   write(Ovdje),
                   write(' sadrzi:'),
                   nl,
                   listaStvari(Ovdje).


% Pravila za kretanje
mozeIci(dzungla3) :-
    polozaj(cesta),
    write('Ne možete se vratiti u dzunglu, nemate više lijanu!'),
    nl,
    !,
    fail.

mozeIci(Mjesto) :- polozaj(X), prolaz(X,Mjesto).

mozeIci(Mjesto) :- Mjesto \= 'dzungla3',
                   write('Ne postoji prolaz za '),
                   write(Mjesto),
                   nl,
                   fail.
                   
pokret(Mjesto) :- retract(polozaj(_)), asserta(polozaj(Mjesto)).

idiU(Mjesto) :- Mjesto \= 'cesta',
                Mjesto \= 'grad',
                Mjesto \= 'dzungla1',
                Mjesto \= 'dzungla3',
                mozeIci(Mjesto),
                pokret(Mjesto),
                gledaj.
                
% Ostavljanje mape
idiU(grad) :- ima(mapa),
              write('Ne možete iæi u grad dok imate mapu!'),
              nl,
              write('Morate je ostaviti, mozda ce je jos netko zatrebati da prezivi dzunglu!!!'),
              nl,
              !,
              fail.

idiU(grad) :- \+ ima(mapa),
              mozeIci(grad),
              pokret(grad),
              gledaj.


% Prepreka za ulazak u dzunglu
idiU(dzungla1) :- \+ ima(mapa),
                  write('Prije nego krenes u dzunglu bilo bi pametno pogledati oko sebe!'),
                  nl,
                  write('Mozda postoji nesto sto ce ti pomoci na putu!'),
                  nl,
                  fail.

idiU(dzungla1) :- ima(mapa),
                  mozeIci(dzungla1),
                  pokret(dzungla1),
                  gledaj.


% Prepreka za prelaz rijeke
idiU(dzungla3) :- mozeIci(dzungla3),
                  prokuhana(voda),
                  popijena(voda),
                  pokret(dzungla3),
                  gledaj.

idiU(dzungla3) :- \+prokuhana(voda),
                  write('Osvjezi se prije prelaska rijeke! Popij prokuhanu vodu!'),
                  nl,
                  fail.

idiU(dzungla3) :- prokuhana(voda),
                  \+popijena(voda),
                  write('Osvjezi se prije prelaska rijeke! Popij prokuhanu vodu!'),
                  nl,
                  fail.
                  
                  
% Prepraka za preskakanje litice

idiU(cesta) :- zaveziLijanu.

zaveziLijanu :- ima(lijana),
                mozeIci(cesta),
                retract(ima(lijana)),
                pokret(cesta),
                write('Iskoristili ste lijanu i preskocili smrtonosnu liticu!'),
                nl,
                gledaj.

zaveziLijanu :- \+ima(lijana),
                write('Potrebna vam je lijana da biste preskocili na cestu!'),
                nl,
                fail.


% Pravila za uzimanje stvari
mozeUzeti(Stvar) :- polozaj(X),
                    jeU(Stvar,X),
                    opisStvari(Stvar,opis(_ ,Y)),
                    Y=<20000.

mozeUzeti(Stvar) :- polozaj(X),
                    jeU(Stvar,X),
                    opisStvari(Stvar,opis(_ ,Y)),
                    Y>20000,
                    write('To nije moguce uzeti'),
                    nl,
                    fail.

mozeUzeti(Stvar) :- polozaj(X),
                    not(jeU(Stvar,X)),
                    write('Toga nema ovdje'),
                    nl,
                    fail.

uzetiSveIz(Sadrzaj) :- lokacija(Item, Sadrzaj),
                       retract(lokacija(Item, Sadrzaj)),
                       asserta(ima(Item)),
                       fail.
uzetiSveIz(_).

uzeti(Stvar) :- retract(lokacija(Stvar, _)),
                asserta(ima(Stvar)),
                uzetiSveIz(Stvar),
                write('Uzeo sam '),
                write(Stvar),
                nl.

uzmi(Stvar) :- mozeUzeti(Stvar),
               uzeti(Stvar).
               

% Pravilo za ispustanje stvari
ispusti(Stvar) :- ima(Stvar),
                  retract(ima(Stvar)),
                  polozaj(TrenutnaLokacija),
                  asserta(lokacija(Stvar, TrenutnaLokacija)),
                  write('Ispustio si '),
                  write(Stvar),
                  write(' na '),
                  write(TrenutnaLokacija),
                  nl.

ispusti(Stvar) :- \+ ima(Stvar),
                  write('Nemate '),
                  write(Stvar),
                  write(' koju biste mogli ispustiti.'),
                  nl.


% Inventar lika
inventar :- write('Trenutno imam:'),
            nl,
            popisStvari.

popisStvari:-ima(X),write(X),nl,fail.
popisStvari.


% Pravilo za paljenje vatre
zapaliVatru :- ima(sibice),
               ima(drvo),
               \+ vatra(zapaljena),
               retract(ima(drvo)),
               retract(ima(sibice)),
               asserta(vatra(zapaljena)),
               write('Vatra je zapaljena. Drvo i šibice su iskorištene. Sada možete prokuhati vodu.'),
               nl.
               
zapaliVatru :- vatra(zapaljena),
               write('Vatra je vec zapaljena.'),
               nl.
               
zapaliVatru :- \+ima(sibice),
               write('Nemate sibice.'),
               nl,
               fail.
               
zapaliVatru :- \+ima(drvo),
               write('Nemate drva.'),
               nl,
               fail.


% Prokuhavanje vode
prokuhaj :- vatra(zapaljena),
            polozaj(rijeka),
            ima(lonac),
            ima(voda),
            \+prokuhana(voda),
            asserta(prokuhana(voda)),
            write('Prokuhali ste vodu!'),
            nl.
            
prokuhaj :- \+vatra(zapaljena),
            write('Morate prvo zapaliti vatru.'),
            nl,
            fail.
            
prokuhaj :- prokuhana(voda),
            write('Voda je veæ prokuhana.'),
            nl.
            
prokuhaj :- \+ima(lonac),
            write('Nemate lonac.'),
            nl,
            fail.
            
prokuhaj :- \+ima(voda),
            write('Nemate vodu.'),
            nl,
            fail.
            
prokuhaj :- \+polozaj(rijeka),
            write('Morate biti pored rijeke da biste prokuhali vodu.'),
            nl,
            fail.


% Pijenje vode
popij :- prokuhana(voda),
         \+ popijena(voda),
         retract(ima(voda)),
         asserta(popijena(voda)),
         write('Popio si vodu i sada si spreman za daljnji put!'),
         nl.
         
popij :- popijena(voda),
         write('Vec si popio vodu!'),
         nl,
         fail.
         
popij :- \+ima(voda),
         write('Nemas nista za popiti! Uzmi vodu iz rijeke!'),
         nl,
         !,
         fail.
         
popij :- \+prokuhana(voda),
         write('Voda nije prokuhana!'),
         nl,
         !,
         fail.
