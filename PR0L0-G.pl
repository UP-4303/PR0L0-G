%%% INFO-501, TP3
%%% Xavier Maziere INFO-G1
%%% Mattéo Luque INFO-CMI
%%%
%%% Lancez la "requête"
%%% jouer.
%%% pour commencer une partie !
%

% il faut déclarer les prédicats "dynamiques" qui vont être modifiés par le programme.
:- dynamic position/2,
        position_courante/1,
        points_de_vie/1,
        passage/3,
        ouvert/2,
        position_monstre/2,
        pv_monstre/2,
        type_monstre/2,
        active/2,
        compte_tours/1,
        sac/2,
        radar/1,
        mine/1,
        bombe/1,
        detecteur/1,
        visite/1,
        energie_detecteur/1,
        mine_a_enlever/1,
        obtennu/1.

% on remet à jours les positions des objets et du joueur
:- retractall(position(_, _)),
        retractall(position_courante(_)),
        retractall(points_de_vie(_)),
        retractall(passage(_,_,_)),
        retractall(ouvert(_,_)),
        retractall(position_monstre(_,_)),
        retractall(pv_monstre(_,_)),
        retractall(type_monstre(_,_)),
        retractall(active(_,_)),
        retractall(compte_tours(_)),
        retractall(sac(_,_)),
        retractall(radar(_)),
        retractall(mine(_)),
        retractall(detecteur(_)),
        retractall(visite(_)),
        retractall(energie_detecteur(_)),
        retractall(mine_a_enlever(_)),
        retractall(obtennu(_)).

% on déclare des opérateurs, pour autoriser `prendre torche` au lieu de `prendre(torche)`
:- op(1000, fx, prendre).
:- op(1000, fx, aller).
:- op(1000, fx, appuyer).
:- op(1000, fx, utiliser).
:- op(1000, fx, ramasser).
:- op(1000, fx, recharger).
:- op(1000, fx, desactiver).
:- op(1000, fx, ram).
:- op(1000, fx, rec).
:- op(1000, fx, u).
:- op(1000, fx, des).
:- op(1000, fx, a).

% quelques raccourcis
n :- aller(nord).
s :- aller(sud).
e :- aller(est).
o :- aller(ouest).
st :- status().
reg :- regarder().
ram(X) :- ramasser(X).
rec(X) :- recharger(X).
u(X) :- utiliser(X).
des(X) :- desactiver(X).
a(X) :- appuyer(X).

% Position du joueur. Ce prédicat sera modifié au fur et à mesure de la partie (avec `retract` et `assert`)
position_courante((1,1)).

% Points de vie du joueur
points_de_vie(3).

detection_mort(_Cause) :-
        points_de_vie(Points),
        Points > 0, !.

detection_mort(Cause) :-
        !,
        perdre(Cause).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utilitaires

taille([], 0).

taille([_ | Reste], Resultat) :-
        taille(Reste, ResultatReste),
        Resultat is ResultatReste + 1.

getIndex([Elem | _], 0, Elem).

getIndex([_ | Suite], Index, Resultat) :-
        IndexSuivant is Index-1,
        getIndex(Suite, IndexSuivant, Resultat).

isPorte(Pos1, Pos2) :-
        porte(_, Pos1, Pos2, _, _).
isPorte(Pos1, Pos2) :-
        porte(_, Pos2, Pos1, _, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Passages entre les différentes cases
% Numéros d'ouverture, position1, position2, direction1-2, direction2-1
porte([1], (1,1), (1,2), sud, nord).
porte([2], (1,3), (1,4), sud, nord).
porte([3,4], (2,2), (3,2), est, ouest).
porte([3,4], (3,1), (4,1), est, ouest).
porte([6], (4,1), (4,2), sud, nord).
porte([3], (2,3), (3,3), est, ouest).
porte([6], (4,4), (5,4), est, ouest).
porte([7], (5,1), (5,2), sud, nord).
porte([5], (6,2), (6,3), sud, nord).

passage((1,1), est, (2,1)).
passage((2,1), ouest, (1,1)).

passage((1,2), est, (2,2)).
passage((2,2), ouest, (1,2)).
passage((1,2), sud, (1,3)).
passage((1,3), nord, (1,2)).

passage((3,1), sud, (3,2)).
passage((3,2), nord, (3,1)).

passage((1,4), est, (2,4)).
passage((2,4), ouest, (1,4)).
passage((2,4), est, (3,4)).
passage((3,4), ouest, (2,4)).
passage((2,3), sud, (2,4)).
passage((2,4), nord, (2,3)).

passage((3,3), est, (4,3)).
passage((4,3), ouest, (3,3)).
passage((4,3), est, (5,3)).
passage((5,3), ouest, (4,3)).
passage((4,2), sud, (4,3)).
passage((4,3), nord, (4,2)).
passage((4,3), sud, (4,4)).
passage((4,4), nord, (4,3)).
passage((5,2), sud, (5,3)).
passage((5,3), nord, (5,2)).

passage((5,1), est, (6,1)).
passage((6,1), ouest, (5,1)).
passage((6,1), sud, (6,2)).
passage((6,2), nord, (6,1)).

passage((6,3), sud, (6,4)).
passage((6,4), nord, (6,3)).

passage((4,4), est, (5,4)).
passage((5,4), ouest, (4,4)).

% Portes ouvertes
ouvert((4,4), (5,4)).

% salles

salle(c, [(1,1), (2,1)]).
salle(v, [(1,2), (2,2), (1,3)]).
salle(o, [(1,4), (2,4), (2,3), (3,4)]).
salle(m, [(3,1), (3,2)]).
salle(i, [(4,1)]).
salle(p, [(3,3), (4,2), (4,3), (4,4), (5,2), (5,3)]).
salle(e, [(5,4)]).
salle(g, [(5,1), (6,1), (6,2)]).
salle(r, [(6,3), (6,4)]).

% nombre porte
nb_porte(1,1).
nb_porte(2,1).
nb_porte(3,3).
nb_porte(4,2).
nb_porte(5,1).
nb_porte(6,2).
nb_porte(7,1).

% À utiliser sous la forme : directions_possibles((1,2), R, []).
directions_possibles(Position, [(Possible, Direction) | Suite], DejaTrouve) :-
        passage(Position, Direction, Possible),
        \+member((Possible, Direction), DejaTrouve),
        directions_possibles(Position, Suite, [(Possible, Direction) | DejaTrouve]).

directions_possibles(_Position, [], _DejaTrouve).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Position des objets
position(batterie, (2,1)).
position(batterie, (3,3)).
position(batterie, (5,1)).

position(bombe, (5,4)).

position(mine, (3,4)).

position(detecteur, (3,2)).
energie_detecteur(3).

% inventaire de base

sac(mine, 1).

radar(charge).

obtennu(radar).

% position bouton

bouton(1, (2,1)).
bouton(4, (2,1)).
bouton(6, (3,1)).
bouton(4, (4,1)).
bouton(6, (6,1)).
bouton(7, (4,2)).
bouton(2, (1,3)).
bouton(3, (5,3)).
bouton(5, (1,4)).
bouton(3, (3,4)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gestion tour du joueur

compte_tours(0).

passer() :-
        tour_monstres([]), !,
        consommation_detecteur,
        activation_detecteur,
        activation_mine,
        enlever_mines,
        compte_tours(X),
        retract(compte_tours(X)),
        assert(compte_tours(0)), !.

action_joueur() :- % l'action réalisé est la troisième -> les monstres agissent
        compte_tours(X),
        X = 2,
        passer(), !.

action_joueur() :- % il s'agit de la première ou deuxième action
        compte_tours(X),
        New_X is X + 1,
        retract(compte_tours(X)),
        assert(compte_tours(New_X)), !.

% gestion détécteur
consommation_detecteur() :-
        detecteur(Pos),
        energie_detecteur(Energie),
        Energie == 1,
        retract(detecteur(Pos)),
        retract(energie_detecteur(Energie)),
        assert(energie_detecteur(0)),
        assert(position(detecteur, Pos)),
        write("Le détecteur en "), write(Pos), write(" a céssé de fonctionner."),!, nl.

consommation_detecteur() :-
        detecteur(_),
        energie_detecteur(Energie),
        New_Energie is Energie - 1,
        retract(energie_detecteur(Energie)),
        assert(energie_detecteur(New_Energie)),!.

consommation_detecteur().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Monstres

% ID, type
% type_monstre(5, chien). % TEST MONSTER
type_monstre(1, chien).
type_monstre(2, chien).
type_monstre(3, rampant).
type_monstre(4, hurleur).

% ID, position
% position_monstre(5, (2,1)).
position_monstre(1, (3,1)).
position_monstre(2, (4,4)).
position_monstre(3, (4,1)).
position_monstre(4, (6,1)).

% ID, PV
% pv_monstre(5, 1).
pv_monstre(1, 1).
pv_monstre(2, 1).
pv_monstre(3, 2).
pv_monstre(4, 2).

% Comportements spéciaux (aléatoires par défaut)
comportement_suiveur(chien).
comportement_suiveur(hurleur).

% Points de déplacement
points_deplacements(chien, 2).
points_deplacements(hurleur, 1).
points_deplacements(rampant, 1).

points_attaque(chien, 1).
points_attaque(hurleur, 1).
points_attaque(rampant, 1).

getIndexPlusPasBouger(_Position, Directions, Random, Resultat) :-
        getIndex(Directions, Random, Resultat).

getIndexPlusPasBouger(Position, _Directions, _Random, Resultat) :-
        Resultat = (Position, center).

% Ne pas bouger est une direction valide !
direction_aleatoire(Position, Resultat) :-
        directions_possibles(Position, Directions, []),
        taille(Directions, Taille),
        TaillePlusUn is Taille+1, % Pour avoir "ne pas bouger" en dernier
        Random is random(TaillePlusUn),
        getIndexPlusPasBouger(Position, Directions, Random, Resultat).

% pathfinder
% départ, arrivée, résultat, cases explorées
pathfinder(X, X, [X], []).

pathfinder(X, Y, [X, Y], Explore) :-
        \+member(X, Explore),
        \+member(Y, Explore),
        passage(X, _Direction, Y).

pathfinder(X, Y, [X|Xs], Explore) :-
        \+member(X, Explore),
        passage(X, _Direction, W),
        pathfinder(W, Y, Xs, [X|Explore]).

% COMPORTEMENT SUIVEUR
% chemin trouvé, mais est déjà sur la case
deplacement_monstre(Id) :-
        type_monstre(Id, Type),
        comportement_suiveur(Type),
                position_monstre(Id, (X,Y)),
                position_courante((X,Y)).

% chemin trouvé, avance d'une case
deplacement_monstre(Id) :-
        type_monstre(Id, Type),
        comportement_suiveur(Type),
                position_monstre(Id, Pos),
                position_courante(PosJoueur),
                pathfinder(Pos, PosJoueur, [_, NewPos | _], []), !,
                        retract(position_monstre(Id, Pos)),
                        assert(position_monstre(Id, NewPos)).

% COMPORTEMENT DEFAUT (ALEATOIRE)
deplacement_monstre(Id) :-
        position_monstre(Id, Pos),
        direction_aleatoire(Pos, (NewPos, _Direction)), !,
                retract(position_monstre(Id, Pos)),
                assert(position_monstre(Id, NewPos)).

% Avance de plusieurs cases
deplacements_monstre(_Id, PointsDep) :-
        PointsDep =< 0.

deplacements_monstre(Id, _PointsDep) :-
        position_monstre(Id, Pos),
        position_courante(PosJoueur),
        pathfinder(Pos, PosJoueur, [Pos], []).                  % Si le pathfinder ne contient qu'une case, le monstre n'a pas beosin de bouger.

deplacements_monstre(Id, _PointsDep) :-
        position_monstre(Id, Pos),
        position_courante(PosJoueur),
        pathfinder(Pos, PosJoueur, [_, NewPos | _], []),
        isPorte(Pos, NewPos).                                   % Si il s'apprête à traverser une porte après le premier mouvement, on l'en empêche.

deplacements_monstre(Id, PointsDep) :-
        deplacement_monstre(Id),
        NewPointsDep is PointsDep-1,
        deplacements_monstre(Id, NewPointsDep).

premier_deplacement_monstre(Id, PointsDep) :-
        position_monstre(Id, Pos),
        deplacement_monstre(Id),
        position_monstre(Id, NewPos),
        \+isPorte(Pos, NewPos),
                NewPointsDep is PointsDep-1,
                deplacements_monstre(Id, NewPointsDep).

premier_deplacement_monstre(_Id, _PointsDep).

attaque_monstre(Id) :-
        position_monstre(Id, Position),
        position_courante(PositionJoueur),
        PositionJoueur == Position,
                type_monstre(Id, Type),
                points_de_vie(PV),
                points_attaque(Type, Attaque),
                NouvPV is PV - Attaque,
                retract(points_de_vie(PV)),
                assert(points_de_vie(NouvPV)),
                detection_mort(Type).

attaque_monstre(_).

tour_monstre(Id) :-
        type_monstre(Id, Type),
        points_deplacements(Type, PointsDep),
        premier_deplacement_monstre(Id, PointsDep),
        attaque_monstre(Id).

tour_monstres(DejaJoue) :-
        type_monstre(Id, _Type),
        \+member(Id, DejaJoue),
        tour_monstre(Id),
        tour_monstres([Id | DejaJoue]).

tour_monstres(_).

retract_monstre(Id) :-
        retractall(type_monstre(Id, _)),
        retractall(position_monstre(Id, _)),
        retractall(pv_monstre(Id, _)).

degat_monstre(Id, Degats) :-
        pv_monstre(Id, PV),
        NewPV is PV - Degats,
        retract(pv_monstre(Id, PV)),
        assert(pv_monstre(Id, NewPV)),
        detection_mort_monstre(Id).

detection_mort_monstre(Id) :-
        pv_monstre(Id, PV),
        PV =< 0,
        type_monstre(Id, Type),
        mort_monstre(Type),
        retract_monstre(Id).
detection_mort_monstre(_Id).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% interaction objet monstre

arret_detecteur(Pos) :-
        retract(energie_detecteur(_)),
        assert(energie_detecteur(0)),
        retract(detecteur(Pos)),
        assert(position(detecteur, Pos)),
        write("Le détecteur en "), write(Pos), write(" a céssé de fonctionner."),!, nl.

activation_detecteur() :-
        detecteur(Pos),
        position_monstre(Id, Pos),
        \+type_monstre(Id, hurleur),
        nl, write("ENNEMI DÉTÉCTÉ EN "), write(Pos), nl,
        fail.

activation_detecteur() :-
        detecteur(Pos),
        position_monstre(Id, Pos),
        type_monstre(Id, hurleur),
        arret_detecteur(Pos),
        fail.

activation_detecteur().

activation_mine() :-
        mine(Pos),
        position_monstre(Id, Pos),
        degat_monstre(Id, 1),
        assert_mine_a_enlever(Pos),
        fail.
activation_mine().

assert_mine_a_enlever(Pos) :-
        \+mine_a_enlever(Pos),
        assert(mine_a_enlever(Pos)).
assert_mine_a_enlever(_Pos).

enlever_mines() :-
        mine_a_enlever(Pos),
        degat_joueur_mine(Pos),
        nl, write("BOOM"), nl, write("La mine en "), write(Pos), write(" a explosé"), nl,
        retract(mine(Pos)),
        retract(mine_a_enlever(Pos)),
        fail.
enlever_mines().

degat_joueur_mine(Pos)  :-
        position_courante(Pos),
        points_de_vie(PV),
        NouvPV is PV - 1,
        retract(points_de_vie(PV)),
        assert(points_de_vie(NouvPV)),
        detection_mort(mine), !.
degat_joueur_mine(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ramasser un objet

check_objet_obtenu() :-
        sac(Obj, _Num),
        \+ obtennu(Obj),
        assert(obtennu(Obj)),
        decrire_objet(Obj),nl,
        check_objet_obtenu().
check_objet_obtenu().

ramasser(Obj) :- % un exemplaire de l'objet est déjà dens le sac
        position_courante(Pos),
        position(Obj, Pos),
        sac(Obj, Num),
        New_Num is Num + 1,
        retract(sac(Obj,Num)),
        retract(position(Obj, Pos)),
        assert(sac(Obj,New_Num)),
        write("OK."), nl,
        check_objet_obtenu,
        action_joueur,
        !.

ramasser(Obj) :- % aucun exemplaire de cet objet est dans le sac
        position_courante(Pos),
        position(Obj, Pos),
        retract(position(Obj, Pos)),
        assert(sac(Obj,1)),
        write("OK."), nl,
        action_joueur,
        !.

ramasser(detecteur) :-
        position_courante(Pos),
        detecteur(Pos),
        write("Le détecteur ici est activé, il doit dabord être désactivé."), nl,!.

ramasser(mine) :-
        position_courante(Pos),
        mine(Pos),
        write("La mine ici est amorcée, elle doit d'abord être désactivé."), nl,!.

ramasser(bombe) :-
        position_courante(Pos),
        bombe(Pos),
        write("La bombe ici est amorcée, elle doit d'abord être désactivé."), nl,!.

ramasser(X) :- % l'objet n'est pas reconnu
        write("??? Je ne vois pas de "),
        write(X),
        write(" ici."), nl,
        fail.

% affichage inventaire

aff_obj_inventaire(detecteur, _Num) :-
        energie_detecteur(Energie),
        write("un détecteur avec "), write(Energie), write(" charges restante"),!, nl.
aff_obj_inventaire(Obj, Num) :-
        write(Num), write(" x "), write(Obj),!, nl.

inventaire() :- % affiche le contennu de l'inventaire
        write("vous avez en votre possession :"), nl,
        radar(Charge),
        write("un radar "), write(Charge), nl,
        sac(Obj,Num),
        aff_obj_inventaire(Obj, Num),
        fail.

status() :-
        points_de_vie(PV),
        write("Il vous reste "), write(PV), write(" points de vie."), nl,
        inventaire().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% utilistaion Objet %%%

% radar
check_monstre(Pos, Dir) :-
        position_monstre(_, Pos),
        write("    "), write(Dir), nl.
check_monstre(_,_).

utiliser(radar) :- % radar déchargé
        radar(decharge),
        write("Malheureusement, votre radar est déchargé, avant de pouvoir le réutiliser il faut le recharger."), nl, !.

utiliser(radar) :- % radar chargé
        position_courante((X,Y)),
        Left_x is X - 1,
        Right_x is X + 1,
        Up_y is Y - 1,
        Down_y is Y + 1,
        retract(radar(charge)),
        write("Le radar indique des présences non-identifiées :"), nl,
        check_monstre((X,Y), "sur votre position"),!,
        check_monstre((X, Up_y), "au nord"),!,
        check_monstre((Right_x,Up_y), "au nord-est"),!,
        check_monstre((Right_x,Y), "à l'est"),!,
        check_monstre((Right_x,Down_y), "au sud-est"),!,
        check_monstre((X,Down_y), "au sud"),!,
        check_monstre((Left_x,Down_y), "au sud-ouest"),!,
        check_monstre((Left_x,Y), "à l'ouest"),!,
        check_monstre((Left_x,Up_y), "au nord-ouest"),!,
        assert(radar(decharge)),
        !, action_joueur. 

% mine
utiliser(mine) :- % une seule mine dans l'inventaire
        sac(mine, 1),
        position_courante(Pos),
        assert(mine(Pos)),
        retract(sac(mine, 1)),!,
        write("Vous avez posé une mine là où vous vous trouvez, vous retenez l'endroit où elle est afin de ne pas la déclencher."), nl,
        !, action_joueur.

utiliser(mine) :- % au moins 2 mines dans l'inventaire
        sac(mine, Num),
        position_courante(Pos),
        assert(mine(Pos)),
        New_Num is Num - 1,
        retract(sac(mine, Num)),
        assert(sac(mine, New_Num)),!,
        write("Vous avez posé une mine là où vous vous trouvez, vous retenez l'endroit où elle est afin de ne pas la déclencher."), nl,
        !, action_joueur.

utiliser(mine) :- % aucune mine dans l'inventaire
        write("vous n'avez pas de mine à poser"),!, nl.

% detecteur
utiliser(detecteur) :-
        sac(detecteur, _Num),
        energie_detecteur(Energie),
        Energie = 0,
        write("Le détecteur n'a plus d'énergie, il faut le recharger avant de pouvoir l'utiliser"),!, nl.

utiliser(detecteur) :- % detecteur dans l'inventaire
        sac(detecteur, 1),
        position_courante(Pos),
        assert(detecteur(Pos)),
        retract(sac(detecteur, 1)),!,
        write("Vous avez posé un detecteur là où vous vous trouvez."), nl,
        !, action_joueur.

utiliser(detecteur) :- % aucun detecteur dans l'inventaire
        write("Vous n'avez pas de déctecteur à poser."),!, nl.

% bombe
utiliser(bombe) :-
        sac(bombe, _),
        position_courante(Pos),
        bombe(Pos),
        write("Vous feriez mieux de ne pas y aller trop fort sur les explosifs, vous ne cherchez pas à faire exploser le vaiseau ?"),!, nl.

utiliser(bombe) :- % une seule bombe dans l'inventaire
        sac(bombe, 1),
        position_courante(Pos),
        assert(bombe(Pos)),
        retract(sac(bombe, 1)),!,
        write("Vous avez posé une bombe là où vous vous trouvez, attention à ne pas vous tuer avec."), nl,
        !, action_joueur.

utiliser(bombe) :- % au moins 2 bombes dans l'inventaire
        sac(bombe, Num),
        position_courante(Pos),
        assert(bombe(Pos)),
        New_Num is Num - 1,
        retract(sac(bombe, Num)),
        assert(sac(bombe, New_Num)),!,
        write("Vous avez posé une bombe là où vous vous trouvez, attention à ne pas vous tuer avec."), nl,
        !, action_joueur.

utiliser(bombe) :- % aucune bombe dans l'inventaire
        write("Vous n'avez pas de bombe à poser."),!, nl.

% autre
utiliser(_) :- % l'objet n'est pas reconnus
        write("Vous ne pouvez pas utiliser ça."), nl.

% batterie
consomme_batterie() :- % une seule batterie dans l'inventaire
        sac(batterie, 1),
        retract(sac(batterie, 1)),!.

consomme_batterie() :- % au moins 2 batteries dans l'inventaire
        sac(batterie, Num),
        New_Num is Num - 1,
        retract(sac(batterie, Num)),
        assert(sac(batterie, New_Num)),!.

recharger(radar) :-
        radar(charge),
        write("Votre radar est déjà chargé, pas la peine de comnsommer une batterie pour rien"),!, nl.

recharger(radar) :-
        radar(decharge),
        sac(batterie, _Num),
        consomme_batterie,
        retract(radar(decharge)),
        assert(radar(charge)),
        write("Vous rechargez votre radar, vous pouvez à nouveau l'utiliser."), nl,
        !, action_joueur.


recharger(detecteur) :-
        sac(detecteur, _Num),
        energie_detecteur(Energie),
        Energie = 3,
        write("Le détecteur est déjà chargé à bloc, inutile de gacher une batterie."),!, nl.

recharger(detecteur) :-
        sac(detecteur, _Num),
        consomme_batterie,
        retract(energie_detecteur(_)),
        assert(energie_detecteur(3)),
        write("Vous recharger le détecteur, il est maintenant utilisable à nouveau"), nl,
        !, action_joueur.

recharger(_) :-
        \+ sac(batterie, _Num),
        write("Vous n'avez pas de batterie à disposition."), nl, !.

recharger(detecteur) :-
        write("Vous n'aves pas de détecteur sur vous."),!, nl.

recharger(_) :-
        write("Qu'est-ce que vous esseyez de faire ?"),!, nl.

% désactiver

desactiver(mine) :-
        position_courante(Pos),
        mine(Pos),
        retract(mine(Pos)),
        assert(position(mine, Pos)),
        write("Vous avez désamorcé la mine."), nl,!.

desactiver(bombe) :-
        position_courante(Pos),
        bombe(Pos),
        retract(bombe(Pos)),
        assert(position(bombe, Pos)),
        write("Vous avez désamorcé la bombe."), nl,!.

desactiver(detecteur) :-
        position_courante(Pos),
        detecteur(Pos),
        retract(detecteur(Pos)),
        assert(position(detecteur, Pos)),
        write("Vous avez désactivé le détecteur."), nl,!.

desactiver(_) :-
        write("Qu'est-ce que vous bricolez ?"), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% activation objets

% bombe

tuer_joueur_salle(Id) :-
        position_courante(Pos),
        salle(Id, L_pos),
        member(Pos, L_pos),
        perdre(bombe).
tuer_joueur_salle(_).

tuer_monstre_salle(Id_s) :-
        salle(Id_s, L_pos),
        position_monstre(Id_m, Pos),
        member(Pos, L_pos),
        type_monstre(Id_m, Type),
        mort_monstre(Type),
        retract_monstre(Id_m),
        fail.
tuer_monstre_salle(_).

detoner(Pos) :-
        bombe(Pos),
        salle(Id, L_pos),
        member(Pos, L_pos),
        write("Vous appuyez sur le détonnateur. Quelques instants plus tard, un fracas étourdissant retentit."), nl,!,
        tuer_monstre_salle(Id),!,
        tuer_joueur_salle(Id).

detoner(_Pos) :-
        write("Il n'y a pas de bombe à cet endroit là."), nl.

detoner(X,Y) :-
        detoner((X,Y)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% déplacements du joueur

check_salle_visite(Pos) :-
        salle(Salle, L),
        member(Pos, L),
        \+ visite(Salle),
        assert(visite(Salle)),
        carte,
        regarder().
check_salle_visite(_).

check_salle_visite() :-
        position_courante(Position),
        check_salle_visite(Position).

aller(Direction) :-
        position_courante(Ici),
        passage(Ici, Direction, La),
        retract(position_courante(Ici)),
        assert(position_courante(La)),
        check_salle_visite(La),
        action_joueur,
        !.

aller(est) :-
        position_courante((6,4)),
        gagner().

aller(_) :-
        write("Vous ne pouvez pas aller par là."),
        fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% appuyer un bouton
        
check_porte(S,E,A,R) :- % ferme
        ouvert(S, E),
        retract(passage(S, A, E)),
        retract(passage(E, R, S)),
        retract(ouvert(S, E)), 
        write("Une porte se ferme."), nl.
        
check_porte(S,E,A,R) :- % ouvre
        assert(passage(S, A, E)),
        assert(passage(E, R, S)),
        assert(ouvert(S, E)),
        write("Une porte s'ouvre."), nl.

toggle_porte(N,X) :-
        0 < X,
        porte(L,S,E,A,R),
        member(N,L),
        \+ active(S,E), % vérification que la porte n'as pas étée ouverte/fermée dans une itéraion précédente
        check_porte(S,E,A,R),
        assert(active(S,E)),!,
    	New_X is X-1,
        toggle_porte(N,New_X).

toggle_porte(_N,X) :-
        X =< 0,
        retractall(active(_,_)).

appuyer(N) :-
        position_courante(P),
        bouton(N, P),
        nb_porte(N,X),
        toggle_porte(N,X),
        action_joueur, !.
        
appuyer(N) :-
        position_courante(P),
        \+ bouton(N, P),
        write("Il n'y a pas de bouton avec ce numéro ici."), nl.

affiche_position(Position) :-
        nl, write("Vous êtes en "), write(Position), write("."), nl.

% regarder autour de soi
regarder() :-
        position_courante(Position),
        salle(Salle, Cases),
        member(Position, Cases), !,
                decrire_salle(Salle),
                affiche_position(Position),
                lister_objets_piece(Cases), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% afficher la liste des objets à l'emplacement donné

aff_ouvert_ferme(Pos1, Pos2) :-
        ouvert(Pos1, Pos2),
        write("ouverte").
aff_ouvert_ferme(Pos1, Pos2) :-
        \+ouvert(Pos1, Pos2),
        write("fermée").

affiche_direction_porte(est) :-
        write("À l'Est, ").

affiche_direction_porte(ouest) :-
        write("À l'Ouest, ").

affiche_direction_porte(nord) :-
        write("Au Nord, ").

affiche_direction_porte(sud) :-
        write("Au Sud, ").

affiche_id_porte([]).
affiche_id_porte([X|L]) :-
        write(X), write(" "), affiche_id_porte(L).

aff_monstres(Position) :-
        position_monstre(Id, Position),
        type_monstre(Id, Type),
        decrire_monstre(Type),
        fail.
aff_monstres(_Position).

aff_obj(Pos) :-
        position(detecteur, Pos),
        energie_detecteur(Energie),
        write("Il y a un détécteur avec "), write(Energie), write(" charge restante posée au sol."), nl,
        fail.
aff_obj(Pos) :-
        position(X, Pos),
        write("Il y a une "), write(X), write(" posée au sol."), nl,
        fail.
aff_obj(_).

aff_bouton(Pos) :-
        bouton(Y, Pos),
        write("Il y a un bouton numéroté "), write(Y), write(" au mur."), nl,
        fail.
aff_bouton(_).

aff_portes1(Pos1) :-
        porte(Numeros, Pos1, Pos2, Direction1, _Direction2),
        affiche_direction_porte(Direction1),
        write("il y a une porte "), aff_ouvert_ferme(Pos1, Pos2), write(" avec "), affiche_id_porte(Numeros), write("écrit dessus."), nl,
        fail.
aff_portes1(_).

aff_portes2(Pos2) :-
        porte(Numeros, Pos1, Pos2, _Direction1, Direction2),
        affiche_direction_porte(Direction2),
        write("il y a une porte "), aff_ouvert_ferme(Pos1, Pos2), write(" avec "), affiche_id_porte(Numeros), write("écrit dessus."), nl,
        fail.
aff_portes2(_).

aff_mines(Pos) :-
        mine(Pos),
        write("Il y a une mine activée au sol."), nl,
        fail.
aff_mines(_).

aff_bombe(Pos) :-
        bombe(Pos),
        write("Il y a une bombe activée au sol."), nl,
        fail.
aff_bombe(_).

aff_detecteur(Pos) :-
        detecteur(Pos),
        energie_detecteur(Energie),
        write("Il y a un détecteur activée au sol, il lui reste "), write(Energie), write(" charges"), nl,
        fail.
aff_detecteur(_).

lister_objets(Position) :-
        aff_obj(Position), !,
        aff_bouton(Position), !,
        aff_portes1(Position), !, % on affiche les portes qui ont leur point de départ sur la position actuelle
        aff_portes2(Position), !, % on affiche les portes qui ont leur point d'arrivé sur la position actuelle
        aff_mines(Position), !,
        aff_bombe(Position), !,
        aff_detecteur(Position), !,
        aff_monstres(Position), !.
lister_objets(_) :-
        write("Cette case est vide.").

lister_objets_piece([Case | Reste]) :-
        nl, write("Case "), write(Case), write(" :"), nl,
        lister_objets(Case),
        lister_objets_piece(Reste).
lister_objets_piece([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% afficher la carte avec brouillard de guerre

empty() :- write("      ").
top() :- write("┌────┐").
slice() :- write("│    │").
bottom() :- write("└────┘").
left() :- write("│     ").
right() :- write("     │").
top_left() :- write("┌─────").
top_right() :- write("─────┐").
bottom_left() :- write("└─────").
bottom_right() :- write("─────┘").
path() :- write("──────").
top_path() :- write("┘    └").
bottom_path() :- write("┐    ┌").
left_top() :- write("┘    │").
right_top() :- write("│    └").
left_bottom() :- write("┐    │").
right_bottom() :- write("│    ┌").
top_door() :- write("┌─  ─┐").
bottom_door() :- write("└─  ─┘").
left_door() :- write("      ").
right_door() :- write("      ").
slice_door_left() :- write("     │").
slice_door_right() :- write("│     ").
top_left_door() :- write("┌─  ──").
top_right_door() :- write("─  ──┐").
bottom_left_door() :- write("└─  ──").
bottom_right_door() :- write("─  ──┘").
path_door() :- write("──  ──").

writeSalleIfVisite(Salle, Str) :-
        visite(Salle),
        call(Str), !.
writeSalleIfVisite(_Salle, _Str) :-
        call(empty), !.

connector_empty() :- write(" ").
connector_full() :- write("─").

writeConnectorIfVisite(Salle) :-
        visite(Salle),
        call(connector_full), !.
writeConnectorIfVisite(_Salle) :-
        call(connector_empty), !.

carte() :-
        write("     1      2      3      4      5      6  "), nl,
        write("  "), writeSalleIfVisite(c, top_left),          writeConnectorIfVisite(c),  writeSalleIfVisite(c, top_right),         call(connector_empty),      writeSalleIfVisite(m, top),               call(connector_empty),      writeSalleIfVisite(i, top),               call(connector_empty),      writeSalleIfVisite(g, top_left),          writeConnectorIfVisite(g),  writeSalleIfVisite(g, top_right),    nl,
        write("1 "), writeSalleIfVisite(c, left),              call(connector_empty),      writeSalleIfVisite(c, right),             call(connector_empty),      writeSalleIfVisite(m, slice_door_right),  call(connector_empty),      writeSalleIfVisite(i, slice_door_left),   call(connector_empty),      writeSalleIfVisite(g, left),              call(connector_empty),      writeSalleIfVisite(g, right),        nl,
        write("  "), writeSalleIfVisite(c, bottom_left_door),  writeConnectorIfVisite(c),  writeSalleIfVisite(c, bottom_right),      call(connector_empty),      writeSalleIfVisite(m, slice),             call(connector_empty),      writeSalleIfVisite(i, bottom_door),       call(connector_empty),      writeSalleIfVisite(g, bottom_left_door),  writeConnectorIfVisite(g),  writeSalleIfVisite(g, left_bottom),  nl,
        write("  "), writeSalleIfVisite(v, top_left_door),     writeConnectorIfVisite(v),  writeSalleIfVisite(v, top_right),         call(connector_empty),      writeSalleIfVisite(m, slice),             call(connector_empty),      writeSalleIfVisite(p, top_door),          call(connector_empty),      writeSalleIfVisite(p, top_door),          call(connector_empty),      writeSalleIfVisite(g, slice),        nl,
        write("2 "), writeSalleIfVisite(v, left),              call(connector_empty),      writeSalleIfVisite(v, right_door),        call(connector_empty),      writeSalleIfVisite(m, slice_door_left),   call(connector_empty),      writeSalleIfVisite(p, slice),             call(connector_empty),      writeSalleIfVisite(p, slice),             call(connector_empty),      writeSalleIfVisite(g, slice),        nl,
        write("  "), writeSalleIfVisite(v, right_bottom),      writeConnectorIfVisite(v),  writeSalleIfVisite(v, bottom_right),      call(connector_empty),      writeSalleIfVisite(m, bottom),            call(connector_empty),      writeSalleIfVisite(p, slice),             call(connector_empty),      writeSalleIfVisite(p, slice),             call(connector_empty),      writeSalleIfVisite(g, bottom_door),  nl,
        write("  "), writeSalleIfVisite(v, slice),             call(connector_empty),      writeSalleIfVisite(o, top),               call(connector_empty),      writeSalleIfVisite(p, top_left),          writeConnectorIfVisite(p),  writeSalleIfVisite(p, top_path),          writeConnectorIfVisite(p),  writeSalleIfVisite(p, left_top),          call(connector_empty),      writeSalleIfVisite(r, top_door),     nl,
        write("3 "), writeSalleIfVisite(v, slice),             call(connector_empty),      writeSalleIfVisite(o, slice_door_right),  call(connector_empty),      writeSalleIfVisite(p, left_door),         call(connector_empty),      call(empty),                              call(connector_empty),      writeSalleIfVisite(p, right),             call(connector_empty),      writeSalleIfVisite(r, slice),        nl,
        write("  "), writeSalleIfVisite(v, bottom_door),       call(connector_empty),      writeSalleIfVisite(o, slice),             call(connector_empty),      writeSalleIfVisite(p, bottom_left),       writeConnectorIfVisite(p),  writeSalleIfVisite(p, bottom_path),       writeConnectorIfVisite(p),  writeSalleIfVisite(p, bottom_right),      call(connector_empty),      writeSalleIfVisite(r, slice),        nl,
        write("  "), writeSalleIfVisite(o, top_left_door),     writeConnectorIfVisite(o),  writeSalleIfVisite(o, top_path),          writeConnectorIfVisite(o),  writeSalleIfVisite(o, top_right),         call(connector_empty),      writeSalleIfVisite(p, slice),             call(connector_empty),      writeSalleIfVisite(e, top),               call(connector_empty),      writeSalleIfVisite(r, slice),        nl,
        write("4 "), writeSalleIfVisite(o, left),              call(connector_empty),      call(empty),                              call(connector_empty),      writeSalleIfVisite(o, right),             call(connector_empty),      writeSalleIfVisite(p, slice_door_right),  call(connector_empty),      writeSalleIfVisite(e, slice_door_left),   call(connector_empty),      writeSalleIfVisite(r, slice),        nl,
        write("  "), writeSalleIfVisite(o, bottom_left),       writeConnectorIfVisite(o),  writeSalleIfVisite(o, path),              writeConnectorIfVisite(o),  writeSalleIfVisite(o, bottom_right),      call(connector_empty),      writeSalleIfVisite(p, bottom),            call(connector_empty),      writeSalleIfVisite(e, bottom),            call(connector_empty),      writeSalleIfVisite(r, bottom),       nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% affiche les instructions du jeu
instructions :-
        nl,
        write("Ce jeu est au tour par tour. Vous pouvez faire 3 actions, après quoi ce sera le tour des... autres."), nl,
        nl,
        write("Les commandes doivent être données avec la syntaxe Prolog habituelle."), nl,
        write("Les commandes ne demandant pas de point d'action sont :"), nl,
        write("jouer.                   -- pour commencer une partie."), nl,
        write("fin.                     -- pour terminer la partie et quitter."), nl,
        write("instructions.            -- pour revoir ce message !."), nl,
        write("status. st.              -- pour visualiser l'inventaire et les points de vie."), nl,
        write("regarder. reg.           -- pour revoir ce qui est présent dans la pièce."), nl,
        write("carte.                   -- pour revoir la carte actuelle."), nl,
        write("desactiver. des.         -- pour désactiver un objet actif afin de pouvoir le ramasser."), nl,
        nl,
        write("Les commandes demandant un point d'action sont :"), nl,
        write("appuyer numérobouton. a. -- pour appuyer sur un bouton"), nl,
        write("ramasser objet. ram.     -- pour ramasser un objet."), nl,
        write("recharger objet. rec.    -- pour recharger un objet à battrie."), nl,
        write("utiliser objet. u.       -- pour utiliser un objet."), nl,
        write("detoner position*.        -- pour faire détoner une bombe."), nl,
        write("aller direction.         -- pour aller dans cette direction."), nl,
        write("n. s.  e.  o.            -- pour aller dans cette direction (nord / sud / est / ouest)."), nl,
        write("passer.                  -- pour mettre fin à votre tour."), nl,
        write("* les positions doivent être écrit de la manière suivante '(X,Y)'")
        nl.

introduction :-
        nl,
        write("Alors que vous voyagez cryogénisé dans l'espace à bord d'un vaisseau minier, le PR0L0-G, en tant qu'ingénieur, un étrange astéroïde vous percute."), nl,
        write("L'ordinateur de bord est dans un état critique, incapable de redémarer les propulseurs."), nl,
        write("Son protocole le pousse alors à révéiller l'équipage, mais il ne reste que vous."), nl,
        write("Les autres membres cryogénisés dans la zone sont décédés de la collision."), nl,
        write("Chance, le moteur principal n'est pas très loin. Il semblerait également qu'il n'y ai pas de déprésurisation dans la zone."), nl,
        write("Mais êtes-vous hors de danger pour autant ?"), nl,
        nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lancer une nouvelle partie
jouer :-
        instructions, !,
        introduction, !,
        check_salle_visite, !,
        check_objet_obtenu, !,
        decrire_objet(radar),!,
        status.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% descriptions des emplacements
decrire(Pos) :-
        salle(X, L),
        member(Pos, L),
        decrire_salle(X).

decrire_salle(c) :-
        write("La salle de cryogénisation est en pagaille. Les pods autours de vous sont tous brisés ou manquants."), nl,
        write("Il n'y a aucune chance de les réparer. Et même si c'était possible, leurs propriétaires sont sans aucun doute déjà morts."), nl,
        write("Paix à leurs âmes. Vous n'étiez pas particulièrement proche d'eux mais c'étaient tout de même vos collègues..."), nl.

decrire_salle(v) :-
        write("Vous voici dans la salle de vie. Une dizaine de fauteuils 'confortables' fournits par la corporation se trouvent le long des murs."), nl,
        write("Des armoires avec des magazines de la décénie dernière que vous avez tous lus au moins deux fois sont éparpillés au sol, certains sont même déchiquetés."), nl,
        write("Une odeur qui ne vous est pas familière flotte dans l'air, quelque chose cloche..."), nl.

decrire_salle(o) :-
        write("La salle à outils. Un immense laser abrasif occupe le centre de la pièce."), nl,
        write("Impossible d'en faire quoique ce soit, ses circuits semblent avoir été rongés par de l'acide."), nl.

decrire_salle(m) :-
        write("Vous êtes dans la salle à manger."), nl,
        write("Une odeur de putréfaction stagne dans la pièce."), nl,
        write("Vous n'êtes vraiment pas à l'aise ici."), nl.

decrire_salle(i) :-
        write("L'infirmerie. Des caisses vérouillées contiennent tout les médicaments."), nl,
        write("Même si vous pouviez les ouvrir, vos connaissance en médecine sont très limitées."), nl.

decrire_salle(p) :-
        write("Vos pas résonnent dans le couloir principal."), nl,
        write("Vous entendez vos collègues parler dans un coin de la pièce, et..."), nl,
        write("Non, c'est votre imagination."), nl.

decrire_salle(g) :-
        write("Le générateur est encore en route."), nl,
        write("Il aura besoin de combustilbe dans... 2 ans."), nl,
        write("Ne trainez pas trop, il faut encore que vous rejoignez le site d'exploitation minière et que vous raffiniez les matériaux."), nl.

decrire_salle(r) :-
        write("La salle du propulseur principal."), nl,
        write("Vous entendez des bruits de grincement métallique."), nl,
        write("Il devrait y avoir une trappe pour accéder aux mécanismes en bas à droite..."), nl.

decrire_salle(e) :-
        write("Mais... Seul le commandant est censé pouvoir ouvrir cette salle de stockage..."), nl,
        write("Les explosifs qu'elle contient sont particulièrement dangereux."), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Descriptions des monstres

decrire_monstre(chien) :-
        write("Une sorte de chien hideux et enragé s'apprête à vous bondir dessus.").

decrire_monstre(hurleur) :-
        write("Une créature humanoïde drappée d'une muqueuse noire se tient devant vous. Elle s'approche lentement en poussant un cri terrifiant.").

decrire_monstre(rampant) :-
        write("Quelque chose rampe sur le sol, en éméttant quelques gargouillis. Un corps difforme, avec une multitude de bras. Il semble se déplacer sans vraiment faire attention à vous, mais mieux vaut vous en éloigner.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Descriptions des objets

decrire_objet(mine) :-
        write("La mine, simple et efficace, une fois placée elle se déclenchera lorsque qu'une entitée hostile arrive sur la case où elle a été placée."),nl,
        write("Elle inflige 1 point de dégat à toutes les entités présentes sur sa case lors de l'activation."),nl,
        write("Le joueur ne déclenche pas l'explosion de la mine en revanche il peut être pris dans l'explosion."), nl.

decrire_objet(batterie) :-
        write("La batterie, un consommable indispensable, elle permet de recharger les objet nécéssitant de l'énergie comme le radar."), nl.

decrire_objet(bombe) :-
        write("La bombe, un explposif minier, dangereux et puissant, le soufle de l'explosion affectera toute la salle soyez prudent lors de l'utlisation."),nl,
        write("Pour la faire exploser, il faut utiliser la commande 'detoner positionDeLaBombe.' attention à l'écritude de la position, pour plus de détails utiliser la commande 'instructions.'."), nl.

decrire_objet(radar) :-
        write("Le radar est un outils très pratique, il permet de scanner les alentours afin de détecter les menaces."),nl,
        write("Il indique les présences hostiles dans un carré de coté 3 centré sur le joueur."), nl,nl.

decrire_objet(detecteur) :-
        write("Le detecteur, un outils des plus utils, une fois activé il vous avertira lorsqu'une présence arrive sur la case où il se trouve."),nl,
        write("Il possède une autonomie de trois round après quoi il est nécéssaire de le recharger."),nl,
        write("Il est possible de le recharger avant que ses réserves ne soit complétement épuisées cependant l'énergie qu'il lui restait sera perdu."), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fin du jeu

mort() :- nl,
        write("VOUS ÊTES MORT."), nl,
        write("Voilà qui est triste. Ne vous en faites pas, vous pouvez toujours retenter votre chance."), nl,
        write("Ce jeu a volontairement été conçu pour être difficile à battre du premier coup."), nl,
        write("Apprenez de vos erreurs, comprenez le fonctionnement de vos outils et vous finirez par gagner."), nl.

fin() :- nl,
        write("MERCI D'AVOIR JOUÉ !"), nl,
        write("Ce jeu a été réalisé par deux étudiants, entièrement en prolog."), nl,
        write("Vous aurez peut être droit à une suite, si vous laissez une étoile sur le GitHub..."), nl,
        write("https://github.com/UP-4303/PR0L0-G"), nl,
        halt.


gagner() :- nl,
        write("Vous entrez dans la zone de maintenance du réacteur principal."), nl,
        write("Vous fermez la trappe à l'entrée de celle-ci, pour que rien ne puisse venir vous déranger."), nl,
        write("Enfin hors de danger ! ... Pour l'instant..."), nl,
        fin().

perdre(chien) :- nl,
        write("Une créature vous a rattrapé."), nl,
        write("Elle serre votre mollet avec sa gueule jusqu'à l'arracher, vous faisant tomber au sol."), nl,
        write("L'hémoragie vous fait perdre conscience rapidement..."), nl,
        mort(),
        fin().

perdre(hurleur) :- nl,
        write("Un cri strident retentit juste devant vous."), nl,
        write("Vous avez à peine le temps de voir la main du monstre plonger vers vous que tombez, raide mort."), nl,
        mort(),
        fin().

perdre(rampant) :- nl,
        write("Quelque chose a attrapé votre chaussure."), nl,
        write("Vous en perdez l'équilibre."), nl,
        write("En essayant de vous relever, vous voyez la masse terrifiante se dresser, et vous sauter dessus."), nl,
        mort(),
        fin().

perdre(bombe) :- nl,
        write("Vous ne vouliez pas que ces monstres puissent survire, et vous étiez prêt à en payer le prix."), nl,
        mort(),
        finDePartie().

perdre(mine) :- nl,
        write("'BIP BIP BIP'"), nl,
        write("Vous tournez la tête en entendant l'alerte de la mine."), nl,
        write("C'est trop tard."), nl,
        mort(),
        fin().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mort des monstres

mort_monstre(chien) :-
        write("Un cri rauque mélé de douleur suivit d'un bruit sourd retenti."), nl.

mort_monstre(rampant) :-
        write("Un hissement surnaturel, semblant venir de plusieurs bouches différentes se fait entendre."), nl.

mort_monstre(hurleur) :-
        write("Un hurlement guturale terrifiant se répercute dans votre tête suivit d'un silence des plus opressant."), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CHEAT CODES
cc_goto((X,Y)) :-
        retractall(position_courante(_)),
        assert(position_courante((X,Y))), nl,
        write("CHEAT CODE GOTO ! Téléportation effectuée avec succès"), nl.

cc_get_seed(Seed) :-
        random_property(state(Seed)),
        nl, write(Seed), nl, nl.

cc_set_seed(Seed) :-
       set_random(state(Seed)).

cc_give(detecteur, _Quant) :-
        write("malheureusement, du aux limitaion des développeurs, il n'est pas possible d'avoir plusieurs détecteurs :("),!, nl.
cc_give(Item, Quant) :-
        sac(Item, N),
        New_N is N + Quant,
        retract(sac(Item, N)),
        assert(sac(Item, New_N)),!, nl,
        write("CHEAT CODE Give ! Récupération effectuée avec succès"), nl.

cc_give(Item, Quant) :-
        assert(sac(Item, Quant)),!, nl,
        write("CHEAT CODE Give ! Récupération effectuée avec succès"), nl.

cc_set_vie(Pv) :-
        retract(points_de_vie(_)),
        assert(points_de_vie(Pv)),
        write("CHEAT CODE Set Vie ! modification des points de vie effectuée avec succès"), nl.

cc_open(N) :-
        nb_porte(N,X),
        toggle_porte(N,X),
        write("CHEAT CODE open ! ouverture de porte effectuée avec succès"), nl.
