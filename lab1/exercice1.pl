choix(hors_d_oeuvre, salade, 1).
choix(hors_d_oeuvre, pate, 6).
choix(poisson, sole, 2).
choix(poisson, thon, 4).
choix(viande, porc, 7).
choix(viande, boeuf, 3).
choix(dessert, glace, 5).
choix(dessert, fruit, 1).

plat(poisson, Contenu, Points) :-
    choix(poisson, Contenu, Points).
plat(viande, Contenu, Points) :-
    choix(viande, Contenu, Points).

repas(H, P, D) :-
    choix(hors_d_oeuvre, H, _),
    choix(dessert, D, _),
    plat(_, P, _).

repasLeger(H, P, D) :-
    choix(hors_d_oeuvre, H, H_points),
    choix(dessert, D, D_points),
    plat(_, P, P_points),
    (H_points + D_points + P_points) < 10.
