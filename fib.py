from functools import lru_cache

# Cilj: izračunajte vrednosti Fibonaccijevega zaporadja za 100, 500, 1000,
# 10**5, and 10**6 člen.
# Za vsako definicijo preizkusite kako pozne člene lahko izračuante in poglejte
# zakaj se pojavi problem (neučinkovitost, pregloboka rekurzija,
# premalo spomina ...).

# Definirajte naivno rekurzivno različico.
# Omejitev: Prepočasno.
def fib(n):
    if n in [0,1]:
        return n
    else:
        return fib(n-1) + fib(n-2)

# Z uporabo dekoratorja izboljšajte naivno različico.
# Omejitev: Preseže največjo dovoljeno globino rekurzija za ~350.

@lru_cache(maxsize=None)
def fib_cache(n):
    #print(n) samo da vidim katare mi gre ponovno računat
    if n in [0,1]:
        return n
    else:
        return fib_cache(n-1) + fib_cache(n-2)

# Nariši drevo klicov za navadno rekurzivno fib funkcijo pri n=5 in
# ugotovi kateri podproblemi so klicani večkrat.

# Definirajte rekurzivno memoizirano funkcijo fib brez uporabe dekoratorja.
# Omejitev: Preseže največjo dovoljeno globino rekurzija za ~1000.

rezultati = {}
def fib_memo_rec(n):
    if n in [0,1]:
        return n
    if n not in rezultati.keys():
        y = fib_memo_rec(n-1) + fib_memo_rec(n-2)
        rezultati[n] = y
        return y
    else:
        return rezultati[n]

# Na katere podprobleme se direktno skicuje rekurzivna definicija fib?

# Definirajte fib ki gradi rezultat od spodaj navzgor (torej računa in si zapomni
# vrednosti od 1 proti n.)

def fib_memo_iter(n):
    rezultati = {}
    rezultati[0] = 0
    rezultati[1] = 1
    for i in range(2, n+1):
        rezultati[i] = rezultati[i-1] + rezultati[i-2]
    return rezultati[n]

# Izboljšajte prejšnjo različico tako, da hrani zgolj rezultate, ki jih v
# nadaljevanju nujno potrebuje.

# MOJA OPCIJA
#def fib_iter(n):
#    rezultati = {}
#    rezultati[0] = 0
#    rezultati[1] = 1
#    for i in range(2, n+1):
#        rezultati[i] = rezultati[i-1] + rezultati[i-2]
#        rezultati.pop(i-2, None)
#    #print(rezultati)
#    return rezultati[n]

def fib_iter(n):
    if n in [0,1]:
        return n
    else:
        x_0 = 0
        x_1 = 1
        for i in range(2, n+1):
            x = x_0 + x_1
            x_0 = x_1
            x_1 = x
    return x_1
