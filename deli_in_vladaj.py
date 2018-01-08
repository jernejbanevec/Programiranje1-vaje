##########################################################################
# Želimo definirati pivotiranje na mestu za tabelo a.
# Ker bi želeli pivotirati zgolj dele tabele a, se hkrati omejimo na
# del tabele, ki se nahaja med indeksoma start in end.
# Na primer, za start = 0 in end = 8 tabelo
#
# [10, 4, 5, 15, 11, 2, 17, 0, 18]
#
# preuredimo v
#
# [0, 2, 5, 4, 10, 11, 17, 15, 18]
#
# (Možnih je več različnih rešitev, pomembno je, da je element 10 pivot.)
#
# Sestavi funkcijo pivot_list(a, start, end), ki preuredi tabelo a tako,
# da bo a[start] postal pivot za del tabele med indeksoma start in end.
# Funkcija naj vrne indeks, na katerem je po preurejanju pristal pivot.
# Funkcija naj deluje v času O(n), kjer je n dolžina tabele a.
# Primer:
#
#     >>> a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
#     >>> pivot_list(a, 1, 7)
#     3
#     >>> a
#     [10, 2, 0, 4, 11, 15, 17, 5, 18]
##########################################################################

#te funk "zamenjaj" pozneje nisem uporabil
def zamenjaj(seznam, a, b):
    t = seznam.index(a)
    index_a = seznam.index(a)
    index_a = seznam.index(b)
    index_b = t
    seznam[index_a] = a
    seznam[index_b] = b
    return seznam

def pivot_list(a, start, end):
    pivot = a[start]
    storeIndex = start + 1
    for i in range (start + 1, end + 1):
        if a[i] < pivot:
            temp = a[i]
            a[i] = a[storeIndex]
            a[storeIndex] = temp
            storeIndex += 1
    
    a[start] = a[storeIndex - 1]
    a[storeIndex - 1] = pivot
    return storeIndex - 1
        

##########################################################################
# Tabelo a želimo urediti z algoritmom hitrega urejanja, ki smo ga
# spoznali na predavanjih.
#
# Napišite funkcijo quicksort(a), ki uredi tabelo a s pomočjo pivotiranja.
# Poskrbi, da algoritem deluje 'na mestu', torej ne uporablja novih tabel.
#
# Namig: definirajte pomožno funkcijo quicksort_part(a, start, end), ki
#        uredi zgolj del tabele a.
#
#   >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#   >>> quicksort(a)
#   [2, 3, 4, 5, 10, 11, 15, 17, 18]
##########################################################################


#PSEVDOKODA
#for each (unsorted) partition
#
#set first element as pivot
#
#  storeIndex = pivotIndex + 1
#
#  for i = pivotIndex + 1 to rightmostIndex
#
#    if element[i] < element[pivot]
#
#      swap(i, storeIndex); storeIndex++
#
#  swap(pivot, storeIndex - 1)

def quicksort_part(a, start, end):
    if start < end:
        meja = pivot_list(a, start, end)
        quicksort_part(a, start, meja - 1)
        quicksort_part(a, meja + 1, end)
    return a
    
def quicksort(a):
    quicksort_part(a, 0, len(a) - 1)
    return a

##########################################################################
# V tabeli a želimo poiskati vrednost k-tega elementa po velikosti.
# Na primer, če je
#
# >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#
# potem je tretji element po velikosti enak 5, ker so od njega manši
# elementi 2, 3 in 4. Pri tem štejemo indekse od 0 naprej, se pravi
# "ničti" element je 2.
#
# Sestavite funkcijo kth_element(a, k), ki v tabeli a poišče k-ti element
# po velikosti. Funkcija sme spremeniti tabelo a.
#
# Namig: ponovno si pomagaj s pomožno funkcijo.
##########################################################################

#easy way

def kth_element1(a, k):
    return quicksort(a)[k]

#harder way

#def kth_element_part(a, start, end, k):
#    meja = pivot_list(a, start, end)
#    if meja == a[k]:
#        return a[k]
#    elif meja > a[k]:
#        kth_element_part(a, start, meja - 1, k)
#    else:
#        kth_element_part(a, meja + 1, end, k - meja - 1)


def kth_element_part(a, start, end, k):
    if start < end:
        meja = pivot_list(a, start, end)
        if k < meja:
            return kth_element_part(a, start, meja-1, k)
        elif k > meja:
            return kth_element_part(a, meja+1, end-1, k - meja - 1)
        else:
            return a[k]
    else:
        return a[end]
    
def kth_element(a, k):
    return kth_element_part(a, 0, len(a) - 1, k)
    
