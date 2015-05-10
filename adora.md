Język Adora
===

Opis ogólny
---

Obiektowy język programowania, imperatywny, statycznie typowany, refleksyjny, z rozbudowanym systemem typów i częściowym wsparciem dla funkcyjnego stylu programowania.

Język jest opracywany przeze mnie na potrzeby tego zadania, i z pewnością w wielu miejscach niedopracowany. Niewykluczone, że jakieś elementy składni będę musiał dodać bądź zmienić w trakcie implementacji, żeby ostatecznie wyszło z tego coś sensownego.
Zależało mi na tym, żeby wcisnąć tu parę

###System typów

Ważnym elementem języka ma być system typów, częściowo oddzielający dziedziczenie implementacji od dziedziczenia interfejsu. Mają się na niego składać klasy (class), podobne do haskellowych, oraz struktury (struct), porządkujące dane i implementujące ich interfejs (określany przez klasy). Klasy i struktury mogą być parametryzowane innymi typami (coś w rodzaju typów generycznych z Javy).

Dostęp do pól (atrybutów) struktur jest chroniony, publicznie dostępne są natomiast własności (properties), mające zdefiniowane gettery i settery (być może tylko jeden z nich), i używane tak jak zwykle atrybuty (wywołanie metody jest niejawne).

Funkcje mają być obiektami pierwszej klasy. Metody jako takie nie, ale metody związane z obiektem mają się dać używać jako funkcje (tzn. `object.foo()` jest równoważne z `(object.foo)()`.

Nazwy klas i struktur zaczynają się wielką literą i mogą się między sobą pokrywać. Nazwa struktury jest wyrażeniem, i może być używana jako funkcja (konstruktor).

###Biblioteka standardowa i typy wbudowane

Niestety, nie zdążyłem jeszcze porządnie przemyśleć biblioteki standardowej i typów wbudowanych, więc mogę podać tylko ogólną listę przewidzianych składników:

- standardowe typy takie jak `Bool`, `Int`, `Char`, `Double`, `String`
- kontenery: listy, tablice, krotki
- zestaw podstawowych klas, takich jak (być może inaczej nazwane):

    + `Object`, bazowa klasa dziedziczona _implicite_ przez wszystkie inne
    + `Show`, `Eq` i `Ord` z Haskella,
    + `Bool`,
    + `Index` (udostępniające składnię indeksu),
    + `Function` udostępniające składnię wywołania funkcji,
    + `Copy`
    + `Type` (refleksja)

- obiekt typu `Type`jest konstruktorems
- wejście/wyjście (co najmniej jakieś podstawowe funkcje do obsługi stdin/stdout)

###Inne szczegóły

- niejawne argumenty:

    + `self` w metodach (w tym setterach i getterach) jest obiektem, na którym metoda została wywołana
    + `value` w setterze jest przypisywaną wartością

- domknięcia

- instrukcja `return` powinna być pomijalna, jeżeli ostatnie wyrażenie w bloku ma odpowiedni typ

- w języku brakuje wyjątków; chciałbym je dodać, jeżeli starczy mi na to czasu, ale prawdopodobnie tak się nie stanie. Dlatego nie umieszczam ich na razie w składni, nie chcąc przeciążać jej nadmiarowymi i nieprzemyślanymi strukturami.

###Nazwa

Nazwa języka pochodzi od imienia jednej z bohaterek Terrego Pratchetta, Adory Dearheart. Została wybrana ze względu na ładne brzmienie, i nie ma w niej żadnego ukrytego sensu.

Przykładowy kod
---

Przykładowy kod znajduje się w pliku `example.ad`.

Składnia
---

Składnia jest oparta o wcięcia i końce linii (w bnfc implementowane przez nawiasy klamrowe i średniki).
Niektóre konstrukcje mogą wyglądać nienaturalnie - głównie włączenie składni nazw typów w sładnię wyrażeń -
ale było to konieczne, żeby zachować jednoznaczność składni bez wprowadzania dodatkowych znaków sterujących.

Plik bnfc to `adora.cf`.
