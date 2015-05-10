Język Adora
===

Realizacja
---

Wersja którą przesyłam w pierwszym terminie jest b. mocno okrojona, ale ma duży potencjał do rozwoju. Rzeczy działające w tym momencie:

- 3 mniej lub bardziej używalne typy (int, bool i funkcje)
- zmienne z przypisaniem
- instrukcja `if` (z `elif` i `else`)
- pętla `while` z `continue` i `break`
- podstawowa arytmetyka (`+`, `-`, `*`, `//` (`div`), `%` (`mod`)) i porównania (`==`, `!=`)
- funkcje anonimowe (lambda wyrażenia), zagnieżdżane, z domknięciami a'la JavaScript
    (jest to jednyny sposób tworzenia funkcji)

Brakuje obsługi I/O, zamiast tego każda instrukcja będąca wyrażeniem wypisuje swoją wartość (po wyliczeniu).
Nie ma również kontroli typów, statycznej ani dynamicznej (tzn. żadna wartość nie zostanie źle zinterpretowana, ale w trakcie wykonania
program może się po prostu wysypywać na błędzie `Non-exhaustive patterns ...`).

Wydaje mi się, że wychodzi z tego 12 punktów (na 16 brakuje I/O), i na ten moment jest to dla mnie całkowicie wystarczające.
Przed drugim terminem oddawania mam zamiar wysłać wersję znacznie bogatszą, na pełne 24 punkty.


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

###Nazwa

Nazwa języka pochodzi od imienia jednej z bohaterek Terrego Pratchetta, Adory Dearheart. Została wybrana ze względu na ładne brzmienie, i nie ma w niej żadnego ukrytego sensu.

Składnia
---

Składnia jest oparta o wcięcia i końce linii (w bnfc implementowane przez nawiasy klamrowe i średniki).
Niektóre konstrukcje mogą wyglądać nienaturalnie - głównie włączenie składni nazw typów w sładnię wyrażeń -
ale było to konieczne, żeby zachować jednoznaczność składni bez wprowadzania dodatkowych znaków sterujących.

Plik bnfc to `adora.cf`.
