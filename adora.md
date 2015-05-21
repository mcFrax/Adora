Język Adora
===

Realizacja
---

Rzeczy działające w tym momencie:

- typy wbudowane Int, Bool, Char, funkcje, typy definiowane przez użytkownika
- `if` (z opcjonalnymi `elif` i `else`), `while` z `continue` i `break`
- arytmetyka (`+`, `-`, `*`, `//` (`div`), `%` (`mod`)) (`good/arithmetic.adora`)
- porównania (`==`, `!=`, `<`, `<=`, `>`, `>=`) (`good/arithmetic.adora`)
    Porównania można łączyć w łańcuszki, jak w Pythonie (np. `1 < 2 < 3`).
- funkcje anonimowe (lambda wyrażenia), zagnieżdżane, z domknięciami a'la JavaScript
    (jest to jednyny sposób tworzenia funkcji, które nie są metodami)
- podstawowa obsługa I/O, w postaci instrukcji `print` (`good/print.adora`)
- statyczne sprawdzanie różnych rzeczy (ale na razie nie typów), np. obecności zmiennych. (dużo przykładów w `bad/`).
- "jawna obsługa błędów wykonania", tzn. wymuszone jest wykonywanie wszystkich
    obliczeń na bieżąco, a wyjątki są przechwytywane i wypisywane
    (natomiast nie da się ich przechwycić w programie) (`bad/div0.adora`)
- instrukcja `assert` (`good/assert.adora`, `bad/assert.adora`)

Brakuje kontroli typów, statycznej ani dynamicznej (tzn. żadna wartość nie zostanie źle zinterpretowana, ale w trakcie wykonania
program może się po prostu wysypywać na błędzie `Non-exhaustive patterns ...`).

Starałem się, żeby większość ficzerów miała odpowiadający sobie przykład w `good/` lub `bad/` (w przypadku różnych sprawdzeń).

###Nazwa

Nazwa języka pochodzi od imienia jednej z bohaterek Terrego Pratchetta, Adory Dearheart. Została wybrana ze względu na ładne brzmienie, i nie ma w niej żadnego ukrytego sensu.

Składnia
---

Składnia jest oparta o wcięcia i końce linii (w bnfc implementowane przez nawiasy klamrowe i średniki).
Niektóre konstrukcje mogą wyglądać nienaturalnie - głównie włączenie składni nazw typów w sładnię wyrażeń -
ale było to konieczne, żeby zachować jednoznaczność gramatyki bez komplikowania składni.

Plik bnfc to `adora.cf`.
