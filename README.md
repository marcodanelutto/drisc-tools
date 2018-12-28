# drisc-tools
Strumenti per l'esecuzione di programmi assmbler per il corso di Architettura degli elaboratori 

## Prerequisiti
L'intero pacchetto è scritto in `ocaml` e dunque occorre aver installato l'intero ambiete `ocaml`

## Installazione
Nella directory dove avete scompattato i sorgenti eseguire

- `make clean`
- `make`

## PARSER
Il comando 
  -  `parse nomefile`  
  
effettua un controllo delle sintassi di un file con istruzioni e pseuodistruzioni DRISC, una per linea, e segnala eventuali errori. Alcune istruzioni potrebbero non essere riconosciute


## INTERPRETE
Il comando  
  - `drisc nomefile numeropassi`
  
esegue numeropassi istruzioni del file nonmefile. Ad ogni istruzione esegue un dunm dei registri e delle locazioni di memoria dichiarate mediante le pseudo istruzioni

