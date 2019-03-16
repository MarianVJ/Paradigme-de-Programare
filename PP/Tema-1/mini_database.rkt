
(define NULL 'null)

;====================================
;=            Cerința 1             =
;= Definirea elementelor de control =
;=          20 de puncte            =
;====================================

;= Funcțiile de acces
(define init-database
  (λ ()   '() ))

(define create-table
  (λ (table columns-name)
   (list (cons table columns-name))
    ))

(define get-name
  (λ (table)
    (car (car table))))

(define get-columns
  (λ (table)
    (cdr (car table))
    ))

(define get-tables
  (λ (db)
    db))

(define get-table
  (λ (db table-name)
   (foldl (λ (tab acc)( if(equal? (car (car tab)) table-name) tab acc)) '() db) 
    ))

(define add-table
  (λ (db table)
    (cons table db) ))

;iau fiecare tabela si o adaug in acumulator (cu exccceptia celei cu numele de "table-name"
(define remove-table
  (λ (db table-name)
   (foldr (lambda(tabela baza)(if (equal? (car (car tabela)) table-name) baza (cons tabela baza))) (init-database) db)
    ))

;= Pentru testare, va trebui să definiți o bază de date (având identificatorul db) cu următoarele tabele

;============================================================================================
;=                         Tabela Studenți                                                   =
;= +----------------+-----------+---------+-------+-------+                                  =
;= | Număr matricol |   Nume    | Prenume | Grupă | Medie |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;= |            123 | Ionescu   | Gigel   | 321CA |  9.82 |                                  =
;= |            124 | Popescu   | Maria   | 321CB |  9.91 |                                  =
;= |            125 | Popa      | Ionel   | 321CC |  9.99 |                                  =
;= |            126 | Georgescu | Ioana   | 321CD |  9.87 |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;=                                                                                           =
;=                                         Tabela Cursuri                                    =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | Anul | Semestru |            Disciplină             | Număr credite | Număr teme |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | I    | I        | Programarea calculatoarelor       |             5 |          2 |      =
;= | II   | II       | Paradigme de programare           |             6 |          3 |      =
;= | III  | I        | Algoritmi paraleli și distribuiți |             5 |          3 |      =
;= | IV   | I        | Inteligență artificială           |             6 |          3 |      =
;= | I    | II       | Structuri de date                 |             5 |          3 |      =
;= | III  | II       | Baze de date                      |             5 |          0 |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;============================================================================================
(define db '(  (("Studenți" "Număr matricol" "Nume"  "Prenume" "Grupă" "Medie") (123 "Ionescu" "Gigel" "321CA" 9.82)(124 "Popescu" "Maria" "321CB" 9.91)
                                                                                 (125 "Popa" "Ionel" "321CC" 9.99) (126 "Georgescu" "Ioana" "321CD" 9.87) )
               (("Cursuri" "Anul" "Semestru"  "Disciplină" "Număr credite" "Număr teme" ) ("I" "I" "Programarea calculatoarelor" 5 2)
                                                                                          ("II" "II" "Paradigme de programare" 6 3)
                                                                                          ("III" "I" "Algoritmi paraleli și distribuiți" 5 3)
                                                                                          ("IV" "I" "Inteligență artificială" 6 3 )
                                                                                          ("I" "II" "Structuri de date" 5 3)
                                                                                          ("III" "II" "Baze de date" 5 0))
                                                                                           ))

;====================================
;=            Cerința 2             =
;=         Operația insert          =
;=            10 puncte             =
;====================================

;;Iar din record valoarea asociata unui coloane
(define (valoare-asoc coloana-nume record)
(car (foldl  (lambda(x acc) (if (equal? (car x) coloana-nume) (cons (cdr x) acc) acc)) '() record)))

;;Adaug linia la finalul tabelei noua linie de inserat 
(define (insert-line table record)
(let ((coloane (foldl (lambda ( x acc) (cons (car x) acc)) '() record)) (coloane-totale (get-columns table)))
  (append table (list (map (lambda (x) (if (member x coloane) (valoare-asoc x record) NULL)) coloane-totale)))
  ))

;Iterez prin fiecare tabela si cand gasesc , tabela in care trebuie inserata linie , apelez functia auxiliara de mai sus
(define insert
  (λ (db table-name record)
    (map (lambda(table)(if(equal? (get-name table) table-name) (insert-line table record) table)) db)
    ))

;====================================
;=            Cerința 3 a)          =
;=     Operația simple-select       =
;=             10 puncte            =
;====================================

;Daca nu are nicio linie in tabela , atunci returnez lista vida altfel iterez prin fiecare coloana din columns , si intorc valorile
;corespunzatoare coloanei de pe fiecare linie ( daca pe poz coresp unei coloane se afla NULL nu o mai adaug in rezultat)
(define simple-select
  (λ (db table-name columns)
   (let ((table (get-table db table-name)))
     (if(>  (length table) 1)
     (map  (lambda (x)
               (let ((index (index-of (get-columns table) x)))
                 (foldr (lambda(x acc) (if(equal? x NULL) acc (cons (list-ref x index) acc))) '() (cdr table)))) columns)
     '()
     ))))

;====================================
;=            Cerința 3 b)          =
;=           Operația select        =
;=            30 de puncte          =
;====================================

;Aceasta functie primeste o linie si returneaza valorea corespunzatoare pentru coloana col din acea linie
(define (get-poz lin col col-tot )
(list-ref lin ( index-of col col-tot)))

;Aceasta functie testeaza daca sunt indeplinie de linia 'lin' toate conditiile din conditions si returneaza true altfel returneaza false
;(inainte testez daca linia 'lin' in care componenta sa NULL
(define (all-conditions lin col conditions)
  (if (member NULL lin)
  #f
  (foldr (lambda(x acc) ( if(equal? (get-poz lin col (cadr x)) NULL)#f (and acc ((car x) (get-poz lin col (cadr x)) (caddr x))))) #t conditions)))

;Aceasta functie realizeaza toate restrictiile , si returneaza tabela modificata ( pastreaza liniile care indeplinesc taote conditiile din contitions)
(define (verif-cond table conditions)
  (let ((coloane (get-columns table)) (linii (cdr table)))
    ( foldr (lambda(x acc) (if (all-conditions x coloane conditions) (cons x acc) acc))   '()  linii)))


;;Dupa ce obtin tabela , cu liniile care indeplinesc toate conditiile , iau fiecare 'membru' din columns si apelez functie simple select pentru
;;a extrage coloana corespunzatoare ( daca nu este pereche  ( adica daca nu are operator)) altfel , verific ce operator este si aplic operatia pe
;;coloana obtinuta cu simple-select

(define select
  (λ (db table-name columns conditions)
    (let* ( (antet (car (get-table db table-name)))  (tabela-res (cons (cons antet (verif-cond (get-table db table-name) conditions)) '()) ) )
      (map (lambda(x)
             (if (not (pair? x))
             (car (simple-select tabela-res (car antet) (list x)))
              ( let* ((op (car x))(col-selected (car (simple-select tabela-res (car antet) (list (cdr x))))))(
                       cond
                      ((equal? op 'min)(foldl min (car col-selected) (cdr col-selected)))
                      ((equal? op 'max)(foldl max (car col-selected) (cdr col-selected)))
                      ((equal? op 'count) (car (foldl  (lambda ( x acc)(if (member x (cdr acc)) acc (cons (+ (car acc) 1) (cons x (cdr acc)))))'(0) col-selected)))
                      ((equal? op 'sum) (foldl + 0 col-selected))
                      ((equal? op 'avg) (/ (foldl + 0 col-selected) (length col-selected)))
                      ((equal? op 'sort-asc) (sort col-selected <))
                      (else (sort col-selected > ))
                     ))))  columns))))




;====================================
;=             Cerința 4            =
;=           Operația update        =
;=            20 de puncte          =
;====================================

;Functia primeste linia ( numele coloanelor din tabela liniei) si valorile 'values' ce trebuie updatate
;Iterez prin fiecare pozitie din linie , daca acea coloana se afla in values ( adica trebuie updatata ) , iau
;valoarea corespunzatoare si o inlocuiesc 
(define (line-update linie coloane values)
  (foldl  (lambda (elem acc) (if (index-of coloane (car elem))               
                (let* ((nume-col (car elem)) (val-col (cdr elem)) (poz-col (+ 1 (index-of coloane nume-col)))
                                             (first-part (take acc poz-col)) (last-part (drop acc poz-col)))
                 (append (take first-part (- poz-col 1)) (list val-col) last-part))
                 acc ))
          linie values))

;Functia primeste tabela de modificat valorile ce trebuiesc reintroduse , verific fiecare linie daca ideplineste toate conditiile
;Daca da apelez functia line-update si introduc 
(define (update-my-table table values conditions)(
 let ((coloane (cdr(car table))) (linii (cdr table)))
 (cons (car table) (map (lambda(x) (if (all-conditions x coloane conditions) (line-update x coloane values)  x)) linii))))

;;Caut tabela ce trebuie updatata si cand o gasesc apelez functia auxiliara de mai sus
(define update
  (λ (db table-name values conditions)
    (map (lambda(x) (if (equal? table-name (get-name x)) (update-my-table x values conditions) x))  db)))

;====================================
;=             Cerința 5            =
;=           Operația remove        =
;=              10 puncte           =
;====================================

;IAu linie cu linie , daca linai indeplineste toate conditiile atunci o sterg , altfel nu 
(define (delete-from-table table conditions )
(if (null? conditions)
    (cons (car table) '())
    (let ((coloane (cdr(car table))) (linii (cdr table)))
      (cons (car table) (foldr (lambda(x acc) (if (all-conditions x coloane conditions) acc  (cons x acc))) '() linii)))))

;Sterg liniile din tablea cu numele 'table-name'  cu ajutorul functiei de mai sus
(define delete
  (λ (db table-name conditions)
    (map (lambda(x) (if (equal? table-name (get-name x)) (delete-from-table x conditions) x))  db) ))

;====================================
;=               Bonus              =
;=            Natural Join          =
;=            20 de puncte          =
;====================================




;;Aceasta functie aplica toate conditiile din conditions asupra unei tabele si returneaza tabela fara elementele ce nu indeplinesc conditiile 
(define (aplica-cond table conditions)
  (let ((coloane (get-columns table)) (linii (cdr table)))
    (cons (car table) (
     foldr (lambda(x acc) (if (all-conditions x coloane conditions) (cons x acc) acc))   '()  linii))))

;Aceasta functie primeste doua liste cu nume de coloane , si returneaza coloana comuna din cele 2 liste
(define (coloana-comuna col1 col2)
  (foldl (lambda(x acc) (if (member x col2) x acc)) '() col1))


;Aceasta functie primeste tabela ( care are numarul de intrari mai putine decat coloana maxima) si creeaza o coloana corespunzatoare noii tabele
; pe care se face natural join 
(define (auxiliar-func second-table num-col maxim-col com-col)
  (let* ((index-col (index-of (get-columns second-table) num-col)) (index-val (index-of (get-columns second-table) com-col))(linii (cdr second-table)))
   (map  (lambda(x) (foldr  (lambda(lin acc) (if (member x lin) (list-ref lin index-col ) acc)) 0 linii))  maxim-col)))

;Aceasta functie primeste 2 coloane si pastraza in col1 doar elementele care se afla si in col2
(define (intersect-colons col1 col2)
  (foldr   (lambda(x acc) (if (member x col2) (cons x acc) acc)) '()  col1))



;Aceasta functie pentru inceput extrage informatiile necesare din tabela ( cele 2 tabele pe care se face join)
;Dupa care calculeaza numele coloanei comune
;Si apoi prima-col reprezinta valorile coloanei comune pt tabela 1 ( si doi-col pentru tabela2)
;maxim-col reprezinta coloana maxima ( am calculat-o pentru a o folosi in program
;Functia returneaza coloanele din noua taabela creeata ( doar coloanele din columns) : Daca este un nume de coloana , din tabela
;care are mai dimensiuni mai mari atunci pursi simplu vom extrage cu select acea coloana , altfel
;se apeleaza functia auxiliar-func 
(define natural-join
  (λ (db tables columns conditions)
    (
      let*  ((tabel1 (aplica-cond (get-table db (car tables)) conditions))
             (tabel2  (aplica-cond (get-table db (last tables)) conditions))
             (com-col (coloana-comuna (get-columns tabel1) (get-columns tabel2)))
             (prima-col-aux (car (select (list tabel1) (get-name tabel1) (list com-col) '())))
             (doi-col (intersect-colons ( car (select (list tabel2) (get-name tabel2) (list com-col) '()))  prima-col-aux))
             (prima-col (intersect-colons prima-col-aux doi-col))
             (maxim-col (if (>= (length prima-col) (length doi-col)) prima-col doi-col))
             (maxim-table (if (>= (length prima-col) (length doi-col)) tabel1 tabel2))
             (second-table (if (>= (length prima-col) (length doi-col)) tabel2 tabel1)))
       ( map (lambda(x) ( cond
                           ((equal? x com-col)  maxim-col)
                           ((member x  (get-columns maxim-table)) (car (select (list maxim-table) (get-name maxim-table) (list x) '())))
                           (else (auxiliar-func second-table x  maxim-col  com-col ))))
                            columns ))))
