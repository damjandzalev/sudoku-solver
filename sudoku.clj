"Damjan Dzalev 171204, Domasna rabota 5 sudoku"
"vizuelizacija - prva tabela input, vtora rezultat po izvrsuvanje na solve, povik (vizuelizacija)"
(defn atomic? "domasna4-grupa1-a"
  [v]
	(not (coll? v))
)

(defn member? "domasna4-grupa1-b"
  [x lst]
	(cond
		(empty? lst)
			false
		(distinct? x (first lst))
			(member? x (rest lst))
		:else
			true
	)
)

(defn append "domasna4-grupa1-g"
  [lst1 lst2]
	(cond
		(not (empty? lst1))
			(cons (first lst1) (append (rest lst1) lst2))
		:else
			lst2
	)
)

(defn my-flatten "domasna4-grupa1-k" [list-of-lists]
	(cond
    (empty? list-of-lists)
      '()
    :else
      (append (first list-of-lists) (my-flatten (rest list-of-lists)))
	)
)

(defn my-reduce "domasna4-grupa3-v"
  ([f value? lst]
      (if
        (not (empty?  lst))
          (my-reduce f (f value? (first lst)) (rest lst))
          value?
      )
  )
  ([f lst]
        (reduce f (first lst) (rest lst))
  )
)

(defn append2
  "append, no za mnozestva i vektori, napraveno so conj,Tail rekurzija,
  Na kraj se dobiva rezultantnata lista vo lst1. Vo sekoj rekurziven povik
  prviot element se dodava vo lst1 so conj. Koga ke se isprazni lst2
  se vraka lst1"
  [lst1 lst2]
	(cond
		(not (empty? lst2))
			(append2 (conj lst1 (first lst2) ) (rest lst2))
		:else
			lst1
	)
)

(defn matrix-row "
  Ja vraka (x+1)-tata redica od sudoku problemot."
  [x matrix]
  (nth matrix x)
)

(defn matrix-column "
  Pravi vektor so vrednostite od (x+1)-tata kolona. Se dodeka ima redici vo matrix
  gi dodava elementite kon rezultantniot vektor zadrzuvajki go nivniot redosled.
  Koga ke se isprazni matricata se vraka prazen vektor za da mozi append2 funkcijata
  da se izizvrsuva."
  [x matrix]
  (if (empty? matrix)
    []
    (append2 [(nth (first matrix) x)] (matrix-column x (rest matrix)))
  )
)

(defn matrix-box
  "x i y se koordinati na elementot koj sto se razgleduva. Ovaa funkcija ja vraka kockata
  vo koja se naoga elementot vo sudoku problemot. Spored koordinatite funkcijata pravi vektor
  od elementite na kockata. Ova go pravi so pomos na matrix-column i matrix-row. So matrix-row
  se zemaat trite redici kade shto se naoga kockata koja se bara i pravi matrica so tri redici i
  so matrix-column se zemaat trite koloni od kockata i trite vektori od kolonite potoa se
  spojuvaat so reduce append2 vo eden vektor. "
  [x y matrix]
  (cond
    (and (<= x 2) (<= y 2))
      (reduce append2 [
        (matrix-column 0 (reduce append2 [[(nth matrix 0)] [(nth matrix 1)] [(nth matrix 2)]]) )
        (matrix-column 1 (reduce append2 [[(nth matrix 0)] [(nth matrix 1)] [(nth matrix 2)]]) )
        (matrix-column 2 (reduce append2 [[(nth matrix 0)] [(nth matrix 1)] [(nth matrix 2)]]) )]
      )
    (and (<= x 2) (<= y 5))
      (reduce append2 [
        (matrix-column 3 (reduce append2 [[(nth matrix 0)] [(nth matrix 1)] [(nth matrix 2)]]) )
        (matrix-column 4 (reduce append2 [[(nth matrix 0)] [(nth matrix 1)] [(nth matrix 2)]]) )
        (matrix-column 5 (reduce append2 [[(nth matrix 0)] [(nth matrix 1)] [(nth matrix 2)]]) )]
      )
    (and (<= x 2) (<= y 8))
      (reduce append2 [
        (matrix-column 6 (reduce append2 [[(nth matrix 0)] [(nth matrix 1)] [(nth matrix 2)]]) )
        (matrix-column 7 (reduce append2 [[(nth matrix 0)] [(nth matrix 1)] [(nth matrix 2)]]) )
        (matrix-column 8 (reduce append2 [[(nth matrix 0)] [(nth matrix 1)] [(nth matrix 2)]]) )]
      )
    (and (<= x 5) (<= y 2))
      (reduce append2 [
        (matrix-column 0 (reduce append2 [[(nth matrix 3)] [(nth matrix 4)] [(nth matrix 5)]]) )
        (matrix-column 1 (reduce append2 [[(nth matrix 3)] [(nth matrix 4)] [(nth matrix 5)]]) )
        (matrix-column 2 (reduce append2 [[(nth matrix 3)] [(nth matrix 4)] [(nth matrix 5)]]) )]
       )
    (and (<= x 5) (<= y 5))
      (reduce append2 [
        (matrix-column 3 (reduce append2 [[(nth matrix 3)] [(nth matrix 4)] [(nth matrix 5)]]) )
        (matrix-column 4 (reduce append2 [[(nth matrix 3)] [(nth matrix 4)] [(nth matrix 5)]]) )
        (matrix-column 5 (reduce append2 [[(nth matrix 3)] [(nth matrix 4)] [(nth matrix 5)]]) )]
      )
    (and (<= x 5) (<= y 8))
      (reduce append2 [
        (matrix-column 6 (reduce append2 [[(nth matrix 3)] [(nth matrix 4)] [(nth matrix 5)]]) )
        (matrix-column 7 (reduce append2 [[(nth matrix 3)] [(nth matrix 4)] [(nth matrix 5)]]) )
        (matrix-column 8 (reduce append2 [[(nth matrix 3)] [(nth matrix 4)] [(nth matrix 5)]]) )]
      )
    (and (<= x 8) (<= y 2))
      (reduce append2 [
        (matrix-column 0 (reduce append2 [[(nth matrix 6)] [(nth matrix 7)] [(nth matrix 8)]]) )
        (matrix-column 1 (reduce append2 [[(nth matrix 6)] [(nth matrix 7)] [(nth matrix 8)]]) )
        (matrix-column 2 (reduce append2 [[(nth matrix 6)] [(nth matrix 7)] [(nth matrix 8)]]) )]
      )
    (and (<= x 8) (<= y 5))
      (reduce append2 [
        (matrix-column 3 (reduce append2 [[(nth matrix 6)] [(nth matrix 7)] [(nth matrix 8)]]) )
        (matrix-column 4 (reduce append2 [[(nth matrix 6)] [(nth matrix 7)] [(nth matrix 8)]]) )
        (matrix-column 5 (reduce append2 [[(nth matrix 6)] [(nth matrix 7)] [(nth matrix 8)]]) )]
      )
    :else
      (reduce append2 [
        (matrix-column 6 (reduce append2 [[(nth matrix 6)] [(nth matrix 7)] [(nth matrix 8)]]) )
        (matrix-column 7 (reduce append2 [[(nth matrix 6)] [(nth matrix 7)] [(nth matrix 8)]]) )
        (matrix-column 8 (reduce append2 [[(nth matrix 6)] [(nth matrix 7)] [(nth matrix 8)]]) )]
      )
  )
)

(defn transform-row
  "Rezultantnata redica ja pravi so dodavanje na novite elementi nanazad.
  Ako ima elementi vo row togas povtorno se povikuva funkcijata i koga ke se
  isprazni vraka prazen vektor. Vrakajki se nanazad vektor od prviot element
  i rezultantniot vektor od naredniot povik se spojuvaat so append2 vo eden
  vektor i se vrakaat kako rezultat."
  [row]
  (cond
    (empty? row)
      []
    (or (= (first row) 0) (= (first row) ""))
      (append2 [#{1 2 3 4 5 6 7 8 9}] (transform-row (rest row) )  )
    :else
      (append2 [(set [(first row)])]  (transform-row (rest row) ) )
  )
)

(defn transform "
  Rezultantnata matrica ja pravi so dodavanje na novite redici nanazad.
  Ako ima redici vo matricata togas povtorno se povikuva funkcijata i koga ke
  ostani edna redica go vraka rezultatot od transformacijata na taa redica.
  Vrakajki se nanazad vektor od prvata redica i rezultantniot vektor od naredniot
  povik se spojuvaat so append2 vo eden vektor i se vrakaat kako rezultat."
  [matrix]
  (if
    (empty? (rest matrix))
      [(transform-row (first matrix))]
      (append2 [(transform-row (first matrix))] (transform (rest matrix) ) )
  )
)

(defn distinct-element-sudoku
  "Ovaa funkcija zema mnozestvo od mozni vrednosti za nekoj element i vektor od
  elementite na redicata kolonata i kockata vo koj se naoga elementot. So niv pravi
  novo mnozestvo od mozni vrednosti za elementot i go vraka kako rezultat. Mnozestvoto
  go pravi so pomos na append2. Go izminuva pocetnoto mnozestvo i ako nekoj element od
  mnozestvoto pripaga vo vektorot togas ne go dodava kon rezultantnoto mnozestvo."
  [mnozestvo vektor]
  (cond
    (empty? mnozestvo)
      #{}
    (not (member? (first mnozestvo) vektor))
      (append2 #{(first mnozestvo)} (distinct-element-sudoku (set (rest mnozestvo)) vektor))
    :else
      (distinct-element-sudoku (set (rest mnozestvo)) vektor)
  )
)

(defn distinct-row-sudoku "
  Ovaa funkcija zema redica, matricata vo koja se naoga redicata i na koj red se naoga - x
  i pravi nova redica. Kon novata redica ako elementot e atomicen go dodava direktno kon
  rezultantnata redica, a ako e mnozestvo pravi novo mnozestvo so distinct-element-sudoku
  i go dodava novoto mnozestvo kon rezultantnata redica. Vektorot od koj se proveruva dali
  nekoj element mozi da bidi vrednost na dadenata pozicija se pravi so reduce append2
  i vektorite koi se dobivaat so matrix-row, matrix-column i matrix-box.
  Dodavanjeto na elementi se izvrsuva nanazad."
  [row matrix x]
  (if (atomic? row)
    []
    (cond
      (empty? row)
        []
      (atomic? (first row))
        (append2 (vector (first row) ) (distinct-row-sudoku (rest row)  matrix x))
      :else
        (append2 (vector (distinct-element-sudoku (first row)
            (reduce append2 [
            (matrix-row x matrix)
            (matrix-column (- 9 (count row) ) matrix )
            (matrix-box x (- 9 (count row) ) matrix ) ] ) ) )
          (distinct-row-sudoku (rest row) matrix x) )
    )
  )
)

(defn distinct-sudoku "
  Prima matrica i vraka kako rezultat matrica. Novata matrica ja pravi so promenuvanje na sekoj red
  vo distinct-row-sudoku i negovo dodavanje kon rezultantnata matrica. Koga ke se pominat site redovi
  od matricata funkcijata vraka prazen vektor za da mozi da se izizvrsuva append2. Dodavanjeto na redovi
  se izvrsuva nanazad."
  [matrix1 matrix2]
    (cond
      (empty? matrix1)
        []
      :else
        (append2 [(distinct-row-sudoku (first matrix1) matrix2 (- 9 (count matrix1)))] (distinct-sudoku (rest matrix1) matrix2))
    )
)

(defn remove-first [x row]
  "Go brisi prvoto pojavuvanje na x vo row. Ako prviot element od row go dodava kon rezultantnata
  redica od rekurzivniot povik. Ako go najdi x vo row, togas kako rezultat se vraka ostatokot od row
  bez x. Ako voopsto nema pojavuvanje na x i row se isprazni se vraka prazen vektor. Dodavanjeto se
  izvrsuva nanazad."
  (cond
    (empty? row)
      []
    (= x (first row))
      (rest row)
    :else
      (cons (first row ) (remove-first x (rest row)))
    )
)

(defn distinct-sets-row-sudoku "
  Zema redica, matrica i koj br.redica e vo matricata i vraka rezultat redica.
  Ovaa funkcija za sekoj element sto e mnozestvo, proveruva dali postoi edna vrednost koja ne se pojavuva
  vo redicata, kolonata ili kockata, vo drugite elementi. Go zema mnozestvoto vozmozni vrednosti(ako ne e atomicen)
  i pravi vektor od site mnozestva vo redicata, kolonata ili kockata (proveruva za site posebno) i gi izramnuva
  pritoa ne go zema mnozestvoto kade sto se naoga elementot koj se proveruva. So pomos na distinct-element-sudoku
  se proveruva dali postoi takva unikatna vrednost. Ako ima unikatna ja dodava nea, ako ne go dodava mnozestvoto
  kon novata redica."
  [row matrix x]
  (if (atomic? row)
    []
    (cond
      (empty? row)
        []
      (atomic? (first row))
        (append2 [(first row)] (distinct-sets-row-sudoku (rest row)  matrix x))
      :else
      (cond
        (= (count (distinct-element-sudoku (first row) (my-flatten (filter coll? (remove-first (first row) (matrix-row x matrix)))) ) ) 1)
          (append2 [(first (distinct-element-sudoku (first row) (my-flatten (filter coll? (remove-first (first row) (matrix-row x matrix))))) )] (distinct-sets-row-sudoku (rest row)  matrix x))
        (= (count (distinct-element-sudoku (first row) (my-flatten (filter coll? (remove-first (first row ) (matrix-column (- 9 (count row) ) matrix )))) ) ) 1)
          (append2 [(first (distinct-element-sudoku (first row) (my-flatten (filter coll? (remove-first (first row ) (matrix-column (- 9 (count row) ) matrix )))) ) )] (distinct-sets-row-sudoku (rest row)  matrix x))
        (= (count (distinct-element-sudoku (first row) (my-flatten (filter coll? (remove-first (first row )  (matrix-box x (- 9 (count row) ) matrix ) ) ) ) ) ) 1)
          (append2 [(first (distinct-element-sudoku (first row) (my-flatten (filter coll? (remove-first (first row )  (matrix-box x (- 9 (count row) ) matrix ) ) ) ) ) )] (distinct-sets-row-sudoku (rest row)  matrix x))
        :else
          (append2 [(first row)] (distinct-sets-row-sudoku (rest row)  matrix x))
      )
    )
  )
)

(defn distinct-sets-sudoku "
  Funkcionira kako distinct-sudoku, no za mnozestvata proveruva dali vo niv postoi unikatna vrednost koja ja
  nema vo drugite mnozestva."
  [matrix1 matrix2]
  (cond
    (empty? matrix1)
      []
    :else
      (append2 [(distinct-sets-row-sudoku (first matrix1) matrix2 (- 9 (count matrix1)))] (distinct-sets-sudoku (rest matrix1) matrix2))
  )
)

(defn to-atomic "
  Zema matrica i za rezultat vraka matrica. Ja vraka istata matrica, no mnozestvata so eden element gi pretvora
  vo atomicni elementi, vrednosta vo mnozestvoto. Dodavanjeto na redici i elementi se izvrsuva nanazad i se pravi so
  append2.
  "
  ([matrix]
     (if (empty? matrix)
        []
        (append2 [(to-atomic (first matrix) [])] (to-atomic (rest matrix)))) )
  ([row row2]
    (if (empty? row)
      []
      (if (atomic? (first row) )
        (append2 [(first row) ] (to-atomic (rest row) row2) )
        (if (= (count (first row) ) 1)
          (append2 [(first ( first row) ) ] (to-atomic (rest row) row2) )
          (append2 [( first row) ] (to-atomic (rest row) row2) ) )
      )
    )
  )
)

(defn get-mnozestva "
  Vo rezultatot se dobivaat site mnozestva od vozmozni vrednosti na site elementi od matricata.
  Se izminuvaat site redici i so filter coll? se dobivaat site mnozestva od redicata i se dodavaat kon
  rezultantnata lista. Dodavanjeto na novite mnozestva se izvrsuva nanazad.
  "
  [matrix]
  (if (empty? matrix)
    ()
    (conj (get-mnozestva (rest matrix)) (filter coll? (first matrix)))
  )
)

(defn create-new-row-matrix "
  Pravi nova redica so toa sto prvoto mnozestvo go zamenuva so vrednosta curr. Koga ke se zameni prekinuva
  rekurzijata. Dodavanjeto se pravi so append2 i se izvrsuva nanazad."
  [row curr]
  (if (coll? (first row))
      (append2 curr (rest row))
      (append2 [(first row)] (create-new-row-matrix (rest row) curr))
  )
)

(defn create-new-matrix "
  Pravi nova matrica od vleznata. Prvoto mnozestvo go zamenuva so curr. Ako vo redicata ima mnozestvo
  ja menuva so create-new-matrix. Koga ke ja smeni zavrsuva rekurzijata i kon rezultatot se dodava ostanatiot
  del od matricata."
  [matrix curr]
  (if (some coll? (first matrix))
    (append2 [(create-new-row-matrix (first matrix) curr)] (rest matrix))
    (append2 [(first matrix)] (create-new-matrix (rest matrix) curr))
  )
)

(def glmatrix (ref []))"referenca za rezultanta matrica od try-all (rezultatot na solve)"

(defn not-sudoku-row "
  Proveruva dali ima dva isti elementi vo redicata, kolonata ili kockata za sekoj element od row
  x e vo koj red sea naoga row. Ako se najdat dva isti elementi funkcijata vraka false, ako ne gi proveruva
  ostanatite elementi. Ako se isprazni redicata se vraka true zatoa sto ne se najdeni dva isti elementi."
  [row matrix x]
  (if (empty? row)
    true
    (if (not (coll? (first row)) )
      (if (member? (first row) (remove-first (first row) (remove-first (first row) (remove-first (first row) (reduce append2 [(matrix-row x matrix)(matrix-column (- 9 (count row) ) matrix )(matrix-box x (- 9 (count row) ) matrix ) ] )))))
        false
        (not-sudoku-row (rest row) matrix x)
      )
      (not-sudoku-row (rest row) matrix x)
    )
  )
)

(defn not-sudoku"
  Na vlez prima matrica i na izlez vraka true ili false. False vraka ako postoi greska vo matricata(da ima
  dve isti vrednosti vo ista redica,kolona ili kocka. Proverkata ja pravi so izminuvanje na site elementi
  i nivna proverka dali se unikatni."
  ([matrix]
    (not-sudoku matrix matrix)
  )
  ([matrix1 matrix2]
    (if (empty? matrix1)
      true
      (if (not-sudoku-row (first matrix1) matrix2 (- 9 (count matrix1)))
        (not-sudoku (rest matrix1) matrix2)
        false
      )
    )
  )
)

(defn try-all "
  Vo prviot if ima uste eden if za dali e validno resenie na sudokuto (ne se izvrsuvase pravilno vo clojure-1.8.0)
  Funkcijata gi proveruva site mozni vrednosti za site elementi koi imaat mnozestvo od mozni vrednosti i proveruva dali
  e tocno resenieto. Ako ne e togas proveruva naredna mozna vrednost za poslednoto mnozestvo sto se proveruvalo.
  Taka se dodeka ima mnozestva vo matricata. Koga vo matricata ke ima samo atomicni elementi togas funkcijata ja vraka
  vrednosta od referencata glmatrix, pri sekoja proverka se zapamtuva posledniot obid vo glmatrix. Ako se ispolnat uslovite
  da e validno resenieto posledniot funkcijata vraka true i na kraj se vraka matricata od glmatrix vo funkcijata solve
  i toa e kraj na izvrsuvanjeto na funkcijata solve."
  ([mnozestva matrix]
    (if (empty? mnozestva)
      (if (not (not-sudoku matrix))
        false
        true
      )
      (if (some empty? mnozestva)
        false
        (if (not (not-sudoku matrix))
          false
          (if (try-all (my-flatten (get-mnozestva (dosync (ref-set glmatrix (to-atomic (distinct-sudoku (create-new-matrix matrix [(first (first mnozestva))]) (create-new-matrix matrix [(first (first mnozestva))])))) ))) @glmatrix)
            @glmatrix
            (try-all (cons (set (rest (first mnozestva))) (rest mnozestva)) matrix)
          )
        )
      )
    )
  )
)

(defn solve "
  Na pocetok solve se povikuva so eden argument. Togas se transformira matricata i se izvrsuva prviot cekor,
  se pravat mnozestvata na mozni vrednosti za sekoj element (distinct-sudoku) i tie mnozestva sto se edinecen
  element gi pretvora vo atomicni (to-atomic). Rekurzivniot povik e so dva argumenti, prviot e matricata sto se
  promenila, vtoriot e starata matrica. Ovaj cekor se povtoruva se dodeka prviot i vtoriot argument ne se ednakvi.
  Koga ke se izedacat ako ne e resena matricata togas se izvrsuva distinct-sets-sudoku, i naredniot povik se izvrsuva
  so tri argumenti. Ako ima promena togas se pravi istoto od prviot cekor i se povikuva rekurzivno so dva argumenti,
  ako nema promena togas se probuvaat site mozni kombinacii na mnozestvata (try-all) za da se stigni do resenieto."
  ([matrix]
    (solve (to-atomic (distinct-sudoku (to-atomic (transform matrix)) (to-atomic (transform matrix)))) [])
  )
  ([matrix something]
    (if (= matrix something)
      (if (some coll? (my-flatten matrix))
        (solve (to-atomic (distinct-sets-sudoku matrix matrix)) matrix 2)
        matrix
      )
      (if (some coll? (my-flatten matrix))
        (solve (to-atomic (distinct-sudoku matrix matrix)) matrix)
        matrix
      )
    )
  )
  ([matrix something k]
    (if (= matrix something)
      (try-all (my-flatten (get-mnozestva matrix)) matrix)
      (solve (distinct-sudoku matrix matrix) matrix)
    )
  )
)

(ns user (:import
     (java.awt BorderLayout)
     (java.awt Font)
     (java.awt.event ActionEvent ActionListener)
     (javax.swing.table TableModel AbstractTableModel DefaultTableCellRenderer)
     (javax.swing JTable JScrollPane JFrame JButton SwingUtilities JLabel SwingConstants JOptionPane)))

(def tabelaa (ref [["" "" "" "" "" "" "" "" "" ]
                              ["" "" "" "" "" "" "" "" "" ]
                              ["" "" "" "" "" "" "" "" "" ]
                              ["" "" "" "" "" "" "" "" "" ]
                              ["" "" "" "" "" "" "" "" "" ]
                              ["" "" "" "" "" "" "" "" "" ]
                              ["" "" "" "" "" "" "" "" "" ]
                              ["" "" "" "" "" "" "" "" "" ]
                              ["" "" "" "" "" "" "" "" "" ]]))

(def tabelab (ref @tabelaa))

(defn old-table-model1
  "clojure.inspector old-table-model kopija
  smenato (isCellEditable ) od false vo true
  dodadeno setValueAt"
  [data]
  (let [row1 (first @data)
	colcnt (count row1)
	cnt (count @data)
	vals (if (map? row1) vals identity)]
    (proxy [AbstractTableModel] []
      (addTableModelListener [tableModelListener])
      (getColumnClass [columnIndex] Object)
      (getColumnCount [] colcnt)
      (getColumnName [columnIndex]
	(if (map? row1)
	  (name (nth (keys row1) columnIndex))
	  (str columnIndex)))
      (getRowCount [] cnt)
      (getValueAt [rowIndex columnIndex]
	(nth (vals (nth @data rowIndex)) columnIndex))
      (isCellEditable [rowIndex columnIndex] true)
      (removeTableModelListener [tableModelListener])
      (setValueAt [aValue rowIndex columnIndex]

      (dosync (ref-set data
                (into [] (append
                  (take rowIndex @data)
                  (into [] (append
                    (cons (into [] (append
                      (take columnIndex (nth @data rowIndex))
                      (append
                        (cons (if (or (= aValue "") (= (Integer/parseInt aValue) 0) (> (count aValue) 1)) "" (Integer/parseInt aValue)) [])
                        (drop (+ columnIndex 1) (nth @data rowIndex)))) ) [])
                    (drop (+ rowIndex 1) @data))))
                  )))))))

(def button1 (JButton. "SOLVE"))
(def button2 (JButton. "CLEAR"))

(def table-result (JTable. (old-table-model1 tabelab)))
(def table-input (JTable. (old-table-model1 tabelaa)))

(def frame (JFrame. "SUDOKU"))
(def check (ref []))
(def act-solve
  (proxy [ActionListener] []
    (actionPerformed [event1]
     (dosync (ref-set check (solve @tabelaa)))
     (if (or (not @check) (not (not-sudoku @check)))
        (do
          (JOptionPane/showMessageDialog nil "Invalid input!")
          (dosync (ref-set check []))
        )
        (do
          (dosync (ref-set tabelab @check))
          (dosync (ref-set check []))
          (doto
            table-result
            (.setModel (old-table-model1 tabelab))
          )
        )
      )
    )
  )
)

(def act-clear
  (proxy [ActionListener] []
    (actionPerformed [event1]
      (dosync (ref-set tabelaa [["" "" "" "" "" "" "" "" "" ]
                              ["" "" "" "" "" "" "" "" "" ]
                              ["" "" "" "" "" "" "" "" "" ]
                              ["" "" "" "" "" "" "" "" "" ]
                              ["" "" "" "" "" "" "" "" "" ]
                              ["" "" "" "" "" "" "" "" "" ]
                              ["" "" "" "" "" "" "" "" "" ]
                              ["" "" "" "" "" "" "" "" "" ]
                              ["" "" "" "" "" "" "" "" "" ]]))
      (dosync (ref-set tabelab @tabelaa))
      (doto
        table-input
        (.setModel (old-table-model1 tabelaa))
      )
      (doto
        table-result
        (.setModel (old-table-model1 tabelab))
      )
    )
  )
)

(defn vizuelizacija
  []
  (doto frame
    (.add
      (doto
        table-input
        (.setFont (Font. "SERIF" Font/PLAIN 30))
        (.setRowHeight 30)
      )
      (BorderLayout/NORTH)
    )
    (.add
      (doto
        button1
        (.setVisible true)
        (.setFont (Font. "SERIF" Font/PLAIN 35))
        (.addActionListener act-solve)
      )
      (BorderLayout/WEST)
    )
    (.add
      (doto
        button2
        (.setVisible true)
        (.setFont (Font. "SERIF" Font/PLAIN 35))
        (.addActionListener act-clear)
      )
      (BorderLayout/EAST)
    )
    (.add
      (doto
        table-result
        (.setFont (Font. "SERIF" Font/PLAIN 30))
        (.setRowHeight 30)
      )
      (BorderLayout/SOUTH)
    )
  (.setSize 315 630)
  (.setVisible true))
)
