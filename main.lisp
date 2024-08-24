; read file 
(defun readFile (filename)
  (with-open-file (stream filename :if-does-not-exist nil)
    (if (not stream)
      nil
      (loop for line = (read-line stream nil)
          while line
          collect (coerce line 'list)))))


(defun formatFileData (matrix)
  "Takes in a matrix and returns a list"
  (apply #'append matrix))


; create priorety queue
(defun makeEmptyPriorityQueue ()
  nil)

(defun makePriorityQueue (aLst)
  (sort aLst #'< :key #'cdr))

(defun enQueue (pQueue val) 
  (makePriorityQueue (append pQueue (list val)))) ; make faster

(defun deQueue (pQueue)
  (cdr pQueue))

(defun peek (pQueue)
  (car pQueue))

;create biunary tree

(defun makeBinaryTree (root n1 n2)
  (list root n1 n2))

; create alist freq table


(defun fillFreqTable (str)
  (let ((freqTable nil) (val nil))
    (loop for c in str do 
          (setf val (assoc c freqTable)) 
          (if val 
            (setf (cdr (assoc c freqTable)) (1+ (cdr val)))   ; the character is in the alist so we append the freq by one 
            (setf freqTable (enQueue freqTable (cons c 1))))) ; if val is null than the character c is not in the alist so add it with a freq of one
    freqTable))

; create huffman tree
(defun createHuffmanTree (freqTable)
  (if (= (length freqTable) 1)
    (car freqTable)
    (createHuffmanTree
      (enQueue (cddr freqTable)
        (let ((node1 (pop freqTable)) (node2 (pop freqTable)))
          (cons 
            (makeBinaryTree nil node1 node2)
              (+ (cdr node1) (cdr node2))))))))


(defun dottedListP (lst)
  (not (listp (cdr lst))))



(defun cleanHuffmanTree (tree)
  (cond
    ((atom tree) tree)
    ((dottedListP tree) (cleanHuffmanTree (car tree)))
    ((listP tree) (mapcar #'cleanHuffmanTree tree))
    (t tree)))

; iterate through file and encode it with the huffman tree
(defun huffmanEncodeChar (tree char &optional (path nil) (i 0)) 
  (let ((c1 (nth (+ 1 (* i 2)) tree)) (c2 (nth (+ 2 (* i 2)) tree)))
    (or
      (cond 
        ((null c1) nil)
        ((listp c1) (huffmanEncodeChar c1 char (append path (list 1))))
        ((char= c1 char) (append path (list 1)))
        (t nil))
    
      (cond 
        ((null c2) nil)
        ((listp c2) (huffmanEncodeChar c2 char (append path (list 0))))
        ((char= c2 char) (append path (list 0)))
        (t nil))
      )))


(defun huffmanEncodeString (tree str)
  (let ((path nil))
    (loop for c in str do 
          (push (huffmanEncodeChar tree c) path))
    (reverse path)))
; write new encoded data to file


; reverse to decode

(defun huffmanDecodeBin (tree binList)
  "decodes one char from a huffman encoded binary"
  (let ((treeI 0) (binI 0))
    (if (= (nth binI binList) 1)
      (setf treeI (+ 1 (* treeI 2)))
      (setf treeI (+ 2 (* treeI 2))))

    (if (listp (nth treeI tree))
      (huffmanDecodeBin (nth treeI tree) (cdr binList))
      (nth treeI tree))))

(defun huffmanDecodeBinList (tree binMatrix)
  (let ((strResult nil))
    (loop for b in binMatrix do
          (push (huffmanDecodeBin tree b) strResult))
    (reverse strResult)))

(defun writeFile (fName contents)
  (with-open-file (stream fName
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  (format stream "~a~%" contents)))


(print "What is the file you want to compress?")
(force-output)

(defvar *fileData*
  (formatFileData 
    (readFile 
      (read-line))))


(defvar *tree* 
  (cleanHuffmanTree
    (createHuffmanTree 
      (fillFreqtable 
        *fileData*))))


(defvar *bin* (huffmanEncodeString *tree* *fileData*))

(print "What is the new file name?")
(force-output)

(writeFile (read-line) (append *tree* *bin*))

(print (readFile (read-line)))
;(print (huffmanDecodeBinList *tree* *bin*))


