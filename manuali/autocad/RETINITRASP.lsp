;;; RETINITRASP.lsp - Imposta la trasparenza di tutti i retini del disegno,
;;; anche dentro i blocchi, per evitare i retini neri pieni in stampa
;;; quando il file viene usato come xrif.
;;;
;;; BIM News - I manuali di Claude - AutoCAD 2026
;;; Versione pubblicata sul sito: usala su una COPIA del file dell'architetto,
;;; mai sull'originale ricevuto.
;;;
;;; Uso:  (load "RETINITRASP")  poi  RETINITRASP
;;;   valore da 0 a 90 (Invio = 50, simile alla dissolvenza degli xrif a schermo)
;;; Tutte le modifiche sono un'unica operazione: un solo ANNULLA le toglie.

(vl-load-com)

(defun c:RETINITRASP (/ doc val n bloccati blk obj lay res)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ "\nRETINITRASP - trasparenza dei retini.")
  (setq val (getint "\nTrasparenza da 0 a 90 <50>: "))
  (if (or (null val) (< val 0) (> val 90)) (setq val 50))
  (setq n 0 bloccati '())
  (vla-StartUndoMark doc)
  ;; tutti i blocchi del disegno: spazio modello, layout e definizioni di blocco
  (vlax-for blk (vla-get-Blocks doc)
    (if (= (vla-get-IsXRef blk) :vlax-false)
      (vlax-for obj blk
        (if (= (vla-get-ObjectName obj) "AcDbHatch")
          (progn
            (setq res (vl-catch-all-apply 'vla-put-EntityTransparency (list obj (itoa val))))
            (if (vl-catch-all-error-p res)
              (progn
                (setq lay (vla-get-Layer obj))
                (if (not (member lay bloccati)) (setq bloccati (cons lay bloccati))))
              (setq n (1+ n))))))))
  (vla-EndUndoMark doc)
  (princ (strcat "\nRetini modificati: " (itoa n) " (trasparenza " (itoa val) ")."))
  (if bloccati
    (progn
      (princ "\nRetini non modificati, probabilmente su layer bloccati:")
      (foreach l bloccati (princ (strcat "\n  - " l)))
      (princ "\nSblocca questi layer e rilancia il comando.")))
  (princ "\nSalva la copia e punta l'xrif alla copia (SOSTXRIF). In stampa spunta \"Stampa trasparenza\".")
  (princ)
)

(princ "\nRETINITRASP caricato. Scrivi RETINITRASP per usarlo.")
(princ)
