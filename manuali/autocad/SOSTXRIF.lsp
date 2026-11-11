;;; SOSTXRIF.lsp - Sostituisce il file di un riferimento esterno (xrif)
;;; e confronta l'ingombro dell'istanza prima e dopo, per dire se il
;;; nuovo disegno e' allineato al vecchio.
;;;
;;; BIM News - I manuali di Claude - AutoCAD 2026
;;; Versione pubblicata sul sito: provala sempre su una COPIA del disegno.
;;;
;;; Uso:  (load "SOSTXRIF")  poi  SOSTXRIF
;;;   1. seleziona l'xrif nel disegno
;;;   2. scegli il nuovo file DWG
;;;   3. imposta la tolleranza (Invio = 0.01 unita' del disegno)
;;;   4. leggi il risultato: ALLINEATO, TRASLATO, SCALA DIVERSA, INGOMBRO CAMBIATO
;;; Tutte le modifiche sono un'unica operazione: un solo ANNULLA le toglie.

(vl-load-com)

(defun sx:bbox (obj / pmin pmax)
  ;; ingombro dell'istanza in coordinate del disegno: (xmin ymin xmax ymax)
  (vla-GetBoundingBox obj 'pmin 'pmax)
  (setq pmin (vlax-safearray->list pmin)
        pmax (vlax-safearray->list pmax))
  (list (car pmin) (cadr pmin) (car pmax) (cadr pmax))
)

(defun sx:fmt (x) (rtos x 2 4))

(defun c:SOSTXRIF (/ doc sel ent obj bname bdef oldpath newpath tol
                     b1 b2 w1 h1 w2 h2 dx dy dx2 dy2 rx ry esito ans)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ "\nSOSTXRIF - sostituzione di un xrif con verifica dell'allineamento.")
  (setq sel (entsel "\nSeleziona l'xrif da sostituire: "))
  (cond
    ((null sel) (princ "\nNessun oggetto selezionato."))
    ((/= (cdr (assoc 0 (entget (setq ent (car sel))))) "INSERT")
     (princ "\nL'oggetto selezionato non e' un riferimento esterno."))
    (T
     (setq obj   (vlax-ename->vla-object ent)
           bname (vla-get-EffectiveName obj)
           bdef  (vla-Item (vla-get-Blocks doc) bname))
     (if (= (vla-get-IsXRef bdef) :vlax-false)
       (princ (strcat "\n\"" bname "\" e' un blocco, non un xrif."))
       (progn
         (setq oldpath (vla-get-Path bdef))
         (princ (strcat "\nXrif: " bname "\nFile attuale: " oldpath))
         (setq newpath (getfiled "Scegli il nuovo file DWG" oldpath "dwg" 0))
         (if (null newpath)
           (princ "\nOperazione annullata: nessun file scelto.")
           (progn
             (setq tol (getreal "\nTolleranza in unita' del disegno <0.01>: "))
             (if (or (null tol) (<= tol 0.0)) (setq tol 0.01))
             (vla-StartUndoMark doc)
             (setq b1 (sx:bbox obj))
             (vla-put-Path bdef newpath)
             (vla-Reload bdef)
             (setq b2 (sx:bbox obj))
             (setq w1 (- (nth 2 b1) (nth 0 b1))  h1 (- (nth 3 b1) (nth 1 b1))
                   w2 (- (nth 2 b2) (nth 0 b2))  h2 (- (nth 3 b2) (nth 1 b2))
                   dx (- (nth 0 b2) (nth 0 b1))  dy (- (nth 1 b2) (nth 1 b1))
                   dx2 (- (nth 2 b2) (nth 2 b1)) dy2 (- (nth 3 b2) (nth 3 b1)))
             (setq rx (if (> (abs w1) 1e-9) (/ w2 w1) 0.0)
                   ry (if (> (abs h1) 1e-9) (/ h2 h1) 0.0))
             (setq esito
               (cond
                 ((and (< (abs dx) tol) (< (abs dy) tol) (< (abs dx2) tol) (< (abs dy2) tol))
                  "ALLINEATO")
                 ((and (< (abs (- w2 w1)) tol) (< (abs (- h2 h1)) tol))
                  "TRASLATO")
                 ((and (> rx 0.0) (< (abs (- rx ry)) 0.001) (> (abs (- rx 1.0)) 0.001))
                  "SCALA DIVERSA")
                 (T "INGOMBRO CAMBIATO")))
             (princ "\n----------------------------------------------")
             (princ (strcat "\nNuovo file:  " newpath))
             (princ (strcat "\nIngombro prima:  " (sx:fmt w1) " x " (sx:fmt h1)))
             (princ (strcat "\nIngombro dopo:   " (sx:fmt w2) " x " (sx:fmt h2)))
             (princ (strcat "\nSpostamento dell'angolo in basso a sinistra: dX " (sx:fmt dx) "  dY " (sx:fmt dy)))
             (princ (strcat "\nRISULTATO: " esito))
             (cond
               ((= esito "ALLINEATO") (princ "\nIl nuovo file e' sovrapposto al vecchio: nulla da fare."))
               ((= esito "TRASLATO")
                (princ "\nStesso ingombro, spostato. Verifica il motivo con l'architetto prima di riallineare.")
                (initget "Si No")
                (setq ans (getkword "\nSpostare l'istanza selezionata per riallinearla? [Si/No] <No>: "))
                (if (= ans "Si")
                  (progn
                    (vla-Move obj (vlax-3d-point (list dx dy 0.0)) (vlax-3d-point '(0.0 0.0 0.0)))
                    (princ "\nIstanza spostata. Le altre istanze dello stesso xrif non sono state toccate."))))
               ((= esito "SCALA DIVERSA")
                (princ (strcat "\nRapporto di scala circa " (sx:fmt rx) ": probabile differenza di unita' (m, cm, mm) tra i due file.")))
               (T (princ "\nIl contenuto e' cambiato: controlla le piante una per una (vedi la guida, \"Verificare piante spostate o modificate\").")))
             (princ "\nRicorda di rinominare l'xrif nella palette Riferimenti esterni con il nome della nuova versione.")
             (princ "\n----------------------------------------------")
             (vla-EndUndoMark doc)))))))
  (princ)
)

(princ "\nSOSTXRIF caricato. Scrivi SOSTXRIF per usarlo.")
(princ)
